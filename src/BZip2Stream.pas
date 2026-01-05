unit BZip2Stream;

{$I+}  { Enable I/O checking }

interface

uses
  Windows, Classes, SysUtils, ExtractZLib;

type
  /// <summary>
  /// Exception class for BZIP2 decompression errors
  /// </summary>
  EBZip2Error = class(Exception);

  /// <summary>
  /// Streaming BZIP2 decompressor implementing TStream interface
  /// Allows efficient decompression without temporary files
  /// </summary>
  /// <remarks>
  /// Uses BZIP2 algorithm for decompression.
  /// Supports streaming operations for large files.
  /// </remarks>
  TBZip2InputStream = class(TStream)
  private
    FSourceStream: TStream;
    FOwnsStream: Boolean;
    FBuffer: PByte;
    FBufferSize: Integer;
    FBufferPos: Integer;
    FBufferEnd: Integer;
    FDecompressor: Pointer;
    FHeaderRead: Boolean;
    FFinished: Boolean;
    
    procedure InitDecompressor;
    procedure FreeDecompressor;
    function DecompressChunk: Integer;
  protected
    function GetSize: Int64; override;
  public
    /// <summary>
    /// Creates BZIP2 input stream from source stream
    /// </summary>
    /// <param name="ASource">Source stream with compressed BZIP2 data</param>
    /// <param name="AOwnsStream">If True, source stream will be freed on destruction</param>
    constructor Create(ASource: TStream; AOwnsStream: Boolean = False);
    destructor Destroy; override;
    
    /// <summary>
    /// Reads decompressed data from stream
    /// </summary>
    /// <param name="Buffer">Destination buffer</param>
    /// <param name="Count">Number of bytes to read</param>
    /// <returns>Actual number of bytes read</returns>
    function Read(var Buffer; Count: Longint): Longint; override;
    
    /// <summary>
    /// Write not supported for input stream
    /// </summary>
    function Write(const Buffer; Count: Longint): Longint; override;
    
    /// <summary>
    /// Seek not supported for compressed stream
    /// </summary>
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

implementation

const
  BUFFER_SIZE = 65536; // 64 KB buffer
  
  // BZIP2 Stream format constants
  BZ2_HEADER_MAGIC: array[0..2] of Byte = ($42, $5A, $68); // 'BZh'
  
  // BZIP2 return codes
  BZ_OK = 0;
  BZ_RUN_OK = 1;
  BZ_FLUSH_OK = 2;
  BZ_FINISH_OK = 3;
  BZ_STREAM_END = 4;
  BZ_SEQUENCE_ERROR = -1;
  BZ_PARAM_ERROR = -2;
  BZ_MEM_ERROR = -3;
  BZ_DATA_ERROR = -4;
  BZ_DATA_ERROR_MAGIC = -5;
  BZ_IO_ERROR = -6;
  BZ_UNEXPECTED_EOF = -7;
  BZ_OUTBUFF_FULL = -8;
  BZ_CONFIG_ERROR = -9;

type
  bz_stream = record
    next_in: PChar;
    avail_in: Cardinal;
    total_in_lo32: Cardinal;
    total_in_hi32: Cardinal;
    
    next_out: PChar;
    avail_out: Cardinal;
    total_out_lo32: Cardinal;
    total_out_hi32: Cardinal;
    
    state: Pointer;
    
    bzalloc: Pointer;
    bzfree: Pointer;
    opaque: Pointer;
  end;

// BZIP2 function types
type
  T_BZ2_bzDecompressInit = function(var strm: bz_stream; verbosity: Integer; small: Integer): Integer; cdecl;
  T_BZ2_bzDecompress = function(var strm: bz_stream): Integer; cdecl;
  T_BZ2_bzDecompressEnd = function(var strm: bz_stream): Integer; cdecl;

var
  BZip2Handle: THandle = 0;
  _BZ2_bzDecompressInit: T_BZ2_bzDecompressInit = nil;
  _BZ2_bzDecompress: T_BZ2_bzDecompress = nil;
  _BZ2_bzDecompressEnd: T_BZ2_bzDecompressEnd = nil;

// Load BZIP2 library dynamically
procedure LoadBZip2DLL; forward;

{ TBZip2InputStream }

constructor TBZip2InputStream.Create(ASource: TStream; AOwnsStream: Boolean);
begin
  inherited Create;
  FSourceStream := ASource;
  FOwnsStream := AOwnsStream;
  FBufferSize := BUFFER_SIZE;
  GetMem(FBuffer, FBufferSize);
  FBufferPos := 0;
  FBufferEnd := 0;
  FDecompressor := nil;
  FHeaderRead := False;
  FFinished := False;
  
  InitDecompressor;
end;

destructor TBZip2InputStream.Destroy;
begin
  FreeDecompressor;
  FreeMem(FBuffer);
  if FOwnsStream then
    FSourceStream.Free;
  inherited;
end;

procedure TBZip2InputStream.InitDecompressor;
var
  Header: array[0..3] of Byte;
  BytesRead: Integer;
  ret: Integer;
  Stream: ^bz_stream;
begin
  // Read and verify BZIP2 header
  BytesRead := FSourceStream.Read(Header, 4);
  if BytesRead <> 4 then
    raise EBZip2Error.Create('Invalid BZIP2 stream: header too short');
    
  if not CompareMem(@Header, @BZ2_HEADER_MAGIC, 3) then
    raise EBZip2Error.Create('Invalid BZIP2 stream: bad magic bytes');
    
  // Header[3] contains block size (1-9)
  if not (Chr(Header[3]) in ['1'..'9']) then
    raise EBZip2Error.Create('Invalid BZIP2 stream: invalid block size');
    
  FHeaderRead := True;
  
  // Load BZIP2 library
  LoadBZip2DLL;
  
  // Initialize BZIP2 decoder
  GetMem(FDecompressor, SizeOf(bz_stream));
  Stream := FDecompressor;
  FillChar(Stream^, SizeOf(bz_stream), 0);
  
  Stream^.bzalloc := nil;
  Stream^.bzfree := nil;
  Stream^.opaque := nil;
  
  ret := _BZ2_bzDecompressInit(Stream^, 0, 0);
  if ret <> BZ_OK then
    raise EBZip2Error.CreateFmt('BZIP2 decompression init failed: error %d', [ret]);
end;

procedure TBZip2InputStream.FreeDecompressor;
var
  Stream: ^bz_stream;
begin
  if Assigned(FDecompressor) then
  begin
    Stream := FDecompressor;
    if Assigned(_BZ2_bzDecompressEnd) then
      _BZ2_bzDecompressEnd(Stream^);
    FreeMem(FDecompressor);
    FDecompressor := nil;
  end;
end;

function TBZip2InputStream.DecompressChunk: Integer;
var
  InBuf: array[0..4095] of Byte;
  BytesRead: Integer;
  Stream: ^bz_stream;
  ret: Integer;
begin
  Result := 0;
  
  if FFinished then
    Exit;
    
  Stream := FDecompressor;
  
  // Setup output buffer
  Stream^.next_out := PChar(FBuffer);
  Stream^.avail_out := FBufferSize;
  
  while (Stream^.avail_out > 0) and not FFinished do
  begin
    // Read more input if needed
    if Stream^.avail_in = 0 then
    begin
      BytesRead := FSourceStream.Read(InBuf, SizeOf(InBuf));
      if BytesRead = 0 then
      begin
        FFinished := True;
        Break;
      end;
      Stream^.next_in := @InBuf[0];
      Stream^.avail_in := BytesRead;
    end;
    
    // Decompress
    ret := _BZ2_bzDecompress(Stream^);
    
    if ret = BZ_STREAM_END then
    begin
      FFinished := True;
      Break;
    end
    else if ret <> BZ_OK then
    begin
      raise EBZip2Error.CreateFmt('BZIP2 decompression failed: error %d', [ret]);
    end;
  end;
  
  Result := Integer(FBufferSize) - Integer(Stream^.avail_out);
end;

function TBZip2InputStream.Read(var Buffer; Count: Longint): Longint;
var
  Dest: PByte;
  BytesToCopy: Integer;
  BytesDecompressed: Integer;
begin
  Result := 0;
  Dest := @Buffer;
  
  while (Count > 0) and not FFinished do
  begin
    // If buffer is empty, decompress more data
    if FBufferPos >= FBufferEnd then
    begin
      FBufferPos := 0;
      BytesDecompressed := DecompressChunk;
      FBufferEnd := BytesDecompressed;
      
      if BytesDecompressed = 0 then
        Break; // End of stream
    end;
    
    // Copy from buffer to output
    BytesToCopy := FBufferEnd - FBufferPos;
    if BytesToCopy > Count then
      BytesToCopy := Count;
      
    Move(Pointer(Integer(FBuffer) + FBufferPos)^, Dest^, BytesToCopy);
    Inc(FBufferPos, BytesToCopy);
    Inc(Dest, BytesToCopy);
    Inc(Result, BytesToCopy);
    Dec(Count, BytesToCopy);
  end;
end;

function TBZip2InputStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EBZip2Error.Create('Cannot write to BZIP2 input stream');
end;

function TBZip2InputStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  raise EBZip2Error.Create('Cannot seek in BZIP2 compressed stream');
end;

function TBZip2InputStream.GetSize: Int64;
begin
  Result := -1; // Unknown size for compressed stream
end;

procedure LoadBZip2DLL;
var
  DLLPath: string;
  ExePath: string;
  LibPath: string;
  LastError: DWORD;
  ErrorMsg: array[0..1023] of Char;
begin
  if BZip2Handle <> 0 then
    Exit;
    
  LastError := 0;
  try
    ExtractBZip2DLL;
  except
    // Continue, DLL might already exist
  end;
  
  // Try AppData location first
  DLLPath := GetLibraryPath + 'libbz2.dll';
  if FileExists(DLLPath) then
  begin
    BZip2Handle := LoadLibrary(PChar(DLLPath));
    if BZip2Handle = 0 then
      LastError := GetLastError;
  end;
  
  // If not found, try exe directory
  if BZip2Handle = 0 then
  begin
    ExePath := ExtractFilePath(ParamStr(0)) + 'libbz2.dll';
    if FileExists(ExePath) then
    begin
      BZip2Handle := LoadLibrary(PChar(ExePath));
      if BZip2Handle = 0 then
        LastError := GetLastError;
    end;
  end;
  
  // Try lib/ subdirectory
  if BZip2Handle = 0 then
  begin
    LibPath := ExtractFilePath(ParamStr(0)) + 'lib\libbz2.dll';
    if FileExists(LibPath) then
    begin
      BZip2Handle := LoadLibrary(PChar(LibPath));
      if BZip2Handle = 0 then
        LastError := GetLastError;
    end;
  end;
  
  if BZip2Handle = 0 then
  begin
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, LastError, 0, 
                  ErrorMsg, SizeOf(ErrorMsg), nil);
    raise EBZip2Error.CreateFmt(
      'Failed to load libbz2.dll. Tried:' + #13#10 +
      '  1. %s' + #13#10 +
      '  2. %s' + #13#10 +
      '  3. %s' + #13#10 +
      'Windows Error %d: %s' + #13#10 +
      'To enable BZIP2 support, download libbz2.dll from:' + #13#10 +
      'https://sourceforge.net/projects/gnuwin32/files/bzip2/',
      [DLLPath, ExePath, LibPath, LastError, Trim(string(ErrorMsg))]);
  end;
    
  @_BZ2_bzDecompressInit := GetProcAddress(BZip2Handle, 'BZ2_bzDecompressInit');
  @_BZ2_bzDecompress := GetProcAddress(BZip2Handle, 'BZ2_bzDecompress');
  @_BZ2_bzDecompressEnd := GetProcAddress(BZip2Handle, 'BZ2_bzDecompressEnd');
  
  if not Assigned(_BZ2_bzDecompressInit) then
    raise EBZip2Error.Create('Failed to load BZ2_bzDecompressInit from libbz2.dll');
  if not Assigned(_BZ2_bzDecompress) then
    raise EBZip2Error.Create('Failed to load BZ2_bzDecompress from libbz2.dll');
  if not Assigned(_BZ2_bzDecompressEnd) then
    raise EBZip2Error.Create('Failed to load BZ2_bzDecompressEnd from libbz2.dll');
end;

end.
