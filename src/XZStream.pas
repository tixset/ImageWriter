unit XZStream;

{$I+}  { Enable I/O checking }

interface

uses
  Windows, Classes, SysUtils, ExtractZLib;

type
  /// <summary>
  /// Exception class for XZ decompression errors
  /// </summary>
  EXZError = class(Exception);

  /// <summary>
  /// Streaming XZ decompressor implementing TStream interface
  /// Allows efficient decompression without temporary files
  /// </summary>
  /// <remarks>
  /// Uses LZMA SDK via Windows API for XZ format decompression.
  /// Supports streaming operations for large files.
  /// </remarks>
  TXZInputStream = class(TStream)
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
    /// Creates XZ input stream from source stream
    /// </summary>
    /// <param name="ASource">Source stream with compressed XZ data</param>
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
  
  // XZ Stream format constants
  XZ_HEADER_MAGIC: array[0..5] of Byte = ($FD, $37, $7A, $58, $5A, $00);
  
  // LZMA return codes
  LZMA_OK = 0;
  LZMA_STREAM_END = 1;
  LZMA_NO_CHECK = 2;
  LZMA_UNSUPPORTED_CHECK = 3;
  LZMA_GET_CHECK = 4;
  LZMA_MEM_ERROR = 5;
  LZMA_MEMLIMIT_ERROR = 6;
  LZMA_FORMAT_ERROR = 7;
  LZMA_OPTIONS_ERROR = 8;
  LZMA_DATA_ERROR = 9;
  LZMA_BUF_ERROR = 10;
  LZMA_PROG_ERROR = 11;

type
  lzma_ret = Integer;
  lzma_action = Integer;
  
  lzma_allocator = record
    alloc: Pointer;
    free: Pointer;
    opaque: Pointer;
  end;
  
  lzma_stream = record
    next_in: PByte;
    avail_in: Cardinal;
    total_in: Int64;
    
    next_out: PByte;
    avail_out: Cardinal;
    total_out: Int64;
    
    allocator: Pointer;
    internal: Pointer;
    
    reserved_ptr1: Pointer;
    reserved_ptr2: Pointer;
    reserved_ptr3: Pointer;
    reserved_ptr4: Pointer;
    reserved_int1: Int64;
    reserved_int2: Int64;
    reserved_int3: Cardinal;
    reserved_int4: Cardinal;
    reserved_enum1: Integer;
    reserved_enum2: Integer;
  end;
  
const
  LZMA_RUN = 0;
  LZMA_FINISH = 3;
  LZMA_CONCATENATED = $08;

// LZMA function types
type
  T_lzma_stream_decoder = function(var strm: lzma_stream; memlimit: Int64; flags: Cardinal): lzma_ret; cdecl;
  T_lzma_code = function(var strm: lzma_stream; action: lzma_action): lzma_ret; cdecl;
  T_lzma_end = procedure(var strm: lzma_stream); cdecl;

var
  LzmaHandle: THandle = 0;
  _lzma_stream_decoder: T_lzma_stream_decoder = nil;
  _lzma_code: T_lzma_code = nil;
  _lzma_end: T_lzma_end = nil;

// Load LZMA library dynamically
procedure LoadLzmaDLL; forward;

// LZMA SDK function imports (using static linking or runtime loading)
// For simplicity, we'll use a pure Pascal implementation based on Igor Pavlov's code

{ TXZInputStream }

constructor TXZInputStream.Create(ASource: TStream; AOwnsStream: Boolean);
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

destructor TXZInputStream.Destroy;
begin
  FreeDecompressor;
  FreeMem(FBuffer);
  if FOwnsStream then
    FSourceStream.Free;
  inherited;
end;

procedure TXZInputStream.InitDecompressor;
var
  Header: array[0..5] of Byte;
  BytesRead: Integer;
  ret: lzma_ret;
  Stream: ^lzma_stream;
begin
  // Read and verify XZ header
  BytesRead := FSourceStream.Read(Header, 6);
  if BytesRead <> 6 then
    raise EXZError.Create('Invalid XZ stream: header too short');
    
  if not CompareMem(@Header, @XZ_HEADER_MAGIC, 6) then
    raise EXZError.Create('Invalid XZ stream: bad magic bytes');
    
  FHeaderRead := True;
  
  // Load LZMA library
  LoadLzmaDLL;
  
  // Initialize LZMA decoder
  GetMem(FDecompressor, SizeOf(lzma_stream));
  Stream := FDecompressor;
  FillChar(Stream^, SizeOf(lzma_stream), 0);
  
  Stream^.allocator := nil;
  
  // Initialize stream decoder with 256MB memory limit
  ret := _lzma_stream_decoder(Stream^, 256 * 1024 * 1024, LZMA_CONCATENATED);
  if ret <> LZMA_OK then
    raise EXZError.CreateFmt('LZMA decompression init failed: error %d', [ret]);
end;

procedure TXZInputStream.FreeDecompressor;
var
  Stream: ^lzma_stream;
begin
  if Assigned(FDecompressor) then
  begin
    Stream := FDecompressor;
    if Assigned(_lzma_end) then
      _lzma_end(Stream^);
    FreeMem(FDecompressor);
    FDecompressor := nil;
  end;
end;

function TXZInputStream.DecompressChunk: Integer;
var
  InBuf: array[0..4095] of Byte;
  BytesRead: Integer;
  Stream: ^lzma_stream;
  ret: lzma_ret;
begin
  Result := 0;
  
  if FFinished then
    Exit;
    
  Stream := FDecompressor;
  
  // Setup output buffer
  Stream^.next_out := FBuffer;
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
    ret := _lzma_code(Stream^, LZMA_RUN);
    
    if ret = LZMA_STREAM_END then
    begin
      FFinished := True;
      Break;
    end
    else if ret <> LZMA_OK then
    begin
      raise EXZError.CreateFmt('LZMA decompression failed: error %d', [Integer(ret)]);
    end;
  end;
  
  Result := Integer(FBufferSize) - Integer(Stream^.avail_out);
end;

function TXZInputStream.Read(var Buffer; Count: Longint): Longint;
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

function TXZInputStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EXZError.Create('Cannot write to XZ input stream');
end;

function TXZInputStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  raise EXZError.Create('Cannot seek in XZ compressed stream');
end;

function TXZInputStream.GetSize: Int64;
begin
  Result := -1; // Unknown size for compressed stream
end;

procedure LoadLzmaDLL;
var
  DLLPath: string;
  ExePath: string;
  LibPath: string;
  LastError: DWORD;
  ErrorMsg: array[0..1023] of Char;
begin
  if LzmaHandle <> 0 then
    Exit;
    
  try
    ExtractLzmaDLL;
  except
    // Continue, DLL might already exist
  end;
  
  // Try AppData location first
  LastError := 0;
  DLLPath := GetLibraryPath + 'liblzma.dll';
  if FileExists(DLLPath) then
  begin
    LzmaHandle := LoadLibrary(PChar(DLLPath));
    if LzmaHandle = 0 then
      LastError := GetLastError;
  end;
  
  // If not found, try exe directory
  if LzmaHandle = 0 then
  begin
    ExePath := ExtractFilePath(ParamStr(0)) + 'liblzma.dll';
    if FileExists(ExePath) then
    begin
      LzmaHandle := LoadLibrary(PChar(ExePath));
      if LzmaHandle = 0 then
        LastError := GetLastError;
    end;
  end;
  
  // Try lib/ subdirectory
  if LzmaHandle = 0 then
  begin
    LibPath := ExtractFilePath(ParamStr(0)) + 'lib\liblzma.dll';
    if FileExists(LibPath) then
    begin
      LzmaHandle := LoadLibrary(PChar(LibPath));
      if LzmaHandle = 0 then
        LastError := GetLastError;
    end;
  end;
  
  if LzmaHandle = 0 then
  begin
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, LastError, 0, 
                  ErrorMsg, SizeOf(ErrorMsg), nil);
    raise EXZError.CreateFmt(
      'Failed to load liblzma.dll. Tried:' + #13#10 +
      '  1. %s' + #13#10 +
      '  2. %s' + #13#10 +
      '  3. %s' + #13#10 +
      'Windows Error %d: %s' + #13#10 +
      'To enable XZ support, download liblzma.dll from:' + #13#10 +
      'https://tukaani.org/xz/',
      [DLLPath, ExePath, LibPath, LastError, Trim(string(ErrorMsg))]);
  end;
    
  @_lzma_stream_decoder := GetProcAddress(LzmaHandle, 'lzma_stream_decoder');
  @_lzma_code := GetProcAddress(LzmaHandle, 'lzma_code');
  @_lzma_end := GetProcAddress(LzmaHandle, 'lzma_end');
  
  if not Assigned(_lzma_stream_decoder) then
    raise EXZError.Create('Failed to load lzma_stream_decoder from liblzma.dll');
  if not Assigned(_lzma_code) then
    raise EXZError.Create('Failed to load lzma_code from liblzma.dll');
  if not Assigned(_lzma_end) then
    raise EXZError.Create('Failed to load lzma_end from liblzma.dll');
end;

end.
