unit TarStream;

{$I+}  { Enable I/O checking }

interface

uses
  Windows, Classes, SysUtils;

type
  /// <summary>
  /// Exception class for TAR extraction errors
  /// </summary>
  ETarError = class(Exception);

  /// <summary>
  /// TAR header structure (POSIX ustar format)
  /// </summary>
  {$A1}  // Byte alignment
  TTarHeader = packed record
    Name: array[0..99] of AnsiChar;      // File name
    Mode: array[0..7] of AnsiChar;       // File mode (octal)
    UID: array[0..7] of AnsiChar;        // Owner user ID (octal)
    GID: array[0..7] of AnsiChar;        // Owner group ID (octal)
    Size: array[0..11] of AnsiChar;      // File size in bytes (octal)
    MTime: array[0..11] of AnsiChar;     // Modification time (octal)
    ChkSum: array[0..7] of AnsiChar;     // Checksum (octal)
    TypeFlag: AnsiChar;                  // File type
    LinkName: array[0..99] of AnsiChar;  // Linked file name
    Magic: array[0..5] of AnsiChar;      // "ustar" magic
    Version: array[0..1] of AnsiChar;    // Version "00"
    UName: array[0..31] of AnsiChar;     // Owner user name
    GName: array[0..31] of AnsiChar;     // Owner group name
    DevMajor: array[0..7] of AnsiChar;   // Device major number
    DevMinor: array[0..7] of AnsiChar;   // Device minor number
    Prefix: array[0..154] of AnsiChar;   // Filename prefix
    Padding: array[0..11] of AnsiChar;   // Padding to 512 bytes
  end;
  {$A+}

  /// <summary>
  /// Streaming TAR extractor implementing TStream interface
  /// Extracts first file from TAR archive for disk image writing
  /// </summary>
  /// <remarks>
  /// Supports POSIX ustar format. Reads 512-byte headers.
  /// Only extracts first regular file (skips directories, symlinks).
  /// </remarks>
  TTarInputStream = class(TStream)
  private
    FSourceStream: TStream;
    FOwnsStream: Boolean;
    FFileSize: Int64;
    FBytesRead: Int64;
    FHeaderRead: Boolean;
    FFinished: Boolean;
    
    function ParseOctal(const OctalStr: array of AnsiChar; MaxLen: Integer): Int64;
    function ReadHeader: Boolean;
    procedure SkipToNextBlock;
  protected
    function GetSize: Int64; override;
  public
    /// <summary>
    /// Creates TAR input stream from source stream
    /// </summary>
    /// <param name="ASource">Source stream with TAR data</param>
    /// <param name="AOwnsStream">If True, source stream will be freed on destruction</param>
    constructor Create(ASource: TStream; AOwnsStream: Boolean = False);
    destructor Destroy; override;
    
    /// <summary>
    /// Reads data from first file in TAR archive
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
    /// Seek not supported for TAR stream
    /// </summary>
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    
    /// <summary>
    /// Get extracted file size from TAR header
    /// </summary>
    property FileSize: Int64 read FFileSize;
  end;

implementation

const
  TAR_BLOCK_SIZE = 512;
  TAR_MAGIC = 'ustar';
  
  // TAR file type flags
  TAR_TYPE_REGULAR = '0';
  TAR_TYPE_REGULAR_ALT = #0;
  TAR_TYPE_DIRECTORY = '5';

{ TTarInputStream }

constructor TTarInputStream.Create(ASource: TStream; AOwnsStream: Boolean);
begin
  inherited Create;
  FSourceStream := ASource;
  FOwnsStream := AOwnsStream;
  FFileSize := 0;
  FBytesRead := 0;
  FHeaderRead := False;
  FFinished := False;
  
  // Read header on creation
  if not ReadHeader then
    raise ETarError.Create('Failed to read TAR header or no regular file found');
end;

destructor TTarInputStream.Destroy;
begin
  if FOwnsStream then
    FSourceStream.Free;
  inherited;
end;

function TTarInputStream.ParseOctal(const OctalStr: array of AnsiChar; MaxLen: Integer): Int64;
var
  I: Integer;
  S: string;
begin
  Result := 0;
  S := '';
  
  // Extract string, stop at space or null
  for I := 0 to MaxLen - 1 do
  begin
    if (OctalStr[I] = ' ') or (OctalStr[I] = #0) then
      Break;
    S := S + OctalStr[I];
  end;
  
  // Convert octal string to integer
  for I := 1 to Length(S) do
  begin
    if (S[I] >= '0') and (S[I] <= '7') then
      Result := Result * 8 + (Ord(S[I]) - Ord('0'))
    else
      Break;
  end;
end;

function TTarInputStream.ReadHeader: Boolean;
var
  Header: TTarHeader;
  BytesRead: Integer;
  FileName: string;
  I: Integer;
begin
  Result := False;
  
  // Read TAR headers until we find a regular file
  while True do
  begin
    FillChar(Header, SizeOf(Header), 0);
    BytesRead := FSourceStream.Read(Header, SizeOf(Header));
    
    if BytesRead <> SizeOf(Header) then
      Exit; // End of stream
      
    // Check for end of archive (all zeros)
    if Header.Name[0] = #0 then
      Exit;
      
    // Verify magic signature
    if (Header.Magic[0] <> 'u') or (Header.Magic[1] <> 's') or 
       (Header.Magic[2] <> 't') or (Header.Magic[3] <> 'a') or
       (Header.Magic[4] <> 'r') then
      raise ETarError.Create('Invalid TAR header: bad magic signature');
      
    // Extract filename
    FileName := '';
    for I := 0 to 99 do
    begin
      if Header.Name[I] = #0 then
        Break;
      FileName := FileName + Header.Name[I];
    end;
    
    // Parse file size
    FFileSize := ParseOctal(Header.Size, 12);
    
    // Check if this is a regular file
    if (Header.TypeFlag = TAR_TYPE_REGULAR) or (Header.TypeFlag = TAR_TYPE_REGULAR_ALT) then
    begin
      // Found a regular file, we're done
      FHeaderRead := True;
      Result := True;
      Exit;
    end
    else
    begin
      // Skip this entry (directory, symlink, etc.)
      SkipToNextBlock;
    end;
  end;
end;

procedure TTarInputStream.SkipToNextBlock;
var
  SkipSize: Int64;
  Buffer: array[0..511] of Byte;
  ToRead: Integer;
begin
  // Calculate size to skip (rounded up to TAR_BLOCK_SIZE)
  SkipSize := FFileSize;
  if (SkipSize mod TAR_BLOCK_SIZE) <> 0 then
    SkipSize := ((SkipSize div TAR_BLOCK_SIZE) + 1) * TAR_BLOCK_SIZE;
    
  // Skip data
  while SkipSize > 0 do
  begin
    if SkipSize > SizeOf(Buffer) then
      ToRead := SizeOf(Buffer)
    else
      ToRead := SkipSize;
      
    FSourceStream.Read(Buffer, ToRead);
    Dec(SkipSize, ToRead);
  end;
end;

function TTarInputStream.Read(var Buffer; Count: Longint): Longint;
var
  BytesToRead: Int64;
begin
  Result := 0;
  
  if FFinished or not FHeaderRead then
    Exit;
    
  // Don't read beyond file size
  BytesToRead := Count;
  if FBytesRead + BytesToRead > FFileSize then
    BytesToRead := FFileSize - FBytesRead;
    
  if BytesToRead <= 0 then
  begin
    FFinished := True;
    Exit;
  end;
  
  // Read from source stream
  Result := FSourceStream.Read(Buffer, BytesToRead);
  Inc(FBytesRead, Result);
  
  if FBytesRead >= FFileSize then
    FFinished := True;
end;

function TTarInputStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise ETarError.Create('Cannot write to TAR input stream');
end;

function TTarInputStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  raise ETarError.Create('Cannot seek in TAR stream');
end;

function TTarInputStream.GetSize: Int64;
begin
  Result := FFileSize;
end;

end.
