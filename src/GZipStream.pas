{******************************************************************************}
{                                                                              }
{  ImageWriter - GZip Stream Handling Unit                                    }
{                                                                              }
{  Copyright (c) 2024-2025 Anton Zelenov <tixset@gmail.com>                   }
{  GitHub: https://github.com/tixset/ImageWriter                              }
{  Based on dd for Windows by John Newbigin (http://chrysocome.net/dd)        }
{                                                                              }
{  This program is free software: you can redistribute it and/or modify       }
{  it under the terms of the GNU General Public License as published by       }
{  the Free Software Foundation, either version 3 of the License, or          }
{  (at your option) any later version.                                        }
{                                                                              }
{  Description:                                                                }
{    Stream classes for reading and writing GZIP and ZIP compressed files.    }
{    Provides transparent compression/decompression during I/O operations.    }
{                                                                              }
{******************************************************************************}

unit GZipStream;

interface

uses
  Windows, SysUtils, Classes, ZLib;

type
  // Wrapper for reading GZIP files as if they were regular files
  TGZipInputStream = class(TStream)
  private
    FSourceFile: TFileStream;
    FDecompressor: TZDecompressionStream;
    FTotalBytesRead: Int64;
  public
    constructor Create(SourceStream: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property BytesRead: Int64 read FTotalBytesRead;
  end;

  // Wrapper for writing GZIP files as if they were regular files  
  TGZipOutputStream = class(TStream)
  private
    FTargetFile: TFileStream;
    FCompressor: TZCompressionStream;
    FTotalBytesWritten: Int64;
  public
    constructor Create(TargetStream: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property BytesWritten: Int64 read FTotalBytesWritten;
  end;

  // Wrapper for reading raw DEFLATE data from ZIP files
  TZipDeflateStream = class(TStream)
  private
    FSourceStream: TStream;
    FZStream: TZStreamRec;
    FBuffer: array[0..65535] of Byte;
    FInitialized: Boolean;
  public
    constructor Create(SourceStream: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  // Wrapper for reading ZIP files as if they were regular files
  TZipInputStream = class(TStream)
  private
    FSourceFile: TFileStream;
    FDecompressor: TZipDeflateStream;
    FTotalBytesRead: Int64;
    FUncompressedSize: Int64;
    FCompressedSize: Int64;
    FDataStartPos: Int64;
    FIsStored: Boolean;
  public
    constructor Create(SourceStream: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property BytesRead: Int64 read FTotalBytesRead;
    property UncompressedSize: Int64 read FUncompressedSize;
  end;

  // Wrapper for writing ZIP files as if they were regular files
  TZipOutputStream = class(TStream)
  private
    FTargetFile: TFileStream;
    FCompressor: TZCompressionStream;
    FTotalBytesWritten: Int64;
    FStartPos: Int64;
    FCrc32: DWORD;
    FUncompressedSize: DWORD;
    procedure UpdateCRC32(const Buffer; Count: Integer);
    procedure WriteZipHeader(const FileName: string);
    procedure WriteZipFooter;
  public
    constructor Create(TargetStream: TStream; const FileName: string);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property BytesWritten: Int64 read FTotalBytesWritten;
  end;

function GetZipComment(const FileName: string): string;
function GetGZipComment(const FileName: string): string;

implementation

const
  Z_NO_FLUSH = 0;
  Z_OK = 0;
  Z_STREAM_END = 1;

{ TGZipInputStream }

constructor TGZipInputStream.Create(SourceStream: TStream);
var
  gzHeader: array[0..9] of Byte;
  flags: Byte;
  skipBytes: Word;
  temp: Byte;
begin
  inherited Create;
  
  // Ensure ZLib is available before using it
  EnsureZLibAvailable;
  
  FTotalBytesRead := 0;
  
  FSourceFile := SourceStream as TFileStream;
  
  // Read and validate GZIP header
  if FSourceFile.Read(gzHeader, 10) <> 10 then
    raise Exception.Create('Invalid GZIP file: too small');
    
  if (gzHeader[0] <> $1F) or (gzHeader[1] <> $8B) then
    raise Exception.Create('Invalid GZIP magic number');
    
  if gzHeader[2] <> $08 then
    raise Exception.Create('Unsupported compression method');
    
  flags := gzHeader[3];
  
  // Skip extra fields if present
  if (flags and $04) <> 0 then
  begin
    FSourceFile.Read(skipBytes, 2);
    FSourceFile.Seek(skipBytes, soFromCurrent);
  end;
  
  // Skip filename if present
  if (flags and $08) <> 0 then
  begin
    repeat
      FSourceFile.Read(temp, 1);
    until temp = 0;
  end;
  
  // Skip comment if present
  if (flags and $10) <> 0 then
  begin
    repeat
      FSourceFile.Read(temp, 1);
    until temp = 0;
  end;
  
  // Skip CRC16 if present
  if (flags and $02) <> 0 then
    FSourceFile.Seek(2, soFromCurrent);
    
  // Create decompressor starting from current position
  FDecompressor := TZDecompressionStream.Create(FSourceFile);
end;

destructor TGZipInputStream.Destroy;
begin
  FDecompressor.Free;
  FSourceFile.Free;
  inherited;
end;

function TGZipInputStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FDecompressor.Read(Buffer, Count);
  Inc(FTotalBytesRead, Result);
end;

function TGZipInputStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise Exception.Create('Cannot write to input stream');
end;

function TGZipInputStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  // GZIP streams don't support seeking
  raise Exception.Create('GZIP streams do not support seeking');
end;

{ TGZipOutputStream }

constructor TGZipOutputStream.Create(TargetStream: TStream);
var
  gzHeader: array[0..9] of Byte;
begin
  inherited Create;
  
  // Ensure ZLib is available before using it
  EnsureZLibAvailable;
  
  FTotalBytesWritten := 0;
  
  FTargetFile := TargetStream as TFileStream;
  
  // Write GZIP header
  gzHeader[0] := $1F; 
  gzHeader[1] := $8B;  // Magic
  gzHeader[2] := $08;  // Compression method: deflate
  gzHeader[3] := $00;  // Flags: no extras
  gzHeader[4] := $00; gzHeader[5] := $00; gzHeader[6] := $00; gzHeader[7] := $00;  // Modification time
  gzHeader[8] := $00;  // Extra flags
  gzHeader[9] := $FF;  // OS: unknown
  
  FTargetFile.Write(gzHeader, 10);
  
  // Create compressor
  FCompressor := TZCompressionStream.Create(FTargetFile, zcMax);
end;

destructor TGZipOutputStream.Destroy;
begin
  FCompressor.Free;
  FTargetFile.Free;
  inherited;
end;

function TGZipOutputStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise Exception.Create('Cannot read from output stream');
end;

function TGZipOutputStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FCompressor.Write(Buffer, Count);
  Inc(FTotalBytesWritten, Result);
end;

function TGZipOutputStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  // GZIP streams don't support seeking
  raise Exception.Create('GZIP streams do not support seeking');
end;

{ TZipDeflateStream }

constructor TZipDeflateStream.Create(SourceStream: TStream);
var
  result: Integer;
begin
  inherited Create;
  
  // Ensure ZLib is available before using it
  EnsureZLibAvailable;
  
  FSourceStream := SourceStream;
  FInitialized := False;
  
  // Initialize zlib stream structure
  FillChar(FZStream, SizeOf(FZStream), 0);
  FZStream.zalloc := nil;
  FZStream.zfree := nil;
  FZStream.opaque := nil;
  FZStream.next_in := nil;
  FZStream.avail_in := 0;
  
  // Use InflateInit2 with negative windowBits for raw DEFLATE (no zlib/gzip headers)
  // -15 means: 15 bit window, raw deflate (no headers)
  result := InflateInit2(FZStream, -15);
  if result <> Z_OK then
    raise Exception.Create('Failed to initialize ZIP deflate decompressor: ' + IntToStr(result));
    
  FInitialized := True;
end;

destructor TZipDeflateStream.Destroy;
begin
  if FInitialized then
    inflateEnd(FZStream);
  inherited;
end;

function TZipDeflateStream.Read(var Buffer; Count: Longint): Longint;
var
  bytesRead: Integer;
  zresult: Integer;
begin
  Result := 0;
  bytesRead := 0; // Initialize to avoid hint
  
  FZStream.next_out := @Buffer;
  FZStream.avail_out := Count;
  
  while FZStream.avail_out > 0 do
  begin
    // If input buffer is empty, refill it
    if FZStream.avail_in = 0 then
    begin
      bytesRead := FSourceStream.Read(FBuffer, SizeOf(FBuffer));
      if bytesRead <= 0 then
        Break; // End of input stream
        
      FZStream.next_in := @FBuffer[0];
      FZStream.avail_in := bytesRead;
    end;
    
    // Decompress
    zresult := inflate(FZStream, Z_NO_FLUSH);
    
    if zresult = Z_STREAM_END then
      Break; // Decompression complete
      
    if (zresult <> Z_OK) and (zresult <> Z_STREAM_END) then
      raise Exception.Create('ZIP decompression error: ' + IntToStr(zresult));
  end;
  
  Result := Count - FZStream.avail_out;
end;

function TZipDeflateStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise Exception.Create('Cannot write to ZIP deflate input stream');
end;

function TZipDeflateStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  raise Exception.Create('ZIP deflate streams do not support seeking');
end;

{ TZipInputStream }

constructor TZipInputStream.Create(SourceStream: TStream);
var
  signature: DWORD;
  version, flags, method: WORD;
  modTime, modDate: WORD;
  crc32: DWORD;
  compSize, uncompSize: DWORD;
  fileNameLen, extraLen: WORD;
  fileName: AnsiString;
  hasDataDescriptor: Boolean;
begin
  inherited Create;
  
  // Ensure ZLib is available before using it
  EnsureZLibAvailable;
  
  FTotalBytesRead := 0;
  FUncompressedSize := 0;
  FCompressedSize := 0;
  
  FSourceFile := SourceStream as TFileStream;
  
  // Read ZIP local file header (PK\x03\x04)
  FSourceFile.Read(signature, 4);
  if signature <> $04034b50 then
    raise Exception.Create('Invalid ZIP file signature');
    
  FSourceFile.Read(version, 2);  // Version needed to extract
  FSourceFile.Read(flags, 2);     // General purpose bit flag
  FSourceFile.Read(method, 2);    // Compression method
  
  // Check if method is supported
  if (method <> 0) and (method <> 8) then  // 0=stored, 8=DEFLATE
    raise Exception.Create('Only STORED and DEFLATE compression methods supported (method=' + IntToStr(method) + ')');
    
  FSourceFile.Read(modTime, 2);   // Last mod file time
  FSourceFile.Read(modDate, 2);   // Last mod file date
  FSourceFile.Read(crc32, 4);     // CRC-32
  FSourceFile.Read(compSize, 4);   // Compressed size
  FSourceFile.Read(uncompSize, 4); // Uncompressed size
  FSourceFile.Read(fileNameLen, 2);       // File name length
  FSourceFile.Read(extraLen, 2);          // Extra field length
  
  // Check for data descriptor flag (bit 3)
  hasDataDescriptor := (flags and $0008) <> 0;
  
  if hasDataDescriptor then
  begin
    // Sizes are in data descriptor after compressed data
    // Try to find them in central directory or estimate
    FUncompressedSize := 0;  // Unknown, will read until end
    FCompressedSize := 0;
  end
  else
  begin
    FUncompressedSize := uncompSize;
    FCompressedSize := compSize;
  end;
  
  // Skip filename
  if fileNameLen > 0 then
  begin
    SetLength(fileName, fileNameLen);
    FSourceFile.Read(fileName[1], fileNameLen);
  end;
  
  // Skip extra field
  if extraLen > 0 then
    FSourceFile.Seek(extraLen, soFromCurrent);
    
  FDataStartPos := FSourceFile.Position;
  
  FIsStored := (method = 0);
    
  // Now positioned at compressed data - create decompressor
  if not FIsStored then
    FDecompressor := TZipDeflateStream.Create(FSourceFile)  // DEFLATE
  else
    FDecompressor := nil;  // Stored (uncompressed)
end;

destructor TZipInputStream.Destroy;
begin
  if Assigned(FDecompressor) then
    FDecompressor.Free;
  FSourceFile.Free;
  inherited;
end;

function TZipInputStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Assigned(FDecompressor) then
  begin
    // Compressed data - use decompressor
    Result := FDecompressor.Read(Buffer, Count);
  end
  else
  begin
    // Stored (uncompressed) data - read directly
    Result := FSourceFile.Read(Buffer, Count);
  end;
  Inc(FTotalBytesRead, Result);
end;

function TZipInputStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise Exception.Create('Cannot write to input stream');
end;

function TZipInputStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  // ZIP streams don't support seeking
  raise Exception.Create('ZIP streams do not support seeking');
end;

{ TZipOutputStream }

const
  CRC32_TABLE: array[0..255] of DWORD = (
    $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535, $9E6495A3,
    $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
    $1DB71064, $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
    $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
    $3B6E20C8, $4C69105E, $D56041E4, $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
    $26D930AC, $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924, $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,
    $76DC4190, $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
    $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E, $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
    $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
    $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
    $4369E96A, $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,
    $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
    $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8, $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
    $F00F9344, $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB, $196C3671, $6E6B06E7,
    $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
    $CB61B38C, $BC66831A, $256FD2A0, $5268E236, $CC0C7795, $BB0B4703, $220216B9, $5505262F,
    $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,
    $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9, $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
    $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
    $88085AE6, $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7, $4969474D, $3E6E77DB,
    $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693, $54DE5729, $23D967BF,
    $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D
  );

procedure TZipOutputStream.UpdateCRC32(const Buffer; Count: Integer);
var
  i: Integer;
  p: PByte;
begin
  p := @Buffer;
  for i := 0 to Count - 1 do
  begin
    FCrc32 := CRC32_TABLE[(FCrc32 xor p^) and $FF] xor (FCrc32 shr 8);
    Inc(p);
  end;
end;

procedure TZipOutputStream.WriteZipHeader(const FileName: string);
var
  signature: DWORD;
  version, flags, method: WORD;
  modTime, modDate: WORD;
  crc32, compSize, uncompSize: DWORD;
  fileNameLen, extraLen: WORD;
  fileNameAnsi: AnsiString;
begin
  // Local file header signature
  signature := $04034b50;  // PK\x03\x04
  FTargetFile.Write(signature, 4);
  
  version := 20;  // Version 2.0
  FTargetFile.Write(version, 2);
  
  flags := $0000;  // No flags
  FTargetFile.Write(flags, 2);
  
  method := 8;  // DEFLATE
  FTargetFile.Write(method, 2);
  
  // DOS time/date (use current time)
  modTime := 0;
  modDate := 0;
  FTargetFile.Write(modTime, 2);
  FTargetFile.Write(modDate, 2);
  
  // CRC-32, compressed size, uncompressed size (will be filled later)
  crc32 := 0;
  compSize := 0;
  uncompSize := 0;
  FTargetFile.Write(crc32, 4);
  FTargetFile.Write(compSize, 4);
  FTargetFile.Write(uncompSize, 4);
  
  // Filename
  fileNameAnsi := AnsiString(FileName);
  fileNameLen := Length(fileNameAnsi);
  extraLen := 0;
  FTargetFile.Write(fileNameLen, 2);
  FTargetFile.Write(extraLen, 2);
  
  if fileNameLen > 0 then
    FTargetFile.Write(fileNameAnsi[1], fileNameLen);
    
  FStartPos := FTargetFile.Position;
end;

procedure TZipOutputStream.WriteZipFooter;
var
  signature: DWORD;
  dirSize, dirOffset: DWORD;
  compSize: DWORD;
  localHeaderPos: Int64;
  w0, w1, w8, w20: WORD;
  d0: DWORD;
  fileName: array[0..7] of AnsiChar;
begin
  // Get compressed size
  compSize := DWORD(FTargetFile.Position - FStartPos);
  
  // Update local header with actual CRC and sizes
  localHeaderPos := FTargetFile.Position;
  FTargetFile.Seek(14, soFromBeginning);  // Position to CRC field
  FTargetFile.Write(FCrc32, 4);
  FTargetFile.Write(compSize, 4);
  FTargetFile.Write(FUncompressedSize, 4);
  FTargetFile.Seek(localHeaderPos, soFromBeginning);
  
  // Central directory file header (simplified - single file)
  signature := $02014b50;  // PK\x01\x02
  FTargetFile.Write(signature, 4);
  
  // Version made by, version needed, flags, method
  w20 := 20;
  w0 := $0000;
  w8 := 8;
  FTargetFile.Write(w20, 2);
  FTargetFile.Write(w20, 2);
  FTargetFile.Write(w0, 2);
  FTargetFile.Write(w8, 2);
  
  // Time, date, CRC, sizes
  w0 := 0;
  FTargetFile.Write(w0, 2);
  FTargetFile.Write(w0, 2);
  FTargetFile.Write(FCrc32, 4);
  FTargetFile.Write(compSize, 4);
  FTargetFile.Write(FUncompressedSize, 4);
  
  // Filename length, extra, comment, disk start, internal attr, external attr, local header offset
  w8 := 8;
  w0 := 0;
  d0 := 0;
  FTargetFile.Write(w8, 2);  // filename length for "disk.img"
  FTargetFile.Write(w0, 2);
  FTargetFile.Write(w0, 2);
  FTargetFile.Write(w0, 2);
  FTargetFile.Write(w0, 2);
  FTargetFile.Write(d0, 4);
  FTargetFile.Write(d0, 4);
  
  fileName := 'disk.img';
  FTargetFile.Write(fileName, 8);
  
  dirOffset := DWORD(FTargetFile.Position);
  
  // End of central directory
  signature := $06054b50;  // PK\x05\x06
  FTargetFile.Write(signature, 4);
  w0 := 0;
  w1 := 1;
  FTargetFile.Write(w0, 2);  // Disk number
  FTargetFile.Write(w0, 2);  // Start disk
  FTargetFile.Write(w1, 2);  // Entries on this disk
  FTargetFile.Write(w1, 2);  // Total entries
  dirSize := DWORD(FTargetFile.Position - dirOffset - 4);
  FTargetFile.Write(dirSize, 4);
  FTargetFile.Write(dirOffset, 4);
  w0 := 0;
  FTargetFile.Write(w0, 2);  // Comment length
end;

constructor TZipOutputStream.Create(TargetStream: TStream; const FileName: string);
begin
  inherited Create;
  
  // Ensure ZLib is available before using it
  EnsureZLibAvailable;
  
  FTotalBytesWritten := 0;
  FCrc32 := $FFFFFFFF;  // Initial CRC value
  FUncompressedSize := 0;
  
  FTargetFile := TargetStream as TFileStream;
  
  // Write ZIP local file header
  WriteZipHeader(FileName);
  
  // Create compressor for DEFLATE
  FCompressor := TZCompressionStream.Create(FTargetFile, zcMax);
end;

destructor TZipOutputStream.Destroy;
begin
  if Assigned(FCompressor) then
  begin
    FCompressor.Free;
    FCrc32 := FCrc32 xor $FFFFFFFF;  // Finalize CRC
    WriteZipFooter;
  end;
  FTargetFile.Free;
  inherited;
end;

function TZipOutputStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise Exception.Create('Cannot read from output stream');
end;

function TZipOutputStream.Write(const Buffer; Count: Longint): Longint;
begin
  UpdateCRC32(Buffer, Count);
  Result := FCompressor.Write(Buffer, Count);
  Inc(FTotalBytesWritten, Result);
  Inc(FUncompressedSize, Count);
end;

function TZipOutputStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  // ZIP streams don't support seeking
  raise Exception.Create('ZIP streams do not support seeking');
end;

// Extract ZIP comment from archive file
function GetZipComment(const FileName: string): string;
var
  F: TFileStream;
  signature: DWORD;
  commentLen: WORD;
  comment: AnsiString;
  fileSize: Int64;
  searchPos: Integer;
  maxSearch: Integer;
  startSearch: Integer;
begin
  Result := '';
  if not FileExists(FileName) then Exit;
  
  try
    F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      fileSize := F.Size;
      if fileSize < 22 then Exit; // Minimum ZIP size
      
      // Search for End of Central Directory signature from end of file
      // Maximum comment length is 65535 bytes
      maxSearch := 65535 + 22;
      if maxSearch > fileSize then
        maxSearch := fileSize;
      
      startSearch := fileSize - maxSearch;
      if startSearch < 0 then
        startSearch := 0;
        
      searchPos := fileSize - 22;
      while searchPos >= startSearch do
      begin
        F.Position := searchPos;
        F.Read(signature, 4);
        
        if signature = $06054b50 then // PK\x05\x06 - End of central directory
        begin
          F.Seek(searchPos + 20, soFromBeginning); // Skip to comment length field
          F.Read(commentLen, 2);
          
          if commentLen > 0 then
          begin
            SetLength(comment, commentLen);
            F.Read(comment[1], commentLen);
            Result := string(comment);
          end;
          Break;
        end;
        Dec(searchPos);
      end;
    finally
      F.Free;
    end;
  except
    Result := '';
  end;
end;

// Extract GZIP comment from archive file
function GetGZipComment(const FileName: string): string;
var
  F: TFileStream;
  magic: WORD;
  method, flags: Byte;
  mtime: DWORD;
  xflags, os: Byte;
  commentLen: Integer;
  comment: AnsiString;
  b: Byte;
begin
  Result := '';
  if not FileExists(FileName) then Exit;
  
  try
    F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      if F.Size < 10 then Exit;
      
      // Read GZIP header
      F.Read(magic, 2);
      if magic <> $8B1F then Exit; // Not a GZIP file
      
      F.Read(method, 1);
      F.Read(flags, 1);
      F.Read(mtime, 4);
      F.Read(xflags, 1);
      F.Read(os, 1);
      
      // Check if FCOMMENT flag is set (bit 4)
      if (flags and $10) = 0 then Exit;
      
      // Skip FEXTRA if present (bit 2)
      if (flags and $04) <> 0 then
      begin
        F.Read(commentLen, 2);
        F.Seek(commentLen, soFromCurrent);
      end;
      
      // Skip FNAME if present (bit 3)
      if (flags and $08) <> 0 then
      begin
        repeat
          F.Read(b, 1);
        until (b = 0) or (F.Position >= F.Size);
      end;
      
      // Read comment (null-terminated string)
      if (flags and $10) <> 0 then
      begin
        comment := '';
        repeat
          F.Read(b, 1);
          if b <> 0 then
            comment := comment + AnsiChar(b);
        until (b = 0) or (F.Position >= F.Size);
        Result := string(comment);
      end;
    finally
      F.Free;
    end;
  except
    Result := '';
  end;
end;

end.
