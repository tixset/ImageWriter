{******************************************************************************}
{  ZLib.pas - ZLib compression library wrapper with dynamic loading           }
{                                                                              }
{  Original copyright (c) 2000 base2 technologies                             }
{  Original copyright (c) 1997 Borland International                          }
{  Modified for ImageWriter by Anton Zelenov <tixset@gmail.com> (2024-2025)   }
{                                                                              }
{  This program is free software: you can redistribute it and/or modify       }
{  it under the terms of the GNU General Public License as published by       }
{  the Free Software Foundation, either version 3 of the License, or          }
{  (at your option) any later version.                                        }
{                                                                              }
{  Description:                                                                }
{    Wrapper for ZLib compression/decompression functions with dynamic        }
{    DLL loading. Loads zlib1.dll on-demand when compression is needed.       }
{                                                                              }
{  Revision history:                                                           }
{    2024-12-20  Modified to use dynamic DLL loading instead of static        }
{                imports. Loads zlib1.dll only when needed.                   }
{    2000-06-13  Optimized, fixed, rewrote, and enhanced the zlib.pas unit    }
{                included on the delphi cd (zlib version 1.1.3)               }
{******************************************************************************}

unit ZLib;
// $Header: /home/cso/jnewbigin/cvsroot/rawwrite/Zlib.pas,v 1.2 2005/12/05 08:26:02 jnewbigin Exp $

{$IFDEF FPC}
{$MODE Delphi}
{$LINKLIB z}
{$ENDIF}

{*
  Behavior note: by default on Windows we attempt to use the system zlib DLL
  (`zlib1.dll`) so that the unit builds even if `zlib\*.obj` files are not
  present. If you prefer to link the legacy object modules, define
  `ZLIB_USE_OBJ` in your project (for example: -dZLIB_USE_OBJ).
*}
{$IFDEF WIN32}
  {$IFNDEF ZLIB_USE_OBJ}
    {$DEFINE ZLIB_USE_DLL}
  {$ENDIF}
{$ENDIF}

interface

uses
  Sysutils, Classes;

const
  ZLIB_VERSION = '1.1.3';

type
  TZAlloc = function (opaque: Pointer; items, size: Integer): Pointer;
  TZFree  = procedure (opaque, block: Pointer);

  TZCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax);

  {** TZStreamRec ***********************************************************}

  TZStreamRec = packed record
    next_in  : PChar;     // next input byte
    avail_in : Longint;   // number of bytes available at next_in
    total_in : Longint;   // total nb of input bytes read so far

    next_out : PChar;     // next output byte should be put here
    avail_out: Longint;   // remaining free space at next_out
    total_out: Longint;   // total nb of bytes output so far

    msg      : PChar;     // last error message, NULL if no error
    state    : Pointer;   // not visible by applications

    zalloc   : TZAlloc;   // used to allocate the internal state
    zfree    : TZFree;    // used to free the internal state
    opaque   : Pointer;   // private data object passed to zalloc and zfree

    data_type: Integer;   // best guess about the data type: ascii or binary
    adler    : Longint;   // adler32 value of the uncompressed data
    reserved : Longint;   // reserved for future use
  end;

  {** TCustomZStream ********************************************************}

  TCustomZStream = class(TStream)
  private
    FStream    : TStream;
    FStreamPos : Integer;
    FOnProgress: TNotifyEvent;

    FZStream   : TZStreamRec;
    FBuffer    : Array [Word] of Char;
  protected
    constructor Create(stream: TStream);

    procedure DoProgress; dynamic;

    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

  {** TZCompressionStream ***************************************************}

  TZCompressionStream = class(TCustomZStream)
  private
    function GetCompressionRate: Single;
  public
    constructor Create(dest: TStream; compressionLevel: TZCompressionLevel = zcDefault);
    destructor  Destroy; override;

    function  Read(var buffer; count: Longint): Longint; override;
    function  Write(const buffer; count: Longint): Longint; override;
    function  Seek(offset: Longint; origin: Word): Longint; override;

    property CompressionRate: Single read GetCompressionRate;
    property OnProgress;
  end;

  {** TZDecompressionStream *************************************************}

  TZDecompressionStream = class(TCustomZStream)
  public
    constructor Create(source: TStream);
    destructor  Destroy; override;

    function  Read(var buffer; count: Longint): Longint; override;
    function  Write(const buffer; count: Longint): Longint; override;
    function  Seek(offset: Longint; origin: Word): Longint; override;

    property OnProgress;
  end;

{** Low-level zlib functions ************************************************}

function InflateInit2(var stream: TZStreamRec; windowBits: Integer): Integer;
function inflate(var strm: TZStreamRec; flush: Integer): Integer;
function inflateEnd(var strm: TZStreamRec): Integer;

{** zlib public routines ****************************************************}
{$IFDEF FPC}
{$CALLING CDECL}
{$ENDIF}


{*****************************************************************************
*  ZCompress                                                                 *
*                                                                            *
*  pre-conditions                                                            *
*    inBuffer  = pointer to uncompressed data                                *
*    inSize    = size of inBuffer (bytes)                                    *
*    outBuffer = pointer (unallocated)                                       *
*    level     = compression level                                           *
*                                                                            *
*  post-conditions                                                           *
*    outBuffer = pointer to compressed data (allocated)                      *
*    outSize   = size of outBuffer (bytes)                                   *
*****************************************************************************}

procedure ZCompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer;
  level: TZCompressionLevel = zcDefault);

{*****************************************************************************
*  ZDecompress                                                               *
*                                                                            *
*  pre-conditions                                                            *
*    inBuffer    = pointer to compressed data                                *
*    inSize      = size of inBuffer (bytes)                                  *
*    outBuffer   = pointer (unallocated)                                     *
*    outEstimate = estimated size of uncompressed data (bytes)               *
*                                                                            *
*  post-conditions                                                           *
*    outBuffer = pointer to decompressed data (allocated)                    *
*    outSize   = size of outBuffer (bytes)                                   *
*****************************************************************************}

procedure ZDecompress(const inBuffer: Pointer; inSize: Integer;
 out outBuffer: Pointer; out outSize: Integer; outEstimate: Integer = 0);

{** string routines *********************************************************}

function ZCompressStr(const s: String; level: TZCompressionLevel = zcDefault): String;

function ZDecompressStr(const s: String): String;

{** Dynamic DLL loading ****************************************************}

procedure EnsureZLibAvailable;

type
  EZLibError = class(Exception);

  EZCompressionError = class(EZLibError);
  EZDecompressionError = class(EZLibError);

implementation

uses
  Windows, ExtractZLib, StrUtils;

{** link zlib code **********************************************************}

{$IFDEF WIN32}
{*
  The original unit links precompiled zlib object modules (zlib\*.obj).
  If you don't have those object files available (they are not included
  in this repo), you can compile/link against the system zlib DLL
  instead by defining the conditional symbol ZLIB_USE_DLL in your project
  (for example: -dZLIB_USE_DLL) or by setting it in the IDE project options.
*}
{$IFNDEF ZLIB_USE_DLL}
{$L zlib\deflate.obj}
{$L zlib\inflate.obj}
{$L zlib\infblock.obj}
{$L zlib\inftrees.obj}
{$L zlib\infcodes.obj}
{$L zlib\infutil.obj}
{$L zlib\inffast.obj}
{$L zlib\trees.obj}
{$L zlib\adler32.obj}
{$ELSE}
// Using zlib1.dll import; ensure zlib1.dll is available on PATH/working dir.
{$ENDIF}
{$ENDIF}

{*****************************************************************************
*  note: do not reorder the above -- doing so will result in external        *
*  functions being undefined                                                 *
*****************************************************************************}

const
  {** flush constants *******************************************************}

  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;

  {** return codes **********************************************************}

  Z_OK            = 0;
  Z_STREAM_END    = 1;
  Z_NEED_DICT     = 2;
  Z_ERRNO         = (-1);
  Z_STREAM_ERROR  = (-2);
  Z_DATA_ERROR    = (-3);
  Z_MEM_ERROR     = (-4);
  Z_BUF_ERROR     = (-5);
  Z_VERSION_ERROR = (-6);

  {** compression levels ****************************************************}

  Z_NO_COMPRESSION       =   0;
  Z_BEST_SPEED           =   1;
  Z_BEST_COMPRESSION     =   9;
  Z_DEFAULT_COMPRESSION  = (-1);

  {** compression strategies ************************************************}

  Z_FILTERED            = 1;
  Z_HUFFMAN_ONLY        = 2;
  Z_DEFAULT_STRATEGY    = 0;

  {** data types ************************************************************}

  Z_BINARY   = 0;
  Z_ASCII    = 1;
  Z_UNKNOWN  = 2;

  {** compression methods ***************************************************}

  Z_DEFLATED = 8;

  {** return code messages **************************************************}

  _z_errmsg: array[0..9] of PChar = (
    'need dictionary',      // Z_NEED_DICT      (2)
    'stream end',           // Z_STREAM_END     (1)
    '',                     // Z_OK             (0)
    'file error',           // Z_ERRNO          (-1)
    'stream error',         // Z_STREAM_ERROR   (-2)
    'data error',           // Z_DATA_ERROR     (-3)
    'insufficient memory',  // Z_MEM_ERROR      (-4)
    'buffer error',         // Z_BUF_ERROR      (-5)
    'incompatible version', // Z_VERSION_ERROR  (-6)
    ''
  );

  ZLevels: array [TZCompressionLevel] of Shortint = (
    Z_NO_COMPRESSION,
    Z_BEST_SPEED,
    Z_DEFAULT_COMPRESSION,
    Z_BEST_COMPRESSION
  );

  SZInvalid = 'Invalid ZStream operation!';

{** deflate routines ********************************************************}

{$IFDEF ZLIB_USE_DLL}
type
  T_deflateInit_ = function(var strm: TZStreamRec; level: Integer; version: PChar;
    recsize: Integer): Integer; cdecl;
  T_deflate = function(var strm: TZStreamRec; flush: Integer): Integer; cdecl;
  T_deflateEnd = function(var strm: TZStreamRec): Integer; cdecl;
  T_inflateInit_ = function(var strm: TZStreamRec; version: PChar;
    recsize: Integer): Integer; cdecl;
  T_inflateInit2_ = function(var strm: TZStreamRec; windowBits: Integer; version: PChar;
    recsize: Integer): Integer; cdecl;
  T_inflate = function(var strm: TZStreamRec; flush: Integer): Integer; cdecl;
  T_inflateEnd = function(var strm: TZStreamRec): Integer; cdecl;
  T_inflateReset = function(var strm: TZStreamRec): Integer; cdecl;

var
  ZLibHandle: HMODULE;
  _deflateInit_: T_deflateInit_;
  _deflate: T_deflate;
  _deflateEnd: T_deflateEnd;
  _inflateInit_: T_inflateInit_;
  _inflateInit2_: T_inflateInit2_;
  _inflate: T_inflate;
  _inflateEnd: T_inflateEnd;
  _inflateReset: T_inflateReset;

procedure LoadZLibDLL;
var
  DLLPath: string;
  ExePath: string;
  LastError: DWORD;
  ErrorMsg: array[0..1023] of Char;
begin
  LastError := 0; // Initialize to avoid potential use of uninitialized variable
  if ZLibHandle <> 0 then
    Exit;
    
  try
    ExtractZLibDLL;
  except
    on E: Exception do
      ; // Continue, DLL might already exist or be in exe directory
  end;
  
  // Try AppData location first
  DLLPath := GetZLibPath + 'zlib1.dll';
  if FileExists(DLLPath) then
  begin
    ZLibHandle := LoadLibrary(PChar(DLLPath));
    if ZLibHandle = 0 then
      LastError := GetLastError;
  end;
  
  // If not found, try exe directory
  if ZLibHandle = 0 then
  begin
    ExePath := ExtractFilePath(ParamStr(0)) + 'zlib1.dll';
    if FileExists(ExePath) then
    begin
      ZLibHandle := LoadLibrary(PChar(ExePath));
      if ZLibHandle = 0 then
        LastError := GetLastError;
    end;
  end;
  
  if ZLibHandle = 0 then
  begin
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, LastError, 0, 
                  ErrorMsg, SizeOf(ErrorMsg), nil);
    raise Exception.CreateFmt(
      'Failed to load zlib1.dll. Tried:%s' + #13#10 +
      '  1. %s%s' + #13#10 +
      '  2. %s%s' + #13#10 +
      'Windows Error %d: %s' + #13#10 +
      'Common causes:%s' + #13#10 +
      '  - DLL architecture mismatch (x86 vs x64)%s' + #13#10 +
      '  - Missing MSVC runtime (vcruntime140.dll)%s' + #13#10 +
      '  - File access denied (antivirus/permissions)',
      [#13#10, DLLPath, IfThen(FileExists(DLLPath), ' (exists)', ' (not found)'),
       ExePath, IfThen(FileExists(ExePath), ' (exists)', ' (not found)'),
       LastError, Trim(string(ErrorMsg)), #13#10, #13#10, #13#10]);
  end;
    
  @_deflateInit_ := GetProcAddress(ZLibHandle, 'deflateInit_');
  @_deflate := GetProcAddress(ZLibHandle, 'deflate');
  @_deflateEnd := GetProcAddress(ZLibHandle, 'deflateEnd');
  @_inflateInit_ := GetProcAddress(ZLibHandle, 'inflateInit_');
  @_inflateInit2_ := GetProcAddress(ZLibHandle, 'inflateInit2_');
  @_inflate := GetProcAddress(ZLibHandle, 'inflate');
  @_inflateEnd := GetProcAddress(ZLibHandle, 'inflateEnd');
  @_inflateReset := GetProcAddress(ZLibHandle, 'inflateReset');
  
  if not Assigned(_deflate) then
    raise Exception.Create('Failed to load deflate from zlib1.dll');
  if not Assigned(_inflate) then
    raise Exception.Create('Failed to load inflate from zlib1.dll');
  if not Assigned(_deflateInit_) then
    raise Exception.Create('Failed to load deflateInit_ from zlib1.dll');
  if not Assigned(_inflateInit_) then
    raise Exception.Create('Failed to load inflateInit_ from zlib1.dll');
end;

procedure EnsureZLibAvailable;
begin
  if ZLibHandle = 0 then
    LoadZLibDLL;
end;

function deflateInit_(var strm: TZStreamRec; level: Integer; version: PChar;
  recsize: Integer): Integer;
begin
  if ZLibHandle = 0 then LoadZLibDLL;
  Result := _deflateInit_(strm, level, version, recsize);
end;

function deflate(var strm: TZStreamRec; flush: Integer): Integer;
begin
  if ZLibHandle = 0 then LoadZLibDLL;
  Result := _deflate(strm, flush);
end;

function deflateEnd(var strm: TZStreamRec): Integer;
begin
  if ZLibHandle = 0 then LoadZLibDLL;
  Result := _deflateEnd(strm);
end;
{$ELSE}
function deflateInit_(var strm: TZStreamRec; level: Integer; version: PChar;
  recsize: Integer): Integer; external;

function deflate(var strm: TZStreamRec; flush: Integer): Integer;
  external;

function deflateEnd(var strm: TZStreamRec): Integer; external;
{$ENDIF}

{** inflate routines ********************************************************}

{$IFDEF ZLIB_USE_DLL}
function inflateInit_(var strm: TZStreamRec; version: PChar;
  recsize: Integer): Integer;
begin
  if ZLibHandle = 0 then LoadZLibDLL;
  Result := _inflateInit_(strm, version, recsize);
end;

function inflateInit2_(var strm: TZStreamRec; windowBits: Integer; version: PChar;
  recsize: Integer): Integer;
begin
  if ZLibHandle = 0 then LoadZLibDLL;
  Result := _inflateInit2_(strm, windowBits, version, recsize);
end;

function inflate(var strm: TZStreamRec; flush: Integer): Integer;
begin
  if ZLibHandle = 0 then LoadZLibDLL;
  Result := _inflate(strm, flush);
end;

function inflateEnd(var strm: TZStreamRec): Integer;
begin
  if ZLibHandle = 0 then LoadZLibDLL;
  Result := _inflateEnd(strm);
end;

function inflateReset(var strm: TZStreamRec): Integer;
begin
  if ZLibHandle = 0 then LoadZLibDLL;
  Result := _inflateReset(strm);
end;
{$ELSE}
function inflateInit_(var strm: TZStreamRec; version: PChar;
  recsize: Integer): Integer; external;

function inflateInit2_(var strm: TZStreamRec; windowBits: Integer; version: PChar;
  recsize: Integer): Integer; external;

function inflate(var strm: TZStreamRec; flush: Integer): Integer;
  external;

function inflateEnd(var strm: TZStreamRec): Integer; external;

function inflateReset(var strm: TZStreamRec): Integer; external;
{$ENDIF}

{** zlib function implementations *******************************************}

function zcalloc(opaque: Pointer; items, size: Integer): Pointer;
begin
  GetMem(result,items * size);
end;

procedure zcfree(opaque, block: Pointer);
begin
  FreeMem(block);
end;

{** c function implementations **********************************************}

procedure _memset(p: Pointer; b: Byte; count: Integer); cdecl;
begin
  FillChar(p^,count,b);
end;

procedure _memcpy(dest, source: Pointer; count: Integer); cdecl;
begin
  Move(source^,dest^,count);
end;

{** custom zlib routines ****************************************************}

function DeflateInit(var stream: TZStreamRec; level: Integer): Integer;
begin
  result := DeflateInit_(stream,level,ZLIB_VERSION,SizeOf(TZStreamRec));
end;

// function DeflateInit2(var stream: TZStreamRec; level, method, windowBits,
//   memLevel, strategy: Integer): Integer;
// begin
//   result := DeflateInit2_(stream,level,method,windowBits,memLevel,
//     strategy,ZLIB_VERSION,SizeOf(TZStreamRec));
// end;

function InflateInit(var stream: TZStreamRec): Integer;
begin
  result := InflateInit_(stream,ZLIB_VERSION,SizeOf(TZStreamRec));
end;

function InflateInit2(var stream: TZStreamRec; windowBits: Integer): Integer;
begin
  result := InflateInit2_(stream,windowBits,ZLIB_VERSION,SizeOf(TZStreamRec));
end;

{****************************************************************************}

function ZCompressCheck(code: Integer): Integer;
begin
  result := code;

  if code < 0 then
  begin
    raise EZCompressionError.Create(_z_errmsg[2 - code]);
  end;
end;

function ZDecompressCheck(code: Integer): Integer;
begin
  Result := code;

  if code < 0 then
  begin
    raise EZDecompressionError.Create(_z_errmsg[2 - code]);
  end;
end;

procedure ZCompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer;
  level: TZCompressionLevel);
const
  delta = 256;
var
  zstream: TZStreamRec;
begin
  FillChar(zstream,SizeOf(TZStreamRec),0);

  outSize := ((inSize + (inSize div 10) + 12) + 255) and not 255;
  GetMem(outBuffer,outSize);

  try
    zstream.next_in := inBuffer;
    zstream.avail_in := inSize;
    zstream.next_out := outBuffer;
    zstream.avail_out := outSize;

    ZCompressCheck(DeflateInit(zstream,ZLevels[level]));

    try
      while ZCompressCheck(deflate(zstream,Z_FINISH)) <> Z_STREAM_END do
      begin
        Inc(outSize,delta);
        ReallocMem(outBuffer,outSize);

        zstream.next_out := PChar(Integer(outBuffer) + zstream.total_out);
        zstream.avail_out := delta;
      end;
    finally
      ZCompressCheck(deflateEnd(zstream));
    end;

    ReallocMem(outBuffer,zstream.total_out);
    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;

procedure ZDecompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer; outEstimate: Integer);
var
  zstream: TZStreamRec;
  delta  : Integer;
begin
  FillChar(zstream,SizeOf(TZStreamRec),0);

  delta := (inSize + 255) and not 255;

  if outEstimate = 0 then outSize := delta
  else outSize := outEstimate;

  GetMem(outBuffer,outSize);

  try
    zstream.next_in := inBuffer;
    zstream.avail_in := inSize;
    zstream.next_out := outBuffer;
    zstream.avail_out := outSize;

    ZDecompressCheck(InflateInit(zstream));

    try
      while ZDecompressCheck(inflate(zstream,Z_NO_FLUSH)) <> Z_STREAM_END do
      begin
        Inc(outSize,delta);
        ReallocMem(outBuffer,outSize);

        zstream.next_out := PChar(Integer(outBuffer) + zstream.total_out);
        zstream.avail_out := delta;
      end;
    finally
      ZDecompressCheck(inflateEnd(zstream));
    end;

    ReallocMem(outBuffer,zstream.total_out);
    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;

function ZCompressStr(const s: String; level: TZCompressionLevel): String;
var
  buffer: Pointer;
  size  : Integer;
begin
  ZCompress(PChar(s),Length(s),buffer,size,level);

  SetLength(result,size);
  Move(buffer^,result[1],size);

  FreeMem(buffer);
end;

function ZDecompressStr(const s: String): String;
var
  buffer: Pointer;
  size  : Integer;
begin
  ZDecompress(PChar(s),Length(s),buffer,size);

  SetLength(result,size);
  Move(buffer^,result[1],size);

  FreeMem(buffer);
end;

{** TCustomZStream **********************************************************}

constructor TCustomZStream.Create(stream: TStream);
begin
  inherited Create;

  FStream := stream;
  FStreamPos := stream.Position;
end;

procedure TCustomZStream.DoProgress;
begin
  if Assigned(FOnProgress) then FOnProgress(Self);
end;

{** TZCompressionStream *****************************************************}

constructor TZCompressionStream.Create(dest: TStream;
  compressionLevel: TZCompressionLevel);
begin
  inherited Create(dest);

  FZStream.next_out := FBuffer;
  FZStream.avail_out := SizeOf(FBuffer);

  ZCompressCheck(DeflateInit(FZStream,ZLevels[compressionLevel]));
end;

destructor TZCompressionStream.Destroy;
begin
  FZStream.next_in := Nil;
  FZStream.avail_in := 0;

  try
    if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

    while ZCompressCheck(deflate(FZStream,Z_FINISH)) <> Z_STREAM_END do
    begin
      FStream.WriteBuffer(FBuffer,SizeOf(FBuffer) - FZStream.avail_out);

      FZStream.next_out := FBuffer;
      FZStream.avail_out := SizeOf(FBuffer);
    end;

    if FZStream.avail_out < SizeOf(FBuffer) then
    begin
      FStream.WriteBuffer(FBuffer,SizeOf(FBuffer) - FZStream.avail_out);
    end;
  finally
    deflateEnd(FZStream);
  end;

  inherited Destroy;
end;

function TZCompressionStream.Read(var buffer; count: Longint): Longint;
begin
  raise EZCompressionError.Create(SZInvalid);
end;

function TZCompressionStream.Write(const buffer; count: Longint): Longint;
begin
  FZStream.next_in := @buffer;
  FZStream.avail_in := count;

  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  while FZStream.avail_in > 0 do
  begin
    ZCompressCheck(deflate(FZStream,Z_NO_FLUSH));

    if FZStream.avail_out = 0 then
    begin
      FStream.WriteBuffer(FBuffer,SizeOf(FBuffer));

      FZStream.next_out := FBuffer;
      FZStream.avail_out := SizeOf(FBuffer);

      FStreamPos := FStream.Position;

      DoProgress;
    end;
  end;

  result := Count;
end;

function TZCompressionStream.Seek(offset: Longint; origin: Word): Longint;
begin
  if (offset = 0) and (origin = soFromCurrent) then
  begin
    result := FZStream.total_in;
  end
  else raise EZCompressionError.Create(SZInvalid);
end;

function TZCompressionStream.GetCompressionRate: Single;
begin
  if FZStream.total_in = 0 then result := 0
  else result := (1.0 - (FZStream.total_out / FZStream.total_in)) * 100.0;
end;

{** TZDecompressionStream ***************************************************}

constructor TZDecompressionStream.Create(source: TStream);
begin
  inherited Create(source);

  FZStream.next_in := FBuffer;
  FZStream.avail_in := 0;

  ZDecompressCheck(InflateInit(FZStream));
end;

destructor TZDecompressionStream.Destroy;
begin
  inflateEnd(FZStream);

  inherited Destroy;
end;

function TZDecompressionStream.Read(var buffer; count: Longint): Longint;
begin
  FZStream.next_out := @buffer;
  FZStream.avail_out := count;

  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  while FZStream.avail_out > 0 do
  begin
    if FZStream.avail_in = 0 then
    begin
      FZStream.avail_in := FStream.Read(FBuffer,SizeOf(FBuffer));

      if FZStream.avail_in = 0 then
      begin
        result := count - FZStream.avail_out;

        Exit;
      end;

      FZStream.next_in := FBuffer;
      FStreamPos := FStream.Position;

      DoProgress;
    end;

    ZDecompressCheck(inflate(FZStream,Z_NO_FLUSH));
  end;

  result := Count;
end;

function TZDecompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EZDecompressionError.Create(SZInvalid);
end;

function TZDecompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  buf: Array [0..4095] of Char;
  i  : Integer;
begin
  if (offset = 0) and (origin = soFromBeginning) then
  begin
    ZDecompressCheck(inflateReset(FZStream));

    FZStream.next_in := FBuffer;
    FZStream.avail_in := 0;

    FStream.Position := 0;
    FStreamPos := 0;
  end
  else if ((offset >= 0) and (origin = soFromCurrent)) or
          (((offset - FZStream.total_out) > 0) and (origin = soFromBeginning)) then
  begin
    if origin = soFromBeginning then Dec(offset,FZStream.total_out);

    if offset > 0 then
    begin
      for i := 1 to offset div SizeOf(buf) do ReadBuffer(buf,SizeOf(buf));
      ReadBuffer(buf,offset mod SizeOf(buf));
    end;
  end
  else raise EZDecompressionError.Create(SZInvalid);

  result := FZStream.total_out;
end;

{$IFDEF ZLIB_USE_DLL}
initialization
  ZLibHandle := 0;
  _deflateInit_ := nil;
  _deflate := nil;
  _deflateEnd := nil;
  _inflateInit_ := nil;
  _inflateInit2_ := nil;
  _inflate := nil;
  _inflateEnd := nil;
  _inflateReset := nil;
{$ENDIF}

end.
