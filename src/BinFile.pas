{******************************************************************************}
{                                                                              }
{  ImageWriter - Binary File Operations Unit                                  }
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
{    Abstract base class for binary file operations (read/write).             }
{                                                                              }
{******************************************************************************}

unit BinFile;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, SysUtils;

type
  TBinFile = class
  private
    FHandle: THandle;
    FFileName: string;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    function Open: Boolean; virtual;
    procedure Close; virtual;
    function Read(var Buffer; Count: Integer): Integer; virtual;
    function Write(const Buffer; Count: Integer): Integer; virtual;
    function Seek(Offset: Int64): Boolean; virtual;
    function GetSize: Int64; virtual;
    property Handle: THandle read FHandle;
    property FileName: string read FFileName;
  end;

  // Windows-specific implementation for physical drives
  TWinBinFile = class(TBinFile)
  public
    function Open: Boolean; override;
    function GetSize: Int64; override;
  end;

  // Unix-style file implementation (for regular files)
  TUnixBinFile = class(TBinFile)
  public
    function Open: Boolean; override;
  end;

implementation

uses
  WinIOCTL, Native;

constructor TBinFile.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  FHandle := INVALID_HANDLE_VALUE;
end;

destructor TBinFile.Destroy;
begin
  Close;
  inherited;
end;

function TBinFile.Open: Boolean;
begin
  Result := False;
end;

procedure TBinFile.Close;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    CloseHandle(FHandle);
    FHandle := INVALID_HANDLE_VALUE;
  end;
end;

function TBinFile.Read(var Buffer; Count: Integer): Integer;
var
  BytesRead: DWORD;
begin
  if FHandle = INVALID_HANDLE_VALUE then
  begin
    Result := -1;
    Exit;
  end;
  
  if ReadFile(FHandle, Buffer, Count, BytesRead, nil) then
    Result := BytesRead
  else
    Result := -1;
end;

function TBinFile.Write(const Buffer; Count: Integer): Integer;
var
  BytesWritten: DWORD;
begin
  if FHandle = INVALID_HANDLE_VALUE then
  begin
    Result := -1;
    Exit;
  end;
  
  if WriteFile(FHandle, Buffer, Count, BytesWritten, nil) then
    Result := BytesWritten
  else
    Result := -1;
end;

function TBinFile.Seek(Offset: Int64): Boolean;
var
  LowPart, HighPart: DWORD;
begin
  if FHandle = INVALID_HANDLE_VALUE then
  begin
    Result := False;
    Exit;
  end;
  
  LowPart := DWORD(Offset and $FFFFFFFF);
  HighPart := DWORD(Offset shr 32);
  
  Result := SetFilePointer(FHandle, LowPart, @HighPart, FILE_BEGIN) <> $FFFFFFFF;
end;

function TBinFile.GetSize: Int64;
var
  LowPart, HighPart: DWORD;
begin
  Result := 0;
  
  if FHandle = INVALID_HANDLE_VALUE then
    Exit;
    
  LowPart := GetFileSize(FHandle, @HighPart);
  if LowPart = $FFFFFFFF then
  begin
    if GetLastError <> NO_ERROR then
      Exit;
  end;
  
  Result := (Int64(HighPart) shl 32) or LowPart;
end;

{ TWinBinFile }

function TWinBinFile.Open: Boolean;
begin
  Close;
  
  // Open physical drive with read/write access
  FHandle := CreateFile(
    PChar(FFileName),
    GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil,
    OPEN_EXISTING,
    FILE_FLAG_NO_BUFFERING or FILE_FLAG_WRITE_THROUGH,
    0
  );
  
  Result := FHandle <> INVALID_HANDLE_VALUE;
end;

function TWinBinFile.GetSize: Int64;
var
  DiskGeometry: TDISK_GEOMETRY;
  BytesReturned: DWORD;
  Cylinders: Int64;
begin
  Result := 0;
  
  if FHandle = INVALID_HANDLE_VALUE then
    Exit;
  
  // Use IOCTL to get disk size
  if DeviceIoControl(
    FHandle,
    CtlCode(IOCTL_DISK_BASE, $00, METHOD_BUFFERED, FILE_ANY_ACCESS),
    nil, 0,
    @DiskGeometry, SizeOf(DiskGeometry),
    BytesReturned, nil
  ) then
  begin
    // Convert LARGE_INTEGER to Int64
    Cylinders := DiskGeometry.Cylinders.QuadPart;
    
    Result := Cylinders * 
              Int64(DiskGeometry.TracksPerCylinder) * 
              Int64(DiskGeometry.SectorsPerTrack) * 
              Int64(DiskGeometry.BytesPerSector);
  end;
end;

{ TUnixBinFile }

function TUnixBinFile.Open: Boolean;
begin
  Close;
  
  // Open regular file
  FHandle := CreateFile(
    PChar(FFileName),
    GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ,
    nil,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    0
  );
  
  Result := FHandle <> INVALID_HANDLE_VALUE;
end;

end.
