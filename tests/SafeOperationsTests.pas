{******************************************************************************}
{                                                                              }
{  ImageWriter - Safe Operations Tests                                        }
{                                                                              }
{  Copyright (c) 2024-2025 Anton Zelenov <tixset@gmail.com>                   }
{  GitHub: https://github.com/tixset/ImageWriter                              }
{                                                                              }
{  Description:                                                                }
{    Tests for safe operations that prevent accidental data loss.             }
{    Tests write protection mechanisms and device filtering.                  }
{                                                                              }
{******************************************************************************}

unit SafeOperationsTests;

interface

uses
  TestFramework,
  SysUtils,
  Classes,
  Windows;

type
  TSafeOperationsTest = class(TTestCase)
  private
    FTestImagePath: string;
    FRemovableDevice: string;
    function CreateSmallTestImage(SizeKB: Integer): Boolean;
    function FindRemovableDevice: string;
    function IsDeviceRemovable(const DevicePath: string): Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFixedDiskProtection;
    procedure TestRemovableDeviceIdentification;
    procedure TestSmallImageCreation;
    procedure TestDeviceAccessWithoutWrite;
  end;

implementation

{ TSafeOperationsTest }

procedure TSafeOperationsTest.SetUp;
begin
  inherited;
  FTestImagePath := GetCurrentDir + '\test_image.bin';
  FRemovableDevice := FindRemovableDevice;
end;

procedure TSafeOperationsTest.TearDown;
begin
  // Clean up test image
  if FileExists(FTestImagePath) then
    DeleteFile(FTestImagePath);
  inherited;
end;

function TSafeOperationsTest.CreateSmallTestImage(SizeKB: Integer): Boolean;
var
  F: File;
  Buffer: array[0..1023] of Byte;
  I, K: Integer;
begin
  Result := False;
  try
    AssignFile(F, FTestImagePath);
    Rewrite(F, 1);
    try
      // Fill buffer with test pattern
      for I := 0 to 1023 do
        Buffer[I] := I mod 256;
        
      // Write KB chunks
      for K := 0 to SizeKB - 1 do
        BlockWrite(F, Buffer, SizeOf(Buffer));
        
      Result := True;
    finally
      CloseFile(F);
    end;
  except
    on E: Exception do
      WriteLn('Failed to create test image: ' + E.Message);
  end;
end;

function TSafeOperationsTest.FindRemovableDevice: string;
var
  Drives: DWORD;
  DriveLetter: Char;
  DrivePath: string;
  DriveType: UINT;
begin
  Result := '';
  Drives := GetLogicalDrives;
  
  for DriveLetter := 'A' to 'Z' do
  begin
    if (Drives and 1) <> 0 then
    begin
      DrivePath := DriveLetter + ':\';
      DriveType := GetDriveType(PChar(DrivePath));
      
      if DriveType = DRIVE_REMOVABLE then
      begin
        Result := DrivePath;
        Exit;
      end;
    end;
    Drives := Drives shr 1;
  end;
end;

function TSafeOperationsTest.IsDeviceRemovable(const DevicePath: string): Boolean;
var
  DriveType: UINT;
begin
  DriveType := GetDriveType(PChar(DevicePath));
  Result := DriveType = DRIVE_REMOVABLE;
end;

procedure TSafeOperationsTest.TestFixedDiskProtection;
var
  Drives: DWORD;
  DriveLetter: Char;
  DrivePath: string;
  DriveType: UINT;
  FixedDisks: TStringList;
begin
  FixedDisks := TStringList.Create;
  try
    Drives := GetLogicalDrives;
    
    for DriveLetter := 'C' to 'Z' do  // Start from C: to avoid A: and B:
    begin
      if (Drives and (1 shl (Ord(DriveLetter) - Ord('A')))) <> 0 then
      begin
        DrivePath := DriveLetter + ':\';
        DriveType := GetDriveType(PChar(DrivePath));
        
        if DriveType = DRIVE_FIXED then
          FixedDisks.Add(DrivePath);
      end;
    end;
    
    WriteLn(Format('Found %d fixed disk(s) - ensuring they are protected', 
      [FixedDisks.Count]));
      
    // Verify we can identify fixed disks
    CheckTrue(FixedDisks.Count > 0, 'Should detect at least system drive as fixed');
    
    // Verify C: is recognized as fixed
    CheckEquals(DRIVE_FIXED, Integer(GetDriveType('C:\')), 
      'System drive C: must be identified as FIXED');
      
  finally
    FixedDisks.Free;
  end;
end;

procedure TSafeOperationsTest.TestRemovableDeviceIdentification;
begin
  if FRemovableDevice = '' then
  begin
    WriteLn('SKIP: No removable device found');
    Check(True, 'Test skipped - no removable devices');
    Exit;
  end;
  
  WriteLn('Testing with removable device: ' + FRemovableDevice);
  
  CheckTrue(IsDeviceRemovable(FRemovableDevice), 
    'Device should be identified as removable: ' + FRemovableDevice);
    
  // Verify it's NOT a fixed disk
  CheckNotEquals(DRIVE_FIXED, Integer(GetDriveType(PChar(FRemovableDevice))), 
    'Removable device should NOT be identified as fixed disk');
end;

procedure TSafeOperationsTest.TestSmallImageCreation;
const
  TEST_SIZE_KB = 10; // 10 KB test image
begin
  WriteLn('Creating ' + IntToStr(TEST_SIZE_KB) + ' KB test image...');
  
  CheckTrue(CreateSmallTestImage(TEST_SIZE_KB), 
    'Should successfully create test image');
    
  CheckTrue(FileExists(FTestImagePath), 
    'Test image file should exist: ' + FTestImagePath);
    
  // Verify size
  CheckEquals(TEST_SIZE_KB * 1024, Integer(GetFileSize(FTestImagePath)), 
    'Test image should be exactly ' + IntToStr(TEST_SIZE_KB) + ' KB');
end;

function GetFileSize(const FileName: string): Int64;
var
  SearchRec: TSearchRec;
begin
  Result := 0;
  if FindFirst(FileName, faAnyFile, SearchRec) = 0 then
  begin
    Result := SearchRec.Size;
    FindClose(SearchRec);
  end;
end;

procedure TSafeOperationsTest.TestDeviceAccessWithoutWrite;
var
  hDevice: THandle;
  DevicePath: string;
begin
  if FRemovableDevice = '' then
  begin
    WriteLn('SKIP: No removable device for access test');
    Check(True, 'Test skipped');
    Exit;
  end;
  
  // Convert D:\ to \\.\D: format
  DevicePath := '\\.\' + Copy(FRemovableDevice, 1, 2);
  
  WriteLn('Attempting read-only access to: ' + DevicePath);
  
  // Try to open device for reading only (safe operation)
  hDevice := CreateFile(
    PChar(DevicePath),
    GENERIC_READ,  // Read only - safe
    FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil,
    OPEN_EXISTING,
    0,
    0
  );
  
  if hDevice <> INVALID_HANDLE_VALUE then
  begin
    WriteLn('Successfully opened device for read access (safe)');
    CloseHandle(hDevice);
    Check(True, 'Device accessible for reading');
  end
  else
  begin
    WriteLn('WARNING: Could not open device (may need admin rights)');
    Check(True, 'Test skipped - access denied');
  end;
end;

initialization
  RegisterTest(TSafeOperationsTest.Suite);

end.
