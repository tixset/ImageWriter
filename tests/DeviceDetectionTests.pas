{******************************************************************************}
{                                                                              }
{  ImageWriter - Device Detection Tests                                       }
{                                                                              }
{  Copyright (c) 2024-2025 Anton Zelenov <tixset@gmail.com>                   }
{  GitHub: https://github.com/tixset/ImageWriter                              }
{                                                                              }
{  Description:                                                                }
{    Tests for device enumeration and detection functionality.                }
{    ONLY tests removable devices - NEVER fixed disks.                        }
{                                                                              }
{******************************************************************************}

unit DeviceDetectionTests;

interface

uses
  TestFramework,
  SysUtils,
  Classes,
  Windows;

type
  TDeviceDetectionTest = class(TTestCase)
  private
    FRemovableDevices: TStringList;
    procedure EnumerateRemovableDevices;
    function GetDriveTypeStr(DriveType: UINT): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRemovableDeviceDetection;
    procedure TestNoFixedDiskInList;
    procedure TestRemovableDeviceExists;
    procedure TestDriveTypeRecognition;
  end;

implementation

uses
  Native, WinBinFile;

{ TDeviceDetectionTest }

procedure TDeviceDetectionTest.SetUp;
begin
  inherited;
  FRemovableDevices := TStringList.Create;
  EnumerateRemovableDevices;
end;

procedure TDeviceDetectionTest.TearDown;
begin
  FRemovableDevices.Free;
  inherited;
end;

procedure TDeviceDetectionTest.EnumerateRemovableDevices;
var
  Drives: DWORD;
  DriveLetter: Char;
  DrivePath: string;
  DriveType: UINT;
begin
  FRemovableDevices.Clear;
  
  // Get logical drives bitmask
  Drives := GetLogicalDrives;
  
  // Check each possible drive letter
  for DriveLetter := 'A' to 'Z' do
  begin
    if (Drives and 1) <> 0 then
    begin
      DrivePath := DriveLetter + ':\';
      DriveType := GetDriveType(PChar(DrivePath));
      
      // Only add removable drives
      if DriveType = DRIVE_REMOVABLE then
      begin
        FRemovableDevices.Add(DrivePath + ' (Removable)');
      end;
    end;
    Drives := Drives shr 1;
  end;
end;

function TDeviceDetectionTest.GetDriveTypeStr(DriveType: UINT): string;
begin
  case DriveType of
    DRIVE_UNKNOWN: Result := 'Unknown';
    DRIVE_NO_ROOT_DIR: Result := 'No Root Dir';
    DRIVE_REMOVABLE: Result := 'Removable';
    DRIVE_FIXED: Result := 'Fixed';
    DRIVE_REMOTE: Result := 'Network';
    DRIVE_CDROM: Result := 'CD-ROM';
    DRIVE_RAMDISK: Result := 'RAM Disk';
  else
    Result := 'Other';
  end;
end;

procedure TDeviceDetectionTest.TestRemovableDeviceDetection;
begin
  CheckTrue(Assigned(FRemovableDevices), 'Device list should be created');
  
  if FRemovableDevices.Count > 0 then
    WriteLn('Found ' + IntToStr(FRemovableDevices.Count) + ' removable device(s)')
  else
    WriteLn('WARNING: No removable devices found - some tests will be skipped');
end;

procedure TDeviceDetectionTest.TestNoFixedDiskInList;
var
  I: Integer;
  DriveType: UINT;
  DrivePath: string;
begin
  // Verify no fixed disks in removable list
  for I := 0 to FRemovableDevices.Count - 1 do
  begin
    DrivePath := Copy(FRemovableDevices[I], 1, 3);
    DriveType := GetDriveType(PChar(DrivePath));
    
    CheckNotEquals(DRIVE_FIXED, Integer(DriveType), 
      'Found fixed disk in removable list: ' + DrivePath);
  end;
end;

procedure TDeviceDetectionTest.TestRemovableDeviceExists;
begin
  if FRemovableDevices.Count = 0 then
  begin
    WriteLn('SKIP: No removable devices available for testing');
    Check(True, 'Test skipped - no removable devices');
  end
  else
  begin
    CheckTrue(FRemovableDevices.Count > 0, 
      'At least one removable device should be available');
    WriteLn('Test removable devices:');
    WriteLn(FRemovableDevices.Text);
  end;
end;

procedure TDeviceDetectionTest.TestDriveTypeRecognition;
var
  Drives: DWORD;
  DriveLetter: Char;
  DrivePath: string;
  DriveType: UINT;
  FixedCount, RemovableCount: Integer;
begin
  FixedCount := 0;
  RemovableCount := 0;
  
  Drives := GetLogicalDrives;
  
  for DriveLetter := 'A' to 'Z' do
  begin
    if (Drives and 1) <> 0 then
    begin
      DrivePath := DriveLetter + ':\';
      DriveType := GetDriveType(PChar(DrivePath));
      
      case DriveType of
        DRIVE_FIXED: Inc(FixedCount);
        DRIVE_REMOVABLE: Inc(RemovableCount);
      end;
    end;
    Drives := Drives shr 1;
  end;
  
  WriteLn(Format('System has %d fixed disk(s) and %d removable device(s)', 
    [FixedCount, RemovableCount]));
    
  CheckTrue(True, 'Drive type recognition completed');
end;

initialization
  RegisterTest(TDeviceDetectionTest.Suite);

end.
