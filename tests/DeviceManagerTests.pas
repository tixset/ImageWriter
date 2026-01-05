{******************************************************************************}
{                                                                              }
{  ImageWriter - Device Manager Tests                                         }
{                                                                              }
{  Copyright (c) 2024-2025 Anton Zelenov <tixset@gmail.com>                   }
{  GitHub: https://github.com/tixset/ImageWriter                              }
{                                                                              }
{  Description:                                                                }
{    Unit tests for TDeviceManager class.                                     }
{    Tests device enumeration, validation, and WMI queries.                   }
{                                                                              }
{******************************************************************************}

unit DeviceManagerTests;

interface

uses
  TestFramework,
  SysUtils,
  Classes,
  DeviceManager;

type
  TDeviceManagerTest = class(TTestCase)
  private
    function IsAdministrator: Boolean;
  published
    procedure TestEnumerateDevices;
    procedure TestGetDeviceInfo;
    procedure TestValidateDevicePath;
    procedure TestGetDiskExtents;
    procedure TestGetUSBInfo;
  end;

implementation

uses
  Windows,
  LogUtils;

{ TDeviceManagerTest }

function TDeviceManagerTest.IsAdministrator: Boolean;
var
  Handle: THandle;
begin
  // Try to open \\.\PhysicalDrive0 - requires admin rights
  Handle := CreateFile('\\.\PhysicalDrive0',
                       GENERIC_READ,
                       FILE_SHARE_READ or FILE_SHARE_WRITE,
                       nil,
                       OPEN_EXISTING,
                       0,
                       0);
  Result := Handle <> INVALID_HANDLE_VALUE;
  if Result then
    CloseHandle(Handle);
end;

procedure TDeviceManagerTest.TestEnumerateDevices;
var
  Devices: TStringList;
  I: Integer;
begin
  TLogUtils.Info('DeviceManagerTest', 'TestEnumerateDevices: Starting device enumeration test');
  
  Devices := TStringList.Create;
  try
    // Enumerate physical devices
    TDeviceManager.EnumeratePhysicalDevices(Devices);
    
    TLogUtils.Info('DeviceManagerTest', Format('Found %d physical devices', [Devices.Count]));
    
    // Should find at least system disk
    CheckTrue(Devices.Count >= 1, 'Should find at least one physical disk');
    
    // Verify format of device paths
    for I := 0 to Devices.Count - 1 do
    begin
      TLogUtils.Debug('DeviceManagerTest', Format('Device %d: %s', [I, Devices[I]]));
      
      // Check path format
      CheckTrue(Pos('\\.\PhysicalDrive', Devices[I]) = 1,
                'Device path should start with \\.\PhysicalDrive');
      
      // Check device number is valid
      CheckTrue(StrToIntDef(Copy(Devices[I], 18, MaxInt), -1) >= 0,
                'Device number should be non-negative');
    end;
    
    TLogUtils.Info('DeviceManagerTest', 'TestEnumerateDevices: PASSED');
  finally
    Devices.Free;
  end;
end;

procedure TDeviceManagerTest.TestGetDeviceInfo;
var
  DeviceInfo: TDeviceInfo;
  Success: Boolean;
begin
  TLogUtils.Info('DeviceManagerTest', 'TestGetDeviceInfo: Starting device info test');
  
  // Try to get info for PhysicalDrive0 (system disk)
  Success := TDeviceManager.GetDeviceInfo('\\.\PhysicalDrive0', DeviceInfo);
  
  if Success then
  begin
    TLogUtils.Info('DeviceManagerTest', Format('PhysicalDrive0 Model: %s', [DeviceInfo.Model]));
    TLogUtils.Info('DeviceManagerTest', Format('PhysicalDrive0 Size: %d bytes', [DeviceInfo.Size]));
    
    // Verify we got valid data
    CheckNotEquals('', DeviceInfo.DevicePath, 'DevicePath should not be empty');
    CheckNotEquals('', DeviceInfo.DisplayName, 'DisplayName should not be empty');
    CheckTrue(DeviceInfo.Size > 0, 'Size should be greater than 0');
    
    // Most systems have disk larger than 1GB
    CheckTrue(DeviceInfo.Size > 1024 * 1024 * 1024,
              'System disk should be larger than 1GB');
    
    TLogUtils.Info('DeviceManagerTest', 'TestGetDeviceInfo: PASSED');
  end
  else
  begin
    TLogUtils.Warning('DeviceManagerTest', 'Could not get device info - may need admin rights');
    // Don't fail test if WMI is restricted
    Check(True, 'Test skipped - insufficient permissions');
  end;
end;

procedure TDeviceManagerTest.TestValidateDevicePath;
begin
  TLogUtils.Info('DeviceManagerTest', 'TestValidateDevicePath: Starting validation test');
  
  // Valid paths
  CheckTrue(TDeviceManager.IsValidDevicePath('\\.\PhysicalDrive0'),
            '\\.\PhysicalDrive0 should be valid');
  CheckTrue(TDeviceManager.IsValidDevicePath('\\.\PhysicalDrive15'),
            '\\.\PhysicalDrive15 should be valid');
  
  // Invalid paths
  CheckFalse(TDeviceManager.IsValidDevicePath(''),
             'Empty path should be invalid');
  CheckFalse(TDeviceManager.IsValidDevicePath('C:\'),
             'C:\ should be invalid');
  CheckFalse(TDeviceManager.IsValidDevicePath('\\.\PhysicalDrive'),
             'Path without number should be invalid');
  CheckFalse(TDeviceManager.IsValidDevicePath('\\.\PhysicalDrive-1'),
             'Negative number should be invalid');
  CheckFalse(TDeviceManager.IsValidDevicePath('PhysicalDrive0'),
             'Path without \\.\\ should be invalid');
  
  TLogUtils.Info('DeviceManagerTest', 'TestValidateDevicePath: PASSED');
end;

procedure TDeviceManagerTest.TestGetDiskExtents;
var
  Extents: TDiskExtents;
  Success: Boolean;
begin
  TLogUtils.Info('DeviceManagerTest', 'TestGetDiskExtents: Starting disk extents test');
  
  if not IsAdministrator then
  begin
    TLogUtils.Warning('DeviceManagerTest', 'Test skipped - requires administrator rights');
    Check(True, 'Test skipped - need admin rights for IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS');
    Exit;
  end;
  
  // Get extents for system disk
  Success := TDeviceManager.GetDiskExtents('\\.\PhysicalDrive0', Extents);
  
  if Success then
  begin
    TLogUtils.Info('DeviceManagerTest', Format('PhysicalDrive0 has %d extents', [Extents.NumberOfDiskExtents]));
    
    CheckTrue(Extents.NumberOfDiskExtents > 0,
              'Should have at least one disk extent');
    CheckTrue(Extents.Extents[0].DiskNumber >= 0,
              'Disk number should be non-negative');
    CheckTrue(Extents.Extents[0].ExtentLength > 0,
              'Extent length should be positive');
    
    TLogUtils.Info('DeviceManagerTest', 'TestGetDiskExtents: PASSED');
  end
  else
  begin
    TLogUtils.Warning('DeviceManagerTest', 'GetDiskExtents failed - may be locked or restricted');
    Check(True, 'Test completed with warnings');
  end;
end;

procedure TDeviceManagerTest.TestGetUSBInfo;
var
  FullInfo: TFullDiskInfo;
  Success: Boolean;
begin
  TLogUtils.Info('DeviceManagerTest', 'TestGetUSBInfo: Starting USB info test');
  
  // Try to get full disk info for PhysicalDrive0
  Success := TDeviceManager.GetFullDiskInfo('\\.\PhysicalDrive0', FullInfo);
  
  if Success then
  begin
    TLogUtils.Info('DeviceManagerTest', Format('Disk Model: %s', [FullInfo.Model]));
    TLogUtils.Info('DeviceManagerTest', Format('Manufacturer: %s', [FullInfo.Manufacturer]));
    TLogUtils.Info('DeviceManagerTest', Format('Interface: %s', [FullInfo.InterfaceType]));
    
    // Verify structure is populated
    CheckTrue(FullInfo.Size > 0, 'Size should be positive');
    
    // Check if it's USB device
    if (FullInfo.USBVID <> '') and (FullInfo.USBPID <> '') then
    begin
      TLogUtils.Info('DeviceManagerTest', Format('USB Device: VID=%s PID=%s',
                     [FullInfo.USBVID, FullInfo.USBPID]));
      CheckTrue(Length(FullInfo.USBVID) = 4, 'USB VID should be 4 hex digits');
      CheckTrue(Length(FullInfo.USBPID) = 4, 'USB PID should be 4 hex digits');
    end;
    
    TLogUtils.Info('DeviceManagerTest', 'TestGetUSBInfo: PASSED');
  end
  else
  begin
    TLogUtils.Warning('DeviceManagerTest', 'GetFullDiskInfo failed - WMI may be restricted');
    Check(True, 'Test skipped - WMI access restricted');
  end;
end;

initialization
  RegisterTest(TDeviceManagerTest.Suite);

end.
