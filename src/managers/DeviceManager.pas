{******************************************************************************}
{                                                                              }
{  ImageWriter - Device Manager Unit                                          }
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
{    Device enumeration and management via WMI.                                }
{    Handles physical disk detection and size queries without admin rights.   }
{                                                                              }
{******************************************************************************}

unit DeviceManager;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, Classes, SysUtils, ActiveX, ComObj, Variants, LogUtils;

type
  // Device information structure
  TDeviceInfo = record
    DevicePath: string;
    DisplayName: string;
    Size: Int64;
    Model: string;
    IsRemovable: Boolean;
  end;
  
  // Full device information from WMI
  TFullDiskInfo = record
    Model: string;
    Manufacturer: string;
    SerialNumber: string;
    FirmwareRevision: string;
    Size: Int64;
    InterfaceType: string;
    MediaType: string;
    PNPDeviceID: string;
    USBVID: string;
    USBPID: string;
    BytesPerSector: Integer;
    SectorsPerTrack: Integer;
  end;

  /// <summary>
  /// Device manager for physical disk enumeration and information retrieval
  /// </summary>
  /// <remarks>
  /// Uses WMI (Windows Management Instrumentation) for device information.
  /// Does not require administrative privileges for basic operations.
  /// </remarks>
  TDeviceManager = class
  private
    FDeviceList: TStringList;
    FShowFixedDisks: Boolean;
    
    function GetDiskInfoWMI(DiskNumber: Integer; out Model: string; out Size: Int64): Boolean;
    function IsRemovableDrive(DiskNumber: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    
    /// <summary>
    /// Enumerate all available physical disks
    /// </summary>
    /// <param name="DeviceList">List to receive device paths (e.g., \\.\PhysicalDrive0)</param>
    /// <param name="ShowFixed">Include fixed (non-removable) disks in enumeration</param>
    /// <remarks>
    /// Uses WMI Win32_DiskDrive query. Filters removable/fixed based on MediaType.
    /// Requires CoInitialize to be called before use.
    /// </remarks>
    procedure EnumerateDevices(DeviceList: TStringList; ShowFixed: Boolean);
    
    /// <summary>
    /// Get device path from combo box index
    /// </summary>
    /// <param name="ComboIndex">Index in internal device list</param>
    /// <returns>Device path string (e.g., \\.\PhysicalDrive1)</returns>
    function GetDevicePath(ComboIndex: Integer): string;
    
    /// <summary>
    /// Get disk size using WMI (no admin rights required)
    /// </summary>
    /// <param name="DiskNumber">Physical disk number (0, 1, 2, etc.)</param>
    /// <returns>Disk size in bytes, or 0 if query fails</returns>
    /// <remarks>
    /// Queries Win32_DiskDrive.Size property. More reliable than IOCTL for non-admin users.
    /// </remarks>
    function GetDiskSizeViaWMI(DiskNumber: Integer): Int64;
    
    /// <summary>
    /// Get comprehensive disk information via WMI
    /// </summary>
    /// <param name="DiskNumber">Physical disk number</param>
    /// <param name="Info">Output structure with all disk properties</param>
    /// <returns>True if successful, False on WMI query failure</returns>
    /// <remarks>
    /// Retrieves Model, Manufacturer, SerialNumber, FirmwareRevision, Size, InterfaceType,
    /// MediaType, PNPDeviceID, USB VID/PID, BytesPerSector, SectorsPerTrack.
    /// </remarks>
    function GetFullDiskInfoWMI(DiskNumber: Integer; out Info: TFullDiskInfo): Boolean;
    
    /// <summary>
    /// Get physical disk size using IOCTL
    /// </summary>
    /// <param name="DevicePath">Full device path (e.g., \\.\PhysicalDrive0)</param>
    /// <returns>Disk size in bytes, or 0 on failure</returns>
    /// <remarks>
    /// Uses IOCTL_DISK_GET_LENGTH_INFO. May require administrative privileges.
    /// Prefer GetDiskSizeViaWMI for non-admin scenarios.
    /// </remarks>
    function GetPhysicalDiskSize(const DevicePath: string): Int64;
    
    /// <summary>
    /// Extract disk number from device path
    /// </summary>
    /// <param name="DevicePath">Device path string</param>
    /// <returns>Disk number, or -1 if invalid path format</returns>
    /// <remarks>
    /// Parses "\\.\PhysicalDrive123" to return 123.
    /// </remarks>
    function ExtractDiskNumber(const DevicePath: string): Integer;
    
    /// <summary>
    /// Show or hide fixed (non-removable) disks in enumeration
    /// </summary>
    property ShowFixedDisks: Boolean read FShowFixedDisks write FShowFixedDisks;
  end;

implementation

uses
  WinIOCTL, Native;

{ TDeviceManager }

constructor TDeviceManager.Create;
begin
  inherited Create;
  FDeviceList := TStringList.Create;
  FShowFixedDisks := False;
end;

destructor TDeviceManager.Destroy;
begin
  FDeviceList.Free;
  inherited Destroy;
end;

function TDeviceManager.GetDiskSizeViaWMI(DiskNumber: Integer): Int64;
var
  WMI: OleVariant;
  Services: OleVariant;
  ObjectSet: OleVariant;
  WMIObject: OleVariant;
  Enum: IEnumVariant;
  Value: Cardinal;
  Query: string;
begin
  Result := 0;
  
  try
    CoInitialize(nil);
    try
      WMI := CreateOleObject('WbemScripting.SWbemLocator');
      Services := WMI.ConnectServer('.', 'root\CIMV2', '', '');
      
      Query := Format('SELECT Size FROM Win32_DiskDrive WHERE DeviceID="\\\\.\\PHYSICALDRIVE%d"', [DiskNumber]);
      ObjectSet := Services.ExecQuery(Query);
      
      Enum := IUnknown(ObjectSet._NewEnum) as IEnumVariant;
      if Enum.Next(1, WMIObject, Value) = S_OK then
      begin
        if not VarIsNull(WMIObject.Size) then
          Result := StrToInt64Def(VarToStr(WMIObject.Size), 0);
      end;
    finally
      CoUninitialize;
    end;
  except
    // Ignore WMI errors
  end;
end;

function TDeviceManager.GetDiskInfoWMI(DiskNumber: Integer; out Model: string; out Size: Int64): Boolean;
var
  WMI: OleVariant;
  Services: OleVariant;
  ObjectSet: OleVariant;
  WMIObject: OleVariant;
  Enum: IEnumVariant;
  Value: Cardinal;
  Query: string;
begin
  Result := False;
  Model := '';
  Size := 0;
  
  try
    CoInitialize(nil);
    try
      WMI := CreateOleObject('WbemScripting.SWbemLocator');
      Services := WMI.ConnectServer('.', 'root\CIMV2', '', '');
      
      Query := Format('SELECT * FROM Win32_DiskDrive WHERE DeviceID="\\\\.\\PHYSICALDRIVE%d"', [DiskNumber]);
      ObjectSet := Services.ExecQuery(Query);
      
      Enum := IUnknown(ObjectSet._NewEnum) as IEnumVariant;
      if Enum.Next(1, WMIObject, Value) = S_OK then
      begin
        if not VarIsNull(WMIObject.Model) then
          Model := VarToStr(WMIObject.Model);
          
        if not VarIsNull(WMIObject.Size) then
          Size := StrToInt64Def(VarToStr(WMIObject.Size), 0);
          
        Result := True;
      end;
    finally
      CoUninitialize;
    end;
  except
    // Ignore WMI errors
  end;
end;

function TDeviceManager.IsRemovableDrive(DiskNumber: Integer): Boolean;
var
  WMI: OleVariant;
  Services: OleVariant;
  ObjectSet: OleVariant;
  WMIObject: OleVariant;
  Enum: IEnumVariant;
  Value: Cardinal;
  Query: string;
  MediaType: string;
begin
  Result := False;
  
  try
    CoInitialize(nil);
    try
      WMI := CreateOleObject('WbemScripting.SWbemLocator');
      Services := WMI.ConnectServer('.', 'root\CIMV2', '', '');
      
      Query := Format('SELECT MediaType FROM Win32_DiskDrive WHERE DeviceID="\\\\.\\PHYSICALDRIVE%d"', [DiskNumber]);
      ObjectSet := Services.ExecQuery(Query);
      
      Enum := IUnknown(ObjectSet._NewEnum) as IEnumVariant;
      if Enum.Next(1, WMIObject, Value) = S_OK then
      begin
        if not VarIsNull(WMIObject.MediaType) then
        begin
          MediaType := UpperCase(VarToStr(WMIObject.MediaType));
          Result := (Pos('REMOVABLE', MediaType) > 0) or (Pos('USB', MediaType) > 0);
        end;
      end;
    finally
      CoUninitialize;
    end;
  except
    // Assume removable on error (safer)
    Result := True;
  end;
end;

procedure TDeviceManager.EnumerateDevices(DeviceList: TStringList; ShowFixed: Boolean);
var
  i: Integer;
  DevicePath: string;
  Model: string;
  Size: Int64;
  IsRemovable: Boolean;
  DisplayName: string;
begin
  DeviceList.Clear;
  FShowFixedDisks := ShowFixed;
  
  // Enumerate PhysicalDrive0 to PhysicalDrive15
  for i := 0 to 15 do
  begin
    DevicePath := Format('\\?\PhysicalDrive%d', [i]);
    
    // Try to get disk info via WMI
    if GetDiskInfoWMI(i, Model, Size) then
    begin
      IsRemovable := IsRemovableDrive(i);
      
      // Skip fixed disks if not showing them
      if (not ShowFixed) and (not IsRemovable) then
        Continue;
        
      // Build display name
      if Model <> '' then
        DisplayName := Format('#%d %s', [i, Model])
      else
        DisplayName := Format('#%d Physical Drive %d', [i, i]);
        
      // Add size if available
      if Size > 0 then
      begin
        if Size >= 1073741824 then
          DisplayName := DisplayName + Format(' [%.1f GB]', [Size / 1073741824])
        else if Size >= 1048576 then
          DisplayName := DisplayName + Format(' [%d MB]', [Size div 1048576])
        else
          DisplayName := DisplayName + Format(' [%d KB]', [Size div 1024]);
      end;
      
      DeviceList.AddObject(DisplayName, TObject(i));
    end;
  end;
end;

function TDeviceManager.GetDevicePath(ComboIndex: Integer): string;
var
  DiskNumber: Integer;
begin
  Result := '';
  
  if (ComboIndex >= 0) and (ComboIndex < FDeviceList.Count) then
  begin
    DiskNumber := Integer(FDeviceList.Objects[ComboIndex]);
    Result := Format('\\?\PhysicalDrive%d', [DiskNumber]);
  end;
end;

function TDeviceManager.GetPhysicalDiskSize(const DevicePath: string): Int64;
var
  DiskNumber: Integer;
  h: THandle;
begin
  Result := 0;
  
  // Extract disk number from path
  DiskNumber := ExtractDiskNumber(DevicePath);
  
  if DiskNumber >= 0 then
  begin
    // Try WMI first (doesn't require admin rights)
    Result := GetDiskSizeViaWMI(DiskNumber);
    if Result > 0 then
      Exit;
  end;
  
  // Fall back to direct disk access (requires admin rights)
  h := CreateFile(PChar(DevicePath), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if h <> INVALID_HANDLE_VALUE then
  begin
    try
      Result := GetDiskSize(h);
    finally
      CloseHandle(h);
    end;
  end;
end;

function TDeviceManager.ExtractDiskNumber(const DevicePath: string): Integer;
var
  i: Integer;
  NumStr: string;
begin
  Result := -1;
  NumStr := '';
  
  // Extract number from paths like "\\?\PhysicalDrive2"
  for i := Length(DevicePath) downto 1 do
  begin
    if DevicePath[i] in ['0'..'9'] then
      NumStr := DevicePath[i] + NumStr
    else if NumStr <> '' then
      Break;
  end;
  
  if NumStr <> '' then
    Result := StrToIntDef(NumStr, -1);
end;

function TDeviceManager.GetFullDiskInfoWMI(DiskNumber: Integer; out Info: TFullDiskInfo): Boolean;
var
  Locator, WMIService, DiskDrive, Disks: OleVariant;
  Query: string;
  Enum: IEnumVariant;
  Value: OleVariant;
  Fetched: Cardinal;
  PNPDeviceID: string;
  PosVID, PosPID: Integer;
  Retry: Integer;
  RetryDelay: Integer;
begin
  Result := False;
  FillChar(Info, SizeOf(Info), 0);
  
  // Retry logic for newly connected devices (WMI may not have updated yet)
  RetryDelay := 500; // ms between retries
  for Retry := 1 to 5 do
  begin
    try
      Locator := CreateOleObject('WbemScripting.SWbemLocator');
      WMIService := Locator.ConnectServer('.', 'root\CIMV2', '', '');
      if VarIsNull(WMIService) or VarIsEmpty(WMIService) then
      begin
        if Retry < 5 then
        begin
          Sleep(RetryDelay);
          Continue;
        end;
        Exit;
      end;

      Query := Format('SELECT * FROM Win32_DiskDrive WHERE DeviceID="\\\\.\\PHYSICALDRIVE%d"', [DiskNumber]);
      Disks := WMIService.ExecQuery(Query);

      if VarIsNull(Disks) or VarIsEmpty(Disks) then
      begin
        if Retry < 5 then
        begin
          TLogUtils.Log(Format('WMI: Query returned null/empty (attempt %d/5)', [Retry]), llDebug);
          Sleep(RetryDelay);
          Continue;
        end;
        TLogUtils.Log('WMI: Query failed after 5 attempts - no data', llWarning);
        Exit;
      end;

      Enum := IUnknown(Disks._NewEnum) as IEnumVariant;
      
      if Enum.Next(1, Value, Fetched) = S_OK then
      begin
        try
          DiskDrive := Value;
          
          if not VarIsNull(DiskDrive.Model) then
            Info.Model := Trim(VarToStr(DiskDrive.Model));
          if not VarIsNull(DiskDrive.Manufacturer) then
            Info.Manufacturer := Trim(VarToStr(DiskDrive.Manufacturer));
          if not VarIsNull(DiskDrive.SerialNumber) then
            Info.SerialNumber := Trim(VarToStr(DiskDrive.SerialNumber));
          if not VarIsNull(DiskDrive.FirmwareRevision) then
            Info.FirmwareRevision := Trim(VarToStr(DiskDrive.FirmwareRevision));
          if not VarIsNull(DiskDrive.Size) then
            Info.Size := StrToInt64Def(VarToStr(DiskDrive.Size), 0);
          if not VarIsNull(DiskDrive.InterfaceType) then
            Info.InterfaceType := VarToStr(DiskDrive.InterfaceType);
          if not VarIsNull(DiskDrive.MediaType) then
            Info.MediaType := VarToStr(DiskDrive.MediaType);
          if not VarIsNull(DiskDrive.BytesPerSector) then
            Info.BytesPerSector := StrToIntDef(VarToStr(DiskDrive.BytesPerSector), 0);
          if not VarIsNull(DiskDrive.SectorsPerTrack) then
            Info.SectorsPerTrack := StrToIntDef(VarToStr(DiskDrive.SectorsPerTrack), 0);
            
          if not VarIsNull(DiskDrive.PNPDeviceID) then
          begin
            PNPDeviceID := VarToStr(DiskDrive.PNPDeviceID);
            Info.PNPDeviceID := PNPDeviceID;
            
            // Parse VID&PID for USB devices
            PosVID := Pos('VID_', UpperCase(PNPDeviceID));
            PosPID := Pos('PID_', UpperCase(PNPDeviceID));
            if (PosVID > 0) and (PosPID > 0) then
            begin
              Info.USBVID := Copy(PNPDeviceID, PosVID + 4, 4);
              Info.USBPID := Copy(PNPDeviceID, PosPID + 4, 4);
            end;
          end;
          
          Result := True;
          Break; // Success - exit retry loop
        finally
          Value := Unassigned;
        end;
      end
      else
      begin
        // No data returned - device may not be ready yet
        if Retry < 5 then
        begin
          TLogUtils.Log(Format('WMI: No records in result set (attempt %d/5)', [Retry]), llDebug);
          Sleep(RetryDelay);
        end
        else
        begin
          TLogUtils.Log('WMI: No records after 5 attempts', llWarning);
          Exit;
        end;
      end;
    except
      on E: Exception do
      begin
        // WMI exception - retry if not last attempt
        if Retry < 5 then
        begin
          TLogUtils.Log(Format('WMI: Exception on attempt %d/5: %s', [Retry, E.Message]), llDebug);
          Sleep(RetryDelay);
        end
        else
        begin
          // Last retry failed - return False
          TLogUtils.Log(Format('WMI: Failed after 5 attempts - %s', [E.Message]), llWarning);
          Result := False;
          Exit;
        end;
      end;
    end;
  end;
end;

end.
