{******************************************************************************}
{                                                                              }
{  ImageWriter - Worker Commands Unit                                         }
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
{    Command definitions and structures for worker process communication.     }
{                                                                              }
{******************************************************************************}

unit WorkerCommands;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, SysUtils, Classes, IPCPipe, ActiveX, ComObj, Variants;

// Process commands in persistent worker mode
procedure ProcessPersistentCommands(Pipe: TIPCPipeServer);

implementation

uses
  WinIOCTL, volume, BinFile, Native;

// Lock volume with elevated privileges
function LockVolumeWorker(const VolumePath: string): Boolean;
var
  VolumeHandle: THandle;
  BytesReturned: DWORD;
  Retries: Integer;
begin
  Result := False;
  
  // Open volume with admin rights
  VolumeHandle := CreateFile(PChar(VolumePath), GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  
  if VolumeHandle = INVALID_HANDLE_VALUE then
    Exit;
  
  try
    // Try to lock volume (retry up to 10 times)
    Retries := 0;
    while Retries < 10 do
    begin
      if DeviceIoControl(VolumeHandle, FSCTL_LOCK_VOLUME, nil, 0, nil, 0, BytesReturned, nil) then
      begin
        Result := True;
        Exit;
      end;
      Inc(Retries);
      Sleep(500);
    end;
  finally
    CloseHandle(VolumeHandle);
  end;
end;

// Unlock volume with elevated privileges
function UnlockVolumeWorker(const VolumePath: string): Boolean;
var
  VolumeHandle: THandle;
  BytesReturned: DWORD;
begin
  Result := False;
  
  // Open volume with admin rights
  VolumeHandle := CreateFile(PChar(VolumePath), GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  
  if VolumeHandle = INVALID_HANDLE_VALUE then
    Exit;
  
  try
    Result := DeviceIoControl(VolumeHandle, FSCTL_UNLOCK_VOLUME, nil, 0, nil, 0, BytesReturned, nil);
  finally
    CloseHandle(VolumeHandle);
  end;
end;

// Dismount volume with elevated privileges
function DismountVolumeWorker(const VolumePath: string): Boolean;
var
  VolumeHandle: THandle;
  BytesReturned: DWORD;
begin
  Result := False;
  
  // Open volume with admin rights
  VolumeHandle := CreateFile(PChar(VolumePath), GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  
  if VolumeHandle = INVALID_HANDLE_VALUE then
    Exit;
  
  try
    Result := DeviceIoControl(VolumeHandle, FSCTL_DISMOUNT_VOLUME, nil, 0, nil, 0, BytesReturned, nil);
  finally
    CloseHandle(VolumeHandle);
  end;
end;

// Get disk size
function GetPhysicalDiskSizeWorker(const DevicePath: string): Int64;
var
  h: THandle;
begin
  Result := 0;
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

// Get WMI disk information
function GetDiskInfoWMIWorker(DiskNumber: Integer): string;
var
  Locator, WMIService, DiskDrive, Disks: OleVariant;
  Query: string;
  Enum: IEnumVariant;
  Value: OleVariant;
  Fetched: Cardinal;
  PNPDeviceID, VID, PID: string;
  PosVID, PosPID: Integer;
  Result_: TStringList;
begin
  Result := '';
  Result_ := TStringList.Create;
  try
    CoInitialize(nil);
    try
      // Connect to WMI
      Locator := CreateOleObject('WbemScripting.SWbemLocator');
      WMIService := Locator.ConnectServer('.', 'root\CIMV2', '', '');
      if VarIsNull(WMIService) or VarIsEmpty(WMIService) then
        Exit;

      // Query Win32_DiskDrive by disk number
      Query := Format('SELECT * FROM Win32_DiskDrive WHERE DeviceID="\\\\.\\PHYSICALDRIVE%d"', [DiskNumber]);
      Disks := WMIService.ExecQuery(Query);
      
      if VarIsNull(Disks) or VarIsEmpty(Disks) then
        Exit;

      // Get enumerator
      Enum := IUnknown(Disks._NewEnum) as IEnumVariant;
      
      // Iterate results
      while Enum.Next(1, Value, Fetched) = S_OK do
      begin
        try
          DiskDrive := Value;
          
          Result_.Add('=== Device Information ===');
          Result_.Add('');
          
          // Hardware
          Result_.Add('Hardware:');
          if not VarIsNull(DiskDrive.Model) then
            Result_.Add('  Model: ' + Trim(VarToStr(DiskDrive.Model)));
          if not VarIsNull(DiskDrive.Manufacturer) then
            Result_.Add('  Manufacturer: ' + Trim(VarToStr(DiskDrive.Manufacturer)));
          if not VarIsNull(DiskDrive.SerialNumber) then
            Result_.Add('  Serial Number: ' + Trim(VarToStr(DiskDrive.SerialNumber)));
          if not VarIsNull(DiskDrive.FirmwareRevision) then
            Result_.Add('  Firmware: ' + Trim(VarToStr(DiskDrive.FirmwareRevision)));
          if not VarIsNull(DiskDrive.Size) then
            Result_.Add('  Capacity: ' + VarToStr(DiskDrive.Size));
          
          Result_.Add('');
          
          // Connection
          Result_.Add('Connection:');
          if not VarIsNull(DiskDrive.InterfaceType) then
            Result_.Add('  Interface: ' + VarToStr(DiskDrive.InterfaceType));
          if not VarIsNull(DiskDrive.MediaType) then
            Result_.Add('  Media Type: ' + VarToStr(DiskDrive.MediaType));
          
          if not VarIsNull(DiskDrive.PNPDeviceID) then
          begin
            PNPDeviceID := VarToStr(DiskDrive.PNPDeviceID);
            Result_.Add('  Device ID: ' + PNPDeviceID);
            
            // Parse VID&PID for USB devices
            PosVID := Pos('VID_', UpperCase(PNPDeviceID));
            PosPID := Pos('PID_', UpperCase(PNPDeviceID));
            if (PosVID > 0) and (PosPID > 0) then
            begin
              VID := Copy(PNPDeviceID, PosVID + 4, 4);
              PID := Copy(PNPDeviceID, PosPID + 4, 4);
              Result_.Add('  USB VID:PID: ' + VID + ':' + PID);
            end;
          end;
          
          Result_.Add('');
          
          // Geometry
          if not VarIsNull(DiskDrive.BytesPerSector) and not VarIsNull(DiskDrive.SectorsPerTrack) then
          begin
            Result_.Add('Geometry:');
            Result_.Add('  Bytes per sector: ' + VarToStr(DiskDrive.BytesPerSector));
            Result_.Add('  Sectors per track: ' + VarToStr(DiskDrive.SectorsPerTrack));
            if not VarIsNull(DiskDrive.TracksPerCylinder) then
              Result_.Add('  Tracks per cylinder: ' + VarToStr(DiskDrive.TracksPerCylinder));
            if not VarIsNull(DiskDrive.TotalCylinders) then
              Result_.Add('  Total cylinders: ' + VarToStr(DiskDrive.TotalCylinders));
            if not VarIsNull(DiskDrive.TotalHeads) then
              Result_.Add('  Total heads: ' + VarToStr(DiskDrive.TotalHeads));
            if not VarIsNull(DiskDrive.TotalSectors) then
              Result_.Add('  Total sectors: ' + VarToStr(DiskDrive.TotalSectors));
            if not VarIsNull(DiskDrive.TotalTracks) then
              Result_.Add('  Total tracks: ' + VarToStr(DiskDrive.TotalTracks));
          end;
          
          Result_.Add('');
          Result_.Add('===========================');
          
        finally
          VarClear(Value);
        end;
      end;
      
    finally
      CoUninitialize;
    end;
    
    Result := Result_.Text;
  finally
    Result_.Free;
  end;
end;

// Read partitions information
function GetPartitionsInfo(const DevicePath: string; DiskSize: Int64): string;
type
  TMBRPartitionEntry = packed record
    BootIndicator: Byte;
    StartHead: Byte;
    StartSector: Byte;
    StartCylinder: Byte;
    PartitionType: Byte;
    EndHead: Byte;
    EndSector: Byte;
    EndCylinder: Byte;
    StartLBA: DWORD;
    SizeInSectors: DWORD;
  end;
  
  TMBRSector = packed record
    BootCode: array[0..445] of Byte;
    Partitions: array[0..3] of TMBRPartitionEntry;
    Signature: Word;
  end;
  
var
  Result_: TStringList;
  MBR: TMBRSector;
  i, validParts: Integer;
  PartStart, PartSize: Int64;
  UsedSpace: Int64;
  unallocatedSpace: Int64;
  unallocatedPercent: Integer;
  DeviceFile: TBinFile;
  BytesRead: Integer;
begin
  Result := '';
  Result_ := TStringList.Create;
  try
    Result_.Add('=== Partition Table Analysis ===');
    
    // Open device
    DeviceFile := TWinBinFile.Create(DevicePath);
    try
      if not DeviceFile.Open then
      begin
        Result_.Add('ERROR: Cannot open device');
        Result := Result_.Text;
        Exit;
      end;
      
      // Read MBR
      DeviceFile.Seek(0);
      FillChar(MBR, SizeOf(MBR), 0);
      BytesRead := DeviceFile.Read(MBR, SizeOf(MBR));
      
      if BytesRead <> SizeOf(MBR) then
      begin
        Result_.Add('ERROR: Failed to read MBR sector');
        Result := Result_.Text;
        Exit;
      end;
      
      // Check MBR signature
      if MBR.Signature <> $AA55 then
      begin
        Result_.Add('Invalid MBR signature');
        Result := Result_.Text;
        Exit;
      end;
      
      // Analyze partitions
      Result_.Add('');
      Result_.Add('MBR Partitions:');
      validParts := 0;
      UsedSpace := 0;
      
      for i := 0 to 3 do
      begin
        if MBR.Partitions[i].PartitionType <> 0 then
        begin
          Inc(validParts);
          PartStart := Int64(MBR.Partitions[i].StartLBA) * 512;
          PartSize := Int64(MBR.Partitions[i].SizeInSectors) * 512;
          UsedSpace := UsedSpace + PartSize;
          
          Result_.Add(Format('  Partition %d: Type=0x%02X, Start=%d MB, Size=%d MB',
            [i + 1, MBR.Partitions[i].PartitionType, 
             PartStart div (1024*1024), PartSize div (1024*1024)]));
        end;
      end;
      
      if validParts = 0 then
        Result_.Add('  No partitions found')
      else
      begin
        Result_.Add('');
        Result_.Add(Format('Total partitions: %d', [validParts]));
        
        // Calculate unallocated space
        if DiskSize > UsedSpace then
        begin
          unallocatedSpace := DiskSize - UsedSpace;
          unallocatedPercent := Round((unallocatedSpace / DiskSize) * 100);
          Result_.Add(Format('Unallocated space: %d MB (%d%% of disk)',
            [unallocatedSpace div (1024*1024), unallocatedPercent]));
        end
        else
          Result_.Add('Unallocated space: None');
      end;
      
    finally
      DeviceFile.Free;
    end;
    
    Result := Result_.Text;
  finally
    Result_.Free;
  end;
end;

// Process commands in persistent mode
procedure ProcessPersistentCommands(Pipe: TIPCPipeServer);
var
  MsgType: TIPCMessageType;
  Data: string;
  Cmd: string;
  Params: TStringList;
  Response: string;
  DiskNum: Integer;
  DiskPath: string;
  DiskSize: Int64;
begin
  Params := TStringList.Create;
  try
    while True do
    begin
      // Wait for command from GUI
      if not Pipe.ReceiveMessage(MsgType, Data, 1000) then
      begin
        // Check if pipe still connected
        if not Pipe.Connected then
          Break;
        Continue;
      end;
      
      case MsgType of
        ipcCommand:
          begin
            // Parse command: "COMMAND|param1|param2|..."
            Params.Clear;
            Params.Delimiter := '|';
            Params.DelimitedText := Data;
            
            if Params.Count = 0 then
              Continue;
            
            Cmd := UpperCase(Params[0]);
            Response := '';
            
            try
              if Cmd = 'GETDISKINFO' then
              begin
                // Get WMI disk information
                if Params.Count >= 2 then
                begin
                  DiskNum := StrToIntDef(Params[1], -1);
                  if DiskNum >= 0 then
                    Response := GetDiskInfoWMIWorker(DiskNum)
                  else
                    Response := 'ERROR: Invalid disk number';
                end
                else
                  Response := 'ERROR: Missing disk number parameter';
              end
              else if Cmd = 'GETPARTITIONS' then
              begin
                // Get partition table information
                if Params.Count >= 3 then
                begin
                  DiskPath := Params[1];
                  DiskSize := StrToInt64Def(Params[2], 0);
                  if DiskSize > 0 then
                    Response := GetPartitionsInfo(DiskPath, DiskSize)
                  else
                    Response := 'ERROR: Invalid disk size';
                end
                else
                  Response := 'ERROR: Missing parameters';
              end
              else if Cmd = 'GETDISKSIZE' then
              begin
                // Get physical disk size
                if Params.Count >= 2 then
                begin
                  DiskPath := Params[1];
                  DiskSize := GetPhysicalDiskSizeWorker(DiskPath);
                  Response := IntToStr(DiskSize);
                end
                else
                  Response := 'ERROR: Missing device path';
              end
              else if Cmd = 'PING' then
              begin
                // Keep-alive ping
                Response := 'PONG';
              end
              else if Cmd = 'LOCKVOLUME' then
              begin
                // Lock volume with elevated privileges
                if Params.Count >= 2 then
                begin
                  if LockVolumeWorker(Params[1]) then
                    Response := 'OK'
                  else
                    Response := 'ERROR: Failed to lock volume';
                end
                else
                  Response := 'ERROR: Missing volume path';
              end
              else if Cmd = 'UNLOCKVOLUME' then
              begin
                // Unlock volume with elevated privileges
                if Params.Count >= 2 then
                begin
                  if UnlockVolumeWorker(Params[1]) then
                    Response := 'OK'
                  else
                    Response := 'ERROR: Failed to unlock volume';
                end
                else
                  Response := 'ERROR: Missing volume path';
              end
              else if Cmd = 'DISMOUNTVOLUME' then
              begin
                // Dismount volume with elevated privileges
                if Params.Count >= 2 then
                begin
                  if DismountVolumeWorker(Params[1]) then
                    Response := 'OK'
                  else
                    Response := 'ERROR: Failed to dismount volume';
                end
                else
                  Response := 'ERROR: Missing volume path';
              end
              else
                Response := 'ERROR: Unknown command: ' + Cmd;
                
            except
              on E: Exception do
                Response := 'ERROR: ' + E.Message;
            end;
            
            // Send response
            Pipe.SendMessage(ipcResponse, Response);
          end;
          
        ipcShutdown:
          begin
            // Shutdown request from GUI
            Pipe.SendMessage(ipcResponse, 'OK');
            Break;
          end;
          
        ipcCancel:
          begin
            // Ignore cancel in command mode
          end;
      end;
    end;
  finally
    Params.Free;
  end;
end;

end.
