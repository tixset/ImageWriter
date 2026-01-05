{******************************************************************************}
{                                                                              }
{  ImageWriter - Device Info Helper Unit                                      }
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
{    Device information display utilities.                                    }
{    Provides formatted device info for UI display.                           }
{                                                                              }
{******************************************************************************}

unit DeviceInfoHelper;

interface

uses
  Windows, SysUtils, Classes, WinIOCTL;

type
  TLogCallback = procedure(const Msg: string; Level: Integer) of object;
  
  TDeviceInfoHelper = class
  private
    FOnLog: TLogCallback;
    procedure Log(const Msg: string; Level: Integer = 1);
    class function MakeProgressBar(Percent: Integer; Width: Integer = 20): string;
  public
    constructor Create(OnLog: TLogCallback);
    
    // Display filesystem information for mounted drives
    procedure ShowMountedFilesystems(const DriveLetters: string);
    
    // Format bytes to human-readable format
    class function BytesToHuman(Bytes: Int64): string;
  end;

implementation

constructor TDeviceInfoHelper.Create(OnLog: TLogCallback);
begin
  inherited Create;
  FOnLog := OnLog;
end;

procedure TDeviceInfoHelper.Log(const Msg: string; Level: Integer);
begin
  if Assigned(FOnLog) then
    FOnLog(Msg, Level);
end;

procedure TDeviceInfoHelper.ShowMountedFilesystems(const DriveLetters: string);
var
  i: Integer;
  drive: Char;
  volumeName: array[0..MAX_PATH] of Char;
  fileSystemName: array[0..MAX_PATH] of Char;
  serialNumber, maxComponentLen, fileSystemFlags: DWORD;
  sectorsPerCluster, bytesPerSector, freeClusters, totalClusters: DWORD;
  freeBytes, totalBytes, totalFreeBytes: Int64;
  usedPercent: Integer;
begin
  Log('=== Mounted Filesystems ===', 1);
  for i := 1 to Length(DriveLetters) do
  begin
    drive := DriveLetters[i];
    // Skip non-alphabetic characters (like commas)
    if not (drive in ['A'..'Z', 'a'..'z']) then
      Continue;

    if GetVolumeInformation(PChar(drive + ':\'), volumeName, SizeOf(volumeName), 
       @serialNumber, maxComponentLen, fileSystemFlags, fileSystemName, SizeOf(fileSystemName)) then
    begin
      if Trim(volumeName) <> '' then
        Log('Drive ' + drive + ': "' + volumeName + '"', 1)
      else
        Log('Drive ' + drive + ': (No label)', 1);
      Log('  Filesystem: ' + fileSystemName + ' (Serial: ' + 
          Copy(IntToHex(serialNumber, 8), 1, 4) + '-' + 
          Copy(IntToHex(serialNumber, 8), 5, 4) + ')', 1);
      
      // Get cluster size
      if GetDiskFreeSpace(PChar(drive + ':\'), sectorsPerCluster, bytesPerSector, freeClusters, totalClusters) then
        Log('  Cluster size: ' + IntToStr(sectorsPerCluster * bytesPerSector) + ' bytes', 1);
      
      // Get usage percentage
      if GetDiskFreeSpaceEx(PChar(drive + ':\'), freeBytes, totalBytes, @totalFreeBytes) then
      begin
        if totalBytes > 0 then
        begin
          usedPercent := Round(((totalBytes - freeBytes) / totalBytes) * 100);
          Log(Format('  Usage: %s used / %s total (%d%%)', 
              [BytesToHuman(totalBytes - freeBytes), BytesToHuman(totalBytes), usedPercent]), 1);
          Log('         ' + MakeProgressBar(usedPercent), 1);
        end;
      end;
      
      // Filesystem flags
      if (fileSystemFlags and $00000002) <> 0 then
        Log('  Case preserved: Yes', 0)  // 0 = llDebug
      else
        Log('  Case preserved: No', 0);

      if (fileSystemFlags and $00080000) <> 0 then
        Log('  Read-only: Yes', 2)  // 2 = llWarning
      else
        Log('  Read-only: No', 0);
    end
    else
      Log('  Drive ' + drive + ': - Unable to get volume information (Error: ' + 
          IntToStr(GetLastError) + ')', 2);
  end;

  Log('=================================' + #13#10, 1);
end;

class function TDeviceInfoHelper.BytesToHuman(Bytes: Int64): string;
const
  KB = 1024;
  MB = 1024 * KB;
  GB = 1024 * MB;
  TB = Int64(1024) * GB;
begin
  if Bytes >= TB then
    Result := Format('%.2f TB', [Bytes / TB])
  else if Bytes >= GB then
    Result := Format('%.2f GB', [Bytes / GB])
  else if Bytes >= MB then
    Result := Format('%.2f MB', [Bytes / MB])
  else if Bytes >= KB then
    Result := Format('%.2f KB', [Bytes / KB])
  else
    Result := Format('%d bytes', [Bytes]);
end;

class function TDeviceInfoHelper.MakeProgressBar(Percent: Integer; Width: Integer): string;
var
  filled, empty, i: Integer;
begin
  // Clamp percent to 0-100
  if Percent < 0 then Percent := 0;
  if Percent > 100 then Percent := 100;
  
  // Calculate filled and empty parts
  filled := (Percent * Width) div 100;
  empty := Width - filled;
  
  // Build progress bar: [##########..........] 50%
  Result := '[';
  for i := 1 to filled do
    Result := Result + '#';
  for i := 1 to empty do
    Result := Result + '.';
  Result := Result + '] ' + IntToStr(Percent) + '%';
end;

end.
