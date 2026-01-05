{******************************************************************************}
{                                                                              }
{  ImageWriter - Partition Analyzer Unit                                      }
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
{    Partition table parsing for MBR and GPT formats.                         }
{    Analyzes disk images to detect partition structure.                     }
{                                                                              }
{******************************************************************************}

unit PartitionAnalyzer;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, SysUtils, Classes, PartitionInfo;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError);
  TLogCallback = procedure(S: string; Level: TLogLevel = llInfo) of object;
  
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

  TGPTHeader = packed record
    Signature: array[0..7] of AnsiChar;
    Revision: DWORD;
    HeaderSize: DWORD;
    HeaderCRC32: DWORD;
    Reserved: DWORD;
    CurrentLBA: Int64;
    BackupLBA: Int64;
    FirstUsableLBA: Int64;
    LastUsableLBA: Int64;
    DiskGUID: TGUID;
    PartitionEntryLBA: Int64;
    NumberOfPartitionEntries: DWORD;
    SizeOfPartitionEntry: DWORD;
    PartitionArrayCRC32: DWORD;
  end;

  TGPTPartitionEntry = packed record
    PartitionTypeGUID: TGUID;
    UniquePartitionGUID: TGUID;
    StartingLBA: Int64;
    EndingLBA: Int64;
    Attributes: Int64;
    PartitionName: array[0..35] of WideChar;
  end;

  TPartitionAnalyzer = class
  private
    FLogCallback: TLogCallback;
    procedure Log(const Msg: string; Level: TLogLevel = llInfo);
    function ReadDiskSector(const DevicePath: string; SectorNumber: Int64; var Buffer): Boolean;
    function DetectFilesystemType(const DevicePath: string; StartLBA: Int64): string;
  public
    constructor Create(LogCallback: TLogCallback);
    
    // Partition type name lookups
    function GetPartitionTypeName(PartType: Byte): string;
    function GetGPTPartitionTypeName(const GUID: TGUID): string;
    function GetGPTAttributesString(Attributes: Int64): string;
    function GUIDToString(const GUID: TGUID): string;
    
    // Main analysis method
    procedure ShowAllPartitions(const DevicePath: string; DiskSize: Int64);
    
    // Validation method
    function ValidatePartitionTable(const DevicePath: string; DiskSize: Int64; out ErrorMessage: string): Boolean;
  end;

function BytesToHuman(Bytes: Int64): string;

implementation

function BytesToHuman(Bytes: Int64): string;
const
  KB = 1024;
  MB = KB * 1024;
  GB = MB * 1024;
  TB = Int64(GB) * 1024;
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
    Result := IntToStr(Bytes) + ' bytes';
end;

constructor TPartitionAnalyzer.Create(LogCallback: TLogCallback);
begin
  inherited Create;
  FLogCallback := LogCallback;
end;

procedure TPartitionAnalyzer.Log(const Msg: string; Level: TLogLevel = llInfo);
begin
  if Assigned(FLogCallback) then
    FLogCallback(Msg, Level);
end;

function TPartitionAnalyzer.ReadDiskSector(const DevicePath: string; SectorNumber: Int64; var Buffer): Boolean;
var
  hDevice: THandle;
  bytesRead: DWORD;
  offset: Int64;
  lowOffset, highOffset: DWORD;
begin
  Result := False;
  
  hDevice := CreateFile(PChar(DevicePath), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
                        nil, OPEN_EXISTING, 0, 0);
  if hDevice = INVALID_HANDLE_VALUE then
    Exit;
    
  try
    offset := SectorNumber * 512;
    lowOffset := DWORD(offset and $FFFFFFFF);
    highOffset := DWORD((offset shr 32) and $FFFFFFFF);
    
    if SetFilePointer(hDevice, Integer(lowOffset), @highOffset, FILE_BEGIN) = $FFFFFFFF then
      if GetLastError <> NO_ERROR then
        Exit;
      
    Result := ReadFile(hDevice, Buffer, 512, bytesRead, nil) and (bytesRead = 512);
  finally
    CloseHandle(hDevice);
  end;
end;

function TPartitionAnalyzer.DetectFilesystemType(const DevicePath: string; StartLBA: Int64): string;
var
  Buffer: array[0..4095] of Byte;
  FSType: array[0..7] of AnsiChar;
begin
  Result := 'Unknown';
  
  if not ReadDiskSector(DevicePath, StartLBA, Buffer) then
    Exit;
    
  // Check for NTFS
  Move(Buffer[$03], FSType, 8);
  if Copy(string(FSType), 1, 4) = 'NTFS' then
  begin
    Result := 'NTFS';
    Exit;
  end;
  
  // Check for FAT32
  Move(Buffer[$52], FSType, 8);
  if Copy(string(FSType), 1, 5) = 'FAT32' then
  begin
    Result := 'FAT32';
    Exit;
  end;
  
  // Check for FAT16
  Move(Buffer[$36], FSType, 8);
  if (Copy(string(FSType), 1, 3) = 'FAT') or (Copy(string(FSType), 1, 6) = 'FAT16') then
  begin
    Result := 'FAT16';
    Exit;
  end;
  
  // Check for exFAT
  Move(Buffer[$03], FSType, 8);
  if Copy(string(FSType), 1, 5) = 'EXFAT' then
  begin
    Result := 'exFAT';
    Exit;
  end;
  
  // Check for ext2/3/4 - skip, requires reading multiple sectors
end;

function TPartitionAnalyzer.GUIDToString(const GUID: TGUID): string;
begin
  Result := Format('{%.8X-%.4X-%.4X-%.2X%.2X-%.2X%.2X%.2X%.2X%.2X%.2X}',
    [GUID.D1, GUID.D2, GUID.D3, GUID.D4[0], GUID.D4[1], GUID.D4[2], GUID.D4[3],
     GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]]);
end;

function TPartitionAnalyzer.GetPartitionTypeName(PartType: Byte): string;
begin
  case PartType of
    $00: Result := 'Empty';
    $01: Result := 'FAT12';
    $04: Result := 'FAT16 <32MB';
    $05: Result := 'Extended';
    $06: Result := 'FAT16';
    $07: Result := 'NTFS/exFAT';
    $0B: Result := 'FAT32';
    $0C: Result := 'FAT32 LBA';
    $0E: Result := 'FAT16 LBA';
    $0F: Result := 'Extended LBA';
    $11: Result := 'Hidden FAT12';
    $12: Result := 'Compaq diagnostics';
    $14: Result := 'Hidden FAT16 <32MB';
    $16: Result := 'Hidden FAT16';
    $17: Result := 'Hidden NTFS';
    $1B: Result := 'Hidden FAT32';
    $1C: Result := 'Hidden FAT32 LBA';
    $1E: Result := 'Hidden FAT16 LBA';
    $27: Result := 'Windows RE';
    $42: Result := 'Windows Dynamic';
    $82: Result := 'Linux swap / Solaris';
    $83: Result := 'Linux';
    $84: Result := 'Hibernation';
    $85: Result := 'Linux Extended';
    $86: Result := 'NTFS volume set';
    $87: Result := 'NTFS volume set';
    $8E: Result := 'Linux LVM';
    $A0: Result := 'Hibernation';
    $A5: Result := 'FreeBSD';
    $A6: Result := 'OpenBSD';
    $A8: Result := 'Mac OS X';
    $A9: Result := 'NetBSD';
    $AB: Result := 'Mac OS X Boot';
    $AF: Result := 'Mac OS X HFS+';
    $B7: Result := 'BSDI';
    $B8: Result := 'BSDI swap';
    $BE: Result := 'Solaris Boot';
    $BF: Result := 'Solaris';
    $C1: Result := 'DR-DOS FAT12';
    $C4: Result := 'DR-DOS FAT16';
    $C6: Result := 'DR-DOS FAT16';
    $EB: Result := 'BeOS';
    $EE: Result := 'GPT Protective MBR';
    $EF: Result := 'EFI System';
    $FB: Result := 'VMware VMFS';
    $FC: Result := 'VMware swap';
    $FD: Result := 'Linux RAID';
  else
    Result := Format('Unknown (0x%.2X)', [PartType]);
  end;
end;

function TPartitionAnalyzer.GetGPTPartitionTypeName(const GUID: TGUID): string;
var
  GUIDStr: string;
begin
  GUIDStr := UpperCase(GUIDToString(GUID));
  
  // Unused/Empty
  if GUIDStr = '{00000000-0000-0000-0000-000000000000}' then
  begin
    Result := 'Unused';
    Exit;
  end;
  
  // EFI System
  if GUIDStr = '{C12A7328-F81F-11D2-BA4B-00A0C93EC93B}' then
  begin
    Result := 'EFI System Partition';
    Exit;
  end;
  
  // Microsoft Reserved (MSR)
  if GUIDStr = '{E3C9E316-0B5C-4DB8-817D-F92DF00215AE}' then
  begin
    Result := 'Microsoft Reserved (MSR)';
    Exit;
  end;
  
  // Microsoft Basic Data
  if GUIDStr = '{EBD0A0A2-B9E5-4433-87C0-68B6B72699C7}' then
  begin
    Result := 'Microsoft Basic Data';
    Exit;
  end;
  
  // Windows Recovery
  if GUIDStr = '{DE94BBA4-06D1-4D40-A16A-BFD50179D6AC}' then
  begin
    Result := 'Windows Recovery Environment';
    Exit;
  end;
  
  // Linux filesystem
  if GUIDStr = '{0FC63DAF-8483-4772-8E79-3D69D8477DE4}' then
  begin
    Result := 'Linux filesystem';
    Exit;
  end;
  
  // Linux swap
  if GUIDStr = '{0657FD6D-A4AB-43C4-84E5-0933C84B4F4F}' then
  begin
    Result := 'Linux swap';
    Exit;
  end;
  
  // Linux LVM
  if GUIDStr = '{E6D6D379-F507-44C2-A23C-238F2A3DF928}' then
  begin
    Result := 'Linux LVM';
    Exit;
  end;
  
  // Linux RAID
  if GUIDStr = '{A19D880F-05FC-4D3B-A006-743F0F84911E}' then
  begin
    Result := 'Linux RAID';
    Exit;
  end;
  
  // Linux extended boot
  if GUIDStr = '{BC13C2FF-59E6-4262-A352-B275FD6F7172}' then
  begin
    Result := 'Linux /boot';
    Exit;
  end;
  
  // Linux /home
  if GUIDStr = '{933AC7E1-2EB4-4F13-B844-0E14E2AEF915}' then
  begin
    Result := 'Linux /home';
    Exit;
  end;
  
  // Linux /srv
  if GUIDStr = '{3B8F8425-20E0-4F3B-907F-1A25A76F98E8}' then
  begin
    Result := 'Linux /srv';
    Exit;
  end;
  
  // Linux dm-crypt
  if GUIDStr = '{7FFEC5C9-2D00-49B7-8941-3EA10A5586B7}' then
  begin
    Result := 'Linux dm-crypt';
    Exit;
  end;
  
  // Linux LUKS
  if GUIDStr = '{CA7D7CCB-63ED-4C53-861C-1742536059CC}' then
  begin
    Result := 'Linux LUKS';
    Exit;
  end;
  
  // FreeBSD Boot
  if GUIDStr = '{83BD6B9D-7F41-11DC-BE0B-001560B84F0F}' then
  begin
    Result := 'FreeBSD Boot';
    Exit;
  end;
  
  // FreeBSD Data
  if GUIDStr = '{516E7CB4-6ECF-11D6-8FF8-00022D09712B}' then
  begin
    Result := 'FreeBSD Data';
    Exit;
  end;
  
  // FreeBSD Swap
  if GUIDStr = '{516E7CB5-6ECF-11D6-8FF8-00022D09712B}' then
  begin
    Result := 'FreeBSD Swap';
    Exit;
  end;
  
  // Apple HFS+
  if GUIDStr = '{48465300-0000-11AA-AA11-00306543ECAC}' then
  begin
    Result := 'Apple HFS+';
    Exit;
  end;
  
  // Apple APFS
  if GUIDStr = '{7C3457EF-0000-11AA-AA11-00306543ECAC}' then
  begin
    Result := 'Apple APFS';
    Exit;
  end;
  
  // Apple Boot
  if GUIDStr = '{426F6F74-0000-11AA-AA11-00306543ECAC}' then
  begin
    Result := 'Apple Boot';
    Exit;
  end;
  
  Result := 'Unknown';
end;

function TPartitionAnalyzer.GetGPTAttributesString(Attributes: Int64): string;
var
  AttrList: TStringList;
begin
  AttrList := TStringList.Create;
  try
    if (Attributes and $0000000000000001) <> 0 then
      AttrList.Add('Required');
    if (Attributes and $0000000000000002) <> 0 then
      AttrList.Add('No Block IO');
    if (Attributes and $0000000000000004) <> 0 then
      AttrList.Add('Legacy BIOS Bootable');
    if (Attributes and $1000000000000000) <> 0 then
      AttrList.Add('Read-only');
    if (Attributes and $4000000000000000) <> 0 then
      AttrList.Add('Hidden');
    if (Attributes and $8000000000000000) <> 0 then
      AttrList.Add('Do not automount');
      
    if AttrList.Count > 0 then
      Result := AttrList.CommaText
    else
      Result := 'None';
  finally
    AttrList.Free;
  end;
end;

procedure TPartitionAnalyzer.ShowAllPartitions(const DevicePath: string; DiskSize: Int64);
var
  MBR: TMBRSector;
  GPTHeader: TGPTHeader;
  GPTEntry: TGPTPartitionEntry;
  i, validParts: Integer;
  PartStart, PartSize: Int64;
  UsedSpace: Int64;
  unallocatedSpace: Int64;
  unallocatedPercent: Integer;
  PartitionSector: array[0..511] of Byte;
  IsGPT: Boolean;
  PartName: WideString;
  EmptyGUID: TGUID;
  SectorOffset: Int64;
  EntryOffset: Integer;
begin
  Log('=== Partition Table Analysis ===', llInfo);
  
  // Read MBR
  if not ReadDiskSector(DevicePath, 0, MBR) then
  begin
    Log('Failed to read MBR sector', llError);
    Exit;
  end;
  
  // Check MBR signature
  if MBR.Signature <> $AA55 then
  begin
    Log('Invalid MBR signature', llWarning);
    Exit;
  end;
  
  // Check if GPT or MBR
  IsGPT := (MBR.Partitions[0].PartitionType = $EE);

  if IsGPT then
  begin
    Log('Partition table type: GPT', llInfo);
    
    // Read GPT header (sector 1)
    if not ReadDiskSector(DevicePath, 1, GPTHeader) then
    begin
      Log('Failed to read GPT header', llError);
      Exit;
    end;
    
    // Check GPT signature
    if Copy(string(GPTHeader.Signature), 1, 8) <> 'EFI PART' then
    begin
      Log('Invalid GPT signature', llWarning);
      Exit;
    end;

    Log('GPT Disk GUID: ' + GUIDToString(GPTHeader.DiskGUID), llInfo);
    Log('GPT partition slots: ' + IntToStr(GPTHeader.NumberOfPartitionEntries) + ' (max capacity)', llDebug);

    // Validate GPT header to prevent overflow
    if GPTHeader.NumberOfPartitionEntries > 256 then
    begin
      Log('WARNING: Too many GPT entries (' + IntToStr(GPTHeader.NumberOfPartitionEntries) + '), limiting to 256', llWarning);
      GPTHeader.NumberOfPartitionEntries := 256;
    end;

    if GPTHeader.SizeOfPartitionEntry < 128 then
    begin
      Log('ERROR: Invalid GPT partition entry size (' + IntToStr(GPTHeader.SizeOfPartitionEntry) + ')', llError);
      Exit;
    end;

    FillChar(EmptyGUID, SizeOf(EmptyGUID), 0);
    validParts := 0;
    UsedSpace := 0;
    
    // Read GPT partition entries (usually start from sector 2)
    for i := 0 to Integer(GPTHeader.NumberOfPartitionEntries) - 1 do
    begin
      SectorOffset := GPTHeader.PartitionEntryLBA + (Cardinal(i) * GPTHeader.SizeOfPartitionEntry) div 512;
      EntryOffset := (Cardinal(i) * GPTHeader.SizeOfPartitionEntry) mod 512;
      
      // Check buffer boundaries
      if EntryOffset + SizeOf(TGPTPartitionEntry) > 512 then
      begin
        Log('WARNING: GPT entry spans sectors at index ' + IntToStr(i) + ', skipping', llWarning);
        Continue;
      end;
      
      if not ReadDiskSector(DevicePath, SectorOffset, PartitionSector) then
      begin
        Log('WARNING: Failed to read GPT sector ' + IntToStr(SectorOffset), llWarning);
        Continue;
      end;

      Move(PartitionSector[EntryOffset], GPTEntry, SizeOf(TGPTPartitionEntry));
      
      // Skip empty entries
      if CompareMem(@GPTEntry.PartitionTypeGUID, @EmptyGUID, SizeOf(TGUID)) then
        Continue;

      Inc(validParts);
      PartStart := Int64(GPTEntry.StartingLBA) * 512;
      PartSize := (Int64(GPTEntry.EndingLBA) - Int64(GPTEntry.StartingLBA) + 1) * 512;
      UsedSpace := UsedSpace + PartSize;

      PartName := WideCharToString(GPTEntry.PartitionName);
      if PartName = '' then
        PartName := 'Unnamed';

      Log('', llInfo);
      Log(Format('Partition %d: "%s"', [validParts, PartName]), llInfo);
      Log(Format('  Type: %s', [GetGPTPartitionTypeName(GPTEntry.PartitionTypeGUID)]), llInfo);
      Log(Format('  Type GUID: %s', [GUIDToString(GPTEntry.PartitionTypeGUID)]), llDebug);
      Log(Format('  Partition GUID (PARTUUID): %s', [GUIDToString(GPTEntry.UniquePartitionGUID)]), llInfo);
      Log(Format('  Attributes: %s', [GetGPTAttributesString(GPTEntry.Attributes)]), llInfo);
      Log(Format('  Location: Start %s, Size %s', [BytesToHuman(PartStart), BytesToHuman(PartSize)]), llInfo);
      Log(Format('  LBA Range: %d - %d', [GPTEntry.StartingLBA, GPTEntry.EndingLBA]), llDebug);
    end;

    Log('', llInfo);
    Log('Summary:', llInfo);
    Log('  Total partitions: ' + IntToStr(validParts), llInfo);
  end
  else
  begin
    Log('Partition table type: MBR', llInfo);

    validParts := 0;
    UsedSpace := 0;

    for i := 0 to 3 do
    begin
      if MBR.Partitions[i].PartitionType = $00 then
        Continue;

      Inc(validParts);
      PartStart := Int64(MBR.Partitions[i].StartLBA) * 512;
      PartSize := Int64(MBR.Partitions[i].SizeInSectors) * 512;
      UsedSpace := UsedSpace + PartSize;

      Log('', llInfo);
      Log(Format('Partition %d: %s', [validParts, GetPartitionTypeName(MBR.Partitions[i].PartitionType)]), llInfo);
      if MBR.Partitions[i].BootIndicator = $80 then
        Log('  Bootable: Yes', llInfo)
      else
        Log('  Bootable: No', llDebug);
      Log(Format('  Start: %s, Size: %s', [BytesToHuman(PartStart), BytesToHuman(PartSize)]), llInfo);
      Log(Format('  LBA: %d, Sectors: %d', [MBR.Partitions[i].StartLBA, MBR.Partitions[i].SizeInSectors]), llDebug);
      Log('  Filesystem: ' + DetectFilesystemType(DevicePath, MBR.Partitions[i].StartLBA), llInfo);
    end;

    Log('', llInfo);
    Log('Summary:', llInfo);
    Log('  Total partitions: ' + IntToStr(validParts), llInfo);
  end;
  
  // Show unallocated space
  if DiskSize > UsedSpace then
  begin
    unallocatedSpace := DiskSize - UsedSpace;
    unallocatedPercent := Round((unallocatedSpace / DiskSize) * 100);
    if unallocatedPercent < 5 then
      Log(Format('  Unallocated space: %s (%d%% of disk)', [BytesToHuman(unallocatedSpace), unallocatedPercent]), llInfo)
    else
      Log(Format('  Unallocated space: %s (%d%% of disk)', [BytesToHuman(unallocatedSpace), unallocatedPercent]), llWarning);
  end
  else
    Log('  Unallocated space: None', llInfo);

  Log('', llInfo);
  Log('=================================' + #13#10, llInfo);
end;

function TPartitionAnalyzer.ValidatePartitionTable(const DevicePath: string; DiskSize: Int64; out ErrorMessage: string): Boolean;
var
  MBR: TMBRSector;
  GPTHeader: TGPTHeader;
  GPTEntry: TGPTPartitionEntry;
  i: Integer;
  PartStart, PartEnd, PartSize: Int64;
  TotalUsedSpace: Int64;
  IsGPT: Boolean;
  PartitionSector: array[0..511] of Byte;
  EmptyGUID: TGUID;
  SectorOffset: Int64;
  EntryOffset: Integer;
  MaxValidLBA: Int64;
begin
  Result := True;
  ErrorMessage := '';
  TotalUsedSpace := 0;
  
  // Calculate maximum valid LBA based on disk size
  if DiskSize > 0 then
    MaxValidLBA := (DiskSize div 512) - 1
  else
  begin
    ErrorMessage := 'Invalid disk size (0 bytes)';
    Result := False;
    Exit;
  end;
  
  // Read MBR
  if not ReadDiskSector(DevicePath, 0, MBR) then
  begin
    ErrorMessage := 'Failed to read MBR sector';
    Result := False;
    Exit;
  end;
  
  // Check MBR signature
  if MBR.Signature <> $AA55 then
  begin
    ErrorMessage := 'Invalid MBR signature';
    Result := False;
    Exit;
  end;
  
  // Check if GPT or MBR
  IsGPT := (MBR.Partitions[0].PartitionType = $EE);

  if IsGPT then
  begin
    // Read GPT header
    if not ReadDiskSector(DevicePath, 1, GPTHeader) then
    begin
      ErrorMessage := 'Failed to read GPT header';
      Result := False;
      Exit;
    end;
    
    // Check GPT signature
    if Copy(string(GPTHeader.Signature), 1, 8) <> 'EFI PART' then
    begin
      ErrorMessage := 'Invalid GPT signature';
      Result := False;
      Exit;
    end;

    // Validate GPT header
    if GPTHeader.NumberOfPartitionEntries > 256 then
    begin
      ErrorMessage := 'Too many GPT entries: ' + IntToStr(GPTHeader.NumberOfPartitionEntries);
      Result := False;
      Exit;
    end;

    if GPTHeader.SizeOfPartitionEntry < 128 then
    begin
      ErrorMessage := 'Invalid GPT partition entry size: ' + IntToStr(GPTHeader.SizeOfPartitionEntry);
      Result := False;
      Exit;
    end;

    FillChar(EmptyGUID, SizeOf(EmptyGUID), 0);
    
    // Check each partition
    for i := 0 to Integer(GPTHeader.NumberOfPartitionEntries) - 1 do
    begin
      SectorOffset := GPTHeader.PartitionEntryLBA + (Cardinal(i) * GPTHeader.SizeOfPartitionEntry) div 512;
      EntryOffset := (Cardinal(i) * GPTHeader.SizeOfPartitionEntry) mod 512;
      
      if not ReadDiskSector(DevicePath, SectorOffset, PartitionSector) then
        Continue;

      Move(PartitionSector[EntryOffset], GPTEntry, SizeOf(TGPTPartitionEntry));
      
      // Skip empty entries
      if CompareMem(@GPTEntry.PartitionTypeGUID, @EmptyGUID, SizeOf(TGUID)) then
        Continue;

      // Validate partition boundaries
      PartStart := Int64(GPTEntry.StartingLBA);
      PartEnd := Int64(GPTEntry.EndingLBA);
      PartSize := (PartEnd - PartStart + 1) * 512;
      
      // Check if partition exceeds disk size
      if PartEnd > MaxValidLBA then
      begin
        ErrorMessage := Format('Partition %d exceeds disk size: End LBA %d > Max LBA %d (Partition end %.2f GB > Disk %.2f GB)', 
          [i + 1, PartEnd, MaxValidLBA, (PartEnd * 512) / 1073741824.0, (MaxValidLBA * 512) / 1073741824.0]);
        Result := False;
        Exit;
      end;
      
      // Check for negative size
      if PartStart > PartEnd then
      begin
        ErrorMessage := Format('Partition %d has invalid range: Start LBA %d > End LBA %d', 
          [i + 1, PartStart, PartEnd]);
        Result := False;
        Exit;
      end;
      
      TotalUsedSpace := TotalUsedSpace + PartSize;
    end;
    
    // Check if total partition size exceeds disk
    if TotalUsedSpace > DiskSize then
    begin
      ErrorMessage := Format('Total partition size (%.2f GB) exceeds disk size (%.2f GB)', 
        [TotalUsedSpace / 1073741824.0, DiskSize / 1073741824.0]);
      Result := False;
      Exit;
    end;
  end
  else
  begin
    // Validate MBR partitions
    for i := 0 to 3 do
    begin
      if MBR.Partitions[i].PartitionType = 0 then
        Continue;
        
      PartStart := Int64(MBR.Partitions[i].StartLBA);
      PartSize := Int64(MBR.Partitions[i].SizeInSectors) * 512;
      PartEnd := PartStart + (PartSize div 512) - 1;
      
      // Check if partition exceeds disk size
      if PartEnd > MaxValidLBA then
      begin
        ErrorMessage := Format('MBR Partition %d exceeds disk size: End LBA %d > Max LBA %d (Partition end %.2f GB > Disk %.2f GB)', 
          [i + 1, PartEnd, MaxValidLBA, (PartEnd * 512) / 1073741824.0, (MaxValidLBA * 512) / 1073741824.0]);
        Result := False;
        Exit;
      end;
      
      TotalUsedSpace := TotalUsedSpace + PartSize;
    end;
    
    // Check if total partition size exceeds disk
    if TotalUsedSpace > DiskSize then
    begin
      ErrorMessage := Format('Total MBR partition size (%.2f GB) exceeds disk size (%.2f GB)', 
        [TotalUsedSpace / 1073741824.0, DiskSize / 1073741824.0]);
      Result := False;
      Exit;
    end;
  end;
end;

end.
