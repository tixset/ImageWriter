unit PartitionInfo;

interface

uses
  Windows, SysUtils, Classes;

type
  // Partition table type
  TPartitionTableType = (pttUnknown, pttMBR, pttGPT);

  // Information about a single partition
  TPartitionEntry = record
    PartitionNumber: Integer;       // Partition number (1-based)
    PartitionType: string;          // Partition type (hex for MBR, GUID for GPT)
    PartitionName: string;          // Partition name (for GPT) or type description
    StartLBA: Int64;                // Starting sector
    SizeInSectors: Int64;           // Size in sectors
    SizeInMB: Int64;                // Size in MB
    IsBootable: Boolean;            // Boot flag (for MBR)
  end;

  // MBR Partition Entry (16 bytes)
  TMBRPartitionEntry = packed record
    BootIndicator: Byte;            // 0x80 = bootable, 0x00 = non-bootable
    StartCHS: array[0..2] of Byte;  // Starting CHS address
    PartitionType: Byte;            // Partition type
    EndCHS: array[0..2] of Byte;    // Ending CHS address
    StartLBA: DWORD;                // Starting LBA address
    SizeInSectors: DWORD;           // Size in sectors
  end;

  // MBR Structure (512 bytes)
  TMBRSector = packed record
    BootCode: array[0..445] of Byte;         // Boot code
    PartitionTable: array[0..3] of TMBRPartitionEntry; // 4 partition entries
    Signature: WORD;                         // 0xAA55
  end;

  // GPT Header (512 bytes, simplified)
  TGPTHeader = packed record
    Signature: array[0..7] of Byte;  // "EFI PART"
    Revision: DWORD;
    HeaderSize: DWORD;
    HeaderCRC32: DWORD;
    Reserved: DWORD;
    CurrentLBA: Int64;
    BackupLBA: Int64;
    FirstUsableLBA: Int64;
    LastUsableLBA: Int64;
    DiskGUID: array[0..15] of Byte;
    PartitionEntryLBA: Int64;
    NumberOfPartitionEntries: DWORD;
    SizeOfPartitionEntry: DWORD;
    PartitionArrayCRC32: DWORD;
    Reserved2: array[0..419] of Byte; // Padding to 512 bytes
  end;

  // GPT Partition Entry (128 bytes)
  TGPTPartitionEntry = packed record
    PartitionTypeGUID: array[0..15] of Byte;
    UniquePartitionGUID: array[0..15] of Byte;
    StartingLBA: Int64;
    EndingLBA: Int64;
    Attributes: Int64;
    PartitionName: array[0..71] of Byte; // UTF-16LE, 36 characters max
  end;

// Main functions (file-based)
function DetectPartitionTableType(const ImageFile: string): TPartitionTableType;
function ParseMBRPartitions(const ImageFile: string; var Partitions: array of TPartitionEntry): Integer;
function ParseGPTPartitions(const ImageFile: string; var Partitions: array of TPartitionEntry): Integer;

// Stream-based functions for archive analysis
function DetectPartitionTableTypeFromStream(Stream: TStream): TPartitionTableType;
function ParseMBRPartitionsFromStream(Stream: TStream; var Partitions: array of TPartitionEntry): Integer;
function ParseGPTPartitionsFromStream(Stream: TStream; var Partitions: array of TPartitionEntry): Integer;

// Helper functions
function GetMBRPartitionTypeName(PartitionType: Byte): string;
function FormatPartitionInfo(const Partitions: array of TPartitionEntry; Count: Integer): string;
function GUIDToString(const GUID: array of Byte): string;

implementation

// Convert GUID to string
function GUIDToString(const GUID: array of Byte): string;
begin
  Result := Format('{%.2X%.2X%.2X%.2X-%.2X%.2X-%.2X%.2X-%.2X%.2X-%.2X%.2X%.2X%.2X%.2X%.2X}',
    [GUID[3], GUID[2], GUID[1], GUID[0],  // DWORD
     GUID[5], GUID[4],                      // WORD
     GUID[7], GUID[6],                      // WORD
     GUID[8], GUID[9],                      // BYTE[2]
     GUID[10], GUID[11], GUID[12], GUID[13], GUID[14], GUID[15]]); // BYTE[6]
end;

// Get MBR partition type name
function GetMBRPartitionTypeName(PartitionType: Byte): string;
begin
  case PartitionType of
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
    $14: Result := 'Hidden FAT16 <32MB';
    $16: Result := 'Hidden FAT16';
    $1B: Result := 'Hidden FAT32';
    $1C: Result := 'Hidden FAT32 LBA';
    $1E: Result := 'Hidden FAT16 LBA';
    $27: Result := 'Windows RE';
    $42: Result := 'Windows Dynamic';
    $82: Result := 'Linux Swap';
    $83: Result := 'Linux';
    $85: Result := 'Linux Extended';
    $8E: Result := 'Linux LVM';
    $A0: Result := 'Hibernation';
    $A5: Result := 'FreeBSD';
    $A6: Result := 'OpenBSD';
    $A8: Result := 'Mac OS X';
    $A9: Result := 'NetBSD';
    $AB: Result := 'Mac OS X Boot';
    $AF: Result := 'Mac OS X HFS+';
    $BE: Result := 'Solaris Boot';
    $BF: Result := 'Solaris';
    $EE: Result := 'GPT Protective';
    $EF: Result := 'EFI System';
    $FB: Result := 'VMware VMFS';
    $FC: Result := 'VMware Swap';
  else
    Result := Format('Unknown (0x%.2X)', [PartitionType]);
  end;
end;

// Detect partition table type
function DetectPartitionTableType(const ImageFile: string): TPartitionTableType;
var
  F: File;
  MBR: TMBRSector;
  GPTHeader: TGPTHeader;
  BytesRead: Integer;
begin
  Result := pttUnknown;
  
  if not FileExists(ImageFile) then
    Exit;

  AssignFile(F, ImageFile);
  FileMode := 0; // Read-only
  {$I-}
  Reset(F, 1);
  {$I+}
  if IOResult <> 0 then
    Exit;

  try
    // Read MBR (sector 0)
    BlockRead(F, MBR, SizeOf(TMBRSector), BytesRead);
    if BytesRead <> SizeOf(TMBRSector) then
      Exit;

    // Check MBR signature
    if MBR.Signature <> $AA55 then
      Exit;

    // Check if there's a protective MBR for GPT
    if (MBR.PartitionTable[0].PartitionType = $EE) then
    begin
      // Read GPT header (sector 1)
      Seek(F, 512);
      BlockRead(F, GPTHeader, SizeOf(TGPTHeader), BytesRead);
      if BytesRead <> SizeOf(TGPTHeader) then
        Exit;

      // Check GPT signature
      if (GPTHeader.Signature[0] = Ord('E')) and
         (GPTHeader.Signature[1] = Ord('F')) and
         (GPTHeader.Signature[2] = Ord('I')) and
         (GPTHeader.Signature[3] = Ord(' ')) and
         (GPTHeader.Signature[4] = Ord('P')) and
         (GPTHeader.Signature[5] = Ord('A')) and
         (GPTHeader.Signature[6] = Ord('R')) and
         (GPTHeader.Signature[7] = Ord('T')) then
        Result := pttGPT
      else
        Result := pttMBR; // Protective MBR but no GPT header
    end
    else
      Result := pttMBR;

  finally
    CloseFile(F);
  end;
end;

// Parse MBR partitions
function ParseMBRPartitions(const ImageFile: string; var Partitions: array of TPartitionEntry): Integer;
var
  F: File;
  MBR: TMBRSector;
  BytesRead: Integer;
  i: Integer;
begin
  Result := 0;

  if not FileExists(ImageFile) then
    Exit;

  AssignFile(F, ImageFile);
  FileMode := 0; // Read-only
  {$I-}
  Reset(F, 1);
  {$I+}
  if IOResult <> 0 then
    Exit;

  try
    BlockRead(F, MBR, SizeOf(TMBRSector), BytesRead);
    if BytesRead <> SizeOf(TMBRSector) then
      Exit;

    if MBR.Signature <> $AA55 then
      Exit;

    // Парсим 4 первичных раздела
    for i := 0 to 3 do
    begin
      if MBR.PartitionTable[i].PartitionType <> $00 then // Не пустой раздел
      begin
        Partitions[Result].PartitionNumber := Result + 1;
        Partitions[Result].PartitionType := Format('0x%.2X', [MBR.PartitionTable[i].PartitionType]);
        Partitions[Result].PartitionName := GetMBRPartitionTypeName(MBR.PartitionTable[i].PartitionType);
        Partitions[Result].StartLBA := MBR.PartitionTable[i].StartLBA;
        Partitions[Result].SizeInSectors := MBR.PartitionTable[i].SizeInSectors;
        Partitions[Result].SizeInMB := (Int64(MBR.PartitionTable[i].SizeInSectors) * 512) div (1024 * 1024);
        Partitions[Result].IsBootable := (MBR.PartitionTable[i].BootIndicator = $80);
        Inc(Result);
      end;
    end;

  finally
    CloseFile(F);
  end;
end;

// Parse GPT partitions
function ParseGPTPartitions(const ImageFile: string; var Partitions: array of TPartitionEntry): Integer;
var
  F: File;
  GPTHeader: TGPTHeader;
  GPTEntry: TGPTPartitionEntry;
  BytesRead: Integer;
  i: Integer;
  PartitionName: WideString;
  EmptyGUID: array[0..15] of Byte;
begin
  Result := 0;

  if not FileExists(ImageFile) then
    Exit;

  AssignFile(F, ImageFile);
  FileMode := 0; // Read-only
  {$I-}
  Reset(F, 1);
  {$I+}
  if IOResult <> 0 then
    Exit;

  try
    // Skip protective MBR, read GPT header (sector 1)
    Seek(F, 512);
    BlockRead(F, GPTHeader, SizeOf(TGPTHeader), BytesRead);
    if BytesRead <> SizeOf(TGPTHeader) then
      Exit;

    // Check signature
    if (GPTHeader.Signature[0] <> Ord('E')) or
       (GPTHeader.Signature[1] <> Ord('F')) or
       (GPTHeader.Signature[2] <> Ord('I')) or
       (GPTHeader.Signature[3] <> Ord(' ')) then
      Exit;

    // Seek to partition array start
    Seek(F, GPTHeader.PartitionEntryLBA * 512);

    // Initialize empty GUID for comparison
    FillChar(EmptyGUID, SizeOf(EmptyGUID), 0);

    // Read partition entries
    for i := 0 to Integer(GPTHeader.NumberOfPartitionEntries) - 1 do
    begin
      BlockRead(F, GPTEntry, SizeOf(TGPTPartitionEntry), BytesRead);
      if BytesRead <> SizeOf(TGPTPartitionEntry) then
        Break;

      // Check if partition is not empty (all zeros in type GUID)
      if CompareMem(@GPTEntry.PartitionTypeGUID, @EmptyGUID, 16) then
        Continue;

      // Extract partition name (UTF-16LE)
      SetLength(PartitionName, 36);
      Move(GPTEntry.PartitionName, PartitionName[1], 72);
      PartitionName := PWideChar(@GPTEntry.PartitionName[0]);

      Partitions[Result].PartitionNumber := Result + 1;
      Partitions[Result].PartitionType := GUIDToString(GPTEntry.PartitionTypeGUID);
      Partitions[Result].PartitionName := PartitionName;
      Partitions[Result].StartLBA := GPTEntry.StartingLBA;
      Partitions[Result].SizeInSectors := GPTEntry.EndingLBA - GPTEntry.StartingLBA + 1;
      Partitions[Result].SizeInMB := (Partitions[Result].SizeInSectors * 512) div (1024 * 1024);
      Partitions[Result].IsBootable := False; // GPT не использует флаг bootable
      Inc(Result);

      if Result >= Length(Partitions) then
        Break;
    end;

  finally
    CloseFile(F);
  end;
end;

// Format partition information for display
function FormatPartitionInfo(const Partitions: array of TPartitionEntry; Count: Integer): string;
var
  i: Integer;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('--- Partition Table Information ---');
    
    if Count = 0 then
    begin
      Lines.Add('No partitions found or unable to read partition table.');
    end
    else
    begin
      Lines.Add(Format('Found %d partition(s):', [Count]));
      Lines.Add('');
      
      for i := 0 to Count - 1 do
      begin
        Lines.Add(Format('Partition %d:', [Partitions[i].PartitionNumber]));
        Lines.Add(Format('  Type: %s - %s', [Partitions[i].PartitionType, Partitions[i].PartitionName]));
        Lines.Add(Format('  Start LBA: %d', [Partitions[i].StartLBA]));
        Lines.Add(Format('  Size: %d sectors (%d MB)', [Partitions[i].SizeInSectors, Partitions[i].SizeInMB]));
        if Partitions[i].IsBootable then
          Lines.Add('  Bootable: Yes');
        Lines.Add('');
      end;
    end;
    
    Lines.Add('--- End of Partition Information ---');
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

// Stream-based version of DetectPartitionTableType for archives
function DetectPartitionTableTypeFromStream(Stream: TStream): TPartitionTableType;
var
  MBR: TMBRSector;
  GPTHeader: TGPTHeader;
  BytesRead: Integer;
  OriginalPosition: Int64;
begin
  Result := pttUnknown;
  
  if not Assigned(Stream) then
    Exit;
    
  // Save current position
  OriginalPosition := Stream.Position;
  
  try
    // Read MBR (sector 0)
    Stream.Position := 0;
    BytesRead := Stream.Read(MBR, SizeOf(TMBRSector));
    if BytesRead <> SizeOf(TMBRSector) then
      Exit;

    // Check MBR signature
    if MBR.Signature <> $AA55 then
      Exit;

    // Check if there's a protective MBR for GPT
    if (MBR.PartitionTable[0].PartitionType = $EE) then
    begin
      // Read GPT header (sector 1)
      Stream.Position := 512;
      BytesRead := Stream.Read(GPTHeader, SizeOf(TGPTHeader));
      if BytesRead <> SizeOf(TGPTHeader) then
        Exit;

      // Check GPT signature
      if (GPTHeader.Signature[0] = Ord('E')) and
         (GPTHeader.Signature[1] = Ord('F')) and
         (GPTHeader.Signature[2] = Ord('I')) and
         (GPTHeader.Signature[3] = Ord(' ')) and
         (GPTHeader.Signature[4] = Ord('P')) and
         (GPTHeader.Signature[5] = Ord('A')) and
         (GPTHeader.Signature[6] = Ord('R')) and
         (GPTHeader.Signature[7] = Ord('T')) then
        Result := pttGPT
      else
        Result := pttMBR; // Protective MBR but no GPT header
    end
    else
      Result := pttMBR;
      
  finally
    // Restore position
    Stream.Position := OriginalPosition;
  end;
end;

// Stream-based version of ParseMBRPartitions for archives
function ParseMBRPartitionsFromStream(Stream: TStream; var Partitions: array of TPartitionEntry): Integer;
var
  MBR: TMBRSector;
  BytesRead: Integer;
  i: Integer;
  OriginalPosition: Int64;
begin
  Result := 0;

  if not Assigned(Stream) then
    Exit;
    
  OriginalPosition := Stream.Position;
  
  try
    Stream.Position := 0;
    BytesRead := Stream.Read(MBR, SizeOf(TMBRSector));
    if BytesRead <> SizeOf(TMBRSector) then
      Exit;

    if MBR.Signature <> $AA55 then
      Exit;

    // Парсим 4 первичных раздела
    for i := 0 to 3 do
    begin
      if MBR.PartitionTable[i].PartitionType <> $00 then // Не пустой раздел
      begin
        Partitions[Result].PartitionNumber := Result + 1;
        Partitions[Result].PartitionType := Format('0x%.2X', [MBR.PartitionTable[i].PartitionType]);
        Partitions[Result].PartitionName := GetMBRPartitionTypeName(MBR.PartitionTable[i].PartitionType);
        Partitions[Result].StartLBA := MBR.PartitionTable[i].StartLBA;
        Partitions[Result].SizeInSectors := MBR.PartitionTable[i].SizeInSectors;
        Partitions[Result].SizeInMB := (Int64(MBR.PartitionTable[i].SizeInSectors) * 512) div (1024 * 1024);
        Partitions[Result].IsBootable := (MBR.PartitionTable[i].BootIndicator = $80);
        Inc(Result);
      end;
    end;
    
  finally
    Stream.Position := OriginalPosition;
  end;
end;

// Stream-based version of ParseGPTPartitions for archives
function ParseGPTPartitionsFromStream(Stream: TStream; var Partitions: array of TPartitionEntry): Integer;
var
  GPTHeader: TGPTHeader;
  GPTEntry: TGPTPartitionEntry;
  BytesRead: Integer;
  i: Integer;
  PartitionName: WideString;
  EmptyGUID: array[0..15] of Byte;
  OriginalPosition: Int64;
begin
  Result := 0;

  if not Assigned(Stream) then
    Exit;
    
  OriginalPosition := Stream.Position;
  
  try
    // Skip protective MBR, read GPT header (sector 1)
    Stream.Position := 512;
    BytesRead := Stream.Read(GPTHeader, SizeOf(TGPTHeader));
    if BytesRead <> SizeOf(TGPTHeader) then
      Exit;

    // Check signature
    if (GPTHeader.Signature[0] <> Ord('E')) or
       (GPTHeader.Signature[1] <> Ord('F')) or
       (GPTHeader.Signature[2] <> Ord('I')) or
       (GPTHeader.Signature[3] <> Ord(' ')) then
      Exit;

    // Seek to partition array start
    Stream.Position := GPTHeader.PartitionEntryLBA * 512;

    // Initialize empty GUID for comparison
    FillChar(EmptyGUID, SizeOf(EmptyGUID), 0);

    // Read partition entries
    for i := 0 to Integer(GPTHeader.NumberOfPartitionEntries) - 1 do
    begin
      BytesRead := Stream.Read(GPTEntry, SizeOf(TGPTPartitionEntry));
      if BytesRead <> SizeOf(TGPTPartitionEntry) then
        Break;

      // Check if partition is not empty (all zeros in type GUID)
      if CompareMem(@GPTEntry.PartitionTypeGUID, @EmptyGUID, 16) then
        Continue;

      // Extract partition name (UTF-16LE)
      SetLength(PartitionName, 36);
      Move(GPTEntry.PartitionName, PartitionName[1], 72);
      PartitionName := PWideChar(@GPTEntry.PartitionName[0]);

      Partitions[Result].PartitionNumber := Result + 1;
      Partitions[Result].PartitionType := GUIDToString(GPTEntry.PartitionTypeGUID);
      Partitions[Result].PartitionName := PartitionName;
      Partitions[Result].StartLBA := GPTEntry.StartingLBA;
      Partitions[Result].SizeInSectors := GPTEntry.EndingLBA - GPTEntry.StartingLBA + 1;
      Partitions[Result].SizeInMB := (Partitions[Result].SizeInSectors * 512) div (1024 * 1024);
      Partitions[Result].IsBootable := False; // GPT не использует флаг bootable
      Inc(Result);

      if Result >= Length(Partitions) then
        Break;
    end;
    
  finally
    Stream.Position := OriginalPosition;
  end;
end;

end.
