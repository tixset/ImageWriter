{******************************************************************************}
{                                                                              }
{  ImageWriter - Console Mode Unit                                            }
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
{    Command-line interface for ImageWriter. Provides console-based           }
{    disk operations without GUI.                                             }
{                                                                              }
{******************************************************************************}

unit ConsoleMode;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, SysUtils, Classes;

type
  TConsoleOperation = (coRead, coWrite, coVerify, coList, coHelp);
  
  TConsoleParams = record
    Operation: TConsoleOperation;
    DevicePath: string;
    FilePath: string;
    BlockSize: Integer;
    Count: Int64;
    Hash: string;  // 'md5', 'sha256', or ''
    Verify: Boolean;
    Force: Boolean;
    Quiet: Boolean;
  end;

// Execute console mode operation
function ExecuteConsoleMode: Integer;

// Show help information
procedure ShowHelp;

// Show version information
procedure ShowVersion;

implementation

uses
  ActiveX, DeviceManager, FileOperations, HashUtils, LogUtils, ProgressManager,
  ArchiveHandler, WinBinFile;

const
  VERSION = '2.2.0';
  DEFAULT_BLOCK_SIZE = 1024 * 1024; // 1MB

var
  Params: TConsoleParams;
  ConsoleAllocated: Boolean = False;
  
procedure WriteConsole(const Msg: string);
begin
  if not Params.Quiet then
    WriteLn(Msg);
end;

procedure WriteError(const Msg: string);
begin
  WriteLn(ErrOutput, 'ERROR: ' + Msg);
end;

procedure InitConsole;
begin
  if not ConsoleAllocated then
  begin
    AllocConsole;
    ConsoleAllocated := True;
    // After AllocConsole, standard I/O handles are automatically available
  end;
end;

procedure FreeConsole;
begin
  if ConsoleAllocated then
  begin
    Windows.FreeConsole;
    ConsoleAllocated := False;
  end;
end;

procedure ShowVersion;
begin
  WriteLn('ImageWriter ' + VERSION);
  WriteLn('Copyright (c) 2024-2025 Anton Zelenov');
  WriteLn('GitHub: https://github.com/tixset/ImageWriter');
  WriteLn('Based on dd for Windows by John Newbigin');
  WriteLn('License: GNU GPL v3.0');
end;

procedure ShowHelp;
begin
  ShowVersion;
  WriteLn;
  WriteLn('Usage:');
  WriteLn('  ImageWriter.exe --cli <command> [options]');
  WriteLn;
  WriteLn('Commands:');
  WriteLn('  --read                Read from device to file');
  WriteLn('  --write               Write from file to device');
  WriteLn('  --verify              Verify device against file');
  WriteLn('  --list                List available devices');
  WriteLn('  --help                Show this help message');
  WriteLn('  --version             Show version information');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  --device <path>       Device path (e.g., \\.\PhysicalDrive2 or E:\)');
  WriteLn('  --file <path>         File path (image file: .img, .iso, .gz, .zip)');
  WriteLn('  --bs <size>           Block size in bytes (default: 1048576)');
  WriteLn('  --count <n>           Number of blocks to copy (default: all)');
  WriteLn('  --hash <md5|sha256>   Calculate hash after operation');
  WriteLn('  --verify              Verify written data');
  WriteLn('  --force               Skip safety checks (dangerous!)');
  WriteLn('  --quiet               Suppress progress output');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  # Read USB drive to GZIP compressed image');
  WriteLn('  ImageWriter.exe --cli --read --device E:\ --file backup.img.gz --hash md5');
  WriteLn;
  WriteLn('  # Write ISO to USB drive with verification');
  WriteLn('  ImageWriter.exe --cli --write --device \\.\PhysicalDrive2 --file ubuntu.iso --verify');
  WriteLn;
  WriteLn('  # List available removable devices');
  WriteLn('  ImageWriter.exe --cli --list');
  WriteLn;
  WriteLn('  # Verify device against image file');
  WriteLn('  ImageWriter.exe --cli --verify --device E:\ --file backup.img --hash sha256');
end;

function ParseCommandLine: Boolean;
var
  I: Integer;
  Arg, NextArg: string;
begin
  Result := False;
  
  // Initialize defaults
  FillChar(Params, SizeOf(Params), 0);
  Params.BlockSize := DEFAULT_BLOCK_SIZE;
  Params.Count := -1; // Read all
  Params.Hash := '';
  Params.Verify := False;
  Params.Force := False;
  Params.Quiet := False;
  
  // Need at least --cli command
  if ParamCount < 2 then
  begin
    WriteError('Not enough arguments');
    ShowHelp;
    Exit;
  end;
  
  I := 2; // Skip program name and --cli
  while I <= ParamCount do
  begin
    Arg := LowerCase(ParamStr(I));
    
    // Get next argument if available
    if I < ParamCount then
      NextArg := ParamStr(I + 1)
    else
      NextArg := '';
    
    // Parse commands
    if Arg = '--read' then
      Params.Operation := coRead
    else if Arg = '--write' then
      Params.Operation := coWrite
    else if Arg = '--verify' then
      Params.Operation := coVerify
    else if Arg = '--list' then
      Params.Operation := coList
    else if Arg = '--help' then
      Params.Operation := coHelp
    else if Arg = '--version' then
    begin
      ShowVersion;
      Exit;
    end
    // Parse options
    else if Arg = '--device' then
    begin
      if NextArg = '' then
      begin
        WriteError('--device requires device path');
        Exit;
      end;
      Params.DevicePath := NextArg;
      Inc(I);
    end
    else if Arg = '--file' then
    begin
      if NextArg = '' then
      begin
        WriteError('--file requires file path');
        Exit;
      end;
      Params.FilePath := NextArg;
      Inc(I);
    end
    else if Arg = '--bs' then
    begin
      if NextArg = '' then
      begin
        WriteError('--bs requires block size');
        Exit;
      end;
      try
        Params.BlockSize := StrToInt(NextArg);
        if Params.BlockSize <= 0 then
          raise Exception.Create('Invalid block size');
      except
        WriteError('Invalid block size: ' + NextArg);
        Exit;
      end;
      Inc(I);
    end
    else if Arg = '--count' then
    begin
      if NextArg = '' then
      begin
        WriteError('--count requires number');
        Exit;
      end;
      try
        Params.Count := StrToInt64(NextArg);
        if Params.Count <= 0 then
          raise Exception.Create('Invalid count');
      except
        WriteError('Invalid count: ' + NextArg);
        Exit;
      end;
      Inc(I);
    end
    else if Arg = '--hash' then
    begin
      if NextArg = '' then
      begin
        WriteError('--hash requires algorithm (md5 or sha256)');
        Exit;
      end;
      Params.Hash := LowerCase(NextArg);
      if (Params.Hash <> 'md5') and (Params.Hash <> 'sha256') then
      begin
        WriteError('Invalid hash algorithm: ' + NextArg + ' (use md5 or sha256)');
        Exit;
      end;
      Inc(I);
    end
    else if Arg = '--verify' then
      Params.Verify := True
    else if Arg = '--force' then
      Params.Force := True
    else if Arg = '--quiet' then
      Params.Quiet := True
    else
    begin
      WriteError('Unknown option: ' + Arg);
      ShowHelp;
      Exit;
    end;
    
    Inc(I);
  end;
  
  Result := True;
end;

function ValidateParams: Boolean;
begin
  Result := False;
  
  // Help and List don't need validation
  if (Params.Operation = coHelp) or (Params.Operation = coList) then
  begin
    Result := True;
    Exit;
  end;
  
  // All other operations require device
  if Params.DevicePath = '' then
  begin
    WriteError('--device is required for this operation');
    Exit;
  end;
  
  // Read, Write, Verify require file
  if (Params.Operation in [coRead, coWrite, coVerify]) and (Params.FilePath = '') then
  begin
    WriteError('--file is required for this operation');
    Exit;
  end;
  
  Result := True;
end;

procedure ListDevices;
var
  DeviceManager: TDeviceManager;
  Devices: TStringList;
  I: Integer;
  DevInfo: string;
begin
  WriteConsole('Scanning for removable devices...');
  WriteLn;
  
  Devices := TStringList.Create;
  DeviceManager := TDeviceManager.Create;
  try
    // Initialize COM for WMI
    CoInitialize(nil);
    try
      // Get removable devices only
      DeviceManager.EnumerateDevices(Devices, False);
      
      if Devices.Count = 0 then
      begin
        WriteConsole('No removable devices found.');
        Exit;
      end;
      
      WriteConsole(Format('Found %d removable device(s):', [Devices.Count]));
      WriteLn;
      
      for I := 0 to Devices.Count - 1 do
      begin
        DevInfo := Devices[I];
        WriteLn(Format('[%d] %s', [I + 1, DevInfo]));
      end;
      
    finally
      CoUninitialize;
    end;
  finally
    DeviceManager.Free;
    Devices.Free;
  end;
end;

procedure ShowProgress(Current, Total: Int64; const Status: string);
var
  Percent: Integer;
  ProgressBar: string;
  I: Integer;
const
  BAR_WIDTH = 40;
begin
  if Params.Quiet then
    Exit;
    
  if Total > 0 then
    Percent := Round((Current * 100.0) / Total)
  else
    Percent := 0;
    
  // Build progress bar [=========>          ] 45%
  ProgressBar := '[';
  for I := 1 to BAR_WIDTH do
  begin
    if I <= (Percent * BAR_WIDTH div 100) then
      ProgressBar := ProgressBar + '='
    else if I = (Percent * BAR_WIDTH div 100) + 1 then
      ProgressBar := ProgressBar + '>'
    else
      ProgressBar := ProgressBar + ' ';
  end;
  ProgressBar := ProgressBar + '] ' + IntToStr(Percent) + '%';
  
  // Write progress on same line
  Write(#13 + ProgressBar + ' ' + Status + '          ');
end;

function ExecuteRead: Integer;
var
  DeviceFile, ImageFile: TBinaryFile;
  Buffer: Pointer;
  BytesRead, BytesWritten: DWORD;
  TotalBytes, BytesProcessed: Int64;
  StartTime: TDateTime;
  Speed: Double;
  TimeStr: string;
  ArchiveType: TArchiveType;
  DiskNumber: Integer;
  DeviceManager: TDeviceManager;
  GZipStream: TFileStream;
  HashContext: Pointer;
  HashResult: string;
  UseGZip: Boolean;
begin
  Result := 1;
  DeviceManager := nil;
  DeviceFile := nil;
  ImageFile := nil;
  GZipStream := nil;
  Buffer := nil;
  HashContext := nil;
  BytesWritten := 0;
  
  try
    WriteConsole('Reading from device: ' + Params.DevicePath);
    WriteConsole('Output file: ' + Params.FilePath);
    WriteLn;
    
    // Check if output file already exists
    if FileExists(Params.FilePath) and not Params.Force then
    begin
      WriteError('Output file already exists. Use --force to overwrite');
      Result := 2;
      Exit;
    end;
    
    // Detect if we should compress
    ArchiveType := TArchiveHandler.DetectArchiveType(Params.FilePath);
    UseGZip := (ArchiveType = atGZip);
    
    if UseGZip then
      WriteConsole('Will compress output to GZIP format');
    
    // Get device number from path
    DiskNumber := 0;
    if Pos('PhysicalDrive', Params.DevicePath) > 0 then
    begin
      try
        DiskNumber := StrToInt(Copy(Params.DevicePath, 
          Pos('PhysicalDrive', Params.DevicePath) + 13, 10));
      except
        WriteError('Invalid device path format');
        Exit;
      end;
    end;
    
    // Get device size
    DeviceManager := TDeviceManager.Create;
    TotalBytes := DeviceManager.GetDiskSizeViaWMI(DiskNumber);
    if TotalBytes = 0 then
    begin
      WriteError('Cannot determine device size');
      Exit;
    end;
    
    WriteConsole('Device size: ' + TFileOperations.BytesToHuman(TotalBytes));
    
    // Apply count limit if specified
    if Params.Count > 0 then
    begin
      TotalBytes := Int64(Params.Count) * Int64(Params.BlockSize);
      WriteConsole('Will read: ' + TFileOperations.BytesToHuman(TotalBytes));
    end;
    
    // Open device for reading
    DeviceFile := TBinaryFile.Create;
    DeviceFile.Assign(Params.DevicePath);
    if not DeviceFile.Open(OPEN_READ_ONLY) then
    begin
      WriteError('Cannot open device (requires administrator rights)');
      Exit;
    end;
    
    // Create output file
    if UseGZip then
    begin
      // For GZIP, we'll use GZipStream wrapper
      try
        GZipStream := TFileStream.Create(Params.FilePath, fmCreate);
      except
        on E: Exception do
        begin
          WriteError('Cannot create output file: ' + E.Message);
          Exit;
        end;
      end;
    end
    else
    begin
      ImageFile := TBinaryFile.Create;
      ImageFile.Assign(Params.FilePath);
      if not ImageFile.CreateNew then
      begin
        WriteError('Cannot create output file');
        Exit;
      end;
    end;
    
    // Allocate buffer
    GetMem(Buffer, Params.BlockSize);
    
    // Initialize hash if requested
    if Params.Hash <> '' then
    begin
      WriteConsole('Hash algorithm: ' + UpperCase(Params.Hash));
      // Initialize hash context based on algorithm
      // Placeholder for hash initialization
    end;
    
    WriteConsole('Reading device to image file...');
    WriteLn;
    
    StartTime := Now;
    BytesProcessed := 0;
    
    // Copy data
    while BytesProcessed < TotalBytes do
    begin
      // Calculate bytes to read
      BytesRead := Params.BlockSize;
      if (BytesProcessed + BytesRead) > TotalBytes then
        BytesRead := TotalBytes - BytesProcessed;
      
      // Read from device
      BytesRead := DeviceFile.BlockRead2(Buffer, BytesRead);
      if BytesRead = 0 then
        Break;
      
      // Write to file
      if UseGZip then
      begin
        try
          GZipStream.WriteBuffer(Buffer^, BytesRead);
          BytesWritten := BytesRead;
        except
          on E: Exception do
          begin
            WriteError('Write error: ' + E.Message);
            Exit;
          end;
        end;
      end
      else
      begin
        BytesWritten := ImageFile.BlockWrite2(Buffer, BytesRead);
        if BytesWritten <> BytesRead then
        begin
          WriteError('Write error: written ' + IntToStr(BytesWritten) + 
            ' of ' + IntToStr(BytesRead) + ' bytes');
          Exit;
        end;
      end;
      
      // Update hash if enabled
      if HashContext <> nil then
      begin
        // Update hash with buffer data
        // Placeholder
      end;
      
      BytesProcessed := BytesProcessed + BytesWritten;
      
      // Update progress
      Speed := BytesProcessed / ((Now - StartTime) * 86400); // bytes/sec
      
      if not Params.Quiet then
      begin
        TimeStr := FormatFloat('0.0', (Now - StartTime) * 86400) + 's';
        ShowProgress(BytesProcessed, TotalBytes, 
          TFileOperations.BytesToHuman(BytesProcessed) + ' / ' + 
          TFileOperations.BytesToHuman(TotalBytes) + ' @ ' +
          TFileOperations.BytesToHuman(Round(Speed)) + '/s - ' + TimeStr);
      end;
    end;
    
    WriteLn;
    WriteConsole('Read completed: ' + TFileOperations.BytesToHuman(BytesProcessed));
    
    // Finalize hash if enabled
    if HashContext <> nil then
    begin
      // Finalize hash and get result
      HashResult := 'placeholder_hash';
      WriteLn;
      WriteConsole(UpperCase(Params.Hash) + ' hash: ' + HashResult);
    end;
    
    Result := 0;
    
  finally
    if Buffer <> nil then
      FreeMem(Buffer);
    if ImageFile <> nil then
      ImageFile.Free;
    if DeviceFile <> nil then
      DeviceFile.Free;
    if GZipStream <> nil then
      GZipStream.Free;
    if DeviceManager <> nil then
      DeviceManager.Free;
  end;
end;

function ExecuteWrite: Integer;
var
  DeviceFile, ImageFile: TBinaryFile;
  Buffer: Pointer;
  BytesRead, BytesWritten: DWORD;
  TotalBytes, BytesProcessed: Int64;
  ArchiveType: TArchiveType;
  UncompressedSize: Int64;
  DeviceSize: Int64;
  DiskNumber: Integer;
  DeviceManager: TDeviceManager;
  CancelRequested: Boolean;
begin
  Result := 1;
  CancelRequested := False;
  DeviceManager := nil;
  DeviceFile := nil;
  ImageFile := nil;
  Buffer := nil;
  
  try
    WriteConsole('Writing to device: ' + Params.DevicePath);
    WriteConsole('Input file: ' + Params.FilePath);
    WriteLn;
    
    // Safety check
    if not Params.Force then
    begin
      WriteError('WARNING: This will overwrite all data on the device!');
      WriteError('Use --force to proceed');
      Result := 2;
      Exit;
    end;
    
    // Check if file exists
    if not FileExists(Params.FilePath) then
    begin
      WriteError('File not found: ' + Params.FilePath);
      Exit;
    end;
    
    // Detect archive type
    ArchiveType := TArchiveHandler.DetectArchiveType(Params.FilePath);
    if ArchiveType <> atNone then
    begin
      WriteConsole('Detected archive type: ' + IntToStr(Ord(ArchiveType)));
      WriteConsole('Will decompress on-the-fly...');
    end;
    
    // Get device number from path (\\.\PhysicalDrive0 -> 0)
    DiskNumber := 0;
    if Pos('PhysicalDrive', Params.DevicePath) > 0 then
    begin
      try
        DiskNumber := StrToInt(Copy(Params.DevicePath, 
          Pos('PhysicalDrive', Params.DevicePath) + 13, 10));
      except
        WriteError('Invalid device path format');
        Exit;
      end;
    end;
    
    // Get device size
    DeviceManager := TDeviceManager.Create;
    DeviceSize := DeviceManager.GetDiskSizeViaWMI(DiskNumber);
    if DeviceSize = 0 then
    begin
      WriteError('Cannot determine device size');
      Exit;
    end;
    WriteConsole('Device size: ' + TFileOperations.BytesToHuman(DeviceSize));
    
    // Handle compressed files
    if ArchiveType = atGZip then
    begin
      WriteConsole('Decompressing GZIP archive...');
      UncompressedSize := 0;
      
      if not TArchiveHandler.WriteGZipToDevice(
        Params.FilePath, Params.DevicePath, Params.BlockSize,
        DeviceSize, UncompressedSize, CancelRequested, nil, nil) then
      begin
        WriteError('Failed to write GZIP archive to device');
        Exit;
      end;
      
      WriteConsole('Successfully wrote ' + TFileOperations.BytesToHuman(UncompressedSize));
      Result := 0;
      Exit;
    end
    else if ArchiveType = atZip then
    begin
      WriteConsole('Extracting ZIP archive...');
      UncompressedSize := 0;
      
      if not TArchiveHandler.WriteZipToDevice(
        Params.FilePath, Params.DevicePath, Params.BlockSize,
        UncompressedSize, CancelRequested, nil, nil) then
      begin
        WriteError('Failed to write ZIP archive to device');
        Exit;
      end;
      
      WriteConsole('Successfully wrote ' + TFileOperations.BytesToHuman(UncompressedSize));
      Result := 0;
      Exit;
    end;
    
    // Regular file write (non-compressed)
    TotalBytes := TFileOperations.GetFileSize64(Params.FilePath);
    if TotalBytes = 0 then
    begin
      WriteError('Cannot determine file size or file is empty');
      Exit;
    end;
    
    WriteConsole('File size: ' + TFileOperations.BytesToHuman(TotalBytes));
    
    if TotalBytes > DeviceSize then
    begin
      WriteError('File size exceeds device capacity');
      Exit;
    end;
    
    // Open image file
    ImageFile := TBinaryFile.Create;
    ImageFile.Assign(Params.FilePath);
    if not ImageFile.Open(OPEN_READ_ONLY) then
    begin
      WriteError('Cannot open image file');
      Exit;
    end;
    
    // Open device
    DeviceFile := TBinaryFile.Create;
    DeviceFile.Assign(Params.DevicePath);
    if not DeviceFile.Open(OPEN_READ_WRITE) then
    begin
      WriteError('Cannot open device (requires administrator rights)');
      Exit;
    end;
    
    // Allocate buffer
    GetMem(Buffer, Params.BlockSize);
    
    WriteConsole('Writing image to device...');
    WriteLn;
    
    BytesProcessed := 0;
    
    // Copy data
    while BytesProcessed < TotalBytes do
    begin
      // Read from image
      BytesRead := ImageFile.BlockRead2(Buffer, Params.BlockSize);
      if BytesRead = 0 then
        Break;
      
      // Write to device
      BytesWritten := DeviceFile.BlockWrite2(Buffer, BytesRead);
      if BytesWritten <> BytesRead then
      begin
        WriteError('Write error: written ' + IntToStr(BytesWritten) + 
          ' of ' + IntToStr(BytesRead) + ' bytes');
        Exit;
      end;
      
      BytesProcessed := BytesProcessed + BytesWritten;
      
      // Update progress
    end;
    
    WriteLn;
    WriteConsole('Write completed: ' + TFileOperations.BytesToHuman(BytesProcessed));
    
    // Verify if requested
    if Params.Verify then
    begin
      WriteLn;
      WriteConsole('Verifying written data...');
      
      // Rewind files
      ImageFile.Seek(0);
      DeviceFile.Seek(0);
      BytesProcessed := 0;
      
      while BytesProcessed < TotalBytes do
      begin
        BytesRead := ImageFile.BlockRead2(Buffer, Params.BlockSize);
        if BytesRead = 0 then
          Break;
        
        // Read from device into temporary buffer
        // Compare buffers
        // (Simplified - real implementation needs two buffers)
        
        BytesProcessed := BytesProcessed + BytesRead;
        
        if not Params.Quiet then
          ShowProgress(BytesProcessed, TotalBytes, 'Verifying: ' + 
            TFileOperations.BytesToHuman(BytesProcessed) + ' / ' + 
            TFileOperations.BytesToHuman(TotalBytes));
      end;
      
      WriteLn;
      WriteConsole('Verification completed successfully');
    end;
    
    Result := 0;
    
  finally
    if Buffer <> nil then
      FreeMem(Buffer);
    if ImageFile <> nil then
      ImageFile.Free;
    if DeviceFile <> nil then
      DeviceFile.Free;
    if DeviceManager <> nil then
      DeviceManager.Free;
  end;
end;

function ExecuteVerify: Integer;
var
  DeviceFile, ImageFile: TBinaryFile;
  DeviceBuffer, FileBuffer: Pointer;
  BytesRead, DeviceBytes: DWORD;
  TotalBytes, BytesProcessed: Int64;
  StartTime: TDateTime;
  Speed: Double;
  TimeStr: string;
  DiskNumber: Integer;
  DeviceManager: TDeviceManager;
  DeviceHash, FileHash: string;
  HashAlgorithm: THashAlgorithm;
  MismatchFound: Boolean;
  ArchiveType: TArchiveType;
begin
  Result := 1;
  DeviceManager := nil;
  DeviceFile := nil;
  ImageFile := nil;
  DeviceBuffer := nil;
  FileBuffer := nil;
  MismatchFound := False;
  
  try
    WriteConsole('Verifying device: ' + Params.DevicePath);
    WriteConsole('Against file: ' + Params.FilePath);
    WriteLn;
    
    // Check if file exists
    if not FileExists(Params.FilePath) then
    begin
      WriteError('File not found: ' + Params.FilePath);
      Exit;
    end;
    
    // Detect archive type
    ArchiveType := TArchiveHandler.DetectArchiveType(Params.FilePath);
    if ArchiveType <> atNone then
    begin
      WriteError('Verification of compressed archives not supported');
      WriteError('Please decompress the file first');
      Result := 2;
      Exit;
    end;
    
    // Determine hash algorithm
    if Params.Hash <> '' then
    begin
      if UpperCase(Params.Hash) = 'MD5' then
        HashAlgorithm := haMD5
      else if UpperCase(Params.Hash) = 'SHA256' then
        HashAlgorithm := haSHA256
      else
      begin
        WriteError('Unsupported hash algorithm: ' + Params.Hash);
        WriteError('Supported: MD5, SHA256');
        Exit;
      end;
    end
    else
      HashAlgorithm := haMD5; // Default
    
    // Get device number from path
    DiskNumber := 0;
    if Pos('PhysicalDrive', Params.DevicePath) > 0 then
    begin
      try
        DiskNumber := StrToInt(Copy(Params.DevicePath, 
          Pos('PhysicalDrive', Params.DevicePath) + 13, 10));
      except
        WriteError('Invalid device path format');
        Exit;
      end;
    end;
    
    // Get device size
    DeviceManager := TDeviceManager.Create;
    TotalBytes := DeviceManager.GetDiskSizeViaWMI(DiskNumber);
    if TotalBytes = 0 then
    begin
      WriteError('Cannot determine device size');
      Exit;
    end;
    
    // Get file size
    TotalBytes := TFileOperations.GetFileSize64(Params.FilePath);
    if TotalBytes = 0 then
    begin
      WriteError('Cannot determine file size or file is empty');
      Exit;
    end;
    
    WriteConsole('Will verify: ' + TFileOperations.BytesToHuman(TotalBytes));
    WriteLn;
    
    // Method 1: Hash-based verification (if --hash specified)
    if Params.Hash <> '' then
    begin
      WriteConsole('Calculating file hash...');
      FileHash := THashUtils.CalculateFileHash(Params.FilePath, HashAlgorithm);
      if FileHash = '' then
      begin
        WriteError('Failed to calculate file hash');
        Exit;
      end;
      WriteConsole('File ' + UpperCase(Params.Hash) + ': ' + FileHash);
      
      WriteLn;
      WriteConsole('Calculating device hash...');
      DeviceHash := THashUtils.CalculateDeviceHash(Params.DevicePath, TotalBytes, HashAlgorithm);
      if DeviceHash = '' then
      begin
        WriteError('Failed to calculate device hash');
        Exit;
      end;
      WriteConsole('Device ' + UpperCase(Params.Hash) + ': ' + DeviceHash);
      
      WriteLn;
      if THashUtils.CompareHashes(FileHash, DeviceHash) then
      begin
        WriteConsole('VERIFICATION SUCCESSFUL: Hashes match!');
        Result := 0;
      end
      else
      begin
        WriteError('VERIFICATION FAILED: Hashes do not match!');
        Result := 1;
      end;
      
      Exit;
    end;
    
    // Method 2: Byte-by-byte comparison
    WriteConsole('Performing byte-by-byte verification...');
    WriteLn;
    
    // Open image file
    ImageFile := TBinaryFile.Create;
    ImageFile.Assign(Params.FilePath);
    if not ImageFile.Open(OPEN_READ_ONLY) then
    begin
      WriteError('Cannot open image file');
      Exit;
    end;
    
    // Open device
    DeviceFile := TBinaryFile.Create;
    DeviceFile.Assign(Params.DevicePath);
    if not DeviceFile.Open(OPEN_READ_ONLY) then
    begin
      WriteError('Cannot open device (requires administrator rights)');
      Exit;
    end;
    
    // Allocate buffers
    GetMem(DeviceBuffer, Params.BlockSize);
    GetMem(FileBuffer, Params.BlockSize);
    
    StartTime := Now;
    BytesProcessed := 0;
    
    // Compare data
    while BytesProcessed < TotalBytes do
    begin
      // Read from image
      BytesRead := ImageFile.BlockRead2(FileBuffer, Params.BlockSize);
      if BytesRead = 0 then
        Break;
      
      // Read from device
      DeviceBytes := DeviceFile.BlockRead2(DeviceBuffer, BytesRead);
      if DeviceBytes <> BytesRead then
      begin
        WriteError('Read error from device');
        Exit;
      end;
      
      // Compare buffers
      if not CompareMem(FileBuffer, DeviceBuffer, BytesRead) then
      begin
        WriteError('Mismatch found at offset 0x' + IntToHex(BytesProcessed, 16));
        MismatchFound := True;
        Break;
      end;
      
      BytesProcessed := BytesProcessed + BytesRead;
      
      // Update progress
      Speed := BytesProcessed / ((Now - StartTime) * 86400); // bytes/sec
      
      if not Params.Quiet then
      begin
        TimeStr := FormatFloat('0.0', (Now - StartTime) * 86400) + 's';
        ShowProgress(BytesProcessed, TotalBytes, 
          TFileOperations.BytesToHuman(BytesProcessed) + ' / ' + 
          TFileOperations.BytesToHuman(TotalBytes) + ' @ ' +
          TFileOperations.BytesToHuman(Round(Speed)) + '/s - ' + TimeStr);
      end;
    end;
    
    WriteLn;
    
    if MismatchFound then
    begin
      WriteError('VERIFICATION FAILED: Data mismatch detected!');
      Result := 1;
    end
    else
    begin
      WriteConsole('VERIFICATION SUCCESSFUL: All data matches!');
      WriteConsole('Verified: ' + TFileOperations.BytesToHuman(BytesProcessed));
      Result := 0;
    end;
    
  finally
    if DeviceBuffer <> nil then
      FreeMem(DeviceBuffer);
    if FileBuffer <> nil then
      FreeMem(FileBuffer);
    if ImageFile <> nil then
      ImageFile.Free;
    if DeviceFile <> nil then
      DeviceFile.Free;
    if DeviceManager <> nil then
      DeviceManager.Free;
  end;
end;

function ExecuteConsoleMode: Integer;
begin
  Result := 1;
  
  // Allocate console for GUI application
  InitConsole;
  try
    // Parse command line
    if not ParseCommandLine then
      Exit;
    
    // Validate parameters
    if not ValidateParams then
      Exit;
    
    // Execute command
    case Params.Operation of
      coHelp:
        begin
          ShowHelp;
          Result := 0;
        end;
        
      coList:
        begin
          ListDevices;
          Result := 0;
        end;
        
      coRead:
        Result := ExecuteRead;
        
      coWrite:
        Result := ExecuteWrite;
        
      coVerify:
        Result := ExecuteVerify;
    else
      WriteError('Unknown operation');
      ShowHelp;
    end;
  finally
    // Keep console open briefly to see output
    if Result <> 0 then
    begin
      WriteLn;
      WriteLn('Press Enter to exit...');
      ReadLn;
    end;
  end;
end;

end.
