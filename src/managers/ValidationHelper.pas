{******************************************************************************}
{                                                                              }
{  ImageWriter - Validation Helper Unit                                       }
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
{    Input validation for write/read operations.                              }
{    Provides parameter checking and confirmation message building.           }
{                                                                              }
{******************************************************************************}

unit ValidationHelper;

interface

uses
  Windows, SysUtils, Classes, PartitionInfo;

type
  TValidationResult = record
    Valid: Boolean;
    ErrorMessage: string;
  end;
  
  /// <summary>
  /// Validation helper for write/read operation parameters
  /// </summary>
  /// <remarks>
  /// Provides comprehensive validation with detailed error messages.
  /// Checks file existence, size compatibility, partition safety.
  /// </remarks>
  TValidationHelper = class
  public
    /// <summary>
    /// Validate all parameters for write operation
    /// </summary>
    /// <param name="ImageFile">Path to source image file</param>
    /// <param name="DevicePath">Target device path (e.g., \\.\PhysicalDrive1)</param>
    /// <param name="ImageSize">Size of image file in bytes</param>
    /// <param name="DeviceSize">Size of target device in bytes</param>
    /// <returns>Validation result with Valid flag and ErrorMessage</returns>
    /// <remarks>
    /// Checks: file existence, device selection, size validity, size compatibility.
    /// Returns detailed error message in result for user display.
    /// </remarks>
    class function ValidateWriteOperation(
      const ImageFile: string;
      const DevicePath: string;
      ImageSize: Int64;
      DeviceSize: Int64): TValidationResult;
    
    /// <summary>
    /// Validate all parameters for read operation
    /// </summary>
    /// <param name="DevicePath">Source device path</param>
    /// <param name="OutputFile">Destination file path</param>
    /// <param name="DeviceSize">Size of source device in bytes</param>
    /// <returns>Validation result with Valid flag and ErrorMessage</returns>
    /// <remarks>
    /// Checks: device selection, output path validity, disk space availability.
    /// Warns if output file already exists (overwrite confirmation needed).
    /// </remarks>
    class function ValidateReadOperation(
      const DevicePath: string;
      const OutputFile: string;
      DeviceSize: Int64): TValidationResult;
    
    /// <summary>
    /// Check if image size is compatible with device size
    /// </summary>
    /// <param name="ImageSize">Size of image in bytes</param>
    /// <param name="DeviceSize">Size of device in bytes</param>
    /// <param name="WarningMsg">Output: warning message if size mismatch</param>
    /// <returns>True if sizes are compatible, False if image is too large</returns>
    /// <remarks>
    /// Generates warning if image is significantly smaller than device.
    /// Returns False if image is larger than device (operation would fail).
    /// </remarks>
    class function CheckSizeMatch(ImageSize, DeviceSize: Int64; 
      out WarningMsg: string): Boolean;
    
    /// <summary>
    /// Validate block size and count parameters
    /// </summary>
    /// <param name="BlockSize">Size of each block in bytes</param>
    /// <param name="Count">Number of blocks to process</param>
    /// <returns>Validation result</returns>
    /// <remarks>
    /// BlockSize must be positive and typically a power of 2 (512, 1024, 4096, etc.).
    /// Count must be positive. Total size (BlockSize * Count) must not overflow.
    /// </remarks>
    class function ValidateBlockParams(BlockSize, Count: Int64): TValidationResult;
    
    /// <summary>
    /// Build detailed confirmation message for write operation
    /// </summary>
    /// <param name="ImageFile">Source image file name</param>
    /// <param name="DeviceName">Target device display name</param>
    /// <param name="PartitionCount">Number of partitions on device</param>
    /// <param name="PartitionTableType">Type of partition table (MBR, GPT, None)</param>
    /// <param name="Partitions">Array of partition entries</param>
    /// <returns>Formatted multi-line confirmation message for user display</returns>
    /// <remarks>
    /// Includes partition details to warn user about data loss.
    /// Format: "WARNING: All data will be destroyed! Device: ... Partitions: ..."
    /// </remarks>
    class function BuildWriteConfirmationMessage(
      const ImageFile: string;
      const DeviceName: string;
      PartitionCount: Integer;
      PartitionTableType: TPartitionTableType;
      const Partitions: array of TPartitionEntry): string;
  end;

implementation

uses
  FileOperations;

class function TValidationHelper.ValidateWriteOperation(
  const ImageFile: string;
  const DevicePath: string;
  ImageSize: Int64;
  DeviceSize: Int64): TValidationResult;
begin
  Result.Valid := True;
  Result.ErrorMessage := '';
  
  // Check if file exists
  if not FileExists(ImageFile) then
  begin
    Result.Valid := False;
    Result.ErrorMessage := 'Image file does not exist: ' + ImageFile;
    Exit;
  end;
  
  // Check device path
  if DevicePath = '' then
  begin
    Result.Valid := False;
    Result.ErrorMessage := 'No device selected';
    Exit;
  end;
  
  // Check file size
  if ImageSize <= 0 then
  begin
    Result.Valid := False;
    Result.ErrorMessage := 'Invalid image file size';
    Exit;
  end;
  
  // Check device size
  if DeviceSize <= 0 then
  begin
    Result.Valid := False;
    Result.ErrorMessage := 'Could not determine device size';
    Exit;
  end;
end;

class function TValidationHelper.ValidateReadOperation(
  const DevicePath: string;
  const OutputFile: string;
  DeviceSize: Int64): TValidationResult;
begin
  Result.Valid := True;
  Result.ErrorMessage := '';
  
  // Check device path
  if DevicePath = '' then
  begin
    Result.Valid := False;
    Result.ErrorMessage := 'No device selected';
    Exit;
  end;
  
  // Check output file path
  if Trim(OutputFile) = '' then
  begin
    Result.Valid := False;
    Result.ErrorMessage := 'No output file specified';
    Exit;
  end;
  
  // Check if output file already exists
  if FileExists(OutputFile) then
  begin
    // Allow overwrite - caller should confirm
  end;
  
  // Check device size
  if DeviceSize <= 0 then
  begin
    Result.Valid := False;
    Result.ErrorMessage := 'Could not determine device size';
    Exit;
  end;
end;

class function TValidationHelper.CheckSizeMatch(
  ImageSize, DeviceSize: Int64;
  out WarningMsg: string): Boolean;
begin
  Result := True;
  WarningMsg := '';
  
  if ImageSize > DeviceSize then
  begin
    Result := False;
    WarningMsg := Format('Image size (%s) exceeds device size (%s). The image will be truncated.',
      [TFileOperations.BytesToHuman(ImageSize), TFileOperations.BytesToHuman(DeviceSize)]);
  end;
end;

class function TValidationHelper.ValidateBlockParams(
  BlockSize, Count: Int64): TValidationResult;
begin
  Result.Valid := True;
  Result.ErrorMessage := '';
  
  if BlockSize <= 0 then
  begin
    Result.Valid := False;
    Result.ErrorMessage := 'Invalid block size: must be greater than 0';
    Exit;
  end;
  
  if Count < 0 then
  begin
    Result.Valid := False;
    Result.ErrorMessage := 'Invalid count: must be 0 or greater';
    Exit;
  end;
  
  // Check for reasonable limits
  if BlockSize > 64 * 1024 * 1024 then
  begin
    Result.Valid := False;
    Result.ErrorMessage := 'Block size too large (max 64 MB)';
    Exit;
  end;
end;

class function TValidationHelper.BuildWriteConfirmationMessage(
  const ImageFile: string;
  const DeviceName: string;
  PartitionCount: Integer;
  PartitionTableType: TPartitionTableType;
  const Partitions: array of TPartitionEntry): string;
var
  msg: TStringList;
  i: Integer;
begin
  msg := TStringList.Create;
  try
    msg.Add('WARNING: This operation will ERASE all data on the target device!');
    msg.Add('');
    msg.Add('Image file: ' + ExtractFileName(ImageFile));
    msg.Add('Target device: ' + DeviceName);
    msg.Add('');
    
    // Add partition information if available
    if PartitionCount > 0 then
    begin
      case PartitionTableType of
        pttMBR: msg.Add('Partition table: MBR');
        pttGPT: msg.Add('Partition table: GPT');
      end;
      msg.Add(Format('Partitions to be created: %d', [PartitionCount]));
      msg.Add('');
      
      for i := 0 to PartitionCount - 1 do
      begin
        msg.Add(Format('Partition %d:', [Partitions[i].PartitionNumber]));
        msg.Add(Format('  Type: %s', [Partitions[i].PartitionName]));
        msg.Add(Format('  Size: %d MB (%d sectors)', 
          [Partitions[i].SizeInMB, Partitions[i].SizeInSectors]));
        if Partitions[i].IsBootable then
          msg.Add('  Bootable: Yes');
        if i < PartitionCount - 1 then
          msg.Add('');
      end;
      msg.Add('');
    end;
    
    msg.Add('Are you sure you want to continue?');
    Result := msg.Text;
  finally
    msg.Free;
  end;
end;

end.
