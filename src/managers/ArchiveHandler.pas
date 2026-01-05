{******************************************************************************}
{                                                                              }
{  ArchiveHandler - Archive Compression/Decompression Module                  }
{                                                                              }
{  Copyright (c) 2024-2025 Anton Zelenov <tixset@gmail.com>                   }
{  GitHub: https://github.com/tixset/ImageWriter                              }
{                                                                              }
{  This module provides utilities for handling ZIP and GZIP archives during   }
{  read/write operations. Supports streaming compression and decompression.   }
{                                                                              }
{******************************************************************************}

unit ArchiveHandler;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, SysUtils, Classes, Forms, Dialogs, ExtractZLib, GZipStream, 
  XZStream, BZip2Stream, TarStream, SevenZipStream, NetShareAuth;

type
  TLogProc = procedure(const Msg: string; Level: Integer) of object;
  TStreamingCopyToDeviceFunc = function(InStream: TStream; const OutFileName: string; BlockSize, TotalSize: Int64): Boolean;
  TStreamingCopyFromDeviceFunc = function(const DevicePath: string; OutStream: TStream; BlockSize, TotalSize: Int64): Boolean;
  
  TArchiveType = (atNone, atZip, atGZip, atXZ, atBZip2, at7Zip, atTarGz, atTarXz);
  
  /// <summary>
  /// Archive handler for compressed image file support
  /// </summary>
  /// <remarks>
  /// Supports ZIP and GZIP formats with streaming decompression.
  /// Enables direct write of compressed images to devices without temp files.
  /// Read operations can compress device contents to archive format.
  /// </remarks>
  TArchiveHandler = class
  private
    class function GetArchiveComment(const FileName: string; ArchiveType: TArchiveType): string;
  public
    /// <summary>
    /// Detect archive type from file extension
    /// </summary>
    /// <param name="FileName">File path to analyze</param>
    /// <returns>Archive type: atNone, atZip, atGZip, atXZ, atBZip2, at7Zip, atTarGz, atTarXz</returns>
    /// <remarks>
    /// Detection is extension-based: .zip -> atZip, .gz/.gzip -> atGZip, .xz -> atXZ, 
    /// .bz2/.bzip2 -> atBZip2, .7z -> at7Zip, .tar.gz -> atTarGz, .tar.xz -> atTarXz
    /// </remarks>
    class function DetectArchiveType(const FileName: string): TArchiveType;
    
    /// <summary>
    /// Display archive comment to user (if present)
    /// </summary>
    /// <param name="FileName">Archive file path</param>
    /// <param name="ArchiveType">Type of archive</param>
    /// <returns>True if comment was displayed</returns>
    /// <remarks>
    /// ZIP files may contain embedded comments with warnings or instructions.
    /// </remarks>
    class function ShowArchiveComment(const FileName: string; ArchiveType: TArchiveType): Boolean;
    
    /// <summary>
    /// Write decompressed ZIP archive contents to device
    /// </summary>
    /// <param name="ArchiveFile">Source ZIP file path</param>
    /// <param name="DevicePath">Target device path (e.g., \\.\PhysicalDrive1)</param>
    /// <param name="BlockSize">I/O block size in bytes</param>
    /// <param name="UncompressedSize">Output: actual uncompressed size written</param>
    /// <param name="CancelRequested">Input/Output: cancellation flag</param>
    /// <param name="LogProc">Logging callback procedure</param>
    /// <param name="StreamingCopyToDevice">Device write callback function</param>
    /// <returns>True if successful, False on error</returns>
    /// <remarks>
    /// Extracts first file from ZIP in memory, streams to device.
    /// Supports ZIP64 format for >4GB files. Handles password-protected archives.
    /// </remarks>
    class function WriteZipToDevice(
      const ArchiveFile, DevicePath: string;
      BlockSize: Int64;
      var UncompressedSize: Int64;
      var CancelRequested: Boolean;
      LogProc: TLogProc;
      StreamingCopyToDevice: TStreamingCopyToDeviceFunc): Boolean;
      
    /// <summary>
    /// Write decompressed GZIP archive contents to device
    /// </summary>
    /// <param name="ArchiveFile">Source GZIP file path (.gz)</param>
    /// <param name="DevicePath">Target device path</param>
    /// <param name="BlockSize">I/O block size in bytes</param>
    /// <param name="DeviceSize">Target device size (for size validation)</param>
    /// <param name="EstimatedSize">Output: estimated uncompressed size</param>
    /// <param name="CancelRequested">Input/Output: cancellation flag</param>
    /// <param name="LogProc">Logging callback procedure</param>
    /// <param name="StreamingCopyToDevice">Device write callback function</param>
    /// <returns>True if successful, False on error</returns>
    /// <remarks>
    /// Streaming decompression - no temp file created. Memory-efficient.
    /// GZIP format doesn't store uncompressed size, so estimation may be needed.
    /// </remarks>
    class function WriteGZipToDevice(
      const ArchiveFile, DevicePath: string;
      BlockSize: Int64;
      DeviceSize: Int64;
      var EstimatedSize: Int64;
      var CancelRequested: Boolean;
      LogProc: TLogProc;
      StreamingCopyToDevice: TStreamingCopyToDeviceFunc): Boolean;
    
    /// <summary>
    /// Write decompressed XZ archive contents to device
    /// </summary>
    /// <param name="ArchiveFile">Source XZ file path (.xz)</param>
    /// <param name="DevicePath">Target device path</param>
    /// <param name="BlockSize">I/O block size in bytes</param>
    /// <param name="DeviceSize">Target device size (for size validation)</param>
    /// <param name="EstimatedSize">Output: estimated uncompressed size</param>
    /// <param name="CancelRequested">Input/Output: cancellation flag</param>
    /// <param name="LogProc">Logging callback procedure</param>
    /// <param name="StreamingCopyToDevice">Device write callback function</param>
    /// <returns>True if successful, False on error</returns>
    /// <remarks>
    /// Streaming LZMA decompression - no temp file created. Memory-efficient.
    /// XZ format provides excellent compression for disk images.
    /// </remarks>
    class function WriteXZToDevice(
      const ArchiveFile, DevicePath: string;
      BlockSize: Int64;
      DeviceSize: Int64;
      var EstimatedSize: Int64;
      var CancelRequested: Boolean;
      LogProc: TLogProc;
      StreamingCopyToDevice: TStreamingCopyToDeviceFunc): Boolean;
    
    /// <summary>
    /// Write decompressed BZIP2 archive contents to device
    /// </summary>
    /// <param name="ArchiveFile">Source BZIP2 file path (.bz2)</param>
    /// <param name="DevicePath">Target device path</param>
    /// <param name="BlockSize">I/O block size in bytes</param>
    /// <param name="DeviceSize">Target device size (for size validation)</param>
    /// <param name="EstimatedSize">Output: estimated uncompressed size</param>
    /// <param name="CancelRequested">Input/Output: cancellation flag</param>
    /// <param name="LogProc">Logging callback procedure</param>
    /// <param name="StreamingCopyToDevice">Device write callback function</param>
    /// <returns>True if successful, False on error</returns>
    /// <remarks>
    /// Streaming BZIP2 decompression - no temp file created.
    /// BZIP2 provides good compression with moderate CPU usage.
    /// </remarks>
    class function WriteBZip2ToDevice(
      const ArchiveFile, DevicePath: string;
      BlockSize: Int64;
      DeviceSize: Int64;
      var EstimatedSize: Int64;
      var CancelRequested: Boolean;
      LogProc: TLogProc;
      StreamingCopyToDevice: TStreamingCopyToDeviceFunc): Boolean;
    
    /// <summary>
    /// Write decompressed tar.gz archive contents to device
    /// </summary>
    /// <param name="ArchiveFile">Source tar.gz file path</param>
    /// <param name="DevicePath">Target device path</param>
    /// <param name="BlockSize">I/O block size in bytes</param>
    /// <param name="DeviceSize">Target device size (for size validation)</param>
    /// <param name="EstimatedSize">Output: estimated uncompressed size</param>
    /// <param name="CancelRequested">Input/Output: cancellation flag</param>
    /// <param name="LogProc">Logging callback procedure</param>
    /// <param name="StreamingCopyToDevice">Device write callback function</param>
    /// <returns>True if successful, False on error</returns>
    /// <remarks>
    /// Two-layer decompression: GZIP then TAR extraction.
    /// Extracts first file from TAR archive.
    /// </remarks>
    class function WriteTarGzToDevice(
      const ArchiveFile, DevicePath: string;
      BlockSize: Int64;
      DeviceSize: Int64;
      var EstimatedSize: Int64;
      var CancelRequested: Boolean;
      LogProc: TLogProc;
      StreamingCopyToDevice: TStreamingCopyToDeviceFunc): Boolean;
    
    /// <summary>
    /// Write decompressed tar.xz archive contents to device
    /// </summary>
    /// <param name="ArchiveFile">Source tar.xz file path</param>
    /// <param name="DevicePath">Target device path</param>
    /// <param name="BlockSize">I/O block size in bytes</param>
    /// <param name="DeviceSize">Target device size (for size validation)</param>
    /// <param name="EstimatedSize">Output: estimated uncompressed size</param>
    /// <param name="CancelRequested">Input/Output: cancellation flag</param>
    /// <param name="LogProc">Logging callback procedure</param>
    /// <param name="StreamingCopyToDevice">Device write callback function</param>
    /// <returns>True if successful, False on error</returns>
    /// <remarks>
    /// Two-layer decompression: XZ then TAR extraction.
    /// Extracts first file from TAR archive.
    /// </remarks>
    class function WriteTarXzToDevice(
      const ArchiveFile, DevicePath: string;
      BlockSize: Int64;
      DeviceSize: Int64;
      var EstimatedSize: Int64;
      var CancelRequested: Boolean;
      LogProc: TLogProc;
      StreamingCopyToDevice: TStreamingCopyToDeviceFunc): Boolean;
    
    /// <summary>
    /// Write decompressed 7z archive contents to device
    /// </summary>
    /// <param name="ArchiveFile">Source 7z file path</param>
    /// <param name="DevicePath">Target device path</param>
    /// <param name="BlockSize">I/O block size in bytes</param>
    /// <param name="DeviceSize">Target device size (for size validation)</param>
    /// <param name="EstimatedSize">Output: estimated uncompressed size</param>
    /// <param name="CancelRequested">Input/Output: cancellation flag</param>
    /// <param name="LogProc">Logging callback procedure</param>
    /// <param name="StreamingCopyToDevice">Device write callback function</param>
    /// <returns>True if successful, False on error</returns>
    /// <remarks>
    /// Uses 7z.exe for extraction. Requires 7-Zip installed.
    /// Extracts first file from 7z archive.
    /// </remarks>
    class function Write7ZipToDevice(
      const ArchiveFile, DevicePath: string;
      BlockSize: Int64;
      DeviceSize: Int64;
      var EstimatedSize: Int64;
      var CancelRequested: Boolean;
      LogProc: TLogProc;
      StreamingCopyToDevice: TStreamingCopyToDeviceFunc): Boolean;
    
    /// <summary>
    /// Read device contents and compress to ZIP archive
    /// </summary>
    /// <param name="DevicePath">Source device path</param>
    /// <param name="ArchiveFile">Target ZIP file path</param>
    /// <param name="BlockSize">I/O block size in bytes</param>
    /// <param name="DeviceSize">Source device size in bytes</param>
    /// <param name="CancelRequested">Input/Output: cancellation flag</param>
    /// <param name="LogProc">Logging callback procedure</param>
    /// <param name="StreamingCopyFromDevice">Device read callback function</param>
    /// <returns>True if successful, False on error</returns>
    /// <remarks>
    /// Creates ZIP archive with single entry named after device.
    /// Compression level is optimized for speed vs. size ratio.
    /// </remarks>
    class function ReadDeviceToZip(
      const DevicePath, ArchiveFile: string;
      BlockSize, DeviceSize: Int64;
      var CancelRequested: Boolean;
      LogProc: TLogProc;
      StreamingCopyFromDevice: TStreamingCopyFromDeviceFunc): Boolean;
      
    /// <summary>
    /// Read device contents and compress to GZIP archive
    /// </summary>
    /// <param name="DevicePath">Source device path</param>
    /// <param name="ArchiveFile">Target GZIP file path (.gz)</param>
    /// <param name="BlockSize">I/O block size in bytes</param>
    /// <param name="DeviceSize">Source device size in bytes</param>
    /// <param name="CancelRequested">Input/Output: cancellation flag</param>
    /// <param name="LogProc">Logging callback procedure</param>
    /// <param name="StreamingCopyFromDevice">Device read callback function</param>
    /// <returns>True if successful, False on error</returns>
    /// <remarks>
    /// Streaming compression - device data is compressed on-the-fly.
    /// GZIP provides better compression than ZIP for disk images.
    /// </remarks>
    class function ReadDeviceToGZip(
      const DevicePath, ArchiveFile: string;
      BlockSize, DeviceSize: Int64;
      var CancelRequested: Boolean;
      LogProc: TLogProc;
      StreamingCopyFromDevice: TStreamingCopyFromDeviceFunc): Boolean;
      
    /// <summary>
    /// Read device contents and compress to XZ archive
    /// </summary>
    /// <param name="DevicePath">Source device path</param>
    /// <param name="ArchiveFile">Target XZ file path (.xz)</param>
    /// <param name="BlockSize">I/O block size in bytes</param>
    /// <param name="DeviceSize">Source device size in bytes</param>
    /// <param name="CancelRequested">Input/Output: cancellation flag</param>
    /// <param name="LogProc">Logging callback procedure</param>
    /// <param name="StreamingCopyFromDevice">Device read callback function</param>
    /// <returns>True if successful, False on error</returns>
    /// <remarks>
    /// Uses 7z.exe with stdin pipe for streaming compression.
    /// XZ/LZMA provides excellent compression for disk images (4:1 typical).
    /// </remarks>
    class function ReadDeviceToXZ(
      const DevicePath, ArchiveFile: string;
      BlockSize, DeviceSize: Int64;
      var CancelRequested: Boolean;
      LogProc: TLogProc;
      StreamingCopyFromDevice: TStreamingCopyFromDeviceFunc): Boolean;
      
    /// <summary>
    /// Read device contents and compress to BZIP2 archive
    /// </summary>
    /// <param name="DevicePath">Source device path</param>
    /// <param name="ArchiveFile">Target BZIP2 file path (.bz2)</param>
    /// <param name="BlockSize">I/O block size in bytes</param>
    /// <param name="DeviceSize">Source device size in bytes</param>
    /// <param name="CancelRequested">Input/Output: cancellation flag</param>
    /// <param name="LogProc">Logging callback procedure</param>
    /// <param name="StreamingCopyFromDevice">Device read callback function</param>
    /// <returns>True if successful, False on error</returns>
    /// <remarks>
    /// Uses 7z.exe with stdin pipe for streaming compression.
    /// BZIP2 provides good compression for disk images (3:1 typical).
    /// </remarks>
    class function ReadDeviceToBZip2(
      const DevicePath, ArchiveFile: string;
      BlockSize, DeviceSize: Int64;
      var CancelRequested: Boolean;
      LogProc: TLogProc;
      StreamingCopyFromDevice: TStreamingCopyFromDeviceFunc): Boolean;
      
    /// <summary>
    /// Read device contents and compress to 7z archive
    /// </summary>
    /// <param name="DevicePath">Source device path</param>
    /// <param name="ArchiveFile">Target 7z file path (.7z)</param>
    /// <param name="BlockSize">I/O block size in bytes</param>
    /// <param name="DeviceSize">Source device size in bytes</param>
    /// <param name="CancelRequested">Input/Output: cancellation flag</param>
    /// <param name="LogProc">Logging callback procedure</param>
    /// <param name="StreamingCopyFromDevice">Device read callback function</param>
    /// <returns>True if successful, False on error</returns>
    /// <remarks>
    /// Uses 7z.exe with stdin pipe for streaming compression.
    /// 7z format provides best compression with LZMA2 algorithm.
    /// </remarks>
    class function ReadDeviceTo7Zip(
      const DevicePath, ArchiveFile: string;
      BlockSize, DeviceSize: Int64;
      var CancelRequested: Boolean;
      LogProc: TLogProc;
      StreamingCopyFromDevice: TStreamingCopyFromDeviceFunc): Boolean;
  end;

implementation

const
  llDebug = 0;
  llInfo = 1;
  llWarning = 2;
  llError = 3;
  mrYes = 6; // Dialog result constant

class function TArchiveHandler.DetectArchiveType(const FileName: string): TArchiveType;
var
  Ext, BaseExt: string;
begin
  Result := atNone;
  Ext := LowerCase(ExtractFileExt(FileName));
  
  // ZIP format
  if (Ext = '.zip') then
  begin
    Result := atZip;
    Exit;
  end;
  
  // 7-Zip format
  if (Ext = '.7z') then
  begin
    Result := at7Zip;
    Exit;
  end;
  
  // BZIP2 format
  if (Ext = '.bz2') or (Ext = '.bzip2') then
  begin
    Result := atBZip2;
    Exit;
  end;
  
  // XZ format (may be .tar.xz)
  if (Ext = '.xz') then
  begin
    BaseExt := LowerCase(ExtractFileExt(ChangeFileExt(FileName, '')));
    if BaseExt = '.tar' then
      Result := atTarXz
    else
      Result := atXZ;
    Exit;
  end;
  
  // GZIP format (may be .tar.gz)
  if (Ext = '.gz') or (Ext = '.gzip') then
  begin
    Result := atGZip;
    BaseExt := LowerCase(ExtractFileExt(ChangeFileExt(FileName, '')));
    if BaseExt = '.tar' then
      Result := atTarGz;
  end;
end;

class function TArchiveHandler.GetArchiveComment(const FileName: string; ArchiveType: TArchiveType): string;
begin
  Result := '';
  case ArchiveType of
    atZip: Result := GetZipComment(FileName);
    atGZip: Result := GetGZipComment(FileName);
  end;
end;

class function TArchiveHandler.ShowArchiveComment(const FileName: string; ArchiveType: TArchiveType): Boolean;
var
  Comment: string;
begin
  Result := True; // Default: proceed with operation
  Comment := GetArchiveComment(FileName, ArchiveType);
  if Comment <> '' then
  begin
    Result := (MessageDlg(Comment, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
  end;
end;

class function TArchiveHandler.WriteZipToDevice(
  const ArchiveFile, DevicePath: string;
  BlockSize: Int64;
  var UncompressedSize: Int64;
  var CancelRequested: Boolean;
  LogProc: TLogProc;
  StreamingCopyToDevice: TStreamingCopyToDeviceFunc): Boolean;
var
  fileStream: TFileStream;
  zipStream: TZipInputStream;
  diskSize: Int64;
begin
  Result := False;
  
  try
    if not EnsureNetworkShareAccess(ArchiveFile) then
    begin
      if Assigned(LogProc) then
        LogProc('Network folder access error: ' + ArchiveFile, llError);
      Exit;
    end;
    
    fileStream := TFileStream.Create(ArchiveFile, fmOpenRead or fmShareDenyWrite);
    try
      zipStream := TZipInputStream.Create(fileStream);
      try
        diskSize := zipStream.UncompressedSize;
        UncompressedSize := diskSize;
        
        if diskSize > 0 then
        begin
          if Assigned(LogProc) then
            LogProc('Uncompressed size: ' + IntToStr(diskSize) + ' bytes', llInfo);
        end;
        
        if not StreamingCopyToDevice(zipStream, DevicePath, BlockSize, diskSize) then
        begin
          if Assigned(LogProc) then
            LogProc('ERROR: Streaming copy failed', llError);
          Result := False;
        end
        else if CancelRequested then
        begin
          if Assigned(LogProc) then
            LogProc('=== ZIP decompression cancelled ===', llWarning);
          Result := False;
        end
        else
        begin
          if Assigned(LogProc) then
            LogProc('=== ZIP decompression completed successfully ===', llInfo);
          Result := True;
        end;
      finally
        try
          zipStream.Free;
        except
          // Ignore errors when closing stream during cancel
        end;
      end;
    finally
      try
        fileStream.Free;
      except
        // Ignore errors when closing stream during cancel
      end;
    end;
  except
    on E: Exception do
    begin
      if Assigned(LogProc) then
        LogProc('ERROR: ' + E.Message, llError);
      Result := False;
    end;
  end;
end;

class function TArchiveHandler.WriteGZipToDevice(
  const ArchiveFile, DevicePath: string;
  BlockSize: Int64;
  DeviceSize: Int64;
  var EstimatedSize: Int64;
  var CancelRequested: Boolean;
  LogProc: TLogProc;
  StreamingCopyToDevice: TStreamingCopyToDeviceFunc): Boolean;
var
  fileStream: TFileStream;
  gzipStream: TGZipInputStream;
  diskSize: Int64;
begin
  Result := False;
  
  try
    if not EnsureNetworkShareAccess(ArchiveFile) then
    begin
      if Assigned(LogProc) then
        LogProc('Network folder access error: ' + ArchiveFile, llError);
      Exit;
    end;
    
    fileStream := TFileStream.Create(ArchiveFile, fmOpenRead or fmShareDenyWrite);
    try
      gzipStream := TGZipInputStream.Create(fileStream);
      try
        // For GZIP, original size is stored at the end of file (not in header)
        // Estimate based on compressed size * average compression ratio
        diskSize := fileStream.Size * 3; // Typical compression ratio 3:1 for disk images
        if Assigned(LogProc) then
          LogProc('Estimated uncompressed size (GZIP): ' + IntToStr(diskSize) + ' bytes (based on 3:1 ratio)', llInfo);
        
        // Check if estimate exceeds device size
        if (DeviceSize > 0) and (diskSize > DeviceSize) then
        begin
          diskSize := DeviceSize;
          if Assigned(LogProc) then
            LogProc('Using device size instead: ' + IntToStr(diskSize) + ' bytes', llInfo);
        end;
        
        EstimatedSize := diskSize;
        
        if not StreamingCopyToDevice(gzipStream, DevicePath, BlockSize, diskSize) then
        begin
          if Assigned(LogProc) then
            LogProc('ERROR: Streaming copy failed', llError);
          Result := False;
        end
        else if CancelRequested then
        begin
          if Assigned(LogProc) then
            LogProc('=== GZIP decompression cancelled ===', llWarning);
          Result := False;
        end
        else
        begin
          if Assigned(LogProc) then
            LogProc('=== GZIP decompression completed successfully ===', llInfo);
          Result := True;
        end;
      finally
        try
          gzipStream.Free;
        except
          // Ignore errors when closing stream during cancel
        end;
      end;
    finally
      try
        fileStream.Free;
      except
        // Ignore errors when closing stream during cancel
      end;
    end;
  except
    on E: Exception do
    begin
      if Assigned(LogProc) then
        LogProc('ERROR: ' + E.Message, llError);
      Result := False;
    end;
  end;
end;

class function TArchiveHandler.WriteXZToDevice(
  const ArchiveFile, DevicePath: string;
  BlockSize: Int64;
  DeviceSize: Int64;
  var EstimatedSize: Int64;
  var CancelRequested: Boolean;
  LogProc: TLogProc;
  StreamingCopyToDevice: TStreamingCopyToDeviceFunc): Boolean;
var
  fileStream: TFileStream;
  xzStream: TXZInputStream;
  diskSize: Int64;
begin
  Result := False;
  
  try
    if not EnsureNetworkShareAccess(ArchiveFile) then
    begin
      if Assigned(LogProc) then
        LogProc('Network folder access error: ' + ArchiveFile, llError);
      Exit;
    end;
    
    fileStream := TFileStream.Create(ArchiveFile, fmOpenRead or fmShareDenyWrite);
    try
      xzStream := TXZInputStream.Create(fileStream);
      try
        // XZ format doesn't store uncompressed size in header
        // Estimate based on compressed size * average compression ratio
        diskSize := fileStream.Size * 4; // Typical compression ratio 4:1 for XZ
        if Assigned(LogProc) then
          LogProc('Estimated uncompressed size (XZ): ' + IntToStr(diskSize) + ' bytes (based on 4:1 ratio)', llInfo);
        
        // Check if estimate exceeds device size
        if (DeviceSize > 0) and (diskSize > DeviceSize) then
        begin
          diskSize := DeviceSize;
          if Assigned(LogProc) then
            LogProc('Using device size instead: ' + IntToStr(diskSize) + ' bytes', llInfo);
        end;
        
        EstimatedSize := diskSize;
        
        if not StreamingCopyToDevice(xzStream, DevicePath, BlockSize, diskSize) then
        begin
          if Assigned(LogProc) then
            LogProc('ERROR: Streaming copy failed', llError);
          Result := False;
        end
        else if CancelRequested then
        begin
          if Assigned(LogProc) then
            LogProc('=== XZ decompression cancelled ===', llWarning);
          Result := False;
        end
        else
        begin
          if Assigned(LogProc) then
            LogProc('=== XZ decompression completed successfully ===', llInfo);
          Result := True;
        end;
      finally
        try
          xzStream.Free;
        except
          // Ignore errors when closing stream during cancel
        end;
      end;
    finally
      try
        fileStream.Free;
      except
        // Ignore errors when closing stream during cancel
      end;
    end;
  except
    on E: Exception do
    begin
      if Assigned(LogProc) then
        LogProc('ERROR: ' + E.Message, llError);
      Result := False;
    end;
  end;
end;

class function TArchiveHandler.WriteBZip2ToDevice(
  const ArchiveFile, DevicePath: string;
  BlockSize: Int64;
  DeviceSize: Int64;
  var EstimatedSize: Int64;
  var CancelRequested: Boolean;
  LogProc: TLogProc;
  StreamingCopyToDevice: TStreamingCopyToDeviceFunc): Boolean;
var
  fileStream: TFileStream;
  bzip2Stream: TBZip2InputStream;
  diskSize: Int64;
begin
  Result := False;
  
  try
    if not EnsureNetworkShareAccess(ArchiveFile) then
    begin
      if Assigned(LogProc) then
        LogProc('Network folder access error: ' + ArchiveFile, llError);
      Exit;
    end;
    
    fileStream := TFileStream.Create(ArchiveFile, fmOpenRead or fmShareDenyWrite);
    try
      bzip2Stream := TBZip2InputStream.Create(fileStream);
      try
        // BZIP2 format doesn't store uncompressed size
        // Estimate based on compressed size * average compression ratio
        diskSize := fileStream.Size * 3; // Typical compression ratio 3:1 for BZIP2
        if Assigned(LogProc) then
          LogProc('Estimated uncompressed size (BZIP2): ' + IntToStr(diskSize) + ' bytes (based on 3:1 ratio)', llInfo);
        
        // Check if estimate exceeds device size
        if (DeviceSize > 0) and (diskSize > DeviceSize) then
        begin
          diskSize := DeviceSize;
          if Assigned(LogProc) then
            LogProc('Using device size instead: ' + IntToStr(diskSize) + ' bytes', llInfo);
        end;
        
        EstimatedSize := diskSize;
        
        if not StreamingCopyToDevice(bzip2Stream, DevicePath, BlockSize, diskSize) then
        begin
          if Assigned(LogProc) then
            LogProc('ERROR: Streaming copy failed', llError);
          Result := False;
        end
        else if CancelRequested then
        begin
          if Assigned(LogProc) then
            LogProc('=== BZIP2 decompression cancelled ===', llWarning);
          Result := False;
        end
        else
        begin
          if Assigned(LogProc) then
            LogProc('=== BZIP2 decompression completed successfully ===', llInfo);
          Result := True;
        end;
      finally
        try
          bzip2Stream.Free;
        except
          // Ignore errors when closing stream during cancel
        end;
      end;
    finally
      try
        fileStream.Free;
      except
        // Ignore errors when closing stream during cancel
      end;
    end;
  except
    on E: Exception do
    begin
      if Assigned(LogProc) then
        LogProc('ERROR: ' + E.Message, llError);
      Result := False;
    end;
  end;
end;

class function TArchiveHandler.WriteTarGzToDevice(
  const ArchiveFile, DevicePath: string;
  BlockSize: Int64;
  DeviceSize: Int64;
  var EstimatedSize: Int64;
  var CancelRequested: Boolean;
  LogProc: TLogProc;
  StreamingCopyToDevice: TStreamingCopyToDeviceFunc): Boolean;
var
  fileStream: TFileStream;
  gzipStream: TGZipInputStream;
  tarStream: TTarInputStream;
  diskSize: Int64;
begin
  Result := False;
  
  try
    if not EnsureNetworkShareAccess(ArchiveFile) then
    begin
      if Assigned(LogProc) then
        LogProc('Network folder access error: ' + ArchiveFile, llError);
      Exit;
    end;
    
    fileStream := TFileStream.Create(ArchiveFile, fmOpenRead or fmShareDenyWrite);
    try
      gzipStream := TGZipInputStream.Create(fileStream);
      try
        tarStream := TTarInputStream.Create(gzipStream);
        try
          // Get actual file size from TAR header
          diskSize := tarStream.FileSize;
          if Assigned(LogProc) then
            LogProc('TAR file size: ' + IntToStr(diskSize) + ' bytes', llInfo);
          
          // Check if size exceeds device size
          if (DeviceSize > 0) and (diskSize > DeviceSize) then
          begin
            diskSize := DeviceSize;
            if Assigned(LogProc) then
              LogProc('Using device size instead: ' + IntToStr(diskSize) + ' bytes', llInfo);
          end;
          
          EstimatedSize := diskSize;
          
          if not StreamingCopyToDevice(tarStream, DevicePath, BlockSize, diskSize) then
          begin
            if Assigned(LogProc) then
              LogProc('ERROR: Streaming copy failed', llError);
            Result := False;
          end
          else if CancelRequested then
          begin
            if Assigned(LogProc) then
              LogProc('=== tar.gz decompression cancelled ===', llWarning);
            Result := False;
          end
          else
          begin
            if Assigned(LogProc) then
              LogProc('=== tar.gz decompression completed successfully ===', llInfo);
            Result := True;
          end;
        finally
          try
            tarStream.Free;
          except
            // Ignore errors when closing stream during cancel
          end;
        end;
      finally
        try
          gzipStream.Free;
        except
          // Ignore errors when closing stream during cancel
        end;
      end;
    finally
      try
        fileStream.Free;
      except
        // Ignore errors when closing stream during cancel
      end;
    end;
  except
    on E: Exception do
    begin
      if Assigned(LogProc) then
        LogProc('ERROR: ' + E.Message, llError);
      Result := False;
    end;
  end;
end;

class function TArchiveHandler.WriteTarXzToDevice(
  const ArchiveFile, DevicePath: string;
  BlockSize: Int64;
  DeviceSize: Int64;
  var EstimatedSize: Int64;
  var CancelRequested: Boolean;
  LogProc: TLogProc;
  StreamingCopyToDevice: TStreamingCopyToDeviceFunc): Boolean;
var
  fileStream: TFileStream;
  xzStream: TXZInputStream;
  tarStream: TTarInputStream;
  diskSize: Int64;
begin
  Result := False;
  
  try
    if not EnsureNetworkShareAccess(ArchiveFile) then
    begin
      if Assigned(LogProc) then
        LogProc('Network folder access error: ' + ArchiveFile, llError);
      Exit;
    end;
    
    fileStream := TFileStream.Create(ArchiveFile, fmOpenRead or fmShareDenyWrite);
    try
      xzStream := TXZInputStream.Create(fileStream);
      try
        tarStream := TTarInputStream.Create(xzStream);
        try
          // Get actual file size from TAR header
          diskSize := tarStream.FileSize;
          if Assigned(LogProc) then
            LogProc('TAR file size: ' + IntToStr(diskSize) + ' bytes', llInfo);
          
          // Check if size exceeds device size
          if (DeviceSize > 0) and (diskSize > DeviceSize) then
          begin
            diskSize := DeviceSize;
            if Assigned(LogProc) then
              LogProc('Using device size instead: ' + IntToStr(diskSize) + ' bytes', llInfo);
          end;
          
          EstimatedSize := diskSize;
          
          if not StreamingCopyToDevice(tarStream, DevicePath, BlockSize, diskSize) then
          begin
            if Assigned(LogProc) then
              LogProc('ERROR: Streaming copy failed', llError);
            Result := False;
          end
          else if CancelRequested then
          begin
            if Assigned(LogProc) then
              LogProc('=== tar.xz decompression cancelled ===', llWarning);
            Result := False;
          end
          else
          begin
            if Assigned(LogProc) then
              LogProc('=== tar.xz decompression completed successfully ===', llInfo);
            Result := True;
          end;
        finally
          try
            tarStream.Free;
          except
            // Ignore errors when closing stream during cancel
          end;
        end;
      finally
        try
          xzStream.Free;
        except
          // Ignore errors when closing stream during cancel
        end;
      end;
    finally
      try
        fileStream.Free;
      except
        // Ignore errors when closing stream during cancel
      end;
    end;
  except
    on E: Exception do
    begin
      if Assigned(LogProc) then
        LogProc('ERROR: ' + E.Message, llError);
      Result := False;
    end;
  end;
end;

class function TArchiveHandler.Write7ZipToDevice(
  const ArchiveFile, DevicePath: string;
  BlockSize: Int64;
  DeviceSize: Int64;
  var EstimatedSize: Int64;
  var CancelRequested: Boolean;
  LogProc: TLogProc;
  StreamingCopyToDevice: TStreamingCopyToDeviceFunc): Boolean;
var
  sevenZipStream: TSevenZipInputStream;
  diskSize: Int64;
begin
  Result := False;
  
  try
    if not EnsureNetworkShareAccess(ArchiveFile) then
    begin
      if Assigned(LogProc) then
        LogProc('Network folder access error: ' + ArchiveFile, llError);
      Exit;
    end;
    
    sevenZipStream := TSevenZipInputStream.Create(ArchiveFile);
    try
      // Get actual file size
      diskSize := sevenZipStream.Size;
      if Assigned(LogProc) then
        LogProc('7z file size: ' + IntToStr(diskSize) + ' bytes', llInfo);
      
      // Check if size exceeds device size
      if (DeviceSize > 0) and (diskSize > DeviceSize) then
      begin
        diskSize := DeviceSize;
        if Assigned(LogProc) then
          LogProc('Using device size instead: ' + IntToStr(diskSize) + ' bytes', llInfo);
      end;
      
      EstimatedSize := diskSize;
      
      if not StreamingCopyToDevice(sevenZipStream, DevicePath, BlockSize, diskSize) then
      begin
        if Assigned(LogProc) then
          LogProc('ERROR: Streaming copy failed', llError);
        Result := False;
      end
      else if CancelRequested then
      begin
        if Assigned(LogProc) then
          LogProc('=== 7z decompression cancelled ===', llWarning);
        Result := False;
      end
      else
      begin
        if Assigned(LogProc) then
          LogProc('=== 7z decompression completed successfully ===', llInfo);
        Result := True;
      end;
    finally
      try
        sevenZipStream.Free;
      except
        // Ignore errors when closing stream during cancel
      end;
    end;
  except
    on E: Exception do
    begin
      if Assigned(LogProc) then
        LogProc('ERROR: ' + E.Message, llError);
      Result := False;
    end;
  end;
end;

class function TArchiveHandler.ReadDeviceToZip(
  const DevicePath, ArchiveFile: string;
  BlockSize, DeviceSize: Int64;
  var CancelRequested: Boolean;
  LogProc: TLogProc;
  StreamingCopyFromDevice: TStreamingCopyFromDeviceFunc): Boolean;
var
  fileStream: TFileStream;
  zipStream: TZipOutputStream;
begin
  Result := False;
  
  try
    fileStream := TFileStream.Create(ArchiveFile, fmCreate);
    try
      // Use archive name for file inside (replace extension with .img)
      zipStream := TZipOutputStream.Create(fileStream, ChangeFileExt(ExtractFileName(ArchiveFile), '.img'));
      try
        if not StreamingCopyFromDevice(DevicePath, zipStream, BlockSize, DeviceSize) then
        begin
          if Assigned(LogProc) then
            LogProc('ERROR: Streaming copy failed', llError);
        end
        else if CancelRequested then
        begin
          if Assigned(LogProc) then
            LogProc('=== ZIP compression cancelled ===', llWarning);
        end
        else
        begin
          if Assigned(LogProc) then
            LogProc('=== ZIP compression completed successfully ===', llInfo);
          Result := True;
        end;
      finally
        try
          zipStream.Free;
        except
          // Ignore errors when closing stream during cancel
        end;
      end;
    finally
      try
        fileStream.Free;
      except
        // Ignore errors when closing stream during cancel
      end;
    end;
  except
    on E: Exception do
    begin
      if Assigned(LogProc) then
        LogProc('ERROR: ' + E.Message, llError);
      Result := False;
    end;
  end;
end;

class function TArchiveHandler.ReadDeviceToGZip(
  const DevicePath, ArchiveFile: string;
  BlockSize, DeviceSize: Int64;
  var CancelRequested: Boolean;
  LogProc: TLogProc;
  StreamingCopyFromDevice: TStreamingCopyFromDeviceFunc): Boolean;
var
  fileStream: TFileStream;
  gzipStream: TGZipOutputStream;
begin
  Result := False;
  
  try
    fileStream := TFileStream.Create(ArchiveFile, fmCreate);
    try
      gzipStream := TGZipOutputStream.Create(fileStream);
      try
        if not StreamingCopyFromDevice(DevicePath, gzipStream, BlockSize, DeviceSize) then
        begin
          if Assigned(LogProc) then
            LogProc('ERROR: Streaming copy failed', llError);
        end
        else if CancelRequested then
        begin
          if Assigned(LogProc) then
            LogProc('=== GZIP compression cancelled ===', llWarning);
        end
        else
        begin
          if Assigned(LogProc) then
            LogProc('=== GZIP compression completed successfully ===', llInfo);
          Result := True;
        end;
      finally
        try
          gzipStream.Free;
        except
          // Ignore errors when closing stream during cancel
        end;
      end;
    finally
      try
        fileStream.Free;
      except
        // Ignore errors when closing stream during cancel
      end;
    end;
  except
    on E: Exception do
    begin
      if Assigned(LogProc) then
        LogProc('ERROR: ' + E.Message, llError);
      Result := False;
    end;
  end;
end;

class function TArchiveHandler.ReadDeviceToXZ(
  const DevicePath, ArchiveFile: string;
  BlockSize, DeviceSize: Int64;
  var CancelRequested: Boolean;
  LogProc: TLogProc;
  StreamingCopyFromDevice: TStreamingCopyFromDeviceFunc): Boolean;
var
  xzStream: TSevenZipOutputStream;
begin
  Result := False;
  
  try
    xzStream := TSevenZipOutputStream.Create(ArchiveFile, 'txz');
    try
      if not StreamingCopyFromDevice(DevicePath, xzStream, BlockSize, DeviceSize) then
      begin
        if Assigned(LogProc) then
          LogProc('ERROR: Streaming copy failed', llError);
      end
      else if CancelRequested then
      begin
        if Assigned(LogProc) then
          LogProc('=== XZ compression cancelled ===', llWarning);
      end
      else
      begin
        if Assigned(LogProc) then
          LogProc('=== XZ compression completed successfully ===', llInfo);
        Result := True;
      end;
    finally
      try
        xzStream.Free;
      except
        // Ignore errors when closing stream during cancel
      end;
    end;
  except
    on E: Exception do
    begin
      if Assigned(LogProc) then
        LogProc('ERROR: ' + E.Message, llError);
      Result := False;
    end;
  end;
end;

class function TArchiveHandler.ReadDeviceToBZip2(
  const DevicePath, ArchiveFile: string;
  BlockSize, DeviceSize: Int64;
  var CancelRequested: Boolean;
  LogProc: TLogProc;
  StreamingCopyFromDevice: TStreamingCopyFromDeviceFunc): Boolean;
var
  bz2Stream: TSevenZipOutputStream;
begin
  Result := False;
  
  try
    bz2Stream := TSevenZipOutputStream.Create(ArchiveFile, 'tbz2');
    try
      if not StreamingCopyFromDevice(DevicePath, bz2Stream, BlockSize, DeviceSize) then
      begin
        if Assigned(LogProc) then
          LogProc('ERROR: Streaming copy failed', llError);
      end
      else if CancelRequested then
      begin
        if Assigned(LogProc) then
          LogProc('=== BZIP2 compression cancelled ===', llWarning);
      end
      else
      begin
        if Assigned(LogProc) then
          LogProc('=== BZIP2 compression completed successfully ===', llInfo);
        Result := True;
      end;
    finally
      try
        bz2Stream.Free;
      except
        // Ignore errors when closing stream during cancel
      end;
    end;
  except
    on E: Exception do
    begin
      if Assigned(LogProc) then
        LogProc('ERROR: ' + E.Message, llError);
      Result := False;
    end;
  end;
end;

class function TArchiveHandler.ReadDeviceTo7Zip(
  const DevicePath, ArchiveFile: string;
  BlockSize, DeviceSize: Int64;
  var CancelRequested: Boolean;
  LogProc: TLogProc;
  StreamingCopyFromDevice: TStreamingCopyFromDeviceFunc): Boolean;
var
  sevenzStream: TSevenZipOutputStream;
begin
  Result := False;
  
  try
    sevenzStream := TSevenZipOutputStream.Create(ArchiveFile, 't7z');
    try
      if not StreamingCopyFromDevice(DevicePath, sevenzStream, BlockSize, DeviceSize) then
      begin
        if Assigned(LogProc) then
          LogProc('ERROR: Streaming copy failed', llError);
      end
      else if CancelRequested then
      begin
        if Assigned(LogProc) then
          LogProc('=== 7z compression cancelled ===', llWarning);
      end
      else
      begin
        if Assigned(LogProc) then
          LogProc('=== 7z compression completed successfully ===', llInfo);
        Result := True;
      end;
    finally
      try
        sevenzStream.Free;
      except
        // Ignore errors when closing stream during cancel
      end;
    end;
  except
    on E: Exception do
    begin
      if Assigned(LogProc) then
        LogProc('ERROR: ' + E.Message, llError);
      Result := False;
    end;
  end;
end;

end.
