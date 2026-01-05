{******************************************************************************}
{                                                                              }
{  ArchivePartitionReader - Partition Analysis from Archives                  }
{                                                                              }
{  Copyright (c) 2024-2025 Anton Zelenov <tixset@gmail.com>                   }
{  GitHub: https://github.com/tixset/ImageWriter                              }
{                                                                              }
{  This module reads partition table info from disk images inside archives    }
{  without full extraction - only first 64KB needed for MBR/GPT analysis.     }
{                                                                              }
{******************************************************************************}

unit ArchivePartitionReader;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, SysUtils, Classes, PartitionInfo, ArchiveHandler,
  GZipStream, XZStream, BZip2Stream, TarStream, SevenZipStream;

type
  /// <summary>
  /// Helper class for reading partition tables from archived disk images
  /// </summary>
  TArchivePartitionReader = class
  public
    /// <summary>
    /// Detect partition table type in archived image
    /// </summary>
    /// <param name="ArchiveFile">Archive file path (.zip, .gz, .xz, etc.)</param>
    /// <param name="ArchiveType">Type of archive</param>
    /// <returns>Partition table type: pttMBR, pttGPT, or pttUnknown</returns>
    class function DetectPartitionTableInArchive(const ArchiveFile: string; ArchiveType: TArchiveType): TPartitionTableType;
    
    /// <summary>
    /// Parse MBR partitions from archived image
    /// </summary>
    /// <param name="ArchiveFile">Archive file path</param>
    /// <param name="ArchiveType">Type of archive</param>
    /// <param name="Partitions">Output array of partition entries</param>
    /// <returns>Number of partitions found</returns>
    class function ParseMBRPartitionsFromArchive(const ArchiveFile: string; ArchiveType: TArchiveType; var Partitions: array of TPartitionEntry): Integer;
    
    /// <summary>
    /// Parse GPT partitions from archived image
    /// </summary>
    /// <param name="ArchiveFile">Archive file path</param>
    /// <param name="ArchiveType">Type of archive</param>
    /// <param name="Partitions">Output array of partition entries</param>
    /// <returns>Number of partitions found</returns>
    class function ParseGPTPartitionsFromArchive(const ArchiveFile: string; ArchiveType: TArchiveType; var Partitions: array of TPartitionEntry): Integer;
    
  private
    /// <summary>
    /// Read first N bytes from archive into memory stream
    /// </summary>
    /// <param name="ArchiveFile">Archive file path</param>
    /// <param name="ArchiveType">Type of archive</param>
    /// <param name="MaxBytes">Maximum bytes to read (default: 65536 = 64KB for GPT)</param>
    /// <returns>Memory stream with decompressed data, or nil on error</returns>
    class function ReadArchiveHeader(const ArchiveFile: string; ArchiveType: TArchiveType; MaxBytes: Integer = 65536): TMemoryStream;
  end;

implementation

const
  DEFAULT_HEADER_SIZE = 65536; // 64 KB

{ TArchivePartitionReader }

class function TArchivePartitionReader.ReadArchiveHeader(const ArchiveFile: string; ArchiveType: TArchiveType; MaxBytes: Integer = 65536): TMemoryStream;
var
  fileStream: TFileStream;
  zipStream: TZipInputStream;
  gzipStream: TGZipInputStream;
  xzStream: TXZInputStream;
  bz2Stream: TBZip2InputStream;
  tarStream: TTarInputStream;
  sevenzStream: TSevenZipInputStream;
  buffer: array[0..8191] of Byte;
  bytesRead: Integer;
  totalRead: Integer;
begin
  Result := TMemoryStream.Create;
  totalRead := 0;
  fileStream := nil;
  
  try
    // Open file as stream (except for 7z which uses external process)
    if ArchiveType <> at7Zip then
      fileStream := TFileStream.Create(ArchiveFile, fmOpenRead or fmShareDenyWrite);
    
    case ArchiveType of
      atZip:
        begin
          zipStream := TZipInputStream.Create(fileStream);
          try
            while (totalRead < MaxBytes) do
            begin
              bytesRead := zipStream.Read(buffer, SizeOf(buffer));
              if bytesRead <= 0 then
                Break;
              Result.Write(buffer, bytesRead);
              Inc(totalRead, bytesRead);
            end;
          finally
            zipStream.Free;
          end;
        end;
        
      atGZip:
        begin
          gzipStream := TGZipInputStream.Create(fileStream);
          try
            while (totalRead < MaxBytes) do
            begin
              bytesRead := gzipStream.Read(buffer, SizeOf(buffer));
              if bytesRead <= 0 then
                Break;
              Result.Write(buffer, bytesRead);
              Inc(totalRead, bytesRead);
            end;
          finally
            gzipStream.Free;
          end;
        end;
        
      atXZ:
        begin
          xzStream := TXZInputStream.Create(fileStream);
          try
            while (totalRead < MaxBytes) do
            begin
              bytesRead := xzStream.Read(buffer, SizeOf(buffer));
              if bytesRead <= 0 then
                Break;
              Result.Write(buffer, bytesRead);
              Inc(totalRead, bytesRead);
            end;
          finally
            xzStream.Free;
          end;
        end;
        
      atBZip2:
        begin
          bz2Stream := TBZip2InputStream.Create(fileStream);
          try
            while (totalRead < MaxBytes) do
            begin
              bytesRead := bz2Stream.Read(buffer, SizeOf(buffer));
              if bytesRead <= 0 then
                Break;
              Result.Write(buffer, bytesRead);
              Inc(totalRead, bytesRead);
            end;
          finally
            bz2Stream.Free;
          end;
        end;
        
      atTarGz:
        begin
          gzipStream := TGZipInputStream.Create(fileStream);
          try
            tarStream := TTarInputStream.Create(gzipStream, False);
            try
              while (totalRead < MaxBytes) do
              begin
                bytesRead := tarStream.Read(buffer, SizeOf(buffer));
                if bytesRead <= 0 then
                  Break;
                Result.Write(buffer, bytesRead);
                Inc(totalRead, bytesRead);
              end;
            finally
              tarStream.Free;
            end;
          finally
            gzipStream.Free;
          end;
        end;
        
      atTarXz:
        begin
          xzStream := TXZInputStream.Create(fileStream);
          try
            tarStream := TTarInputStream.Create(xzStream, False);
            try
              while (totalRead < MaxBytes) do
              begin
                bytesRead := tarStream.Read(buffer, SizeOf(buffer));
                if bytesRead <= 0 then
                  Break;
                Result.Write(buffer, bytesRead);
                Inc(totalRead, bytesRead);
              end;
            finally
              tarStream.Free;
            end;
          finally
            xzStream.Free;
          end;
        end;
        
      at7Zip:
        begin
          sevenzStream := TSevenZipInputStream.Create(ArchiveFile);
          try
            while (totalRead < MaxBytes) do
            begin
              bytesRead := sevenzStream.Read(buffer, SizeOf(buffer));
              if bytesRead <= 0 then
                Break;
              Result.Write(buffer, bytesRead);
              Inc(totalRead, bytesRead);
            end;
          finally
            sevenzStream.Free;
          end;
        end;
        
    else
      // Unknown archive type
      FreeAndNil(Result);
      if Assigned(fileStream) then
        FreeAndNil(fileStream);
      Exit;
    end;
    
    // Reset stream position to beginning
    Result.Position := 0;
    
  except
    on E: Exception do
    begin
      FreeAndNil(Result);
      raise;
    end;
  end;
  
  // Clean up file stream
  if Assigned(fileStream) then
    FreeAndNil(fileStream);
end;

class function TArchivePartitionReader.DetectPartitionTableInArchive(const ArchiveFile: string; ArchiveType: TArchiveType): TPartitionTableType;
var
  headerStream: TMemoryStream;
begin
  Result := pttUnknown;
  
  headerStream := ReadArchiveHeader(ArchiveFile, ArchiveType, 1024); // Only 1KB for detection
  if not Assigned(headerStream) then
    Exit;
    
  try
    Result := DetectPartitionTableTypeFromStream(headerStream);
  finally
    headerStream.Free;
  end;
end;

class function TArchivePartitionReader.ParseMBRPartitionsFromArchive(const ArchiveFile: string; ArchiveType: TArchiveType; var Partitions: array of TPartitionEntry): Integer;
var
  headerStream: TMemoryStream;
begin
  Result := 0;
  
  headerStream := ReadArchiveHeader(ArchiveFile, ArchiveType, 512); // Only first sector
  if not Assigned(headerStream) then
    Exit;
    
  try
    Result := ParseMBRPartitionsFromStream(headerStream, Partitions);
  finally
    headerStream.Free;
  end;
end;

class function TArchivePartitionReader.ParseGPTPartitionsFromArchive(const ArchiveFile: string; ArchiveType: TArchiveType; var Partitions: array of TPartitionEntry): Integer;
var
  headerStream: TMemoryStream;
begin
  Result := 0;
  
  headerStream := ReadArchiveHeader(ArchiveFile, ArchiveType, 65536); // 64KB for GPT
  if not Assigned(headerStream) then
    Exit;
    
  try
    Result := ParseGPTPartitionsFromStream(headerStream, Partitions);
  finally
    headerStream.Free;
  end;
end;

end.
