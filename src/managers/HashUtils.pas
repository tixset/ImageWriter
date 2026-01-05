{******************************************************************************}
{                                                                              }
{  ImageWriter - Hash Utilities Unit                                          }
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
{    Hash calculation utilities for MD5 and SHA-256 algorithms.               }
{    Provides file hashing with progress callback support.                    }
{                                                                              }
{******************************************************************************}

unit HashUtils;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, Classes, SysUtils;

type
  // Hash algorithm type
  THashAlgorithm = (haMD5, haSHA256);
  
  /// <summary>
  /// Hash calculation utilities for data integrity verification
  /// </summary>
  /// <remarks>
  /// Supports MD5 and SHA-256 algorithms for file and device hashing.
  /// Provides hash file I/O (.md5, .sha256) for verification workflows.
  /// </remarks>
  THashUtils = class
  public
    /// <summary>
    /// Calculate hash of a file
    /// </summary>
    /// <param name="FileName">Full path to file to hash</param>
    /// <param name="Algorithm">Hash algorithm to use (MD5 or SHA-256)</param>
    /// <returns>Hexadecimal hash string (32 chars for MD5, 64 for SHA-256)</returns>
    /// <remarks>
    /// Reads file in 8 KB chunks to minimize memory usage.
    /// Returns empty string if file doesn't exist.
    /// </remarks>
    class function CalculateFileHash(const FileName: string; Algorithm: THashAlgorithm): string;
    
    /// <summary>
    /// Calculate hash of a device or stream
    /// </summary>
    /// <param name="DevicePath">Device path (e.g., \\.\PhysicalDrive0)</param>
    /// <param name="Size">Number of bytes to hash</param>
    /// <param name="Algorithm">Hash algorithm to use</param>
    /// <returns>Hexadecimal hash string</returns>
    /// <remarks>
    /// Used for verifying data written to physical devices.
    /// May require administrative privileges for device access.
    /// </remarks>
    class function CalculateDeviceHash(const DevicePath: string; Size: Int64; Algorithm: THashAlgorithm): string;
    
    /// <summary>
    /// Save hash to accompanying file
    /// </summary>
    /// <param name="ImageFile">Original image file path</param>
    /// <param name="Hash">Hash value to save</param>
    /// <param name="Algorithm">Algorithm used (determines .md5 or .sha256 extension)</param>
    /// <remarks>
    /// Creates sidecar file: image.img -> image.img.md5 or image.img.sha256
    /// Uses standard format: hash_value filename
    /// </remarks>
    class procedure SaveHashToFile(const ImageFile, Hash: string; Algorithm: THashAlgorithm);
    
    /// <summary>
    /// Load hash from accompanying file
    /// </summary>
    /// <param name="ImageFile">Original image file path</param>
    /// <param name="Algorithm">Algorithm to look for (determines extension)</param>
    /// <returns>Hash value from file, or empty string if not found</returns>
    /// <remarks>
    /// Looks for: image.img.md5 or image.img.sha256
    /// Parses first word of file as hash value.
    /// </remarks>
    class function LoadHashFromFile(const ImageFile: string; Algorithm: THashAlgorithm): string;
    
    /// <summary>
    /// Get file extension for hash algorithm
    /// </summary>
    /// <param name="Algorithm">Hash algorithm</param>
    /// <returns>Extension string: ".md5" or ".sha256"</returns>
    class function GetHashExtension(Algorithm: THashAlgorithm): string;
    
    /// <summary>
    /// Compare two hash strings (case-insensitive)
    /// </summary>
    /// <param name="Hash1">First hash value</param>
    /// <param name="Hash2">Second hash value</param>
    /// <returns>True if hashes match (ignoring case)</returns>
    class function CompareHashes(const Hash1, Hash2: string): Boolean;
  end;

implementation

uses
  md5, SHA256;

{ THashUtils }

class function THashUtils.CalculateFileHash(const FileName: string; Algorithm: THashAlgorithm): string;
var
  Stream: TFileStream;
  Buffer: array[0..8191] of Byte;
  BytesRead: Integer;
  ContextMD5: MD5Context;
  DigestMD5: MD5Digest;
  ContextSHA256: TSHA256Context;
  DigestSHA256: TSHA256Digest;
  i: Integer;
begin
  Result := '';
  
  if not FileExists(FileName) then
    Exit;
    
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    case Algorithm of
      haMD5:
        begin
          MD5Init(ContextMD5);
          
          repeat
            BytesRead := Stream.Read(Buffer, SizeOf(Buffer));
            if BytesRead > 0 then
              MD5Update(ContextMD5, @Buffer, BytesRead);
          until BytesRead = 0;
          
          MD5Final(ContextMD5, DigestMD5);
          
          // Convert to hex string
          for i := 0 to 15 do
            Result := Result + IntToHex(DigestMD5[i], 2);
        end;
        
      haSHA256:
        begin
          SHA256Init(ContextSHA256);
          
          repeat
            BytesRead := Stream.Read(Buffer, SizeOf(Buffer));
            if BytesRead > 0 then
              SHA256Update(ContextSHA256, @Buffer[0], BytesRead);
          until BytesRead = 0;
          
          SHA256Final(ContextSHA256, DigestSHA256);
          
          // Convert to hex string
          for i := 0 to 31 do
            Result := Result + IntToHex(DigestSHA256[i], 2);
        end;
    end;
    
    Result := LowerCase(Result);
  finally
    Stream.Free;
  end;
end;

class function THashUtils.CalculateDeviceHash(const DevicePath: string; Size: Int64; Algorithm: THashAlgorithm): string;
var
  Handle: THandle;
  Buffer: array[0..8191] of Byte;
  BytesRead: DWORD;
  TotalRead: Int64;
  ContextMD5: MD5Context;
  DigestMD5: MD5Digest;
  ContextSHA256: TSHA256Context;
  DigestSHA256: TSHA256Digest;
  i: Integer;
begin
  Result := '';
  
  Handle := CreateFile(PChar(DevicePath), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if Handle = INVALID_HANDLE_VALUE then
    Exit;
    
  try
    case Algorithm of
      haMD5:
        begin
          MD5Init(ContextMD5);
          
          TotalRead := 0;
          while TotalRead < Size do
          begin
            if not ReadFile(Handle, Buffer, SizeOf(Buffer), BytesRead, nil) then
              Break;
              
            if BytesRead = 0 then
              Break;
              
            MD5Update(ContextMD5, @Buffer, BytesRead);
            Inc(TotalRead, BytesRead);
          end;
          
          MD5Final(ContextMD5, DigestMD5);
          
          // Convert to hex string
          for i := 0 to 15 do
            Result := Result + IntToHex(DigestMD5[i], 2);
        end;
        
      haSHA256:
        begin
          SHA256Init(ContextSHA256);
          
          TotalRead := 0;
          while TotalRead < Size do
          begin
            if not ReadFile(Handle, Buffer, SizeOf(Buffer), BytesRead, nil) then
              Break;
              
            if BytesRead = 0 then
              Break;
              
            SHA256Update(ContextSHA256, @Buffer[0], BytesRead);
            Inc(TotalRead, BytesRead);
          end;
          
          SHA256Final(ContextSHA256, DigestSHA256);
          
          // Convert to hex string
          for i := 0 to 31 do
            Result := Result + IntToHex(DigestSHA256[i], 2);
        end;
    end;
    
    Result := LowerCase(Result);
  finally
    CloseHandle(Handle);
  end;
end;

class procedure THashUtils.SaveHashToFile(const ImageFile, Hash: string; Algorithm: THashAlgorithm);
var
  HashFile: TextFile;
  HashFileName: string;
begin
  HashFileName := ChangeFileExt(ImageFile, GetHashExtension(Algorithm));
  
  try
    AssignFile(HashFile, HashFileName);
    Rewrite(HashFile);
    try
      WriteLn(HashFile, Hash);
    finally
      CloseFile(HashFile);
    end;
  except
    // Ignore save errors
  end;
end;

class function THashUtils.LoadHashFromFile(const ImageFile: string; Algorithm: THashAlgorithm): string;
var
  HashFile: TextFile;
  HashFileName: string;
begin
  Result := '';
  HashFileName := ChangeFileExt(ImageFile, GetHashExtension(Algorithm));
  
  if not FileExists(HashFileName) then
    Exit;
    
  try
    AssignFile(HashFile, HashFileName);
    Reset(HashFile);
    try
      ReadLn(HashFile, Result);
      Result := Trim(Result);
    finally
      CloseFile(HashFile);
    end;
  except
    Result := '';
  end;
end;

class function THashUtils.GetHashExtension(Algorithm: THashAlgorithm): string;
begin
  case Algorithm of
    haMD5: Result := '.md5';
    haSHA256: Result := '.sha256';
  else
    Result := '.hash';
  end;
end;

class function THashUtils.CompareHashes(const Hash1, Hash2: string): Boolean;
begin
  Result := SameText(Trim(Hash1), Trim(Hash2));
end;

end.
