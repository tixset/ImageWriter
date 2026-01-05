{******************************************************************************}
{                                                                              }
{  ImageWriter - UI Helper Unit                                               }
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
{    UI helper functions for block size calculation and formatting.           }
{    Provides utilities for human-readable size display.                      }
{                                                                              }
{******************************************************************************}

unit UIHelper;

interface

uses
  Windows, SysUtils, Classes, StdCtrls, ComCtrls, Forms;

type
  TOperationMode = (omWrite, omRead);
  TLogProc = procedure(const Msg: string; Level: Integer) of object;
  
  TUIHelper = class
  public
    constructor Create;
    
    // Calculate optimal block size based on size
    function CalculateBlockSize(Size: Int64): Int64;
    
    // Calculate block count
    function CalculateBlockCount(TotalSize, BlockSize: Int64): Int64;
    
    // Update BS and Count edit boxes based on operation mode and sizes
    procedure RecalculateBlockSizeAndCount(
      Mode: TOperationMode;
      EditBS, EditCount: TEdit;
      const InputFile: string;
      DiskSize: Int64;
      OnLogProc: TLogProc);
    
    // Format bytes to human-readable string
    class function BytesToHuman(Bytes: Int64): string;
    
    // Parse size string (e.g., "1G", "512M") to Int64
    class function ParseSize(const SizeStr: string): Int64;
  end;

implementation

constructor TUIHelper.Create;
begin
  inherited Create;
end;

function TUIHelper.CalculateBlockSize(Size: Int64): Int64;
begin
  // Auto-calculate block size based on size
  if Size >= 1024 * 1024 * 1024 then
    Result := 8 * 1024 * 1024      // >= 1 GB: 8 MB blocks
  else if Size >= 100 * 1024 * 1024 then
    Result := 1024 * 1024          // >= 100 MB: 1 MB blocks
  else
    Result := 512 * 1024;          // < 100 MB: 512 KB blocks
end;

function TUIHelper.CalculateBlockCount(TotalSize, BlockSize: Int64): Int64;
begin
  if BlockSize <= 0 then
  begin
    Result := 0;
    Exit;
  end;
  
  Result := TotalSize div BlockSize;
  if (TotalSize mod BlockSize) <> 0 then
    Inc(Result);
end;

procedure TUIHelper.RecalculateBlockSizeAndCount(
  Mode: TOperationMode;
  EditBS, EditCount: TEdit;
  const InputFile: string;
  DiskSize: Int64;
  OnLogProc: TLogProc);
var
  blockSize, count, fileSize: Int64;
  calculated: Boolean;
  
  function GetFileSize64(const FileName: string): Int64;
  var
    sr: TSearchRec;
  begin
    Result := 0;
    {$WARN SYMBOL_PLATFORM OFF}
    if FindFirst(FileName, faAnyFile, sr) = 0 then
    begin
      Result := Int64(sr.FindData.nFileSizeHigh) shl 32 + sr.FindData.nFileSizeLow;
      FindClose(sr);
    end;
    {$WARN SYMBOL_PLATFORM ON}
  end;
  
begin
  calculated := False;
  
  if Mode = omWrite then
  begin
    // Write mode: calculate based on file size
    if (InputFile <> '') and FileExists(InputFile) then
    begin
      fileSize := GetFileSize64(InputFile);
      if fileSize > 0 then
      begin
        blockSize := CalculateBlockSize(fileSize);
        count := CalculateBlockCount(fileSize, blockSize);
        
        if Assigned(EditBS) then
          EditBS.Text := IntToStr(blockSize);
        if Assigned(EditCount) then
          EditCount.Text := IntToStr(count);
        calculated := True;

        if Assigned(OnLogProc) then
          OnLogProc('Auto-calc (Write): BS=' + IntToStr(blockSize) + ', Count=' + IntToStr(count) + 
                   ' (based on file: ' + BytesToHuman(fileSize) + ')', 0); // 0 = llDebug
      end;
    end
    else if DiskSize > 0 then
    begin
      // No file selected, use disk size for BS
      blockSize := CalculateBlockSize(DiskSize);
      count := CalculateBlockCount(DiskSize, blockSize);
      
      if Assigned(EditBS) then
        EditBS.Text := IntToStr(blockSize);
      if Assigned(EditCount) then
        EditCount.Text := IntToStr(count);
      calculated := True;
      
      if Assigned(OnLogProc) then
        OnLogProc('Auto-calc: BS=' + IntToStr(blockSize) + ', Count=' + IntToStr(count) + 
                 ' (based on disk size)', 0); // 0 = llDebug
    end;
  end
  else
  begin
    // Read mode: calculate based on disk size
    if DiskSize > 0 then
    begin
      blockSize := CalculateBlockSize(DiskSize);
      count := CalculateBlockCount(DiskSize, blockSize);
      
      if Assigned(EditBS) then
        EditBS.Text := IntToStr(blockSize);
      if Assigned(EditCount) then
        EditCount.Text := IntToStr(count);
      calculated := True;

      if Assigned(OnLogProc) then
        OnLogProc('Auto-calc (Read): BS=' + IntToStr(blockSize) + ', Count=' + IntToStr(count) + 
                 ' (based on disk: ' + BytesToHuman(DiskSize) + ')', 0); // 0 = llDebug
    end;
  end;
  
  // If nothing calculated, set to 0
  if not calculated then
  begin
    if Assigned(EditBS) and (Trim(EditBS.Text) = '') then
      EditBS.Text := '0';
    if Assigned(EditCount) and (Trim(EditCount.Text) = '') then
      EditCount.Text := '0';
  end;
end;

class function TUIHelper.BytesToHuman(Bytes: Int64): string;
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

class function TUIHelper.ParseSize(const SizeStr: string): Int64;
var
  numStr, suffix: string;
  num: Extended;
  i: Integer;
begin
  Result := 0;
  if SizeStr = '' then
    Exit;
    
  // Find where digits end
  i := 1;
  while (i <= Length(SizeStr)) and (SizeStr[i] in ['0'..'9', '.', ',']) do
    Inc(i);
  
  numStr := Trim(Copy(SizeStr, 1, i - 1));
  suffix := UpperCase(Trim(Copy(SizeStr, i, Length(SizeStr))));
  
  if not TryStrToFloat(numStr, num) then
    Exit;
    
  // Parse suffix
  if (suffix = 'T') or (suffix = 'TB') then
    Result := Round(num * 1024 * 1024 * 1024 * 1024)
  else if (suffix = 'G') or (suffix = 'GB') then
    Result := Round(num * 1024 * 1024 * 1024)
  else if (suffix = 'M') or (suffix = 'MB') then
    Result := Round(num * 1024 * 1024)
  else if (suffix = 'K') or (suffix = 'KB') then
    Result := Round(num * 1024)
  else
    Result := Round(num);
end;

end.
