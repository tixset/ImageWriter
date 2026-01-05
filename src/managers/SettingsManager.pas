{******************************************************************************}
{                                                                              }
{  ImageWriter - Settings Manager Unit                                        }
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
{    Application settings and file history management.                        }
{    Handles loading/saving settings to AppData folder.                       }
{                                                                              }
{******************************************************************************}

unit SettingsManager;

interface

uses
  Windows, SysUtils, Classes, StdCtrls, ComCtrls;

type
  TSettingsManager = class
  private
    FConfigPath: string;
    FHistoryPath: string;
    FMaxHistoryItems: Integer;
    
    function GetAppDataPath: string;
    function GetDefaultConfigPath: string;
    function GetDefaultHistoryPath: string;
  public
    constructor Create;
    
    // Settings management
    procedure LoadSettings(
      EditBS, EditCount, EditSeek, EditSkip: TEdit;
      CheckAdvanced, CheckShowFixed, CheckVerifyHash, CheckAutoEject: TCheckBox;
      ComboHashAlgo: TComboBox);
    
    procedure SaveSettings(
      EditBS, EditCount, EditSeek, EditSkip: TEdit;
      CheckAdvanced, CheckShowFixed, CheckVerifyHash, CheckAutoEject: TCheckBox;
      ComboHashAlgo: TComboBox);
    
    // File history management
    procedure LoadFileHistory(ComboBox: TComboBox);
    procedure SaveFileHistory(ComboBox: TComboBox);
    procedure AddToFileHistory(ComboBox: TComboBox; const FilePath: string);
    
    property ConfigPath: string read FConfigPath write FConfigPath;
    property HistoryPath: string read FHistoryPath write FHistoryPath;
    property MaxHistoryItems: Integer read FMaxHistoryItems write FMaxHistoryItems;
  end;

implementation

uses
  StrUtils;

// Shell API function for getting special folder paths
function SHGetFolderPath(hwndOwner: HWND; nFolder: Integer; hToken: THandle;
  dwFlags: DWORD; pszPath: PChar): HRESULT; stdcall; external 'shell32.dll' name 'SHGetFolderPathA';

constructor TSettingsManager.Create;
begin
  inherited Create;
  FConfigPath := GetDefaultConfigPath;
  FHistoryPath := GetDefaultHistoryPath;
  FMaxHistoryItems := 10;
end;

function TSettingsManager.GetAppDataPath: string;
const
  CSIDL_LOCAL_APPDATA = $001C;  // Local AppData folder constant
var
  Path: array[0..MAX_PATH] of Char;
begin
  // Try to get LocalAppData folder (%LOCALAPPDATA%)
  if SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0, 0, @Path) = S_OK then
  begin
    Result := IncludeTrailingPathDelimiter(Path) + 'ImageWriter\';
    // Create directory if it doesn't exist
    if not DirectoryExists(Result) then
      ForceDirectories(Result);
  end
  else
    // Fallback to application directory if AppData not available
    Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

function TSettingsManager.GetDefaultConfigPath: string;
begin
  Result := GetAppDataPath + 'ImageWriter.ini';
end;

function TSettingsManager.GetDefaultHistoryPath: string;
begin
  Result := GetAppDataPath + 'history.txt';
end;

procedure TSettingsManager.LoadSettings(
  EditBS, EditCount, EditSeek, EditSkip: TEdit;
  CheckAdvanced, CheckShowFixed, CheckVerifyHash, CheckAutoEject: TCheckBox;
  ComboHashAlgo: TComboBox);
var
  ConfigFile: TextFile;
  Line, Key, Value: string;
  P: Integer;
begin
  if not FileExists(FConfigPath) then
    Exit;

  try
    AssignFile(ConfigFile, FConfigPath);
    Reset(ConfigFile);
    try
      while not Eof(ConfigFile) do
      begin
        ReadLn(ConfigFile, Line);
        Line := Trim(Line);
        if (Line = '') or (Line[1] = ';') or (Line[1] = '#') then
          Continue;

        P := Pos('=', Line);
        if P > 0 then
        begin
          Key := Trim(Copy(Line, 1, P - 1));
          Value := Trim(Copy(Line, P + 1, Length(Line)));

          if Key = 'BlockSize' then
            EditBS.Text := Value
          else if Key = 'Count' then
            EditCount.Text := Value
          else if Key = 'Seek' then
            EditSeek.Text := Value
          else if Key = 'Skip' then
            EditSkip.Text := Value
          else if Key = 'AdvancedMode' then
            CheckAdvanced.Checked := (Value = '1') or (LowerCase(Value) = 'true')
          else if Key = 'ShowFixed' then
            CheckShowFixed.Checked := (Value = '1') or (LowerCase(Value) = 'true')
          else if Key = 'VerifyHash' then
            CheckVerifyHash.Checked := (Value = '1') or (LowerCase(Value) = 'true')
          else if Key = 'AutoEject' then
            CheckAutoEject.Checked := (Value = '1') or (LowerCase(Value) = 'true')
          else if Key = 'HashAlgorithm' then
          begin
            if Assigned(ComboHashAlgo) and (Value = 'MD5') then
              ComboHashAlgo.ItemIndex := 0;
          end;
        end;
      end;
    finally
      CloseFile(ConfigFile);
    end;
  except
    // Ignore errors loading settings
  end;
end;

procedure TSettingsManager.SaveSettings(
  EditBS, EditCount, EditSeek, EditSkip: TEdit;
  CheckAdvanced, CheckShowFixed, CheckVerifyHash, CheckAutoEject: TCheckBox;
  ComboHashAlgo: TComboBox);
var
  ConfigFile: TextFile;
begin
  try
    AssignFile(ConfigFile, FConfigPath);
    Rewrite(ConfigFile);
    try
      WriteLn(ConfigFile, '; ImageWriter Configuration');
      WriteLn(ConfigFile, 'BlockSize=', EditBS.Text);
      WriteLn(ConfigFile, 'Count=', EditCount.Text);
      WriteLn(ConfigFile, 'Seek=', EditSeek.Text);
      WriteLn(ConfigFile, 'Skip=', EditSkip.Text);
      WriteLn(ConfigFile, 'AdvancedMode=', IfThen(CheckAdvanced.Checked, '1', '0'));
      WriteLn(ConfigFile, 'ShowFixed=', IfThen(CheckShowFixed.Checked, '1', '0'));
      WriteLn(ConfigFile, 'VerifyHash=', IfThen(CheckVerifyHash.Checked, '1', '0'));
      WriteLn(ConfigFile, 'AutoEject=', IfThen(CheckAutoEject.Checked, '1', '0'));
      if Assigned(ComboHashAlgo) then
        WriteLn(ConfigFile, 'HashAlgorithm=', ComboHashAlgo.Text)
      else
        WriteLn(ConfigFile, 'HashAlgorithm=MD5');
    finally
      CloseFile(ConfigFile);
    end;
  except
    // Ignore errors saving settings
  end;
end;

procedure TSettingsManager.LoadFileHistory(ComboBox: TComboBox);
var
  HistoryFile: TextFile;
  Line: string;
begin
  if not Assigned(ComboBox) then
    Exit;

  ComboBox.Items.Clear;

  if not FileExists(FHistoryPath) then
    Exit;

  try
    AssignFile(HistoryFile, FHistoryPath);
    Reset(HistoryFile);
    try
      while not Eof(HistoryFile) and (ComboBox.Items.Count < FMaxHistoryItems) do
      begin
        ReadLn(HistoryFile, Line);
        Line := Trim(Line);
        if (Line <> '') and FileExists(Line) then
          ComboBox.Items.Add(Line);
      end;
    finally
      CloseFile(HistoryFile);
    end;
  except
    // Ignore errors loading history
  end;
end;

procedure TSettingsManager.SaveFileHistory(ComboBox: TComboBox);
var
  HistoryFile: TextFile;
  i: Integer;
begin
  if not Assigned(ComboBox) then
    Exit;

  try
    AssignFile(HistoryFile, FHistoryPath);
    Rewrite(HistoryFile);
    try
      for i := 0 to ComboBox.Items.Count - 1 do
      begin
        if i >= FMaxHistoryItems then
          Break;
        WriteLn(HistoryFile, ComboBox.Items[i]);
      end;
    finally
      CloseFile(HistoryFile);
    end;
  except
    // Ignore errors saving history
  end;
end;

procedure TSettingsManager.AddToFileHistory(ComboBox: TComboBox; const FilePath: string);
var
  NormalizedPath: string;
  idx: Integer;
begin
  if not Assigned(ComboBox) then
    Exit;

  if Trim(FilePath) = '' then
    Exit;

  // Normalize path
  NormalizedPath := ExpandFileName(FilePath);

  // Check if already in list
  idx := ComboBox.Items.IndexOf(NormalizedPath);
  if idx >= 0 then
    ComboBox.Items.Delete(idx);

  // Add to top of list
  ComboBox.Items.Insert(0, NormalizedPath);

  // Limit to max items
  while ComboBox.Items.Count > FMaxHistoryItems do
    ComboBox.Items.Delete(ComboBox.Items.Count - 1);

  // Save updated history
  SaveFileHistory(ComboBox);
end;

end.
