{******************************************************************************}
{                                                                              }
{  ImageWriter - Main Program File                                            }
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
{    Main entry point for ImageWriter application.                            }
{    GUI-based disk image writing utility with worker mode support.           }
{                                                                              }
{******************************************************************************}

program ImageWriter;
{$APPTYPE GUI}

// Include application resources (icon, manifest, zlib1.dll, version info)
{$R resources\resources.res}

uses
  Windows,
  SysUtils,
  Forms,
  Dialogs,
  MainForm in 'src\mainform.pas' {MainForm},
  ExtractZLib in 'src\ExtractZLib.pas',
  ElevationHelper in 'src\ElevationHelper.pas',
  IPCPipe in 'src\IPCPipe.pas',
  WorkerMode in 'src\WorkerMode.pas',
  ConsoleMode in 'src\ConsoleMode.pas',
  WorkerCommands in 'src\WorkerCommands.pas',
  BinFile in 'src\BinFile.pas',
  DeviceManager in 'src\managers\DeviceManager.pas',
  HashUtils in 'src\managers\HashUtils.pas',
  FileOperations in 'src\managers\FileOperations.pas',
  LogUtils in 'src\managers\LogUtils.pas',
  ProgressManager in 'src\managers\ProgressManager.pas',
  PartitionAnalyzer in 'src\managers\PartitionAnalyzer.pas',
  UIHelper in 'src\managers\UIHelper.pas',
  DeviceInfoHelper in 'src\managers\DeviceInfoHelper.pas',
  ValidationHelper in 'src\managers\ValidationHelper.pas',
  SettingsManager in 'src\managers\SettingsManager.pas',
  OperationUIHelper in 'src\managers\OperationUIHelper.pas',
  ArchiveHandler in 'src\managers\ArchiveHandler.pas',
  ArchivePartitionReader in 'src\managers\ArchivePartitionReader.pas',
  DeviceBenchmark in 'src\managers\DeviceBenchmark.pas',
  BenchmarkForm in 'src\BenchmarkForm.pas' {BenchmarkForm};

type
  TExceptionLogger = class
    procedure LogException(Sender: TObject; E: Exception);
  end;

var
  GMutex: THandle;
  ExceptionLogger: TExceptionLogger;

procedure TExceptionLogger.LogException(Sender: TObject; E: Exception);
var
  LogMsg: string;
  LogFile: TextFile;
  LogFileName: string;
begin
  // Suppress default error dialog - log exception to file instead
  LogMsg := '[' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + '] EXCEPTION: ' + 
            E.ClassName + ': ' + E.Message;
  
  try
    LogFileName := ExtractFilePath(ParamStr(0)) + 'log\ImageWriter_errors.log';
    if not DirectoryExists(ExtractFilePath(LogFileName)) then
      CreateDir(ExtractFilePath(LogFileName));
      
    AssignFile(LogFile, LogFileName);
    if FileExists(LogFileName) then
      Append(LogFile)
    else
      Rewrite(LogFile);
    WriteLn(LogFile, LogMsg);
    CloseFile(LogFile);
  except
    // Silent fail
  end;
  
  // Also log via form if available
  if Assigned(AppMainForm) then
  begin
    try
      with TMainForm(AppMainForm) do
        MemoLog.Lines.Add('[ERROR] ' + E.ClassName + ': ' + E.Message);
    except
      // Ignore
    end;
  end;
end;

begin
  // Check if running in worker mode
  if (ParamCount > 0) and (ParamStr(1) = '--worker') then
  begin
    // Worker mode: execute disk operations with elevated privileges
    ExitCode := ExecuteWorkerMode;
    Exit;
  end;

  // Check if running in console mode (CLI)
  if (ParamCount > 0) and (ParamStr(1) = '--cli') then
  begin
    // Console mode: command-line interface
    ExitCode := ExecuteConsoleMode;
    Exit;
  end;

  // Normal GUI mode - check for single instance
  GMutex := CreateMutex(nil, True, 'Global\ImageWriter_GUI_Mutex');
  if GetLastError = ERROR_ALREADY_EXISTS then
  begin
    MessageBox(0, 'ImageWriter is already running.', 'ImageWriter', MB_OK or MB_ICONINFORMATION);
    if GMutex <> 0 then
      CloseHandle(GMutex);
    Exit;
  end;

  try
    Application.Initialize;
    // Install global exception handler to log errors instead of showing MessageBox
    ExceptionLogger := TExceptionLogger.Create;
    Application.OnException := ExceptionLogger.LogException;
    Application.CreateForm(TMainForm, AppMainForm);
    Application.Run;
  finally
    if GMutex <> 0 then
    begin
      ReleaseMutex(GMutex);
      CloseHandle(GMutex);
    end;
  end;
end.
