{******************************************************************************}
{                                                                              }
{  ImageWriter - Logging Utilities Unit                                       }
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
{    Centralized logging system with multiple log levels and file output.     }
{    Provides callback mechanism for UI integration.                          }
{                                                                              }
{******************************************************************************}

unit LogUtils;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, Classes, SysUtils;

type
  // Log level enumeration
  TLogLevel = (llDebug, llInfo, llWarning, llError, llCritical);

  // Log callback signature
  TLogCallback = procedure(const Message: string; Level: TLogLevel) of object;

  /// <summary>
  /// Centralized logging system for ImageWriter
  /// </summary>
  /// <remarks>
  /// Thread-safe logging with file output, callbacks, and log rotation.
  /// Supports filtering by log level and automatic file size management.
  /// </remarks>
  TLogUtils = class
  public
    /// <summary>
    /// Initialize the logging system
    /// </summary>
    /// <param name="LogFileName">Full path to log file</param>
    /// <param name="EnableFileLog">Enable writing to log file</param>
    /// <remarks>
    /// Creates log file if it doesn't exist. Safe to call multiple times.
    /// </remarks>
    class procedure Initialize(const LogFileName: string; EnableFileLog: Boolean);
    
    /// <summary>
    /// Set callback function for log messages
    /// </summary>
    /// <param name="Callback">Callback procedure to receive log messages</param>
    /// <remarks>
    /// Use for UI integration to display log messages in real-time.
    /// </remarks>
    class procedure SetLogCallback(Callback: TLogCallback);
    
    /// <summary>
    /// Log a message with specified level
    /// </summary>
    /// <param name="Message">Log message text</param>
    /// <param name="Level">Severity level of the message</param>
    /// <remarks>
    /// Messages below MinimumLogLevel are filtered out.
    /// Triggers log rotation if file size exceeds MaxLogFileSize.
    /// </remarks>
    class procedure Log(const Message: string; Level: TLogLevel);
    
    /// <summary>
    /// Log a formatted message
    /// </summary>
    /// <param name="Fmt">Format string (uses Format function)</param>
    /// <param name="Args">Format arguments</param>
    /// <param name="Level">Severity level</param>
    class procedure LogFmt(const Fmt: string; const Args: array of const; Level: TLogLevel);
    
    /// <summary>
    /// Log debug message (lowest priority)
    /// </summary>
    /// <param name="Message">Debug message</param>
    /// <remarks>
    /// Use for detailed diagnostic information during development.
    /// </remarks>
    class procedure Debug(const Message: string);
    
    /// <summary>
    /// Log informational message
    /// </summary>
    /// <param name="Message">Info message</param>
    /// <remarks>
    /// Use for general operational information.
    /// </remarks>
    class procedure Info(const Message: string);
    
    /// <summary>
    /// Log warning message
    /// </summary>
    /// <param name="Message">Warning message</param>
    /// <remarks>
    /// Use for potentially harmful situations that don't prevent operation.
    /// </remarks>
    class procedure Warning(const Message: string);
    
    /// <summary>
    /// Log error message
    /// </summary>
    /// <param name="Message">Error message</param>
    /// <remarks>
    /// Use for error conditions that may affect functionality.
    /// </remarks>
    class procedure Error(const Message: string);
    
    /// <summary>
    /// Log critical error message (highest priority)
    /// </summary>
    /// <param name="Message">Critical error message</param>
    /// <remarks>
    /// Use for fatal errors that prevent continued operation.
    /// </remarks>
    class procedure Critical(const Message: string);
    
    /// <summary>
    /// Get string representation of log level
    /// </summary>
    /// <param name="Level">Log level to convert</param>
    /// <returns>String name: DEBUG, INFO, WARNING, ERROR, CRITICAL</returns>
    class function GetLogLevelName(Level: TLogLevel): string;
    
    /// <summary>
    /// Format current timestamp for log entries
    /// </summary>
    /// <returns>Timestamp in format: yyyy-mm-dd hh:nn:ss</returns>
    class function FormatTimestamp: string;
    
    /// <summary>
    /// Configure file logging
    /// </summary>
    /// <param name="FileName">Full path to log file</param>
    /// <param name="Enable">Enable or disable file logging</param>
    class procedure SetFileLogging(const FileName: string; Enable: Boolean);
    
    /// <summary>
    /// Set minimum log level filter
    /// </summary>
    /// <param name="Level">Minimum level to log</param>
    /// <remarks>
    /// Messages below this level are discarded. Default is llDebug (all messages).
    /// </remarks>
    class procedure SetMinimumLevel(Level: TLogLevel);
    
    /// <summary>
    /// Set maximum log file size before rotation
    /// </summary>
    /// <param name="SizeInBytes">Maximum file size in bytes</param>
    /// <remarks>
    /// Default is 10 MB. When exceeded, log file is rotated.
    /// </remarks>
    class procedure SetMaxFileSize(SizeInBytes: Int64);
    
    /// <summary>
    /// Configure log file rotation
    /// </summary>
    /// <param name="Enable">Enable automatic rotation</param>
    /// <param name="MaxFiles">Maximum number of backup files to keep (default 5)</param>
    /// <remarks>
    /// Rotated files are named: logfile.1.log, logfile.2.log, etc.
    /// </remarks>
    class procedure SetLogRotation(Enable: Boolean; MaxFiles: Integer = 5);
    
    /// <summary>
    /// Get current log file size
    /// </summary>
    /// <returns>File size in bytes, or 0 if file doesn't exist</returns>
    class function GetLogFileSize: Int64;
    
    /// <summary>
    /// Manually trigger log file rotation
    /// </summary>
    /// <remarks>
    /// Normally called automatically when file size exceeds MaxLogFileSize.
    /// Safe to call manually for forced rotation.
    /// </remarks>
    class procedure RotateLogFile;
  end;

var
  // Module-level variables for logging state
  LogCallback: TLogCallback;
  LogToFile: Boolean;
  LogFileName: string;
  MinimumLogLevel: TLogLevel;
  MaxLogFileSize: Int64;
  LogRotationEnabled: Boolean;
  MaxRotationFiles: Integer;

implementation

{ TLogUtils }

class procedure TLogUtils.Initialize(const LogFileName: string; EnableFileLog: Boolean);
var
  F: TextFile;
begin
  LogCallback := nil;
  LogToFile := EnableFileLog;
  LogUtils.LogFileName := LogFileName;
  MinimumLogLevel := llDebug;  // Default: log everything
  MaxLogFileSize := 10 * 1024 * 1024;  // Default: 10 MB
  LogRotationEnabled := False;
  MaxRotationFiles := 5;
  
  if LogToFile and (LogUtils.LogFileName <> '') then
  begin
    // Create log file if needed
    if not FileExists(LogUtils.LogFileName) then
    begin
      AssignFile(F, LogUtils.LogFileName);
      Rewrite(F);
      WriteLn(F, '=== ImageWriter Log ===');
      WriteLn(F, 'Started: ' + FormatTimestamp);
      CloseFile(F);
    end;
  end;
end;

class procedure TLogUtils.SetLogCallback(Callback: TLogCallback);
begin
  LogCallback := Callback;
end;

class procedure TLogUtils.Log(const Message: string; Level: TLogLevel);
var
  F: TextFile;
  LogLine: string;
begin
  // Filter by minimum level
  if Level < MinimumLogLevel then
    Exit;

  LogLine := Format('[%s] %s: %s', [FormatTimestamp, GetLogLevelName(Level), Message]);
  
  // Call callback if assigned
  if Assigned(LogCallback) then
    LogCallback(Message, Level);
  
  // Write to file if enabled
  if LogToFile and (LogFileName <> '') then
  begin
    try
      // Check if rotation is needed
      if LogRotationEnabled and (GetLogFileSize >= MaxLogFileSize) then
        RotateLogFile;
      
      AssignFile(F, LogFileName);
      if FileExists(LogFileName) then
        Append(F)
      else
        Rewrite(F);
      try
        WriteLn(F, LogLine);
      finally
        CloseFile(F);
      end;
    except
      // Silently ignore logging errors
    end;
  end;
end;

class procedure TLogUtils.LogFmt(const Fmt: string; const Args: array of const; Level: TLogLevel);
begin
  Log(Format(Fmt, Args), Level);
end;

class procedure TLogUtils.Debug(const Message: string);
begin
  Log(Message, llDebug);
end;

class procedure TLogUtils.Info(const Message: string);
begin
  Log(Message, llInfo);
end;

class procedure TLogUtils.Warning(const Message: string);
begin
  Log(Message, llWarning);
end;

class procedure TLogUtils.Error(const Message: string);
begin
  Log(Message, llError);
end;

class procedure TLogUtils.Critical(const Message: string);
begin
  Log(Message, llCritical);
end;

class function TLogUtils.GetLogLevelName(Level: TLogLevel): string;
begin
  case Level of
    llDebug:    Result := 'DEBUG';
    llInfo:     Result := 'INFO';
    llWarning:  Result := 'WARNING';
    llError:    Result := 'ERROR';
    llCritical: Result := 'CRITICAL';
  else
    Result := 'UNKNOWN';
  end;
end;

class function TLogUtils.FormatTimestamp: string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
end;

class procedure TLogUtils.SetFileLogging(const FileName: string; Enable: Boolean);
begin
  LogFileName := FileName;
  LogToFile := Enable;
end;

class procedure TLogUtils.SetMinimumLevel(Level: TLogLevel);
begin
  MinimumLogLevel := Level;
end;

class procedure TLogUtils.SetMaxFileSize(SizeInBytes: Int64);
begin
  if SizeInBytes > 0 then
    MaxLogFileSize := SizeInBytes;
end;

class procedure TLogUtils.SetLogRotation(Enable: Boolean; MaxFiles: Integer);
begin
  LogRotationEnabled := Enable;
  if MaxFiles > 0 then
    MaxRotationFiles := MaxFiles;
end;

class function TLogUtils.GetLogFileSize: Int64;
var
  SearchRec: TSearchRec;
begin
  Result := 0;
  if LogFileName = '' then
    Exit;
    
  if FindFirst(LogFileName, faAnyFile, SearchRec) = 0 then
  begin
    Result := SearchRec.Size;
    FindClose(SearchRec);
  end;
end;

class procedure TLogUtils.RotateLogFile;
var
  i: Integer;
  OldName, NewName: string;
  BaseFileName, Extension: string;
begin
  if LogFileName = '' then
    Exit;
    
  try
    // Extract base name and extension
    BaseFileName := ChangeFileExt(LogFileName, '');
    Extension := ExtractFileExt(LogFileName);
    
    // Delete oldest backup if exists
    if MaxRotationFiles > 0 then
    begin
      OldName := Format('%s.%d%s', [BaseFileName, MaxRotationFiles, Extension]);
      if FileExists(OldName) then
        DeleteFile(OldName);
    end;
    
    // Rotate existing backups
    for i := MaxRotationFiles - 1 downto 1 do
    begin
      OldName := Format('%s.%d%s', [BaseFileName, i, Extension]);
      NewName := Format('%s.%d%s', [BaseFileName, i + 1, Extension]);
      if FileExists(OldName) then
        RenameFile(OldName, NewName);
    end;
    
    // Rotate current log to .1
    if FileExists(LogFileName) then
    begin
      NewName := Format('%s.1%s', [BaseFileName, Extension]);
      RenameFile(LogFileName, NewName);
    end;
  except
    // Silently ignore rotation errors
  end;
end;

end.
