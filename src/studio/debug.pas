{******************************************************************************}
{                                                                              }
{  ImageWriter - Debug Utilities (DEPRECATED - use LogUtils instead)         }
{                                                                              }
{  This module is kept for backwards compatibility only.                      }
{  New code should use LogUtils.pas instead.                                  }
{                                                                              }
{  Original: dd for Windows by John Newbigin                                  }
{  Modified: 2025-12-22 - Added wrapper to redirect to LogUtils              }
{                                                                              }
{******************************************************************************}

unit debug;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

// Legacy code - redirects to LogUtils for unified logging
{$IFDEF DEBUG}
{$MESSAGE WARN 'debug.pas is deprecated. Please use LogUtils.pas instead.'}
{$ENDIF}

interface

uses
  Windows, SysUtils, LogUtils;  // LogUtils for unified logging

// Legacy callback type - kept for backwards compatibility
type
  DebugEvent = procedure(S: String) of object;

// Legacy procedures - all redirect to LogUtils
procedure Log(S: String);
procedure SetDebug(d: DebugEvent);
procedure UseWriteln;
procedure UseStdError;
procedure HexDump(P: PChar; Length: Integer);

// Legacy helper classes - kept for compatibility
type
  TWriteLine = class
  public
    procedure WriteLine(S: String);
  end;

type
  TStdError = class
  private
    h: THandle;
  public
    constructor Create;
    procedure Write(S: String);
    procedure WriteLine(S: String);
  end;

function IsDebuggerPresent: Boolean;

var
  stderr: TStdError;

implementation

uses WinIOCTL;

var
  // Legacy callback variable - redirected to LogUtils
  fOnDebug: DebugEvent;

type
  TIsDebuggerPresent = function: BOOL; stdcall;

{ Wrapper Adapter for LogUtils Integration }

// Adapter class to bridge LogUtils callback (method) to debug callback (procedure)
type
  TLogUtilsAdapter = class
  public
    procedure OnLogMessage(const Message: string; Level: TLogLevel);
  end;

var
  LogAdapter: TLogUtilsAdapter;

procedure TLogUtilsAdapter.OnLogMessage(const Message: string; Level: TLogLevel);
begin
  if Assigned(fOnDebug) then
    fOnDebug(Message);
end;

{ Legacy Log Procedure - now redirects to LogUtils }

procedure Log(S: String);
begin
  // Redirect to LogUtils.Info for unified logging
  TLogUtils.Info(S);
  
  // Also call legacy callback if assigned (backwards compatibility)
  if Assigned(fOnDebug) then
    fOnDebug(S);
end;

{ SetDebug - Configures both legacy and LogUtils callbacks }

procedure SetDebug(d: DebugEvent);
begin
  fOnDebug := d;
  
  // Create adapter if needed
  if not Assigned(LogAdapter) then
    LogAdapter := TLogUtilsAdapter.Create;
  
  // If callback is assigned, register adapter with LogUtils
  if Assigned(d) then
    TLogUtils.SetLogCallback(LogAdapter.OnLogMessage)
  else
    TLogUtils.SetLogCallback(nil);
end;

{ TWriteLine - Legacy console output adapter }

procedure TWriteLine.WriteLine(S: String);
begin
{$IFDEF FPC}
  Writeln(S + #13);
{$ELSE}
  Writeln(Output, S);
{$ENDIF}
end;

{ TStdError - Legacy stderr output adapter }

constructor TStdError.Create;
begin
  h := GetStdHandle(STD_ERROR_HANDLE);
end;

procedure TStdError.Write(S: String);
var
  Done: DWORD;
begin
  WriteFile2(h, PChar(S), Length(S), Done, nil);
end;

procedure TStdError.WriteLine(S: String);
var
  Done: DWORD;
begin
  S := S + #13 + #10;
  WriteFile2(h, PChar(S), Length(S), Done, nil);
end;

{ UseWriteln - Configure console output }

procedure UseWriteln;
var
  wl: TWriteLine;
begin
  wl := TWriteLine.Create;
  SetDebug(wl.WriteLine);
end;

{ UseStdError - Configure stderr output }

procedure UseStdError;
begin
  stderr := TStdError.Create;
  SetDebug(stderr.WriteLine);
end;

{ IsDebuggerPresent - Check if debugger is attached }

function IsDebuggerPresent: Boolean;
var
  hModule: hInst;
  JIsDebuggerPresent: TIsDebuggerPresent;
  P: Pointer;
begin
  Result := False;

  // Get IsDebuggerPresent from kernel32.dll
  hModule := GetModuleHandle('kernel32.dll');
  if hModule = 0 then
  begin
    hModule := LoadLibrary('kernel32.dll');
  end;

  if hModule <> 0 then
  begin
    P := GetProcAddress(hModule, 'IsDebuggerPresent');
    if P <> nil then
    begin
      JIsDebuggerPresent := P;
      Result := JIsDebuggerPresent;
    end;
  end;
end;

{ HexDump - Dump memory in hex format (uses Log internally) }

procedure HexDump(P: PChar; Length: Integer);
var
  S: String;
  S2: String;
  i: Integer;
begin
  S := '0000 ';
  S2 := '';
  for i := 0 to Length - 1 do
  begin
    S := S + IntToHex(Ord(P[i]), 2);
    if not (Ord(P[i]) in [0, 7, 8, 9, 10, 13]) then
      S2 := S2 + P[i]
    else
      S2 := S2 + ' ';
    S := S + ' ';
    if System.Length(S) >= 52 then
    begin
      Log(S + ' ' + S2);  // Now redirects to LogUtils.Info
      S := IntToHex(i + 1, 4) + ' ';
      S2 := '';
    end;
  end;
  // Log remaining data if any
  if S2 <> '' then
    Log(S + ' ' + S2);
end;

initialization
  LogAdapter := nil;

finalization
  if Assigned(LogAdapter) then
    LogAdapter.Free;

end.
