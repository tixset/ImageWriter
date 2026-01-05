{******************************************************************************}
{                                                                              }
{  OperationUIHelper - Operation UI State Management Module                   }
{                                                                              }
{  Copyright (c) 2024-2025 Anton Zelenov <tixset@gmail.com>                   }
{  GitHub: https://github.com/tixset/ImageWriter                              }
{                                                                              }
{  This module provides utilities for managing UI state during operations.    }
{  Handles initialization, progress updates, and status display.              }
{                                                                              }
{******************************************************************************}

unit OperationUIHelper;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, SysUtils, ComCtrls, Gauges, Classes;

type
  ULONGLONG = Int64;
  
  ITaskbarList3 = interface(IUnknown)
    ['{EA1AFB91-9E28-4B86-90E9-9E9F8A5EEFAF}']
    function HrInit: HRESULT; stdcall;
    function AddTab(hwnd: HWND): HRESULT; stdcall;
    function DeleteTab(hwnd: HWND): HRESULT; stdcall;
    function ActivateTab(hwnd: HWND): HRESULT; stdcall;
    function SetActiveAlt(hwnd: HWND): HRESULT; stdcall;
    function MarkFullscreenWindow(hwnd: HWND; fFullscreen: BOOL): HRESULT; stdcall;
    function SetProgressValue(hwnd: HWND; ullCompleted: ULONGLONG; ullTotal: ULONGLONG): HRESULT; stdcall;
    function SetProgressState(hwnd: HWND; tbpFlags: Integer): HRESULT; stdcall;
  end;
  
  TOperationUIHelper = class
  public
    class procedure InitializeOperationUI(
      StatusBar: TStatusBar;
      ProgressBar: TGauge;
      const StatusText: string;
      TotalBytes: Int64;
      TaskbarList: IInterface);
      
    class procedure ResetOperationUI(
      StatusBar: TStatusBar;
      ProgressBar: TGauge);
      
    class procedure UpdateStatusBar(
      StatusBar: TStatusBar;
      const StatusText: string); overload;
      
    class procedure UpdateStatusBar(
      StatusBar: TStatusBar;
      PanelIndex: Integer;
      const PanelText: string); overload;
      
    class function FormatElapsedTime(StartTime: TDateTime): string;
    class function FormatSpeed(BytesProcessed: Int64; ElapsedSeconds: Double): string;
  end;

implementation

uses
  DateUtils;

class procedure TOperationUIHelper.InitializeOperationUI(
  StatusBar: TStatusBar;
  ProgressBar: TGauge;
  const StatusText: string;
  TotalBytes: Int64;
  TaskbarList: IInterface);
const
  TBPF_NORMAL = 2;
var
  TBList: ITaskbarList3;
begin
  // Initialize status bar panels
  if Assigned(StatusBar) then
  begin
    if StatusBar.Panels.Count > 0 then
      StatusBar.Panels[0].Text := '0%';
    if StatusBar.Panels.Count > 1 then
      StatusBar.Panels[1].Text := '';
    if StatusBar.Panels.Count > 2 then
      StatusBar.Panels[2].Text := '';
    if StatusBar.Panels.Count > 3 then
      StatusBar.Panels[3].Text := StatusText;
  end;
  
  // Reset progress bar
  if Assigned(ProgressBar) then
    ProgressBar.Progress := 0;
  
  // Set taskbar progress (Windows 7+)
  if Assigned(TaskbarList) then
  begin
    try
      TBList := TaskbarList as ITaskbarList3;
      TBList.SetProgressState(GetForegroundWindow, TBPF_NORMAL);
    except
      // Silently ignore if interface is not supported
    end;
  end;
end;

class procedure TOperationUIHelper.ResetOperationUI(
  StatusBar: TStatusBar;
  ProgressBar: TGauge);
begin
  if Assigned(ProgressBar) then
    ProgressBar.Progress := 0;
    
  if Assigned(StatusBar) then
  begin
    if StatusBar.Panels.Count > 0 then
      StatusBar.Panels[0].Text := '';
    if StatusBar.Panels.Count > 1 then
      StatusBar.Panels[1].Text := '';
    if StatusBar.Panels.Count > 2 then
      StatusBar.Panels[2].Text := '';
    if StatusBar.Panels.Count > 3 then
      StatusBar.Panels[3].Text := 'Idle';
  end;
end;

class procedure TOperationUIHelper.UpdateStatusBar(
  StatusBar: TStatusBar;
  const StatusText: string);
begin
  if Assigned(StatusBar) and (StatusBar.Panels.Count > 3) then
    StatusBar.Panels[3].Text := StatusText;
end;

class procedure TOperationUIHelper.UpdateStatusBar(
  StatusBar: TStatusBar;
  PanelIndex: Integer;
  const PanelText: string);
begin
  if Assigned(StatusBar) and (PanelIndex >= 0) and (PanelIndex < StatusBar.Panels.Count) then
    StatusBar.Panels[PanelIndex].Text := PanelText;
end;

class function TOperationUIHelper.FormatElapsedTime(StartTime: TDateTime): string;
var
  Elapsed: TDateTime;
  Hours, Minutes, Seconds: Word;
begin
  Elapsed := Now - StartTime;
  Hours := Trunc(Elapsed * 24);
  Minutes := Trunc(Frac(Elapsed * 24) * 60);
  Seconds := Trunc(Frac(Frac(Elapsed * 24) * 60) * 60);
  
  if Hours > 0 then
    Result := Format('%d:%2.2d:%2.2d', [Hours, Minutes, Seconds])
  else
    Result := Format('%d:%2.2d', [Minutes, Seconds]);
end;

class function TOperationUIHelper.FormatSpeed(BytesProcessed: Int64; ElapsedSeconds: Double): string;
var
  Speed: Double;
begin
  if ElapsedSeconds <= 0 then
  begin
    Result := '0 B/s';
    Exit;
  end;
  
  Speed := BytesProcessed / ElapsedSeconds;
  
  if Speed >= 1073741824 then // >= 1 GB/s
    Result := Format('%.2f GB/s', [Speed / 1073741824])
  else if Speed >= 1048576 then // >= 1 MB/s
    Result := Format('%.2f MB/s', [Speed / 1048576])
  else if Speed >= 1024 then // >= 1 KB/s
    Result := Format('%.2f KB/s', [Speed / 1024])
  else
    Result := Format('%.0f B/s', [Speed]);
end;

end.
