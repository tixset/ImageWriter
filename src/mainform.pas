{******************************************************************************}
{                                                                              }
{  ImageWriter - Main Form Unit                                               }
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
{    Main application window with GUI for disk image writing operations.      }
{    Handles device enumeration, image selection, and write/read operations.  }
{                                                                              }
{******************************************************************************}

unit MainForm;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, StrUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls, ShellAPI, ComObj, ActiveX, Menus,
  Gauges, Buttons, studio_tools, debug, ExtractZLib, GZipStream, XZStream, BZip2Stream, 
  TarStream, SevenZipStream, md5, sha256,
  ElevationHelper, IPCPipe, WorkerMode, NetShareAuth, Native, WinIOCTL, BinFile,
  volume, ImgList, PartitionInfo,
  // Refactored modules
  DeviceManager, HashUtils, FileOperations, LogUtils, ProgressManager, PartitionAnalyzer, 
  UIHelper, DeviceInfoHelper, ValidationHelper, SettingsManager, OperationUIHelper, 
  ArchiveHandler, ArchivePartitionReader, DeviceBenchmark, BenchmarkForm;

const
  CLSID_TaskbarList: TGUID = '{56FDF344-FD6D-11d0-958A-006097C9A090}';
  TBPF_NOPROGRESS = 0;
  TBPF_INDETERMINATE = 1;
  TBPF_NORMAL = 2;
  TBPF_ERROR = 4;
  TBPF_PAUSED = 8;
  ABOVE_NORMAL_PRIORITY_CLASS = $00008000;
  HANDLE_MARKER_WORKER_LOCKED = $80000000; // High bit set = worker locked volume
  WM_REFRESH_DEVICES = WM_USER + 100; // Custom message for delayed device refresh

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

  TLogLevel = (llDebug, llInfo, llWarning, llError);
  
  TDeviceHealthStatus = (dhsUnknown, dhsHealthy, dhsWarning, dhsCritical);
  
  TDeviceHealthInfo = record
    Status: TDeviceHealthStatus;
    WarningCount: Integer;
    CriticalCount: Integer;
    LastCheckTime: TDateTime;
    SerialNumber: string;
    Model: string;
    ErrorMessage: string;
  end;
  
  TSMARTHistory = record
    CheckTime: TDateTime;
    Temperature: Integer;
    ReallocatedSectors: Integer;
    PendingSectors: Integer;
    PowerOnHours: Int64;
  end;

  // Callback for RTF stream
  TEditStreamCallback = function(dwCookie: Longint; pbBuff: PByte; cb: Longint; var pcb: Longint): DWORD; stdcall;

  TEditStream = record
    dwCookie: Longint;
    dwError: Longint;
    pfnCallback: TEditStreamCallback;
  end;

type
  TMainForm = class(TForm)
    PanelTop: TPanel;
    EditIn: TComboBox;
    LabelBS: TLabel;
    EditBS: TEdit;
    LabelCount: TLabel;
    EditCount: TEdit;
    LabelSeek: TLabel;
    EditSeek: TEdit;
    LabelSkip: TLabel;
    EditSkip: TEdit;
    CheckVerifyHash: TCheckBox;
    CheckAutoEject: TCheckBox;
    ButtonStart: TBitBtn;
    ButtonBenchmark: TBitBtn;
    ProgressBar: TGauge;
    MemoLog: TRichEdit;
    ButtonBrowse: TBitBtn;
    ComboDevice: TComboBox;
    RadioGroupMode: TRadioGroup;
    CheckAdvanced: TCheckBox;
    CheckShowFixed: TCheckBox;
    StatusBar: TStatusBar;
    ButtonRefresh: TBitBtn;
    ComboHashAlgo: TComboBox;
    PopupMenuLog: TPopupMenu;
    MenuItemSelectAll: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemSeparator: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    SaveDialogLog: TSaveDialog;
    ButtonIcons: TImageList;
    ButtonQuietMode: TBitBtn;
    ButtonHealthReport: TBitBtn;
    MenuIcons: TImageList;
    MenuItemSeparator1: TMenuItem;
    MenuItemClearLog: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ComboDeviceChange(Sender: TObject);
    procedure CheckShowFixedClick(Sender: TObject);
    procedure CheckAdvancedClick(Sender: TObject);
    procedure RadioGroupModeClick(Sender: TObject);
    procedure EditInChange(Sender: TObject);
    procedure ButtonBenchmarkClick(Sender: TObject);
    procedure WMDeviceChange(var Msg: TMessage); message WM_DEVICECHANGE;
    procedure WMRefreshDevices(var Msg: TMessage); message WM_REFRESH_DEVICES;
    procedure MenuItemSelectAllClick(Sender: TObject);
    procedure MenuItemCopyClick(Sender: TObject);
    procedure MenuItemSaveAsClick(Sender: TObject);
    procedure ButtonRefreshClick(Sender: TObject);
    procedure ButtonQuietModeClick(Sender: TObject);
    procedure CheckVerifyHashClick(Sender: TObject);
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
    procedure ButtonHealthReportClick(Sender: TObject);
    procedure ComboDeviceDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure ComboDeviceMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
  private
    FThread: TThread;
    FLastLogProgress: Int64;
    FStartTime: TDateTime;
    FLastProgress: Int64;
    FLastUpdateTime: TDateTime;
    FDevicePath: string;
    FIsWriteMode: Boolean;
    FVolumeHandles: array of THandle;
    FLogFileName: string;
    FTaskbarList: ITaskbarList3;
    FTotalBytes: Int64;
    FOperationSuccess: Boolean;
    FDebugMode: Boolean; // Enable detailed debug logging with --debug argument
    FWorkerProcess: THandle; // Handle for elevated worker process
    FUseElevation: Boolean;   // Whether to use elevated worker process
    FWorkerPipe: TIPCPipeClient; // IPC pipe for persistent worker
    FWorkerPipeName: string; // Pipe name for persistent worker
    FOriginalPriority: DWORD; // Original process priority
    FQuietModeActive: Boolean; // Whether quiet mode is active
    FLastPartitionTableType: TPartitionTableType; // Last detected partition table type
    FLastPartitions: array[0..127] of TPartitionEntry; // Last parsed partitions
    FLastPartitionCount: Integer; // Number of partitions in last image
    FOperationInProgress: Boolean; // Flag for synchronous operations (archive extraction)
    FCancelRequested: Boolean; // Flag for cancelling synchronous operations
    FSettingsManager: TSettingsManager; // Settings and file history manager
    FDeviceHealthInfo: array[0..9] of TDeviceHealthInfo; // Health info for up to 10 devices
    FSMARTHistory: array[0..9, 0..99] of TSMARTHistory; // 100 history entries per device
    FCurrentDeviceHealth: TDeviceHealthStatus; // Current selected device health
    FSpeedWarningThreshold: Double; // MB/s threshold for speed warning (default 5.0)
    procedure HandleLog(S: string; Level: TLogLevel = llInfo);
    procedure HandleLogSimple(S: string); // For SetDebug compatibility
    procedure UpdateProgress(Progress: Int64);
    procedure ThreadCompleted(Sender: TObject);
    procedure ButtonBrowseClick(Sender: TObject);
    procedure UpdateButtonCaption;
    procedure PopulateDevices;
    function BytesToHuman(Bytes: Int64): string;
    function BytesShort(Bytes: Int64): string;
    function BuildWriteConfirmationMessage(const ImageFile, DeviceName: string): string;
    function ParseSize(const SizeStr: string; DefaultValue: Int64 = 512): Int64;
    function DriveTotalBytes(DriveLetter: Char): Int64;
    function GetDevicePath(ComboIndex: Integer): string;
    function ExtractDiskNumberFromCombo(ComboIndex: Integer): Integer;
    function ExtractDiskNumberFromPath(const DevicePath: string): Integer;
    function GetDiskSizeViaWMI(DiskNumber: Integer): Int64;
    // PrepareVolumeForWrite helpers
    function ExtractVolumeLetters(diskNum: Integer): string;
    procedure LockAndDismountVolume(const volDevice: string; volLetter: Char; UseWorker: Boolean);
    procedure UnlockVolumeHandle(VolumeHandle: THandle; UseWorker: Boolean);
    function GetPhysicalDiskSize(const DevicePath: string): Int64;
    function GetFileSize64(const FileName: string): Int64;
    function LockVolume(const VolumePath: string): THandle;
    function UnlockVolume(VolumeHandle: THandle): Boolean;
    function DismountVolume(VolumeHandle: THandle): Boolean;
    procedure PrepareVolumeForWrite(const DevicePath: string);
    procedure LoadFileHistory;
    procedure SaveFileHistory;
    procedure AddToFileHistory(const FilePath: string);
    function CalculateFileMD5(const FileName: string): string;
    function CalculateDeviceMD5(const DevicePath: string; Size: Int64): string;
    procedure SaveHashToFile(const ImageFile, Hash: string);
    procedure RestoreVolume(const DevicePath: string);
    procedure RecalculateBlockSizeAndCount;
    procedure LogForUIHelper(const Msg: string; Level: Integer);
    procedure AddFormattedLogMessage(const Msg: string; TextColor: TColor; IsBold: Boolean = True);
    procedure CleanupOldLogs(const LogDir: string);
    procedure ShowOperationResult;
    procedure UpdateAdvancedControls;
    procedure GetDiskInfoWMI(DiskNumber: Integer);
    procedure ShowAllPartitions(const DevicePath: string; DiskSize: Int64);
    procedure PartitionAnalyzerLog(S: string; Level: PartitionAnalyzer.TLogLevel);
    procedure AnalyzeDiskHealth(DiskNumber: Integer; const Info: TFullDiskInfo; SMARTAvailable: Boolean; const SMARTData: TSMARTData);
    function GetDeviceHealthStatus(DiskNumber: Integer): TDeviceHealthStatus;
    procedure SaveSMARTHistory(DiskNumber: Integer; const SMARTData: TSMARTData);
    function AnalyzeSMARTTrend(DiskNumber: Integer): string;
    procedure UpdateComboDeviceWithHealth;
    function GetHealthStatusIcon(Status: TDeviceHealthStatus): string;
    function GetHealthStatusColor(Status: TDeviceHealthStatus): TColor;
    procedure ShowHealthReport;
    function ValidateDeviceForWrite(DiskNumber: Integer): Boolean;
    function SendWorkerCommand(const Command: string): string;
    procedure StartPersistentWorker;
    procedure StopPersistentWorker;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure UpdateDeviceInfo;
    procedure SetQuietMode(Enabled: Boolean);
    // PopulateDevices helpers
    procedure ScanDriveLetters(List: TStringList);
    procedure MapDrivesToPhysicalDisks(List, mapLetters, mapSize: TStringList);
    procedure BuildDeviceComboItems(mapLetters, mapSize: TStringList);
    // EditInChange helpers
    procedure ProcessPartitions(const filePath: string; TableType: TPartitionTableType; var Partitions: array of TPartitionEntry; PartitionCount: Integer);
    // HandleLog helpers
    procedure GetLogLevelFormat(Level: TLogLevel; out levelStr: string; out color: TColor);
    procedure WriteLogToFile(const LogLine: string);
    // ButtonStartClick helpers
    function HandleCancelRequest: Boolean;
    function CheckArchiveComment(const inFile: string; isZip, isGZip: Boolean): Boolean;
    function CheckDeviceSizeForWrite(const devicePath: string; fileSize: Int64): Boolean;
    function ValidateReadOperation(const devicePath, outFile: string): Boolean;
    procedure HandleRegularOperation(isWriteMode: Boolean; const inFile, outFile: string; blockSize: Int64);
    function LaunchElevatedWorker(isWriteMode: Boolean; const devicePath: string): Boolean;
    procedure InitializeOperationUI(isWriteMode: Boolean);
    function ValidateWriteOperation(const inFile, devicePath: string; blockSize: Int64): Boolean;
    procedure HandleZipWrite(const inFile, devicePath: string; blockSize: Int64);
    procedure HandleGZipWrite(const inFile, devicePath: string; blockSize: Int64);
    procedure HandleXZWrite(const inFile, devicePath: string; blockSize: Int64);
    procedure HandleBZip2Write(const inFile, devicePath: string; blockSize: Int64);
    procedure HandleTarGzWrite(const inFile, devicePath: string; blockSize: Int64);
    procedure HandleTarXzWrite(const inFile, devicePath: string; blockSize: Int64);
    procedure Handle7ZipWrite(const inFile, devicePath: string; blockSize: Int64);
    procedure HandleZipRead(const devicePath, outFile: string; blockSize, diskSize: Int64);
    procedure HandleGZipRead(const devicePath, outFile: string; blockSize, diskSize: Int64);
    procedure HandleXZRead(const devicePath, outFile: string; blockSize, diskSize: Int64);
    procedure HandleBZip2Read(const devicePath, outFile: string; blockSize, diskSize: Int64);
    procedure Handle7ZipRead(const devicePath, outFile: string; blockSize, diskSize: Int64);
    procedure FinalizeArchiveOperation(const devicePath: string);
    // ThreadCompleted helpers
    procedure VerifyHashAfterWrite;
    procedure VerifyHashAfterRead;
    procedure FinalizeOperation;
    procedure EjectDevice(const DevicePath: string);
    // FormCreate helpers
    procedure InitializeLogging;
    procedure InitializeUI;
    procedure InitializeEventHandlers;
  public
  end;

  // Thread for IPC communication with elevated worker process
  TWorkerIPCThread = class(TThread)
  private
    FOwner: TMainForm;
    FPipe: TIPCPipeClient;
    FPipeName: string;
    FWorkerProcess: THandle;
    FSuccess: Boolean;
    FLogMessage: string;
    FProgress: Int64;
    FTotal: Int64;
    FIsWriteMode: Boolean;
    FFilePath: string;
    FBlockSize: Integer;
    procedure SyncLog;
    procedure SyncProgress;
    procedure DoCompleted;
    procedure SendFileData; // Send file data for WRITE mode
    procedure ReceiveFileData; // Receive file data for READ mode
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TMainForm; const PipeName: string; WorkerProcess: THandle; IsWriteMode: Boolean; const FilePath: string; BlockSize: Integer);
    destructor Destroy; override;
    procedure SendCancel;
  end;

  TDDThread = class(TThread)
  private
    FInFile, FOutFile: string;
    FBlockSize, FCount, FSeek, FSkip: Int64;
    FNoTruncateOut, FStopType: Boolean;
    FProgress: Int64;
    FOwner: TMainForm;
    FError: DWORD;
    FFinished: Boolean;
    FSuccess: Boolean;
    FExceptionMessage: string;
    FBlocksProcessed: Int64;
    procedure SyncProgress;
    procedure SyncLog;
    procedure DoCompleted;
    function Callback(Progress: Int64; Error: DWORD): Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TMainForm; const InFile, OutFile: string; BlockSize, Count, Skip, Seek: Int64; NoTruncateOut, StopType: Boolean);
  end;

var
  AppMainForm: TMainForm;

implementation

function IsVistaOrLater: Boolean;
var
  VerInfo: TOSVersionInfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(VerInfo);
  if GetVersionEx(VerInfo) then
    Result := VerInfo.dwMajorVersion >= 6
  else
    Result := False;
end;

{$R *.dfm}

const
  CREATE_NO_WINDOW = $08000000;
  DETACHED_PROCESS = $00000008;
  FSCTL_LOCK_VOLUME = $90018;
  FSCTL_UNLOCK_VOLUME = $9001C;
  FSCTL_DISMOUNT_VOLUME = $90020;
  STILL_ACTIVE = 259;

function IsGZipFile(const FileName: string): Boolean; forward;
function IsTarGZFile(const FileName: string): Boolean; forward;
function IsZipFile(const FileName: string): Boolean; forward;
function IsXZFile(const FileName: string): Boolean; forward;
function IsTarXZFile(const FileName: string): Boolean; forward;
function IsCompressedImage(const FileName: string): Boolean; forward;

function IsArchiveFile(const FileName: string): Boolean; forward;

function IsSpecialDevice(const Path: string): Boolean; forward;

function GetDriveTypeString(const FilePath: string): string; forward;

function StreamingCopyToDevice(InStream: TStream; OutFileName: string; BlockSize, TotalSize: Int64): Boolean; forward;

function StreamingCopyFromDevice(InFileName: string; OutStream: TStream; BlockSize, TotalSize: Int64): Boolean; forward;

constructor TDDThread.Create(Owner: TMainForm; const InFile, OutFile: string; BlockSize, Count, Skip, Seek: Int64; NoTruncateOut, StopType: Boolean);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FOwner := Owner;
  FInFile := InFile;
  FOutFile := OutFile;
  FBlockSize := BlockSize;
  FCount := Count;
  FSkip := Skip;
  FSeek := Seek;
  FNoTruncateOut := NoTruncateOut;
  FStopType := StopType;
  FFinished := False;
  FSuccess := True;
  FBlocksProcessed := 0;
  Resume;
end;

procedure TDDThread.SyncProgress;
begin
  if Assigned(FOwner) then
    FOwner.UpdateProgress(FProgress);
end;

procedure TDDThread.DoCompleted;
begin
  if Assigned(FOwner) then
  begin
    FOwner.FOperationSuccess := FSuccess;
    FOwner.ThreadCompleted(FOwner);
  end;
end;

function TDDThread.Callback(Progress: Int64; Error: DWORD): Boolean;
begin
  FProgress := Progress;
  FError := Error;
  
  // Calculate blocks from bytes
  if FBlockSize > 0 then
    FBlocksProcessed := Progress div FBlockSize;

  try
    Synchronize(SyncProgress);
  except
  end;
  Result := Terminated;
end;

procedure TDDThread.Execute;
begin
  try
    try
      DoDD(FInFile, FOutFile, FBlockSize, FCount, FSkip, FSeek, FNoTruncateOut, FStopType, Callback);
    except
      on E: Exception do
      begin
        if Assigned(FOwner) then
        begin
          FSuccess := False;
          FExceptionMessage := 'Error: ' + E.ClassName + ': ' + E.Message;
          Synchronize(SyncLog);
        end;
      end;
    end;
  finally
    FFinished := True;
    Synchronize(DoCompleted);
  end;
end;

procedure TDDThread.SyncLog;
begin
  if Assigned(FOwner) then
    FOwner.HandleLog(FExceptionMessage, llError);
end;

{ TMainForm }

procedure TMainForm.InitializeLogging;
var
  LogDir: string;
begin
  LogDir := ExtractZLib.GetZLibPath + 'log\';
  if not DirectoryExists(LogDir) then
    ForceDirectories(LogDir);
  FLogFileName := LogDir + 'ImageWriter_' + FormatDateTime('yyyymmdd', Now) + '.log';
  CleanupOldLogs(LogDir);
  
  MemoLog.Clear;
  SetDebug(HandleLogSimple);
  HandleLog('Log file: ' + FLogFileName + #13#10, llDebug);
end;

procedure TMainForm.InitializeUI;
begin
  Application.Title := 'ImageWriter';
  Caption := 'ImageWriter ' + AppVersion;
  
  // Initialize Taskbar List for Windows 7+
  try
    FTaskbarList := CreateComObject(CLSID_TaskbarList) as ITaskbarList3;
    if Assigned(FTaskbarList) then
      FTaskbarList.HrInit;
  except
    FTaskbarList := nil;
  end;
  
  // Initialize edit fields
  try
    EditIn.Text := '';
    EditBS.Text := '0';
    EditCount.Text := '0';
    EditSeek.Text := '0';
    EditSkip.Text := '0';
  except
    on E: Exception do
      HandleLog('Error setting edit fields: ' + E.Message, llError);
  end;
  
  try
    ProgressBar.Progress := 0;
  except
    on E: Exception do
      HandleLog('Error setting progress bar: ' + E.Message, llError);
  end;
  
  try
    UpdateButtonCaption;
  except
    on E: Exception do
      HandleLog('Error updating button caption: ' + E.Message, llError);
  end;
  
  try
    StatusBar.Panels[0].Text := '';
    StatusBar.Panels[1].Text := '';
    StatusBar.Panels[2].Text := '';
    StatusBar.Panels[3].Text := 'Idle';
  except
    on E: Exception do
      HandleLog('Error setting status bar: ' + E.Message, llError);
  end;
  
  // Настроить tooltip для ComboDevice (будет обновляться при смене устройства)
  if Assigned(ComboDevice) then
  begin
    ComboDevice.ShowHint := True;
    ComboDevice.Hint := 'Select target device';
  end;
end;

procedure TMainForm.InitializeEventHandlers;
begin
  ButtonBrowse.OnClick := ButtonBrowseClick;
  RadioGroupMode.OnClick := RadioGroupModeClick;
  EditIn.OnChange := EditInChange;
  EditIn.OnSelect := EditInChange;
  CheckAdvanced.OnClick := CheckAdvancedClick;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Disable maximize button
  BorderIcons := BorderIcons - [biMaximize];
  
  // Set above normal priority for better disk I/O performance
  SetPriorityClass(GetCurrentProcess, ABOVE_NORMAL_PRIORITY_CLASS);
  
  // Initialize logging system
  InitializeLogging;
  
  // Enable drag & drop for the form
  DragAcceptFiles(Handle, True);
  
  // Initialize UI components
  InitializeUI;

  // Log startup information
  HandleLog('=== ImageWriter Startup ===', llInfo);
  HandleLog('Program Info:', llInfo);
  HandleLog('  Program Path: ' + ParamStr(0), llInfo);
  HandleLog('  Version: ' + AppVersion, llInfo);
  {$IFDEF IMAGEWRITER_PRO}
  HandleLog('  Edition: ImageWriter Pro', llInfo);
  {$ELSE}
  HandleLog('  Edition: ImageWriter', llInfo);
  {$ENDIF}
  HandleLog('  Author: Anton Zelenov', llInfo);
  HandleLog('  Email: tixset@gmail.com', llInfo);
  HandleLog('  GitHub: https://github.com/tixset/ImageWriter', llInfo);
  HandleLog('Workspace:', llInfo);
  HandleLog('  Computer: ' + GetEnvironmentVariable('COMPUTERNAME'), llInfo);
  HandleLog('  User: ' + GetEnvironmentVariable('USERNAME'), llInfo);
  HandleLog('IsProcessElevated: ' + BoolToStr(IsProcessElevated, True), llInfo);
  HandleLog('IsVistaOrLater: ' + BoolToStr(IsVistaOrLater, True), llInfo);
  HandleLog('=================================' + #13#10, llInfo);
  
  // Initialize state variables
  FLastLogProgress := 0;
  FWorkerProcess := 0;
  FUseElevation := False;
  FWorkerPipe := nil;
  FWorkerPipeName := '';
  FOriginalPriority := NORMAL_PRIORITY_CLASS;
  FQuietModeActive := False;
  FLastPartitionTableType := pttUnknown;
  FLastPartitionCount := 0;
  SetLength(FVolumeHandles, 0);
  
  // Инициализация health tracking
  FSpeedWarningThreshold := 5.0; // MB/s для USB 3.0
  FCurrentDeviceHealth := dhsUnknown;
  FillChar(FDeviceHealthInfo, SizeOf(FDeviceHealthInfo), 0);
  FillChar(FSMARTHistory, SizeOf(FSMARTHistory), 0);
  
  // Check for --debug command line argument
  FDebugMode := False;
  if (ParamCount > 0) and (LowerCase(ParamStr(1)) = '--debug') then
  begin
    FDebugMode := True;
    HandleLog('Debug mode enabled via --debug argument', llInfo);
  end;
  
  // Populate device list
  HandleLog('Scanning for available devices...', llInfo);
  
  try
    PopulateDevices;
  except
    on E: Exception do
      HandleLog('Error populating devices: ' + E.Message, llError);
  end;
  
  // Setup event handlers
  InitializeEventHandlers;
  
  // Create settings manager and load settings
  FSettingsManager := TSettingsManager.Create;
  LoadFileHistory;
  LoadSettings;
  
  // Initialize advanced controls visibility
  UpdateAdvancedControls;
  
  {$IFDEF IMAGEWRITER_PRO}
  // Start persistent worker in Pro edition
  StartPersistentWorker;
  {$ENDIF}

  // Set ready state
  StatusBar.Panels[3].Text := 'Idle';
  HandleLog('Ready!' + #13#10, llInfo);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Disable drag & drop
  DragAcceptFiles(Handle, False);
  
  {$IFDEF IMAGEWRITER_PRO}
  // Stop persistent worker
  StopPersistentWorker;
  {$ENDIF}
  
  // Terminate worker process if running
  if Assigned(FThread) and (FThread is TWorkerIPCThread) then
  begin
    TWorkerIPCThread(FThread).SendCancel;
    FThread.Terminate;
    FThread.WaitFor;
    FThread := nil;
  end;

  SaveFileHistory;
  SaveSettings;
  
  // Destroy settings manager
  FSettingsManager.Free;
  
  SetDebug(nil);
end;

procedure TMainForm.CleanupOldLogs(const LogDir: string);
var
  SR: TSearchRec;
  LogFiles: TStringList;
  TotalSize: Int64;
  i: Integer;
  FilePath: string;
const
  MAX_LOG_SIZE = 100 * 1024 * 1024; // 100 MB
begin
  TotalSize := 0;
  LogFiles := TStringList.Create;
  try
    // Collect all log files and their sizes
    if FindFirst(LogDir + 'ImageWriter_*.log', faAnyFile, SR) = 0 then
    begin
      repeat
        if (SR.Attr and faDirectory) = 0 then
        begin
          FilePath := LogDir + SR.Name;
          LogFiles.AddObject(FilePath, TObject(SR.Size));
          TotalSize := TotalSize + SR.Size;
        end;
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
    
    // If total size exceeds limit, delete old files
    if TotalSize > MAX_LOG_SIZE then
    begin
      // Sort by name (files named with date, so sorting by name = by date)
      LogFiles.Sort;
      
      // Delete old files until we reach acceptable size (50% of limit)
      i := 0;
      while (TotalSize > MAX_LOG_SIZE div 2) and (i < LogFiles.Count) do
      begin
        FilePath := LogFiles[i];
        // Do not delete current log file
        if FilePath <> FLogFileName then
        begin
          try
            DeleteFile(FilePath);
            TotalSize := TotalSize - Integer(LogFiles.Objects[i]);
          except
            // Ignore deletion errors
          end;
        end;
        Inc(i);
      end;
    end;
  finally
    LogFiles.Free;
  end;
end;

procedure TMainForm.GetDiskInfoWMI(DiskNumber: Integer);
var
  DevMgr: TDeviceManager;
  Info: TFullDiskInfo;
  BenchMgr: TDeviceBenchmark;
  SMARTData: TSMARTData;
  SMARTAvailable: Boolean;
{$IFDEF IMAGEWRITER_PRO}
  Response: string;
  Lines: TStringList;
  i: Integer;
{$ENDIF}
begin
  {$IFDEF IMAGEWRITER_PRO}
  // In Pro edition, use persistent worker
  if Assigned(FWorkerPipe) and FWorkerPipe.Connected then
  begin
    Response := SendWorkerCommand('GETDISKINFO|' + IntToStr(DiskNumber));
    if (Response <> '') and (Pos('ERROR:', Response) <> 1) then
    begin
      Lines := TStringList.Create;
      try
        Lines.Text := Response;
        for i := 0 to Lines.Count - 1 do
          HandleLog(Lines[i], llInfo);
      finally
        Lines.Free;
      end;
    end;
    Exit;
  end;
  {$ENDIF}

  DevMgr := TDeviceManager.Create;
  try
    if DevMgr.GetFullDiskInfoWMI(DiskNumber, Info) then
    begin
      HandleLog('=== Device Information ===', llInfo);
      
      // Hardware
      HandleLog('Hardware:', llInfo);
      if Info.Model <> '' then
        HandleLog('  Model: ' + Info.Model, llInfo);
      if Info.Manufacturer <> '' then
        HandleLog('  Manufacturer: ' + Info.Manufacturer, llInfo);
      if Info.SerialNumber <> '' then
        HandleLog('  Serial Number: ' + Info.SerialNumber, llInfo);
      if Info.FirmwareRevision <> '' then
        HandleLog('  Firmware: ' + Info.FirmwareRevision, llInfo);
      if Info.Size > 0 then
        HandleLog('  Capacity: ' + BytesToHuman(Info.Size), llInfo);
      
      // Connection
      HandleLog('Connection:', llInfo);
      if Info.InterfaceType <> '' then
        HandleLog('  Interface: ' + Info.InterfaceType, llInfo);
      if Info.MediaType <> '' then
        HandleLog('  Media Type: ' + Info.MediaType, llInfo);
      if (Info.USBVID <> '') and (Info.USBPID <> '') then
        HandleLog('  USB VID:PID: ' + Info.USBVID + ':' + Info.USBPID, llInfo);
      if Info.PNPDeviceID <> '' then
        HandleLog('  Device ID: ' + Info.PNPDeviceID, llDebug);
      
      // Geometry
      if (Info.BytesPerSector > 0) or (Info.SectorsPerTrack > 0) then
      begin
        HandleLog('Geometry:', llInfo);
        if Info.BytesPerSector > 0 then
          HandleLog('  Bytes per sector: ' + IntToStr(Info.BytesPerSector), llInfo);
        if Info.SectorsPerTrack > 0 then
          HandleLog('  Sectors per track: ' + IntToStr(Info.SectorsPerTrack), llInfo);
      end;

      HandleLog('=================================' + #13#10, llInfo);
      
      // Получить SMART данные и выполнить диагностику
      BenchMgr := TDeviceBenchmark.Create('\\.\PhysicalDrive' + IntToStr(DiskNumber));
      try
        SMARTAvailable := BenchMgr.GetSMARTData(SMARTData);
        AnalyzeDiskHealth(DiskNumber, Info, SMARTAvailable, SMARTData);
      finally
        BenchMgr.Free;
      end;
    end
    else
    begin
      // WMI failed - perform basic health check without device info
      HandleLog('WMI: Unable to get disk information', llWarning);
      HandleLog('Performing basic health check without WMI data...' + #13#10, llInfo);
      
      // Попробовать получить SMART данные и выполнить базовую диагностику
      BenchMgr := TDeviceBenchmark.Create('\\.\PhysicalDrive' + IntToStr(DiskNumber));
      try
        SMARTAvailable := BenchMgr.GetSMARTData(SMARTData);
        AnalyzeDiskHealth(DiskNumber, Info, SMARTAvailable, SMARTData);
      finally
        BenchMgr.Free;
      end;
    end;
  finally
    DevMgr.Free;
  end;
end;

procedure TMainForm.PartitionAnalyzerLog(S: string; Level: PartitionAnalyzer.TLogLevel);
var
  MainFormLevel: MainForm.TLogLevel;
begin
  // Map PartitionAnalyzer.TLogLevel to MainForm.TLogLevel
  case Level of
    PartitionAnalyzer.llDebug: MainFormLevel := MainForm.llDebug;
    PartitionAnalyzer.llInfo: MainFormLevel := MainForm.llInfo;
    PartitionAnalyzer.llWarning: MainFormLevel := MainForm.llWarning;
    PartitionAnalyzer.llError: MainFormLevel := MainForm.llError;
  else
    MainFormLevel := MainForm.llInfo;
  end;
  HandleLog(S, MainFormLevel);
end;

procedure TMainForm.AnalyzeDiskHealth(DiskNumber: Integer; const Info: TFullDiskInfo; SMARTAvailable: Boolean; const SMARTData: TSMARTData);
var
  healthIssues: Integer;
  warningCount: Integer;
  criticalCount: Integer;
  i: Integer;
  attrValue: Integer;
  DevicePath: string;
  PartitionIssues: Boolean;
  Analyzer: TPartitionAnalyzer;
  PartErrorMsg: string;
  ActualDiskSize: Int64;
begin
  healthIssues := 0;
  warningCount := 0;
  criticalCount := 0;
  PartitionIssues := False;
  
  HandleLog('=== Device Health Analysis ===', llInfo);
  
  // Check WMI parameters
  HandleLog('Hardware Diagnostics:', llInfo);
  
  // Check 1: Device capacity
  if Info.Size = 0 then
  begin
    HandleLog('  Capacity: 0 bytes [!] Device not recognized or access denied', llError);
    Inc(criticalCount);
  end
  else if Info.Size < 1048576 then // < 1 MB
  begin
    HandleLog('  Capacity: ' + BytesToHuman(Info.Size) + ' [!] Suspiciously small capacity', llWarning);
    Inc(warningCount);
  end
  else
    HandleLog('  Capacity: ' + BytesToHuman(Info.Size) + ' [OK]', llInfo);
  
  // Check 2: Serial number (counterfeit detection)
  if Trim(Info.SerialNumber) = '' then
  begin
    HandleLog('  Serial Number: (empty) [!] Possible counterfeit device', llWarning);
    Inc(warningCount);
  end
  else
    HandleLog('  Serial Number: ' + Info.SerialNumber + ' [OK]', llInfo);
  
  // Check 3: Sector size
  if Info.BytesPerSector > 0 then
  begin
    if (Info.BytesPerSector <> 512) and (Info.BytesPerSector <> 4096) then
    begin
      HandleLog('  Bytes per Sector: ' + IntToStr(Info.BytesPerSector) + ' [!] Non-standard sector size', llWarning);
      Inc(warningCount);
    end
    else
      HandleLog('  Bytes per Sector: ' + IntToStr(Info.BytesPerSector) + ' [OK]', llInfo);
  end;
  
  // SMART diagnostics
  if SMARTAvailable then
  begin
    HandleLog('SMART Diagnostics:', llInfo);
    
    // Overall health
    if SMARTData.OverallHealth = 'Healthy' then
      HandleLog('  Overall Health: ' + SMARTData.OverallHealth + ' [OK]', llInfo)
    else if SMARTData.OverallHealth = 'WARNING' then
    begin
      HandleLog('  Overall Health: ' + SMARTData.OverallHealth + ' [!] Device degradation detected', llWarning);
      Inc(warningCount);
    end
    else if SMARTData.OverallHealth = 'CRITICAL' then
    begin
      HandleLog('  Overall Health: ' + SMARTData.OverallHealth + ' [!] Critical condition - data loss risk!', llError);
      Inc(criticalCount);
    end
    else
      HandleLog('  Overall Health: ' + SMARTData.OverallHealth, llInfo);
    
    // Temperature check
    if SMARTData.Temperature > 0 then
    begin
      if SMARTData.Temperature > 70 then
      begin
        HandleLog('  Temperature: ' + IntToStr(SMARTData.Temperature) + '°C [!] Overheating - critical!', llError);
        Inc(criticalCount);
      end
      else if SMARTData.Temperature > 60 then
      begin
        HandleLog('  Temperature: ' + IntToStr(SMARTData.Temperature) + '°C [!] High temperature', llWarning);
        Inc(warningCount);
      end
      else
        HandleLog('  Temperature: ' + IntToStr(SMARTData.Temperature) + '°C [OK]', llInfo);
    end;
    
    // Critical SMART attributes check
    HandleLog('Critical SMART Attributes:', llInfo);
    
    for i := 0 to 29 do
    begin
      if SMARTData.Attributes[i].ID > 0 then
      begin
        attrValue := SMARTData.Attributes[i].RawValue;
        
        case SMARTData.Attributes[i].ID of
          5: // Reallocated Sectors Count
            begin
              if attrValue > 0 then
              begin
                HandleLog('  ID 5 (Reallocated Sectors): ' + IntToStr(attrValue) + ' [!] Bad blocks detected', llWarning);
                Inc(warningCount);
              end
              else
                HandleLog('  ID 5 (Reallocated Sectors): ' + IntToStr(attrValue) + ' [OK]', llInfo);
            end;
          
          197: // Current Pending Sector Count
            begin
              if attrValue > 0 then
              begin
                HandleLog('  ID 197 (Pending Sectors): ' + IntToStr(attrValue) + ' [!] CRITICAL - unstable sectors!', llError);
                Inc(criticalCount);
              end
              else
                HandleLog('  ID 197 (Pending Sectors): ' + IntToStr(attrValue) + ' [OK]', llInfo);
            end;
          
          198: // Offline Uncorrectable
            begin
              if attrValue > 0 then
              begin
                HandleLog('  ID 198 (Uncorrectable Sectors): ' + IntToStr(attrValue) + ' [!] CRITICAL - permanent damage!', llError);
                Inc(criticalCount);
              end
              else
                HandleLog('  ID 198 (Uncorrectable Sectors): ' + IntToStr(attrValue) + ' [OK]', llInfo);
            end;
          
          187: // Reported Uncorrectable Errors
            begin
              if attrValue > 0 then
              begin
                HandleLog('  ID 187 (Reported Errors): ' + IntToStr(attrValue) + ' [!] ECC errors detected', llWarning);
                Inc(warningCount);
              end
              else
                HandleLog('  ID 187 (Reported Errors): ' + IntToStr(attrValue) + ' [OK]', llInfo);
            end;
        end;
      end;
    end;
    
    // Power-on hours (wear indicator)
    if SMARTData.PowerOnHours > 0 then
    begin
      if SMARTData.PowerOnHours > 20000 then
      begin
        HandleLog('  Power-On Hours: ' + IntToStr(SMARTData.PowerOnHours) + ' [!] Old device - consider replacement', llWarning);
        Inc(warningCount);
      end
      else
        HandleLog('  Power-On Hours: ' + IntToStr(SMARTData.PowerOnHours) + ' [OK]', llInfo);
    end;
  end
  else
    HandleLog('SMART Diagnostics: Not available (may not be supported)', llInfo);
  
  // Partition table validation
  HandleLog('Partition Table Diagnostics:', llInfo);
  if (DiskNumber >= 0) and (DiskNumber <= 9) then
  begin
    DevicePath := '\\.\PhysicalDrive' + IntToStr(DiskNumber);
    Analyzer := TPartitionAnalyzer.Create(nil);
    try
      // Use actual disk size from IOCTL instead of WMI (WMI can be inaccurate)
      ActualDiskSize := GetPhysicalDiskSize(DevicePath);
      if ActualDiskSize = 0 then
        ActualDiskSize := Info.Size; // Fallback to WMI if IOCTL fails
      
      if not Analyzer.ValidatePartitionTable(DevicePath, ActualDiskSize, PartErrorMsg) then
      begin
        HandleLog('  Partition Table: [!] ' + PartErrorMsg, llError);
        Inc(criticalCount);
        PartitionIssues := True;
      end
      else
        HandleLog('  Partition Table: [OK] Valid', llInfo);
    finally
      Analyzer.Free;
    end;
  end
  else
    HandleLog('  Partition Table: Unable to validate (device path unknown)', llWarning);
  
  // Summary
  HandleLog('', llInfo);
  HandleLog('Health Summary:', llInfo);
  
  if (criticalCount = 0) and (warningCount = 0) then
    HandleLog('  Status: [OK] Device appears healthy', llInfo)
  else if criticalCount > 0 then
    HandleLog('  Status: [X] CRITICAL - ' + IntToStr(criticalCount) + ' critical issue(s), ' + IntToStr(warningCount) + ' warning(s)', llError)
  else
    HandleLog('  Status: [!] WARNING - ' + IntToStr(warningCount) + ' issue(s) detected', llWarning);
  
  if criticalCount > 0 then
    HandleLog('  Recommendation: Do NOT use this device for important data!', llError)
  else if warningCount > 0 then
    HandleLog('  Recommendation: Use with caution, monitor device health', llWarning)
  else if PartitionIssues then
    HandleLog('  Recommendation: Partition table is corrupted - reformat the device', llError);
  
  HandleLog('=================================' + #13#10, llInfo);
  
  // Сохранить health status для устройства
  if (DiskNumber >= 0) and (DiskNumber <= 9) then
  begin
    FDeviceHealthInfo[DiskNumber].Status := dhsUnknown;
    if criticalCount > 0 then
      FDeviceHealthInfo[DiskNumber].Status := dhsCritical
    else if warningCount > 0 then
      FDeviceHealthInfo[DiskNumber].Status := dhsWarning
    else if (criticalCount = 0) and (warningCount = 0) and not PartitionIssues then
      FDeviceHealthInfo[DiskNumber].Status := dhsHealthy;
      
    FDeviceHealthInfo[DiskNumber].WarningCount := warningCount;
    FDeviceHealthInfo[DiskNumber].CriticalCount := criticalCount;
    FDeviceHealthInfo[DiskNumber].LastCheckTime := Now;
    FDeviceHealthInfo[DiskNumber].SerialNumber := Info.SerialNumber;
    FDeviceHealthInfo[DiskNumber].Model := Info.Model;
    if PartitionIssues then
      FDeviceHealthInfo[DiskNumber].ErrorMessage := PartErrorMsg
    else
      FDeviceHealthInfo[DiskNumber].ErrorMessage := '';
      
    FCurrentDeviceHealth := FDeviceHealthInfo[DiskNumber].Status;
    
    // Сохранить SMART историю
    if SMARTAvailable then
      SaveSMARTHistory(DiskNumber, SMARTData);
  end;
end;

function TMainForm.GetDeviceHealthStatus(DiskNumber: Integer): TDeviceHealthStatus;
begin
  Result := dhsUnknown;
  if (DiskNumber >= 0) and (DiskNumber <= 9) then
    Result := FDeviceHealthInfo[DiskNumber].Status;
end;

procedure TMainForm.SaveSMARTHistory(DiskNumber: Integer; const SMARTData: TSMARTData);
var
  i, j: Integer;
  found: Boolean;
begin
  if (DiskNumber < 0) or (DiskNumber > 9) then
    Exit;
    
  // Сдвинуть историю
  for i := 99 downto 1 do
    FSMARTHistory[DiskNumber, i] := FSMARTHistory[DiskNumber, i - 1];
    
  // Добавить новую запись
  FSMARTHistory[DiskNumber, 0].CheckTime := Now;
  FSMARTHistory[DiskNumber, 0].Temperature := SMARTData.Temperature;
  FSMARTHistory[DiskNumber, 0].PowerOnHours := SMARTData.PowerOnHours;
  
  // Извлечь критические атрибуты
  FSMARTHistory[DiskNumber, 0].ReallocatedSectors := 0;
  FSMARTHistory[DiskNumber, 0].PendingSectors := 0;
  for i := 0 to 29 do
  begin
    if SMARTData.Attributes[i].ID = 5 then
      FSMARTHistory[DiskNumber, 0].ReallocatedSectors := SMARTData.Attributes[i].RawValue
    else if SMARTData.Attributes[i].ID = 197 then
      FSMARTHistory[DiskNumber, 0].PendingSectors := SMARTData.Attributes[i].RawValue;
  end;
end;

function TMainForm.AnalyzeSMARTTrend(DiskNumber: Integer): string;
var
  i, validEntries: Integer;
  tempIncrease, sectorIncrease: Boolean;
  avgTemp, maxTemp: Integer;
  oldestTemp, newestTemp: Integer;
  oldestSectors, newestSectors: Integer;
begin
  Result := '';
  
  if (DiskNumber < 0) or (DiskNumber > 9) then
    Exit;
    
  // Подсчитать валидные записи
  validEntries := 0;
  for i := 0 to 99 do
  begin
    if FSMARTHistory[DiskNumber, i].CheckTime > 0 then
      Inc(validEntries)
    else
      Break;
  end;
  
  if validEntries < 2 then
  begin
    Result := 'Insufficient history data (need at least 2 measurements)';
    Exit;
  end;
  
  // Анализ температуры
  avgTemp := 0;
  maxTemp := 0;
  for i := 0 to validEntries - 1 do
  begin
    avgTemp := avgTemp + FSMARTHistory[DiskNumber, i].Temperature;
    if FSMARTHistory[DiskNumber, i].Temperature > maxTemp then
      maxTemp := FSMARTHistory[DiskNumber, i].Temperature;
  end;
  avgTemp := avgTemp div validEntries;
  
  oldestTemp := FSMARTHistory[DiskNumber, validEntries - 1].Temperature;
  newestTemp := FSMARTHistory[DiskNumber, 0].Temperature;
  tempIncrease := (newestTemp - oldestTemp) > 5;
  
  // Анализ секторов
  oldestSectors := FSMARTHistory[DiskNumber, validEntries - 1].ReallocatedSectors + 
                   FSMARTHistory[DiskNumber, validEntries - 1].PendingSectors;
  newestSectors := FSMARTHistory[DiskNumber, 0].ReallocatedSectors + 
                   FSMARTHistory[DiskNumber, 0].PendingSectors;
  sectorIncrease := newestSectors > oldestSectors;
  
  Result := Format('Trend Analysis (%d measurements):', [validEntries]) + #13#10;
  Result := Result + Format('  Temperature: Avg %d°C, Max %d°C', [avgTemp, maxTemp]);
  if tempIncrease then
    Result := Result + ' [!] Rising trend (+' + IntToStr(newestTemp - oldestTemp) + '°C)';
  Result := Result + #13#10;
  
  Result := Result + Format('  Bad Sectors: %d currently', [newestSectors]);
  if sectorIncrease then
    Result := Result + ' [!] Degradation detected (+' + IntToStr(newestSectors - oldestSectors) + ' sectors)';
end;

procedure TMainForm.UpdateComboDeviceWithHealth;
begin
  // Перерисовать ComboBox с иконками статуса
  ComboDevice.Invalidate;
end;

function TMainForm.GetHealthStatusIcon(Status: TDeviceHealthStatus): string;
begin
  case Status of
    dhsHealthy: Result := '[OK]';
    dhsWarning: Result := '[!] ';
    dhsCritical: Result := '[X] ';
  else
    Result := '[?] ';
  end;
end;

function TMainForm.GetHealthStatusColor(Status: TDeviceHealthStatus): TColor;
begin
  case Status of
    dhsHealthy: Result := RGB(200, 255, 200);  // Light green
    dhsWarning: Result := RGB(255, 220, 180);  // Light orange
    dhsCritical: Result := RGB(255, 200, 200); // Light red
  else
    Result := clWhite;
  end;
end;

procedure TMainForm.ShowHealthReport;
var
  DiskNumber: Integer;
  Report: TStringList;
  Trend: string;
begin
  DiskNumber := ExtractDiskNumberFromCombo(ComboDevice.ItemIndex);
  if DiskNumber < 0 then
  begin
    ShowMessage('Please select a device first');
    Exit;
  end;
  
  Report := TStringList.Create;
  try
    Report.Add('=== Device Health Report ===');
    Report.Add('');
    Report.Add('Device: ' + FDeviceHealthInfo[DiskNumber].Model);
    Report.Add('Serial: ' + FDeviceHealthInfo[DiskNumber].SerialNumber);
    Report.Add('Status: ' + GetHealthStatusIcon(FDeviceHealthInfo[DiskNumber].Status));
    Report.Add('Last Check: ' + DateTimeToStr(FDeviceHealthInfo[DiskNumber].LastCheckTime));
    Report.Add('');
    Report.Add('Issues Found:');
    Report.Add('  Warnings: ' + IntToStr(FDeviceHealthInfo[DiskNumber].WarningCount));
    Report.Add('  Critical: ' + IntToStr(FDeviceHealthInfo[DiskNumber].CriticalCount));
    
    if FDeviceHealthInfo[DiskNumber].ErrorMessage <> '' then
    begin
      Report.Add('');
      Report.Add('Error: ' + FDeviceHealthInfo[DiskNumber].ErrorMessage);
    end;
    
    // Добавить трендовый анализ
    Trend := AnalyzeSMARTTrend(DiskNumber);
    if Trend <> '' then
    begin
      Report.Add('');
      Report.Add(Trend);
    end;
    
    Report.Add('');
    Report.Add('================================');
    
    ShowMessage(Report.Text);
  finally
    Report.Free;
  end;
end;

function TMainForm.ValidateDeviceForWrite(DiskNumber: Integer): Boolean;
var
  Status: TDeviceHealthStatus;
begin
  Result := True;
  Status := GetDeviceHealthStatus(DiskNumber);
  
  case Status of
    dhsCritical:
      begin
        ShowMessage('CRITICAL DEVICE ERROR!' + #13#10 + #13#10 +
                   'This device has critical health issues.' + #13#10 +
                   'Writing to this device is BLOCKED to prevent data loss.' + #13#10 + #13#10 +
                   'Issues: ' + IntToStr(FDeviceHealthInfo[DiskNumber].CriticalCount) + ' critical' + #13#10 +
                   'Recommendation: Replace this device immediately.');
        Result := False;
      end;
      
    dhsWarning:
      begin
        if MessageDlg('Device Health Warning' + #13#10 + #13#10 +
                     'This device has ' + IntToStr(FDeviceHealthInfo[DiskNumber].WarningCount) + ' health warning(s).' + #13#10 +
                     'Writing to this device may result in data loss or corruption.' + #13#10 + #13#10 +
                     'Do you want to continue anyway?',
                     mtWarning, [mbYes, mbNo], 0) = mrNo then
          Result := False;
      end;
  end;
end;

procedure TMainForm.ShowAllPartitions(const DevicePath: string; DiskSize: Int64);
var
  Analyzer: TPartitionAnalyzer;
begin
  Analyzer := TPartitionAnalyzer.Create(PartitionAnalyzerLog);
  try
    Analyzer.ShowAllPartitions(DevicePath, DiskSize);
  finally
    Analyzer.Free;
  end;
end;

procedure TMainForm.CheckShowFixedClick(Sender: TObject);
begin
  PopulateDevices;
end;

procedure TMainForm.CheckAdvancedClick(Sender: TObject);
begin
  UpdateAdvancedControls;
end;

procedure TMainForm.UpdateAdvancedControls;
var
  ShowAdvanced: Boolean;
begin
  ShowAdvanced := CheckAdvanced.Checked;
  
  // Show/hide labels
  LabelBS.Enabled := ShowAdvanced;
  LabelCount.Enabled := ShowAdvanced;
  LabelSeek.Enabled := ShowAdvanced;
  LabelSkip.Enabled := ShowAdvanced;
  
  // Show/hide edit controls
  EditBS.Enabled := ShowAdvanced;
  EditCount.Enabled := ShowAdvanced;
  EditSeek.Enabled := ShowAdvanced;
  EditSkip.Enabled := ShowAdvanced;
  
  // Show/hide checkboxes
  CheckShowFixed.Enabled := ShowAdvanced;
  CheckVerifyHash.Enabled := ShowAdvanced;

  if Assigned(ComboHashAlgo) then
    ComboHashAlgo.Enabled := ShowAdvanced;
end;

procedure TMainForm.UpdateDeviceInfo;
var
  devPath: string;
begin
  if ComboDevice.ItemIndex < 0 then
    Exit;

  devPath := GetDevicePath(ComboDevice.ItemIndex);
end;

procedure TMainForm.ButtonRefreshClick(Sender: TObject);
begin
  HandleLog('Refreshing device list...', llInfo);
  PopulateDevices;
  HandleLog('Device list refreshed', llInfo);
end;

procedure TMainForm.SetQuietMode(Enabled: Boolean);
var
  CurrentPriority: DWORD;
begin
  if Enabled then
  begin
    // Save current priority and lower it
    CurrentPriority := GetPriorityClass(GetCurrentProcess);
    if CurrentPriority <> 0 then
      FOriginalPriority := CurrentPriority;

    SetPriorityClass(GetCurrentProcess, IDLE_PRIORITY_CLASS);
    HandleLog('Process priority lowered to IDLE', llInfo);
    
    // Minimize window
    Application.Minimize;
    WindowState := wsMinimized;

    FQuietModeActive := True;
    ButtonQuietMode.Caption := 'Normal Mode';
  end
  else
  begin
    // Restore original priority
    if FOriginalPriority <> 0 then
    begin
      SetPriorityClass(GetCurrentProcess, FOriginalPriority);
      HandleLog('Process priority restored to ' + IntToStr(FOriginalPriority), llInfo);
    end;
    
    // Restore window
    Application.Restore;
    WindowState := wsNormal;

    FQuietModeActive := False;
    ButtonQuietMode.Caption := 'Quiet Mode';
  end;
end;

procedure TMainForm.ButtonQuietModeClick(Sender: TObject);
begin
  SetQuietMode(not FQuietModeActive);
end;

procedure TMainForm.WMDropFiles(var Msg: TMessage);
var
  hDrop: THandle;
  FileCount, i, FileNameLen: Integer;
  FileName: array[0..MAX_PATH] of Char;
begin
  hDrop := THandle(Msg.WParam);
  try
    FileCount := DragQueryFile(hDrop, $FFFFFFFF, nil, 0);
    if FileCount > 0 then
    begin
      // Use the first file
      FileNameLen := DragQueryFile(hDrop, 0, FileName, SizeOf(FileName));
      if FileNameLen > 0 then
      begin
        EditIn.Text := FileName;
        AddToFileHistory(FileName);
        HandleLog('File dropped: ' + FileName, llInfo);
        
        // Log additional files if multiple were dropped
        if FileCount > 1 then
        begin
          HandleLog(Format('Note: %d files dropped, using first file only', [FileCount]), llInfo);
          for i := 1 to FileCount - 1 do
          begin
            FileNameLen := DragQueryFile(hDrop, i, FileName, SizeOf(FileName));
            if FileNameLen > 0 then
              HandleLog('  Ignored: ' + FileName, llInfo);
          end;
        end;
      end;
    end;
  finally
    DragFinish(hDrop);
  end;
end;

procedure TMainForm.ProcessPartitions(const filePath: string; TableType: TPartitionTableType; var Partitions: array of TPartitionEntry; PartitionCount: Integer);
var
  PartitionInfo: string;
  TableTypeName: string;
begin
  // Save partition info
  FLastPartitionTableType := TableType;
  FLastPartitionCount := PartitionCount;
  
  if PartitionCount > 0 then
    Move(Partitions, FLastPartitions, SizeOf(TPartitionEntry) * PartitionCount);
  
  // Determine table type name
  case TableType of
    pttMBR: TableTypeName := 'MBR';
    pttGPT: TableTypeName := 'GPT';
  else
    TableTypeName := 'Unknown';
  end;
  
  if PartitionCount > 0 then
  begin
    PartitionInfo := FormatPartitionInfo(Partitions, PartitionCount);
    HandleLog(PartitionInfo, llInfo);
    HandleLog(Format('Successfully read %d partition(s) from %s table', [PartitionCount, TableTypeName]), llInfo);
  end
  else
    HandleLog(Format('No %s partitions found', [TableTypeName]), llWarning);
end;

procedure TMainForm.RadioGroupModeClick(Sender: TObject);
begin
  try
    RecalculateBlockSizeAndCount;
    UpdateButtonCaption;
  except
    on E: Exception do
      HandleLog('Error in RadioGroupModeClick: ' + E.Message, llError);
  end;
end;

procedure TMainForm.EditInChange(Sender: TObject);
var
  filePath: string;
  PartitionTableType: TPartitionTableType;
  Partitions: array[0..127] of TPartitionEntry;
  PartitionCount: Integer;
  archiveType: TArchiveType;
begin
  filePath := Trim(EditIn.Text);
  
  // Check for special devices first
  if IsSpecialDevice(filePath) then
  begin
    HandleLog('Selected special device: ' + filePath, llInfo);
    if filePath = '/dev/zero' then
      HandleLog('Device type: Zero-filled data stream', llInfo)
    else if filePath = '/dev/random' then
      HandleLog('Device type: Random data stream (MT19937)', llInfo)
    else if filePath = '/dev/null' then
      HandleLog('Device type: Null device (discard)', llInfo)
    else if filePath = '-' then
      HandleLog('Device type: Standard input/output', llInfo);
    RecalculateBlockSizeAndCount;
    Exit;
  end;
  
  // If file path is not empty and file exists, log info and recalculate
  if (filePath <> '') and FileExists(filePath) then
  begin
    HandleLog('Selected file: ' + filePath, llInfo);
    HandleLog('Drive type: ' + GetDriveTypeString(filePath), llInfo);
    
    // Detect archive type first
    archiveType := TArchiveHandler.DetectArchiveType(filePath);
    
    // Determine file type
    case archiveType of
      atTarGz: HandleLog('File type: Archive (TAR.GZ)', llInfo);
      atTarXz: HandleLog('File type: Archive (TAR.XZ)', llInfo);
      atXZ: HandleLog('File type: Archive (XZ)', llInfo);
      atZip: HandleLog('File type: Archive (ZIP)', llInfo);
      atGZip: HandleLog('File type: Archive (GZIP)', llInfo);
      atBZip2: HandleLog('File type: Archive (BZIP2)', llInfo);
      at7Zip: HandleLog('File type: Archive (7z)', llInfo);
    else
      HandleLog('File type: Disk image', llInfo);
    end;

    // Analyze partition table (only for Write mode)
    if (RadioGroupMode.ItemIndex = 0) then // Write mode
    begin
      try
        HandleLog('Analyzing partition table...', llDebug);
        
        // Check if it's an archive - analyze contents without extraction
        if archiveType <> atNone then
        begin
          HandleLog('Analyzing partition table inside archive (streaming first 64KB)...', llDebug);
          PartitionTableType := TArchivePartitionReader.DetectPartitionTableInArchive(filePath, archiveType);
        end
        else
        begin
          // Regular file - direct analysis
          PartitionTableType := DetectPartitionTableType(filePath);
        end;
        
        HandleLog('Detection result: ' + IntToStr(Ord(PartitionTableType)), llDebug);
        
        case PartitionTableType of
          pttMBR:
            begin
              HandleLog('Partition table: MBR detected', llInfo);
              
              if archiveType <> atNone then
                PartitionCount := TArchivePartitionReader.ParseMBRPartitionsFromArchive(filePath, archiveType, Partitions)
              else
                PartitionCount := ParseMBRPartitions(filePath, Partitions);
                
              HandleLog('Parsed partitions count: ' + IntToStr(PartitionCount), llDebug);
              ProcessPartitions(filePath, PartitionTableType, Partitions, PartitionCount);
            end;
          pttGPT:
            begin
              HandleLog('Partition table: GPT detected', llInfo);
              
              if archiveType <> atNone then
                PartitionCount := TArchivePartitionReader.ParseGPTPartitionsFromArchive(filePath, archiveType, Partitions)
              else
                PartitionCount := ParseGPTPartitions(filePath, Partitions);
                
              HandleLog('Parsed partitions count: ' + IntToStr(PartitionCount), llDebug);
              ProcessPartitions(filePath, PartitionTableType, Partitions, PartitionCount);
            end;
          pttUnknown:
            begin
              HandleLog('Partition table: Not detected or unsupported format', llWarning);
              FLastPartitionTableType := pttUnknown;
              FLastPartitionCount := 0;
            end;
        end;
      except
        on E: Exception do
          HandleLog('Error analyzing partitions: ' + E.Message, llError);
      end;
    end;

    RecalculateBlockSizeAndCount;
  end
  else if filePath = '' then
  begin
    // File path cleared, recalculate with device only
    RecalculateBlockSizeAndCount;
  end;
end;

procedure TMainForm.WMDeviceChange(var Msg: TMessage);
const
  DBT_DEVICEARRIVAL = $8000;
  DBT_DEVICEREMOVECOMPLETE = $8004;
begin
  // Update device list on connect/disconnect
  if (Msg.wParam = DBT_DEVICEARRIVAL) or (Msg.wParam = DBT_DEVICEREMOVECOMPLETE) then
  begin
    HandleLog('Device change detected, refreshing device list...', llDebug);
    // Post message to refresh devices after Windows finishes processing device change
    // This avoids COM reentrancy error when calling WMI from within WM_DEVICECHANGE handler
    PostMessage(Handle, WM_REFRESH_DEVICES, 0, 0);
  end;
  inherited;
end;

procedure TMainForm.WMRefreshDevices(var Msg: TMessage);
begin
  // Wait for device to initialize (USB devices need time for WMI to recognize them)
  Sleep(1000);
  PopulateDevices;
end;

procedure TMainForm.GetLogLevelFormat(Level: TLogLevel; out levelStr: string; out color: TColor);
begin
  // Default initialization
  levelStr := '[INFO]   ';
  color := clBlack;
  
  case Level of
    llDebug:
      begin
        levelStr := '[DEBUG]  ';
        color := clGray;
      end;
    llInfo:
      begin
        levelStr := '[INFO]   ';
        color := clBlack;
      end;
    llWarning:
      begin
        levelStr := '[WARNING]';
        color := clOlive;
      end;
    llError:
      begin
        levelStr := '[ERROR]  ';
        color := clRed;
      end;
  end;
end;

const
  EM_STREAMIN = WM_USER + 73;
  SF_TEXT = $0001;
  SF_RTF = $0002;
  SFF_SELECTION = $8000;

var
  RTFStreamData: string;

function RTFStreamInCallback(dwCookie: Longint; pbBuff: PByte; cb: Longint; var pcb: Longint): DWORD; stdcall;
var
  Remaining: Integer;
begin
  Result := 0;
  Remaining := Length(RTFStreamData) - dwCookie;
  if Remaining > 0 then
  begin
    if Remaining > cb then
      pcb := cb
    else
      pcb := Remaining;
    Move(RTFStreamData[dwCookie + 1], pbBuff^, pcb);
  end
  else
    pcb := 0;
end;

procedure InsertRTFText(RichEdit: TRichEdit; const RTFText: string);
var
  EditStream: TEditStream;
begin
  RTFStreamData := RTFText;
  
  EditStream.dwCookie := 0;
  EditStream.dwError := 0;
  EditStream.pfnCallback := RTFStreamInCallback;
  
  SendMessage(RichEdit.Handle, EM_STREAMIN, SF_RTF or SFF_SELECTION, Longint(@EditStream));
end;

procedure TMainForm.WriteLogToFile(const LogLine: string);
var
  LogStream: TFileStream;
  AnsiLine: AnsiString;
begin
  if FLogFileName = '' then
    Exit;
    
  try
    if FileExists(FLogFileName) then
      LogStream := TFileStream.Create(FLogFileName, fmOpenWrite or fmShareDenyWrite)
    else
      LogStream := TFileStream.Create(FLogFileName, fmCreate or fmShareDenyWrite);
    try
      AnsiLine := AnsiString(LogLine + #13#10);
      LogStream.Seek(0, soFromEnd);
      LogStream.WriteBuffer(AnsiLine[1], Length(AnsiLine));
    finally
      LogStream.Free;
    end;
  except
    // Ignore file write errors
  end;
end;

procedure TMainForm.HandleLog(S: string; Level: TLogLevel = llInfo);
var
  timestamp: string;
  levelStr: string;
  color: TColor;
  LogLine: string;
  i, startPos, colonPos: Integer;
  textBefore, textAfter: string;
  valueColor: TColor;
  isHeader: Boolean;
begin
  timestamp := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  
  // Get log level format
  GetLogLevelFormat(Level, levelStr, color);
  
  // Build log line
  LogLine := '[' + timestamp + '] ' + levelStr + ' ' + S;
  
  // Clear old entries if more than 1000
  if MemoLog.Lines.Count > 1000 then
    MemoLog.Lines.Delete(0);
  
  // Add formatted text to memo with bold labels and colored values
  MemoLog.SelStart := Length(MemoLog.Text);
  
  // For WARNING/ERROR - insert entire line with background via RTF
  if Level = llWarning then
  begin
    // Soft orange background for entire line via RTF
    InsertRTFText(MemoLog, Format('{\rtf1\ansi{\colortbl;\red128\green128\blue128;\red0\green0\blue0;\red255\green220\blue180;}' +
                                  '\highlight3\cf1 [%s] \cf2\b %s \cf0\b0\highlight0\par\pard}', 
                                  [timestamp, levelStr + ' ' + S]));
    
    // Reset formatting after RTF insertion
    MemoLog.SelAttributes.Color := clBlack;
    MemoLog.SelAttributes.Style := [];
    
    // Write to file
    WriteLogToFile(LogLine);
    Exit;
  end
  else if Level = llError then
  begin
    // Soft red background for entire line via RTF
    InsertRTFText(MemoLog, Format('{\rtf1\ansi{\colortbl;\red128\green128\blue128;\red0\green0\blue0;\red255\green200\blue200;}' +
                                  '\highlight3\cf1 [%s] \cf2\b %s \cf0\b0\highlight0\par\pard}', 
                                  [timestamp, levelStr + ' ' + S]));
    
    // Reset formatting after RTF insertion
    MemoLog.SelAttributes.Color := clBlack;
    MemoLog.SelAttributes.Style := [];
    
    // Write to file
    WriteLogToFile(LogLine);
    Exit;
  end;
  
  // Normal formatting for INFO/DEBUG
  
  // Check for success message (completed successfully, verification passed, ready, etc.)
  if (Pos('successfully', LowerCase(S)) > 0) or
     (Pos('verification passed', LowerCase(S)) > 0) or
     (Pos('completed', LowerCase(S)) > 0) or
     (Pos('success', LowerCase(S)) > 0) or
     (Trim(LowerCase(S)) = 'ready!') then
  begin
    // Light green background for success
    InsertRTFText(MemoLog, Format('{\rtf1\ansi{\colortbl;\red128\green128\blue128;\red0\green0\blue0;\red200\green255\blue200;}' +
                                  '\highlight3\cf1 [%s] \cf2\b %s \cf0\b0\highlight0\par\pard}', 
                                  [timestamp, levelStr + ' ' + S]));
    
    // Reset formatting after RTF insertion
    MemoLog.SelAttributes.Color := clBlack;
    MemoLog.SelAttributes.Style := [];
    
    // Write to file
    WriteLogToFile(LogLine);
    Exit;
  end;
  
  // Add timestamp in gray
  MemoLog.SelAttributes.Color := clGray;
  MemoLog.SelAttributes.Style := [];
  MemoLog.SelText := '[' + timestamp + '] ';
  
  // Add level in bold with original color
  MemoLog.SelAttributes.Color := color;
  MemoLog.SelAttributes.Style := [fsBold];
  MemoLog.SelText := levelStr + ' ';
  
  // Check if this is a header line (=== Text ===)
  isHeader := (Pos('===', S) = 1) or (Pos('---', S) = 1) or (Pos('***', S) = 1);
  
  if isHeader then
  begin
    // Bold header
    MemoLog.SelAttributes.Color := clNavy;
    MemoLog.SelAttributes.Style := [fsBold];
    MemoLog.SelText := S + #13#10;
  end
  else
  begin
  
  // Parse and format message (labels: bold, values: colored)
  i := 1;
  while i <= Length(S) do
  begin
    // Find next colon
    colonPos := PosEx(':', S, i);
    
    if colonPos > 0 then
    begin
      // Extract text before colon (label)
      textBefore := Trim(Copy(S, i, colonPos - i));
      
      // Check if it's a label (words possibly with spaces, but reasonable length)
      if (Length(textBefore) > 0) and (Length(textBefore) < 60) and
         (Pos('/', textBefore) = 0) and (Pos('\', textBefore) = 0) and
         (Pos('http', LowerCase(textBefore)) = 0) then
      begin
        // Add any leading spaces
        startPos := i;
        while (startPos <= Length(S)) and (S[startPos] = ' ') do
        begin
          MemoLog.SelAttributes.Style := [];
          MemoLog.SelAttributes.Color := clBlack;
          MemoLog.SelText := ' ';
          Inc(startPos);
        end;
        
        // This is a label - make it bold
        MemoLog.SelAttributes.Color := clBlack;
        MemoLog.SelAttributes.Style := [fsBold];
        MemoLog.SelText := textBefore + ':';
        
        // Find value after colon
        i := colonPos + 1;
        while (i <= Length(S)) and (S[i] = ' ') do Inc(i); // Skip spaces
        
        startPos := i;
        // Extract value until comma, semicolon, or significant whitespace
        while (i <= Length(S)) and not (S[i] in [',', ';', #13, #10]) do
          Inc(i);
        
        textAfter := Trim(Copy(S, startPos, i - startPos));
        
        // Determine value color based on content
        if (Pos('MB', UpperCase(textAfter)) > 0) or (Pos('GB', UpperCase(textAfter)) > 0) or 
           (Pos('KB', UpperCase(textAfter)) > 0) or (Pos('TB', UpperCase(textAfter)) > 0) or
           (Pos('%', textAfter) > 0) or (Pos('bytes', LowerCase(textAfter)) > 0) then
          valueColor := clBlue  // Sizes and percentages
        else if (Pos('OK', UpperCase(textAfter)) > 0) or (Pos('Success', textAfter) > 0) or
                (Pos('Yes', textAfter) > 0) or (Pos('True', textAfter) > 0) or
                (Pos('Healthy', textAfter) > 0) or
                (Pos('enabled', LowerCase(textAfter)) > 0) or (Pos('active', LowerCase(textAfter)) > 0) then
          valueColor := clGreen  // Success indicators
        else if (Pos('Error', textAfter) > 0) or (Pos('Failed', textAfter) > 0) or
                (Pos('No', textAfter) > 0) or (Pos('False', textAfter) > 0) or
                (Pos('WARNING', UpperCase(textAfter)) > 0) or (Pos('CRITICAL', UpperCase(textAfter)) > 0) or
                (Pos('[!]', textAfter) > 0) or (Pos('(empty)', textAfter) > 0) or
                (Pos('disabled', LowerCase(textAfter)) > 0) or (Pos('invalid', LowerCase(textAfter)) > 0) then
          valueColor := clRed  // Error indicators
        else if (Pos('PhysicalDrive', textAfter) > 0) or (Pos('Drive ', textAfter) > 0) or
                (Pos('\\?\', textAfter) > 0) or (Pos('PHYSICALDRIVE', UpperCase(textAfter)) > 0) then
          valueColor := clPurple  // Drive names
        else if (textAfter <> '') and (textAfter[1] in ['0'..'9']) then
          valueColor := clNavy  // Numbers
        else if Length(textAfter) > 0 then
          valueColor := clTeal  // Other values
        else
          valueColor := clBlack;
        
        // Add space and value in color
        if Length(textAfter) > 0 then
        begin
          MemoLog.SelAttributes.Style := [];
          MemoLog.SelText := ' ';
          
          MemoLog.SelAttributes.Color := valueColor;
          MemoLog.SelAttributes.Style := [];
          MemoLog.SelText := textAfter;
        end;
      end
      else
      begin
        // Not a label, just add as regular text
        MemoLog.SelAttributes.Color := clBlack;
        MemoLog.SelAttributes.Style := [];
        MemoLog.SelText := textBefore + ':';
        i := colonPos + 1;
      end;
    end
    else
    begin
      // No more colons, add remaining text
      textAfter := Copy(S, i, Length(S) - i + 1);
      MemoLog.SelAttributes.Color := clBlack;
      MemoLog.SelAttributes.Style := [];
      MemoLog.SelText := textAfter;
      Break;
    end;
  end;
  
  // Add newline
  MemoLog.SelText := #13#10;
  end;
  
  // Reset formatting
  MemoLog.SelAttributes.Color := clBlack;
  MemoLog.SelAttributes.Style := [];
  
  // Scroll to last line
  MemoLog.SelStart := Length(MemoLog.Text);
  MemoLog.Perform(EM_SCROLLCARET, 0, 0);
  
  // Write to file
  WriteLogToFile(LogLine);
end;

procedure TMainForm.HandleLogSimple(S: string);
begin
  // Wrapper for SetDebug compatibility - uses llInfo for operation statistics
  HandleLog(S, llInfo);
end;

function TMainForm.ParseSize(const SizeStr: string; DefaultValue: Int64 = 512): Int64;
begin
  Result := TFileOperations.ParseSize(SizeStr, DefaultValue);
end;

procedure TMainForm.ButtonBrowseClick(Sender: TObject);
var
  ofd: TOpenDialog;
  sfd: TSaveDialog;
begin
  if RadioGroupMode.ItemIndex = 0 then
  begin
    // Write mode - select file to write to disk
    HandleLog('Opening file selection dialog (Write mode)...', llDebug);
    ofd := TOpenDialog.Create(nil);
    try
      ofd.Filter := 'All Images|*.img;*.iso;*.zip;*.gz;*.xz;*.img.gz;*.img.xz;*.iso.gz;*.iso.xz;*.tar.gz;*.tar.xz|' +
                    'Disk Images|*.img;*.iso|' +
                    'Archives|*.zip;*.gz;*.xz;*.img.gz;*.img.xz;*.iso.gz;*.iso.xz;*.tar.gz;*.tar.xz|' +
                    'All files|*.*';
      if ofd.Execute then
      begin
        EditIn.Text := ofd.FileName;
        // Trigger EditInChange manually to ensure partition analysis runs
        EditInChange(EditIn);
      end
      else
        HandleLog('File selection cancelled', llDebug);
    finally
      ofd.Free;
    end;
  end
  else
  begin
    // Read mode - select file to save disk image
    HandleLog('Opening file save dialog (Read mode)...', llDebug);
    sfd := TSaveDialog.Create(nil);
    try
      sfd.Filter := 'Disk Images|*.img;*.iso|' +
                    'Compressed Images|*.img.gz;*.img.xz;*.iso.gz;*.iso.xz;*.gz;*.xz;*.tar.gz;*.tar.xz|' +
                    'All files|*.*';
      sfd.DefaultExt := 'img';
      if sfd.Execute then
      begin
        EditIn.Text := sfd.FileName;
        // Trigger EditInChange manually
        EditInChange(EditIn);
      end
      else
        HandleLog('File save cancelled', llDebug);
    finally
      sfd.Free;
    end;
  end;
end;

procedure TMainForm.CheckVerifyHashClick(Sender: TObject);
begin
  // When verify hash is enabled, ensure SHA256 is selected (default)
  if CheckVerifyHash.Checked then
  begin
    if ComboHashAlgo.ItemIndex <> 0 then // 0 is SHA256 now
    begin
      ComboHashAlgo.ItemIndex := 0;
      HandleLog('Hash verification enabled, using SHA256', llDebug);
    end;
  end;
end;

procedure TMainForm.ComboDeviceChange(Sender: TObject);
var
  devPath, letters: string;
  idx, p1, p2: Integer;
  diskSize: Int64;
  diskNum: Integer;
  deviceInfoHelper: TDeviceInfoHelper;
  HealthInfo: TDeviceHealthInfo;
  TooltipText: string;
begin
  if ComboDevice.ItemIndex < 0 then
    Exit;

  devPath := GetDevicePath(ComboDevice.ItemIndex);
  
  // Extract disk number from combo text
  idx := ComboDevice.ItemIndex;
  diskNum := ExtractDiskNumberFromCombo(idx);
  
  // Extract letters from combo text like "#0 [E:2G]"
  if idx >= 0 then
  begin
    p1 := Pos('[', ComboDevice.Items[idx]);
    p2 := Pos(':', ComboDevice.Items[idx]);
    if (p1 > 0) and (p2 > p1) then
    begin
      letters := Copy(ComboDevice.Items[idx], p1 + 1, p2 - p1 - 1);
      HandleLog('Selected device: ' + letters + ': -> ' + devPath, llInfo);
      
      // Update device info label (используется уже полученная WMI информация)
      UpdateDeviceInfo;
      
      // Get disk size and analyze partitions
      diskSize := GetPhysicalDiskSize(devPath);
      if diskSize > 0 then
        ShowAllPartitions(devPath, diskSize);
      
      // Show mounted filesystems using DeviceInfoHelper
      deviceInfoHelper := TDeviceInfoHelper.Create(LogForUIHelper);
      try
        deviceInfoHelper.ShowMountedFilesystems(letters);
      finally
        deviceInfoHelper.Free;
      end;
      
      // Обновить Tooltip с health информацией
      if (diskNum >= 0) and (diskNum <= 9) then
      begin
        HealthInfo := FDeviceHealthInfo[diskNum];
        TooltipText := GetHealthStatusIcon(HealthInfo.Status) + ' ' + HealthInfo.Model;
        if HealthInfo.LastCheckTime > 0 then
        begin
          TooltipText := TooltipText + #13#10 + 
                        'Health: ' + GetHealthStatusIcon(HealthInfo.Status) + ' ';
          if HealthInfo.Status = dhsHealthy then
            TooltipText := TooltipText + 'Healthy'
          else if HealthInfo.Status = dhsWarning then
            TooltipText := TooltipText + IntToStr(HealthInfo.WarningCount) + ' warnings'
          else if HealthInfo.Status = dhsCritical then
            TooltipText := TooltipText + IntToStr(HealthInfo.CriticalCount) + ' CRITICAL issues';
        end;
        ComboDevice.Hint := TooltipText;
        
        // Перерисовать ComboBox с обновлённой иконкой
        ComboDevice.Invalidate;
      end;
    end;
  end;

  RecalculateBlockSizeAndCount;
end;

function TMainForm.ExtractDiskNumberFromCombo(ComboIndex: Integer): Integer;
var
  numStr: string;
  p1, p2: Integer;
begin
  Result := -1;
  
  if (ComboIndex < 0) or (ComboIndex >= ComboDevice.Items.Count) then
    Exit;
    
  p1 := Pos('#', ComboDevice.Items[ComboIndex]);
  p2 := Pos('[', ComboDevice.Items[ComboIndex]);
  if (p1 > 0) and (p2 > p1) then
  begin
    numStr := Trim(Copy(ComboDevice.Items[ComboIndex], p1 + 1, p2 - p1 - 1));
    Result := StrToIntDef(numStr, -1);
  end;
end;

function TMainForm.ExtractDiskNumberFromPath(const DevicePath: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  
  for i := Length(DevicePath) downto 1 do
  begin
    if DevicePath[i] in ['0'..'9'] then
    begin
      Result := StrToIntDef(Copy(DevicePath, i, Length(DevicePath) - i + 1), -1);
      Break;
    end;
  end;
end;

function TMainForm.GetDevicePath(ComboIndex: Integer): string;
var
  diskNum: Integer;
begin
  Result := '';
  
  diskNum := ExtractDiskNumberFromCombo(ComboIndex);
  if diskNum >= 0 then
    Result := '\\?\PhysicalDrive' + IntToStr(diskNum);
end;

function TMainForm.GetDiskSizeViaWMI(DiskNumber: Integer): Int64;
var
  DevMgr: TDeviceManager;
begin
  DevMgr := TDeviceManager.Create;
  try
    Result := DevMgr.GetDiskSizeViaWMI(DiskNumber);
  finally
    DevMgr.Free;
  end;
end;

function TMainForm.ExtractVolumeLetters(diskNum: Integer): string;
var
  i: Integer;
  volumePath: string;
begin
  Result := '';
  
  for i := 0 to ComboDevice.Items.Count - 1 do
  begin
    if Pos('#' + IntToStr(diskNum) + ' ', ComboDevice.Items[i]) = 1 then
    begin
      if Pos('[', ComboDevice.Items[i]) > 0 then
      begin
        volumePath := Copy(ComboDevice.Items[i], Pos('[', ComboDevice.Items[i]) + 1, 100);
        Result := Copy(volumePath, 1, Pos(':', volumePath) - 1);
      end;
      Break;
    end;
  end;
end;

procedure TMainForm.LockAndDismountVolume(const volDevice: string; volLetter: Char; UseWorker: Boolean);
var
  volumeHandle: THandle;
{$IFDEF IMAGEWRITER_PRO}
  Response: string;
{$ENDIF}
begin
  if UseWorker then
  begin
    {$IFDEF IMAGEWRITER_PRO}
    if Assigned(FWorkerPipe) and FWorkerPipe.Connected then
    begin
      HandleLog('Locking volume via worker: ' + volDevice, llDebug);
      Response := SendWorkerCommand('LOCKVOLUME|' + volDevice);
      if Pos('OK', Response) > 0 then
      begin
        HandleLog('Volume locked successfully via worker', llDebug);
        Response := SendWorkerCommand('DISMOUNTVOLUME|' + volDevice);
        if Pos('OK', Response) > 0 then
          HandleLog('Volume dismounted successfully via worker', llDebug)
        else
          HandleLog('Dismount failed via worker: ' + Response, llWarning);
        
        SetLength(FVolumeHandles, Length(FVolumeHandles) + 1);
        FVolumeHandles[High(FVolumeHandles)] := THandle(Ord(volLetter) or HANDLE_MARKER_WORKER_LOCKED);
      end
      else
        HandleLog('Lock failed via worker: ' + Response, llWarning);
    end
    else
    {$ENDIF}
    begin
      HandleLog('Worker not available for volume lock', llWarning);
      volumeHandle := LockVolume(volDevice);
      if volumeHandle <> INVALID_HANDLE_VALUE then
      begin
        DismountVolume(volumeHandle);
        SetLength(FVolumeHandles, Length(FVolumeHandles) + 1);
        FVolumeHandles[High(FVolumeHandles)] := volumeHandle;
      end;
    end;
  end
  else
  begin
    volumeHandle := LockVolume(volDevice);
    if volumeHandle <> INVALID_HANDLE_VALUE then
    begin
      DismountVolume(volumeHandle);
      SetLength(FVolumeHandles, Length(FVolumeHandles) + 1);
      FVolumeHandles[High(FVolumeHandles)] := volumeHandle;
    end;
  end;
end;

procedure TMainForm.UnlockVolumeHandle(VolumeHandle: THandle; UseWorker: Boolean);
var
  volLetter: Char;
  volDevice: string;
{$IFDEF IMAGEWRITER_PRO}
  Response: string;
{$ENDIF}
begin
  if VolumeHandle = INVALID_HANDLE_VALUE then
    Exit;
    
  if UseWorker and ((VolumeHandle and HANDLE_MARKER_WORKER_LOCKED) <> 0) then
  begin
    volLetter := Chr(VolumeHandle and $FF);
    if not (volLetter in ['A'..'Z']) then
    begin
      HandleLog('Invalid volume letter in handle: ' + IntToStr(Ord(volLetter)), llError);
      Exit;
    end;
    
    volDevice := '\\.' + volLetter + ':';
    
    {$IFDEF IMAGEWRITER_PRO}
    if Assigned(FWorkerPipe) and FWorkerPipe.Connected then
    begin
      HandleLog('Unlocking volume via worker: ' + volDevice, llDebug);
      Response := SendWorkerCommand('UNLOCKVOLUME|' + volDevice);
      if Pos('OK', Response) > 0 then
        HandleLog('Volume unlocked successfully via worker', llDebug)
      else
        HandleLog('Unlock failed via worker: ' + Response, llWarning);
    end
    else
    {$ENDIF}
      HandleLog('Worker not available for volume unlock', llWarning);
  end
  else
    UnlockVolume(VolumeHandle);
end;

function TMainForm.GetPhysicalDiskSize(const DevicePath: string): Int64;
var
  h: THandle;
  Response: string;
begin
  Result := 0;
  
  // Check if persistent worker is available (not IFDEF, check at runtime)
  if Assigned(FWorkerPipe) and FWorkerPipe.Connected then
  begin
    try
      Response := SendWorkerCommand('GETDISKSIZE|' + DevicePath);
      Result := StrToInt64Def(Response, 0);
      if Result > 0 then
        HandleLog('Disk size from worker: ' + BytesToHuman(Result) + ' (' + IntToStr(Result) + ' bytes)', llDebug);
      Exit;
    except
      on E: Exception do
        HandleLog('Error getting disk size from worker: ' + E.Message, llError);
    end;
  end;
  
  // Skip trying to open physical drives without admin rights
  if (Pos('PHYSICALDRIVE', UpperCase(DevicePath)) > 0) and not IsProcessElevated then
  begin
    // Will show size as "Admin rights required" in UI
    Exit;
  end;

  h := CreateFile(PChar(DevicePath), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if h <> INVALID_HANDLE_VALUE then
  begin
    try
      Result := GetDiskSize(h);
      // Log moved to callers to avoid duplicate logs
    finally
      CloseHandle(h);
    end;
  end
  else
    HandleLog('Cannot open device ' + DevicePath + ' to get size (Error: ' + IntToStr(GetLastError) + ')', llError);
end;

function TMainForm.GetFileSize64(const FileName: string): Int64;
begin
  Result := TFileOperations.GetFileSize64(FileName);
end;

function TMainForm.LockVolume(const VolumePath: string): THandle;
var
  bytesReturned: DWORD;
  retries: Integer;
  VolumeHandle: THandle;
begin
  Result := INVALID_HANDLE_VALUE;
  HandleLog('Locking volume: ' + VolumePath, llDebug);

  VolumeHandle := CreateFile(PChar(VolumePath), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);

  if VolumeHandle = INVALID_HANDLE_VALUE then
  begin
    HandleLog('Cannot open volume ' + VolumePath + ' (Error: ' + IntToStr(GetLastError) + ')', llError);
    Exit;
  end;
  
  // Try to lock volume (retry up to 10 times)
  retries := 0;
  while retries < 10 do
  begin
    if DeviceIoControl(VolumeHandle, FSCTL_LOCK_VOLUME, nil, 0, nil, 0, bytesReturned, nil) then
    begin
      HandleLog('Volume locked successfully', llDebug);
      Result := VolumeHandle;
      Exit;
    end;
    Inc(retries);
    Sleep(500);
  end;

  HandleLog('Cannot lock volume (Error: ' + IntToStr(GetLastError) + ')', llError);
  CloseHandle(VolumeHandle);
  Result := INVALID_HANDLE_VALUE;
end;

function TMainForm.UnlockVolume(VolumeHandle: THandle): Boolean;
var
  bytesReturned: DWORD;
begin
  Result := False;
  if VolumeHandle = INVALID_HANDLE_VALUE then
    Exit;

  HandleLog('Unlocking volume...', llDebug);
  Result := DeviceIoControl(VolumeHandle, FSCTL_UNLOCK_VOLUME, nil, 0, nil, 0, bytesReturned, nil);

  if Result then
    HandleLog('Volume unlocked successfully', llDebug)
  else
    HandleLog('Cannot unlock volume (Error: ' + IntToStr(GetLastError) + ')', llError);

  CloseHandle(VolumeHandle);
end;

function TMainForm.DismountVolume(VolumeHandle: THandle): Boolean;
var
  bytesReturned: DWORD;
begin
  Result := False;
  if VolumeHandle = INVALID_HANDLE_VALUE then
    Exit;

  HandleLog('Dismounting volume...', llDebug);
  Result := DeviceIoControl(VolumeHandle, FSCTL_DISMOUNT_VOLUME, nil, 0, nil, 0, bytesReturned, nil);

  if Result then
    HandleLog('Volume dismounted successfully', llDebug)
  else
    HandleLog('Cannot dismount volume (Error: ' + IntToStr(GetLastError) + ')', llError);
end;

procedure TMainForm.PrepareVolumeForWrite(const DevicePath: string);
var
  diskNum, volIdx: Integer;
  volumeLetters: string;
  volLetter: Char;
  volDevice: string;
  UseWorkerForLock: Boolean;
begin
  HandleLog('=== Preparing volume for write ===', llInfo);
  
  // Always clean up previous volume handles first
  if Length(FVolumeHandles) > 0 then
  begin
    HandleLog('Cleaning up previous volume handles', llWarning);
    RestoreVolume(DevicePath);
  end;
  SetLength(FVolumeHandles, 0);
  
  // Check if we should use worker for volume operations
  UseWorkerForLock := FUseElevation and not IsProcessElevated;
  
  // Extract disk number from DevicePath
  diskNum := ExtractDiskNumberFromPath(DevicePath);
  if diskNum < 0 then
  begin
    HandleLog('Cannot extract disk number from path: ' + DevicePath, llWarning);
    Exit;
  end;

  HandleLog('Disk number: ' + IntToStr(diskNum), llDebug);
  
  // Extract volume letters for this disk
  volumeLetters := ExtractVolumeLetters(diskNum);
  if volumeLetters <> '' then
  begin
    HandleLog('Volume letters found: ' + volumeLetters, llDebug);
    
    // Lock and dismount each volume
    for volIdx := 1 to Length(volumeLetters) do
    begin
      if volumeLetters[volIdx] in ['A'..'Z'] then
      begin
        volLetter := volumeLetters[volIdx];
        volDevice := '\\.\' + volLetter + ':';
        LockAndDismountVolume(volDevice, volLetter, UseWorkerForLock);
      end;
    end;
  end;

  HandleLog('=== Volume prepared ===', llInfo);
end;

procedure TMainForm.RestoreVolume(const DevicePath: string);
var
  i: Integer;
  UseWorkerForUnlock: Boolean;
begin
  UseWorkerForUnlock := FUseElevation and not IsProcessElevated;

  if Length(FVolumeHandles) > 0 then
  begin
    HandleLog('Unlocking ' + IntToStr(Length(FVolumeHandles)) + ' volume(s)...', llDebug);
    for i := 0 to High(FVolumeHandles) do
      UnlockVolumeHandle(FVolumeHandles[i], UseWorkerForUnlock);
    
    SetLength(FVolumeHandles, 0);
    HandleLog('All volumes unlocked', llDebug);
  end
  else
    HandleLog('No volume handles to unlock', llDebug);

  HandleLog('=== Volume restored ===', llInfo);
end;

procedure TMainForm.ShowOperationResult;
begin
  // Operation completion reset progress and show result
  if Assigned(FTaskbarList) then
  begin
    FTaskbarList.SetProgressState(Application.Handle, TBPF_NOPROGRESS);
    FlashWindow(Application.Handle, True);
  end;

  if FOperationSuccess then
    AddFormattedLogMessage('=== Operation completed successfully ===', clGreen)
  else
    AddFormattedLogMessage('=== Operation failed ===', clRed);

  StatusBar.Panels[0].Text := '';
  StatusBar.Panels[1].Text := '';
  StatusBar.Panels[2].Text := '';
  StatusBar.Panels[3].Text := 'Idle';
  UpdateButtonCaption;
end;

procedure TMainForm.RecalculateBlockSizeAndCount;
var
  devicePath, inFile: string;
  diskSize: Int64;
  isWriteMode: Boolean;
  uiHelper: TUIHelper;
begin
  // Safety check for UI components
  if not Assigned(RadioGroupMode) or not Assigned(ComboDevice) or not Assigned(EditIn) or not Assigned(EditBS) or not Assigned(EditCount) then
    Exit;
    
  isWriteMode := (RadioGroupMode.ItemIndex = 0);
  
  // Get device path
  if ComboDevice.ItemIndex >= 0 then
    devicePath := GetDevicePath(ComboDevice.ItemIndex)
  else
    devicePath := '';
    
  // Get input file
  inFile := Trim(EditIn.Text);
  
  // Get disk size
  if devicePath <> '' then
    diskSize := GetPhysicalDiskSize(devicePath)
  else
    diskSize := 0;

  // Use UIHelper to calculate
  uiHelper := TUIHelper.Create;
  try
    if isWriteMode then
      uiHelper.RecalculateBlockSizeAndCount(omWrite, EditBS, EditCount, inFile, diskSize, LogForUIHelper)
    else
      uiHelper.RecalculateBlockSizeAndCount(omRead, EditBS, EditCount, inFile, diskSize, LogForUIHelper);
  finally
    uiHelper.Free;
  end;
end;

procedure TMainForm.LogForUIHelper(const Msg: string; Level: Integer);
begin
  HandleLog(Msg, TLogLevel(Level));
end;

procedure TMainForm.AddFormattedLogMessage(const Msg: string; TextColor: TColor; IsBold: Boolean = True);
begin
  MemoLog.SelStart := Length(MemoLog.Text);
  MemoLog.SelAttributes.Color := TextColor;
  if IsBold then
    MemoLog.SelAttributes.Style := [fsBold]
  else
    MemoLog.SelAttributes.Style := [];
  MemoLog.Lines.Add(Msg);
  MemoLog.SelAttributes.Color := clBlack;
  MemoLog.SelAttributes.Style := [];
end;

// Helper: Scan drive letters A-Z and build list of drive|volLink|size
procedure TMainForm.ScanDriveLetters(List: TStringList);
var
  drive: Char;
  driveType: UINT;
  volName, volLink, entry: string;
  sizeInt, off, ln: Int64;
  h: THandle;
  foundCount: Integer;
begin
  foundCount := 0;
  sizeInt := 0; // Initialize to avoid potential use of uninitialized variable
  HandleLog('Enumerating drive letters A-Z...', llDebug);
  
  for drive := 'A' to 'Z' do
  begin
    driveType := GetDriveType(PChar(drive + ':\\'));
    if driveType in [DRIVE_FIXED, DRIVE_REMOVABLE] then
    begin
      // Skip FIXED drives if checkbox not checked
      if (driveType = DRIVE_FIXED) and (not CheckShowFixed.Checked) then
        Continue;
        
      SetLength(volName, 1024);
      if JGetVolumeNameForVolumeMountPoint(PChar(drive + ':\\'), PChar(volName), Length(volName)) then
      begin
        try
          volName := StrPas(PChar(volName));
          volLink := NativeReadLink('\\??\\' + Copy(volName, 5, Length(volName) - 5));
          sizeInt := DriveTotalBytes(drive);
        except
          on E: Exception do
          begin
            HandleLog('Error reading drive ' + drive + ': ' + E.Message, llDebug);
            sizeInt := 0;
            volLink := '';
            Continue;
          end;
        end;
        
        // Always use GetDiskExtents to get accurate volLink and size
        h := CreateFile(PChar('\\.\' + drive + ':'), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
        if h <> INVALID_HANDLE_VALUE then
        begin
          try
            if GetDiskExtents(h, volLink, off, ln) then
            begin
              if (sizeInt = 0) and (ln > 0) then
                sizeInt := ln;
            end;
          finally
            CloseHandle(h);
          end;
        end;

        entry := drive + '|' + volLink + '|' + IntToStr(sizeInt);
        List.Add(entry);
        HandleLog('Found drive ' + drive + ': -> ' + volLink + ' (' + BytesToHuman(sizeInt) + ')', llDebug);
        Inc(foundCount);
      end;
    end;
  end;
  
  HandleLog('Total drives found: ' + IntToStr(foundCount), llDebug);
end;

// Helper: Map drive letters to physical disk numbers
procedure TMainForm.MapDrivesToPhysicalDisks(List, mapLetters, mapSize: TStringList);
var
  i, j, p1, p2, idx: Integer;
  entry, part2, driveStr, volLink, sizeStr, numStr, devPath: string;
  drive: Char;
  sizeInt, existingSize, off, ln: Int64;
  h: THandle;
begin
  HandleLog('Mapping drives to physical disks...', llDebug);
  
  for j := 0 to List.Count - 1 do
  begin
    entry := List[j];
    p1 := Pos('|', entry);
    if p1 = 0 then
      Continue;
      
    drive := entry[1];
    driveStr := drive;
    part2 := Copy(entry, p1 + 1, Length(entry));
    p2 := Pos('|', part2);
    
    if p2 > 0 then
    begin
      volLink := Copy(part2, 1, p2 - 1);
      sizeStr := Copy(part2, p2 + 1, Length(part2));
      sizeInt := StrToInt64Def(sizeStr, 0);
    end
    else
    begin
      volLink := part2;
      sizeInt := 0;
    end;

    h := CreateFile(PChar('\\.\' + drive + ':'), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    try
      devPath := '';
      off := 0;
      ln := 0;
      
      if (h <> INVALID_HANDLE_VALUE) and GetDiskExtents(h, devPath, off, ln) then
      begin
        if ln > 0 then
          sizeInt := ln;
        HandleLog('Drive ' + drive + ': GetDiskExtents -> ' + devPath + ' (offset: ' + IntToStr(off) + ', length: ' + IntToStr(ln) + ')', llDebug);
        
        p1 := Pos('Harddisk', devPath);
        numStr := '';
        if p1 > 0 then
        begin
          for i := p1 + 8 to Length(devPath) do
          begin
            if devPath[i] in ['0'..'9'] then
              numStr := numStr + devPath[i]
            else
              Break;
          end;
        end;
      end
      else
      begin
        HandleLog('Drive ' + drive + ': GetDiskExtents failed, using fallback from volume link', llDebug);
        p1 := Pos('Harddisk', volLink);
        numStr := '';
        if p1 > 0 then
        begin
          for i := p1 + 8 to Length(volLink) do
          begin
            if volLink[i] in ['0'..'9'] then
              numStr := numStr + volLink[i]
            else
              Break;
          end;
        end;
      end;
    finally
      if h <> INVALID_HANDLE_VALUE then
        CloseHandle(h);
    end;

    if numStr <> '' then
    begin
      idx := mapLetters.IndexOfName(numStr);
      if idx = -1 then
      begin
        mapLetters.Values[numStr] := driveStr;
        mapSize.Values[numStr] := IntToStr(sizeInt);
        HandleLog('Mapped disk #' + numStr + ' -> drive ' + driveStr + ' (' + BytesToHuman(sizeInt) + ')', llDebug);
      end
      else
      begin
        if Pos(driveStr, mapLetters.Values[numStr]) = 0 then
          mapLetters.Values[numStr] := mapLetters.Values[numStr] + ',' + driveStr;
        existingSize := StrToInt64Def(mapSize.Values[numStr], 0);
        if sizeInt > existingSize then
          mapSize.Values[numStr] := IntToStr(sizeInt);
        HandleLog('Updated disk #' + numStr + ' -> added drive ' + driveStr + ', letters: ' + mapLetters.Values[numStr], llDebug);
      end;
    end;
  end;
end;

// Helper: Build ComboDevice items from physical disk mapping
procedure TMainForm.BuildDeviceComboItems(mapLetters, mapSize: TStringList);
var
  num, idx: Integer;
  numStr: string;
  diskSize: Int64;
begin
  HandleLog('Building device list...', llDebug);
  
  for num := 0 to 64 do
  begin
    numStr := IntToStr(num);
    idx := mapLetters.IndexOfName(numStr);
    
    if idx >= 0 then
    begin
      // Try to get size via WMI first (works without admin rights)
      diskSize := GetDiskSizeViaWMI(num);
      
      // If WMI failed, try IOCTL (requires admin rights)
      if diskSize <= 0 then
        diskSize := GetPhysicalDiskSize('\\?\PhysicalDrive' + numStr);
      
      // If both failed, use partition extent size as fallback
      if diskSize <= 0 then
        diskSize := StrToInt64Def(mapSize.Values[numStr], 0);

      ComboDevice.Items.Add('#' + numStr + ' [' + mapLetters.Values[numStr] + ':' + BytesShort(diskSize) + ']');
      HandleLog('Added to list: PhysicalDrive' + numStr + ' [' + mapLetters.Values[numStr] + '] ' + BytesToHuman(diskSize), llDebug);
      
      // Выполнить диагностику здоровья при обнаружении диска
      GetDiskInfoWMI(num);
    end;
  end;
end;

procedure TMainForm.PopulateDevices;
var
  List, Devs, mapLetters, mapSize: TStringList;
begin
  ComboDevice.Items.BeginUpdate;
  try
    ComboDevice.Items.Clear;
    LoadVolume;
    
    List := TStringList.Create;
    try
      // Step 1: Scan drive letters A-Z
      ScanDriveLetters(List);
      
      // Step 2: Map drive letters to physical disk numbers
      Devs := TStringList.Create;
      mapLetters := TStringList.Create;
      mapSize := TStringList.Create;
      try
        NativeDir('\Device', Devs);
        MapDrivesToPhysicalDisks(List, mapLetters, mapSize);
        
        // Step 3: Build ComboDevice items from mapping
        BuildDeviceComboItems(mapLetters, mapSize);
      finally
        Devs.Free;
        mapLetters.Free;
        mapSize.Free;
      end;
    finally
      List.Free;
    end;

    if ComboDevice.Items.Count > 0 then
    begin
      ComboDevice.ItemIndex := 0;
      HandleLog('Total physical disks available: ' + IntToStr(ComboDevice.Items.Count), llInfo);
      ComboDeviceChange(nil); // Trigger auto-calculation of BS
    end
    else
      HandleLog('No physical disks detected!', llWarning);
  finally
    ComboDevice.Items.EndUpdate;
  end;
end;

function TMainForm.BuildWriteConfirmationMessage(const ImageFile, DeviceName: string): string;
begin
  Result := TValidationHelper.BuildWriteConfirmationMessage(
    ImageFile, DeviceName, FLastPartitionCount, FLastPartitionTableType, FLastPartitions);
end;

function TMainForm.BytesToHuman(Bytes: Int64): string;
begin
  Result := TFileOperations.BytesToHuman(Bytes);
end;

function TMainForm.BytesShort(Bytes: Int64): string;
begin
  Result := TFileOperations.BytesShort(Bytes);
end;

function TMainForm.DriveTotalBytes(DriveLetter: Char): Int64;
var
  SectorsPerCluster, BytesPerSector, NumberOfFreeClusters, TotalNumberOfClusters: DWORD;
  dir: string;
begin
  Result := 0;
  dir := DriveLetter + ':\';
  if GetDiskFreeSpace(PChar(dir), SectorsPerCluster, BytesPerSector, NumberOfFreeClusters, TotalNumberOfClusters) then
    Result := Int64(TotalNumberOfClusters) * Int64(SectorsPerCluster) * Int64(BytesPerSector);
end;

procedure TMainForm.UpdateProgress(Progress: Int64);
var
  cnt64, totalBytes: Int64;
  percent: Integer;
  blocks: Int64;
  blockSize: Int64;
  speed: Double;
  eta: Double;
  percentStr, etaStr, statusStr: string;
  ProgMgr: TProgressManager;
  shouldLog: Boolean;
begin
  blockSize := ParseSize(EditBS.Text, 512);
  if blockSize > 0 then
    blocks := Progress div blockSize
  else
    blocks := 0;
  
  // Create progress manager for calculations
  ProgMgr := TProgressManager.Create;
  try
    ProgMgr.Start(FTotalBytes);
    ProgMgr.StartTime := FStartTime; // Use existing start time
    
    // Throttle UI updates to once per 0.5 seconds
    if not ProgMgr.ShouldUpdate and (FLastProgress > 0) then
      Exit;
    
    // Log every 5 seconds for debugging
    shouldLog := FDebugMode and (ProgMgr.ShouldUpdate or (FLastProgress = 0));
    
    if shouldLog then
      HandleLog('UpdateProgress: Progress=' + BytesToHuman(Progress) + ', FTotalBytes=' + BytesToHuman(FTotalBytes) + ', blocks=' + IntToStr(blocks), llDebug);
      
    // Log progress every 10000 blocks
    if (blocks > 0) and ((blocks - FLastLogProgress) >= 10000) then
    begin
      FLastLogProgress := blocks;
      HandleLog('Progress: ' + IntToStr(blocks) + ' blocks (' + BytesToHuman(Progress) + ')', llDebug);
    end;
    
    // Update progress bar
    if EditCount.Text <> '' then
    begin
      try
        cnt64 := StrToInt64(EditCount.Text);
        if cnt64 > 0 then
        begin
          totalBytes := cnt64 * blockSize;
          if shouldLog then
            HandleLog('Progress calculation: cnt64=' + IntToStr(cnt64) + ' blocks, totalBytes=' + BytesToHuman(totalBytes) + ', Progress=' + BytesToHuman(Progress), llDebug);
          
          ProgressBar.MaxValue := 1000;
          percent := ProgMgr.CalculatePercent(blocks, cnt64, ProgressBar.MaxValue);
          ProgressBar.Progress := percent;
          
          // Update Windows 7+ taskbar
          if Assigned(FTaskbarList) and (FTotalBytes > 0) then
            FTaskbarList.SetProgressValue(Application.Handle, Progress, FTotalBytes);
          
          // Calculate speed and ETA
          percentStr := IntToStr((percent * 100) div 1000) + '%';
          if ProgMgr.CalculateSpeed(Progress, speed) and (speed > 0) and (Progress < totalBytes) then
          begin
            eta := ProgMgr.CalculateETA(Progress, speed);
            etaStr := ProgMgr.FormatETA(eta);
            statusStr := ProgMgr.FormatSpeed(speed);
            
            // Проверка скорости - предупреждение при низкой скорости
            if (speed > 0) and (speed < FSpeedWarningThreshold) and (Progress > 1048576) then // После 1 МБ
            begin
              if (blocks mod 10000) = 0 then // Каждые 10000 блоков
                HandleLog(Format('WARNING: Low write speed detected: %.2f MB/s (expected > %.1f MB/s)',
                  [speed, FSpeedWarningThreshold]), llWarning);
            end;
          end
          else
          begin
            etaStr := '';
            statusStr := '';
          end;

          StatusBar.Panels[0].Text := percentStr;
          StatusBar.Panels[1].Text := etaStr;
          StatusBar.Panels[2].Text := statusStr;
        end
        else
          ProgressBar.Progress := Integer(blocks mod 1000);
      except
        ProgressBar.Progress := Integer(blocks mod 1000);
      end;
    end
    else
      ProgressBar.Progress := Integer(blocks mod 1000);
  finally
    ProgMgr.Free;
  end;
end;

// Helper: Verify hash after write operation
procedure TMainForm.VerifyHashAfterWrite;
var
  Hash, DeviceHash: string;
  ImageSize: Int64;
begin
  HandleLog('Calculating hash of source file...', llInfo);
  Hash := CalculateFileMD5(EditIn.Text);

  if Hash <> '' then
  begin
    HandleLog('Source file MD5: ' + Hash, llInfo);
    
    // Calculate hash of written device data
    ImageSize := GetFileSize64(EditIn.Text);
    HandleLog('Calculating hash of written device data...', llInfo);
    DeviceHash := CalculateDeviceMD5(FDevicePath, ImageSize);

    if DeviceHash <> '' then
    begin
      HandleLog('Device data MD5: ' + DeviceHash, llInfo);

      if Hash = DeviceHash then
        HandleLog('Hash verification PASSED - data written correctly', llInfo)
      else
      begin
        HandleLog('Hash verification FAILED - data mismatch!', llError);
        FOperationSuccess := False;
      end;
    end
    else
      HandleLog('Failed to calculate device hash', llWarning);
  end
  else
    HandleLog('Failed to calculate source file hash', llWarning);
end;

// Helper: Verify hash after read operation
procedure TMainForm.VerifyHashAfterRead;
var
  Hash: string;
begin
  HandleLog('Calculating hash of output file...', llInfo);
  Hash := CalculateFileMD5(EditIn.Text);

  if Hash <> '' then
  begin
    HandleLog('Output file MD5: ' + Hash, llInfo);
    SaveHashToFile(EditIn.Text, Hash);
  end
  else
    HandleLog('Failed to calculate output file hash', llWarning);
end;

// Helper: Finalize operation (UI updates)
procedure TMainForm.FinalizeOperation;
begin
  // Complete taskbar progress
  if Assigned(FTaskbarList) then
  begin
    if FOperationSuccess then
      FTaskbarList.SetProgressState(Application.Handle, TBPF_NOPROGRESS)
    else
      FTaskbarList.SetProgressState(Application.Handle, TBPF_ERROR);
    FlashWindow(Application.Handle, True);
  end;

  FTotalBytes := 0;
  
  // Display operation result
  if FOperationSuccess then
  begin
    AddFormattedLogMessage('=== Operation completed successfully ===', clGreen);
    // Play success sound
    MessageBeep(MB_OK);
    
    // Auto-eject if enabled and write mode
    if FIsWriteMode and CheckAutoEject.Checked and (FDevicePath <> '') then
    begin
      HandleLog('Auto-eject enabled, attempting to eject device...', llInfo);
      Sleep(500); // Small delay to ensure write completion
      EjectDevice(FDevicePath);
    end;
  end
  else
  begin
    AddFormattedLogMessage('=== Operation failed ===', clRed);
    // Play error sound
    MessageBeep(MB_ICONERROR);
  end;

  StatusBar.Panels[0].Text := '';
  StatusBar.Panels[1].Text := '';
  StatusBar.Panels[2].Text := '';
  StatusBar.Panels[3].Text := 'Idle';
  
  // Restore normal priority if quiet mode was active
  if FQuietModeActive then
    SetQuietMode(False);
end;

procedure TMainForm.ThreadCompleted(Sender: TObject);
begin
  // Restore volume after write
  if FIsWriteMode and (FDevicePath <> '') then
    RestoreVolume(FDevicePath);
  
  // Hash verification after operation
  if FOperationSuccess and CheckVerifyHash.Checked then
  begin
    if FIsWriteMode then
      VerifyHashAfterWrite
    else
      VerifyHashAfterRead;
  end;
  
  // Finalize operation (UI updates, taskbar, etc.)
  FinalizeOperation;
  
  // Reset thread pointer at the very end, before updating button
  FThread := nil;
  UpdateButtonCaption;
end;

procedure TMainForm.EjectDevice(const DevicePath: string);
var
  DiskNumber: Integer;
  DeviceHandle: THandle;
  BytesReturned: DWORD;
begin
  HandleLog('Attempting to eject device: ' + DevicePath, llInfo);
  
  try
    // Extract disk number from path
    DiskNumber := ExtractDiskNumberFromPath(DevicePath);
    if DiskNumber < 0 then
    begin
      HandleLog('Cannot extract disk number from path', llWarning);
      Exit;
    end;
    
    // Open device handle
    DeviceHandle := CreateFile(
      PChar(DevicePath),
      GENERIC_READ or GENERIC_WRITE,
      FILE_SHARE_READ or FILE_SHARE_WRITE,
      nil,
      OPEN_EXISTING,
      0,
      0);
    
    if DeviceHandle = INVALID_HANDLE_VALUE then
    begin
      HandleLog('Cannot open device for eject: ' + SysErrorMessage(GetLastError), llWarning);
      Exit;
    end;
    
    try
      // Try to eject media
      if not DeviceIoControl(
        DeviceHandle,
        IOCTL_STORAGE_EJECT_MEDIA,
        nil, 0,
        nil, 0,
        BytesReturned,
        nil) then
      begin
        HandleLog('Eject failed: ' + SysErrorMessage(GetLastError), llWarning);
      end
      else
      begin
        HandleLog('Device ejected successfully', llInfo);
        MessageBeep(MB_ICONINFORMATION);
      end;
    finally
      CloseHandle(DeviceHandle);
    end;
  except
    on E: Exception do
      HandleLog('Exception during eject: ' + E.Message, llError);
  end;
end;

procedure TMainForm.UpdateButtonCaption;
begin
  // Safety check for critical UI components
  if not Assigned(RadioGroupMode) or not Assigned(ButtonStart) then
    Exit;
    
  if Assigned(FThread) or FOperationInProgress then
  begin
    ButtonStart.Caption := 'Cancel';
    ButtonStart.Hint := 'Cancel current operation (will stop safely)';
    if Assigned(ButtonIcons) and (ButtonIcons.Count >= 2) then
    begin
      ButtonStart.Glyph := nil;
      ButtonIcons.GetBitmap(0, ButtonStart.Glyph);
    end;
    if Assigned(ButtonQuietMode) then
      ButtonQuietMode.Enabled := True; // Show Quiet Mode button during operation
  end
  else
  begin
    case RadioGroupMode.ItemIndex of
      0: begin  // Write
        ButtonStart.Caption := 'Write';
        ButtonStart.Hint := 'Write image file to selected device';
      end;
      1: begin  // Read
        ButtonStart.Caption := 'Read';
        ButtonStart.Hint := 'Read from selected device to image file';
      end;
      2: begin  // Verify
        ButtonStart.Caption := 'Verify';
        ButtonStart.Hint := 'Verify device contents match image file';
      end;
    end;
    if Assigned(ButtonIcons) and (ButtonIcons.Count >= 2) then
    begin
      ButtonStart.Glyph := nil;
      ButtonIcons.GetBitmap(1, ButtonStart.Glyph);
    end;
    if Assigned(ButtonQuietMode) then
      ButtonQuietMode.Enabled := False; // Hide Quiet Mode button when idle
  end;
end;

// Helper: Handle cancel request for ongoing operations
function TMainForm.HandleCancelRequest: Boolean;
begin
  Result := False;
  
  // If Cancel pressed stop thread or synchronous operation
  if Assigned(FThread) then
  begin
    HandleLog('Cancelling operation...', llWarning);
    
    // If using worker IPC thread, send cancel message
    if FThread is TWorkerIPCThread then
      TWorkerIPCThread(FThread).SendCancel
    else
      FThread.Terminate;
    Result := True;
    Exit;
  end;
  
  // If synchronous operation (archive extraction) is in progress, set cancel flag
  if FOperationInProgress then
  begin
    HandleLog('Cancel requested for synchronous operation', llWarning);
    FCancelRequested := True;
    Result := True;
  end;
end;

// Helper: Initialize UI for operation start
procedure TMainForm.InitializeOperationUI(isWriteMode: Boolean);
begin
  FStartTime := Now;
  FLastProgress := 0;
  FLastUpdateTime := Now;
  StatusBar.Panels[0].Text := '0%';
  StatusBar.Panels[1].Text := '';
  StatusBar.Panels[2].Text := '';

  if Assigned(FTaskbarList) then
    FTaskbarList.SetProgressState(Application.Handle, TBPF_NORMAL);

  ProgressBar.Progress := 0;
  
  if isWriteMode then
  begin
    StatusBar.Panels[3].Text := 'Writing...';
    HandleLog('DEBUG: StatusBar.Panels[3] set to: Writing...', llDebug);
  end
  else
  begin
    StatusBar.Panels[3].Text := 'Reading...';
    HandleLog('DEBUG: StatusBar.Panels[3] set to: Reading...', llDebug);
  end;
  HandleLog('DEBUG: Current Panels[3].Text = ' + StatusBar.Panels[3].Text, llDebug);
end;

// Helper: Launch elevated worker process
function TMainForm.LaunchElevatedWorker(isWriteMode: Boolean; const devicePath: string): Boolean;
var
  WorkerParams: TWorkerParams;
  PipeName, CmdLine: string;
  sei: TShellExecuteInfo;
  blockSize: Int64;
begin
  Result := False;
  
  HandleLog('Elevated privileges required for disk access', llInfo);
  HandleLog('Launching worker process...', llInfo);
  
  // Prepare worker parameters
  FillChar(WorkerParams, SizeOf(WorkerParams), 0);

  if isWriteMode then
    WorkerParams.Operation := woWrite
  else
    WorkerParams.Operation := woRead;

  WorkerParams.DevicePath := devicePath;
  WorkerParams.FilePath := EditIn.Text;
  blockSize := ParseSize(EditBS.Text, 512);
  WorkerParams.BlockSize := blockSize;
  WorkerParams.Count := StrToInt64Def(Trim(EditCount.Text), 0);
  WorkerParams.Seek := StrToInt64Def(Trim(EditSeek.Text), 0);
  WorkerParams.Skip := StrToInt64Def(Trim(EditSkip.Text), 0);
  WorkerParams.CheckSize := False;
  WorkerParams.VerifyHash := CheckVerifyHash.Checked;
  
  // Create unique pipe name for IPC
  PipeName := CreatePipeName('ImageWriter');
  WorkerParams.PipeName := PipeName;
  
  // Build command line
  CmdLine := BuildWorkerCommandLine(WorkerParams);
  
  // Launch worker process with elevation
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.Wnd := Handle;
  sei.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI or SEE_MASK_NOCLOSEPROCESS;
  sei.lpVerb := 'runas';
  sei.lpFile := PChar(ParamStr(0));
  sei.lpParameters := PChar(CmdLine);
  sei.nShow := SW_HIDE;

  if not ShellExecuteEx(@sei) then
  begin
    HandleLog('Failed to launch worker process: ' + SysErrorMessage(GetLastError), llError);
    Exit;
  end;

  if sei.hProcess = 0 then
  begin
    HandleLog('Worker process handle is invalid', llError);
    Exit;
  end;

  FWorkerProcess := sei.hProcess;
  HandleLog('Worker process launched successfully', llInfo);
  
  // Initialize UI for operation
  InitializeOperationUI(isWriteMode);
  
  // Create IPC thread to communicate with worker
  FThread := TWorkerIPCThread.Create(Self, PipeName, FWorkerProcess, isWriteMode, EditIn.Text, blockSize);
  FWorkerProcess := 0;

  UpdateButtonCaption;
  Result := True;
end;

// Helper: Validate write operation parameters
function TMainForm.ValidateWriteOperation(const inFile, devicePath: string; blockSize: Int64): Boolean;
begin
  Result := False;
  
  // Check for special devices or regular files
  if not IsSpecialDevice(inFile) then
  begin
    if (inFile = '') or (not FileExists(inFile)) then
    begin
      HandleLog('Input file not found: ' + inFile, llError);
      Exit;
    end;
  end
  else
  begin
    HandleLog('Using special device: ' + inFile, llInfo);
  end;

  if devicePath = '' then
  begin
    HandleLog('No device selected', llError);
    Exit;
  end;
  
  if blockSize <= 0 then
  begin
    HandleLog('Invalid block size', llError);
    Exit;
  end;
  
  Result := True;
end;

procedure TMainForm.FinalizeArchiveOperation(const devicePath: string);
begin
  // devicePath is empty for read operations
  if devicePath <> '' then
    RestoreVolume(devicePath);
    
  FOperationInProgress := False;
  
  // Write operations use UpdateButtonCaption, read operations use ShowOperationResult
  if devicePath <> '' then
    UpdateButtonCaption
  else
    ShowOperationResult;
end;

procedure TMainForm.HandleZipWrite(const inFile, devicePath: string; blockSize: Int64);
var
  fileStream: TFileStream;
  zipStream: TZipInputStream;
  diskSize, count: Int64;
begin
  // Show confirmation dialog for archive write
  if MessageDlg(BuildWriteConfirmationMessage(inFile, ComboDevice.Text), 
                mtWarning, [mbYes, mbNo], 0) <> mrYes then
  begin
    HandleLog('Archive write cancelled by user', llWarning);
    Exit;
  end;
  
  HandleLog('Using streaming ZIP decompression...', llInfo);
  FStartTime := Now;
  FLastProgress := 0;
  FLastUpdateTime := Now;
  
  TOperationUIHelper.InitializeOperationUI(StatusBar, ProgressBar, 'Extracting & Writing...', 0, FTaskbarList);
  FCancelRequested := False;
  FOperationInProgress := True;
  UpdateButtonCaption;
  
  // Prepare volume for write (lock and dismount)
  PrepareVolumeForWrite(devicePath);

  try
    if not EnsureNetworkShareAccess(inFile) then
    begin
      HandleLog('Network folder access error: ' + inFile, llError);
      FOperationSuccess := False;
      Exit;
    end;
    fileStream := TFileStream.Create(inFile, fmOpenRead or fmShareDenyWrite);
    try
      zipStream := TZipInputStream.Create(fileStream);
      try
        diskSize := (zipStream as TZipInputStream).UncompressedSize;
        if diskSize > 0 then
        begin
          HandleLog('Uncompressed size: ' + BytesToHuman(diskSize), llInfo);
          FTotalBytes := diskSize;
          count := diskSize div blockSize;
          if (diskSize mod blockSize) <> 0 then
            Inc(count);
          EditCount.Text := IntToStr(count);
          HandleLog('Auto-calculated block count: ' + IntToStr(count) + ' blocks (based on archive size)', llDebug);
        end;
        if not StreamingCopyToDevice(zipStream, devicePath, blockSize, diskSize) then
        begin
          HandleLog('Streaming copy failed', llError);
          FOperationSuccess := False;
        end
        else if FCancelRequested then
        begin
          HandleLog('=== ZIP decompression cancelled ===', llWarning);
          FOperationSuccess := False;
        end
        else
        begin
          HandleLog('=== ZIP decompression completed successfully ===', llInfo);
          FOperationSuccess := True;
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
      HandleLog('' + E.Message, llError);
      FOperationSuccess := False;
    end;
  end;
  
  FinalizeArchiveOperation(devicePath);
end;

procedure TMainForm.HandleGZipWrite(const inFile, devicePath: string; blockSize: Int64);
var
  fileStream: TFileStream;
  gzipStream: TGZipInputStream;
  diskSize, deviceSize, count: Int64;
begin
  // Show confirmation dialog for archive write
  if MessageDlg(BuildWriteConfirmationMessage(inFile, ComboDevice.Text), 
                mtWarning, [mbYes, mbNo], 0) <> mrYes then
  begin
    HandleLog('Archive write cancelled by user', llWarning);
    Exit;
  end;
  
  HandleLog('Using streaming GZIP decompression...', llInfo);
  FStartTime := Now;
  FLastProgress := 0;
  FLastUpdateTime := Now;
  
  TOperationUIHelper.InitializeOperationUI(StatusBar, ProgressBar, 'Extracting & Writing...', 0, FTaskbarList);
  FCancelRequested := False;
  FOperationInProgress := True;
  UpdateButtonCaption;
  
  // Prepare volume for write (lock and dismount)
  PrepareVolumeForWrite(devicePath);

  try
    if not EnsureNetworkShareAccess(inFile) then
    begin
      HandleLog('Network folder access error: ' + inFile, llError);
      FOperationSuccess := False;
      Exit;
    end;
    fileStream := TFileStream.Create(inFile, fmOpenRead or fmShareDenyWrite);
    try
      gzipStream := TGZipInputStream.Create(fileStream);
      try
        diskSize := fileStream.Size * 3;
        HandleLog('Estimated uncompressed size (GZIP): ' + BytesToHuman(diskSize) + ' (based on 3:1 ratio)', llInfo);
        
        deviceSize := GetPhysicalDiskSize(devicePath);
        if (deviceSize > 0) and (diskSize > deviceSize) then
        begin
          diskSize := deviceSize;
          HandleLog('Using device size instead: ' + BytesToHuman(diskSize), llInfo);
        end;
        
        FTotalBytes := diskSize;
        if diskSize > 0 then
        begin
          count := diskSize div blockSize;
          if (diskSize mod blockSize) <> 0 then
            Inc(count);
          EditCount.Text := IntToStr(count);
          HandleLog('Auto-calculated block count: ' + IntToStr(count) + ' blocks (estimated)', llDebug);
        end;
        if not StreamingCopyToDevice(gzipStream, devicePath, blockSize, diskSize) then
        begin
          HandleLog('Streaming copy failed', llError);
          FOperationSuccess := False;
        end
        else if FCancelRequested then
        begin
          HandleLog('=== GZIP decompression cancelled ===', llWarning);
          FOperationSuccess := False;
        end
        else
        begin
          HandleLog('=== GZIP decompression completed successfully ===', llInfo);
          FOperationSuccess := True;
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
      HandleLog('' + E.Message, llError);
      FOperationSuccess := False;
    end;
  end;
  
  FinalizeArchiveOperation(devicePath);
end;

procedure TMainForm.HandleXZWrite(const inFile, devicePath: string; blockSize: Int64);
var
  fileStream: TFileStream;
  xzStream: TXZInputStream;
  diskSize, deviceSize, count: Int64;
begin
  // Show confirmation dialog for archive write
  if MessageDlg(BuildWriteConfirmationMessage(inFile, ComboDevice.Text), 
                mtWarning, [mbYes, mbNo], 0) <> mrYes then
  begin
    HandleLog('Archive write cancelled by user', llWarning);
    Exit;
  end;
  
  HandleLog('Using streaming XZ decompression...', llInfo);
  FStartTime := Now;
  FLastProgress := 0;
  FLastUpdateTime := Now;
  
  TOperationUIHelper.InitializeOperationUI(StatusBar, ProgressBar, 'Extracting & Writing...', 0, FTaskbarList);
  FCancelRequested := False;
  FOperationInProgress := True;
  UpdateButtonCaption;
  
  // Prepare volume for write (lock and dismount)
  PrepareVolumeForWrite(devicePath);

  try
    if not EnsureNetworkShareAccess(inFile) then
    begin
      HandleLog('Network folder access error: ' + inFile, llError);
      FOperationSuccess := False;
      Exit;
    end;
    fileStream := TFileStream.Create(inFile, fmOpenRead or fmShareDenyWrite);
    try
      xzStream := TXZInputStream.Create(fileStream);
      try
        // XZ typically has better compression ratio (4:1 or better)
        diskSize := fileStream.Size * 4;
        HandleLog('Estimated uncompressed size (XZ): ' + BytesToHuman(diskSize) + ' (based on 4:1 ratio)', llInfo);
        
        deviceSize := GetPhysicalDiskSize(devicePath);
        if (deviceSize > 0) and (diskSize > deviceSize) then
        begin
          diskSize := deviceSize;
          HandleLog('Using device size instead: ' + BytesToHuman(diskSize), llInfo);
        end;
        
        FTotalBytes := diskSize;
        if diskSize > 0 then
        begin
          count := diskSize div blockSize;
          if (diskSize mod blockSize) <> 0 then
            Inc(count);
          EditCount.Text := IntToStr(count);
          HandleLog('Auto-calculated block count: ' + IntToStr(count) + ' blocks (estimated)', llDebug);
        end;
        if not StreamingCopyToDevice(xzStream, devicePath, blockSize, diskSize) then
        begin
          HandleLog('Streaming copy failed', llError);
          FOperationSuccess := False;
        end
        else if FCancelRequested then
        begin
          HandleLog('=== XZ decompression cancelled ===', llWarning);
          FOperationSuccess := False;
        end
        else
        begin
          HandleLog('=== XZ decompression completed successfully ===', llInfo);
          FOperationSuccess := True;
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
      HandleLog('' + E.Message, llError);
      FOperationSuccess := False;
    end;
  end;
  
  FinalizeArchiveOperation(devicePath);
end;

procedure TMainForm.HandleBZip2Write(const inFile, devicePath: string; blockSize: Int64);
var
  fileStream: TFileStream;
  bzip2Stream: TBZip2InputStream;
  diskSize, deviceSize, count: Int64;
begin
  // Show confirmation dialog for archive write
  if MessageDlg(BuildWriteConfirmationMessage(inFile, ComboDevice.Text), 
                mtWarning, [mbYes, mbNo], 0) <> mrYes then
  begin
    HandleLog('Archive write cancelled by user', llWarning);
    Exit;
  end;
  
  HandleLog('Using streaming BZIP2 decompression...', llInfo);
  FStartTime := Now;
  FLastProgress := 0;
  FLastUpdateTime := Now;
  
  TOperationUIHelper.InitializeOperationUI(StatusBar, ProgressBar, 'Extracting & Writing...', 0, FTaskbarList);
  FCancelRequested := False;
  FOperationInProgress := True;
  UpdateButtonCaption;
  
  // Prepare volume for write (lock and dismount)
  PrepareVolumeForWrite(devicePath);

  try
    if not EnsureNetworkShareAccess(inFile) then
    begin
      HandleLog('Network folder access error: ' + inFile, llError);
      FOperationSuccess := False;
      Exit;
    end;
    fileStream := TFileStream.Create(inFile, fmOpenRead or fmShareDenyWrite);
    try
      bzip2Stream := TBZip2InputStream.Create(fileStream);
      try
        // BZIP2 typically has ~3:1 compression ratio
        diskSize := fileStream.Size * 3;
        HandleLog('Estimated uncompressed size (BZIP2): ' + BytesToHuman(diskSize) + ' (based on 3:1 ratio)', llInfo);
        
        deviceSize := GetPhysicalDiskSize(devicePath);
        if (deviceSize > 0) and (diskSize > deviceSize) then
        begin
          diskSize := deviceSize;
          HandleLog('Using device size instead: ' + BytesToHuman(diskSize), llInfo);
        end;
        
        FTotalBytes := diskSize;
        if diskSize > 0 then
        begin
          count := diskSize div blockSize;
          if (diskSize mod blockSize) <> 0 then
            Inc(count);
          EditCount.Text := IntToStr(count);
          HandleLog('Auto-calculated block count: ' + IntToStr(count) + ' blocks (estimated)', llDebug);
        end;
        if not StreamingCopyToDevice(bzip2Stream, devicePath, blockSize, diskSize) then
        begin
          HandleLog('Streaming copy failed', llError);
          FOperationSuccess := False;
        end
        else if FCancelRequested then
        begin
          HandleLog('=== BZIP2 decompression cancelled ===', llWarning);
          FOperationSuccess := False;
        end
        else
        begin
          HandleLog('=== BZIP2 decompression completed successfully ===', llInfo);
          FOperationSuccess := True;
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
      HandleLog('' + E.Message, llError);
      FOperationSuccess := False;
    end;
  end;
  
  FinalizeArchiveOperation(devicePath);
end;

procedure TMainForm.HandleTarGzWrite(const inFile, devicePath: string; blockSize: Int64);
var
  fileStream: TFileStream;
  gzipStream: TGZipInputStream;
  tarStream: TTarInputStream;
  diskSize, deviceSize, count: Int64;
begin
  // Show confirmation dialog for archive write
  if MessageDlg(BuildWriteConfirmationMessage(inFile, ComboDevice.Text), 
                mtWarning, [mbYes, mbNo], 0) <> mrYes then
  begin
    HandleLog('Archive write cancelled by user', llWarning);
    Exit;
  end;
  
  HandleLog('Using streaming tar.gz decompression...', llInfo);
  FStartTime := Now;
  FLastProgress := 0;
  FLastUpdateTime := Now;
  
  TOperationUIHelper.InitializeOperationUI(StatusBar, ProgressBar, 'Extracting & Writing...', 0, FTaskbarList);
  FCancelRequested := False;
  FOperationInProgress := True;
  UpdateButtonCaption;
  
  // Prepare volume for write (lock and dismount)
  PrepareVolumeForWrite(devicePath);

  try
    if not EnsureNetworkShareAccess(inFile) then
    begin
      HandleLog('Network folder access error: ' + inFile, llError);
      FOperationSuccess := False;
      Exit;
    end;
    fileStream := TFileStream.Create(inFile, fmOpenRead or fmShareDenyWrite);
    try
      gzipStream := TGZipInputStream.Create(fileStream);
      try
        tarStream := TTarInputStream.Create(gzipStream);
        try
          // Get actual size from TAR header
          diskSize := tarStream.FileSize;
          HandleLog('TAR file size: ' + BytesToHuman(diskSize), llInfo);
          
          deviceSize := GetPhysicalDiskSize(devicePath);
          if (deviceSize > 0) and (diskSize > deviceSize) then
          begin
            diskSize := deviceSize;
            HandleLog('Using device size instead: ' + BytesToHuman(diskSize), llInfo);
          end;
          
          FTotalBytes := diskSize;
          if diskSize > 0 then
          begin
            count := diskSize div blockSize;
            if (diskSize mod blockSize) <> 0 then
              Inc(count);
            EditCount.Text := IntToStr(count);
            HandleLog('Auto-calculated block count: ' + IntToStr(count) + ' blocks', llDebug);
          end;
          if not StreamingCopyToDevice(tarStream, devicePath, blockSize, diskSize) then
          begin
            HandleLog('Streaming copy failed', llError);
            FOperationSuccess := False;
          end
          else if FCancelRequested then
          begin
            HandleLog('=== tar.gz decompression cancelled ===', llWarning);
            FOperationSuccess := False;
          end
          else
          begin
            HandleLog('=== tar.gz decompression completed successfully ===', llInfo);
            FOperationSuccess := True;
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
      HandleLog('' + E.Message, llError);
      FOperationSuccess := False;
    end;
  end;
  
  FinalizeArchiveOperation(devicePath);
end;

procedure TMainForm.HandleTarXzWrite(const inFile, devicePath: string; blockSize: Int64);
var
  fileStream: TFileStream;
  xzStream: TXZInputStream;
  tarStream: TTarInputStream;
  diskSize, deviceSize, count: Int64;
begin
  // Show confirmation dialog for archive write
  if MessageDlg(BuildWriteConfirmationMessage(inFile, ComboDevice.Text), 
                mtWarning, [mbYes, mbNo], 0) <> mrYes then
  begin
    HandleLog('Archive write cancelled by user', llWarning);
    Exit;
  end;
  
  HandleLog('Using streaming tar.xz decompression...', llInfo);
  FStartTime := Now;
  FLastProgress := 0;
  FLastUpdateTime := Now;
  
  TOperationUIHelper.InitializeOperationUI(StatusBar, ProgressBar, 'Extracting & Writing...', 0, FTaskbarList);
  FCancelRequested := False;
  FOperationInProgress := True;
  UpdateButtonCaption;
  
  // Prepare volume for write (lock and dismount)
  PrepareVolumeForWrite(devicePath);

  try
    if not EnsureNetworkShareAccess(inFile) then
    begin
      HandleLog('Network folder access error: ' + inFile, llError);
      FOperationSuccess := False;
      Exit;
    end;
    fileStream := TFileStream.Create(inFile, fmOpenRead or fmShareDenyWrite);
    try
      xzStream := TXZInputStream.Create(fileStream);
      try
        tarStream := TTarInputStream.Create(xzStream);
        try
          // Get actual size from TAR header
          diskSize := tarStream.FileSize;
          HandleLog('TAR file size: ' + BytesToHuman(diskSize), llInfo);
          
          deviceSize := GetPhysicalDiskSize(devicePath);
          if (deviceSize > 0) and (diskSize > deviceSize) then
          begin
            diskSize := deviceSize;
            HandleLog('Using device size instead: ' + BytesToHuman(diskSize), llInfo);
          end;
          
          FTotalBytes := diskSize;
          if diskSize > 0 then
          begin
            count := diskSize div blockSize;
            if (diskSize mod blockSize) <> 0 then
              Inc(count);
            EditCount.Text := IntToStr(count);
            HandleLog('Auto-calculated block count: ' + IntToStr(count) + ' blocks', llDebug);
          end;
          if not StreamingCopyToDevice(tarStream, devicePath, blockSize, diskSize) then
          begin
            HandleLog('Streaming copy failed', llError);
            FOperationSuccess := False;
          end
          else if FCancelRequested then
          begin
            HandleLog('=== tar.xz decompression cancelled ===', llWarning);
            FOperationSuccess := False;
          end
          else
          begin
            HandleLog('=== tar.xz decompression completed successfully ===', llInfo);
            FOperationSuccess := True;
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
      HandleLog('' + E.Message, llError);
      FOperationSuccess := False;
    end;
  end;
  
  FinalizeArchiveOperation(devicePath);
end;

procedure TMainForm.Handle7ZipWrite(const inFile, devicePath: string; blockSize: Int64);
var
  sevenZipStream: TSevenZipInputStream;
  diskSize, deviceSize, count: Int64;
begin
  // Show confirmation dialog for archive write
  if MessageDlg(BuildWriteConfirmationMessage(inFile, ComboDevice.Text), 
                mtWarning, [mbYes, mbNo], 0) <> mrYes then
  begin
    HandleLog('Archive write cancelled by user', llWarning);
    Exit;
  end;
  
  HandleLog('Using 7z decompression...', llInfo);
  FStartTime := Now;
  FLastProgress := 0;
  FLastUpdateTime := Now;
  
  TOperationUIHelper.InitializeOperationUI(StatusBar, ProgressBar, 'Extracting & Writing...', 0, FTaskbarList);
  FCancelRequested := False;
  FOperationInProgress := True;
  UpdateButtonCaption;
  
  // Prepare volume for write (lock and dismount)
  PrepareVolumeForWrite(devicePath);

  try
    if not EnsureNetworkShareAccess(inFile) then
    begin
      HandleLog('Network folder access error: ' + inFile, llError);
      FOperationSuccess := False;
      Exit;
    end;
    sevenZipStream := TSevenZipInputStream.Create(inFile);
    try
      // Get actual size
      diskSize := sevenZipStream.Size;
      HandleLog('7z file size: ' + BytesToHuman(diskSize), llInfo);
      
      deviceSize := GetPhysicalDiskSize(devicePath);
      if (deviceSize > 0) and (diskSize > deviceSize) then
      begin
        diskSize := deviceSize;
        HandleLog('Using device size instead: ' + BytesToHuman(diskSize), llInfo);
      end;
      
      FTotalBytes := diskSize;
      if diskSize > 0 then
      begin
        count := diskSize div blockSize;
        if (diskSize mod blockSize) <> 0 then
          Inc(count);
        EditCount.Text := IntToStr(count);
        HandleLog('Auto-calculated block count: ' + IntToStr(count) + ' blocks', llDebug);
      end;
      if not StreamingCopyToDevice(sevenZipStream, devicePath, blockSize, diskSize) then
      begin
        HandleLog('Streaming copy failed', llError);
        FOperationSuccess := False;
      end
      else if FCancelRequested then
      begin
        HandleLog('=== 7z decompression cancelled ===', llWarning);
        FOperationSuccess := False;
      end
      else
      begin
        HandleLog('=== 7z decompression completed successfully ===', llInfo);
        FOperationSuccess := True;
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
      HandleLog('' + E.Message, llError);
      FOperationSuccess := False;
    end;
  end;
  
  FinalizeArchiveOperation(devicePath);
end;

procedure TMainForm.HandleZipRead(const devicePath, outFile: string; blockSize, diskSize: Int64);
var
  fileStream: TFileStream;
  zipStream: TZipOutputStream;
begin
  HandleLog('Using streaming ZIP compression...', llInfo);
  FStartTime := Now;
  FLastProgress := 0;
  FLastUpdateTime := Now;
  FTotalBytes := diskSize;
  
  TOperationUIHelper.InitializeOperationUI(StatusBar, ProgressBar, 'Starting...', diskSize, FTaskbarList);
  FCancelRequested := False;
  FOperationInProgress := True;
  UpdateButtonCaption;

  try
    fileStream := TFileStream.Create(outFile, fmCreate);
    try
      zipStream := TZipOutputStream.Create(fileStream, ChangeFileExt(ExtractFileName(outFile), '.img'));
      try
        if not StreamingCopyFromDevice(devicePath, zipStream, blockSize, diskSize) then
        begin
          HandleLog('Streaming copy failed', llError);
          FOperationSuccess := False;
        end
        else if FCancelRequested then
        begin
          HandleLog('=== ZIP compression cancelled ===', llWarning);
          FOperationSuccess := False;
        end
        else
        begin
          HandleLog('=== ZIP compression completed successfully ===', llInfo);
          FOperationSuccess := True;
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
      HandleLog('' + E.Message, llError);
      FOperationSuccess := False;
    end;
  end;
  
  FinalizeArchiveOperation(devicePath);
end;

procedure TMainForm.HandleGZipRead(const devicePath, outFile: string; blockSize, diskSize: Int64);
var
  fileStream: TFileStream;
  gzipStream: TGZipOutputStream;
begin
  HandleLog('Using streaming GZIP compression...', llInfo);
  FStartTime := Now;
  FLastProgress := 0;
  FLastUpdateTime := Now;
  FTotalBytes := diskSize;
  
  TOperationUIHelper.InitializeOperationUI(StatusBar, ProgressBar, 'Starting...', diskSize, FTaskbarList);
  FCancelRequested := False;
  FOperationInProgress := True;
  UpdateButtonCaption;

  try
    fileStream := TFileStream.Create(outFile, fmCreate);
    try
      gzipStream := TGZipOutputStream.Create(fileStream);
      try
        if not StreamingCopyFromDevice(devicePath, gzipStream, blockSize, diskSize) then
        begin
          HandleLog('Streaming copy failed', llError);
          FOperationSuccess := False;
        end
        else if FCancelRequested then
        begin
          HandleLog('=== GZIP compression cancelled ===', llWarning);
          FOperationSuccess := False;
        end
        else
        begin
          HandleLog('=== GZIP compression completed successfully ===', llInfo);
          FOperationSuccess := True;
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
      HandleLog('' + E.Message, llError);
      FOperationSuccess := False;
    end;
  end;
  
  FinalizeArchiveOperation(devicePath);
end;

procedure TMainForm.HandleXZRead(const devicePath, outFile: string; blockSize, diskSize: Int64);
var
  xzStream: TSevenZipOutputStream;
begin
  HandleLog('Using 7z.exe for XZ compression (stdin pipe)...', llInfo);
  FStartTime := Now;
  FLastProgress := 0;
  FLastUpdateTime := Now;
  FTotalBytes := diskSize;
  
  TOperationUIHelper.InitializeOperationUI(StatusBar, ProgressBar, 'Starting...', diskSize, FTaskbarList);
  FCancelRequested := False;
  FOperationInProgress := True;
  UpdateButtonCaption;

  try
    xzStream := TSevenZipOutputStream.Create(outFile, 'txz');
    try
      if not StreamingCopyFromDevice(devicePath, xzStream, blockSize, diskSize) then
      begin
        HandleLog('Streaming copy failed', llError);
        FOperationSuccess := False;
      end
      else if FCancelRequested then
      begin
        HandleLog('=== XZ compression cancelled ===', llWarning);
        FOperationSuccess := False;
      end
      else
      begin
        HandleLog('=== XZ compression completed successfully ===', llInfo);
        FOperationSuccess := True;
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
      HandleLog('' + E.Message, llError);
      FOperationSuccess := False;
    end;
  end;
  
  FinalizeArchiveOperation(devicePath);
end;

procedure TMainForm.HandleBZip2Read(const devicePath, outFile: string; blockSize, diskSize: Int64);
var
  bz2Stream: TSevenZipOutputStream;
begin
  HandleLog('Using 7z.exe for BZIP2 compression (stdin pipe)...', llInfo);
  FStartTime := Now;
  FLastProgress := 0;
  FLastUpdateTime := Now;
  FTotalBytes := diskSize;
  
  TOperationUIHelper.InitializeOperationUI(StatusBar, ProgressBar, 'Starting...', diskSize, FTaskbarList);
  FCancelRequested := False;
  FOperationInProgress := True;
  UpdateButtonCaption;

  try
    bz2Stream := TSevenZipOutputStream.Create(outFile, 'tbz2');
    try
      if not StreamingCopyFromDevice(devicePath, bz2Stream, blockSize, diskSize) then
      begin
        HandleLog('Streaming copy failed', llError);
        FOperationSuccess := False;
      end
      else if FCancelRequested then
      begin
        HandleLog('=== BZIP2 compression cancelled ===', llWarning);
        FOperationSuccess := False;
      end
      else
      begin
        HandleLog('=== BZIP2 compression completed successfully ===', llInfo);
        FOperationSuccess := True;
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
      HandleLog('' + E.Message, llError);
      FOperationSuccess := False;
    end;
  end;
  
  FinalizeArchiveOperation(devicePath);
end;

procedure TMainForm.Handle7ZipRead(const devicePath, outFile: string; blockSize, diskSize: Int64);
var
  sevenzStream: TSevenZipOutputStream;
begin
  HandleLog('Using 7z.exe for 7z compression (stdin pipe)...', llInfo);
  FStartTime := Now;
  FLastProgress := 0;
  FLastUpdateTime := Now;
  FTotalBytes := diskSize;
  
  TOperationUIHelper.InitializeOperationUI(StatusBar, ProgressBar, 'Starting...', diskSize, FTaskbarList);
  FCancelRequested := False;
  FOperationInProgress := True;
  UpdateButtonCaption;

  try
    sevenzStream := TSevenZipOutputStream.Create(outFile, 't7z');
    try
      if not StreamingCopyFromDevice(devicePath, sevenzStream, blockSize, diskSize) then
      begin
        HandleLog('Streaming copy failed', llError);
        FOperationSuccess := False;
      end
      else if FCancelRequested then
      begin
        HandleLog('=== 7z compression cancelled ===', llWarning);
        FOperationSuccess := False;
      end
      else
      begin
        HandleLog('=== 7z compression completed successfully ===', llInfo);
        FOperationSuccess := True;
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
      HandleLog('' + E.Message, llError);
      FOperationSuccess := False;
    end;
  end;
  
  FinalizeArchiveOperation(devicePath);
end;

procedure TMainForm.HandleRegularOperation(isWriteMode: Boolean; const inFile, outFile: string; blockSize: Int64);
var
  count, seek, skip, fileSize: Int64;
  noTrunc, stopType: Boolean;
  devicePath: string;
begin
  // Get devicePath from outFile (write mode) or inFile (read mode)
  if isWriteMode then
    devicePath := outFile
  else
    devicePath := inFile;

  count := StrToInt64Def(EditCount.Text, -1);
  HandleLog('Count from EditCount.Text: ' + IntToStr(count), llDebug);
  
  // CRITICAL FIX: Recalculate count based on FILE size, not device size
  // This ensures progress bar shows correct percentage
  if isWriteMode and (inFile <> '') and FileExists(inFile) and not IsGZipFile(inFile) and not IsZipFile(inFile) then
  begin
    fileSize := GetFileSize64(inFile);
    HandleLog('File size: ' + BytesToHuman(fileSize) + ' (' + IntToStr(fileSize) + ' bytes)', llInfo);
    if fileSize > 0 then
    begin
      count := fileSize div blockSize;
      if (fileSize mod blockSize) <> 0 then
        Inc(count);
      HandleLog('Recalculated count based on file size: ' + IntToStr(count) + ' blocks', llInfo);
    end;
  end;
  
  seek := StrToInt64Def(EditSeek.Text, 0);
  skip := StrToInt64Def(EditSkip.Text, 0);
  noTrunc := False;
  stopType := (count > 0); // Check size only if count is specified

  HandleLog('Block size: ' + IntToStr(blockSize) + ' bytes', llDebug);
  HandleLog('Block count: ' + IntToStr(count), llDebug);
  if count > 0 then
    HandleLog('Total data size: ' + BytesToHuman(count * blockSize), llDebug);
  HandleLog('Seek: ' + IntToStr(seek) + ' blocks', llDebug);
  HandleLog('Skip: ' + IntToStr(skip) + ' blocks', llDebug);
  HandleLog('Check size: ' + BoolToStr(stopType, True), llDebug);
  
  // Show confirmation dialog before writing
  if isWriteMode then
  begin
    if MessageDlg(BuildWriteConfirmationMessage(inFile, ComboDevice.Text), 
                  mtWarning, [mbYes, mbNo], 0) <> mrYes then
    begin
      HandleLog('Operation cancelled by user', llWarning);
      Exit;
    end;
  end;
  
  // Prepare volume for write (lock and dismount)
  if isWriteMode then
    PrepareVolumeForWrite(devicePath);
    
  HandleLog('Starting copy operation...', llDebug);

  FOperationSuccess := True; // Assume success, will change on error
  FStartTime := Now;
  FLastProgress := 0;
  FLastUpdateTime := Now;
  StatusBar.Panels[0].Text := '0%';
  StatusBar.Panels[1].Text := '';
  StatusBar.Panels[2].Text := '';
  if isWriteMode then
    StatusBar.Panels[3].Text := 'Writing...'
  else
    StatusBar.Panels[3].Text := 'Reading...';
  
  // Set FTotalBytes for progress
  if count > 0 then
  begin
    FTotalBytes := count * blockSize;
    HandleLog('FTotalBytes set to: ' + BytesToHuman(FTotalBytes) + ' (' + IntToStr(FTotalBytes) + ' bytes) = ' + IntToStr(count) + ' blocks * ' + IntToStr(blockSize) + ' bytes', llInfo);
  end
  else
  begin
    FTotalBytes := 0;
    HandleLog('FTotalBytes set to: 0 (count is 0 or negative)', llWarning);
  end;
  
  // Set taskbar progress
  if Assigned(FTaskbarList) then
    FTaskbarList.SetProgressState(Application.Handle, TBPF_NORMAL);

  ProgressBar.Progress := 0;

  // Create thread (it will auto-start via Resume in constructor)
  FThread := TDDThread.Create(Self, inFile, outFile, blockSize, count, skip, seek, noTrunc, stopType);
  
  // Update button caption after FThread is assigned
  UpdateButtonCaption;
end;

function TMainForm.CheckArchiveComment(const inFile: string; isZip, isGZip: Boolean): Boolean;
var
  archiveComment: string;
begin
  Result := True; // Proceed by default
  
  if isZip or isGZip then
  begin
    if isZip then
    begin
      archiveComment := GetZipComment(inFile);
      HandleLog('File type: Archive (ZIP)', llInfo);
    end
    else
    begin
      archiveComment := GetGZipComment(inFile);
      HandleLog('File type: Archive (GZIP)', llInfo);
    end;

    if archiveComment <> '' then
    begin
      if MessageDlg(archiveComment, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      begin
        HandleLog('Operation cancelled by user', llWarning);
        Result := False;
      end;
    end;
  end
  else
    HandleLog('File type: Regular file', llInfo);
end;

function TMainForm.CheckDeviceSizeForWrite(const devicePath: string; fileSize: Int64): Boolean;
var
  deviceSize: Int64;
begin
  Result := True; // Proceed by default
  
  deviceSize := GetPhysicalDiskSize(devicePath);
  if (deviceSize > 0) and (fileSize > 0) and (fileSize > deviceSize) then
  begin
    HandleLog('Image file is larger than target device', llWarning);
    HandleLog('File: ' + BytesToHuman(fileSize) + ', Device: ' + BytesToHuman(deviceSize), llWarning);
    
    if MessageDlg('Warning: Image size (' + BytesToHuman(fileSize) + ') exceeds device size (' + 
                  BytesToHuman(deviceSize) + '). The image will be truncated. Continue?',
                  mtWarning, [mbYes, mbNo], 0) <> mrYes then
    begin
      HandleLog('Operation cancelled by user', llWarning);
      Result := False;
    end;
  end;
end;

function TMainForm.ValidateReadOperation(const devicePath, outFile: string): Boolean;
begin
  Result := False;
  
  if devicePath = '' then
  begin
    HandleLog('No device selected', llError);
    Exit;
  end;

  if outFile = '' then
  begin
    HandleLog('Output file not specified', llError);
    Exit;
  end;
  
  // Check if output file already exists
  if FileExists(outFile) then
  begin
    if MessageDlg('File "' + outFile + '" already exists. Do you want to overwrite it?', 
                  mtWarning, [mbYes, mbNo], 0) <> mrYes then
    begin
      HandleLog('Operation cancelled by user - file already exists', llWarning);
      Exit;
    end;
  end;
  
  Result := True;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
var
  inFile, outFile, devicePath: string;
  blockSize, count, diskSize, fileSize: Int64;
  isWriteMode: Boolean;
  isGZip: Boolean;
  isZip: Boolean;
  archiveType: TArchiveType;
  DiskNum: Integer;
begin
  // Handle cancel request for ongoing operations
  if HandleCancelRequest then
    Exit;

  isWriteMode := (RadioGroupMode.ItemIndex = 0);

  HandleLog('=== Starting operation ===', llInfo);
  HandleLog('Mode: ' + RadioGroupMode.Items[RadioGroupMode.ItemIndex], llInfo);
  
  // Save file to history on operation start
  if Trim(EditIn.Text) <> '' then
    AddToFileHistory(EditIn.Text);
  
  // Determine if we need elevated privileges (working with physical disk)
  devicePath := GetDevicePath(ComboDevice.ItemIndex);
  FUseElevation := (Pos('PHYSICALDRIVE', UpperCase(devicePath)) > 0);

  HandleLog('Device path: ' + devicePath, llDebug);
  HandleLog('FUseElevation = ' + BoolToStr(FUseElevation, True), llDebug);
  HandleLog('IsProcessElevated = ' + BoolToStr(IsProcessElevated, True), llDebug);
  
  // If elevation is needed and we don't have admin rights, launch worker process
  if FUseElevation and not IsProcessElevated then
  begin
    if LaunchElevatedWorker(isWriteMode, devicePath) then
      Exit
    else
      Exit; // Failed to launch worker
  end;

  if isWriteMode then
  begin
    // Write mode: file -> device
    inFile := EditIn.Text;
    devicePath := GetDevicePath(ComboDevice.ItemIndex);
    outFile := devicePath;
    FDevicePath := devicePath;
    FIsWriteMode := True;
    
    // Проверить health status устройства перед записью
    DiskNum := ExtractDiskNumberFromPath(devicePath);
    if (DiskNum >= 0) and not ValidateDeviceForWrite(DiskNum) then
    begin
      HandleLog('Operation cancelled due to device health issues', llWarning);
      Exit;
    end;
    
    // Block size is already calculated in ComboDeviceChange
    blockSize := ParseSize(EditBS.Text, 512);
    if blockSize <= 0 then
    begin
      HandleLog('Invalid block size: ' + EditBS.Text, llError);
      blockSize := 512;
      EditBS.Text := '512';
    end;

    // Validate write operation parameters
    if not ValidateWriteOperation(inFile, devicePath, blockSize) then
      Exit;

    
    // Check archive type using ArchiveHandler
    archiveType := TArchiveHandler.DetectArchiveType(inFile);
    isGZip := (archiveType = atGZip);
    isZip := (archiveType = atZip);
    
    // Check for archive comment and confirm
    if not CheckArchiveComment(inFile, isZip, isGZip) then
      Exit;

    HandleLog('Source file: ' + inFile, llInfo);
    HandleLog('Target device: ' + devicePath + ' (' + ComboDevice.Text + ')', llInfo);
    
    // Get file size and calculate block count
    fileSize := 0; // Initialize to avoid potential use of uninitialized variable
    try
      fileSize := GetFileSize64(inFile);
      if fileSize > 0 then
        HandleLog('File size: ' + BytesToHuman(fileSize) + ' (' + IntToStr(fileSize) + ' bytes)', llInfo);
        
      // Auto-calculate count based on file size
      if (Trim(EditCount.Text) = '') and (blockSize > 0) then
      begin
        count := fileSize div blockSize;
        if (fileSize mod blockSize) <> 0 then
          Inc(count);
        EditCount.Text := IntToStr(count);
        HandleLog('Auto-calculated block count: ' + IntToStr(count) + ' blocks (based on file size)', llDebug);
      end;
    except
      on E: Exception do
        HandleLog('Cannot determine file size: ' + E.Message, llWarning);
    end;

    // Check if image fits on device
    if not CheckDeviceSizeForWrite(devicePath, fileSize) then
      Exit;

    HandleLog('Writing: ' + inFile + ' -> ' + devicePath, llInfo);
    
    // Handle archive formats
    case archiveType of
      atZip:
        begin
          HandleZipWrite(inFile, devicePath, blockSize);
          Exit;
        end;
      atGZip:
        begin
          HandleGZipWrite(inFile, devicePath, blockSize);
          Exit;
        end;
      atXZ:
        begin
          HandleXZWrite(inFile, devicePath, blockSize);
          Exit;
        end;
      atBZip2:
        begin
          HandleBZip2Write(inFile, devicePath, blockSize);
          Exit;
        end;
      atTarGz:
        begin
          HandleTarGzWrite(inFile, devicePath, blockSize);
          Exit;
        end;
      atTarXz:
        begin
          HandleTarXzWrite(inFile, devicePath, blockSize);
          Exit;
        end;
      at7Zip:
        begin
          Handle7ZipWrite(inFile, devicePath, blockSize);
          Exit;
        end;
    end;
  end
  else
  begin
    // Read mode: device -> file
    devicePath := GetDevicePath(ComboDevice.ItemIndex);
    inFile := devicePath;
    outFile := EditIn.Text;

    FDevicePath := devicePath;
    FIsWriteMode := False;
    
    // Validate read operation parameters
    if not ValidateReadOperation(devicePath, outFile) then
      Exit;
    
    // Check if output should be GZIP or ZIP archive
    isGZip := IsGZipFile(outFile);
    isZip := IsZipFile(outFile);
    if isZip then
      HandleLog('File type: Archive (ZIP)', llInfo)
    else if isGZip then
      HandleLog('File type: Archive (GZIP)', llInfo)
    else
      HandleLog('File type: Regular file', llInfo);

    HandleLog('Source device: ' + devicePath + ' (' + ComboDevice.Text + ')', llInfo);
    HandleLog('Target file: ' + outFile, llInfo);
    
    // Block size is already calculated in ComboDeviceChange
    blockSize := ParseSize(EditBS.Text, 512);
    if blockSize <= 0 then
    begin
      HandleLog('Invalid block size: ' + EditBS.Text, llError);
      blockSize := 512;
      EditBS.Text := '512';
    end;

    // Get disk size and calculate block count
    diskSize := GetPhysicalDiskSize(devicePath);
    // Auto-calculate count based on disk size
    if Trim(EditCount.Text) = '' then
    begin
      if diskSize > 0 then
      begin
        count := diskSize div blockSize;
        if (diskSize mod blockSize) <> 0 then
          Inc(count);
        EditCount.Text := IntToStr(count);
        HandleLog('Auto-calculated block count: ' + IntToStr(count) + ' blocks (based on disk size)', llDebug);
        HandleLog('Enabled size checking for device read', llDebug);
      end
      else
        HandleLog('Cannot determine disk size', llWarning);
    end;

    HandleLog('Reading: ' + devicePath + ' -> ' + outFile, llInfo);
    
    // Read count from EditCount (can be auto-calculated or specified manually)
    count := StrToInt64Def(EditCount.Text, -1);
    if count > 0 then
      diskSize := count * blockSize; // Use size from count (diskSize already retrieved above)
    
    // Detect archive type for compression
    archiveType := TArchiveHandler.DetectArchiveType(outFile);
    
    case archiveType of
      atZip:
        begin
          HandleZipRead(devicePath, outFile, blockSize, diskSize);
          Exit;
        end;
      atGZip:
        begin
          HandleGZipRead(devicePath, outFile, blockSize, diskSize);
          Exit;
        end;
      atXZ:
        begin
          HandleXZRead(devicePath, outFile, blockSize, diskSize);
          Exit;
        end;
      atBZip2:
        begin
          HandleBZip2Read(devicePath, outFile, blockSize, diskSize);
          Exit;
        end;
      at7Zip:
        begin
          Handle7ZipRead(devicePath, outFile, blockSize, diskSize);
          Exit;
        end;
    end;
  end;

  // Handle regular (non-archive) operations
  HandleRegularOperation(isWriteMode, inFile, outFile, blockSize);
end;

function IsZipFile(const FileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Result := (Ext = '.zip');
end;

function IsXZFile(const FileName: string): Boolean;
var
  Ext, BaseExt: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Result := (Ext = '.xz');
  
  // Check double extensions: .img.xz, .iso.xz, .bin.xz
  if Result then
  begin
    BaseExt := LowerCase(ExtractFileExt(ChangeFileExt(FileName, '')));
    Result := (BaseExt = '.img') or (BaseExt = '.iso') or (BaseExt = '.bin') or (BaseExt = '');
  end;
end;

function IsTarXZFile(const FileName: string): Boolean;
var
  Ext: string;
  BaseName: string;
begin
  Result := False;
  Ext := LowerCase(ExtractFileExt(FileName));
  if (Ext = '.xz') then
  begin
    BaseName := ChangeFileExt(FileName, '');
    Ext := LowerCase(ExtractFileExt(BaseName));
    Result := (Ext = '.tar');
  end;
end;

function IsCompressedImage(const FileName: string): Boolean;
var
  Ext, BaseExt: string;
  archiveType: TArchiveType;
begin
  archiveType := TArchiveHandler.DetectArchiveType(FileName);
  Result := archiveType <> atNone;
  
  // For tar archives, they're not considered direct disk images
  if (archiveType = atTarGz) or (archiveType = atTarXz) then
    Result := False;
  
  // For compressed files, check if it's an image (double extension)
  if Result and (archiveType in [atGZip, atXZ, atBZip2]) then
  begin
    Ext := LowerCase(ExtractFileExt(FileName));
    if (Ext = '.gz') or (Ext = '.xz') or (Ext = '.bz2') then
    begin
      BaseExt := LowerCase(ExtractFileExt(ChangeFileExt(FileName, '')));
      // Accept .img, .iso, .bin or no extension
      Result := (BaseExt = '.img') or (BaseExt = '.iso') or (BaseExt = '.bin') or (BaseExt = '');
    end;
  end;
end;

function IsArchiveFile(const FileName: string): Boolean;
begin
  Result := TArchiveHandler.DetectArchiveType(FileName) <> atNone;
end;

function GetDriveTypeString(const FilePath: string): string;
var
  DriveType: UINT;
  RootPath: string;
begin
  Result := 'Unknown';
  
  // Check for UNC path (\\server\share or \\192.168.x.x\share)
  if (Length(FilePath) >= 2) and (FilePath[1] = '\') and (FilePath[2] = '\') then
  begin
    Result := 'Network (UNC)';
    Exit;
  end;
  
  // Get drive letter (C:, D:, etc.)
  if (Length(FilePath) >= 2) and (FilePath[2] = ':') then
  begin
    RootPath := Copy(FilePath, 1, 3); // "C:\"
    DriveType := GetDriveType(PChar(RootPath));

    case DriveType of
      DRIVE_REMOVABLE:
        Result := 'Removable';
      DRIVE_FIXED:
        Result := 'Fixed';
      DRIVE_REMOTE:
        Result := 'Network';
      DRIVE_CDROM:
        Result := 'CD-ROM';
      DRIVE_RAMDISK:
        Result := 'RAM Disk';
      DRIVE_NO_ROOT_DIR:
        Result := 'Invalid';
    else
      Result := 'Unknown';
    end;
  end;
end;

function IsGZipFile(const FileName: string): Boolean;
var
  Ext, BaseExt: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Result := (Ext = '.gz') or (Ext = '.gzip');
  
  // Check double extensions: .img.gz, .iso.gz, .bin.gz
  if Result then
  begin
    BaseExt := LowerCase(ExtractFileExt(ChangeFileExt(FileName, '')));
    Result := (BaseExt = '.img') or (BaseExt = '.iso') or (BaseExt = '.bin') or (BaseExt = '');
  end;
end;

function IsTarGZFile(const FileName: string): Boolean;
var
  Ext: string;
  BaseName: string;
begin
  Result := False;
  Ext := LowerCase(ExtractFileExt(FileName));
  if (Ext = '.gz') or (Ext = '.gzip') then
  begin
    BaseName := ChangeFileExt(FileName, '');
    Ext := LowerCase(ExtractFileExt(BaseName));
    Result := (Ext = '.tar');
  end;
end;

function IsSpecialDevice(const Path: string): Boolean;
begin
  Result := (Path = '/dev/zero') or (Path = '/dev/random') or (Path = '/dev/null') or (Path = '-');
end;

function TMainForm.CalculateFileMD5(const FileName: string): string;
var
  HashAlgo: string;
  Algorithm: THashAlgorithm;
begin
  try
    HashAlgo := 'MD5';
    if Assigned(ComboHashAlgo) and (ComboHashAlgo.ItemIndex >= 0) then
      HashAlgo := ComboHashAlgo.Text;
    
    if HashAlgo = 'SHA256' then
      Algorithm := haSHA256
    else
      Algorithm := haMD5;
    
    Result := THashUtils.CalculateFileHash(FileName, Algorithm);
  except
    Result := '';
  end;
end;

function TMainForm.CalculateDeviceMD5(const DevicePath: string; Size: Int64): string;
var
  HashAlgo: string;
  Algorithm: THashAlgorithm;
begin
  try
    HashAlgo := 'MD5';
    if Assigned(ComboHashAlgo) and (ComboHashAlgo.ItemIndex >= 0) then
      HashAlgo := ComboHashAlgo.Text;
    
    if HashAlgo = 'SHA256' then
      Algorithm := haSHA256
    else
      Algorithm := haMD5;
    
    Result := THashUtils.CalculateDeviceHash(DevicePath, Size, Algorithm);
  except
    on E: Exception do
    begin
      HandleLog('' + E.Message, llError);
      Result := '';
    end;
  end;
end;

procedure TMainForm.SaveHashToFile(const ImageFile, Hash: string);
var
  HashAlgo: string;
  Algorithm: THashAlgorithm;
begin
  HashAlgo := 'MD5';
  if Assigned(ComboHashAlgo) and (ComboHashAlgo.ItemIndex >= 0) then
    HashAlgo := ComboHashAlgo.Text;
  
  if HashAlgo = 'SHA256' then
    Algorithm := haSHA256
  else
    Algorithm := haMD5;
  
  try
    THashUtils.SaveHashToFile(ImageFile, Hash, Algorithm);
    HandleLog('Hash saved to: ' + ChangeFileExt(ImageFile, THashUtils.GetHashExtension(Algorithm)), llInfo);
  except
    on E: Exception do
      HandleLog('Failed to save hash file: ' + E.Message, llError);
  end;
end;

procedure TMainForm.LoadFileHistory;
begin
  FSettingsManager.LoadFileHistory(EditIn);
  
  // Initialize with special devices if combobox is empty
  if EditIn.Items.Count = 0 then
  begin
    EditIn.Items.Add('/dev/random');
    EditIn.Items.Add('/dev/zero');
  end;
end;

procedure TMainForm.SaveFileHistory;
begin
  FSettingsManager.SaveFileHistory(EditIn);
end;

procedure TMainForm.AddToFileHistory(const FilePath: string);
begin
  FSettingsManager.AddToFileHistory(EditIn, FilePath);
end;

procedure TMainForm.LoadSettings;
begin
  FSettingsManager.LoadSettings(EditBS, EditCount, EditSeek, EditSkip,
    CheckAdvanced, CheckShowFixed, CheckVerifyHash, CheckAutoEject, ComboHashAlgo);
end;

procedure TMainForm.SaveSettings;
begin
  FSettingsManager.SaveSettings(EditBS, EditCount, EditSeek, EditSkip,
    CheckAdvanced, CheckShowFixed, CheckVerifyHash, CheckAutoEject, ComboHashAlgo);
end;

function StreamingCopyToDevice(InStream: TStream; OutFileName: string; BlockSize, TotalSize: Int64): Boolean;
var
  OutHandle: THandle;
  Buffer: Pointer;
  BytesRead, BytesWritten: DWORD;
  TotalProcessed: Int64;
  LastError: DWORD;
begin
  Result := False;

  // Special case: empty stream
  if TotalSize = 0 then
  begin
    if Assigned(AppMainForm) then
      AppMainForm.HandleLog('Empty stream (0 bytes), nothing to write', llInfo);
    Result := True;
    Exit;
  end;

  if Assigned(AppMainForm) then
    AppMainForm.HandleLog('Opening device for writing: ' + OutFileName, llDebug);
  OutHandle := CreateFile(PChar(OutFileName), GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if OutHandle = INVALID_HANDLE_VALUE then
  begin
    LastError := GetLastError;
    if Assigned(AppMainForm) then
      AppMainForm.HandleLog('Failed to open device. Error code: ' + IntToStr(LastError), llError);
    Exit;
  end;

  if Assigned(AppMainForm) then
  begin
    AppMainForm.HandleLog('Device opened successfully', llDebug);
    AppMainForm.HandleLog('Block size: ' + IntToStr(BlockSize) + ' bytes', llDebug);
    AppMainForm.HandleLog('Total size to write: ' + AppMainForm.BytesToHuman(TotalSize), llDebug);
  end;

  GetMem(Buffer, BlockSize);
  try
    TotalProcessed := 0;
    while TotalProcessed < TotalSize do
    begin
      // Process UI messages first to handle cancel requests
      Application.ProcessMessages;
      
      // Check for cancel request
      if Assigned(AppMainForm) and AppMainForm.FCancelRequested then
      begin
        AppMainForm.HandleLog('Operation cancelled by user', llWarning);
        Break;
      end;
      
      BytesRead := InStream.Read(Buffer^, BlockSize);
      if BytesRead = 0 then
      begin
        if Assigned(AppMainForm) then
          AppMainForm.HandleLog('Stream ended, read 0 bytes', llDebug);
        Break;
      end;

      // Final check before writing to disk
      if Assigned(AppMainForm) and AppMainForm.FCancelRequested then
      begin
        AppMainForm.HandleLog('Operation cancelled before write', llWarning);
        Break;
      end;

      if not WriteFile(OutHandle, Buffer^, BytesRead, BytesWritten, nil) then
      begin
        LastError := GetLastError;
        if Assigned(AppMainForm) then
        begin
          AppMainForm.HandleLog('WriteFile failed. Error code: ' + IntToStr(LastError), llError);
          AppMainForm.HandleLog('Total written before error: ' + AppMainForm.BytesToHuman(TotalProcessed), llInfo);
        end;
        Break;
      end;

      if BytesWritten <> BytesRead then
      begin
        if Assigned(AppMainForm) then
        begin
          AppMainForm.HandleLog('Partial write! Read: ' + IntToStr(BytesRead) + ', Written: ' + IntToStr(BytesWritten), llError);
          AppMainForm.HandleLog('Disk may be full or hardware error occurred', llError);
        end;
        Result := False;
        Break;
      end;

      Inc(TotalProcessed, BytesWritten);
      
      // Update progress bar
      if Assigned(AppMainForm) then
        AppMainForm.UpdateProgress(TotalProcessed);
    end;

    if Assigned(AppMainForm) then
      AppMainForm.HandleLog('Total bytes written: ' + AppMainForm.BytesToHuman(TotalProcessed), llInfo);
    Result := (TotalProcessed > 0);
  finally
    FreeMem(Buffer);
    if OutHandle <> INVALID_HANDLE_VALUE then
    begin
      // Flush write cache to disk
      if not FlushFileBuffers(OutHandle) then
      begin
        if Assigned(AppMainForm) then
          AppMainForm.HandleLog('Failed to flush buffers: ' + SysErrorMessage(GetLastError), llWarning);
      end;
      CloseHandle(OutHandle);
      if Assigned(AppMainForm) then
        AppMainForm.HandleLog('Device closed', llDebug);
    end;
  end;
end;

function StreamingCopyFromDevice(InFileName: string; OutStream: TStream; BlockSize, TotalSize: Int64): Boolean;
var
  InHandle: THandle;
  Buffer: Pointer;
  BytesRead, BytesWritten: DWORD;
  TotalProcessed: Int64;
  LastError: DWORD;
begin
  Result := False;

  if Assigned(AppMainForm) then
    AppMainForm.HandleLog('Opening device for reading: ' + InFileName, llDebug);
  InHandle := CreateFile(PChar(InFileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if InHandle = INVALID_HANDLE_VALUE then
  begin
    LastError := GetLastError;
    if Assigned(AppMainForm) then
      AppMainForm.HandleLog('Failed to open device. Error code: ' + IntToStr(LastError), llError);
    Exit;
  end;

  if Assigned(AppMainForm) then
  begin
    AppMainForm.HandleLog('Device opened successfully', llDebug);
    AppMainForm.HandleLog('Block size: ' + IntToStr(BlockSize) + ' bytes', llDebug);
    AppMainForm.HandleLog('Total size to read: ' + AppMainForm.BytesToHuman(TotalSize), llDebug);
  end;

  GetMem(Buffer, BlockSize);
  try
    TotalProcessed := 0;
    while TotalProcessed < TotalSize do
    begin
      // Process UI messages first to handle cancel requests
      Application.ProcessMessages;
      
      // Check for cancel request
      if Assigned(AppMainForm) and AppMainForm.FCancelRequested then
      begin
        AppMainForm.HandleLog('Operation cancelled by user', llWarning);
        Break;
      end;
      
      if not ReadFile(InHandle, Buffer^, BlockSize, BytesRead, nil) then
      begin
        LastError := GetLastError;
        if Assigned(AppMainForm) then
        begin
          AppMainForm.HandleLog('ReadFile failed. Error code: ' + IntToStr(LastError), llError);
          AppMainForm.HandleLog('Total read before error: ' + AppMainForm.BytesToHuman(TotalProcessed), llInfo);
        end;
        Break;
      end;
      if BytesRead = 0 then
      begin
        if Assigned(AppMainForm) then
          AppMainForm.HandleLog('Device read ended (0 bytes)', llDebug);
        Break;
      end;

      BytesWritten := OutStream.Write(Buffer^, BytesRead);
      if BytesWritten <> BytesRead then
      begin
        if Assigned(AppMainForm) then
          AppMainForm.HandleLog('Partial stream write! Read: ' + IntToStr(BytesRead) + ', Written: ' + IntToStr(BytesWritten), llWarning);
      end;

      Inc(TotalProcessed, BytesWritten);
      
      // Limit TotalProcessed to TotalSize value
      if TotalProcessed > TotalSize then
        TotalProcessed := TotalSize;
      
      // Update progress bar
      if Assigned(AppMainForm) then
        AppMainForm.UpdateProgress(TotalProcessed);
      
      // Process UI messages to prevent freezing
      Application.ProcessMessages;
    end;

    if Assigned(AppMainForm) then
      AppMainForm.HandleLog('Total bytes read: ' + AppMainForm.BytesToHuman(TotalProcessed), llInfo);
    Result := (TotalProcessed > 0);
  finally
    FreeMem(Buffer);
    CloseHandle(InHandle);
    if Assigned(AppMainForm) then
      AppMainForm.HandleLog('Device closed', llDebug);
  end;
end;

// === Context Menu Handlers ===

procedure TMainForm.MenuItemSelectAllClick(Sender: TObject);
begin
  MemoLog.SelectAll;
end;

procedure TMainForm.MenuItemCopyClick(Sender: TObject);
begin
  if MemoLog.SelLength > 0 then
    MemoLog.CopyToClipboard
  else
  begin
    MemoLog.SelectAll;
    MemoLog.CopyToClipboard;
    MemoLog.SelLength := 0; // Deselect
  end;
end;

procedure TMainForm.MenuItemSaveAsClick(Sender: TObject);
var
  FileStream: TFileStream;
  LogText: string;
begin
  SaveDialogLog.FileName := 'ImageWriter_Log_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.txt';
  if SaveDialogLog.Execute then
  begin
    try
      LogText := MemoLog.Text;
      FileStream := TFileStream.Create(SaveDialogLog.FileName, fmCreate);
      try
        if Length(LogText) > 0 then
          FileStream.WriteBuffer(LogText[1], Length(LogText));
        HandleLog('Log saved to: ' + SaveDialogLog.FileName, llInfo);
      finally
        FileStream.Free;
      end;
    except
      on E: Exception do
        HandleLog('Error saving log: ' + E.Message, llError);
    end;
  end;
end;

{ TWorkerIPCThread }

constructor TWorkerIPCThread.Create(Owner: TMainForm; const PipeName: string; WorkerProcess: THandle; IsWriteMode: Boolean; const FilePath: string; BlockSize: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FOwner := Owner;
  FPipeName := PipeName;
  FWorkerProcess := WorkerProcess;
  FSuccess := True;
  FProgress := 0;
  FTotal := 0;
  FIsWriteMode := IsWriteMode;
  FFilePath := FilePath;
  FBlockSize := BlockSize;

  FPipe := TIPCPipeClient.Create(PipeName);
  Resume;
end;

destructor TWorkerIPCThread.Destroy;
begin
  // Send cancel signal to worker
  if Assigned(FPipe) and FPipe.Connected then
  begin
    try
      FPipe.SendMessage(ipcCancel, '');
      Sleep(100); // Give worker time to process cancel
    except
      // Ignore errors on shutdown
    end;
  end;

  if Assigned(FPipe) then
    FPipe.Free;
  
  // Wait for worker to exit (up to 5 seconds)
  if FWorkerProcess <> 0 then
  begin
    if WaitForSingleObject(FWorkerProcess, 5000) = WAIT_TIMEOUT then
    begin
      // Worker didn't exit gracefully, force terminate
      TerminateProcess(FWorkerProcess, 1);
    end;
    CloseHandle(FWorkerProcess);
  end;

  inherited;
end;

procedure TWorkerIPCThread.SyncLog;
begin
  if Assigned(FOwner) and (FLogMessage <> '') then
    FOwner.HandleLog(FLogMessage, llInfo);
end;

procedure TWorkerIPCThread.SyncProgress;
begin
  if Assigned(FOwner) then
  begin
    FOwner.FTotalBytes := FTotal;
    FOwner.UpdateProgress(FProgress);
  end;
end;

procedure TWorkerIPCThread.DoCompleted;
begin
  if Assigned(FOwner) then
  begin
    FOwner.FOperationSuccess := FSuccess;
    FOwner.ThreadCompleted(FOwner);
  end;
end;

procedure TWorkerIPCThread.SendCancel;
begin
  // Terminate the thread to break out of SendFileData loop
  Terminate;
  
  // Send cancel signal to worker
  if Assigned(FPipe) and FPipe.Connected then
  begin
    try
      FPipe.SendMessage(ipcCancel, '');
    except
      // Ignore errors if pipe is broken
    end;
  end;
end;

procedure TWorkerIPCThread.SendFileData;
var
  FileStream: TFileStream;
  Buffer: array[0..4095] of Byte; // 4KB buffer to fit in pipe buffer
  BytesRead: Integer;
  TotalSize: Int64;
  TotalSent: Int64;
  DataStr: string;
  MsgType: TIPCMessageType;
  Data: string;
  ReadyReceived: Boolean;
begin
  try
    FLogMessage := 'Opening file: ' + FFilePath;
    Synchronize(SyncLog);

    if not EnsureNetworkShareAccess(FFilePath) then
    begin
      FLogMessage := 'Network folder access error: ' + FFilePath;
      Synchronize(SyncLog);
      Exit;
    end;
    FileStream := TFileStream.Create(FFilePath, fmOpenRead or fmShareDenyWrite);
    try
      TotalSize := FileStream.Size;
      TotalSent := 0;

      FLogMessage := 'File opened, size: ' + IntToStr(TotalSize) + ' bytes';
      Synchronize(SyncLog);
      
      // Wait for worker to send ready signal
      FLogMessage := 'Waiting for worker ready signal...';
      Synchronize(SyncLog);

      ReadyReceived := False;
      while not ReadyReceived and not Terminated do
      begin
        if FPipe.ReceiveMessage(MsgType, Data, 100) then
        begin
          case MsgType of
            ipcReady:
              begin
                ReadyReceived := True;
                FLogMessage := 'Worker is ready, starting transmission';
                Synchronize(SyncLog);
              end;
            ipcLog:
              begin
                FLogMessage := Data;
                Synchronize(SyncLog);
              end;
            ipcError:
              begin
                FLogMessage := 'ERROR from worker: ' + Data;
                Synchronize(SyncLog);
                Exit;
              end;
          end;
        end;
      end;

      if not ReadyReceived then
      begin
        FLogMessage := 'ERROR: Worker did not send ready signal';
        Synchronize(SyncLog);
        Exit;
      end;
      
      // Read any remaining messages from worker before we start sending
      while FPipe.ReceiveMessage(MsgType, Data, 0) do
      begin
        case MsgType of
          ipcLog:
            begin
              FLogMessage := Data;
              Synchronize(SyncLog);
            end;
          ipcError:
            begin
              FLogMessage := 'ERROR from worker: ' + Data;
              Synchronize(SyncLog);
              Exit;
            end;
        end;
      end;
      
      // Send total size as first message
      FLogMessage := 'Sending total size to worker...';
      Synchronize(SyncLog);

      if Terminated then
        Exit;

      if not FPipe.SendMessage(ipcData, IntToStr(TotalSize)) then
      begin
        FLogMessage := 'ERROR: Failed to send total size';
        Synchronize(SyncLog);
        Exit;
      end;

      FLogMessage := 'Total size sent, starting data transmission...';
      Synchronize(SyncLog);

      FLogMessage := 'Sending file data: ' + IntToStr(TotalSize) + ' bytes';
      Synchronize(SyncLog);
      
      // Send file in blocks
      while TotalSent < TotalSize do
      begin
        if Terminated then
        begin
          FLogMessage := 'ERROR: Terminated flag set during transmission at ' + IntToStr(TotalSent) + ' bytes';
          Synchronize(SyncLog);
          Break;
        end;
        
        // Check for incoming messages (errors) while sending
        while FPipe.ReceiveMessage(MsgType, Data, 0) do
        begin
          case MsgType of
            ipcLog:
              begin
                FLogMessage := Data;
                Synchronize(SyncLog);
              end;
            ipcError:
              begin
                FLogMessage := 'ERROR from worker: ' + Data;
                Synchronize(SyncLog);
                Exit;
              end;
            ipcCompleted:
              begin
                FLogMessage := 'Worker completed writing';
                Synchronize(SyncLog);
                Exit;
              end;
          end;
        end;

        BytesRead := FileStream.Read(Buffer, SizeOf(Buffer));
        if BytesRead <= 0 then
        begin
          FLogMessage := 'ERROR: FileStream.Read returned ' + IntToStr(BytesRead) + ' at position ' + IntToStr(TotalSent);
          Synchronize(SyncLog);
          Break;
        end;
        
        // Convert buffer to string for sending
        SetLength(DataStr, BytesRead);
        Move(Buffer[0], DataStr[1], BytesRead);

        if not FPipe.SendMessage(ipcData, DataStr) then
        begin
          FLogMessage := 'ERROR: Failed to send data block at ' + IntToStr(TotalSent) + ' bytes';
          Synchronize(SyncLog);
          Break;
        end;

        Inc(TotalSent, BytesRead);
        
        // Update progress based on sent data (every 256KB to avoid overhead)
        if (TotalSent mod 262144 = 0) or (TotalSent >= TotalSize) then
        begin
          FProgress := TotalSent;
          FTotal := TotalSize;
          Synchronize(SyncProgress);
        end;
      end;
      
      // Send completion message
      if TotalSent < TotalSize then
      begin
        FLogMessage := 'WARNING: Only sent ' + IntToStr(TotalSent) + ' of ' + IntToStr(TotalSize) + ' bytes';
        Synchronize(SyncLog);
      end
      else
      begin
        // Ensure progress shows 100%
        FProgress := TotalSize;
        FTotal := TotalSize;
        Synchronize(SyncProgress);
      end;

      if not FPipe.SendMessage(ipcCompleted, '') then
      begin
        FLogMessage := 'WARNING: Failed to send completion signal';
        Synchronize(SyncLog);
      end;

      FLogMessage := 'File data sent successfully (' + IntToStr(TotalSent) + ' bytes)';
      Synchronize(SyncLog);

    finally
      FileStream.Free;
    end;

  except
    on E: Exception do
    begin
      FLogMessage := 'ERROR sending file: ' + E.Message;
      Synchronize(SyncLog);
    end;
  end;
end;

procedure TWorkerIPCThread.ReceiveFileData;
var
  FileStream: TFileStream;
  MsgType: TIPCMessageType;
  Data: string;
  TotalSize: Int64;
  TotalReceived: Int64;
  SizeReceived: Boolean;
begin
  try
    FLogMessage := 'Creating file: ' + FFilePath;
    Synchronize(SyncLog);

    if not EnsureNetworkShareAccess(FFilePath) then
    begin
      FLogMessage := 'Network folder access error: ' + FFilePath;
      Synchronize(SyncLog);
      Exit;
    end;
    
    FileStream := TFileStream.Create(FFilePath, fmCreate);
    try
      TotalSize := 0;
      TotalReceived := 0;

      FLogMessage := 'File created, waiting for data from worker...';
      Synchronize(SyncLog);
      
      // Wait for worker to send ready signal
      SizeReceived := False;
      
      while (not SizeReceived or (TotalReceived < TotalSize)) and not Terminated do
      begin
        if FPipe.ReceiveMessage(MsgType, Data, 100) then
        begin
          case MsgType of
            ipcReady:
              begin
                FLogMessage := 'Worker is ready to send data';
                Synchronize(SyncLog);
              end;
              
            ipcLog:
              begin
                FLogMessage := Data;
                Synchronize(SyncLog);
              end;
              
            ipcData:
              begin
                if not SizeReceived then
                begin
                  // First data message contains total size
                  TotalSize := StrToInt64Def(Data, 0);
                  SizeReceived := True;
                  FLogMessage := 'Total size: ' + IntToStr(TotalSize) + ' bytes';
                  Synchronize(SyncLog);
                  FTotal := TotalSize;
                  FProgress := 0;
                  Synchronize(SyncProgress);
                end
                else
                begin
                  // Data block
                  if Length(Data) > 0 then
                  begin
                    FileStream.Write(Data[1], Length(Data));
                    Inc(TotalReceived, Length(Data));
                    FProgress := TotalReceived;
                    Synchronize(SyncProgress);
                  end;
                end;
              end;
              
            ipcError:
              begin
                FLogMessage := 'ERROR from worker: ' + Data;
                Synchronize(SyncLog);
                Exit;
              end;
              
            ipcCompleted:
              begin
                FLogMessage := 'Worker completed reading';
                Synchronize(SyncLog);
                Break;
              end;
          end;
        end;
      end;

      FLogMessage := 'File data received successfully (' + IntToStr(TotalReceived) + ' bytes)';
      Synchronize(SyncLog);

    finally
      FileStream.Free;
    end;

  except
    on E: Exception do
    begin
      FLogMessage := 'ERROR receiving file: ' + E.Message;
      Synchronize(SyncLog);
    end;
  end;
end;

procedure TWorkerIPCThread.Execute;
var
  MsgType: TIPCMessageType;
  Data: string;
  Parts: TStringList;
  WorkerRunning: Boolean;
  ExitCode: DWORD;
begin
  try
    // Connect to worker's pipe
    if not FPipe.Connect(10000) then
    begin
      FLogMessage := 'ERROR: Cannot connect to worker process';
      Synchronize(SyncLog);
      FSuccess := False;
      Synchronize(DoCompleted);
      Exit;
    end;

    FLogMessage := 'Connected to worker process';
    Synchronize(SyncLog);

    // If WRITE mode, send file data to worker
    if FIsWriteMode then
    begin
      SendFileData;
      FLogMessage := 'File transmission complete, waiting for worker response...';
      Synchronize(SyncLog);
    end
    else
    begin
      // READ mode - receive file data from worker
      ReceiveFileData;
      FLogMessage := 'File reception complete, waiting for worker confirmation...';
      Synchronize(SyncLog);
    end;

    WorkerRunning := True;
    Parts := TStringList.Create;
    try
      // Receive messages from worker
      FLogMessage := 'Entering message receive loop';
      Synchronize(SyncLog);
      while WorkerRunning and not Terminated do
      begin
        if FPipe.ReceiveMessage(MsgType, Data, 100) then
        begin
          FLogMessage := 'Received: ' + IntToStr(Ord(MsgType)) + ' (' + Data + ')';
          Synchronize(SyncLog);

          case MsgType of
            ipcLog:
              begin
                FLogMessage := Data;
                Synchronize(SyncLog);
              end;

            ipcProgress:
              begin
                Parts.Clear;
                Parts.Delimiter := '|';
                Parts.DelimitedText := Data;
                if Parts.Count >= 2 then
                begin
                  FProgress := StrToInt64Def(Parts[0], 0);
                  FTotal := StrToInt64Def(Parts[1], 0);
                  Synchronize(SyncProgress);
                end;
              end;

            ipcCompleted:
              begin
                FLogMessage := 'Operation completed successfully';
                Synchronize(SyncLog);
                FSuccess := True;
                WorkerRunning := False;
              end;

            ipcError:
              begin
                FLogMessage := 'ERROR: ' + Data;
                Synchronize(SyncLog);
                FSuccess := False;
                WorkerRunning := False;
              end;
          end;
        end;

        // Check if worker process is still running
        if GetExitCodeProcess(FWorkerProcess, ExitCode) then
        begin
          if ExitCode <> STILL_ACTIVE then
          begin
            WorkerRunning := False;
            if ExitCode <> 0 then
            begin
              FLogMessage := 'Worker process exited with code: ' + IntToStr(ExitCode);
              Synchronize(SyncLog);
              FSuccess := False;
            end;
          end;
        end;

        Sleep(50);
      end;
    finally
      Parts.Free;
    end;

  except
    on E: Exception do
    begin
      FLogMessage := 'ERROR: ' + E.Message;
      Synchronize(SyncLog);
      FSuccess := False;
    end;
  end;

  Synchronize(DoCompleted);
end;

// Send command to persistent worker and get response
function TMainForm.SendWorkerCommand(const Command: string): string;
const
  RESPONSE_TIMEOUT = 10000; // 10 seconds
{$IFDEF IMAGEWRITER_PRO}
var
  TimeoutStart: Cardinal;
  MsgType: TIPCMessageType;
  Response: string;
{$ENDIF}
begin
  Result := '';
  
  {$IFDEF IMAGEWRITER_PRO}
  if not Assigned(FWorkerPipe) or not FWorkerPipe.Connected then
  begin
    HandleLog('Worker not connected', llError);
    Exit;
  end;
  
  // Send command
  if not FWorkerPipe.SendMessage(ipcCommand, Command) then
  begin
    HandleLog('Failed to send command to worker', llError);
    Exit;
  end;
  
  // Wait for response
  TimeoutStart := GetTickCount;
  while True do
  begin
    if FWorkerPipe.ReceiveMessage(MsgType, Response, 100) then
    begin
      case MsgType of
        ipcResponse:
          begin
            Result := Response;
            Break;
          end;
        ipcLog:
          HandleLog(Response, llDebug);
        ipcError:
          begin
            HandleLog('Worker error: ' + Response, llError);
            Result := 'ERROR: ' + Response;
            Break;
          end;
      end;
    end;

    if GetTickCount - TimeoutStart > RESPONSE_TIMEOUT then
    begin
      HandleLog('Worker response timeout', llError);
      Break;
    end;

    Application.ProcessMessages;
  end;
  {$ENDIF}
end;

// Start persistent worker process
procedure TMainForm.StartPersistentWorker;
{$IFDEF IMAGEWRITER_PRO}
var
  Params: TWorkerParams;
  CmdLine: string;
  ExePath: string;
  ExitCode: DWORD;
  Retries: Integer;
  sei: TShellExecuteInfo;
{$ENDIF}
begin
  {$IFDEF IMAGEWRITER_PRO}
  if Assigned(FWorkerPipe) then
    Exit; // Already started

  HandleLog('Starting persistent worker...', llDebug);
  
  // Generate unique pipe name
  FWorkerPipeName := 'ImageWriterPro_' + IntToStr(GetCurrentProcessId) + '_' + IntToStr(GetTickCount);
  
  // Prepare worker parameters
  FillChar(Params, SizeOf(Params), 0);
  Params.PipeName := FWorkerPipeName;
  Params.Persistent := True;
  
  // Build command line
  CmdLine := BuildWorkerCommandLine(Params);
  ExePath := ParamStr(0);
  
  // Run elevated worker process using ShellExecuteEx
  HandleLog('Running elevated worker: ' + ExePath + ' ' + CmdLine, llDebug);
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.Wnd := Handle;
  sei.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI or SEE_MASK_NOCLOSEPROCESS;
  sei.lpVerb := 'runas';
  sei.lpFile := PChar(ExePath);
  sei.lpParameters := PChar(CmdLine);
  sei.nShow := SW_HIDE;

  if not ShellExecuteEx(@sei) then
  begin
    HandleLog('Failed to launch worker process: ' + SysErrorMessage(GetLastError), llError);
    MessageBox(Handle, 'Failed to start elevated worker process.' + #13#10 + 'Administrator privileges are required.', 'Error', MB_OK or MB_ICONERROR);
    Exit;
  end;

  if sei.hProcess = 0 then
  begin
    HandleLog('Worker process handle is invalid', llError);
    Exit;
  end;

  FWorkerProcess := sei.hProcess;
  
  // Create IPC pipe client
  FWorkerPipe := TIPCPipeClient.Create(FWorkerPipeName);
  
  // Wait for worker to start and connect
  HandleLog('Waiting for worker connection...', llDebug);
  Retries := 0;
  while Retries < 50 do
  begin
    if FWorkerPipe.Connect(500) then
    begin
      HandleLog('Worker connected successfully', llDebug);
      
      // Test connection with ping
      if SendWorkerCommand('PING') = 'PONG' then
      begin
        HandleLog('Worker is ready', llInfo);
        Exit;
      end;
    end;
    
    // Check if worker process crashed
    if GetExitCodeProcess(FWorkerProcess, ExitCode) then
    begin
      if ExitCode <> STILL_ACTIVE then
      begin
        HandleLog('Worker process exited with code: ' + IntToStr(ExitCode), llError);
        FreeAndNil(FWorkerPipe);
        FWorkerProcess := 0;
        Exit;
      end;
    end;

    Inc(Retries);
    Application.ProcessMessages;
  end;

  HandleLog('Worker connection timeout', llError);
  FreeAndNil(FWorkerPipe);
  if FWorkerProcess <> 0 then
  begin
    TerminateProcess(FWorkerProcess, 1);
    CloseHandle(FWorkerProcess);
    FWorkerProcess := 0;
  end;
  {$ELSE}
  // Do nothing in base version
  {$ENDIF}
end;

// Stop persistent worker process
procedure TMainForm.StopPersistentWorker;
{$IFDEF IMAGEWRITER_PRO}
var
  ExitCode: DWORD;
  WaitCount: Integer;
{$ENDIF}
begin
  {$IFDEF IMAGEWRITER_PRO}
  if not Assigned(FWorkerPipe) then
    Exit;

  HandleLog('Shutting down worker...', llDebug);
  
  // Send shutdown command
  if FWorkerPipe.Connected then
  begin
    FWorkerPipe.SendMessage(ipcShutdown, '');
    
    // Wait for worker to exit gracefully
    WaitCount := 0;
    while WaitCount < 30 do
    begin
      if GetExitCodeProcess(FWorkerProcess, ExitCode) then
      begin
        if ExitCode <> STILL_ACTIVE then
        begin
          HandleLog('Worker exited gracefully', llDebug);
          Break;
        end;
      end;
      Sleep(100);
      Inc(WaitCount);
    end;
  end;
  
  // Force terminate if still running
  if GetExitCodeProcess(FWorkerProcess, ExitCode) then
  begin
    if ExitCode = STILL_ACTIVE then
    begin
      HandleLog('Force terminating worker', llDebug);
      TerminateProcess(FWorkerProcess, 0);
    end;
  end;

  CloseHandle(FWorkerProcess);
  FWorkerProcess := 0;

  FreeAndNil(FWorkerPipe);
  {$ELSE}
  // Do nothing in base version
  {$ENDIF}
end;

procedure TMainForm.ButtonBenchmarkClick(Sender: TObject);
var
  devicePath: string;
  benchForm: TBenchmarkForm;
begin
  // Get selected device
  if ComboDevice.ItemIndex < 0 then
  begin
    MessageDlg('Please select a device first.', mtWarning, [mbOK], 0);
    Exit;
  end;
  
  devicePath := ComboDevice.Items[ComboDevice.ItemIndex];
  
  // Extract device path (remove description after " - ")
  if Pos(' - ', devicePath) > 0 then
    devicePath := Copy(devicePath, 1, Pos(' - ', devicePath) - 1);
  
  TLogUtils.Info(Format('MainForm: Opening benchmark dialog for device: %s', [devicePath]));
  
  try
    benchForm := TBenchmarkForm.Create(Self);
    try
      benchForm.Initialize(devicePath);
      benchForm.ShowModal;
    finally
      benchForm.Free;
    end;
  except
    on E: Exception do
    begin
      TLogUtils.Error('MainForm: Failed to open benchmark dialog: ' + E.Message);
      MessageDlg('Failed to open benchmark dialog: ' + E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TMainForm.ButtonHealthReportClick(Sender: TObject);
begin
  ShowHealthReport;
end;

procedure TMainForm.ComboDeviceDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Canvas: TCanvas;
  DeviceText: string;
  DiskNumber: Integer;
  HealthStatus: TDeviceHealthStatus;
  StatusIcon: string;
  IconWidth: Integer;
  TextRect: TRect;
begin
  if Index < 0 then
    Exit;
    
  Canvas := TComboBox(Control).Canvas;
  DeviceText := TComboBox(Control).Items[Index];
  
  // Извлечь номер диска из текста
  DiskNumber := ExtractDiskNumberFromCombo(Index);
  HealthStatus := GetDeviceHealthStatus(DiskNumber);
  StatusIcon := GetHealthStatusIcon(HealthStatus);
  
  // Установить цвет фона
  if odSelected in State then
    Canvas.Brush.Color := clHighlight
  else
    Canvas.Brush.Color := GetHealthStatusColor(HealthStatus);
    
  Canvas.FillRect(Rect);
  
  // Нарисовать иконку статуса
  IconWidth := Canvas.TextWidth(StatusIcon + ' ');
  Canvas.Font.Color := clBlack;
  Canvas.TextOut(Rect.Left + 2, Rect.Top + 2, StatusIcon);
  
  // Нарисовать текст устройства
  if odSelected in State then
    Canvas.Font.Color := clHighlightText
  else
    Canvas.Font.Color := clBlack;
    
  TextRect := Rect;
  TextRect.Left := TextRect.Left + IconWidth + 4;
  Canvas.TextOut(TextRect.Left, TextRect.Top + 2, DeviceText);
end;

procedure TMainForm.ComboDeviceMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
begin
  Height := 20; // Высота строки с иконкой
end;

end.

