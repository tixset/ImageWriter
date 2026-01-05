{******************************************************************************}
{                                                                              }
{  BenchmarkForm - Device Benchmark and SMART Data UI                         }
{                                                                              }
{  Copyright (c) 2024-2025 Anton Zelenov <tixset@gmail.com>                   }
{  GitHub: https://github.com/tixset/ImageWriter                              }
{                                                                              }
{******************************************************************************}

unit BenchmarkForm;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, DeviceBenchmark, LogUtils;

type
  TBenchmarkForm = class(TForm)
    PageControl: TPageControl;
    TabBenchmark: TTabSheet;
    TabSMART: TTabSheet;
    TabDeviceInfo: TTabSheet;
    
    // Benchmark tab
    GroupTestType: TGroupBox;
    RadioSeqRead: TRadioButton;
    RadioSeqWrite: TRadioButton;
    RadioRandRead: TRadioButton;
    RadioRandWrite: TRadioButton;
    LabelTestSize: TLabel;
    EditTestSize: TEdit;
    ButtonRunTest: TButton;
    ButtonRunAll: TButton;
    
    GroupProgress: TGroupBox;
    LabelCurrentTest: TLabel;
    LabelCurrentTestValue: TLabel;
    LabelCurrentSpeed: TLabel;
    LabelCurrentSpeedValue: TLabel;
    ProgressBar: TProgressBar;
    
    GroupResults: TGroupBox;
    MemoResults: TMemo;
    
    // SMART tab
    GroupSMARTInfo: TGroupBox;
    LabelHealthStatus: TLabel;
    LabelHealthValue: TLabel;
    LabelTemperature: TLabel;
    LabelTempValue: TLabel;
    LabelPowerOn: TLabel;
    LabelPowerOnValue: TLabel;
    ButtonRefreshSMART: TButton;
    ButtonExportSMART: TButton;
    
    GroupSMARTAttributes: TGroupBox;
    ListView: TListView;
    
    // Device Info tab
    GroupDeviceDetails: TGroupBox;
    MemoDeviceInfo: TMemo;
    
    // Common buttons
    ButtonClose: TButton;
    ButtonCancel: TButton;
    
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonRunTestClick(Sender: TObject);
    procedure ButtonRunAllClick(Sender: TObject);
    procedure ButtonRefreshSMARTClick(Sender: TObject);
    procedure ButtonExportSMARTClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure RadioSeqWriteClick(Sender: TObject);
    procedure RadioRandWriteClick(Sender: TObject);
    
  private
    FDevicePath: string;
    FBenchmark: TDeviceBenchmark;
    FRunning: Boolean;
    
    procedure BenchmarkProgress(Progress: Integer; Speed: Double; CurrentOp: string);
    procedure DisplayBenchmarkResult(const Result: TBenchmarkResult);
    procedure DisplaySMARTData(const Data: TSMARTData);
    procedure LoadDeviceInfo;
    procedure UpdateUI(Running: Boolean);
    
  public
    procedure Initialize(const DevicePath: string);
  end;

implementation

{$R *.dfm}

uses
  DateUtils, DeviceManager;

{ TBenchmarkForm }

procedure TBenchmarkForm.FormCreate(Sender: TObject);
begin
  FRunning := False;
  FBenchmark := nil;
  
  MemoResults.Clear;
  MemoResults.Lines.Add('Ready to run benchmarks.');
  MemoResults.Lines.Add('');
  MemoResults.Lines.Add('WARNING: Write tests are destructive and will overwrite data!');
  MemoResults.Lines.Add('Make sure to backup important data before running write tests.');
end;

procedure TBenchmarkForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FBenchmark) then
  begin
    FBenchmark.Cancel;
    FreeAndNil(FBenchmark);
  end;
end;

procedure TBenchmarkForm.Initialize(const DevicePath: string);
begin
  FDevicePath := DevicePath;
  
  Caption := Format('Device Benchmark & Diagnostics - %s', [DevicePath]);
  
  if Assigned(FBenchmark) then
    FreeAndNil(FBenchmark);
    
  FBenchmark := TDeviceBenchmark.Create(DevicePath);
  FBenchmark.OnProgress := BenchmarkProgress;
  
  LoadDeviceInfo;
  
  // Try to load SMART data
  ButtonRefreshSMARTClick(nil);
end;

procedure TBenchmarkForm.BenchmarkProgress(Progress: Integer; Speed: Double; CurrentOp: string);
begin
  ProgressBar.Position := Progress;
  LabelCurrentTestValue.Caption := CurrentOp;
  LabelCurrentSpeedValue.Caption := FormatSpeed(Speed);
  Application.ProcessMessages;
end;

procedure TBenchmarkForm.DisplayBenchmarkResult(const Result: TBenchmarkResult);
var
  testName: string;
begin
  case Result.TestType of
    btSequentialRead: testName := 'Sequential Read';
    btSequentialWrite: testName := 'Sequential Write';
    btRandomRead: testName := 'Random Read (4KB)';
    btRandomWrite: testName := 'Random Write (4KB)';
  else
    testName := 'Unknown Test';
  end;
  
  MemoResults.Lines.Add('');
  MemoResults.Lines.Add('========================================');
  MemoResults.Lines.Add(Format('Test: %s', [testName]));
  MemoResults.Lines.Add('========================================');
  
  if Result.Success then
  begin
    MemoResults.Lines.Add(Format('Status:        SUCCESS', []));
    MemoResults.Lines.Add(Format('Average Speed: %s', [FormatSpeed(Result.AvgSpeed)]));
    MemoResults.Lines.Add(Format('Minimum Speed: %s', [FormatSpeed(Result.MinSpeed)]));
    MemoResults.Lines.Add(Format('Maximum Speed: %s', [FormatSpeed(Result.MaxSpeed)]));
    MemoResults.Lines.Add(Format('Total Bytes:   %.2f MB', [Result.TotalBytes / (1024 * 1024)]));
    MemoResults.Lines.Add(Format('Total Time:    %.2f seconds', [Result.TotalTime]));
  end
  else
  begin
    MemoResults.Lines.Add(Format('Status:        FAILED', []));
    MemoResults.Lines.Add(Format('Error:         %s', [Result.ErrorMessage]));
  end;
  
  MemoResults.Lines.Add('');
end;

procedure TBenchmarkForm.DisplaySMARTData(const Data: TSMARTData);
var
  i: Integer;
  item: TListItem;
begin
  if not Data.Available then
  begin
    LabelHealthValue.Caption := 'Not Available';
    LabelHealthValue.Font.Color := clGray;
    LabelTempValue.Caption := 'N/A';
    LabelPowerOnValue.Caption := 'N/A';
    
    ListView.Items.Clear;
    Exit;
  end;
  
  // Update health status
  LabelHealthValue.Caption := Data.OverallHealth;
  
  if Data.OverallHealth = 'Healthy' then
    LabelHealthValue.Font.Color := clGreen
  else if Data.OverallHealth = 'Warning' then
    LabelHealthValue.Font.Color := $0080FF  // Orange
  else
    LabelHealthValue.Font.Color := clRed;
    
  // Update temperature
  if Data.Temperature > 0 then
    LabelTempValue.Caption := Format('%d °C', [Data.Temperature])
  else
    LabelTempValue.Caption := 'N/A';
    
  // Update power-on hours
  if Data.PowerOnHours > 0 then
    LabelPowerOnValue.Caption := Format('%d hours (%.1f days)', 
      [Data.PowerOnHours, Data.PowerOnHours / 24.0])
  else
    LabelPowerOnValue.Caption := 'N/A';
    
  // Update attributes list
  ListView.Items.Clear;
  
  for i := 0 to Data.AttributeCount - 1 do
  begin
    item := ListView.Items.Add;
    item.Caption := IntToStr(Data.Attributes[i].ID);
    item.SubItems.Add(Data.Attributes[i].Name);
    item.SubItems.Add(IntToStr(Data.Attributes[i].Value));
    item.SubItems.Add(IntToStr(Data.Attributes[i].Worst));
    item.SubItems.Add(IntToStr(Data.Attributes[i].Threshold));
    item.SubItems.Add(IntToStr(Data.Attributes[i].RawValue));
    item.SubItems.Add(Data.Attributes[i].Status);
    
    // Color code status
    if Data.Attributes[i].Status = 'OK' then
      item.SubItemImages[5] := -1  // No icon
    else if Data.Attributes[i].Status = 'Warning' then
      item.SubItemImages[5] := -1
    else if Data.Attributes[i].Status = 'Critical' then
      item.SubItemImages[5] := -1;
  end;
end;

procedure TBenchmarkForm.LoadDeviceInfo;
begin
  MemoDeviceInfo.Clear;
  MemoDeviceInfo.Lines.Add('Device Information');
  MemoDeviceInfo.Lines.Add('==================');
  MemoDeviceInfo.Lines.Add('');
  MemoDeviceInfo.Lines.Add(Format('Device Path: %s', [FDevicePath]));
  MemoDeviceInfo.Lines.Add('');
  MemoDeviceInfo.Lines.Add('Note: Extended device information (manufacturer, serial number,');
  MemoDeviceInfo.Lines.Add('interface type, capacity) can be queried via WMI or IOCTL.');
  MemoDeviceInfo.Lines.Add('This requires additional implementation.');
end;

procedure TBenchmarkForm.UpdateUI(Running: Boolean);
begin
  FRunning := Running;
  
  ButtonRunTest.Enabled := not Running;
  ButtonRunAll.Enabled := not Running;
  ButtonCancel.Enabled := Running;
  
  RadioSeqRead.Enabled := not Running;
  RadioSeqWrite.Enabled := not Running;
  RadioRandRead.Enabled := not Running;
  RadioRandWrite.Enabled := not Running;
  EditTestSize.Enabled := not Running;
  
  if not Running then
  begin
    ProgressBar.Position := 0;
    LabelCurrentTestValue.Caption := 'Ready';
    LabelCurrentSpeedValue.Caption := '0.00 MB/s';
  end;
end;

procedure TBenchmarkForm.ButtonRunTestClick(Sender: TObject);
var
  testType: TBenchmarkType;
  testSize: Integer;
  result: TBenchmarkResult;
begin
  if FRunning then
    Exit;
    
  // Determine test type
  if RadioSeqRead.Checked then
    testType := btSequentialRead
  else if RadioSeqWrite.Checked then
    testType := btSequentialWrite
  else if RadioRandRead.Checked then
    testType := btRandomRead
  else
    testType := btRandomWrite;
    
  // Get test size
  testSize := StrToIntDef(EditTestSize.Text, 100);
  if (testSize < 1) or (testSize > 10000) then
  begin
    MessageDlg('Test size must be between 1 and 10000 MB.', mtError, [mbOK], 0);
    Exit;
  end;
  
  // Confirm destructive tests
  if (testType = btSequentialWrite) or (testType = btRandomWrite) then
  begin
    if MessageDlg('WARNING: This test will OVERWRITE data on the device!' + #13#10 +
                  'All existing data will be lost!' + #13#10#13#10 +
                  'Are you sure you want to continue?',
                  mtWarning, [mbYes, mbNo], 0) <> mrYes then
      Exit;
  end;
  
  UpdateUI(True);
  
  try
    if FBenchmark.RunBenchmark(testType, testSize, result) then
      DisplayBenchmarkResult(result)
    else
      MessageDlg('Benchmark failed. Check log for details.', mtError, [mbOK], 0);
  finally
    UpdateUI(False);
  end;
end;

procedure TBenchmarkForm.ButtonRunAllClick(Sender: TObject);
var
  results: array[0..3] of TBenchmarkResult;
  successCount: Integer;
  i: Integer;
begin
  if FRunning then
    Exit;
    
  if MessageDlg('Run all benchmark tests?' + #13#10 +
                'This will take several minutes.' + #13#10#13#10 +
                'Note: Write tests are DESTRUCTIVE!',
                mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;
    
  UpdateUI(True);
  MemoResults.Clear;
  
  try
    successCount := FBenchmark.RunAllBenchmarks(results);
    
    for i := 0 to 3 do
      DisplayBenchmarkResult(results[i]);
      
    MemoResults.Lines.Add('');
    MemoResults.Lines.Add(Format('Completed: %d/4 tests successful', [successCount]));
    
  finally
    UpdateUI(False);
  end;
end;

procedure TBenchmarkForm.ButtonRefreshSMARTClick(Sender: TObject);
var
  smartData: TSMARTData;
begin
  if not Assigned(FBenchmark) then
    Exit;
    
  if FBenchmark.GetSMARTData(smartData) then
    DisplaySMARTData(smartData)
  else
  begin
    LabelHealthValue.Caption := 'Not Supported';
    LabelHealthValue.Font.Color := clGray;
    
    MessageDlg('SMART data is not available for this device.' + #13#10 +
               'This feature requires ATA/SATA devices and may not work with USB adapters.',
               mtInformation, [mbOK], 0);
  end;
end;

procedure TBenchmarkForm.ButtonExportSMARTClick(Sender: TObject);
var
  saveDialog: TSaveDialog;
  i: Integer;
  outputFile: TextFile;
  smartData: TSMARTData;
begin
  if not Assigned(FBenchmark) then
    Exit;
    
  saveDialog := TSaveDialog.Create(nil);
  try
    saveDialog.Filter := 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*';
    saveDialog.DefaultExt := 'txt';
    saveDialog.FileName := 'SMART_Data.txt';
    
    if saveDialog.Execute then
    begin
      if FBenchmark.GetSMARTData(smartData) and smartData.Available then
      begin
        AssignFile(outputFile, saveDialog.FileName);
        try
          Rewrite(outputFile);
          
          WriteLn(outputFile, 'SMART Data Export');
          WriteLn(outputFile, '=================');
          WriteLn(outputFile, 'Device: ', FDevicePath);
          WriteLn(outputFile, 'Date: ', DateTimeToStr(Now));
          WriteLn(outputFile, '');
          WriteLn(outputFile, 'Overall Health: ', smartData.OverallHealth);
          WriteLn(outputFile, 'Temperature: ', smartData.Temperature, ' °C');
          WriteLn(outputFile, 'Power-On Hours: ', smartData.PowerOnHours);
          WriteLn(outputFile, 'Power Cycle Count: ', smartData.PowerCycleCount);
          WriteLn(outputFile, '');
          WriteLn(outputFile, 'Attributes:');
          WriteLn(outputFile, '-----------');
          
          for i := 0 to smartData.AttributeCount - 1 do
          begin
            WriteLn(outputFile, Format('ID %3d: %-40s Value: %3d Worst: %3d Threshold: %3d Raw: %10d Status: %s',
              [smartData.Attributes[i].ID,
               smartData.Attributes[i].Name,
               smartData.Attributes[i].Value,
               smartData.Attributes[i].Worst,
               smartData.Attributes[i].Threshold,
               smartData.Attributes[i].RawValue,
               smartData.Attributes[i].Status]));
          end;
          
          CloseFile(outputFile);
          MessageDlg('SMART data exported successfully.', mtInformation, [mbOK], 0);
          
        except
          on E: Exception do
            MessageDlg('Failed to export SMART data: ' + E.Message, mtError, [mbOK], 0);
        end;
      end
      else
        MessageDlg('No SMART data available to export.', mtWarning, [mbOK], 0);
    end;
  finally
    saveDialog.Free;
  end;
end;

procedure TBenchmarkForm.ButtonCancelClick(Sender: TObject);
begin
  if FRunning and Assigned(FBenchmark) then
  begin
    FBenchmark.Cancel;
    UpdateUI(False);
  end;
end;

procedure TBenchmarkForm.RadioSeqWriteClick(Sender: TObject);
begin
  if RadioSeqWrite.Checked then
    RadioSeqWrite.Font.Color := clRed
  else
    RadioSeqWrite.Font.Color := clWindowText;
end;

procedure TBenchmarkForm.RadioRandWriteClick(Sender: TObject);
begin
  if RadioRandWrite.Checked then
    RadioRandWrite.Font.Color := clRed
  else
    RadioRandWrite.Font.Color := clWindowText;
end;

end.
