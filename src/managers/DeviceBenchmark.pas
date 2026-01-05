{******************************************************************************}
{                                                                              }
{  DeviceBenchmark - Device Benchmarking and SMART Data Module                }
{                                                                              }
{  Copyright (c) 2024-2025 Anton Zelenov <tixset@gmail.com>                   }
{  GitHub: https://github.com/tixset/ImageWriter                              }
{                                                                              }
{  This module provides disk benchmarking capabilities and SMART data         }
{  retrieval for health monitoring.                                           }
{                                                                              }
{******************************************************************************}

unit DeviceBenchmark;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, SysUtils, Classes, Math;

type
  /// <summary>
  /// Benchmark test type
  /// </summary>
  TBenchmarkType = (
    btSequentialRead,   // Sequential read test
    btSequentialWrite,  // Sequential write test
    btRandomRead,       // Random read test (4KB blocks)
    btRandomWrite       // Random write test (4KB blocks)
  );

  /// <summary>
  /// Benchmark progress callback
  /// </summary>
  TBenchmarkProgressCallback = procedure(Progress: Integer; Speed: Double; CurrentOp: string) of object;

  /// <summary>
  /// Benchmark result structure
  /// </summary>
  TBenchmarkResult = record
    TestType: TBenchmarkType;
    MinSpeed: Double;       // MB/s
    MaxSpeed: Double;       // MB/s
    AvgSpeed: Double;       // MB/s
    TotalBytes: Int64;
    TotalTime: Double;      // seconds
    Success: Boolean;
    ErrorMessage: string;
  end;

  /// <summary>
  /// SMART attribute structure
  /// </summary>
  TSMARTAttribute = record
    ID: Byte;
    Name: string;
    Value: Byte;
    Worst: Byte;
    Threshold: Byte;
    RawValue: Int64;
    Status: string;  // OK, Warning, Critical
  end;

  /// <summary>
  /// SMART data structure
  /// </summary>
  TSMARTData = record
    Available: Boolean;
    OverallHealth: string;  // Healthy, Warning, Critical
    Temperature: Integer;   // Celsius
    PowerOnHours: Int64;
    PowerCycleCount: Int64;
    AttributeCount: Integer;
    Attributes: array[0..29] of TSMARTAttribute;
  end;

  /// <summary>
  /// Device benchmark and SMART data manager
  /// </summary>
  TDeviceBenchmark = class
  private
    FDevicePath: string;
    FCancelled: Boolean;
    FProgressCallback: TBenchmarkProgressCallback;
    
    function OpenDevice(ReadOnly: Boolean): THandle;
    procedure CloseDevice(DeviceHandle: THandle);
    function PerformSequentialTest(DeviceHandle: THandle; IsWrite: Boolean; var BenchResult: TBenchmarkResult): Boolean;
    function PerformRandomTest(DeviceHandle: THandle; IsWrite: Boolean; var BenchResult: TBenchmarkResult): Boolean;
  public
    /// <summary>
    /// Create benchmark manager for device
    /// </summary>
    constructor Create(const DevicePath: string);
    
    /// <summary>
    /// Run benchmark test
    /// </summary>
    /// <param name="TestType">Type of benchmark test to run</param>
    /// <param name="TestSize">Size of data to test in MB (default: 100MB)</param>
    /// <param name="BenchResult">Output benchmark results</param>
    /// <returns>True if successful</returns>
    function RunBenchmark(TestType: TBenchmarkType; TestSize: Integer; out BenchResult: TBenchmarkResult): Boolean;
    
    /// <summary>
    /// Run all benchmark tests
    /// </summary>
    /// <param name="Results">Array to store all test results</param>
    /// <returns>Number of successful tests</returns>
    function RunAllBenchmarks(out Results: array of TBenchmarkResult): Integer;
    
    /// <summary>
    /// Get SMART data from device
    /// </summary>
    /// <param name="Data">Output SMART data structure</param>
    /// <returns>True if SMART data available and retrieved successfully</returns>
    function GetSMARTData(out Data: TSMARTData): Boolean;
    
    /// <summary>
    /// Cancel running benchmark
    /// </summary>
    procedure Cancel;
    
    /// <summary>
    /// Set progress callback
    /// </summary>
    property OnProgress: TBenchmarkProgressCallback read FProgressCallback write FProgressCallback;
  end;

  /// <summary>
  /// Get SMART attribute name by ID
  /// </summary>
  function GetSMARTAttributeName(AttributeID: Byte): string;
  
  /// <summary>
  /// Format speed value for display
  /// </summary>
  function FormatSpeed(SpeedMBps: Double): string;

implementation

uses
  WinIOCTL, LogUtils;

const
  // Benchmark constants
  BENCHMARK_BLOCK_SIZE = 1024 * 1024; // 1MB blocks for sequential
  BENCHMARK_RANDOM_BLOCK = 4096;      // 4KB blocks for random
  BENCHMARK_ITERATIONS = 10;           // Number of iterations for averaging
  
  // SMART constants
  SMART_CMD = $B0;
  IDENTIFY_CMD = $EC;

{ Helper Functions }

function GetSMARTAttributeName(AttributeID: Byte): string;
begin
  case AttributeID of
    1: Result := 'Raw Read Error Rate';
    3: Result := 'Spin Up Time';
    4: Result := 'Start/Stop Count';
    5: Result := 'Reallocated Sectors Count';
    7: Result := 'Seek Error Rate';
    9: Result := 'Power-On Hours';
    10: Result := 'Spin Retry Count';
    11: Result := 'Calibration Retry Count';
    12: Result := 'Power Cycle Count';
    184: Result := 'End-to-End Error';
    187: Result := 'Reported Uncorrectable Errors';
    188: Result := 'Command Timeout';
    189: Result := 'High Fly Writes';
    190: Result := 'Airflow Temperature';
    194: Result := 'Temperature';
    195: Result := 'Hardware ECC Recovered';
    196: Result := 'Reallocation Event Count';
    197: Result := 'Current Pending Sector Count';
    198: Result := 'Offline Uncorrectable';
    199: Result := 'Ultra DMA CRC Error Count';
    200: Result := 'Write Error Rate';
    241: Result := 'Total LBAs Written';
    242: Result := 'Total LBAs Read';
  else
    Result := Format('Unknown Attribute %d', [AttributeID]);
  end;
end;

function FormatSpeed(SpeedMBps: Double): string;
begin
  if SpeedMBps < 1.0 then
    Result := Format('%.2f KB/s', [SpeedMBps * 1024])
  else if SpeedMBps < 1024.0 then
    Result := Format('%.2f MB/s', [SpeedMBps])
  else
    Result := Format('%.2f GB/s', [SpeedMBps / 1024]);
end;

{ TDeviceBenchmark }

constructor TDeviceBenchmark.Create(const DevicePath: string);
begin
  inherited Create;
  FDevicePath := DevicePath;
  FCancelled := False;
  FProgressCallback := nil;
end;

function TDeviceBenchmark.OpenDevice(ReadOnly: Boolean): THandle;
var
  accessMode: DWORD;
begin
  if ReadOnly then
    accessMode := GENERIC_READ
  else
    accessMode := GENERIC_READ or GENERIC_WRITE;
    
  Result := CreateFile(
    PChar(FDevicePath),
    accessMode,
    FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil,
    OPEN_EXISTING,
    FILE_FLAG_NO_BUFFERING or FILE_FLAG_WRITE_THROUGH,
    0
  );
  
  if Result = INVALID_HANDLE_VALUE then
    TLogUtils.Error(Format('DeviceBenchmark: Failed to open device %s: %s', 
      [FDevicePath, SysErrorMessage(GetLastError)]));
end;

procedure TDeviceBenchmark.CloseDevice(DeviceHandle: THandle);
begin
  if DeviceHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(DeviceHandle);
end;

function TDeviceBenchmark.PerformSequentialTest(DeviceHandle: THandle; IsWrite: Boolean; var BenchResult: TBenchmarkResult): Boolean;
var
  buffer: Pointer;
  bytesProcessed: DWORD;
  totalBytes: Int64;
  testSize: Int64;
  startTime, endTime, iterTime: TDateTime;
  speed: Double;
  speeds: array of Double;
  i: Integer;
begin
  Result := False;
  BenchResult.Success := False;
  BenchResult.TotalBytes := 0;
  BenchResult.TotalTime := 0;
  
  // Allocate aligned buffer
  GetMem(buffer, BENCHMARK_BLOCK_SIZE);
  try
    // Fill buffer with test pattern
    if IsWrite then
      FillChar(buffer^, BENCHMARK_BLOCK_SIZE, $AA);
    
    testSize := BenchResult.TotalBytes;
    SetLength(speeds, BENCHMARK_ITERATIONS);
    
    startTime := Now;
    
    for i := 0 to BENCHMARK_ITERATIONS - 1 do
    begin
      if FCancelled then
        Exit;
        
      iterTime := Now;
      totalBytes := 0;
      
      // Reset file pointer
      SetFilePointer(DeviceHandle, 0, nil, FILE_BEGIN);
      
      while totalBytes < testSize do
      begin
        if FCancelled then
          Exit;
          
        if IsWrite then
        begin
          if not WriteFile(DeviceHandle, buffer^, BENCHMARK_BLOCK_SIZE, bytesProcessed, nil) then
            Break;
        end
        else
        begin
          if not ReadFile(DeviceHandle, buffer^, BENCHMARK_BLOCK_SIZE, bytesProcessed, nil) then
            Break;
        end;
        
        Inc(totalBytes, bytesProcessed);
        
        if Assigned(FProgressCallback) then
        begin
          speed := (totalBytes / (1024 * 1024)) / ((Now - iterTime) * 24 * 60 * 60);
          FProgressCallback(
            Round((i * testSize + totalBytes) * 100 / (BENCHMARK_ITERATIONS * testSize)),
            speed,
            Format('Iteration %d/%d', [i + 1, BENCHMARK_ITERATIONS])
          );
        end;
      end;
      
      // Calculate speed for this iteration
      speeds[i] := (totalBytes / (1024 * 1024)) / ((Now - iterTime) * 24 * 60 * 60);
    end;
    
    endTime := Now;
    
    // Calculate statistics
    BenchResult.MinSpeed := speeds[0];
    BenchResult.MaxSpeed := speeds[0];
    BenchResult.AvgSpeed := 0;
    
    for i := 0 to High(speeds) do
    begin
      BenchResult.AvgSpeed := BenchResult.AvgSpeed + speeds[i];
      if speeds[i] < BenchResult.MinSpeed then
        BenchResult.MinSpeed := speeds[i];
      if speeds[i] > BenchResult.MaxSpeed then
        BenchResult.MaxSpeed := speeds[i];
    end;
    
    BenchResult.AvgSpeed := BenchResult.AvgSpeed / Length(speeds);
    BenchResult.TotalTime := (endTime - startTime) * 24 * 60 * 60;
    BenchResult.Success := True;
    Result := True;
    
  finally
    FreeMem(buffer);
  end;
end;

function TDeviceBenchmark.PerformRandomTest(DeviceHandle: THandle; IsWrite: Boolean; var BenchResult: TBenchmarkResult): Boolean;
var
  buffer: Pointer;
  bytesProcessed: DWORD;
  totalBytes: Int64;
  testSize: Int64;
  startTime, endTime: TDateTime;
  randomPos: Int64;
  maxPos: Int64;
  speed: Double;
begin
  Result := False;
  BenchResult.Success := False;
  BenchResult.TotalBytes := 0;
  BenchResult.TotalTime := 0;
  
  GetMem(buffer, BENCHMARK_RANDOM_BLOCK);
  try
    if IsWrite then
      FillChar(buffer^, BENCHMARK_RANDOM_BLOCK, $55);
    
    testSize := BenchResult.TotalBytes;
    maxPos := testSize - BENCHMARK_RANDOM_BLOCK;
    totalBytes := 0;
    
    startTime := Now;
    
    while totalBytes < testSize do
    begin
      if FCancelled then
        Exit;
        
      // Random position
      randomPos := Random(maxPos div BENCHMARK_RANDOM_BLOCK) * BENCHMARK_RANDOM_BLOCK;
      SetFilePointer(DeviceHandle, randomPos, nil, FILE_BEGIN);
      
      if IsWrite then
      begin
        if not WriteFile(DeviceHandle, buffer^, BENCHMARK_RANDOM_BLOCK, bytesProcessed, nil) then
          Break;
      end
      else
      begin
        if not ReadFile(DeviceHandle, buffer^, BENCHMARK_RANDOM_BLOCK, bytesProcessed, nil) then
          Break;
      end;
      
      Inc(totalBytes, bytesProcessed);
      
      if Assigned(FProgressCallback) and ((totalBytes mod (1024 * 1024)) = 0) then
      begin
        speed := (totalBytes / (1024 * 1024)) / ((Now - startTime) * 24 * 60 * 60);
        FProgressCallback(
          Round(totalBytes * 100 / testSize),
          speed,
          'Random access test'
        );
      end;
    end;
    
    endTime := Now;
    
    BenchResult.TotalBytes := totalBytes;
    BenchResult.TotalTime := (endTime - startTime) * 24 * 60 * 60;
    BenchResult.AvgSpeed := (totalBytes / (1024 * 1024)) / BenchResult.TotalTime;
    BenchResult.MinSpeed := BenchResult.AvgSpeed * 0.8; // Approximation
    BenchResult.MaxSpeed := BenchResult.AvgSpeed * 1.2;
    BenchResult.Success := True;
    Result := True;
    
  finally
    FreeMem(buffer);
  end;
end;

function TDeviceBenchmark.RunBenchmark(TestType: TBenchmarkType; TestSize: Integer; out BenchResult: TBenchmarkResult): Boolean;
var
  deviceHandle: THandle;
  readOnly: Boolean;
  testName: string;
begin
  Result := False;
  BenchResult.TestType := TestType;
  BenchResult.TotalBytes := Int64(TestSize) * 1024 * 1024; // Convert MB to bytes
  BenchResult.Success := False;
  FCancelled := False;
  
  // Get test type name
  case TestType of
    btSequentialRead: testName := 'Sequential Read';
    btSequentialWrite: testName := 'Sequential Write';
    btRandomRead: testName := 'Random Read';
    btRandomWrite: testName := 'Random Write';
  else
    testName := 'Unknown';
  end;
  
  readOnly := (TestType = btSequentialRead) or (TestType = btRandomRead);
  
  TLogUtils.Info(Format('DeviceBenchmark: Starting %s benchmark (test size: %d MB)', 
    [testName, TestSize]));
  
  deviceHandle := OpenDevice(readOnly);
  if deviceHandle = INVALID_HANDLE_VALUE then
  begin
    BenchResult.ErrorMessage := 'Failed to open device';
    Exit;
  end;
  
  try
    case TestType of
      btSequentialRead:
        BenchResult.Success := PerformSequentialTest(deviceHandle, False, BenchResult);
      btSequentialWrite:
        BenchResult.Success := PerformSequentialTest(deviceHandle, True, BenchResult);
      btRandomRead:
        BenchResult.Success := PerformRandomTest(deviceHandle, False, BenchResult);
      btRandomWrite:
        BenchResult.Success := PerformRandomTest(deviceHandle, True, BenchResult);
    end;
    
    if BenchResult.Success then
    begin
      TLogUtils.Info(Format('DeviceBenchmark: Benchmark completed: Avg=%.2f MB/s, Min=%.2f MB/s, Max=%.2f MB/s', 
        [BenchResult.AvgSpeed, BenchResult.MinSpeed, BenchResult.MaxSpeed]));
      Result := True;
    end
    else
      TLogUtils.Warning('DeviceBenchmark: Benchmark failed or cancelled');
      
  finally
    CloseDevice(deviceHandle);
  end;
end;

function TDeviceBenchmark.RunAllBenchmarks(out Results: array of TBenchmarkResult): Integer;
var
  i: Integer;
  testType: TBenchmarkType;
begin
  Result := 0;
  
  for i := 0 to 3 do
  begin
    if FCancelled then
      Break;
      
    testType := TBenchmarkType(i);
    
    if RunBenchmark(testType, 100, Results[i]) then
      Inc(Result);
  end;
end;

function TDeviceBenchmark.GetSMARTData(out Data: TSMARTData): Boolean;
var
  DeviceHandle: THandle;
  InParams: SENDCMDINPARAMS;
  OutParams: array[0..sizeof(SENDCMDOUTPARAMS) + 512 - 1] of Byte;
  BytesReturned: DWORD;
  SmartData: PByte;
  i, attrIndex: Integer;
  AttrID, AttrValue, AttrWorst: Byte;
  RawValue: Int64;
  
  function GetAttributeName(ID: Byte): string;
  begin
    case ID of
      1: Result := 'Read Error Rate';
      5: Result := 'Reallocated Sectors Count';
      9: Result := 'Power-On Hours';
      12: Result := 'Power Cycle Count';
      194: Result := 'Temperature';
      195: Result := 'Hardware ECC Recovered';
      196: Result := 'Reallocation Event Count';
      197: Result := 'Current Pending Sector Count';
      198: Result := 'Offline Uncorrectable Sector Count';
      199: Result := 'UltraDMA CRC Error Count';
      200: Result := 'Write Error Rate';
    else
      Result := 'Attribute ' + IntToStr(ID);
    end;
  end;
  
  function GetAttributeStatus(Value, Worst, Threshold: Byte): string;
  begin
    if Threshold = 0 then
      Result := 'OK'
    else if Value <= Threshold then
      Result := 'CRITICAL'
    else if Value <= Threshold + 10 then
      Result := 'WARNING'
    else
      Result := 'OK';
  end;
  
begin
  // Initialize structure
  FillChar(Data, SizeOf(Data), 0);
  Data.Available := False;
  Data.OverallHealth := 'Unknown';
  Result := False;
  
  TLogUtils.Info('DeviceBenchmark: Attempting to read SMART data from ' + FDevicePath);
  
  // Open device for SMART access
  DeviceHandle := CreateFile(
    PChar(FDevicePath),
    GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil,
    OPEN_EXISTING,
    0,
    0
  );
  
  if DeviceHandle = INVALID_HANDLE_VALUE then
  begin
    TLogUtils.Warning('DeviceBenchmark: Cannot open device for SMART reading (Error: ' + IntToStr(GetLastError) + ')');
    Exit;
  end;
  
  try
    // Prepare SMART READ VALUES command
    FillChar(InParams, SizeOf(InParams), 0);
    FillChar(OutParams, SizeOf(OutParams), 0);
    
    InParams.cBufferSize := 512;
    InParams.irDriveRegs.bFeaturesReg := SMART_READ_VALUES;
    InParams.irDriveRegs.bSectorCountReg := 1;
    InParams.irDriveRegs.bSectorNumberReg := 1;
    InParams.irDriveRegs.bCylLowReg := $4F;  // SMART signature
    InParams.irDriveRegs.bCylHighReg := $C2; // SMART signature
    InParams.irDriveRegs.bDriveHeadReg := $A0;
    InParams.irDriveRegs.bCommandReg := ATA_SMART_CMD;
    
    // Send IOCTL
    if not DeviceIoControl(
      DeviceHandle,
      IOCTL_ATA_PASS_THROUGH,
      @InParams,
      SizeOf(InParams) - 1,
      @OutParams,
      SizeOf(OutParams),
      BytesReturned,
      nil
    ) then
    begin
      TLogUtils.Warning('DeviceBenchmark: SMART command failed (Error: ' + IntToStr(GetLastError) + ')');
      TLogUtils.Info('DeviceBenchmark: This device may not support SMART or requires different access method');
      Exit;
    end;
    
    // Parse SMART data
    SmartData := @PSENDCMDOUTPARAMS(@OutParams)^.bBuffer;
    Data.Available := True;
    Data.OverallHealth := 'Healthy';
    attrIndex := 0;
    
    // SMART attributes start at offset 2, 12 bytes each, up to 30 attributes
    for i := 0 to 29 do
    begin
      AttrID := PByteArray(SmartData)^[2 + i * 12];
      
      // Skip empty attributes (ID = 0)
      if AttrID = 0 then
        Continue;
        
      AttrValue := PByteArray(SmartData)^[2 + i * 12 + 3];
      AttrWorst := PByteArray(SmartData)^[2 + i * 12 + 4];
      
      // Raw value is 6 bytes at offset 5
      RawValue := 0;
      Move(PByteArray(SmartData)^[2 + i * 12 + 5], RawValue, 6);
      
      // Store attribute
      if attrIndex < 30 then
      begin
        Data.Attributes[attrIndex].ID := AttrID;
        Data.Attributes[attrIndex].Name := GetAttributeName(AttrID);
        Data.Attributes[attrIndex].Value := AttrValue;
        Data.Attributes[attrIndex].Worst := AttrWorst;
        Data.Attributes[attrIndex].Threshold := 0; // Will be read separately
        Data.Attributes[attrIndex].RawValue := RawValue;
        Data.Attributes[attrIndex].Status := GetAttributeStatus(AttrValue, AttrWorst, 0);
        
        // Extract specific values
        case AttrID of
          9: Data.PowerOnHours := RawValue;
          12: Data.PowerCycleCount := RawValue;
          194: Data.Temperature := Integer(RawValue and $FF);
        end;
        
        // Update overall health
        if Data.Attributes[attrIndex].Status = 'CRITICAL' then
          Data.OverallHealth := 'CRITICAL'
        else if (Data.Attributes[attrIndex].Status = 'WARNING') and (Data.OverallHealth <> 'CRITICAL') then
          Data.OverallHealth := 'WARNING';
        
        Inc(attrIndex);
      end;
    end;
    
    Data.AttributeCount := attrIndex;
    
    TLogUtils.Info('DeviceBenchmark: SMART data retrieved successfully (' + 
      IntToStr(attrIndex) + ' attributes, Health: ' + Data.OverallHealth + ')');
    Result := True;
    
  finally
    CloseHandle(DeviceHandle);
  end;
end;

procedure TDeviceBenchmark.Cancel;
begin
  FCancelled := True;
  TLogUtils.Info('DeviceBenchmark: Benchmark cancelled by user');
end;

end.
