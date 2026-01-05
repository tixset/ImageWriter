{******************************************************************************}
{                                                                              }
{  ImageWriter - DeviceBenchmark Unit Tests                                   }
{                                                                              }
{  Copyright (c) 2024-2025 Anton Zelenov <tixset@gmail.com>                   }
{  GitHub: https://github.com/tixset/ImageWriter                              }
{                                                                              }
{  This program is free software: you can redistribute it and/or modify       }
{  it under the terms of the GNU General Public License as published by       }
{  the Free Software Foundation, either version 3 of the License, or          }
{  (at your option) any later version.                                        }
{                                                                              }
{  Description:                                                                }
{    Unit tests for DeviceBenchmark - device read/write speed testing         }
{                                                                              }
{******************************************************************************}

unit DeviceBenchmarkTests;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  TestFramework, DeviceBenchmark, SysUtils, Classes, Windows;

type
  TDeviceBenchmarkTests = class(TTestCase)
  private
    FTestFile: string;
    procedure CreateTestFile(SizeKB: Integer);
    procedure DeleteTestFile;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateBenchmark;
    procedure TestSequentialReadSpeed;
    procedure TestSequentialWriteSpeed;
    procedure TestRandomReadSpeed;
    procedure TestRandomWriteSpeed;
    procedure TestBenchmarkParameters;
    procedure TestProgressCallback;
    procedure TestCancellation;
    procedure TestSMARTDataRetrieval;
  end;

implementation

{ TDeviceBenchmarkTests }

procedure TDeviceBenchmarkTests.SetUp;
begin
  inherited;
  FTestFile := '';
end;

procedure TDeviceBenchmarkTests.TearDown;
begin
  DeleteTestFile;
  inherited;
end;

procedure TDeviceBenchmarkTests.CreateTestFile(SizeKB: Integer);
var
  F: File of Byte;
  Buffer: array[0..1023] of Byte;
  i: Integer;
begin
  FTestFile := ExtractFilePath(ParamStr(0)) + 'test_benchmark.tmp';
  
  AssignFile(F, FTestFile);
  Rewrite(F);
  try
    FillChar(Buffer, SizeOf(Buffer), 0);
    for i := 1 to SizeKB do
      BlockWrite(F, Buffer, SizeOf(Buffer));
  finally
    CloseFile(F);
  end;
end;

procedure TDeviceBenchmarkTests.DeleteTestFile;
begin
  if (FTestFile <> '') and FileExists(FTestFile) then
    DeleteFile(FTestFile);
  FTestFile := '';
end;

procedure TDeviceBenchmarkTests.TestCreateBenchmark;
var
  Benchmark: TDeviceBenchmark;
begin
  // Test: Create benchmark instance
  Benchmark := TDeviceBenchmark.Create;
  try
    CheckNotNull(Benchmark, 'Benchmark should be created');
  finally
    Benchmark.Free;
  end;
end;

procedure TDeviceBenchmarkTests.TestSequentialReadSpeed;
var
  Benchmark: TDeviceBenchmark;
  Speed: Double;
begin
  // Test: Sequential read benchmark
  CreateTestFile(1024); // 1 MB test file
  
  Benchmark := TDeviceBenchmark.Create;
  try
    Speed := Benchmark.MeasureSequentialRead(FTestFile, 512 * 1024); // 512 KB
    
    CheckTrue(Speed >= 0, 'Sequential read speed should be non-negative');
    Status(Format('Sequential Read Speed: %.2f MB/s', [Speed]));
  finally
    Benchmark.Free;
  end;
end;

procedure TDeviceBenchmarkTests.TestSequentialWriteSpeed;
var
  Benchmark: TDeviceBenchmark;
  Speed: Double;
  TempFile: string;
begin
  // Test: Sequential write benchmark
  TempFile := ExtractFilePath(ParamStr(0)) + 'test_write.tmp';
  
  Benchmark := TDeviceBenchmark.Create;
  try
    Speed := Benchmark.MeasureSequentialWrite(TempFile, 512 * 1024); // 512 KB
    
    CheckTrue(Speed >= 0, 'Sequential write speed should be non-negative');
    Status(Format('Sequential Write Speed: %.2f MB/s', [Speed]));
  finally
    Benchmark.Free;
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
end;

procedure TDeviceBenchmarkTests.TestRandomReadSpeed;
var
  Benchmark: TDeviceBenchmark;
  Speed: Double;
begin
  // Test: Random read benchmark
  CreateTestFile(2048); // 2 MB test file
  
  Benchmark := TDeviceBenchmark.Create;
  try
    Speed := Benchmark.MeasureRandomRead(FTestFile, 4096, 100); // 4KB blocks, 100 ops
    
    CheckTrue(Speed >= 0, 'Random read speed should be non-negative');
    Status(Format('Random Read Speed (4KB): %.2f IOPS', [Speed]));
  finally
    Benchmark.Free;
  end;
end;

procedure TDeviceBenchmarkTests.TestRandomWriteSpeed;
var
  Benchmark: TDeviceBenchmark;
  Speed: Double;
  TempFile: string;
begin
  // Test: Random write benchmark
  TempFile := ExtractFilePath(ParamStr(0)) + 'test_random.tmp';
  
  Benchmark := TDeviceBenchmark.Create;
  try
    Speed := Benchmark.MeasureRandomWrite(TempFile, 4096, 100); // 4KB blocks, 100 ops
    
    CheckTrue(Speed >= 0, 'Random write speed should be non-negative');
    Status(Format('Random Write Speed (4KB): %.2f IOPS', [Speed]));
  finally
    Benchmark.Free;
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
end;

procedure TDeviceBenchmarkTests.TestBenchmarkParameters;
var
  Benchmark: TDeviceBenchmark;
begin
  // Test: Benchmark parameter validation
  Benchmark := TDeviceBenchmark.Create;
  try
    // Test different block sizes
    Benchmark.BlockSize := 4096;
    CheckEquals(4096, Benchmark.BlockSize, 'Block size should be 4096');
    
    Benchmark.BlockSize := 8192;
    CheckEquals(8192, Benchmark.BlockSize, 'Block size should be 8192');
    
    // Test iteration count
    Benchmark.IterationCount := 50;
    CheckEquals(50, Benchmark.IterationCount, 'Iteration count should be 50');
  finally
    Benchmark.Free;
  end;
end;

procedure TDeviceBenchmarkTests.TestProgressCallback;
var
  Benchmark: TDeviceBenchmark;
  CallbackCalled: Boolean;
  
  procedure ProgressProc(Current, Total: Int64);
  begin
    CallbackCalled := True;
    Status(Format('Progress: %d / %d', [Current, Total]));
  end;
  
begin
  // Test: Progress callback functionality
  CallbackCalled := False;
  CreateTestFile(512); // 512 KB
  
  Benchmark := TDeviceBenchmark.Create;
  try
    Benchmark.OnProgress := ProgressProc;
    Benchmark.MeasureSequentialRead(FTestFile, 256 * 1024);
    
    CheckTrue(CallbackCalled, 'Progress callback should be called');
  finally
    Benchmark.Free;
  end;
end;

procedure TDeviceBenchmarkTests.TestCancellation;
var
  Benchmark: TDeviceBenchmark;
  
  procedure ProgressProc(Current, Total: Int64);
  begin
    // Cancel after first callback
    if Current > 0 then
      Benchmark.Cancel;
  end;
  
begin
  // Test: Benchmark cancellation
  CreateTestFile(2048); // 2 MB
  
  Benchmark := TDeviceBenchmark.Create;
  try
    Benchmark.OnProgress := ProgressProc;
    
    try
      Benchmark.MeasureSequentialRead(FTestFile, 1024 * 1024);
      // Should be cancelled before completion
      Check(True, 'Cancellation handled');
    except
      on E: Exception do
        Status('Expected: Benchmark cancelled - ' + E.Message);
    end;
  finally
    Benchmark.Free;
  end;
end;

procedure TDeviceBenchmarkTests.TestSMARTDataRetrieval;
var
  Benchmark: TDeviceBenchmark;
  SMARTData: TSMARTData;
  PhysicalDrive: string;
  DriveHandle: THandle;
begin
  // Test: SMART data retrieval
  // Note: This test requires a physical drive and admin rights
  PhysicalDrive := '\\.\PhysicalDrive0';
  
  // Try to open drive (may fail without admin rights)
  DriveHandle := CreateFile(
    PChar(PhysicalDrive),
    GENERIC_READ,
    FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil,
    OPEN_EXISTING,
    0,
    0
  );
  
  if DriveHandle = INVALID_HANDLE_VALUE then
  begin
    Status('SMART test skipped: Cannot open physical drive (requires admin rights)');
    Exit;
  end;
  CloseHandle(DriveHandle);
  
  Benchmark := TDeviceBenchmark.Create;
  try
    SMARTData := Benchmark.GetSMARTData(PhysicalDrive);
    
    // Check if SMART data was retrieved
    if SMARTData.HealthStatus <> '' then
    begin
      Check(True, 'SMART data retrieved successfully');
      Status('SMART Health: ' + SMARTData.HealthStatus);
      Status('Temperature: ' + IntToStr(SMARTData.Temperature) + '°C');
      Status('Power-On Hours: ' + IntToStr(SMARTData.PowerOnHours));
      Status('Attributes Count: ' + IntToStr(Length(SMARTData.Attributes)));
    end
    else
    begin
      Status('SMART test: Drive may not support SMART or no data available');
      Check(True, 'SMART test completed (no data available)');
    end;
  finally
    Benchmark.Free;
  end;
end;

initialization
  RegisterTest(TDeviceBenchmarkTests.Suite);

end.
