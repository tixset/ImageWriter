{******************************************************************************}
{                                                                              }
{  ImageWriter - BatchLogHelper Unit Tests                                    }
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
{    Unit tests for BatchLogHelper - batching high-frequency log messages     }
{                                                                              }
{******************************************************************************}

unit BatchLogHelperTests;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  TestFramework, BatchLogHelper, SysUtils, Classes;

type
  TBatchLogHelperTests = class(TTestCase)
  private
    FHelper: TBatchLogHelper;
    FLogMessages: TStringList;
    procedure LogCallback(const Message: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateWithDefaults;
    procedure TestOperationCountBatching;
    procedure TestTimeIntervalBatching;
    procedure TestForceLog;
    procedure TestReset;
    procedure TestBothConditions;
    procedure TestMultipleOperations;
  end;

implementation

{ TBatchLogHelperTests }

procedure TBatchLogHelperTests.SetUp;
begin
  inherited;
  FLogMessages := TStringList.Create;
  FHelper := nil;
end;

procedure TBatchLogHelperTests.TearDown;
begin
  if Assigned(FHelper) then
    FHelper.Free;
  FLogMessages.Free;
  inherited;
end;

procedure TBatchLogHelperTests.LogCallback(const Message: string);
begin
  FLogMessages.Add(Message);
end;

procedure TBatchLogHelperTests.TestCreateWithDefaults;
begin
  // Test: Create with default parameters
  FHelper := TBatchLogHelper.Create('TestOp', 100, 5000);
  
  CheckNotNull(FHelper, 'Helper should be created');
  CheckEquals(0, FLogMessages.Count, 'No logs should be written on creation');
end;

procedure TBatchLogHelperTests.TestOperationCountBatching;
var
  i: Integer;
begin
  // Test: Log only after 10 operations
  FHelper := TBatchLogHelper.Create('Write', 10, 60000);
  
  // First 9 operations - should not log
  for i := 1 to 9 do
  begin
    if FHelper.LogIfNeeded then
      LogCallback(Format('Write: operation %d', [FHelper.OperationCount]));
  end;
  CheckEquals(0, FLogMessages.Count, 'Should not log before reaching operation count');
  
  // 10th operation - should log
  if FHelper.LogIfNeeded then
    LogCallback(Format('Write: operation %d', [FHelper.OperationCount]));
  CheckEquals(1, FLogMessages.Count, 'Should log after 10 operations');
  
  // Next 9 operations - should not log
  for i := 11 to 19 do
  begin
    if FHelper.LogIfNeeded then
      LogCallback(Format('Write: operation %d', [FHelper.OperationCount]));
  end;
  CheckEquals(1, FLogMessages.Count, 'Should not log again before next interval');
  
  // 20th operation - should log again
  if FHelper.LogIfNeeded then
    LogCallback(Format('Write: operation %d', [FHelper.OperationCount]));
  CheckEquals(2, FLogMessages.Count, 'Should log after another 10 operations');
end;

procedure TBatchLogHelperTests.TestTimeIntervalBatching;
begin
  // Test: Log after time interval (simulated)
  FHelper := TBatchLogHelper.Create('Read', 1000, 1000); // 1 second interval
  
  // First call - should log
  if FHelper.LogIfNeeded then
    LogCallback('Read: first operation');
  CheckEquals(1, FLogMessages.Count, 'First operation should log');
  
  // Immediate second call - should not log (time not elapsed)
  if FHelper.LogIfNeeded then
    LogCallback('Read: second operation');
  CheckEquals(1, FLogMessages.Count, 'Should not log immediately');
  
  // Wait 1.1 seconds (simulated by manual time update)
  Sleep(1100);
  
  if FHelper.LogIfNeeded then
    LogCallback('Read: after wait');
  CheckEquals(2, FLogMessages.Count, 'Should log after time interval');
end;

procedure TBatchLogHelperTests.TestForceLog;
var
  i: Integer;
begin
  // Test: ForceLog overrides batching
  FHelper := TBatchLogHelper.Create('Verify', 100, 60000);
  
  // Do 5 operations (less than interval)
  for i := 1 to 5 do
    FHelper.LogIfNeeded;
  
  // Force log
  FHelper.ForceLog;
  LogCallback('Verify: forced log');
  CheckEquals(1, FLogMessages.Count, 'ForceLog should allow logging');
  
  // Next call should not log (interval not reached)
  if FHelper.LogIfNeeded then
    LogCallback('Verify: after force');
  CheckEquals(1, FLogMessages.Count, 'Should not log after ForceLog');
end;

procedure TBatchLogHelperTests.TestReset;
var
  i: Integer;
begin
  // Test: Reset clears counters
  FHelper := TBatchLogHelper.Create('Hash', 10, 60000);
  
  // Do 5 operations
  for i := 1 to 5 do
    FHelper.LogIfNeeded;
  
  CheckEquals(5, FHelper.OperationCount, 'Should have 5 operations');
  
  // Reset
  FHelper.Reset;
  CheckEquals(0, FHelper.OperationCount, 'OperationCount should be reset to 0');
  
  // Next log should happen at operation 10 from reset
  for i := 1 to 9 do
    FHelper.LogIfNeeded;
  CheckEquals(9, FHelper.OperationCount, 'Should have 9 operations after reset');
end;

procedure TBatchLogHelperTests.TestBothConditions;
begin
  // Test: Either condition (operations OR time) triggers log
  FHelper := TBatchLogHelper.Create('Copy', 5, 2000); // 5 ops or 2 seconds
  
  // Do 4 operations - should not log
  FHelper.LogIfNeeded;
  FHelper.LogIfNeeded;
  FHelper.LogIfNeeded;
  FHelper.LogIfNeeded;
  
  // 5th operation - should log
  if FHelper.LogIfNeeded then
    LogCallback('Copy: 5 operations');
  CheckEquals(1, FLogMessages.Count, 'Should log after 5 operations');
  
  // Wait 2+ seconds - should log even with 1 operation
  Sleep(2100);
  if FHelper.LogIfNeeded then
    LogCallback('Copy: after time');
  CheckEquals(2, FLogMessages.Count, 'Should log after time interval');
end;

procedure TBatchLogHelperTests.TestMultipleOperations;
var
  i: Integer;
begin
  // Test: Realistic scenario - 1000 operations with batching
  FHelper := TBatchLogHelper.Create('Write', 100, 5000);
  
  for i := 1 to 1000 do
  begin
    if FHelper.LogIfNeeded then
      LogCallback(Format('Write: operation %d', [i]));
  end;
  
  // Should have logged 10 times (1000 / 100)
  CheckEquals(10, FLogMessages.Count, 
    Format('Should have 10 log messages, got %d', [FLogMessages.Count]));
  
  // Force final log
  FHelper.ForceLog;
  LogCallback('Write: completed');
  CheckEquals(11, FLogMessages.Count, 'Should have 11 messages with final log');
end;

initialization
  RegisterTest(TBatchLogHelperTests.Suite);

end.
