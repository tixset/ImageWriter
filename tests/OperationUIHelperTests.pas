{******************************************************************************}
{                                                                              }
{  ImageWriter - OperationUIHelper Unit Tests                                 }
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
{    Unit tests for OperationUIHelper - UI operation helper functions         }
{                                                                              }
{******************************************************************************}

unit OperationUIHelperTests;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  TestFramework, OperationUIHelper, SysUtils, Classes, Controls, ComCtrls;

type
  TOperationUIHelperTests = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFormatBytes;
    procedure TestFormatSpeed;
    procedure TestFormatTime;
    procedure TestCalculateETA;
    procedure TestFormatProgress;
    procedure TestBuildStatusMessage;
    procedure TestValidateNumericInput;
    procedure TestParseBlockSize;
  end;

implementation

{ TOperationUIHelperTests }

procedure TOperationUIHelperTests.SetUp;
begin
  inherited;
end;

procedure TOperationUIHelperTests.TearDown;
begin
  inherited;
end;

procedure TOperationUIHelperTests.TestFormatBytes;
var
  Result: string;
begin
  // Test: Format bytes to human-readable format
  
  Result := TOperationUIHelper.FormatBytes(512);
  CheckEquals('512 B', Result, 'Should format 512 bytes');
  
  Result := TOperationUIHelper.FormatBytes(1024);
  CheckEquals('1.00 KB', Result, 'Should format 1 KB');
  
  Result := TOperationUIHelper.FormatBytes(1024 * 1024);
  CheckEquals('1.00 MB', Result, 'Should format 1 MB');
  
  Result := TOperationUIHelper.FormatBytes(1024 * 1024 * 1024);
  CheckEquals('1.00 GB', Result, 'Should format 1 GB');
  
  Result := TOperationUIHelper.FormatBytes(Int64(1024) * 1024 * 1024 * 1024);
  CheckEquals('1.00 TB', Result, 'Should format 1 TB');
end;

procedure TOperationUIHelperTests.TestFormatSpeed;
var
  Result: string;
begin
  // Test: Format transfer speed
  
  Result := TOperationUIHelper.FormatSpeed(512.0);
  CheckEquals('512.00 B/s', Result, 'Should format bytes per second');
  
  Result := TOperationUIHelper.FormatSpeed(1024.0 * 1024);
  CheckEquals('1.00 MB/s', Result, 'Should format MB/s');
  
  Result := TOperationUIHelper.FormatSpeed(15.5 * 1024 * 1024);
  CheckEquals('15.50 MB/s', Result, 'Should format decimal MB/s');
end;

procedure TOperationUIHelperTests.TestFormatTime;
var
  Result: string;
begin
  // Test: Format time duration
  
  Result := TOperationUIHelper.FormatTime(30);
  CheckEquals('00:00:30', Result, 'Should format 30 seconds');
  
  Result := TOperationUIHelper.FormatTime(90);
  CheckEquals('00:01:30', Result, 'Should format 1 minute 30 seconds');
  
  Result := TOperationUIHelper.FormatTime(3661);
  CheckEquals('01:01:01', Result, 'Should format 1 hour 1 minute 1 second');
  
  Result := TOperationUIHelper.FormatTime(86400);
  CheckEquals('24:00:00', Result, 'Should format 24 hours');
end;

procedure TOperationUIHelperTests.TestCalculateETA;
var
  ETA: Integer;
begin
  // Test: Calculate estimated time to completion
  
  // 50% complete, 60 seconds elapsed -> 60 seconds remaining
  ETA := TOperationUIHelper.CalculateETA(50, 100, 60);
  CheckEquals(60, ETA, 'Should calculate 60 seconds remaining');
  
  // 25% complete, 30 seconds elapsed -> 90 seconds remaining
  ETA := TOperationUIHelper.CalculateETA(25, 100, 30);
  CheckEquals(90, ETA, 'Should calculate 90 seconds remaining');
  
  // 0% complete -> should return 0 or max value
  ETA := TOperationUIHelper.CalculateETA(0, 100, 30);
  CheckTrue(ETA >= 0, 'Should handle 0% completion');
  
  // 100% complete -> should return 0
  ETA := TOperationUIHelper.CalculateETA(100, 100, 60);
  CheckEquals(0, ETA, 'Should return 0 for completed operation');
end;

procedure TOperationUIHelperTests.TestFormatProgress;
var
  Result: string;
begin
  // Test: Format progress percentage
  
  Result := TOperationUIHelper.FormatProgress(50, 100);
  CheckEquals('50%', Result, 'Should format 50%');
  
  Result := TOperationUIHelper.FormatProgress(33, 100);
  CheckEquals('33%', Result, 'Should format 33%');
  
  Result := TOperationUIHelper.FormatProgress(100, 100);
  CheckEquals('100%', Result, 'Should format 100%');
  
  Result := TOperationUIHelper.FormatProgress(0, 100);
  CheckEquals('0%', Result, 'Should format 0%');
end;

procedure TOperationUIHelperTests.TestBuildStatusMessage;
var
  Result: string;
begin
  // Test: Build comprehensive status message
  
  Result := TOperationUIHelper.BuildStatusMessage(
    'Writing',
    50 * 1024 * 1024,  // 50 MB processed
    100 * 1024 * 1024, // 100 MB total
    10.5 * 1024 * 1024, // 10.5 MB/s speed
    60  // 60 seconds elapsed
  );
  
  CheckTrue(Pos('Writing', Result) > 0, 'Should contain operation name');
  CheckTrue(Pos('50', Result) > 0, 'Should contain progress percentage');
  CheckTrue(Pos('MB/s', Result) > 0, 'Should contain speed');
end;

procedure TOperationUIHelperTests.TestValidateNumericInput;
var
  Valid: Boolean;
  Value: Integer;
begin
  // Test: Validate numeric input
  
  Valid := TOperationUIHelper.ValidateNumericInput('123', Value);
  CheckTrue(Valid, 'Should validate valid number');
  CheckEquals(123, Value, 'Should parse correct value');
  
  Valid := TOperationUIHelper.ValidateNumericInput('0', Value);
  CheckTrue(Valid, 'Should validate zero');
  CheckEquals(0, Value, 'Should parse zero');
  
  Valid := TOperationUIHelper.ValidateNumericInput('abc', Value);
  CheckFalse(Valid, 'Should reject non-numeric input');
  
  Valid := TOperationUIHelper.ValidateNumericInput('', Value);
  CheckFalse(Valid, 'Should reject empty input');
  
  Valid := TOperationUIHelper.ValidateNumericInput('-5', Value);
  CheckFalse(Valid, 'Should reject negative numbers');
end;

procedure TOperationUIHelperTests.TestParseBlockSize;
var
  Size: Int64;
begin
  // Test: Parse block size with suffixes
  
  Size := TOperationUIHelper.ParseBlockSize('512');
  CheckEquals(512, Size, 'Should parse plain number');
  
  Size := TOperationUIHelper.ParseBlockSize('1K');
  CheckEquals(1024, Size, 'Should parse KB suffix');
  
  Size := TOperationUIHelper.ParseBlockSize('1M');
  CheckEquals(1024 * 1024, Size, 'Should parse MB suffix');
  
  Size := TOperationUIHelper.ParseBlockSize('1G');
  CheckEquals(Int64(1024) * 1024 * 1024, Size, 'Should parse GB suffix');
  
  Size := TOperationUIHelper.ParseBlockSize('invalid');
  CheckEquals(-1, Size, 'Should return -1 for invalid input');
end;

initialization
  RegisterTest(TOperationUIHelperTests.Suite);

end.
