program LogUtilsFeatureTest;

{$APPTYPE CONSOLE}

uses
  SysUtils, LogUtils in '..\src\managers\LogUtils.pas';

procedure TestBasicLogging;
begin
  WriteLn('=== Testing Basic Logging ===');
  TLogUtils.Initialize('test.log', True);
  
  TLogUtils.Debug('This is a debug message');
  TLogUtils.Info('This is an info message');
  TLogUtils.Warning('This is a warning message');
  TLogUtils.Error('This is an error message');
  TLogUtils.Critical('This is a critical message');
  
  WriteLn('Basic logging test completed. Check test.log');
end;

procedure TestMinimumLevel;
begin
  WriteLn('');
  WriteLn('=== Testing Minimum Level Filter ===');
  
  // Set minimum level to Warning - should filter out Debug and Info
  TLogUtils.SetMinimumLevel(llWarning);
  WriteLn('Set minimum level to Warning');
  
  TLogUtils.Debug('This debug should NOT appear');
  TLogUtils.Info('This info should NOT appear');
  TLogUtils.Warning('This warning SHOULD appear');
  TLogUtils.Error('This error SHOULD appear');
  
  WriteLn('Minimum level filter test completed');
  
  // Reset to Debug
  TLogUtils.SetMinimumLevel(llDebug);
end;

procedure TestLogRotation;
var
  i: Integer;
  LargeString: string;
begin
  WriteLn('');
  WriteLn('=== Testing Log Rotation ===');
  
  // Enable rotation with max file size of 1KB
  TLogUtils.SetMaxFileSize(1024);  // 1 KB
  TLogUtils.SetLogRotation(True, 3);  // Keep 3 backup files
  WriteLn('Enabled log rotation (1KB max, 3 backups)');
  
  // Generate large log entries to trigger rotation
  SetLength(LargeString, 200);
  FillChar(LargeString[1], 200, 'X');
  
  for i := 1 to 10 do
  begin
    TLogUtils.Info(Format('Large entry %d: %s', [i, LargeString]));
  end;
  
  WriteLn('Log rotation test completed');
  WriteLn('Check for test.log, test.1.log, test.2.log, test.3.log');
  
  // Check file sizes
  if FileExists('test.log') then
    WriteLn('test.log size: ' + IntToStr(TLogUtils.GetLogFileSize) + ' bytes');
end;

procedure TestFormattedLogging;
begin
  WriteLn('');
  WriteLn('=== Testing Formatted Logging ===');
  
  TLogUtils.LogFmt('Formatted message: %s = %d', ['Answer', 42], llInfo);
  TLogUtils.LogFmt('Multiple values: %d, %s, %.2f', [123, 'test', 3.14159], llDebug);
  
  WriteLn('Formatted logging test completed');
end;

begin
  WriteLn('ImageWriter LogUtils Feature Test');
  WriteLn('==================================');
  WriteLn('');
  
  try
    TestBasicLogging;
    TestMinimumLevel;
    TestFormattedLogging;
    TestLogRotation;
    
    WriteLn('');
    WriteLn('All tests completed successfully!');
    WriteLn('Press Enter to exit...');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ' + E.Message);
      ReadLn;
    end;
  end;
end.
