{******************************************************************************}
{                                                                              }
{  ImageWriter - Automated Test Suite                                         }
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
{    Main test runner for ImageWriter automated tests.                        }
{    Uses DUnit framework for unit testing.                                   }
{                                                                              }
{******************************************************************************}

program ImageWriterTests;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  TestFramework,
  TextTestRunner,
  DeviceDetectionTests in 'DeviceDetectionTests.pas',
  SafeOperationsTests in 'SafeOperationsTests.pas',
  ZLibTests in 'ZLibTests.pas',
  BatchLogHelperTests in 'BatchLogHelperTests.pas',
  ArchivePartitionReaderTests in 'ArchivePartitionReaderTests.pas',
  DeviceBenchmarkTests in 'DeviceBenchmarkTests.pas',
  OperationUIHelperTests in 'OperationUIHelperTests.pas';

begin
  WriteLn('==============================================');
  WriteLn('  ImageWriter Automated Test Suite');
  WriteLn('  WARNING: Tests require removable USB device');
  WriteLn('  Tests will NOT run on fixed disks');
  WriteLn('==============================================');
  WriteLn;
  
  try
    TextTestRunner.RunRegisteredTests(rxbHaltOnFailures);
  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
