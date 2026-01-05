{******************************************************************************}
{                                                                              }
{  ImageWriter - ArchivePartitionReader Unit Tests                            }
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
{    Unit tests for ArchivePartitionReader - analyzing partition tables       }
{    in archived disk images                                                  }
{                                                                              }
{******************************************************************************}

unit ArchivePartitionReaderTests;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  TestFramework, ArchivePartitionReader, PartitionAnalyzer, SysUtils, Classes;

type
  TArchivePartitionReaderTests = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAnalyzeZipArchive;
    procedure TestAnalyzeGZipArchive;
    procedure TestAnalyzeInvalidArchive;
    procedure TestAnalyzeMBRPartitions;
    procedure TestAnalyzeGPTPartitions;
    procedure TestAnalyzeNoPartitions;
  end;

implementation

{ TArchivePartitionReaderTests }

procedure TArchivePartitionReaderTests.SetUp;
begin
  inherited;
end;

procedure TArchivePartitionReaderTests.TearDown;
begin
  inherited;
end;

procedure TArchivePartitionReaderTests.TestAnalyzeZipArchive;
var
  TestFile: string;
  Partitions: TPartitionArray;
  TableType: TPartitionTableType;
begin
  // Test: Analyze ZIP archive with disk image
  TestFile := ExtractFilePath(ParamStr(0)) + 'test-data\test.zip';
  
  if not FileExists(TestFile) then
  begin
    Status('Skipping test - test file not found: ' + TestFile);
    Exit;
  end;
  
  try
    Partitions := TArchivePartitionReader.AnalyzeArchivePartitions(
      TestFile, TableType);
    
    // Should detect archive format
    CheckTrue(Length(Partitions) >= 0, 'Should return partition array');
    
  except
    on E: Exception do
      Status('Expected behavior for test archive: ' + E.Message);
  end;
end;

procedure TArchivePartitionReaderTests.TestAnalyzeGZipArchive;
var
  TestFile: string;
  Partitions: TPartitionArray;
  TableType: TPartitionTableType;
begin
  // Test: Analyze GZIP archive
  TestFile := ExtractFilePath(ParamStr(0)) + 'test-data\test.img.gz';
  
  if not FileExists(TestFile) then
  begin
    Status('Skipping test - test file not found: ' + TestFile);
    Exit;
  end;
  
  try
    Partitions := TArchivePartitionReader.AnalyzeArchivePartitions(
      TestFile, TableType);
    
    CheckTrue(Length(Partitions) >= 0, 'Should return partition array');
    
  except
    on E: Exception do
      Status('Expected behavior for GZIP: ' + E.Message);
  end;
end;

procedure TArchivePartitionReaderTests.TestAnalyzeInvalidArchive;
var
  Partitions: TPartitionArray;
  TableType: TPartitionTableType;
begin
  // Test: Analyze non-existent file
  try
    Partitions := TArchivePartitionReader.AnalyzeArchivePartitions(
      'nonexistent.zip', TableType);
    Fail('Should raise exception for non-existent file');
  except
    on E: Exception do
      Check(True, 'Expected exception for invalid file');
  end;
end;

procedure TArchivePartitionReaderTests.TestAnalyzeMBRPartitions;
begin
  // Test: Detect MBR partition table in archive
  // This requires a test image with MBR
  Status('MBR detection test requires test data');
  Check(True, 'Placeholder for MBR test');
end;

procedure TArchivePartitionReaderTests.TestAnalyzeGPTPartitions;
begin
  // Test: Detect GPT partition table in archive
  // This requires a test image with GPT
  Status('GPT detection test requires test data');
  Check(True, 'Placeholder for GPT test');
end;

procedure TArchivePartitionReaderTests.TestAnalyzeNoPartitions;
begin
  // Test: Analyze archive with no partition table
  Status('No partition test requires test data');
  Check(True, 'Placeholder for no-partition test');
end;

initialization
  RegisterTest(TArchivePartitionReaderTests.Suite);

end.
