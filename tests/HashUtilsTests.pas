{******************************************************************************}
{                                                                              }
{  ImageWriter - Hash Utilities Tests                                         }
{                                                                              }
{  Copyright (c) 2024-2025 Anton Zelenov <tixset@gmail.com>                   }
{  GitHub: https://github.com/tixset/ImageWriter                              }
{                                                                              }
{  Description:                                                                }
{    Unit tests for THashUtils class.                                         }
{    Tests MD5, SHA256 hashing and verification.                              }
{                                                                              }
{******************************************************************************}

unit HashUtilsTests;

interface

uses
  TestFramework,
  SysUtils,
  Classes,
  HashUtils;

type
  THashUtilsTest = class(TTestCase)
  private
    FTestFile: string;
    procedure CreateTestFile(const Content: string);
    procedure DeleteTestFile;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMD5Hash;
    procedure TestSHA256Hash;
    procedure TestVerifyHash;
    procedure TestHashProgress;
    procedure TestEmptyFile;
    procedure TestLargeFile;
  end;

implementation

uses
  Windows,
  LogUtils;

{ THashUtilsTest }

procedure THashUtilsTest.SetUp;
begin
  inherited;
  FTestFile := ExtractFilePath(ParamStr(0)) + 'test_hash.tmp';
end;

procedure THashUtilsTest.TearDown;
begin
  DeleteTestFile;
  inherited;
end;

procedure THashUtilsTest.CreateTestFile(const Content: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FTestFile, fmCreate);
  try
    if Length(Content) > 0 then
      Stream.WriteBuffer(Content[1], Length(Content));
  finally
    Stream.Free;
  end;
end;

procedure THashUtilsTest.DeleteTestFile;
begin
  if FileExists(FTestFile) then
    DeleteFile(FTestFile);
end;

procedure THashUtilsTest.TestMD5Hash;
var
  Hash: string;
begin
  TLogUtils.Info('HashUtilsTest', 'TestMD5Hash: Starting MD5 hash test');
  
  // Create test file with known content
  CreateTestFile('Hello, World!');
  
  // Compute MD5
  Hash := THashUtils.ComputeMD5(FTestFile);
  
  TLogUtils.Info('HashUtilsTest', Format('MD5 of "Hello, World!": %s', [Hash]));
  
  // Known MD5 hash of "Hello, World!"
  CheckEquals('65A8E27D8879283831B664BD8B7F0AD4', UpperCase(Hash),
              'MD5 hash mismatch for "Hello, World!"');
  
  // Verify hash length (32 hex digits)
  CheckEquals(32, Length(Hash), 'MD5 hash should be 32 characters');
  
  TLogUtils.Info('HashUtilsTest', 'TestMD5Hash: PASSED');
end;

procedure THashUtilsTest.TestSHA256Hash;
var
  Hash: string;
begin
  TLogUtils.Info('HashUtilsTest', 'TestSHA256Hash: Starting SHA256 hash test');
  
  // Create test file with known content
  CreateTestFile('Hello, World!');
  
  // Compute SHA256
  Hash := THashUtils.ComputeSHA256(FTestFile);
  
  TLogUtils.Info('HashUtilsTest', Format('SHA256 of "Hello, World!": %s', [Hash]));
  
  // Known SHA256 hash of "Hello, World!"
  CheckEquals('DFFD6021BB2BD5B0AF676290809EC3A53191DD81C7F70A4B28688A362182986F',
              UpperCase(Hash),
              'SHA256 hash mismatch for "Hello, World!"');
  
  // Verify hash length (64 hex digits)
  CheckEquals(64, Length(Hash), 'SHA256 hash should be 64 characters');
  
  TLogUtils.Info('HashUtilsTest', 'TestSHA256Hash: PASSED');
end;

procedure THashUtilsTest.TestVerifyHash;
var
  ExpectedMD5: string;
  ExpectedSHA256: string;
begin
  TLogUtils.Info('HashUtilsTest', 'TestVerifyHash: Starting hash verification test');
  
  // Create test file
  CreateTestFile('Test content for verification');
  
  // Compute expected hashes
  ExpectedMD5 := THashUtils.ComputeMD5(FTestFile);
  ExpectedSHA256 := THashUtils.ComputeSHA256(FTestFile);
  
  TLogUtils.Info('HashUtilsTest', Format('Expected MD5: %s', [ExpectedMD5]));
  TLogUtils.Info('HashUtilsTest', Format('Expected SHA256: %s', [ExpectedSHA256]));
  
  // Verify correct hashes
  CheckTrue(THashUtils.VerifyMD5(FTestFile, ExpectedMD5),
            'MD5 verification should succeed with correct hash');
  CheckTrue(THashUtils.VerifySHA256(FTestFile, ExpectedSHA256),
            'SHA256 verification should succeed with correct hash');
  
  // Verify incorrect hashes
  CheckFalse(THashUtils.VerifyMD5(FTestFile, '00000000000000000000000000000000'),
             'MD5 verification should fail with incorrect hash');
  CheckFalse(THashUtils.VerifySHA256(FTestFile, '0000000000000000000000000000000000000000000000000000000000000000'),
             'SHA256 verification should fail with incorrect hash');
  
  TLogUtils.Info('HashUtilsTest', 'TestVerifyHash: PASSED');
end;

procedure THashUtilsTest.TestHashProgress;
var
  Hash: string;
  ProgressCalled: Boolean;
  
  procedure OnProgress(Percent: Integer);
  begin
    ProgressCalled := True;
    TLogUtils.Debug('HashUtilsTest', Format('Hash progress: %d%%', [Percent]));
    CheckTrue((Percent >= 0) and (Percent <= 100), 'Progress should be 0-100%');
  end;
  
begin
  TLogUtils.Info('HashUtilsTest', 'TestHashProgress: Starting progress callback test');
  
  // Create larger test file (1 MB)
  CreateTestFile(StringOfChar('A', 1024 * 1024));
  
  ProgressCalled := False;
  
  // Compute hash with progress callback
  Hash := THashUtils.ComputeMD5WithProgress(FTestFile, OnProgress);
  
  CheckNotEquals('', Hash, 'Hash should not be empty');
  CheckTrue(ProgressCalled, 'Progress callback should be called');
  
  TLogUtils.Info('HashUtilsTest', 'TestHashProgress: PASSED');
end;

procedure THashUtilsTest.TestEmptyFile;
var
  MD5Hash: string;
  SHA256Hash: string;
begin
  TLogUtils.Info('HashUtilsTest', 'TestEmptyFile: Starting empty file hash test');
  
  // Create empty file
  CreateTestFile('');
  
  // Compute hashes of empty file
  MD5Hash := THashUtils.ComputeMD5(FTestFile);
  SHA256Hash := THashUtils.ComputeSHA256(FTestFile);
  
  TLogUtils.Info('HashUtilsTest', Format('MD5 of empty file: %s', [MD5Hash]));
  TLogUtils.Info('HashUtilsTest', Format('SHA256 of empty file: %s', [SHA256Hash]));
  
  // Known hash of empty file
  CheckEquals('D41D8CD98F00B204E9800998ECF8427E', UpperCase(MD5Hash),
              'MD5 of empty file should match known value');
  CheckEquals('E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855',
              UpperCase(SHA256Hash),
              'SHA256 of empty file should match known value');
  
  TLogUtils.Info('HashUtilsTest', 'TestEmptyFile: PASSED');
end;

procedure THashUtilsTest.TestLargeFile;
var
  Stream: TFileStream;
  Buffer: array[0..1023] of Byte;
  I: Integer;
  Hash: string;
  StartTime: DWORD;
  ElapsedMs: DWORD;
begin
  TLogUtils.Info('HashUtilsTest', 'TestLargeFile: Starting large file hash test');
  
  // Create 10 MB file
  Stream := TFileStream.Create(FTestFile, fmCreate);
  try
    FillChar(Buffer, SizeOf(Buffer), $AA);
    for I := 1 to 10 * 1024 do // 10 MB
      Stream.WriteBuffer(Buffer, SizeOf(Buffer));
  finally
    Stream.Free;
  end;
  
  TLogUtils.Info('HashUtilsTest', 'Created 10 MB test file');
  
  // Measure hash performance
  StartTime := GetTickCount;
  Hash := THashUtils.ComputeMD5(FTestFile);
  ElapsedMs := GetTickCount - StartTime;
  
  TLogUtils.Info('HashUtilsTest', Format('MD5 computed in %d ms', [ElapsedMs]));
  TLogUtils.Info('HashUtilsTest', Format('Hash: %s', [Hash]));
  
  CheckNotEquals('', Hash, 'Hash should not be empty');
  CheckEquals(32, Length(Hash), 'MD5 hash should be 32 characters');
  
  // Performance check - should complete in reasonable time (< 5 seconds for 10 MB)
  CheckTrue(ElapsedMs < 5000, Format('Hash took too long: %d ms', [ElapsedMs]));
  
  TLogUtils.Info('HashUtilsTest', 'TestLargeFile: PASSED');
end;

initialization
  RegisterTest(THashUtilsTest.Suite);

end.
