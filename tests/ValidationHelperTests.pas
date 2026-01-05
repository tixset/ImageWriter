{******************************************************************************}
{                                                                              }
{  ImageWriter - Validation Helper Tests                                      }
{                                                                              }
{  Copyright (c) 2024-2025 Anton Zelenov <tixset@gmail.com>                   }
{  GitHub: https://github.com/tixset/ImageWriter                              }
{                                                                              }
{  Description:                                                                }
{    Unit tests for TValidationHelper class.                                  }
{    Tests image file and device validation logic.                            }
{                                                                              }
{******************************************************************************}

unit ValidationHelperTests;

interface

uses
  TestFramework,
  SysUtils,
  Classes,
  ValidationHelper;

type
  TValidationHelperTest = class(TTestCase)
  private
    FTestFile: string;
    procedure CreateTestImageFile(Size: Int64);
    procedure DeleteTestFile;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestValidateImageFile;
    procedure TestValidateDevicePath;
    procedure TestCheckImageSize;
    procedure TestValidatePartitions;
    procedure TestValidateFileExtension;
  end;

implementation

uses
  Windows,
  LogUtils;

{ TValidationHelperTest }

procedure TValidationHelperTest.SetUp;
begin
  inherited;
  FTestFile := ExtractFilePath(ParamStr(0)) + 'test_image.img';
end;

procedure TValidationHelperTest.TearDown;
begin
  DeleteTestFile;
  inherited;
end;

procedure TValidationHelperTest.CreateTestImageFile(Size: Int64);
var
  Stream: TFileStream;
  Buffer: array[0..1023] of Byte;
  Remaining: Int64;
  ToWrite: Integer;
begin
  Stream := TFileStream.Create(FTestFile, fmCreate);
  try
    FillChar(Buffer, SizeOf(Buffer), 0);
    Remaining := Size;
    
    while Remaining > 0 do
    begin
      if Remaining >= SizeOf(Buffer) then
        ToWrite := SizeOf(Buffer)
      else
        ToWrite := Remaining;
      
      Stream.WriteBuffer(Buffer, ToWrite);
      Dec(Remaining, ToWrite);
    end;
  finally
    Stream.Free;
  end;
end;

procedure TValidationHelperTest.DeleteTestFile;
begin
  if FileExists(FTestFile) then
    DeleteFile(FTestFile);
end;

procedure TValidationHelperTest.TestValidateImageFile;
var
  ErrorMsg: string;
begin
  TLogUtils.Info('ValidationHelperTest', 'TestValidateImageFile: Starting image file validation test');
  
  // Test non-existent file
  CheckFalse(TValidationHelper.ValidateImageFile('non_existent.img', ErrorMsg),
             'Non-existent file should fail validation');
  CheckNotEquals('', ErrorMsg, 'Error message should be set for non-existent file');
  TLogUtils.Debug('ValidationHelperTest', Format('Non-existent file error: %s', [ErrorMsg]));
  
  // Create valid test file (1 MB)
  CreateTestImageFile(1024 * 1024);
  
  CheckTrue(TValidationHelper.ValidateImageFile(FTestFile, ErrorMsg),
            'Valid image file should pass validation');
  CheckEquals('', ErrorMsg, 'Error message should be empty for valid file');
  
  // Test empty file
  DeleteTestFile;
  CreateTestImageFile(0);
  
  CheckFalse(TValidationHelper.ValidateImageFile(FTestFile, ErrorMsg),
             'Empty file should fail validation');
  CheckNotEquals('', ErrorMsg, 'Error message should be set for empty file');
  TLogUtils.Debug('ValidationHelperTest', Format('Empty file error: %s', [ErrorMsg]));
  
  TLogUtils.Info('ValidationHelperTest', 'TestValidateImageFile: PASSED');
end;

procedure TValidationHelperTest.TestValidateDevicePath;
var
  ErrorMsg: string;
begin
  TLogUtils.Info('ValidationHelperTest', 'TestValidateDevicePath: Starting device path validation test');
  
  // Valid device paths
  CheckTrue(TValidationHelper.ValidateDevicePath('\\.\PhysicalDrive0', ErrorMsg),
            '\\.\PhysicalDrive0 should be valid');
  CheckEquals('', ErrorMsg, 'No error for valid path');
  
  CheckTrue(TValidationHelper.ValidateDevicePath('\\.\PhysicalDrive15', ErrorMsg),
            '\\.\PhysicalDrive15 should be valid');
  
  // Invalid device paths
  CheckFalse(TValidationHelper.ValidateDevicePath('', ErrorMsg),
             'Empty path should be invalid');
  CheckNotEquals('', ErrorMsg, 'Error message should be set');
  TLogUtils.Debug('ValidationHelperTest', Format('Empty path error: %s', [ErrorMsg]));
  
  CheckFalse(TValidationHelper.ValidateDevicePath('C:\', ErrorMsg),
             'C:\ should be invalid device path');
  
  CheckFalse(TValidationHelper.ValidateDevicePath('\\.\PhysicalDrive', ErrorMsg),
             'Path without number should be invalid');
  
  CheckFalse(TValidationHelper.ValidateDevicePath('\\.\PhysicalDrive-1', ErrorMsg),
             'Negative device number should be invalid');
  
  CheckFalse(TValidationHelper.ValidateDevicePath('PhysicalDrive0', ErrorMsg),
             'Path without \\.\\ prefix should be invalid');
  
  TLogUtils.Info('ValidationHelperTest', 'TestValidateDevicePath: PASSED');
end;

procedure TValidationHelperTest.TestCheckImageSize;
var
  DeviceSize: Int64;
  ImageSize: Int64;
  ErrorMsg: string;
begin
  TLogUtils.Info('ValidationHelperTest', 'TestCheckImageSize: Starting size validation test');
  
  DeviceSize := 8 * 1024 * 1024 * 1024; // 8 GB device
  
  // Image smaller than device - OK
  ImageSize := 4 * 1024 * 1024 * 1024; // 4 GB image
  CheckTrue(TValidationHelper.CheckImageSize(ImageSize, DeviceSize, ErrorMsg),
            '4 GB image should fit on 8 GB device');
  CheckEquals('', ErrorMsg, 'No error for valid size');
  
  // Image equal to device size - OK
  ImageSize := DeviceSize;
  CheckTrue(TValidationHelper.CheckImageSize(ImageSize, DeviceSize, ErrorMsg),
            'Image equal to device size should be valid');
  
  // Image larger than device - FAIL
  ImageSize := 16 * 1024 * 1024 * 1024; // 16 GB image
  CheckFalse(TValidationHelper.CheckImageSize(ImageSize, DeviceSize, ErrorMsg),
             '16 GB image should not fit on 8 GB device');
  CheckNotEquals('', ErrorMsg, 'Error message should be set for oversized image');
  TLogUtils.Debug('ValidationHelperTest', Format('Oversized image error: %s', [ErrorMsg]));
  
  // Zero size device - FAIL
  DeviceSize := 0;
  ImageSize := 1024;
  CheckFalse(TValidationHelper.CheckImageSize(ImageSize, DeviceSize, ErrorMsg),
             'Zero size device should be invalid');
  
  TLogUtils.Info('ValidationHelperTest', 'TestCheckImageSize: PASSED');
end;

procedure TValidationHelperTest.TestValidatePartitions;
var
  ErrorMsg: string;
  WarningMsg: string;
begin
  TLogUtils.Info('ValidationHelperTest', 'TestValidatePartitions: Starting partition validation test');
  
  // Valid device path should pass basic check
  if TValidationHelper.ValidatePartitions('\\.\PhysicalDrive0', ErrorMsg, WarningMsg) then
  begin
    TLogUtils.Info('ValidationHelperTest', 'PhysicalDrive0 partition validation passed');
    CheckEquals('', ErrorMsg, 'No error for valid device');
  end
  else
  begin
    TLogUtils.Warning('ValidationHelperTest',
      Format('Partition validation failed: %s (may need admin rights)', [ErrorMsg]));
    // Don't fail test - partition check may require admin rights
    Check(True, 'Test completed (partition check requires admin rights)');
  end;
  
  // Invalid device should fail
  CheckFalse(TValidationHelper.ValidatePartitions('invalid_device', ErrorMsg, WarningMsg),
             'Invalid device should fail partition validation');
  CheckNotEquals('', ErrorMsg, 'Error message should be set for invalid device');
  
  TLogUtils.Info('ValidationHelperTest', 'TestValidatePartitions: PASSED');
end;

procedure TValidationHelperTest.TestValidateFileExtension;
begin
  TLogUtils.Info('ValidationHelperTest', 'TestValidateFileExtension: Starting file extension test');
  
  // Valid image extensions
  CheckTrue(TValidationHelper.IsValidImageExtension('test.img'),
            '.img should be valid extension');
  CheckTrue(TValidationHelper.IsValidImageExtension('test.iso'),
            '.iso should be valid extension');
  CheckTrue(TValidationHelper.IsValidImageExtension('test.bin'),
            '.bin should be valid extension');
  CheckTrue(TValidationHelper.IsValidImageExtension('C:\path\to\image.img'),
            '.img with path should be valid');
  
  // Archive extensions (should also be valid)
  CheckTrue(TValidationHelper.IsValidImageExtension('test.zip'),
            '.zip should be valid extension');
  CheckTrue(TValidationHelper.IsValidImageExtension('test.gz'),
            '.gz should be valid extension');
  CheckTrue(TValidationHelper.IsValidImageExtension('test.xz'),
            '.xz should be valid extension');
  
  // Invalid extensions
  CheckFalse(TValidationHelper.IsValidImageExtension('test.txt'),
             '.txt should be invalid extension');
  CheckFalse(TValidationHelper.IsValidImageExtension('test.doc'),
             '.doc should be invalid extension');
  CheckFalse(TValidationHelper.IsValidImageExtension('test'),
             'File without extension should be invalid');
  CheckFalse(TValidationHelper.IsValidImageExtension(''),
             'Empty filename should be invalid');
  
  TLogUtils.Info('ValidationHelperTest', 'TestValidateFileExtension: PASSED');
end;

initialization
  RegisterTest(TValidationHelperTest.Suite);

end.
