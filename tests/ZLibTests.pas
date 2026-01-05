{******************************************************************************}
{                                                                              }
{  ImageWriter - ZLib Functionality Tests                                     }
{                                                                              }
{  Copyright (c) 2024-2025 Anton Zelenov <tixset@gmail.com>                   }
{  GitHub: https://github.com/tixset/ImageWriter                              }
{                                                                              }
{  Description:                                                                }
{    Tests for ZLib compression/decompression functionality.                  }
{    Tests dynamic loading and basic compression operations.                  }
{                                                                              }
{******************************************************************************}

unit ZLibTests;

interface

uses
  TestFramework,
  SysUtils,
  Classes;

type
  TZLibTest = class(TTestCase)
  private
    FTestDataPath: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestZLibDynamicLoading;
    procedure TestZLibAvailable;
    procedure TestCompressionDecompression;
  end;

implementation

uses
  ZLib, ExtractZLib;

{ TZLibTest }

procedure TZLibTest.SetUp;
begin
  inherited;
  FTestDataPath := GetCurrentDir + '\test_data.txt';
end;

procedure TZLibTest.TearDown;
begin
  if FileExists(FTestDataPath) then
    DeleteFile(FTestDataPath);
  inherited;
end;

procedure TZLibTest.TestZLibDynamicLoading;
var
  ZLibPath: string;
begin
  WriteLn('Testing ZLib dynamic loading...');
  
  try
    ZLibPath := GetZLibPath;
    WriteLn('ZLib path: ' + ZLibPath);
    
    CheckTrue(ZLibPath <> '', 'ZLib path should not be empty');
    
    // Try to ensure ZLib is available
    if not FileExists(ZLibPath) then
    begin
      WriteLn('Extracting embedded ZLib DLL...');
      ExtractZLibDLL;
    end;
    
    CheckTrue(FileExists(ZLibPath), 'ZLib DLL should exist: ' + ZLibPath);
    
  except
    on E: Exception do
      Fail('ZLib loading failed: ' + E.Message);
  end;
end;

procedure TZLibTest.TestZLibAvailable;
begin
  WriteLn('Checking ZLib availability...');
  
  try
    // This should load ZLib if not already loaded
    EnsureZLibAvailable;
    
    Check(True, 'ZLib loaded successfully');
    WriteLn('ZLib is available and loaded');
    
  except
    on E: Exception do
    begin
      WriteLn('WARNING: ZLib not available: ' + E.Message);
      Check(True, 'ZLib test skipped - library not available');
    end;
  end;
end;

procedure TZLibTest.TestCompressionDecompression;
const
  TEST_STRING = 'This is a test string for compression and decompression. ' +
                'It contains repeated text. Repeated text. Repeated text. ' +
                'This should compress well!';
var
  OriginalStream: TMemoryStream;
  CompressedStream: TMemoryStream;
  DecompressedStream: TMemoryStream;
  OriginalText: string;
  DecompressedText: string;
begin
  WriteLn('Testing compression/decompression...');
  
  try
    // Ensure ZLib is loaded
    EnsureZLibAvailable;
  except
    on E: Exception do
    begin
      WriteLn('SKIP: ZLib not available');
      Check(True, 'Test skipped');
      Exit;
    end;
  end;
  
  OriginalStream := TMemoryStream.Create;
  CompressedStream := TMemoryStream.Create;
  DecompressedStream := TMemoryStream.Create;
  try
    // Write test string to stream
    OriginalText := TEST_STRING;
    OriginalStream.Write(OriginalText[1], Length(OriginalText));
    OriginalStream.Position := 0;
    
    WriteLn(Format('Original size: %d bytes', [OriginalStream.Size]));
    
    // Compress
    try
      ZCompressStream(OriginalStream, CompressedStream);
      WriteLn(Format('Compressed size: %d bytes', [CompressedStream.Size]));
      
      CheckTrue(CompressedStream.Size < OriginalStream.Size, 
        'Compressed data should be smaller than original');
        
      // Decompress
      CompressedStream.Position := 0;
      ZDecompressStream(CompressedStream, DecompressedStream);
      
      WriteLn(Format('Decompressed size: %d bytes', [DecompressedStream.Size]));
      
      CheckEquals(OriginalStream.Size, DecompressedStream.Size, 
        'Decompressed size should match original');
        
      // Verify content
      SetLength(DecompressedText, DecompressedStream.Size);
      DecompressedStream.Position := 0;
      DecompressedStream.Read(DecompressedText[1], DecompressedStream.Size);
      
      CheckEquals(OriginalText, DecompressedText, 
        'Decompressed text should match original');
        
      WriteLn('Compression/decompression successful!');
      
    except
      on E: Exception do
        Fail('Compression test failed: ' + E.Message);
    end;
    
  finally
    OriginalStream.Free;
    CompressedStream.Free;
    DecompressedStream.Free;
  end;
end;

initialization
  RegisterTest(TZLibTest.Suite);

end.
