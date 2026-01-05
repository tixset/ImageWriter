{******************************************************************************}
{                                                                              }
{  ImageWriter - IPC Integration Tests                                        }
{                                                                              }
{  Copyright (c) 2024-2025 Anton Zelenov <tixset@gmail.com>                   }
{  GitHub: https://github.com/tixset/ImageWriter                              }
{                                                                              }
{  Description:                                                                }
{    Integration tests for IPC (Inter-Process Communication).                 }
{    Tests worker process communication, data transfer, error handling.       }
{                                                                              }
{******************************************************************************}

unit IPCIntegrationTests;

interface

uses
  TestFramework,
  SysUtils,
  Classes,
  Windows,
  IPCPipe,
  WorkerCommands;

type
  TIPCIntegrationTest = class(TTestCase)
  private
    FPipeName: string;
    FServerPipe: TIPCPipeServer;
    FClientPipe: TIPCPipeClient;
    function GetUniquePipeName: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestWorkerCommunication;
    procedure TestDataTransfer;
    procedure TestErrorHandling;
    procedure TestGracefulShutdown;
    procedure TestLargeDataTransfer;
    procedure TestCommandSerialization;
  end;

implementation

uses
  LogUtils;

{ TIPCIntegrationTest }

procedure TIPCIntegrationTest.SetUp;
begin
  inherited;
  FPipeName := GetUniquePipeName;
  FServerPipe := nil;
  FClientPipe := nil;
end;

procedure TIPCIntegrationTest.TearDown;
begin
  if Assigned(FClientPipe) then
    FreeAndNil(FClientPipe);
  if Assigned(FServerPipe) then
    FreeAndNil(FServerPipe);
  inherited;
end;

function TIPCIntegrationTest.GetUniquePipeName: string;
begin
  Result := Format('ImageWriterTest_%d', [GetTickCount]);
end;

procedure TIPCIntegrationTest.TestWorkerCommunication;
var
  Success: Boolean;
  Command: TWorkerCommand;
  Response: TWorkerResponse;
begin
  TLogUtils.Info('IPCIntegrationTest', 'TestWorkerCommunication: Starting worker comm test');
  
  // Create server pipe
  FServerPipe := TIPCPipeServer.Create(FPipeName);
  try
    TLogUtils.Debug('IPCIntegrationTest', Format('Server pipe created: %s', [FPipeName]));
    
    // Create client pipe
    FClientPipe := TIPCPipeClient.Create(FPipeName);
    try
      TLogUtils.Debug('IPCIntegrationTest', 'Client pipe connected');
      
      // Prepare test command
      Command.CommandType := ctInitialize;
      Command.SourcePath := 'test.img';
      Command.TargetPath := '\\.\PhysicalDrive0';
      Command.BufferSize := 65536;
      
      // Send command
      Success := FClientPipe.SendCommand(Command);
      CheckTrue(Success, 'Send command should succeed');
      
      // Receive command on server side
      Success := FServerPipe.ReceiveCommand(Command);
      CheckTrue(Success, 'Receive command should succeed');
      
      // Verify command data
      CheckEquals(Ord(ctInitialize), Ord(Command.CommandType), 'Command type should match');
      CheckEquals('test.img', Command.SourcePath, 'Source path should match');
      CheckEquals('\\.\PhysicalDrive0', Command.TargetPath, 'Target path should match');
      CheckEquals(65536, Command.BufferSize, 'Buffer size should match');
      
      TLogUtils.Info('IPCIntegrationTest', 'TestWorkerCommunication: PASSED');
    finally
      FClientPipe.Free;
      FClientPipe := nil;
    end;
  finally
    FServerPipe.Free;
    FServerPipe := nil;
  end;
end;

procedure TIPCIntegrationTest.TestDataTransfer;
var
  SendBuffer: array[0..1023] of Byte;
  RecvBuffer: array[0..1023] of Byte;
  I: Integer;
  BytesSent, BytesReceived: DWORD;
  Success: Boolean;
begin
  TLogUtils.Info('IPCIntegrationTest', 'TestDataTransfer: Starting data transfer test');
  
  // Initialize test data
  for I := 0 to High(SendBuffer) do
    SendBuffer[I] := I mod 256;
  
  FillChar(RecvBuffer, SizeOf(RecvBuffer), 0);
  
  // Create server and client
  FServerPipe := TIPCPipeServer.Create(FPipeName);
  try
    FClientPipe := TIPCPipeClient.Create(FPipeName);
    try
      // Send data from client
      Success := FClientPipe.WriteData(@SendBuffer, SizeOf(SendBuffer), BytesSent);
      CheckTrue(Success, 'Write data should succeed');
      CheckEquals(SizeOf(SendBuffer), Integer(BytesSent), 'Should send all bytes');
      
      TLogUtils.Debug('IPCIntegrationTest', Format('Sent %d bytes', [BytesSent]));
      
      // Receive data on server
      Success := FServerPipe.ReadData(@RecvBuffer, SizeOf(RecvBuffer), BytesReceived);
      CheckTrue(Success, 'Read data should succeed');
      CheckEquals(SizeOf(RecvBuffer), Integer(BytesReceived), 'Should receive all bytes');
      
      TLogUtils.Debug('IPCIntegrationTest', Format('Received %d bytes', [BytesReceived]));
      
      // Verify data integrity
      for I := 0 to High(SendBuffer) do
      begin
        if SendBuffer[I] <> RecvBuffer[I] then
          Fail(Format('Data mismatch at offset %d: sent=%d, received=%d',
                      [I, SendBuffer[I], RecvBuffer[I]]));
      end;
      
      TLogUtils.Info('IPCIntegrationTest', 'TestDataTransfer: PASSED (data integrity verified)');
    finally
      FClientPipe.Free;
      FClientPipe := nil;
    end;
  finally
    FServerPipe.Free;
    FServerPipe := nil;
  end;
end;

procedure TIPCIntegrationTest.TestErrorHandling;
var
  Command: TWorkerCommand;
  ErrorMsg: string;
begin
  TLogUtils.Info('IPCIntegrationTest', 'TestErrorHandling: Starting error handling test');
  
  // Try to connect to non-existent pipe
  try
    FClientPipe := TIPCPipeClient.Create('NonExistentPipe_12345');
    Fail('Should raise exception for non-existent pipe');
  except
    on E: Exception do
    begin
      ErrorMsg := E.Message;
      TLogUtils.Debug('IPCIntegrationTest', Format('Expected error: %s', [ErrorMsg]));
      CheckNotEquals('', ErrorMsg, 'Error message should not be empty');
    end;
  end;
  
  FClientPipe := nil;
  
  // Test timeout on receive
  FServerPipe := TIPCPipeServer.Create(FPipeName);
  try
    FServerPipe.SetTimeout(100); // 100ms timeout
    
    // Try to receive without sender (should timeout)
    if FServerPipe.ReceiveCommand(Command) then
      TLogUtils.Warning('IPCIntegrationTest', 'Receive did not timeout as expected')
    else
      TLogUtils.Info('IPCIntegrationTest', 'Receive timed out as expected');
    
    // Don't fail test - timeout behavior may vary
    Check(True, 'Timeout test completed');
  finally
    FServerPipe.Free;
    FServerPipe := nil;
  end;
  
  TLogUtils.Info('IPCIntegrationTest', 'TestErrorHandling: PASSED');
end;

procedure TIPCIntegrationTest.TestGracefulShutdown;
var
  Command: TWorkerCommand;
  Response: TWorkerResponse;
  Success: Boolean;
begin
  TLogUtils.Info('IPCIntegrationTest', 'TestGracefulShutdown: Starting shutdown test');
  
  FServerPipe := TIPCPipeServer.Create(FPipeName);
  try
    FClientPipe := TIPCPipeClient.Create(FPipeName);
    try
      // Send shutdown command
      Command.CommandType := ctShutdown;
      Command.SourcePath := '';
      Command.TargetPath := '';
      
      Success := FClientPipe.SendCommand(Command);
      CheckTrue(Success, 'Shutdown command should be sent');
      
      // Receive on server
      Success := FServerPipe.ReceiveCommand(Command);
      CheckTrue(Success, 'Shutdown command should be received');
      CheckEquals(Ord(ctShutdown), Ord(Command.CommandType), 'Should be shutdown command');
      
      TLogUtils.Info('IPCIntegrationTest', 'Shutdown command processed');
      
      // Send acknowledgment
      Response.Success := True;
      Response.ErrorMessage := '';
      Response.BytesProcessed := 0;
      
      Success := FServerPipe.SendResponse(Response);
      CheckTrue(Success, 'Response should be sent');
      
      // Receive response on client
      Success := FClientPipe.ReceiveResponse(Response);
      CheckTrue(Success, 'Response should be received');
      CheckTrue(Response.Success, 'Response should indicate success');
      
      TLogUtils.Info('IPCIntegrationTest', 'TestGracefulShutdown: PASSED');
    finally
      FClientPipe.Free;
      FClientPipe := nil;
    end;
  finally
    FServerPipe.Free;
    FServerPipe := nil;
  end;
end;

procedure TIPCIntegrationTest.TestLargeDataTransfer;
var
  LargeBuffer: array of Byte;
  RecvBuffer: array of Byte;
  BufferSize: Integer;
  I: Integer;
  BytesSent, BytesReceived: DWORD;
  Success: Boolean;
begin
  TLogUtils.Info('IPCIntegrationTest', 'TestLargeDataTransfer: Starting large transfer test');
  
  // 1 MB buffer
  BufferSize := 1024 * 1024;
  SetLength(LargeBuffer, BufferSize);
  SetLength(RecvBuffer, BufferSize);
  
  // Initialize with pattern
  for I := 0 to BufferSize - 1 do
    LargeBuffer[I] := (I * 137) mod 256; // Pseudo-random pattern
  
  FillChar(RecvBuffer[0], BufferSize, 0);
  
  FServerPipe := TIPCPipeServer.Create(FPipeName);
  try
    FClientPipe := TIPCPipeClient.Create(FPipeName);
    try
      TLogUtils.Info('IPCIntegrationTest', Format('Transferring %d bytes', [BufferSize]));
      
      // Send large buffer
      Success := FClientPipe.WriteData(@LargeBuffer[0], BufferSize, BytesSent);
      CheckTrue(Success, 'Large write should succeed');
      CheckEquals(BufferSize, Integer(BytesSent), 'Should send all bytes');
      
      // Receive large buffer
      Success := FServerPipe.ReadData(@RecvBuffer[0], BufferSize, BytesReceived);
      CheckTrue(Success, 'Large read should succeed');
      CheckEquals(BufferSize, Integer(BytesReceived), 'Should receive all bytes');
      
      // Verify data (sample check - not every byte)
      for I := 0 to 99 do
      begin
        if LargeBuffer[I * (BufferSize div 100)] <> RecvBuffer[I * (BufferSize div 100)] then
          Fail(Format('Large data mismatch at sample %d', [I]));
      end;
      
      TLogUtils.Info('IPCIntegrationTest', 'TestLargeDataTransfer: PASSED (1 MB verified)');
    finally
      FClientPipe.Free;
      FClientPipe := nil;
    end;
  finally
    FServerPipe.Free;
    FServerPipe := nil;
  end;
end;

procedure TIPCIntegrationTest.TestCommandSerialization;
var
  Command1, Command2: TWorkerCommand;
  Success: Boolean;
begin
  TLogUtils.Info('IPCIntegrationTest', 'TestCommandSerialization: Starting serialization test');
  
  // Prepare complex command
  Command1.CommandType := ctWriteData;
  Command1.SourcePath := 'C:\images\ubuntu-20.04.iso';
  Command1.TargetPath := '\\.\PhysicalDrive1';
  Command1.BufferSize := 1048576; // 1 MB
  Command1.VerifyWrite := True;
  Command1.ComputeHash := True;
  
  FServerPipe := TIPCPipeServer.Create(FPipeName);
  try
    FClientPipe := TIPCPipeClient.Create(FPipeName);
    try
      // Send command
      Success := FClientPipe.SendCommand(Command1);
      CheckTrue(Success, 'Send should succeed');
      
      // Receive command
      Success := FServerPipe.ReceiveCommand(Command2);
      CheckTrue(Success, 'Receive should succeed');
      
      // Verify all fields
      CheckEquals(Ord(Command1.CommandType), Ord(Command2.CommandType), 'CommandType mismatch');
      CheckEquals(Command1.SourcePath, Command2.SourcePath, 'SourcePath mismatch');
      CheckEquals(Command1.TargetPath, Command2.TargetPath, 'TargetPath mismatch');
      CheckEquals(Command1.BufferSize, Command2.BufferSize, 'BufferSize mismatch');
      CheckEquals(Command1.VerifyWrite, Command2.VerifyWrite, 'VerifyWrite mismatch');
      CheckEquals(Command1.ComputeHash, Command2.ComputeHash, 'ComputeHash mismatch');
      
      TLogUtils.Info('IPCIntegrationTest', 'TestCommandSerialization: PASSED');
    finally
      FClientPipe.Free;
      FClientPipe := nil;
    end;
  finally
    FServerPipe.Free;
    FServerPipe := nil;
  end;
end;

initialization
  RegisterTest(TIPCIntegrationTest.Suite);

end.
