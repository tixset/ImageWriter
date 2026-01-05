{******************************************************************************}
{                                                                              }
{  ImageWriter - Worker Mode Unit                                             }
{                                                                              }
{  Copyright (c) 2024-2025 Anton Zelenov <tixset@gmail.com>                   }
{  GitHub: https://github.com/tixset/ImageWriter                              }
{  Based on dd for Windows by John Newbigin (http://chrysocome.net/dd)        }
{                                                                              }
{  This program is free software: you can redistribute it and/or modify       }
{  it under the terms of the GNU General Public License as published by       }
{  the Free Software Foundation, either version 3 of the License, or          }
{  (at your option) any later version.                                        }
{                                                                              }
{  Description:                                                                }
{    Worker process mode for elevated disk operations. Handles IPC            }
{    communication with main process and executes privileged operations.      }
{                                                                              }
{******************************************************************************}

unit WorkerMode;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, SysUtils, Classes, IPCPipe;

type
  TWorkerOperation = (woRead, woWrite, woVerify);

  TWorkerParams = record
    Operation: TWorkerOperation;
    DevicePath: string;
    FilePath: string;
    BlockSize: Integer;
    Count: Int64;
    Seek: Int64;
    Skip: Int64;
    CheckSize: Boolean;
    VerifyHash: Boolean;
    HashAlgorithm: string;  // 'MD5' or 'SHA256'
    PipeName: string;
    Persistent: Boolean;  // Run in persistent mode (daemon)
  end;

// Parse command line and execute worker operation
function ExecuteWorkerMode: Integer;

// Build command line for worker process
function BuildWorkerCommandLine(const Params: TWorkerParams): string;

// Parse worker command line
function ParseWorkerCommandLine(out Params: TWorkerParams): Boolean;

implementation

uses
  ExtractZLib, GZipStream, ZLib, md5, sha256, BinFile, WorkerCommands;

// Helper function for Delphi 7 compatibility (StrToInt64Def not available)
function StrToInt64Def(const S: string; Default: Int64): Int64;
begin
  try
    Result := StrToInt64(S);
  except
    Result := Default;
  end;
end;

type
  TWorkerThread = class(TThread)
  private
    FParams: TWorkerParams;
    FPipe: TIPCPipeServer;
    FCancelled: Boolean;
    procedure SendLog(const Msg: string);
    procedure SendProgress(Progress, Total: Int64);
    procedure CheckForCancel;
  protected
    procedure Execute; override;
  public
    constructor Create(const AParams: TWorkerParams; APipe: TIPCPipeServer);
  end;

{ TWorkerThread }

constructor TWorkerThread.Create(const AParams: TWorkerParams; APipe: TIPCPipeServer);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FParams := AParams;
  FPipe := APipe;
  FCancelled := False;
end;

procedure TWorkerThread.SendLog(const Msg: string);
begin
  if Assigned(FPipe) and FPipe.Connected then
    FPipe.SendMessage(ipcLog, Msg);
end;

procedure TWorkerThread.SendProgress(Progress, Total: Int64);
var
  Data: string;
begin
  if Assigned(FPipe) and FPipe.Connected then
  begin
    Data := IntToStr(Progress) + '|' + IntToStr(Total);
    FPipe.SendMessage(ipcProgress, Data);
  end;
end;

procedure TWorkerThread.CheckForCancel;
var
  MsgType: TIPCMessageType;
  Data: string;
begin
  if Assigned(FPipe) and FPipe.Connected then
  begin
    if FPipe.ReceiveMessage(MsgType, Data, 0) then
    begin
      if MsgType = ipcCancel then
        FCancelled := True;
    end;
  end;
end;

procedure TWorkerThread.Execute;
var
  DeviceFile: TBinFile;
  Buffer: array[0..65535] of Byte;  // 64KB buffer
  BytesRead, BytesWritten: Integer;
  TotalProcessed: Int64;
  TotalSize: Int64;
  MsgType: TIPCMessageType;
  Data: string;
  ContextMD5: MD5Context;
  ContextSHA256: TSHA256Context;
  DigestMD5: MD5Digest;
  DigestSHA256: TSHA256Digest;
  HashResult: string;
  UseSHA256: Boolean;
begin
  try
    SendLog('Worker started');
    
    // Open device
    DeviceFile := nil;
    try
      if Pos('\\.\', FParams.DevicePath) = 1 then
        DeviceFile := TWinBinFile.Create(FParams.DevicePath)
      else
        DeviceFile := TUnixBinFile.Create(FParams.DevicePath);

      if not DeviceFile.Open then
      begin
        SendLog('ERROR: Cannot open device: ' + FParams.DevicePath);
        FPipe.SendMessage(ipcError, 'Cannot open device');
        Exit;
      end;

      SendLog('Device opened: ' + FParams.DevicePath);

      // Calculate total size
      if FParams.Count > 0 then
        TotalSize := FParams.Count * FParams.BlockSize
      else
        TotalSize := DeviceFile.GetSize;

      SendLog('Total size: ' + IntToStr(TotalSize) + ' bytes');

      // Seek if needed
      if FParams.Seek > 0 then
      begin
        DeviceFile.Seek(FParams.Seek * FParams.BlockSize);
        SendLog('Seeked to: ' + IntToStr(FParams.Seek * FParams.BlockSize));
      end;

      TotalProcessed := 0;

      if FParams.Operation = woRead then
      begin
        // READ operation: send data via IPC to GUI
        SendLog('Sending ready signal to GUI...');
        FPipe.SendMessage(ipcReady, 'ready');
        
        // Send total size as first message
        FPipe.SendMessage(ipcData, IntToStr(TotalSize));
        SendLog('Total size sent: ' + IntToStr(TotalSize) + ' bytes');

        while (TotalProcessed < TotalSize) and not FCancelled do
        begin
          CheckForCancel;
          if FCancelled then Break;

          BytesRead := DeviceFile.Read(Buffer, SizeOf(Buffer));
          if BytesRead <= 0 then Break;

          // Limit to TotalSize
          if TotalProcessed + BytesRead > TotalSize then
            BytesRead := TotalSize - TotalProcessed;

          // Send data block via IPC
          SetLength(Data, BytesRead);
          Move(Buffer[0], Data[1], BytesRead);
          if not FPipe.SendMessage(ipcData, Data) then
          begin
            SendLog('ERROR: Failed to send data via IPC');
            FPipe.SendMessage(ipcError, 'Data transmission failed');
            Exit;
          end;

          Inc(TotalProcessed, BytesRead);
          SendProgress(TotalProcessed, TotalSize);
        end;

        if FCancelled then
          SendLog('Operation cancelled by user')
        else
        begin
          SendLog('Read completed: ' + IntToStr(TotalProcessed) + ' bytes');
          SendLog(IntToStr(TotalProcessed div FParams.BlockSize) + '+0 records in');
          SendLog(IntToStr(TotalProcessed div FParams.BlockSize) + '+0 records out');
        end;
      end
      else if FParams.Operation = woWrite then
      begin
        // WRITE operation: receive data via IPC
        SendLog('Waiting for data from GUI...');        
        // Send ready signal to GUI
        FPipe.SendMessage(ipcReady, 'ready');
        // Don't send log here - would cause deadlock as GUI starts sending data immediately
        // Total size will be sent in first progress message
        TotalSize := 0;

        while not FCancelled do
        begin
          CheckForCancel;
          if FCancelled then Break;
          
          // Check if we received all expected data
          if (TotalSize > 0) and (TotalProcessed >= TotalSize) then
          begin
            SendLog('All data received (' + IntToStr(TotalProcessed) + ' bytes)');
            Break;
          end;

          // Receive data block from GUI
          if FPipe.ReceiveMessage(MsgType, Data, 100) then
          begin
            case MsgType of
              ipcData:
                begin
                  if TotalSize = 0 then
                  begin
                    // First message contains total size as string
                    TotalSize := StrToInt64Def(Data, 0);
                    SendLog('Total size: ' + IntToStr(TotalSize) + ' bytes');
                  end
                  else
                  begin
                    // Data block
                    if Length(Data) > 0 then
                    begin
                      BytesWritten := DeviceFile.Write(Data[1], Length(Data));
                      if BytesWritten <> Length(Data) then
                      begin
                        SendLog('ERROR: Write failed');
                        FPipe.SendMessage(ipcError, 'Write failed');
                        Exit;
                      end;
                      
                      Inc(TotalProcessed, BytesWritten);
                      // Don't send progress here to avoid pipe deadlock
                      // GUI will update progress based on sent data
                    end;
                  end;
                end;
              
              ipcCompleted:
                begin
                  // GUI finished sending data
                  // Continue reading buffered data before exiting
                  SendLog('Received completion signal from GUI');
                  // Don't break yet - let the size check handle exit
                end;
                
              ipcCancel:
                begin
                  FCancelled := True;
                  Break;
                end;
            end;
          end;
        end;
        
        // Check if pipe was disconnected (GUI closed)
        if not FPipe.Connected then
        begin
          SendLog('GUI disconnected, terminating...');
          FCancelled := True;
        end;

        if FCancelled then
          SendLog('Operation cancelled by user')
        else
        begin
          SendLog('Write completed: ' + IntToStr(TotalProcessed) + ' bytes');
          SendLog(IntToStr(TotalProcessed div FParams.BlockSize) + '+0 records in');
          SendLog(IntToStr(TotalProcessed div FParams.BlockSize) + '+0 records out');
        end;
      end
      else if FParams.Operation = woVerify then
      begin
        // VERIFY operation: calculate hash and send to GUI
        SendLog('Starting verification...');
        
        UseSHA256 := (FParams.HashAlgorithm = 'SHA256');
        
        if UseSHA256 then
        begin
          SHA256Init(ContextSHA256);
          SendLog('Using SHA256 algorithm');
        end
        else
        begin
          MD5Init(ContextMD5);
          SendLog('Using MD5 algorithm');
        end;

        TotalProcessed := 0;
        
        while (TotalProcessed < TotalSize) and not FCancelled do
        begin
          CheckForCancel;
          if FCancelled then Break;

          BytesRead := DeviceFile.Read(Buffer, SizeOf(Buffer));
          if BytesRead <= 0 then Break;

          // Limit to TotalSize
          if TotalProcessed + BytesRead > TotalSize then
            BytesRead := TotalSize - TotalProcessed;

          // Update hash
          if UseSHA256 then
            SHA256Update(ContextSHA256, @Buffer[0], BytesRead)
          else
            MD5Update(ContextMD5, @Buffer[0], BytesRead);

          Inc(TotalProcessed, BytesRead);
          SendProgress(TotalProcessed, TotalSize);
        end;

        if FCancelled then
        begin
          SendLog('Verification cancelled by user');
        end
        else
        begin
          // Finalize hash
          if UseSHA256 then
          begin
            SHA256Final(ContextSHA256, DigestSHA256);
            HashResult := SHA256Print(DigestSHA256);
          end
          else
          begin
            MD5Final(ContextMD5, DigestMD5);
            HashResult := MD5Print(DigestMD5);
          end;

          SendLog('Verification completed: ' + IntToStr(TotalProcessed) + ' bytes');
          SendLog('Hash (' + FParams.HashAlgorithm + '): ' + HashResult);
          
          // Send hash result to GUI via ipcResponse
          FPipe.SendMessage(ipcResponse, HashResult);
        end;
      end;

    finally
      if Assigned(DeviceFile) then
        DeviceFile.Free;
    end;

    if not FCancelled then
      FPipe.SendMessage(ipcCompleted, '');

  except
    on E: Exception do
    begin
      SendLog('ERROR: ' + E.Message);
      FPipe.SendMessage(ipcError, E.Message);
    end;
  end;
end;

function ExecuteWorkerMode: Integer;
var
  Params: TWorkerParams;
  Pipe: TIPCPipeServer;
  WorkerThread: TWorkerThread;
  WorkerMutex: THandle;
begin
  Result := 1; // Error by default
  
  if not ParseWorkerCommandLine(Params) then
  begin
    Exit;
  end;

  // Check for single worker instance using pipe name as unique identifier
  WorkerMutex := CreateMutex(nil, True, PChar('Global\ImageWriterPro_Worker_' + Params.PipeName));
  if GetLastError = ERROR_ALREADY_EXISTS then
  begin
    // Another worker with same pipe name is already running
    if WorkerMutex <> 0 then
      CloseHandle(WorkerMutex);
    Exit;
  end;

  try
    Pipe := TIPCPipeServer.Create(Params.PipeName);
    try
      if not Pipe.WaitForConnection(30000) then
      begin
        Exit;
      end;

      // Check if running in persistent mode
      if Params.Persistent then
      begin
        // Persistent mode: process commands until shutdown
        ProcessPersistentCommands(Pipe);
        Result := 0;
      end
      else
      begin
        // Single operation mode: execute and exit
        WorkerThread := TWorkerThread.Create(Params, Pipe);
        try
          WorkerThread.WaitFor;
          Result := 0; // Success
        finally
          WorkerThread.Free;
        end;
      end;

    finally
      Pipe.Free;
    end;
  finally
    if WorkerMutex <> 0 then
    begin
      ReleaseMutex(WorkerMutex);
      CloseHandle(WorkerMutex);
    end;
  end;
end;

function BuildWorkerCommandLine(const Params: TWorkerParams): string;
begin
  Result := '--worker';
  Result := Result + ' --pipe "' + Params.PipeName + '"';
  if Params.Persistent then
    Result := Result + ' --persistent'
  else
  begin
    Result := Result + ' --operation ' + IntToStr(Ord(Params.Operation));
    Result := Result + ' --device "' + Params.DevicePath + '"';
    Result := Result + ' --file "' + Params.FilePath + '"';
    Result := Result + ' --bs ' + IntToStr(Params.BlockSize);
    Result := Result + ' --count ' + IntToStr(Params.Count);
    Result := Result + ' --seek ' + IntToStr(Params.Seek);
    Result := Result + ' --skip ' + IntToStr(Params.Skip);
    if Params.CheckSize then
      Result := Result + ' --checksize';
    if Params.VerifyHash then
      Result := Result + ' --verifyhash';
  end;
end;

function ParseWorkerCommandLine(out Params: TWorkerParams): Boolean;
var
  i: Integer;
  Param: string;
  FoundPipe: Boolean;
begin
  FillChar(Params, SizeOf(Params), 0);
  FoundPipe := False;
  Result := False;
  
  i := 1;
  while i <= ParamCount do
  begin
    Param := ParamStr(i);
    
    if Param = '--pipe' then
    begin
      Inc(i);
      if i <= ParamCount then
      begin
        Params.PipeName := ParamStr(i);
        FoundPipe := True;
      end;
    end
    else if Param = '--operation' then
    begin
      Inc(i);
      if i <= ParamCount then
        Params.Operation := TWorkerOperation(StrToIntDef(ParamStr(i), 0));
    end
    else if Param = '--device' then
    begin
      Inc(i);
      if i <= ParamCount then
        Params.DevicePath := ParamStr(i);
    end
    else if Param = '--file' then
    begin
      Inc(i);
      if i <= ParamCount then
        Params.FilePath := ParamStr(i);
    end
    else if Param = '--bs' then
    begin
      Inc(i);
      if i <= ParamCount then
        Params.BlockSize := StrToIntDef(ParamStr(i), 512);
    end
    else if Param = '--count' then
    begin
      Inc(i);
      if i <= ParamCount then
        Params.Count := StrToInt64Def(ParamStr(i), 0);
    end
    else if Param = '--seek' then
    begin
      Inc(i);
      if i <= ParamCount then
        Params.Seek := StrToInt64Def(ParamStr(i), 0);
    end
    else if Param = '--skip' then
    begin
      Inc(i);
      if i <= ParamCount then
        Params.Skip := StrToInt64Def(ParamStr(i), 0);
    end
    else if Param = '--checksize' then
      Params.CheckSize := True
    else if Param = '--verifyhash' then
      Params.VerifyHash := True
    else if Param = '--persistent' then
      Params.Persistent := True;
      
    Inc(i);
  end;
  
  // For persistent mode, only pipe name is required
  if Params.Persistent then
    Result := FoundPipe
  else
    Result := (Params.PipeName <> '') and (Params.DevicePath <> '');
end;

end.
