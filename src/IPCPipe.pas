{******************************************************************************}
{                                                                              }
{  ImageWriter - Inter-Process Communication Pipe Unit                        }
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
{    Provides named pipe communication between main GUI and elevated worker.  }
{    Handles command serialization and progress reporting.                    }
{                                                                              }
{******************************************************************************}

unit IPCPipe;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, SysUtils, Classes;

type
  // Message types for IPC communication
  TIPCMessageType = (
    ipcLog,           // Log message
    ipcProgress,      // Progress update
    ipcCompleted,     // Operation completed
    ipcError,         // Error occurred
    ipcCancel,        // Cancel request from GUI
    ipcData,          // Data block for write operation
    ipcReady,         // Worker ready to receive data
    ipcCommand,       // Command from GUI to worker
    ipcResponse,      // Response from worker to GUI
    ipcShutdown,      // Shutdown worker
    ipcLockVolume,    // Lock volume command
    ipcUnlockVolume   // Unlock volume command
  );

  // IPC Message structure
  TIPCMessage = record
    MessageType: TIPCMessageType;
    DataSize: Integer;
    // Data follows after this header
  end;
  PIPCMessage = ^TIPCMessage;

  // Server side (Worker process)
  TIPCPipeServer = class
  private
    FPipeName: string;
    FPipeHandle: THandle;
    FConnected: Boolean;
  public
    constructor Create(const APipeName: string);
    destructor Destroy; override;
    function WaitForConnection(TimeoutMS: DWORD = INFINITE): Boolean;
    function SendMessage(MsgType: TIPCMessageType; const Data: string): Boolean;
    function ReceiveMessage(out MsgType: TIPCMessageType; out Data: string; TimeoutMS: DWORD = 0): Boolean;
    procedure Disconnect;
    property Connected: Boolean read FConnected;
  end;

  // Client side (GUI process)
  TIPCPipeClient = class
  private
    FPipeName: string;
    FPipeHandle: THandle;
    FConnected: Boolean;
  public
    constructor Create(const APipeName: string);
    destructor Destroy; override;
    function Connect(TimeoutMS: DWORD = 5000): Boolean;
    function SendMessage(MsgType: TIPCMessageType; const Data: string): Boolean;
    function ReceiveMessage(out MsgType: TIPCMessageType; out Data: string; TimeoutMS: DWORD = 0): Boolean;
    procedure Disconnect;
    property Connected: Boolean read FConnected;
  end;

implementation

const
  PIPE_BUFFER_SIZE = 65536;

{ TIPCPipeServer }

constructor TIPCPipeServer.Create(const APipeName: string);
begin
  inherited Create;
  FPipeName := APipeName;
  FPipeHandle := INVALID_HANDLE_VALUE;
  FConnected := False;
end;

destructor TIPCPipeServer.Destroy;
begin
  Disconnect;
  inherited;
end;

function TIPCPipeServer.WaitForConnection(TimeoutMS: DWORD = INFINITE): Boolean;
var
  sa: SECURITY_ATTRIBUTES;
  sd: SECURITY_DESCRIPTOR;
begin
  Result := False;
  
  if FConnected then
    Exit;

  // Create security descriptor that allows everyone to connect
  InitializeSecurityDescriptor(@sd, SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@sd, True, nil, False);
  sa.nLength := SizeOf(SECURITY_ATTRIBUTES);
  sa.lpSecurityDescriptor := @sd;
  sa.bInheritHandle := False;

  FPipeHandle := CreateNamedPipe(
    PChar(FPipeName),
    PIPE_ACCESS_DUPLEX,
    PIPE_TYPE_MESSAGE or PIPE_READMODE_MESSAGE or PIPE_WAIT,
    1, // Only one instance
    PIPE_BUFFER_SIZE,
    PIPE_BUFFER_SIZE,
    TimeoutMS,
    @sa
  );

  if FPipeHandle = INVALID_HANDLE_VALUE then
    Exit;

  Result := ConnectNamedPipe(FPipeHandle, nil);
  if not Result then
  begin
    // ERROR_PIPE_CONNECTED means client connected before ConnectNamedPipe was called
    Result := GetLastError = ERROR_PIPE_CONNECTED;
  end;

  FConnected := Result;
end;

function TIPCPipeServer.SendMessage(MsgType: TIPCMessageType; const Data: string): Boolean;
var
  Header: TIPCMessage;
  BytesWritten: DWORD;
  DataBytes: array of Byte;
begin
  Result := False;
  
  if not FConnected then
    Exit;

  SetLength(DataBytes, Length(Data));
  if Length(Data) > 0 then
    Move(Data[1], DataBytes[0], Length(Data));

  Header.MessageType := MsgType;
  Header.DataSize := Length(DataBytes);

  // Write header
  if not WriteFile(FPipeHandle, Header, SizeOf(Header), BytesWritten, nil) then
    Exit;

  // Write data if any
  if Header.DataSize > 0 then
  begin
    if not WriteFile(FPipeHandle, DataBytes[0], Header.DataSize, BytesWritten, nil) then
      Exit;
  end;

  FlushFileBuffers(FPipeHandle);
  Result := True;
end;

function TIPCPipeServer.ReceiveMessage(out MsgType: TIPCMessageType; out Data: string; TimeoutMS: DWORD = 0): Boolean;
var
  Header: TIPCMessage;
  BytesRead: DWORD;
  DataBytes: array of Byte;
  Available: DWORD;
begin
  Result := False;
  Data := '';
  
  if not FConnected then
    Exit;

  // Check if data is available (non-blocking check if TimeoutMS = 0)
  if TimeoutMS = 0 then
  begin
    if not PeekNamedPipe(FPipeHandle, nil, 0, nil, @Available, nil) then
      Exit;
    if Available = 0 then
      Exit;
  end;

  // Read header
  if not ReadFile(FPipeHandle, Header, SizeOf(Header), BytesRead, nil) then
    Exit;

  if BytesRead <> SizeOf(Header) then
    Exit;

  MsgType := Header.MessageType;

  // Read data if any
  if Header.DataSize > 0 then
  begin
    SetLength(DataBytes, Header.DataSize);
    if not ReadFile(FPipeHandle, DataBytes[0], Header.DataSize, BytesRead, nil) then
      Exit;
      
    if BytesRead <> DWORD(Header.DataSize) then
      Exit;
      
    SetLength(Data, Header.DataSize);
    Move(DataBytes[0], Data[1], Header.DataSize);
  end;

  Result := True;
end;

procedure TIPCPipeServer.Disconnect;
begin
  if FPipeHandle <> INVALID_HANDLE_VALUE then
  begin
    if FConnected then
      DisconnectNamedPipe(FPipeHandle);
    CloseHandle(FPipeHandle);
    FPipeHandle := INVALID_HANDLE_VALUE;
  end;
  FConnected := False;
end;

{ TIPCPipeClient }

constructor TIPCPipeClient.Create(const APipeName: string);
begin
  inherited Create;
  FPipeName := APipeName;
  FPipeHandle := INVALID_HANDLE_VALUE;
  FConnected := False;
end;

destructor TIPCPipeClient.Destroy;
begin
  Disconnect;
  inherited;
end;

function TIPCPipeClient.Connect(TimeoutMS: DWORD = 5000): Boolean;
var
  StartTime: DWORD;
begin
  Result := False;
  
  if FConnected then
    Exit;

  StartTime := GetTickCount;
  
  // Try to connect within timeout period
  repeat
    FPipeHandle := CreateFile(
      PChar(FPipeName),
      GENERIC_READ or GENERIC_WRITE,
      0,
      nil,
      OPEN_EXISTING,
      0,
      0
    );

    if FPipeHandle <> INVALID_HANDLE_VALUE then
    begin
      FConnected := True;
      Result := True;
      Exit;
    end;

    // If pipe doesn't exist yet, wait a bit and retry
    if GetLastError = ERROR_FILE_NOT_FOUND then
      Sleep(100)
    else
      Exit; // Other error, give up

  until (GetTickCount - StartTime) >= TimeoutMS;
end;

function TIPCPipeClient.SendMessage(MsgType: TIPCMessageType; const Data: string): Boolean;
var
  Header: TIPCMessage;
  BytesWritten: DWORD;
  DataBytes: array of Byte;
begin
  Result := False;
  
  if not FConnected then
    Exit;

  SetLength(DataBytes, Length(Data));
  if Length(Data) > 0 then
    Move(Data[1], DataBytes[0], Length(Data));

  Header.MessageType := MsgType;
  Header.DataSize := Length(DataBytes);

  // Write header
  if not WriteFile(FPipeHandle, Header, SizeOf(Header), BytesWritten, nil) then
    Exit;

  // Write data if any
  if Header.DataSize > 0 then
  begin
    if not WriteFile(FPipeHandle, DataBytes[0], Header.DataSize, BytesWritten, nil) then
      Exit;
  end;

  FlushFileBuffers(FPipeHandle);
  Result := True;
end;

function TIPCPipeClient.ReceiveMessage(out MsgType: TIPCMessageType; out Data: string; TimeoutMS: DWORD = 0): Boolean;
var
  Header: TIPCMessage;
  BytesRead: DWORD;
  DataBytes: array of Byte;
  Available: DWORD;
begin
  Result := False;
  Data := '';
  
  if not FConnected then
    Exit;

  // Check if data is available
  if TimeoutMS = 0 then
  begin
    if not PeekNamedPipe(FPipeHandle, nil, 0, nil, @Available, nil) then
      Exit;
    if Available = 0 then
      Exit;
  end;

  // Read header
  if not ReadFile(FPipeHandle, Header, SizeOf(Header), BytesRead, nil) then
    Exit;

  if BytesRead <> SizeOf(Header) then
    Exit;

  MsgType := Header.MessageType;

  // Read data if any
  if Header.DataSize > 0 then
  begin
    SetLength(DataBytes, Header.DataSize);
    if not ReadFile(FPipeHandle, DataBytes[0], Header.DataSize, BytesRead, nil) then
      Exit;
      
    if BytesRead <> DWORD(Header.DataSize) then
      Exit;
      
    SetLength(Data, Header.DataSize);
    Move(DataBytes[0], Data[1], Header.DataSize);
  end;

  Result := True;
end;

procedure TIPCPipeClient.Disconnect;
begin
  if FPipeHandle <> INVALID_HANDLE_VALUE then
  begin
    CloseHandle(FPipeHandle);
    FPipeHandle := INVALID_HANDLE_VALUE;
  end;
  FConnected := False;
end;

end.
