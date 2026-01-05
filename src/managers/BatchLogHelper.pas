{******************************************************************************}
{                                                                              }
{  ImageWriter - Batch Logging Helper Unit                                    }
{                                                                              }
{  Copyright (c) 2024-2025 Anton Zelenov <tixset@gmail.com>                   }
{  GitHub: https://github.com/tixset/ImageWriter                              }
{                                                                              }
{  Description:                                                                }
{    Helper for batching high-frequency log messages to prevent spam.         }
{    Based on recommendations from LOG_FORMAT_SPECIFICATION.md                }
{                                                                              }
{******************************************************************************}

unit BatchLogHelper;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, SysUtils, LogUtils;

type
  /// <summary>
  /// Helper for batching log messages during disk operations
  /// </summary>
  /// <remarks>
  /// Prevents log spam by logging only every N operations or every T seconds.
  /// Use for high-frequency operations like block-by-block disk I/O.
  /// </remarks>
  TBatchLogHelper = class
  private
    FLastLogTime: Cardinal;        // GetTickCount of last log
    FLastLogCounter: Int64;        // Counter value at last log
    FBatchInterval: Integer;       // Log every N operations
    FTimeInterval: Cardinal;       // Log every T milliseconds
    FOperationName: string;        // Operation name for logs
  public
    /// <summary>
    /// Create batch log helper
    /// </summary>
    /// <param name="OperationName">Name of operation (e.g., 'Write', 'Read')</param>
    /// <param name="BatchInterval">Log every N operations (default: 100)</param>
    /// <param name="TimeInterval">Log every T milliseconds (default: 5000)</param>
    constructor Create(const OperationName: string; BatchInterval: Integer = 100; TimeInterval: Cardinal = 5000);
    
    /// <summary>
    /// Check if should log now and log if needed
    /// </summary>
    /// <param name="Counter">Current operation counter (e.g., block number)</param>
    /// <param name="Message">Message to log</param>
    /// <param name="Level">Log level</param>
    /// <returns>True if logged, False if skipped</returns>
    function LogIfNeeded(Counter: Int64; const Message: string; Level: TLogLevel = llInfo): Boolean;
    
    /// <summary>
    /// Force logging regardless of intervals
    /// </summary>
    /// <param name="Counter">Current counter value</param>
    /// <param name="Message">Message to log</param>
    /// <param name="Level">Log level</param>
    procedure ForceLog(Counter: Int64; const Message: string; Level: TLogLevel = llInfo);
    
    /// <summary>
    /// Reset helper state (call at operation start)
    /// </summary>
    procedure Reset;
  end;

implementation

{ TBatchLogHelper }

constructor TBatchLogHelper.Create(const OperationName: string; BatchInterval: Integer; TimeInterval: Cardinal);
begin
  inherited Create;
  FOperationName := OperationName;
  FBatchInterval := BatchInterval;
  FTimeInterval := TimeInterval;
  Reset;
end;

function TBatchLogHelper.LogIfNeeded(Counter: Int64; const Message: string; Level: TLogLevel): Boolean;
var
  CurrentTime: Cardinal;
  TimeDiff: Cardinal;
  CounterDiff: Int64;
begin
  Result := False;
  CurrentTime := GetTickCount;
  
  // Calculate differences
  if CurrentTime >= FLastLogTime then
    TimeDiff := CurrentTime - FLastLogTime
  else
    TimeDiff := (High(Cardinal) - FLastLogTime) + CurrentTime; // Handle wraparound
  
  CounterDiff := Counter - FLastLogCounter;
  
  // Check if should log
  if (CounterDiff >= FBatchInterval) or (TimeDiff >= FTimeInterval) then
  begin
    ForceLog(Counter, Message, Level);
    Result := True;
  end;
end;

procedure TBatchLogHelper.ForceLog(Counter: Int64; const Message: string; Level: TLogLevel);
begin
  TLogUtils.Log(FOperationName + ': ' + Message, Level);
  FLastLogTime := GetTickCount;
  FLastLogCounter := Counter;
end;

procedure TBatchLogHelper.Reset;
begin
  FLastLogTime := GetTickCount;
  FLastLogCounter := 0;
end;

end.
