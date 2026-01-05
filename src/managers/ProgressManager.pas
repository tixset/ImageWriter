{******************************************************************************}
{                                                                              }
{  ImageWriter - Progress Manager Unit                                        }
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
{    Progress tracking and calculation for I/O operations.                    }
{    Provides speed, ETA calculations and UI update throttling.               }
{                                                                              }
{******************************************************************************}

unit ProgressManager;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, SysUtils, Classes;

type
  /// <summary>
  /// Progress tracking and calculation manager for I/O operations
  /// </summary>
  /// <remarks>
  /// Provides speed calculation, ETA estimation, progress percentage,
  /// and UI update throttling to prevent excessive refreshes.
  /// </remarks>
  TProgressManager = class
  private
    FStartTime: TDateTime;
    FTotalBytes: Int64;
    FLastUpdateTime: TDateTime;
    FUpdateInterval: Double; // seconds
  public
    constructor Create;
    
    /// <summary>
    /// Initialize progress tracking for new operation
    /// </summary>
    /// <param name="TotalBytes">Total number of bytes to process</param>
    /// <remarks>
    /// Records start time and resets update throttling.
    /// </remarks>
    procedure Start(TotalBytes: Int64);
    
    /// <summary>
    /// Calculate progress percentage
    /// </summary>
    /// <param name="CurrentBytes">Bytes processed so far</param>
    /// <param name="TotalBytes">Total bytes to process</param>
    /// <param name="MaxValue">Maximum value for percentage (e.g., 100 or 1000)</param>
    /// <returns>Progress value from 0 to MaxValue</returns>
    /// <remarks>
    /// Using MaxValue=1000 provides 0.1% precision for progress bars.
    /// </remarks>
    function CalculatePercent(CurrentBytes, TotalBytes: Int64; MaxValue: Integer): Integer;
    
    /// <summary>
    /// Calculate current transfer speed
    /// </summary>
    /// <param name="CurrentBytes">Bytes transferred since start</param>
    /// <param name="Speed">Output: speed in bytes per second</param>
    /// <returns>True if calculation successful, False if insufficient time elapsed</returns>
    /// <remarks>
    /// Returns False if less than 100ms elapsed to avoid division by zero.
    /// </remarks>
    function CalculateSpeed(CurrentBytes: Int64; out Speed: Double): Boolean;
    
    /// <summary>
    /// Calculate estimated time to completion
    /// </summary>
    /// <param name="CurrentBytes">Bytes transferred so far</param>
    /// <param name="Speed">Current transfer speed (bytes/sec)</param>
    /// <returns>Estimated seconds until completion</returns>
    /// <remarks>
    /// Returns 0 if speed is zero or CurrentBytes >= TotalBytes.
    /// </remarks>
    function CalculateETA(CurrentBytes: Int64; Speed: Double): Double;
    
    /// <summary>
    /// Format ETA for user display
    /// </summary>
    /// <param name="Seconds">ETA in seconds</param>
    /// <returns>Formatted string (e.g., "5m 32s", "1h 15m 30s")</returns>
    function FormatETA(Seconds: Double): string;
    
    /// <summary>
    /// Format transfer speed for user display
    /// </summary>
    /// <param name="BytesPerSec">Speed in bytes per second</param>
    /// <returns>Formatted string with appropriate units (B/s, KB/s, MB/s, GB/s)</returns>
    function FormatSpeed(BytesPerSec: Double): string;
    
    /// <summary>
    /// Check if UI should be updated (throttling)
    /// </summary>
    /// <returns>True if UpdateInterval has elapsed since last update</returns>
    /// <remarks>
    /// Prevents excessive UI updates. Default interval is 0.5 seconds.
    /// </remarks>
    function ShouldUpdate: Boolean;
    
    /// <summary>
    /// Reset progress tracking state
    /// </summary>
    procedure Reset;
    
    /// <summary>
    /// Operation start timestamp
    /// </summary>
    property StartTime: TDateTime read FStartTime write FStartTime;
    
    /// <summary>
    /// Total bytes to process in current operation
    /// </summary>
    property TotalBytes: Int64 read FTotalBytes write FTotalBytes;
    
    /// <summary>
    /// Minimum seconds between UI updates (default 0.5)
    /// </summary>
    property UpdateInterval: Double read FUpdateInterval write FUpdateInterval;
  end;

implementation

uses
  FileOperations;

{ TProgressManager }

constructor TProgressManager.Create;
begin
  inherited Create;
  FUpdateInterval := 0.5; // Update every 0.5 seconds by default
  Reset;
end;

procedure TProgressManager.Start(TotalBytes: Int64);
begin
  FStartTime := Now;
  FTotalBytes := TotalBytes;
  FLastUpdateTime := 0;
end;

function TProgressManager.CalculatePercent(CurrentBytes, TotalBytes: Int64; MaxValue: Integer): Integer;
begin
  Result := 0;
  if TotalBytes > 0 then
  begin
    Result := Integer((CurrentBytes * MaxValue) div TotalBytes);
    if Result < 0 then
      Result := 0;
    if Result > MaxValue then
      Result := MaxValue;
  end;
end;

function TProgressManager.CalculateSpeed(CurrentBytes: Int64; out Speed: Double): Boolean;
var
  Elapsed: Double;
begin
  Result := False;
  Speed := 0;
  
  Elapsed := (Now - FStartTime) * 86400; // seconds
  if Elapsed > 0.5 then
  begin
    Speed := CurrentBytes / Elapsed; // bytes per second
    Result := True;
  end;
end;

function TProgressManager.CalculateETA(CurrentBytes: Int64; Speed: Double): Double;
begin
  Result := 0;
  if (Speed > 0) and (FTotalBytes > CurrentBytes) then
    Result := (FTotalBytes - CurrentBytes) / Speed;
end;

function TProgressManager.FormatETA(Seconds: Double): string;
begin
  if Seconds < 60 then
    Result := Format('%.0fs', [Seconds])
  else if Seconds < 3600 then
    Result := Format('%.1fm', [Seconds / 60])
  else
    Result := Format('%.1fh', [Seconds / 3600]);
end;

function TProgressManager.FormatSpeed(BytesPerSec: Double): string;
begin
  Result := Format('%s/s', [TFileOperations.BytesShort(Trunc(BytesPerSec))]);
end;

function TProgressManager.ShouldUpdate: Boolean;
var
  CurrentTime: TDateTime;
  TimeSinceLastUpdate: Double;
begin
  CurrentTime := Now;
  TimeSinceLastUpdate := (CurrentTime - FLastUpdateTime) * 86400; // seconds
  
  Result := (TimeSinceLastUpdate >= FUpdateInterval) or (FLastUpdateTime = 0);
  if Result then
    FLastUpdateTime := CurrentTime;
end;

procedure TProgressManager.Reset;
begin
  FStartTime := 0;
  FTotalBytes := 0;
  FLastUpdateTime := 0;
end;

end.
