unit SevenZipStream;

{$I+}  { Enable I/O checking }

interface

uses
  Windows, Classes, SysUtils;

type
  /// <summary>
  /// Exception class for 7-Zip errors
  /// </summary>
  ESevenZipError = class(Exception);

  /// <summary>
  /// Streaming 7z decompressor implementing TStream interface
  /// Uses 7z.exe with stdout pipe (no temporary files)
  /// </summary>
  /// <remarks>
  /// Requires 7z.exe in application directory or system PATH.
  /// Extracts first file from 7z archive via stdout pipe: 7z.exe x -so archive.7z
  /// Streaming decompression without temporary files.
  /// </remarks>
  TSevenZipInputStream = class(TStream)
  private
    FArchiveFile: string;
    FProcessHandle: THandle;
    FThreadHandle: THandle;
    FStdoutRead: THandle;
    FStdoutWrite: THandle;
    FFinished: Boolean;
    FTotalBytesRead: Int64;
    
    procedure StartExtraction;
    procedure Cleanup;
    function Find7ZipExecutable: string;
  protected
    function GetSize: Int64; override;
  public
    /// <summary>
    /// Creates 7z input stream from file path
    /// </summary>
    /// <param name="AFileName">7z archive file path</param>
    constructor Create(const AFileName: string);
    
    destructor Destroy; override;
    
    /// <summary>
    /// Reads decompressed data from stdout pipe
    /// </summary>
    /// <param name="Buffer">Destination buffer</param>
    /// <param name="Count">Number of bytes to read</param>
    /// <returns>Actual number of bytes read</returns>
    function Read(var Buffer; Count: Longint): Longint; override;
    
    /// <summary>
    /// Write not supported for input stream
    /// </summary>
    function Write(const Buffer; Count: Longint): Longint; override;
    
    /// <summary>
    /// Seek not supported for pipe stream
    /// </summary>
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  /// <summary>
  /// Streaming 7z compressor implementing TStream interface
  /// Uses 7z.exe with stdin pipe (no temporary files)
  /// </summary>
  /// <remarks>
  /// Compresses data from stdin pipe to archive: 7z.exe a -si -txz archive.xz
  /// Supports multiple formats: 7z, xz, bz2, gz, tar
  /// </remarks>
  TSevenZipOutputStream = class(TStream)
  private
    FArchiveFile: string;
    FArchiveFormat: string; // txz, tbz2, tgz, t7z
    FProcessHandle: THandle;
    FThreadHandle: THandle;
    FStdinRead: THandle;
    FStdinWrite: THandle;
    FFinished: Boolean;
    FTotalBytesWritten: Int64;
    
    procedure StartCompression;
    procedure Cleanup;
    function Find7ZipExecutable: string;
  protected
    function GetSize: Int64; override;
  public
    /// <summary>
    /// Creates 7z output stream for compression
    /// </summary>
    /// <param name="AFileName">Archive file path (.7z, .xz, .bz2, .gz, .tar.gz, etc.)</param>
    /// <param name="AFormat">Archive format override (txz, tbz2, tgz, t7z). Auto-detected if empty.</param>
    constructor Create(const AFileName: string; const AFormat: string = '');
    
    destructor Destroy; override;
    
    /// <summary>
    /// Read not supported for output stream
    /// </summary>
    function Read(var Buffer; Count: Longint): Longint; override;
    
    /// <summary>
    /// Writes data to stdin pipe for compression
    /// </summary>
    /// <param name="Buffer">Source buffer</param>
    /// <param name="Count">Number of bytes to write</param>
    /// <returns>Actual number of bytes written</returns>
    function Write(const Buffer; Count: Longint): Longint; override;
    
    /// <summary>
    /// Seek not supported for pipe stream
    /// </summary>
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;


implementation

const
  SEVEN_ZIP_MAGIC: array[0..5] of Byte = ($37, $7A, $BC, $AF, $27, $1C);

{ Helper function to find 7-Zip executable }
function FindSevenZipExe: string;
begin
  Result := '';
  
  // Check application directory
  if FileExists(ExtractFilePath(ParamStr(0)) + '7z.exe') then
    Result := ExtractFilePath(ParamStr(0)) + '7z.exe'
  else if FileExists(ExtractFilePath(ParamStr(0)) + '7za.exe') then
    Result := ExtractFilePath(ParamStr(0)) + '7za.exe'
  // Check Program Files
  else if FileExists('C:\Program Files\7-Zip\7z.exe') then
    Result := 'C:\Program Files\7-Zip\7z.exe'
  else if FileExists('C:\Program Files (x86)\7-Zip\7z.exe') then
    Result := 'C:\Program Files (x86)\7-Zip\7z.exe';
end;

{ TSevenZipInputStream }

constructor TSevenZipInputStream.Create(const AFileName: string);
begin
  inherited Create;
  FArchiveFile := AFileName;
  FProcessHandle := 0;
  FThreadHandle := 0;
  FStdoutRead := 0;
  FStdoutWrite := 0;
  FFinished := False;
  FTotalBytesRead := 0;
  
  if not FileExists(FArchiveFile) then
    raise ESevenZipError.Create('Archive file not found: ' + FArchiveFile);
  
  StartExtraction;
end;

destructor TSevenZipInputStream.Destroy;
begin
  Cleanup;
  inherited;
end;

function TSevenZipInputStream.Find7ZipExecutable: string;
begin
  Result := FindSevenZipExe;
  if Result = '' then
    raise ESevenZipError.Create('7-Zip not found. Please install 7-Zip or place 7z.exe in application directory.');
end;

procedure TSevenZipInputStream.StartExtraction;
var
  SevenZipExe: string;
  CommandLine: string;
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  SecurityAttr: TSecurityAttributes;
begin
  SevenZipExe := Find7ZipExecutable;
  
  // Create pipe for stdout
  SecurityAttr.nLength := SizeOf(TSecurityAttributes);
  SecurityAttr.bInheritHandle := True;
  SecurityAttr.lpSecurityDescriptor := nil;
  
  if not CreatePipe(FStdoutRead, FStdoutWrite, @SecurityAttr, 0) then
    raise ESevenZipError.Create('Failed to create stdout pipe');
  
  // Ensure read handle is not inherited
  SetHandleInformation(FStdoutRead, HANDLE_FLAG_INHERIT, 0);
  
  try
    // Build command line: 7z.exe x -so "archive.7z"
    CommandLine := '"' + SevenZipExe + '" x -so "' + FArchiveFile + '"';
    
    // Setup process startup info
    FillChar(StartInfo, SizeOf(StartInfo), 0);
    StartInfo.cb := SizeOf(StartInfo);
    StartInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    StartInfo.wShowWindow := SW_HIDE;
    StartInfo.hStdOutput := FStdoutWrite;
    StartInfo.hStdError := GetStdHandle(STD_ERROR_HANDLE);
    StartInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
    
    // Create 7z process
    if not CreateProcess(nil, PChar(CommandLine), nil, nil, True,
                        CREATE_NO_WINDOW, nil, nil, StartInfo, ProcInfo) then
    begin
      Cleanup;
      raise ESevenZipError.Create('Failed to start 7-Zip process');
    end;
    
    FProcessHandle := ProcInfo.hProcess;
    FThreadHandle := ProcInfo.hThread;
    
    // Close write end of pipe (7z process owns it now)
    CloseHandle(FStdoutWrite);
    FStdoutWrite := 0;
    
  except
    Cleanup;
    raise;
  end;
end;

procedure TSevenZipInputStream.Cleanup;
var
  ExitCode: DWORD;
begin
  // Close pipe handles
  if FStdoutRead <> 0 then
  begin
    CloseHandle(FStdoutRead);
    FStdoutRead := 0;
  end;
  
  if FStdoutWrite <> 0 then
  begin
    CloseHandle(FStdoutWrite);
    FStdoutWrite := 0;
  end;
  
  // Terminate process if still running
  if FProcessHandle <> 0 then
  begin
    if GetExitCodeProcess(FProcessHandle, ExitCode) then
    begin
      if ExitCode = STILL_ACTIVE then
        TerminateProcess(FProcessHandle, 1);
    end;
    CloseHandle(FProcessHandle);
    FProcessHandle := 0;
  end;
  
  if FThreadHandle <> 0 then
  begin
    CloseHandle(FThreadHandle);
    FThreadHandle := 0;
  end;
end;

function TSevenZipInputStream.Read(var Buffer; Count: Longint): Longint;
var
  BytesRead: DWORD;
begin
  Result := 0;
  
  if FFinished or (FStdoutRead = 0) then
    Exit;
  
  if not ReadFile(FStdoutRead, Buffer, Count, BytesRead, nil) then
  begin
    FFinished := True;
    Exit;
  end;
  
  Result := BytesRead;
  Inc(FTotalBytesRead, BytesRead);
  
  if BytesRead = 0 then
    FFinished := True;
end;

function TSevenZipInputStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise ESevenZipError.Create('Cannot write to 7z input stream');
end;

function TSevenZipInputStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  raise ESevenZipError.Create('Seek not supported for pipe stream');
end;

function TSevenZipInputStream.GetSize: Int64;
begin
  // Size unknown for streaming decompression
  Result := -1;
end;

{ TSevenZipOutputStream }

constructor TSevenZipOutputStream.Create(const AFileName: string; const AFormat: string = '');
var
  Ext: string;
begin
  inherited Create;
  FArchiveFile := AFileName;
  FProcessHandle := 0;
  FThreadHandle := 0;
  FStdinRead := 0;
  FStdinWrite := 0;
  FFinished := False;
  FTotalBytesWritten := 0;
  
  // Auto-detect format from extension if not specified
  if AFormat <> '' then
    FArchiveFormat := AFormat
  else
  begin
    Ext := LowerCase(ExtractFileExt(AFileName));
    if Ext = '.xz' then
      FArchiveFormat := 'txz'
    else if Ext = '.bz2' then
      FArchiveFormat := 'tbz2'
    else if (Ext = '.gz') or (Ext = '.gzip') then
      FArchiveFormat := 'tgz'
    else if Ext = '.7z' then
      FArchiveFormat := 't7z'
    else
      FArchiveFormat := 't7z'; // Default to 7z format
  end;
  
  StartCompression;
end;

destructor TSevenZipOutputStream.Destroy;
var
  ExitCode: DWORD;
begin
  // Close stdin write to signal EOF
  if FStdinWrite <> 0 then
  begin
    CloseHandle(FStdinWrite);
    FStdinWrite := 0;
  end;
  
  // Wait for compression to complete
  if FProcessHandle <> 0 then
  begin
    WaitForSingleObject(FProcessHandle, 30000); // 30 second timeout
    GetExitCodeProcess(FProcessHandle, ExitCode);
  end;
  
  Cleanup;
  inherited;
end;

function TSevenZipOutputStream.Find7ZipExecutable: string;
begin
  Result := FindSevenZipExe;
  if Result = '' then
    raise ESevenZipError.Create('7-Zip not found. Please install 7-Zip or place 7z.exe in application directory.');
end;

procedure TSevenZipOutputStream.StartCompression;
var
  SevenZipExe: string;
  CommandLine: string;
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  SecurityAttr: TSecurityAttributes;
begin
  SevenZipExe := Find7ZipExecutable;
  
  // Create pipe for stdin
  SecurityAttr.nLength := SizeOf(TSecurityAttributes);
  SecurityAttr.bInheritHandle := True;
  SecurityAttr.lpSecurityDescriptor := nil;
  
  if not CreatePipe(FStdinRead, FStdinWrite, @SecurityAttr, 0) then
    raise ESevenZipError.Create('Failed to create stdin pipe');
  
  // Ensure write handle is not inherited
  SetHandleInformation(FStdinWrite, HANDLE_FLAG_INHERIT, 0);
  
  try
    // Build command line: 7z.exe a -si -txz "archive.xz" -mx=5
    CommandLine := '"' + SevenZipExe + '" a -si -' + FArchiveFormat + 
                   ' "' + FArchiveFile + '" -mx=5';
    
    // Setup process startup info
    FillChar(StartInfo, SizeOf(StartInfo), 0);
    StartInfo.cb := SizeOf(StartInfo);
    StartInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    StartInfo.wShowWindow := SW_HIDE;
    StartInfo.hStdInput := FStdinRead;
    StartInfo.hStdOutput := GetStdHandle(STD_OUTPUT_HANDLE);
    StartInfo.hStdError := GetStdHandle(STD_ERROR_HANDLE);
    
    // Create 7z process
    if not CreateProcess(nil, PChar(CommandLine), nil, nil, True,
                        CREATE_NO_WINDOW, nil, nil, StartInfo, ProcInfo) then
    begin
      Cleanup;
      raise ESevenZipError.Create('Failed to start 7-Zip compression process');
    end;
    
    FProcessHandle := ProcInfo.hProcess;
    FThreadHandle := ProcInfo.hThread;
    
    // Close read end of pipe (7z process owns it now)
    CloseHandle(FStdinRead);
    FStdinRead := 0;
    
  except
    Cleanup;
    raise;
  end;
end;

procedure TSevenZipOutputStream.Cleanup;
begin
  if FStdinRead <> 0 then
  begin
    CloseHandle(FStdinRead);
    FStdinRead := 0;
  end;
  
  if FStdinWrite <> 0 then
  begin
    CloseHandle(FStdinWrite);
    FStdinWrite := 0;
  end;
  
  if FProcessHandle <> 0 then
  begin
    CloseHandle(FProcessHandle);
    FProcessHandle := 0;
  end;
  
  if FThreadHandle <> 0 then
  begin
    CloseHandle(FThreadHandle);
    FThreadHandle := 0;
  end;
end;

function TSevenZipOutputStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise ESevenZipError.Create('Cannot read from 7z output stream');
end;

function TSevenZipOutputStream.Write(const Buffer; Count: Longint): Longint;
var
  BytesWritten: DWORD;
begin
  Result := 0;
  
  if FFinished or (FStdinWrite = 0) then
    Exit;
  
  if not WriteFile(FStdinWrite, Buffer, Count, BytesWritten, nil) then
  begin
    FFinished := True;
    raise ESevenZipError.Create('Failed to write to 7z stdin pipe');
  end;
  
  Result := BytesWritten;
  Inc(FTotalBytesWritten, BytesWritten);
end;

function TSevenZipOutputStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  raise ESevenZipError.Create('Seek not supported for pipe stream');
end;

function TSevenZipOutputStream.GetSize: Int64;
begin
  Result := FTotalBytesWritten;
end;

end.
