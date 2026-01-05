{******************************************************************************}
{                                                                              }
{  ImageWriter - Volume Management Unit                                       }
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
{    Volume locking and unlocking operations for safe disk access.            }
{                                                                              }
{******************************************************************************}

unit volume;

interface

uses Windows, sysutils;

const
  kernel32  = 'kernel32.dll';

type
   TFindFirstVolume = function (lpszVolumeName : PAnsiChar; cchBufferLength : DWORD): THANDLE; stdcall;
   TFindNextVolume = function (hFindVolume : THANDLE; lpszVolumeName : PAnsiChar; cchBufferLength : DWORD): BOOL; stdcall;
   TFindVolumeClose = function (hFindVolume : THANDLE): BOOL; stdcall;

   TFindFirstVolumeMountPoint = function (lpszRootPathName : PAnsiChar; lpszVolumeMountPoint : PAnsiChar; cchBufferLength : DWORD): THANDLE; stdcall;
   TFindNextVolumeMountPoint = function (hFindVolumeMountPoint : THANDLE; lpszVolumeMountPoint : PAnsiChar; cchBufferLength : DWORD): BOOL; stdcall;
   TFindVolumeMountPointClose = function (hFindVolumeMountPoint : THANDLE): BOOL; stdcall;

   TGetVolumeNameForVolumeMountPoint = function (lpszVolumeMountPoint : PAnsiChar; lpszVolumeName : PAnsiChar; cchBufferLength : DWORD): BOOL; stdcall;

   TDeleteVolumeMountPoint = function (lpszVolumeMountPoint : PAnsiChar) : BOOL; stdcall;
   TSetVolumeMountPoint = function (lpszVolumeMountPoint : PAnsiChar; lpszVolumeName : PAnsiChar) : BOOL; stdcall;

   procedure LoadVolume;
var
   JFindFirstVolume : TFindFirstVolume;
   JFindNextVolume  : TFindNextVolume;
   JFindVolumeClose : TFindVolumeClose;

   JFindFirstVolumeMountPoint : TFindFirstVolumeMountPoint;
   JFindNextVolumeMountPoint  : TFindNextVolumeMountPoint;
   JFindVolumeMountPointClose : TFindVolumeMountPointClose;

   JGetVolumeNameForVolumeMountPoint : TGetVolumeNameForVolumeMountPoint;

   JDeleteVolumeMountPoint : TDeleteVolumeMountPoint;
   JSetVolumeMountPoint    : TSetVolumeMountPoint;

implementation
var
   VolumeLoaded : Boolean = False;


procedure LoadVolume;
var
   hModule : hInst;
   Error : DWORD;

   function GetAddress(ProcName : PChar) : Pointer;
   begin
      Result := GetProcAddress(hModule, ProcName);
      if Result = nil then
      begin
         // Don't raise exception - just log and return nil
         // Some functions may not be available on older Windows versions
         // raise Exception.Create('Could not find procedure ' + ProcName);
      end;
   end;
begin
   if VolumeLoaded then exit;

   // Load WinINET...
   hModule := GetModuleHandle(kernel32);
   if hModule = 0 then
   begin
      //   kernel32 is not yet loaded... (unlkely but...)
      hModule := LoadLibrary(kernel32);
      if hModule = 0 then
      begin
         Error := GetLastError;
         raise Exception.Create('Error loading kernel32 Library.  ' + SysErrorMessage(Error));
      end;
   end;

   // by here we have an hModule or have raised an exception
   // Map the function addresses...
   JFindFirstVolume := GetAddress('FindFirstVolumeA');
   JFindNextVolume  := GetAddress('FindNextVolumeA');
   JFindVolumeClose := GetAddress('FindVolumeClose');

   JFindFirstVolumeMountPoint := GetAddress('FindFirstVolumeMountPointA');
   JFindNextVolumeMountPoint  := GetAddress('FindNextVolumeMountPointA');
   JFindVolumeMountPointClose := GetAddress('FindVolumeMountPointClose');

   JGetVolumeNameForVolumeMountPoint := GetAddress('GetVolumeNameForVolumeMountPointA');

   JDeleteVolumeMountPoint := GetAddress('DeleteVolumeMountPointA');
   JSetVolumeMountPoint    := GetAddress('SetVolumeMountPointA');

   VolumeLoaded := True;
end;

end.
