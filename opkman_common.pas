{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

 Author: Balázs Székely
 Abstract:
   Common functions, procedures.
}
unit opkman_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Forms, Controls, LazIDEIntf, LazFileUtils;

type
  TPackageAction = (paDownloadTo, paInstall);

var
  LocalRepository: String;
  LocalRepositoryArchive: String;
  LocalRepositoryPackages: String;
  LocalRepositoryConfig: String;
  LocalRepositoryConfigFile: String;
  ConfigFile: String;
  PackageAction: TPackageAction;
  ForceDownload: Boolean = False;
  ForceExtract: Boolean = False;

function MessageDlgEx(const AMsg: String; ADlgType: TMsgDlgType;  AButtons:
  TMsgDlgButtons; AParent: TForm): TModalResult;
function InitLocalRepository: Boolean;
function SecToHourAndMin(const ASec: LongInt): String;
function FormatSize(Size: Int64): String;
function FormatSpeed(Speed: LongInt): String;
function GetDirSize(const ADirName: String; var AFileCnt, ADirCnt: Integer): Int64;

implementation
uses opkman_const;

function MessageDlgEx(const AMsg: string; ADlgType: TMsgDlgType;
  AButtons: TMsgDlgButtons; AParent: TForm): TModalResult;
var
  MsgFrm: TForm;
begin
  MsgFrm := CreateMessageDialog(AMsg, ADlgType, AButtons);
  try
    MsgFrm.Position := poDefaultSizeOnly;
    MsgFrm.FormStyle := fsSystemStayOnTop;
    MsgFrm.Left := AParent.Left + (AParent.Width - MsgFrm.Width) div 2;
    MsgFrm.Top := AParent.Top + (AParent.Height - MsgFrm.Height) div 2;
    Result := MsgFrm.ShowModal;
  finally
    MsgFrm.Free
  end;
end;

function InitLocalRepository: Boolean;
begin
  LocalRepository := AppendPathDelim(AppendPathDelim(LazarusIDE.GetPrimaryConfigPath) + cLocalRepository);
  if not DirectoryExistsUTF8(LocalRepository) then
    CreateDirUTF8(LocalRepository);
  LocalRepositoryArchive := AppendPathDelim(LocalRepository + AppendPathDelim(cLocalRepositoryArchive));
  if not DirectoryExistsUTF8(LocalRepositoryArchive) then
    CreateDirUTF8(LocalRepositoryArchive);
  LocalRepositoryPackages := AppendPathDelim(LocalRepository + AppendPathDelim(cLocalRepositoryPackages));
  if not DirectoryExists(LocalRepositoryPackages) then
    CreateDir(LocalRepositoryPackages);
  LocalRepositoryConfig := AppendPathDelim(LocalRepository + AppendPathDelim(cLocalRepositoryConfig));
  if not DirectoryExists(LocalRepositoryConfig) then
    CreateDir(LocalRepositoryConfig);
  LocalRepositoryConfigFile := LocalRepositoryConfig + cLocalRepositoryConfigFile;
  Result := DirectoryExistsUTF8(LocalRepository) and
            DirectoryExistsUTF8(LocalRepositoryArchive) and
            DirectoryExistsUTF8(LocalRepositoryConfig) and
            DirectoryExistsUTF8(LocalRepositoryPackages);
end;

function SecToHourAndMin(const ASec: LongInt): String;
var
  Hour, Min, Sec: LongInt;
begin
   Hour := Trunc(ASec/3600);
   Min  := Trunc((ASec - Hour*3600)/60);
   Sec  := ASec - Hour*3600 - 60*Min;
   Result := IntToStr(Hour) + 'h: ' + IntToStr(Min) + 'm: ' + IntToStr(Sec) + 's';
end;

function FormatSize(Size: Int64): String;
const
  KB = 1024;
  MB = 1024 * KB;
  GB = 1024 * MB;
begin
  if Size < KB then
    Result := FormatFloat('#,##0 Bytes', Size)
  else if Size < MB then
    Result := FormatFloat('#,##0.0 KB', Size / KB)
  else if Size < GB then
    Result := FormatFloat('#,##0.0 MB', Size / MB)
  else
    Result := FormatFloat('#,##0.0 GB', Size / GB);
end;

function FormatSpeed(Speed: LongInt): String;
const
  KB = 1024;
  MB = 1024 * KB;
  GB = 1024 * MB;
begin
  if Speed < KB then
    Result := FormatFloat('#,##0 bits/s', Speed)
  else if Speed < MB then
    Result := FormatFloat('#,##0.0 kB/s', Speed / KB)
  else if Speed < GB then
    Result := FormatFloat('#,##0.0 MB/s', Speed / MB)
  else
    Result := FormatFloat('#,##0.0 GB/s', Speed / GB);
end;


function GetDirSize(const ADirName: String; var AFileCnt, ADirCnt: Integer): Int64;
var
  DirSize: Int64;

  procedure GetSize(const ADirName: String);
  var
    SR: TSearchRec;
    DirName: String;
  begin
    DirName := AppendPathDelim(ADirName);
    if FindFirst(DirName + '*.*', faAnyFile - faDirectory, SR) = 0 then
    begin
      try
        repeat
          Inc(AFileCnt);
          DirSize:= DirSize + SR.Size;
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
    end;
    if FindFirst(DirName + '*.*', faAnyFile, SR) = 0 then
    begin
      try
        repeat
          if ((SR.Attr and faDirectory) <> 0)  and (SR.Name <> '.') and (SR.Name <> '..') then
           begin
             Inc(ADirCnt);
             GetSize(DirName + SR.Name);
           end;
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
    end;
  end;
begin
  DirSize := 0;
  AFileCnt := 0;
  ADirCnt := 0;
  GetSize(ADirName);
  Result := DirSize;
end;

end.

