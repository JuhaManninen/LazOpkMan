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
  Implementation of the package installer class.
}
unit opkman_installer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, Controls, PackageIntf;

type
  TInstallStatus = (isSuccess, isPartiallyFailed, isFailed);

  { TPackageInstaller }
  TOnPackageInstallProgress = procedure(Sender: TObject; ACnt, ATotCnt, AMsgTyp: Integer; AMsg: String) of object;
  TOnPackageInstallError = procedure(Sender: TObject; APackageName, AMsg: String) of object;
  TOnPackageInstallCompleted = TNotifyEvent;
  TPackageInstaller = class(TThread)
  private
    FPackageList: TObjectList;
    FNeedToBreak: Boolean;
    FNeedToRebuild: Boolean;
    FCnt: Integer;
    FTotCnt: Integer;
    FPackageName: String;
    FMsgTyp: Integer;
    FMsg: String;
    FStarted: Boolean;
    FInstallStatus: TInstallStatus;
    FOnPackageInstallProgress: TOnPackageInstallProgress;
    FOnPackageInstallError: TOnPackageInstallError;
    FOnPackageInstallCompleted: TOnPackageInstallCompleted;
    function AddPackageToIDE(const APackageFileName: String): TIDEPackage;
    procedure DoOnPackageInstallProgress;
    procedure DoOnPackageInstallError;
    procedure DoOnPackageInstallCompleted;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartInstall;
    procedure StopInstall;
  published
    property NeedToBreak: Boolean read FNeedToBreak write FNeedToBreak;
    property NeedToRebuild: Boolean read FNeedToRebuild write FNeedToRebuild;
    property InstallStatus: TInstallStatus read FInstallStatus write FInstallStatus;
    property OnPackageInstallProgress: TOnPackageInstallProgress read FOnPackageInstallProgress write FOnPackageInstallProgress;
    property OnPackageInstallError: TOnPackageInstallError read FOnPackageInstallError write FOnPackageInstallError;
    property OnPackageInstallCompleted: TOnPackageInstallCompleted read FOnPackageInstallCompleted write FOnPackageInstallCompleted;
  end;

var
  PackageInstaller: TPackageInstaller = nil;

implementation
uses opkman_serializablepackages, opkman_const;
{ TPackageInstaller }

constructor TPackageInstaller.Create;
begin
  inherited Create(True);
  FPackageList := TObjectList.Create(False);
end;

destructor TPackageInstaller.Destroy;
begin
  FPackageList.Free;
  inherited Destroy;
end;

function TPackageInstaller.AddPackageToIDE(const APackageFileName: String): TIDEPackage;
var
  I: Integer;
begin
  Result := nil;
  if PackageEditingInterface.DoOpenPackageFile(APackageFileName, [pofRevert, pofDoNotOpenEditor], True) = mrOk then
  begin
    for I := 0 to PackageEditingInterface.GetPackageCount - 1 do
    begin
      if UpperCase(PackageEditingInterface.GetPackages(I).Filename) = UpperCase(APackageFileName) then
      begin
        Result := PackageEditingInterface.GetPackages(I);
        Break;
      end;
    end;
  end;
end;

procedure TPackageInstaller.DoOnPackageInstallProgress;
begin
  if Assigned(FOnPackageInstallProgress) then
    FOnPackageInstallProgress(Self, FCnt, FTotCnt, FMsgTyp, FMsg);
end;

procedure TPackageInstaller.DoOnPackageInstallError;
begin
  if Assigned(FOnPackageInstallError) then
    FOnPackageInstallError(Self, FPackageName, FMsg);
end;

procedure TPackageInstaller.DoOnPackageInstallCompleted;
begin
  if Assigned(FOnPackageInstallCompleted) then
    FOnPackageInstallCompleted(Self);
end;

procedure TPackageInstaller.Execute;
var
  I: Integer;
  IDEPackage: TIDEPackage;
  PkgInstallInIDEFlags: TPkgInstallInIDEFlags;
  ErrCnt: Integer;
begin
  ErrCnt := 0;
  FCnt := 0;
  InstallStatus := isFailed;
  for I := 0 to SerializablePackages.Count - 1 do
  begin
    if SerializablePackages.Items[I].IsInstallable then
    begin
      if NeedToBreak then
        Break;
      Inc(FCnt);
      FMsgTyp := 0;
      FPackageName := SerializablePackages.Items[I].PackageFileName;
      FMsg := rsProgrssFrmInfo0 + ' "' + SerializablePackages.Items[I].PackageFileName + '".';
      Synchronize(@DoOnPackageInstallProgress);
      if FileExists(SerializablePackages.Items[I].PackageAbsolutePath) then
      begin
        NeedToRebuild := SerializablePackages.Items[I].PackageType in [ptRunAndDesignTime, ptDesigntime];
        IDEPackage := AddPackageToIDE(SerializablePackages.Items[I].PackageAbsolutePath);
        Randomize;
        Sleep(2000);
        if IDEPackage <> nil then
        begin
          FPackageList.Add(IDEPackage);
          FMsgTyp := 1;
          FMsg := rsProgrssFrmInfo1;
          SerializablePackages.Items[I].PackageStates := SerializablePackages.Items[I].PackageStates + [psInstalled];
          Synchronize(@DoOnPackageInstallProgress);
        end
        else
        begin
          Inc(ErrCnt);
          FMsg := rsProgressFrmError3 + ' "' + SerializablePackages.Items[I].PackageFileName + '".';
          SerializablePackages.Items[I].PackageStates := SerializablePackages.Items[I].PackageStates + [psError];
          Synchronize(@DoOnPackageInstallError);
        end;
      end;
    end;
  end;
  if FPackageList.Count > 0 then
  begin
    PkgInstallInIDEFlags := [piiifQuiet];
    if PackageEditingInterface.InstallPackages(FPackageList, PkgInstallInIDEFlags) = mrOk then
    begin
      if ErrCnt = 0 then
        InstallStatus := isSuccess
      else
        InstallStatus := isPartiallyFailed;
    end;
  end;
  Synchronize(@DoOnPackageInstallCompleted);
end;

procedure TPackageInstaller.StartInstall;
var
  I: Integer;
begin
  if FStarted then
    Exit;
  FStarted := True;
  FTotCnt := 0;
  for I := 0 to SerializablePackages.Count - 1 do
    if SerializablePackages.Items[I].IsInstallable then
      Inc(FTotCnt);
  Start;
end;

procedure TPackageInstaller.StopInstall;
begin
  FNeedToBreak := True;
  FStarted := False;
end;

end.

