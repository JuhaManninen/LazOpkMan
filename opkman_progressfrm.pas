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
}
unit opkman_progressfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls;

type

  { TProgressFrm }

  TProgressFrm = class(TForm)
    bCancel: TButton;
    cbExtractOpen: TCheckBox;
    imInstall: TImageList;
    lbEllapsed: TLabel;
    lbEllapsedData: TLabel;
    lbPackage: TLabel;
    lbPackageData: TLabel;
    lbReceived: TLabel;
    lbReceivedTotal: TLabel;
    lbRemaining: TLabel;
    lbRemainingData: TLabel;
    lbSpeed: TLabel;
    lbSpeedData: TLabel;
    lwInstall: TListView;
    pb: TProgressBar;
    pbTotal: TProgressBar;
    pnLabels: TPanel;
    pnButtons: TPanel;
    procedure bCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCanClose: Boolean;
    FSuccess: Boolean;
    FMdlRes: TModalResult;
    FType: Integer;
    FCnt, FTotCnt: Integer;
  public
    procedure DoOnPackageDownloadProgress(Sender: TObject; {%H-}AFrom, ATo: String; ACnt, ATotCnt: Integer;
      ACurPos, ACurSize, ATotPos, ATotSize: Int64; AEllapsed, ARemaining, ASpeed: LongInt);
    procedure DoOnPackageDownloadError(Sender: TObject; APackageName: String; const AErrMsg: String = '');
    procedure DoOnPackageDownloadCompleted(Sender: TObject);
    procedure DoOnZipProgress(Sender: TObject; AZipfile: String; ACnt, ATotCnt: Integer;
      ACurPos, ACurSize, ATotPos, ATotSize: Int64; AEllapsed, ARemaining, ASpeed: LongInt);
    procedure DoOnZipError(Sender: TObject; APackageName: String; const AErrMsg: String);
    procedure DoOnZipCompleted(Sender: TObject);
    procedure DoOnPackageInstallProgress(Sender: TObject; ACnt, ATotCnt, AMsgTyp: Integer; AMsg: String);
    procedure DoOnPackageInstallError(Sender: TObject; APackageName, AMsg: String);
    procedure DoOnPackageInstallCompleted(Sender: TObject);
    procedure SetupControls(const AType: Integer);
  end;

var
  ProgressFrm: TProgressFrm;

implementation
uses opkman_common, opkman_const, opkman_downloader, opkman_zipper, opkman_installer;
{$R *.lfm}

{ TProgressFrm }

procedure TProgressFrm.FormShow(Sender: TObject);
begin
  FCanClose := False;
  FCnt := 0;
  FTotCnt := 0;
  FMdlRes := mrNone;
end;

procedure TProgressFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if not FCanClose then
    CloseAction := caNone;
  if FSuccess then
    ModalResult := mrOk
  else
    ModalResult := mrCancel;
end;

procedure TProgressFrm.FormResize(Sender: TObject);
begin
  bCancel.Left := (Self.Width - bCancel.Width) div 2;
end;

procedure TProgressFrm.DoOnPackageDownloadProgress(Sender: TObject; AFrom, ATo: String;
  ACnt, ATotCnt: Integer; ACurPos, ACurSize, ATotPos, ATotSize: Int64;
  AEllapsed, ARemaining, ASpeed: LongInt);
begin
  Caption := rsProgressfrmCaption0 + '(' + IntToStr(ACnt) + '/' + IntToStr(ATotCnt) +')' + rsProgressFrmCaption3;
  lbPackageData.Caption := ExtractFileName(ATo);
  lbSpeedData.Caption := FormatSpeed(ASpeed);
  lbSpeedData.Update;
  lbEllapsedData.Caption := SecToHourAndMin(AEllapsed);
  lbEllapsedData.Update;
  lbRemainingData.Caption := SecToHourAndMin(ARemaining);
  lbRemainingData.Update;
  lbReceived.Caption := rsProgressFrmlbReceivedCaption0 + '  ' + FormatSize(ACurPos) + ' / ' + FormatSize(ACurSize);
  lbReceived.Update;
  pb.Position := Round((ACurPos/ACurSize) * 100);
  pb.Update;
  lbReceivedTotal.Caption := rsProgressFrmlbReceivedTotalCaption0 + '  ' + FormatSize(ATotPos) + ' / ' + FormatSize(ATotSize);
  lbReceivedTotal.Update;
  pbTotal.Position := Round((ATotPos/ATotSize) * 100);
  pbTotal.Update;
  FCnt := ACnt;
  FTotCnt := ATotCnt;
end;

procedure TProgressFrm.DoOnPackageDownloadError(Sender: TObject; APackageName: String;
  const AErrMsg: String);
var
  Msg: String;
begin
  if ((FMdlRes = mrNone) or (FMdlRes = mrYes) or (FMdlRes = mrNo)) then
  begin
    if (FCnt < FTotCnt) then
    begin
      Msg := rsProgressFrmError0 + ' "' + APackageName + '". ' + rsProgressFrmError1 + sLineBreak  + '"' +
             AErrMsg + '"' + sLineBreak + rsProgressFrmConfirm0;
      FMdlRes := MessageDlgEx(Msg, mtConfirmation, [mbYes, mbYesToAll, mbNo], Self);
    end
    else
    begin
      Msg :=  rsProgressFrmError0 + '"' + APackageName + '".' + rsProgressFrmError1 + sLineBreak  + '"' + AErrMsg + '"';
      MessageDlgEx(Msg, mtConfirmation, [mbOk], Self);
      FMdlRes := mrNo;
    end;
  end;
  if FMdlRes = mrNo then
  begin
    FCanClose := True;
    FSuccess := False;
    PackageDownloader.OnPackageDownloadProgress := nil;
    PackageDownloader.OnPackageDownloadError := nil;
    PackageDownloader.CancelDownloadPackages;
    Close;
  end;
end;

procedure TProgressFrm.DoOnPackageDownloadCompleted(Sender: TObject);
begin
  FCanClose := True;
  FSuccess := True;
  Close;
end;

procedure TProgressFrm.DoOnZipProgress(Sender: TObject; AZipfile: String;
  ACnt, ATotCnt: Integer;  ACurPos, ACurSize, ATotPos, ATotSize: Int64;
  AEllapsed, ARemaining, ASpeed: LongInt);
begin
  Caption := rsProgressfrmCaption1 + '(' + IntToStr(ACnt) + '/' + IntToStr(ATotCnt) +')' + rsProgressFrmCaption3;
  lbPackageData.Caption := AZipFile;
  lbSpeedData.Caption := FormatSpeed(ASpeed);
  lbSpeedData.Update;
  lbEllapsedData.Caption := SecToHourAndMin(AEllapsed);
  lbEllapsedData.Update;
  lbRemainingData.Caption := SecToHourAndMin(ARemaining);
  lbRemainingData.Update;
  lbReceived.Caption := rsProgressFrmlbReceivedCaption1 + '  ' + FormatSize(ACurPos) + ' / ' + FormatSize(ACurSize);
  lbReceived.Update;
  pb.Position := Round((ACurPos/ACurSize) * 100);
  pb.Update;
  lbReceivedTotal.Caption := rsProgressFrmlbReceivedTotalCaption1 + '  ' + FormatSize(ATotPos) + ' / ' + FormatSize(ATotSize);
  lbReceivedTotal.Update;
  pbTotal.Position := Round((ATotPos/ATotSize) * 100);
  pbTotal.Update;
  FCnt := ACnt;
  FTotCnt := ATotCnt;
end;

procedure TProgressFrm.DoOnZipError(Sender: TObject; APackageName: String; const AErrMsg: String);
var
  Msg: String;
begin
  if ((FMdlRes = mrNone) or (FMdlRes = mrYes) or (FMdlRes = mrNo)) then
  begin
    if (FCnt < FTotCnt) then
    begin
      Msg := rsProgressFrmError2 + '"' + APackageName + '".' + rsProgressFrmError1 + sLineBreak + '"' +
             AErrMsg + '"' + sLineBreak + rsProgressFrmConfirm0;
      FMdlRes := MessageDlgEx(Msg, mtConfirmation, [mbYes, mbYesToAll, mbNo],  Self);
    end
    else
    begin
      Msg := rsProgressFrmError2 + '"' + APackageName + '".' + rsProgressFrmError1 + sLineBreak + '"' + AErrMsg + '"';
      MessageDlgEx(Msg, mtConfirmation, [mbOk], Self);
      FMdlRes := mrNo;
    end;
  end;
  if FMdlRes = mrNo then
  begin
    FCanClose := True;
    FSuccess := False;
    PackageUnzipper.OnZipProgress := nil;
    PackageUnzipper.OnZipError := nil;
    PackageUnzipper.StopUnZip;
    Close;
  end;
end;

procedure TProgressFrm.DoOnZipCompleted(Sender: TObject);
begin
  FCanClose := True;
  FSuccess := True;
  Close;
end;

procedure TProgressFrm.DoOnPackageInstallProgress(Sender: TObject; ACnt, ATotCnt,
  AMsgTyp: Integer; AMsg: String);
begin
  FCnt := ACnt;
  FTotCnt := ATotCnt;
  Caption := rsProgressFrmCaption2 + '(' + IntToStr(ACnt) + '/' + IntToStr(ATotCnt) +')' + rsProgressFrmCaption3;
  with lwInstall.Items.Add do
   begin
     Caption := AMsg;
     ImageIndex := AMsgTyp;
     StateIndex := AMsgTyp;
   end;
  lwInstall.Items[lwInstall.Items.Count - 1].MakeVisible(False);
end;

procedure TProgressFrm.DoOnPackageInstallError(Sender: TObject; APackageName, AMsg: String);
var
  Msg: String;
begin
  with lwInstall.Items.Add do
  begin
    Caption := AMsg;
    ImageIndex := 2;
    StateIndex := 2;
  end;
  lwInstall.Items[lwInstall.Items.Count - 1].MakeVisible(False);

  if ((FMdlRes = mrNone) or (FMdlRes = mrYes) or (FMdlRes = mrNo)) then
  begin
    if (FCnt < FTotCnt) then
    begin
      Msg := rsProgressFrmError3 + '"' + APackageName + '".' + rsProgressFrmError1 + sLineBreak + '"' +
             AMsg + '"' + sLineBreak + rsProgressFrmConfirm0;
      FMdlRes := MessageDlgEx(Msg, mtConfirmation, [mbYes, mbYesToAll, mbNo],  Self);
    end
    else
    begin
      Msg := rsProgressFrmError3 + '"' + APackageName + '".' + rsProgressFrmError1 + sLineBreak + '"' + AMsg + '"';
      MessageDlgEx(Msg, mtConfirmation, [mbOk], Self);
      FMdlRes := mrNo;
    end;
  end;

  if FMdlRes = mrNo then
  begin
    FCanClose := True;
    FSuccess := False;
    PackageInstaller.OnPackageInstallProgress := nil;
    PackageInstaller.OnPackageInstallError := nil;
    PackageInstaller.NeedToBreak := True;
    Close;
  end;
end;

procedure TProgressFrm.DoOnPackageInstallCompleted(Sender: TObject);
begin
  FCanClose := True;
  FSuccess := True;
  Close;
  Sleep(3000);
end;

procedure TProgressFrm.bCancelClick(Sender: TObject);
begin
  FCanClose := True;
  case FType of
    0: begin
         FSuccess := False;
         PackageDownloader.OnPackageDownloadProgress := nil;
         PackageDownloader.OnPackageDownloadError := nil;
         PackageDownloader.CancelDownloadPackages;
       end;
    1: begin
         FSuccess := False;
         PackageUnzipper.OnZipProgress := nil;
         PackageUnzipper.OnZipError := nil;
         PackageUnzipper.StopUnZip;
       end;
    2: begin
         FSuccess := False;
         PackageInstaller.OnPackageInstallProgress := nil;
         PackageInstaller.OnPackageInstallError := nil;
         PackageInstaller.StopInstall;
       end;
   end;
  Close;
end;

procedure TProgressFrm.SetupControls(const AType: Integer);
begin
  FType := AType;
  case AType of
    0: begin  //download
         Caption := rsProgressfrmCaption0 + rsProgressFrmCaption3;
         pnLabels.Visible := True;
         lwInstall.Visible := False;
         lbReceived.Caption := rsProgressFrmlbReceivedCaption0;
         lbReceivedTotal.Caption := rsProgressFrmlbReceivedTotalCaption0;
         cbExtractOpen.Caption := rsProgressFrmcbExtractOpenCaption0;
         cbExtractOpen.Visible := (PackageAction <> paInstall);
       end;
    1: begin //extract
         Caption := rsProgressfrmCaption1 + rsProgressFrmCaption3;
         pnLabels.Visible := True;
         lwInstall.Visible := False;
         lbReceived.Caption := rsProgressFrmlbReceivedCaption1;
         lbReceivedTotal.Caption := rsProgressFrmlbReceivedTotalCaption1;
         cbExtractOpen.Caption := rsProgressFrmcbExtractOpenCaption1;
         cbExtractOpen.Checked := (PackageAction <> paInstall);
         cbExtractOpen.Visible := (PackageAction <> paInstall);
       end;
    2: begin //install
         Caption := rsProgressfrmCaption2 + rsProgressFrmCaption3;
         pnLabels.Visible := False;
         lwInstall.Visible := True;
         cbExtractOpen.Visible := False
       end;
  end;
  lbPackage.Caption := rsProgressFrmlbPackageCaption;
  lbSpeed.Caption := rsProgressFrmlbSpeedCaption;
  lbSpeedData.Caption := rsProgressFrmlbSpeedCalcCaption;
  lbEllapsed.Caption := rsProgressFrmlbEllapsedCaption;
  lbRemaining.Caption := rsProgressFrmlbRemainingCaption;
  pb.Top := lbReceived.Top + lbReceived.Height + 1;
  pbTotal.Top := lbReceivedTotal.Top + lbReceivedTotal.Height + 1;
  bCancel.Top := (pnButtons.Height - bCancel.Height) div 2;
  cbExtractOpen.Top := bCancel.Top + (bCancel.Height - cbExtractOpen.Height) div 2;
end;

end.

