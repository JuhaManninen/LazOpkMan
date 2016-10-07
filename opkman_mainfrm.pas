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
   Implementation of the main dialog.
}
unit opkman_mainfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Menus, ComCtrls, IDECommands, LazFileUtils,
  LCLIntf, fpjson, opkman_downloader, opkman_installer;

type

  { TMainFrm }

  TMainFrm = class(TForm)
    bInstall: TToolButton;
    bOptions: TToolButton;
    bGetStates: TButton;
    cbAll: TCheckBox;
    cbFilterBy: TComboBox;
    cbPackageType: TComboBox;
    cbPackageState: TComboBox;
    edFilter: TEdit;
    imTree: TImageList;
    MenuItem1: TMenuItem;
    miCleanup: TMenuItem;
    miDownloadTo: TMenuItem;
    miInstall: TMenuItem;
    miJSONShow: TMenuItem;
    miJSONHide: TMenuItem;
    mJSON: TMemo;
    pmMemo: TPopupMenu;
    pnFilter: TPanel;
    pnMessage: TPanel;
    pnToolBar: TPanel;
    pnToolBarOptions: TPanel;
    pnToolBarInstall: TPanel;
    pnTop: TPanel;
    pnMain: TPanel;
    pmTree: TPopupMenu;
    pmToolbar: TPopupMenu;
    SDD: TSelectDirectoryDialog;
    spCollapse: TSpeedButton;
    spClear: TSpeedButton;
    spExpand: TSpeedButton;
    spRefresh: TSpeedButton;
    tbButtons: TToolBar;
    tbButtons1: TToolBar;
    procedure bInstallClick(Sender: TObject);
    procedure bOptionsClick(Sender: TObject);
    procedure cbAllClick(Sender: TObject);
    procedure cbFilterByChange(Sender: TObject);
    procedure cbPackageStateChange(Sender: TObject);
    procedure cbPackageTypeChange(Sender: TObject);
    procedure edFilterChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miCleanupClick(Sender: TObject);
    procedure miDownloadToClick(Sender: TObject);
    procedure miInstallClick(Sender: TObject);
    procedure miJSONShowClick(Sender: TObject);
    procedure pnMainResize(Sender: TObject);
    procedure pnTopResize(Sender: TObject);
    procedure spClearClick(Sender: TObject);
    procedure spExpandClick(Sender: TObject);
  private
    procedure EnableDisableControls(const AEnable: Boolean);
    procedure SetupMessage(const AMessage: String = '');
    procedure SetupControls;
    procedure GetPackageList;
    procedure ShowOptions;
    procedure DoChecking(Sender: TObject);
    procedure DoOnJSONProgress(Sender: TObject);
    procedure DoOnJSONDownloadCompleted(Sender: TObject; AJSON: TJSONStringType; AErrTyp: TErrorType; const AErrMsg: String = '');
    function IsSomethingChecked: Boolean;
    function Download(const ADstDir: String; var ADoExtract: Boolean): TModalResult;
    function Extract(const ASrcDir, ADstDir: String; var ADoOpen: Boolean): TModalResult;
    function Install(var AInstallStatus: TInstallStatus; var ANeedToRebuild: Boolean): TModalResult;
  public
  end;

var
  MainFrm: TMainFrm;

implementation

uses opkman_serializablepackages, opkman_visualtree, opkman_const, opkman_common,
  opkman_progressfrm, opkman_zipper, opkman_packagelistfrm, opkman_options,
  opkman_optionsfrm;
{$R *.lfm}

{ TMainFrm }

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  InitLocalRepository;
  PackageOptions := TPackageOptions.Create(LocalRepositoryConfigFile);
  VisualTree := TVisualTree.Create(pnMain, imTree, pmTree);
  VisualTree.OnChecking := @DoChecking;
  SerializablePackages := TSerializablePackages.Create;
  PackageDownloader := TPackageDownloader.Create(PackageOptions.RemoteRepository);
  PackageDownloader.OnJSONProgress := @DoOnJSONProgress;
  PackageDownloader.OnJSONDownloadCompleted := @DoOnJSONDownloadCompleted;
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  PackageDownloader.Free;
  SerializablePackages.Free;
  VisualTree.Free;
  PackageOptions.Free;
end;

procedure TMainFrm.FormShow(Sender: TObject);
begin
  SetupControls;
  GetPackageList;
end;

procedure TMainFrm.GetPackageList;
begin
  VisualTree.VST.Clear;
  if SerializablePackages.Count > 0 then
    SerializablePackages.Clear;
  EnableDisableControls(False);
  SetupMessage(rsMessageDownload);
  PackageDownloader.DownloadJSON(10000);
  VisualTree.UpdatePackageStates;
end;

function TMainFrm.IsSomethingChecked: Boolean;
begin
  Result := VisualTree.VST.CheckedCount > 0;
  if Result then
  begin
    VisualTree.GetPackageList;
    VisualTree.UpdatePackageStates;
  end
  else
    MessageDlgEx(rsNoPackageToDownload, mtInformation, [mbOk], Self)
end;

function TMainFrm.Download(const ADstDir: String; var ADoExtract: Boolean): TModalResult;
begin
  ADoExtract := False;
  ProgressFrm := TProgressFrm.Create(MainFrm);
  try
    ProgressFrm.SetupControls(0);
    PackageDownloader.OnPackageDownloadProgress := @ProgressFrm.DoOnPackageDownloadProgress;
    PackageDownloader.OnPackageDownloadError := @ProgressFrm.DoOnPackageDownloadError;
    PackageDownloader.OnPackageDownloadCompleted := @ProgressFrm.DoOnPackageDownloadCompleted;
    PackageDownloader.DownloadPackages(ADstDir);
    Result := ProgressFrm.ShowModal;
    if Result = mrOK then
      ADoExtract := ProgressFrm.cbExtractOpen.Checked;
  finally
    ProgressFrm.Free;
  end;
end;

function TMainFrm.Extract(const ASrcDir, ADstDir: String; var ADoOpen: Boolean): TModalResult;
begin
  ProgressFrm := TProgressFrm.Create(MainFrm);
  try
    PackageUnzipper := TPackageUnzipper.Create;
    try
      ProgressFrm.SetupControls(1);
      PackageUnzipper.OnZipProgress := @ProgressFrm.DoOnZipProgress;
      PackageUnzipper.OnZipError := @ProgressFrm.DoOnZipError;
      PackageUnzipper.OnZipCompleted := @ProgressFrm.DoOnZipCompleted;
      PackageUnzipper.StartUnZip(ASrcDir, ADstDir);
      Result := ProgressFrm.ShowModal;
      if Result = mrOk then
        ADoOpen := ProgressFrm.cbExtractOpen.Checked;
    finally
      if Assigned(PackageUnzipper) then
        PackageUnzipper := nil;
    end;
 finally
   ProgressFrm.Free;
 end;
end;

function TMainFrm.Install(var AInstallStatus: TInstallStatus;
  var ANeedToRebuild: Boolean): TModalResult;
begin
  ProgressFrm := TProgressFrm.Create(MainFrm);
  try
    PackageInstaller := TPackageInstaller.Create;
    try
      ProgressFrm.SetupControls(2);
      PackageInstaller.OnPackageInstallProgress := @ProgressFrm.DoOnPackageInstallProgress;
      PackageInstaller.OnPackageInstallError := @ProgressFrm.DoOnPackageInstallError;
      PackageInstaller.OnPackageInstallCompleted := @ProgressFrm.DoOnPackageInstallCompleted;
      PackageInstaller.StartInstall;
      Result := ProgressFrm.ShowModal;
      if Result = mrOk then
      begin
        AInstallStatus := PackageInstaller.InstallStatus;
        ANeedToRebuild := PackageInstaller.NeedToRebuild;
      end;
    finally
      if Assigned(PackageInstaller) then
        PackageInstaller := nil;
    end;
  finally
    ProgressFrm.Free;
  end;
end;

procedure TMainFrm.miInstallClick(Sender: TObject);
var
  CanGo: Boolean;
  DoExtract: Boolean;
  DoOpen: Boolean;
  InstallStatus: TInstallStatus;
  NeedToRebuild: Boolean;
begin
  if not IsSomethingChecked then
    Exit;

  CanGo := True;
  VisualTree.UpdatePackageStates;
  PackageListFrm := TPackageListFrm.Create(MainFrm);
  try
    PackageListFrm.lbMessage.Caption := rsMainFrmPackageAlreadyInstalled;
    PackageListFrm.PopulateList(0);
    if PackageListFrm.lwPackages.Items.Count > 0 then
      CanGo := PackageListFrm.ShowModal = mrYes
    else
      CanGo := True;
  finally
    PackageListFrm.Free;
  end;

  if CanGo then
  begin
    PackageAction := paInstall;
    VisualTree.UpdatePackageStates;
    if SerializablePackages.DownloadCount > 0 then
    begin
      DoExtract := True;
      CanGo := Download(LocalRepositoryArchive, DoExtract) = mrOK;
      VisualTree.UpdatePackageStates;
    end;

    if CanGo then
    begin
      if SerializablePackages.ExtractCount > 0 then
      begin
        DoOpen := False;
        CanGo := Extract(LocalRepositoryArchive, LocalRepositoryPackages, DoOpen) = mrOk;
        VisualTree.UpdatePackageStates;
      end;

      if CanGo then
      begin
        VisualTree.UpdatePackageStates;
        if SerializablePackages.InstallCount > 0 then
        begin
          InstallStatus := isFailed;
          NeedToRebuild := False;
          if Install(InstallStatus, NeedToRebuild) = mrOk then
            if (InstallStatus = isSuccess) or (InstallStatus = isPartiallyFailed) then
            begin
              SerializablePackages.MarkRuntimePackages;
              if NeedToRebuild then
                IDECommands.ExecuteIDECommand(Self, ecBuildLazarus);
            end;
        end;
      end;
    end;
  end;
  SerializablePackages.RemoveErrorState;
end;

procedure TMainFrm.miCleanupClick(Sender: TObject);
var
  Cnt: Integer;
begin
  if MessageDlgEx(rsRepositoryCleanup0, mtInformation, [mbYes, mbNo], Self) = mrYes then
  begin
    Cnt := SerializablePackages.Cleanup;
    MessageDlgEx(IntToStr(Cnt) + ' ' + rsRepositoryCleanup1, mtInformation, [mbOk], Self);
  end;
end;

procedure TMainFrm.miDownloadToClick(Sender: TObject);
var
  DstDir: String;
  CanGo: Boolean;
  DoExtract: Boolean;
  DoOpen: Boolean;
begin
  if not IsSomethingChecked then
    Exit;

  if SDD.Execute then
  begin
    CanGo := True;
    DstDir := AppendPathDelim(SDD.FileName);
    CanGo := True;
    VisualTree.UpdatePackageStates;
    PackageListFrm := TPackageListFrm.Create(MainFrm);
    try
      PackageListFrm.lbMessage.Caption := rsMainFrmPackageAlreadyDownloaded;
      PackageListFrm.PopulateList(1, DstDir);
      if PackageListFrm.lwPackages.Items.Count > 0 then
        CanGo := PackageListFrm.ShowModal = mrYes
      else
        CanGo := True;
    finally
      PackageListFrm.Free;
    end;
  end
  else
    CanGo := False;

  if CanGo then
  begin
    PackageAction := paDownloadTo;
    DoExtract := False;
    if Download(DstDir, DoExtract) = mrOK then
    begin
      if SerializablePackages.ExtractCount > 0 then
      begin
        if DoExtract then
        begin
          DoOpen := False;
          if Extract(DstDir, DstDir, DoOpen) = mrOk then
          begin
            if DoOpen then
              OpenDocument(DstDir);
          end;
        end;
      end;
    end;
  end;
  SerializablePackages.RemoveErrorState;
end;

procedure TMainFrm.DoOnJSONDownloadCompleted(Sender: TObject; AJSON: TJSONStringType; AErrTyp: TErrorType; const AErrMsg: String = '');
begin
  case AErrTyp of
    etNone:
      begin
        if (not SerializablePackages.JSONToPackages(AJSON)) or (SerializablePackages.Count = 0) then
        begin
          SetupMessage(rsMessageNoPackage);
          MessageDlgEx(rsMessageError1, mtInformation, [mbOk], Self);
          Exit;
        end;
        EnableDisableControls(True);
        SetupMessage;
        mJSON.Text := AJSON;
        cbAll.Checked := False;
        VisualTree.PopulateTree;
        Caption := rsLazarusPackageManager + '(' + IntToStr(SerializablePackages.Count) + ' ' + rsMainFrmCaption + ')';
      end;
    etConfig:
      begin
        EnableDisableControls(True);
        SetupMessage(rsMessageNoPackage);
        Caption := rsLazarusPackageManager;
        if MessageDlgEx('"' + AErrMsg + '"', mtConfirmation, [mbYes, mbNo], Self) = mrYes then
          ShowOptions;
      end;
    etTimeOut, etHTTPClient:
      begin
        EnableDisableControls(True);
        SetupMessage(rsMessageNoPackage);
        Caption := rsLazarusPackageManager;
        MessageDlgEx(rsMessageError0 + sLineBreak + '"' + AErrMsg + '"', mtInformation, [mbOk], Self);
      end;
  end;
end;

procedure TMainFrm.ShowOptions;
begin
  OptionsFrm := TOptionsFrm.Create(MainFrm);
  try
    OptionsFrm.Caption := rsOptionsFrmCaption;
    OptionsFrm.edRemoteRepository.Text := PackageOptions.RemoteRepository;
    if OptionsFrm.ShowModal = mrOk then
    begin
      spRefresh.Enabled := PackageOptions.RemoteRepository <> '';
      GetPackageList;
    end;
  finally
    OptionsFrm.Free;
  end;
end;

procedure TMainFrm.EnableDisableControls(const AEnable: Boolean);
begin
  cbAll.Enabled := (AEnable) and (SerializablePackages.Count > 0);
  cbFilterBy.Enabled := (AEnable) and (SerializablePackages.Count > 0);
  pnFilter.Enabled := (AEnable) and (SerializablePackages.Count > 0);
  cbPackageState.Enabled := (AEnable) and (SerializablePackages.Count > 0);
  cbPackageType.Enabled := (AEnable) and (SerializablePackages.Count > 0);
  spRefresh.Enabled := (AEnable) and (Trim(PackageOptions.RemoteRepository) <> '');
  spExpand.Enabled := (AEnable) and (SerializablePackages.Count > 0);
  spCollapse.Enabled := (AEnable) and (SerializablePackages.Count > 0);
  VisualTree.VST.Enabled := (AEnable) and (SerializablePackages.Count > 0);
  if edFilter.CanFocus then
    edFilter.SetFocus;
  bOptions.Enabled := (AEnable);
  bInstall.Enabled := (AEnable) and (SerializablePackages.Count > 0);
  miInstall.Enabled := (AEnable) and (SerializablePackages.Count > 0);
  miDownloadTo.Enabled := (AEnable) and (SerializablePackages.Count > 0);
end;

procedure TMainFrm.SetupMessage(const AMessage: String = '');
begin
  if AMessage = '' then
  begin
    pnMessage.SendToBack;
    pnMessage.Visible := False;
  end
  else
  begin
    pnMessage.Caption := AMessage;
    pnMessage.Visible := True;
    pnMessage.BringToFront;
    Application.ProcessMessages;
  end;
end;

procedure TMainFrm.DoChecking(Sender: TObject);
begin
  cbAll.OnClick := nil;
  cbAll.Checked := False;
  cbAll.OnClick := @cbAllClick;
end;

procedure TMainFrm.DoOnJSONProgress(Sender: TObject);
begin
  Application.ProcessMessages;
end;

procedure TMainFrm.cbAllClick(Sender: TObject);
begin
  VisualTree.CheckNodes(cbAll.Checked);
end;

procedure TMainFrm.cbFilterByChange(Sender: TObject);
begin
  VisualTree.ResetFilter;
  case cbFilterBy.ItemIndex of
    0, 2..5, 7,8:
      begin
        cbPackageType.Visible := False;
        cbPackageState.Visible := False;
        pnFilter.Visible := True;
        edFilter.Text := '';
        edFilter.SetFocus;
      end;
   1: begin
        pnFilter.Visible := False;
        cbPackageType.Visible := False;
        cbPackageState.Visible := True;
        cbPackageState.ItemIndex := 0;
        cbPackageState.SetFocus;
      end;
   6: begin
        pnFilter.Visible := False;
        cbPackageState.Visible := False;
        cbPackageType.Visible := True;
        cbPackageType.ItemIndex := 0;
        cbPackageType.SetFocus;
      end;
  end;
end;

procedure TMainFrm.cbPackageTypeChange(Sender: TObject);
begin
  if cbPackageType.ItemIndex > 0 then
    VisualTree.FilterTree('PackageType', cbFilterBy.ItemIndex, cbPackageType.ItemIndex - 1)
  else
    VisualTree.ResetFilter;
end;

procedure TMainFrm.cbPackageStateChange(Sender: TObject);
begin
  if cbPackageState.ItemIndex > 0 then
    VisualTree.FilterTree('PackageStatus', cbFilterBy.ItemIndex, cbPackageState.ItemIndex - 1)
  else
    VisualTree.ResetFilter;
end;

procedure TMainFrm.edFilterChange(Sender: TObject);
begin
  if edFilter.Text <> '' then
    VisualTree.FilterTree(edFilter.Text, cbFilterBy.ItemIndex)
  else
    VisualTree.ResetFilter;
end;

procedure TMainFrm.pnTopResize(Sender: TObject);
begin
  cbFilterBy.Left := (pnTop.Width - pnFilter.Width - cbFilterBy.Width - 5) div 2;
  pnFilter.Left := cbFilterBy.Left + cbFilterBy.Width + 5;
  cbPackageType.Left := pnFilter.Left;
  cbPackageState.Left := pnFilter.Left;
end;

procedure TMainFrm.spClearClick(Sender: TObject);
begin
  edFilter.OnChange := nil;
  edFilter.Text := '';
  VisualTree.ResetFilter;
  edFilter.OnChange := @edFilterChange;
end;

procedure TMainFrm.spExpandClick(Sender: TObject);
begin
  case TSpeedButton(Sender).Tag of
    0: begin
         VisualTree.VST.Clear;
         VisualTree.VST.Invalidate;
         GetPackageList;
       end;
    1: VisualTree.ExpandEx;
    2: VisualTree.VST.FullCollapse();
  end;
end;

procedure TMainFrm.bOptionsClick(Sender: TObject);
begin
  ShowOptions;
end;

procedure TMainFrm.bInstallClick(Sender: TObject);
begin
  miInstallClick(miInstall);
end;

procedure TMainFrm.SetupControls;
begin
  Caption := rsMainFrmCaption;

  pnFilter.Height := cbFilterBy.Height + 1;
  edFilter.Hint := rsMainFrmedFilterHint;
  spClear.Hint := rsMainFrmspClearHint;
  pnTop.Height := cbFilterBy.Top + cbFilterBy.Height + 10;
  cbAll.Top := (pnTop.Height - cbAll.Height) div 2;
  cbAll.Hint := rsMainFrmcbAllHint;
  spRefresh.Top := (pnTop.Height - spRefresh.Height) div 2;
  spRefresh.Hint := rsMainFrmspRefreshHint;
  spExpand.Top:= (pnTop.Height - spExpand.Height + 1) div 2;
  spExpand.Hint := rsMainFrmspExpandHint;
  spCollapse.Top:= (pnTop.Height - spCollapse.Height + 1) div 2;
  spCollapse.Hint := rsMainFrmspCollapseHint;
  cbAll.Caption := rsMainFrmcbAllCaption;

  miJSONShow.Caption := rsMainFrmmiJSONShow;
  miJSONHide.Caption := rsMainFrmmiJSONHide;
  miInstall.Caption := rsMainFrmmiInstall;
  miDownloadTo.Caption := rsMainFrmmiDownloadUnZipp;

  cbFilterBy.Hint := rsMainFrmcbFilterByHint;
  cbFilterBy.Clear;
  cbFilterBy.Items.Add(rsMainFrmVSTHeaderColumnPackageName);
  cbFilterBy.Items.Add(rsMainFrmVSTTextPackageStatus);
  cbFilterBy.Items.Add(rsMainFrmVSTTextVersion);
  cbFilterBy.Items.Add(rsMainFrmVSTTextDescription);
  cbFilterBy.Items.Add(rsMainFrmVSTTextAuthor);
  cbFilterBy.Items.Add(rsMainFrmVSTTextCompatible);
  cbFilterBy.Items.Add(rsMainFrmVSTTextPackagetype);
  cbFilterBy.Items.Add(rsMainFrmVSTTextDependecies);
  cbFilterBy.Items.Add(rsMainFrmVSTTextLicense);
  cbFilterBy.ItemIndex := 0;

  cbPackageType.Clear;
  cbPackageType.Items.Add('');
  cbPackageType.Items.Add(rsMainFrmVSTTextPackageType0);
  cbPackageType.Items.Add(rsMainFrmVSTTextPackageType1);
  cbPackageType.Items.Add(rsMainFrmVSTTextPackageType2);
  cbPackageType.Items.Add(rsMainFrmVSTTextPackageType3);

  cbPackageState.Clear;
  cbPackageState.Items.Add('');
  cbPackageState.Items.Add(rsMainFrmPackageState0);
  cbPackageState.Items.Add(rsMainFrmPackageState1);
  cbPackageState.Items.Add(rsMainFrmPackageState2);
  cbPackageState.Items.Add(rsMainFrmPackageState3);


  bOptions.Caption := rsMainFrmbOptionsCaption;
  bOptions.Hint := rsMainFrmOptionsHint;

  bInstall.Caption := rsMainFrmbInstallCaption;
  bInstall.Hint := rsMainFrmbInstallHint;
end;

procedure TMainFrm.pnMainResize(Sender: TObject);
begin
  pnMessage.Left := (pnMain.Width - pnMessage.Width) div 2;
  pnMessage.Top := (pnMain.Height - pnMessage.Height) div 2;
end;

procedure TMainFrm.miJSONShowClick(Sender: TObject);
begin
  if not mJSON.Visible then
  begin
    EnableDisableControls(False);
    mJSON.Visible := True;
    mJSON.BringToFront;
  end
  else
  begin
    mJSON.SendToBack;
    mJSON.Visible := False;
    EnableDisableControls(True);
  end;
end;

end.






