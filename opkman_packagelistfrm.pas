unit opkman_packagelistfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls;

type

  { TPackageListFrm }

  TPackageListFrm = class(TForm)
    bYes: TButton;
    bNo: TButton;
    lbMessage: TLabel;
    lwPackages: TListView;
    pnUpDown: TPanel;
    pnMessage: TPanel;
    pnButtons: TPanel;
  private
    procedure SetupControls(const AType: Integer);
  public
    procedure PopulateList(const AType: Integer; const AExtra: String = '');
  end;

var
  PackageListFrm: TPackageListFrm;

implementation
uses opkman_const, opkman_serializablepackages;

{$R *.lfm}

{ TPackageListFrm }

procedure TPackageListFrm.SetupControls(const AType: Integer);
begin
  case AType of
    0: Caption := rsPackageListFrmCaption0;
    1: Caption := rsPackageListFrmCaption1;
  end;
  bYes.Caption := rsPackageListFrmbYes;
  bNo.Caption := rsPackageListFrmbNo;
  bYes.Top := (pnButtons.Height - bYes.Height) div 2;
  bNo.Top := (pnButtons.Height - bNo.Height) div 2;
end;

procedure TPackageListFrm.PopulateList(const AType: Integer; const AExtra: String);
var
  I: Integer;
  CanAddToList: Boolean;
begin
  SetupControls(AType);
  for I := 0 to SerializablePackages.Count - 1 do
  begin
    case AType of
      0: CanAddToList := (SerializablePackages.Items[I].Checked) and (psInstalled in SerializablePackages.Items[I].PackageStates);
      1: CanAddToList := (SerializablePackages.Items[I].Checked) and (FileExists(AExtra + SerializablePackages.Items[I].RepositoryFileName));
    end;
    if CanAddToList then
    begin
      with lwPackages.Items.Add do
      begin
        Caption := SerializablePackages.Items[I].PackageName;
        ImageIndex := 0;
        StateIndex := 0;
      end;
    end;
  end;
end;

end.

