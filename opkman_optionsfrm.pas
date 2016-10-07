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
   Implementation of the options dialog.
}

unit opkman_optionsfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, opkman_VirtualTrees, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type

  { TOptionsFrm }

  TOptionsFrm = class(TForm)
    bCancel: TButton;
    bOk: TButton;
    bSettings: TButton;
    edRemoteRepository: TEdit;
    Label1: TLabel;
    pnBottom: TPanel;
    procedure bOkClick(Sender: TObject);
    procedure edRemoteRepositoryKeyPress(Sender: TObject; var Key: char);
  private

  public

  end;

var
  OptionsFrm: TOptionsFrm;

implementation
uses opkman_options, opkman_common, opkman_const;
{$R *.lfm}

{ TOptionsFrm }

procedure TOptionsFrm.bOkClick(Sender: TObject);
begin
  if Trim(edRemoteRepository.Text)  = '' then
  begin
    MessageDlgEx(rsOptionedRemoteRepository, mtInformation, [mbOk], Self);
    edRemoteRepository.SetFocus;
    Exit;
  end;
  PackageOptions.RemoteRepository := OptionsFrm.edRemoteRepository.Text;
  PackageOptions.Save;
  ModalResult := mrOk;
end;

procedure TOptionsFrm.edRemoteRepositoryKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    bOkClick(bOk);
end;

end.

