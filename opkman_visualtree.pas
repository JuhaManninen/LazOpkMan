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
   Implementation of the visual tree, which displays the package sructure
   downloaded from the remote repository.
}
unit opkman_visualtree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Menus, dialogs, contnrs, PackageIntf,
  opkman_VirtualTrees, opkman_common, opkman_serializablepackages;


type
  PData = ^TData;
  TData = record
    DataType: Integer;
    PackageState: TPackageState;
    PackageName: String;
    PackageFileName: String;
    Version: String;
    InstalledVersion: String;
    Description: String;
    Author: String;
    LazCompatibility: String;
    PackageType: TPackageType;
    Dependencies: String;
    License: String;
    RepositoryFileName: String;
    RepositoryFileSize: Int64;
    RepositoryFileHash: String;
    RepositoryDate: TDate;
  end;

  { TVisualTree }

  TVisualTree = class
  private
    FVST: TVirtualStringTree;
    FSortCol: Integer;
    FSortDir: opkman_VirtualTrees.TSortDirection;
    FOnChecking: TNotifyEvent;
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; {%H-}Column: TColumnIndex;
      {%H-}CellPaintMode: TVTCellPaintMode; CellRect: TRect; var {%H-}ContentRect: TRect);
    procedure VSTChecking(Sender: TBaseVirtualTree; {%H-}Node: PVirtualNode;
      var NewState: TCheckState; var {%H-}Allowed: Boolean);
    procedure VSTChecked(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      {%H-}Kind: TVTImageKind; Column: TColumnIndex; var {%H-}Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
    procedure VSTHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure VSTPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; {%H-}Column: TColumnIndex;
      {%H-}TextType: TVSTTextType);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  public
    constructor Create(const AParent: TWinControl; const AImgList: TImageList;
      APopupMenu: TPopupMenu);
    destructor Destroy; override;
  public
    procedure PopulateTree;
    procedure CheckNodes(const Checked: Boolean);
    procedure FilterTree(const AText: String; const AFilterBy: Integer; const AExtraParam: Integer = -1);
    procedure ResetFilter;
    procedure ExpandEx;
    procedure GetPackageList;
    procedure UpdatePackageStates;
  published
    property OnChecking: TNotifyEvent read FOnChecking write FOnChecking;
    property VST: TVirtualStringTree read FVST;
  end;

var
  VisualTree: TVisualTree = nil;

implementation
uses opkman_const;

{ TVisualTree }

constructor TVisualTree.Create(const AParent: TWinControl; const AImgList: TImageList;
  APopupMenu: TPopupMenu);
begin
  FVST := TVirtualStringTree.Create(nil);
  with FVST do
   begin
     Parent := AParent;
     Align := alClient;
     Anchors := [akLeft, akTop, akRight];
     Images := AImgList;
     PopupMenu := APopupMenu;
     Color := clBtnFace;
     DefaultNodeHeight := 25;
     Indent := 22;
     TabOrder := 1;
     DefaultText := '';
     Header.AutoSizeIndex := 1;
     Header.Height := 25;
     with Header.Columns.Add do begin
       Position := 0;
       Width := 250;
       Text := rsMainFrmVSTHeaderColumnPackageName;
     end;
     with Header.Columns.Add do begin
       Position := 1;
       Width := 728;
       Text := rsMainFrmVSTHeaderColumnData;
     end;
     Header.Options := [hoAutoResize, hoColumnResize, hoRestrictDrag, hoShowSortGlyphs, hoVisible, hoAutoSpring];
     Header.SortColumn := 0;
     TabOrder := 2;
     TreeOptions.MiscOptions := [toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning];
     TreeOptions.PaintOptions := [toHideFocusRect, toAlwaysHideSelection, toPopupMode, toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages];
     TreeOptions.SelectionOptions := [toFullRowSelect, toRightClickSelect];
     OnBeforeCellPaint := @VSTBeforeCellPaint;
     OnChecking := @VSTChecking;
     OnChecked := @VSTChecked;
     OnCompareNodes := @VSTCompareNodes;
     OnGetText := @VSTGetText;
     OnPaintText := @VSTPaintText;
     OnGetImageIndex := @VSTGetImageIndex;
     OnHeaderClick := @VSTHeaderClick;
     OnFreeNode := @VSTFreeNode;
   end;
end;

destructor TVisualTree.Destroy;
begin
  FVST.Free;
  inherited Destroy;
end;

procedure TVisualTree.PopulateTree;
var
  I: Integer;
  Node, ChildNode, GrandChildNode: PVirtualNode;
  Data, ChildData, GrandChildData: PData;
begin
  FVST.Clear;
  FVST.NodeDataSize := SizeOf(TData);
  for I := 0 to SerializablePackages.Count - 1 do
  begin
     //add package(DataType = 0)
     Node := FVST.AddChild(nil);
     Node^.CheckType := ctTriStateCheckBox;
     //add Package name(DataType = 0)
     Data := FVST.GetNodeData(Node);
     Data^.PackageName := SerializablePackages.Items[I].PackageName;
     Data^.PackageState := SerializablePackages.Items[I].PackageState;
     Data^.PackageFileName := SerializablePackages.Items[I].PackageFileName;
     Data^.RepositoryFileName := SerializablePackages.Items[I].RepositoryFileName;
     Data^.RepositoryFileSize := SerializablePackages.Items[I].RepositoryFileSize;
     Data^.InstalledVersion := SerializablePackages.Items[I].InstalledFileVersion;
     Data^.DataType := 0;
     //add Version(DataType = 1)
     ChildNode := FVST.AddChild(Node);
     ChildData := FVST.GetNodeData(ChildNode);
     if Assigned(SerializablePackages.Items[I].Version) then
       ChildData^.Version := SerializablePackages.Items[I].Version.AsString;
     ChildData^.DataType := 1;
     //add Description(DataType = 2)
     ChildNode := FVST.AddChild(Node);
     ChildData := FVST.GetNodeData(ChildNode);
     ChildData^.Description := SerializablePackages.Items[I].Description;
     ChildData^.DataType := 2;
     //add Author(DataType = 3)
     ChildNode := FVST.AddChild(Node);
     ChildData := FVST.GetNodeData(ChildNode);
     ChildData^.Author := SerializablePackages.Items[I].Author;
     ChildData^.DataType := 3;
     //add Package Lazarus Compatibility(DataType = 4)
     ChildNode := FVST.AddChild(Node);
     ChildData := FVST.GetNodeData(ChildNode);
     ChildData^.LazCompatibility := SerializablePackages.Items[I].LazCompatibility;
     ChildData^.DataType := 4;
     //add Package type(DataType = 5)
     ChildNode := FVST.AddChild(Node);
     ChildData := FVST.GetNodeData(ChildNode);
     ChildData^.PackageType := SerializablePackages.Items[I].PackageType;
     ChildData^.DataType := 5;
     //add Dependencies(DataType = 6)
     ChildNode := FVST.AddChild(Node);
     ChildData := FVST.GetNodeData(ChildNode);
     if Assigned(SerializablePackages.Items[I].Dependencies) then
       ChildData^.Dependencies := SerializablePackages.Items[I].Dependencies.GetDependenciesAsString(True);
     ChildData^.DataType := 6;
     //add License(DataType = 7)
     ChildNode := FVST.AddChild(Node);
     ChildData := FVST.GetNodeData(ChildNode);
     ChildData^.License := SerializablePackages.Items[I].License;
     ChildData^.DataType := 7;
     //add Miscellanious(DataType = 8)
     ChildNode := FVST.AddChild(Node);
     ChildData := FVST.GetNodeData(ChildNode);
     ChildData^.DataType := 8;
     //add Repository Filename(DataType = 9)
     GrandChildNode := FVST.AddChild(ChildNode);
     GrandChildData := FVST.GetNodeData(GrandChildNode);
     GrandChildData^.RepositoryFileName := SerializablePackages.Items[I].RepositoryFileName;
     GrandChildData^.DataType := 9;
     //add Repository Filesize(DataType = 10)
     GrandChildNode := FVST.AddChild(ChildNode);
     GrandChildData := FVST.GetNodeData(GrandChildNode);
     GrandChildData^.RepositoryFileSize := SerializablePackages.Items[I].RepositoryFileSize;
     GrandChildData^.DataType := 10;
     //add Repository Date(DataType = 11)
     GrandChildNode := FVST.AddChild(ChildNode);
     GrandChildData := FVST.GetNodeData(GrandChildNode);
     GrandChildData^.RepositoryFileHash := SerializablePackages.Items[I].RepositoryFileHash;
     GrandChildData^.DataType := 11;
     //add Repository Date(DataType = 12)
     GrandChildNode := FVST.AddChild(ChildNode);
     GrandChildData := FVST.GetNodeData(GrandChildNode);
     GrandChildData^.RepositoryDate := SerializablePackages.Items[I].RepositoryDate;
     GrandChildData^.DataType := 12;
  end;
  FVST.SortTree(0, opkman_VirtualTrees.sdAscending);
 // ExpandEx;
end;

procedure TVisualTree.CheckNodes(const Checked: Boolean);
var
  Node: PVirtualNode;
begin
  Node := FVST.GetFirst;
  while Assigned(Node) do
  begin
    if Checked then
      FVST.CheckState[Node] := csCheckedNormal
    else
      FVST.CheckState[Node] := csUncheckedNormal;
    Node := FVST.GetNextSibling(Node);
  end;
end;

procedure TVisualTree.FilterTree(const AText: String;
  const AFilterBy: Integer; const AExtraParam: Integer);
var
  Node, RootNode: PVirtualNode;
  Data: PData;
  SubStr: String;
  Str: String;
  P: Integer;
  FilterBy: Integer;
begin
  if AFilterBy > 0 then
    FilterBy := AFilterBy - 1
  else
    FilterBy := AFilterBy;
  SubStr := UpperCase(AText);
  Node := FVST.GetFirst;
  while Assigned(Node) do
  begin
    if FVST.GetNodeLevel(Node) = 0 then
      RootNode := Node
    else
      RootNode := Node^.Parent;
    Data := FVST.GetNodeData(Node);
    if (Data^.DataType = FilterBy) then
    begin
      case FilterBy of
        0: begin
             if AExtraParam = -1 then
               Str := UpperCase(Data^.PackageName)
             else
             begin
               if Data^.PackageState = TPackageState(AExtraParam) then
                 Str := Substr
               else
                 Str := '';
             end;
           end;
        1: Str := UpperCase(Data^.Version);
        2: Str := UpperCase(Data^.Description);
        3: Str := UpperCase(Data^.Author);
        4: Str := UpperCase(Data^.LazCompatibility);
        5: begin
             if Data^.PackageType = TPackageType(AExtraParam) then
               Str := Substr
             else
               Str := '';
           end;
        6: Str := UpperCase(Data^.Dependencies);
        7: Str := UpperCase(Data^.License);
      end;
      P := Pos(SubStr, Str);
      if P > 0 then
        FVST.IsVisible[RootNode] := True
      else
        FVST.IsVisible[RootNode] := False;
    end;
    Node := FVST.GetNext(Node);
  end;
end;

procedure TVisualTree.ResetFilter;
var
  Node: PVirtualNode;
begin
  Node := FVST.GetFirst;
  while Assigned(Node) do
  begin
    FVST.IsVisible[Node] := True;
    Node := FVST.GetNextSibling(Node);
  end;
end;

procedure TVisualTree.ExpandEx;
var
  Node: PVirtualNode;
  Data: PData;
begin
  Node := FVST.GetFirst;
  while Assigned(Node) do
  begin
    Data := FVST.GetNodeData(Node);
    if Data^.DataType = 0 then
      FVST.Expanded[Node] := True
    else if Data^.DataType = 5 then
      FVST.Expanded[Node] := False;
    Node := FVST.GetNext(Node);
  end;
  Node := FVST.GetFirst;
  if Node <> nil then
    FVST.TopNode := Node;
end;

procedure TVisualTree.GetPackageList;
var
  Node: PVirtualNode;
  Data: PData;
  Index: Integer;
begin
  Node := FVST.GetFirst;
  while Assigned(Node) do
  begin
    Data := FVST.GetNodeData(Node);
    Index := SerializablePackages.FindPackage(Data^.PackageName, fpbPackageName);
    if Index <> - 1 then
    begin
      if FVST.CheckState[Node] = csCheckedNormal then
        SerializablePackages.Items[Index].Checked := True
      else if FVST.CheckState[Node] = csUncheckedNormal then
        SerializablePackages.Items[Index].Checked := False
    end;
    Node := FVST.GetNextSibling(Node);
  end;
end;

procedure TVisualTree.UpdatePackageStates;
var
  Node: PVirtualNode;
  Data: PData;
  Index: Integer;
begin
  SerializablePackages.GetPackageStates;
  Node := FVST.GetFirst;
  while Assigned(Node) do
  begin
    if FVST.CheckState[Node] = csCheckedNormal then
    begin
      Data := FVST.GetNodeData(Node);
      Index := SerializablePackages.FindPackage(Data^.PackageName, fpbPackageName);
      if Index <> - 1 then
      begin
        Data^.PackageState := SerializablePackages.Items[Index].PackageState;
        FVST.ReinitNode(Node, False);
        FVST.RepaintNode(Node);
      end;
    end;
    Node := FVST.GetNextSibling(Node);
  end;
end;

procedure TVisualTree.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Data: PData;
begin
  Data := Sender.GetNodeData(Node);
  if Data^.DataType = 0 then
  begin
    if (Node = Sender.FocusedNode) then
     begin
       if Column = 0 then
       begin
         TargetCanvas.Brush.Color := $00E5E5E5;
         TargetCanvas.FillRect(CellRect);
         TargetCanvas.Brush.Color := FVST.Colors.FocusedSelectionColor;
         TargetCanvas.FillRect(ContentRect)
       end
       else
       begin
         TargetCanvas.Brush.Color := FVST.Colors.FocusedSelectionColor;
         TargetCanvas.FillRect(CellRect)
       end;
     end
     else
     begin
       TargetCanvas.Brush.Color := $00E5E5E5;
       TargetCanvas.FillRect(CellRect);
     end;
   end
   else
   begin
     if (Node = Sender.FocusedNode) then
     begin
       TargetCanvas.Brush.Color := FVST.Colors.FocusedSelectionColor;
       if Column = 0 then
         TargetCanvas.FillRect(ContentRect)
       else
         TargetCanvas.FillRect(CellRect);
     end
     else
     begin
       TargetCanvas.Brush.Style := bsClear;
       TargetCanvas.FillRect(CellRect);
     end;
   end;
end;

procedure TVisualTree.VSTChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var NewState: TCheckState; var Allowed: Boolean);
begin
  if NewState = csUncheckedNormal then
  begin
    if Assigned(FOnChecking) then
      FOnChecking(Self);
  end;
end;

procedure TVisualTree.VSTChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  I: Integer;
  NodeSearch: PVirtualNode;
  Data, DataSearch: PData;
  PackageList: TObjectList;
  PackageFileName: String;
begin
  if VST.CheckState[Node] = csCheckedNormal then
  begin
    Data := VST.GetNodeData(Node);
    PackageList := TObjectList.Create(True);
    try
      SerializablePackages.GetPackageDependencies(Data^.PackageName, PackageList, True, True);
      for I := 0 to PackageList.Count - 1 do
      begin
        PackageFileName := TPackageDependency(PackageList.Items[I]).PackageName + '.lpk';
        NodeSearch := VST.GetFirst;
        while Assigned(NodeSearch) do
        begin
          DataSearch := FVST.GetNodeData(NodeSearch);
          if (UpperCase(DataSearch^.PackageFileName) = UpperCase(PackageFileName)) and (FVST.CheckState[NodeSearch] <> csCheckedNormal) then
          begin
            if MessageDlg(rsProgressFrmlbPackageCaption + ' "' + Data^.PackageName + '" ' + rsPackageDependency0 + ' "' + DataSearch^.PackageName + '". ' + rsPackageDependency1, mtConfirmation, [mbYes, mbNo], 0) = mrNo then
              MessageDlg(rsPackageDependency2, mtInformation, [mbOk], 0)
            else
            begin
              FVST.CheckState[NodeSearch] := csCheckedNormal;
              FVST.ReinitNode(NodeSearch, False);
              FVST.RepaintNode(NodeSearch);
              MessageDlg(rsPackageDependency3, mtInformation, [mbOk], 0)
            end;
          end;
          NodeSearch := FVST.GetNextSibling(NodeSearch);
        end;
      end;
    finally
      PackageList.Free;
    end;
  end
  else if VST.CheckState[Node] = csUncheckedNormal then
  begin
    Data := VST.GetNodeData(Node);
    NodeSearch := VST.GetFirst;
    while Assigned(NodeSearch) do
    begin
      if Node <> NodeSearch then
      begin
        DataSearch := FVST.GetNodeData(NodeSearch);
        PackageList := TObjectList.Create(True);
        try
          SerializablePackages.GetPackageDependencies(DataSearch^.PackageName, PackageList, True, True);
          for I := 0 to PackageList.Count - 1 do
          begin
            PackageFileName := TPackageDependency(PackageList.Items[I]).PackageName + '.lpk';
            if (UpperCase(Data^.PackageFileName) = UpperCase(PackageFileName)) and (FVST.CheckState[NodeSearch] = csCheckedNormal) then
            begin
               if MessageDlg(rsProgressFrmlbPackageCaption + ' "' + Data^.PackageName + '" ' + rsPackageDependency4 + ' "' + DataSearch^.PackageName + '" ' + rsPackageDependency5, mtConfirmation, [mbYes, mbNo], 0) = mrNo then
               begin
                 FVST.CheckState[Node] := csCheckedNormal;
                 FVST.ReinitNode(Node, False);
                 FVST.RepaintNode(Node);
               end;
            end;
          end;
        finally
          PackageList.Free;
        end;
      end;
      NodeSearch := FVST.GetNextSibling(NodeSearch);
    end;
  end;
end;

procedure TVisualTree.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1: PData;
  Data2: PData;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  case Column of
    0: if (Data1^.DataType = 0) and (Data1^.DataType = 0) then
         Result := CompareText(Data1^.PackageName, Data2^.PackageName);
    1: if (Data1^.DataType = 0) and (Data1^.DataType = 0) then
         Result := Ord(Data1^.PackageState) - Ord(Data2^.PackageState);
  end;
end;

procedure TVisualTree.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  if Column = 0 then
    ImageIndex := Data^.DataType;
end;

procedure TVisualTree.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  if Column = 0 then
  begin
    case Data^.DataType of
      0: CellText := Data^.PackageName;
      1: CellText := rsMainFrmVSTTextVersion;
      2: CellText := rsMainFrmVSTTextDescription;
      3: CellText := rsMainFrmVSTTextAuthor;
      4: CellText := rsMainFrmVSTTextCompatible;
      5: CellText := rsMainFrmVSTTextPackagetype;
      6: CellText := rsMainFrmVSTTextDependecies;
      7: CellText := rsMainFrmVSTTextLicense;
      8: CellText := rsMainFrmVSTTextMiscellaneous;
      9: CellText := rsMainFrmVSTTextRepositoryFilename;
     10: CellText := rsMainFrmVSTTextRepositoryFileSize;
     11: CellText := rsMainFrmVSTTextRepositoryFileHash;
     12: CellText := rsMainFrmVSTTextRepositoryFileDate;
    end;
  end
  else if Column = 1 then
  begin
    case Data^.DataType of
      0: begin
           case Ord(Data^.PackageState) of
             0: CellText := rsMainFrmPackageState0;
             1: CellText := rsMainFrmPackageState1;
             2: CellText := rsMainFrmPackageState2;
             3: CellText := rsMainFrmPackageState3 + '(Version: ' +  Data^.InstalledVersion + ')';
           end;
         end;
      1: CellText := Data^.Version;
      2: CellText := Data^.Description;
      3: CellText := Data^.Author;
      4: CellText := 'Lazarus: ' + Data^.LazCompatibility;
      5: begin
           case Data^.PackageType of
             ptRunAndDesignTime: CellText := rsMainFrmVSTTextPackageType0;
             ptDesignTime:       CellText := rsMainFrmVSTTextPackageType1;
             ptRunTime:          CellText := rsMainFrmVSTTextPackageType2;
             ptRunTimeOnly:      CellText := rsMainFrmVSTTextPackageType3;
           end;
         end;
      6: CellText := Data^.Dependencies;
      7: CellText := Data^.License;
      8: CellText := '';
      9: CellText := Data^.RepositoryFileName;
     10: CellText := FormatSize(Data^.RepositoryFileSize);
     11: CellText := Data^.RepositoryFileHash;
     12: CellText := FormatDateTime('YYYY.MM.DD', Data^.RepositoryDate);
    end;
  end;
end;

procedure TVisualTree.VSTHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    with Sender, Treeview do
    begin
      if (SortColumn = NoColumn) or (SortColumn <> Column) then
      begin
        SortColumn    := Column;
        SortDirection := opkman_VirtualTrees.sdAscending;
      end
      else
      begin
        if SortDirection = opkman_VirtualTrees.sdAscending then
          SortDirection := opkman_VirtualTrees.sdDescending
        else
          SortDirection := opkman_VirtualTrees.sdAscending;
        FSortDir := SortDirection;
      end;
      SortTree(SortColumn, SortDirection, False);
      FSortCol := Sender.SortColumn;
    end;
  end;
end;

procedure TVisualTree.VSTPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  if Data^.DataType = 0 then
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold]
  else
    TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsBold];
  if Node <> Sender.FocusedNode then
  begin
    if (Data^.DataType = 0) and (Column = 1) then
    begin
      case Ord(Data^.PackageState) of
        0: TargetCanvas.Font.Color := clBlack;
        1: TargetCanvas.Font.Color := clBlack;
        2: TargetCanvas.Font.Color := clBlack;
        3: TargetCanvas.Font.Color := clBlack;
        4: TargetCanvas.Font.Color := clBlack;
      end;
    end
    else
      TargetCanvas.Font.Color := FVST.Font.Color
  end
  else
    TargetCanvas.Font.Color := clWhite;
end;

procedure TVisualTree.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  Finalize(Data^);
end;

end.

