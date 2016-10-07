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
   Implementation of the serializable package class. Information about the
   repository packages are stored in a json file. After the JSON is downloaded
   it gets serialized to a package list.}

unit opkman_serializablepackages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fpjsonrtti, fpjson, jsonparser, md5, contnrs,
  PackageIntf;

type
  TPackageState = (
    psRepository,
    psDownloaded,
    psExtracted,
    psInstalled,
    psError);
  TPackageStates = set of TPackageState;

  TPackageType = (
    ptRunAndDesignTime,
    ptDesignTime,
    ptRunTime,
    ptRunTimeOnly);

  { TPackageVersion }

  TPackageVersion = class(TPersistent)
  private
    FMajor: Integer;
    FMinor: Integer;
    FRelease: Integer;
    FBuild: Integer;
    function GetAsString: String;
    procedure SetAsString(const AValue: String);
    function GetIsNullVersion: Boolean;
  public
    procedure SetDefaults;
    procedure Assign(ASource: TPersistent); override;
    function CompareVersion(AVersion: TPackageVersion): Integer;
    function SameVersion(AVersion: TPackageVersion): Boolean;
    property AsString: String read GetAsString write SetAsString;
  published
     property Major: Integer read FMajor write FMajor;
     property Minor: Integer read FMinor write FMinor;
     property Release: Integer read FRelease write FRelease;
     property Build: Integer read FBuild write FBuild;
     property IsNullVersion: Boolean read GetIsNullVersion;
  end;

  { TPackageDependency }

  TPackageDependency = class(TCollectionItem)
  private
    FMaxVersion: TPackageVersion;
    FMinVersion: TPackageVersion;
    FPackageName: String;
    procedure SetMinVersion(const AValue: TPackageVersion);
    procedure SetMaxVersion(const AValue: TPackageVersion);
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property PackageName: String read FPackageName write FPackageName;
    property MinVersion: TPackageVersion read FMinVersion write SetMinVersion;
    property MaxVersion: TPackageVersion read FMaxVersion write SetMaxVersion;
  end;

  { TPackageDependencies }

  TPackageDependencies = class(TCollection)
  private
    function GetDependency(AIndex: Integer): TPackageDependency;
    procedure SetDependency(AIndex: Integer; const AValue: TPackageDependency);
  public
    function GetDependenciesAsString(const AIsDisplayString: Boolean): String;
    procedure SetDependenciesAsString(const AValue: String);
    property Dependencies[AIndex: Integer]: TPackageDependency read GetDependency write SetDependency; default;
  end;

  { TPackage }

  TPackage = class(TCollectionItem)
  private
    FPackageState: TPackageState;
    FPackageName: String;
    FPackageFileName: String;
    FPackageRelativeFilePath: String;
    FPackageAbsolutePath: String;
    FVersionAsString: String;
    FDescription: String;
    FAuthor: String;
    FLazCompatibility: String;
    FPackageType: TPackageType;
    FLicense: String;
    FDependenciesAsString: String;
    FRepositoryFileName: String;
    FRepositoryFileSize: Int64;
    FRepositoryFileHash: String;
    FRepositoryDate: TDate;
    FVersion: TPackageVersion;
    FDependencies: TPackageDependencies;
    FPackageStates: TPackageStates;
    FChecked: Boolean;
    FInstalledFileName: String;
    FInstalledFileVersion: String;
    FPackageBaseDir: String;
    function GetVersionAsString: String;
    function GetDependenciesAsString: String;
    procedure SetVersionAsString(const AValue: String);
    procedure SetDependenciesAsString(const AValue: String);
    function GetDownloadable: Boolean;
    function GetExtractable: Boolean;
    function GetInstallable: Boolean;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  public
    property Version: TPackageVersion read FVersion write FVersion;
    property Dependencies: TPackageDependencies read FDependencies write FDependencies;
    property PackageStates: TPackageStates read FPackageStates write FPackageStates;
    property PackageState: TPackageState read FPackageState write FPackageState;
    property InstalledFileName: String read FInstalledFileName write FInstalledFileName;
    property InstalledFileVersion: String read FInstalledFileVersion write FInstalledFileVersion;
    property PackageBaseDir: String read FPackageBaseDir write FPackageBaseDir;
    property PackageAbsolutePath: String read FPackageAbsolutePath write FPackageAbsolutePath;
    property Checked: Boolean read FChecked write FChecked;
    property IsDownloadable: Boolean read GetDownloadable;
    property IsExtractable: Boolean read GetExtractable;
    property IsInstallable: Boolean read GetInstallable;
  published
    property PackageName: String read FPackageName write FPackageName;
    property PackageFileName: String read FPackageFileName write FPackageFileName;
    property PackageRelativeFilePath: string read FPackageRelativeFilePath write FPackageRelativeFilePath;
    property VersionAsString: String read GetVersionAsString write SetVersionAsString;
    property Description: String read FDescription write FDescription;
    property Author: String read FAuthor write FAuthor;
    property LazCompatibility: String read FLazCompatibility write FLazCompatibility;
    property PackageType: TPackageType read FPackageType write FPackageType;
    property License: String read FLicense write FLicense;
    property DependenciesAsString: String read GetDependenciesAsString write SetDependenciesAsString;
    property RepositoryFileName: String read FRepositoryFileName write FRepositoryFileName;
    property RepositoryFileSize: int64 read FRepositoryFileSize write FRepositoryFileSize;
    property RepositoryFileHash: String read FRepositoryFileHash write FRepositoryFileHash;
    property RepositoryDate: TDate read FRepositoryDate write FRepositoryDate;
  end;

  { TPackages }
  TPackages = class(TPersistent)
  private
    FPackageCount: Integer;
    FPackageList: TCollection;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property PackageCount: Integer read FPackageCount write FPackageCount;
    property PackageList: TCollection read FPackageList;
  end;

  { TSerializablePackages }

  TFindPackageBy = (fpbPackageName, fpbFilename);
  TSerializablePackages = class
  private
    FPackages: TPackages;
    FLastError: String;
    function GetCount: Integer;
    function GetDownloadCount: Integer;
    function GetExtractCount: Integer;
    function GetInstallCount: Integer;
    function GetItem(const AIndex: Integer): TPackage;
    procedure SetItem(const AIndex: Integer; const APackage: TPackage);
    procedure DoGetPackageDependencies(const APackageName: String; ASL: TStringList; ALevel: Integer);
    function IsPackageDownloaded(const APackage: TPackage): Boolean;
    function IsPackageExtracted(const APackage: TPackage): Boolean;
    function IsPackageInstalled(const APackage: TPackage): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function AddPackage(const APackage: TPackage): Integer; overload;
    procedure DeletePackage(const AIndex: Integer);
    procedure Clear;
    function FindPackage(const AValue: String; const AFindPackageBy: TFindPackageBy): Integer;
    function JSONToPackages(const JSON: TJSONStringType): Boolean;
    function PackagesToJSON(var JSON: TJSONStringType): Boolean;
    procedure GetPackageDependencies(const APackageName: String; List: TObjectList; Recurse, OnlyUnresolved: Boolean);
    procedure GetPackageStates;
    procedure RemoveErrorState;
    procedure MarkRuntimePackages;
    function Cleanup: Integer;
  public
    property Count: Integer read GetCount;
    property DownloadCount: Integer read GetDownloadCount;
    property ExtractCount: Integer read GetExtractCount;
    property InstallCount: Integer read GetInstallCount;
    property Items[Index: Integer]: TPackage read GetItem write SetItem;
    property LastError: String read FlastError;
  end;

  TSerializablePackagesEx = class(TSerializablePackages)

  end;

var
  SerializablePackages: TSerializablePackages = nil;

implementation

uses opkman_const, opkman_common;

{ TPackageVersion }

function TPackageVersion.GetAsString: String;
begin
  Result := IntToStr(Major) + '.' + IntToStr(Minor) + '.' + IntToStr(Release) + '.' + IntToStr(Build);
end;

procedure TPackageVersion.SetAsString(const AValue: String);
var
  Version: String;
  P, I: Integer;
begin
  SetDefaults;
  if AValue = '' then
    Exit;
  I := 0;
  Version := Trim(AValue) + '.';
  repeat
     Inc(I);
     P := Pos('.', Version);
     if P <> 0 then
     begin
       case I of
         1: FMajor := StrToIntDef(Copy(Version, 1, P - 1), 0);
         2: FMinor := StrToIntDef(Copy(Version, 1, P - 1), 0);
         3: FRelease := StrToIntDef(Copy(Version, 1, P - 1), 0);
         4: FBuild := StrToIntDef(Copy(Version, 1, P - 1), 0);
       end;
       Delete(Version, 1, P);
     end;
  until (Version = '') or (P = 0) or (I > 4);
end;

function TPackageVersion.GetIsNullVersion: Boolean;
begin
  Result := (FMajor = 0) and (FMinor = 0) and (FRelease = 0) and (FBuild = 0);
end;

procedure TPackageVersion.SetDefaults;
begin
  FMajor := 0;
  FMinor := 0;
  FRelease := 0;
  FBuild := 0;
end;

procedure TPackageVersion.Assign(ASource: TPersistent);
var
  Source: TPackageVersion;
begin
  SetDefaults;
  if ASource is TPackageVersion then
  begin
    Source := ASource as TPackageVersion;
    Major := Source.Major;
    Minor := Source.Minor;
    Release := Source.Release;
    Build := Source.Build;
  end
  else
    inherited Assign(Source);
end;

function TPackageVersion.CompareVersion(AVersion: TPackageVersion): Integer;
begin
  Result := Major - AVersion.Major;
  if (Result = 0) then
  begin
    Result := Minor - AVersion.Minor;
    if (Result = 0) then
    begin
      Result := Release - AVersion.Release;
      if (Result = 0) then
        Result := Build - AVersion.Build;
    end;
  end;
end;

function TPackageVersion.SameVersion(AVersion: TPackageVersion): Boolean;
begin
  Result := CompareVersion(AVersion) = 0;
end;

{ TPackageDependency }

procedure TPackageDependency.SetMinVersion(const AValue: TPackageVersion);
begin
  FMinVersion.Assign(AValue);
end;

procedure TPackageDependency.SetMaxVersion(const AValue: TPackageVersion);
begin
  FMaxVersion.Assign(AValue);
end;

procedure TPackageDependency.Assign(ASource: TPersistent);
var
  Source: TPackageDependency;
begin
  if ASource is TPackageDependency then
  begin
    Source := ASource as TPackageDependency;
    FPackageName := Source.PackageName;
    if Assigned(Source.MinVersion) then
    begin
      FMinVersion := TPackageVersion.Create;
      FMinVersion.Assign(Source.MinVersion);
    end;
    if Assigned(Source.MaxVersion) then
    begin
      FMaxVersion := TPackageVersion.Create;
      FMaxVersion.Assign(Source.MaxVersion);
    end;
  end
  else
    inherited Assign(Source);
end;

constructor TPackageDependency.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FMinVersion := TPackageVersion.Create;
  FMaxVersion := TPackageVersion.Create;
end;

destructor TPackageDependency.Destroy;
begin
  if Assigned(FMinVersion) then
    FMinVersion.Free;
  if Assigned(fMaxVersion) then
    FMaxVersion.Free;
  inherited Destroy;
end;

{ TPackageDependencies }

function TPackageDependencies.GetDependency(AIndex: Integer): TPackageDependency;
begin
  Result := TPackageDependency(Items[AIndex]);
end;

procedure TPackageDependencies.SetDependency(AIndex: Integer;
  const AValue: TPackageDependency);
begin
  Items[AIndex] := AValue;
end;

function TPackageDependencies.GetDependenciesAsString(const AIsDisplayString: Boolean): String;
var
  I: Integer;
  MinVer, MaxVer: String;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    MinVer := '';
    MaxVer := '';
    if not Dependencies[I].FMinVersion.IsNullVersion then
    begin
      if AIsDisplayString then
        MinVer := '(>=' + IntToStr(Dependencies[I].FMinVersion.Major) + '.' + IntToStr(Dependencies[I].FMinVersion.Minor) + ')'
      else
        MinVer := '(' + Dependencies[I].FMinVersion.AsString + ')';
    end;
    if not Dependencies[I].FMaxVersion.IsNullVersion then
    begin
      if AIsDisplayString then
        MaxVer := '(<=' + IntToStr(Dependencies[I].FMaxVersion.Major) + '.' + IntToStr(Dependencies[I].FMaxVersion.Minor) + ')'
      else
        MaxVer := '(' + Dependencies[I].FMaxVersion.AsString + ')'
    end;
    if Result = '' then
      Result := Dependencies[I].PackageName + MinVer + MaxVer
    else
      Result := Result + ', ' + Dependencies[I].PackageName + MinVer + MaxVer;
  end;
end;

procedure TPackageDependencies.SetDependenciesAsString(const AValue: String);
var
  PackageDependency: TPackageDependency;
  SL: TStringList;
  P1, P2: Integer;
  Str: String;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Delimiter := ',';
    SL.DelimitedText := AValue;
    for I := 0 to SL.Count - 1  do
    begin
      Str := Trim(SL.Strings[I]);
      PackageDependency := TPackageDependency(Self.Add);
      if not Assigned(PackageDependency.FMinVersion) then
        PackageDependency.FMinVersion := TPackageVersion.Create;
      if not Assigned(PackageDependency.FMaxVersion) then
        PackageDependency.FMaxVersion := TPackageVersion.Create;
      P1 := Pos('(', Str);
      P2 := Pos(')', Str);
      if (P1 <> 0) and (P2 <> 0) then
      begin
        PackageDependency.PackageName := Trim(Copy(Str, 1, P1 - 1));
        PackageDependency.FMinVersion.AsString := Trim(Copy(Str, P1 + 1, P2 - P1 - 1));
        System.Delete(Str, 1, P2);
        if Length(Trim(Str)) > 0 then
          PackageDependency.FMaxVersion.AsString := Trim(Copy(Str, 2, Length(Str) - 2));
      end
      else
        PackageDependency.PackageName := Trim(Str);
    end;
  finally
    SL.Free;
  end;
end;

{ TPackages }

constructor TPackages.Create;
begin
  FPackageList := TCollection.Create(TPackage);
end;

destructor TPackages.Destroy;
begin
  FPackageList.Free;
  inherited Destroy;
end;

{ TPackage }

function TPackage.GetVersionAsString: String;
begin
  Result := FVersion.AsString;
end;

function TPackage.GetDependenciesAsString: String;
begin
  Result := FDependencies.GetDependenciesAsString(False);
end;

procedure TPackage.SetVersionAsString(const AValue: String);
begin
  if not Assigned(FVersion) then
  begin
    FVersion := TPackageVersion.Create;
    FVersion.AsString := AValue;
  end;
  FVersionAsString := AValue;
end;

procedure TPackage.SetDependenciesAsString(const AValue: String);
begin
  if not Assigned(FDependencies) then
  begin
    FDependencies := TPackageDependencies.Create(TPackageDependency);
    FDependencies.SetDependenciesAsString(AValue);
  end;
  FDependenciesAsString := AValue;
end;

function TPackage.GetDownloadable: Boolean;
begin
  case PackageAction of
    paDownloadTo:
      Result := (Checked) and (not (psError in PackageStates));
    paInstall:
      Result := (ForceDownload) or
                (
                  (Checked) and
                  (psRepository in PackageStates) and
                  (not (psError in PackageStates)) and
                  (not (psDownloaded in PackageStates)) and
                  (not (psExtracted in PackageStates))
                 );
  end;
end;

function TPackage.GetExtractable: Boolean;
begin
  case PackageAction of
     paDownloadTo:
       Result := (Checked) and (not (psError in PackageStates));
     paInstall:
       Result := (ForceExtract) or
                 (
                    (Checked) and
                    (psDownloaded in PackageStates) and
                    (not (psError in PackageStates)) and
                    (not (psExtracted in PackageStates)) and
                    (not (psInstalled in PackageStates))
                  );
   end;
end;

function TPackage.GetInstallable: Boolean;
begin
  case PackageAction of
     paDownloadTo:
       Result := (Checked) and (not (psError in PackageStates));
     paInstall:
       Result := (Checked) and
                 (psExtracted in PackageStates) and
                 (not (psError in PackageStates));
   end;
end;

constructor TPackage.Create;
begin
  FVersion := TPackageVersion.Create;
  FVersion.SetDefaults;
  PackageStates := [];
  FDependencies := TPackageDependencies.Create(TPackageDependency);
end;

destructor TPackage.Destroy;
begin
  if Assigned(FVersion) then
    FreeAndNil(FVersion);
  if Assigned(FDependencies) then
    FreeAndNil(FDependencies);
  inherited Destroy;
end;

{ TSerializablePackages }

function TSerializablePackages.GetCount: Integer;
begin
  Result := FPackages.PackageList.Count;
end;

function TSerializablePackages.GetDownloadCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].IsDownloadable then
      Inc(Result);
end;

function TSerializablePackages.GetExtractCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].IsExtractable then
      Inc(Result);
end;

function TSerializablePackages.GetInstallCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].IsInstallable then
      Inc(Result);
end;

function TSerializablePackages.GetItem(const AIndex: Integer): TPackage;
begin
  Result := nil;
  if AIndex > FPackages.PackageList.Count - 1 then
    Exit;
  Result := TPackage(FPackages.PackageList.Items[AIndex]);
end;

procedure TSerializablePackages.SetItem(const AIndex: Integer;
  const APackage: TPackage);
begin
  if AIndex > FPackages.PackageList.Count - 1 then
    Exit;
  FPackages.PackageList.Items[AIndex] := TPackage(APackage);
end;

procedure TSerializablePackages.DoGetPackageDependencies(
  const APackageName: String; ASL: TStringList; ALevel: Integer);
var
  P: TPackage;
  D2, D1: TPackageDependency;
  I, J: Integer;
begin
  if (ALevel > 10) then
    Exit;
  I := FindPackage(APackageName, fpbPackageName);
  if I = -1 then
    Exit;
  P := Items[I];
  for I := 0 to P.Dependencies.Count - 1 do
  begin
    D1 := P.Dependencies[I];
    J := ASL.IndexOf(APackageName);
    If J = -1 then
    begin
      D2 := TPackageDependency.Create(nil);
      D2.Assign(D1);
      ASL.AddObject(D2.PackageName, D2);
    end
    else
    begin
      D2 := ASL.Objects[J] as TPackageDependency;
      if D1.MinVersion.CompareVersion(D2.MinVersion) > 0 then
        D2.MinVersion.Assign(D1.MinVersion);
    end;
    if (ALevel >= 0) and (J = -1) Then
      DoGetPackageDependencies(D2.PackageName, ASL, ALevel + 1);
  end;
end;

constructor TSerializablePackages.Create;
begin
  FPackages := TPackages.Create;
end;

destructor TSerializablePackages.Destroy;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Items[I].Free;
  FPackages.Free;
  inherited Destroy;
end;

function TSerializablePackages.AddPackage(const APackage: TPackage): Integer;
var
  Index: Integer;
  Package: TPackage;
begin
  Result := -1;
  Index := FindPackage(APackage.PackageName, fpbPackageName);
  if Index <> -1 then
  begin
    FLastError := rsMainFrmPackageNameAlreadyExists;
    Exit;
  end;
  Index := FindPackage(APackage.RepositoryFileName, fpbFilename);
  if (Index <> -1) then
  begin
    FLastError := rsMainFrmFilenameAlreadyExists;
    Exit;
  end;
  Package := TPackage(FPackages.PackageList.Add);
  Package.PackageName := APackage.PackageName;
  Package.PackageFileName := APackage.PackageFileName;
  Package.PackageRelativeFilePath := APackage.PackageRelativeFilePath;
  Package.Version := TPackageVersion.Create;
  Package.Version.AsString := APackage.Version.AsString;
  Package.Description := APackage.Description;
  Package.Author := APackage.Author;
  Package.LazCompatibility := APackage.LazCompatibility;
  Package.PackageType := APackage.PackageType;
  Package.Dependencies := TPackageDependencies.Create(TPackageDependency);
  Package.Dependencies.SetDependenciesAsString(APackage.GetDependenciesAsString);
  Package.License := APackage.License;
  Package.RepositoryFileName := APackage.RepositoryFileName;
  Package.RepositoryFileSize := APackage.RepositoryFileSize;
  Package.RepositoryFileHash := APackage.RepositoryFileHash;
  Package.RepositoryDate := APackage.RepositoryDate;
  Result := FPackages.PackageList.Count - 1;
end;

procedure TSerializablePackages.DeletePackage(const AIndex: Integer);
begin
  if AIndex > FPackages.PackageList.Count - 1 then
    Exit;
  Items[AIndex].Free;
  FPackages.PackageList.Delete(AIndex);
end;

procedure TSerializablePackages.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Items[I].Free;
  FPackages.PackageList.Clear;
end;

function TSerializablePackages.FindPackage(const AValue: String;
  const AFindPackageBy: TFindPackageBy): Integer;
var
  I: Integer;
  NeedToBreak: Boolean;
begin
  Result := -1;
  for I := 0 to FPackages.PackageList.Count - 1 do
  begin
    case AFindPackageBy of
      fpbPackageName: NeedToBreak := Items[I].PackageName = AValue;
      fpbFilename: NeedToBreak := Items[I].RepositoryFileName = AValue
    end;
    if NeedToBreak then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TSerializablePackages.JSONToPackages(const JSON: TJSONStringType): Boolean;
var
  JSONDeStreamer: TJSONDeStreamer;
  I: Integer;
begin
  Result := False;
  FPackages.PackageList.Clear;
  JSONDeStreamer := TJSONDeStreamer.Create(nil);
  try
    try
      JSONDeStreamer.JSONToObject(JSON, FPackages.PackageList);
      for I := 0 to FPackages.PackageList.Count - 1 do
        TPackage(FPackages.PackageList.Items[I]).PackageStates := TPackage(FPackages.PackageList.Items[I]).PackageStates + [psRepository];
      GetPackageStates;
      Result := True;
    except
      Result := False;
    end;
  finally
    JSONDeStreamer.Destroy;
  end;
end;

function TSerializablePackages.PackagesToJSON(var JSON: TJSONStringType): Boolean;
var
  JSONStreamer: TJSONStreamer;
begin
  Result := False;
  JSONStreamer := TJSONStreamer.Create(nil);
  try
    JSONStreamer.Options := JSONStreamer.Options + [jsoUseFormatString, jsoTStringsAsObject];
    try
      JSON := JSONStreamer.ObjectToJSONString(FPackages.PackageList);
      Result := True;
    except
      Result := False;
    end;
  finally
    JSONStreamer.Destroy;
  end;
end;

procedure TSerializablePackages.GetPackageDependencies(const APackageName: String;
  List: TObjectList; Recurse, OnlyUnresolved: Boolean);
var
  SL: TStringList;
  I, J: Integer;
  PackageFileName: String;
  Installed: Boolean;
begin
  SL := TStringList.Create;
  try
    SL.Sorted := True;
    DoGetPackageDependencies(APackageName, SL, Ord(Recurse) - 1);
    if OnlyUnresolved then
    begin
      for I := 0 to SL.Count - 1 do
      begin
        PackageFileName := TPackageDependency(SL.Objects[I]).PackageName + '.lpk';
        Installed := False;
        for J := 0 to PackageEditingInterface.GetPackageCount - 1 do
        begin
          if UpperCase(ExtractFileName(PackageEditingInterface.GetPackages(J).Filename)) = UpperCase(PackageFileName) then
          begin
            Installed := True;
            Break;
          end;
        end;
        if not Installed then
          List.Add(SL.Objects[I]);
      end;
    end
    else
      for I := 0 to SL.Count - 1 do
        List.Add(SL.Objects[I]);
  finally
    SL.Free;
  end;
end;

function TSerializablePackages.IsPackageDownloaded(const APackage: TPackage): Boolean;
var
  FileName: String;
begin
  FileName := LocalRepositoryArchive + APackage.RepositoryFileName;
  Result := (FileExists(FileName)) and
            (MD5Print(MD5File(FileName)) = APackage.RepositoryFileHash) and
            (FileUtil.FileSize(FileName) = APackage.RepositoryFileSize);
end;

function TSerializablePackages.IsPackageExtracted(const APackage: TPackage): Boolean;
var
  P: Integer;
begin
  Result := False;
  APackage.FPackageRelativeFilePath := StringReplace(APackage.PackageRelativeFilePath, '\/', PathDelim, [rfReplaceAll]);
  P := Pos(PathDelim, APackage.FPackageRelativeFilePath);
  if P <> 0 then
    APackage.PackageBaseDir := Copy(APackage.FPackageRelativeFilePath, 1, P - 1);
  Result := FileExists(LocalRepositoryPackages + APackage.FPackageRelativeFilePath + APackage.PackageFileName);
  if Result then
    APackage.FPackageAbsolutePath := LocalRepositoryPackages + APackage.FPackageRelativeFilePath + APackage.PackageFileName;
end;

function TSerializablePackages.IsPackageInstalled(const APackage: TPackage): Boolean;
var
  PackageCnt: Integer;
  I: Integer;
  Package: TIDEPackage;
  FileName: String;
begin
  Result := False;
  case APackage.PackageType of
    ptRunTime, ptRunTimeOnly:
      begin
        FileName := StringReplace(APackage.PackageFileName, '.lpk', '.opkman', [rfIgnoreCase]);
        Result := IsPackageExtracted(APackage) and
                  FileExists(LocalRepositoryPackages + APackage.FPackageRelativeFilePath + FileName);
        if Result then
        begin
          APackage.InstalledFileName := LocalRepositoryPackages + APackage.FPackageRelativeFilePath + APackage.PackageFileName;
          APackage.InstalledFileVersion := APackage.VersionAsString;
          Result := True;
        end
      end;
    ptDesignTime, ptRunAndDesignTime:
      begin
        PackageCnt := PackageEditingInterface.GetPackageCount;
        for I := 0 to PackageCnt - 1 do
        begin
          Package := PackageEditingInterface.GetPackages(I);
          if ExtractFileName(Package.FileName) = APackage.PackageFileName then
          begin
            APackage.InstalledFileName := Package.Filename;
            APackage.InstalledFileVersion := IntToStr(Package.Version.Major) + '.' +
                                             IntToStr(Package.Version.Minor) + '.' +
                                             IntToStr(Package.Version.Build) + '.' +
                                             IntToStr(Package.Version.Release);
            Result := True;
            Break;
          end;
        end;
      end;
  end;
end;

procedure TSerializablePackages.GetPackageStates;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Items[I].PackageState := psRepository;
    if IsPackageDownloaded(Items[I]) then
    begin
      Items[I].PackageStates := Items[I].PackageStates + [psDownloaded];
      Items[I].PackageState := psDownloaded;
    end
    else
      Items[I].PackageStates := Items[I].PackageStates - [psDownloaded];

    if IsPackageExtracted(Items[I]) then
    begin
      Items[I].PackageStates := Items[I].PackageStates + [psExtracted];
      Items[I].PackageState := psExtracted;
    end
    else
      Items[I].PackageStates := Items[I].PackageStates - [psExtracted];

    if IsPackageInstalled(Items[I]) then
    begin
      Items[I].PackageStates := Items[I].PackageStates + [psInstalled];
      Items[I].PackageState := psInstalled;
    end
    else
      Items[I].PackageStates := Items[I].PackageStates - [psInstalled];
  end;
end;

procedure TSerializablePackages.RemoveErrorState;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
   if psError in Items[I].PackageStates then
     Items[I].PackageStates := Items[I].PackageStates - [psError];
end;

procedure TSerializablePackages.MarkRuntimePackages;
var
  I: Integer;
  FileName: String;
begin
  for I := 0 to Count - 1 do
  begin
    if (Items[I].Checked) and
       (psInstalled in Items[I].PackageStates) and
         (not (psError in Items[I].PackageStates)) and
           (Items[I].PackageType in [ptRunTime, ptRunTimeOnly]) then
    begin
      FileName := StringReplace(Items[I].PackageFileName, '.lpk', '.opkman', [rfIgnoreCase]);
      FileCreate(LocalRepositoryPackages + Items[I].FPackageRelativeFilePath + FileName);
    end;
  end;
end;

function TSerializablePackages.Cleanup: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
  begin
    if not IsPackageInstalled(Items[I]) then
    begin
      if IsPackageDownloaded(Items[I]) then
      begin
        if DeleteFile(LocalRepositoryArchive + Items[I].RepositoryFileName) then
          Inc(Result);
      end;
      if IsPackageExtracted(Items[I]) then
        DeleteDirectory(LocalRepositoryPackages + Items[I].PackageBaseDir, False);
    end;
  end;
end;


end.
