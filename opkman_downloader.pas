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
   Implementation of the package downloader class.
}
unit opkman_downloader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, opkman_httpclient, opkman_timer, opkman_common;

type
  TDownloadType = (dtJSON, dtPackage);
  TErrorType = (etNone, etConfig, etTimeOut, etHTTPClient);
  TOnTimer = TNotifyEvent;
  TOnJSONProgress = TNotifyEvent;
  TOnJSONDownloadCompleted = procedure(Sender: TObject; AJSON: TJSONStringType; AErrTyp: TErrorType; const AErrMsg: String = '') of object;
  TOnWriteStream = procedure(Sender: TObject; APos: Int64) of object;
  TOnPackageDownloadProgress = procedure(Sender: TObject; AFrom, ATo: String; ACnt, ATotCnt: Integer; ACurPos, ACurSize, ATotPos, ATotSize: Int64;
    AEllapsed, ARemaining, ASpeed: LongInt) of object;
  TOnPackageDownloadError = procedure(Sender: TObject; APackageName: String; const AErrMsg: String = '') of object;
  TOnPackageDownloadCompleted = TNotifyEvent;

  { TDownloadStream }

   TDownloadStream = class(TStream)
   private
     FOnWriteStream: TOnWriteStream;
     FStream: TStream;
   public
     constructor Create(AStream: TStream);
     destructor Destroy; override;
     function Read(var Buffer; Count: LongInt): LongInt; override;
     function Write(const Buffer; Count: LongInt): LongInt; override;
     function Seek(Offset: LongInt; Origin: Word): LongInt; override;
     procedure DoProgress;
   published
     property OnWriteStream: TOnWriteStream read FOnWriteStream write FOnWriteStream;
   end;

  { TThreadDownload }

  TThreadDownload = class(TThread)
  private
    FHTTPClient: TFPHTTPClient;
    FOnJSONComplete: TOnJSONDownloadCompleted;
    FOnJSONProgress: TNotifyEvent;
    FRemoteJSONFile: String;
    FErrMsg: String;
    FDownloadType: TDownloadType;
    FErrTyp: TErrorType;
    FMS: TMemoryStream;
    FFrom: String;
    FTo: String;
    FCnt: Integer;
    FTotCnt: Integer;
    FCurPos: Int64;
    FCurSize: Int64;
    FTotPos: Int64;
    FTotPosTmp: Int64;
    FTotSize: Int64;
    FEllapsed: Integer;
    FRemaining: Integer;
    FSpeed: Integer;
    FTimer: TThreadTimer;
    FNeedToBreak: Boolean;
    FDownloadTo: String;
    FOnPackageDownloadProgress: TOnPackageDownloadProgress;
    FOnPackageDownloadError: TOnPackageDownloadError;
    FOnPackageDownloadCompleted: TOnPackageDownloadCompleted;
    procedure DoOnTimer(Sender: TObject);
    procedure DoOnJSONProgress;
    procedure DoOnJSONDownloadCompleted;
    procedure DoOnWriteStream(Sender: TObject; APos: Int64);
    procedure DoOnPackageDownloadProgress;
    procedure DoOnPackageDownloadError;
    procedure DoOnpackageDownloadCompleted;
  protected
     procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DownloadJSON(const ATimeOut: Integer = -1);
    procedure DownloadPackages(const ADownloadTo: String);
   published
     property OnJSONProgress: TNotifyEvent read FOnJSONProgress write FOnJSONProgress;
     property OnJSONDownloadCompleted: TOnJSONDownloadCompleted read FOnJSONComplete write FOnJSONComplete;
     property OnPackageDownloadCompleted: TOnPackageDownloadCompleted read FOnPackageDownloadCompleted write FOnPackageDownloadCompleted;
     property OnPackageDownloadError: TOnPackageDownloadError read FOnPackageDownloadError write FOnPackageDownloadError;
     property OnPackageDownloadProgress: TOnPackageDownloadProgress read FOnPackageDownloadProgress write FOnPackageDownloadProgress;
     property NeedToBreak: Boolean read FNeedToBreak write FNeedToBreak;
  end;

  { TPackageDownloader }

  TPackageDownloader = class
  private
    FJSON: TJSONStringType;
    FDownload: TThreadDownload;
    FRemoteRepository: String;
    FLastError: String;
    FOnJSONProgress: TNotifyEvent;
    FOnJSONDownloadCompleted: TOnJSONDownloadCompleted;
    FOnPackageDownloadProgress: TOnPackageDownloadProgress;
    FOnPackageDownloadError: TOnPackageDownloadError;
    FOnPackageDownloadCompleted: TOnPackageDownloadCompleted;
    procedure DoOnJSONProgress(Sender: TObject);
    procedure DoOnJSONDownloadCompleted(Sender: TObject; AJSON: TJSONStringType; AErrTyp: TErrorType; const AErrMsg: String = '');
    procedure DoOnPackageDownloadProgress(Sender: TObject; AFrom, ATo: String; ACnt, ATotCnt: Integer;
      ACurPos, ACurSize, ATotPos, ATotSize: Int64; AEllapsed, ARemaining, ASpeed: LongInt);
    procedure DoOnPackageDownloadError(Sender: TObject; APackageName: String; const AErrMsg: String = '');
    procedure DoOnPackageDownloadCompleted(Sender: TObject);

  public
    constructor Create(const ARemoteRepository: String);
    destructor Destroy; override;
    procedure DownloadJSON(const ATimeOut: Integer = -1);
    procedure DownloadPackages(const ADownloadTo: String);
    procedure CancelDownloadPackages;
  published
    property RemoteRepository: String read FRemoteRepository write FRemoteRepository;
    property LastError: String read FLastError write FLastError;
    property JSON: TJSONStringType read FJSON;
    property OnJSONProgress: TNotifyEvent read FOnJSONProgress write FOnJSONProgress;
    property OnJSONDownloadCompleted: TOnJSONDownloadCompleted read FOnJSONDownloadCompleted write FOnJSONDownloadCompleted;
    property OnPackageDownloadProgress: TOnPackageDownloadProgress read FOnPackageDownloadProgress write FOnPackageDownloadProgress;
    property OnPackageDownloadError: TOnPackageDownloadError read FOnPackageDownloadError write FOnPackageDownloadError;
    property OnPackageDownloadCompleted: TOnPackageDownloadCompleted read FOnPackageDownloadCompleted write FOnPackageDownloadCompleted;
  end;

var
  PackageDownloader: TPackageDownloader = nil;

implementation

uses opkman_const, opkman_serializablepackages, opkman_options;

{ TDownloadStream }
constructor TDownloadStream.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  FStream.Position := 0;
end;

destructor TDownloadStream.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TDownloadStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result := FStream.Read(Buffer, Count);
end;

function TDownloadStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  Result := FStream.Write(Buffer, Count);
  DoProgress;
end;

function TDownloadStream.Seek(Offset: LongInt; Origin: Word): LongInt;
begin
  Result := FStream.Seek(Offset, Origin);
end;

procedure TDownloadStream.DoProgress;
begin
  if Assigned(FOnWriteStream) then
    FOnWriteStream(Self, Self.Position);
end;

{ TThreadDownload }

procedure TThreadDownload.DoOnPackageDownloadProgress;
begin
  if Assigned(FOnPackageDownloadCompleted) then
    FOnPackageDownloadProgress(Self, FFrom, FTo, FCnt, FTotCnt, FCurPos, FCurSize, FTotPosTmp, FTotSize, FEllapsed, FRemaining, FSpeed);
end;

procedure TThreadDownload.DoOnPackageDownloadError;
begin
  if Assigned(FOnPackageDownloadError) then
    FOnPackageDownloadError(Self, ExtractFileName(FTo), FErrMsg);
end;

procedure TThreadDownload.DoOnpackageDownloadCompleted;
begin
  if Assigned(FOnPackageDownloadCompleted) then
    FOnPackageDownloadCompleted(Self);
end;

procedure TThreadDownload.DoOnJSONDownloadCompleted;
var
  JSON: TJSONStringType;
begin
  if Assigned(FOnJSONComplete) then
  begin
    if (FErrTyp = etNone) or (FMS.Size > 0) then
    begin
      SetLength(JSON, FMS.Size);
      FMS.Read(Pointer(JSON)^, Length(JSON));
      FOnJSONComplete(Self, JSON, FErrTyp, '');
    end
    else
      FOnJSONComplete(Self, '', FErrTyp, FErrMsg);
  end;
end;

procedure TThreadDownload.DoOnTimer(Sender: TObject);
begin
  if FDownloadType = dtJSON then
  begin
    FHTTPClient.NeedToBreak := True;
    FErrMsg := rsMessageError2;
    FErrTyp := etTimeOut;
    FTimer.StopTimer;
    Synchronize(@DoOnJSONDownloadCompleted);
    FOnJSONComplete := nil;
  end
  else if FDownloadType = dtPackage then
  begin
    Inc(FEllapsed);
    FSpeed := Round(FTotPosTmp/FEllapsed);
    FRemaining := Round((FTotSize - FTotPosTmp)/FSpeed);
  end;
end;

procedure TThreadDownload.DoOnJSONProgress;
begin
  if Assigned(FOnJSONProgress) then
    FOnJSONProgress(Self);
end;

procedure TThreadDownload.DoOnWriteStream(Sender: TObject; APos: Int64);
begin
  FCurPos := APos;
  FTotPosTmp := FTotPos + APos;
  Synchronize(@DoOnPackageDownloadProgress);
  Sleep(5);
end;

procedure TThreadDownload.Execute;
var
  I: Integer;
  DS: TDownloadStream;
begin
  FErrMsg := '';
  FErrTyp := etNone;
  if FDownloadType = dtJSON then
  begin
    Synchronize(@DoOnJSONProgress);
    if FRemoteJSONFile <> cRemoteJSONFile then
    begin
      try
        FHTTPClient.Get(FRemoteJSONFile, FMS);
        if FMS.Size > 0 then
          FMS.Position := 0;
      except
        on E: Exception do
        begin
          FErrMsg := E.Message;
          FErrTyp := etHTTPClient;
        end;
      end;
    end
    else
    begin
      FErrTyp := etConfig;
      FErrMsg := rsMessageNoRepository0 + sLineBreak + rsMessageNoRepository1;
    end;
    if FTimer.Enabled then
      FTimer.StopTimer;
    Synchronize(@DoOnJSONDownloadCompleted);
  end
  else
  begin
    FCnt := 0;
    for I := 0 to SerializablePackages.Count - 1 do
    begin
      if NeedToBreak then
        Break;
      if SerializablePackages.Items[I].IsDownloadable then
      begin
        Inc(FCnt);
        FFrom := PackageOptions.RemoteRepository + SerializablePackages.Items[I].RepositoryFileName;
        FTo := FDownloadTo + SerializablePackages.Items[I].RepositoryFileName;
        FCurSize := SerializablePackages.Items[I].RepositoryFileSize;
        DS := TDownloadStream.Create(TFileStream.Create(FTo, fmCreate));
        try
          DS.FOnWriteStream := @DoOnWriteStream;
          try
            FHTTPClient.HTTPMethod('GET', FFrom, DS, [200]);
            SerializablePackages.Items[I].PackageStates := SerializablePackages.Items[I].PackageStates + [psDownloaded];
          except
            on E: Exception do
            begin
              FErrMsg := E.Message;
              FErrTyp := etHTTPClient;
              SerializablePackages.Items[I].PackageStates := SerializablePackages.Items[I].PackageStates - [psDownloaded];
              SerializablePackages.Items[I].PackageStates := SerializablePackages.Items[I].PackageStates + [psError];
              Synchronize(@DoOnPackageDownloadError);
            end;
          end;
        finally
          DS.Free
        end;
        if psError in  SerializablePackages.Items[I].PackageStates then
          DeleteFile(FTo);
        FTotPos := FTotPos + FCurSize;
      end;
    end;
    if (FNeedToBreak) then
      DeleteFile(FTo)
    else
      Synchronize(@DoOnPackageDownloadCompleted);
  end;
end;

constructor TThreadDownload.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FTimer := nil;
  FMS := TMemoryStream.Create;
  FHTTPClient := TFPHTTPClient.Create(nil);
end;

destructor TThreadDownload.Destroy;
begin
  if FTimer.Enabled then
    FTimer.StopTimer;
  FTimer.Terminate;
  FHTTPClient.Free;
  FMS.Free;
  inherited Destroy;
end;

procedure TThreadDownload.DownloadJSON(const ATimeOut: Integer = -1);
begin
  FRemoteJSONFile := PackageOptions.RemoteRepository + cRemoteJSONFile;
  FDownloadType := dtJSON;
  FTimer := TThreadTimer.Create;
  FTimer.Interval := ATimeOut;
  FTimer.OnTimer := @DoOnTimer;
  FTimer.StartTimer;
  Start;
end;

procedure TThreadDownload.DownloadPackages(const ADownloadTo: String);
var
  I: Integer;
begin
  FDownloadTo := ADownloadTo;
  FTotCnt := 0;
  FTotSize := 0;
  for I := 0 to SerializablePackages.Count - 1 do
  begin
    if SerializablePackages.Items[I].IsDownloadable then
    begin
      Inc(FTotCnt);
      FTotSize := FTotSize + SerializablePackages.Items[I].RepositoryFileSize;
    end;
  end;
  FDownloadType := dtPackage;
  FTimer := TThreadTimer.Create;
  FTimer.OnTimer := @DoOnTimer;
  FTimer.StartTimer;
  Start;
end;

{ TPackageDownloader}

procedure TPackageDownloader.DoOnPackageDownloadProgress(Sender: TObject; AFrom, ATo: String;
  ACnt, ATotCnt: Integer; ACurPos, ACurSize, ATotPos, ATotSize: Int64;
  AEllapsed, ARemaining, ASpeed: LongInt);
begin
  if Assigned(FOnPackageDownloadProgress) then
    FOnPackageDownloadProgress(Self, AFrom, ATo, ACnt, ATotCnt, ACurPos, ACurSize, ATotPos, ATotSize, AEllapsed, ARemaining, ASpeed);
end;

procedure TPackageDownloader.DoOnPackageDownloadError(Sender: TObject;
  APackageName: String; const AErrMsg: String);
begin
  if Assigned(FOnPackageDownloadError) then
    FOnPackageDownloadError(Self, APackageName, AErrMsg);
end;

procedure TPackageDownloader.DoOnPackageDownloadCompleted(Sender: TObject);
begin
  if Assigned(FOnPackageDownloadCompleted) then
    FOnPackageDownloadCompleted(Sender);
end;

procedure TPackageDownloader.DoOnJSONProgress(Sender: TObject);
begin
  if Assigned(FOnJSONProgress) then
    FOnJSONProgress(Self);
end;

procedure TPackageDownloader.DoOnJSONDownloadCompleted(Sender: TObject;
  AJSON: TJSONStringType; AErrTyp: TErrorType; const AErrMsg: String);
begin
  FJSON := AJSON;
  if Assigned(FOnJSONDownloadCompleted) then
    FOnJSONDownloadCompleted(Self, AJSON, AErrTyp, AErrMsg);
end;

constructor TPackageDownloader.Create(const ARemoteRepository: String);
begin
  FRemoteRepository := ARemoteRepository;
  if (Length(FRemoteRepository) > 0) and (not IsPathDelimiter(FRemoteRepository, Length(FRemoteRepository))) then
    FRemoteRepository := FRemoteRepository + '/';
end;

destructor TPackageDownloader.Destroy;
begin
  inherited Destroy;
end;

procedure TPackageDownloader.DownloadJSON(const ATimeOut: Integer = -1);
begin
  FDownload := TThreadDownload.Create;
  FDownload.OnJSONProgress := @DoOnJSONProgress;
  FDownload.OnJSONDownloadCompleted := @DoOnJSONDownloadCompleted;
  FDownload.DownloadJSON(ATimeOut);
end;

procedure TPackageDownloader.DownloadPackages(const ADownloadTo: String);
begin
  FDownload := TThreadDownload.Create;
  FDownload.OnPackageDownloadProgress := @DoOnPackageDownloadProgress;
  FDownload.OnPackageDownloadError := @DoOnPackageDownloadError;
  FDownload.OnPackageDownloadCompleted := @DoOnPackageDownloadCompleted;
  FDownload.DownloadPackages(ADownloadTo);
end;

procedure TPackageDownloader.CancelDownloadPackages;
begin
  if Assigned(FDownload) then
  begin
    FDownload.FHTTPClient.NeedToBreak := True;
    FDownload.FTimer.StopTimer;
    FDownload.NeedToBreak := True;
  end;
end;

end.

