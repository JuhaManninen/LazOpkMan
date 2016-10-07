unit opkman_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_XMLCfg;

type
  { TPackageOptions }

  TPackageOptions = class
   private
     FXML: TXMLConfig;
     FRemoteRepository: String;
     FChanged: Boolean;
     procedure LoadDefault;
   public
     constructor Create(const AFileName: String);
     destructor Destroy; override;
   public
     procedure Save;
     procedure Load;
   published
     property Changed: Boolean read FChanged write FChanged;
     property RemoteRepository: string read FRemoteRepository write FRemoteRepository;
  end;

var
  PackageOptions: TPackageOptions = nil;

implementation

{ TPackageOptions }

constructor TPackageOptions.Create(const AFileName: String);
begin
  FXML := TXMLConfig.Create(AFileName);
  if FileExists(AFileName) then
    Load
  else
    LoadDefault;
  end;

destructor TPackageOptions.Destroy;
begin
  if FChanged then
    Save;
  FXML.Free;
  inherited Destroy;
end;

procedure TPackageOptions.Load;
begin
  FRemoteRepository := FXML.GetValue('RemoteRepository/Value', FRemoteRepository);
end;

procedure TPackageOptions.Save;
begin
  FXML.SetDeleteValue('RemoteRepository/Value', FRemoteRepository, '');
  FXML.Flush;
end;

procedure TPackageOptions.LoadDefault;
begin
  FRemoteRepository := 'http://104.199.177.61/';
  Save;
end;

end.

