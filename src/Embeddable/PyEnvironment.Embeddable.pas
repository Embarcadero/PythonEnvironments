(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Embeddable'                               *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:                    https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(*  Functionality:  PyEnvironment Embeddable                              *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free for his own tasks *)
(* and projects, as long as this header and its copyright text is intact. *)
(* For changed versions of this code, which are public distributed the    *)
(* following additional conditions have to be fullfilled:                 *)
(* 1) The header has to contain a comment on the change and the author of *)
(*    it.                                                                 *)
(* 2) A copy of the changed source has to be sent to the above E-Mail     *)
(*    address or my then valid address, if this is possible to the        *)
(*    author.                                                             *)
(* The second condition has the target to maintain an up to date central  *)
(* version of the component. If this condition is not acceptable for      *)
(* confidential or legal reasons, everyone is free to derive a component  *)
(* or to generate a diff file to my or other original sources.            *)
(**************************************************************************)
unit PyEnvironment.Embeddable;

interface

uses
  System.Classes, System.SysUtils, System.Zip,
  PyEnvironment, PyEnvironment.Distribution, PythonEngine;

type
  (*-----------------------------------------------------------------------*)
  (*                                                                       *)
  (*                      Embeddables structure example                    *)
  (*                                                                       *)
  (* [Root] Directory                                                      *)
  (*  +-- python version/                                                  *)
  (*       +-- python zip                                                  *)
  (*-----------------------------------------------------------------------*)

  TPyCustomEmbeddableDistribution = class;
  TZipProgress = procedure(Sender: TObject;
    ADistribution: TPyCustomEmbeddableDistribution; FileName: string;
    Header: TZipHeader; Position: Int64) of object;

  TPyCustomEmbeddableDistribution = class(TPyDistribution)
  private
    FEmbeddablePackage: string;
    FEnvironmentPath: string;
    FOnZipProgress: TZipProgress;
    function FindSharedLibrary(): string;
    function FindExecutable(): string;
  private
    procedure DoZipProgressEvt(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);
    {$IFDEF POSIX}
    function FileIsExecutable(const AFilePath: string): boolean;
    {$ENDIF POSIX}
  protected
    function EnvironmentExists(): boolean;
    /// <summary>
    ///   Navigates through the embeddables searching for a compatible distribution.
    /// </summary>
    function EmbeddableExists(): boolean;
    /// <summary>
    ///   Creates a new environment based on the current settings.
    ///   An embeddable distribution will be used as an "image".
    /// </summary>
    procedure CreateEnvironment(); virtual;
    procedure LoadSettings(); virtual;
  protected
    function GetEnvironmentPath(): string;
  public
    procedure Setup(); override;
  public
    property EmbeddablePackage: string read FEmbeddablePackage write FEmbeddablePackage;
  published
    property EnvironmentPath: string read FEnvironmentPath write FEnvironmentPath;
    property OnZipProgress: TZipProgress read FOnZipProgress write FOnZipProgress;
  end;

  TPyEmbeddableDistribution = class(TPyCustomEmbeddableDistribution)
  private
    FScanned: boolean;
  protected
    procedure LoadSettings(); override;
  public
    property Scanned: boolean read FScanned write FScanned;
  published
    property EmbeddablePackage;
  end;

  TPyEmbeddableCustomCollection = class(TPyDistributionCollection);

  TPyEmbeddableCollection = class(TPyEmbeddableCustomCollection);

  TPyCustomEmbeddedEnvironment = class(TPyEnvironment)
  private
    FOnZipProgress: TZipProgress;
  published
    property OnZipProgress: TZipProgress read FOnZipProgress write FOnZipProgress;
  end;

  [ComponentPlatforms(pidAllPlatforms)]
  TPyEmbeddedEnvironment = class(TPyCustomEmbeddedEnvironment)
  private type
    TScanner = class(TPersistent)
    private
      FAutoScan: boolean;
      FEmbeddablesPath: string;
      FEnvironmentPath: string;
    public
      procedure Scan(ACallback: TProc<TPythonVersionProp, string>);
    published
      property AutoScan: boolean read FAutoScan write FAutoScan default false;
      property EmbeddablesPath: string read FEmbeddablesPath write FEmbeddablesPath;
      /// <summary>
      ///   Default environment path.
      /// </summary>
      property EnvironmentPath: string read FEnvironmentPath write FEnvironmentPath;
    end;
  private
    FScanner: TScanner;
    procedure SetScanner(const Value: TScanner);
  protected
    function CreateCollection(): TPyDistributionCollection; override;
    procedure Prepare(); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  published
    property Distributions;
    property Scanner: TScanner read FScanner write SetScanner;
  end;

  EEmbeddableNotFound = class(Exception);

implementation

uses
  System.IOUtils, System.Character, System.StrUtils,
  PyEnvironment.Path,
  PyTools.ExecCmd,
  PyEnvironment.Notification
  {$IFDEF POSIX}
  , Posix.SysStat, Posix.Stdlib, Posix.String_, Posix.Errno
  {$ENDIF}
  ;

{ TPyCustomEmbeddableDistribution }

procedure TPyCustomEmbeddableDistribution.CreateEnvironment;
begin
  //Unzip the embeddable package into the target directory.
  GetNotifier<TPyCustomEnvironment>.NotifyAll(BEFORE_UNZIP_NOTIFICATION, Self);
  TZipFile.ExtractZipFile(FEmbeddablePackage, GetEnvironmentPath(), DoZipProgressEvt);
  GetNotifier<TPyCustomEnvironment>.NotifyAll(AFTER_UNZIP_NOTIFICATION, Self);
end;

procedure TPyCustomEmbeddableDistribution.DoZipProgressEvt(Sender: TObject; FileName: string;
  Header: TZipHeader; Position: Int64);
begin
  if Assigned(FOnZipProgress) then
    if (TThread.Current.ThreadID <> MainThreadID) then
      TThread.Queue(nil, procedure() begin
        FOnZipProgress(Sender, Self, FileName, Header, Position);
      end)
    else
      FOnZipProgress(Sender, Self, FileName, Header, Position);
end;

function TPyCustomEmbeddableDistribution.EmbeddableExists: boolean;
begin
  Result := TFile.Exists(FEmbeddablePackage);
end;

function TPyCustomEmbeddableDistribution.EnvironmentExists: boolean;
begin
  Result := TDirectory.Exists(GetEnvironmentPath());
end;

{$IFDEF POSIX}
function TPyCustomEmbeddableDistribution.FileIsExecutable(
  const AFilePath: string): boolean;
begin
  {$WARN SYMBOL_PLATFORM OFF}
  Result := (TFileAttribute.faOwnerExecute in TFile.GetAttributes(AFilePath))
    or (TFileAttribute.faGroupExecute in TFile.GetAttributes(AFilePath))
    or (TFileAttribute.faOthersExecute in TFile.GetAttributes(AFilePath));
  {$WARN SYMBOL_PLATFORM ON}
end;
{$ENDIF POSIX}

function TPyCustomEmbeddableDistribution.FindExecutable: string;
var
  LFiles: TArray<string>;
  {$IFDEF POSIX}
  LFile: string;
  {$ENDIF POSIX}
begin
  LFiles := [];
  {$IFDEF MSWINDOWS}
  Result := TPath.Combine(GetEnvironmentPath(), 'python.exe');
  if not TFile.Exists(Result) then
    Result := String.Empty;
  {$ELSE}
  Result := TPath.Combine(GetEnvironmentPath(), 'bin');
  LFiles := TDirectory.GetFiles(Result, 'python*', TSearchOption.soTopDirectoryOnly,
    function(const Path: string; const SearchRec: TSearchRec): boolean
    begin
      Result := Char.IsDigit(SearchRec.Name, Length(SearchRec.Name) - 1);
    end);

  {$IFDEF POSIX}
  for LFile in LFiles do begin
    if (TPath.GetFileName(LFile) = 'python' + PythonVersion) and (FileIsExecutable(LFile)) then
      Exit(LFile);
  end;
  {$ENDIF POSIX}

  {$WARN SYMBOL_PLATFORM OFF}
  if Length(LFiles) > 0 then begin
    Result := LFiles[High(LFiles)];
    if (TFileAttribute.faOwnerExecute in TFile.GetAttributes(Result))
      or (TFileAttribute.faGroupExecute in TFile.GetAttributes(Result))
      or (TFileAttribute.faOthersExecute in TFile.GetAttributes(Result)) then //Avoiding symlinks
        Exit;
  {$WARN SYMBOL_PLATFORM ON}

    Result := LFiles[Low(LFiles)];
    if not TFile.Exists(Result) then
      Result := String.Empty;
  end else
    Result := String.Empty;
  {$ENDIF}
end;

function TPyCustomEmbeddableDistribution.FindSharedLibrary: string;
var
  I: integer;
  LLibName: string;
  LPath: string;
  LSearch: string;
  LFiles: TArray<string>;
begin
  for I := Low(PYTHON_KNOWN_VERSIONS) to High(PYTHON_KNOWN_VERSIONS) do
    if PythonVersion.StartsWith(PYTHON_KNOWN_VERSIONS[I].RegVersion) then begin
      LLibName := PYTHON_KNOWN_VERSIONS[I].DllName;
      Break;
    end;

  {$IFDEF MSWINDOWS}
  LPath := GetEnvironmentPath();
  {$ELSE}
  LPath := TPath.Combine(GetEnvironmentPath(), 'lib');
  {$ENDIF}

  Result := TPath.Combine(LPath, LLibName);
  if not TFile.Exists(Result) then begin
    LSearch := LLibName.Replace(TPath.GetExtension(LLibName), '') + '*' + TPath.GetExtension(LLibName);
    LFiles := TDirectory.GetFiles(
      LPath,
      LSearch, //Python <= 3.7 might contain a "m" as a sufix.
      TSearchOption.soTopDirectoryOnly);
    if Length(LFiles) > 0 then begin
      Result := LFiles[Low(LFiles)];
    end else
      Result := String.Empty;
  end;

  {$IFDEF LINUX}
  if TFile.Exists(Result + '.1.0') then //Targets directly to the so file instead of a symlink.
    Result := Result + '.1.0';
  {$ENDIF}
end;

procedure TPyCustomEmbeddableDistribution.LoadSettings;
begin
  Home := TPyEnvironmentPath.ResolvePath(GetEnvironmentPath());
  SharedLibrary := TPyEnvironmentPath.ResolvePath(FindSharedLibrary());
  Executable := TPyEnvironmentPath.ResolvePath(FindExecutable());
end;

function TPyCustomEmbeddableDistribution.GetEnvironmentPath: string;
begin
  Result := TPyEnvironmentPath.ResolvePath(EnvironmentPath);
end;

procedure TPyCustomEmbeddableDistribution.Setup;
begin
  inherited;
  if not EnvironmentExists() then begin
    if not EmbeddableExists() then
      raise EEmbeddableNotFound.CreateFmt(
        'Embeddable package not found.' + #13#10 + '%s', [FEmbeddablePackage]);

    GetNotifier<TPyCustomEnvironment>.NotifyAll(BEFORE_CREATE_ENVIRONMENT_NOTIFICATION, Self);
    CreateEnvironment();
    GetNotifier<TPyCustomEnvironment>.NotifyAll(AFTER_CREATE_ENVIRONMENT_NOTIFICATION, Self);
  end;

  LoadSettings();
end;

{ TPyEmbeddableDistribution }

procedure TPyEmbeddableDistribution.LoadSettings;
begin
  if FScanned then
    inherited;
end;

{ TPyEmbeddedEnvironment }

constructor TPyEmbeddedEnvironment.Create(AOwner: TComponent);
begin
  inherited;
  FScanner := TScanner.Create();
end;

destructor TPyEmbeddedEnvironment.Destroy;
begin
  FScanner.Free();
  inherited;
end;

function TPyEmbeddedEnvironment.CreateCollection: TPyDistributionCollection;
begin
  Result := TPyEmbeddableCollection.Create(Self, TPyEmbeddableDistribution);
end;

procedure TPyEmbeddedEnvironment.Prepare;
var
  LDistribution: TPyEmbeddableDistribution;
begin
  if FScanner.AutoScan then begin
    FScanner.Scan(
      procedure(APyVersionInfo: TPythonVersionProp; AEmbeddablePackage: string) begin
        if Assigned(Distributions.LocateEnvironment(APyVersionInfo.RegVersion)) then
          Exit;

        LDistribution := TPyEmbeddableDistribution(Distributions.Add());
        LDistribution.Scanned := true;
        LDistribution.PythonVersion := APyVersionInfo.RegVersion;
        LDistribution.EnvironmentPath := TPath.Combine(
          TPyEnvironmentPath.ResolvePath(
            FScanner.EnvironmentPath),
          APyVersionInfo.RegVersion);
        LDistribution.EmbeddablePackage := AEmbeddablePackage;
        LDistribution.OnZipProgress := FOnZipProgress;
      end);
  end;
  inherited;
end;

procedure TPyEmbeddedEnvironment.SetScanner(const Value: TScanner);
begin
  FScanner.Assign(Value);
end;

{ TPyEmbeddedEnvironment.TScanner }

procedure TPyEmbeddedEnvironment.TScanner.Scan(
  ACallback: TProc<TPythonVersionProp, string>);
var
  I: Integer;
  LPath: string;
  LFiles: TArray<string>;
begin
  if not Assigned(ACallback) then
    Exit;

  if not TDirectory.Exists(FEmbeddablesPath) then
    raise Exception.Create('Directory not found.');

  for I := Low(PYTHON_KNOWN_VERSIONS) to High(PYTHON_KNOWN_VERSIONS) do begin
    LPath := TPath.Combine(FEmbeddablesPath, PYTHON_KNOWN_VERSIONS[I].RegVersion);
    if not TDirectory.Exists(LPath) then
      Continue;

    LFiles := TDirectory.GetFiles(LPath, '*.zip', TSearchOption.soTopDirectoryOnly);
    if (Length(LFiles) = 0) then
      Continue;

    ACallback(PYTHON_KNOWN_VERSIONS[I], LFiles[0]);
  end;
end;

end.
