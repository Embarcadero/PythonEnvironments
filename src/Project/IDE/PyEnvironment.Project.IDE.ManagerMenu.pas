unit PyEnvironment.Project.IDE.ManagerMenu;

interface

uses
  System.Classes, System.SysUtils,
  ToolsAPI,
  PyEnvironment.Project.IDE.Deploy,
  PyEnvironment.Project.IDE.Types;

type
  TPyEnvironmentProjectManagerMenu = class(TNotifierObject, IOTALocalMenu, IOTAProjectManagerMenu)
  strict private
    FCaption: string;
    FExecuteProc: TProc;
    FName: string;
    FParent: string;
    FPosition: Integer;
    FVerb: string;
    FChecked: boolean;
  strict protected
    { IOTALocalMenu }
    function GetCaption: string;
    function GetChecked: Boolean; virtual;
    function GetEnabled: Boolean; virtual;
    function GetHelpContext: Integer;
    function GetName: string;
    function GetParent: string;
    function GetPosition: Integer;
    function GetVerb: string;
    procedure SetCaption(const AValue: string);
    procedure SetChecked(AValue: Boolean);
    procedure SetEnabled(AValue: Boolean);
    procedure SetHelpContext(AValue: Integer);
    procedure SetName(const AValue: string);
    procedure SetParent(const AValue: string);
    procedure SetPosition(AValue: Integer);
    procedure SetVerb(const AValue: string);
    { IOTAProjectManagerMenu }
    function GetIsMultiSelectable: Boolean;
    procedure Execute(const AMenuContextList: IInterfaceList); overload;
    function PostExecute(const AMenuContextList: IInterfaceList): Boolean;
    function PreExecute(const AMenuContextList: IInterfaceList): Boolean;
    procedure SetIsMultiSelectable(AValue: Boolean);
  public
    constructor Create(const ACaption, AVerb: string; const APosition: Integer;
      const AExecuteProc: TProc = nil; const AName: string = '';
      const AParent: string = ''; const AChecked: boolean = false);
  end;

  TPyEnvironmentProjectManagerMenuSeparator = class(TPyEnvironmentProjectManagerMenu)
  public
    constructor Create(const APosition: Integer); reintroduce;
  end;

  TPyEnvironmentProjectManagerMenuEnablePythonEnvironment = class(TPyEnvironmentProjectManagerMenu)
  strict private
    FIsPyEnvironmentEnabled: boolean;
  strict protected
    function GetEnabled: Boolean; override;
  public const
    MENU_CAPTIONS: array[Boolean] of string = ('Enable Python', 'Disable Python');
  public
    constructor Create(const AProject: IOTAProject; const APosition: Integer); reintroduce;
  end;

  TPyEnvironmentProjectManagerMenuPythonEnvironmentVersion = class(TPyEnvironmentProjectManagerMenu)
  strict private const
    MENU_VERB = 'PythonEnvironmentVersion';
    MENU_CAPTION = 'Python Version';
  strict private
    FIsPyEnvironmentEnabled: boolean;
  strict protected
    function GetEnabled: Boolean; override;
    procedure AddSubItems(const AProject: IOTAProject; const AList: IInterfaceList);
  public
    constructor Create(const ASubMenuList: IInterfaceList; const AProject: IOTAProject; const APosition: Integer); reintroduce;
  end;

  TPyEnvironmentProjectManagerMenuPythonEnvironmentVersionSubItem = class(TPyEnvironmentProjectManagerMenu)
  strict private
    FProject: IOTAProject;
    FParent: IOTALocalMenu;
    FPythonVersion: string;
    procedure SetDeployFiles(const AProject: IOTAProject;
      const AConfig: TPyEnvironmentProjectConfig;
      const APlatform: TPyEnvironmentProjectPlatform;
      const AEnabled: Boolean);
    procedure SetPythonVersion(const AProject: IOTAProject;
      const AEnabled: Boolean);
  strict protected
    function GetEnabled: Boolean; override;
    function GetChecked: boolean; override;
  public
    constructor Create(const AProject: IOTAProject; const APosition: Integer;
      const AParent: IOTALocalMenu;
      const APythonVersion: string); reintroduce;
  end;

implementation

uses
  System.IOUtils,
  PyEnvironment.Project,
  PyEnvironment.Project.IDE.Helper;

{ TPyEnvironmentProjectManagerMenu }

constructor TPyEnvironmentProjectManagerMenu.Create(const ACaption, AVerb: string;
  const APosition: Integer; const AExecuteProc: TProc = nil;
  const AName: string = ''; const AParent: string = ''; const AChecked: boolean = false);
begin
  inherited Create;
  FCaption := ACaption;
  FName := AName;
  FParent := AParent;
  FPosition := APosition;
  FVerb := AVerb;
  FExecuteProc := AExecuteProc;
  FChecked := AChecked;
end;

procedure TPyEnvironmentProjectManagerMenu.Execute(const AMenuContextList: IInterfaceList);
begin
  if Assigned(FExecuteProc) then
    FExecuteProc;
end;

function TPyEnvironmentProjectManagerMenu.GetCaption: string;
begin
  Result := FCaption;
end;

function TPyEnvironmentProjectManagerMenu.GetChecked: Boolean;
begin
  Result := FChecked;
end;

function TPyEnvironmentProjectManagerMenu.GetEnabled: Boolean;
begin
  Result := True;
end;

function TPyEnvironmentProjectManagerMenu.GetHelpContext: Integer;
begin
  Result := 0;
end;

function TPyEnvironmentProjectManagerMenu.GetIsMultiSelectable: Boolean;
begin
  Result := False;
end;

function TPyEnvironmentProjectManagerMenu.GetName: string;
begin
  Result := FName;
end;

function TPyEnvironmentProjectManagerMenu.GetParent: string;
begin
  Result := FParent;
end;

function TPyEnvironmentProjectManagerMenu.GetPosition: Integer;
begin
  Result := FPosition;
end;

function TPyEnvironmentProjectManagerMenu.GetVerb: string;
begin
  Result := FVerb;
end;

function TPyEnvironmentProjectManagerMenu.PostExecute(const AMenuContextList: IInterfaceList): Boolean;
begin
  Result := False;
end;

function TPyEnvironmentProjectManagerMenu.PreExecute(const AMenuContextList: IInterfaceList): Boolean;
begin
  Result := False;
end;

procedure TPyEnvironmentProjectManagerMenu.SetCaption(const AValue: string);
begin
end;

procedure TPyEnvironmentProjectManagerMenu.SetChecked(AValue: Boolean);
begin
  FChecked := AValue;
end;

procedure TPyEnvironmentProjectManagerMenu.SetEnabled(AValue: Boolean);
begin
end;

procedure TPyEnvironmentProjectManagerMenu.SetHelpContext(AValue: Integer);
begin
end;

procedure TPyEnvironmentProjectManagerMenu.SetIsMultiSelectable(AValue: Boolean);
begin
end;

procedure TPyEnvironmentProjectManagerMenu.SetName(const AValue: string);
begin
  FName := AValue;
end;

procedure TPyEnvironmentProjectManagerMenu.SetParent(const AValue: string);
begin
  FParent := AValue;
end;

procedure TPyEnvironmentProjectManagerMenu.SetPosition(AValue: Integer);
begin
  FPosition := AValue;
end;

procedure TPyEnvironmentProjectManagerMenu.SetVerb(const AValue: string);
begin
end;

{ TPyEnvironmentProjectManagerMenuSeparator }

constructor TPyEnvironmentProjectManagerMenuSeparator.Create(const APosition: Integer);
begin
  inherited Create('-', '', APosition);
end;

{ TPyEnvironmentProjectManagerMenuEnablePythonEnvironment }

constructor TPyEnvironmentProjectManagerMenuEnablePythonEnvironment.Create(
  const AProject: IOTAProject; const APosition: Integer);
begin
  FIsPyEnvironmentEnabled := TPyEnvironmentProjectHelper.IsPyEnvironmentDefined[AProject];
  inherited Create(MENU_CAPTIONS[FIsPyEnvironmentEnabled], String.Empty, APosition,
    procedure() begin
      FIsPyEnvironmentEnabled := not FIsPyEnvironmentEnabled;
      if not FIsPyEnvironmentEnabled then begin
        TPyEnvironmentProjectHelper.RemoveDeployFilesOfClass(AProject);
        TPyEnvironmentProjectHelper.CurrentPythonVersion[AProject] := String.Empty;
      end;
      TPyEnvironmentProjectHelper.IsPyEnvironmentDefined[AProject] := FIsPyEnvironmentEnabled;
    end);
end;

function TPyEnvironmentProjectManagerMenuEnablePythonEnvironment.GetEnabled: Boolean;
begin
  Result := FIsPyEnvironmentEnabled or TPyEnvironmentProjectDeploy.Found;
end;

{ TPyEnvironmentProjectManagerMenuPythonEnvironmentVersion }

procedure TPyEnvironmentProjectManagerMenuPythonEnvironmentVersion.AddSubItems(
  const AProject: IOTAProject; const AList: IInterfaceList);
var
  LPythonVersion: string;
  LPosition: Integer;
begin
  LPosition := 0;
  for LPythonVersion in TPyEnvironmentProjectDeploy.PYTHON_VERSIONS do begin
    AList.Add(
      TPyEnvironmentProjectManagerMenuPythonEnvironmentVersionSubItem.Create(
        AProject, GetPosition() + LPosition, Self, LPythonVersion
      ));
    Inc(LPosition);
  end;
end;

constructor TPyEnvironmentProjectManagerMenuPythonEnvironmentVersion.Create(
  const ASubMenuList: IInterfaceList; const AProject: IOTAProject;
  const APosition: Integer);
begin
  FIsPyEnvironmentEnabled := TPyEnvironmentProjectHelper.IsPyEnvironmentDefined[AProject];
  inherited Create(MENU_CAPTION, MENU_VERB, APosition);
  AddSubItems(AProject, ASubMenuList);
end;

function TPyEnvironmentProjectManagerMenuPythonEnvironmentVersion.GetEnabled: Boolean;
begin
  Result := FIsPyEnvironmentEnabled;
end;

{ TPyEnvironmentProjectManagerMenuPythonEnvironmentVersionSubItem }

constructor TPyEnvironmentProjectManagerMenuPythonEnvironmentVersionSubItem.Create(
  const AProject: IOTAProject; const APosition: Integer;
  const AParent: IOTALocalMenu; const APythonVersion: string);
begin
  FProject := AProject;
  FParent := AParent;
  FPythonVersion := APythonVersion;
  inherited Create(APythonVersion, String.Empty, APosition,
    procedure() begin
      if TPyEnvironmentProjectHelper.CurrentPythonVersion[FProject] <> FPythonVersion then begin
        //Remove old version files
        SetPythonVersion(FProject, false);
        TPyEnvironmentProjectHelper.CurrentPythonVersion[FProject] := FPythonVersion;
        //Add new version files
        SetPythonVersion(FProject, true);
      end;
    end, String.Empty, AParent.GetVerb());
end;

function TPyEnvironmentProjectManagerMenuPythonEnvironmentVersionSubItem.GetChecked: boolean;
begin
  Result := TPyEnvironmentProjectHelper.CurrentPythonVersion[FProject] = FPythonVersion;
end;

function TPyEnvironmentProjectManagerMenuPythonEnvironmentVersionSubItem.GetEnabled: Boolean;
begin
  Result := FParent.GetEnabled();
end;

procedure TPyEnvironmentProjectManagerMenuPythonEnvironmentVersionSubItem.SetDeployFiles(
  const AProject: IOTAProject; const AConfig: TPyEnvironmentProjectConfig;
  const APlatform: TPyEnvironmentProjectPlatform; const AEnabled: Boolean);
var
  LDeployFile: TPyEnvironmentDeployFile;
  LPythonVersion: string;
begin
  if TPyEnvironmentProjectHelper.SupportsEnvironmentDeployment(AProject) then
  begin
    LPythonVersion := TPyEnvironmentProjectHelper.CurrentPythonVersion[AProject];
    if AEnabled and (APlatform in TPyEnvironmentProjectDeploy.SUPPORTED_PLATFORMS) then begin
      for LDeployFile in TPyEnvironmentProjectDeploy.GetDeployFiles(LPythonVersion, APlatform) do
        TPyEnvironmentProjectHelper.AddDeployFile(AProject, AConfig, LDeployFile);
    end else begin
      for LDeployFile in TPyEnvironmentProjectDeploy.GetDeployFiles(LPythonVersion, APlatform) do begin
        TPyEnvironmentProjectHelper.RemoveDeployFile(
          AProject, AConfig, APlatform, LDeployFile.LocalFileName, LDeployFile.RemotePath);
        if LDeployFile.CopyToOutput then
          TPyEnvironmentOTAHelper.TryRemoveOutputFile(
            AProject, APlatform, AConfig, TPath.GetFileName(LDeployFile.LocalFileName));
      end;
    end;
  end;
end;

procedure TPyEnvironmentProjectManagerMenuPythonEnvironmentVersionSubItem.SetPythonVersion(
  const AProject: IOTAProject; const AEnabled: Boolean);

  function SupportsPlatform(const APlatform: TPyEnvironmentProjectPlatform): Boolean;
  var
    LPlatformName: string;
    LSupportedPlatform: string;
  begin
    if APlatform <> TPyEnvironmentProjectPlatform.Unknown then begin
      LPlatformName := APlatform.ToString;
      for LSupportedPlatform in AProject.SupportedPlatforms do
        if SameText(LPlatformName, LSupportedPlatform) then
          Exit(True);
    end;
    Result := False;
  end;

var
  LPlatform: TPyEnvironmentProjectPlatform;
  LConfig: TPyEnvironmentProjectConfig;
  LProjectOptions: IOTAProjectOptions;
begin
  for LPlatform := Low(TPyEnvironmentProjectPlatform) to High(TPyEnvironmentProjectPlatform) do
    if SupportsPlatform(LPlatform) then
      for LConfig := Low(TPyEnvironmentProjectConfig) to High(TPyEnvironmentProjectConfig) do
        SetDeployFiles(AProject, LConfig, LPlatform, AEnabled);

  // Remove remaing files from old versions
  if not AEnabled then
    TPyEnvironmentProjectHelper.RemoveDeployFilesOfClass(AProject);

  LProjectOptions := AProject.ProjectOptions;
  if Assigned(LProjectOptions) then
    LProjectOptions.ModifiedState := True;
end;

end.
