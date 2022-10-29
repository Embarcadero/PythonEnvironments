(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment'                                          *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality:  PyEnvironment  layer                                  *)
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
unit PyEnvironment;

interface

uses
  System.Classes,
  System.Rtti,
  System.SysUtils,
  System.Threading,
  System.Generics.Collections,
  System.Types,
  System.SysConst,
  PythonEngine,
  PyTools.Cancelation,
  PyEnvironment.Distribution;

type
  {$SCOPEDENUMS ON}
  TPyPluginEvent = (
    BeforeSetup, AfterSetup,
    BeforeActivate, AfterActivate,
    BeforeDeactivate, AfterDeactivate);
  {$SCOPEDENUMS OFF}
  TPyPluginEvents = set of TPyPluginEvent;

  TPyPluginInfo = record
  public
    Name: string;
    Description: string;

    InstallsWhen: TPyPluginEvents;
    UninstallsWhen: TPyPluginEvents;
    LoadsWhen: TPyPluginEvents;
    UnloadsWhen: TPyPluginEvents;
  end;

  TPyEnvironmentBeforeSetup = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentAfterSetup = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentBeforeActivate = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentAfterActivate = procedure(Sender: TObject; const APythonVersion: string; const AActivated: boolean) of object;
  TPyEnvironmentBeforeDeactivate = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentAfterDeactivate = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentReady = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentError = procedure(Sender: TObject; const AException: Exception) of object;
  //Plugins
  TPyEnvironmentPluginInstall = procedure(const APlugin: TObject; const AInfo: TPyPluginInfo) of object;
  TPyEnvironmentPluginUninstall = procedure(const APlugin: TObject; const AInfo: TPyPluginInfo) of object;
  TPyEnvironmentPluginLoad = procedure(const APlugin: TObject; const AInfo: TPyPluginInfo) of object;
  TPyEnvironmentPluginUnload = procedure(const APlugin: TObject; const AInfo: TPyPluginInfo) of object;

  IPyEnvironmentPlugin = interface
    ['{11906029-81A6-4F94-ACDA-2DBB346C6E3A}']
    function GetInfo(): TPyPluginInfo;
    
    procedure InstallPlugin(const ACancelation: ICancelation);
    procedure UninstallPlugin(const ACancelation: ICancelation);

    procedure LoadPlugin(const ACancelation: ICancelation);
    procedure UnloadPlugin(const ACancelation: ICancelation);

    function IsInstalled(): boolean;
    
    property Info: TPyPluginInfo read GetInfo;
  end;  
  
  TPyCustomEnvironment = class(TComponent)
  protected type
    TEnvironmentTaskAsyncResult = class(TBaseAsyncResult)
    private
      FAsyncTask: TProc<ICancelation>;
      FAsyncCallback: TAsyncCallback;
      FCancelation: ICancelation;
    protected
      procedure Complete; override;
      procedure Schedule; override;
      procedure AsyncDispatch; override;
      function DoCancel: Boolean; override;
    public
      constructor Create(const AContext: TObject;
        const AAsyncTask: TProc<ICancelation>;
        const AAsyncCallback: TAsyncCallback = nil);
    end;

    TAsyncFuncCallback<TResult> = reference to procedure (
      const ASyncResult: IAsyncResult; const AResult: TResult);

    TEnvironmentTaskAsyncResult<TResult> = class(TEnvironmentTaskAsyncResult)
    private
      FRetVal: TResult;
    public
      constructor Create(const AContext: TObject; const AAsyncTask: TFunc<ICancelation, TResult>;
        const AAsyncFuncCallback: TAsyncFuncCallback<TResult> = nil);
      function GetRetVal: TResult;
    end;

    TAsyncSetup = class sealed(TEnvironmentTaskAsyncResult<boolean>);

    TAsyncActivate = class sealed(TEnvironmentTaskAsyncResult<boolean>)
    private
      FSetupResult: IAsyncResult;
    protected
      procedure AsyncDispatch; override;
    public
      constructor Create(const AContext: TObject;
        const ASetupResult: IAsyncResult;
        const AAsyncTask: TFunc<ICancelation, boolean>;
        const AAsyncFuncCallback: TAsyncFuncCallback<boolean> = nil);
    end;
  private
    FDistributions: TPyDistributionCollection;
    FPlugins: TList<IPyEnvironmentPlugin>;
    FAutoLoad: boolean;
    FPythonEngine: TPythonEngine;
    FPythonVersion: string;
    //Async options
    FSynchronizeEvents: boolean;
    //Events
    FBeforeSetup: TPyEnvironmentBeforeSetup;
    FAfterSetup: TPyEnvironmentAfterSetup;
    FBeforeActivate: TPyEnvironmentBeforeActivate;
    FAfterActivate: TPyEnvironmentAfterActivate;
    FBeforeDeactivate: TPyEnvironmentBeforeDeactivate;
    FAfterDeactivate: TPyEnvironmentAfterDeactivate;
    FOnReady: TPyEnvironmentReady;
    FOnError: TPyEnvironmentError;
    //Plugin events
    FOnPluginInstall: TPyEnvironmentPluginInstall;
    FOnPluginUninstall: TPyEnvironmentPluginUninstall;
    FOnPluginLoad: TPyEnvironmentPluginLoad;
    FOnPluginUnload: TPyEnvironmentPluginUnload;
    procedure SetDistributions(const ADistributions: TPyDistributionCollection);
    procedure SetPythonEngine(const APythonEngine: TPythonEngine);
    procedure DoAutoLoad;
    //Hooked routines
    function InternalSetup(const APythonVersion: string;
      const ACancelation: ICancelation): boolean;
    function InternalActivate(const APythonVersion: string;
      const ACancelation: ICancelation): boolean;
    procedure InternalDeactivate(const ACancelation: ICancelation);
    //Event handlers
    procedure DoBeforeSetup(APythonVersion: string; const ACancelation: ICancelation);
    procedure DoAfterSetup(APythonVersion: string; 
      const ADistribution: TPyDistribution; const ACancelation: ICancelation);
    procedure DoBeforeActivate(APythonVersion: string; const ACancelation: ICancelation);
    procedure DoAfterActivate(
      APythonVersion: string; const ADistribution: TPyDistribution; 
      var Result: Boolean; const ACancelation: ICancelation);
    procedure DoBeforeDeactivate(const ACancelation: ICancelation);
    procedure DoAfterDeactivate(const ACancelation: ICancelation);
    procedure DoPluginInstall(const APlugin: TObject; const AInfo: TPyPluginInfo);
    procedure DoPluginUninstall(const APlugin: TObject; const AInfo: TPyPluginInfo);
    procedure DoPluginLoad(const APlugin: TObject; const AInfo: TPyPluginInfo);
    procedure DoPluginUnload(const APlugin: TObject; const AInfo: TPyPluginInfo);
    //It won't be ready until setup, activate, install and load plugins
    procedure DoInternalReady;
    procedure DoInternalError;
  protected
    procedure Loaded(); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure HandleEvent(const ASynchronizeEvents: boolean;
      const AThreadProc: TThreadProcedure);
    //Plugins install/uninstall/load/unload
    procedure InstallAndLoadPlugins(const AEvent: TPyPluginEvent;
      const ACancelation: ICancelation); virtual;
    procedure UninstallAndUnloadPlugins(const AEvent: TPyPluginEvent;
      const ACancelation: ICancelation); virtual;
  protected
    procedure SetPythonVersion(const Value: string); virtual;
    function CreateCollection(): TPyDistributionCollection; virtual; abstract;
    procedure Prepare(const ACancelation: ICancelation); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    /// <summary>
    ///   Setup an environment.
    /// </summary>
    function Setup(APythonVersion: string = ''): boolean;
    /// <summary>
    ///   Activate an environment.
    /// </summary>
    function Activate(APythonVersion: string = ''): boolean;
    /// <summary>
    ///   Deactivate an environment.
    /// </summary>
    procedure Deactivate();

    function SetupAsync(const APythonVersion: string;
      const ACallback: TAsyncFuncCallback<boolean> = nil): IAsyncResult; overload;
    function SetupAsync(
      const ACallback: TAsyncFuncCallback<boolean> = nil): IAsyncResult; overload;
    function ActivateAsync(const APythonVersion: string;
      const ASetupAsync: IAsyncResult;
      const ACallback: TAsyncFuncCallback<boolean> = nil): IAsyncResult; overload;
    function ActivateAsync(const ASetupAsync: IAsyncResult;
      const ACallback: TAsyncFuncCallback<boolean> = nil): IAsyncResult; overload;
    function ActivateAsync(
      const ACallback: TAsyncFuncCallback<boolean> = nil): IAsyncResult; overload;
    /// <summary>
    /// Async begin setup an environment.
    /// </summary>
    function BeginSetup(const APythonVersion: string = ''): IAsyncResult;
    /// <summary>
    /// Wait for the async setup action to complete.
    /// </summary>
    function EndSetup(const ASyncResult: IAsyncResult): boolean;
    /// <summary>
    ///   Queue the activate action to the main thread.
    /// </summary>
    function BeginActivate(const APythonVersion: string = ''): IAsyncResult;
    /// <summary>
    ///   Wait for the activate action to complete.
    ///   Never call this routine directly from main thread.
    /// </summary>
    function EndActivate(const ASyncResult: IAsyncResult): boolean;
    //Plugins
    procedure AddPlugin(const APlugin: IPyEnvironmentPlugin);
    procedure RemovePlugin(const APlugin: IPyEnvironmentPlugin);
  public
    property Distributions: TPyDistributionCollection read FDistributions write SetDistributions;
    property AutoLoad: boolean read FAutoLoad write FAutoLoad;
    property PythonVersion: string read FPythonVersion write SetPythonVersion;
    property PythonEngine: TPythonEngine read FPythonEngine write SetPythonEngine;
    /// <summary>
    ///   Automatically sync events when running async.
    /// </summary>
    property SynchronizeEvents: boolean read FSynchronizeEvents write FSynchronizeEvents default true;
  published
    property BeforeSetup: TPyEnvironmentBeforeSetup read FBeforeSetup write FBeforeSetup;
    property AfterSetup: TPyEnvironmentAfterSetup read FAfterSetup write FAfterSetup;
    property BeforeActivate: TPyEnvironmentBeforeActivate read FBeforeActivate write FBeforeActivate;
    property AfterActivate: TPyEnvironmentAfterActivate read FAfterActivate write FAfterActivate;
    property BeforeDeactivate: TPyEnvironmentBeforeDeactivate read FBeforeDeactivate write FBeforeDeactivate;
    property AfterDeactivate: TPyEnvironmentAfterDeactivate read FAfterDeactivate write FAfterDeactivate;    
    property OnError: TPyEnvironmentError read FOnError write FOnError;
    property OnReady: TPyEnvironmentReady read FOnReady write FOnReady;
    //Plugin events
    property OnPluginInstall: TPyEnvironmentPluginInstall read FOnPluginInstall write FOnPluginInstall;
    property OnPluginUninstall: TPyEnvironmentPluginUninstall read FOnPluginUninstall write FOnPluginUninstall;
    property OnPluginLoad: TPyEnvironmentPluginLoad read FOnPluginLoad write FOnPluginLoad;
    property OnPluginUnload: TPyEnvironmentPluginUnload read FOnPluginUnload write FOnPluginUnload;
  end;

  TPyEnvironment = class(TPyCustomEnvironment)
  private
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetChildOwner: TComponent; override;
  published
    property AutoLoad;
    property SynchronizeEvents;
    property PythonVersion;
    property PythonEngine;
  end;

implementation

uses
  System.IOUtils,
  System.StrUtils,
  TypInfo,
  PyEnvironment.Path;

{ TPyCustomEnvironment }

constructor TPyCustomEnvironment.Create(AOwner: TComponent);
begin
  inherited;
  FDistributions := CreateCollection();
  FPlugins := TList<IPyEnvironmentPlugin>.Create();
  FSynchronizeEvents := true;
end;

destructor TPyCustomEnvironment.Destroy;
begin
  FPlugins.Free();
  FDistributions.Free();
  SetPythonEngine(nil);
  inherited;
end;

procedure TPyCustomEnvironment.Loaded;
begin
  inherited;
  DoAutoLoad();
end;

procedure TPyCustomEnvironment.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) then begin
    if (AComponent = FPythonEngine) then
      FPythonEngine := nil;
  end;
end;

procedure TPyCustomEnvironment.SetDistributions(
  const ADistributions: TPyDistributionCollection);
begin
  FDistributions.Assign(ADistributions);
end;

procedure TPyCustomEnvironment.SetPythonEngine(const APythonEngine: TPythonEngine);
begin
  if (APythonEngine <> FPythonEngine) then begin
    if Assigned(FPythonEngine) then
      FPythonEngine.RemoveFreeNotification(Self);
    FPythonEngine := APythonEngine;
    if Assigned(FPythonEngine) then begin
      FPythonEngine.FreeNotification(Self);
      if (csDesigning in ComponentState) then
        FPythonEngine.AutoLoad := false;
    end;
  end;
end;

procedure TPyCustomEnvironment.SetPythonVersion(const Value: string);
begin
  FPythonVersion := Value;
end;

procedure TPyCustomEnvironment.DoAutoLoad;
begin
  if not (csDesigning in ComponentState) and FAutoLoad
    and not (PythonVersion.IsEmpty) then
      if Setup(FPythonVersion) then
        Activate(FPythonVersion);
end;

function TPyCustomEnvironment.Setup(APythonVersion: string): boolean;
var
  LCancelation: ICancelation;
begin
  LCancelation := TCancelation.Create();
  Result := InternalSetup(APythonVersion, LCancelation);
end;

function TPyCustomEnvironment.SetupAsync(
  const ACallback: TAsyncFuncCallback<boolean>): IAsyncResult;
begin
  Result := SetupAsync(PythonVersion, ACallback);
end;

function TPyCustomEnvironment.SetupAsync(const APythonVersion: string;
  const ACallback: TAsyncFuncCallback<boolean>): IAsyncResult;
begin
  Result := TAsyncSetup.Create(Self,
    function(ACancelation: ICancelation): boolean
    begin
      Result := InternalSetup(
        IfThen(APythonVersion.IsEmpty(), PythonVersion, APythonVersion),
        ACancelation);
    end).Invoke();
end;

function TPyCustomEnvironment.BeginSetup(const APythonVersion: string): IAsyncResult;
begin
  Result := SetupAsync(APythonVersion);
end;

function TPyCustomEnvironment.EndSetup(const ASyncResult: IAsyncResult): boolean;
begin
  Result := (ASyncResult as TAsyncSetup).GetRetVal();
end;

function TPyCustomEnvironment.BeginActivate(
  const APythonVersion: string): IAsyncResult;
begin
  Result := ActivateAsync(APythonVersion, nil, nil);
end;

function TPyCustomEnvironment.EndActivate(
  const ASyncResult: IAsyncResult): boolean;
begin
  Result := TAsyncActivate(ASyncResult).GetRetVal();
end;

function TPyCustomEnvironment.Activate(APythonVersion: string): boolean;
var
  LCancelation: ICancelation;
begin
  LCancelation := TCancelation.Create();

  Result := InternalActivate(
    IfThen(APythonVersion.IsEmpty(), PythonVersion, APythonVersion),
    LCancelation);
end;

function TPyCustomEnvironment.ActivateAsync(const APythonVersion: string;
  const ASetupAsync: IAsyncResult; const ACallback: TAsyncFuncCallback<boolean>): IAsyncResult;
begin
  Result := TAsyncActivate.Create(Self, ASetupAsync,
    function(ACancelation: ICancelation): boolean
    begin
      Result := InternalActivate(
        IfThen(APythonVersion.IsEmpty(), PythonVersion, APythonVersion),
        ACancelation);
    end, ACallback).Invoke();
end;

function TPyCustomEnvironment.ActivateAsync(const ASetupAsync: IAsyncResult;
  const ACallback: TAsyncFuncCallback<boolean>): IAsyncResult;
begin
  Result := ActivateAsync(PythonVersion, ASetupAsync, ACallback);
end;

function TPyCustomEnvironment.ActivateAsync(
  const ACallback: TAsyncFuncCallback<boolean>): IAsyncResult;
begin
  Result := ActivateAsync(nil, ACallback);
end;

procedure TPyCustomEnvironment.Deactivate;
var
  LCancelation: ICancelation;
begin
  LCancelation := TCancelation.Create();

  InternalDeactivate(LCancelation);
end;

procedure TPyCustomEnvironment.DoBeforeSetup(APythonVersion: string;
  const ACancelation: ICancelation);
begin
  if Assigned(FBeforeSetup) then
    HandleEvent(FSynchronizeEvents, procedure() begin
      FBeforeSetup(Self, APythonVersion);
    end);
end;

procedure TPyCustomEnvironment.DoAfterSetup(APythonVersion: string;
  const ADistribution: TPyDistribution; const ACancelation: ICancelation);
begin
  if Assigned(FAfterSetup) then
    HandleEvent(FSynchronizeEvents, procedure() begin
      FAfterSetup(Self, APythonVersion);
    end);
end;

procedure TPyCustomEnvironment.DoBeforeActivate(APythonVersion: string;
  const ACancelation: ICancelation);
begin
  if Assigned(FBeforeActivate) then
    HandleEvent(FSynchronizeEvents, procedure() begin
      FBeforeActivate(Self, APythonVersion);
    end);
end;

procedure TPyCustomEnvironment.DoAfterActivate(APythonVersion: string;
  const ADistribution: TPyDistribution; var Result: Boolean;
  const ACancelation: ICancelation);
var
  LResult: Boolean;
begin
  LResult := Result;
  if Assigned(FAfterActivate) then
    HandleEvent(FSynchronizeEvents, procedure() begin
      FAfterActivate(Self, APythonVersion, LResult);
    end);
  Result := LResult;
end;

procedure TPyCustomEnvironment.DoBeforeDeactivate(
  const ACancelation: ICancelation);
begin
  if Assigned(FBeforeDeactivate) then
    HandleEvent(FSynchronizeEvents, procedure() begin
      FBeforeDeactivate(Self, FPythonEngine.RegVersion);
    end);
end;

procedure TPyCustomEnvironment.DoAfterDeactivate(
  const ACancelation: ICancelation);
begin
  if Assigned(FAfterDeactivate) then
    HandleEvent(FSynchronizeEvents, procedure() begin
      FAfterDeactivate(Self, FPythonEngine.RegVersion);
    end);
end;

procedure TPyCustomEnvironment.DoPluginInstall(const APlugin: TObject;
  const AInfo: TPyPluginInfo);
begin
  if Assigned(FOnPluginInstall) then
    HandleEvent(FSynchronizeEvents, procedure() begin
      FOnPluginInstall(APlugin, AInfo);
    end);
end;

procedure TPyCustomEnvironment.DoPluginUninstall(const APlugin: TObject;
  const AInfo: TPyPluginInfo);
begin
  if Assigned(FOnPluginUninstall) then
    HandleEvent(FSynchronizeEvents, procedure() begin
      FOnPluginUninstall(APlugin, AInfo);
    end);
end;

procedure TPyCustomEnvironment.DoPluginLoad(const APlugin: TObject;
  const AInfo: TPyPluginInfo);
begin
  if Assigned(FOnPluginLoad) then
    HandleEvent(FSynchronizeEvents, procedure() begin
      FOnPluginLoad(APlugin, AInfo);
    end);
end;

procedure TPyCustomEnvironment.DoPluginUnload(const APlugin: TObject;
  const AInfo: TPyPluginInfo);
begin
  if Assigned(FOnPluginUnload) then
    HandleEvent(FSynchronizeEvents, procedure() begin
      FOnPluginUnload(APlugin, AInfo);
    end);
end;

procedure TPyCustomEnvironment.DoInternalReady;
begin
  if Assigned(FOnReady) then
    TThread.Synchronize(nil, procedure() begin
      FOnReady(Self, FPythonEngine.RegVersion);
    end);
end;

procedure TPyCustomEnvironment.DoInternalError;
begin
  if Assigned(FOnError) then begin
    HandleEvent(FSynchronizeEvents,
      procedure()
      var
        LException: Exception;
      begin
        LException := Exception(ExceptObject());
        FOnError(Self, LException);
      end);

    Abort();
  end else begin
    try
      raise Exception(AcquireExceptionObject()) at ExceptAddr();
    finally
      ReleaseExceptionObject();
    end;
  end;
end;

procedure TPyCustomEnvironment.HandleEvent(const ASynchronizeEvents: boolean;
  const AThreadProc: TThreadProcedure);
begin
  if ASynchronizeEvents and Assigned(WakeMainThread) and
    (MainThreadID <> TThread.Current.ThreadID) then
      TThread.Synchronize(TThread.Current, AThreadProc)
  else
    AThreadProc();
end;

function TPyCustomEnvironment.InternalSetup(const APythonVersion: string;
  const ACancelation: ICancelation): boolean;
var
  LDistribution: TPyDistribution;
begin
  Assert(Assigned(ACancelation), 'Invalid argument "ACancelation".');

  LDistribution := nil;

  DoBeforeSetup(APythonVersion, ACancelation);
  try
    InstallAndLoadPlugins(TPyPluginEvent.BeforeSetup, ACancelation);

    Prepare(ACancelation);

    LDistribution := FDistributions.LocateEnvironment(
      IfThen(APythonVersion.IsEmpty(), PythonVersion, APythonVersion));

    if not Assigned(LDistribution) then
      Exit(false);

    Result := LDistribution.Setup(ACancelation);

    if not Result then
      Exit;

    InstallAndLoadPlugins(TPyPluginEvent.AfterSetup, ACancelation);
  except
    on E: EAbort do
      raise
    else
      DoInternalError();
  end;

  DoAfterSetup(APythonVersion, LDistribution, ACancelation);

  Result := true;
end;

function TPyCustomEnvironment.InternalActivate(const APythonVersion: string;
  const ACancelation: ICancelation): boolean;
var
  LDistribution: TPyDistribution;
begin
  Assert(Assigned(ACancelation), 'Invalid argument "ACancelation".');

  DoBeforeActivate(APythonVersion, ACancelation);

  try
    InstallAndLoadPlugins(TPyPluginEvent.BeforeActivate, ACancelation);
  except
    on E: EAbort do
      raise
    else
      DoInternalError();
  end;

  //An engine is required
  if not Assigned(FPythonEngine) then
    Exit(false);

  LDistribution := FDistributions.LocateEnvironment(
    IfThen(APythonVersion.IsEmpty(), PythonVersion, APythonVersion));

  if not Assigned(LDistribution) then
    Exit(false);

  if not LDistribution.IsAvailable() then
    Exit(false);

  try
    FPythonEngine.UseLastKnownVersion := false;
    FPythonEngine.PythonHome := TPyEnvironmentPath.ResolvePath(LDistribution.Home);
    FPythonEngine.ProgramName := TPyEnvironmentPath.ResolvePath(LDistribution.Executable);
    FPythonEngine.DllPath := TPath.GetDirectoryName(TPyEnvironmentPath.ResolvePath(LDistribution.SharedLibrary));
    FPythonEngine.DllName := TPath.GetFileName(LDistribution.SharedLibrary);
    TThread.Synchronize(nil, procedure() begin
      FPythonEngine.LoadDll();
    end);

    Result := FPythonEngine.IsHandleValid();

    if not Result then
      Exit;
  except
    on E: EAbort do
      raise
    else
      DoInternalError();
  end;

  DoAfterActivate(APythonVersion, LDistribution, Result, ACancelation);

  try
    InstallAndLoadPlugins(TPyPluginEvent.AfterActivate, ACancelation);
  except
    on E: EAbort do
      raise
    else
      DoInternalError();
  end;

  DoInternalReady();
end;

procedure TPyCustomEnvironment.InternalDeactivate(
  const ACancelation: ICancelation);
begin
  Assert(Assigned(ACancelation), 'Invalid argument "ACancelation".');

  DoBeforeDeactivate(ACancelation);

  //We need a working engine
  if not Assigned(FPythonEngine) then
    Exit();

  try
    InstallAndLoadPlugins(TPyPluginEvent.BeforeDeactivate, ACancelation);

    FPythonEngine.UnloadDll();
    FPythonEngine.PythonHome := String.Empty;
    FPythonEngine.ProgramName := String.Empty;
    FPythonEngine.DllPath := String.Empty;
    FPythonEngine.DllName := String.Empty;

    InstallAndLoadPlugins(TPyPluginEvent.AfterDeactivate, ACancelation);
  except
    on E: EAbort do
      raise
    else
      DoInternalError();
  end;

  DoAfterDeactivate(ACancelation);
end;

procedure TPyCustomEnvironment.Prepare(const ACancelation: ICancelation);
begin
  if PythonVersion.Trim().IsEmpty() and (Distributions.Count > 0) then
    PythonVersion := TPyDistribution(Distributions.Items[0]).PythonVersion;
end;

procedure TPyCustomEnvironment.AddPlugin(const APlugin: IPyEnvironmentPlugin);
begin
  FPlugins.Add(APlugin);
end;

procedure TPyCustomEnvironment.RemovePlugin(const APlugin: IPyEnvironmentPlugin);
begin
  FPlugins.Remove(APlugin);
end;

procedure TPyCustomEnvironment.InstallAndLoadPlugins(const AEvent: TPyPluginEvent;
  const ACancelation: ICancelation);
var
  LPlugin: IPyEnvironmentPlugin;
begin
  for LPlugin in FPlugins do begin
    if (AEvent in LPlugin.Info.InstallsWhen) and not LPlugin.IsInstalled() then begin
      DoPluginInstall(LPlugin as TObject, LPlugin.Info);
      LPlugin.InstallPlugin(ACancelation);
    end;

    ACancelation.CheckCancelled();

    if (AEvent in LPlugin.Info.LoadsWhen) and LPlugin.IsInstalled() then begin
      DoPluginLoad(LPlugin as TObject, LPlugin.Info);
      LPlugin.LoadPlugin(ACancelation);
    end;
  end;
end;

procedure TPyCustomEnvironment.UninstallAndUnloadPlugins(const AEvent: TPyPluginEvent;
  const ACancelation: ICancelation);
var
  LPlugin: IPyEnvironmentPlugin;
begin
  for LPlugin in FPlugins do begin
    if (AEvent in LPlugin.Info.UnloadsWhen) and LPlugin.IsInstalled() then begin
      DoPluginUnload(LPlugin as TObject, LPlugin.Info);
      LPlugin.UnloadPlugin(ACancelation);
    end;

    if not (AEvent in LPlugin.Info.UninstallsWhen) then
      Continue;

    if not LPlugin.IsInstalled() then
      Continue;

    DoPluginUninstall(LPlugin as TObject, LPlugin.Info);
    LPlugin.UninstallPlugin(ACancelation);
  end;
end;

{ TPyEnvironment }

procedure TPyEnvironment.DefineProperties(Filer: TFiler);
begin
  inherited;
end;

function TPyEnvironment.GetChildOwner: TComponent;
begin
  Result := Self;
end;

procedure TPyEnvironment.GetChildren(Proc: TGetChildProc; Root: TComponent);
//var
//  I: Integer;
begin
  inherited GetChildren(Proc, Root);
//  for I := 0 to ComponentCount - 1 do
//    Proc(Components[I]);
end;

{ TPyCustomEnvironment.TEnvironmentTaskAsyncResult }

constructor TPyCustomEnvironment.TEnvironmentTaskAsyncResult.Create(
  const AContext: TObject; const AAsyncTask: TProc<ICancelation>;
  const AAsyncCallback: TAsyncCallback);
begin
  inherited Create(AContext);
  FAsyncTask := AAsyncTask;
  FAsyncCallback := AAsyncCallback;
  FCancelation := TCancelation.Create();
end;

procedure TPyCustomEnvironment.TEnvironmentTaskAsyncResult.AsyncDispatch;
begin
  FAsyncTask(FCancelation);
end;

procedure TPyCustomEnvironment.TEnvironmentTaskAsyncResult.Complete;
begin
  inherited;
  if Assigned(FAsyncCallback) then
    FAsyncCallback(Self as IAsyncResult);
end;

function TPyCustomEnvironment.TEnvironmentTaskAsyncResult.DoCancel: Boolean;
begin
  FCancelation.Cancel();

  Result := true;
end;

procedure TPyCustomEnvironment.TEnvironmentTaskAsyncResult.Schedule;
begin
  TTask.Run(DoAsyncDispatch);
end;

{ TPyCustomEnvironment.TEnvironmentTaskAsyncResult<TResult> }

constructor TPyCustomEnvironment.TEnvironmentTaskAsyncResult<TResult>.Create(
  const AContext: TObject; const AAsyncTask: TFunc<ICancelation, TResult>;
  const AAsyncFuncCallback: TAsyncFuncCallback<TResult>);
begin
  inherited Create(AContext,
    procedure(ACancelation: ICancelation)
    begin
      FRetVal := AAsyncTask(ACancelation);
    end,
    procedure(const AAsyncResult: IAsyncResult)
    begin
      if Assigned(AAsyncFuncCallback) then
        AAsyncFuncCallback(AAsyncResult, FRetVal);
    end);
end;

function TPyCustomEnvironment.TEnvironmentTaskAsyncResult<TResult>.GetRetVal: TResult;
begin
  WaitForCompletion;
  Result := FRetVal;
end;

{ TPyCustomEnvironment.TAsyncActivate }

procedure TPyCustomEnvironment.TAsyncActivate.AsyncDispatch;
begin
  if (FSetupResult as TAsyncSetup).GetRetVal() then
    if not FSetupResult.IsCancelled then
      inherited
    else
      Cancel();
end;

constructor TPyCustomEnvironment.TAsyncActivate.Create(const AContext: TObject;
  const ASetupResult: IAsyncResult; const AAsyncTask: TFunc<ICancelation, boolean>;
  const AAsyncFuncCallback: TAsyncFuncCallback<boolean>);
begin
  inherited Create(AContext, AAsyncTask, AAsyncFuncCallback);
  FSetupResult := ASetupResult;
end;

end.
