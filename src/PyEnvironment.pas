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
  end;

  TPyEnvironmentBeforeSetup = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentAfterSetup = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentBeforeActivate = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentAfterActivate = procedure(Sender: TObject; const APythonVersion: string; const AActivated: boolean) of object;
  TPyEnvironmentBeforeDeactivate = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentAfterDeactivate = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentReady = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentError = procedure(Sender: TObject; const AException: Exception) of object;

  TPyEnvironmentPluginInstall = procedure(const APlugin: TObject; const AInfo: TPyPluginInfo) of object;
  TPyEnvironmentPluginUninstall = procedure(const APlugin: TObject; const AInfo: TPyPluginInfo) of object;

  IPyEnvironmentPlugin = interface
    ['{11906029-81A6-4F94-ACDA-2DBB346C6E3A}']
    function GetInfo(): TPyPluginInfo;
    
    function Install(const AArgs: TArray<TValue> = []): IAsyncResult;
    function Uninstall(const AArgs: TArray<TValue> = []): IAsyncResult;

    function IsInstalled(): boolean;
    
    property Info: TPyPluginInfo read GetInfo;
  end;  
  
  TPyCustomEnvironment = class(TComponent)
  protected type
    TEnvironmentTaskAsyncResult = class(TBaseAsyncResult)
    public type
      TScheduler = (Task, Queue, ForceQueue);
    private
      FAsyncTask: TProc<ICancelation>;
      FAsyncCallback: TAsyncCallback;
      FScheduler: TScheduler;
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

      property Scheduler: TScheduler read FScheduler write FScheduler;
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
      procedure Schedule; override;
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
    procedure SetDistributions(const ADistributions: TPyDistributionCollection);
    procedure SetPythonEngine(const APythonEngine: TPythonEngine);
    procedure DoAutoLoad;
    //Hooked routines
    function InternalSetup(const APythonVersion: string;
      const ACancelation: ICancelation): boolean;
    function InternalActivate(const APythonVersion: string;
      const ACancelation: ICancelation): boolean;
    procedure InternalDeactivate(const ACancelation: ICancelation);
    //State handlers
    procedure DoBeforeSetup(APythonVersion: string; const ACancelation: ICancelation);
    procedure DoAfterSetup(APythonVersion: string; 
      const ADistribution: TPyDistribution; const ACancelation: ICancelation);
    procedure DoBeforeActivate(APythonVersion: string; const ACancelation: ICancelation);
    procedure DoAfterActivate(
      APythonVersion: string; const ADistribution: TPyDistribution; 
      var Result: Boolean; const ACancelation: ICancelation);
    procedure DoBeforeDeactivate(const ACancelation: ICancelation);
    procedure DoAfterDeactivate(const ACancelation: ICancelation);
    /// <summary>
    ///   It won't be ready until setup, activate, run add-ons and install packages
    /// </summary>
    procedure DoInternalReady;
    procedure DoInternalError;
  protected
    procedure Loaded(); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    //Plugins install/uninstall
    procedure InstallPlugins(const AEvent: TPyPluginEvent;
      const ACancelation: ICancelation); virtual;
    procedure UninstallPlugins(const AEvent: TPyPluginEvent;
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
    procedure RemovePlugion(const APlugin: IPyEnvironmentPlugin);
  public
    property Distributions: TPyDistributionCollection read FDistributions write SetDistributions;
    
    property AutoLoad: boolean read FAutoLoad write FAutoLoad;
    property PythonVersion: string read FPythonVersion write SetPythonVersion;
    property PythonEngine: TPythonEngine read FPythonEngine write SetPythonEngine;
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
  end;

  TPyEnvironment = class(TPyCustomEnvironment)
  private
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetChildOwner: TComponent; override;
  published
    property AutoLoad;
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
  FDistributions := CreateCollection();
  FPlugins := TList<IPyEnvironmentPlugin>.Create();
  inherited;
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
  if FAutoLoad and not (PythonVersion.IsEmpty) then
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
    FBeforeSetup(Self, APythonVersion);
end;

procedure TPyCustomEnvironment.DoAfterSetup(APythonVersion: string;
  const ADistribution: TPyDistribution; const ACancelation: ICancelation);
begin
  if Assigned(FAfterSetup) then
    FAfterSetup(Self, APythonVersion);
end;

procedure TPyCustomEnvironment.DoBeforeActivate(APythonVersion: string;
  const ACancelation: ICancelation);
begin
  if Assigned(FBeforeActivate) then
    FBeforeActivate(Self, APythonVersion);
end;

procedure TPyCustomEnvironment.DoAfterActivate(APythonVersion: string;
  const ADistribution: TPyDistribution; var Result: Boolean;
  const ACancelation: ICancelation);
begin
  if Assigned(FAfterActivate) then
    FAfterActivate(Self, APythonVersion, Result);
end;

procedure TPyCustomEnvironment.DoBeforeDeactivate(
  const ACancelation: ICancelation);
begin
  if Assigned(FBeforeDeactivate) then
    FBeforeDeactivate(Self, FPythonEngine.RegVersion);
end;

procedure TPyCustomEnvironment.DoAfterDeactivate(
  const ACancelation: ICancelation);
begin
  if Assigned(FAfterDeactivate) then
    FAfterDeactivate(Self, FPythonEngine.RegVersion);
end;

procedure TPyCustomEnvironment.DoInternalReady;
begin
  if Assigned(FOnReady) then
    FOnReady(Self, FPythonEngine.RegVersion);
end;

procedure TPyCustomEnvironment.DoInternalError;
begin
  if Assigned(FOnError) then
    FOnError(Self, Exception(ExceptObject()))
  else begin
    try
      raise Exception(AcquireExceptionObject()) at ExceptAddr();
    finally
      ReleaseExceptionObject();
    end;
  end;
end;

procedure TPyCustomEnvironment.DoAutoLoad;
begin
  if Setup(FPythonVersion) then
    Activate(FPythonVersion);
end;

function TPyCustomEnvironment.InternalSetup(const APythonVersion: string;
  const ACancelation: ICancelation): boolean;
var
  LDistribution: TPyDistribution;
begin
  DoBeforeSetup(APythonVersion, ACancelation);
  try
    InstallPlugins(TPyPluginEvent.BeforeSetup, ACancelation);

    Prepare(ACancelation);

    LDistribution := FDistributions.LocateEnvironment(
      IfThen(APythonVersion.IsEmpty(), PythonVersion, APythonVersion));

    if not Assigned(LDistribution) then
      Exit(false);

    Result := LDistribution.Setup(ACancelation);

    if not Result then
      Exit;

    InstallPlugins(TPyPluginEvent.AfterSetup, ACancelation);
  except
    DoInternalError();
    Exit(false);
  end;

  DoAfterSetup(APythonVersion, LDistribution, ACancelation);
end;

function TPyCustomEnvironment.InternalActivate(const APythonVersion: string;
  const ACancelation: ICancelation): boolean;
var
  LDistribution: TPyDistribution;
begin
  DoBeforeActivate(APythonVersion, ACancelation);

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
    InstallPlugins(TPyPluginEvent.BeforeActivate, ACancelation);

    FPythonEngine.UseLastKnownVersion := false;
    FPythonEngine.PythonHome := TPyEnvironmentPath.ResolvePath(LDistribution.Home);
    FPythonEngine.ProgramName := TPyEnvironmentPath.ResolvePath(LDistribution.Executable);
    FPythonEngine.DllPath := TPath.GetDirectoryName(TPyEnvironmentPath.ResolvePath(LDistribution.SharedLibrary));
    FPythonEngine.DllName := TPath.GetFileName(LDistribution.SharedLibrary);
    FPythonEngine.LoadDll();

    Result := FPythonEngine.IsHandleValid();

    if not Result then
      Exit;

    InstallPlugins(TPyPluginEvent.AfterActivate, ACancelation);
  except
    DoInternalError();
    Exit(false);
  end;

  DoAfterActivate(APythonVersion, LDistribution, Result, ACancelation);

  DoInternalReady();
end;

procedure TPyCustomEnvironment.InternalDeactivate(
  const ACancelation: ICancelation);
begin
  DoBeforeDeactivate(ACancelation);

  //We need a working engine
  if not Assigned(FPythonEngine) then
    Exit();

  try
    InstallPlugins(TPyPluginEvent.BeforeDeactivate, ACancelation);

    FPythonEngine.UnloadDll();
    FPythonEngine.PythonHome := String.Empty;
    FPythonEngine.ProgramName := String.Empty;
    FPythonEngine.DllPath := String.Empty;
    FPythonEngine.DllName := String.Empty;

    InstallPlugins(TPyPluginEvent.AfterDeactivate, ACancelation);
  except
    DoInternalError();
    Exit;
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

procedure TPyCustomEnvironment.RemovePlugion(const APlugin: IPyEnvironmentPlugin);
begin
  FPlugins.Remove(APlugin);
end;

procedure TPyCustomEnvironment.InstallPlugins(const AEvent: TPyPluginEvent;
  const ACancelation: ICancelation);
var
  LPlugin: IPyEnvironmentPlugin;
  LAsyncInstall: IAsyncResult;
begin
  for LPlugin in FPlugins do begin
    if not (AEvent in LPlugin.Info.InstallsWhen) then
      Continue;

    if LPlugin.IsInstalled() then
      Continue;

    if ACancelation.IsCanceled() then
      Break;

    if Assigned(FOnPluginInstall) then
      FOnPluginInstall(LPlugin as TObject, LPlugin.Info);

    LAsyncInstall := LPlugin.Install();
    while (LAsyncInstall.AsyncWaitEvent.WaitFor(100) <> TWaitResult.wrSignaled) 
      and not LAsyncInstall.IsCancelled do begin
        if ACancelation.IsCanceled() then
          LAsyncInstall.Cancel()
    end;

    //Catches plugin exception
    (LAsyncInstall as TBaseAsyncResult).WaitForCompletion();
  end;
end;

procedure TPyCustomEnvironment.UninstallPlugins(const AEvent: TPyPluginEvent;
  const ACancelation: ICancelation);
var
  LPlugin: IPyEnvironmentPlugin;
  LAsyncUninstall: IAsyncResult;
begin
  for LPlugin in FPlugins do begin
    if not (AEvent in LPlugin.Info.UninstallsWhen) then
      Continue;

    if not LPlugin.IsInstalled() then
      Continue;

    if Assigned(FOnPluginUninstall) then
      FOnPluginUninstall(LPlugin as TObject, LPlugin.Info);

    LAsyncUninstall := LPlugin.Uninstall();
    while (LAsyncUninstall.AsyncWaitEvent.WaitFor(100) <> TWaitResult.wrSignaled)
      and not LAsyncUninstall.IsCancelled do begin
      if ACancelation.IsCanceled() then
        LAsyncUninstall.Cancel();
    end;

    //Catches plugin exception
    (LAsyncUninstall as TBaseAsyncResult).WaitForCompletion();
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
  FScheduler := TScheduler.Task;
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
  case FScheduler of
    Task: TTask.Run(DoAsyncDispatch);
    Queue: TThread.Queue(TThread.Current, DoAsyncDispatch);
    ForceQueue: TThread.ForceQueue(TThread.Current, DoAsyncDispatch);
  end;
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

constructor TPyCustomEnvironment.TAsyncActivate.Create(const AContext: TObject;
  const ASetupResult: IAsyncResult; const AAsyncTask: TFunc<ICancelation, boolean>;
  const AAsyncFuncCallback: TAsyncFuncCallback<boolean>);
begin
  inherited Create(AContext, AAsyncTask, AAsyncFuncCallback);

  if Assigned(ASetupResult) then
    Scheduler := TScheduler.Queue
  else
    Scheduler := TScheduler.ForceQueue;

  FSetupResult := ASetupResult;
end;

procedure TPyCustomEnvironment.TAsyncActivate.Schedule;
begin
  if Assigned(FSetupResult) then begin
    TTask.Run(procedure() begin
      if (FSetupResult as TAsyncSetup).GetRetVal() then
        inherited;
    end);
  end else
    inherited;
end;

end.
