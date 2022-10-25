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
  System.SysUtils,
  System.Threading,
  System.Generics.Collections,
  System.Types,
  System.SysConst,
  PythonEngine,
  PyTools.Cancelation,
  PyEnvironment.Distribution,
  PyEnvironment.Notification;

type
  TPyEnvironmentBeforeSetup = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentAfterSetup = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentBeforeActivate = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentAfterActivate = procedure(Sender: TObject; const APythonVersion: string; const AActivated: boolean) of object;
  TPyEnvironmentBeforeDeactivate = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentAfterDeactivate = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentReady = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentError = procedure(Sender: TObject; const AException: Exception) of object;

  TPyCustomEnvironment = class(TComponent, IEnvironmentNotifier<TPyCustomEnvironment>)
  protected type
    TEnvironmentTaskAsyncResult = class(TBaseAsyncResult)
    public type
      TScheduler = (Task, Queue, ForceQueue);
    private
      FAsyncTask: TProc;
      FCancelCallback: TProc;
      FAsyncCallback: TAsyncCallback;
      FScheduler: TScheduler;
    protected
      procedure Complete; override;
      procedure Schedule; override;
      procedure AsyncDispatch; override;
      function DoCancel: Boolean; override;
    public
      constructor Create(const AContext: TObject; const AAsyncTask: TProc;
        const ACancelCallback: TProc = nil; AAsyncCallback: TAsyncCallback = nil);

      property Scheduler: TScheduler read FScheduler write FScheduler;
    end;
    TAsyncFuncCallback<TResult> = reference to procedure (
      const ASyncResult: IAsyncResult; const AResult: TResult);
    TAsyncSetup = class sealed(TEnvironmentTaskAsyncResult);
    TAsyncActivate<TResult> = class sealed(TEnvironmentTaskAsyncResult)
    private
      FRetVal: TResult;
      FSetupResult: IAsyncResult;
    protected
      procedure Schedule; override;
    protected
      constructor Create(const AContext: TObject; const ASetupResult: IAsyncResult; const AAsyncTask: TFunc<TResult>;
        const ACancelCallback: TProc = nil; AAsyncFuncCallback: TAsyncFuncCallback<TResult> = nil);
      function GetRetVal: TResult;
    end;
  private
    FDistributions: TPyDistributionCollection;
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
    //Delegators
    FEnvironmentNotifier: IEnvironmentNotifier<TPyCustomEnvironment>;
    procedure SetDistributions(const ADistributions: TPyDistributionCollection);
    procedure SetPythonEngine(const APythonEngine: TPythonEngine);
    procedure DoAutoLoad;
    //Hooked routines
    procedure InternalSetup(const APythonVersion: string;
      const ACancelation: ICancelation);
    function InternalActivate(const APythonVersion: string;
      const ACancelation: ICancelation): boolean;
    procedure InternalDeactivate();
    //State handlers
    procedure DoBeforeSetup(APythonVersion: string);
    procedure DoAfterSetup(APythonVersion: string; const ADistribution: TPyDistribution);
    procedure DoBeforeActivate(APythonVersion: string);
    procedure DoAfterActivate(
      APythonVersion: string; const ADistribution: TPyDistribution; var Result: Boolean);
    procedure DoBeforeDeactivate;
    procedure DoAfterDeactivate;
    /// <summary>
    ///   It won't be ready until setup, activate, run add-ons and install packages
    /// </summary>
    //procedure DoInternalReady;
    procedure DoInternalError;
  protected
    property EnvironmentNotifier: IEnvironmentNotifier<TPyCustomEnvironment>
      read FEnvironmentNotifier
      implements IEnvironmentNotifier<TPyCustomEnvironment>;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  protected
    procedure SetPythonVersion(const Value: string); virtual;
    function CreateCollection(): TPyDistributionCollection; virtual; abstract;
    procedure Prepare(const ACancelation: ICancelation); virtual;
    //IEnvironmentNotifier<TPyCustomEnvironment> delegation
    procedure InternalNotifyAll(ANotification: TEnvironmentNotification;
      ADistribution: TPyDistribution); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    /// <summary>
    ///   Setup an environment.
    /// </summary>
    procedure Setup(APythonVersion: string = '');
    /// <summary>
    ///   Activate an environment.
    /// </summary>
    function Activate(APythonVersion: string = ''): boolean;
    /// <summary>
    ///   Deactivate an environment.
    /// </summary>
    procedure Deactivate();

    function SetupAsync(const APythonVersion: string;
      const ACallback: TAsyncCallback = nil): IAsyncResult; overload;
    function SetupAsync(
      const ACallback: TAsyncCallback = nil): IAsyncResult; overload;
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
    procedure EndSetup(const ASyncResult: IAsyncResult);
    /// <summary>
    ///   Queue the activate action to the main thread.
    /// </summary>
    function BeginActivate(const APythonVersion: string = ''): IAsyncResult;
    /// <summary>
    ///   Wait for the activate action to complete.
    ///   Never call this routine directly from main thread.
    /// </summary>
    function EndActivate(const ASyncResult: IAsyncResult): boolean;
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
    property OnReady: TPyEnvironmentReady read FOnReady write FOnReady;
    property OnError: TPyEnvironmentError read FOnError write FOnError;
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
  FEnvironmentNotifier := TEnvironmentBroadcaster<TPyCustomEnvironment>.Create(Self);
  inherited;
end;

destructor TPyCustomEnvironment.Destroy;
begin
  FDistributions.Free();
  inherited;
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
        FPythonEngine.AutoLoad := false
      else if FAutoLoad and not (PythonVersion.IsEmpty) and (csLoading in ComponentState) then
        DoAutoLoad();
    end;
  end;
end;

procedure TPyCustomEnvironment.SetPythonVersion(const Value: string);
begin
  FPythonVersion := Value;
end;

procedure TPyCustomEnvironment.Setup(APythonVersion: string);
begin
  InternalSetup(APythonVersion, TCancelation.Create());
end;

function TPyCustomEnvironment.SetupAsync(
  const ACallback: TAsyncCallback): IAsyncResult;
begin
  Result := SetupAsync(PythonVersion, ACallback);
end;

function TPyCustomEnvironment.SetupAsync(const APythonVersion: string;
  const ACallback: TAsyncCallback): IAsyncResult;
var
  LCancelation: ICancelation;
begin
  LCancelation := TCancelation.Create();

  Result := TAsyncSetup.Create(Self,
    procedure()
    begin
      InternalSetup(
        IfThen(APythonVersion.IsEmpty(), PythonVersion, APythonVersion),
        LCancelation);
    end,
    procedure()
    begin
      LCancelation.Cancel();
    end, ACallback).Invoke();
end;

function TPyCustomEnvironment.BeginSetup(const APythonVersion: string): IAsyncResult;
begin
  Result := SetupAsync(APythonVersion);
end;

procedure TPyCustomEnvironment.EndSetup(const ASyncResult: IAsyncResult);
begin
  (ASyncResult as TBaseAsyncResult).WaitForCompletion();
end;

function TPyCustomEnvironment.BeginActivate(
  const APythonVersion: string): IAsyncResult;
begin
  Result := ActivateAsync(APythonVersion, nil, nil);
end;

function TPyCustomEnvironment.EndActivate(
  const ASyncResult: IAsyncResult): boolean;
begin
  Result := TAsyncActivate<boolean>(ASyncResult).GetRetVal();
end;

function TPyCustomEnvironment.Activate(APythonVersion: string): boolean;
begin
  Result := InternalActivate(
    IfThen(APythonVersion.IsEmpty(), PythonVersion, APythonVersion),
    TCancelation.Create());
end;

function TPyCustomEnvironment.ActivateAsync(const APythonVersion: string;
  const ASetupAsync: IAsyncResult; const ACallback: TAsyncFuncCallback<boolean>): IAsyncResult;
var
  LCancelation: ICancelation;
begin
  LCancelation := TCancelation.Create();

  Result := TAsyncActivate<boolean>.Create(Self, ASetupAsync,
    function(): boolean
    begin
      Result := InternalActivate(
        IfThen(APythonVersion.IsEmpty(), PythonVersion, APythonVersion),
        LCancelation);
    end,
    procedure()
    begin
      LCancelation.Cancel();
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
begin
  InternalDeactivate();
end;

procedure TPyCustomEnvironment.DoAfterDeactivate;
begin
  if Assigned(FAfterDeactivate) then
    FAfterDeactivate(Self, FPythonEngine.RegVersion);

  InternalNotifyAll(AFTER_DEACTIVATE_NOTIFICATION, nil);
end;

procedure TPyCustomEnvironment.DoBeforeDeactivate;
begin
  if Assigned(FBeforeDeactivate) then
    FBeforeDeactivate(Self, FPythonEngine.RegVersion);

  InternalNotifyAll(BEFORE_DEACTIVATE_NOTIFICATION, nil);
end;

procedure TPyCustomEnvironment.DoAfterActivate(APythonVersion: string;
  const ADistribution: TPyDistribution; var Result: Boolean);
begin
  if Assigned(FAfterActivate) then
    FAfterActivate(Self, APythonVersion, Result);

  InternalNotifyAll(AFTER_ACTIVATE_NOTIFICATION, ADistribution);
end;

procedure TPyCustomEnvironment.DoBeforeActivate(APythonVersion: string);
begin
  if Assigned(FBeforeDeactivate) then
    FBeforeDeactivate(Self, APythonVersion);

  InternalNotifyAll(BEFORE_ACTIVATE_NOTIFICATION, nil);
end;

procedure TPyCustomEnvironment.DoAfterSetup(APythonVersion: string;
  const ADistribution: TPyDistribution);
begin
  if Assigned(FAfterSetup) then
    FAfterSetup(Self, APythonVersion);

  InternalNotifyAll(AFTER_SETUP_NOTIFICATION, ADistribution);
end;

procedure TPyCustomEnvironment.DoBeforeSetup(APythonVersion: string);
begin
  if Assigned(FBeforeSetup) then
    FBeforeSetup(Self, APythonVersion);

  InternalNotifyAll(BEFORE_SETUP_NOTIFICATION, nil);
end;

//procedure TPyCustomEnvironment.DoInternalReady;
//begin
//  if Assigned(FOnReady) then
//    FOnReady(Self, FPythonEngine.RegVersion);
//
//  InternalNotifyAll(INTERNAL_READY_NOTIFICATION, nil);
//end;

procedure TPyCustomEnvironment.DoInternalError;
begin
  try
    if Assigned(FOnError) then
      FOnError(Self, Exception(ExceptObject()))
    else begin
      try
        raise Exception(AcquireExceptionObject()) at ExceptAddr();
      finally
        ReleaseExceptionObject();
      end;
    end;
  finally
    InternalNotifyAll(INTERNAL_ERROR_NOTIFICATION, nil);
  end;
end;

procedure TPyCustomEnvironment.DoAutoLoad;
begin
  Setup(FPythonVersion);
  Activate(FPythonVersion);
end;

procedure TPyCustomEnvironment.InternalSetup(const APythonVersion: string;
  const ACancelation: ICancelation);
var
  LDistribution: TPyDistribution;
begin
  DoBeforeSetup(APythonVersion);
  try
    Prepare(ACancelation);

    LDistribution := FDistributions.LocateEnvironment(
      IfThen(APythonVersion.IsEmpty(), PythonVersion, APythonVersion));

    if not Assigned(LDistribution) then
      Exit();

    LDistribution.Setup(ACancelation);
  except
    on E: Exception do begin
      DoInternalError();
      Exit;
    end;
  end;

  DoAfterSetup(APythonVersion, LDistribution);
end;

function TPyCustomEnvironment.InternalActivate(const APythonVersion: string;
  const ACancelation: ICancelation): boolean;
var
  LDistribution: TPyDistribution;
begin
  DoBeforeActivate(APythonVersion);

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
    FPythonEngine.LoadDll();

    Result := FPythonEngine.IsHandleValid();
  except
    on E: Exception do
      DoInternalError();
  end;

  DoAfterActivate(APythonVersion, LDistribution, Result);
end;

procedure TPyCustomEnvironment.InternalDeactivate;
begin
  DoBeforeDeactivate();

  //We need a running engine
  if not Assigned(FPythonEngine) then
    Exit();

  try
    FPythonEngine.UnloadDll();
    FPythonEngine.PythonHome := String.Empty;
    FPythonEngine.ProgramName := String.Empty;
    FPythonEngine.DllPath := String.Empty;
    FPythonEngine.DllName := String.Empty;
  except
    on E: Exception do
      DoInternalError();
  end;

  DoAfterDeactivate();
end;

procedure TPyCustomEnvironment.InternalNotifyAll(ANotification: TEnvironmentNotification;
  ADistribution: TPyDistribution);
begin
  FEnvironmentNotifier.NotifyAll(ANotification, ADistribution);
end;

procedure TPyCustomEnvironment.Prepare(const ACancelation: ICancelation);
begin
  if PythonVersion.Trim().IsEmpty() and (Distributions.Count > 0) then
    PythonVersion := TPyDistribution(Distributions.Items[0]).PythonVersion;
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
  const AContext: TObject; const AAsyncTask: TProc;
  const ACancelCallback: TProc; AAsyncCallback: TAsyncCallback);
begin
  inherited Create(AContext);
  FAsyncTask := AAsyncTask;
  FCancelCallback := ACancelCallback;
  FAsyncCallback := AAsyncCallback;
  FScheduler := TScheduler.Task;
end;

procedure TPyCustomEnvironment.TEnvironmentTaskAsyncResult.AsyncDispatch;
begin
  FAsyncTask();
end;

procedure TPyCustomEnvironment.TEnvironmentTaskAsyncResult.Complete;
begin
  inherited;
  if Assigned(FAsyncCallback) then
    FAsyncCallback(Self as IAsyncResult);
end;

function TPyCustomEnvironment.TEnvironmentTaskAsyncResult.DoCancel: Boolean;
begin
  if Assigned(FCancelCallback) then
    FCancelCallback();

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

{ TPyCustomEnvironment.TAsyncActivate<TResult> }

constructor TPyCustomEnvironment.TAsyncActivate<TResult>.Create(
  const AContext: TObject; const ASetupResult: IAsyncResult;
  const AAsyncTask: TFunc<TResult>; const ACancelCallback: TProc;
  AAsyncFuncCallback: TAsyncFuncCallback<TResult>);
begin
  inherited Create(AContext,
    procedure()
    begin
      FRetVal := AAsyncTask();
    end,
    ACancelCallback,
    procedure(const AAsyncResult: IAsyncResult)
    begin
      if Assigned(AAsyncFuncCallback) then
        AAsyncFuncCallback(AAsyncResult, FRetVal);
    end);

  if Assigned(ASetupResult) then
    Scheduler := TScheduler.Queue
  else
    Scheduler := TScheduler.ForceQueue;

  FSetupResult := ASetupResult;
end;

function TPyCustomEnvironment.TAsyncActivate<TResult>.GetRetVal: TResult;
begin
  WaitForCompletion;
  Result := FRetVal;
end;

procedure TPyCustomEnvironment.TAsyncActivate<TResult>.Schedule;
begin
  if Assigned(FSetupResult) then begin
    TTask.Run(procedure() begin
      FSetupResult.AsyncWaitEvent.WaitFor(INFINITE);
      inherited;
    end);
  end else
    inherited;
end;

initialization
  RegisterClass(TPyEnvironment);

finalization
  UnRegisterClass(TPyEnvironment);

end.
