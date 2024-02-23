(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Project.IDE.Deploy.Manager'               *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality:  Manage files deploy operations                        *)
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
unit PyEnvironment.Project.IDE.Deploy.Manager;

interface

uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Threading,
  System.Generics.Collections,
  PyEnvironment.Project.IDE.Types,
  PyEnvironment.Project.IDE.Message,
  PyEnvironment.Project.IDE.Deploy.Intf;

type
  TEnvironmentProjectDeployManager = class
  private
    class var FMacLock: TObject;
  private
    FModel: TDeployFilesModel;
    FMessages: TDictionary<TDeployTask, TTextProgressBarMessage>;
    FMessagesPtr: TDictionary<TDeployTask, pointer>;

    function GetMessage(const ATask: TDeployTask): TTextProgressBarMessage;
    function GetMessagePtr(const ATask: TDeployTask): pointer;

    procedure DoStart(const AText: string; const ATask: TDeployTask);
    procedure DoProgress(APercentage: integer; const ATask: TDeployTask);
    procedure DoFinish(const AOutput: TDeployTaskOutput; const ATask: TDeployTask);

    class function SafeExec<T>(const AModel: TDeployFilesModel; const AProc: TFunc<T>): T;

    function InternalExecute(const ADeployer: IDeploymentTask): TPyEnvironmentDeployFiles;
  public
    class constructor Create();
    class destructor Destroy();

    constructor Create(const AModel: TDeployFilesModel);
    destructor Destroy(); override;

    class function Execute(const AModel: TDeployFilesModel): TPyEnvironmentDeployFiles;
  end;

implementation

uses
  Vcl.Graphics,
  PyEnvironment.Project.IDE.Deploy.Factory;

{ TEnvironmentProjectDeployManager }

class constructor TEnvironmentProjectDeployManager.Create;
begin
  FMacLock := TObject.Create();
end;

class destructor TEnvironmentProjectDeployManager.Destroy;
begin
  FMacLock.Free();
end;

constructor TEnvironmentProjectDeployManager.Create(const AModel: TDeployFilesModel);
begin
  inherited Create;
  FModel := AModel;
  FMessages := TDictionary<TDeployTask, TTextProgressBarMessage>.Create();
  FMessagesPtr := TDictionary<TDeployTask, pointer>.Create();
end;

destructor TEnvironmentProjectDeployManager.Destroy;
begin
  FMessagesPtr.Free();
  FMessages.Free();
  inherited;
end;

function TEnvironmentProjectDeployManager.GetMessage(
  const ATask: TDeployTask): TTextProgressBarMessage;
begin
  if not FMessages.ContainsKey(ATask) then begin
    Result := TTextProgressBarMessage.Create();
    Result.PrintPercentage := (ATask = TDeployTask.Download);
    FMessages.Add(ATask, Result);
  end;
  FMessages.TryGetValue(ATask, Result);
end;

function TEnvironmentProjectDeployManager.GetMessagePtr(
  const ATask: TDeployTask): pointer;
begin
  FMessagesPtr.TryGetValue(ATask, Result);
end;

procedure TEnvironmentProjectDeployManager.DoStart(const AText: string;
  const ATask: TDeployTask);
var
  LMessage: TTextProgressBarMessage;
  LMessagePtr: pointer;
begin
  LMessage := GetMessage(ATask);

  TThread.Synchronize(nil, procedure() begin
    LMessage.Text := AText;
    LMessagePtr := TPythonMessage.CustomMessage(LMessage);
  end);

  FMessagesPtr.Add(ATask, LMessagePtr);
end;

procedure TEnvironmentProjectDeployManager.DoProgress(
  APercentage: integer; const ATask: TDeployTask);
var
  LMessage: TTextProgressBarMessage;
begin
  LMessage := GetMessage(ATask);

  if LMessage.Percentage < APercentage then
    TThread.Queue(nil, procedure() begin
      LMessage.Percentage := APercentage;
    end);
end;

procedure TEnvironmentProjectDeployManager.DoFinish(
  const AOutput: TDeployTaskOutput; const ATask: TDeployTask);
var
  LMessage: TTextProgressBarMessage;
  LMessagePtr: pointer;
begin
  LMessage := GetMessage(ATask);
  LMessagePtr := GetMessagePtr(ATask);

  TThread.Queue(nil, procedure() begin
    if AOutput.Success then begin
      LMessage.PrintPercentage := false;
      LMessage.Percentage := 0;
      LMessage.Text := '[Complete] - ' + LMessage.Text;
      if not AOutput.Description.IsEmpty and not AOutput.Text.IsEmpty then
        TPythonMessage.ToolMessage(
          AOutput.Description, AOutput.Text, String.Empty, LMessagePtr)
    end else begin
      LMessage.Text := '[Failed] - ' + LMessage.Text;
      if not AOutput.Description.IsEmpty and not AOutput.Text.IsEmpty then
        TPythonMessage.ToolMessage(
          AOutput.Description, AOutput.Text, String.Empty, LMessagePtr)
    end;
  end);
end;

function TEnvironmentProjectDeployManager.InternalExecute(
  const ADeployer: IDeploymentTask): TPyEnvironmentDeployFiles;
begin
  ADeployer.OnStartTask := procedure(ATask: TDeployTask; AMessage: string)
    begin
      DoStart(AMessage, ATask);
    end;

  ADeployer.OnFinishTask := procedure(ATask: TDeployTask; [ref] AOutput: TDeployTaskOutput)
    begin
      DoFinish(AOutput, ATask);
    end;

  ADeployer.OnProgress := procedure(ATask: TDeployTask; APercentage: integer)
    begin
      DoProgress(APercentage, ATask);
    end;

  ADeployer.Execute(Result);
end;

class function TEnvironmentProjectDeployManager.SafeExec<T>(
  const AModel: TDeployFilesModel; const AProc: TFunc<T>): T;
const
  MAC_PLATFORMS = [TPyEnvironmentProjectPlatform.OSX64, TPyEnvironmentProjectPlatform.OSXARM64];
var
  LMacLock: boolean;
begin
  Assert(Assigned(AProc), 'Argument "AProc" not assigned.');

  // MacOSX64 and MacOSXARM64 can't run async, because they share the same bundle
  LMacLock := (AModel.Platform in MAC_PLATFORMS);

  if LMacLock then
    TMonitor.Enter(FMacLock);
  try
    Result := AProc();
    if LMacLock then
      TMonitor.Pulse(FMacLock);
  finally
    if LMacLock then
      TMonitor.Exit(FMacLock);
  end;
end;

class function TEnvironmentProjectDeployManager.Execute(
  const AModel: TDeployFilesModel): TPyEnvironmentDeployFiles;
begin
  Result := SafeExec<TPyEnvironmentDeployFiles>(AModel,
    function(): TPyEnvironmentDeployFiles
    var
      LDeployer: IDeploymentTask;
      LInstance: TEnvironmentProjectDeployManager;
    begin
      LDeployer := TPyEnvironmentProjectDeploySimpleFactory.CreateInstance(AModel);

      if not Assigned(LDeployer) then
        Exit;

      LInstance := TEnvironmentProjectDeployManager.Create(AModel);
      try
        Result := LInstance.InternalExecute(LDeployer);
      finally
        LInstance.Free();
      end;
    end);
end;

end.
