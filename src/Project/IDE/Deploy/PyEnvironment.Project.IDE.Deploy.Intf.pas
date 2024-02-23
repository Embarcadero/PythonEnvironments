(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Project.IDE.Deploy.Intf'                  *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:         https://github.com/Embarcadero/PythonEnviroments *)
(**************************************************************************)
(*  Functionality:  Define interfaces for Deployment classes              *)
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
unit PyEnvironment.Project.IDE.Deploy.Intf;

interface

uses
  PyEnvironment.Project.IDE.Types;

type
  IDeploymentTask = interface
    ['{79448D47-A73E-42F5-B15E-7AE24C800152}']
    function GetStartTaskCallback: TDeployTaskStartCallback;
    procedure SetStartTaskCallback(Value: TDeployTaskStartCallback);
    function GetFinishTaskCallback: TDeployTaskFinishCallback;
    procedure SetFinishTaskCallback(Value: TDeployTaskFinishCallback);
    function GetProgressCallback: TDeployTaskProgressCallback;
    procedure SetProgressCallback(Value: TDeployTaskProgressCallback);

    function ShouldDownload(): boolean;
    function ShouldMake(): boolean;

    function Download(const AInput: TDeployTaskInput): TDeployTaskOutput;
    function Make(const AInput: TDeployTaskInput): TDeployTaskOutput;
    function Deploy(const AInput: TDeployTaskInput): TDeployTaskOutput;
    function Clean(const AInput: TDeployTaskInput): TDeployTaskOutput;

    /// <summary>
    /// Execute all operations.
    /// </summary>
    function Execute(out AFiles: TPyEnvironmentDeployFiles): boolean;

    property OnStartTask: TDeployTaskStartCallback read GetStartTaskCallback write SetStartTaskCallback;
    property OnFinishTask: TDeployTaskFinishCallback read GetFinishTaskCallback write SetFinishTaskCallback;
    property OnProgress: TDeployTaskProgressCallback read GetProgressCallback write SetProgressCallback;
  end;

implementation

end.
