unit PyEnvironment.Project.IDE.Registration;

interface

procedure Register();

implementation

uses
  PyEnvironment.Project.IDE.Menu;

procedure Register();
begin
  TPyEnvironmentProjectMenuCreatorNotifier.Register();
  TPyEnvironmenCompileNotifier.Register();
end;

end.
