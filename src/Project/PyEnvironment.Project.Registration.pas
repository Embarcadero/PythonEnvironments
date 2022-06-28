unit PyEnvironment.Project.Registration;

interface

procedure Register();

implementation

uses
  PyEnvironment.Project.Menu;

procedure Register();
begin
  TPyEnvironmentProjectMenuCreatorNotifier.Register();
  TPyEnvironmenCompileNotifier.Register();
end;

end.
