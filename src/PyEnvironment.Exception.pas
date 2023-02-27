unit PyEnvironment.Exception;

interface

uses
  System.SysUtils;

type
  EInvalidFileStructure = class(Exception);

  EEmbeddableNotFound = class(Exception);

  EPipSetupFailed = class(Exception);

  EFileNotFoundException = System.SysUtils.EFileNotFoundException;

  EDirectoryNotFoundException = System.SysUtils.EDirectoryNotFoundException;

  ESymlinkFailed = class(Exception);

implementation

end.
