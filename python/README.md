Python 3 embeddables for Windows, Linux, MacOS and Android
================

These Python builds are provided by the [python3-embeddable](https://github.com/lmbelo/python3-embeddable) project. 

Notes
------------
Android requires the executables to be deployed with the app bundle and must follow the lib__.so naming convention.
MacOS requires the executables to be deployed with the app bundle.

Tips
------------
1) Download the Python embeddables artifacts generated by the [CI](https://github.com/lmbelo/python3-embeddable/actions), then run the "extract_deliverables.py" script to extract the required files and create the folder structure;
2) Run the "provide_deliverables.py" script to create the Delphi code that generates the deployment automation. A new file called "deliverables_cmds.txt" will be created. Copy it into PyEnvironment.Project.IDE.Deploy.TPyEnvironmentProjectDeploy.Create;
3) Run the "generate_rc.py" script to generate the .rc files and create the folder structure. Copy it under resources. Remove all them from the Delphi project and add the new ones.