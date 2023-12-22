Python 3 embeddables for Windows, Linux, MacOS and Android
================

These Python builds are provided by the [python3-embeddable](https://github.com/lmbelo/python3-embeddable) and [python3-apple-embeddable](https://github.com/lmbelo/python3-apple-embeddable) project. 

Notes
------------
Android requires the executables to be deployed with the app bundle and must follow the lib__.so naming convention. We send the Python launcher with a special name and create a symlink on bin. The same applies to the interpreter.
iOS requires executables to be deployed as independent frameworks.
Apple docs says to make frameworks for OSX as well, but the extension modules seems to work fine on lib-dynload. We only make framework for the Python interpreter.
Nothing special for Windows or Linux.