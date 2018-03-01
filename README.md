# adataptempo
This project is a port of [C++ TapTempo](https://github.com/moleculext/taptempo/) in Ada.

The purpose is only educational to show the differences between C++ code and Ada on a same project.

## dependencies

This project depends on [Parse_Args](https://github.com/jhumphry/parse_args/) for arguments parsing and [ZanyBlue](https://sourceforge.net/projects/zanyblue/files/) for i18n.

## Building the code

To build the project, you need you checkout the code from _Parse_Args_, get the code from _ZanyBlue_ and then use _gprbuild_ like this

```bash
export ADA_PROJECT_PATH=PATH_TO_PARSE_ARGS:PATH_TO_ZANY_BLUE_GPR
make
```
## Using it

Just launch it !

```bash
bin/adataptempo
```

Just add the _h_ option parameter to get the help.
