# adataptempo
This project is a port of [C++ TapTempo](https://github.com/moleculext/taptempo/) in Ada.

The purpose is only educational to show the differences between C++ code and Ada on a same project.

## dependencies

This project only depends on [Parse_Args](https://github.com/jhumphry/parse_args/) for arguments parsing.
It might also depend on [ZanyBlue](https://sourceforge.net/projects/zanyblue/files/) in the future for I18N.

## Building the code

To build the project, you need you checkout the code from _Parse_Args_ and then use _gprbuild_ like this

```bash
gprbuild -aP PATH_TO_PARSE_ARGS
```
## Using it

Just launch it !

```bash
bin/adataptempo
```

Just add the _h_ option parameter to get the help.
