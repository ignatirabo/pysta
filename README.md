# Forward symbolic execution for taint analysis in Python

## Installation

### Building

This project is implemented for OCaml 4.14.0, but any version should work. To install OCaml and OPAM you can check the following [link](https://opam.ocaml.org/doc/Install.html).

Also several opam libraries are required:
- `z3` is being used to check propositions,
- `bitv` is used to model security lattices,
- `dune` is the builder, and
- `apron` is a library for abstract domains.
These can be installed with `opam install z3 bitv dune apron`


The project is built using [dune](https://dune.build/):
- to build: `make`, equivalent to `dune build --root src main.exe`.
- to clean: `make clean`, equivalent to `dune --root src clean`.

The link to the executable can be found in the current directory.

It is important to notice that the compilation does not work with bytecode automatically, we need to define a environment path.

### Build issues

There are some issues installing the dependencies of this project, at least with an M1 CPU.
A workaround is specifying `LDFLAGS` and `CFLAGS` manually.
```
env LDFLAGS="-L/opt/local/lib" CFLAGS="-I/opt/local/include" opam install z3
```
This also works for `pyre-ast`

### Pysa (optional)

TBD

## Usage

Most standard usage is
```
./main.exe programs/<program_name>.py
```
This command will analyze the given program and give a number of output traces.

The analyzer will try to automatically check Pysa's database for a starting location.
If the program has no main, it is required that Pysa's database has an entry point.

## Folder structure

- `src/` contains all source OCaml files plus *dune* build files.
- `programs/` contains programs to analyze.

## Limitations

- Our tool is not analyzing Python programs exactly, but a subset.
