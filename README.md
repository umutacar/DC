# Guide
A ![guide to Diderot Compiler](https://github.com/diderot-edu/guide)
is available online on github.


# Executable Binaries
Binaries are available for `MacOS` and `Ubuntu` (Linux) under the directory `bin`.  You can also generate these binaries by following the instructions below.

# Building DC


## Compiling DC Binaries.

If you are running MacOS or Linux, you can use the provided binaries under the directory `bin`. But you can also build them yourself after you install OCaml see below.

Running  `make` should make a number of executables
0. `texml.native` translates latex to xml
1.  Depracated but kept for backward compatibility `texml.native` translates latex to xml
2. `tex2tex.native` translates latex to latex (used for debugging)
3. `texel.native` normalizes latex by "atomizing" contents and labeling the document
4. `mdxml.native` translates markdown to xml
5. `md2md.native` translates markdown to markdown (used for debugging).


The executables `tex2tex.native` and `md2md.native` are used for debugging purposes.  Thy parse LaTeX and Markdown (respectively), create an AST (Abstract Syntax Tree), and then write/return the AST back as code.  The output written/returned back should be the same as the source, modulo comments and some whitespace.  This idempotence helps debug the compiler.


## Installation

Basic installation instruction follow.  Compiling DC requires a relatively Basic OCAML installation and the utility `make`.

### Installing OCaml compiler
1. Install [OPAM](https://opam.ocaml.org/doc/Install.html)
  - easiest way: `sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)`
2. Install [bubblewrap](https://github.com/projectatomic/bubblewrap)
  - On 16.04, `dwayne`'s instructions [here](https://github.com/ocaml/opam/issues/3424) worked well:
    - download [http://security.ubuntu.com/ubuntu/pool/main/b/bubblewrap/bubblewrap_0.2.1-1ubuntu0.1_i386.deb](http://security.ubuntu.com/ubuntu/pool/main/b/bubblewrap/bubblewrap_0.2.1-1ubuntu0.1_i386.deb)
    - `sudo dpkg -i bubblewrap_0.2.1-1ubuntu0.1_i386.deb`
  - It looks like other distros/versions have it in the repos instead
3. Remaining steps copied from the OPAM section on the [OCaml website](https://ocaml.org/docs/install.html):
  - `opam init`
  - ``eval `opam env` ``
  - `opam switch create 4.06.1`
  - ``eval `opam env` ``
  - make sure it worked: `which ocaml` and `ocaml -version`

### Installing packages for DC
Assuming you got OPAM as described above:
- `opam install core getopt menhir re2`
and add  -package ppx_fields_conv
to the Makefile

### Ocaml interpreter
Place the following in your root directory file .ocamlinit

```
#use "topfind";;
#thread;;
#camlp4o;;
#require "core.top";;
#require "core.syntax";;
```

### latex parser for identifying implicit references
To use the latex parser, run 'python tex_parser.py <path_to_latex_file>'. This will identify the key phrases in the given latex file and write the key phrase, label pairs to a text file with the same name as the latex file.
