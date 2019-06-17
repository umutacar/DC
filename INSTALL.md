# Anecdotal install steps on Ubuntu 16.04 #

 Starting with no OCaml experience.

 ## Installing OCaml compiler ##
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

 ## Installing packages for MeTaL ##
Assuming you got OPAM as described above:
- `opam install core getopt menhir re2`
and add  -package ppx_fields_conv
to the Makefile

## Ocaml interpreter
Place the following in your root directory file .ocamlinit

```
#use "topfind";;
#thread;;
#camlp4o;;
#require "core.top";;
#require "core.syntax";;
```

 ## Installing MeTaL ##
- clone this repo and run `make` in the `MeTaL` directory
- The executable `texmlt` or `texmlt.native` generates xml from latex.
  Example usage from within the root of MeTaL.
  ```
  $./texmlt examples/genome.tex -preamble examples/preamble.tex -o genome.xml
  ```
- The executable `texel` or `texel.native` elaborates the input latex file in several ways.
  ```
  $./texel examples/genome.tex -o genome_elaborated.xml


 #### Disabling debug output ####
If you get lots of debug output when compiling to xml, (eg: `!lexer matched` and `!found word:`) you can disable debugging output to get rid of it
- edit the file `MeTaL/pervasives/utils.ml`
  - find the line `let debug = true` and change it to `let debug = false`
  - re-run `make` in the `MeTaL` directory and copy the file `tex2xml.native` to wherever you want it