# Building the library
Running  `make` should make two executables
1. `tex2tex.native`
2. `tex2xml.native`

The executable `tex2xml.native` generates xml code from latex.  

The executable `tex2tex.native` is primarily used for debugging purposes.  It parses latex and creates its AST (Abstract Syntax Tree) and then writes/returns it back.  The output written/returned back should be exactly the same as the source.  This idempotence is used the debug the parser (front ent of the transpiler).


# Important 
This code is far from polished. 


# Contents
The new ocaml implementation of MeTaL consists only of two directories
 
1. `xml/`
2. `tex/`

The rest of the directories are python stuff and should be depracated and removed overe time.