# Building MTL library
Running  `make` should make a number of executables
1. `texml.native` translates latex to xml
2. `tex2tex.native` translates latex to latex (used for debugging)
3. `texel.native` normalizes latex by "atomizing" contents and labeling the document
4. `mdxml.native` translates markdown to xml
5. `md2md.native` translates markdown to markdown (used for debugging).


The executables `tex2tex.native` and `md2md.native` are used for debugging purposes.  Thy parse LaTeX and Markdown (respectively), create an AST (Abstract Syntax Tree), and then write/return the AST back as code.  The output written/returned back should be the same as the source, modulo comments and some whitespace.  This idempotence helps debug the compiler.


# Note
This code is far from polished.  

