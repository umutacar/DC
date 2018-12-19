%token EOF

%token <string> TEXT

%token B_CHAP
%token E_CHAP
%token B_SECT
%token E_SECT
%token B_SUB
%token E_SUB
%token B_SUBSUB
%token E_SUBSUB
%token B_GRP
%token E_GRP
%token B_GRM
%token E_GRM
%token B_DFN
%token E_DFN
%token B_EXMPL
%token E_EXMPL

%start <Syntax.block list> blocks
%%

blocks:
  | EOF                   { [] }
  | b = block bs = blocks { b::bs }
  ;

block:
  | B_CHAP    c = contents  E_CHAP    { Syntax.Chapter c }
  | B_SECT    c = contents  E_SECT    { Syntax.Section c }
  | B_SUB     c = contents  E_SUB     { Syntax.Subsection c }
  | B_SUBSUB  c = contents  E_SUBSUB  { Syntax.Subsubsection c }
  | B_GRP     c = contents  E_GRP     { Syntax.Group c }
  | B_GRM     t = body      E_GRM     { Syntax.Atom (Syntax.Gram t) }
  | B_DFN     t = body      E_DFN     { Syntax.Atom (Syntax.Definition t) }
  | B_EXMPL   t = body      E_EXMPL   { Syntax.Atom (Syntax.Example t) }
  ;

body:
  |                   { "" }
  | t = TEXT b = body { t ^ "\n" ^ b }

contents:
  |                         { [] }
  | t = TEXT   c = contents { Syntax.Content t::c }
  | b = block  c = contents { b::c }
  ;
