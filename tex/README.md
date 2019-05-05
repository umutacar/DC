# DEVELOPMENT
## USAGE
  To compile run the parser 
  `$ build.sh`

  To generate the parser try
  `$ menhir --explain parser.mly`
  This explains the conflicts.
  
  To understand conflicts try
  `$ menhir --dump --explain parser.mly`
  and look into file `parser.conflicts` and `parser.automaton`

  To generate .messages file menhir --list-errors parser.mly > parser.messages

  To compile a particular module try something like this
  ocamlfind ocamlc -package core -c tex2html.ml

## DEBUG
Turn this on to see the various parser steps.
$ export OCAMLRUNPARAM='p'


## OCAML 

### Commands for building using ocamlbuild
$ ocamlbuild -use-ocamlfind -quiet top.native

###  Commands for hand compiling
$ ocamllex lexer.mll
$ ocamlbuild -use-ocamlfind  lexer.ml -quiet lexer.native
$ lexer.native


# Grammar

## Chapters and sections

  We have four levels of sectioning:
  chapter, section, subsection, subsubsection 

  These have to be "properly nested" as in the order above.  For example, "subsubsection" inside "section" is disallowed.

  Each section has the form: 
  X ::= heading + label + blocks + subX
  where X = chapter | section | subsection | subsubsection
  and subX = section | subsection | subsubsection | "nothing"
  respectively

  A "block" is either an "element" or a "cluster".  

  An "element" is an atom or a "flex/group."
 
  A "cluster" is a sequence of elements.
  
 
# OVERALL STRATEGY

Our strategy rests on several observations.

* Latex is a mess.  We are not going to try to parse it. 

* We have learned from DEX experience that it is possible to parse
  latex superficially and translate it to whatever format we want in a
  simple way using whatever third party tools available.
	
* The DEX way works but it can be cumbersome to insist on the DEX
  format.  In this project, we will take a few simple steps to improve
  on DEX.

* One improvement will be to parse something that looks more like LATEX than DEX.

* Another improvement will be to use a better language and parsing
  infrastructure so that we can generate some error messages.

* Our basic solution is to parse latex at the level of "blocks" consisting of chunks of latex separated by special syntactic terminals that we care abou.

# Background on Lexing 

This is from the Dragon book on compilers.  (The book is pretty hard
to read because they don't use types and rely instead of English
descriptions.)

Some terminology:

*Token* is the abstract data type through which lexer and
parser communicate.  For exaple, an identifier is a token, so in an integer etc.

datatype token = Identifier of string
               | Integer of string

*Lexeme* is the string that matches the token, that is the string in the above type.


# Algorithm


## Lexer
  On the lexing side, we don't need to use a formal lexer.
	We will just split the input into lines.

  We well process each line to check if it is a heading and indicate
  if so.  We will then return the line as a token.

  We probably don't need to use a real lexer to do this. It would be
  better to roll our own because it is whitespace sensitive what we
  are trying to do.

## Parser

  On the parsing side, we should use yacc or menhir probably menhir.

  Menhir is an LR(1) parser.

  


# Background on Parsing

LR(1) is a form of shift-reduce parser.

Shift reduce parser is a bottom up parser.  It operates as follows.

* Given a string (input) I, look in I for patterns that match the RHS
  of a production of the form nonterminal -> pattern, e.g.,
  A -> abc

* [Reduce] If there is a match replace patter with the nonterminal, LHS of the production.

* Continue.

One way to implement a shift reduce parser is to use a stack.

* Initially stack contains $ (bottom)

* If the top of the stack matches a RHS of a production rule, reduce
  and replace the matched pannern with the LHS nonterminal

* If not, then shift the next character (left to right order) onto the stack.

It turns out that the stack algorithm is consistent with a rightmost derivation of the string starting from the start sting.  That is we can derive the string by applying the productions to the rightmost nonterminal....

## LR parsing
LR parsing refers to a reading the input from Left to right and producing a Rightmost derivation in reverse.  So it is a bottom up shift reduce parser.

LR parsers are powerful enough for most programming language grammars.  But they require a parser generator to construct, e.g., yacc, menhir, etc.
	
  

