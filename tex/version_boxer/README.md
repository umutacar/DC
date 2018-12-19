# Overall Strategy

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

* Our basic solution is to parse latex in a line by line manner.  Glue
  lines into blocks and treat blocks as elements which we translate.

* The syntactic changes that we will require from Latex will be
  minimal.  The files will *look* like LaTeX. But they will have some
  extra newline characters.

* Basically we are taking a MarkDown view of Latex.  

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
	
  

# The grammar
```
// A word
word = non_space^+

// latex env's
heading_begin = \begin{word}
heading_end = \end{word}

// latex headings
heading_chapter = \chapter{word}
heading_section = \section{word}
heading_subsection = \subsection{word}
heading_subsubsection = \subsubsection{word}
heading_paragraph = \paragraph{word}
heading_subparagraph = \subparagraph{word}

// all headings
heading = heading_begin | heading_end | heading_chapter | heading_section | heading_subsection |
          heading_subsubsection | heading_paragraph | heading_subparagraph

// empty lines
empty_lines = \n^+

// possible empty lines
pos_empty_lines = \n^*

// content_line is a number of non-empty non-heading
content_line = non_empty_line_not_heading
content_lines = content_line^+

// content is content_lines with possibly empty linens in between
content = content_lines
        | content_lines + empty_lines + content
				

// an atom is content wrapped in an environment
atom = heading_begin pos_empty_lines content pos_empty_lines heading_end    

atoms = atom^+

subparagraph = subparagraph_str  atoms
paragraph = paragraph_str  [atom | subparagraph]^+
subsubsection = subsubsection_str [atom | paragraph | subparagraph]^+
subsection = subsection_str [atom | paragraph | subparagraph]^+ subsubsection^*
section ..

```