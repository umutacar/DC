# Design

This is a summary of the basic design of the compiler.

## Basic Structure

The compiler has two front-ends one for LaTeX and one for Markdown.  These are translated into an AST (Abstract Syntax Tree), which can be thought as an IL (Intermediate-level Language), which is apart from the "leaves" (which are markdown/latex) is LaTeX.  I favored using LaTeX as an IL to keep things simple and lightweight.  Not doing so would have required inventing new syntax for an IL.  This could be independently interesting but probably is not a significant problem.

## Markdown Grammar

Markdows is pretty straightforward, for now, probably because we don't handle the full markdown.  

## LaTeX Grammar

The grammar is primarily designed to avoid conflicts in the parser.  

For example,  plural items, such as sections, atoms, etc can be tricky.   If a plural item can be empty and it is wrapped by an option, it will lead to conflicts.  I therefore avoid options and allow all plurals to be empty.

The parsing infrastructure is separated into two stages.  First, we use a "top-level"pparser to parse latex into "atoms" and then we use a separate atom lexer and parser atom/atom_lexer and atom/atom_parser) to parse each atom.  The motivation is to reduce complexity. 

The top level parser itself is skinny but relies on a lexer (tex/tex_lexer.mll) that does some fancy look aheads.  The lexer maintains a state machine so that it can identify the beginning and the end of "paragraphs".  The basic idea of the top level lexer is to tokenize the input at the level of 
* chunks: which are non-space fragments of text that could (optionally come with a label),
* spaces: such as horizontal and vertical
* headings and commands such as section names, groups, group commands (fold etc).

Crucially, the top level lexer eliminates all comments to streamline the subsequent phases of the compilation.

Because latex has quite a flexible structure, and allows regions of text to be skipped completely and don't obey the laws of the language, e.g., with  "verbatim/comment/lstinline" commands, at the top level, we have to be quite careful about these.  To this end, we define two kinds of lexing/parsing mechanisms.  
* The "skip" mechanism does not care about comments and all and simply skips regions of text until an enclosing command is found.  This "skip_env" is what we use for comment/verbatim/lstinline environments. Note that this does not allow for nesting.
* The "take" mechanism, used for all other environments,  skips over comments and allows nesting of enironments. Skipping over comments is necessary because otherwise, we could think that the text within comment is significant, e.g. 
```
\begin{definition}
% \end{definition}
Something
\end{definition}
```

The atom-level parser ...

## preambles and tailtexts

  One difficulty in the parser was accommodating text, mainly whitespace, outside the atoms. 

  To solve  this problem, I allow 
   * each "leaf" in the AST tree, which is either an atom/group to have a "preamble" text, and
   * each sequence of atoms/groups (elements), which is called a *block* can have a "tailtext". 
 

## Segments: chapters and sections

*  We have four levels of sectioning:
  chapter, section, subsection, subsubsection

  These don't have to be "properly nested" 

  A segment is a heading followed by a block.

  A block is a sequence of elements with tail text.

  An element is either an atom or a group 

  A group is a preamble text followed by a sequence of atoms and tailtext.
  Note that the tailtext belong to the sequence of atoms.

  An atom is a preamble text followed by \begin{atom}...\end{atom}

## Point System
Note: I seemed to have done some work on developing a point system for Diderot but I have not documented it carefully.  What exists in the PRs is somewhat outdated.  I am starting to create some documentation here by inspecting the code.

It is possible to assign points to atoms like this

\begin{problem}[4p][Title]
...
\end{problem}

\begin{problem}[4pts][Title]
...
\end{problem}

## Problems, Prompts, Cookies

  Each atom can be followed by a problem. The user writes the problem as a sequence of command-body pairs of the form

  \problem_kind Instructions
  list of cookies
  List of prompts

  A prompt is a (\problem_part_kind Instructions
                 followed by             
                 \list of cookies).


  A cookie could look like this
  \hint[optional cost] some hint   
  \explain[optional cost] some explanation
  \sol[optional cost] some solution
  \notes[optional cost] some notes
  \rubric[optional cost] a rubric
  
  The optional costs are not always meaningful.  The idea is that if a student demants a hint, they will pay some cost and this could be specified.
  
  For simplicity, the parser simply parses the list and then processes it to construct the parts.  

## Automatic labeling

The idea is to require the user to follow some discipline in labeling segments such as
ch:chapter_label
sec:section_label
sec:paragraph_label
(encourage using the same prefix, because paragraph can become section later and you don't want to change all the references to the label)
grp:cluster_label
grp:flex_label
(here gr stands for group)

We then generate labels automatically. The generated labels follow the labeling discipline above but if the user's original labels did not follow this approach, then the algorithm works but the auto-generated labels are not uniform with the author's style anymore, which is probably a small annoyance.

The algorithm employs a few heuristics so that the labels can be reasonably good.

We maintain a hash table of all the labels in the chapter and check against it to make sure that all labels are unique.
We try out words from the title (for sections) or title + body for atoms, giving priority to titles.
We avoid some common stop words and diderot keywords.
We avoid contents of comments, latex commands etc, and contents of \label{} and \depend commands.
For groups, which usually don't have a title, and therefore we use the titles and bodies of nested atoms. For the bodies, we take the first 1/2 of each atom's body, for no real good reason other reducing work Note that because of the prefix difference, the same label can be used for an atom and its group and there will be no collision (hence this is not a good reason).
For sections (section/subsection/subsection/paragraph), we expect a title. In the odd case that the title is empty, we look into the titles and bodies of the atoms & groups up until the first nested section if any.
If all this fails, we generate a unique number.
TESTING
Tested it on several 210 chapters. Works surprisingly well.

### Limitation


# JOURNAL

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
	
  

