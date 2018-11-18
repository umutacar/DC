# Background on Lexing and Parsing

This is from the Dragon book on compilers.  (The book is pretty hard
to read because they don't use types and rely instead of English
descriptions.)

Some terminology:

*Token* is the abstract data type through which lexer and
parser communicate.  For exaple, an identifier is a token, so in an integer etc.

datatype token = Identifier of string
               | Integer of string

*Lexeme* is the string that matches the token, that is the string in the above type.


# The grammar
```
word = non_space^+

// latex env's
begin_str = \begin{word}
end_str = \end{word}

// latex headings
chapter_str = \chapter{word}
section_str = \section{word}
subsection_str = \subsection{word}
subsubsection_str = \subsubsection{word}
paragraph_str = \paragraph{word}
subparagraph_str = \subparagraph{word}

// all headings
heading = begin_str | end_str | chapter_str | section_str | subsection_str |
          subsubsection_str | paragraph_str | subparagraph_str

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
atom = begin_str pos_empty_lines content pos_empty_lines end_str    

atoms = atom^+

subparagraph = subparagraph_str  atoms
paragraph = paragraph_str  [atom | subparagraph]^+
subsubsection = subsubsection_str [atom | paragraph | subparagraph]^+
subsection = subsection_str [atom | paragraph | subparagraph]^+ subsubsection^*
section ..

```