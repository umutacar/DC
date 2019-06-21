# Typesetting with LaTex and MTL (MeTaL)

MTL tries to remain compatible with LaTeX.  The basic idea is that if you have a LaTeX source that you are able to compile and generate PDF from, then you should be able to use MTL to compile your LaTeX source and generate XML.  The resulting XML can then be uploaded to Diderot to obtain an interactive book.


## Examples 
  See directories `book` (a book with parts and chapters) and `booklet` (with chapters and no parts) for examples diderot chapters.

## Basic syntax 

  The basic syntax is LaTeX like.  The key difference is that the content is organized as "elements" which are "atoms" or "flex's".

```
  \chapter{Introduction}
  \label{ch:intro}  % Chapters must have a label.
   
  \begin{preamble}
  \label{intro::preamble} % Optional but recommended atom label.
   ...
  \end{preamble}

  \section{Overview}
  \label{sec:intro::overview} % Optional but recommended section label.   
  <elements>

  \subsection{An Example}
  \label{sec:intro::overview::example} % Optional but recommended section label.   

  <elements>

  \subsubsection {...}
  \label...    
  <elements>

  \paragraph {...}
  <elements>

```

Here `elements` is a sequence of "atoms" and "flex'es" as

An atom is either a plain paragraph or a paragraph consisting of a single environment of the form
```
\begin{<atom>}
optional but highly recommended: \label{atom-label}
<atom body>
\end{<atom>}
```

or  
```
\begin{flex}
\begin{<atom>}[optional title]
\label{atom-label}


\end{<atom>}

\begin{<atom>}[optional title]
\label{atom-label}


\end{<atom>}

<... additional atoms if desired>
\end{flex}
```  

## Atoms

An atom is either plain text paragraph or it is a paragraph that is a  special latex environment. Note that atoms are defined by "vertical white spaces", i.e., they are single standing paragraphs.  White space therofere matters, though in the common case, this goes along with your intuition.\

In addition to paragraphs, there are many atoms to choose from.  Here is a complete list.  Let me (umut@cs.cmu.edu) know if you want others.

* `algorithm`
* `assumption`
* `code`
* `corollary`
* `costspec`
* `datastr`
* `datatype`
* `definition`
* `example`
* `exercise`
* `hint`
* `important`
* `lemma`
* `note`
* `gram`  (non descript atom, i.e., a paragraph)
* `preamble` (only as the first atom of chapter)
* `problem`
* `proof`
* `proposition`
* `remark`
* `reminder`
* `solution`
* `syntax`
* `task`
* `teachask`
* `teachnote`
* `theorem`

Currently, we only allow you to use these atoms.  This means that if you have a paragraph that starts with anything else, you will likely encounter an error.  For example 

```
\begin{thm}
...
\end{thm}
```

is not a legitimate atom.  Currently, we expect you to wrap this with another atom, or don't make it aragraph,  For example,

```
The following theorem...
\begin{thm}
\end{thm}
```
or

```
\begin{gram}
\begin{thm}
\end{thm}
\end{gram}
```


## Labels

Labels play an important role in Diderot, because they allow identifying atoms uniquely.  Try to give a label to each atom, flex, section, subsection...

Important: All labels in a book must be unique.  Diderot generates labels for all atoms even if you don't give them one; see the tool `texel`.

I recommended  giving each chapter a unique label, and prepending each label with that of the chapter, e.g.,
```
\chapter{Introduction}
\label{ch:intro}

\begin{preamble}
\label{prml:intro::preamble}
...
\end{preamble}

\section{Overview}
\label{sec:intro::overview}


Here is a paragraph atom without a label. 


\begin{gram}
\label{grm:intro::present}
In this  section, we present...
\end{gram}



Here is another paragraph atom, consisting of two environments:
\begin{itemize}
...
\end{itemize}
\begin{enumerate}
...
\end{enumerate}

```

The following label prefixes are recommended.  You can ask MTL to generate labels for you, using the `texel` tool.  In doing so, MTL will use the following prefixes for each kind of atom

```
algorithm : "alg"
assumption : "asm"
code : "cd"
corollary : "crl"
costspec : "cst"
datastr : "dtstr"
datatype : "adt"
definition : "def"
example : "xmpl"
exercise : "xrcs"
hint : "hint"
important : "imp"
lemma : "lem"
note : "nt"
gram : "grm"
preamble : "prmbl"
problem : "prb"
proof : "prf"
proposition : "prop"
remark : "rmrk"
reminder : "rmdr"
slide : "slide"
solution : "sol"
syntax : "syn"
task : "tsk"
theorem : "thm"
```

## Label references

Use 
```
\href{label}{ref text}
```
for references or  the standard 
```
\ref{label}.
```
We replace the former with `\hyperref[][]` command so that we can get proper  linked refs is latex/ pdf.


## The rest

There is not really much else to it.  There are some caveats.

* For XML translation work, the chapter should be compileable to PDF.

* Fancy packages will not work.  Stick to basic latex and AMS Math packages.

* For figures specify the width/height in terms of concrete units, e.g.,
  width = 4in, height = 8cm.

* We use mathjax to math environments.  This works in many cases, especially for AMS Math consistent usages.  There are a few important caveats. 

 - Once you switch to math, try to stay in math.  You can switch to text mode using \mbox{} but if you use macros inside mbox, they might not work (because mathjax don't know about your macros).

 -  The "tabular" environment does not work in MathJax.  Use "array" instead.

 -  The environment 
    ```
    \begin{alignat} 
    ... 
    \end{alignat}
    ```
 
    should be wrapped with `\htmlmath', e.g.,
    ```
     \htmlmath{
     \begin{alignat} 
     ... 
     \end{alignat}
     }
    ``` 

* You can use itemize and enumerate in their basic form.  Changing label format with enumitem package and similar packages do not work.  You can imitate these by using heading for your items.  

* In general labeling and referencing is relatively limited to atoms.  You can label atoms and refer to them, but you cannot label codelines, items in lists, etc.

  

# Compiling

The following instructions are tested on Mac OS X and Ubuntu.  The binaries in `bin` might not work on systems that are not Mac or Linux/Unix-like. 

## Overview

See as examples the directories `book` and `booklet`.

The relevant files are 

* `templates/diderot.sty`

   Supplies diderot definitions needed for compiling latex to pdf's.
   You don't need to modify this file.

* `templates/preamble.tex` 

   Supplies your macros that will be used by generating a pdf via pdflatex.  Nearly all packages and macros should be included here.  Each chapter will be compiled in the context of this file.  Ideally this file should
   - include as few packagase as possible
   - define no environment definitions
   - macros should be simple

* `templates/preamble-mtl.tex` 

   Equivalent of preamble.tex but it is customized for hmtl output.  This usually means that most macros will remain the same but some will be simplified to work with `pandoc`.  If you don't need to customize, you can keep just one preamble.  The example in directory `booklet` does so.
    

## Structuring the book

### Booklets
 
  Booklets are books that don't have parts. For these  I recommend creating one directory per chapter and placing a single main.tex file to include all contain that you want.  Don't use \input's within the tex files.  Place all media (images, videos etc) under a media/ subdirectory. 
  
   ch1/main.tex
   ch1/media/: all my media files, *.png *.jgp, *.graffle, etc.
   ch2/main.tex
   ch2/media/: all my media files for chapter 2, *.png *.jgp, *.graffle, etc.
   ch3/main.tex

## Books
   Books have parts and chapters. I recommend structuring these as follows.

   part1/ch1.tex
   part1/ch2.tex
   part1/media-ch1/
   part1/media-ch2/
   part2/ch3.tex
   part2/ch4.tex
   part2/ch5.tex
   part2/media-ch3/
   part2/media-ch4/
   part2/media-ch5/

   
## Making PDF of the whole book

```
$ make book.pdf
```

## Making PDF a specific chapter

  * Extend book.tex to include the chapter
  * Extend Makefile, follow example.

To compile ch2 type

```
$ make ch2
```

## Making XML of a specific chapter

```
$ make ch2/main.xml
```

Error messages from the XML translator are terrible.  They have not been well developed.  But, if you are able to generate a PDF, then you should be able to generate an XML. If you encounter a puzzling error try the "debug" version which will give you an idea of where it blew up.   

```
$ make ch2/main.xmldbg
```

# Usage


## Binaries
  There are three separate tools that are available to the user

### Tool: texmlt  (read "tech-melt")
This tools translates the given input LaTeX file to xml.

Example: `texmlt -meta ./meta input_file.tex -o output_file.xml`

The meta direcotry contains some files that may be used in the xml translation.  You can ignore this to start with.

### Tool: texmlt.dbg 
This tools is the "debug" version of the texmlt binary above. As you might notite, `texmlt` doesn't currenty give reasonable error messages.  The debug version prints out the text that it parses, so you can have some sense of where things have gone wrong.  As you will learn below, `texmlt` should work if your latex sources are otherwise correct (you can run them through pdflatex), so hopefully, you will not have to use this binary much.  

Example: `texmlt -meta ./meta input_file.tex -o output_file.xml `


### Tool: tex2tex
This tools reads in your LaTeX sources, parses them, and writes it back.  It drops comments and normalized the whitespace but should otherwise return back a LaTeX file that is essentially the same as the input file.   You should not need to use this binary, which is primarily used for testing during development.

Examples: 
```
$ bin/tex2tex ./graph-contraction/star.tex -o ./s.tex
$ diff ./graph-contraction/star.tex ./s.tex
```
### Tool: texel
This tool "normalizes" your latex sources.  This means that it

* atomizes your code, wrapping each paragraph into a non-descript "gram" atom if it is not already wrapped.

* wraps each atom by a "group", if not already wrapped by one.

* gives each segment (section, subsection, subsubsection, paragraph, atom) of the input file a label and it wraps each atom into a "group" if it is not already in a group.  A group is one of "cluster" "flex" "mproblem" (multipart problem).  

Generated labels have the form 
```
kind_prefix:chapter_label:segment_label
```
Here kind_prefix could for exmaple be
* `sec`, for section, subsection, subsubsection, paragraph
* `xmpl`, `thm`, for an example or a theorem.

The chapter_label is extracted from the chapter label given.  For exmaple, if the label has any one of the form 
```
ch:star | chapter:star | ch_star | ch__star | ch:_star | chap:_star
```
chapter_label will be `star`.

The tool takes the label, split it at the delimiters [:_]+ and if the prefix starts with "ch" it take the rest of the label as the chapter label.
 
Some exmaple full labels:
* xmpl:star:simpleexample
* thm:star:costbound



