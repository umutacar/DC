# Diderot Usage and Examples

# Installation

  To use Diderot install version 2.7.2.
  Problem: how? 
  Info here, but no install package.
  https://pandoc.org/releases.html
 [pandoc](https://pandoc.org/ "pandoc")

# Examples 
  See directories `book` (a book with parts and chapters) and `booklet` (with chapters and no parts) for examples.


# Binaries
  There are three separate tools that are available to the user

## Tool: texmlt
This tools translates the given input LaTeX file to xml.

Example: texmlt -meta ./meta input_file.tex -o output_file.xml 


## Tool: tex2tex
This tools reads in your LaTeX sources, parses them, and writes it back.  It drops comments and normalized the whitespace but shold otherwise return back a LaTeX file that is essentially the same as the input file. 

Example: Try

$ bin/tex2tex ./graph-contraction/star.tex -o ./s.tex
$ diff ./graph-contraction/star.tex ./s.tex
  
## Tool: texel
This tool "normalizes" your latex sources.  This means that it gives each segment of the input file a label and it wraps each atom into a "group" if it is not already in a group.  A group is one of "cluster" "flex" "mproblem" (multipart problem).  

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
```

Here `elements` is a sequence of "atoms" and "flex'es" as
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
There are many atoms to choose from.  Here is a complete list.  Let me (umut@cs.cmu.edu) know if you want others.

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
* `gram`
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


## Labels

Labels play an important role in Diderot, because they allow identifying atoms uniquely.  Try to give a label to each atom, flex, section, subsection...

Important: All labels in a book must be unique.  I recommended  giving each chapter a unique label, and prepending each label with that of the chapter, e.g.,
```
\chapter{Introduction}
\label{ch:intro}

\begin{preamble}
\label{prml:intro::preamble}
...
\end{preamble}

\section{Overview}
\label{sec:intro::overview}

\begin{gram}
\label{grm:intro::present}
In this  section, we present...
\end{gram}

```

The following label prefixes are recommended.  You can ask MTL to generate labels for you (this feature will soon be coming).  In doing so, MTL will use the following prefixes for each kind of atom

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

* Fancy packages will not work.

* For figures specify the width/height in terms of concrete measures, e.g.,
  width = 4in

* We use mathjax to math environments.  This works in many cases, especially for AMS Math consistent usages.  There are a few important caveats. 

 - Once you switch to math, try to stay in math.  You can switch to text mode using \mbox{} but if you use macros inside mbox, they might not work (because mathjax don't know about your text macros).

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
The relevant files are 

* `templates/diderot.sty`

   Supplies diderot definitions needed for compiling latex to pdf's.

* `templates/preamble.tex` 

   Supplies various macros that will be used by generating a pdf via pdflatex.  Nearly all packages and macros should be included here.  Each chapter will be compiled in the context of this file.  Ideally this file should
   - include as few packagase as possible
   - define no environment definitions; environments  shoud  be supplied by diderot.sty
   - macros should be simple

* `templates/preamble-mtl.tex` 

   Equivalent of preamble.tex but it is customized for hmtl output.  This usually means that most macros will remain the same but some will be simplified to work with pandoc.  If you don't need to customize, you can keep just one preamble.  The example in directory `booklet` does so.
    

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




