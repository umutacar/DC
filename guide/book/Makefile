PDFLATEX = pdflatex
LATEX = latex
WWW-BOOK = ~/Google\ Drive/algorithms-book

PREAMBLE = ./templates/diderot-preamble.tex
# resource paths don't seem to work
PANDOC = pandoc --verbose --mathjax -f latex

ifeq ($(OS),Windows_NT)
	MTL_HOME = ../../mtl/bin/windows
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Linux)
		MTL_HOME = ../../mtl/bin/ubuntu
	endif
	ifeq ($(UNAME_S),Darwin)
		MTL_HOME = ../../mtl/bin/macos
	endif
endif

TEX2XML = $(MTL_HOME)/tex2xml
TEX2XMLDBG = $(MTL_HOME)/tex2xml.dbg

default: book

FORCE: 

.PHONY: book html book-www book.pdf 


%.xml : %.tex FORCE
	$(TEX2XML) $< $(PREAMBLE) $@

%.xmldbg : %.tex
	$(TEX2XMLDBG) $< $(PREAMBLE) $@

clean: 
	rm *.aux *.idx *.log *.out *.toc */*.aux */*.idx */*.log */*.out 

reset: 
	make clean; rm *.pdf; rm*.html; rm  *~; rm */*~; rm  \#*\#; rm */\#*\#; 



######################################################################
## PDFs


book:
	$(PDFLATEX) --jobname="book" '\input{book}' ; 
	$(PDFLATEX) --jobname="book" '\input{book}' ; \


book.pdf: book

## Probability
probability: book probability/theory.tex probability/randomvars.tex probability/expectation.tex
	$(PDFLATEX) --jobname="probability" '\includeonly{probability/part, probability/theory, probability/randomvars, probability/expectation} \input{book}'

## Graph-Contraction
graph-contraction: book graph-contraction/contraction.tex  graph-contraction/edge.tex  graph-contraction/star.tex graph-contraction/connectivity.tex

	$(PDFLATEX) --jobname="graph-contraction" '\includeonly{graph-contraction/part, graph-contraction/introduction, graph-contraction/edge, graph-contraction/star, graph-contraction/connectivity} \input{book}'

