PDFLATEX = pdflatex
LATEX = latex

PREAMBLE = ./preamble.tex
PANDOC = pandoc --verbose --mathjax -f latex

DC_HOME = ~/DC


LABELTEX = $(DC_HOME)/texel
DC = $(DC_HOME)/dc -meta ./meta
DC_DBG = $(DC_HOME)/dc.dbg

FORCE: 

.PHONY: book html book-www book.pdf 


#	printf '%s\n' '\def\targethtml{}' >> book-html.tex
html: FORCE
	$(PANDOC) -s book-pandoc.tex > book.html
#	rm book-html.tex

%_led.tex : %.tex FORCE
	$(LABELTEX) $<  -o $@

%.xml : %.tex FORCE
	$(DC) -preamble $(PREAMBLE)  $< -o $@

%.xmldbg : %.tex
	$(DC_DBG) -preamble $(PREAMBLE)  $< -o $@

clean: 
	rm *.aux *.idx *.log *.out *.toc */*.aux */*.idx */*.log */*.out 

reset: 
	make clean; rm *.pdf; rm*.html; rm  *~; rm */*~; rm  \#*\#; rm */\#*\#; 

