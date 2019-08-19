PDFLATEX = pdflatex
LATEX = latex
FLAG_VERBOSE = -v 
FLAG_DEBUG = -d

PREAMBLE = ./preamble.tex
PANDOC = pandoc --verbose --mathjax -f latex

DC_HOME = ~/DC


LABELTEX = $(DC_HOME)/texel
DC = $(DC_HOME)/dc -meta ./meta

FORCE: 

.PHONY: book html book-www book.pdf 


#	printf '%s\n' '\def\targethtml{}' >> book-html.tex
html: FORCE
	$(PANDOC) -s book-pandoc.tex > book.html
#	rm book-html.tex

%_led.tex : %.tex FORCE
	$(LABELTEX) $<  -o $@


######################################################################
## BEGIN: XML
######################################################################

%.xml : %.tex FORCE
ifdef debug
ifdef verbose
	$(DC) $(FLAG_DEBUG) $(FLAG_VERBOSE)  -meta ./meta -preamble $(PREAMBLE) $< -o $@
 else
	$(DC) $(FLAG_DEBUG) -meta ./meta -preamble $(PREAMBLE)  $< -o $@
endif
else
ifdef verbose
	$(DC) $(FLAG_VERBOSE)  -meta ./meta -preamble $(PREAMBLE) $< -o $@
 else
	$(DC)  -meta ./meta -preamble $(PREAMBLE)  $< -o $@
endif
endif

%.xml : %.md FORCE
ifdef debug
ifdef verbose
	$(DC) $(FLAG_DEBUG) $(FLAG_VERBOSE) $< -o $@
 else
	$(DC) $(FLAG_DEBUG) $< -o $@
endif
else
ifdef verbose
	$(DC) $(FLAG_VERBOSE) $< -o $@
 else
	$(DC) $< -o $@
endif
endif



######################################################################
## END: XML
######################################################################

clean: 
	rm *.aux *.idx *.log *.out *.toc */*.aux */*.idx */*.log */*.out 

reset: 
	make clean; rm *.pdf; rm*.html; rm  *~; rm */*~; rm  \#*\#; rm */\#*\#; 

