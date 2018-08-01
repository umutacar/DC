PDFLATEX = pdflatex
LATEX = latex

# resource paths don't seem to work
PANDOC = pandoc --verbose --resource-path=.:preliminaries:./preliminaries/media


default: book.html

book.pdf: FORCE
	$(PDFLATEX) book.tex

book.html: FORCE
	$(PANDOC) -f latex --mathjax -s book_pandoc.tex > book.html

clean: 
	rm *.aux *.idx *.log *.out *.toc */*.aux */*.idx */*.log */*.out 

reset: 
	make clean; rm *.pdf; rm  *~; rm */*~; rm  \#*\#; rm */\#*\#

FORCE: 
