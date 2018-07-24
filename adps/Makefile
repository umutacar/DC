PDFLATEX = pdflatex
LATEX = latex
PANDOC = pandoc


default: book

book.pdf: 
	$(PDFLATEX) book.tex

book.html: 
	$(PANDOC) -f latex --mathjax -s book.tex > book.html

clean: 
	rm *.aux *.idx *.log *.out *.toc */*.aux */*.idx */*.log */*.out 

reset: 
	make clean; rm *.pdf; rm  *~; rm */*~; rm  \#*\#; rm */\#*\#
