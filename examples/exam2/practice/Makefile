PREFIX:=exam
FILE:=$(PREFIX).tex

pdf: $(FILE)
	pdflatex --jobname="$(PREFIX)" '\newcommand{\ANS}{}\input{$(FILE)}'
	pdflatex --jobname="$(PREFIX)" '\newcommand{\ANS}{}\input{$(FILE)}'
	pdflatex --jobname="$(PREFIX)" '\newcommand{\ANS}{}\input{$(FILE)}'
	pdflatex --jobname="$(PREFIX)" '\newcommand{\ANS}{}\input{$(FILE)}'

sol: $(FILE)
	pdflatex --jobname="$(PREFIX)-sol" '\newcommand{\ANS}{\printanswers}\input{$(FILE)}'
	pdflatex --jobname="$(PREFIX)-sol" '\newcommand{\ANS}{\printanswers}\input{$(FILE)}'
	pdflatex --jobname="$(PREFIX)-sol" '\newcommand{\ANS}{\printanswers}\input{$(FILE)}'
	pdflatex --jobname="$(PREFIX)-sol" '\newcommand{\ANS}{\printanswers}\input{$(FILE)}'
clean:
	rm -f $(PREFIX)-sol.aux
	rm -f $(PREFIX)-sol.log
	rm -f $(PREFIX)-sol.pdf
	rm -f $(PREFIX).aux
	rm -f $(PREFIX).log
	rm -f $(PREFIX).pdf
	rm -f *~
