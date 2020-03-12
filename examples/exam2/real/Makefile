PREFIX:=exam
FILE:=$(PREFIX).tex

pdf: $(FILE)
	pdflatex --jobname="$(PREFIX)" '\newif\ifrubric\rubricfalse\newif\iffep\fepfalse\newcommand{\ANS}{}\input{$(FILE)}'
	pdflatex --jobname="$(PREFIX)" '\newif\ifrubric\rubricfalse\newif\iffep\fepfalse\newcommand{\ANS}{}\input{$(FILE)}'


sol: $(FILE)
	pdflatex --jobname="$(PREFIX)-sol" '\newif\ifrubric\rubricfalse\newif\iffep\feptrue\newcommand{\ANS}{\printanswerstrue}\input{$(FILE)}'
	pdflatex --jobname="$(PREFIX)-sol" '\newif\ifrubric\rubricfalse\newif\iffep\feptrue\newcommand{\ANS}{\printanswerstrue}\input{$(FILE)}'


rubric: $(FILE)
	pdflatex --jobname="$(PREFIX)-sol" '\newif\ifrubric\rubrictrue\newif\iffep\feptrue\newcommand{\ANS}{\printanswerstrue}\input{$(FILE)}'
	pdflatex --jobname="$(PREFIX)-sol" '\newif\ifrubric\rubrictrue\newif\iffep\feptrue\newcommand{\ANS}{\printanswerstrue}\input{$(FILE)}'


clean:
	rm -f $(PREFIX)-sol.aux
	rm -f $(PREFIX)-sol.log
	rm -f $(PREFIX)-sol.pdf
	rm -f $(PREFIX).aux
	rm -f $(PREFIX).log
	rm -f $(PREFIX).pdf
	rm -f *~
