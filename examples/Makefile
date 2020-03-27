PDFLATEX = pdflatex
LATEX = latex
FLAG_VERBOSE = -v 
FLAG_DEBUG = -d


######################################################################
## BEGIN: DIDEROT SETUP


# Import course information
LABEL_COURSE="CMU:Pittsburgh, PA:15210:Spring:2019-20"

# Label for textbook
LABEL_TEXTBOOK="EXAMS"

GUIDE_DIR = ../../diderot/diderot-guide
DIDEROT_ADMIN = ../../diderot/diderot-cli/diderot_admin
DIDEROT_ADMIN = ../../diderot/diderot-cli/diderot_admin --url http://dev-umut33.bryy7bqk8b.us-east-1.elasticbeanstalk.com
DIDEROT_ADMIN = ../../diderot/diderot-cli/diderot_admin --url https://staging.diderot.dev
# DIDEROT_ADMIN = ../../diderot/diderot-cli/diderot_admin --url http://127.0.0.1:8000/



## END: DIDEROT SETUP
######################################################################

# Set up some variables
NO=0
PART_NO=0
FILE=""
ATTACH="does_not_exist"

# Setup local files
PREAMBLE = ./preamble.tex
PREAMBLE = ./templates/preamble-exam.tex
PANDOC = pandoc --verbose --mathjax -f latex

DC_HOME = ~/DC
LABELTEX = $(DC_HOME)/texel
DC = $(DC_HOME)/dc -meta ./meta

FORCE: 

.PHONY: book html book-www book.pdf 


clean: 
	rm *.aux *.idx *.log *.out *.toc */*.aux */*.idx */*.log */*.out 

reset: 
	make clean; rm *.pdf; rm*.html; rm  *~; rm */*~; rm  \#*\#; rm */\#*\#; 


#	printf '%s\n' '\def\targethtml{}' >> book-html.tex
html: FORCE
	$(PANDOC) -s book-pandoc.tex > book.html
#	rm book-html.tex

%_led.tex : %.tex FORCE
	$(LABELTEX) $<  -o $@


######################################################################
## BEGIN: XML

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


## END: XML
######################################################################


######################################################################
## BEGIN: Diderot API


upload: $(FILE).xml
ifdef ATTACH
	$(DIDEROT_ADMIN) upload_chapter $(LABEL_COURSE) $(LABEL_TEXTBOOK) --chapter_number $(NO) --xml $(FILE).xml --xml_pdf $(FILE).pdf --attach $(ATTACH)
else 
	$(DIDEROT_ADMIN) upload_chapter  $(LABEL_COURSE) $(LABEL_TEXTBOOK) --chapter_number $(NO) --xml $(FILE).xml --xml_pdf $(FILE).pdf
endif

upload_xml: 
	-$(DIDEROT_ADMIN) create_chapter $(LABEL_COURSE) $(LABEL_TEXTBOOK) --number $(NO)
	$(DIDEROT_ADMIN) upload_chapter $(LABEL_COURSE) $(LABEL_TEXTBOOK) --chapter_number $(NO) --xml $(FILE).xml

upload_xml_attach: 
	-$(DIDEROT_ADMIN) create_chapter $(LABEL_COURSE) $(LABEL_TEXTBOOK) --number $(NO)
	$(DIDEROT_ADMIN) upload_chapter $(LABEL_COURSE) $(LABEL_TEXTBOOK) --chapter_number $(NO) --xml $(FILE).xml --attach $(ATTACH)

upload_xml_pdf: 
	-$(DIDEROT_ADMIN) create_chapter $(LABEL_COURSE) $(LABEL_TEXTBOOK) --part $(PART_NO) --number $(NO)
	$(DIDEROT_ADMIN) upload_chapter $(LABEL_COURSE) $(LABEL_TEXTBOOK) --chapter_number $(NO) --xml $(FILE).xml --xml_pdf $(FILE).pdf

upload_xml_pdf_attach: 
	-$(DIDEROT_ADMIN) create_chapter $(LABEL_COURSE) $(LABEL_TEXTBOOK) --part $(PART_NO) --number $(NO)
	$(DIDEROT_ADMIN) upload_chapter $(LABEL_COURSE) $(LABEL_TEXTBOOK) --chapter_number $(NO) --xml $(FILE).xml --xml_pdf $(FILE).pdf --attach $(ATTACH)

list_chapters: 
	/Users/umut/diderot/diderot-cli/diderot_admin list_chapters $(LABEL_COURSE) $(LABEL_TEXTBOOK)

list_parts: 
	/Users/umut/diderot/diderot-cli/diderot_admin list_parts $(LABEL_COURSE) $(LABEL_TEXTBOOK)


## END: Diderot API
######################################################################

upload_exam2_practice: NO=1 
upload_exam2_practice: FILE=exam2/practice/exam
upload_exam2_practice: ATTACH=exam2/practice/media/*
upload_exam2_practice: exam2/practice/exam.xml upload_xml_attach


upload_exam2_real: NO=3
upload_exam2_real: FILE=exam2/real/exam
upload_exam2_real: ATTACH=exam2/real/media/*
upload_exam2_real: exam2/real/exam.xml upload_xml_attach

