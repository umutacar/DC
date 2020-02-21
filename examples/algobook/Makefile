######################################################################
## Makefile
## This makefile can be used
## 1) to generate pdfs
## 2) to generate xmls
## 3) to upload chapters.  
##    this functionality saves a lot of time.  simply create the parts
##    manually or via the cli.  and you can create the chapters.
######################################################################


## BEGIN: DIDEROT SETUP

# Import course information
# include ../diderot-course-information

# Label for textbook
LABEL_TEXTBOOK="TB"

GUIDE_DIR = ../../diderot-guide
DIDEROT_ADMIN = ../../diderot-cli/diderot_admin
DIDEROT_ADMIN = ../../diderot-cli/diderot_admin --url https://staging.diderot.one

## END: DIDEROT SETUP
 


# Set up some variables
NO=0
PART_NO=0
FILE=""
ATTACH="does_not_exist"

######################################################################
## Begin: Setup

PDFLATEX = pdflatex --interaction=batchmode
PDFLATEX = pdflatex

LATEX = latex

PREAMBLE = ./templates/preamble-diderot.tex
PANDOC = pandoc --mathjax -f latex

FLAG_VERBOSE = -v 
FLAG_DBG = -d

DC = ../../dc -meta ./meta
## End: Setup
######################################################################

######################################################################
## Begin: Compile

FORCE: 

.PHONY: book html book-www book.pdf

book:
	$(PDFLATEX) --jobname="book" '\input{book}' ; 
	$(PDFLATEX) --jobname="book" '\input{book}' 

book-www: book
	cp book.pdf $(WWW-BOOK)

exam:
	$(PDFLATEX) --jobname="exam" '\input{exam}' ; 
	$(PDFLATEX) --jobname="exam" '\input{exam}'  


# Generate html. Might have to modify book-html.tex
html: FORCE
	$(PANDOC) -s book-html.tex > book.html

%.pdf : %.tex book
	$(PDFLATEX) --shell-escape --jobname="target" "\includeonly{$*}\input{book} ";
	mv target.pdf $@

%.xml : %.tex FORCE
ifdef debug
ifdef verbose
	$(DC) $(FLAG_DBG) $(FLAG_VERBOSE) $< -preamble $(PREAMBLE) -o $@
 else
	$(DC) $(FLAG_DBG) $< -preamble $(PREAMBLE) -o $@
endif
else
ifdef verbose
	$(DC) $(FLAG_VERBOSE) $< -preamble $(PREAMBLE) -o $@
 else
	$(DC) $< -preamble $(PREAMBLE) -o $@
endif
endif

clean: 
	rm *.aux *.idx *.log *.out *.toc */*.aux */*.idx */*.log */*.out 

reset: 
	make clean; rm *.pdf; rm*.html; rm  *~; rm */*~; rm  \#*\#; rm */\#*\#; 


all-xml: \
introduction/introduction.xml introduction/parallelism.xml introduction/spec.xml genome/genome.xml \
background/sets.xml background/graphs.xml \
language/lambda.xml language/sparc.xml language/functional.xml \
analysis/asymptotics.xml analysis/models.xml analysis/recurrences.xml \
sequences/introduction.xml sequences/adt.xml sequences/arrays.xml \
sequences/cost.xml sequences/examples.xml sequences/ephemeral.xml \
design/basics.xml design/divide-conquer.xml design/contraction.xml \
mcss/mcss.xml \
probability/spaces.xml probability/randomvars.xml probability/expectation.xml \
probability/spaces.xml probability/randomvars.xml probability/expectation.xml \
randomization/introduction.xml randomization/select.xml randomization/qsort.xml \
bsts/adt.xml bsts/parametric.xml bsts/treaps.xml bsts/augment.xml \
sets-and-tables/sets.xml sets-and-tables/tables.xml \
sets-and-tables/ordered.xml sets-and-tables/examples.xml \
graphs/graphs.xml \
graph-search/search.xml graph-search/bfs.xml \
shortest-paths/introduction.xml shortest-paths/dijkstra.xml \
shortest-paths/bellmanford.xml shortest-paths/johnson.xml \
graph-contraction/introduction.xml graph-contraction/edge.xml \
graph-contraction/star.xml graph-contraction/connectivity.xml \
mst/intro.xml mst/seq.xml mst/par.xml \
dp/intro.xml dp/ssandmed.xml dp/implement.xml dp/obst.xml \
pq/pq.xml \
hashing/foundations.xml hashing/tables.xml \
concurrency/threads-new.xml concurrency/mutex.xml \

some-xml: \
background/sets.xml background/graphs.xml \
language/lambda.xml language/sparc.xml language/functional.xml \
sequences/introduction.xml sequences/adt.xml sequences/arrays.xml \
sequences/cost.xml sequences/examples.xml sequences/ephemeral.xml \
mcss/mcss.xml \
randomization/introduction.xml randomization/select.xml randomization/qsort.xml \
bsts/adt.xml bsts/parametric.xml bsts/treaps.xml bsts/augment.xml \
shortest-paths/introduction.xml shortest-paths/dijkstra.xml \
shortest-paths/bellmanford.xml shortest-paths/johnson.xml \
graph-contraction/introduction.xml graph-contraction/edge.xml \
graph-contraction/star.xml graph-contraction/connectivity.xml \



## intro
introduction: \
introduction/introduction.pdf introduction/parallelism.pdf introduction/spec.pdf genome/genome.pdf \
introduction/introduction.xml introduction/parallelism.xml introduction/spec.xml genome/genome.xml

part-introduction: book introduction/introduction.tex introduction/parallelism.tex introduction/spec.tex genome/genome.tex
	$(PDFLATEX) --jobname="introduction" '\includeonly{introduction/introduction, introduction/parallelism,  introduction/spec, genome/genome}\input{book} '

part-introduction-www: intro
	cp intro.pdf $(WWW-BOOK)

## background
background:  \
background/sets.pdf background/graphs.pdf \
background/sets.xml background/graphs.xml

part-background: book background/sets.tex background/graphs.tex
	$(PDFLATEX) --jobname="background" '\includeonly{background/part, background/sets, background/graphs} \input{book} '

part-background-www: background
	cp background.pdf $(WWW-BOOK)

## language
language: \
language/introduction.pdf language/lambda.pdf language/sparc.pdf language/functional.pdf \
language/introduction.xml language/lambda.xml language/sparc.xml language/functional.xml

part-language: book language/introduction.tex language/functional.tex language/lambda.tex language/sparc.tex 
	$(PDFLATEX) --jobname="language" '\includeonly{language/part, language/introduction, language/functional, language/lambda, language/sparc} \input{book} '

part-language-www: language
	cp language.pdf $(WWW-BOOK)

## analysis
analysis: \
analysis/asymptotics.pdf analysis/models.pdf analysis/recurrences.pdf \
analysis/asymptotics.xml analysis/models.xml analysis/recurrences.xml
 
part-analysis: book analysis/asymptotics.tex analysis/models.tex analysis/recurrences.tex
	$(PDFLATEX) --jobname="analysis" '\includeonly{analysis/part, analysis/asymptotics, analysis/models, analysis/recurrences} \input{book}'

part-analysis-www: analysis
	cp analysis.pdf $(WWW-BOOK)

## Sequences
sequences: \
sequences/introduction.pdf sequences/adt.pdf sequences/arrays.pdf sequences/cost.pdf sequences/examples.pdf sequences/ephemeral.pdf \
sequences/introduction.xml sequences/adt.xml sequences/arrays.xml sequences/cost.xml sequences/examples.xml sequences/ephemeral.xml

part-sequences: book sequences/introduction.tex sequences/adt.tex sequences/arrays.tex sequences/cost.tex sequences/examples.tex sequences/ephemeral.tex
	$(PDFLATEX) --jobname="sequences" '\includeonly{sequences/part, sequences/introduction, sequences/adt, sequences/arrays, sequences/cost, sequences/examples, sequences/ephemeral} \input{book}'

part-sequences-www: sequences
	cp sequences.pdf $(WWW-BOOK)

## Design
design: \
design/basics.pdf design/divide-conquer.pdf design/contraction.pdf mcss/mcss.pdf\
design/basics.xml design/divide-conquer.xml design/contraction.xml mcss/mcss.xml

part-design: book design/basics.tex design/divide-conquer.tex design/contraction.tex mcss/mcss.tex
	$(PDFLATEX) --jobname="design" '\includeonly{design/part, design/basics, design/divide-conquer, design/contraction, mcss/mcss} \input{book}'

part-design-www: design
	cp design.pdf $(WWW-BOOK)

## Probability
probability: \
probability/spaces.pdf probability/randomvars.pdf probability/expectation.pdf \
probability/spaces.xml probability/randomvars.xml probability/expectation.xml

part-probability: book probability/spaces.tex probability/randomvars.tex probability/expectation.tex
	$(PDFLATEX) --jobname="probability" '\includeonly{probability/part, probability/spaces, probability/randomvars, probability/expectation} \input{book}'

part-probability-www: probability
	cp probability.pdf $(WWW-BOOK)

## Randomization
randomization: \
randomization/introduction.pdf randomization/select.pdf randomization/qsort.pdf  \
randomization/introduction.xml randomization/select.xml randomization/qsort.xml

part-randomization: book randomization/introduction.tex randomization/select.tex randomization/qsort.tex
	$(PDFLATEX) --jobname="randomization" '\includeonly{randomization/part, randomization/introduction, randomization/select, randomization/qsort} \input{book}'

part-randomization-www: randomization
	cp randomization.pdf $(WWW-BOOK)

## Bsts
bsts: \
bsts/adt.pdf bsts/parametric.pdf bsts/treaps.pdf bsts/augment.pdf \
bsts/adt.xml bsts/parametric.xml bsts/treaps.xml bsts/augment.xml

part-bsts: book bsts/adt.tex bsts/parametric.tex bsts/treaps.tex bsts/augment.tex
	$(PDFLATEX) --jobname="bsts" '\includeonly{bsts/part, bsts/adt, bsts/parametric, bsts/treaps, bsts/augment} \input{book}'

part-bsts-www: bsts
	cp bsts.pdf $(WWW-BOOK)

## Sets-And-Tables
sets-and-tables: \
sets-and-tables/sets.pdf sets-and-tables/tables.pdf sets-and-tables/ordered.pdf sets-and-tables/examples.pdf  \
sets-and-tables/sets.xml sets-and-tables/tables.xml sets-and-tables/ordered.xml sets-and-tables/examples.xml

part-sets-and-tables: book sets-and-tables/sets.tex sets-and-tables/tables.tex sets-and-tables/ordered.tex sets-and-tables/examples.tex
	$(PDFLATEX) --jobname="sets-and-tables" '\includeonly{sets-and-tables/part, sets-and-tables/sets, sets-and-tables/tables, sets-and-tables/ordered, sets-and-tables/examples} \input{book}'

part-sets-and-tables-www: sets-and-tables
	cp sets-and-tables.pdf $(WWW-BOOK)

## Graphs
graphs: \
graphs/graphs.pdf graph-search/search.pdf graph-search/bfs.pdf graph-search/dfs.pdf \
graphs/graphs.xml graph-search/search.xml graph-search/bfs.xml graph-search/dfs.xml
 
part-graphs: book graphs/graphs.tex graph-search/search.tex graph-search/bfs.tex graph-search/dfs.tex 
	$(PDFLATEX) --jobname="graphs" '\includeonly{graphs/part, graphs/graphs, graph-search/search, graph-search/bfs, graph-search/dfs} \input{book}'

part-graphs-www: graphs
	cp graphs.pdf $(WWW-BOOK)

## Shortest-Paths
shortest-paths: \
shortest-paths/introduction.pdf shortest-paths/dijkstra.pdf shortest-paths/bellmanford.pdf shortest-paths/johnson.pdf \
shortest-paths/introduction.xml shortest-paths/dijkstra.xml shortest-paths/bellmanford.xml shortest-paths/johnson.xml

part-shortest-paths: book shortest-paths/introduction.tex  shortest-paths/dijkstra.tex  shortest-paths/bellmanford.tex  shortest-paths/johnson.tex

	$(PDFLATEX) --jobname="shortest-paths" '\includeonly{shortest-paths/part, shortest-paths/introduction, shortest-paths/dijkstra, shortest-paths/bellmanford, shortest-paths/johnson} \input{book}'

## Graph-Contraction
graph-contraction: \
graph-contraction/introduction.pdf graph-contraction/edge.pdf graph-contraction/star.pdf graph-contraction/connectivity.pdf \
graph-contraction/introduction.xml graph-contraction/edge.xml graph-contraction/star.xml graph-contraction/connectivity.xml

part-graph-contraction: book graph-contraction/introduction.tex  graph-contraction/edge.tex  graph-contraction/star.tex graph-contraction/connectivity.tex

	$(PDFLATEX) --jobname="graph-contraction" '\includeonly{graph-contraction/part, graph-contraction/introduction, graph-contraction/edge, graph-contraction/star, graph-contraction/connectivity} \input{book}'

part-graph-contraction-www: graph-contraction
	cp graph-contraction.pdf $(WWW-BOOK)


## Mst
mst: \
mst/intro.pdf mst/seq.pdf mst/par.pdf \
mst/intro.xml mst/seq.xml mst/par.xml

part-mst: book mst/intro.tex mst/seq.tex mst/par.tex
	$(PDFLATEX) --jobname="mst" '\includeonly{mst/part, mst/intro, mst/seq, mst/par} \input{book}'

part-mst-www: mst
	cp mst.pdf $(WWW-BOOK)

## DP
dp: \
dp/intro.pdf dp/ssandmed.pdf dp/implement.pdf \
dp/intro.xml dp/ssandmed.xml dp/implement.xml

part-dp: book dp/part.tex dp/intro.tex dp/ssandmed.tex dp/implement.tex
	$(PDFLATEX) --jobname="dp" '\includeonly{dp/part, dp/intro, dp/ssandmed, dp/implement, dp/obst} \input{book}'

part-dp-www: dp
	cp dp.pdf $(WWW-BOOK)

## Priority Queues
pq:\
pq/pq.pdf \
pq/pdf.xml 

part-pq: book pq/part.tex pq/pq.tex 
	$(PDFLATEX) --jobname="pq" '\includeonly{pq/part, pq/pq, pq/pq} \input{book}'

part-pq-www: pq
	cp pq.pdf $(WWW-BOOK)

## Hashing
hashing: \
hashing/foundations.pdf hashing/tables.pdf \
hashing/foundations.xml hashing/tables.xml  

part-hashing: book hashing/part.tex hashing/foundations.tex hashing/tables.tex 
	$(PDFLATEX) --jobname="hashing" '\includeonly{hashing/part, hashing/foundations, hashing/tables} \input{book}'

part-hashing-www: hashing
	cp hashing.pdf $(WWW-BOOK)

## Concurrency
concurrency: \
concurrency/part.pdf concurrency/threads.pdf   concurrency/mutex.pdf \
concurrency/part.xml concurrency/threads.xml   concurrency/mutex.xml  

part-concurrency: book concurrency/part.tex concurrency/threads.tex   concurrency/mutex.tex  
	$(PDFLATEX) --jobname="concurrency" '\includeonly{concurrency/part, concurrency/threads-new, concurrency/sequences, concurrency/mutex} \input{book}'

part-concurrency-www: concurrency
	cp concurrency.pdf $(WWW-BOOK)


all-www: book book-www part-intro-www part-background-www part-language-www \
         part-analysis-www part-sequences-www part-design-www	part-probability-www \
         part-randomization-www part-bsts-www part-sets-and-tables-www \
         part-graphs-www part-shortest-paths-www part-shortest-paths-www \
         part-graph-contraction-www part-mst-www part-dp-www
	cp book.pdf $(WWW-BOOK)



## Quiz
quiz: exam  quiz/quiz.tex 
	$(PDFLATEX) --jobname="quiz" '\includeonly{quiz/quiz-simple} \input{exam}'

part-concurrency-www: concurrency


######################################################################
## BEGIN: Diderot API


upload: $(FILE).xml
ifdef ATTACH
	$(DIDEROT_ADMIN) upload_chapter $(LABEL_COURSE) $(LABEL_TEXTBOOK) --chapter_number $(NO) --xml $(FILE).xml --xml_pdf $(FILE).pdf --attach $(ATTACH)
else 
	$(DIDEROT_ADMIN) upload_chapter  $(LABEL_COURSE) $(LABEL_TEXTBOOK) --chapter_number $(NO) --xml $(FILE).xml --xml_pdf $(FILE).pdf
endif

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

######################################################################
## BEGIN: Diderot Uploads

# upload part: introduction
create_part_introduction:
	-$(DIDEROT_ADMIN) create_part $(LABEL_COURSE) $(LABEL_TEXTBOOK) "Introduction" "1" --label "part:introduction"

upload_introduction_introduction: PART_NO=1 
upload_introduction_introduction: NO=1 
upload_introduction_introduction: FILE=introduction/introduction
upload_introduction_introduction: introduction/introduction.xml introduction/introduction.pdf upload_xml_pdf

upload_introduction_parallelism: PART_NO=1 
upload_introduction_parallelism: NO=2 
upload_introduction_parallelism: FILE=introduction/parallelism
upload_introduction_parallelism: introduction/parallelism.xml introduction/parallelism.pdf upload_xml_pdf

upload_introduction_spec: PART_NO=1 
upload_introduction_spec: NO=3 
upload_introduction_spec: FILE=introduction/spec
upload_introduction_spec: introduction/spec.xml introduction/spec.pdf upload_xml_pdf

upload_introduction_genome: PART_NO=1 
upload_introduction_genome: NO=4
upload_introduction_genome: FILE=genome/genome
upload_introduction_genome: ATTACH=genome/media 
upload_introduction_genome: genome/genome.xml genome/genome.pdf upload_xml_pdf_attach

# upload part: background

create_part_background:
	-$(DIDEROT_ADMIN) create_part $(LABEL_COURSE) $(LABEL_TEXTBOOK) "Background" "2" --label "part:bg"

upload_background_sets: PART_NO=2
upload_background_sets: NO=5 
upload_background_sets: FILE=background/sets
upload_background_sets: background/sets.xml background/sets.pdf upload_xml_pdf

upload_background_graphs: PART_NO=2 
upload_background_graphs: NO=6 
upload_background_graphs: FILE=background/graphs
upload_background_graphs: ATTACH=background/media-graphs 
upload_background_graphs: background/graphs.xml background/graphs.pdf upload_xml_pdf_attach

# upload part: language

create_part_language:
	-$(DIDEROT_ADMIN) create_part $(LABEL_COURSE) $(LABEL_TEXTBOOK) "An Algorithmic Language" "3" --label "part:language"

upload_language_introduction: PART_NO=3
upload_language_introduction: NO=7 
upload_language_introduction: FILE=language/introduction
upload_language_introduction: language/introduction.xml language/introduction.pdf upload_xml_pdf

upload_language_functional: PART_NO=3
upload_language_functional: NO=8 
upload_language_functional: FILE=language/functional
upload_language_functional: language/functional.xml language/functional.pdf upload_xml_pdf

upload_language_lambda: PART_NO=3
upload_language_lambda: NO=9 
upload_language_lambda: FILE=language/lambda
upload_language_lambda: language/lambda.xml language/lambda.pdf upload_xml_pdf

upload_language_sparc: PART_NO=3
upload_language_sparc: NO=10 
upload_language_sparc: FILE=language/sparc
upload_language_sparc: language/sparc.xml language/sparc.pdf upload_xml_pdf

## Upload Part Analysis

create_part_analysis:
	-$(DIDEROT_ADMIN) create_part $(LABEL_COURSE) $(LABEL_TEXTBOOK) "Algorithm Analysis" "4" --label "part:analysis"

upload_analysis_introduction: PART_NO=4
upload_analysis_introduction: NO=11
upload_analysis_introduction: FILE=analysis/introduction
upload_analysis_introduction: analysis/introduction.xml analysis/introduction.pdf upload_xml_pdf

upload_analysis_asymptotics: PART_NO=4
upload_analysis_asymptotics: NO=12 
upload_analysis_asymptotics: FILE=analysis/asymptotics
upload_analysis_asymptotics: ATTACH=analysis/media-asymptotics 
upload_analysis_asymptotics: analysis/asymptotics.xml analysis/asymptotics.pdf upload_xml_pdf_attach

upload_analysis_models: PART_NO=4
upload_analysis_models: NO=13 
upload_analysis_models: FILE=analysis/models
upload_analysis_models: ATTACH=analysis/media-models
upload_analysis_models: analysis/models.xml analysis/models.pdf upload_xml_pdf_attach

upload_analysis_recurrences: PART_NO=4
upload_analysis_recurrences: NO=14 
upload_analysis_recurrences: FILE=analysis/recurrences
upload_analysis_recurrences: ATTACH=analysis/media-recurrences
upload_analysis_recurrences: analysis/recurrences.xml analysis/recurrences.pdf upload_xml_pdf_attach


## Upload Part Sequences

create_part_sequences:
	-$(DIDEROT_ADMIN) create_part $(LABEL_COURSE) $(LABEL_TEXTBOOK) "Sequences" "5" --label "part:sequences"

upload_sequences_introduction: PART_NO=5
upload_sequences_introduction: NO=15
upload_sequences_introduction: FILE=sequences/introduction
upload_sequences_introduction: sequences/introduction.xml sequences/introduction.pdf upload_xml_pdf

upload_sequences_adt: PART_NO=5
upload_sequences_adt: NO=16 
upload_sequences_adt: FILE=sequences/adt
upload_sequences_adt: sequences/adt.xml sequences/adt.pdf upload_xml_pdf

upload_sequences_arrays: PART_NO=5
upload_sequences_arrays: NO=17 
upload_sequences_arrays: FILE=sequences/arrays
upload_sequences_arrays: sequences/arrays.xml sequences/arrays.pdf upload_xml_pdf

upload_sequences_cost: PART_NO=5
upload_sequences_cost: NO=18
upload_sequences_cost: FILE=sequences/cost
upload_sequences_cost: sequences/cost.xml sequences/cost.pdf upload_xml_pdf

upload_sequences_examples: PART_NO=5
upload_sequences_examples: NO=19
upload_sequences_examples: FILE=sequences/examples
upload_sequences_examples: sequences/examples.xml sequences/examples.pdf upload_xml_pdf

upload_sequences_ephemeral: PART_NO=5
upload_sequences_ephemeral: NO=20
upload_sequences_ephemeral: FILE=sequences/ephemeral
upload_sequences_ephemeral: sequences/ephemeral.xml sequences/ephemeral.pdf upload_xml_pdf


upload_sequences_trees: PART_NO=5
upload_sequences_trees: NO=21
upload_sequences_trees: FILE=sequences/trees
upload_sequences_trees: sequences/trees.xml sequences/trees.pdf upload_xml_pdf


## Upload Part Design

create_part_design:
	-$(DIDEROT_ADMIN) create_part $(LABEL_COURSE) $(LABEL_TEXTBOOK) "Algorithm Design" "6" --label "part:design"

upload_design_introduction: PART_NO=6
upload_design_introduction: NO=22
upload_design_introduction: FILE=design/introduction
upload_design_introduction: ATTACH=design/introduction/media
upload_design_introduction: design/introduction.xml design/introduction.pdf upload_xml_pdf_attach

upload_design_basics: PART_NO=6
upload_design_basics: NO=23
upload_design_basics: FILE=design/basics
upload_design_basics: ATTACH=design/media-basics
upload_design_basics: design/basics.xml design/basics.pdf upload_xml_pdf_attach

upload_design_dc: PART_NO=6
upload_design_dc: NO=24
upload_design_dc: FILE=design/divide-conquer
upload_design_dc: ATTACH=design/media-dc
upload_design_dc: design/divide-conquer.xml design/divide-conquer.pdf upload_xml_pdf_attach

upload_design_c: PART_NO=6
upload_design_c: NO=25
upload_design_c: FILE=design/contraction
upload_design_c: ATTACH=design/media-contraction
upload_design_c: design/contraction.xml design/contraction.pdf upload_xml_pdf_attach

upload_design_mcss: PART_NO=6
upload_design_mcss: NO=26
upload_design_mcss: FILE=mcss/mcss
upload_design_mcss: ATTACH=mcss/media
upload_design_mcss: mcss/mcss.xml mcss/mcss.pdf upload_xml_pdf_attach


## Upload Part Probability

create_part_probability:
	-$(DIDEROT_ADMIN) create_part $(LABEL_COURSE) $(LABEL_TEXTBOOK) "Probability Theory" "7" --label "part:probability"

upload_probability_introduction: PART_NO=7
upload_probability_introduction: NO=27
upload_probability_introduction: FILE=probability/introduction
upload_probability_introduction: probability/introduction.xml probability/introduction.pdf upload_xml_pdf

upload_probability_spaces: PART_NO=7
upload_probability_spaces: NO=28
upload_probability_spaces: FILE=probability/spaces
upload_probability_spaces: probability/spaces.xml probability/spaces.pdf upload_xml_pdf

upload_probability_randomvars: PART_NO=7
upload_probability_randomvars: NO=29
upload_probability_randomvars: FILE=probability/randomvars
upload_probability_randomvars: probability/randomvars.xml probability/randomvars.pdf upload_xml_pdf

upload_probability_expectation: PART_NO=7
upload_probability_expectation: NO=30
upload_probability_expectation: FILE=probability/expectation
upload_probability_expectation: probability/expectation.xml probability/expectation.pdf upload_xml_pdf


## Upload Part Randomization

create_part_randomization:
	-$(DIDEROT_ADMIN) create_part $(LABEL_COURSE) $(LABEL_TEXTBOOK) "Randomized Algorithms" "8" --label "part:randomization"

upload_randomization_introduction: PART_NO=8
upload_randomization_introduction: NO=31
upload_randomization_introduction: FILE=randomization/introduction
upload_randomization_introduction: ATTACH=randomization/media-introduction
upload_randomization_introduction: randomization/introduction.xml randomization/introduction.pdf upload_xml_pdf

upload_randomization_select: PART_NO=8
upload_randomization_select: NO=32
upload_randomization_select: FILE=randomization/select
upload_randomization_select: ATTACH=randomization/media-select
upload_randomization_select: randomization/select.xml randomization/select.pdf upload_xml_pdf_attach

upload_randomization_qsort: PART_NO=8
upload_randomization_qsort: NO=33
upload_randomization_qsort: FILE=randomization/qsort
upload_randomization_qsort: ATTACH=randomization/media-qsort
upload_randomization_qsort: randomization/qsort.xml randomization/qsort.pdf upload_xml_pdf_attach


## Upload Part BST

create_part_bst:
	-$(DIDEROT_ADMIN) create_part $(LABEL_COURSE) $(LABEL_TEXTBOOK) "Binary Search Trees" "9" --label "part:bst"

## Upload Part Sets and Tables

create_part_st:
	-$(DIDEROT_ADMIN) create_part $(LABEL_COURSE) $(LABEL_TEXTBOOK) "Sets and Tables" "10" --label "part:sets-and-tables"

## Upload Part Graphs

create_part_graphs:
	-$(DIDEROT_ADMIN) create_part $(LABEL_COURSE) $(LABEL_TEXTBOOK) "Graphs and Graph Search" "11" --label "part:graphs"

## Upload Shortest Paths

create_part_sp:
	-$(DIDEROT_ADMIN) create_part $(LABEL_COURSE) $(LABEL_TEXTBOOK) "Shortest Paths" "12" --label "part:shortest-paths"

## Upload Graph Contraction

create_part_gc:
	-$(DIDEROT_ADMIN) create_part $(LABEL_COURSE) $(LABEL_TEXTBOOK) "Graph Contraction" "13" --label "part:graphcon"


## Upload Minimum Spanning Trees

create_part_mst:
	-$(DIDEROT_ADMIN) create_part $(LABEL_COURSE) $(LABEL_TEXTBOOK) "Minimum Spanning Trees" "14" --label "part:mst"


## Upload Dynamic Programming

create_part_dp:
	-$(DIDEROT_ADMIN) create_part $(LABEL_COURSE) $(LABEL_TEXTBOOK) "Dynamic Programming" "15" --label "part:dp"

## Upload Priority Queues

create_part_pq:
	-$(DIDEROT_ADMIN) create_part $(LABEL_COURSE) $(LABEL_TEXTBOOK) "Priority Queues" "16" --label "part:pq"

## Upload Hashing

create_part_hashing:
	-$(DIDEROT_ADMIN) create_part $(LABEL_COURSE) $(LABEL_TEXTBOOK) "Hashing" "17" --label "part:hashing"


## Upload Concurrency

create_part_concurrency:
	-$(DIDEROT_ADMIN) create_part $(LABEL_COURSE) $(LABEL_TEXTBOOK) "Concurrency" "17" --label "part:concurrency"

## END: Diderot Uploads
######################################################################
