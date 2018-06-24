PYTHON = python
DILDEX = dildex.py


default: none


example: example/example.dex 
	$(PYTHON) $(DILDEX) example/example.dex 

genome-example: genome-example/genome.dex 
	$(PYTHON) $(DILDEX) genome-example/genome.dex 

intro: introduction/introduction.dex 
	$(PYTHON) $(DILDEX) introduction/introduction.dex 

prelim: preliminaries/preliminaries.dex
	$(PYTHON) $(DILDEX) preliminaries/preliminaries.dex

lang: language/language.dex
	$(PYTHON) $(DILDEX) language/language.dex

models: models/models.dex
	$(PYTHON) $(DILDEX) models/models.dex

design: design/design.dex
	$(PYTHON) $(DILDEX) design/design.dex

recur: recurrences/recurrences.dex
	$(PYTHON) $(DILDEX) recurrences/recurrences.dex

genome: genome/genome.dex
	$(PYTHON) $(DILDEX) genome/genome.dex

seq: sequences/sequences.dex
	$(PYTHON) $(DILDEX) sequences/sequences.dex

mcss: mcss/mcss.dex
	$(PYTHON) $(DILDEX) mcss/mcss.dex

random: randomized/randomized.dex
	$(PYTHON) $(DILDEX) randomized/randomized.dex

bsts: bsts/bsts.dex
	$(PYTHON) $(DILDEX) bsts/bsts.dex

all:
	$(PYTHON) $(DILDEX) introduction/introduction.dex; \
	$(PYTHON) $(DILDEX) preliminaries/preliminaries.dex; \
	$(PYTHON) $(DILDEX) language/language.dex; \
	$(PYTHON) $(DILDEX) models/models.dex; \
	$(PYTHON) $(DILDEX) design/design.dex; \
	$(PYTHON) $(DILDEX) recurrences/recurrences.dex; \
	$(PYTHON) $(DILDEX) genome/genome.dex; \
	$(PYTHON) $(DILDEX) sequences/sequences.dex; \
	$(PYTHON) $(DILDEX) mcss/mcss.dex; \
	$(PYTHON) $(DILDEX) randomized/randomized.dex; \
	$(PYTHON) $(DILDEX) bsts/bsts.dex; \

clean: 
	rm *.aux *.idx *.log *.out *.toc */*.aux */*.idx */*.log */*.out 

reset: 
	make clean; rm *.pdf; rm  *~; rm */*~; rm  \#*\#; rm */\#*\#

none:
	echo 'Please specify a target chapter: make intro/prelim/lang/models/design/recur/genome/seq/mcss/random/bsts/all'
