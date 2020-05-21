DC_HOME = /Users/umut/DC
GUIDE_DIR = /Users/umut/diderot/diderot-guide
CONCORDE_DIR = /Users/umut/diderot/concorde

default: 
	docker run -it -v $(DC_HOME):/dc umutacar/dc:lambda.4.07  

guide_ubuntu: default
	cp $(DC_HOME)/_build/top/dc.native $(GUIDE_DIR)/bin/ubuntu/dc

concorde: default
	cp $(DC_HOME)/_build/top/dc.native $(CONCORDE_DIR)/bin/


