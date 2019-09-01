DC_HOME = /Users/umut/DC
GUIDE_DIR = /Users/umut/diderot/guide
CONCORDE_DIR = /Users/umut/diderot/concorde

default: 
	docker run -it -v $(DC_HOME):/dc umutacar/dc:lambda  

guide_ubuntu: default
	cp $(DC_HOME)/_build/top/dc.native $(GUIDE_DIR)/bin/ubuntu/dc
	cp $(DC_HOME)/_build/top/texel.native $(GUIDE_DIR)/bin/ubuntu/texel

concorde: default
	cp $(DC_HOME)/_build/top/dc.native $(CONCORDE_DIR)/bin/


