DC_HOME = /Users/umut/DC
GUIDE_DIR = /Users/umut/diderot/guide
CONCORDE_DIR = /Users/umut/diderot/concorde

default: 
	docker run -it -v $(DC_HOME):/mtl umutacar/mtl:lambda  

guide_ubuntu: default
	cp $(DC_HOME)/_build/top/dc.native $(GUIDE_DIR)/bin/ubuntu/dc

concorde: default
	cp $(DC_HOME)/_build/top/dc.native $(CONCORDE_DIR)/bin/


