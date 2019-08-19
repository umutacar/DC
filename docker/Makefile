DC_HOME = /Users/umut/DC
GUIDE_DIR = /Users/umut/diderot/guide

default: 
	docker run -it -v $(DC_HOME):/mtl umutacar/mtl:lambda  

guide_ubuntu: default
	cp $(DC_HOME)/_build/top/dc.native $(GUIDE_DIR)/bin/ubuntu/dc
	cp $(DC_HOME)/_build/tex/texml.native $(GUIDE_DIR)/bin/ubuntu/texml

