 # do not forget to do "make trans" to udpate the caml to pml translator when needed

OCAMLC=ocamlc

TOOLS=bench.out analyse.out
PROGS_SPEED=array-sum #smvm dmm quickhull quicksort worst barnes-hut bintree-sum # warning: no more than 8
PROGS=$(PROGS_SPEED) tau tau-mem tautree bintree-sum seq-sum

timeout: timeout.c
	gcc time.c -o timeout

bench.out: bench.ml shared.ml
	$(OCAMLC) -o bench.out unix.cma str.cma shared.ml bench.ml

analyse.out: analyse.ml shared.ml
	$(OCAMLC) -o analyse.out unix.cma str.cma shared.ml analyse.ml

tools: $(TOOLS)

trans:
	make -C ../trans

progs: tools force timeout
	make -j -C ../examples $(PROGS:=.out) $(PROGS:=_sq.out) $(PROGS:=_ws.out)

counts:
	make -j -C ../examples $(PROGS:=_count.out)

progcounts:
	make -j -C ../examples $(PROGS:=.out) $(PROGS:=_sq.out) $(PROGS:=_ws.out) $(PROGS:=_count.out)

all: progs force
	./bench.out all
	./analyse.out all

speedup: progs force
	./bench.out speedup


# todo: this generic rule does not work
#define MACHINE_template =
#%.$(1): force
#	$$(if $$(basename $$@), ./bench.out -machine $(1) -prog $$(basename $$@) speedup
#endef
#$(foreach machine,$(MACHINES),$(eval $(call MACHINE_template,$(machine))))

.srv-53-06: force
.srv-53-07: force
.srv-53-08: force
.srv-53-09: force
.srv-53-010: force
.srv-53-011: force
.srv-53-012: force
.srv-53-013: force
.srv-53-014: force

BIG= #-big

%.srv-53-06: force
	./bench.out $(BIG) -machine srv-53-06 -prog $(basename $@) speedup
%.srv-53-07: force
	./bench.out $(BIG) -machine srv-53-07 -prog $(basename $@) speedup
%.srv-53-08: force
	./bench.out $(BIG) -machine srv-53-08 -prog $(basename $@) speedup
%.srv-53-09: force
	./bench.out $(BIG) -machine srv-53-09 -prog $(basename $@) speedup
%.srv-53-010: force
	./bench.out $(BIG) -machine srv-53-010 -prog $(basename $@) speedup
%.srv-53-011: force
	./bench.out $(BIG) -machine srv-53-011 -prog $(basename $@) speedup
%.srv-53-012: force
	./bench.out $(BIG) -machine srv-53-012 -prog $(basename $@) speedup
%.srv-53-013: force
	./bench.out $(BIG) -machine srv-53-013 -prog $(basename $@) speedup
%.srv-53-014: force
	./bench.out $(BIG) -machine srv-53-014 -prog $(basename $@) speedup

MACHINES=srv-53-06 srv-53-07 srv-53-08 srv-53-09 srv-53-010 srv-53-011 srv-53-012 srv-53-013 srv-53-014
BIGTARGETS=$(join $(PROGS_SPEED), $(addprefix ., $(MACHINES)))

big: $(BIGTARGETS)
#	echo $(BIGTARGETS)

%.exe: 
	make -j -C ../examples/ $(basename $@).out $(basename $@)_sq.out $(basename $@)_ws.out


SAVE_REAL=$(foreach f, $(SAVE), $(shell [ -f $f ] && echo $f))

save: force
	@mkdir -p save 
	$(foreach g, $(SAVE_REAL), $(shell cp $g save/$g))

force:

clean:
	rm -f *.gnuplot *.dat *.aux *.log *.pdf *.cmi *.cmo *.cmx results*.txt *.eps *~

clean-more:
	rm -f *.out




#./bench.out -tries 1 -cutoff 200 -prog dmm speedup
# ./bench.out -prog worst -timeout 15 -size 1000000 -proc 8 cutoff
# ./bench.out -prog worst -proc 2 -tries 2 cutoff
# ./bench.out -prog tau -proc 2 -machines local cutoff ./analyse.out cutoff
# ./bench.out -prog tau-mem -proc 2 -cutoff 10 -machines local cutoff; ./analyse.out cutoff


