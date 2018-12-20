# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain

OCB_FLAGS = -use-ocamlfind -I tex -I xml
OCB = ocamlbuild $(OCB_FLAGS)

all: native

clean:
	$(OCB) -clean

native:
	$(OCB) main.native

profile:
	$(OCB) -tag profile main.native

debug:
	$(OCB) -tag debug main.byte

