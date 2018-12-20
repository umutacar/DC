# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain

OCB_FLAGS = -use-ocamlfind -I tex -I xml
OCB = ocamlbuild $(OCB_FLAGS)

all: tex2tex.native

clean:
	$(OCB) -clean

# tex2tex
tex2tex.native:
	$(OCB) tex2tex.native

tex2tex.profile:
	$(OCB) -tag profile tex2tex.native

tex2tex.debug:
	$(OCB) -tag debug tex2tex.byte

# tex2xml
tex2xml.native:
	$(OCB) tex2xml.native

tex2xml.profile:
	$(OCB) -tag profile tex2xml.native

tex2xml.debug:
	$(OCB) -tag debug tex2xml.byte

