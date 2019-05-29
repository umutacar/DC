# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain

OCB_FLAGS = -use-ocamlfind -package re2 -package core -I tex -I xml -I pervasives
OCB = ocamlbuild $(OCB_FLAGS)
DEPEND = pervasives/utils.ml pervasives/errorCode.ml tex/ast.ml  tex/lexer.mll tex/mdSyntax.ml tex/parser.mly tex/tex2html.ml tex/texSyntax.ml tex/preprocessor.ml xml/xmlConstants.ml xml/xmlSyntax.ml 

all: labeltex.native traverse.native tex2tex.native tex2xml.native 

clean:
	$(OCB) -clean

# tex2tex
labeltex.native: $(DEPEND) tex/labeltex.ml
	$(OCB) labeltex.native

# tex2tex
tex2tex.native: $(DEPEND) tex/tex2tex.ml
	$(OCB) tex2tex.native

tex2tex.profile: $(DEPEND) tex/tex2tex.ml
	$(OCB) -tag profile tex2tex.native

tex2tex.debug: $(DEPEND) tex/tex2tex.ml
	$(OCB) -tag debug tex2tex.byte

# tex2xml
tex2xml.native: $(DEPEND) tex/tex2xml.ml
	$(OCB) tex2xml.native

tex2xml.profile: $(DEPEND) tex/tex2xml.ml
	$(OCB) -tag profile tex2xml.native

tex2xml.debug: $(DEPEND) tex/tex2xml.ml
	$(OCB) -tag debug tex2xml.byte


# traverse
traverse.native: $(DEPEND) tex/traverse.ml
	$(OCB) traverse.native

traverse.profile: $(DEPEND) tex/traverse.ml
	$(OCB) -tag profile traverse.native

traverse.debug: $(DEPEND) tex/traverse.ml
	$(OCB) -tag debug traverse.byte

