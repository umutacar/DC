# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain

OCB_FLAGS = -use-ocamlfind -package re2 -package core -I atom -I tex -I xml -I pervasives 
OCB = ocamlbuild $(OCB_FLAGS)
DEPEND = \
  pervasives/utils.ml pervasives/errorCode.ml \
  tex/ast.ml  tex/lexer.mll tex/mdSyntax.ml tex/parser.mly tex/tex2html.ml tex/texSyntax.ml tex/preprocessor.ml \
  xml/xmlConstants.ml xml/xmlSyntax.ml \
  atom/atomize.ml atom/atomLexer.mll atom/atomParser.mly

all: atomize.native traverse.native tex2xml.native texel.native texmlt.native 

clean:
	$(OCB) -clean


# atom
atomize.native: $(DEPEND) atom/atomize.ml
	$(OCB) atomize.native


# tex2xml
tex2xml.native: $(DEPEND) tex/tex2xml.ml
	$(OCB) tex2xml.native

tex2xml.profile: $(DEPEND) tex/tex2xml.ml
	$(OCB) -tag profile tex2xml.native

tex2xml.debug: $(DEPEND) tex/tex2xml.ml
	$(OCB) -tag debug tex2xml.byte

# texel
texel.native: $(DEPEND) tex/texel.ml
	$(OCB) texel.native

texel.profile: $(DEPEND) tex/texel.ml
	$(OCB) -tag profile texel.native

texel.debug: $(DEPEND) tex/texel.ml
	$(OCB) -tag debug texel.byte


# texmlt
texmlt.native: $(DEPEND) tex/texmlt.ml
	$(OCB) texmlt.native

texmlt.profile: $(DEPEND) tex/texmlt.ml
	$(OCB) -tag profile texmlt.native

texmlt.debug: $(DEPEND) tex/texmlt.ml
	$(OCB) -tag debug texmlt.byte


# traverse
traverse.native: $(DEPEND) tex/traverse.ml
	$(OCB) traverse.native

traverse.profile: $(DEPEND) tex/traverse.ml
	$(OCB) -tag profile traverse.native

traverse.debug: $(DEPEND) tex/traverse.ml
	$(OCB) -tag debug traverse.byte

