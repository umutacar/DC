# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain

OCB_FLAGS = -use-ocamlfind -package re2 -package core -I ast -I english  -I tex -I xml -I pervasives 
OCB = ocamlbuild $(OCB_FLAGS)
DEPEND = \
  ast/ast.ml \
  english/english_words.ml \
  pervasives/utils.ml pervasives/error_code.ml \
  tex/tex_atom_lexer.mll tex/tex_atom_parser.mly tex/tex_ast.ml tex/tex_labels.ml tex/tex_lexer.mll tex/tex_parser.mly tex/tex2html.ml tex/tex_syntax.ml tex/preprocessor.ml \
  xml/xml_constants.ml xml/xml_syntax.ml 
default: tex2tex.native texel.native texmlt.native
all: tex2tex.native texel.native texmlt.native 

clean:
	$(OCB) -clean


# atom
atomize.native: $(DEPEND) atom/atomize.ml
	$(OCB) atomize.native


# tex2tex
tex2tex.native: $(DEPEND) tex/tex2tex.ml
	$(OCB) tex2tex.native

tex2tex.profile: $(DEPEND) tex/tex2tex.ml
	$(OCB) -tag profile tex2tex.native

tex2tex.debug: $(DEPEND) tex/tex2tex.ml
	$(OCB) -tag debug tex2tex.byte


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

