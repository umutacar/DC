# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain

OCB_FLAGS = -use-ocamlfind -package re2 -package core -I atom -I ast -I english -I md -I pervasives  -I tex -I top -I xml 
OCB = ocamlbuild $(OCB_FLAGS)
GUIDE_DIR = ~/diderot/guide

#  il/il_syntax.ml il/il_ast.ml \

DEPEND = \
  ast/ast.ml \
  english/english_words.ml \
  md/md_lexer.mll md/md_parser.mly md/md_syntax.ml md/md2html.ml  md/md2md.ml \
  pervasives/utils.ml pervasives/error_code.ml \
  tex/tex_atom_lexer.mll tex/tex_atom_parser.mly tex/tex_comment_lexer.mll tex/tex_labels.ml tex/tex_lexer.mll tex/tex_parser.mly tex/tex2html.ml tex/tex_syntax.ml tex/preprocessor.ml \
  top/dc.ml \
  xml/xml_constants.ml xml/xml_syntax.ml 

default: dc.native tex2tex.native
# cc.native 
# texml.native 
# texel.native 

all: dc.native md2md.native \
  tex2tex.native texel.native texml.native

clean:
	$(OCB) -clean
	rm -f *.native
	rm -f *.dbg
	rm -f *.profile

cc.native: $(DEPEND) top/cc.ml
	$(OCB) cc.native

dc.native: $(DEPEND) top/dc.ml
	$(OCB) dc.native

# md2md
md2md.native: $(DEPEND) md/md2md.ml
	$(OCB) md2md.native

# md2md
mdxml.native: $(DEPEND) md/mdxml.ml
	$(OCB) mdxml.native

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

# texml
texml.native: $(DEPEND) tex/texml.ml
	$(OCB) texml.native

texml.profile: $(DEPEND) tex/texml.ml
	$(OCB) -tag profile texml.native

texml.debug: $(DEPEND) tex/texml.ml
	$(OCB) -tag debug texml.byte

# traverse
traverse.native: $(DEPEND) tex/traverse.ml
	$(OCB) traverse.native

traverse.profile: $(DEPEND) tex/traverse.ml
	$(OCB) -tag profile traverse.native

traverse.debug: $(DEPEND) tex/traverse.ml
	$(OCB) -tag debug traverse.byte

bin_macos:
	cp tex2tex.native bin/macos/tex2tex
	cp texel.native bin/macos/texel

bin_ubuntu:
	cp tex2tex.native bin/ubuntu/tex2tex
	cp texel.native bin/ubuntu/texel

readme: ./README.md
	pandoc README.md -o README.pdf

guide: ./$(GUIDE_DIR)README.md
	pandoc $(GUIDE_DIR)README.md -o $(GUIDE_DIR)README.pdf

guide_macos: all
	cp dc.native $(GUIDE_DIR)/bin/macos/dc
#	cp texel.native $(GUIDE_DIR)/bin/macos/texel
#	cp tex2tex.native $(GUIDE_DIR)/bin/macos/tex2tex
#	cp texml.native $(GUIDE_DIR)/bin/macos/texml

guide_ubuntu: 
	cp _build/top/dc.native $(GUIDE_DIR)/bin/macos/dc
#	cp  _build/tex/texel.native $(GUIDE_DIR)/bin/ubuntu/texel
#	cp  _build/tex/tex2tex.native $(GUIDE_DIR)/bin/ubuntu/tex2tex
#	cp  _build/tex/texml.native $(GUIDE_DIR)/bin/ubuntu/texml

