# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain

OCB_FLAGS = -use-ocamlfind -package re2 -package core -I atom -I ast -I english -I md -I pervasives  -I tex -I xml 
OCB = ocamlbuild $(OCB_FLAGS)

#  il/il_syntax.ml il/il_ast.ml \

DEPEND = \
  atom/atom_lexer.mll atom/atom_parser.mly \
  english/english_words.ml \
  md/md_lexer.mll md/md_parser.mly md/md_syntax.ml  md/md2md.ml md/mdxml.ml  \
  pervasives/utils.ml pervasives/error_code.ml \
  tex/tex_labels.ml tex/tex_lexer.mll tex/tex_parser.mly tex/tex2html.ml tex/tex_syntax.ml tex/preprocessor.ml \
  xml/xml_constants.ml xml/xml_syntax.ml 
default: md2md.native mdxml.native \
  tex2tex.native texel.native texmlt.native texml.native
all: md2md.native mdxml.native \
  tex2tex.native texel.native texmlt.native texml.native

clean:
	$(OCB) -clean
	rm -f *.native
	rm -f *.dbg
	rm -f *.profile


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


bin_macos:
	cp tex2tex.native bin/macos/tex2tex
	cp texel.native bin/macos/texel
	cp texml.native bin/macos/texml

bin_macos_dbg:
	cp tex2tex.native bin/macos/tex2tex.dbg
	cp texel.native bin/macos/texel.dbg
	cp texml.native bin/macos/texml.dbg

bin_ubuntu:
	cp tex2tex.native bin/ubuntu/tex2tex
	cp texel.native bin/ubuntu/texel
	cp texml.native bin/ubuntu/texml

bin_ubuntu_dbg:
	cp tex2tex.native bin/ubuntu/tex2tex.dbg
	cp texel.native bin/ubuntu/texel.dbg
	cp texml.native bin/ubuntu/texml.dbg


guide_macos:
	cp tex2tex.native guide/mtl/bin/macos/tex2tex
	cp texel.native guide/mtl/bin/macos/texel
	cp texml.native guide/mtl/bin/macos/texml

guide_macos_dbg:
	cp tex2tex.native guide/mtl/bin/macos/tex2tex.dbg
	cp texel.native guide/mtl/bin/macos/texel.dbg
	cp texml.native guide/mtl/bin/macos/texml.dbg


guide_ubuntu:
	cp tex2tex.native guide/mtl/bin/ubuntu/tex2tex
	cp texel.native guide/mtl/bin/ubuntu/texel
	cp texml.native guide/mtl/bin/ubuntu/texml

guide_ubuntu_dbg:
	cp tex2tex.native guide/mtl/bin/ubuntu/tex2tex.dbg
	cp texel.native guide/mtl/bin/ubuntu/texel.dbg
	cp texml.native guide/mtl/bin/ubuntu/texml.dbg
