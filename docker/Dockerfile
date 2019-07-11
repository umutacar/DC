FROM ocaml/opam2:centos
RUN opam switch 4.08
RUN eval $(opam env)
RUN opam install -y core ocamlbuild getopt menhir re2
