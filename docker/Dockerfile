FROM ocaml/opam2:centos
RUN opam switch 4.07
RUN eval $(opam env)
RUN opam install -y core ocamlbuild.0.12.0 getopt menhir re2
