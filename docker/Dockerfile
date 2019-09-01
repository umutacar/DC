FROM ocaml/opam2:centos
RUN opam switch 4.07
RUN eval $(opam env)
# RUN opam install -y core ocamlbuild.0.12.0 getopt menhir re2
RUN opam install -y core ocamlbuild ocamlnet getopt menhir re2
ADD build.sh .
CMD ["./build.sh"]
