module Tex = Tex_syntax
open Core
open Utils


let mk_begin_atom name point_val title kw_args =
  match name with
  | "problem" -> "\\begin{problem}" ^ point_val ^ "{" ^ title ^ "}"
  | _ -> Tex.mk_begin_atom name point_val title kw_args


(* SAM_NOTE: this is problematic. \begin{questions} seems to need at least one
 * \question inside, or else it explodes.
 *)
let problem_environment =
  String.concat ~sep:"\n"
  [ "\\newenvironment{problem}[2][0.]{%"
  ; "\\begin{framed}"
  ; "\\textbf{Problem: {#2}}"
  ; "\\begin{questions}"
  ; "}{%"
  ; "\\end{questions}"
  ; "\\end{framed}"
  ; "}"
  ]


let top_stuff title =
  String.concat ~sep:"\n"
  [ "\\documentclass{exam}"
  ; "\\usepackage{diderot_exam}"
  ; "\\title{" ^ title ^ "}"
  (* ; "" *)
  (* ; "\\newenvironment{preamble}{\\begin{framed}}{\\end{framed}}" *)
  (* ; "\\newenvironment{cluster}[1][0.]{}{}" *)
  (* ; problem_environment *)
  (* ; "\\newcommand{\\chapter}[2][0.]{}" *)
  ; ""
  ; "\\begin{document}"
  ; "\\maketitle"
  ]


let bottom_stuff =
  "\\end{document}"
