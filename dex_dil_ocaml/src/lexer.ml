open Core

let lex lines =
  let lines = ref lines in
  let begin_matcher = Str.regexp "\\\\begin\\{\\([a-z]*\\)\\}\\(\\[\\(.*\\)\\]\\)?" in
  let end_matcher = Str.regexp "\\\\end\\{\\([a-z]*\\)\\}\\(\\[\\(.*\\)\\]\\)?" in
  let rec next () = match !lines with
      [] -> Parser.EOF
    | l::ls -> lines := ls;
      let l = String.strip l in
      if Str.string_match begin_matcher l 0 then match Str.matched_group 1 l with
          "chapter" -> Parser.B_CHAP
        | "section" -> Parser.B_SECT
        | "subsection" -> Parser.B_SUB
        | "subsubsection" -> Parser.B_SUBSUB
        | "group" -> Parser.B_GRP
        | "gram" -> Parser.B_GRM
        | "definition" -> Parser.B_DFN
        | "example" -> Parser.B_EXMPL
        | _ -> Parser.TEXT l else
      if Str.string_match end_matcher l 0 then match Str.matched_group 1 l with
          "chapter" -> Parser.E_CHAP
        | "section" -> Parser.E_SECT
        | "subsection" -> Parser.E_SUB
        | "subsubsection" -> Parser.E_SUBSUB
        | "group" -> Parser.E_GRP
        | "gram" -> Parser.E_GRM
        | "definition" -> Parser.E_DFN
        | "example" -> Parser.E_EXMPL
        | _ -> Parser.TEXT l
      else if l = "" then next () else Parser.TEXT l
  in
  fun () -> let token = next () in (token, Lexing.dummy_pos, Lexing.dummy_pos)
