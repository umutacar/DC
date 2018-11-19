open Core

let lex lines =
  let lines = ref lines in
  let begin_matcher = Str.regexp "\\\\begin\\{\\([a-z]*\\)\\}\\(\\[\\(.*\\)\\]\\)?" in
  let end_matcher = Str.regexp "\\\\end\\{\\([a-z]*\\)\\}\\(\\[\\(.*\\)\\]\\)?" in
  let rec next () = match !lines with
      [] -> 0
    | l::ls -> lines := ls;
      let l = String.strip l in
      if Str.string_match begin_matcher l 0 then match Str.matched_group 1 l with
          "chapter" -> Parser.A
        | "section" -> Parser.A
        | "subsection" -> Parser.A
        | "subsubsection" -> Parser.A
        | "group" -> Parser.A
        | "gram" -> Parser.A
        | "definition" -> Parser.A
        | "example" -> Parser.A
        | _ -> Parser.Text l
			else if Str.string_match end_matcher l 0 then match Str.matched_group 1 l with
          "chapter" -> Parser.B
        | "section" -> Parser.B
        | "subsection" -> Parser.B
        | "subsubsection" -> Parser.B
        | "group" -> Parser.B
        | "gram" -> Parser.B
        | "definition" -> Parser.B
        | "example" -> Parser.B
        | _ -> Parser.Text l
      else if l = "" then next () else Parser.Text l
  in
  fun () -> let token = next () in (token, Lexing.dummy_pos, Lexing.dummy_pos)
