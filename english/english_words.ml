open Core
open Utils
module Tex = Tex_syntax

(* Turn off prints *)
let d_printf args = 
    ifprintf stdout args
let d_printf_strlist x y = 
	()

let mk_plural s = s ^ "s"

let stop_words = 
  (* English stopwords, from python NLTK package *)
  ["i";
   "me";
   "my";
   "myself";
   "we";
   "our";
   "ours";
   "ourselves";
   "you";
   "your";
   "yours";
   "yourself";
   "yourselves";
   "he";
   "him";
   "his";
   "himself";
   "she";
   "her";
   "hers";
   "herself";
   "it";
   "its";
   "itself";
   "they";
   "them";
   "their";
   "theirs";
   "themselves";
   "what";
   "which";
   "who";
   "whom";
   "this";
   "that";
   "these";
   "those";
   "am";
   "is";
   "are";
   "was";
   "were";
   "be";
   "been";
   "being";
   "have";
   "has";
   "had";
   "having";
   "do";
   "does";
   "did";
   "doing";
   "a";
   "an";
   "the";
   "and";
   "but";
   "if";
   "or";
   "because";
   "as";
   "until";
   "while";
   "of";
   "at";
   "by";
   "for";
   "with";
   "about";
   "against";
   "between";
   "into";
   "through";
   "during";
   "before";
   "after";
   "above";
   "below";
   "to";
   "from";
   "up";
   "down";
   "in";
   "out";
   "on";
   "off";
   "over";
   "under";
   "again";
   "further";
   "then";
   "once";
   "here";
   "there";
   "when";
   "where";
   "why";
   "how";
   "all";
   "any";
   "both";
   "each";
   "few";
   "more";
   "most";
   "other";
   "some";
   "such";
   "no";
   "nor";
   "not";
   "only";
   "own";
   "same";
   "so";
   "than";
   "too";
   "very";
   "s";
   "t";
   "can";
   "will";
   "just";
   "don";
   "should";
   "now"
  ]
  @
  (* Diderot stopwords *)
  [Tex.kw_chapter; mk_plural Tex.kw_chapter;
   Tex.kw_section; mk_plural Tex.kw_section;
   Tex.kw_subsection; mk_plural Tex.kw_subsection;
   Tex.kw_subsubsection; mk_plural Tex.kw_subsubsection;
   Tex.kw_paragraph; mk_plural Tex.kw_paragraph;
   Tex.kw_flex; mk_plural Tex.kw_flex;
   Tex.kw_problem_cluster; mk_plural Tex.kw_problem_cluster;
   Tex.kw_algorithm; mk_plural Tex.kw_algorithm;
   Tex.kw_assumption; mk_plural Tex.kw_assumption;
   Tex.kw_code; mk_plural Tex.kw_code;
   Tex.kw_corollary; mk_plural Tex.kw_corollary;
   Tex.kw_costspec; mk_plural Tex.kw_costspec;
   Tex.kw_datastr; mk_plural Tex.kw_datastr;
   Tex.kw_datatype; mk_plural Tex.kw_datatype;
   Tex.kw_definition; mk_plural Tex.kw_definition;
   Tex.kw_example; mk_plural Tex.kw_example;
   Tex.kw_exercise; mk_plural Tex.kw_exercise;
   Tex.kw_hint; mk_plural Tex.kw_hint;
   Tex.kw_important; 
   Tex.kw_lemma; mk_plural Tex.kw_lemma;
   Tex.kw_note; mk_plural Tex.kw_note;
   Tex.kw_gram; mk_plural Tex.kw_gram;
   Tex.kw_preamble; mk_plural Tex.kw_preamble;
   Tex.kw_problem; mk_plural Tex.kw_problem;
   Tex.kw_proof; mk_plural Tex.kw_proof;
   Tex.kw_proposition; mk_plural Tex.kw_proposition;
   Tex.kw_remark; mk_plural Tex.kw_remark;
   Tex.kw_reminder; mk_plural Tex.kw_reminder;
   Tex.kw_slide; mk_plural Tex.kw_slide;
   Tex.kw_solution; mk_plural Tex.kw_solution;
   Tex.kw_syntax; 
   Tex.kw_task; mk_plural Tex.kw_task;
   Tex.kw_theorem; mk_plural Tex.kw_theorem
  ]
  @
  (* quantifers, those not included in the stopwords above *)
  [
   "couple";
   "enough";
   "lots";
   "little";
   "many";
   "much";
   "plenty";
   "several"
  ] 
  @
  (* adverbs *)
  [ 
   "actually";
   "accordingly";
   "across";
   "adjacent";
   "afterward";
   "ahead";
   "along";
   "also";
   "another";
   "background";
   "begin";
   "behind";
   "besides";
   "beyond";
   "briefly";
   "consequence";
   "consequently";
   "contrast";
   "contrary";
   "conversely";
   "currently";
   "directly";
   "fact";
   "finally";
   "first";
   "following";
   "furthermore";
   "gradually";
   "hence";
   "however";
   "last";
   "lastly";
   "later";
   "left";
   "like";
   "manner";
   "meantime";
   "meanwhile";
   "moreover";
   "nearby";
   "next";
   "nevertheless";
   "nonetheless";
   "presently";
   "opposite";
   "result";
   "right";
   "second";
   "short";
   "side";
   "similarly";
   "since";
   "soon";
   "specific";
   "specifically";
   "spite";
   "still";
   "summary";
   "subsequently";
   "thereafter";
   "therefore";
   "thus";
   "top";
   "ultimately";
   "yet"
  ]

let table_stop_words = 
  let s = List.map stop_words ~f:(fun x -> (x, ())) in
    match Hashtbl.of_alist (module String) s with
    | `Ok t -> t
    | `Duplicate_key x ->
       (printf "Fatal Error: Duplicate entry in the stop words table %s\n"  x;
        exit 1)


(* Predicate to check that the word is significant
 * that is more than 1 character and not a stopword.
 *)
let is_significant_word (x: string): bool =      
  (String.length x > 1) 
  & 
  (try let _ = Hashtbl.find_exn table_stop_words x  in
          false
    with Caml.Not_found -> true
  )
	 & 
	 (* drop math *)
	 (not (str_match_full "[\\^-_0123456789]+" x))


(* Tokenizes body with respect to spaces
 * after doing some sanitization such as 
 * removal of comments, deletion of \label, \depend, \begin \end, math.
 * Drops stop words and trivial words of 1 character, and 
 * returns a list of tokens where words with 4 or more characters come first.
 *)
let tokenize_spaces body = 
  (* Delete all comments *)
(* This is not necessary. Also break uniformity with pandoc.
   Not necessary because we delete comments.
  let body = Str.global_replace (Str.regexp ("%.*" ^ Tex.pattern_newline)) "" body in
*)
  (* Delete labels *)
  (* It might seem like a good idea to reuse them but this can be bad
   * because it could generate permutations of the same words.
   *) 
  let body = Str.global_replace (Str.regexp "\\\\label{[^}]*}") "" body in

  
  (* Delete depends. These refer to other content should be reused for labeling. *)

  let body = Str.global_replace (Str.regexp "\\\\depend{[^}]*}") "" body in

  (* Delete all latex environments *)
  let body = Str.global_replace (Str.regexp "\\\\begin{[^}]*}") "" body in
  let body = Str.global_replace (Str.regexp "\\\\end{[^}]+}") "" body in
  (* Delete all math *)
  let body = Str.global_replace (Str.regexp "\\$[^\\$]*\\$") "" body in

  (* Delete all latex commands *)
  let body = Str.global_replace (Str.regexp "\\\\[A-Za-z]+") "" body in

  (* Replace all  non-(alpha-numeric plus dash plus underscore) characters with space *)
  (* Regexp for this may seem strange. *)
  let body = Str.global_replace (Str.regexp "[^-^0-9^A-Z^a-z]+") " " body in
  let body = Str.global_replace (Str.regexp "[\\^]+") " " body in

  (* Now split at all whitespaces, including for windows form feed \x0c *)
  let tokens = Str.split (Str.regexp Tex.pattern_whitespace) body in
      (* splits the string at space* 's.  
         if none is found, returns the whole string.
        *)
  let tokens = List.map tokens  String.lowercase in 
  let tokens = List.filter tokens ~f:is_significant_word in
  let (tokens_small, tokens_big) = List.partition_tf ~f:(fun x -> String.length x <= 3) tokens in
  (* Reorder so small words are not preferred *)
  let tokens = tokens_big @ tokens_small in
  let _ = d_printf_strlist "tokenize_spaces: tokens = %s\n" tokens in
    tokens


let tokenize_spaces_opt body_opt = 
  match body_opt with
	| None -> [ ]
	| Some body -> tokenize_spaces body 
