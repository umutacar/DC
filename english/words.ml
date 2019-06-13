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
  [kw_chapter; mk_plural kw_chapter;
   kw_section; mk_plural kw_section;
   kw_subsection; mk_plural kw_subsection;
   kw_subsubsection; mk_plural kw_subsubsection;
   kw_paragraph; mk_plural kw_paragraph;
   kw_flex; mk_plural kw_flex;
   kw_problem_cluster; mk_plural kw_problem_cluster;
   kw_algorithm; mk_plural kw_algorithm;
   kw_assumption; mk_plural kw_assumption;
   kw_code; mk_plural kw_code;
   kw_corollary; mk_plural kw_corollary;
   kw_costspec; mk_plural kw_costspec;
   kw_datastr; mk_plural kw_datastr;
   kw_datatype; mk_plural kw_datatype;
   kw_definition; mk_plural kw_definition;
   kw_example; mk_plural kw_example;
   kw_exercise; mk_plural kw_exercise;
   kw_hint; mk_plural kw_hint;
   kw_important; 
   kw_lemma; mk_plural kw_lemma;
   kw_note; mk_plural kw_note;
   kw_gram; mk_plural kw_gram;
   kw_preamble; mk_plural kw_preamble;
   kw_problem; mk_plural kw_problem;
   kw_proof; mk_plural kw_proof;
   kw_proposition; mk_plural kw_proposition;
   kw_remark; mk_plural kw_remark;
   kw_reminder; mk_plural kw_reminder;
   kw_slide; mk_plural kw_slide;
   kw_solution; mk_plural kw_solution;
   kw_syntax; 
   kw_task; mk_plural kw_task;
   kw_theorem; mk_plural kw_theorem
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

(* Predicate to check that this is a good label *)
let label_good x =      
  (String.length x > 1) 
  & 
  (try let _ = Hashtbl.find_exn table_stop_words x  in
          false
    with Caml.Not_found -> true
  )

