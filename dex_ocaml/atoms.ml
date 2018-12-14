(* Atom keywords *)
let kw_atom_algorithm = "algorithm"
let kw_atom_code =  "code"
let kw_atom_corollary = "corollary" 
let kw_atom_cost_spec = "cost_spec" 
let kw_atom_datatype = "datatype" 
let kw_atom_datastr = "datastr" 
let kw_atom_datastr = "datastr" 
let kw_atom_definition = "definition"
let kw_atom_example = "example"
let kw_atom_exercise = "exercise"
let kw_atom_hint = "hint"
let kw_atom_important = "important"
let kw_atom_lemma = "lemma"
let kw_atom_note = "note"
let kw_atom_paragraph = "gram"
let kw_atom_preamble = "preamble"
let kw_atom_problem = "problem"
let kw_atom_proof = "proof"
let kw_atom_proposition = "proposition"
let kw_atom_remark = "remark"
let kw_atom_solution = "solution"
let kw_atom_syntax = "syntax"
let kw_atom_teach_ask = "teach_ask"
let kw_atom_theorem = "theorem"


(* This is identity mapping.
   We don't distinguish between keywords and the internal represtation of atoms. *)
let all_atoms = 
  [
   (kw_atom_algorithm, kw_atom_algorithm);
   (kw_atom_code, kw_atom_code);
   (kw_atom_corollary, kw_atom_corollary);
   (kw_atom_cost_spec, kw_atom_cost_spec);
   (kw_atom_datatype, kw_atom_datatype);
   (kw_atom_datastr, kw_atom_datastr);
   (kw_atom_datastr, kw_atom_datastr);
   (kw_atom_definition, kw_atom_definition);
   (kw_atom_example, kw_atom_example);
   (kw_atom_exercise, kw_atom_exercise);
   (kw_atom_hint, kw_atom_hint);
   (kw_atom_important, kw_atom_important);
   (kw_atom_lemma, kw_atom_lemma);
   (kw_atom_note, kw_atom_note);
   (kw_atom_paragraph, kw_atom_paragraph);
   (kw_atom_preamble, kw_atom_preamble);
   (kw_atom_problem, kw_atom_problem);
   (kw_atom_proof, kw_atom_proof);
   (kw_atom_proposition, kw_atom_proposition);
   (kw_atom_remark, kw_atom_remark);
   (kw_atom_solution, kw_atom_solution);
   (kw_atom_syntax, kw_atom_syntax);
   (kw_atom_teach_ask, kw_atom_teach_ask);
   (kw_atom_theorem, kw_atom_theorem)
  ]
