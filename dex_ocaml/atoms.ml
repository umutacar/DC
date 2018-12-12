(* Atom keywords *)
let atom_algorithm = "algorithm"
let atom_code =  "code"
let atom_corollary = "corollary" 
let atom_cost_spec = "cost_spec" 
let atom_datatype = "datatype" 
let atom_datastr = "datastr" 
let atom_datastr = "datastr" 
let atom_definition = "definition"
let atom_example = "example"
let atom_exercise = "exercise"
let atom_hint = "hint"
let atom_important = "important"
let atom_lemma = "lemma"
let atom_note = "note"
let atom_paragraph = "gram"
let atom_preamble = "preamble"
let atom_problem = "problem"
let atom_proof = "proof"
let atom_proposition = "proposition"
let atom_remark = "remark"
let atom_solution = "solution"
let atom_syntax = "syntax"
let atom_teach_ask = "teach_ask"
let atom_theorem = "theorem"


(* This is identity mapping.
   We don't distinguish between keywords and the internal represtation of atoms. *)
let all_atoms = 
  [
   (atom_algorithm, atom_algorithm);
   (atom_code, atom_code);
   (atom_corollary, atom_corollary);
   (atom_cost_spec, atom_cost_spec);
   (atom_datatype, atom_datatype);
   (atom_datastr, atom_datastr);
   (atom_datastr, atom_datastr);
   (atom_definition, atom_definition);
   (atom_example, atom_example);
   (atom_exercise, atom_exercise);
   (atom_hint, atom_hint);
   (atom_important, atom_important);
   (atom_lemma, atom_lemma);
   (atom_note, atom_note);
   (atom_paragraph, atom_paragraph);
   (atom_preamble, atom_preamble);
   (atom_problem, atom_problem);
   (atom_proof, atom_proof);
   (atom_proposition, atom_proposition);
   (atom_remark, atom_remark);
   (atom_solution, atom_solution);
   (atom_syntax, atom_syntax);
   (atom_teach_ask, atom_teach_ask);
   (atom_theorem, atom_theorem)
  ]
