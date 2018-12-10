type atom_kind = string
type atom_body = string
type title = string
type heading = string

type atom = Atom of atom_kind * title option * heading * atom_body * heading
type group = Group of title option * heading * atom list * heading

type chapter = 
  Chapter of title * heading * block list * section list

and section = 
  Section of title * heading * block list

and block = 
  | Block_Group of group
  | Block_Atom of atom

let atomToString a as Atom(topt, hb, ab, he) = 
  match topt
  | None => hb ^ ab ^ he
  | Some t => hb ^ "title:" ^ t ^ ab ^ he

let groupToString g as Group(topt, hb, ats, he) = 
  let atoms = List.map atomToString ats in
    match topt
    | None => hb ^ ats ^ he
    | Some t => hb ^ "title:" ^ t ^ ats ^ he

let rec astToString ast = 
  match ast 
  | Chapter (t, h, bs, ss) =>
    let blocks = List.map toString bs in 
    let sections = List.map toString ss in
      h ^ blocks ^ ss
  | Section (t, h, bs) =>
    let blocks = List.map toString bs in
      h ^ blocks
  | Block_Group g => groupToString g
  | Block_Atom a => atomToString a
