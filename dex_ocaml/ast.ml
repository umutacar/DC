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

let map_concat f xs = 
  let xs_s = List.map f xs in
  let result = List.fold_left (fun result x -> result ^ x) "" xs_s in
    result

let atomToString (Atom(kind, topt, hb, ab, he)) = 
  match topt with
  | None -> hb ^ ab ^ he
  | Some t -> hb ^ "kind:" ^ kind ^ "title:" ^ t ^ ab ^ he

let groupToString (Group(topt, hb, ats, he)) = 
  let atoms = map_concat atomToString ats in
    match topt with
    | None -> hb ^ atoms ^ he
    | Some t -> hb ^ "title:" ^ t ^ atoms ^ he

let blockToString b = 
  match b with
  | Block_Group g -> groupToString g
  | Block_Atom a -> atomToString a

let rec sectionToString (Section (t, h, bs)) =
  let blocks = map_concat blockToString bs in
    h ^ blocks

let chapterToString (Chapter (t, h, bs, ss)) =
  let blocks = map_concat blockToString bs in
  let sections = map_concat sectionToString ss in
    h ^ blocks ^ sections





