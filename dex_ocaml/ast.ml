type atom_kind = string
type title = string

type chapter = 
  Chapter of title option * (block list) option * (section list) option

and section = 
  Section of title option * (block list) option 

and block = 
  Group of title option * (atom list) option

and atom = 
  Atom of atom_kind * title option * string option
