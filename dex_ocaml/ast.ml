type atom_kind = string
type atom_body = string
type title = string

type chapter = 
  Chapter of title * block list option * section list

and section = 
  Section of title * block list

and block = 
  Group of title * atom list

and atom = 
  Atom of atom_kind * title * atom_body
