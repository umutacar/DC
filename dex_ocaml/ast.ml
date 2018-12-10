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
