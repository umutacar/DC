open Core
open Syntax

let rec pp_block = function
| Chapter bs -> "Chapter " ^ pp_blocks bs
| Section bs -> "Section " ^ pp_blocks bs
| Subsection bs -> "Subsection " ^ pp_blocks bs
| Subsubsection bs -> "Subsubsection " ^ pp_blocks bs
| Group bs -> "Group " ^ pp_blocks bs
| Atom a -> pp_atom a
| Content c -> "\"" ^ pp_content c ^ "\""

and pp_atom = function
| Gram s -> "Gram \"" ^ pp_content s ^ "\""
| Definition s -> "Definition \"" ^ pp_content s ^ "\""
| Example s -> "Example \"" ^ pp_content s ^ "\""

and pp_blocks bs = "[" ^ String.concat ~sep:", " (List.map ~f:pp_block bs) ^ "]"

and pp_content c = if String.length c < 10 then c else String.slice c 0 5 ^ "..."
