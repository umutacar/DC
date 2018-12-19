type block =
| Chapter of block list
| Section of block list
| Subsection of block list
| Subsubsection of block list
| Group of block list
| Atom of atom
| Content of string

and atom =
| Gram of string
| Definition of string
| Example of string
