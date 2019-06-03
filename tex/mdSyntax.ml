(**********************************************************************
 ** tex/mdSyntax.ml
 ** Note: markdown is sensitive to whitespace. Do not change 
 ** the whitespaces into the syntax below.
 **********************************************************************)
let firstline = "startFrom"
let numbers = "numberLines"

let mk_code_block_arg_indicate value = 
  "." ^ value

let mk_code_block_arg key value = 
  key ^ "=" ^ "\"" ^ value ^ "\""

let add_to_code_block_arg arg_new arg_opt = 
  match arg_opt with 
  | None -> Some arg_new
  | Some arg -> Some (arg_new ^ " " ^ arg)
