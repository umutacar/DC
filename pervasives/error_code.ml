let labeling_error: int ref = ref ~-100
let parse_error = ref ~-200

let newError cnt = 
  let () = cnt := !cnt + 1 in
    !cnt

let newLabelingError () = 
  newError labeling_error

let newParseError () = 
  newError parse_error

let labeling_error_hash_table_corrupted = newLabelingError ()
let labeling_error_unknown_atom = newLabelingError ()

let parse_error_something_wrong = newParseError ()
let parse_error_arg_expecting_key_value = newParseError ()
let parse_error_arg_expecting_nonempty_string = newParseError ()
let parse_error_multiple_titles = newParseError ()
let parse_error_is_target_text_status_of_unknown_kind = newParseError ()
let parse_error_single_paragraph_status_of_unknown_kind = newParseError ()
