let debug = false
let d_printf args = 
  if debug then
    fprintf stdout args
  else 
    ifprintf stdout args

