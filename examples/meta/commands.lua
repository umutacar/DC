-- This script includes the contents of a code block as raw html
-- if it starts with %%% diderot_html tag

function CodeBlock(cb)
  print ('cb.text =', cb.text) 

  -- Regex explanation
  -- ^ mathches start of string
  -- %% matches "%", % is special char and used for escapes
  -- %s matches space
  -- (.*) mathes anything and returns as a group

  local html_body = string.match (cb.text, "^%%%%%%%% diderot_html%s*\n(.*)")
  print ('html_body =', html_body) 
  if html_body then
    return pandoc.RawBlock('html', html_body)
  end
end
