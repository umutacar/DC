-- This script promotes the language attribute of a pandoc
-- codeBlock to a class.  
-- This should allows using custom languages with 
-- Kate (XML) specifications.

function CodeBlock(cb)
  local rawHtml = cb.text:match('^%s*%%%%%%html\n(.*)')
  if rawHtml then
    return pandoc.RawBlock('html', rawHtml)
  end
end
