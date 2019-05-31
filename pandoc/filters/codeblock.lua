-- This script promotes the language attribute of a pandoc
-- codeBlock to a class.  
-- This should allows us to use custom languages with 
-- Kate XML specifications.

function CodeBlock(block)
  -- if classes is empty
  -- then promote language to classes.
  -- else block remains unchanged.
  
  if #block.classes == 0 then
    l = block.attributes['language']
    -- if no language is defined 
    -- then no change necessary.
    -- else make the language a class.
    if l == nil then
      return block
    else  
      -- delete language atttribute
      block.attributes['language'] = nil

      -- make lang a class, lua counts from 1.
      block.classes[1] = l      

      return block
    end
  else 
    return block
  end
end
