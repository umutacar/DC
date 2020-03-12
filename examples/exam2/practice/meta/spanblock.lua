-- This script deletes spans consisting of just a label.

function SpanBlock(block)
  
  l = block.attributes['label']
  -- if no label is defined
  -- then no change necessary.
  -- else check that the block text is same as label
  -- if so, then delete, else skip.
  if l == nil then
    return block
  else  
    body = block.content 
    target = '[' .. l .. ']'
    if body = target then
      -- delete this block
      return {}
    else
      return block
  end
end
