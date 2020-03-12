-- This script includes the contents of a code block as raw html
-- if it starts with %%%html tag
-- This is meant for including a video.
-- Based on this post: https://superuser.com/questions/1393318/allow-raw-html-in-latex-code-with-pandoc
-- \newcommand{\download}[1]{
-- \begin{verbatim}
-- %%%html
-- <a download="diderot-picture.jpg">
-- \end{verbatim}}

function CodeBlock(cb)
  local rawHtml = cb.text:match('^%s*%%%%%%html\n(.*)')
  if rawHtml then
    return pandoc.RawBlock('html', rawHtml)
  end
end
