-- This script includes the contents of a code block as raw html
-- if it starts with %%%html tag
-- This is meant for including a video.
-- Basen on this post: https://superuser.com/questions/1393318/allow-raw-html-in-latex-code-with-pandoc
-- \newcommand{\includevideo}[1]{
-- \begin{verbatim}
-- %%%html
-- <iframe width="560" height="315" src="https://www.youtube.com/embed/eOrAa0itnHM" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen>
-- </iframe>
-- \end{verbatim}

function CodeBlock(cb)
  local rawHtml = cb.text:match('^%s*%%%%%%html\n(.*)')
  if rawHtml then
    return pandoc.RawBlock('html', rawHtml)
  end
end
