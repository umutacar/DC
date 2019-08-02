* lstinline inside arguments is not tested well.  create some tests.
* captions now accept optional title argument but it is not tested.  test using atmm book.
   
* Caption could take an optional argument handle that as follows:
  -- Extend atom parser to recognize parser with optional argument
     translate perhaps the title to something like this, perhaps maintain it separately
  \caption{title}{text} --> title:text 
  -- Update the diderot command \diderotdrop to take into account the caption title.
* Update \includegraphics{scale = 0.33} to \includegraphics{width = my_width} 

this can be calculated based on some fixed width.  Is there a robust way to do this?