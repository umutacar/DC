* atmm book uses lots of multiparagraph arguments for examples   
  \sidenote{
   Para 1
  
   Para 2
  }
 
  This breaks in MTL because we don't take "{ .. }" as a chunk but dive into newlines.  We could skip over open and close braces.  I have to think about the implications of that.   

  This seems to break, because we can see things like \left\{ and \[  and \] though the latter will be ok.  So if we handle espace characters first, perhaps we will be ok?
   

* lstinline inside arguments is not tested well.  create some tests.
* captions now accept optional title argument but it is not tested.  test using atmm book.
   
* Caption could take an optional argument handle that as follows:
  -- Extend atom parser to recognize parser with optional argument
     translate perhaps the title to something like this, perhaps maintain it separately
  \caption{title}{text} --> title:text 
  -- Update the diderot command \diderotdrop to take into account the caption title.

* If an environment is not an atom, it should not be treated as such.  Update the promotion rule so that it looks for a prescribed set of atoms.

* Update \includegraphics{scale = 0.33} to \includegraphics{width = my_width} 

this can be calculated based on some fixed width.  Is there a robust way to do this?