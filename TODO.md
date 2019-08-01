* atom lexer does not take into account lstinline's inside arguments and environments.  it should.
* Make a PR from pure to master.
* Caption could take an optional argument handle that as follows:
  -- Extend atom parser to recognize parser with optional argument
     translate perhaps the title to something like this, perhaps maintain it separately
  \caption{title}{text} --> title:text 
  -- Update the diderot command \diderotdrop to take into account the caption title.

* If an environment is not an atom, it should not be treated as such.  Update the promotion rule so that it looks for a prescribed set of atoms.

* Update \includegraphics{scale = 0.33} to \includegraphics{width = my_width} 

this can be calculated based on some fixed width.  Is there a robust way to do this?