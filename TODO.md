* Regression test pure with 15122.
* Make a PR from pure to master.
* Caption could take an optional argument handle that as follows:
  -- Extend atom parser to recognize parser with optional argument
     translate perhaps the title to something like this, perhaps maintain it separately
  \caption{title}{text} --> title:text 
  -- Update the diderot command \diderotdrop to take into account the caption title.
  