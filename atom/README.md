# Grammar

## TODO

* There is a conflict because of environments.

  Distinguish between environments in paragraph for 

  \begin{env}
  \end{env}

  and those that are in the middle
  
  some stuff here and there \begin{itemize} ... \end{itemize}

  this is going to require splitting the content line into multiple rules to make sure that a env is not alone by itself.

* [Don't follow] One idea would be start each paragraph with an empty line.
  The question is how to determine the end.  We want to say that when we encounter an empty line, but then in the lexer, we would have to consume that empty line and include in the paragraph, this in turn makes it impossible to include it in the next paragraph.

  I think this is a bad idea.  For example

  \subsection{x}
  \label{y}
  In this section...

this will require a new line.

* There are some details about how how to deal with headings and various whitespaces in them.  Should they all be taken as part of the heading or shoud they be accounted for in the parser.  Trying to account for them in the parser complicates the parser, and leads complex code and conflicts.  So perhaps best to deal with them in the lexer.  I left an example such conflict in the parser.

It seems that it would be best to deal with these in the lexer, though this seems not easy without restricting the grammar is some fashion.




## Idea
The basic idea is to scan the document at the level of 
* lines
* environments,

and differentiate between "empty lines", "content lines", and "comment lines".

We then put these together to recognize paragraphs and wrap them by grams.

For this to succeed, it is important to match headings in the lexer by taking them directly as a single token.  We do this by forward scanning until the heading ends.

 

## Lexer

In the lexer, we distinguish between
* space = [' ' '\t' '\n' '\r']
* horizontal spaces: ' ' and '\t'
* percent = '%'
* percent_esc = '\%'
* significant characters = all but spaces and percentage
* comments: when we see a comment we take in the lexer and return in.  Note that special char '\%' is longer so it will match if it is used first.
* environments: parser matches on environment markers \begin{..} and takes them as one blob and returns it as environment.

## Parser
In the parser, we have
* hspace: horizontal spaces
* sigchar: significant characters =  as above + percent_esc + latex environments
* char: all charactecs = hspace + sigchar

based on these we define:
* newline =  simply the newline char
* emptyline = horizontal space + newline
* commentline = horizontal space + comment
* (non-empty, content) line = horizantal spaces + sigchar + any_chars + newline
* environments: as env's returned by lexer
* text paragraph: a content line followed by any number of empty content and comment lines, followed by an emptyline.
* ignorables: as a sequence of empty and comment lines.

