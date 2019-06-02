# Grammar

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

