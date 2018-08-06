import pyparsing as pp 
from pervasives.syntax import * 

# latex comment
latex_comment =  pp.Literal('%') + pp.restOfLine() + pp.lineEnd()

######################################################################
## Shared Datex and Dil 

## symbols
kw_sq_open = pp.Literal(SQUARE_BRACKET_OPEN).suppress()
kw_sq_close = pp.Literal(SQUARE_BRACKET_CLOSE).suppress()
kw_curly_open = pp.Literal(CURLY_BRACKET_OPEN).suppress()
kw_curly_close = pp.Literal(CURLY_BRACKET_CLOSE).suppress()
kw_comma = pp.Literal(COMMA).suppress()
kw_semicolon = pp.Literal(SEMI_COLON).suppress()
kw_end = pp.StringEnd()


## phrases: a phrase is zero or more words and commas
phrase_gram = pp.Word(pp.alphanums+PHRASE_SYMBOLS)
# this does not work, don't understand why
#phrase_unit = pp.Combine(pp.OneOrMore(~kw_semicolon + ~kw_sq_close + ~kw_end + pp.printables))
#phrase = pp.OneOrMore(pp.Word(pp.alphanums+COMMA)).setName('phrase').setResultsName('phrase')
phrase = pp.OneOrMore(phrase_gram).setName('phrase').setResultsName('phrase')
phrase = phrase.setParseAction(lambda x: " ".join(x['phrase']))

# A list of phrases is phrases separeted by semicolons
list_phrases = pp.Forward()
list_phrases << phrase + pp.Optional(kw_semicolon + list_phrases)

## Title phrases, clauses
title_phrases = list_phrases.setName('title').setResultsName('title')
title_phrases = title_phrases.setParseAction(lambda x:", ".join(x.asList()))
title_clause = (kw_sq_open + title_phrases + kw_sq_close).setResultsName('title_clause')
title_clause = title_clause.setName('title_clause').setResultsName('title_clause')

## Parent phrases, clauses
parents_phrases =  list_phrases.setName('parents').setResultsName('parents')
parents_clause = (kw_sq_open + parents_phrases + kw_sq_close)
parents_clause = parents_clause.setName('parents_clause').setResultsName('parents_clause')
