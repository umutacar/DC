* Is this needed:  -package netstring
and let encode_url url = 
  Netencoding.Url.encode url

Here is how you install:
- `opam install ocamlnet`



* Interesting piece of knowledge

browser address interpretation.
// --> absolute.
/ --> relative to domain.
no slash --> relative to current page.

   
