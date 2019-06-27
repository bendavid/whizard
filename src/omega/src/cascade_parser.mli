type token =
  | FLAVOR of ( string )
  | INT of ( int )
  | LPAREN
  | RPAREN
  | AND
  | OR
  | PLUS
  | COLON
  | NOT
  | ONSHELL
  | OFFSHELL
  | GAUSS
  | END

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  (string, int list) Cascade_syntax.t 
