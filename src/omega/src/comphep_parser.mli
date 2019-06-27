type token =
  | SYMBOL of ( string )
  | INT of ( int )
  | I
  | LPAREN
  | RPAREN
  | DOT
  | MULT
  | DIV
  | POWER
  | PLUS
  | MINUS
  | END

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  Comphep_syntax.raw 
