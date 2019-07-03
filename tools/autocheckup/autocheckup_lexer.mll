(* $Id: autocheckup_lexer.mll 2457 2010-05-03 09:29:08Z ohl $  *)

{
open Autocheckup_parser
let string_buffer = Buffer.create 32
}

let digit = ['0'-'9']
let upper = ['A'-'Z']
let lower = ['a'-'z']
let char = upper | lower
let word1 = char | digit | '_' | '-' | '/' 
let word = char | digit | '_' | '-' | '+' | '@' | '=' | ':' | '/' | '.'
let white = [' ' '\t' '\n' '\r' ]
let newline = ( '\n' | '\r' | "\r\n" )

(* We use a very liberal definition of strings for flavor names. *)
rule token = parse
    white      	{ token lexbuf }     (* skip blanks *)
  | '='        	{ LET }
  | "+="       	{ ADDTO }
  | "-="       	{ REMOVE }
  | "!="       	{ PROHIBIT }
  | ';'        	{ SEMI }
  | '{'        	{ OPEN }
  | '}'        	{ CLOSE }
  | '#' [^'\n']* '\n'
                { token lexbuf }     (* skip comments *)
  | word1 word* { STRING (Lexing.lexeme lexbuf) }
  | '"'        	{ Buffer.reset string_buffer;
               	  string lexbuf;
               	  STRING (Buffer.contents string_buffer) }
  | eof        	{ END }

and string = parse
    '"'        	{ () }
  | "\\\\"     	{ Buffer.add_char string_buffer '\\';
                  string lexbuf }
  | "\\\""     	{ Buffer.add_char string_buffer '"';
		  string lexbuf }
  | newline    	{ invalid_arg "newline in string" }
  | _          	{ Buffer.add_char string_buffer (Lexing.lexeme_char lexbuf 0);
	       	  string lexbuf }
