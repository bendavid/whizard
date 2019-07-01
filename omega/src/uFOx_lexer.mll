(* vertex_lexer.mll --

   Copyright (C) 1999-2016 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>
       with contributions from
       Christian Speckner <cnspeckn@googlemail.com>

   WHIZARD is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   WHIZARD is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *)

{
open Lexing
open UFOx_parser

let string_of_char c =
  String.make 1 c

let int_of_char c =
  int_of_string (string_of_char c)

let init_position fname lexbuf =
  let curr_p = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { curr_p with
      pos_fname = fname;
      pos_lnum = 1;
      pos_bol = curr_p.pos_cnum };
  lexbuf

}

let digit = ['0'-'9']
let upper = ['A'-'Z']
let lower = ['a'-'z']
let char = upper | lower
let word = char | digit | '_'
let white = [' ' '\t' '\n']

rule token = parse
    white             { token lexbuf }     (* skip blanks *)
  | '('        	      { LPAREN }
  | ')'        	      { RPAREN }
  | ','        	      { COMMA }
  | '*' '*'    	      { POWER }
  | '*'        	      { TIMES }
  | '/'        	      { DIV }
  | '+'        	      { PLUS }
  | '-'        	      { MINUS }
  | ( '-'? digit+ as i ) ( '.' '0'* )?
                      { INT (int_of_string i) }
  | '-'? digit* '.' digit+ ( ['E''e'] '-'? digit+ )? as x
                      { FLOAT (float_of_string x) }
  | char word* ('.' char word+ )? as s
                      { ID s }
  | _ as c            { failwith ("invalid character at `" ^
				    string_of_char c ^ "'") }
  | eof               { END }


