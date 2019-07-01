(* $Id: cascade_lexer.mll 3670 2012-01-21 19:33:07Z jr_reuter $

   Copyright (C) 1999-2012 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>
       Christian Speckner <christian.speckner@physik.uni-freiburg.de>

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
open Cascade_parser
let unquote s =
  String.sub s 1 (String.length s - 2)
}

let digit = ['0'-'9']
let upper = ['A'-'Z']
let lower = ['a'-'z']
let char = upper | lower
let white = [' ' '\t' '\n']

(* We use a very liberal definition of strings for flavor names. *)
rule token = parse
    white      { token lexbuf }     (* skip blanks *)
  | '%' [^'\n']* '\n'
               { token lexbuf }     (* skip comments *)
  | digit+     { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '+'        { PLUS }
  | ':'        { COLON }
  | '~'        { OFFSHELL }
  | '='        { ONSHELL }
  | '#'        { GAUSS }
  | '!'        { NOT }
  | '&' '&'?   { AND }
  | '|' '|'?   { OR }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | char [^ ' ' '\t' '\n' '|' '&' '(' ')' ':']*
               { FLAVOR (Lexing.lexeme lexbuf) }
  | '"' [^ '"']* '"'
               { FLAVOR (unquote (Lexing.lexeme lexbuf)) }
  | eof        { END }
