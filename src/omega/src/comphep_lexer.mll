(* $Id: comphep_lexer.mll 4926 2013-12-04 12:35:06Z jr_reuter $

   Copyright (C) 1999-2014 by

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
open Comphep_parser
}

let digit = ['0'-'9']
let upper = ['A'-'Z']
let lower = ['a'-'z']
let alpha = upper | lower
let alphanum = alpha | digit

let symbol = alpha alphanum*
let integer = digit+

rule token = parse
    [' ' '\t']    { token lexbuf }     (* skip blanks *)
  | "("           { LPAREN }
  | ")"           { RPAREN }
  | "i"           { I }
  | "."           { DOT }
  | "**"          { POWER }
  | "*"           { MULT }
  | "/"           { DIV }
  | "+"           { PLUS }
  | "-"           { MINUS }
  | symbol        { SYMBOL (Lexing.lexeme lexbuf) }
  | integer       { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | _             { failwith ("lexer fails @" ^ Lexing.lexeme lexbuf) }
  | eof           { END }




