/* orders_parser.mly --

   Copyright (C) 2023-2025 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

%{
open Orders_syntax
let parse_error msg =
  raise (Syntax_Error (msg, symbol_start (), symbol_end ()))
%}

%token < string > ID
%token < int > INT
%token OR AND EQ BACKSLASH TILDE RANGE COMMA
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token SEMI
%token END

%left OR
%left AND
%left BACKSLASH
%nonassoc TILDE

%start main
%type < Orders_syntax.t > main

%%

main:
    END                           { And [] }
  | condition END                 { $1 }
  | conjunction END               { And $1 }
  | alternative END               { Or $1 }
;

condition:
    atom                          { Atom $1 }
  | LPAREN conjunction RPAREN     { And $2 }
  | LPAREN alternative RPAREN     { Or $2 }
;

conjunction:
    condition                 { [$1] }
  | condition AND conjunction { $1 :: $3 }
  | condition SEMI conjunction { $1 :: $3 }
;

alternative:
    condition                 { [$1] }
  | condition OR alternative  { $1 :: $3 }
;

atom:
    set EQ LBRACE range RBRACE      { Slices ($1, $4) }
  | set EQ LBRACKET range RBRACKET  { Interval ($1, $4) }
  | set EQ INT                      { Exact ($1, $3) }
  | set                             { Null $1 }
;

set:
    LBRACE RBRACE         { Set [] }
  | ID                    { Set [$1] }
  | LBRACE orders RBRACE  { Set $2 }
  | TILDE set             { Complement $2 }
  | set BACKSLASH set     { Diff ($1, $3) }
;

orders:
    ID                    { [$1] }
  | ID COMMA orders       { $1 :: $3 }
;

range:
    RANGE INT     { Max $2 }
  | INT RANGE     { Min $1 }
  | INT RANGE INT { Range ($1, $3) }
  | INT           { Range ($1, $1) }
;

