/* $Id: cascade_parser.mly 6465 2015-01-10 15:22:31Z jr_reuter $

   Copyright (C) 1999-2015 by

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

%{
open Cascade_syntax
let parse_error msg =
  raise (Syntax_Error (msg, symbol_start (), symbol_end ()))
%}

%token < string > FLAVOR
%token < int > INT
%token LPAREN RPAREN
%token AND OR PLUS COLON NOT
%token ONSHELL OFFSHELL GAUSS
%token END
%left OR
%left AND
%left PLUS COLON
%left NOT

%start main
%type < (string, int list) Cascade_syntax.t > main

%%

main:
    END                             { mk_true () }
  | cascades END                    { $1 }
;

cascades:
    cascade                         { $1 }
  | LPAREN cascades RPAREN          { $2 }
  | cascades AND cascades           { mk_and $1 $3 }
  | cascades OR cascades            { mk_or $1 $3 }
;

cascade:
    momentum_list                   { mk_any_flavor $1 }
  | momentum_list ONSHELL flavor_list
                                    { mk_on_shell $3 $1 }
  | momentum_list ONSHELL NOT flavor_list
                                    { mk_on_shell_not $4 $1 }
  | momentum_list OFFSHELL flavor_list
                                    { mk_off_shell $3 $1 }
  | momentum_list OFFSHELL NOT flavor_list
                                    { mk_off_shell_not $4 $1 }
  | momentum_list GAUSS flavor_list { mk_gauss $3 $1 }
  | momentum_list GAUSS NOT flavor_list
                                    { mk_gauss_not $4 $1 }
;

momentum_list:
  | momentum                        { [$1] }
  | momentum_list PLUS momentum     { $3 :: $1 }
;

momentum:
    INT                             { $1 }
;

flavor_list:
    FLAVOR                          { [$1] }
  | flavor_list COLON FLAVOR        { $3 :: $1 }
;
