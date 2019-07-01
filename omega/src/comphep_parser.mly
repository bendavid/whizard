/* $Id: comphep_parser.mly 6465 2015-01-10 15:22:31Z jr_reuter $

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
module S = Comphep_syntax
%}

%token < string > SYMBOL
%token < int > INT
%token I
%token LPAREN RPAREN
%token DOT MULT DIV POWER PLUS MINUS
%token END

%left PLUS MINUS
%left MULT DIV
%nonassoc UNARY
%nonassoc POWER
%nonassoc DOT

%start expr
%type < Comphep_syntax.raw > expr

%%

expr:
    e END                  { $1 }
;

e:
    SYMBOL                 { S.symbol $1 }
  | INT                    { S.integer $1 }
  | I                      { S.imag }
  | SYMBOL LPAREN e RPAREN { S.apply $1 $3 }
  | LPAREN e RPAREN        { $2 }
  | e DOT e                { S.dot $1 $3 }
  | e MULT e               { S.multiply $1 $3 }
  | e DIV e                { S.divide $1 $3 }
  | e PLUS e               { S.add $1 $3 }
  | e MINUS e              { S.subtract $1 $3 }
  | PLUS e %prec UNARY     { $2 }
  | MINUS e %prec UNARY    { S.neg $2 }
  | e POWER INT            { S.power $1 $3 }
;
