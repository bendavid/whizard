/* $Id: autocheckup_parser.mly 2451 2010-04-30 10:27:01Z ohl $ */

%{
open Autocheckup_syntax
let parse_error msg =
  raise (Syntax_Error (msg, symbol_start (), symbol_end ()))
%}

%token < string > STRING
%token LET ADDTO REMOVE PROHIBIT SEMI
%token OPEN CLOSE
%token END

%start main
%type < Autocheckup_syntax.t > main

%%

main:
    specs END                             { $1 }
  | specs SEMI END                        { $1 }
  | END                                   { [] }
;

specs:
    spec                                  { [$1] }
  | spec SEMI specs                       { $1 :: $3 }
;

spec:
    OPEN CLOSE                            { Autocheckup_syntax.group [] }
  | OPEN specs CLOSE                      { Autocheckup_syntax.group $2 }
  | OPEN specs SEMI CLOSE                 { Autocheckup_syntax.group $2 }
  | STRING LET strings                    { Autocheckup_syntax.spec $1 $3 }
  | STRING ADDTO strings                  { Autocheckup_syntax.addto $1 $3 }
  | STRING REMOVE strings                 { Autocheckup_syntax.remove $1 $3 }
  | STRING PROHIBIT strings               { Autocheckup_syntax.prohibit $1 $3 }
;

strings:
    STRING                                { [$1] }
  | STRING strings                        { $1 :: $2 }
;

