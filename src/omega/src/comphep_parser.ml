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

open Parsing;;
# 24 "../../../../src/omega/src/comphep_parser.mly"
module S = Comphep_syntax
# 19 "comphep_parser.ml"
let yytransl_const = [|
  259 (* I *);
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* DOT *);
  263 (* MULT *);
  264 (* DIV *);
  265 (* POWER *);
  266 (* PLUS *);
  267 (* MINUS *);
  268 (* END *);
    0|]

let yytransl_block = [|
  257 (* SYMBOL *);
  258 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\004\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\002\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\003\000\004\000\000\000\000\000\000\000\
\015\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\006\000\007\000\
\000\000\000\000\014\000\000\000\000\000\005\000"

let yydgoto = "\002\000\
\009\000\010\000"

let yysindex = "\003\000\
\014\255\000\000\003\255\000\000\000\000\014\255\014\255\014\255\
\000\000\068\255\014\255\076\255\253\254\253\254\014\255\014\255\
\014\255\006\255\014\255\014\255\000\000\083\255\000\000\000\000\
\253\254\253\254\000\000\013\255\013\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\021\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\029\255\037\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\045\255\053\255\000\000\057\255\061\255\000\000"

let yygindex = "\000\000\
\000\000\250\255"

let yytablesize = 94
let yytable = "\012\000\
\013\000\014\000\015\000\001\000\022\000\018\000\011\000\027\000\
\024\000\025\000\026\000\000\000\028\000\029\000\003\000\004\000\
\005\000\006\000\015\000\016\000\017\000\018\000\000\000\007\000\
\008\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\012\000\000\000\012\000\012\000\000\000\012\000\012\000\
\012\000\013\000\000\000\013\000\013\000\000\000\013\000\013\000\
\013\000\008\000\000\000\008\000\008\000\000\000\008\000\008\000\
\008\000\009\000\000\000\009\000\009\000\010\000\009\000\009\000\
\009\000\011\000\010\000\010\000\010\000\000\000\011\000\011\000\
\011\000\015\000\016\000\017\000\018\000\019\000\020\000\021\000\
\023\000\015\000\016\000\017\000\018\000\019\000\020\000\030\000\
\015\000\016\000\017\000\018\000\019\000\020\000"

let yycheck = "\006\000\
\007\000\008\000\006\001\001\000\011\000\009\001\004\001\002\001\
\015\000\016\000\017\000\255\255\019\000\020\000\001\001\002\001\
\003\001\004\001\006\001\007\001\008\001\009\001\255\255\010\001\
\011\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\005\001\255\255\007\001\008\001\255\255\010\001\011\001\
\012\001\005\001\255\255\007\001\008\001\255\255\010\001\011\001\
\012\001\005\001\255\255\007\001\008\001\255\255\010\001\011\001\
\012\001\005\001\255\255\007\001\008\001\005\001\010\001\011\001\
\012\001\005\001\010\001\011\001\012\001\255\255\010\001\011\001\
\012\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001"

let yynames_const = "\
  I\000\
  LPAREN\000\
  RPAREN\000\
  DOT\000\
  MULT\000\
  DIV\000\
  POWER\000\
  PLUS\000\
  MINUS\000\
  END\000\
  "

let yynames_block = "\
  SYMBOL\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'e) in
    Obj.repr(
# 46 "../../../../src/omega/src/comphep_parser.mly"
                           ( _1 )
# 124 "comphep_parser.ml"
               :  Comphep_syntax.raw ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  string ) in
    Obj.repr(
# 50 "../../../../src/omega/src/comphep_parser.mly"
                           ( S.symbol _1 )
# 131 "comphep_parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  int ) in
    Obj.repr(
# 51 "../../../../src/omega/src/comphep_parser.mly"
                           ( S.integer _1 )
# 138 "comphep_parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "../../../../src/omega/src/comphep_parser.mly"
                           ( S.imag )
# 144 "comphep_parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 :  string ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'e) in
    Obj.repr(
# 53 "../../../../src/omega/src/comphep_parser.mly"
                           ( S.apply _1 _3 )
# 152 "comphep_parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'e) in
    Obj.repr(
# 54 "../../../../src/omega/src/comphep_parser.mly"
                           ( _2 )
# 159 "comphep_parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 55 "../../../../src/omega/src/comphep_parser.mly"
                           ( S.dot _1 _3 )
# 167 "comphep_parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 56 "../../../../src/omega/src/comphep_parser.mly"
                           ( S.multiply _1 _3 )
# 175 "comphep_parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 57 "../../../../src/omega/src/comphep_parser.mly"
                           ( S.divide _1 _3 )
# 183 "comphep_parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 58 "../../../../src/omega/src/comphep_parser.mly"
                           ( S.add _1 _3 )
# 191 "comphep_parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 59 "../../../../src/omega/src/comphep_parser.mly"
                           ( S.subtract _1 _3 )
# 199 "comphep_parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 60 "../../../../src/omega/src/comphep_parser.mly"
                           ( _2 )
# 206 "comphep_parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 61 "../../../../src/omega/src/comphep_parser.mly"
                           ( S.neg _2 )
# 213 "comphep_parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  int ) in
    Obj.repr(
# 62 "../../../../src/omega/src/comphep_parser.mly"
                           ( S.power _1 _3 )
# 221 "comphep_parser.ml"
               : 'e))
(* Entry expr *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let expr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  Comphep_syntax.raw )
