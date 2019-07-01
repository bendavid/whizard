type token =
  | FLAVOR of ( string )
  | INT of ( int )
  | LPAREN
  | RPAREN
  | AND
  | OR
  | PLUS
  | COLON
  | NOT
  | ONSHELL
  | OFFSHELL
  | GAUSS
  | END

open Parsing;;
# 25 "../../../../src/omega/src/cascade_parser.mly"
open Cascade_syntax
let parse_error msg =
  raise (Syntax_Error (msg, symbol_start (), symbol_end ()))
# 22 "cascade_parser.ml"
let yytransl_const = [|
  259 (* LPAREN *);
  260 (* RPAREN *);
  261 (* AND *);
  262 (* OR *);
  263 (* PLUS *);
  264 (* COLON *);
  265 (* NOT *);
  266 (* ONSHELL *);
  267 (* OFFSHELL *);
  268 (* GAUSS *);
  269 (* END *);
    0|]

let yytransl_block = [|
  257 (* FLAVOR *);
  258 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\004\000\004\000\006\000\
\005\000\005\000\000\000"

let yylen = "\002\000\
\001\000\002\000\001\000\003\000\003\000\003\000\001\000\003\000\
\004\000\003\000\004\000\003\000\004\000\001\000\003\000\001\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\016\000\000\000\001\000\019\000\000\000\003\000\
\000\000\014\000\000\000\000\000\000\000\002\000\000\000\000\000\
\000\000\000\000\004\000\005\000\000\000\015\000\017\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\018\000"

let yydgoto = "\002\000\
\006\000\007\000\008\000\009\000\025\000\010\000"

let yysindex = "\005\000\
\000\255\000\000\000\000\022\255\000\000\000\000\255\254\000\000\
\054\255\000\000\063\255\022\255\022\255\000\000\006\255\048\255\
\050\255\053\255\000\000\000\000\005\255\000\000\000\000\027\255\
\019\255\027\255\019\255\027\255\019\255\019\255\039\255\019\255\
\019\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\010\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\037\255\000\000\000\000\000\000\
\013\255\000\000\016\255\000\000\026\255\029\255\000\000\032\255\
\042\255\000\000"

let yygindex = "\000\000\
\000\000\040\000\000\000\000\000\239\255\041\000"

let yytablesize = 69
let yytable = "\027\000\
\029\000\003\000\004\000\012\000\013\000\001\000\030\000\003\000\
\032\000\012\000\033\000\014\000\005\000\007\000\007\000\007\000\
\008\000\008\000\008\000\010\000\010\000\010\000\007\000\003\000\
\004\000\008\000\031\000\023\000\010\000\012\000\012\000\012\000\
\009\000\009\000\009\000\011\000\011\000\011\000\012\000\034\000\
\006\000\009\000\006\000\011\000\011\000\013\000\013\000\013\000\
\023\000\006\000\023\000\020\000\021\000\023\000\013\000\022\000\
\024\000\000\000\026\000\000\000\015\000\028\000\000\000\016\000\
\017\000\018\000\019\000\012\000\013\000"

let yycheck = "\017\000\
\018\000\002\001\003\001\005\001\006\001\001\000\024\000\002\001\
\026\000\005\001\028\000\013\001\013\001\004\001\005\001\006\001\
\004\001\005\001\006\001\004\001\005\001\006\001\013\001\002\001\
\003\001\013\001\008\001\001\001\013\001\004\001\005\001\006\001\
\004\001\005\001\006\001\004\001\005\001\006\001\013\001\001\001\
\004\001\013\001\006\001\004\000\013\001\004\001\005\001\006\001\
\001\001\013\001\001\001\012\000\013\000\001\001\013\001\015\000\
\009\001\255\255\009\001\255\255\007\001\009\001\255\255\010\001\
\011\001\012\001\004\001\005\001\006\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  AND\000\
  OR\000\
  PLUS\000\
  COLON\000\
  NOT\000\
  ONSHELL\000\
  OFFSHELL\000\
  GAUSS\000\
  END\000\
  "

let yynames_block = "\
  FLAVOR\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "../../../../src/omega/src/cascade_parser.mly"
                                    ( mk_true () )
# 127 "cascade_parser.ml"
               :  (string, int list) Cascade_syntax.t ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cascades) in
    Obj.repr(
# 48 "../../../../src/omega/src/cascade_parser.mly"
                                    ( _1 )
# 134 "cascade_parser.ml"
               :  (string, int list) Cascade_syntax.t ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cascade) in
    Obj.repr(
# 52 "../../../../src/omega/src/cascade_parser.mly"
                                    ( _1 )
# 141 "cascade_parser.ml"
               : 'cascades))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cascades) in
    Obj.repr(
# 53 "../../../../src/omega/src/cascade_parser.mly"
                                    ( _2 )
# 148 "cascade_parser.ml"
               : 'cascades))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cascades) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cascades) in
    Obj.repr(
# 54 "../../../../src/omega/src/cascade_parser.mly"
                                    ( mk_and _1 _3 )
# 156 "cascade_parser.ml"
               : 'cascades))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cascades) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cascades) in
    Obj.repr(
# 55 "../../../../src/omega/src/cascade_parser.mly"
                                    ( mk_or _1 _3 )
# 164 "cascade_parser.ml"
               : 'cascades))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'momentum_list) in
    Obj.repr(
# 59 "../../../../src/omega/src/cascade_parser.mly"
                                    ( mk_any_flavor _1 )
# 171 "cascade_parser.ml"
               : 'cascade))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'momentum_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'flavor_list) in
    Obj.repr(
# 61 "../../../../src/omega/src/cascade_parser.mly"
                                    ( mk_on_shell _3 _1 )
# 179 "cascade_parser.ml"
               : 'cascade))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'momentum_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'flavor_list) in
    Obj.repr(
# 63 "../../../../src/omega/src/cascade_parser.mly"
                                    ( mk_on_shell_not _4 _1 )
# 187 "cascade_parser.ml"
               : 'cascade))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'momentum_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'flavor_list) in
    Obj.repr(
# 65 "../../../../src/omega/src/cascade_parser.mly"
                                    ( mk_off_shell _3 _1 )
# 195 "cascade_parser.ml"
               : 'cascade))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'momentum_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'flavor_list) in
    Obj.repr(
# 67 "../../../../src/omega/src/cascade_parser.mly"
                                    ( mk_off_shell_not _4 _1 )
# 203 "cascade_parser.ml"
               : 'cascade))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'momentum_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'flavor_list) in
    Obj.repr(
# 68 "../../../../src/omega/src/cascade_parser.mly"
                                    ( mk_gauss _3 _1 )
# 211 "cascade_parser.ml"
               : 'cascade))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'momentum_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'flavor_list) in
    Obj.repr(
# 70 "../../../../src/omega/src/cascade_parser.mly"
                                    ( mk_gauss_not _4 _1 )
# 219 "cascade_parser.ml"
               : 'cascade))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'momentum) in
    Obj.repr(
# 74 "../../../../src/omega/src/cascade_parser.mly"
                                    ( [_1] )
# 226 "cascade_parser.ml"
               : 'momentum_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'momentum_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'momentum) in
    Obj.repr(
# 75 "../../../../src/omega/src/cascade_parser.mly"
                                    ( _3 :: _1 )
# 234 "cascade_parser.ml"
               : 'momentum_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  int ) in
    Obj.repr(
# 79 "../../../../src/omega/src/cascade_parser.mly"
                                    ( _1 )
# 241 "cascade_parser.ml"
               : 'momentum))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  string ) in
    Obj.repr(
# 83 "../../../../src/omega/src/cascade_parser.mly"
                                    ( [_1] )
# 248 "cascade_parser.ml"
               : 'flavor_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'flavor_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  string ) in
    Obj.repr(
# 84 "../../../../src/omega/src/cascade_parser.mly"
                                    ( _3 :: _1 )
# 256 "cascade_parser.ml"
               : 'flavor_list))
(* Entry main *)
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
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  (string, int list) Cascade_syntax.t )
