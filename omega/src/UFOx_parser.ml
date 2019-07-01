type token =
  | INT of ( int )
  | FLOAT of ( float )
  | ID of ( string )
  | PLUS
  | MINUS
  | TIMES
  | POWER
  | DIV
  | LPAREN
  | RPAREN
  | COMMA
  | DOT
  | END

open Parsing;;
let _ = parse_error;;
# 31 "../../../omega/src/UFOx_parser.mly"
module X = UFOx_syntax

let parse_error msg =
  raise (UFOx_syntax.Syntax_Error
	   (msg, symbol_start_pos (), symbol_end_pos ()))

let invalid_parameter_attr () =
  parse_error "invalid parameter attribute"

# 29 "UFOx_parser.ml"
let yytransl_const = [|
  260 (* PLUS *);
  261 (* MINUS *);
  262 (* TIMES *);
  263 (* POWER *);
  264 (* DIV *);
  265 (* LPAREN *);
  266 (* RPAREN *);
  267 (* COMMA *);
  268 (* DOT *);
  269 (* END *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* FLOAT *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\003\000\003\000\
\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\003\000\003\000\003\000\003\000\
\002\000\002\000\003\000\003\000\003\000\004\000\001\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\003\000\000\000\000\000\000\000\000\000\
\017\000\000\000\000\000\009\000\010\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\013\000\000\000\000\000\012\000\
\000\000\000\000\000\000\011\000\000\000\000\000\014\000\016\000"

let yydgoto = "\002\000\
\009\000\022\000\023\000"

let yysindex = "\003\000\
\073\255\000\000\000\000\000\000\248\254\073\255\073\255\073\255\
\000\000\055\255\028\255\000\000\000\000\087\255\073\255\073\255\
\073\255\073\255\073\255\000\000\000\000\079\255\000\255\000\000\
\092\255\092\255\005\255\000\000\005\255\073\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\015\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\003\255\000\000\000\000\
\254\254\060\255\035\255\000\000\045\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\250\255"

let yytablesize = 100
let yytable = "\010\000\
\011\000\005\000\005\000\001\000\012\000\013\000\014\000\005\000\
\005\000\031\000\005\000\018\000\015\000\025\000\026\000\027\000\
\028\000\029\000\004\000\004\000\004\000\004\000\004\000\032\000\
\004\000\004\000\000\000\004\000\003\000\004\000\005\000\006\000\
\007\000\000\000\000\000\000\000\008\000\021\000\007\000\007\000\
\007\000\000\000\007\000\000\000\007\000\007\000\000\000\007\000\
\008\000\008\000\008\000\000\000\008\000\000\000\008\000\008\000\
\000\000\008\000\015\000\016\000\017\000\018\000\019\000\006\000\
\006\000\000\000\000\000\020\000\000\000\006\000\006\000\000\000\
\006\000\003\000\004\000\005\000\006\000\007\000\000\000\000\000\
\000\000\008\000\015\000\016\000\017\000\018\000\019\000\000\000\
\000\000\030\000\015\000\016\000\017\000\018\000\019\000\000\000\
\024\000\017\000\018\000\019\000"

let yycheck = "\001\000\
\009\001\004\001\005\001\001\000\006\000\007\000\008\000\010\001\
\011\001\010\001\013\001\007\001\010\001\015\000\016\000\017\000\
\018\000\019\000\004\001\005\001\006\001\007\001\008\001\030\000\
\010\001\011\001\255\255\013\001\001\001\002\001\003\001\004\001\
\005\001\255\255\255\255\255\255\009\001\010\001\004\001\005\001\
\006\001\255\255\008\001\255\255\010\001\011\001\255\255\013\001\
\004\001\005\001\006\001\255\255\008\001\255\255\010\001\011\001\
\255\255\013\001\004\001\005\001\006\001\007\001\008\001\004\001\
\005\001\255\255\255\255\013\001\255\255\010\001\011\001\255\255\
\013\001\001\001\002\001\003\001\004\001\005\001\255\255\255\255\
\255\255\009\001\004\001\005\001\006\001\007\001\008\001\255\255\
\255\255\011\001\004\001\005\001\006\001\007\001\008\001\255\255\
\010\001\006\001\007\001\008\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  POWER\000\
  DIV\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  DOT\000\
  END\000\
  "

let yynames_block = "\
  INT\000\
  FLOAT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 61 "../../../omega/src/UFOx_parser.mly"
            ( _1 )
# 140 "UFOx_parser.ml"
               :  UFOx_syntax.expr ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  int ) in
    Obj.repr(
# 65 "../../../omega/src/UFOx_parser.mly"
                      ( X.integer _1 )
# 147 "UFOx_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  float ) in
    Obj.repr(
# 66 "../../../omega/src/UFOx_parser.mly"
                      ( X.float _1 )
# 154 "UFOx_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  string ) in
    Obj.repr(
# 67 "../../../omega/src/UFOx_parser.mly"
                      ( X.variable _1 )
# 161 "UFOx_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "../../../omega/src/UFOx_parser.mly"
                      ( X.add _1 _3 )
# 169 "UFOx_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "../../../omega/src/UFOx_parser.mly"
                      ( X.subtract _1 _3 )
# 177 "UFOx_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "../../../omega/src/UFOx_parser.mly"
                      ( X.multiply _1 _3 )
# 185 "UFOx_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "../../../omega/src/UFOx_parser.mly"
                      ( X.divide _1 _3 )
# 193 "UFOx_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "../../../omega/src/UFOx_parser.mly"
                          ( _2 )
# 200 "UFOx_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "../../../omega/src/UFOx_parser.mly"
                          ( X.multiply (X.integer (-1)) _2 )
# 207 "UFOx_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "../../../omega/src/UFOx_parser.mly"
                       ( X.power _1 _3 )
# 215 "UFOx_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 75 "../../../omega/src/UFOx_parser.mly"
                          ( _2 )
# 222 "UFOx_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  string ) in
    Obj.repr(
# 76 "../../../omega/src/UFOx_parser.mly"
                          ( X.apply _1 [] )
# 229 "UFOx_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 :  string ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 77 "../../../omega/src/UFOx_parser.mly"
                          ( X.apply _1 _3 )
# 237 "UFOx_parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "../../../omega/src/UFOx_parser.mly"
                   ( [_1] )
# 244 "UFOx_parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 82 "../../../omega/src/UFOx_parser.mly"
                   ( _1 :: _3 )
# 252 "UFOx_parser.ml"
               : 'args))
(* Entry input *)
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
let input (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  UFOx_syntax.expr )
