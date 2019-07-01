type token =
  | INT of ( int )
  | FLOAT of ( float )
  | STRING of ( string )
  | ID of ( string )
  | DOT
  | COMMA
  | COLON
  | EQUAL
  | PLUS
  | MINUS
  | DIV
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | END

open Parsing;;
let _ = parse_error;;
# 31 "../../../omega/src/UFO_parser.mly"
module U = UFO_syntax

let parse_error msg =
  raise (UFO_syntax.Syntax_Error
	   (msg, symbol_start_pos (), symbol_end_pos ()))

let invalid_parameter_attr () =
  parse_error "invalid parameter attribute"

# 34 "UFO_parser.ml"
let yytransl_const = [|
  261 (* DOT *);
  262 (* COMMA *);
  263 (* COLON *);
  264 (* EQUAL *);
  265 (* PLUS *);
  266 (* MINUS *);
  267 (* DIV *);
  268 (* LPAREN *);
  269 (* RPAREN *);
  270 (* LBRACE *);
  271 (* RBRACE *);
  272 (* LBRACKET *);
  273 (* RBRACKET *);
  274 (* END *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* FLOAT *);
  259 (* STRING *);
  260 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\004\000\004\000\
\005\000\005\000\006\000\006\000\006\000\007\000\007\000\007\000\
\007\000\007\000\008\000\008\000\008\000\008\000\009\000\009\000\
\009\000\010\000\010\000\012\000\012\000\011\000\011\000\013\000\
\013\000\016\000\014\000\014\000\017\000\015\000\015\000\018\000\
\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\005\000\006\000\003\000\001\000\003\000\
\001\000\003\000\003\000\003\000\003\000\001\000\003\000\001\000\
\001\000\001\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\001\000\003\000\001\000\003\000\001\000\003\000\001\000\
\003\000\003\000\001\000\003\000\007\000\001\000\003\000\005\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\041\000\000\000\000\000\000\000\001\000\
\003\000\006\000\007\000\000\000\000\000\000\000\008\000\000\000\
\004\000\000\000\000\000\000\000\005\000\000\000\000\000\016\000\
\017\000\000\000\000\000\000\000\011\000\012\000\013\000\010\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\019\000\000\000\000\000\000\000\000\000\
\015\000\000\000\000\000\000\000\023\000\024\000\025\000\000\000\
\000\000\000\000\000\000\000\000\000\000\020\000\021\000\022\000\
\034\000\000\000\000\000\033\000\000\000\036\000\000\000\039\000\
\029\000\031\000\027\000\000\000\000\000\000\000\040\000\000\000\
\000\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\045\000\018\000\019\000\029\000\030\000\
\031\000\052\000\047\000\048\000\036\000\037\000\038\000\039\000\
\040\000\041\000"

let yysindex = "\013\000\
\023\255\000\000\025\255\000\000\020\255\023\255\028\255\000\000\
\000\000\000\000\000\000\011\255\030\255\255\254\000\000\032\255\
\000\000\026\255\035\255\005\255\000\000\038\255\033\255\000\000\
\000\000\008\255\001\255\040\255\000\000\000\000\000\000\000\000\
\042\255\039\255\024\255\034\255\036\255\037\255\041\255\044\255\
\047\255\048\255\049\255\000\000\031\255\043\255\045\255\046\255\
\000\000\055\255\051\255\052\255\000\000\000\000\000\000\056\255\
\054\255\057\255\060\255\061\255\063\255\000\000\000\000\000\000\
\000\000\067\255\064\255\000\000\069\255\000\000\063\255\000\000\
\000\000\000\000\000\000\059\255\071\255\068\255\000\000\063\255\
\040\255"

let yyrindex = "\000\000\
\058\255\000\000\000\000\000\000\000\000\058\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\065\255\000\000\000\000\000\000\004\255\000\000\
\000\000\000\000\000\000\016\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\062\255\066\255\
\070\255\072\255\073\255\000\000\013\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\009\255"

let yygindex = "\000\000\
\000\000\042\000\000\000\249\255\036\000\000\000\000\000\000\000\
\000\000\230\255\019\000\021\000\026\000\027\000\025\000\000\000\
\000\000\000\000"

let yytablesize = 90
let yytable = "\012\000\
\046\000\042\000\016\000\043\000\011\000\023\000\024\000\025\000\
\011\000\014\000\034\000\017\000\028\000\001\000\037\000\013\000\
\014\000\044\000\026\000\035\000\027\000\018\000\014\000\037\000\
\051\000\026\000\003\000\011\000\018\000\026\000\010\000\011\000\
\007\000\015\000\075\000\013\000\061\000\008\000\021\000\020\000\
\022\000\016\000\049\000\033\000\013\000\050\000\056\000\009\000\
\053\000\057\000\054\000\055\000\058\000\059\000\060\000\065\000\
\066\000\032\000\034\000\062\000\042\000\063\000\064\000\043\000\
\067\000\069\000\011\000\076\000\071\000\051\000\077\000\078\000\
\081\000\079\000\080\000\002\000\032\000\009\000\074\000\073\000\
\035\000\068\000\072\000\070\000\038\000\000\000\000\000\000\000\
\028\000\030\000"

let yycheck = "\007\000\
\027\000\001\001\004\001\003\001\004\001\001\001\002\001\003\001\
\004\001\006\001\003\001\013\001\020\000\001\000\006\001\005\001\
\013\001\017\001\014\001\012\001\016\001\006\001\012\001\015\001\
\001\001\013\001\004\001\004\001\013\001\017\001\003\001\004\001\
\008\001\004\001\061\000\005\001\006\001\018\001\013\001\008\001\
\006\001\004\001\001\001\011\001\005\001\007\001\006\001\006\000\
\015\001\006\001\015\001\015\001\006\001\006\001\006\001\001\001\
\006\001\022\000\003\001\017\001\001\001\017\001\017\001\003\001\
\013\001\012\001\004\001\001\001\012\001\001\001\007\001\013\001\
\080\000\003\001\007\001\018\001\015\001\013\001\060\000\059\000\
\015\001\056\000\058\000\057\000\015\001\255\255\255\255\255\255\
\017\001\017\001"

let yynames_const = "\
  DOT\000\
  COMMA\000\
  COLON\000\
  EQUAL\000\
  PLUS\000\
  MINUS\000\
  DIV\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACKET\000\
  RBRACKET\000\
  END\000\
  "

let yynames_block = "\
  INT\000\
  FLOAT\000\
  STRING\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'declarations) in
    Obj.repr(
# 59 "../../../omega/src/UFO_parser.mly"
                    ( _1 )
# 184 "UFO_parser.ml"
               :  UFO_syntax.t ))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "../../../omega/src/UFO_parser.mly"
                            ( [] )
# 190 "UFO_parser.ml"
               : 'declarations))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'declaration) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'declarations) in
    Obj.repr(
# 64 "../../../omega/src/UFO_parser.mly"
                            ( _1 :: _2 )
# 198 "UFO_parser.ml"
               : 'declarations))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 :  string ) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    Obj.repr(
# 68 "../../../omega/src/UFO_parser.mly"
                                          ( { U.name = _1;
					      U.kind = _3;
					      U.attribs = [] } )
# 208 "UFO_parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 :  string ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'name) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'attributes) in
    Obj.repr(
# 71 "../../../omega/src/UFO_parser.mly"
                                          ( { U.name = _1;
					      U.kind = _3;
					      U.attribs = _5 } )
# 219 "UFO_parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  string ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  string ) in
    Obj.repr(
# 74 "../../../omega/src/UFO_parser.mly"
                                          ( { U.name = _1;
					      U.kind = ["$"; _3]; (* HACK! *)
					      U.attribs = [] } )
# 229 "UFO_parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  string ) in
    Obj.repr(
# 80 "../../../omega/src/UFO_parser.mly"
               ( [_1] )
# 236 "UFO_parser.ml"
               : 'name))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  string ) in
    Obj.repr(
# 81 "../../../omega/src/UFO_parser.mly"
               ( _3 :: _1 )
# 244 "UFO_parser.ml"
               : 'name))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'attribute) in
    Obj.repr(
# 85 "../../../omega/src/UFO_parser.mly"
                              ( [_1] )
# 251 "UFO_parser.ml"
               : 'attributes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'attribute) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributes) in
    Obj.repr(
# 86 "../../../omega/src/UFO_parser.mly"
                              ( _1 :: _3 )
# 259 "UFO_parser.ml"
               : 'attributes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  string ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 90 "../../../omega/src/UFO_parser.mly"
                       ( { U.a_name = _1; U.a_value = _3 } )
# 267 "UFO_parser.ml"
               : 'attribute))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  string ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 91 "../../../omega/src/UFO_parser.mly"
                       ( { U.a_name = _1; U.a_value = _3 } )
# 275 "UFO_parser.ml"
               : 'attribute))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  string ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'dictionary) in
    Obj.repr(
# 92 "../../../omega/src/UFO_parser.mly"
                       ( { U.a_name = _1; U.a_value = _3 } )
# 283 "UFO_parser.ml"
               : 'attribute))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  int ) in
    Obj.repr(
# 96 "../../../omega/src/UFO_parser.mly"
               ( U.Integer _1 )
# 290 "UFO_parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  int ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  int ) in
    Obj.repr(
# 97 "../../../omega/src/UFO_parser.mly"
               ( U.Fraction (_1, _3) )
# 298 "UFO_parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  float ) in
    Obj.repr(
# 98 "../../../omega/src/UFO_parser.mly"
               ( U.Float _1 )
# 305 "UFO_parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  string ) in
    Obj.repr(
# 99 "../../../omega/src/UFO_parser.mly"
               ( U.String _1 )
# 312 "UFO_parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 100 "../../../omega/src/UFO_parser.mly"
               ( U.Name _1 )
# 319 "UFO_parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    Obj.repr(
# 104 "../../../omega/src/UFO_parser.mly"
                            ( U.Empty_List )
# 325 "UFO_parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'names) in
    Obj.repr(
# 105 "../../../omega/src/UFO_parser.mly"
                              ( U.Name_List _2 )
# 332 "UFO_parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'strings) in
    Obj.repr(
# 106 "../../../omega/src/UFO_parser.mly"
                              ( U.String_List _2 )
# 339 "UFO_parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'integers) in
    Obj.repr(
# 107 "../../../omega/src/UFO_parser.mly"
                              ( U.Integer_List _2 )
# 346 "UFO_parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'orders) in
    Obj.repr(
# 111 "../../../omega/src/UFO_parser.mly"
                           ( U.Order_Dictionary _2 )
# 353 "UFO_parser.ml"
               : 'dictionary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'couplings) in
    Obj.repr(
# 112 "../../../omega/src/UFO_parser.mly"
                           ( U.Coupling_Dictionary _2 )
# 360 "UFO_parser.ml"
               : 'dictionary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'decays) in
    Obj.repr(
# 113 "../../../omega/src/UFO_parser.mly"
                           ( U.Decay_Dictionary _2 )
# 367 "UFO_parser.ml"
               : 'dictionary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 117 "../../../omega/src/UFO_parser.mly"
                    ( [_1] )
# 374 "UFO_parser.ml"
               : 'names))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'names) in
    Obj.repr(
# 118 "../../../omega/src/UFO_parser.mly"
                    ( _1 :: _3 )
# 382 "UFO_parser.ml"
               : 'names))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  int ) in
    Obj.repr(
# 122 "../../../omega/src/UFO_parser.mly"
                      ( [_1] )
# 389 "UFO_parser.ml"
               : 'integers))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  int ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'integers) in
    Obj.repr(
# 123 "../../../omega/src/UFO_parser.mly"
                      ( _1 :: _3 )
# 397 "UFO_parser.ml"
               : 'integers))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  string ) in
    Obj.repr(
# 127 "../../../omega/src/UFO_parser.mly"
                        ( [_1] )
# 404 "UFO_parser.ml"
               : 'strings))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  string ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'strings) in
    Obj.repr(
# 128 "../../../omega/src/UFO_parser.mly"
                        ( _1 :: _3 )
# 412 "UFO_parser.ml"
               : 'strings))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'order) in
    Obj.repr(
# 132 "../../../omega/src/UFO_parser.mly"
                      ( [_1] )
# 419 "UFO_parser.ml"
               : 'orders))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'order) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'orders) in
    Obj.repr(
# 133 "../../../omega/src/UFO_parser.mly"
                      ( _1 :: _3 )
# 427 "UFO_parser.ml"
               : 'orders))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  string ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  int ) in
    Obj.repr(
# 137 "../../../omega/src/UFO_parser.mly"
                    ( (_1, _3) )
# 435 "UFO_parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'coupling) in
    Obj.repr(
# 141 "../../../omega/src/UFO_parser.mly"
                            ( [_1] )
# 442 "UFO_parser.ml"
               : 'couplings))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'coupling) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'couplings) in
    Obj.repr(
# 142 "../../../omega/src/UFO_parser.mly"
                            ( _1 :: _3 )
# 450 "UFO_parser.ml"
               : 'couplings))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 :  int ) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 :  int ) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 146 "../../../omega/src/UFO_parser.mly"
                                          ( (_2, _4, _7) )
# 459 "UFO_parser.ml"
               : 'coupling))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decay) in
    Obj.repr(
# 150 "../../../omega/src/UFO_parser.mly"
                      ( [_1] )
# 466 "UFO_parser.ml"
               : 'decays))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'decay) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decays) in
    Obj.repr(
# 151 "../../../omega/src/UFO_parser.mly"
                      ( _1 :: _3 )
# 474 "UFO_parser.ml"
               : 'decays))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'names) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 :  string ) in
    Obj.repr(
# 155 "../../../omega/src/UFO_parser.mly"
                                    ( (_2, _5) )
# 482 "UFO_parser.ml"
               : 'decay))
(* Entry file *)
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
let file (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  UFO_syntax.t )
