(* vertex.ml --

   Copyright (C) 1999-2017 by

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

let error_in_string text start_pos end_pos =
  let i = max 0 start_pos.Lexing.pos_cnum in
  let j = min (String.length text) (max (i + 1) end_pos.Lexing.pos_cnum) in
  String.sub text i (j - i)

let error_in_file name start_pos end_pos =
  Printf.sprintf
    "%s:%d.%d-%d.%d"
    name
    start_pos.Lexing.pos_lnum
    (start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol)
    end_pos.Lexing.pos_lnum
    (end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol)

module Expr =
  struct

    type t = UFOx_syntax.expr

    let of_string text =
      try
	UFOx_parser.input
	  UFOx_lexer.token
	  (UFOx_lexer.init_position "" (Lexing.from_string text))
      with
      | UFOx_syntax.Syntax_Error (msg, start_pos, end_pos) ->
	 invalid_arg (Printf.sprintf "syntax error (%s) at: `%s'"
			msg  (error_in_string text start_pos end_pos))
      | Parsing.Parse_error ->
	 invalid_arg ("parse error: " ^ text)

    let of_strings = function
      | [] -> UFOx_syntax.integer 0
      | string :: strings ->
	 List.fold_right
	   (fun s acc -> UFOx_syntax.add (of_string s) acc)
	   strings (of_string string)

  end

let positive integers =
  List.filter (fun (i, _) -> i > 0) integers

let not_positive integers =
  List.filter (fun (i, _) -> i <= 0) integers

let int_list_to_string is =
  "[" ^ String.concat ", " (List.map string_of_int is) ^ "]"
	
module Q = Algebra.Small_Rational

module type Index =
  sig
    val free : (int * 'r) list -> (int * 'r) list
    val summation : (int * 'r) list -> (int * 'r) list
    val classes_to_string : ('r -> string) -> (int * 'r) list -> string
  end

module Index : Index =
  struct

    let free i = positive i
    let summation i = not_positive i

    let classes_to_string rep_to_string index_classes =
      let reps =
	ThoList.uniq (List.sort compare (List.map snd index_classes)) in
      "[" ^
	String.concat ", "
	(List.map
	   (fun r ->
	     (rep_to_string r) ^ "=" ^
	       (int_list_to_string
		  (List.map
		     fst
		     (List.filter (fun (_, r') -> r = r') index_classes))))
	   reps) ^ "]"
      
  end

module type Atom =
  sig
    type t
    val of_expr : string -> UFOx_syntax.expr list -> t
    val to_string : t -> string
    type r
    val classify_indices : t list -> (int * r) list
    val rep_to_string : r -> string
    val rep_of_int : int -> r
    val rep_conjugate : r -> r
    val rep_trivial : r -> bool
    type r_omega
    val omega : r -> r_omega
  end

module type Tensor =
  sig
    type atom
    type t = (atom list * Q.t) list
    val of_expr : UFOx_syntax.expr -> t
    val of_string : string -> t
    val of_strings : string list -> t
    val to_string : t -> string
    type r
    val classify_indices : t -> (int * r) list 
    val rep_to_string : r -> string
    val rep_of_int : int -> r
    val rep_conjugate : r -> r
    val rep_trivial : r -> bool
    type r_omega
    val omega : r -> r_omega
  end

module Tensor (A : Atom) : Tensor
  with type atom = A.t and type r = A.r and type r_omega = A.r_omega =
  struct

    module S = UFOx_syntax

    type atom = A.t
    type t = (atom list * Q.t) list

    let multiply (t1, c1) (t2, c2) =
      (List.sort compare (t1 @ t2), Q.mul c1 c2)

    let compress terms =
      List.map (fun (t, cs) -> (t, Q.sum cs)) (ThoList.factorize terms)

    let rec of_expr e =
      compress (of_expr' e)

    and of_expr' = function
      | S.Integer i -> [([], Q.make i 1)]
      | S.Float _ -> invalid_arg "UFOx.Tensor.of_expr: unexpected float"
      | S.Variable name ->
	 invalid_arg ("UFOx.Tensor.of_expr: unexpected variable '" ^
			 name ^ "'")
      | S.Application (name, args) -> [([A.of_expr name args], Q.unit)]
      | S.Sum (e1, e2) ->
	 of_expr e1 @ of_expr e2
      | S.Difference (e1, e2) ->
	 of_expr e1 @ of_expr (S.Product (S.Integer (-1), e2))
      | S.Product (e1, e2) -> Product.list2 multiply (of_expr e1) (of_expr e2)
      | S.Quotient (n, d) ->
	 begin match of_expr d with
	 | [([], q)] ->
	    List.map (fun (t, c) -> (t, Q.div c q)) (of_expr n)
	 | [] ->
	    failwith "UFOx.Tensor.of_expr: zero denominator"
	 | _ ->
	    failwith "UFOx.Tensor.of_expr: only integer denominators allowed"
	 end
      | S.Power (e, p) ->
	 begin match of_expr e, of_expr p with
	 | [([], q)], [([], p)] ->
	    if Q.is_integer p then
	      [([], Q.pow q (Q.to_integer p))]
	    else
	      failwith "UFOx.Tensor.of_expr: rational power"
	 | [([], q)], _ ->
	    failwith "UFOx.Tensor.of_expr: non-numeric power"
	 | _ -> failwith "UFOx.Tensor.of_expr: power of tensor"
	 end

    type r = A.r
    let rep_to_string = A.rep_to_string
    let rep_of_int = A.rep_of_int
    let rep_conjugate = A.rep_conjugate
    let rep_trivial = A.rep_trivial

    let classify_indices' filter tensors =
      ThoList.uniq
	(List.sort compare
	   (List.map (fun (t, c) -> filter (A.classify_indices t)) tensors))

    let classify_indices tensors =
      let free_indices = classify_indices' Index.free tensors
      and summation_indices = classify_indices' Index.summation tensors in
      match free_indices, summation_indices with
      | [], _ -> failwith "UFOx.Tensor.classify_indices: can't happen!"
      | [f], [s] -> f
      | [_], _ ->
	 invalid_arg
	   "UFOx.Tensor.classify_indices: superfluous summation indices!"
      | _, _ ->
	 invalid_arg "UFOx.Tensor.classify_indices: incompatible free indices!"

    let of_expr e =
      let t = of_expr e in
      let free = classify_indices t in
      t

    let of_string s =
      of_expr (Expr.of_string s)

    let of_strings s =
      of_expr (Expr.of_strings s)

    let term_to_string (tensors, c) =
      if Q.is_null c then
	""
      else
	(if Q.is_negative c then " - " else " + ") ^
	  (let c = Q.abs c in
	   if Q.is_unit c && tensors = [] then
	     ""
	   else
	     Q.to_string c) ^
	  (match tensors with
	  | [] -> ""
	  | tensors ->
	     (if Q.is_unit (Q.abs c) then "" else "*") ^
	       String.concat "*" (List.map A.to_string tensors))

    let term_to_string (tensors, c) =
      if Q.is_null c then
	""
      else
	(if Q.is_negative c then " - " else " + ") ^
	  (let c = Q.abs c in
	   match tensors with
	   | [] -> Q.to_string c
	   | tensors ->
	      String.concat "*"
		((if Q.is_unit c then [] else [Q.to_string c]) @
		    List.map A.to_string tensors))

    let to_string terms =
      String.concat "" (List.map term_to_string terms)
      
    type r_omega = A.r_omega
    let omega = A.omega

  end

module type Lorentz_Atom =
  sig
    type t = private
      | C of int * int
      | Epsilon of int * int * int * int
      | Gamma of int * int * int
      | Gamma5 of int * int
      | Identity of int * int
      | Metric of int * int
      | P of int * int
      | ProjP of int * int
      | ProjM of int * int
      | Sigma of int * int * int * int
  end

module Lorentz_Atom =
  struct
    type t =
      | C of int * int
      | Epsilon of int * int * int * int
      | Gamma of int * int * int
      | Gamma5 of int * int
      | Identity of int * int
      | Metric of int * int
      | P of int * int
      | ProjP of int * int
      | ProjM of int * int
      | Sigma of int * int * int * int
  end

module Lorentz_Atom' : Atom
  with type t = Lorentz_Atom.t and type r_omega = Coupling.lorentz =
  struct
	
    type t = Lorentz_Atom.t

    open Lorentz_Atom
    
    let to_string = function
      | C (i, j) ->
	 Printf.sprintf "C(%d,%d)" i j
      | Epsilon (mu, nu, ka, la) ->
	 Printf.sprintf "Epsilon(%d,%d,%d,%d)" mu nu ka la
      | Gamma (mu, i, j) ->
	 Printf.sprintf "Gamma(%d,%d,%d)" mu i j
      | Gamma5 (i, j) ->
	 Printf.sprintf "Gamma5(%d,%d)" i j
      | Identity (i, j) ->
	 Printf.sprintf "Identity(%d,%d)" i j
      | Metric (mu, nu) ->
	 Printf.sprintf "Metric(%d,%d)" mu nu
      | P (mu, n) ->
	 Printf.sprintf "P(%d,%d)" mu n
      | ProjP (i, j) ->
	 Printf.sprintf "ProjP(%d,%d)" i j
      | ProjM (i, j) ->
	 Printf.sprintf "ProjM(%d,%d)" i j
      | Sigma (mu, nu, i, j) ->
	 Printf.sprintf "Sigma(%d,%d,%d,%d)" mu nu i j

    module S = UFOx_syntax

    let of_expr name args =
      match name, args with
      | "C", [S.Integer i; S.Integer j] -> C (i, j)
      | "C", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to C()"
      | "Epsilon", [S.Integer mu; S.Integer nu; S.Integer ka; S.Integer la] ->
	 Epsilon (mu, nu, ka, la)
      | "Epsilon", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Epsilon()"
      | "Gamma", [S.Integer mu; S.Integer i; S.Integer j] ->
	 Gamma (mu, i, j)
      | "Gamma", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Gamma()"
      | "Gamma5", [S.Integer i; S.Integer j] -> Gamma5 (i, j)
      | "Gamma5", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Gamma5()"
      | "Identity", [S.Integer i; S.Integer j] -> Identity (i, j)
      | "Identity", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Identity()"
      | "Metric", [S.Integer mu; S.Integer nu] -> Metric (mu, nu)
      | "Metric", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Metric()"
      | "P", [S.Integer mu; S.Integer n] -> P (mu, n)
      | "P", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to P()"
      | "ProjP", [S.Integer i; S.Integer j] -> ProjP (i, j)
      | "ProjP", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to ProjP()"
      | "ProjM", [S.Integer i; S.Integer j] -> ProjM (i, j)
      | "ProjM", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to ProjM()"
      | "Sigma", [S.Integer mu; S.Integer nu; S.Integer i; S.Integer j] ->
	 Sigma (mu, nu, i, j)
      | "Sigma", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Sigma()"
      | name, _ ->
	 invalid_arg ("UFOx.Lorentz.of_expr: invalid tensor '" ^ name ^ "'")

    type r = S | V | Sp | CSp | Ghost

    let rep_trivial = function
      | S | Ghost -> true
      | V | Sp | CSp-> false

    let rep_to_string = function
      | S -> "0"
      | V -> "1"
      | Sp -> "1/2"
      | CSp-> "1/2bar"
      | Ghost -> "Ghost"

    let rep_of_int = function
      | -1 -> Ghost
      | 1 -> S
      | 2 -> Sp
      | 3 -> V
      | _ -> invalid_arg "UFOx.Lorentz: impossible representation!"
	 
    let rep_conjugate = function
      | S -> S
      | V -> V
      | Sp -> CSp (* ??? *)
      | CSp -> Sp (* ??? *)
      | Ghost -> Ghost

    let classify_indices1 = function
      | C (i, j) -> [(i, CSp); (j, Sp)] (* ??? *)
      | Gamma5 (i, j) | Identity (i, j)
      | ProjP (i, j) | ProjM (i, j) -> [(i, CSp); (j, Sp)]
      | Epsilon (mu, nu, ka, la) -> [(mu, V); (nu, V); (ka, V); (la, V)]
      | Gamma (mu, i, j) -> [(mu, V); (i, CSp); (j, Sp)]
      | Metric (mu, nu) -> [(mu, V); (nu, V)]
      | P (mu, n) ->  [(mu, V)]
      | Sigma (mu, nu, i, j) -> [(mu, V); (nu, V); (i, CSp); (j, Sp)]

    let classify_indices tensors =
      List.sort compare
	(List.fold_right
	   (fun v acc -> classify_indices1 v @ acc)
	   tensors [])

    type r_omega = Coupling.lorentz
    let omega = function
      | S -> Coupling.Scalar
      | V -> Coupling.Vector
      | Sp -> Coupling.Spinor
      | CSp-> Coupling.ConjSpinor
      | Ghost -> Coupling.Scalar

  end
    
module Lorentz = Tensor(Lorentz_Atom')

module type Color_Atom =
  sig
    type t = private
      | Identity of int * int
      | T of int * int * int
      | F of int * int * int
      | D of int * int * int
      | Epsilon of int * int * int
      | EpsilonBar of int * int * int
      | T6 of int * int * int
      | K6 of int * int * int
      | K6Bar of int * int * int
  end

module Color_Atom =
  struct
    type t =
      | Identity of int * int
      | T of int * int * int
      | F of int * int * int
      | D of int * int * int
      | Epsilon of int * int * int
      | EpsilonBar of int * int * int
      | T6 of int * int * int
      | K6 of int * int * int
      | K6Bar of int * int * int
  end

module Color_Atom' : Atom
  with type t = Color_Atom.t and type r_omega = Color.t =
  struct

    type t = Color_Atom.t

    module S = UFOx_syntax

    open Color_Atom

    let of_expr name args =
      match name, args with
      | "Identity", [S.Integer i; S.Integer j] -> Identity (i, j)
      | "Identity", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to Identity()"
      | "T", [S.Integer a; S.Integer i; S.Integer j] -> T (a, i, j)
      | "T", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to T()"
      | "f", [S.Integer a; S.Integer b; S.Integer c] -> F (a, b, c)
      | "f", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to f()"
      | "d", [S.Integer a; S.Integer b; S.Integer c] -> D (a, b, c)
      | "d", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to d()"
      | "Epsilon", [S.Integer i; S.Integer j; S.Integer k] ->
	 Epsilon (i, j, k)
      | "Epsilon", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to Epsilon()"
      | "EpsilonBar", [S.Integer i; S.Integer j; S.Integer k] ->
	 EpsilonBar (i, j, k)
      | "EpsilonBar", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to EpsilonBar()"
      | "T6", [S.Integer a; S.Integer i'; S.Integer j'] -> T6 (a, i', j')
      | "T6", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to T6()"
      | "K6", [S.Integer i'; S.Integer j; S.Integer k] -> K6 (i', j, k)
      | "K6", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to K6()"
      | "K6Bar", [S.Integer i'; S.Integer j; S.Integer k] -> K6Bar (i', j, k)
      | "K6Bar", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to K6Bar()"
      | name, _ ->
	 invalid_arg ("UFOx.Color.of_expr: invalid tensor '" ^ name ^ "'")
	
    let to_string = function
      | Identity (i, j) -> Printf.sprintf "Identity(%d,%d)" i j
      | T (a, i, j) -> Printf.sprintf "T(%d,%d,%d)" a i j
      | F (a, b, c) -> Printf.sprintf "f(%d,%d,%d)" a b c
      | D (a, b, c) -> Printf.sprintf "d(%d,%d,%d)" a b c
      | Epsilon (i, j, k) -> Printf.sprintf "Epsilon(%d,%d,%d)" i j k
      | EpsilonBar (i, j, k) -> Printf.sprintf "EpsilonBar(%d,%d,%d)" i j k
      | T6 (a, i', j') -> Printf.sprintf "T6(%d,%d,%d)" a i' j'
      | K6 (i', j, k) -> Printf.sprintf "K6(%d,%d,%d)" i' j k
      | K6Bar (i', j, k) -> Printf.sprintf "K6Bar(%d,%d,%d)" i' j k

    type r = S | Sbar | F | C | A

    let rep_trivial = function
      | S | Sbar -> true
      | F | C | A-> false

    let rep_to_string = function
      | S -> "1"
      | Sbar -> "1bar"
      | F -> "3"
      | C -> "3bar"
      | A-> "8"

    let rep_of_int = function
      | 1 -> S
      | -1 -> Sbar (* UFO appears to use this for colorless antiparticles!. *)
      | 3 -> F
      | -3 -> C
      | 8 -> A
      | 6 | -6 -> failwith "UFOx.Color: sextets not supported yet!"
      | _ -> invalid_arg "UFOx.Color: impossible representation!"
	 
    let rep_conjugate = function
      | Sbar -> S
      | S -> Sbar
      | C -> F
      | F -> C
      | A -> A

    let classify_indices1 = function
      | Identity (i, j) -> [(i, C); (j, F)]
      | T (a, i, j) -> [(i, F); (j, C); (a, A)]
      | Color_Atom.F (a, b, c) | D (a, b, c) -> [(a, A); (b, A); (c, A)] 
      | Epsilon (i, j, k) -> [(i, F); (j, F); (k, F)]
      | EpsilonBar (i, j, k) -> [(i, C); (j, C); (k, C)]
      | T6 (a, i', j') ->
	 failwith "UFOx.Color: sextets not supported yet!"
      | K6 (i', j, k) ->
	 failwith "UFOx.Color: sextets not supported yet!"
      | K6Bar (i', j, k) ->
	 failwith "UFOx.Color: sextets not supported yet!"

    let classify_indices tensors =
      List.sort compare
	(List.fold_right
	   (fun v acc -> classify_indices1 v @ acc)
	   tensors [])

    type r_omega = Color.t

    let omega = function
      | S | Sbar -> Color.Singlet
      | F -> Color.SUN (3)
      | C -> Color.SUN (-3)
      | A-> Color.AdjSUN (3)
    
  end

module Color = Tensor(Color_Atom')

module Value =
  struct

    module S = UFOx_syntax

    type builtin =
      | Sqrt
      | Cos
      | Sin
      | Conj

    let builtin_to_string = function
      | Sqrt -> "sqrt"
      | Cos -> "cos"
      | Sin -> "sin"
      | Conj -> "conj"

    let builtin_of_string = function
      | "cmath.sqrt" -> Sqrt
      | "cmath.cos" -> Cos
      | "cmath.sin" -> Sin
      | "complexconjugate" -> Conj
      | name -> failwith ("UFOx.Value: unsupported function: " ^ name)

    type t =
      | Integer of int
      | Rational of Q.t
      | Real of float
      | Complex of float * float
      | Variable of string
      | Sum of t list
      | Difference of t * t
      | Product of t list
      | Quotient of t * t
      | Power of t * t
      | Application of builtin * t list

    let rec to_string = function
      | Integer i ->
	 string_of_int i
      | Rational q ->
	 Q.to_string q
      | Real x ->
	 string_of_float x
      | Complex (r, i) ->
	 Printf.sprintf "%f+I*%f" r i
      | Variable s -> s
      | Sum es ->
	 "(" ^ String.concat "+" (List.map to_string es) ^ ")"
      | Difference (e1, e2) ->
	 "(" ^ to_string e1 ^ "-(" ^ to_string e2 ^ "))"
      | Product es ->
	 String.concat "*" (List.map to_string es)
      | Quotient (e1, e2) ->
	 to_string e1 ^ "/(" ^ to_string e2 ^ ")"
      | Power (e1, e2) ->
	 "(" ^ to_string e1 ^ ")^(" ^ to_string e2 ^ ")"
      | Application (f, es) ->
	 builtin_to_string f ^
	   "(" ^ String.concat "," (List.map to_string es) ^ ")"
	 
    let rec to_coupling atom = function
      | Integer i -> Coupling.Const i
      | Rational q ->
         let n, d = Q.to_ratio q in
         Coupling.Quot (Coupling.Const n, Coupling.Const d)
      | Real x -> Coupling.Atom (atom (string_of_float x))
      | Product es -> Coupling.Prod (List.map (to_coupling atom) es)
      | Variable s -> Coupling.Atom (atom s)
      | Complex (r, i) ->
         Coupling.Sum [Coupling.Atom (atom (string_of_float r));
                       Coupling.Prod [Coupling.I;
                                      Coupling.Atom (atom (string_of_float i))]]
      | Sum es -> Coupling.Sum (List.map (to_coupling atom) es)
      | Difference (e1, e2) ->
         Coupling.Diff (to_coupling atom e1, to_coupling atom e2)
      | Quotient (e1, e2) ->
         Coupling.Quot (to_coupling atom e1, to_coupling atom e2)
      | Power (e1, Integer e2) ->
         Coupling.Pow (to_coupling atom e1, e2)
      | Application (Sin, [e]) -> Coupling.Sin (to_coupling atom e)
      | Application (Cos, [e]) -> Coupling.Cos (to_coupling atom e)
      | Application (Sqrt, [e]) -> Coupling.Sqrt (to_coupling atom e)
      | Application (Conj, [e]) -> Coupling.Conj (to_coupling atom e)
      | Power (e1, _) ->
         invalid_arg "UFOx.Value.to_coupling: non-integer power"
      | Application (_, _) ->
         failwith "UFOx.Value.to_coupling: more than one argument list"

    let compress terms = terms

    let rec of_expr e =
      compress (of_expr' e)

    and of_expr' = function
      | S.Integer i -> Integer i
      | S.Float x -> Real x
      | S.Variable "cmath.pi" -> Variable "pi"
      | S.Variable name -> Variable name
      | S.Sum (e1, e2) ->
	 begin match of_expr e1, of_expr e2 with
	 | Sum e1, Sum e2 -> Sum (e1 @ e2)
	 | e1, Sum e2 -> Sum (e1 :: e2)
	 | Sum e1, e2 -> Sum (e2 :: e1)
	 | e1, e2 -> Sum [e1; e2]
	 end
      | S.Difference (e1, e2) ->
	 Difference (of_expr e1, of_expr e2)
      | S.Product (e1, e2) ->
	 begin match of_expr e1, of_expr e2 with
	 | Product e1, Product e2 -> Product (e1 @ e2)
	 | e1, Product e2 -> Product (e1 :: e2)
	 | Product e1, e2 -> Product (e2 :: e1)
	 | e1, e2 -> Product [e1; e2]
	 end
      | S.Quotient (e1, e2) ->
	 Quotient (of_expr e1, of_expr e2)
      | S.Power (e, p) ->
	 Power (of_expr e, of_expr p)
      | S.Application ("complex", [r; i]) ->
	 begin match of_expr r, of_expr i with
	 | Real r, Real i -> Complex (r, i)
	 | Integer r, Real i -> Complex (float_of_int r, i)
	 | Real r, Integer i -> Complex (r, float_of_int i)
	 | Integer r, Integer i -> Complex (float_of_int r, float_of_int i)
	 | _ -> invalid_arg "UFOx.Value: complex expects two numeric arguments"
	 end
      | S.Application ("complex", _) ->
	 invalid_arg "UFOx.Value: complex expects two arguments"
      | S.Application ("complexconjugate", [e]) ->
	 Application (Conj, [of_expr e])
      | S.Application ("complexconjugate", _) ->
	 invalid_arg "UFOx.Value: complexconjugate expects single argument"
      | S.Application ("cmath.sqrt", [e]) ->
	 Application (Sqrt, [of_expr e])
      | S.Application ("cmath.sqrt", _) ->
	 invalid_arg "UFOx.Value: sqrt expects single argument"
      | S.Application (name, args) ->
	 Application (builtin_of_string name, List.map of_expr args)

  end

module type Test =
  sig
    val example : unit -> unit
    val suite : OUnit.test
  end

