(* vertex.ml --

   Copyright (C) 1999-2026 by

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

let _error_in_file name start_pos end_pos =
  Printf.sprintf
    "%s:%d.%d-%d.%d"
    name
    start_pos.Lexing.pos_lnum
    (start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol)
    end_pos.Lexing.pos_lnum
    (end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol)

module SMap = Map.Make(String)

module Expr =
  struct

    type t = UFOx_syntax.expr

    let of_string text =
      try
	UFOx_parser.input
	  UFOx_lexer.token
	  (UFOx_lexer.init_position "" (Lexing.from_string text))
      with
      | UFO_tools.Lexical_Error (msg, start_pos, end_pos) ->
	 invalid_arg (Printf.sprintf "lexical error (%s) at: `%s'"
			msg  (error_in_string text start_pos end_pos))
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

    open UFOx_syntax

    let rec map f = function
      | Integer _ | Float _ | Quoted _ | Young_Tableau _ as e -> e
      | Variable s as e ->
         begin match f s with
         | Some value -> value
         | None -> e
         end
      | Sum (e1, e2) -> Sum (map f e1, map f e2)
      | Difference (e1, e2) -> Difference (map f e1, map f e2)
      | Product (e1, e2) -> Product (map f e1, map f e2)
      | Quotient (e1, e2) -> Quotient (map f e1, map f e2)
      | Power (e1, e2) -> Power (map f e1, map f e2)
      | Application (s, el) -> Application (s, List.map (map f) el)

    let substitute name value expr =
      map (fun s -> if s = name then Some value else None) expr

    let rename1 name_map name =
      try Some (Variable (SMap.find name name_map)) with Not_found -> None

    let rename alist_names value =
      let name_map =
        List.fold_left
          (fun acc (name, name') -> SMap.add name name' acc)
          SMap.empty alist_names in
      map (rename1 name_map) value

    let _map_name1 f name =
      Some (Variable (f name))

    let map_names f value =
      map (fun name -> Some (Variable (f name))) value

    let half name =
      Quotient (Variable name, Integer 2)

    let variables = UFOx_syntax.variables
    let functions = UFOx_syntax.functions

  end

(* It might seem to be a hack to base the decision of whether a
   sign or parentheses are required on the textual representation
   of a term.  However we control the textual representation, it's
   efficient and we can avoid duplicating quite a bit of code
   testing for terms that might produce minus signs. *)

let starts_with_a_sign s =
  String.length s > 0 && let c = s.[0] in c = '-' || c = '+'

let starts_with_a_plus s =
  String.length s > 0 && s.[0] = '+'

let starts_with_a_minus s =
  String.length s > 0 && s.[0] = '-'

let prepend_binary_plus s =
  if starts_with_a_sign s then
    s
  else
    "+" ^ s

(* The safe version that might produce terms like $-(-a)$. *)

let prepend_binary_minus s =
  if starts_with_a_sign s then
    "-(" ^ s ^ ")"
  else
    "-" ^ s

(* The version that produces fewer parentheses, but must
   assume that a leading minus sign always applies to the
   \emph{whole} term! *)

let _prepend_binary_minus s =
  if starts_with_a_plus s then
    "-" ^ String.sub s 1 (String.length s - 1)
  else if starts_with_a_minus s then
    "+" ^ String.sub s 1 (String.length s - 1)
  else
    "-" ^ s


module Value =
  struct

    module S = UFOx_syntax
    module Q = Algebra.Q

    type builtin =
      | Sqrt
      | Exp | Log | Log10
      | Sin | Asin
      | Cos | Acos
      | Tan | Atan
      | Sinh | Asinh
      | Cosh | Acosh
      | Tanh | Atanh
      | Sec | Asec
      | Csc | Acsc
      | Conj | Abs

    let builtin_to_string = function
      | Sqrt -> "sqrt"
      | Exp -> "exp"
      | Log -> "log"
      | Log10 -> "log10"
      | Sin -> "sin"
      | Cos -> "cos"
      | Tan -> "tan"
      | Asin -> "asin"
      | Acos -> "acos"
      | Atan -> "atan"
      | Sinh -> "sinh"
      | Cosh -> "cosh"
      | Tanh -> "tanh"
      | Asinh -> "asinh"
      | Acosh -> "acosh"
      | Atanh -> "atanh"
      | Sec -> "sec"
      | Csc -> "csc"
      | Asec -> "asec"
      | Acsc -> "acsc"
      | Conj -> "conjg"
      | Abs -> "abs"

    let builtin_of_string = function
      | "cmath.sqrt" -> Sqrt
      | "cmath.exp" -> Exp
      | "cmath.log" -> Log
      | "cmath.log10" -> Log10
      | "cmath.sin" -> Sin
      | "cmath.cos" -> Cos
      | "cmath.tan" -> Tan
      | "cmath.asin" -> Asin
      | "cmath.acos" -> Acos
      | "cmath.atan" -> Atan
      | "cmath.sinh" -> Sinh
      | "cmath.cosh" -> Cosh
      | "cmath.tanh" -> Tanh
      | "cmath.asinh" -> Asinh
      | "cmath.acosh" -> Acosh
      | "cmath.atanh" -> Atanh
      | "sec" -> Sec
      | "csc" -> Csc
      | "asec" -> Asec
      | "acsc" -> Acsc
      | "complexconjugate" -> Conj
      | "abs" -> Abs
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

    (* At first sight, unparsing appears to be simpler than parsing.
       Nevertheless, it can become tricky and error prone if one wants
       to produce readable output that is not cluttered by too many
       parentheses. *)

    let _signed_string_of_float x =
      (if x < 0.0 then "-" else "+") ^ string_of_float (abs_float x)

    (* Collect the numerical factors in a [Product] in order to
       reduce the number of parentheses required.
       \begin{dubious}
         We could include [Rational], but is it worth it?
       \end{dubious} *)

    let collect_factors elist =
      let rec collect_factors' factor elist_rev elist =
        match factor, elist with
        | (Integer 1| Real 1.), [] -> List.rev elist_rev
        | _, [] -> factor :: List.rev elist_rev
        | Integer i1, Integer i2 :: elist' ->
           collect_factors' (Integer (i1 * i2)) elist_rev elist'
        | Integer i, Real x :: elist' | Real x, Integer i :: elist' ->
           collect_factors' (Real (float i *. x)) elist_rev elist'
        | Real x1, Real x2 :: elist' ->
           collect_factors' (Real (x1 *. x2)) elist_rev elist'
        | _, e :: elist' -> collect_factors' factor (e :: elist_rev) elist' in
      collect_factors' (Integer 1) [] elist

    let rec to_string = function
      | Integer i -> string_of_int i
      | Rational q -> Q.to_string q
      | Real x -> string_of_float x
      | Complex (0.0, 1.0) -> "I"
      | Complex (0.0, i) -> group_product (Product [Real i; Complex (0.0, 1.0)])
      | Complex (r, 0.0) -> to_string (Real r)
      | Complex (r, i) -> group_sum (Sum [Real r; Product [Real i; Complex (0.0, 1.0)]])
      | Variable s -> s
      | Sum [] -> "0"
      | Sum [e] -> to_string e
      | Sum (e::es) -> to_string e ^ String.concat "" (List.map with_binary_plus es)
      | Difference (e1, e2) -> to_string e1 ^ prepend_binary_minus (group_sum e2)
      | Product [] -> "1"
      | Product es ->
         begin match collect_factors es with
         | (Integer (-1) | Real (-1.)) :: es -> "-" ^ to_string (Product es)
         | es -> String.concat "*" (List.map group_sum es)
         end
      | Quotient (e1, e2) -> group_numerator e1 ^ "/" ^ group_denominator e2
      | Power ((Power (_, _) as e1, (Power (_, _) as e2))) ->
         "(" ^ group_product e1 ^ ")^(" ^ to_string e2 ^ ")"
      | Power ((Power (_, _) as e1, e2)) ->
         "(" ^ group_product e1 ^ ")^" ^ to_string e2
      | Power (e1, (Power (_, _) as e2)) ->
         group_product e1 ^ "^(" ^ to_string e2 ^ ")"
      | Power ((Integer i as e), Integer p) ->
         if p < 0 then
           group_product (Real (float_of_int i)) ^ "^(" ^ string_of_int p ^ ")"
         else if p = 0 then
           "1"
         else if p <= 4 then
           group_product e ^ "^" ^ string_of_int p
         else
           group_product (Real (float_of_int i)) ^ "^" ^ string_of_int p
      | Power (e1, e2) -> group_product e1 ^ "^" ^ group_product e2
      | Application (f, [Integer i]) -> to_string (Application (f, [Real (float i)]))
      | Application (f, es) ->
	 builtin_to_string f ^ "(" ^ String.concat "," (List.map to_string es) ^ ")"

    (* Expressions that appear as arguments of [Power]s must be
       enclosed in parentheses, unless they are singletons.  In
       a denominator, we don't have to put function applications
       in parentheses.
       \begin{dubious}
         Check this with \texttt{Whizard}'s parser, since this is the
         main (only?) consumer of our output.
       \end{dubious} *)

    and group_product = function
      | Application (_, _) as e -> "(" ^ to_string e ^ ")"
      | e -> group_denominator e

    (* In numerators, we must be careful not to leave an unprotected minus sign,
       since they can appear inside products. *)

    and group_numerator = function
      | Product (_ :: _ as es) ->
         begin match collect_factors es with
         | (Integer (-1) | Real (-1.)) :: es -> "(-" ^ to_string (Product es) ^ ")"
         | es -> String.concat "*" (List.map group_sum es)
         end
      | e -> group_denominator e

    and group_denominator = function
      | Sum [e] | Product [e] -> group_product e
      | Sum ( _ :: _) | Difference (_, _)
      | Product ( _ :: _) | Quotient (_, _) as e -> "(" ^ to_string e ^ ")"
      | e -> to_string e

    (* [Sum]s that appear in [Product]s must be
       enclosed in parentheses, unless they are singletons. *)

    and group_sum = function
      | Sum [e] | Product [e] -> group_sum e
      | Sum ( _ :: _) | Difference (_, _) as e -> "(" ^ to_string e ^ ")"
      | e -> to_string e

    (* Add a ['+'] at the front of a term iff if has no sign. *)

    and with_binary_plus e =
      prepend_binary_plus (to_string e)

    let rec to_coupling atom = function
      | Integer i -> Coupling.Integer i
      | Rational q ->
         let n, d = Q.to_ratio q in
         Coupling.Quot (Coupling.Integer n, Coupling.Integer d)
      | Real x -> Coupling.Float x
      | Product es -> Coupling.Prod (List.map (to_coupling atom) es)
      | Variable s -> Coupling.Atom (atom s)
      | Complex (r, 0.0) -> Coupling.Float r
      | Complex (0.0,  1.0) -> Coupling.I
      | Complex (0.0, -1.0) -> Coupling.Prod [Coupling.I; Coupling.Integer (-1)]
      | Complex (0.0, i) -> Coupling.Prod [Coupling.I; Coupling.Float i]
      | Complex (r, 1.0) ->
         Coupling.Sum [Coupling.Float r; Coupling.I]
      | Complex (r, -1.0) ->
         Coupling.Diff (Coupling.Float r, Coupling.I)
      | Complex (r, i) ->
         Coupling.Sum [Coupling.Float r;
                       Coupling.Prod [Coupling.I; Coupling.Float i]]
      | Sum es -> Coupling.Sum (List.map (to_coupling atom) es)
      | Difference (e1, e2) ->
         Coupling.Diff (to_coupling atom e1, to_coupling atom e2)
      | Quotient (e1, e2) ->
         Coupling.Quot (to_coupling atom e1, to_coupling atom e2)
      | Power (e1, Integer e2) ->
         Coupling.Pow (to_coupling atom e1, e2)
      | Power (e1, e2) ->
         Coupling.PowX (to_coupling atom e1, to_coupling atom e2)
      | Application (f, [e]) -> apply1 (to_coupling atom e) f
      | Application (f, []) ->
         failwith
           ("UFOx.Value.to_coupling:  " ^ builtin_to_string f ^
              ": empty argument list")
      | Application (f, _::_::_) ->
         failwith
           ("UFOx.Value.to_coupling: " ^ builtin_to_string f ^
              ": more than one argument in list")

    and apply1 e = function
      | Sqrt -> Coupling.Sqrt e
      | Exp -> Coupling.Exp e
      | Log -> Coupling.Log e
      | Log10 -> Coupling.Log10 e
      | Sin -> Coupling.Sin e
      | Cos -> Coupling.Cos e
      | Tan -> Coupling.Tan e
      | Asin -> Coupling.Asin e
      | Acos -> Coupling.Acos e
      | Atan -> Coupling.Atan e
      | Sinh -> Coupling.Sinh e
      | Cosh -> Coupling.Cosh e
      | Tanh -> Coupling.Tanh e
      | Sec -> Coupling.Quot (Coupling.Integer 1, Coupling.Cos e)
      | Csc -> Coupling.Quot (Coupling.Integer 1, Coupling.Sin e)
      | Asec -> Coupling.Acos (Coupling.Quot (Coupling.Integer 1, e))
      | Acsc -> Coupling.Asin (Coupling.Quot (Coupling.Integer 1, e))
      | Conj -> Coupling.Conj e
      | Abs -> Coupling.Abs e
      | (Asinh | Acosh | Atanh as f) ->
         failwith
           ("UFOx.Value.to_coupling: function `"
            ^ builtin_to_string f ^ "' not supported yet!")

    (* \begin{dubious}
         The constant propagation here is incomplete.
         [S.Quotient] and [S.Power] are not yet handled
         and in [S.Sum] and [S.Product] only adjacent constants
         are combined.
       \end{dubious}
       \begin{dubious}
         We could include [Rational], but is it worth it?
       \end{dubious} *)

    let compress terms = terms

    let rec of_expr e =
      compress (of_expr' e)

    and of_expr' = function
      | S.Integer i -> Integer i
      | S.Float x -> Real x
      | S.Variable "cmath.pi" -> Variable "pi"
      | S.Quoted name ->
	 invalid_arg ("UFOx.Value.of_expr: unexpected quoted variable '" ^
			 name ^ "'")
      | S.Young_Tableau y ->
	 invalid_arg ("UFOx.Value.of_expr: unexpected Young tableau '" ^
			Young.tableau_to_string string_of_int y ^ "'")
      | S.Variable name -> Variable name
      | S.Sum (e1, e2) ->
	 begin match of_expr e1, of_expr e2 with
         | Integer i1, Integer i2 -> Integer (i1 + i2)
         | Integer i, Real x | Real x, Integer i -> Real (float_of_int i +. x)
         | Real x1, Real x2 -> Real (x1 +. x2)
	 | (Integer 0 | Real 0.), e -> e
	 | e, (Integer 0 | Real 0.) -> e
	 | Sum e1, Sum e2 -> Sum (e1 @ e2)
	 | e1, Sum e2 -> Sum (e1 :: e2)
	 | Sum e1, e2 -> Sum (e1 @ [e2])
	 | e1, e2 -> Sum [e1; e2]
	 end
      | S.Difference (e1, e2) ->
	 begin match of_expr e1, of_expr e2 with
         | Integer i1, Integer i2 -> Integer (i1 - i2)
         | Integer i, Real x -> Real (float_of_int i -. x)
         | Real x, Integer i -> Real (x -. float_of_int i)
         | Real x1, Real x2 -> Real (x1 -. x2)
	 | e1, (Integer 0 | Real 0.) -> e1
	 | e1, e2 -> Difference (e1, e2)
         end
      | S.Product (e1, e2) ->
	 begin match of_expr e1, of_expr e2 with
         | Integer i1, Integer i2 -> Integer (i1 * i2)
         | Integer i, Real x | Real x, Integer i -> Real (float_of_int i *. x)
         | Real x1, Real x2 -> Real (x1 *. x2)
         | (Integer 0 | Real 0.), _ -> Integer 0
         | _, (Integer 0 | Real 0.) -> Integer 0
         | (Integer 1 | Real 1.), e -> e
         | e, (Integer 1 | Real 1.) -> e
	 | Product e1, Product e2 -> Product (e1 @ e2)
	 | e1, Product e2 -> Product (e1 :: e2)
	 | Product e1, e2 -> Product (e1 @ [e2])
	 | e1, e2 -> Product [e1; e2]
	 end
      | S.Quotient (e1, e2) ->
         begin match of_expr e1, of_expr e2 with
         | _, (Integer 0 | Real 0.) ->
            invalid_arg "UFOx.Value: divide by 0"
         | e1, (Integer 1 | Real 1.) -> e1
         | Integer i1, Integer i2 -> Rational (Q.make i1 i2)
         | Real x, Integer i -> Real (x /. float i)
         | Integer i, Real x -> Real (float i /. x)
         | Real x1, Real x2 -> Real (x1 /. x2)
         | e1, e2 -> Quotient (e1, e2)
         end
      | S.Power (e, p) ->
         begin match of_expr e, of_expr p with
         | (Integer 0 | Real 0.), (Integer 0 | Real 0.) ->
            invalid_arg "UFOx.Value: 0^0"
         | _, (Integer 0 | Real 0.) -> Integer 1
         | e, (Integer 1 | Real 1.) -> e
	 | Integer e, Integer p ->
            if p < 0 then
              Power (Real (float_of_int e), Integer p)
            else if p = 0 then
              Integer 1
            else if p <= 4 then
              Power (Integer e, Integer p)
            else
              Power (Real (float_of_int e), Integer p)
	 | e, p -> Power (e, p)
         end
      | S.Application ("complex", [r; i]) ->
	 begin match of_expr r, of_expr i with
	 | r, (Integer 0 | Real 0.0) -> r
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

let positive integers =
  List.filter (fun (i, _) -> i > 0) integers

let not_positive integers =
  List.filter (fun (i, _) -> i <= 0) integers

module type Index =
  sig

    type t = int

    val position : t -> int
    val factor : t -> int
    val unpack : t -> int * int
    val pack : int -> int -> t
    val map_position : (int -> int) -> t -> t
    val to_string : t -> string
    val list_to_string : t list -> string

    val free : (t * 'r) list -> (t * 'r) list
    val summation : (t * 'r) list -> (t * 'r) list
    val classes_to_string : ('r -> string) -> (t * 'r) list -> string

    val fresh_summation : unit -> t
    val named_summation : string -> unit -> t

  end

module Index : Index =
  struct

    type t = int

    let free i = positive i
    let summation i = not_positive i

    let position i =
      if i > 0 then
        i mod 1000
      else
        i

    let factor i =
      if i > 0 then
        i / 1000
      else
        invalid_arg "UFOx.Index.factor: argument not positive"

    let unpack i =
      if i > 0 then
        (position i, factor i)
      else
        (i, 0)

    let pack i j =
      if j > 0 then
        if i > 0 then
          1000 * j + i
        else
          invalid_arg "UFOx.Index.pack: position not positive"
      else if j = 0 then
        i
      else
        invalid_arg "UFOx.Index.pack: factor negative"

    let map_position f i =
      let pos, fac = unpack i in
      pack (f pos) fac

    let to_string i =
      let pos, fac = unpack i in
      if fac = 0 then
        Printf.sprintf "%d" pos
      else
        Printf.sprintf "%d.%d" pos fac

    let _to_string = string_of_int

    let list_to_string is =
      "[" ^ String.concat ", " (List.map to_string is) ^ "]"
	
    let classes_to_string rep_to_string index_classes =
      let reps =
	ThoList.uniq (List.sort compare (List.map snd index_classes)) in
      "[" ^
	String.concat ", "
	(List.map
	   (fun r ->
	     (rep_to_string r) ^ "=" ^
	       (list_to_string
		  (List.map
		     fst
		     (List.filter (fun (_, r') -> r = r') index_classes))))
	   reps) ^ "]"

    type factory =
      { mutable named : int SMap.t;
        mutable used : Sets.Int.t }

    let factory =
      { named = SMap.empty;
        used = Sets.Int.empty }

    let first_anonymous = -1001

    let fresh_summation () =
      let next_anonymous =
        try
          pred (Sets.Int.min_elt factory.used)
        with
        | Not_found -> first_anonymous in
      factory.used <- Sets.Int.add next_anonymous factory.used;
      next_anonymous

    let named_summation name () =
      try
        SMap.find name factory.named
      with
      | Not_found ->
         begin
           let next_named = fresh_summation () in
           factory.named <- SMap.add name next_named factory.named;
           next_named
         end

  end

module type Atom =
  sig
    type t
    val map_indices : (int -> int) -> t -> t
    val rename_indices : (int -> int) -> t -> t
    val contract_pair : t -> t -> t option
    val variable : t -> string option
    val scalar : t -> bool
    val is_unit : t -> bool
    val invertible : t -> bool
    val invert : t -> t
    val of_expr : string -> UFOx_syntax.expr list -> t list
    val to_string : t -> string
    type r
    val classify_indices : t list -> (Index.t * r) list
    val disambiguate_indices : t list -> t list
    val rep_to_string : r -> string
    val rep_to_string_whizard : r -> string
    val rep_of_int : bool -> int -> r
    val rep_of_int_or_young_tableau : bool -> int option -> int Young.tableau option -> r
    val rep_conjugate : r -> r
    val rep_trivial : r -> bool
    type r_omega
    val omega : r -> r_omega
  end

module type Tensor =
  sig
    type atom
    type 'a linear = ('a list * Algebra.QC.t) list
    type t =
      | Linear of atom linear
      | Ratios of (atom linear * atom linear) list
    val map_atoms : (atom -> atom) -> t -> t
    val map_indices : (int -> int) -> t -> t
    val rename_indices : (int -> int) -> t -> t
    val map_coeff : (Algebra.QC.t -> Algebra.QC.t) -> t -> t
    val contract_pairs : t -> t
    val variables : t -> string list
    val of_expr : UFOx_syntax.expr -> t
    val of_string : string -> t
    val of_strings : string list -> t
    val to_string : t -> string
    type r
    val classify_indices : t -> (Index.t * r) list
    val rep_to_string : r -> string
    val rep_to_string_whizard : r -> string
    val rep_of_int : bool -> int -> r
    val rep_of_int_or_young_tableau : bool -> int option -> int Young.tableau option -> r
    val rep_conjugate : r -> r
    val rep_trivial : r -> bool
    type r_omega
    val omega : r -> r_omega
  end

module Tensor (A : Atom) : Tensor
  with type atom = A.t and type r = A.r and type r_omega = A.r_omega =
  struct

    module S = UFOx_syntax
    (* TODO: we have to switch to [Algebra.QC] to support complex
       coefficients, as used in custom propagators. *)
    module Q = Algebra.Q
    module QC = Algebra.QC

    type atom = A.t
    type 'a linear = ('a list * Algebra.QC.t) list
    type t =
      | Linear of atom linear
      | Ratios of (atom linear * atom linear) list

    let term_to_string (tensors, c) =
      if QC.is_null c then
	""
      else
	match tensors with
	| [] -> QC.to_string c
	| tensors ->
	   String.concat
             "*" ((if QC.is_unit c then [] else [QC.to_string c]) @
		    List.map A.to_string tensors)

    let linear_to_string = function
      | [] -> ""
      | term :: terms ->
         term_to_string term ^
           String.concat "" (List.map (fun t -> prepend_binary_plus (term_to_string t)) terms)

    let to_string = function
      | Linear terms -> linear_to_string terms
      | Ratios ratios ->
         String.concat
           " + "
           (List.map
              (fun (n, d) ->
                Printf.sprintf "(%s)/(%s)"
                  (linear_to_string n) (linear_to_string d)) ratios)

    let variables_of_atoms atoms =
      List.fold_left
        (fun acc a ->
          match A.variable a with
          | None -> acc
          | Some name -> Sets.String.add name acc)
        Sets.String.empty atoms

    let variables_of_linear linear =
      List.fold_left
        (fun acc (atoms, _) -> Sets.String.union (variables_of_atoms atoms) acc)
        Sets.String.empty linear

    let variables_set = function
      | Linear linear -> variables_of_linear linear
      | Ratios ratios ->
         List.fold_left
           (fun acc (numerator, denominator) ->
             Sets.String.union
               (variables_of_linear numerator)
               (Sets.String.union (variables_of_linear denominator) acc))
           Sets.String.empty ratios

    let variables t =
      Sets.String.elements (variables_set t)

    let map_ratios f = function
      | Linear n -> Linear (f n)
      | Ratios ratios -> Ratios (List.map (fun (n, d) -> (f n, f d)) ratios)

    let map_summands f t =
      map_ratios (List.map f) t

    let map_numerators f = function
      | Linear n -> Linear (List.map f n)
      | Ratios ratios ->
         Ratios (List.map (fun (n, d) -> (List.map f n, d)) ratios)

    let map_atoms f t =
      map_summands (fun (atoms, q) -> (List.map f atoms, q)) t

    let map_indices f t =
      map_atoms (A.map_indices f) t

    let rename_indices f t =
      map_atoms (A.rename_indices f) t

    let map_coeff f t =
      map_numerators (fun (atoms, q) -> (atoms, f q)) t

    type result =
      | Matched of atom list
      | Unmatched of atom list

    (* [contract_pair a rev_prefix suffix] returns
       [Unmatched (a :: List.rev_append rev_prefix suffix] if
       there is no match (as defined by [A.contract_pair]) and
       [Matched] with the reduced list otherwise. *)
    let rec contract_pair a rev_prefix = function
      | [] -> Unmatched (a :: List.rev rev_prefix)
      | a' :: suffix ->
         begin match A.contract_pair a a' with
         | None -> contract_pair a (a' :: rev_prefix) suffix
         | Some a'' ->
            if A.is_unit a'' then
              Matched (List.rev_append rev_prefix suffix)
            else
              Matched (List.rev_append rev_prefix (a'' :: suffix))
         end

    (* Use [contract_pair] to find all pairs that match according
       to [A.contract_pair]. *)
    let rec contract_pairs1 = function
      | ([] | [_] as t) -> t
      | a :: t ->
         begin match contract_pair a [] t with
         | Unmatched ([]) -> []
         | Unmatched (a' :: t') -> a' :: contract_pairs1 t'
         | Matched t' -> contract_pairs1 t'
         end

    let contract_pairs t =
      map_summands (fun (t', c) -> (contract_pairs1 t', c)) t

    let add t1 t2 =
      match t1, t2 with
      | Linear l1, Linear l2 -> Linear (l1 @ l2)
      | Ratios r, Linear l | Linear l, Ratios r ->
         Ratios ((l, [([], QC.unit)]) :: r)
      | Ratios r1, Ratios r2 -> Ratios (r1 @ r2)

    let multiply1 (t1, c1) (t2, c2) =
      (List.sort compare (t1 @ t2), QC.mul c1 c2)

    let multiply2 t1 t2 =
      Product.list2 multiply1 t1 t2

    let multiply t1 t2 =
      match t1, t2 with
      | Linear l1, Linear l2 -> Linear (multiply2 l1 l2)
      | Ratios r, Linear l | Linear l, Ratios r ->
         Ratios (List.map (fun (n, d) -> (multiply2 l n, d)) r)
      | Ratios r1, Ratios r2 ->
         Ratios (Product.list2
                   (fun (n1, d1) (n2, d2) ->
                     (multiply2 n1 n2, multiply2 d1 d2))
                   r1 r2)

    let rec power n t =
      if n < 0 then
        invalid_arg "UFOx.Tensor.power: n < 0"
      else if n = 0 then
        Linear [([], QC.unit)]
      else if n = 1 then
        t
      else
        multiply t (power (pred n) t)

    let compress ratios =
      map_ratios
        (fun terms ->
          List.map (fun (t, cs) -> (t, QC.sum cs)) (ThoList.factorize terms))
        ratios

    let rec of_expr e =
      contract_pairs (compress (of_expr' e))

    and of_expr' = function
      | S.Integer i -> Linear [([], QC.make (Q.make i 1) Q.null)]
      | S.Float _ -> invalid_arg "UFOx.Tensor.of_expr: unexpected float"
      | S.Quoted name ->
	 invalid_arg ("UFOx.Tensor.of_expr: unexpected quoted variable '" ^
			 name ^ "'")
      | S.Young_Tableau y ->
	 invalid_arg ("UFOx.Tensor.of_expr: unexpected top level Young tableau '" ^
			Young.tableau_to_string string_of_int y ^ "'")
      | S.Variable name ->
         (* There should be a gatekeeper here or in [A.of_expr]: *)
         Linear [(A.of_expr name [], QC.unit)]
      | S.Application ("complex", [re; im]) ->
         begin match of_expr re, of_expr im with
         | Linear [([], re)], Linear [([], im)] ->
            if QC.is_real re && QC.is_real im then
              Linear [([], QC.make (QC.re re) (QC.re im))]
            else
	      invalid_arg ("UFOx.Tensor.of_expr: argument of complex is complex")
         | _ ->
            invalid_arg "UFOx.Tensor.of_expr: unexpected argument of complex"
         end
      | S.Application (name, args) ->
         Linear [(A.of_expr name args, QC.unit)]
      | S.Sum (e1, e2) -> add (of_expr e1) (of_expr e2)
      | S.Difference (e1, e2) ->
	 add (of_expr e1) (of_expr (S.Product (S.Integer (-1), e2)))
      | S.Product (e1, e2) -> multiply (of_expr e1) (of_expr e2)
      | S.Quotient (n, d) ->
	 begin match of_expr n, of_expr d with
	 | _, Linear [] ->
            invalid_arg "UFOx.Tensor.of_expr: zero denominator"
	 | n, Linear [([], q)] -> map_coeff (fun c -> QC.div c q) n
	 | n, Linear ([(invertibles, q)] as d) ->
            if List.for_all A.invertible invertibles then
              let inverses = List.map A.invert invertibles in
              multiply (Linear [(inverses, QC.inv q)]) n
            else
              multiply (Ratios [[([], QC.unit)], d]) n
	 | n, (Linear d as d')->
            if List.for_all (fun (t, _) -> List.for_all A.scalar t) d then
              multiply (Ratios [[([], QC.unit)], d]) n
            else
              invalid_arg ("UFOx.Tensor.of_expr: non scalar denominator: " ^
                             to_string d')
         | _, (Ratios _ as d) ->
            invalid_arg ("UFOx.Tensor.of_expr: illegal denominator: " ^
                           to_string d)
	 end
      | S.Power (e, p) ->
	 begin match of_expr e, of_expr p with
	 | Linear [([], q)], Linear [([], p)] ->
	    if QC.is_real p then
              let re_p = QC.re p in
	      if Q.is_integer re_p then
	        Linear [([], QC.pow q (Q.to_integer re_p))]
	      else
	        invalid_arg "UFOx.Tensor.of_expr: rational power of number"
            else
	      invalid_arg "UFOx.Tensor.of_expr: complex power of number"
	 | Linear [([], _)], _ ->
	    invalid_arg "UFOx.Tensor.of_expr: non-numeric power of number"
	 | t, Linear [([], p)] ->
            if QC.is_integer p then
              power (Q.to_integer (QC.re p)) t
            else
	      invalid_arg "UFOx.Tensor.of_expr: non integer power of tensor"
	 | _ -> invalid_arg "UFOx.Tensor.of_expr: non numeric power of tensor"
	 end

    type r = A.r
    let rep_to_string = A.rep_to_string
    let rep_to_string_whizard = A.rep_to_string_whizard
    let rep_of_int = A.rep_of_int
    let rep_of_int_or_young_tableau = A.rep_of_int_or_young_tableau
    let rep_conjugate = A.rep_conjugate
    let rep_trivial = A.rep_trivial

    let numerators = function
      | Linear tensors -> tensors
      | Ratios ratios -> ThoList.flatmap fst ratios

    let classify_indices' filter tensors =
         ThoList.uniq
	   (List.sort compare
	      (List.map
                 (fun (t, _) -> filter (A.classify_indices t))
                 (numerators tensors)))

    (* NB: the number of summation indices is not guarateed to be
       the same!  Therefore it was foolish to try to check for
       uniqueness \ldots *)
    let classify_indices tensors =
      match classify_indices' Index.free tensors with
      | [] ->
         (* There's always at least an empty list! *)
         failwith "UFOx.Tensor.classify_indices: can't happen!"
      | [f] -> f
      | _ ->
	 invalid_arg "UFOx.Tensor.classify_indices: incompatible free indices!"

    let disambiguate_indices1 (atoms, q) =
      (A.disambiguate_indices atoms, q)

    let disambiguate_indices tensors =
      map_ratios (List.map disambiguate_indices1) tensors

    let check_indices t =
      ignore (classify_indices t)

    let of_expr e =
      let t = disambiguate_indices (of_expr e) in
      check_indices t;
      t

    let of_string s =
      of_expr (Expr.of_string s)

    let of_strings s =
      of_expr (Expr.of_strings s)

    type r_omega = A.r_omega
    let omega = A.omega

  end

module type Lorentz_Atom =
  sig

    type dirac = private
      | C of int * int
      | Gamma of int * int * int
      | Gamma5 of int * int
      | Identity of int * int
      | ProjP of int * int
      | ProjM of int * int
      | Sigma of int * int * int * int

    type vector = (* private *)
      | Epsilon of int * int * int * int
      | Metric of int * int
      | P of int * int

    type scalar = (* private *)
      | Mass of int
      | Width of int
      | P2 of int
      | P12 of int * int
      | Variable of string
      | Coeff of Value.t

    type t = (* private *)
      | Dirac of dirac
      | Vector of vector
      | Scalar of scalar
      | Inverse of scalar

    val map_indices_scalar : (int -> int) -> scalar -> scalar
    val map_indices_vector : (int -> int) -> vector -> vector
    val rename_indices_vector : (int -> int) -> vector -> vector

  end

module Lorentz_Atom =
  struct

    type dirac =
      | C of int * int
      | Gamma of int * int * int
      | Gamma5 of int * int
      | Identity of int * int
      | ProjP of int * int
      | ProjM of int * int
      | Sigma of int * int * int * int

    type vector =
      | Epsilon of int * int * int * int
      | Metric of int * int
      | P of int * int

    type scalar =
      | Mass of int
      | Width of int
      | P2 of int
      | P12 of int * int
      | Variable of string
      | Coeff of Value.t

    type t =
      | Dirac of dirac
      | Vector of vector
      | Scalar of scalar
      | Inverse of scalar

    let map_indices_scalar f = function
      | Mass i -> Mass (f i)
      | Width i -> Width (f i)
      | P2 i -> P2 (f i)
      | P12 (i, j) -> P12 (f i, f j)
      | (Variable _ | Coeff _ as s) -> s

    let map_indices_vector f = function
      | Epsilon (mu, nu, ka, la) -> Epsilon (f mu, f nu, f ka, f la)
      | Metric (mu, nu) -> Metric (f mu, f nu)
      | P (mu, n) -> P (f mu, f n)

    let rename_indices_vector f = function
      | Epsilon (mu, nu, ka, la) -> Epsilon (f mu, f nu, f ka, f la)
      | Metric (mu, nu) -> Metric (f mu, f nu)
      | P (mu, n) -> P (f mu, n)

  end

module Lorentz_Atom' : Atom
  with type t = Lorentz_Atom.t and type r_omega = Coupling.lorentz =
  struct
	
    type t = Lorentz_Atom.t

    open Lorentz_Atom
    
    let map_indices_dirac f = function
      | C (i, j) -> C (f i, f j)
      | Gamma (mu, i, j) -> Gamma (f mu, f i, f j)
      | Gamma5 (i, j) -> Gamma5 (f i, f j)
      | Identity (i, j) -> Identity (f i, f j)
      | ProjP (i, j) -> ProjP (f i, f j)
      | ProjM (i, j) -> ProjM (f i, f j)
      | Sigma (mu, nu, i, j) -> Sigma (f mu, f nu, f i, f j)

    let rename_indices_dirac = map_indices_dirac

    let map_indices_scalar f = function
      | Mass i -> Mass (f i)
      | Width i -> Width (f i)
      | P2 i -> P2 (f i)
      | P12 (i, j) -> P12 (f i, f j)
      | Variable s -> Variable s
      | Coeff c -> Coeff c

    let map_indices f = function
      | Dirac d -> Dirac (map_indices_dirac f d)
      | Vector v -> Vector (map_indices_vector f v)
      | Scalar s -> Scalar (map_indices_scalar f s)
      | Inverse s -> Inverse (map_indices_scalar f s)

    let rename_indices2 fd fv = function
      | Dirac d -> Dirac (rename_indices_dirac fd d)
      | Vector v -> Vector (rename_indices_vector fv v)
      | Scalar s -> Scalar s
      | Inverse s -> Inverse s

    let rename_indices f atom =
      rename_indices2 f f atom

    let contract_pair a1 a2 =
      match a1, a2 with
      | Vector (P (mu1, i1)), Vector (P (mu2, i2)) ->
         if mu1 <= 0 && mu1 = mu2 then
           if i1 = i2 then
             Some (Scalar (P2 i1))
           else
             Some (Scalar (P12 (i1, i2)))
         else
           None
      | Scalar s, Inverse s' | Inverse s, Scalar s' ->
         if s = s' then
           Some (Scalar (Coeff (Value.Integer 1)))
         else
           None
      | _ -> None

    let variable = function
      | Scalar (Variable s) | Inverse (Variable s) -> Some s
      | _ -> None

    let scalar = function
      | Dirac _ | Vector _ -> false
      | Scalar _ | Inverse _ -> true

    let is_unit = function
      | Scalar (Coeff c) | Inverse (Coeff c) ->
         begin match c with
         | Value.Integer 1 -> true
         | Value.Rational q -> Algebra.Q.is_unit q
         | _ -> false
         end
      | _ -> false

    let invertible = scalar

    let invert = function
      | Dirac _ -> invalid_arg "UFOx.Lorentz_Atom.invert Dirac"
      | Vector _ -> invalid_arg "UFOx.Lorentz_Atom.invert Vector"
      | Scalar s -> Inverse s
      | Inverse s -> Scalar s

    let i2s = Index.to_string

    let dirac_to_string = function
      | C (i, j) ->
	 Printf.sprintf "C(%s,%s)" (i2s i) (i2s j)
      | Gamma (mu, i, j) ->
	 Printf.sprintf "Gamma(%s,%s,%s)" (i2s mu) (i2s i) (i2s j)
      | Gamma5 (i, j) ->
	 Printf.sprintf "Gamma5(%s,%s)" (i2s i) (i2s j)
      | Identity (i, j) ->
	 Printf.sprintf "Identity(%s,%s)" (i2s i) (i2s j)
      | ProjP (i, j) ->
	 Printf.sprintf "ProjP(%s,%s)" (i2s i) (i2s j)
      | ProjM (i, j) ->
	 Printf.sprintf "ProjM(%s,%s)" (i2s i) (i2s j)
      | Sigma (mu, nu, i, j) ->
	 Printf.sprintf "Sigma(%s,%s,%s,%s)" (i2s mu) (i2s nu) (i2s i) (i2s j)

    let vector_to_string = function
      | Epsilon (mu, nu, ka, la) ->
	 Printf.sprintf "Epsilon(%s,%s,%s,%s)" (i2s mu) (i2s nu) (i2s ka) (i2s la)
      | Metric (mu, nu) ->
	 Printf.sprintf "Metric(%s,%s)" (i2s mu) (i2s nu)
      | P (mu, n) ->
	 Printf.sprintf "P(%s,%d)" (i2s mu) n

    let scalar_to_string = function
      | Mass id -> Printf.sprintf "Mass(%d)" id
      | Width id -> Printf.sprintf "Width(%d)" id
      | P2 id -> Printf.sprintf "P(%d)**2" id
      | P12 (id1, id2) -> Printf.sprintf "P(%d)*P(%d)" id1 id2
      | Variable s -> s
      | Coeff c -> Value.to_string c

    let to_string = function
      | Dirac d -> dirac_to_string d
      | Vector v -> vector_to_string v
      | Scalar s -> scalar_to_string s
      | Inverse s -> "1/" ^ scalar_to_string s

    module S = UFOx_syntax

    (* \begin{dubious}
         Here we handle some special cases in order to be able to
         parse propagators.  This needs to be made more general,
         but unfortunately the syntax for the propagator extension
         is not well documented and appears to be a bit chaotic!
       \end{dubious} *)

    let quoted_index s =
      Index.named_summation s ()

    let integer_or_id = function
      | S.Integer n -> n
      | S.Variable "id" -> 1
      | _ -> failwith "UFOx.Lorentz_Atom.integer_or_id: impossible"

    let vector_index = function
      | S.Integer n -> n
      | S.Quoted mu -> quoted_index mu
      | S.Variable id ->
         let l = String.length id in
         if l > 1 then
           if id.[0] = 'l' then
             int_of_string (String.sub id 1 (pred l))
           else
             invalid_arg ("UFOx.Lorentz_Atom.vector_index: " ^ id)
         else
           invalid_arg "UFOx.Lorentz_Atom.vector_index: empty variable"
      | _ -> invalid_arg "UFOx.Lorentz_Atom.vector_index"

    let spinor_index = function
      | S.Integer n -> n
      | S.Variable id ->
         let l = String.length id in
         if l > 1 then
           if id.[0] = 's' then
             int_of_string (String.sub id 1 (pred l))
           else
             invalid_arg ("UFOx.Lorentz_Atom.spinor_index: " ^ id)
         else
           invalid_arg "UFOx.Lorentz_Atom.spinor_index: empty variable"
      | _ -> invalid_arg "UFOx.Lorentz_Atom.spinor_index"

    let of_expr name args =
      match name, args with
      | "C", [i; j] -> [Dirac (C (spinor_index i, spinor_index j))]
      | "C", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to C()"
      | "Epsilon", [mu; nu; ka; la] ->
	 [Vector (Epsilon (vector_index mu, vector_index nu,
                           vector_index ka, vector_index la))]
      | "Epsilon", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Epsilon()"
      | "Gamma", [mu; i; j] ->
	 [Dirac (Gamma (vector_index mu, spinor_index i, spinor_index j))]
      | "Gamma", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Gamma()"
      | "Gamma5", [i; j] -> [Dirac (Gamma5 (spinor_index i, spinor_index j))]
      | "Gamma5", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Gamma5()"
      | "Identity", [i; j] -> [Dirac (Identity (spinor_index i, spinor_index j))]
      | "Identity", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Identity()"
      | "Metric", [mu; nu] -> [Vector (Metric (vector_index mu, vector_index nu))]
      | "Metric", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Metric()"
      | "P", [mu; id] -> [Vector (P (vector_index mu, integer_or_id id))]
      | "P", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to P()"
      | "ProjP", [i; j] -> [Dirac (ProjP (spinor_index i, spinor_index j))]
      | "ProjP", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to ProjP()"
      | "ProjM", [i; j] -> [Dirac (ProjM (spinor_index i, spinor_index j))]
      | "ProjM", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to ProjM()"
      | "Sigma", [mu; nu; i; j] ->
         if mu <> nu then
	   [Dirac (Sigma (vector_index mu, vector_index nu,
                          spinor_index i, spinor_index j))]
         else
	   invalid_arg "UFOx.Lorentz.of_expr: implausible arguments to Sigma()"
      | "Sigma", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Sigma()"
      | "PSlash", [i; j; id] ->
         let mu = Index.fresh_summation () in
	 [Dirac (Gamma (mu, spinor_index i, spinor_index j));
          Vector (P (mu, integer_or_id id))]
      | "PSlash", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to PSlash()"
      | "Mass", [id] -> [Scalar (Mass (integer_or_id id))]
      | "Mass", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Mass()"
      | "Width", [id] -> [Scalar (Width (integer_or_id id))]
      | "Width", _ ->
	 invalid_arg "UFOx.Lorentz.of_expr: invalid arguments to Width()"
      | name, [] ->
         [Scalar (Variable name)]
      | name, _ ->
	 invalid_arg ("UFOx.Lorentz.of_expr: invalid tensor '" ^ name ^ "'")

    type r = S | V | T | Sp | CSp | Maj | VSp | CVSp | VMaj | Ghost

    let rep_trivial = function
      | S | Ghost -> true
      | V | T | Sp | CSp | Maj | VSp | CVSp | VMaj -> false

    let rep_to_string = function
      | S -> "0"
      | V -> "1"
      | T -> "2"
      | Sp -> "1/2"
      | CSp-> "1/2bar"
      | Maj -> "1/2M"
      | VSp -> "3/2"
      | CVSp -> "3/2bar"
      | VMaj -> "3/2M"
      | Ghost -> "Ghost"

    let rep_to_string_whizard = function
      | S -> "0"
      | V -> "1"
      | T -> "2"
      | Sp | CSp | Maj -> "1/2"
      | VSp | CVSp | VMaj -> "3/2"
      | Ghost -> "Ghost"

    let rep_of_int neutral = function
      | -1 -> Ghost
      | 1 -> S
      | 2 -> if neutral then Maj else Sp
      | -2 -> if neutral then Maj else CSp (* used by [UFO.Particle.force_conjspinor] *)
      | 3 -> V
      | 4 -> if neutral then VMaj else VSp
      | -4 -> if neutral then VMaj else CVSp (* used by [UFO.Particle.force_conjspinor] *)
      | 5 -> T
      | s when s > 0 ->
         failwith "UFOx.Lorentz: spin > 2 not supported!"
      | _ ->
         invalid_arg "UFOx.Lorentz: invalid non-positive spin value"
	 
    let rep_of_int_or_young_tableau neutral i yt =
      match i, yt with
      | Some i, None -> rep_of_int neutral i
      | None, None -> S
      | _, Some _ -> invalid_arg "UFOx.Lorentz: Young tableau not supported"

    let rep_conjugate = function
      | S -> S
      | V -> V
      | T -> T
      | Sp -> CSp (* ??? *)
      | CSp -> Sp (* ??? *)
      | Maj -> Maj
      | VSp -> CVSp
      | CVSp -> VSp
      | VMaj -> VMaj
      | Ghost -> Ghost

    let classify_vector_indices1 = function
      | Epsilon (mu, nu, ka, la) -> [(mu, V); (nu, V); (ka, V); (la, V)]
      | Metric (mu, nu) -> [(mu, V); (nu, V)]
      | P (mu, _) ->  [(mu, V)]

    let classify_dirac_indices1 = function
      | C (i, j) -> [(i, CSp); (j, Sp)] (* ??? *)
      | Gamma5 (i, j) | Identity (i, j)
      | ProjP (i, j) | ProjM (i, j) -> [(i, CSp); (j, Sp)]
      | Gamma (mu, i, j) -> [(mu, V); (i, CSp); (j, Sp)]
      | Sigma (mu, nu, i, j) -> [(mu, V); (nu, V); (i, CSp); (j, Sp)]

    let classify_indices1 = function
      | Dirac d -> classify_dirac_indices1 d
      | Vector v -> classify_vector_indices1 v
      | Scalar _ | Inverse _ -> []

    module IMap = Map.Make(Int)

    exception Incompatible_factors of r * r

    let product rep1 rep2 =
      match rep1, rep2 with
      | V, V -> T
      | V, Sp -> VSp
      | V, CSp -> CVSp
      | V, Maj -> VMaj
      | Sp, V -> VSp
      | CSp, V -> CVSp
      | Maj, V -> VMaj
      | _, _ -> raise (Incompatible_factors (rep1, rep2))

    let combine_or_add_index (i, rep) map =
      let pos, fac = Index.unpack i in
      try
        let fac', rep' = IMap.find pos map in
        if pos < 0 then
          IMap.add pos (fac, rep) map
        else if fac <> fac' then
          IMap.add pos (0, product rep rep') map
        else if rep <> rep' then (* Can be disambiguated! *)
          IMap.add pos (0, product rep rep') map
        else
          invalid_arg (Printf.sprintf "UFO: duplicate subindex %d" pos)
      with
      | Not_found -> IMap.add pos (fac, rep) map
      | Incompatible_factors (rep1, rep2) ->
         invalid_arg
           (Printf.sprintf
              "UFO: incompatible factors (%s,%s) at %d"
              (rep_to_string rep1) (rep_to_string rep2) pos)

    let combine_or_add_indices atom map =
      List.fold_right combine_or_add_index (classify_indices1 atom) map

    let project_factors (pos, (fac, rep)) =
      if fac = 0 then
        (pos, rep)
      else
        invalid_arg (Printf.sprintf "UFO: leftover subindex %d.%d" pos fac)

    let classify_indices atoms =
      List.map
        project_factors
        (IMap.bindings (List.fold_right combine_or_add_indices atoms IMap.empty))

    let add_factor fac indices pos =
      if pos > 0 then
        if Sets.Int.mem pos indices then
          Index.pack pos fac
        else
          pos
      else
        pos

    let disambiguate_indices1 indices atom =
      rename_indices2 (add_factor 1 indices) (add_factor 2 indices) atom

    let vectorspinors atoms =
      List.fold_left
        (fun acc (i, r) ->
          match r with
          | S | V | T | Sp | CSp | Maj | Ghost -> acc
          | VSp | CVSp | VMaj -> Sets.Int.add i acc)
        Sets.Int.empty (classify_indices atoms)

    let disambiguate_indices atoms =
      let vectorspinor_indices = vectorspinors atoms in
      List.map (disambiguate_indices1 vectorspinor_indices) atoms

    type r_omega = Coupling.lorentz
    let omega = function
      | S -> Coupling.Scalar
      | V -> Coupling.Vector
      | T -> Coupling.Tensor_2
      | Sp -> Coupling.Spinor
      | CSp -> Coupling.ConjSpinor
      | Maj -> Coupling.Majorana
      | VSp -> Coupling.Vectorspinor
      | CVSp -> Coupling.Vectorspinor (* TODO: not really! *)
      | VMaj -> Coupling.Vectorspinor (* TODO: not really! *)
      | Ghost -> Coupling.Scalar

  end
    
module Lorentz = Tensor(Lorentz_Atom')

module type Color_Atom =
  sig
    type t = (* private *)
      | Identity of int * int
      | Identity8 of int * int
      | Delta of int Young.tableau * int * int
      | T of int * int * int
      | TY of int Young.tableau * int * int * int
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
      | Identity8 of int * int
      | Delta of int Young.tableau * int * int
      | T of int * int * int
      | TY of int Young.tableau * int * int * int
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

    let map_indices f = function
      | Identity (i, j) -> Identity (f i, f j)
      | Identity8 (a, b) -> Identity8 (f a, f b)
      | Delta (y, a, b) -> Delta (y, f a, f b)
      | T (a, i, j) -> T (f a, f i, f j)
      | TY (y, a, i, j) -> TY (y, f a, f i, f j)
      | F (a, i, j) -> F (f a, f i, f j)
      | D (a, i, j) -> D (f a, f i, f j)
      | Epsilon (i, j, k) -> Epsilon (f i, f j, f k)
      | EpsilonBar (i, j, k) -> EpsilonBar (f i, f j, f k)
      | T6 (a, i', j') -> T6 (f a, f i', f j')
      | K6 (i', j, k) -> K6 (f i', f j, f k)
      | K6Bar (i', j, k) -> K6Bar (f i', f j, f k)

    let rename_indices = map_indices

    let contract_pair _ _ = None
    let variable _ = None
    let scalar _ = false
    let invertible _ = false
    let is_unit _ = false

    let invert _ =
      invalid_arg "UFOx.Color_Atom.invert"

    let young_tableau_valid_particle y =
      Young.standard_tableau ~offset:1 y

    let of_expr1 name args =
      match name, args with
      | "Identity", [S.Integer i; S.Integer j] -> Identity (i, j)
      | "Identity", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to Identity()"
      | "Delta", [S.Young_Tableau y; S.Integer i; S.Integer j] ->
         if young_tableau_valid_particle y then
            Delta (y, i, j)
         else
	   invalid_arg ("UFOx.Color.of_expr: invalid Young tableau in Delta: " ^
                          Young.tableau_to_string string_of_int y)
      | "Delta", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to Identity()"
      | "T", [S.Integer a; S.Integer i; S.Integer j] -> T (a, i, j)
      | "T", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to T()"
      | "TY", [S.Young_Tableau y; S.Integer a; S.Integer i; S.Integer j] ->
         if young_tableau_valid_particle y then
           TY (y, a, i, j)
         else
	   invalid_arg ("UFOx.Color.of_expr: invalid Young tableau in TY: " ^
                          Young.tableau_to_string string_of_int y)
      | "TY", _ ->
	 invalid_arg "UFOx.Color.of_expr: invalid arguments to TY()"
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
	
    let of_expr name args =
      [of_expr1 name args]

    let to_string = function
      | Identity (i, j) -> Printf.sprintf "Identity(%d,%d)" i j
      | Identity8 (a, b) -> Printf.sprintf "Identity8(%d,%d)" a b
      | Delta (y, a, b) -> Printf.sprintf "Delta(%s,%d,%d)" (Young.tableau_to_string string_of_int y) a b
      | T (a, i, j) -> Printf.sprintf "T(%d,%d,%d)" a i j
      | TY (y, a, i, j) -> Printf.sprintf "TY(%s,%d,%d,%d)" (Young.tableau_to_string string_of_int y) a i j
      | F (a, b, c) -> Printf.sprintf "f(%d,%d,%d)" a b c
      | D (a, b, c) -> Printf.sprintf "d(%d,%d,%d)" a b c
      | Epsilon (i, j, k) -> Printf.sprintf "Epsilon(%d,%d,%d)" i j k
      | EpsilonBar (i, j, k) -> Printf.sprintf "EpsilonBar(%d,%d,%d)" i j k
      | T6 (a, i', j') -> Printf.sprintf "T6(%d,%d,%d)" a i' j'
      | K6 (i', j, k) -> Printf.sprintf "K6(%d,%d,%d)" i' j k
      | K6Bar (i', j, k) -> Printf.sprintf "K6Bar(%d,%d,%d)" i' j k

    type r = S | F | C | A | YT of int Young.tableau

    let conjugate_tableau y =
      Young.map (~-) y

    let young_tableau_valid_UFO y =
      young_tableau_valid_particle y ||
        young_tableau_valid_particle (conjugate_tableau y)

    let young_to_string y =
      ThoList.to_string (ThoList.to_string string_of_int) y

    let rep_trivial = function
      | S | YT [] | YT [[]] -> true
      | F | C | A | YT _ -> false

    let rep_to_string = function
      | S -> "1"
      | F -> "3"
      | C -> "3bar"
      | A -> "8"
      | YT y -> young_to_string y

    let rep_to_string_whizard = function
      | S -> "1"
      | F -> "3"
      | C -> "-3"
      | A -> "8"
      | YT y -> young_to_string y

    let rep_of_int _neutral = function
      | 1 -> S
      | 3 -> F
      | -3 -> C
      | 8 -> A
      | 6 -> YT [[1;2]]
      | -6 -> YT [[-1;-2]]
      | 10 -> YT [[1;2;3]]
      | -10 -> YT [[-1;-2;-3]]
      | n ->
         invalid_arg
           (Printf.sprintf
              "UFOx.Color: impossible representation color = %d!" n)

    let simplify_young_tableau = function
      | [] | [[]] -> S
      | [[i]] ->
         if i < 0 then
           C
         else
           F
      | y -> YT y

    let rep_of_int_or_young_tableau neutral i = function
      | None ->
         begin match i with
         | Some i -> rep_of_int neutral i
         | None ->
            Printf.eprintf "UFO: warning: missing required attribute color!\n";
            S
         end
      | Some y ->
         if young_tableau_valid_UFO y then
           begin match i with
           | None | Some 0 -> YT y
           | Some i ->
              let ri = rep_of_int neutral i in
              if ri = simplify_young_tableau y then
                ri
              else
                invalid_arg
                  (Printf.sprintf
                     "UFOx.Color.rep_of_int_or_young_tableau: color = %d != color_young = %s"
                     i (young_to_string y))
           end
         else
           invalid_arg
             ("UFOx.Color.rep_of_int_or_young_tableau: not a standard tableau: " ^ young_to_string y)

    let rep_conjugate = function
      | S -> S
      | C -> F
      | F -> C
      | A -> A
      | YT y -> YT (conjugate_tableau y)

    (* \begin{dubious}
         Check the particle/anti-particle assignments for
         the sextets with the table on page~\pageref{pg:UFO-Color}!
         \label{pg:classify-indices}
       \end{dubious} *)

    let classify_indices1 = function
      | Identity (i, j) -> [(i, C); (j, F)]
      | Identity8 (a, b) -> [(a, A); (b, A)]
      | Delta (y, a, b) -> [(a, YT (conjugate_tableau y)); (b, YT y)]
      | T (a, i, j) -> [(i, F); (j, C); (a, A)]
      | TY (y, a, i, j) -> [(i, YT y); (j, YT (conjugate_tableau y)); (a, A)]
      | Color_Atom.F (a, b, c) | D (a, b, c) -> [(a, A); (b, A); (c, A)] 
      | Epsilon (i, j, k) -> [(i, F); (j, F); (k, F)]
      | EpsilonBar (i, j, k) -> [(i, C); (j, C); (k, C)]
      | T6 (a, i, j) -> [(a, A); (i, YT [[1;2]]); (j, YT [[-1;-2]])]
      | K6Bar (i, j, k) -> [(i, YT [[-1;-2]]); (j, F); (k, F)]
      | K6 (i, j, k) ->  [(i, YT [[1;2]]); (j, C); (k, C)]

    let classify_indices tensors =
      List.sort compare
	(List.fold_right
	   (fun v acc -> classify_indices1 v @ acc)
	   tensors [])

    let disambiguate_indices atoms =
      atoms

    type r_omega = Color.t

    (* Our encoding of charge conjugation only works
       if the indices start from 1.  In [SU3], we use
       tableau with indices that start from 0. *)

    (* FIXME: $N_C=3$ should not be hardcoded! *)

    let omega = function
      | S -> Color.Singlet
      | F -> Color.SUN (3)
      | C -> Color.SUN (-3)
      | A -> Color.AdjSUN (3)
      | YT [] | YT [[]] -> Color.Singlet
      | YT ([] :: _ as y) -> failwith ("UFOx.Color.omega: invalid tableau: " ^ young_to_string y)
      | YT ((i0 :: _) :: _ as y) ->
         let y = Young.map (fun i -> abs i - 1) y in
         if i0 < 0 then
           Color.YTC y
         else
           Color.YT y

  end

module Color = Tensor(Color_Atom')

module type Test =
  sig
    val suite : OUnit.test
  end

module Test : Test =
  struct

    open OUnit

    let parse_unparse s =
      Value.to_string (Value.of_expr (Expr.of_string s))

    let apup unparsed expr =
      assert_equal ~printer:(fun s -> s) unparsed (parse_unparse expr)

    let apup_id expr =
      apup expr expr

    let suite_arithmetic =
      "arithmetic" >:::
        [ "1 + 2" >:: (fun () -> apup "3" "1+2");
          "1 - 2" >:: (fun () -> apup "-1" "1-2");
          "3 * 2" >:: (fun () -> apup "6" "3*2");
          "3 * (-2)" >:: (fun () -> apup "-6" "3*(-2)");
          "3 / 2" >:: (fun () -> apup "(3/2)" "3/2");
          "4 / 12" >:: (fun () -> apup "(1/3)" "4/12");
          "4 / (-6)" >:: (fun () -> apup "(-2/3)" "4/(-6)");
          "3 * (6 / 12)" >:: (fun () -> apup "3*(1/2)" "3*(6/12)");
          "(3 * 6) / 12)" >:: (fun () -> apup "(3/2)" "(3*6)/12") ]

    let suite_complex =
      "complex" >:::
        [ "1+I" >:: (fun () -> apup "1+I" "1+complex(0,1)");
          "1-I" >:: (fun () -> apup "1-I" "1-complex(0,1)");
          "1-I'" >:: (fun () -> apup "1+(-I)" "1+complex(0,-1)");
          "1+I'" >:: (fun () -> apup "1-(-I)" "1-complex(0,-1)");
          "1+1.+I" >:: (fun () -> apup "1+(1.+I)" "1+complex(1,1)");
          "1+1.-I" >:: (fun () -> apup "1+(1.-I)" "1+complex(1,-1)");
          "1-1.-I" >:: (fun () -> apup "1-(1.+I)" "1-complex(1,1)");
          "1-1.+I" >:: (fun () -> apup "1-(1.-I)" "1-complex(1,-1)");
          "2-I" >:: (fun () -> apup "1-(1.+I)" "1-complex(1,1)");
          "-I+1" >:: (fun () -> apup "-I+1" "-complex(0,1)+1");
          "1.-I+1" >:: (fun () -> apup "(1.-I)+1" "complex(1,-1)+1");
          "1/I" >:: (fun () -> apup "1/I" "1/complex(0,1)");
          "1/1" >:: (fun () -> apup "1" "1/complex(1,0)");
          "1/(-1)" >:: (fun () -> apup "-1" "1/complex(-1,0)");
          "1/(-I)" >:: (fun () -> apup "1/(-I)" "1/complex(0,-1)");
          "1/(2*I)" >:: (fun () -> apup "1/(2.*I)" "1/complex(0,2)");
          "1/(1+I)" >:: (fun () -> apup "1/(1.+I)" "1/complex(1,1)");
          "1/(1-I)" >:: (fun () -> apup "1/(1.-I)" "1/complex(1,-1)");
          "I/2" >:: (fun () -> apup "I/2" "complex(0,1)/2");
          "1/2" >:: (fun () -> apup "(1/2)" "complex(1,0)/2");
          "-1/2" >:: (fun () -> apup "(-1/2)" "complex(-1,0)/2");
          "-I/2" >:: (fun () -> apup "(-I)/2" "complex(0,-1)/2");
          "(2 * I) / 2" >:: (fun () -> apup "(2.*I)/2" "complex(0,2)/2");
          "(1 + I) / 2" >:: (fun () -> apup "(1.+I)/2" "complex(1,1)/2");
          "(1 - I) / 2" >:: (fun () -> apup "(1.-I)/2" "complex(1,-1)/2") ]

    let suite_product =
      "product" >:::
        [ "(-a) * (-b)" >:: (fun () -> apup "a*b" "(-a)*(-b)");
          "a * (-2*b)" >:: (fun () -> apup "-2*a*b" "a*(-2*b)");
          "a * (-2/3*b)" >:: (fun () -> apup "a*(-2/3)*b" "a*(-2/3*b)");
          "(-2*a) * (-2*b)" >:: (fun () -> apup "4*a*b" "(-2*a)*(-2*b)") ]

    let suite_power =
      "power" >:::
        [ "a^b^c^d" >:: (fun () -> apup "a^(b^(c^d))" "a**b**c**d");
          "(a^b)^c^d" >:: (fun () -> apup "(a^b)^(c^d)" "(a**b)**c**d");
          "(a^b)^(c^d)" >:: (fun () -> apup "(a^b)^(c^d)" "(a**b)**(c**d)");
          "((a^b)^c)^d" >:: (fun () -> apup "((a^b)^c)^d" "((a**b)**c)**d") ]

    let suite_apply =
      "apply" >:::
        [ "sin(x) * cos(x)**2" >:: (fun () -> apup "sin(x)*(cos(x))^2" "cmath.sin(x)*cmath.cos(x)**2");
          "sin(x) / cos(x)**2" >:: (fun () -> apup "sin(x)/(cos(x))^2" "cmath.sin(x)/cmath.cos(x)**2");
          "(sin(x) / cos(x))**2" >:: (fun () -> apup "(sin(x)/cos(x))^2" "(cmath.sin(x)/cmath.cos(x))**2") ]

    let suite_expr =
      "unparse/parse" >:::
        [ "a + b" >:: (fun () -> apup_id "a+b");
          "a - b" >:: (fun () -> apup_id "a-b");
          "a + b - c" >:: (fun () -> apup_id "a+b-c");
          "a - b - c" >:: (fun () -> apup_id "a-b-c");
          "-a + b - c" >:: (fun () -> apup_id "-a+b-c");
          "-a - b - c" >:: (fun () -> apup_id "-a-b-c");
          "(a - b) / c" >:: (fun () -> apup_id "(a-b)/c");
          "(a - b) / (c + d)" >:: (fun () -> apup_id "(a-b)/(c+d)");
          "(a + b - c) / d" >:: (fun () -> apup_id "(a+b-c)/d");
          "a^b / c" >:: (fun () -> apup "a^b/c" "a**b/c");
          "(a * b)^c / d" >:: (fun () -> apup "(a*b)^c/d" "(a*b)**c/d");
          "(a * b)^(c/d)" >:: (fun () -> apup "(a*b)^(c/d)" "(a*b)**(c/d)");
          "(a / b)^c / d" >:: (fun () -> apup "(a/b)^c/d" "(a/b)**c/d");
          "(a + b)^c / d" >:: (fun () -> apup "(a+b)^c/d" "(a+b)**c/d");
          "(a - b)^c / d" >:: (fun () -> apup "(a-b)^c/d" "(a-b)**c/d");
          "-a^2" >:: (fun () -> apup "-a^2" "-a**2");
          "(-a)^2" >:: (fun () -> apup "(-a)^2" "(-a)**2");
          "a-b^2" >:: (fun () -> apup "a-b^2" "a-b**2");
          "-a^2 + b + c" >:: (fun () -> apup "-a^2+b+c" "-a**2+b+c");
          "a - b^2 + c" >:: (fun () -> apup "a-b^2+c" "a-b**2+c") ]

    let suite_bugreports =
      "bug reports" >:::
        [ "S2HDMIV:lam1" >::
            (fun () ->
              apup
                "(Mh1^2*RA1x1^2+Mh2^2*RA2x1^2+Mh3^2*RA3x1^2-musq*SB^2)/(CB^2*vH^2)"
                "(Mh1**2*RA1x1**2 + Mh2**2*RA2x1**2 + Mh3**2*RA3x1**2 - musq*SB**2)/(CB**2*vH**2)");
          "loop_sm:AxialZUp" >::
            (fun () -> apup "(3/2)*(-ee*sw)/(6*cw)-(1/2)*cw*ee/(2*sw)" "(3.0/2.0)*(-(ee*sw)/(6.*cw))-(1.0/2.0)*((cw*ee)/(2.*sw))");
          "loop_sm:AxialZUp'" >:: (fun () -> apup "(3/2)*(-ee*sw)/(6*cw)" "(3.0/2.0)*(-(ee*sw)/(6.*cw))");
          "loop_sm:AxialZUp''" >:: (fun () -> apup "(3/2)*(-ee)/2" "(3.0/2.0)*(-ee/2)") ]
      
    let suite =
      "UFOx" >:::
	[suite_arithmetic;
         suite_complex;
         suite_product;
         suite_power;
         suite_apply;
         suite_expr;
         suite_bugreports]

  end

