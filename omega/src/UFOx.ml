(* vertex.ml --

   Copyright (C) 1999-2020 by

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

module SMap = Map.Make (struct type t = string let compare = compare end)

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
      | Integer _ | Float _ | Quoted _ as e -> e
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

    let half name =
      Quotient (Variable name, Integer 2)

    let variables = UFOx_syntax.variables
    let functions = UFOx_syntax.functions

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

    let to_string' = string_of_int

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
    val rep_conjugate : r -> r
    val rep_trivial : r -> bool
    type r_omega
    val omega : r -> r_omega
  end

module type Tensor =
  sig
    type atom
    type t = (atom list * Algebra.QC.t) list
    val map_atoms : (atom -> atom) -> t -> t
    val map_indices : (int -> int) -> t -> t
    val rename_indices : (int -> int) -> t -> t
    val map_coef : (Algebra.QC.t -> Algebra.QC.t) -> t -> t
    val of_expr : UFOx_syntax.expr -> t
    val of_string : string -> t
    val of_strings : string list -> t
    val to_string : t -> string
    type r
    val classify_indices : t -> (Index.t * r) list
    val rep_to_string : r -> string
    val rep_to_string_whizard : r -> string
    val rep_of_int : bool -> int -> r
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
    type t = (atom list * QC.t) list

    let map_atoms f t =
      List.map (fun (atoms, q) -> (List.map f atoms, q)) t

    let map_indices f t =
      map_atoms (A.map_indices f) t

    let rename_indices f t =
      map_atoms (A.rename_indices f) t

    let map_coef f t =
      List.map (fun (atoms, q) -> (atoms, f q)) t

    let multiply (t1, c1) (t2, c2) =
      (List.sort compare (t1 @ t2), QC.mul c1 c2)

    let compress terms =
      List.map (fun (t, cs) -> (t, QC.sum cs)) (ThoList.factorize terms)

    let rec of_expr e =
      compress (of_expr' e)

    and of_expr' = function
      | S.Integer i -> [([], QC.make (Q.make i 1) Q.null)]
      | S.Float _ -> invalid_arg "UFOx.Tensor.of_expr: unexpected float"
      | S.Quoted name ->
	 invalid_arg ("UFOx.Tensor.of_expr: unexpected quoted variable '" ^
			 name ^ "'")
      | S.Variable name ->
	 invalid_arg ("UFOx.Tensor.of_expr: unexpected variable '" ^
			 name ^ "'")
      | S.Application ("complex", [re; im]) ->
         begin match of_expr re, of_expr im with
         | [([], re)], [([], im)] ->
            if QC.is_real re && QC.is_real im then
              [([], QC.make (QC.real re) (QC.real im))]
            else
	      invalid_arg ("UFOx.Tensor.of_expr: argument of complex is complex")
         | _ ->
            invalid_arg "UFOx.Tensor.of_expr: unexpected argument of complex"
         end
      | S.Application (name, args) -> [(A.of_expr name args, QC.unit)]
      | S.Sum (e1, e2) ->
	 of_expr e1 @ of_expr e2
      | S.Difference (e1, e2) ->
	 of_expr e1 @ of_expr (S.Product (S.Integer (-1), e2))
      | S.Product (e1, e2) -> Product.list2 multiply (of_expr e1) (of_expr e2)
      | S.Quotient (n, d) ->
	 begin match of_expr d with
	 | [] -> failwith "UFOx.Tensor.of_expr: zero denominator"
	 | [([], q)] -> List.map (fun (t, c) -> (t, QC.div c q)) (of_expr n)
	 | [(invertibles, q)] ->
            if List.for_all A.invertible invertibles then
              let inverses = List.map A.invert invertibles in
	      List.map (fun (t, c) -> (inverses @ t, QC.div c q)) (of_expr n)
            else
              failwith "UFOx.Tensor.of_expr: non-invertible denominator"
	 | _ -> failwith "UFOx.Tensor.of_expr: illegal denominator"
	 end
      | S.Power (e, p) ->
	 begin match of_expr e, of_expr p with
	 | [([], q)], [([], p)] ->
	    if QC.is_real p then
              let re_p = QC.real p in
	      if Q.is_integer re_p then
	        [([], QC.pow q (Q.to_integer re_p))]
	      else
	        failwith "UFOx.Tensor.of_expr: rational power"
            else
	      failwith "UFOx.Tensor.of_expr: complex power"
	 | [([], q)], _ ->
	    failwith "UFOx.Tensor.of_expr: non-numeric power"
	 | t, [([], p)] ->
            let qc = QC.sub p (QC.make (Q.make 2 1) Q.null) in
            if QC.is_null qc then
              Product.list2 multiply t t
            else
	      failwith "UFOx.Tensor.of_expr: only 2 as power of tensor allowed"
	 | _ -> failwith "UFOx.Tensor.of_expr: power of tensor"
	 end

    type r = A.r
    let rep_to_string = A.rep_to_string
    let rep_to_string_whizard = A.rep_to_string_whizard
    let rep_of_int = A.rep_of_int
    let rep_conjugate = A.rep_conjugate
    let rep_trivial = A.rep_trivial

    let classify_indices' filter tensors =
      ThoList.uniq
	(List.sort compare
	   (List.map (fun (t, c) -> filter (A.classify_indices t)) tensors))

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
      List.map disambiguate_indices1 tensors

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

    let to_string terms =
      String.concat "" (List.map term_to_string terms)
      
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

    type t = (* private *)
      | Dirac of dirac
      | Vector of vector
      | Scalar of scalar
      | Inverse of scalar

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

    type t =
      | Dirac of dirac
      | Vector of vector
      | Scalar of scalar
      | Inverse of scalar

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

    let invert = function
      | Dirac _ -> invalid_arg "UFOx.Lorentz_Atom.invert Dirac"
      | Vector _ -> invalid_arg "UFOx.Lorentz_Atom.invert Vector"
      | Scalar s -> Inverse s
      | Inverse s -> Scalar s

    let invertible = function
      | Dirac _ | Vector _ -> false
      | Scalar _ | Inverse _ -> true

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
      | P (mu, n) ->  [(mu, V)]

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

    module IMap = Map.Make (struct type t = int let compare = compare end)

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
      | Identity8 of int * int
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

    let map_indices f = function
      | Identity (i, j) -> Identity (f i, f j)
      | Identity8 (a, b) -> Identity8 (f a, f b)
      | T (a, i, j) -> T (f a, f i, f j)
      | F (a, i, j) -> F (f a, f i, f j)
      | D (a, i, j) -> D (f a, f i, f j)
      | Epsilon (i, j, k) -> Epsilon (f i, f j, f k)
      | EpsilonBar (i, j, k) -> EpsilonBar (f i, f j, f k)
      | T6 (a, i', j') -> T6 (f a, f i', f j')
      | K6 (i', j, k) -> K6 (f i', f j, f k)
      | K6Bar (i', j, k) -> K6Bar (f i', f j, f k)

    let rename_indices = map_indices

    let invert _ =
      invalid_arg "UFOx.Color_Atom.invert"

    let invertible _ =
      false

    let of_expr1 name args =
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
	
    let of_expr name args =
      [of_expr1 name args]

    let to_string = function
      | Identity (i, j) -> Printf.sprintf "Identity(%d,%d)" i j
      | Identity8 (a, b) -> Printf.sprintf "Identity8(%d,%d)" a b
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

    let rep_to_string_whizard = function
      | S -> "1"
      | Sbar -> "-1"
      | F -> "3"
      | C -> "-3"
      | A-> "8"

    let rep_of_int neutral = function
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
      | Identity8 (a, b) -> [(a, A); (b, A)]
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

    let disambiguate_indices atoms =
      atoms

    type r_omega = Color.t

    (* FIXME: $N_C=3$ should not be hardcoded! *)
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
      | Conj

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
      | Integer i -> string_of_int i
      | Rational q -> Q.to_string q
      | Real x -> string_of_float x
      | Complex (0.0, 1.0) -> "I"
      | Complex (0.0, -1.0) -> "-I"
      | Complex (0.0, i) -> string_of_float i ^ "*I"
      | Complex (r, 1.0) -> string_of_float r ^ "+I"
      | Complex (r, -1.0) -> string_of_float r ^ "-I"
      | Complex (r, i) ->
         string_of_float r ^ (if i < 0.0 then "-" else "+") ^
           string_of_float (abs_float i) ^ "*I"
      | Variable s -> s
      | Sum [] -> "0"
      | Sum [e] -> to_string e
      | Sum es -> "(" ^ String.concat "+" (List.map maybe_parentheses es) ^ ")"
      | Difference (e1, e2) -> to_string e1 ^ "-" ^ maybe_parentheses e2
      | Product [] -> "1"
      | Product ((Integer (-1) | Real (-1.)) :: es) ->
         "-" ^ maybe_parentheses (Product es)
      | Product es -> String.concat "*" (List.map maybe_parentheses es)
      | Quotient (e1, e2) -> to_string e1 ^ "/" ^ maybe_parentheses e2
      | Power (e1, e2) -> maybe_parentheses e1 ^ "^" ^ maybe_parentheses e2
      | Application (f, [Integer i]) ->
         to_string (Application (f, [Real (float i)]))
      | Application (f, es) ->
	 builtin_to_string f ^
	   "(" ^ String.concat "," (List.map to_string es) ^ ")"

    and maybe_parentheses = function
      | Integer i as e ->
         if i < 0 then
           "(" ^ to_string e ^ ")"
         else
           to_string e     
      | Real x as e ->
         if x < 0.0 then
           "(" ^ to_string e ^ ")"
         else
           to_string e
      | Complex (x, 0.0) -> to_string (Real x)
      | Complex (0.0, 1.0) -> "I"
      | Variable _ | Power (_, _) | Application (_, _) as e -> to_string e
      | Sum [e] -> to_string e
      | Product [e] -> maybe_parentheses e
      | e -> "(" ^ to_string e ^ ")"

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
      | (Asinh | Acosh | Atanh as f) ->
         failwith
           ("UFOx.Value.to_coupling: function `"
            ^ builtin_to_string f ^ "' not supported yet!")

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
      | S.Variable name -> Variable name
      | S.Sum (e1, e2) ->
	 begin match of_expr e1, of_expr e2 with
	 | (Integer 0 | Real 0.), e -> e
	 | e, (Integer 0 | Real 0.) -> e
	 | Sum e1, Sum e2 -> Sum (e1 @ e2)
	 | e1, Sum e2 -> Sum (e1 :: e2)
	 | Sum e1, e2 -> Sum (e2 :: e1)
	 | e1, e2 -> Sum [e1; e2]
	 end
      | S.Difference (e1, e2) ->
	 begin match of_expr e1, of_expr e2 with
	 | e1, (Integer 0 | Real 0.) -> e1
	 | e1, e2 -> Difference (e1, e2)
         end
      | S.Product (e1, e2) ->
	 begin match of_expr e1, of_expr e2 with
         | (Integer 0 | Real 0.), _ -> Integer 0
         | _, (Integer 0 | Real 0.) -> Integer 0
         | (Integer 1 | Real 1.), e -> e
         | e, (Integer 1 | Real 1.) -> e
	 | Product e1, Product e2 -> Product (e1 @ e2)
	 | e1, Product e2 -> Product (e1 :: e2)
	 | Product e1, e2 -> Product (e2 :: e1)
	 | e1, e2 -> Product [e1; e2]
	 end
      | S.Quotient (e1, e2) ->
         begin match of_expr e1, of_expr e2 with
         | e1, (Integer 0 | Real 0.) ->
            invalid_arg "UFOx.Value: divide by 0"
         | e1, (Integer 1 | Real 1.) -> e1
         | e1, e2 -> Quotient (e1, e2)
         end
      | S.Power (e, p) ->
         begin match of_expr e, of_expr p with
         | (Integer 0 | Real 0.), (Integer 0 | Real 0.) ->
            invalid_arg "UFOx.Value: 0^0"
         | _, (Integer 0 | Real 0.) -> Integer 1
         | e, (Integer 1 | Real 1.) -> e
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

module type Test =
  sig
    val example : unit -> unit
    val suite : OUnit.test
  end
