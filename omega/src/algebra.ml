(* algebra.ml --

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

(* The terms will be small and there's no need to be fancy and/or efficient.
   It's more important to have a unique representation. *)

module PM = Pmap.List

(* \thocwmodulesection{Coefficients} *)

(* For our algebra, we need coefficient rings. *)

module type CRing =
  sig
    type t
    val null : t
    val unit : t
    val mul : t -> t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val neg : t -> t
    val to_string : t -> string
  end

(* And rational numbers provide a particularly important example: *)

module type Rational =
  sig
    include CRing
    val is_null : t -> bool
    val is_unit : t -> bool
    val is_positive : t -> bool
    val is_negative : t -> bool
    val is_integer : t -> bool
    val make : int -> int -> t
    val abs : t -> t
    val inv : t -> t
    val div : t -> t -> t
    val pow : t -> int -> t
    val sum : t list -> t
    val to_ratio : t -> int * int
    val to_float : t -> float
    val to_integer : t -> int
  end

(* \thocwmodulesection{Naive Rational Arithmetic} *)

(* \begin{dubious}
     This \emph{is} dangerous and will overflow even for simple
     applications.  The production code will have to be linked to
     a library for large integer arithmetic.
   \end{dubious} *)

(* Anyway, here's Euclid's algorithm: *)
let rec gcd i1 i2 =
  if i2 = 0 then
    abs i1
  else
    gcd i2 (i1 mod i2)

let lcm i1 i2 = (i1 / gcd i1 i2) * i2

module Small_Rational : Rational =
  struct
    type t = int * int
    let is_null (n, _) = (n = 0)
    let is_unit (n, d) = (n <> 0) && (n = d)
    let is_positive (n, d) = n * d > 0
    let is_negative (n, d) = n * d < 0
    let is_integer (n, d) = (gcd n d = d)
    let null = (0, 1)
    let unit = (1, 1)
    let make n d =
      let c = gcd n d in
      (n / c, d / c)
    let abs (n, d) = (abs n, abs d)
    let inv (n, d) = (d, n)
    let mul (n1, d1) (n2, d2) = make (n1 * n2) (d1 * d2)
    let div q1 q2 = mul q1 (inv q2)
    let add (n1, d1) (n2, d2) = make (n1 * d2 + n2 * d1) (d1 * d2)
    let sub (n1, d1) (n2, d2) = make (n1 * d2 - n2 * d1) (d1 * d2)
    let neg (n, d) = (- n, d)
    let rec pow q p =
      if p = 0 then
	unit
      else if p < 0 then
	pow (inv q) (-p)
      else
	mul q (pow q (pred p))
    let sum qs =
      List.fold_right add qs null
    let to_ratio (n, d) =
      if d < 0 then
        (-n, -d)
      else
        (n, d)
    let to_float (n, d) = float n /. float d
    let to_string (n, d) =
      if d = 1 then
        Printf.sprintf "%d" n
      else
        let n, d = to_ratio (n, d) in
        Printf.sprintf "(%d/%d)" n d
    let to_integer (n, d) =
      if is_integer (n, d) then
        n
      else
        invalid_arg "Algebra.Small_Rational.to_integer"
  end

(* \thocwmodulesection{Expressions: Terms, Rings and Linear Combinations} *)

(* The tensor algebra will be spanned by an abelian monoid: *)

module type Term =
  sig
    type 'a t
    val unit : unit -> 'a t
    val is_unit : 'a t -> bool
    val atom : 'a -> 'a t
    val power : int -> 'a t -> 'a t
    val mul : 'a t -> 'a t -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val to_string : ('a -> string) -> 'a t -> string
    val derive : ('a -> 'b option) -> 'a t -> ('b * int * 'a t) list
    val product : 'a t list -> 'a t
    val atoms : 'a t -> 'a list
  end

module type Ring =
  sig
    module C : Rational
    type 'a t
    val null : unit -> 'a t
    val unit : unit -> 'a t
    val is_null : 'a t -> bool
    val is_unit : 'a t -> bool
    val atom : 'a -> 'a t
    val scale : C.t -> 'a t -> 'a t
    val add : 'a t -> 'a t -> 'a t
    val sub : 'a t -> 'a t -> 'a t
    val mul : 'a t -> 'a t -> 'a t
    val neg : 'a t -> 'a t
    val derive_inner : ('a -> 'a t) -> 'a t -> 'a t (* this? *)
    val derive_inner' : ('a -> 'a t option) -> 'a t -> 'a t (* or that? *)
    val derive_outer : ('a -> 'b option) -> 'a t -> ('b * 'a t) list
    val sum : 'a t list -> 'a t
    val product : 'a t list -> 'a t
    val atoms : 'a t -> 'a list
    val to_string : ('a -> string) -> 'a t -> string
  end

module type Linear =
  sig
    module C : Ring
    type ('a, 'c) t
    val null : unit -> ('a, 'c) t
    val atom : 'a -> ('a, 'c) t
    val singleton : 'c C.t -> 'a -> ('a, 'c) t
    val scale : 'c C.t -> ('a, 'c) t -> ('a, 'c) t
    val add : ('a, 'c) t -> ('a, 'c) t -> ('a, 'c) t
    val sub : ('a, 'c) t -> ('a, 'c) t -> ('a, 'c) t
    val partial : ('c -> ('a, 'c) t) -> 'c C.t -> ('a, 'c) t
    val linear : (('a, 'c) t * 'c C.t) list -> ('a, 'c) t
    val map : ('a -> 'c C.t -> ('b, 'd) t) -> ('a, 'c) t ->  ('b, 'd) t
    val sum : ('a, 'c) t list -> ('a, 'c) t
    val atoms : ('a, 'c) t -> 'a list * 'c list
    val to_string : ('a -> string) -> ('c -> string) -> ('a, 'c) t -> string
  end

module Term : Term =
  struct

    module M = PM

    type 'a t = ('a, int) M.t

    let unit () = M.empty
    let is_unit = M.is_empty

    let atom f = M.singleton f 1

    let power p x = M.map (( * ) p) x

    let insert1 binop f p term =
      let p' = binop (try M.find compare f term with Not_found -> 0) p in
      if p' = 0 then
        M.remove compare f term
      else
        M.add compare f p' term

    let mul1 f p term = insert1 (+) f p term
    let mul x y = M.fold mul1 x y

    let map f term = M.fold (fun t -> mul1 (f t)) term M.empty

    let to_string fmt term =
      String.concat "*"
        (M.fold (fun f p acc ->
          (if p = 0 then
            "1"
          else if p = 1 then
            fmt f
          else
            "[" ^ fmt f ^ "]^" ^ string_of_int p) :: acc) term [])

    let derive derive1 x =
      M.fold (fun f p dx ->
        if p <> 0 then
          match derive1 f with
          | Some df -> (df, p, mul1 f (pred p) (M.remove compare f x)) :: dx
          | None -> dx
        else
          dx) x []

    let product factors =
      List.fold_left mul (unit ()) factors

    let atoms t =
      List.map fst (PM.elements t)
      
  end

module Make_Ring (C : Rational) (T : Term) : Ring =
  struct

    module C = C
    let one = C.unit

    module M = PM

    type 'a t = ('a T.t, C.t) M.t

    let null () = M.empty
    let is_null = M.is_empty

    let power t p = M.singleton t p
    let unit () = power (T.unit ()) one

    let is_unit t = unit () = t

(* \begin{dubious}
     The following should be correct too, but produces to many false
     positives instead!  What's going on?
   \end{dubious} *)
    let broken__is_unit t =
      match M.elements t with
      | [(t, p)] -> T.is_unit t || C.is_null p
      | _ -> false

    let atom t = power (T.atom t) one

    let scale c x = M.map (C.mul c) x

    let insert1 binop t c sum =
      let c' = binop (try M.find compare t sum with Not_found -> C.null) c in
      if C.is_null c' then
        M.remove compare t sum
      else
        M.add compare t c' sum

    let add x y = M.fold (insert1 C.add) x y

    let sub x y = M.fold (insert1 C.sub) y x

    (* One might be tempted to use [Product.outer_self M.fold] instead,
       but this would require us to combine~[tx] and~[cx] to~[(tx, cx)].  *)

    let fold2 f x y =
      M.fold (fun tx cx -> M.fold (f tx cx) y) x

    let mul x y =
      fold2 (fun tx cx ty cy -> insert1 C.add (T.mul tx ty) (C.mul cx cy))
        x y (null ())

    let neg x =
      sub (null ()) x

    let neg x =
      scale (C.neg C.unit) x

    (* Multiply the [derivatives] by [c] and add the result to [dx]. *)
    let add_derivatives derivatives c dx =
      List.fold_left (fun acc (df, dt_c, dt_t) ->
        add (mul df (power dt_t (C.mul c (C.make dt_c 1)))) acc) dx derivatives

    let derive_inner derive1 x =
      M.fold (fun t ->
        add_derivatives (T.derive (fun f -> Some (derive1 f)) t)) x (null ())

    let derive_inner' derive1 x =
      M.fold (fun t -> add_derivatives (T.derive derive1 t)) x (null ())

    let collect_derivatives derivatives c dx =
      List.fold_left (fun acc (df, dt_c, dt_t) ->
        (df, power dt_t (C.mul c (C.make dt_c 1))) :: acc) dx derivatives

    let derive_outer derive1 x =
      M.fold (fun t -> collect_derivatives (T.derive derive1 t)) x []

    let sum terms =
      List.fold_left add (null ()) terms

    let product factors =
      List.fold_left mul (unit ()) factors

    let atoms t =
      ThoList.uniq (List.sort compare
                      (ThoList.flatmap (fun (t, _) -> T.atoms t) (PM.elements t)))
      
    let to_string fmt sum =
      "(" ^ String.concat " + "
              (M.fold (fun t c acc ->
                if C.is_null c then
                  acc
                else if C.is_unit c then
                  T.to_string fmt t :: acc
                else if C.is_unit (C.neg c) then
                  ("(-" ^ T.to_string fmt t ^ ")") :: acc
                else
                  (C.to_string c ^ "*[" ^ T.to_string fmt t ^ "]") :: acc) sum []) ^ ")"

  end

module Make_Linear (C : Ring) : Linear with module C = C =
  struct

    module C = C

    module M = PM

    type ('a, 'c) t = ('a, 'c C.t) M.t

    let null () = M.empty
    let is_null = M.is_empty
    let atom a = M.singleton a (C.unit ())
    let singleton c a = M.singleton a c

    let scale c x = M.map (C.mul c) x

    let insert1 binop t c sum =
      let c' = binop (try M.find compare t sum with Not_found -> C.null ()) c in
      if C.is_null c' then
        M.remove compare t sum
      else
        M.add compare t c' sum

    let add x y = M.fold (insert1 C.add) x y
    let sub x y = M.fold (insert1 C.sub) y x

    let map f t =
      M.fold (fun a c -> add (f a c)) t M.empty

    let sum terms =
      List.fold_left add (null ()) terms

    let linear terms =
      List.fold_left (fun acc (a, c) -> add (scale c a) acc) (null ()) terms

    let partial derive t =
      let d t' =
        let dt' = derive t' in
        if is_null dt' then
          None
        else
          Some dt' in
      linear (C.derive_outer d t)

    let atoms t =
      let a, c = List.split (PM.elements t) in
      (a, ThoList.uniq (List.sort compare (ThoList.flatmap C.atoms c)))
      
    let to_string fmt cfmt sum =
      "(" ^ String.concat " + "
              (M.fold (fun t c acc ->
                if C.is_null c then
                  acc
                else if C.is_unit c then
                  fmt t :: acc
                else if C.is_unit (C.neg c) then
                  ("(-" ^ fmt t ^ ")") :: acc
                else
                  (C.to_string cfmt c ^ "*" ^ fmt t) :: acc) 
                sum []) ^ ")"

  end

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
