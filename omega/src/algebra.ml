(* algebra.ml --

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

module type Test =
  sig
    val suite : OUnit.test
  end

(* The terms will be small and there's no need to be fancy and/or efficient.
   It's more important to have a unique representation. *)

module PM = Pmap.List

(* \thocwmodulesection{Coefficients} *)

module type CRing =
  sig
    type t
    val null : t
    val is_null : t -> bool
    val add : t -> t -> t
    val neg : t -> t
    val sub : t -> t -> t
    val unit : t
    val is_unit : t -> bool
    val mul : t -> t -> t
    val equal : t -> t -> bool
  end

module type Rational =
  sig
    include CRing
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
    val int : int -> t
    val fraction : int -> t
    val compare : t -> t -> int
    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
    module Test : Test
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

let _lcm i1 i2 = (i1 / gcd i1 i2) * i2

let abs_int = abs

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
      if abs_int d = 1 then
        Printf.sprintf "%d" (d * n)
      else
        let n, d = to_ratio (n, d) in
        Printf.sprintf "(%d/%d)" n d

    let pp fmt qc =
      Format.fprintf fmt "%s" (to_string qc)

    let to_integer (n, d) =
      if is_integer (n, d) then
        n
      else
        invalid_arg "Algebra.Small_Rational.to_integer"

    let int n = make n 1
    let fraction n = make 1 n

    let compare q1 q2 =
      let n1, d1 = to_ratio q1
      and n2, d2 = to_ratio q2 in
      compare (d2 * n1) (d1 * n2)

    let equal (n1, d1) (n2, d2) =
      d2 * n1 = d1 * n2

    module Test =
      struct
        open OUnit

        let assert_equal_rational z1 z2 =
          assert_equal ~printer:to_string ~cmp:equal z1 z2

        let suite_mul =
          "mul" >:::

	    [ "1*1=1" >::
                (fun () ->
                  assert_equal_rational (mul unit unit) unit) ]

        let suite =
          "Algebra.Small_Rational" >:::
	    [suite_mul]
      end

  end

module Q = Small_Rational

(* \thocwmodulesection{Rational Complex Numbers} *)

module type QComplex =
  sig
    include CRing
    type q
    val make : q -> q -> t 
    val re : t -> q
    val im : t -> q
    val conj : t -> t
    val inv : t -> t
    val div : t -> t -> t
    val pow : t -> int -> t
    val sum : t list -> t
    val is_positive : t -> bool
    val is_negative : t -> bool
    val is_integer : t -> bool
    val is_real : t -> bool
    val rational : q -> t
    val int : int -> t
    val fraction : int -> t
    val imag : int -> t
    val compare : t -> t -> int
    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
    module Test : Test
  end

module QComplex (Q : Rational) : QComplex with type q = Q.t =
  struct

    type q = Q.t
    type t = { re : q; im : q }

    let make re im = { re; im }
    let null = { re = Q.null; im = Q.null }
    let unit = { re = Q.unit; im = Q.null }

    let re z = z.re
    let im z = z.im
    let conj z = { re = z.re; im = Q.neg z.im }

    let neg z = { re = Q.neg z.re; im = Q.neg z.im }
    let add z1 z2 = { re = Q.add z1.re z2.re; im = Q.add z1.im z2.im }
    let sub z1 z2 = { re = Q.sub z1.re z2.re; im = Q.sub z1.im z2.im }

    let sum qs =
      List.fold_right add qs null

(* Save one multiplication with respect to the standard formula
   \begin{equation}
     (x+iy)(u+iv) = \lbrack xu-yv\rbrack + i\lbrack(x+u)(y+v)-xu-yv\rbrack\,
   \end{equation}
   at the expense of one addition and two subtractions. *)

    let mul z1 z2 =
      let re12 = Q.mul z1.re z2.re
      and im12 = Q.mul z1.im z2.im in
      { re = Q.sub re12 im12;
        im = Q.sub
               (Q.sub (Q.mul (Q.add z1.re z1.im) (Q.add z2.re z2.im)) re12)
               im12 }

    let inv z =
      let modulus = Q.add (Q.mul z.re z.re) (Q.mul z.im z.im) in
      { re = Q.div z.re modulus;
        im = Q.div (Q.neg z.im) modulus }

    let div n d =
      mul (inv d) n

    let rec pow q p =
      if p = 0 then
	unit
      else if p < 0 then
	pow (inv q) (-p)
      else
	mul q (pow q (pred p))

    let is_real q =
      Q.is_null q.im

    let test_real test q =
      is_real q && test q.re
      
    let is_null = test_real Q.is_null
    let is_unit = test_real Q.is_unit
    let is_positive = test_real Q.is_positive
    let is_negative = test_real Q.is_negative
    let is_integer = test_real Q.is_integer

    let rational q = make q Q.null
    let int n = rational (Q.int n)
    let fraction n = rational (Q.fraction n)
    let imag n = make Q.null (Q.int n)

    let compare { re = re1; im = im1 } { re = re2; im = im2 } =
      let c = compare re1 re2 in
      if c <> 0 then
        c
      else
        compare im1 im2

    let equal c1 c2 =
      compare c1 c2 = 0

    let q_to_string ?(leading_plus=false) q =
      (if Q.is_positive q then
         if leading_plus then
           "+"
         else
           ""
       else
         "-") ^ Q.to_string (Q.abs q)

    let im_to_string ?(leading_plus=false) im =
      if Q.is_unit im then
        if leading_plus then
          "+I"
        else
          "I"
      else if Q.is_unit (Q.neg im) then
        "-I"
      else
        q_to_string ~leading_plus im ^ "*I"

    let to_string z =
      if Q.is_null z.im then
        q_to_string z.re
      else if Q.is_null z.re then
        im_to_string z.im
      else
        Printf.sprintf "(%s%s)" (Q.to_string z.re) (im_to_string ~leading_plus:true z.im)

    let pp fmt qc =
      Format.fprintf fmt "%s" (to_string qc)

    module Test =
      struct
        open OUnit

        let assert_equal_complex z1 z2 =
          assert_equal ~printer:to_string ~cmp:equal z1 z2

        let suite_mul =
          "mul" >:::

	    [ "1*1=1" >::
                (fun () ->
                  assert_equal_complex (mul unit unit) unit) ]

        let suite =
          "Algebra.QComplex" >:::
	    [suite_mul]
      end

  end

module QC = QComplex(Q)

(* \thocwmodulesection{Laurent Polynomials} *)

module type Laurent =
  sig
    include CRing
    type c
    val atom : c -> int -> t
    val const : c -> t
    val scale : c -> t -> t
    val sum : t list -> t
    val product : t list -> t
    val pow : t -> int -> t
    val log : t -> (c * int) option
    val to_list : t -> (c * int) list
    val eval : c -> t -> c
    val compare : t -> t -> int
    val rationals : (Q.t * int) list -> t
    val ints : (int * int) list -> t
    val rational : Q.t -> t
    val int : int -> t
    val fraction : int -> t
    val imag : int -> t
    val nc : int -> t
    val over_nc : int -> t
    val prefactor : t list -> c
    val to_string : string -> t -> string
    val pp : Format.formatter -> t -> unit
    module Test : Test
  end


module Laurent : Laurent with type c = QC.t =
  struct

    (* For the function [to_string] below, it would be convenient to order
       in decreasing order of powers, but [to_string] is slow anyway,
       so we can simply revert the result of [IMap.bindings]. *)
    module IMap = Map.Make(Int)

    type c = QC.t

    let qc_minus_one =
      QC.neg QC.unit

    type t = c IMap.t

    let null = IMap.empty
    let is_null l = IMap.for_all (fun _ -> QC.is_null) l

    let atom qc n =
      if qc = QC.null then
        null
      else
        IMap.singleton n qc

    let const z = atom z 0
    let unit = const QC.unit
    let is_unit l = IMap.equal QC.equal l unit

    let add1 n qc l =
      try
        let qc' = QC.add qc (IMap.find n l) in
        if qc' = QC.null then
          IMap.remove n l
        else
          IMap.add n qc' l
      with
      | Not_found -> IMap.add n qc l

    let add l1 l2 =
      IMap.fold add1 l1 l2

    let sum = function
      | [] -> null
      | [l] -> l
      | l :: l_list ->
         List.fold_left add l l_list

    let scale qc l =
      IMap.map (QC.mul qc) l

    let neg l =
      IMap.map QC.neg l

    let sub l1 l2 =
      add l1 (neg l2)

    (* cf.~[Product.fold2_rev] *)
    let fold2 f l1 l2 acc =
      IMap.fold
        (fun n1 qc1 acc1 ->
          IMap.fold
            (fun n2 qc2 acc2 -> f n1 qc1 n2 qc2 acc2)
            l2 acc1)
        l1 acc

    let mul l1 l2 =
      fold2
        (fun n1 qc1 n2 qc2 acc ->
          add1 (n1 + n2) (QC.mul qc1 qc2) acc)
        l1 l2 null
      
    let product = function
      | [] -> unit
      | [l] -> l
      | l :: l_list ->
         List.fold_left mul l l_list

    let poly_pow multiply one inverse x n  =
      let rec pow' i x' acc =
        if i < 1 then
          acc
        else
          pow' (pred i) x' (multiply x' acc) in
      if n < 0 then
        let x' = inverse x in
        pow' (pred (-n)) x' x'
      else if n = 0 then
        one
      else
        pow' (pred n) x x

    let qc_pow z n =
      poly_pow QC.mul QC.unit QC.inv z n

    let pow l n =
      poly_pow mul unit (fun _ -> invalid_arg "Algebra.Laurent.pow") l n

    let log l =
      match IMap.bindings l with
      | [] -> Some (QC.null, 0)
      | [(p, c)] -> Some (c, p)
      | _ -> None

    let to_list l =
      List.map (fun (p, c) -> (c, p)) (List.rev (IMap.bindings l))

    let q_compare_abs q1 q2 =
      Q.compare (Q.abs q1) (Q.abs q2)

    let closer_to_null c1 c2 =
      let r1 = QC.re c1 and r2 = QC.re c2 in
      if Q.is_null r1 then
        if Q.is_null r2 then
          if q_compare_abs (QC.im c1) (QC.im c2) <= 0 then
            c1
          else
            c2
        else
          c2
      else
        if Q.is_null r2 then
          c1
        else if q_compare_abs r1 r2 <= 0 then
          c1
        else
          c2

    let transfer_sign c1 c2 =
      if Q.compare (QC.re c1) Q.null = Q.compare (QC.re c2) Q.null then
        c2
      else
        QC.neg c2

    let prefactor1 l =
      match IMap.max_binding_opt l with
      | None -> QC.unit
      | Some (_, c1) -> transfer_sign c1 (IMap.fold (fun _ -> closer_to_null) l c1)

    let prefactor = function
      | [] -> QC.unit
      | l1 :: ls ->
         List.fold_left
           (fun acc l -> closer_to_null acc (prefactor1 l))
           (prefactor1 l1) ls

    let q_to_string ?(leading_plus=true) q =
      (if Q.is_positive q then
         if leading_plus then
           "+"
         else
           ""
       else
         "-") ^ Q.to_string (Q.abs q)

    let qc_to_string ?(leading_plus=true) z =
      let r = QC.re z
      and i = QC.im z in
      if Q.is_null i then
        q_to_string ~leading_plus r
      else if Q.is_null r then
        if Q.is_unit i then
          if leading_plus then
            "+I"
          else
            "I"
        else if Q.is_unit (Q.neg i) then
          "-I"
        else
          q_to_string i ^ "*I"
      else
        Printf.sprintf "(%s%s*I)" (Q.to_string r) (q_to_string i)

    let to_string1 ?(leading_plus=true) name (n, qc) =
      if n = 0 then
        qc_to_string ~leading_plus qc
      else if n = 1 then
        if QC.is_unit qc then
          if leading_plus then
            "+" ^ name
          else
            name
        else if qc = qc_minus_one then
          "-" ^ name
        else
          Printf.sprintf "%s*%s" (qc_to_string ~leading_plus qc) name
      else if n = -1 then
        Printf.sprintf "%s/%s" (qc_to_string ~leading_plus qc) name
      else if n > 1 then
        if QC.is_unit qc then
          Printf.sprintf "%s%s^%d" (if leading_plus then "+" else "") name n
        else if qc = qc_minus_one then
          Printf.sprintf "-%s^%d" name n
        else
          Printf.sprintf "%s*%s^%d" (qc_to_string ~leading_plus qc) name n
      else
        Printf.sprintf "%s/%s^%d" (qc_to_string ~leading_plus qc) name (-n)

    let to_string name l =
      match List.rev (IMap.bindings l) with
      | [] -> "0"
      | t::l -> to_string1 ~leading_plus:false name t ^ String.concat "" (List.map (to_string1 name) l)

    let pp fmt l =
      Format.fprintf fmt "%s" (to_string "N" l)

    let eval v l =
      IMap.fold
        (fun n qc acc -> QC.add (QC.mul qc (qc_pow v n)) acc)
        l QC.null

    let compare l1 l2 =
      IMap.compare Stdlib.compare l1 l2

    let equal l1 l2 =
      compare l1 l2 = 0

    (* Laurent polynomials: *)
    let of_pairs f pairs =
      sum (List.map (fun (coeff, power) -> atom (f coeff) power) pairs)

    let rationals = of_pairs QC.rational
    let ints = of_pairs QC.int

    let rational q = rationals [(q, 0)]
    let int n = ints [(n, 0)]
    let fraction n = const (QC.fraction n)
    let imag n = const (QC.imag n)
    let nc n = ints [(n, 1)]
    let over_nc n = ints [(n, -1)]

    module Test =
      struct
        open OUnit

        let assert_equal_string s l =
          assert_equal ~printer:Fun.id s (to_string "N" l)

        let assert_equal_laurent l1 l2 =
          assert_equal ~printer:(to_string "N") ~cmp:equal l1 l2

        let assert_equal_coeff c1 c2 =
          assert_equal ~printer:QC.to_string ~cmp:QC.equal c1 c2

        let suite_to_string =
          "to_string" >:::

	    [ "N" >:: (fun () -> assert_equal_string "N" (ints [(1,1)]));
              "-N" >:: (fun () -> assert_equal_string "-N" (ints [(-1,1)]));
              "2N" >:: (fun () -> assert_equal_string "2*N" (ints [(2,1)]));
              "-2N" >:: (fun () -> assert_equal_string "-2*N" (ints [(-2,1)]));
              "1" >:: (fun () -> assert_equal_string "1" (ints [(1,0)]));
              "-1" >:: (fun () -> assert_equal_string "-1" (ints [(-1,0)]));
              "2" >:: (fun () -> assert_equal_string "2" (ints [(2,0)]));
              "-2" >:: (fun () -> assert_equal_string "-2" (ints [(-2,0)]));
              "1/N" >:: (fun () -> assert_equal_string "1/N" (ints [(1,-1)]));
              "-1/N" >:: (fun () -> assert_equal_string "-1/N" (ints [(-1,-1)]));
              "2/N" >:: (fun () -> assert_equal_string "2/N" (ints [(2,-1)]));
              "-2/N" >:: (fun () -> assert_equal_string "-2/N" (ints [(-2,-1)]));
              "N^2+N+1" >:: (fun () -> assert_equal_string "N^2+N+1" (ints [(1,2); (1,1); (1,0)]));
              "N^2-N+1" >:: (fun () -> assert_equal_string "N^2-N+1" (ints [(1,2); (-1,1); (1,0)]));
              "N^2+2N+1" >:: (fun () -> assert_equal_string "N^2+2*N+1" (ints [(1,2); (2,1); (1,0)]));
              "N^2-2N+1" >:: (fun () -> assert_equal_string "N^2-2*N+1" (ints [(1,2); (-2,1); (1,0)]));
              "N^2+N-1" >:: (fun () -> assert_equal_string "N^2+N-1" (ints [(1,2); (1,1); (-1,0)]));
              "N^2-N-1" >:: (fun () -> assert_equal_string "N^2-N-1" (ints [(1,2); (-1,1); (-1,0)]));
              "N^2+2N-1" >:: (fun () -> assert_equal_string "N^2+2*N-1" (ints [(1,2); (2,1); (-1,0)]));
              "N^2-2N-1" >:: (fun () -> assert_equal_string "N^2-2*N-1" (ints [(1,2); (-2,1); (-1,0)])) ]

        let suite_mul =
          "mul" >:::

	    [ "(1+N)(1-N)=1-N^2" >::
                (fun () ->
                  assert_equal_laurent
                    (sum [unit; atom (QC.neg QC.unit) 2])
                    (product [sum [unit; atom QC.unit 1];
                              sum [unit; atom (QC.neg QC.unit) 1]]));

              "(1+N)(1-1/N)=N-1/N" >::
                (fun () ->
                  assert_equal_laurent
                    (sum [atom QC.unit 1; atom (QC.neg QC.unit) (-1)])
                    (product [sum [unit; atom QC.unit 1];
                              sum [unit; atom (QC.neg QC.unit) (-1)]])); ]

        let suite_prefactor =
          "prefactor" >:::

	    [ "0" >:: (fun () -> assert_equal_coeff QC.unit (prefactor []));
              "1" >:: (fun () -> assert_equal_coeff QC.unit (prefactor [unit]));
              "2" >:: (fun () -> assert_equal_coeff (QC.int 2 ) (prefactor [int 2]));
              "1/2" >:: (fun () -> assert_equal_coeff (QC.fraction 2) (prefactor [fraction 2]));

              "1/2-2N" >::
                (fun () -> assert_equal_coeff
                             (QC.neg (QC.fraction 2))
                             (prefactor [sum [fraction 2; nc (-2)]])) ]

        let suite =
          "Algebra.Laurent" >:::
	    [suite_to_string;
             suite_mul;
             suite_prefactor]
      end

  end

(* \thocwmodulesection{Expressions: Terms, Rings and Linear Combinations} *)

(* The tensor algebra will be spanned by an abelian monoid: *)

module type Term =
  sig
    type 'a t
    val unit : unit -> 'a t
    val is_unit : 'a t -> bool
    val atom : 'a -> 'a t
    val power : 'a t -> int -> 'a t
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

    let power x p = M.map (( * ) p) x

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
    let _broken__is_unit t =
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

    let _neg x =
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
