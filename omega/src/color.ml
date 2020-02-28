(* color.ml --

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

(* Avoid refering to [Pervasives.compare], because [Pervasives] will
   become [Stdlib.Pervasives] in O'Caml 4.07 and [Stdlib] in O'Caml 4.08. *)
let pcompare = compare

(* \thocwmodulesection{Quantum Numbers} *)

type t =
  | Singlet
  | SUN of int
  | AdjSUN of int

let conjugate = function
  | Singlet -> Singlet
  | SUN n -> SUN (-n)
  | AdjSUN n -> AdjSUN n

let compare c1 c2 =
  match c1, c2 with
  | Singlet, Singlet -> 0
  | Singlet, _ -> -1
  | _, Singlet -> 1
  | SUN n, SUN n' -> compare n n'
  | SUN _, AdjSUN _ -> -1
  | AdjSUN _, SUN _ -> 1
  | AdjSUN n, AdjSUN n' -> compare n n'

module type Line =
  sig
    type t
    val conj : t -> t
    val equal : t -> t -> bool
    val to_string : t -> string
  end

module type Cycles =
  sig

    type line
    type t = (line * line) list

(* Contract the graph by connecting lines and return the number of
   cycles together with the contracted graph.
   \begin{dubious}
     The semantics of the contracted graph is not yet 100\%ly fixed.
   \end{dubious} *)
    val contract : t -> int * t

(* The same as [contract], but returns only the number of cycles
   and raises [Open_line] when not all lines are closed. *)
    val count : t -> int
    exception Open_line

    (* Mainly for debugging \ldots *)
    val to_string : t -> string

  end

module Cycles (L : Line) : Cycles with type line = L.t =
  struct

    type line = L.t
    type t = (line * line) list

    exception Open_line

(* NB: The following algorithm for counting the cycles is quadratic since it
   performs nested scans of the lists.  If this was a serious problem one could
   replace the lists of pairs by a [Map] and replace one power by a logarithm. *)

    let rec find_fst c_final c1 disc seen = function
      | [] -> ((L.conj c_final, c1) :: disc, List.rev seen)
      | (c1', c2') as c12' :: rest ->
          if L.equal c1 c1' then
            find_snd c_final (L.conj c2') disc [] (List.rev_append seen rest)
          else
            find_fst c_final c1 disc (c12' :: seen) rest

    and find_snd c_final c2 disc seen = function
      | [] -> ((L.conj c_final, L.conj c2) :: disc, List.rev seen)
      | (c1', c2') as c12' :: rest->
          if L.equal c2' c2 then begin
            if L.equal c1' c_final then
              (disc, List.rev_append seen rest)
            else
              find_fst c_final (L.conj c1') disc [] (List.rev_append seen rest)
          end else
            find_snd c_final c2 disc (c12' :: seen) rest

    let consume = function
      | [] -> ([], [])
      | (c1, c2) :: rest -> find_snd (L.conj c1) (L.conj c2) [] [] rest

    let contract lines =
      let rec contract' acc disc = function
        | [] -> (acc, List.rev disc)
        | rest ->
            begin match consume rest with
            | [], rest' -> contract' (succ acc) disc rest'
            | disc', rest' -> contract' acc (List.rev_append disc' disc) rest'
            end in
      contract' 0 [] lines

    let count lines =
      match contract lines with
      | n, [] -> n
      | n, _ -> raise Open_line

    let to_string lines =
      String.concat ""
        (List.map
           (fun (c1, c2) -> "[" ^ L.to_string c1 ^ "," ^ L.to_string c2 ^ "]")
           lines)

  end

(* \thocwmodulesection{Color Flows} *)

module type Flow =
  sig
    type color
    type t = color list * color list
    val rank : t -> int
    val of_list : int list -> color
    val ghost : unit -> color
    val to_lists : t -> int list list
    val in_to_lists : t -> int list list
    val out_to_lists : t -> int list list
    val ghost_flags : t -> bool list
    val in_ghost_flags : t -> bool list
    val out_ghost_flags : t -> bool list
    type power = { num : int; den : int; power : int }
    type factor = power list
    val factor : t -> t -> factor
    val zero : factor
  end

module Flow : Flow = 
  struct

    type color =
      | Lines of int * int
      | Ghost

    type t = color list * color list

    let rank cflow =
      2

(* \thocwmodulesubsection{Constructors} *)

    let ghost () =
      Ghost

    let of_list = function
      | [c1; c2] -> Lines (c1, c2)
      | _ -> invalid_arg "Color.Flow.of_list: num_lines != 2"

    let to_list = function
      | Lines (c1, c2) -> [c1; c2]
      | Ghost -> [0; 0]

    let to_lists (cfin, cfout) =
      (List.map to_list cfin) @ (List.map to_list cfout)

    let in_to_lists (cfin, _) =
      List.map to_list cfin

    let out_to_lists (_, cfout) =
      List.map to_list cfout

    let ghost_flag = function
      | Lines _ -> false
      | Ghost -> true

    let ghost_flags (cfin, cfout) =
      (List.map ghost_flag cfin) @ (List.map ghost_flag cfout)

    let in_ghost_flags (cfin, _) =
      List.map ghost_flag cfin

    let out_ghost_flags (_, cfout) =
      List.map ghost_flag cfout

(* \thocwmodulesubsection{Evaluation} *)

    type power = { num : int; den : int; power : int }
    type factor = power list
    let zero = []

    let count_ghosts1 colors =
      List.fold_left
        (fun acc -> function Ghost -> succ acc | _ -> acc)
        0 colors

    let count_ghosts (fin, fout) =
      count_ghosts1 fin + count_ghosts1 fout

    type 'a square =
      | Square of 'a
      | Mismatch

    let conjugate = function
      | Lines (c1, c2) -> Lines (-c2, -c1)
      | Ghost -> Ghost

    let cross_in (cin, cout) =
      cin @ (List.map conjugate cout)

    let cross_out (cin, cout) =
      (List.map conjugate cin) @ cout
      
    module C = Cycles (struct
      type t = int
      let conj = (~-)
      let equal = (=)
      let to_string = string_of_int
    end)

    let square f1 f2 =
      let rec square' acc f1' f2' =
        match f1', f2' with
        | [], [] -> Square (List.rev acc)
        | _, [] | [], _ -> Mismatch
        | Ghost :: rest1, Ghost :: rest2 ->
            square' acc rest1 rest2
        | Lines (0, 0) :: rest1, Lines (0, 0) :: rest2 ->
            square' acc rest1 rest2
        | Lines (0, c1') :: rest1, Lines (0, c2') :: rest2 ->
            square' ((c1', c2') :: acc) rest1 rest2
        | Lines (c1, 0) :: rest1, Lines (c2, 0) :: rest2 ->
            square' ((c1, c2) :: acc) rest1 rest2
        | Lines (0, _) :: _, _ | _ , Lines (0, _) :: _
        | Lines (_, 0) :: _, _ | _, Lines (_, 0) :: _ -> Mismatch
        | Lines (_, _) :: _, Ghost :: _ | Ghost :: _, Lines (_, _) :: _ -> Mismatch
        | Lines (c1, c1') :: rest1, Lines (c2, c2') :: rest2 ->
            square' ((c1', c2') :: (c1, c2) :: acc) rest1 rest2 in
      square' [] (cross_out f1) (cross_out f2)

(* In addition to counting closed color loops, we also need to count closed
   gluon loops.  Fortunately, we can use the same algorithm on a different
   data type, provided it doesn't require all lines to be closed. *)

    module C2 = Cycles (struct
      type t = int * int
      let conj (c1, c2) = (- c2, - c1)
      let equal (c1, c2) (c1', c2') = c1 = c1' && c2 = c2'
      let to_string (c1, c2) = "(" ^ string_of_int c1 ^ "," ^ string_of_int c2 ^ ")"
    end)

    let square2 f1 f2 =
      let rec square2' acc f1' f2' =
        match f1', f2' with
        | [], [] -> Square (List.rev acc)
        | _, [] | [], _ -> Mismatch
        | Ghost :: rest1, Ghost :: rest2 ->
            square2' acc rest1 rest2
        | Lines (0, 0) :: rest1, Lines (0, 0) :: rest2 ->
            square2' acc rest1 rest2
        | Lines (0, _) :: rest1, Lines (0, _) :: rest2
        | Lines (_, 0) :: rest1, Lines (_, 0) :: rest2 ->
            square2' acc rest1 rest2
        | Lines (0, _) :: _, _ | _ , Lines (0, _) :: _
        | Lines (_, 0) :: _, _ | _, Lines (_, 0) :: _ -> Mismatch
        | Lines (_, _) :: _, Ghost :: _ | Ghost :: _, Lines (_, _) :: _ -> Mismatch
        | Lines (c1, c1') :: rest1, Lines (c2, c2') :: rest2 ->
            square2' (((c1, c1'), (c2, c2')) :: acc) rest1 rest2 in
      square2' [] (cross_out f1) (cross_out f2)

(* $\ocwlowerid{int\_power}: n\, p \to n^p$
   for integers is missing from [Pervasives]! *)

    let int_power n p =
      let rec int_power' acc i =
        if i < 0 then
          invalid_arg "int_power"
        else if i = 0 then
          acc
        else
          int_power' (n * acc) (pred i) in
      int_power' 1 p

(* Instead of implementing a full fledged algebraic evaluator, let's
   simply expand the binomial by hand:
   \begin{equation}
    \left(\frac{N_C^2-2}{N_C^2}\right)^n =
      \sum_{i=0}^n \binom{n}{i} (-2)^i N_C^{-2i}
   \end{equation} *)

(* NB: Any result of [square] other than [Mismatch] guarantees
   [count_ghosts f1 = count_ghosts f2]. *)

    let factor f1 f2 =
      match square f1 f2, square2 f1 f2 with
      | Mismatch, _ | _, Mismatch -> []
      | Square f12, Square f12' ->
          let num_cycles = C.count f12
          and num_cycles2, disc = C2.contract f12'
          and num_ghosts = count_ghosts f1 in
(*i       Printf.eprintf "f12  = %s -> #loops = %d\n"
            (C.to_string f12) num_cycles;
          Printf.eprintf "f12' = %s -> #loops = %d, disc = %s\n"
            (C2.to_string f12') num_cycles2 (C2.to_string disc);
          flush stderr; i*)
          List.map
            (fun i ->
              let parity = if num_ghosts mod 2 = 0 then 1 else -1
              and power = num_cycles - num_ghosts in
              let coeff = int_power (-2) i * Combinatorics.binomial num_cycles2 i
              and power2 = - 2 * i in
              { num = parity * coeff;
                den = 1;
                power = power + power2 })
            (ThoList.range 0 num_cycles2)

  end

(* later: *)

module General_Flow = 
  struct

    type color =
      | Lines of int list
      | Ghost of int

    type t = color list * color list

    let rank_default = 2 (* Standard model *)

    let rank cflow =
      try
        begin match List.hd cflow with
        | Lines lines -> List.length lines
        | Ghost n_lines -> n_lines
        end
      with
      | _ -> rank_default
  end

(* \thocwmodulesection{Vertex Color Flows} *)

module Q = Algebra.Q
module QC = Algebra.QC

module type Test =
  sig
    val suite : OUnit.test
  end

module type Arrow =
  sig
    type endpoint
    val position : endpoint -> int
    val relocate : (int -> int) -> endpoint -> endpoint
    type tip = endpoint
    type tail = endpoint
    type ghost = endpoint
    type ('tail, 'tip, 'ghost) t =
      | Arrow of 'tail * 'tip
      | Ghost of 'ghost
    type free = (tail, tip, ghost) t
    type factor
    val free_to_string : free -> string
    val factor_to_string : factor -> string
    val map : (endpoint -> endpoint) -> free -> free
    val to_left_factor : (endpoint -> bool) -> free -> factor
    val to_right_factor : (endpoint -> bool) -> free -> factor
    val of_factor : factor -> free
    val negatives : free -> endpoint list
    val is_free : factor -> bool
    val is_ghost : free -> bool
    val single : endpoint -> endpoint -> free
    val double : endpoint -> endpoint -> free list
    val ghost : endpoint -> free
    val chain : int list -> free list
    val cycle : int list -> free list
    type merge =
      | Match of factor
      | Ghost_Match
      | Loop_Match
      | Mismatch
      | No_Match
    val merge : factor -> factor -> merge
    module BinOps : sig
      val (=>) : int -> int -> free
      val (==>) : int -> int -> free list
      val (<=>) : int -> int -> free list
      val (>=>) : int * int -> int -> free
      val (=>>) : int -> int * int -> free
      val (>=>>) : int * int -> int * int -> free
      val (??) : int -> free
    end
    module Test : Test
  end

module Arrow : Arrow =
  struct

    type endpoint =
      | I of int
      | M of int * int

    let position = function
      | I i -> i
      | M (i, _) -> i

    let relocate f = function
      | I i -> I (f i)
      | M (i, n) -> M (f i, n)

    type tip = endpoint
    type tail = endpoint
    type ghost = endpoint

    (* Note that the \emph{same} index can appear multiple
       times on in \emph{each} side. Thus, we \emph{must not}
       combine the arrows in the two factors.
       In fact, we cannot disambiguate them by
       distinguishing tips from tails alone. *)

    type 'a index =
      | Free of 'a
      | SumL of 'a
      | SumR of 'a

    type ('tail, 'tip, 'ghost) t =
      | Arrow of 'tail * 'tip
      | Ghost of 'ghost

    type free = (tail, tip, ghost) t
    type factor = (tail index, tip index, ghost index) t

    let endpoint_to_string = function
      | I i -> string_of_int i
      | M (i, n) -> Printf.sprintf "%d.%d" i n

    let index_to_string = function
      | Free i -> endpoint_to_string i
      | SumL i -> endpoint_to_string i ^ "L"
      | SumR i -> endpoint_to_string i ^ "R"

    let to_string i2s = function
      | Arrow (tail, tip) -> Printf.sprintf "%s>%s" (i2s tail) (i2s tip)
      | Ghost ghost -> Printf.sprintf "{%s}" (i2s ghost)

    let free_to_string = to_string endpoint_to_string

    let factor_to_string = to_string index_to_string

    let index_matches i1 i2 =
      match i1, i2 with
      | SumL i1, SumR i2 | SumR i1, SumL i2 -> i1 = i2
      | _ -> false

    let map f = function
      | Arrow (tail, tip) -> Arrow (f tail, f tip)
      | Ghost ghost -> Ghost (f ghost)

    let free_index = function
      | Free i -> i
      | SumL i -> invalid_arg "Color.Arrow.free_index: leftover LHS summation"
      | SumR i -> invalid_arg "Color.Arrow.free_index: leftover RHS summation"

    let to_left_index is_sum i =
      if is_sum i then
        SumL i
      else
        Free i

    let to_right_index is_sum i =
      if is_sum i then
        SumR i
      else
        Free i

    let to_left_factor is_sum = map (to_left_index is_sum)
    let to_right_factor is_sum = map (to_right_index is_sum)
    let of_factor = map free_index

    let negatives = function
      | Arrow (tail, tip) ->
         if position tail < 0 then
           if position tip < 0 then
             [tail; tip]
           else
             [tail]
         else if position tip < 0 then
           [tip]
         else
           []
      | Ghost ghost ->
         if position ghost < 0 then
           [ghost]
         else
           []

    let is_free = function
      | Arrow (Free _, Free _) | Ghost (Free _) -> true
      | _ -> false

    let is_ghost = function
      | Ghost _ -> true
      | Arrow _ -> false

    let single tail tip =
      Arrow (tail, tip)

    let double a b =
      if a = b then
        [single a b]
      else
        [single a b; single b a]

    let ghost g =
      Ghost g

    type merge =
      | Match of factor
      | Ghost_Match
      | Loop_Match
      | Mismatch
      | No_Match

    let merge arrow1 arrow2 =
      match arrow1, arrow2 with
      | Ghost g1, Ghost g2 ->
         if index_matches g1 g2 then
           Ghost_Match
         else
           No_Match
      | Arrow (tail, tip), Ghost g
      | Ghost g, Arrow (tail, tip) ->
         if index_matches g tail || index_matches g tip then
           Mismatch
         else
           No_Match
      | Arrow (tail, tip), Arrow (tail', tip') ->
         if index_matches tip tail' then
           if index_matches tip' tail then
             Loop_Match
           else
             Match (Arrow (tail, tip'))
         else if index_matches tip' tail then
           Match (Arrow (tail', tip))
         else
           No_Match

    module BinOps =
      struct
        let (=>) i j = single (I i) (I j)
        let (==>) i j = [i => j]
        let (<=>) i j = double (I i) (I j)
        let ( >=> ) (i, n) j = single (M (i, n)) (I j)
        let ( =>> ) i (j, m) = single (I i) (M (j, m))
        let ( >=>> ) (i, n) (j, m) = single (M (i, n)) (M (j, m))
        (* I wanted to use [~~] instead of [??], but ocamlweb doesn't like
           operators starting with [~] in the index. *)
        let (??) i = ghost (I i)
      end

    open BinOps

    (* Composite Arrows. *)

    let rec chain' = function
      | [] -> []
      | [a] -> [a => a]
      | [a; b] -> [a => b]
      | a :: (b :: _ as rest) -> (a => b) :: chain' rest

    let chain = function
      | [] -> []
      | a :: _ as a_list -> chain' a_list

    let rec cycle' a = function
      | [] -> [a => a]
      | [b] -> [b => a]
      | b :: (c :: _ as rest) -> (b => c) :: cycle' a rest

    let cycle = function
      | [] -> []
      | a :: _ as a_list -> cycle' a a_list

    module Test : Test =
      struct

        open OUnit

        let suite_chain =
          "chain" >:::

            [ "chain []" >::
	        (fun () ->
	          assert_equal [] (chain []));

              "chain [1]" >::
	        (fun () ->
	          assert_equal [1 => 1] (chain [1]));

              "chain [1;2]" >::
	        (fun () ->
	          assert_equal [1 => 2] (chain [1; 2]));

              "chain [1;2;3]" >::
	        (fun () ->
	          assert_equal [1 => 2; 2 => 3] (chain [1; 2; 3]));

              "chain [1;2;3;4]" >::
	        (fun () ->
	          assert_equal [1 => 2; 2 => 3; 3 => 4] (chain [1; 2; 3; 4])) ]

        let suite_cycle =
          "cycle" >:::

            [ "cycle []" >::
	        (fun () ->
	          assert_equal [] (cycle []));

              "cycle [1]" >::
	        (fun () ->
	          assert_equal [1 => 1] (cycle [1]));

              "cycle [1;2]" >::
	        (fun () ->
	          assert_equal [1 => 2; 2 => 1] (cycle [1; 2]));

              "cycle [1;2;3]" >::
	        (fun () ->
	          assert_equal [1 => 2; 2 => 3; 3 => 1] (cycle [1; 2; 3]));

              "cycle [1;2;3;4]" >::
	        (fun () ->
	          assert_equal
                    [1 => 2; 2 => 3; 3 => 4; 4 => 1]
                    (cycle [1; 2; 3; 4])) ]

        let suite =
          "Color.Arrow" >:::
	    [suite_chain;
             suite_cycle]

      end

  end

module type Propagator =
  sig
    type cf_in = int
    type cf_out = int
    type t = W | I of cf_in | O of cf_out | IO of cf_in * cf_out | G
    val to_string : t -> string
  end

module Propagator : Propagator =
  struct
    type cf_in = int
    type cf_out = int
    type t = W | I of cf_in | O of cf_out | IO of cf_in * cf_out | G
    let to_string = function
      | W -> "W"
      | I cf -> Printf.sprintf "I(%d)" cf
      | O cf' -> Printf.sprintf "O(%d)" cf'
      | IO (cf, cf') -> Printf.sprintf "IO(%d,%d)" cf cf'
      | G -> "G"
  end

module type LP =
  sig
    val rationals : (Algebra.Q.t * int) list -> Algebra.Laurent.t
    val ints : (int * int) list -> Algebra.Laurent.t

    val rational : Algebra.Q.t -> Algebra.Laurent.t
    val int : int -> Algebra.Laurent.t
    val fraction : int -> Algebra.Laurent.t
    val imag : int -> Algebra.Laurent.t
    val nc : int -> Algebra.Laurent.t
    val over_nc : int -> Algebra.Laurent.t
  end

module LP : LP =
  struct
    module L = Algebra.Laurent

    (* Rationals from integers. *)
    let q_int n = Q.make n 1
    let q_fraction n = Q.make 1 n

    (* Complex rationals: *)
    let qc_rational q = QC.make q Q.null
    let qc_int n = qc_rational (q_int n)
    let qc_fraction n = qc_rational (q_fraction n)
    let qc_imag n = QC.make Q.null (q_int n)

    (* Laurent polynomials: *)
    let of_pairs f pairs =
      L.sum (List.map (fun (coeff, power) -> L.atom (f coeff) power) pairs)

    let rationals = of_pairs qc_rational
    let ints = of_pairs qc_int

    let rational q = rationals [(q, 0)]
    let int n = ints [(n, 0)]
    let fraction n = L.const (qc_fraction n)
    let imag n = L.const (qc_imag n)
    let nc n = ints [(n, 1)]
    let over_nc n = ints [(n, -1)]

  end

module type Birdtracks =
  sig
    type t
    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
    val trivial : t -> bool
    val is_null : t -> bool
    val unit : t
    val null : t
    val two : t
    val half : t
    val third : t
    val minus : t
    val nc : t
    val imag : t
    val ints : (int * int) list -> t
    val const : Algebra.Laurent.t -> t
    val times : t -> t -> t
    val multiply : t list -> t
    val scale : QC.t -> t -> t
    val sum : t list -> t
    val diff : t -> t -> t
    val f_of_rep : (int -> int -> int -> t) -> int -> int -> int -> t
    val d_of_rep : (int -> int -> int -> t) -> int -> int -> int -> t
    module BinOps : sig
      val ( +++ ) : t -> t -> t
      val ( --- ) : t -> t -> t
      val ( *** ) : t -> t -> t
    end
    val map : (int -> int) -> t -> t
    val fuse : int -> t -> Propagator.t list -> (QC.t * Propagator.t) list
    module Test : Test
  end

module Birdtracks =
  struct

    module A = Arrow
    open A.BinOps
    module P = Propagator
    module L = Algebra.Laurent

    type connection = L.t * A.free list
    type t = connection list

    let trivial = function
      | [] -> true
      | [(coeff, [])] -> coeff = L.unit
      | _ -> false

    (* Rationals from integers. *)
    let q_int n = Q.make n 1
    let q_fraction n = Q.make 1 n

    (* Complex rationals: *)
    let qc_rational q = QC.make q Q.null
    let qc_int n = qc_rational (q_int n)
    let qc_fraction n = qc_rational (q_fraction n)
    let qc_imag n = QC.make Q.null (q_int n)

    (* Laurent polynomials: *)
    let laurent_of_pairs f pairs =
      L.sum (List.map (fun (coeff, power) -> L.atom (f coeff) power) pairs)

    let l_rationals = laurent_of_pairs qc_rational
    let l_ints = laurent_of_pairs qc_int

    let l_rational q = l_rationals [(q, 0)]
    let l_int n = l_ints [(n, 0)]
    let l_fraction n = L.const (qc_fraction n)
    let l_imag n = L.const (qc_imag n)
    let l_nc n = l_ints [(n, 1)]
    let l_over_nc n = l_ints [(n, -1)]

    (* Expressions *)
    let unit = []
    let const c = [c, []]
    let ints pairs = const (LP.ints pairs)
    let null = const L.null
    let half = const (LP.fraction 2)
    let third = const (LP.fraction 3)
    let two = const (LP.int 2)
    let minus = const (LP.int (-1))
    let nc = const (LP.nc 1)
    let imag = const (LP.imag 1)

    module AMap = Pmap.Tree

    let find_arrows_opt arrows map =
      try Some (AMap.find pcompare arrows map) with Not_found -> None

    let canonicalize1 (coeff, io_list) =
      (coeff, List.sort pcompare io_list)

    let canonicalize terms =
      let map =
        List.fold_left
          (fun acc term ->
            let coeff, arrows = canonicalize1 term in
            if coeff = L.null then
              acc
            else
              match find_arrows_opt arrows acc with
              | None -> AMap.add pcompare arrows coeff acc
              | Some coeff' ->
                 let coeff'' = L.add coeff coeff' in
                 if coeff'' = L.null then
                   AMap.remove pcompare arrows acc
                 else
                   AMap.add pcompare arrows coeff'' acc)
          AMap.empty terms in
      if AMap.is_empty map then
        null
      else
        AMap.fold (fun arrows coeff acc -> (coeff, arrows) :: acc) map []

    let arrows_to_string_aux f arrows =
      ThoList.to_string f arrows

    let to_string1_aux f (coeff, arrows) =
      Printf.sprintf
        "(%s) * %s"
        (L.to_string "N" coeff) (arrows_to_string_aux f arrows)

    let to_string1_opt_aux f = function
      | None -> "None"
      | Some v -> to_string1_aux f v

    let to_string_raw_aux f v =
      ThoList.to_string (to_string1_aux f) v

    let to_string_aux f v =
      to_string_raw_aux f (canonicalize v)

    let factor_arrows_to_string = arrows_to_string_aux A.factor_to_string
    let factor_to_string1 = to_string1_aux A.factor_to_string
    let factor_to_string1_opt = to_string1_opt_aux A.factor_to_string
    let factor_to_string_raw = to_string_raw_aux A.factor_to_string
    let factor_to_string = to_string_aux A.factor_to_string

    let arrows_to_string = arrows_to_string_aux A.free_to_string
    let to_string1 = to_string1_aux A.free_to_string
    let to_string1_opt = to_string1_opt_aux A.free_to_string
    let to_string_raw = to_string_raw_aux A.free_to_string
    let to_string = to_string_aux A.free_to_string

    let pp fmt v =
      Format.fprintf fmt "%s" (to_string v)

    let is_null v =
      match canonicalize v with
      | [c, _] -> c = L.null
      | _ -> false

    let is_white = function
      | P.W -> true
      | _ -> false

    let map1 f (c, v) =
      (c, List.map (A.map (A.relocate f)) v)

    let map f = List.map (map1 f)

    let add_arrow arrow (coeff, arrows) =
      let rec add_arrow' arrow (coeff, acc) = function
        | [] ->
           (* No opportunities for further matches *)
           Some (coeff, arrow :: acc)
        | arrow' :: arrows' ->
           begin match A.merge arrow arrow' with
           | A.Mismatch ->
              None
           | A.Ghost_Match ->
              Some (L.mul (LP.over_nc (-1)) coeff,
                    List.rev_append acc arrows')
           | A.Loop_Match ->
              Some (L.mul (LP.nc 1) coeff, List.rev_append acc arrows')
           | A.Match arrow'' ->
              if A.is_free arrow'' then
                Some (coeff, arrow'' :: List.rev_append acc arrows')
              else
                (* the new [arrow''] ist not yet saturated, try again: *)
                add_arrow' arrow'' (coeff, acc) arrows'
           | A.No_Match ->
              add_arrow' arrow (coeff, arrow' :: acc) arrows'
           end in
      add_arrow' arrow (coeff, []) arrows

    let logging_add_arrow arrow (coeff, arrows) =
      let result = add_arrow arrow (coeff, arrows) in
      Printf.eprintf
        "add_arrow %s to %s ==> %s\n"
        (A.factor_to_string arrow)
        (factor_to_string1 (coeff, arrows))
        (factor_to_string1_opt result);
      result

    (* We can reject the contributions with unsaturated summation indices
       from Ghost contributions to~$T_a$ only \emph{after} adding all
       arrows that might saturate an open index. *)

    let add_arrows factor1 arrows2 =
      let rec add_arrows' (_, arrows as acc) = function
        | [] ->
           if List.for_all A.is_free arrows then
             Some acc
           else
             None
        | arrow :: arrows ->
           begin match add_arrow arrow acc with
           | None -> None
           | Some acc' -> add_arrows' acc' arrows 
           end in
      add_arrows' factor1 arrows2

    let logging_add_arrows factor1 arrows2 =
      let result = add_arrows factor1 arrows2 in
      Printf.eprintf
        "add_arrows %s to %s ==> %s\n"
        (factor_to_string1 factor1)
        (factor_arrows_to_string arrows2)
        (factor_to_string1_opt result);
      result

    (* Note that a negative index might be summed only
       later in a sequence of binary products and must
       therefore be treated as free in this product.  Therefore,
       we have to classify the indices as summation indices
       \emph{not only} based on their sign, but in addition based on
       whether they appear in both factors. Only then can we reject
       surviving ghosts. *)

    module ESet =
      Set.Make
        (struct
          type t = A.endpoint
          let compare = pcompare
        end)

    let negatives arrows =
      List.fold_left
        (fun acc arrow ->
          List.fold_left
            (fun acc' i -> ESet.add i acc')
            acc (A.negatives arrow))
        ESet.empty arrows

    let times1 (coeff1, arrows1) (coeff2, arrows2) =
      let summations = ESet.inter (negatives arrows1) (negatives arrows2) in
      let is_sum i = ESet.mem i summations in
      let arrows1' = List.map (A.to_left_factor is_sum) arrows1
      and arrows2' = List.map (A.to_right_factor is_sum) arrows2 in
      match add_arrows (coeff1, arrows1') arrows2' with
      | None -> None
      | Some (coeff1, arrows) ->
         Some (L.mul coeff1 coeff2, List.map A.of_factor arrows)

    let logging_times1 factor1 factor2 =
      let result = times1 factor1 factor2 in
      Printf.eprintf
        "%s times1 %s ==> %s\n"
        (to_string1 factor1)
        (to_string1 factor2)
        (to_string1_opt result);
      result

    let sum terms =
      canonicalize (List.concat terms)

    let times term term' =
      canonicalize (Product.list2_opt times1 term term')

    (* \begin{dubious}
         Is that more efficient than the following implementation?
       \end{dubious} *)

    let rec multiply1' acc = function
      | [] -> Some acc
      | factor :: factors ->
         begin match times1 acc factor with
         | None -> None
         | Some acc' -> multiply1' acc' factors
         end

    let multiply1 = function
      | [] -> Some (L.unit, [])
      | [factor] -> Some factor
      | factor :: factors -> multiply1' factor factors

    let multiply termss =
      canonicalize (Product.list_opt multiply1 termss)

    (* \begin{dubious}
         Isn't that the more straightforward implementation?
       \end{dubious} *)

    let multiply = function
      | [] -> []
      | term :: terms ->
         canonicalize (List.fold_left times term terms)

    let scale1 q (coeff, arrows) =
      (L.scale q coeff, arrows)
    let scale q = List.map (scale1 q)

    let diff term1 term2 =
      canonicalize (List.rev_append term1 (scale (qc_int (-1)) term2))

    module BinOps =
      struct
        let ( +++ ) term term' = sum [term; term']
        let ( --- ) = diff
        let ( *** ) = times
      end

    open BinOps

    let trace3 r a b c =
      r a (-1) (-2) *** r b (-2) (-3) *** r c (-3) (-1)

    let f_of_rep r a b c =
      minus *** imag *** (trace3 r a b c --- trace3 r a c b)

    let d_of_rep r a b c =
      trace3 r a b c +++ trace3 r a c b

    module IMap =
      Map.Make (struct type t = int let compare = pcompare end)

    let line_map lines =
      let _, map =
        List.fold_left
          (fun (i, acc) line ->
            (succ i,
             match line with
             | P.W -> acc
             | _ -> IMap.add i line acc))
          (1, IMap.empty)
          lines in
      map

    let find_opt i map =
      try Some (IMap.find i map) with Not_found -> None

    let lines_to_string lines =
      match IMap.bindings lines with
      | [] -> "W"
      | lines ->
         String.concat
           " "
           (List.map
              (fun (i, c) -> Printf.sprintf "%s@%d" (P.to_string c) i)
              lines)

    let clear = IMap.remove

    let add_in i cf lines =
      match find_opt i lines with
      | Some (P.O cf') -> IMap.add i (P.IO (cf, cf')) lines
      | _ -> IMap.add i (P.I cf) lines

    let add_out i cf' lines =
      match find_opt i lines with
      | Some (P.I cf) -> IMap.add i (P.IO (cf, cf')) lines
      | _ -> IMap.add i (P.O cf') lines

    let add_ghost i lines =
      IMap.add i P.G lines

    let connect1 n arrow lines =
      match arrow with
      | A.Ghost g ->
         let g = A.position g in
         if g = n then
           Some (add_ghost n lines)
         else
           begin match find_opt g lines with
           | Some P.G -> Some (clear g lines)
           | _ -> None
           end
      | A.Arrow (i, o) ->
         let i = A.position i
         and o = A.position o in
         if o = n then
           match find_opt i lines with
           | Some (P.I cfi) -> Some (add_in o cfi (clear i lines))
           | Some (P.IO (cfi, cfi')) -> Some (add_in o cfi (add_out i cfi' lines))
           | _ -> None
         else if i = n then
           match find_opt o lines with
           | Some (P.O cfo') -> Some (add_out i cfo' (clear o lines))
           | Some (P.IO (cfo, cfo')) -> Some (add_out i cfo' (add_in o cfo lines))
           | _ -> None
         else
           match find_opt i lines, find_opt o lines with
           | Some (P.I cfi), Some (P.O cfo') when cfi = cfo' ->
              Some (clear o (clear i lines))
           | Some (P.I cfi), Some (P.IO (cfo, cfo')) when cfi = cfo'->
              Some (add_in o cfo (clear i lines))
           | Some (P.IO (cfi, cfi')), Some (P.O cfo') when cfi = cfo' ->
              Some (add_out i cfi' (clear o lines))
           | Some (P.IO (cfi, cfi')), Some (P.IO (cfo, cfo')) when cfi = cfo' ->
              Some (add_in o cfo (add_out i cfi' lines))
           | _ -> None
        
    let connect connections lines =
      let n = succ (List.length lines)
      and lines = line_map lines in
      let rec connect' acc = function
        | arrow :: arrows ->
           begin match connect1 n arrow acc with
           | None -> None
           | Some acc -> connect' acc arrows
           end
        | [] -> Some acc in
      match connect' lines connections with
      | None -> None
      | Some acc ->
         begin match IMap.bindings acc with
         | [] -> Some P.W
         | [(i, cf)] when i = n -> Some cf
         | _ -> None
         end

    let fuse1 nc lines (c, vertex) =
      match connect vertex lines with
      | None -> []
      | Some cf -> [(L.eval (qc_int nc) c, cf)]
             
    let fuse nc vertex lines =
      match vertex with
      | [] ->
         if List.for_all is_white lines then
           [(QC.unit, P.W)]
         else
           []
      | vertex ->
         ThoList.flatmap (fuse1 nc lines) vertex

    module Test : Test =
      struct
        open OUnit

        let vertices1_equal v1 v2 =
          match v1, v2 with
          | None, None -> true
          | Some v1, Some v2 -> (canonicalize1 v1) = (canonicalize1 v2)
          | _ -> false

        let assert_equal_vertices1 v1 v2 =
          assert_equal ~printer:to_string1_opt ~cmp:vertices1_equal v1 v2

        let suite_times1 =
          "times1" >:::

            [ "merge two" >::
	        (fun () ->
	          assert_equal_vertices1
                    (Some (L.unit, 1 ==> 2))
                    (times1 (L.unit,  1 ==> -1) (L.unit, -1 ==>  2)));

              "merge two exchanged" >::
	        (fun () ->
	          assert_equal_vertices1
                    (Some (L.unit, 1 ==> 2))
                    (times1 (L.unit, -1 ==>  2) (L.unit,  1 ==> -1)));

              "ghost1" >::
	        (fun () ->
	          assert_equal_vertices1
                    (Some (l_over_nc (-1), 1 ==> 2))
                    (times1
                       (L.unit, [-1 =>  2; ?? (-3)])
                       (L.unit, [ 1 => -1; ?? (-3)])));

              "ghost2" >::
	        (fun () ->
	          assert_equal_vertices1
                    None
                    (times1
                       (L.unit, [ 1 => -1; ?? (-3)])
                       (L.unit, [-1 =>  2; -3 => -4; -4 => -3])));

              "ghost2 exchanged" >::
	        (fun () ->
	          assert_equal_vertices1
                    None
                    (times1
                       (L.unit, [-1 =>  2; -3 => -4; -4 => -3])
                       (L.unit, [ 1 => -1; ?? (-3)]))) ]

        let suite_canonicalize =
          "canonicalize" >:::

            [ ]

        let line_option_to_string = function
          | None -> "no match"
          | Some line -> P.to_string line

        let test_connect_msg vertex formatter (expected, result) =
          Format.fprintf
            formatter
            "[%s]: expected %s, got %s"
            (arrows_to_string vertex)
            (line_option_to_string expected)
            (line_option_to_string result)

        let test_connect expected lines vertex =
	  assert_equal
            ~printer:line_option_to_string
            expected (connect vertex lines)

        let test_connect_permutations expected lines vertex =
          List.iter
            (fun v ->
	      assert_equal
                ~pp_diff:(test_connect_msg v)
                expected (connect v lines))
            (Combinatorics.permute vertex)

        let suite_connect =
          "connect" >:::

            [ "delta" >::
	        (fun () ->
                  test_connect_permutations
                    (Some (P.I 1))
                    [ P.I 1; P.W ]
                    ( 1 ==> 3 ));

              "f: 1->3->2->1" >::
                (fun () ->
                  test_connect_permutations
                    (Some (P.IO (1, 3)))
                    [P.IO (1, 2); P.IO (2, 3)]
                    (A.cycle [1; 3; 2]));

              "f: 1->2->3->1" >::
                (fun () ->
                  test_connect_permutations
                    (Some (P.IO (1, 2)))
                    [P.IO (3, 2); P.IO (1, 3)]
                    (A.cycle [1; 2; 3])) ]

        let suite =
          "Color.Birdtracks" >:::
	    [suite_times1;
             suite_canonicalize;
             suite_connect]
      end

    let vertices_equal v1 v2 =
      is_null (v1 --- v2)

    let assert_equal_vertices v1 v2 =
      OUnit.assert_equal ~printer:to_string ~cmp:vertices_equal v1 v2

  end
    
(* \thocwmodulesubsection{$\mathrm{SU}(N_C)$}
   We're computing with a general $N_C$, but [epsilon] and [epsilonbar]
   make only sense for $N_C=3$.  Also some of the terminology alludes
   to $N_C=3$: triplet, sextet, octet. *)

module type SU3 =
  sig
    include Birdtracks
    val delta3 : int -> int -> t
    val delta8 : int -> int -> t
    val delta8_loop : int -> int -> t
    val gluon : int -> int -> t
    val t : int -> int -> int -> t
    val f : int -> int -> int -> t
    val d : int -> int -> int -> t
    val epsilon : int -> int -> int -> t
    val epsilonbar : int -> int -> int -> t
    val t6 : int -> int -> int -> t
    val k6 : int -> int -> int -> t
    val k6bar : int -> int -> int -> t
  end

module SU3 : SU3 =
  struct

    module A = Arrow
    open Arrow.BinOps

    module B = Birdtracks
    type t = B.t
    let to_string = B.to_string
    let pp = B.pp
    let trivial = B.trivial
    let is_null = B.is_null
    let null = B.null
    let unit = B.unit
    let const = B.const
    let two = B.two
    let half = B.half
    let third = B.third
    let nc = B.imag
    let minus = B.minus
    let imag = B.imag
    let ints = B.ints
    let sum = B.sum
    let diff = B.diff
    let scale = B.scale
    let times = B.times
    let multiply = B.multiply
    let map = B.map
    let fuse = B.fuse
    let f_of_rep = B.f_of_rep
    let d_of_rep = B.d_of_rep
    module BinOps = B.BinOps

    let delta3 i j =
      [(LP.int 1, i ==> j)]

    let delta8 a b =
      [(LP.int 1, a <=> b)]

    (* If the~$\delta_{ab}$ originates from
       a~$\tr(T_aT_b)$, like an effective~$gg\to H\ldots$
       coupling, it makes a difference in the color
       flow basis and we must write the full expression~(6.2)
       from~\cite{Kilian:2012pz} instead. *)

    let delta8_loop a b =
      [(LP.int 1, a <=> b);
       (LP.int 1, [a => a; ?? b]);
       (LP.int 1, [?? a; b => b]);
       (LP.nc 1, [?? a; ?? b])]

    (* The following can be used for computing polarization sums
       (eventually, this could make the [Flow] module redundant).
       Note that we have $-N_C$ instead of $-1/N_C$ in the ghost
       contribution here, because
       two factors of $-1/N_C$ will be produced by [add_arrow]
       below, when contracting two ghost indices.
       Indeed, with this definition we can maintain
       [multiply [delta8 1 (-1); gluon (-1) (-2); delta8 (-2) 2]
        = delta8 1 2]. *)

    let ghost a b =
      [ (LP.nc (-1), [?? a; ?? b])]

    let gluon a b =
      delta8 a b @ ghost a b

(* \begin{dubious}
     Do we need to introduce an
     index \emph{pair} for each sextet index?  Is that all?
   \end{dubious} *)

    let sextet n m =
      [ (LP.fraction 2, [(n, 0) >=>> (m, 0); (n, 1) >=>> (m, 1)]);
        (LP.fraction 2, [(n, 0) >=>> (m, 1); (n, 1) >=>> (m, 0)]) ]

    (* FIXME: note the flipped [i] and [j]! *)
    let t a j i =
      [ (LP.int 1, [i => a; a => j]);
        (LP.int 1, [i => j; ?? a]) ]

(* Using the normalization~$\tr(T_{a}T_{b}) = \delta_{ab}$
   we find with
   \begin{equation}
   \label{eq:f=tr(TTT)'}
     \ii f_{a_1a_2a_3}
       = \tr\left(T_{a_1}\left\lbrack T_{a_2},T_{a_3}\right\rbrack\right)
       = \tr\left(T_{a_1}T_{a_2}T_{a_3}\right)
       - \tr\left(T_{a_1}T_{a_3}T_{a_2}\right)
   \end{equation}
   and
   \begin{multline}
     \tr\left(T_{a_1}T_{a_2}T_{a_3}\right)
         T_{a_1}^{i_1j_1} T_{a_2}^{i_2j_2} T_{a_3}^{i_3j_3}
       = T_{a_1}^{l_1l_2} T_{a_2}^{l_2l_3} T_{a_3}^{l_3l_1}
         T_{a_1}^{i_1j_1} T_{a_2}^{i_2j_2} T_{a_3}^{i_3j_3} = \\
         \left(                 \delta^{l_1j_1} \delta^{i_1l_2}
                - \frac{1}{N_C} \delta^{l_1l_2} \delta^{i_1j_1}\right)
         \left(                 \delta^{l_2j_2} \delta^{i_2l_3}
                - \frac{1}{N_C} \delta^{l_2l_3} \delta^{i_2j_2}\right)
         \left(                 \delta^{l_3j_3} \delta^{i_3l_1}
                - \frac{1}{N_C} \delta^{l_3l_1} \delta^{i_3j_3}\right)
   \end{multline}
   the decomposition
   \begin{equation}
   \label{eq:fTTT'}
       \ii f_{a_1a_2a_3} T_{a_1}^{i_1j_1}T_{a_2}^{i_2j_2}T_{a_3}^{i_3j_3}
     = \delta^{i_1j_2}\delta^{i_2j_3}\delta^{i_3j_1}
     - \delta^{i_1j_3}\delta^{i_3j_2}\delta^{i_2j_1}\,.
   \end{equation} *)

(*  Indeed,
\begin{verbatim}
symbol nc;
Dimension nc;
vector i1, i2, i3, j1, j2, j3;
index l1, l2, l3;

local [TT] =
   ( j1(l1) * i1(l2) - d_(l1,l2) * i1.j1 / nc )
 * ( j2(l2) * i2(l1) - d_(l2,l1) * i2.j2 / nc );

#procedure TTT(sign)
local [TTT`sign'] =
        ( j1(l1) * i1(l2) - d_(l1,l2) * i1.j1 / nc )
      * ( j2(l2) * i2(l3) - d_(l2,l3) * i2.j2 / nc )
      * ( j3(l3) * i3(l1) - d_(l3,l1) * i3.j3 / nc )
 `sign' ( j1(l1) * i1(l2) - d_(l1,l2) * i1.j1 / nc )
      * ( j3(l2) * i3(l3) - d_(l2,l3) * i3.j3 / nc )
      * ( j2(l3) * i2(l1) - d_(l3,l1) * i2.j2 / nc );
#endprocedure

#call TTT(-)
#call TTT(+)

bracket nc;
print;
.sort
.end
\end{verbatim}
gives
\begin{verbatim}
   [TT] =
       + nc^-1 * (  - i1.j1*i2.j2 )
       + i1.j2*i2.j1;

   [TTT-] =
       + i1.j2*i2.j3*i3.j1 - i1.j3*i2.j1*i3.j2;

   [TTT+] =
       + nc^-2 * (    4*i1.j1*i2.j2*i3.j3 )
       + nc^-1 * (  - 2*i1.j1*i2.j3*i3.j2
                    - 2*i1.j2*i2.j1*i3.j3
                    - 2*i1.j3*i2.j2*i3.j1 )
       + i1.j2*i2.j3*i3.j1 + i1.j3*i2.j1*i3.j2;
\end{verbatim}
*)

(* \begin{dubious}
     What about the overall sign?
   \end{dubious} *)

    let f a b c =
      [ (LP.imag ( 1), A.cycle [a; b; c]);
        (LP.imag (-1), A.cycle [a; c; b]) ]

(* Except for the signs, the symmetric combination
   \emph{is} compatible with~(6.11) in our color flow
   paper~\cite{Kilian:2012pz}.  There the signs are
   probably wrong, as they cancel in~(6.13). *)

    let d a b c =
      [ (LP.int 1, A.cycle [a; b; c]);
        (LP.int 1, A.cycle [a; c; b]);
        (LP.int 2, (a <=> b) @ [?? c]);
        (LP.int 2, (b <=> c) @ [?? a]);
        (LP.int 2, (c <=> a) @ [?? b]);
        (LP.int 2, [a => a; ?? b; ?? c]);
        (LP.int 2, [?? a; b => b; ?? c]);
        (LP.int 2, [?? a; ?? b; c => c]);
        (LP.nc 2, [?? a; ?? b; ?? c]) ]

    let incomplete tensor =
      failwith ("Color.Vertex: " ^ tensor ^ " not supported yet!")

    let experimental tensor =
      Printf.eprintf
        "Color.Vertex: %s support still experimental and untested!\n"
        tensor

    let epsilon i j k = incomplete "epsilon-tensor"
    let epsilonbar i j k = incomplete "epsilon-tensor"

   (* \begin{dubious}
        Is it enough to introduce an index \emph{pair} for
        each sextet index?
      \end{dubious} *)

    (* \begin{dubious}
         We need to find a way to make sure that we use
         particle/antiparticle assignments that a consistent
         with FeynRules.
       \end{dubious} *)

    let t6 a m n =
      experimental "t6-tensor";
      [ (LP.int ( 1), [(n, 0) >=> a; a =>> (m, 0); (n, 1) >=>> (m, 1)]);
        (LP.int (-1), [(n, 0) >=>> (m, 0); (n, 1) >=>> (m, 1); ?? a]) ]

   (* \begin{dubious}
        How much symmetrization is required?
      \end{dubious} *)

    let t6_symmetrized a m n =
      experimental "t6-tensor";
      [ (LP.int ( 1), [(n, 0) >=> a; a =>> (m, 0); (n, 1) >=>> (m, 1)]);
        (LP.int ( 1), [(n, 1) >=> a; a =>> (m, 0); (n, 0) >=>> (m, 1)]);
        (LP.int (-1), [(n, 0) >=>> (m, 0); (n, 1) >=>> (m, 1); ?? a]);
        (LP.int (-1), [(n, 1) >=>> (m, 0); (n, 0) >=>> (m, 1); ?? a]) ]

    let k6 m i j =
      experimental "k6-tensor";
      [ (LP.int 1, [(m, 0) >=> i; (m, 1) >=> j]);
        (LP.int 1, [(m, 1) >=> i; (m, 0) >=> j]) ]

    let k6bar m i j =
      experimental "k6-tensor";
      [ (LP.int 1, [i =>> (m, 0); j =>> (m, 1)]);
        (LP.int 1, [i =>> (m, 1); j =>> (m, 0)]) ]

    (* \thocwmodulesubsection{Unit Tests} *)

    module Test : Test =
      struct

        open OUnit
        module L = Algebra.Laurent

        module B = Birdtracks

        open Birdtracks
        open Birdtracks.BinOps

        let exorcise vertex =
          List.filter
            (fun (_, arrows) -> not (List.exists A.is_ghost arrows))
            vertex

        let suite_sum =
          "sum" >:::

            [ "atoms" >::
                (fun () ->
                  assert_equal_vertices
                    (two *** delta3 1 2)
                    (delta3 1 2 +++ delta3 1 2)) ]

        let suite_diff =
          "diff" >:::

            [ "atoms" >::
                (fun () ->
                  assert_equal_vertices
                    (delta3 3 4)
                    (delta3 1 2 +++ delta3 3 4 --- delta3 1 2)) ]

        let suite_times =
          "times" >:::

            [ "t1*t2=t2*t1" >::
	        (fun () ->
                  let t1 = t (-1) 1 (-2)
                  and t2 = t (-1) (-2) 2 in
	          assert_equal_vertices (t1 *** t2) (t2 *** t1));

              "tr(t1*t2)=tr(t2*t1)" >::
	        (fun () ->
                  let t1 = t 1 (-1) (-2)
                  and t2 = t 2 (-2) (-1) in
	          assert_equal_vertices (t1 *** t2) (t2 *** t1));

              "reorderings" >::
	        (fun () ->
                  let v1 = [(L.unit, [ 1 => -2; -2 => -1; -1 =>  1])]
                  and v2 = [(L.unit, [-1 =>  2;  2 => -2; -2 => -1])]
                  and v' = [(L.unit, [ 1 =>  1;  2 =>  2])] in
	          assert_equal_vertices v' (v1 *** v2)) ]

        let suite_loops =
          "loops" >:::

            [ ]

        let suite_normalization =
          "normalization" >:::

            [ "tr(t*t)" >::
	        (fun () ->
                  (* The use of [exorcise] appears to be legitimate
                     here in the color flow representation, cf.~(6.2)
                     of~\cite{Kilian:2012pz}.  *)
	          assert_equal_vertices
                    (delta8 1 2)
                    (exorcise (t 1 (-1) (-2) *** t 2 (-2) (-1))));
              "d*d" >::
                (fun () ->
                  assert_equal_vertices
                    [ (LP.ints [(2, 1); (-8,-1)], 1 <=> 2);
                      (LP.ints [(2, 0); ( 4,-2)], [1=>1; 2=>2]) ]
                    (exorcise (d 1 (-1) (-2) *** d 2 (-2) (-1)))) ]

        let commutator rep_t i_sum a b i j =
          multiply [rep_t a i i_sum; rep_t b i_sum j]
          --- multiply [rep_t b i i_sum; rep_t a i_sum j]

        let anti_commutator rep_t i_sum a b i j =
          multiply [rep_t a i i_sum; rep_t b i_sum j]
          +++ multiply [rep_t b i i_sum; rep_t a i_sum j]

        let trace3 rep_t a b c =
          rep_t a (-1) (-2) *** rep_t b (-2) (-3) *** rep_t c (-3) (-1)

        let trace3c rep_t a b c =
          third ***
            sum [trace3 rep_t a b c; trace3 rep_t b c a; trace3 rep_t c a b]

        let loop3 a b c =
          [ (LP.int 1, A.cycle (List.rev [a; b; c]));
            (LP.int 1, (a <=> b) @ [?? c]);
            (LP.int 1, (b <=> c) @ [?? a]);
            (LP.int 1, (c <=> a) @ [?? b]);
            (LP.int 1, [a => a; ?? b; ?? c]);
            (LP.int 1, [?? a; b => b; ?? c]);
            (LP.int 1, [?? a; ?? b; c => c]);
            (LP.nc 1, [?? a; ?? b; ?? c]) ]

        let suite_trace =
          "trace" >:::

            [ "tr(ttt)" >::
                (fun () ->
                  assert_equal_vertices (trace3 t 1 2 3) (loop3 1 2 3));

              "tr(ttt) cyclic 1" >::
                (fun () ->
                  assert_equal_vertices (trace3 t 1 2 3) (trace3 t 2 3 1));

              "tr(ttt) cyclic 2" >::
                (fun () ->
                  assert_equal_vertices (trace3 t 1 2 3) (trace3 t 3 1 2)) ]

        let suite_ghosts =
          "ghosts" >:::

            [ "H->gg" >::
	        (fun () ->
	          assert_equal_vertices
                    (delta8_loop 1 2)
                    (t 1 (-1) (-2) *** t 2 (-2) (-1)));

              "H->ggg f" >::
	        (fun () ->
	          assert_equal_vertices
                    (imag *** f 1 2 3)
                    (trace3c t 1 2 3 --- trace3c t 1 3 2));

              "H->ggg d" >::
	        (fun () ->
	          assert_equal_vertices
                    (d 1 2 3)
                    (trace3c t 1 2 3 +++ trace3c t 1 3 2));

              "H->ggg f'" >::
	        (fun () ->
	          assert_equal_vertices
                    (imag *** f 1 2 3)
                    (t 1 (-3) (-2) *** commutator t (-1) 2 3 (-2) (-3)));

              "H->ggg d'" >::
	        (fun () ->
	          assert_equal_vertices
                    (d 1 2 3)
                    (t 1 (-3) (-2) *** anti_commutator t (-1) 2 3 (-2) (-3)));

              "H->ggg cyclic'" >::
	        (fun () ->
                  let trace a b c =
                    t a (-3) (-2) *** commutator t (-1) b c (-2) (-3) in
	          assert_equal_vertices (trace 1 2 3) (trace 2 3 1)) ]

        (* FIXME: note the flipped [i], [j], [l], [k]! *)
        let tt j i l k =
          [ (LP.int 1, [i => l; k => j]);
            (LP.over_nc (-1), [i => j; k => l]) ]

        let ff a1 a2 a3 a4 =
          [ (LP.int (-1), A.cycle [a1; a2; a3; a4]);
            (LP.int ( 1), A.cycle [a2; a1; a3; a4]);
            (LP.int ( 1), A.cycle [a1; a2; a4; a3]);
            (LP.int (-1), A.cycle [a2; a1; a4; a3]) ]

        let tf j i a b =
          [ (LP.imag ( 1), A.chain [i; a; b; j]);
            (LP.imag (-1), A.chain [i; b; a; j]) ]

        let suite_ff =
          "f*f" >:::

            [ "1" >::
	        (fun () ->
	          assert_equal_vertices
                    (ff 1 2 3 4)
                    (f (-1) 1 2 *** f (-1) 3 4)) ]

        let suite_tf =
          "t*f" >:::

            [ "1" >::
	        (fun () ->
	          assert_equal_vertices
                    (tf 1 2 3 4)
                    (t (-1) 1 2 *** f (-1) 3 4)) ]

        let suite_tt =
          "t*t" >:::

            [ "1" >::
	        (fun () ->
	          assert_equal_vertices
                    (tt 1 2 3 4)
                    (t (-1) 1 2 *** t (-1) 3 4)) ]

        let trace_comm rep_t a b c =
          rep_t a (-3) (-2) *** commutator rep_t (-1) b c (-2) (-3)

        (* FIXME: note the flipped [b], [c]! *)
        let t8 a c b =
          imag *** f a b c

        let suite_lie =
          "Lie algebra relations" >:::

            [ "[t,t]=ift" >::
	        (fun () ->
	          assert_equal_vertices
                    (imag *** f 1 2 (-1) *** t (-1) 3 4)
                    (commutator t (-1) 1 2 3 4));

              "if = tr(t[t,t])" >::
	        (fun () ->
	          assert_equal_vertices
                    (f 1 2 3)
                    (f_of_rep t 1 2 3));

              "[f,f]=-ff" >::
	        (fun () ->
	          assert_equal_vertices
                    (minus *** f 1 2 (-1) *** f (-1) 3 4)
                    (commutator f (-1) 1 2 3 4));

              "f = tr(f[f,f])" >::
	        (fun () ->
	          assert_equal_vertices
                    (two *** nc *** f 1 2 3)
                    (trace_comm f 1 2 3));

              "[t8,t8]=ift8" >::
	        (fun () ->
	          assert_equal_vertices
                    (imag *** f 1 2 (-1) *** t8 (-1) 3 4)
                    (commutator t8 (-1) 1 2 3 4));

              "inf = tr(t8[t8,t8])" >::
	        (fun () ->
	          assert_equal_vertices
                    (two *** nc *** f 1 2 3)
                    (f_of_rep t8 1 2 3));

              "[t6,t6]=ift6" >::
	        (fun () ->
	          assert_equal_vertices
                    (imag *** f 1 2 (-1) *** t6 (-1) 3 4)
                    (commutator t6 (-1) 1 2 3 4));

              "inf = tr(t6[t6,t6])" >::
	        (fun () ->
	          assert_equal_vertices
                    (nc *** f 1 2 3)
                    (f_of_rep t6 1 2 3)) ]


        let prod3 rep_t a b c i j =
          rep_t a i (-1) *** rep_t b (-1) (-2) *** rep_t c (-2) j

        let jacobi1 rep_t a b c i j =
          (prod3 rep_t a b c i j --- prod3 rep_t a c b i j)
          --- (prod3 rep_t b c a i j --- prod3 rep_t c b a i j)

        let jacobi rep_t =
          sum [jacobi1 rep_t 1 2 3 4 5;
               jacobi1 rep_t 2 3 1 4 5;
               jacobi1 rep_t 3 1 2 4 5]

        let suite_jacobi =
          "Jacobi identities" >:::

            [ "fund." >:: (fun () -> assert_equal_vertices null (jacobi t));
              "adj." >:: (fun () -> assert_equal_vertices null (jacobi f));
              "S2" >:: (fun () -> assert_equal_vertices null (jacobi t6)) ]

        (* From \texttt{hep-ph/0611341} for $\mathrm{SU}(N)$ for
           the adjoint, symmetric and antisymmetric representations
           \begin{subequations}
             \begin{align}
               C_2(\text{adj}) &= 2N \\
               C_2(S_n) &= \frac{n(N-1)(N+n)}{N} \\
               C_2(A_n) &= \frac{n(N-n)(N+1)}{N}
             \end{align}
           \end{subequations}
           adjusted for our normalization.
           In particular
           \begin{subequations}
             \begin{align}
               C_2(\text{fund.}) = C_2(S_1) &= \frac{N^2-1}{N} \\
                                   C_2(S_2) &= \frac{2(N-1)(N+2)}{N}
                                             = 2 \frac{N^2+N-2}{N}
             \end{align}
           \end{subequations} *)

        (* $N_C-1/N_C=(N_C^2-1)/N_C$ *)
        let cf = LP.ints [(1, 1); (-1, -1)]

        (* $N_C^2-5+4/N_C^2=(N_C^2-1)(N_C^2-4)/N_C^2$ *)
        let c3f = LP.ints [(1, 2); (-5, 0); (4, -2)]

        (* $2N_C$ *)
        let ca = LP.ints [(2, 1)]

        (* $2N_C+2N_C-4/N_C=2(N_C-1)(N_C+2)/N_C$ *)
        let c6 = LP.ints [(2, 1); (2, 0); (-4, -1)]

        let casimir_tt i j =
          [(cf, i ==> j)]

        let casimir_ttt i j =
          [(c3f, i ==> j)]

        let casimir_ff a b =
          [(ca, 1 <=> 2); (LP.int (-2), [1=>1; 2=>2])]

        (* FIXME: normalization and/or symmetrization? *)
        let casimir_t6t6 i j =
          [(cf, [(i,0) >=>> (j,0); (i,1) >=>> (j,1)])]

        let casimir_t6t6_symmetrized i j =
          half ***
            [ (c6, [(i,0) >=>> (j,0); (i,1) >=>> (j,1)]);
              (c6, [(i,0) >=>> (j,1); (i,1) >=>> (j,0)]) ]

        let suite_casimir =
          "Casimir operators" >:::

            [ "t*t" >::
                (* Again, we appear to have the complex conjugate
                   (transposed) representation\ldots *)
	        (fun () ->
	          assert_equal_vertices
                    (casimir_tt 2 1)
                    (t (-1) (-2) 2 *** t (-1) 1 (-2)));

              "t*t*t" >::
	        (fun () ->
	          assert_equal_vertices
                    (casimir_ttt 2 1)
                    (d (-1) (-2) (-3) ***
                       t (-1) 1 (-4) *** t (-2) (-4) (-5) *** t (-3) (-5) 2));

              "f*f" >::
	        (fun () ->
	          assert_equal_vertices
                    (casimir_ff 1 2)
                    (minus *** f (-1) 1 (-2) *** f (-1) (-2) 2));

              "t6*t6" >::
	        (fun () ->
	          assert_equal_vertices
                    (casimir_t6t6 2 1)
                    (t6 (-1) (-2) 2 *** t6 (-1) 1 (-2))) ]

        let suite_colorsums =
          "(squared) color sums" >:::

            [ "gluon normalization" >::
	        (fun () ->
	          assert_equal_vertices
                    (delta8 1 2)
                    (delta8 1 (-1) *** gluon (-1) (-2) *** delta8 (-2) 2));

              "f*f" >::
	        (fun () ->
                  let sum_ff =
                    multiply [ f (-11) (-12) (-13);
                               f (-21) (-22) (-23);
                               gluon (-11) (-21);
                               gluon (-12) (-22);
                               gluon (-13) (-23) ]
                  and expected = ints [(2, 3); (-2, 1)] in
	          assert_equal_vertices expected sum_ff);

              "d*d" >::
	        (fun () ->
                  let sum_dd =
                    multiply [ d (-11) (-12) (-13);
                               d (-21) (-22) (-23);
                               gluon (-11) (-21);
                               gluon (-12) (-22);
                               gluon (-13) (-23) ]
                  and expected = ints [(2, 3); (-10, 1); (8, -1)] in
	          assert_equal_vertices expected sum_dd);

              "f*d" >::
	        (fun () ->
                  let sum_fd =
                    multiply [ f (-11) (-12) (-13);
                               d (-21) (-22) (-23);
                               gluon (-11) (-21);
                               gluon (-12) (-22);
                               gluon (-13) (-23) ] in
	          assert_equal_vertices null sum_fd);

              "Hgg" >::
	        (fun () ->
                  let sum_hgg =
                    multiply [ delta8_loop (-11) (-12);
                               delta8_loop (-21) (-22);
                               gluon (-11) (-21);
                               gluon (-12) (-22) ]
                  and expected = ints [(1, 2); (-1, 0)] in
	          assert_equal_vertices expected sum_hgg) ]

        let suite =
          "Color.SU3" >:::
	    [suite_sum;
	     suite_diff;
	     suite_times;
	     suite_normalization;
	     suite_ghosts;
	     suite_loops;
	     suite_trace;
	     suite_ff;
	     suite_tf;
	     suite_tt;
             suite_lie;
             suite_jacobi;
	     suite_casimir;
             suite_colorsums]

      end

  end

module U3 : SU3 =
  struct

    module A = Arrow
    open Arrow.BinOps

    module B = Birdtracks
    type t = B.t
    let to_string = B.to_string
    let pp = B.pp
    let trivial = B.trivial
    let is_null = B.is_null
    let null = B.null
    let unit = B.unit
    let const = B.const
    let two = B.two
    let half = B.half
    let third = B.third
    let nc = B.imag
    let minus = B.minus
    let imag = B.imag
    let ints = B.ints
    let sum = B.sum
    let diff = B.diff
    let scale = B.scale
    let times = B.times
    let multiply = B.multiply
    let map = B.map
    let fuse = B.fuse
    let f_of_rep = B.f_of_rep
    let d_of_rep = B.d_of_rep
    module BinOps = B.BinOps

    let delta3 i j =
      [(LP.int 1, i ==> j)]

    let delta8 a b =
      [(LP.int 1, a <=> b)]

    let delta8_loop = delta8

    let gluon a b =
      delta8 a b

(* \begin{dubious}
     Do we need to introduce an
     index \emph{pair} for each sextet index?  Is that all?
   \end{dubious} *)

    let sextet n m =
      [ (LP.fraction 2, [(n, 0) >=>> (m, 0); (n, 1) >=>> (m, 1)]);
        (LP.fraction 2, [(n, 0) >=>> (m, 1); (n, 1) >=>> (m, 0)]) ]

    let t a j i =
      [ (LP.int 1, [i => a; a => j]) ]

    let f a b c =
      [ (LP.imag ( 1), A.cycle [a; b; c]);
        (LP.imag (-1), A.cycle [a; c; b]) ]

    let d a b c =
      [ (LP.int 1, A.cycle [a; b; c]);
        (LP.int 1, A.cycle [a; c; b]) ]

    let incomplete tensor =
      failwith ("Color.Vertex: " ^ tensor ^ " not supported yet!")

    let experimental tensor =
      Printf.eprintf
        "Color.Vertex: %s support still experimental and untested!\n"
        tensor

    let epsilon i j k = incomplete "epsilon-tensor"
    let epsilonbar i j k = incomplete "epsilon-tensor"

    let t6 a m n =
      experimental "t6-tensor";
      [ (LP.int ( 1), [(n, 0) >=> a; a =>> (m, 0); (n, 1) >=>> (m, 1)]) ]

   (* \begin{dubious}
        How much symmetrization is required?
      \end{dubious} *)

    let t6_symmetrized a m n =
      experimental "t6-tensor";
      [ (LP.int ( 1), [(n, 0) >=> a; a =>> (m, 0); (n, 1) >=>> (m, 1)]);
        (LP.int ( 1), [(n, 1) >=> a; a =>> (m, 0); (n, 0) >=>> (m, 1)]) ]

    let k6 m i j =
      experimental "k6-tensor";
      [ (LP.int 1, [(m, 0) >=> i; (m, 1) >=> j]);
        (LP.int 1, [(m, 1) >=> i; (m, 0) >=> j]) ]

    let k6bar m i j =
      experimental "k6-tensor";
      [ (LP.int 1, [i =>> (m, 0); j =>> (m, 1)]);
        (LP.int 1, [i =>> (m, 1); j =>> (m, 0)]) ]

    (* \thocwmodulesubsection{Unit Tests} *)

    module Test : Test =
      struct

        open OUnit
        open Birdtracks
        open BinOps

        let suite_lie =
          "Lie algebra relations" >:::

            [ "if = tr(t[t,t])" >::
	        (fun () -> assert_equal_vertices (f 1 2 3) (f_of_rep t 1 2 3)) ]

        (* $N_C=N_C^2/N_C$ *)
        let cf = LP.ints [(1, 1)]

        let casimir_tt i j =
          [(cf, i ==> j)]

        let suite_casimir =
          "Casimir operators" >:::

            [ "t*t" >::
	        (fun () ->
	          assert_equal_vertices
                    (casimir_tt 2 1)
                    (t (-1) (-2) 2 *** t (-1) 1 (-2))) ]

        let suite =
          "Color.U3" >:::
	    [suite_lie;
             suite_casimir]

      end

  end

module Vertex = SU3
