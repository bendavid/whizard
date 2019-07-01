(* color.ml --

   Copyright (C) 1999-2016 by

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

module Flow (* [: Flow] *) = 
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
