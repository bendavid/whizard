(* PArray.mli --

   Copyright (C) 2022-2025 by

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

(* O'Caml arrays ['a array] are a special case of maps
   [int -> 'a] from a subset of the integers into a set,
   where the subset is contiguous and starts with 0.

   In O'Caml, updating element of an ['a array] is not pure,
   since the array is updated in place and all references
   to the array element in other parts of the code are affected.
   This is efficient, but complicates backtracking.

   A ['a PArray.t], on the other hand is updated with
   pure functions, keeping the original array in place. *)

(* The type of persistent array. *)
type 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val map : ('a -> 'b) -> 'a t -> 'b t
val add : int -> 'a -> 'a t -> 'a t
val remove : int -> 'a t -> 'a t
val get_opt : int -> 'a t -> 'a option

(* Create an array from a list of pairs of index and value.  Note that
   we assume the array indices to start from~$0$.  *)
val of_pairs : (int * 'a) list -> 'a t
val to_pairs : 'a t -> (int * 'a) list

(* Compute a list of all entries of the array, starting from the
   index~$0$.  Entries [a] are represented by [Some a] and missing
   entries are represented by [None].
   For example [to_option_list [of_pairs [(2,42)]] evaluates
   to [[None; Some 42]]. *)
val to_option_list : 'a t -> 'a option list

(* For debugging: *)
val to_string : ('a -> string) -> 'a t -> string

(* Order: *)
val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

(* [take_one project_opt parray] tries to
   find one element in [parray] that is mapped to [None] by [project_opt].
   Returns [Nothing projected_parray] if nothing is found and
   [Unique (key, value, projected_parray)] if there is exactly one
   match where [projected_parray] is [parray] with the binding for [key]
   removed and the function [project_opt] has been applied (with the [Some]
   stripped, of course).
   Returns [Multiple (key, value, parray')] if there are multiple
   matches, where [parray'] is [parray] with the binding for [key]
   removed.  In both cases, [key] is one of the matching keys and [value]
   the associated binding.  The rationale is that we can use
   [take_one] to remove bindings from a map until we can
   replace the type of the values by a simpler type,
   e.\,g.~by unboxing. *)

type ('a, 'b) taken = private
  | Nothing of 'b t
  | Single of int * 'a * 'b t
  | Multiple of int * 'a * 'a t

val take_one : (int -> 'a -> 'b option) -> 'a t -> ('a, 'b) taken

module Test : sig val suite : OUnit.test end
