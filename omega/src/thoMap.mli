(* thoMap.mli --

   Copyright (C) 2023- by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>

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

(* \thocwmodulesection{Maps to Sets} *)

module type Buckets =
  sig

    type t
    type key
    type element

    (* The empty map. *)
    val empty : t

    (* Add the [element] to the set indexed by [key].  If there is no
       such set, create it. *)
    val add : key -> element -> t -> t

    (* Return the sets as lists of [elements], indexed by their [key]. *)
    val to_lists : t -> (key * element list) list

    (* The prototypical application of this module is
       group all [element]s with matching [key]s.
       If all [element]s for a given [key] are different, [factorize] is just
       a more efficient implementation of [ThoList.factorize] on
       page~\pageref{ThoList.factorize}, but the latter keeps duplicate
       [element]s for a [key], while this [factorize] keeps only one copy
       for each [key]. *)
    val factorize : (key * element) list -> (key * element list) list

    (* [factorize_batches] is the composition of [factorize] and [List.concat],
       but doesn't build the intermediate list. *)
    val factorize_batches : (key * element) list list -> (key * element list) list

  end

module Buckets (Key : Map.OrderedType) (Element : Set.OrderedType) : Buckets
       with type key = Key.t and type element = Element.t

module Test : sig val suite : OUnit.test end
