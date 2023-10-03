(* NList.mli --

   Copyright (C) 2022-2023 by

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

(* \textit{This is inspired by an example posted on github by Izaak Meckler
   that in turn appears to be based on ideas well known in the Haskell community.} *)

(* These types are just Peano numerals ['n] used as indices
   for [('n, 'a) t].  [z] encodes 0 and ['a s] the successor. *)
type z
type 'a s

(* A [('n, 'a) t] is a list of ['a] of length ['n] with ['n]
   encoded as a church numeral and must not be too large! *)
type ('n, 'a) t

(* Constructors. *)
val empty : (z, 'a) t
val cons : 'a -> ('n, 'a) t -> ('n s, 'a) t

(* Deconstructors. Note that they cannot be applied to the empty list. *)
val hd : ('n s, 'a) t -> 'a
val tl : ('n s, 'a) t -> ('n, 'a) t

(* Turn the a list with typed length into an ordinary list.
   Note also, that we can not implement the inverse function
   [of_list : 'a list -> ('n, 'a t)], because in that case the
   type ['n] depends on the list and is \emph{not} known at
   compile time. *)
val to_list : ('n, 'a) t -> 'a list

(* The usual suspects. *)
val map : ('a -> 'b) -> ('n, 'a) t -> ('n, 'b) t
val fold_right : ('a -> 'b -> 'b) -> ('n, 'a) t -> 'b -> 'b

(* A version of [append] is complicated, since we need to compute
   the sum of the lengths in the type system.  It can be done by
   introducing additional wrappers, but the result is difficult to
   deconstruct and we don't need it for our applications.
   The usual implementation of [rev] will also not work, because we
   need again to maintain the sum of the lengths as an invariant.
   Simple successor relationships are not enough. *)

(* On the other hand, [map2], [fold_right2] etc.{} can be
   implemented easily.  Here, the type shines, because it can
   avoid the [Invalid_argument] exception. *)
val map2 : ('a -> 'b -> 'c) -> ('n, 'a) t -> ('n, 'b) t -> ('n, 'c) t

(* The algorithm is not suitable for long lists, but we expect the
   lists to be very short anyway. *)
val sort : ('a -> 'a -> int) -> ('n, 'a) t -> ('n, 'a) t

