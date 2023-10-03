(* NEList.mli --

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

(* Since O'Caml 3.11, we can use private type abbreviation
   to enforce invariants without sacrificing any performance. *)

(* Once we have decided on an interface that avoids any partial
   functions, most of the implementation will be just an indirection
   to the standard library module. *)

(* A nonempty list ['a t] is represented as a ``normal''
   ['a list] \ldots *)
type 'a t = private 'a list

(* \ldots, but there is no way to construct an empty list,
   since the constructors require at least one element: *)
val make : 'a -> 'a list -> 'a t
val singleton : 'a -> 'a t
val cons : 'a -> 'a t -> 'a t

(* [to_list l] is the same as [l :> elt list], without having to
   specify the element type [elt].  The compiler should inline this. *)
val to_list : 'a t -> 'a list

(* [hd] never fails.  We can also have a [tl] that never fails,
   if we allow it to return an ``normal'' list. *)
val hd : 'a t -> 'a
val tl : 'a t -> 'a list
val tl_opt : 'a t -> 'a t option

(* The inverse of [cons] (uncurried): [snoc l = (hd l, tl l)] and
   [snoc_opt l = (hd l, tl_opt l)], but a little bit more efficient,
   since the list is deconstructed only once. *)
val snoc : 'a t -> 'a * 'a list
val snoc_opt : 'a t -> 'a * 'a t option

val map : ('a -> 'b) -> 'a t -> 'b t
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val sort : ('a -> 'a -> int) -> 'a t -> 'a t

