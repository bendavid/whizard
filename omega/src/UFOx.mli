(* vertex.mli --

   Copyright (C) 1999-2019 by

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

module Expr :
  sig
    type t
    val of_string : string -> t
    val of_strings : string list -> t
    val substitute : string -> t -> t -> t
    val rename : (string * string) list -> t -> t
    val half : string -> t
    val variables : t -> Sets.String_Caseless.t
    val functions : t -> Sets.String_Caseless.t
  end

module type Index =
  sig
    (* Indices are represented by a pair [int * 'r], where
       ['r] denotes the representation the index belongs to.  *)

    (* [free indices] returns all free indices in the
       list [indices], i.\,e.~all positive indices. *)
    val free : (int * 'r) list -> (int * 'r) list

    (* [summation indices] returns all summation indices in the
       list [indices], i.\,e.~all negative indices.  *)
    val summation : (int * 'r) list -> (int * 'r) list

    val classes_to_string : ('r -> string) -> (int * 'r) list -> string

  end

module Index : Index

module type Tensor =
  sig

    type atom

    (* A tensor is linear combination of products of [atom]s
       with rational coefficients. *)
    type t = (atom list * Algebra.Q.t) list

    (* We might need to replace atoms if the syntax is not
       context free. *)
    val map_atoms : (atom -> atom) -> t -> t

    (* We need to rename indices to implement permutations. *)
    val map_indices : (int -> int) -> t -> t

    (* Parsing and unparsing.  Lists of [string]s are
       interpreted as sums. *)
    val of_expr : UFOx_syntax.expr -> t
    val of_string : string -> t
    val of_strings : string list -> t
    val to_string : t -> string

    (* The supported representations. *)
    type r
    val classify_indices : t -> (int * r) list 
    val rep_to_string : r -> string
    val rep_to_string_whizard : r -> string
    val rep_of_int : bool -> int -> r
    val rep_conjugate : r -> r
    val rep_trivial : r -> bool

    (* There is not a 1-to-1 mapping between the representations
       in the model files and the representations used by O'Mega,
       e.\,g.~in [Coupling.lorentz].  We might need to use heuristics. *)
    type r_omega
    val omega : r -> r_omega

  end

module type Atom =
  sig
    type t
    val map_indices : (int -> int) -> t -> t
    val of_expr : string -> UFOx_syntax.expr list -> t
    val to_string : t -> string
    type r
    val classify_indices : t list -> (int * r) list
    val rep_to_string : r -> string
    val rep_to_string_whizard : r -> string
    val rep_of_int : bool -> int -> r
    val rep_conjugate : r -> r
    val rep_trivial : r -> bool
    type r_omega
    val omega : r -> r_omega
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

    type t = private
      | Dirac of dirac
      | Vector of vector

    val map_indices_vector : (int -> int) -> vector -> vector

  end

module Lorentz_Atom : Lorentz_Atom

module Lorentz : Tensor
  with type atom = Lorentz_Atom.t and type r_omega = Coupling.lorentz

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

module Color_Atom : Color_Atom

module Color : Tensor
  with type atom = Color_Atom.t and type r_omega = Color.t

module Value :
  sig
    type t
    val of_expr : Expr.t -> t
    val to_string : t -> string
    val to_coupling : (string -> 'b) -> t -> 'b Coupling.expr
  end

module type Test =
  sig
    val example : unit -> unit
    val suite : OUnit.test
  end
