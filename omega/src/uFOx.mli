(* vertex.mli --

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

module Expr :
  sig
    type t
    val of_string : string -> t
    val of_strings : string list -> t
  end

module Index :
  sig
    val free : (int * 'r) list -> (int * 'r) list
    val summation : (int * 'r) list -> (int * 'r) list
    val classes_to_string : ('r -> string) -> (int * 'r) list -> string
  end

module Q : Algebra.Rational

module type Tensor =
  sig
    type atom
    type t = (atom list * Q.t) list
    val of_expr : UFOx_syntax.expr -> t
    val of_string : string -> t
    val of_strings : string list -> t
    val to_string : t -> string
    type r
    val classify_indices : t -> (int * r) list 
    val rep_to_string : r -> string
    val rep_of_int : int -> r
    val rep_conjugate : r -> r
    val rep_trivial : r -> bool
    type r_omega
    val omega : r -> r_omega
  end

module type Atom =
  sig
    type t
    val of_expr : string -> UFOx_syntax.expr list -> t
    val to_string : t -> string
    type r
    val classify_indices : t list -> (int * r) list
    val rep_to_string : r -> string
    val rep_of_int : int -> r
    val rep_conjugate : r -> r
    val rep_trivial : r -> bool
    type r_omega
    val omega : r -> r_omega
  end

module type Lorentz_Atom =
  sig
    type t = private
      | C of int * int
      | Epsilon of int * int * int * int
      | Gamma of int * int * int
      | Gamma5 of int * int
      | Identity of int * int
      | Metric of int * int
      | P of int * int
      | ProjP of int * int
      | ProjM of int * int
      | Sigma of int * int * int * int
  end

module Lorentz_Atom : Lorentz_Atom

module Lorentz : Tensor
  with type atom = Lorentz_Atom.t and type r_omega = Coupling.lorentz

module type Color_Atom =
  sig
    type t = private
      | Identity of int * int
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
