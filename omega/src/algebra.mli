(* algebra.mli --

   Copyright (C) 1999-2024 by

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

(* \thocwmodulesection{Coefficients} *)

(* For our algebra, we need coefficient rings with addition, subtraction,
   multiplication and the corresponding neutral elements. *)

module type CRing =
  sig
    type t

    (* [add null x = x = add x null] *)
    val null : t
    val is_null : t -> bool
    val add : t -> t -> t

    (* [neg x = sub null x] and [sub x y = add x (neg y)] *)
    val neg : t -> t
    val sub : t -> t -> t

    (* [mul unit x = x = mul x unit] *)
    val unit : t
    val is_unit : t -> bool
    val mul : t -> t -> t

    (* Equality: *)
    val equal : t -> t -> bool

  end

(* Rational numbers provide a particularly important example and they come
   with a partial inverse: *)

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
    (* Convenience: $n \mapsto n/1$ and $n \mapsto 1/n$ *)
    val int : int -> t
    val fraction : int -> t
    (* Order *)
    val compare : t -> t -> int
    (* Tracing, debugging, toplevel and unit testing *)
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

module Small_Rational : Rational
module Q : Rational

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

    (* Convenience: real rationals and integers, *)
    val rational : q -> t
    val int : int -> t

    (* $n \to 1/n$ *)
    val fraction : int -> t

    (* $n \to n\ii$ *)
    val imag : int -> t

    (* Order *)
    val compare : t -> t -> int

    (* Tracing, debugging, toplevel and unit testing *)
    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
    module Test : Test

  end

module QComplex : functor (Q' : Rational) -> QComplex with type q = Q'.t
module QC : QComplex with type q = Q.t

(* \thocwmodulesection{Laurent Polynomials} *)

(* Polynomials, including negative powers, in one variable.
   In our applications, the variable~$x$ will often be~$N_C$,
   the number of colors
   \begin{equation}
     \sum_n c_n N_C^n
   \end{equation} *)
module type Laurent =
  sig

    include CRing

    (* The type of coefficients.  In the implementation below,
       it is [QComplex.t]: complex numbers with rational real
       and imaginary parts. *)
    type c

    (* [atom c n] constructs a term $c x^n$, where $x$ denotes
       the variable. *)
    val atom : c -> int -> t

    (* Shortcut: [const c = atom c 0] *)
    val const : c -> t

    (* Elementary arithmetic *)
    val scale : c -> t -> t
    val sum : t list -> t
    val product : t list -> t
    val pow : t -> int -> t

    (* [log]$(cN_C^n)$ returns [Some]$(c,n)$.  For other terms,
       [log] returns [None]. *)
    val log : t -> (c * int) option

    (* return the corresponding list of coefficients and descending powers *)
    val to_list : t -> (c * int) list

    (* [eval c p] evaluates the polynomial [p] by substituting
       the constant [c] for the variable. *)
    val eval : c -> t -> c

    (* A total ordering.  Does not correspond to any mathematical order. *)
    val compare : t -> t -> int

    (* Provide some convenience functions for constructing coefficients
       from integers and rationals. *)

    (* Rationals coefficients (without imaginary part!)
       $\left\{(q_i,n_i)\right\}_n \mapsto \sum_i q_i x^{n_i}$ *)
    val rationals : (Q.t * int) list -> t

    (* Integer coefficients
       $\left\{(k_i,n_i)\right\}_n \mapsto \sum_i k_i x^{n_i}$ *)
    val ints : (int * int) list -> t

    (* For convenience, some special cases.  Starting with injections *)
    val rational : Q.t -> t
    val int : int -> t

    (* $k\mapsto 1/k = k^{-1}$ *)
    val fraction : int -> t

    (* $k\mapsto k \ii$ *)
    val imag : int -> t

    (* $k\mapsto k x$ *)
    val nc : int -> t

    (* $k\mapsto k / x = k x^{-1}$ *)
    val over_nc : int -> t

    (* Tracing, debugging, toplevel and unit testing *)
    val to_string : string -> t -> string
    val pp : Format.formatter -> t -> unit
    module Test : Test

  end

(* \begin{dubious}
     Could (should?) be functorialized over [QComplex].
     We had to wait until we upgraded our O'Caml requirements to 4.02,
     but that has been done.
   \end{dubious} *)

module Laurent : Laurent with type c = QC.t

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

    (* The derivative of a term is \emph{not} a term,
       but a sum of terms instead:
       \begin{equation}
           D (f_1^{p_1}f_2^{p_2}\cdots f_n^{p_n}) =
             \sum_i (Df_i) p_i f_1^{p_1}f_2^{p_2}\cdots f_i^{p_i-1} \cdots f_n^{p_n}
       \end{equation}
       The function returns the sum as a list of triples
       $(Df_i,p_i, f_1^{p_1}f_2^{p_2}\cdots f_i^{p_i-1} \cdots f_n^{p_n})$.
       Summing the terms is left to the calling module and the $Df_i$ are
       \emph{not} guaranteed to be different.
       NB: The function implementating the inner derivative, is supposed to
       return~[Some]~$Df_i$ and [None], iff~$Df_i$ vanishes. *)
    val derive : ('a -> 'b option) -> 'a t -> ('b * int * 'a t) list

    (* convenience function *)
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

    (* Again
       \begin{equation}
           D (f_1^{p_1}f_2^{p_2}\cdots f_n^{p_n}) =
             \sum_i (Df_i) p_i f_1^{p_1}f_2^{p_2}\cdots f_i^{p_i-1} \cdots f_n^{p_n}
       \end{equation}
       but, iff~$Df_i$ can be identified with a~$f'$, we know how to perform
       the sum. *)

    val derive_inner : ('a -> 'a t) -> 'a t -> 'a t (* this? *)
    val derive_inner' : ('a -> 'a t option) -> 'a t -> 'a t (* or that? *)

(* Below, we will need partial derivatives that lead out of the ring:
   [derive_outer derive_atom term] returns a list of partial derivatives
   ['b] with non-zero coefficients ['a t]: *)
    val derive_outer : ('a -> 'b option) -> 'a t -> ('b * 'a t) list

    (* convenience functions *)
    val sum : 'a t list -> 'a t
    val product : 'a t list -> 'a t

(* The list of all generators appearing in an expression: *)
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

(* A partial derivative w.\,r.\,t.~a vector maps from a coefficient ring to
   the dual vector space.  *)
    val partial : ('c -> ('a, 'c) t) -> 'c C.t -> ('a, 'c) t

(* A linear combination of vectors
   \begin{equation}
     \text{[linear]} \lbrack (v_1, c_1); (v_2, c_2); \ldots; (v_n, c_n)\rbrack
        = \sum_{i=1}^{n} c_i\cdot v_i
   \end{equation} *)
    val linear : (('a, 'c) t * 'c C.t) list -> ('a, 'c) t

(* Some convenience functions *)
    val map : ('a -> 'c C.t -> ('b, 'd) t) -> ('a, 'c) t ->  ('b, 'd) t
    val sum : ('a, 'c) t list -> ('a, 'c) t

(* The list of all generators and the list of all generators of coefficients
   appearing in an expression: *)
    val atoms : ('a, 'c) t -> 'a list * 'c list

    val to_string : ('a -> string) -> ('c -> string) -> ('a, 'c) t -> string

  end

module Term : Term

module Make_Ring (C : Rational) (T : Term) : Ring
module Make_Linear (C : Ring) : Linear with module C = C
