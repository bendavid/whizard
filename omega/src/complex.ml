(* complex.ml --

   Copyright (C) 1999-2023 by

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

module type T =
    sig
      type t

      val null : t
      val one : t

      val real : t -> float
      val imag : t -> float

      val conj : t -> t
      val neg : t -> t
      val inv : t -> t

      val add : t -> t -> t
      val sub : t -> t -> t
      val mul : t -> t -> t
      val div : t -> t -> t

      val abs : t -> float
      val arg : t -> float

      val sqrt : t -> t
      val exp : t -> t
      val log : t -> t

      val of_float2 : float -> float -> t
      val of_int2 : int -> int -> t
      val to_float2 : t -> float * float
      val to_int2 : t -> int * int

      val of_float : float -> t
      val of_int : int -> t
      val to_float : t -> float
      val to_int : t -> int

      val to_string : t -> string
      val of_string : 'a -> 'b
    end

(* The hairier formulae are ``inspired'' by \cite{PTVF92}. *)

module Dense =
  struct

    type t = { re : float; im : float }
    let null = { re = 0.0; im = 0.0 }
    let one = { re = 1.0; im = 0.0 }

    let real z = z.re
    let imag z = z.im
    let conj z = {re = z.re; im = ~-. (z.im) }

    let neg z = {re = ~-. (z.re); im = ~-. (z.im) }
    let add z1 z2 = {re = z1.re +. z2.re; im = z1.im +. z2.im }
    let sub z1 z2 = {re = z1.re -. z2.re; im = z1.im -. z2.im }

(* Save one multiplication with respect to the standard formula
   \begin{equation}
     (x+iy)(u+iv) = \lbrack xu-yv\rbrack + i\lbrack(x+u)(y+v)-xu-yv\rbrack\,
   \end{equation}
   at the expense of one addition and two subtractions. *)

    let mul z1 z2 =
      let re12 = z1.re *. z2.re
      and im12 = z1.im *. z2.im in
      { re = re12 -. im12;
        im = (z1.re +. z1.im) *. (z2.re +. z2.im) -. re12 -. im12 }

(* \begin{equation}
     \frac{x+iy}{u+iv} =
	\begin{cases}
	  \frac{\lbrack x+y(v/u)\rbrack + i\lbrack y-x(v/u)\rbrack}{u+v(v/u)}
				   & \text{for}\;\; |u|\ge|v| \\
	  \frac{\lbrack x(u/v)+y\rbrack + i\lbrack y(u/v)-x\rbrack}{u(u/v+v)}
				   & \text{for}\;\; |u|<|v|
	\end{cases}
   \end{equation} *)
    let div z1 z2 =
      if abs_float z2.re >= abs_float z2.im then
        let r = z2.im /. z2.re in
        let den = z2.re +. r *. z2.im in
        { re = (z1.re +. r *. z1.im) /. den;
          im = (z1.im -. r *. z1.re) /. den }
      else
        let r = z2.re /. z2.im in
        let den = z2.im +. r *. z2.re in
        { re = (r *. z1.re +. z1.im) /. den;
          im = (r *. z1.im -. z1.re) /. den }

    let inv = div one

(* \begin{equation}
     |x+iy| =
	\begin{cases}
	   |x|\sqrt{1+(y/x)^2} & \text{for}\;\; |x|\ge|y| \\
	   |y|\sqrt{1+(x/y)^2} & \text{for}\;\; |x|<|y|
	\end{cases}
   \end{equation} *)
    let abs z =
      let absr = abs_float z.re
      and absi = abs_float z.im in
      if absr = 0.0 then
        absi
      else if absi = 0.0 then
        absr
      else if absr > absi then
        let q = absi /. absr in
        absr *. sqrt (1.0 +. q *. q)
      else
        let q = absr /. absi in
        absi *. sqrt (1.0 +. q *. q)

    let arg z = atan2 z.im z.re

(* Square roots are trickier:
   \begin{equation}
   \label{eq:cont}
     \sqrt{x+iy} =
	\begin{cases}
	  0                                & \text{for}\;\; w=0 \\
	  w + i \left(\frac{y}{2w}\right)  & \text{for}\;\; w\not=0, x\ge0 \\
	  \left(\frac{|y|}{2w}\right) + iw & \text{for}\;\; w\not=0, x<0, y\ge0 \\
	  \left(\frac{|y|}{2w}\right) - iw & \text{for}\;\; w\not=0, x<0, y<0
	\end{cases}
   \end{equation}
   where
   \begin{equation}
     w =
	\begin{cases}
	  0                                & \text{for}\;\; x=y=0 \\
	  \sqrt{|x|} \sqrt{\frac{1+\sqrt{1+(y/x)^2}}{2}} & \text{for}\;\; |x|\ge|y| \\
	  \sqrt{|y|} \sqrt{\frac{|x/y|+\sqrt{1+(x/y)^2}}{2}} & \text{for}\;\; |x|<|y|
	\end{cases}\,.
   \end{equation}
   Equation~(\ref{eq:cont}) is encoded in [cont w]. *)
    let sqrt z =
      if z.re = 0.0 && z.im = 0.0 then
        { re = 0.0; im = 0.0 }
      else
        let absr = abs_float z.re
        and absi = abs_float z.im
        and cont w =
          if z.re >= 0.0 then
            { re = w; im = z.im /. (2. *. w) }
          else
            let im = if z.im >= 0.0 then w else ~-. w in
            { re = z.im /. (2. *. im); im = im }
        in
        if absr >= absi then
          let q = absi /. absr in
          cont ((sqrt absr) *. sqrt (0.5 *. (1.0 +. sqrt (1.0 +. q *. q))))
        else
          let q = absr /. absi in
          cont ((sqrt absi) *. sqrt (0.5 *. (q +. sqrt (1.0 +. q *. q))))

    let exp z =
      let er = exp z.re in
      { re = er *. (cos z.im); im = er *. (sin z.im) } 

    let log z = { re = log (abs z); im = arg z } 

    let of_float2 r i = { re = r; im = i }
    let of_int2 r i = { re = float r; im = float i }
    let to_float2 z = (z.re, z.im)
    let to_int2 z = (truncate z.re, truncate z.im)
    let of_float r = { re = r; im = 0.0 }
    let of_int r = { re = float r; im = 0.0 }
    let to_float z = z.re
    let to_int z = truncate z.re

    let to_string z =
      if z.re <> 0.0 && z.im <> 0.0 then
        Printf.sprintf "%g+%gi" (* starting from 3.04: ["%g%+gi"] *) z.re z.im
      else if z.re <> 0.0 then
        Printf.sprintf "%g" z.re
      else if z.im <> 0.0 then
        Printf.sprintf "%gi" z.im
      else
        "0"

    let of_string z = failwith "Complex.of_string not implemented yet!"

  end

(* \thocwmodulesection{Sparse Representation} *)

(* If the numbers are very likely to be either purely real or imaginary,
   a different representation can reduce the load from the floating point
   unit. *)

module Sparse =
  struct
    module C = Dense

    type t =
      | Real of float
      | Imag of float
      | Complex of C.t

    let null = Real 0.0
    let one = Real 1.0

    let real = function
      | Real x -> x
      | Imag y -> 0.0
      | Complex z -> C.real z

    let imag = function
      | Real x -> 0.0
      | Imag y -> y
      | Complex z -> C.imag z

  end

(* \thocwmodulesection{Suggesting A Default} *)

(* There's no real choice here (yet) \ldots *)
module Default = Dense

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
