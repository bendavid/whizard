(* $Id: color.mli 4926 2013-12-04 12:35:06Z jr_reuter $

   Copyright (C) 1999-2014 by

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

(* Color is not necessarily the~$\textrm{SU}(3)$ of QCD.  Conceptually,
   it can be any \emph{unbroken} symmetry (\emph{broken} symmetries correspond
   to [Model.flavor]).  In order to keep the group theory simple, we confine
   ourselves to the fundamental and adjoint representation
   of a single~$\textrm{SU}(N_C)$ for the moment.  Therefore,
   particles are either color singlets or live in the defining
   representation of $\textrm{SU}(N_C)$: [SUN]$(|N_C|)$, its conjugate
   [SUN]$(-|N_C|)$ or in the adjoint representation of
   $\textrm{SU}(N_C)$: [AdjSUN]$(N_C)$. *)

type t = Singlet | SUN of int | AdjSUN of int

val conjugate : t -> t
val compare : t -> t -> int

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

(* A factor is a list of powers
   \begin{equation}
     \sum_{i}
        \left( \frac{\ocwlowerid{num}_i}{\ocwlowerid{den}_i}
                  \right)^{\ocwlowerid{power}_i}
   \end{equation} *)
    type power = { num : int; den : int; power : int }
    type factor = power list

    val factor : t -> t -> factor
    val zero : factor

  end

module Flow : Flow

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
