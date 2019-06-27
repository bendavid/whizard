(* $Id: process.mli 2276 2010-04-09 17:15:14Z ohl $

   Copyright (C) 1999-2010 by

       Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>

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

    type flavor

(* \begin{dubious}
     Eventually this should become an abstract type:
   \end{dubious} *)
    type t = flavor list * flavor list

    val incoming : t -> flavor list
    val outgoing : t -> flavor list

(* [parse_decay s] decodes a decay description ["a -> b c ..."], where
    each word is split into a bag of flavors separated by [':']s. *)
    type decay
    val parse_decay : string -> decay
    val expand_decays : decay list -> t list

(* [parse_scattering s] decodes a scattering description ["a b -> c d ..."],
    where each word is split into a bag of flavors separated by [':']s. *)
    type scattering
    val parse_scattering : string -> scattering
    val expand_scatterings : scattering list -> t list

(* [parse_process s] decodes process descriptions
   \begin{subequations}
   \begin{align}
     \text{\texttt{"a b c d"}} &\Rightarrow \text{[Any [a; b; c; d]]} \\
     \text{\texttt{"a -> b c d"}} &\Rightarrow \text{[Decay (a, [b; c; d])]} \\
     \text{\texttt{"a b -> c d"}} &\Rightarrow \text{[Scattering (a, b, [c; d])]}
   \end{align}
   \end{subequations}
   where each word is split into a bag of flavors separated by `\texttt{:}'s. *)
    type any
    type process = Any of any | Decay of decay | Scattering of scattering
    val parse_process : string -> process

    val remove_duplicate_final_states : int list list -> t list -> t list

    val diff : t list -> t list -> t list

  end

module Make (M : Model.T) : T with type flavor = M.flavor

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
