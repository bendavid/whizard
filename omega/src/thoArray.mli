(* $Id: thoArray.mli 4926 2013-12-04 12:35:06Z jr_reuter $

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

(* Compressed arrays, i.\,e.~arrays with only unique elements and
   an embedding that allows to recover the original array.
   NB: in the current implementation, compressing saves space,
   if \emph{and only if} objects of type ['a] require more storage
   than integers.  The main use of ['a compressed] is \emph{not} for
   saving space, anyway, but for avoiding the repetition of hard
   calculations. *)
type 'a compressed
val uniq : 'a compressed -> 'a array
val embedding : 'a compressed -> int array

(* These two are inverses of each other: *)
val compress : 'a array -> 'a compressed
val uncompress : 'a compressed -> 'a array

(* One can play the same game for matrices. *)
type 'a compressed2
val uniq2 : 'a compressed2 -> 'a array array
val embedding1 : 'a compressed2 -> int array
val embedding2 : 'a compressed2 -> int array

(* Again, these two are inverses of each other: *)
val compress2 : 'a array array -> 'a compressed2
val uncompress2 : 'a compressed2 -> 'a array array


(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
