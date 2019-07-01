(* $Id: rCS.mli 7444 2016-02-17 15:37:20Z jr_reuter $

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

(* This is a very simple library for exporting and accessing
   \texttt{RCS} and \texttt{CVS} revision control information.
   In addition, module names and short descriptions are supported
   as well.

   If multiple applications are constructed by functors,
   the functions in this module can be used to identify the concrete
   implementations. In the context of O'Mega, this is particularly
   important for physics models and target languages. *)

(* One structure of type [raw] has to be initialized in each file by the raw
   RCS keyword strings.  It can remain private to the module, because it is
   only used as argument to the function [parse]. *)
type raw = { revision : string; date : string; author : string; source : string }

(* Parsed revision control info: *)
type t

(* [parse name description keywords] initializes revision control info: *)
val parse : string -> string list -> raw -> t

(* [rename rcs name description] changes the name and description.
   This is useful if more than one module is defined in a file. *)
val rename : t -> string -> string list -> t

(* Access individual parts of the revision control information: *)
val name : t -> string
val description : t -> string list
val revision : t -> string
val date : t -> string
val author : t -> string

(* This one tries \texttt{URL} (svn), \texttt{Source} (CVS) and \texttt{Id},
   in that order, for the filename. *)
val source : t -> string

(* Return the formatted revision control info as a list of strings
   suitable for printing to the terminal or embedding in the output:  *)
val summary : t -> string list

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
