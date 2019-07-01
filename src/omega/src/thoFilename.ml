(* $Id: thoFilename.ml 3670 2012-01-21 19:33:07Z jr_reuter $

   Copyright (C) 1999-2012 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>
       Christian Speckner <christian.speckner@physik.uni-freiburg.de>

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

let rec split' acc path =
  match Filename.dirname path, Filename.basename path with
  | "/", basename -> "/" :: basename :: acc
  | ".", basename -> basename :: acc
  | dirname, basename -> split' (basename :: acc) dirname

let split path =
  split' [] path

let join = function
  | [] -> "."
  | [basename] -> basename
  | dirname :: rest -> List.fold_left Filename.concat dirname rest

let expand_home path =
  match split path with
  | ("~" | "$HOME" | "${HOME}") :: rest ->
      join ((try Sys.getenv "HOME" with Not_found -> "/tmp") :: rest)
  | _ -> path

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
