(* config.ml.in --

   Copyright (C) 1999-2017 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>
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

let version = "2.4.1"
let date = "Mar 24 2017"
let status = "release"

let default_UFO_dir = "/Users/reuter/local/omega/share/UFO"

let system_cache_dir = "/Users/reuter/local/omega/var/cache"
let user_cache_dir = "/Users/reuter/.whizard/var/cache"

(* \begin{dubious}
     This relies on the assumption that executable names are unique,
     which is not true for the UFO version.
   \end{dubious} *)
let cache_prefix =
  let basename = Filename.basename Sys.executable_name in
  try Filename.chop_extension basename with | _ -> basename

let cache_suffix = "vertices"

let openmp = false

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)





