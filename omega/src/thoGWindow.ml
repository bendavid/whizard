(* thoGWindow.ml --

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

(* \thocwmodulesection{Misc.~Windows} *)

let message ?justify ?title ~text () =
  let w = GWindow.window ?title ~border_width:5 () in
  let v = GPack.vbox ~spacing:8 ~packing:w#add () in
  GMisc.label ~xpad:5 ~ypad:5 ?justify ~text ~packing:v#add ();
  let b = GButton.button ~label:"OK" ~packing:v#add () in
  b#connect#clicked ~callback:w#destroy;
  w#show ()

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
