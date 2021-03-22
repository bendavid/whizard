(* whizard_tool.ml --

   Copyright (C) 1999-2021 by

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

(* \thocwmodulesection{Main Program} *)

let with_file f arg = function
  | None -> ()
  | Some "-" -> f stdout arg
  | Some name ->
      let ch = open_out name in
      f ch arg;
      close_out ch

let _ =
  let usage = "usage: " ^ Sys.argv.(0) ^ " [options]"
  and names = ref []
  and interface = ref None
  and makefile = ref None
  and makefile_processes = ref None in
  Arg.parse
    [ ("-i", Arg.String (fun s -> interface := Some s),
       "write the interface file");
      ("-m", Arg.String (fun s -> makefile := Some s),
       "write the common Makefile");
      ("-p", Arg.String (fun s -> makefile_processes := Some s),
       "write the process Makefile");
      ("-a", Arg.Unit (fun () ->
	interface := Some "process_interface.90";
	makefile := None;
	makefile_processes := Some "Makefile.processes"),
       "write process_interface.f90 and Makefile.processes");
      ("-A", Arg.Unit (fun () ->
	interface := Some "process_interface.90";
	makefile := Some "Makefile.in";
	makefile_processes := Some "Makefile.processes"),
       "write 'em all") ]
    (fun name -> names := name :: !names)
    usage;
  with_file Whizard.write_interface !names !interface;
  with_file Whizard.write_makefile !names !makefile;
  with_file Whizard.write_makefile_processes !names !makefile_processes;
  exit 0

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
