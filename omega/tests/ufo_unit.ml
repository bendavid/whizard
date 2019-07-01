(* omega_unit.ml --

   Copyright (C) 1999-2016 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>

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

let _ =
  let my_name = Sys.argv.(0) in
  let file = ref None
  and line = ref None
  and dir = ref None
  and lorentz = ref None
  and color = ref None
  and skip_tests = ref false
  and skip_example = ref false
  and timing = ref false
  and verbose = ref false
  and usage = "usage: " ^ my_name ^ " ..." in
  Arg.parse
    (Arg.align 
       [ ("-dir", Arg.String (fun s -> dir := Some s),
	  "name UFO output files");
	 ("-file", Arg.String (fun s -> file := Some s),
	  "name UFO output file");
	 ("-line", Arg.String (fun s -> line := Some s),
	  "line UFO fragment");
	 ("-lorentz", Arg.String (fun s -> lorentz := Some s),
	  "expr UFO Lorentz tensor");
	 ("-color", Arg.String (fun s -> color := Some s),
	  "expr UFO color tensor");
	 ("-skip-tests", Arg.Set skip_tests, " skip the tests");
	 ("-skip-example", Arg.Set skip_example, " skip the example");
	 ("-timing", Arg.Set timing, " provide timing information");
	 ("-v", Arg.Set verbose, " be more verbose");
	 ("-verbose", Arg.Set verbose, " be more verbose") ])
    (fun s -> raise (Arg.Bad s))
    usage;
  begin match !file with
  | None -> ()
  | Some name -> ignore (UFO.parse_file name)
  end;
  begin match !line with
  | None -> ()
  | Some s -> ignore (UFO.parse_string s)
  end;
  begin match !dir with
  | None -> ()
  | Some s -> ignore (UFO.parse_directory s)
  end;
  begin match !color with
  | None -> ()
  | Some s ->
     let t = UFOx.Color.of_string s in
     print_endline (UFOx.Color.to_string t);
     print_endline (UFOx.Index.classes_to_string
		      UFOx.Color.rep_to_string
		      (UFOx.Color.classify_indices t))
  end;
  begin match !lorentz with
  | None -> ()
  | Some s ->
     let t = UFOx.Lorentz.of_string s in
     print_endline (UFOx.Lorentz.to_string t);
     print_endline (UFOx.Index.classes_to_string
		      UFOx.Lorentz.rep_to_string
		      (UFOx.Lorentz.classify_indices t))
  end;
  exit 0
