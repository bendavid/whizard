(* $Id: commands.ml,v 1.40 2001/11/01 11:39:06 ohl Exp $ *)
(* Copyright (C) 2001-2011 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
   Circe2 is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by 
   the Free Software Foundation; either version 2, or (at your option)
   any later version.
   Circe2 is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
   GNU General Public License for more details.
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *)  

open Printf

module Maps = Diffmaps.Default
module Div = Division.Make_Poly (Maps)
module Grid = Grid.Make (Div)

(* \subsubsection{Abstract Syntax and Default Values} *)

(* A channel is uniquely specified by PDG particle ids and
   polarizations $\{-1,0,+1\}$, which must match the `events'
   in the given file; as should the luminosity.  The options
   are for tuning the grid. *)

type channel =
    { pid1 : int;
      pol1 : int;
      pid2 : int;
      pol2 : int;
      lumi : float;
      bins1 : int;
      x1_min : float;
      x1_max : float;
      fixed_x1_min : bool;
      fixed_x1_max : bool;
      intervals1 : (int * Maps.t) list;
      bins2 : int;
      x2_min : float;
      x2_max : float;
      fixed_x2_min : bool;
      fixed_x2_max : bool;
      intervals2 : (int * Maps.t) list;
      triangle : bool;
      iterations : int;
      events : string;
      binary : bool;
      columns : int } 

let default_channel =
  { pid1 = 11 (* $e^-$ *);
    pol1 = 0;
    pid2 = -11 (* $e^+$ *);
    pol2 = 0;
    lumi = 0.0;
    bins1 = 20;
    x1_min = 0.0;
    x1_max = 1.0;
    fixed_x1_min = false;
    fixed_x1_max = false;
    intervals1 = [];
    bins2 = 20;
    x2_min = 0.0;
    x2_max = 1.0;
    fixed_x2_min = false;
    fixed_x2_max = false;
    intervals2 = [];
    triangle = false;
    iterations = 1000;
    events = "circe2.events";
    binary = false;
    columns = 3 } 
  
(* A parameter set is uniquely specified by PDG particle ids
   (\emph{par abus de langage}), polarizations (now a floating
   point number for the effective polarization of the beam), and
   center of mass energy.  This must match the `events' in the
   files given for the channels.  The other options are for tuning
   the grid. *)

type design =
    { design : string;
      roots : float;
      design_bins1 : int;
      design_bins2 : int;
      channels : channel list;
      comments : string list }

let default_design =
    { design = "TESLA";
      roots = 500.0;
      design_bins1 = default_channel.bins1;
      design_bins2 = default_channel.bins2;
      channels = [];
      comments = [] }

(* One file can hold more than one grid. *)

type file =
    { name : string; block_data : string option; designs : design list }

let default_file =
  { name = "circe2_tool.out"; block_data = None; designs = [] }

type t = file list

(* \subsubsection{Processing} *)

let report msg =
  prerr_string msg;
  flush stderr

let process_channel ch =
  report ("reading: " ^ ch.events ^ " ...");
  let data =
    if ch.binary then
      Events.of_binary_file ch.columns ch.events
    else
      Events.of_ascii_file 3 ch.events in
  report " done.\n";
  let initial_grid =
    Grid.create ~triangle:ch.triangle
      (Div.create ch.intervals1 ch.bins1 ch.x1_min ch.x1_max)
      (Div.create ch.intervals2 ch.bins2 ch.x2_min ch.x2_max) in
  let grid =
    Grid.of_bigarray ~verbose:true ~iterations:ch.iterations
      ~fixed_x1_min:ch.fixed_x1_min ~fixed_x1_max:ch.fixed_x1_max
      ~fixed_x2_min:ch.fixed_x2_min ~fixed_x2_max:ch.fixed_x2_max
      data initial_grid in
  { Grid.pid1 = ch.pid1;
    Grid.pol1 = ch.pol1;
    Grid.pid2 = ch.pid2;
    Grid.pol2 = ch.pol2;
    Grid.lumi = ch.lumi;
    Grid.g = grid }

module S = Set.Make (struct type t = string let compare = compare end)

let channel_prerequisites acc ch =
  S.add ch.events acc

let process_design oc name block_data design =
  let channels = List.rev_map process_channel design.channels
  and comments = List.rev design.comments in
  let acc =
    { Grid.name = design.design;
      Grid.roots = design.roots;
      Grid.channels = channels;
      Grid.comments = comments } in
  report ("writing: " ^ name ^ " ...");
  Grid.design_to_channel oc acc;
  report " done.\n";
  match block_data with
  | Some (oc, name, nb) ->
      report ("writing: " ^ name ^ " ...");
      Grid.design_as_block_data_to_channel oc (nb, acc);
      report " done.\n";
  | None -> ()

  
let design_prerequisites acc design =
  List.fold_left (channel_prerequisites) acc design.channels

let write_file file =
  let oc = open_out file.name in
  begin match file.block_data with
  | Some name ->
      let bd_oc = open_out name in
      let _ =
        List.fold_left (fun nb design ->
          process_design oc file.name (Some (bd_oc, name, nb)) design;
          succ nb) 1 file.designs in
      close_out bd_oc
  | None -> List.iter (process_design oc file.name None) file.designs
  end;
  close_out oc

let file_prerequisites acc file =
  List.fold_left (design_prerequisites) acc file.designs

let prerequisites files =
  List.fold_left (file_prerequisites) S.empty files

let unreadable name =
  try
    Unix.access name [Unix.R_OK];
    false
  with
  | Unix.Unix_error (_, _, _) -> true

let execute files =
  let missing = S.filter unreadable (prerequisites files) in
  if S.is_empty missing then
    List.iter write_file files
  else
    eprintf "circe2_tool: unreadable input files: %s!\n"
      (String.concat ", " (S.elements missing))

(* \subsubsection{Concrete syntax.} *)

open Genlex

let lexer =
  Genlex.make_lexer
    ["file"; "block_data"; "events"; "binary"; "ascii"; "columns";
     "design"; "roots"; "pid"; "pol"; "unpol"; "lumi";
     "fix"; "free"; "min"; "max";
     "bins"; "triangle"; "notriangle"; "iterations";
     "electron"; "positron"; "photon"; "gamma";
     "map"; "id"; "power"; "resonance";
     "beta"; "eta"; "width"; "center";
     "comment"; "/"; "["; ","; "]"; "{"; "}"; "="; "*" ]

exception Parse_Error of string

let expecting s =
  raise (Parse_Error ("expecting " ^ s))

let int_as_float = parser
    [< 'Float x >] -> x
  | [< 'Int n >] -> float n

let interval_cmd = parser
  | [< 'Int n;
       'Kwd "["; x_min = int_as_float;
       'Kwd ","; x_max = int_as_float;
       'Kwd "]" >] -> (n, x_min, x_max)
  | [< >] -> expecting "interval: `n_bins [x_min, x_max]'"

let power_cmd (n, x_min, x_max) = parser
  | [< 'Kwd "beta"; 'Kwd "="; beta = int_as_float;
       'Kwd "eta"; 'Kwd "="; eta = int_as_float >] ->
         if beta <= -1.0 then begin
           eprintf "circe2: ignoring invalid beta: %g <= -1\n" beta;
           flush stderr;
           (n, Maps.id x_min x_max)
         end else
           let alpha = 1.0 /. (1.0 +. beta) in
           (n, Maps.power ~alpha ~eta x_min x_max)
  | [< >] -> expecting "power map parameters: `beta = float eta = float'"

let resonance_cmd (n, x_min, x_max) = parser
  | [< 'Kwd "center"; 'Kwd "="; eta = int_as_float;
       'Kwd "width"; 'Kwd "="; a = int_as_float >] ->
         (n, Maps.resonance ~eta ~a x_min x_max)
  | [< >] -> expecting "resonance map parameters: `center = float width = float'"

let map_cmd = parser
  | [< 'Kwd "id";
       'Kwd "{"; (n, x_min, x_max) = interval_cmd; 'Kwd "}" >] ->
         (n, Maps.id x_min x_max)
  | [< 'Kwd "power";
       'Kwd "{"; (n, x_min, x_max) = interval_cmd;
       m = power_cmd (n, x_min, x_max); 'Kwd "}" >] -> m
  | [< 'Kwd "resonance";
       'Kwd "{"; (n, x_min, x_max) = interval_cmd;
       m = resonance_cmd (n, x_min, x_max); 'Kwd "}" >] -> m
  | [< >] -> expecting "map: `id', `power' or `resonance'"

let particle_as_int = parser
  | [< 'Int pid >] -> pid
  | [< 'Kwd "electron" >] -> 11
  | [< 'Kwd "positron" >] -> -11
  | [< 'Kwd "photon" >] -> 22
  | [< 'Kwd "gamma" >] -> 22

let polarization_as_int = parser
  | [< 'Int pol >] -> pol
  | [< 'Kwd "unpol" >] -> 0
  | [< >] -> expecting "normalized helicity: integer or `unpol'"

let polarization_as_float = parser
  | [< pol = int_as_float >] -> pol
  | [< 'Kwd "unpol" >] -> 0.0
  | [< >] -> expecting "polarization: float or `unpol'"

type coord = X1 | X2 | X12
let coord = parser
  | [< 'Kwd "/"; 'Int c >] ->
      begin match c with
      | 1 -> X1
      | 2 -> X2
      | _ ->
          eprintf "circe2: ignoring dimension %d (not 1, 2, or *)\n" c;
          X12
      end
  | [<  >] -> X12

type side = Min | Max | Minmax
let side = parser
  | [< 'Kwd "min" >] -> Min
  | [< 'Kwd "max" >] -> Max
  | [< 'Kwd "*" >] -> Minmax
  | [< >] -> expecting "`min', `max' or `*'"

type channel_cmd =
  | Pid of int * coord
  | Pol of int * coord
  | Lumi of float
  | Xmin of float * coord
  | Xmax of float * coord
  | Bins of int * coord
  | Diffmap of (int * Maps.t) * coord
  | Triangle of bool
  | Iterations of int
  | Events of string
  | Binary of bool
  | Columns of int
  | Fix of bool * coord * side

let channel_cmd = parser
  | [< 'Kwd "pid"; c = coord;
       'Kwd "="; n = particle_as_int >] -> Pid (n, c)
  | [< 'Kwd "pol"; c = coord;
       'Kwd "="; p = polarization_as_int >] -> Pol (p, c)
  | [< 'Kwd "fix"; c = coord; 'Kwd "="; s = side >] -> Fix (true, c, s)
  | [< 'Kwd "free"; c = coord; 'Kwd "="; s = side >] -> Fix (false, c, s)
  | [< 'Kwd "bins"; c = coord; 'Kwd "="; 'Int n >] -> Bins (n, c)
  | [< 'Kwd "min"; c = coord; 'Kwd "="; x = int_as_float >] -> Xmin (x, c)
  | [< 'Kwd "max"; c = coord; 'Kwd "="; x = int_as_float >] -> Xmax (x, c)
  | [< 'Kwd "map"; c = coord; 'Kwd "="; m = map_cmd >] -> Diffmap (m, c)
  | [< 'Kwd "lumi"; 'Kwd "="; l = int_as_float >] -> Lumi l
  | [< 'Kwd "columns"; 'Kwd "="; 'Int n >] -> Columns n
  | [< 'Kwd "triangle" >] -> Triangle true
  | [< 'Kwd "notriangle" >] -> Triangle false
  | [< 'Kwd "iterations"; 'Kwd "="; 'Int i >] -> Iterations i
  | [< 'Kwd "events"; 'Kwd "="; 'String s >] -> Events s
  | [< 'Kwd "binary" >] -> Binary true
  | [< 'Kwd "ascii" >] -> Binary false

let rec channel_cmd_list = parser
  | [< cmd = channel_cmd; cmd_list = channel_cmd_list >] ->
       cmd :: cmd_list
  | [< >] -> []

type design_cmd =
  | Design of string
  | Roots of float
  | Design_Bins of int * coord
  | Channels of channel_cmd list
  | Comment of string

let design_cmd = parser
  | [< 'Kwd "bins"; c = coord; 'Kwd "="; 'Int n >] -> Design_Bins (n, c)
  | [< 'Kwd "design"; 'Kwd "="; 'String s >] -> Design s
  | [< 'Kwd "roots"; 'Kwd "="; x = int_as_float >] -> Roots x
  | [< 'Kwd "{"; cmds = channel_cmd_list; 'Kwd "}" >] -> Channels cmds
  | [< 'Kwd "comment"; 'Kwd "="; 'String c >] -> Comment c

let rec design_cmd_list = parser
  | [< cmd = design_cmd; cmd_list = design_cmd_list >] ->
      cmd :: cmd_list
  | [< >] -> []

type file_cmd =
  | File of string
  | Block_data of string
  | Designs of design_cmd list
    
let file_cmd = parser
  | [< 'Kwd "file"; 'Kwd "="; 'String s >] -> File s
  | [< 'Kwd "block_data"; 'Kwd "="; 'String s >] -> Block_data s
  | [< 'Kwd "{"; cmds = design_cmd_list; 'Kwd "}" >] -> Designs cmds

let rec file_cmd_list = parser
  | [< cmd = file_cmd; cmd_list = file_cmd_list >] ->
      cmd :: cmd_list
  | [< >] -> []

let rec file_cmds = parser
  | [< 'Kwd "{"; cmd = file_cmd_list; 'Kwd "}";
       cmd_list = file_cmds >] ->
      cmd :: cmd_list
  | [< >] -> []

type file_cmds = file_cmd list

(* \subsubsection{Translate.} *)

let rec update_fix acc = function
  | b, X12, s -> update_fix (update_fix acc (b, X2, s)) (b, X1, s)
  | b, c, Minmax -> update_fix (update_fix acc (b, c, Max)) (b, c, Min)
  | b, X1, Min -> { acc with fixed_x1_min = b }
  | b, X1, Max -> { acc with fixed_x1_max = b }
  | b, X2, Min -> { acc with fixed_x2_min = b }
  | b, X2, Max -> { acc with fixed_x2_max = b }

let rec update_pid acc = function
  | n, X12 -> update_pid (update_pid acc (n, X2)) (n, X1)
  | n, X1 -> { acc with pid1 = n }
  | n, X2 -> { acc with pid2 = n }

let rec update_pol acc = function
  | n, X12 -> update_pol (update_pol acc (n, X2)) (n, X1)
  | n, X1 -> { acc with pol1 = n }
  | n, X2 -> { acc with pol2 = n }

let rec update_bins acc = function
  | n, X12 -> update_bins (update_bins acc (n, X2)) (n, X1)
  | n, X1 -> { acc with bins1 = n }
  | n, X2 -> { acc with bins2 = n }

let rec update_x_min acc = function
  | x, X12 -> update_x_min (update_x_min acc (x, X2)) (x, X1)
  | x, X1 -> { acc with x1_min = x }
  | x, X2 -> { acc with x2_min = x }

let rec update_x_max acc = function
  | x, X12 -> update_x_max (update_x_max acc (x, X2)) (x, X1)
  | x, X1 -> { acc with x1_max = x }
  | x, X2 -> { acc with x2_max = x }

let rec update_map acc = function
  | m, X12 -> update_map (update_map acc (m, X2)) (m, X1)
  | m, X1 -> { acc with intervals1 = m :: acc.intervals1 }
  | m, X2 -> { acc with intervals2 = m :: acc.intervals2 }

let channel design cmds =
  List.fold_left
    (fun acc -> function
      | Pid (n, c) -> update_pid acc (n, c)
      | Pol (p, c) -> update_pol acc (p, c)
      | Lumi l -> { acc with lumi = l }
      | Diffmap (m, c) -> update_map acc (m, c)
      | Bins (n, c) -> update_bins acc (n, c)
      | Xmin (x, c) -> update_x_min acc (x, c)
      | Xmax (x, c) -> update_x_max acc (x, c)
      | Fix (b, c, s) -> update_fix acc (b, c, s)
      | Triangle b -> { acc with triangle = b }
      | Iterations i -> { acc with iterations = i }
      | Events s -> { acc with events = s }
      | Columns n ->
          if n < 3 then
            invalid_arg "#columns < 3"
          else
            { acc with columns = n }
      | Binary b -> { acc with binary = b })
    default_channel cmds

let rec update_design_bins acc = function
  | n, X12 -> update_design_bins (update_design_bins acc (n, X2)) (n, X1)
  | n, X1 -> { acc with design_bins1 = n }
  | n, X2 -> { acc with design_bins2 = n }

let design cmds =
  List.fold_left
    (fun acc -> function
      | Design s -> { acc with design = s }
      | Roots r -> { acc with roots = r }
      | Design_Bins (n, c) -> update_design_bins acc (n, c)
      | Channels cmds ->
	  { acc with channels = channel acc cmds :: acc.channels }
      | Comment c -> { acc with comments = c :: acc.comments })
    default_design cmds

let file cmds =
  List.fold_right
    (fun cmd acc ->
      match cmd with
      | File s -> { acc with name = s }
      | Block_data s -> { acc with block_data = Some s }
      | Designs cmds -> { acc with designs = design cmds :: acc.designs })
    cmds default_file

(* \subsubsection{API} *)

let parse_file name =
  let ic = open_in name in
  let tokens = lexer (Stream.of_channel ic) in
  let cmds = List.map file (file_cmds tokens) in
  close_in ic;
  cmds

let parse_string s =
  let tokens = lexer (Stream.of_string s) in
  List.map file (file_cmds tokens)

(*i
let pre_parse name =
  let ic = open_in name in
  let tokens = lexer (Stream.of_channel ic) in
  let cmds = file_cmds tokens in
  close_in ic;
  cmds
i*)

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
