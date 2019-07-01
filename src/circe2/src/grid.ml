(* $Id: grid.ml,v 1.40 2004/02/13 19:05:31 ohl Exp $ *)
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

module type T =
  sig
    module D : Division.T

    type t
    val copy : t -> t

    val create : ?triangle:bool -> D.t -> D.t -> t

    val record : t -> float -> float -> float -> unit

    val rebin : ?power:float ->
      ?fixed_x1_min:bool -> ?fixed_x1_max:bool ->
        ?fixed_x2_min:bool -> ?fixed_x2_max:bool -> t -> t

    val normalize : t -> t

    val of_bigarray : ?verbose:bool -> ?power:float ->
      ?iterations:int -> ?margin:float -> ?cutoff:int ->
        ?fixed_x1_min:bool -> ?fixed_x1_max:bool ->
          ?fixed_x2_min:bool -> ?fixed_x2_max:bool ->
            (float, Bigarray.float64_elt,
             Bigarray.fortran_layout) Bigarray.Array2.t -> t -> t

    type channel =
        { pid1 : int;
          pol1 : int;
          pid2 : int;
          pol2 : int;
          lumi : float;
          g : t }

    val to_channel : out_channel -> channel -> unit

    type design =
        { name : string;
          roots : float;
          channels : channel list;
          comments : string list }

    val design_to_channel : out_channel -> design -> unit
    val designs_to_channel : out_channel ->
      ?comments:string list-> design list -> unit
    val designs_to_file : string ->
      ?comments:string list -> design list -> unit

    val design_as_block_data_to_channel :
        out_channel -> int * design -> unit
    val designs_as_block_data_to_channel :
        out_channel -> ?comments:string list-> (int * design) list -> unit
    val designs_as_block_data_to_file :
        string -> ?comments:string list -> (int * design) list -> unit
    val variance : t -> float
  end


module Make (D : Division.T) =
  struct
    module D = D

    type t =
        { d1 : D.t;
          d2 : D.t;
          w : float array;
          var : float array;
          triangle : bool }

    let copy grid =
      { d1 = D.copy grid.d1;
        d2 = D.copy grid.d2;
        w = Array.copy grid.w;
        var = Array.copy grid.var;
        triangle = grid.triangle }

    let create ?(triangle = false) d1 d2 =
      let n = D.n_bins d1 * D.n_bins d2 in
      { d1 = d1;
        d2 = d2;
        w = Array.create n 0.0;
        var = Array.create n 0.0;
        triangle = triangle }

    (* Here's the offset-0 variant of the offset-1 Fortran code: *)        
    let find grid x y =
      D.find grid.d1 x + D.n_bins grid.d1 * D.find grid.d2 y

    let project_triangle triangle x y =
      if triangle then begin
        if x >= y then begin
          (x, y /. x)
        end else begin
          (y, x /. y)
        end
      end else
        (x, y)

    (* Note that there is \emph{no} jacobian here.  It is
       applied by Fortran program interpreting the grid as
       distribution.  It is not needed for the event generator
       anyway. *)
    let record grid x y f =
      let x', y' = project_triangle grid.triangle x y in
      D.record grid.d1 x' f;
      D.record grid.d2 y' f;
      let n = find grid x' y' in
      grid.w.(n) <- grid.w.(n) +. f;
      grid.var.(n) <- grid.var.(n) +. f /. D.caj grid.d1 x' /. D.caj grid.d2 y'

    let rebin ?power ?fixed_x1_min ?fixed_x1_max
        ?fixed_x2_min ?fixed_x2_max grid =
      let n = D.n_bins grid.d1 * D.n_bins grid.d2 in
      { d1 = D.rebin ?power
          ?fixed_min:fixed_x1_min ?fixed_max:fixed_x1_max grid.d1;
        d2 = D.rebin ?power
          ?fixed_min:fixed_x2_min ?fixed_max:fixed_x2_max grid.d2;
        w = Array.create n 0.0;
        var = Array.create n 0.0;
        triangle = grid.triangle }

    let normalize grid =
      let sum_w = Array.fold_left (+.) 0.0 grid.w in
      { d1 = D.copy grid.d1;
        d2 = D.copy grid.d2;
        w = Array.map (fun w -> w /. sum_w) grid.w;
        var = Array.copy grid.var;
        triangle = grid.triangle }

    (* Monitoring the variance in each cell is \emph{not} a good idea for
       approximating distributions of unweighted events: it always vanishes
       for unweighted events, even if they are distributed very unevenly.
       Therefore, we monitor the \emph{global} variance instead: *)

    let variance grid =
      let n = float (Array.length grid.w) in
      let w = Array.fold_left (+.) 0.0 grid.w /. n
      and w2 = Array.fold_left (fun acc w -> acc +. w *. w) 0.0 grid.w /. n in
      w2 -. w *. w

    let variance grid =
      let n = float (Array.length grid.var) in
      let w = Array.fold_left (+.) 0.0 grid.var /. n
      and w2 = Array.fold_left (fun acc w -> acc +. w *. w) 0.0 grid.var /. n in
      w2 -. w *. w

    (* Find the grid with the lowest variance.  Allow local fluctuations and
       stop only after moving to twice the lowest value. *)

    let start_progress_report verbose var =
      if verbose then begin
        eprintf "adapting variance: %g" var;
        flush stderr
      end

    let progress_report verbose soft_limit best_var var =
      if verbose then begin
        if var < best_var then begin
          eprintf ", %g" var;
          flush stderr
        end else begin
          eprintf " [%d]" soft_limit;
          flush stderr
        end
      end

    let stop_progress_report verbose =
      if verbose then begin
        eprintf " done.\n";
        flush stderr
      end

    (* The main routine constructing an adapted grid. *)

    let of_bigarray ?(verbose = false)
        ?power ?(iterations = 1000) ?(margin = 1.5) ?(cutoff = 10)
        ?fixed_x1_min ?fixed_x1_max ?fixed_x2_min ?fixed_x2_max data initial =

      let record_data grid =
        for i2 = 1 to Bigarray.Array2.dim2 data do
          let x = Bigarray.Array2.get data 1 i2
          and y = Bigarray.Array2.get data 2 i2
          and w = Bigarray.Array2.get data 3 i2 in
          try
            record grid x y w
          with
          | Division.Out_of_range (x, (x_min, x_max)) ->
              eprintf "internal error: %g not in [%g,%g]\n" x x_min x_max
        done in

      let rebinner grid =
        rebin ?power
          ?fixed_x1_min ?fixed_x1_max ?fixed_x2_min ?fixed_x2_max grid in

      let rec improve_bigarray hard_limit soft_limit best_var best_grid grid =
        if soft_limit <= 0 || hard_limit <= 0 then
          normalize best_grid
        else begin
          record_data grid;
          let var = variance grid in
          progress_report verbose soft_limit best_var var;
          if var >= margin *. best_var then
            normalize best_grid
          else
            let best_var, best_grid, soft_limit =
              if var < best_var then
                (var, grid, cutoff)
              else
                (best_var, best_grid, pred soft_limit) in

            (* Continuation passing makes recursion with exception handling
               tail recursive.  This is not really needed, because the
               data structures are not to big and recursion is not expected
               to be too deep. It doesn't hurt either, since the idiom is
               sufficiently transparent. *)
            let continue =
              try
                let grid' = rebinner grid in
                fun () -> improve_bigarray
                    (pred hard_limit) soft_limit best_var best_grid grid'
              with
              | Division.Rebinning_failure msg ->
                  eprintf "circe2: rebinning failed: %s!\n" msg;
                  fun () -> best_grid in
            continue ()
        end in

      record_data initial;
      let var = variance initial in
      start_progress_report verbose var;

      let result =
        improve_bigarray iterations cutoff var initial (rebinner initial) in
      stop_progress_report verbose;
      result

    type channel =
        { pid1 : int;
          pol1 : int;
          pid2 : int;
          pol2 : int;
          lumi : float;
          g : t }

    let to_channel oc ch =
      fprintf oc "pid1, pol1, pid2, pol2, lumi\n";
      fprintf oc " %d %d %d %d %G\n"
        ch.pid1 ch.pol1 ch.pid2 ch.pol2 ch.lumi;
      fprintf oc "#bins1, #bins2, triangle?\n";
      fprintf oc " %d %d %s\n"
        (D.n_bins ch.g.d1) (D.n_bins ch.g.d2)
        (if ch.g.triangle then "T" else "F");
      fprintf oc "x1, map1, alpha1, xi1, eta1, a1, b1\n";
      D.to_channel oc ch.g.d1;
      fprintf oc "x2, map2, alpha2, xi2, eta2, a2, b2\n";
      D.to_channel oc ch.g.d2;
      fprintf oc "weights\n";
      Array.iter (fun x ->
        fprintf oc " %s\n" (Float.Double.to_string x)) ch.g.w

    let as_block_data_to_channel oc n_beam n_ch ch =
      let print_integer name value =
        fprintf oc "      data bd%s(%d,%d) /%d/\n" name n_ch n_beam value
      and print_float name value =
        fprintf oc "      data bd%s(%d,%d) /%s/\n"
          name n_ch n_beam (Float.Double.to_string value)
      and print_string name value =
        fprintf oc "      data bd%s(%d,%d) /'%s'/\n" name n_ch n_beam value
      and print_logical name value =
        fprintf oc "      data bd%s(%d,%d) /%s/\n"
          name n_ch n_beam (if value then ".true." else ".false.") in
      print_integer "pid1" ch.pid1;
      print_integer "pol1" ch.pol1;
      print_integer "pid2" ch.pid2;
      print_integer "pol2" ch.pol2;
      print_float "lumi" ch.lumi;
      print_integer "nb1" (D.n_bins ch.g.d1);
      print_integer "nb2" (D.n_bins ch.g.d2);
      print_logical "tria" ch.g.triangle;
      D.as_block_data_to_channel oc "1" n_ch n_beam ch.g.d1;
      D.as_block_data_to_channel oc "2" n_ch n_beam ch.g.d2;
      ()

    type design =
        { name : string;
          roots : float;
          channels : channel list;
          comments : string list }

    type polarization_support =
      | Averaged
      | Helicities
      | Density_Matrices

    let polarization_support design =
      if List.for_all (fun ch -> ch.pol1 = 0 && ch.pol2 = 0)
          design.channels then
        Averaged
      else if List.for_all (fun ch -> ch.pol1 <> 0 && ch.pol2 <> 0)
          design.channels then
        Helicities
      else
        invalid_arg
          "Grid.polarization_support: mixed polarization support!"

    let format_polarization_support = function
      | Averaged -> "averaged"
      | Helicities -> "helicities"
      | Density_Matrices -> "density matrices"

    let getlogin () =
      (Unix.getpwuid (Unix.getuid ())).Unix.pw_name

    let design_to_channel oc design =
      let utc = Unix.gmtime (Unix.time ()) in
      List.iter (fun s -> fprintf oc "! %s\n" s) design.comments;
      fprintf oc "! generated with %s by %s@%s, "
        (Sys.argv.(0)) (getlogin ()) (Unix.gethostname ());
      fprintf oc "%4.4d/%2.2d/%2.2d %2.2d:%2.2d:%2.2d GMT\n"
        (utc.Unix.tm_year + 1900) (utc.Unix.tm_mon + 1) utc.Unix.tm_mday
        utc.Unix.tm_hour utc.Unix.tm_min utc.Unix.tm_sec;
      fprintf oc "CIRCE2 FORMAT#1\n";
      fprintf oc "design, roots\n";
      fprintf oc " '%s' %G\n" design.name design.roots;
      fprintf oc "#channels, pol.support\n";
      fprintf oc " %d '%s'\n"
        (List.length design.channels)
        (format_polarization_support (polarization_support design));
      List.iter (to_channel oc) design.channels;
      fprintf oc "ECRIC2\n"

    let design_as_block_data_to_channel oc (n_design, design) =
      let utc = Unix.gmtime (Unix.time ()) in
      List.iter (fun s -> fprintf oc "c %s\n" s) design.comments;
      fprintf oc "c generated with %s by %s@%s, "
        (Sys.argv.(0)) (Unix.getlogin ()) (Unix.gethostname ());
      fprintf oc "%4.4d/%2.2d/%2.2d %2.2d:%2.2d:%2.2d GMT\n"
        (utc.Unix.tm_year + 1900) (utc.Unix.tm_mon + 1) utc.Unix.tm_mday
        utc.Unix.tm_hour utc.Unix.tm_min utc.Unix.tm_sec;
      fprintf oc "      data bddsgn(%d)  /'%s'/\n" n_design design.name;
      fprintf oc "      data bdbrs(%d)  /%G/\n" n_design design.roots;
      fprintf oc "      data bdnc(%d)   /%d/\n"
        n_design (List.length design.channels);
      let _ =
        List.fold_left (fun n_ch ch ->
          as_block_data_to_channel oc n_design n_ch ch;
          succ n_ch) 1 design.channels in
      ()

    let designs_to_channel oc ?(comments = []) designs =
      List.iter (fun c -> fprintf oc "! %s\n" c) comments;
      List.iter (design_to_channel oc) designs

    let designs_as_block_data_to_channel oc ?(comments = []) designs =
      List.iter (fun c -> fprintf oc "! %s\n" c) comments;
      List.iter (design_as_block_data_to_channel oc) designs

    let designs_to_file name ?comments designs =
      let oc = open_out name in
      designs_to_channel oc ?comments designs;
      close_out oc

    let designs_as_block_data_to_file name ?comments designs =
      let oc = open_out name in
      designs_as_block_data_to_channel oc ?comments designs;
      close_out oc

  end

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
