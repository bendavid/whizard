(* feynmp.ml --

   Copyright (C) 1999-2026 by

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

module type T =
  sig
    type amplitudes
    val amplitudes_to_channel : bool -> amplitudes -> out_channel -> unit
    val amplitudes_sans_color_to_channel : bool -> amplitudes -> out_channel -> unit
    val amplitudes_color_only_to_channel : bool -> amplitudes -> out_channel -> unit
    val amplitudes : bool -> string -> amplitudes -> unit
    val amplitudes_sans_color : bool -> string -> amplitudes -> unit
    val amplitudes_color_only : bool -> string -> amplitudes -> unit
  end

let (<<) f g x = f (g x)
let (>>) f g x = g (f x)

module Make (FM : Fusion.Maker) (P : Momentum.T) (M : Model.T) : T
       with type amplitudes = Fusion.Multi(FM)(P)(M).amplitudes =
  struct

    module F = FM(P)(M)
    module CF = Fusion.Multi(FM)(P)(M)
    module SCM = Orders.Slice(Colorize.It(M))

    type amplitudes = CF.amplitudes

    let opt_array_to_list a =
      let rec opt_array_to_list' acc i a =
	if i < 0 then
	  acc
	else
	  begin match a.(i) with
	  | None -> opt_array_to_list' acc (pred i) a
	  | Some x -> opt_array_to_list' (x :: acc) (pred i) a
	  end in
      opt_array_to_list' [] (Array.length a - 1) a
  
    let amplitudes_by_flavor amplitudes =
      List.map opt_array_to_list (Array.to_list (CF.process_table amplitudes))

(* Take a [CF.amplitude list] assumed to correspond to the same
   external states after stripping the color and return a
   pair of the list of external particles and the corresponding
   Feynman diagrams without color. *)

    let wf1 amplitude = 
      match F.externals amplitude with
      | wf :: _ -> wf
      | [] -> failwith "Omega.forest_sans_color: no external particles"

    let uniq l =
      ThoList.uniq (List.sort compare l)

    let forest_sans_color = function
      | amplitude :: _ as amplitudes ->
	let externals = F.externals amplitude in
	let prune_color wf =
	  (F.flavor_sans_color wf, F.momentum_list wf) in
	let prune_color_and_couplings (wf, c) =
	  (prune_color wf, None) in
	(List.map prune_color externals,
	 uniq
	   (List.map
	      (fun t ->
		Tree.canonicalize
		  (Tree.map prune_color_and_couplings prune_color t))
	      (ThoList.flatmap (fun a -> F.forest (wf1 a) a) amplitudes)))
      | [] -> ([], [])

    let dag_sans_color = function
      | amplitude :: _ as amplitudes ->
        let prune a = a in
        List.map prune amplitudes
      | [] -> []

    let p2s p =
      if p >= 0 && p <= 9 then
        string_of_int p
      else if p <= 36 then
        String.make 1 (Char.chr (Char.code 'A' + p - 10))
      else
        "_"

    let format_p wf =
      String.concat "" (List.map p2s (F.momentum_list wf))

    let variable wf =
      M.flavor_to_string (F.flavor_sans_color wf) ^ "[" ^ format_p wf ^ "]"

    let variable' wf =
      SCM.flavor_to_TeX (F.flavor wf) ^ "(" ^ format_p wf ^ ")"

    let feynmf_style tex propagator color =
      { Tree.style =
          begin match propagator with
          | Coupling.Prop_Feynman
          | Coupling.Prop_Gauge _ ->
            begin match color with
            | Color.AdjSUN _ -> Some ("gluon", tex)
            | _ -> Some ("boson", tex)
            end
          | Coupling.Prop_Col_Feynman -> Some ("gluon", tex)
          | Coupling.Prop_Unitarity
          | Coupling.Prop_Rxi _ -> Some ("dbl_wiggly", tex)
          | Coupling.Prop_Spinor
          | Coupling.Prop_ConjSpinor -> Some ("fermion", tex)
          | _ -> None
          end;
        Tree.rev =
          begin match propagator with
          | Coupling.Prop_Spinor -> true
          | Coupling.Prop_ConjSpinor -> false
          | _ -> false
          end;
        Tree.label = None;
        Tree.tension = None }

    let header incoming outgoing =
      "$ " ^
      String.concat " "
	(List.map (SCM.flavor_to_TeX << F.flavor) incoming) ^
      " \\to " ^
      String.concat " "
	(List.map (SCM.flavor_to_TeX << SCM.conjugate << F.flavor) outgoing) ^
      " $"

    let header_sans_color incoming outgoing =
      "$ " ^
      String.concat " "
	(List.map (M.flavor_to_TeX << fst) incoming) ^
      " \\to " ^
      String.concat " "
	(List.map (M.flavor_to_TeX << M.conjugate << fst) outgoing) ^
      " $"
	
    let diagram incoming tree =
      let fmf wf =
	let f = F.flavor wf in
	feynmf_style "" (SCM.propagator f) (SCM.color f) in
      Tree.map
        (fun (n, _) ->
	  let n' = fmf n in
	  if List.mem n incoming then
            { n' with Tree.rev = not n'.Tree.rev }
	  else
            n')
        (fun l ->
	  if List.mem l incoming then
            l
	  else
            F.conjugate l)
	tree

    let diagram_sans_color incoming tree =
      let fmf (f, p) =
	feynmf_style "" (M.propagator f) (M.color f) in
      Tree.map
	(fun (n, c) ->
	  let n' = fmf n in
	  if List.mem n incoming then
	    { n' with Tree.rev = not n'.Tree.rev }
	  else
	    n')
	(fun (f, p) ->
	  if List.mem (f, p) incoming then
	    (f, p)
	  else
	    (M.conjugate f, p))
	tree

    let feynmf_set amplitude =
      match F.externals amplitude with
      | wf1 :: wf2 :: wfs ->
	let incoming = [wf1; wf2] in
    	{ Tree.header = header incoming wfs;
	  Tree.incoming = incoming;
	  Tree.diagrams =
	    List.map (diagram incoming) (F.forest wf1 amplitude) }
      | _ -> failwith "less than two external particles"

    let feynmf_set_sans_color (externals, trees) =
      match externals with
      | wf1 :: wf2 :: wfs ->
	let incoming = [wf1; wf2] in
	{ Tree.header = header_sans_color incoming wfs;
	  Tree.incoming = incoming;
	  Tree.diagrams =
	    List.map (diagram_sans_color incoming) trees }
      | _ -> failwith "less than two external particles"

    let feynmf_set_sans_color_empty (externals, trees) =
      match externals with
      | wf1 :: wf2 :: wfs ->
	let incoming = [wf1; wf2] in
	{ Tree.header = header_sans_color incoming wfs;
	  Tree.incoming = incoming;
	  Tree.diagrams = [] }
      | _ -> failwith "less than two external particles"

    let uncolored_colored amplitudes =
      { Tree.outer = feynmf_set_sans_color (forest_sans_color amplitudes);
	Tree.inner = List.map feynmf_set amplitudes }

    let uncolored_only amplitudes =
      { Tree.outer = feynmf_set_sans_color (forest_sans_color amplitudes);
	Tree.inner = [] }

    let colored_only amplitudes =
      { Tree.outer = feynmf_set_sans_color_empty (forest_sans_color amplitudes);
	Tree.inner = List.map feynmf_set amplitudes }

    let momentum_to_TeX (_, p) =
      String.concat "" (List.map p2s p)

    let wf_to_TeX (f, _ as wf) =
      M.flavor_to_TeX f ^ "(" ^ momentum_to_TeX wf ^ ")"

    let amplitudes latex name amplitudes =
	Tree.feynmf_sets_wrapped latex name
	  wf_to_TeX momentum_to_TeX variable' format_p
	  (List.map uncolored_colored (amplitudes_by_flavor amplitudes))
	
    let amplitudes_sans_color latex name amplitudes =
	Tree.feynmf_sets_wrapped latex name
	  wf_to_TeX momentum_to_TeX variable' format_p
	  (List.map uncolored_only (amplitudes_by_flavor amplitudes))

    let amplitudes_color_only latex name amplitudes =
	Tree.feynmf_sets_wrapped latex name
	  wf_to_TeX momentum_to_TeX variable' format_p
	  (List.map colored_only (amplitudes_by_flavor amplitudes))

    let amplitudes_to_channel latex amplitudes channel =
	Tree.feynmf_sets_wrapped_to_channel latex channel
	  wf_to_TeX momentum_to_TeX variable' format_p
	  (List.map uncolored_colored (amplitudes_by_flavor amplitudes))
	
    let amplitudes_sans_color_to_channel latex amplitudes channel =
	Tree.feynmf_sets_wrapped_to_channel latex channel
	  wf_to_TeX momentum_to_TeX variable' format_p
	  (List.map uncolored_only (amplitudes_by_flavor amplitudes))

    let amplitudes_color_only_to_channel latex amplitudes channel =
	Tree.feynmf_sets_wrapped_to_channel latex channel
	  wf_to_TeX momentum_to_TeX variable' format_p
	  (List.map colored_only (amplitudes_by_flavor amplitudes))

  end


