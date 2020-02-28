(* ogiga.ml --

   Copyright (C) 1999-2020 by

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

(* NB: this module \emph{must} be compiled with \verb+-labels+,
   since \verb+labltk+ doesn't appear to work in classic mode.  *)

(* \begin{dubious}
     Keep in mind that \texttt{ocamlweb} doesn't work properly with
     O'Caml~3 yet.  The colons in label declarations are typeset with
     erroneous white space.
   \end{dubious} *)

(* \thocwmodulesection{Windows} *)

let window =
  GWindow.window ~width:550 ~height:500 ~title:
    "O'Giga: O'Mega Graphical Interface for Generation and Analysis" ()
let vbox = GPack.vbox ~packing:window#add ()

let menubar = GMenu.menu_bar ~packing:(vbox#pack ~expand:false) ()
let factory = new ThoGMenu.factory menubar
let accel_group = factory#accel_group
let file_menu = factory#add_submenu "File"
let edit_menu = factory#add_submenu "Edit"
let exec_menu = factory#add_submenu "Exec"
let help_menu = factory#add_submenu_right "Help"
let hbox = GPack.hbox ~packing:(vbox#pack ~expand:false) ()

let about () =
  ThoGWindow.message ~justify:`LEFT
    ~text:(String.concat "\n"
            ([ "This is the skeleton for a graphical interface";
               "for O'Mega."; "";
               "There is almost no functionality implemented yet.";
               "I'm still trying to learn GTK+ and LablGTK."]) ()


(* \thocwmodulesection{Main Program} *)

module O = Omega.Make
module F = Fusion
module T = Targets
module M = Models

module SM = M.SM(M.SM)
module SM_ac = M.SM(M.SM_anomalous)

module O1a = O(F.Mixed23)(T.Fortran)(SM)
module O1b = O(F.Mixed23_Majorana)(T.Fortran_Majorana)(SM)

module O2a = O(F.Binary)(T.Fortran)(SM_ac)
module O2b = O(F.Binary_Majorana)(T.Fortran_Majorana)(SM_ac)

module O3a = O(F.Binary)(T.Fortran)(M.QED)
module O3b = O(F.Binary_Majorana)(T.Fortran_Majorana)(M.QED)
module O3c = O(F.Binary)(T.Helas)(M.QED)

module O4a = O(F.Binary)(T.Fortran)(M.YM)
module O4b = O(F.Binary_Majorana)(T.Fortran_Majorana)(M.YM)
module O4c = O(F.Binary)(T.Helas)(M.YM)

module O5a = O(F.Binary)(T.Fortran)(M.SM_Rxi)
module O5b = O(F.Binary_Majorana)(T.Fortran_Majorana)(M.SM_Rxi)

module O6a = O(F.Binary)(T.Fortran)(M.SM_clones)
module O6b = O(F.Binary_Majorana)(T.Fortran_Majorana)(M.SM_clones)

(*i
module O6 = O(F.Binary_Majorana)(T.Fortran_Majorana)(M.MSSM(M.MSSM_no_goldstone))
i*)
 
let flavors = SM.external_flavors
let flavor_to_string = SM.flavor_to_string
let flavors_tree = ThoGMenu.tree_of_nested_lists flavor_to_string (flavors ())

let particle_menu button =
  ThoGMenu.submenu_tree button#set_state flavors_tree

let process incoming outgoing =
  let in1 = incoming.(0)
  and in2 = incoming.(1)
  and incoming = Array.to_list incoming
  and outgoing = Array.to_list outgoing in
  let s =
    String.concat " " (List.map SM.flavor_to_string incoming) ^ " -> " ^
    String.concat " " (List.map SM.flavor_to_string outgoing) in
  O1a.diagrams in1 in2 outgoing

let font =
  Gdk.Font.load "-*-helvetica-medium-r-normal--*-120-*-*-*-*-iso8859-1"

let conjugate (f, p) = (SM.conjugate f, p)
let cross (f, p) = (SM.conjugate f, Momentum.Default.neg p)

let node_to_string (f, p) =
  Printf.sprintf "%s[%s]"
    (SM.flavor_to_string f)
    (String.concat "" (List.map string_of_int (Momentum.Default.to_ints p)))

let create_linear_rectangle n1 n2 f =
  Array.init (n1 * n2) (fun n -> f n (n mod n1) (n / n1))

let rows = 4
let columns = 3

class ['a] menu_button_custom widgets accept format state menu =
  object (self)
    inherit ['a] ThoGMenu.menu_button widgets format state menu as super
    method set_menu menu =
      self#connect#clicked ~callback:(fun () ->
        let m = ThoGMenu.submenu_tree (fun s -> self#set_state s; accept s)
            menu in
        m#popup ~button:3 ~time:0);
      ()
  end

let menu_button_custom accept format state menu
    ?border_width ?width ?height ?packing ?show () =
  new menu_button_custom (ThoGButton.mutable_button_raw
                            ?border_width ?width ?height ?packing ?show ())
    accept format state menu

let line_style flavor =
  match SM.propagator flavor with
  | Coupling.Prop_Scalar | Coupling.Aux_Scalar ->
      ThoGDraw.Plain
  | Coupling.Prop_Spinor | Coupling.Aux_Spinor ->
      ThoGDraw.Arrow ThoGDraw.Forward
  | Coupling.Prop_ConjSpinor | Coupling.Aux_ConjSpinor ->
      ThoGDraw.Arrow ThoGDraw.Backward
  | Coupling.Prop_Majorana | Coupling.Aux_Majorana ->
      ThoGDraw.Name "majorana"
  | Coupling.Prop_Feynman | Coupling.Prop_Gauge _ ->
      begin match SM.color flavor with
      | Color.Singlet -> ThoGDraw.Wiggles
      | Color.AdjSUN _ -> ThoGDraw.Curls
      | Color.SUN _ -> ThoGDraw.Name  ("???: " ^ SM.flavor_to_string flavor)
      end
  | Coupling.Prop_Unitarity | Coupling.Prop_Rxi _
  | Coupling.Aux_Vector | Coupling.Aux_Tensor_1 ->
      ThoGDraw.Double
  | Coupling.Only_Insertion ->
      ThoGDraw.Name (SM.flavor_to_string flavor ^ " insertion")
    
let main () =
  window#connect#destroy ~callback:GMain.Main.quit;
  let factory = new GMenu.factory file_menu ~accel_group in
  factory#add_item "Open..." ~key:GdkKeysyms._O
    ~callback:(fun () -> prerr_endline "open ...");
  factory#add_item "Save" ~key:GdkKeysyms._S
    ~callback:(fun () -> prerr_endline "save");
  factory#add_item "Save as..."
    ~callback:(fun () -> prerr_endline "save as");
  factory#add_separator ();
  factory#add_item "Quit" ~key:GdkKeysyms._Q ~callback:window#destroy;
  let factory = new GMenu.factory edit_menu ~accel_group in
  let dc' = new ThoGDraw.decoration_context in
  factory#add_item "Preferences" ~key:GdkKeysyms._E
    ~callback:(fun () -> ThoGDraw.edit_preferences dc');
  let factory = new GMenu.factory help_menu ~accel_group in
  factory#add_item "About" ~key:GdkKeysyms._A ~callback:about;
  let tooltips = GData.tooltips () in
  let default_flavor = List.hd (snd (List.hd (flavors ()))) in
  let hbox = GPack.hbox ~packing:(vbox#pack ~expand:false) () in
  let tip2 =
    " (left mouse button, SPACE or RET will pop up a menu;" ^
    " right button will select)" in
  let incoming =
    new ThoGMenu.tensor_menu flavor_to_string default_flavor flavors_tree 2
      ~tooltip_maker:(fun i ->
        "incoming particle #" ^ string_of_int (succ i) ^ tip2)
      ~label:"incoming" ~width:50 ~packing:hbox#pack () in
  let smt = ThoGMenu.Leafs (List.map (fun n -> (string_of_int n, n))
                              (ThoList.range 2 8)) in
  let n_outgoing_frame = GBin.frame ~label:"#" ~packing:hbox#pack () in  
  let outgoing =
    new ThoGMenu.tensor_menu flavor_to_string default_flavor flavors_tree 8
      ~tooltip_maker:(fun i ->
        "outgoing particle #" ^ string_of_int (succ i) ^ tip2)
      ~label:"outgoing" ~width:50 ~packing:hbox#pack () in
  let n_outgoing =
    menu_button_custom (fun n -> outgoing#set_active n) string_of_int 4 smt
      ~width:30 ~packing:n_outgoing_frame#add () in
  outgoing#set_active 4;
  let dds = GPack.table ~rows ~columns ~homogeneous:true
      ~packing:(vbox#pack ~expand:true) () in
  let dc = new ThoGDraw.decoration_context in
  let dd = create_linear_rectangle columns rows
      (fun n n1 n2 -> new ThoGDraw.diagram_display
          ~label:(string_of_int (succ n))
          ~node_to_string ~conjugate ~cross
          ~nodes2edge:(fun n _ -> fst n) ~line_style
          ~packing:(dds#attach ~left:n1 ~top:n2 ~expand:`BOTH) dc) in
  let factory = new GMenu.factory exec_menu ~accel_group in
  let diagrams = ref [| |] in
  let num_diagrams = ref 0 in
  let offset = ref 0
  and min_offset = ref 0
  and max_offset = ref 0
  and num_squares = rows * columns in
  let clamp o = max !min_offset (min !max_offset o) in
  let redraw () =
    let last = pred (min !num_diagrams num_squares) in
    for i = 0 to last do
      dd.(i)#viewport#drawable#set_decoration_context dc';
      let i' = i + !offset in
      dd.(i)#set_diagram !diagrams.(i');
      dd.(i)#set_label
        (Printf.sprintf "diagram #%d (of %d)" (succ i') !num_diagrams)
    done;
    for i = succ last to pred num_squares do
      dd.(i)#clear_diagram ();
      dd.(i)#set_label "no diagram"
    done in
  factory#add_item "Execute" ~key:GdkKeysyms._X
    ~callback:(fun () ->
      diagrams := Array.of_list (process incoming#states outgoing#states);
      num_diagrams := Array.length !diagrams;
      min_offset := 0;
      max_offset := !num_diagrams - num_squares;
      offset := !min_offset;
      redraw ());
  window#add_accel_group accel_group;
  window#event#connect#key_press ~callback:(fun evt ->
    let old_offset = !offset in
    let k = GdkEvent.Key.keyval evt in
    if k = GdkKeysyms._b then
      offset := clamp (pred !offset)
    else if k = GdkKeysyms._f then
      offset := clamp (succ !offset)
    else if k = GdkKeysyms._p then
      offset := clamp (!offset - columns)
    else if k = GdkKeysyms._n then
      offset := clamp (!offset + columns);
    if old_offset <> !offset then
      redraw ();

(*i
    Printf.eprintf "key = %s: %d (%d, %d) => %d\n"
      (GdkEvent.Key.string evt) old_offset !min_offset !max_offset !offset;
    flush stderr;
i*)
    true);
  window#show ();
  GMain.Main.main ()

let _ = Printexc.print main ()

(*i
  begin
    let fancy = "omega_logo_fancy.xpm"
    and plain = "omega_logo.xpm" in
    if Sys.file_exists fancy then
      let pixmap = GDraw.pixmap_from_xpm ~file:fancy ~window () in
      ignore (GMisc.pixmap pixmap ~packing:vbox#pack ())
    else if Sys.file_exists plain then
      let pixmap = GDraw.pixmap_from_xpm ~file:plain ~window () in
      ignore (GMisc.pixmap pixmap ~packing:vbox#pack ())
  end;
i*)

module type Integers =
    Model.Mutable with type flavor = int
    and type constant = int and type gauge = int

module Model_Loader (Mutable : Integers)
    (Static : Model.T with type constant = int and type gauge = int) =
  struct

    let kludge_flavor = List.hd (Static.flavors ())
    let kludge_flavor_int = 0
    let kludge_constant = 0
    let kludge_gauge = 0

    let kludge_vertices =
      fun () -> ([], [], [])
    let kludge_fuse =
      ((fun _ _ -> []), (fun _ _ _ -> []), (fun _ -> []))
    let int_to_flavor f = kludge_flavor
    let int_of_flavor f = kludge_flavor_int
    let int_to_constant c = kludge_constant
    let int_to_gauge g = kludge_gauge

    let lift_flavor fct f = fct (int_to_flavor f)
    let lift_constant fct c = fct (int_to_constant c)
    let lift_gauge fct g = fct (int_to_gauge g)

    let load () =
      Mutable.setup
        ~color:(lift_flavor Static.color)
        ~pdg:(lift_flavor Static.pdg)
        ~lorentz:(lift_flavor Static.lorentz)
        ~propagator:(lift_flavor Static.propagator)
        ~width:(lift_flavor Static.width)
        ~goldstone:(fun f ->
          match Static.goldstone (int_to_flavor f) with
          | None -> None
          | Some (f', phase') -> Some (int_of_flavor f', phase'))
        ~conjugate:(fun f ->
          int_of_flavor (Static.conjugate (int_to_flavor f)))
        ~fermion:(lift_flavor Static.fermion)
        ~max_degree:(Static.max_degree ())
        ~vertices:kludge_vertices
        ~fuse:kludge_fuse
        ~flavors:(List.map (fun (s, fl) ->
          (s, List.map int_of_flavor fl)) (Static.external_flavors ()))
        ~parameters:(Static.parameters)
        ~flavor_of_string:(fun s ->
          int_of_flavor (Static.flavor_of_string s))
        ~flavor_to_string:(lift_flavor Static.flavor_to_string)
        ~flavor_symbol:(lift_flavor Static.flavor_symbol)
        ~gauge_symbol:(lift_gauge Static.gauge_symbol)
        ~mass_symbol:(lift_flavor Static.mass_symbol)
        ~width_symbol:(lift_flavor Static.width_symbol)
        ~constant_symbol:(lift_constant Static.constant_symbol)
  end

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
