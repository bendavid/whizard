(* thoGDraw.ml --

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

(* \thocwmodulesection{Tracking Display Sizes} *)

class type resizeable =
  object
    method size_allocate : callback:(Gtk.rectangle -> unit) -> GtkSignal.id
  end

class size signals=
  object (self)

    val mutable width = -1
    val mutable height = -1

    method width = width
    method height = height

    method private resize w h =
      width <- w;
      height <- h

    initializer
      let (_ : GtkSignal.id) = signals#size_allocate
          ~callback:(fun evt -> self#resize evt.Gtk.width evt.Gtk.height) in
      ()
  end

class type ['a, 'b] window =
  object
    method window : 'a Gdk.drawable
    method realize : unit -> unit
    method connect : 'b
    constraint 'b = #resizeable
  end

(* \thocwmodulesection{Coordinate Systems} *)

(* We could try to jump through hoops and inherit from [size], but it is much
   simpler just to repeat the few lines of code.  *)

class coordinates ?(margins = 0)
    ?(xrange = (0.0, 1.0)) ?(yrange = (0.0, 1.0)) signals =
  object (self)

(* ``Input'' parameters: *)
    val mutable width = -1
    val mutable height = -1

    val mutable x_min = fst xrange
    val mutable x_max = snd xrange
    val mutable y_min = fst yrange
    val mutable y_max = snd yrange

    val mutable left_margin = margins
    val mutable right_margin = margins
    val mutable bottom_margin = margins
    val mutable top_margin = margins

(* Derived parameters: *)
    val mutable x_min_pxl = 0
    val mutable x_max_pxl = 100
    val mutable x_delta_pxl = 100
    val mutable y_min_pxl = 0
    val mutable y_max_pxl = 100
    val mutable y_delta_pxl = 100

    val mutable x_delta = 1.0
    val mutable y_delta = 1.0

    val mutable x_pxl_per_unit = 100.0
    val mutable y_pxl_per_unit = 100.0

    method private update =
      x_min_pxl <- left_margin;
      x_max_pxl <- width - right_margin;
      x_delta_pxl <- x_max_pxl - x_min_pxl;
      x_delta <- x_max -. x_min;
      x_pxl_per_unit <- float x_delta_pxl /. x_delta;
      y_min_pxl <- top_margin;
      y_max_pxl <- height - bottom_margin;
      y_delta_pxl <- y_max_pxl - y_min_pxl;
      y_delta <- y_max -. y_min;
      y_pxl_per_unit <- float y_delta_pxl /. y_delta

(* The [resize] method is only called from signal handlers that
   respond to external size changes.  *)

    method private resize w h =
      width <- w; height <- h;
      self#update

    method left_margin m =
      left_margin <- m;
      self#update

    method right_margin m =
      right_margin <- m;
      self#update

    method bottom_margin m =
      bottom_margin <- m;
      self#update

    method top_margin m =
      top_margin <- m;
      self#update

    method margins m =
      left_margin <- m;
      right_margin <- m;
      bottom_margin <- m;
      top_margin <- m;
      self#update
      
    method xrange x0 x1 =
      x_min <- x0; x_max <- x1;
      self#update

    method yrange y0 y1 =
      y_min <- y0; y_max <- y1;
      self#update

    method private x_pxl_per_unit =
      x_pxl_per_unit

    method private y_pxl_per_unit =
      y_pxl_per_unit

    method private project_x x =
      x_min_pxl + truncate (x_pxl_per_unit *. (x -. x_min))

    method private project_y y =
      y_max_pxl - truncate (y_pxl_per_unit *. (y -. y_min))

    method private project (x, y) =
      (self#project_x x, self#project_y y)

    initializer
      let (_ : GtkSignal.id) = signals#size_allocate
          ~callback:(fun evt -> self#resize evt.Gtk.width evt.Gtk.height) in
      self#update
  end

(* \thocwmodulesection{Viewports} *)

let config_file_name = ".ogiga"

let default_font_name =
  "-*-*-*-r-*-*-*-120-*-*-m-*-*-*"

let out_comment oc comment =
  Printf.fprintf oc "(* %s *)\n" comment

let out_string_parameter oc name value =
  Printf.fprintf oc "%s = \"%s\"\n" name value

let out_int_parameter oc name value =
  Printf.fprintf oc "%s = %d\n" name value

class decoration_context =
  object (self)

    val mutable font_name = default_font_name
    val mutable font = Gdk.Font.load default_font_name
    val mutable line_width = 2
    val mutable arrowhead_tip = 8
    val mutable arrowhead_base = 5
    val mutable arrowhead_width = 4
    val mutable wiggle_amp = 3
    val mutable wiggle_len = 10
    val mutable wiggle_res = 1
    val mutable curl_amp = 5
    val mutable curl_len = 10
    val mutable curl_res = 1

    method font = font
    method font_name = font_name
    method line_width = line_width
    method arrowhead_tip = arrowhead_tip
    method arrowhead_base = arrowhead_base
    method arrowhead_width = arrowhead_width
    method wiggle_amp = wiggle_amp
    method wiggle_len = wiggle_len
    method wiggle_res = wiggle_res
    method curl_amp = curl_amp
    method curl_len = curl_len
    method curl_res = curl_res

    method set_font name =
      font_name <- name;
      font <- Gdk.Font.load font_name
    method set_line_width n = line_width <- n
    method set_arrowhead_tip n = arrowhead_tip <- n
    method set_arrowhead_base n = arrowhead_base <- n
    method set_arrowhead_width n = arrowhead_width <- n
    method set_wiggle_amp n = wiggle_amp <- n
    method set_wiggle_len n = wiggle_len <- n
    method set_wiggle_res n = wiggle_res <- n
    method set_curl_amp n = curl_amp <- n
    method set_curl_len n = curl_len <- n
    method set_curl_res n = curl_res <- n

    method to_channel oc =
      out_comment oc "O'Giga decoration options";
      out_string_parameter oc "font" font_name;
      out_int_parameter oc "line_width" line_width;
      out_int_parameter oc "arrowhead_tip" arrowhead_tip;
      out_int_parameter oc "arrowhead_base" arrowhead_base;
      out_int_parameter oc "arrowhead_width" arrowhead_width;
      out_int_parameter oc "wiggle_amp" wiggle_amp;
      out_int_parameter oc "wiggle_len" wiggle_len;
      out_int_parameter oc "wiggle_res" wiggle_res;
      out_int_parameter oc "curl_amp" curl_amp;
      out_int_parameter oc "curl_len" curl_len;
      out_int_parameter oc "curl_res" curl_res

    method save () =
      let oc = open_out config_file_name in
      self#to_channel oc;
      close_out oc

    method of_stream stream =
      let tokens = Genlex.make_lexer ["="] stream in
      let junk3 () =
        Stream.junk tokens;
        Stream.junk tokens;
        Stream.junk tokens in
      let rec process () =
        match Stream.npeek 3 tokens with
        | [] -> ()
        | [Genlex.Ident name; Genlex.Kwd "="; Genlex.String value] ->
            begin match name with
            | "font" -> self#set_font value
            | _ -> invalid_arg "invalid string variable in configuration file"
            end;
            junk3 ();
            process ()
        | [Genlex.Ident name; Genlex.Kwd "="; Genlex.Int value] ->
            begin match name with
            | "line_width" -> self#set_line_width value
            | "arrowhead_tip" -> self#set_arrowhead_tip value
            | "arrowhead_base" -> self#set_arrowhead_base value
            | "arrowhead_width" -> self#set_arrowhead_width value
            | "wiggle_amp" -> self#set_wiggle_amp value
            | "wiggle_len" -> self#set_wiggle_len value
            | "wiggle_res" -> self#set_wiggle_res value
            | "curl_amp" -> self#set_curl_amp value
            | "curl_len" -> self#set_curl_len value
            | "curl_res" -> self#set_curl_res value
            | _ -> invalid_arg "invalid integer variable in configuration file"
            end;
            junk3 ();
            process ()
        | _ -> invalid_arg "parse error in configuration file" in
      process ()

    method restore () =
      if Sys.file_exists config_file_name then
        let ic = open_in config_file_name in
        self#of_stream (Stream.of_channel ic);
        close_in ic

    initializer
      self#restore ()

  end

type horiz = HCenter | Left of int | Right of int
type vert = VCenter | Below of int | Above of int

let align_horiz align w x =
  match align with
  | Right dx -> x + dx
  | Left dx -> x - w - dx
  | HCenter -> x - w / 2
  
let align_vert align h y =
  match align with
  | Above dy -> y - dy
  | Below dy -> y + h + dy
  | VCenter -> y + h / 2
  
let align_box (horiz, vert) (w, h) (x,y) =
  (align_horiz horiz w x, align_vert vert h y)

let pixels ~pos (x0, y0) (x1, y1) (along, perp) =
  let dx = float (x1 - x0)
  and dy = float (y1 - y0) in
  let d = sqrt (dx *. dx +. dy *. dy) in
  let along' = pos +. float along /. d
  and perp' = float perp /. d in
  (x0 + truncate (along' *. dx -. perp' *. dy),
   y0 + truncate (along' *. dy +. perp' *. dx))

let pixel_shape ~pos (x0, y0) (x1, y1) shape =
  List.map (pixels ~pos:0.5 (x0, y0) (x1, y1)) shape

let two_pi = 4.0 *. asin 1.0

class ['a] decorations ?colormap (dc : decoration_context) obj =
  object (self)

    val mutable dc = dc

    inherit ['a] GDraw.drawable ?colormap obj as drawable

    method decoration_context = dc
    method set_decoration_context dc' = dc <- dc'

    method aligned_string ?(font = dc#font)
        ?(align = (HCenter, VCenter)) s xy =
      let x', y' =
        align_box align
          (Gdk.Font.string_width font s, Gdk.Font.string_height font s) xy in
      self#string s ~font ~x:x' ~y:y'

    method arrowhead (x0, y0) (x1, y1) =
      self#polygon ~filled:true
        (pixel_shape ~pos:0.5 (x0, y0) (x1, y1)
           [(dc#arrowhead_tip, 0);
            (-dc#arrowhead_base, dc#arrowhead_width);
            (-dc#arrowhead_base, -dc#arrowhead_width)])

    method double (x0, y0) (x1, y1) =
      let gc = drawable#gc_values in
      let w = gc.Gdk.GC.line_width in
      self#polygon ~filled:false
        [pixels ~pos:0.0 (x0, y0) (x1, y1) (0, w);
         pixels ~pos:1.0 (x0, y0) (x1, y1) (0, w);
         pixels ~pos:1.0 (x0, y0) (x1, y1) (0, -w);
         pixels ~pos:0.0 (x0, y0) (x1, y1) (0, -w)]

    method wiggles (x0, y0) (x1, y1) =
      let amplitude = dc#wiggle_amp
      and step = dc#wiggle_len in
      let dx = float (x1 - x0)
      and dy = float (y1 - y0) in
      let d = sqrt (dx *. dx +. dy *. dy) in
      let num_steps = ceil (d /. float step) in
      let step = d /. num_steps in
      let amplitude = float amplitude in
      let xy along perp =
        let along' = along /. d
        and perp' = perp *. amplitude /. d in
        (x0 + truncate (along' *. dx -. perp' *. dy),
         y0 + truncate (along' *. dy +. perp' *. dx)) in
      let rec wiggles' t =
        if t <= 0.0 then
          [xy 0.0 0.0]
        else
          xy t (sin (t *. two_pi /. step)) :: wiggles' (t -. step /. 10.0) in
      self#lines (wiggles' d)

    method curls (x0, y0) (x1, y1) =
      let amplitude = dc#curl_amp
      and step = dc#curl_len in
      let dx = float (x1 - x0)
      and dy = float (y1 - y0) in
      let d = sqrt (dx *. dx +. dy *. dy) in
      let num_steps = ceil (d /. float step) in
      let step = d /. num_steps in
      let amplitude = float amplitude in
      let xy along perp =
        let along' = along /. d
        and perp' = perp *. amplitude /. d in
        (x0 + truncate (along' *. dx -. perp' *. dy),
         y0 + truncate (along' *. dy +. perp' *. dx)) in
      let rec curls' t =
        if t <= 0.0 then
          [xy 0.0 0.0]
        else
          xy (t +. step /. 2.0 *. cos (t *. two_pi /. step)) (sin (t *. two_pi /. step))
          :: curls' (t -. step /. 10.0) in
      self#lines (curls' d)

  end

class ['a] drawable ?colormap dc misc =
  let () = misc#realize () in
  object (self)

    inherit ['a] decorations ?colormap dc misc#window as drawable
    val size = new size misc#connect

    method clear ?(color = `WHITE) () =
      drawable#set_foreground color;
      drawable#rectangle ~filled:true
        ~x:0 ~y:0 ~width:size#width ~height:size#height ()

  end

type direction =
  | Forward
  | Backward

type line_style =
  | Plain
  | Double
  | Wiggles
  | Curls
  | Dashes
  | Dots
  | Arrow of direction
  | Name of string

class ['a] viewport ?colormap ?margins ?xrange ?yrange dc misc =
  let () = misc#realize () in
  object (self)

    inherit coordinates ?margins ?xrange ?yrange misc#connect

    val drawable = new drawable ?colormap dc misc

    method drawable = (drawable : 'a drawable)

    method arc ?filled ?start ?angle (width, height) (x, y) =
      drawable#arc
        ~x:(self#project_x x - width/2) ~y:(self#project_y y - height/2)
        ~width ~height ?filled ?start ?angle ()

    method point (x, y) =
      drawable#point ~x:(self#project_x x) ~y:(self#project_y y)

    method points xy =
      drawable#points (List.map self#project xy)

    method line (x0, y0) (x1, y1) =
      drawable#line
        ~x:(self#project_x x0) ~y:(self#project_y y0)
        ~x:(self#project_x x1) ~y:(self#project_y y1)

    method lines xy =
      drawable#lines (List.map self#project xy)

    method segments xyxy =
      drawable#segments
        (List.map (fun (xy0, xy1) -> (self#project xy0, self#project xy1)) xyxy)

    method polygon ?filled xy =
      drawable#polygon ?filled (List.map self#project xy)

    method string ?font ?align s xy =
      drawable#aligned_string ?font ?align s (self#project xy)

    method propagator line_style (x0, y0 as xy0) (x1, y1 as xy1) =
      match line_style with
      | Arrow Forward ->
          self#line xy0 xy1;
          drawable#arrowhead (self#project xy0) (self#project xy1)
      | Arrow Backward ->
          self#line xy0 xy1;
          drawable#arrowhead (self#project xy1) (self#project xy0)
      | Plain ->
          self#line xy0 xy1
      | Double ->
          drawable#double (self#project xy0) (self#project xy1)
      | Wiggles ->
          drawable#wiggles (self#project xy0) (self#project xy1)
      | Curls ->
          drawable#curls (self#project xy0) (self#project xy1)
      | Dashes ->
          self#line xy0 xy1;
          drawable#set_foreground (`NAME "red");
          self#string "dashes" (0.5 *. (x0 +. x1), 0.5 *. (y0 +. y1))
      | Dots ->
          self#line xy0 xy1;
          drawable#set_foreground (`NAME "red");
          self#string "dots" (0.5 *. (x0 +. x1), 0.5 *. (y0 +. y1))
      | Name name ->
          self#line xy0 xy1;
          drawable#set_foreground (`NAME "red");
          self#string name (0.5 *. (x0 +. x1), 0.5 *. (y0 +. y1))
  
  end

(* \thocwmodulesection{Diagram Displays} *)

let to_string format tree =
  Tree.to_string (Tree.map format (fun _ -> "") tree)

let layout2 nodes2edge conjugate wf2 tree =
  Tree.layout (Tree.left_to_right 2
                 (Tree.graph_of_tree nodes2edge conjugate wf2 tree))

class ['a, 'edge, 'node] diagram_display
    ~node_to_string ~conjugate ~cross ~nodes2edge ~line_style
    ?label ?width ?height ?packing dc =
  let event_box = GBin.event_box ~border_width:0 ?packing () in
  let frame = GBin.frame ?label ~packing:event_box#add () in  
  let area = GMisc.drawing_area ?width ?height ~packing:frame#add () in
  let vp = new viewport dc area#misc in
  let _ =
    vp#left_margin 50;
    vp#right_margin 50;
    vp#bottom_margin 10;
    vp#top_margin 10 in
  object (self)

    val mutable diagram :
        ('node * ('node, 'node) Tree.t * (unit, 'node) Color.amplitude) option = None

    val mutable label =
      match label with
      | Some s -> s
      | None -> ""

    method set_label s =
      label <- s;
      frame#set_label label

    method viewport = (vp : 'a viewport)
    method event = event_box#event

    method redraw () =
      vp#drawable#clear ();
      begin match diagram with
      | Some (wf2, t, c) ->
          let d = layout2 nodes2edge cross wf2 t in
          vp#drawable#set_line_attributes
            ~width:vp#drawable#decoration_context#line_width ();
          vp#drawable#set_foreground `BLACK;
          Tree.iter_edges
            (fun flavor xy0 xy1 -> vp#propagator (line_style flavor) xy0 xy1) d;
          vp#drawable#set_foreground `BLACK;
          Tree.iter_internal (vp#arc ~filled:true (6, 6)) d;
          Tree.iter_incoming (fun (ext, x, y) ->
            vp#string ~align:(Left 5, VCenter)
              (node_to_string ext) (x, y)) d;
          Tree.iter_outgoing (fun (ext, x, y) ->
            vp#string ~align:(Right 5, VCenter)
              (node_to_string (conjugate ext)) (x, y)) d
      | None -> ()
      end

    method private popup evt =
      begin match diagram with
      | Some (wf2, t, c) ->
          begin match GdkEvent.Button.button evt with
          | 2 ->
              ThoGWindow.message ~title:"O'Giga Color Diagram" ~justify:`LEFT
                ~text:(label ^ ":\n\n" ^
                       Color.to_string (fun () -> "") node_to_string c) ()
          | 3 ->
              ThoGWindow.message ~title:"O'Giga Diagram" ~justify:`LEFT
                ~text:(label ^ ":\n\n" ^ to_string node_to_string t) ()
          | _ -> ()
          end
      | None -> ()
      end

    method clear_diagram () =
      diagram <- None;
      self#redraw ()

    method set_diagram d =
      diagram <- (Some d);
      self#redraw ()

    initializer
      area#event#connect#expose ~callback:(fun evt -> self#redraw (); true);
      self#event#connect#button_press ~callback:(fun evt -> self#popup evt; true);
      self#redraw ()

  end

(* \thocwmodulesection{Preferences} *)

class ['a] demo_diagram_display ~line_style ?label ?width ?height ?packing dc =
  let frame = GBin.frame ?label ?packing () in  
  let area = GMisc.drawing_area ?width ?height ~packing:frame#add () in
  let vp = new viewport ~margins:10 dc area#misc in
  object (self)

    val xy0 = (0.0, 0.5)
    val xy1 = (1.0, 0.5)
        
    method redraw () =
      vp#drawable#clear ();
      vp#drawable#set_line_attributes ~width:dc#line_width ();
      vp#drawable#set_foreground `BLACK;
      vp#propagator line_style xy0 xy1;
      vp#arc ~filled:true (6, 6) xy0;
      vp#arc ~filled:true (6, 6) xy1

    initializer
      area#event#connect#expose ~callback:(fun evt -> self#redraw (); true);
      self#redraw ()

  end

let int_adjustment value (lower, upper) =
  GData.adjustment ~value:(float value)
    ~lower:(float lower) ~upper:(float upper) ~step_incr:1.0
    ~page_incr:10.0 ~page_size:5.0 ()

let notebook_page text (notebook : GPack.notebook) =
  GPack.table ~rows:4 ~columns:4 ~row_spacings:8 ~col_spacings:8
    ~packing:(notebook#append_page ~tab_label:(GMisc.label ~text ())#coerce) ()

let int_edit ?width ?changed text value range (table : GPack.table) row =
  GMisc.label ?width ~justify:`RIGHT ~text:(text ^ ":")
    ~packing:(table#attach ~left:1 ~top:row ~expand:`X) ();
  let spin_button =
    GEdit.spin_button
      ~adjustment:(int_adjustment value range) ~numeric:true ~digits:0
      ~packing:(table#attach ~left:2 ~top:row ~expand:`NONE) () in
  begin match changed with
  | None -> ()
  | Some f ->
      ignore (spin_button#connect#changed
                ~callback:(fun () -> f spin_button#value_as_int))
  end;
  spin_button

let edit_preferences dc =

  let window =
    GWindow.window ~title:"O'Giga Preferences" ~border_width:5 () in
  let hbox = GPack.hbox ~spacing:8 ~packing:window#add () in
  let input = GPack.vbox ~spacing:8 ~packing:hbox#add () in
  let monitor = GPack.vbox ~spacing:8 ~packing:hbox#add () in

  let width = 150
  and height = 30 in
  let fermion =
    new demo_diagram_display ~line_style:(Arrow Forward)
      ~label:"Dirac fermions" ~width ~height ~packing:monitor#add dc in
  let antifermion =
    new demo_diagram_display ~line_style:(Arrow Backward)
      ~label:"Dirac antifermions" ~width ~height ~packing:monitor#add dc in
  let photon =
    new demo_diagram_display ~line_style:Wiggles
      ~label:"Color singlet gauge bosons" ~width ~height ~packing:monitor#add dc in
  let gluon =
    new demo_diagram_display ~line_style:Curls
      ~label:"Gluons" ~width ~height ~packing:monitor#add dc in
  let heavy =
    new demo_diagram_display ~line_style:Double
      ~label:"Heavy gauge bosons" ~width ~height ~packing:monitor#add dc in
  let redraw () =
    fermion#redraw ();
    antifermion#redraw ();
    photon#redraw ();
    gluon#redraw ();
    heavy#redraw () in

  let notebook = GPack.notebook ~scrollable:true ~homogeneous_tabs:true
      ~packing:(input#pack ~expand:true) () in

  let general = notebook_page "General" notebook in
  let line_width =
    int_edit ~changed:(fun n -> dc#set_line_width n; redraw ())
      "line width" dc#line_width (1, 10) general 1 in
  GMisc.label ~justify:`RIGHT ~text:("font:")
    ~packing:(general#attach ~left:1 ~top:2 ~expand:`X) ();
  let font_selection_button =
    GButton.button ~label:"Change"
      ~packing:(general#attach ~left:2 ~top:2 ~expand:`NONE) () in
  font_selection_button#connect#clicked
    ~callback:(fun evt ->
      let fsd = GWindow.font_selection_dialog ~title:"O'Giga Font Selection" () in
      fsd#selection#set_font_name dc#font_name;
      fsd#cancel_button#connect#clicked ~callback:fsd#destroy;
      fsd#ok_button#connect#clicked
        ~callback:(fun evt ->
          begin match fsd#selection#font_name with
          | Some name -> dc#set_font name
          | None -> ()
          end;
          fsd#destroy evt);
      fsd#show ());

  let arrows = notebook_page "Arrows" notebook in
  let ah_tip =
    int_edit ~changed:(fun n -> dc#set_arrowhead_tip n; redraw ())
      "arrowhead tip" dc#arrowhead_tip (1, 50) arrows 1 in
  let ah_base =
    int_edit ~changed:(fun n -> dc#set_arrowhead_base n; redraw ())
      "arrowhead base" dc#arrowhead_base (1, 40) arrows 2 in
  let ah_width =
    int_edit ~changed:(fun n -> dc#set_arrowhead_width n; redraw ())
      "arrowhead width" dc#arrowhead_width (1, 30) arrows 3 in

  let wiggles = notebook_page "Wiggles" notebook in
  let w_amp =
    int_edit ~changed:(fun n -> dc#set_wiggle_amp n; redraw ())
      "wiggle amplitude" dc#wiggle_amp (0, 20) wiggles 1 in
  let w_len =
    int_edit ~changed:(fun n -> dc#set_wiggle_len n; redraw ())
      "wiggle length" dc#wiggle_len (1, 50) wiggles 2 in
  let w_res =
    int_edit ~changed:(fun n -> dc#set_wiggle_res n; redraw ())
      "wiggle resolution" dc#wiggle_res (1, 50) wiggles 3 in

  let curls = notebook_page "Curls" notebook in
  let c_amp =
    int_edit ~changed:(fun n -> dc#set_curl_amp n; redraw ())
      "curl amplitude" dc#curl_amp (0, 20) curls 1 in
  let c_len =
    int_edit ~changed:(fun n -> dc#set_curl_len n; redraw ())
      "curl length" dc#curl_len (1, 50) curls 2 in
  let c_res =
    int_edit ~changed:(fun n -> dc#set_curl_res n; redraw ())
      "curl resolution" dc#curl_res (1, 50) curls 3 in

  let buttons =
    GPack.hbox ~spacing:8 ~packing:(input#pack ~expand:false) () in
  let ok_button =
    GButton.button ~label:"OK" ~packing:buttons#add () in
  let accept_button =
    GButton.button ~label:"Accept" ~packing:buttons#add () in
  let cancel_button =
    GButton.button ~label:"Cancel" ~packing:buttons#add () in
  cancel_button#connect#clicked ~callback:window#destroy;
  accept_button#connect#clicked
    ~callback:(fun evt -> ());
  ok_button#connect#clicked
    ~callback:(fun evt ->
      dc#save ();
      window#destroy evt);

  window#show ()

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
