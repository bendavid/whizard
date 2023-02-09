(* thoGDraw.mli --

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

(* \thocwmodulesection{Tracking Display Sizes} *)

(* Tracking [size_allocate] signals is required for drawing methods that need to
   know the size of the drawable in question.  *)
class type resizeable =
  object
    method size_allocate : callback:(Gtk.rectangle -> unit) -> GtkSignal.id
  end

class size : #resizeable ->
  object
    method width : int
    method height : int
  end

(* The need for the type parameter ['b] in the following is ever so
   slightly nonintuitive.  If it were absent
   (i.\,e.~[method connect : #resizeable]), the free [..] in
   [#resizeable] would be unbound. *)
class type ['a, 'b] window =
  object
    method window : 'a Gdk.drawable
    method realize : unit -> unit
    method connect : 'b
    constraint 'b = #resizeable
  end

(* \thocwmodulesection{Coordinate Systems} *)

(* \begin{figure}
     \begin{center}
       \begin{picture}(120,60)
         \put(  0, 0){\framebox(120,60){}}
         \put( 20,20){\framebox(80,20){}}
         \put( 20,20){\thocwmakebox{0}{0}{bl}{[(x_min,y_min)]}}
         \put(100,20){\thocwmakebox{0}{0}{br}{[(x_max,y_min)]}}
         \put(100,40){\thocwmakebox{0}{0}{tr}{[(x_max,y_max)]}}
         \put( 20,40){\thocwmakebox{0}{0}{tl}{[(x_min,y_max)]}}
         \put( 60,40){\thocwmakebox{0}{0}{b}{[x_delta_pxl]}}
         \put( 20,40){\thocwmakebox{0}{0}{bl}{[x_min_pxl]}}
         \put(100,40){\thocwmakebox{0}{0}{br}{[x_max_pxl]}}
         \put(100,30){\thocwmakebox{0}{0}{l}{[y_delta_pxl]}}
         \put(100,40){\thocwmakebox{0}{0}{tl}{[y_min_pxl]}}
         \put(100,20){\thocwmakebox{0}{0}{bl}{[y_max_pxl]}}
         \put( 20,10){\thocwmakebox{0}{0}{r}{[left_margin]}}
         \put(100,10){\thocwmakebox{0}{0}{l}{[right_margin]}}
         \put( 60,10){\thocwmakebox{0}{0}{c}{[bottom_margin]}}
         \put( 60,50){\thocwmakebox{0}{0}{c}{[top_margin]}}
       \end{picture}
     \end{center}
     \caption{\label{fig:coord}%
       Coordinate systems.}
   \end{figure}
   The tracking of [size_allocate] signals is even more important for mapping
   world (abstract) coordinates to device (pixel) coordinates.  See
   figure~\ref{fig:coord} for the semantics of the device (pixel) and
   logical (floating point) coordinates.  Note that the logical
   coordinates follow mathematical conventions instead of the computer
   graphics conventions.  *) 

class coordinates : ?margins:int ->
  ?xrange:(float * float) -> ?yrange:(float * float) -> #resizeable ->
  object
    method left_margin : int -> unit
    method right_margin : int -> unit
    method bottom_margin : int -> unit
    method top_margin : int -> unit
    method margins : int -> unit
    method xrange : float -> float -> unit
    method yrange : float -> float -> unit
  end

(* There are more private methods, that are in fact more interesting.  In
   particular [project_x], [project_x], and [project] that map from logical
   to device coordinates.  *)

(* \thocwmodulesection{Viewports} *)

(* Useful string drawing requires flexible facilities for specifying the
   alignment.  Here, we can either center the string or specify distances
   from a reference point in pixels.  *)
type horiz = HCenter | Left of int | Right of int
type vert = VCenter | Below of int | Above of int

class decoration_context :
  object
    method font : Gdk.font
    method font_name : string
    method line_width : int
    method arrowhead_tip : int
    method arrowhead_base : int
    method arrowhead_width : int
    method wiggle_amp : int
    method wiggle_len : int
    method wiggle_res : int
    method curl_amp : int
    method curl_len : int
    method curl_res : int
    method set_font : string -> unit
    method set_line_width : int -> unit
    method set_arrowhead_tip : int -> unit
    method set_arrowhead_base : int -> unit
    method set_arrowhead_width : int -> unit
    method set_wiggle_amp : int -> unit
    method set_wiggle_len : int -> unit
    method set_wiggle_res : int -> unit
    method set_curl_amp : int -> unit
    method set_curl_len : int -> unit
    method set_curl_res : int -> unit
    method to_channel : out_channel -> unit
    method of_stream : char Stream.t -> unit
    method save : unit -> unit
    method restore : unit -> unit
  end

class ['a] decorations : ?colormap:Gdk.colormap ->
  decoration_context -> 'a Gdk.drawable ->
    object
      inherit ['a] GDraw.drawable
      method decoration_context : decoration_context
      method set_decoration_context : decoration_context -> unit
      method aligned_string : ?font:Gdk.font -> ?align:(horiz * vert) ->
        string -> int * int -> unit
      method arrowhead : int * int -> int * int -> unit
      method double : int * int -> int * int -> unit
      method wiggles : int * int -> int * int -> unit
      method curls : int * int -> int * int -> unit
    end

(* When we keep track of the size, we can easily provide an extension
   of [GDraw.drawable] that knows how to clear itself to a given background
   color.  *)

class ['a] drawable : ?colormap:Gdk.colormap ->
  decoration_context -> ('a, 'b) #window ->
    object
      inherit ['a] decorations
      method clear : ?color:GDraw.color -> unit -> unit
    end

(* \begin{dubious}
     Conceptually, [['a] decorations] and [['a] decorations] should be
     orthogonal and be implemented by aggregation.  Unfortunately,
     using [GDraw.drawable] with aggregation is complicated by
     the fact that each object has its own graphics context [Gdk.GC].
   \end{dubious} *)

(* The ['a viewport] (where ['a] will mostly be [[`window]], but can
   also be [[`pixmap]] or [[`bitmap]]) is an abstraction of ['a drawable],
   with both coordinates running in $0\ldots1$ instead of physical
   pixel numbers.  *)

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

class ['a] viewport : ?colormap:Gdk.colormap -> ?margins:int ->
  ?xrange:(float * float) -> ?yrange:(float * float) ->
    decoration_context -> ('a, 'b) #window ->
      object
        inherit coordinates
        method drawable : 'a drawable
        method point : float * float -> unit
        method points : (float * float) list -> unit
        method arc : ?filled:bool -> ?start:float -> ?angle:float ->
          int * int -> float * float -> unit
        method line : float * float -> float * float -> unit
        method lines : (float * float) list -> unit
        method segments : ((float * float) * (float * float)) list -> unit
        method polygon : ?filled:bool -> (float * float) list -> unit
        method string : ?font:Gdk.font -> ?align:(horiz * vert) ->
          string -> float * float -> unit
        method propagator : line_style -> float * float -> float * float -> unit
      end

(* \thocwmodulesection{Diagram Displays} *)

class ['a, 'edge, 'node] diagram_display :
    node_to_string:('node -> string) ->
      conjugate:('node -> 'node) -> cross:('node -> 'node) ->
        nodes2edge:('node -> 'node -> 'edge) ->
          line_style:('edge -> line_style) ->
            ?label:string -> ?width:int -> ?height:int ->
              ?packing:(GObj.widget -> unit) -> decoration_context ->
                object
                  method viewport : 'a viewport
                  method event : GObj.event_ops
                  method set_label : string -> unit
                  method set_diagram :
                      'node * ('node, 'node) Tree.t *
                      (unit, 'node) Color.amplitude -> unit
                  method clear_diagram : unit -> unit
                  method redraw : unit -> unit
                end

(* \thocwmodulesection{Preferences} *)

class ['a] demo_diagram_display :
  line_style:line_style -> ?label:string ->
    ?width:int -> ?height:int -> ?packing:(GObj.widget -> unit) ->
      decoration_context ->
        object
          method redraw : unit -> unit
        end

val edit_preferences : decoration_context -> unit

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
