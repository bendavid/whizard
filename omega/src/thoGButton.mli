(* thoGButton.mli --

   Copyright (C) 1999-2019 by

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

(* Plain [GButton.button]s have an immutable label.  We can remedy this
   situation by adding an explicit label and exporting its [set_text]
   method. *)

class mutable_button : Gtk.button Gtk.obj * GMisc.label ->
  object
    inherit GButton.button
    method set_text : string -> unit
  end

val mutable_button_raw :
    ?text:string -> ?border_width:int -> ?width:int -> ?height:int ->
      ?packing:(GObj.widget -> unit) ->
        ?show:bool -> unit -> Gtk.button Gtk.obj * GMisc.label

val mutable_button :
    ?text:string -> ?border_width:int -> ?width:int -> ?height:int ->
      ?packing:(GObj.widget -> unit) -> ?show:bool -> unit -> mutable_button

class ['a] stateful_button : Gtk.button Gtk.obj * GMisc.label ->
  ('a -> string) -> 'a ->
    object
      inherit mutable_button
      method state : 'a
      method set_state : 'a -> unit
    end

val stateful_button : ('a -> string) -> 'a ->
  ?text:string -> ?border_width:int -> ?width:int -> ?height:int ->
    ?packing:(GObj.widget -> unit) -> ?show:bool -> unit -> 'a stateful_button

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
