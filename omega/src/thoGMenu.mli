(* thoGMenu.mli --

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

(* Lists of possible particles can be pretty long.  Therefore it is
   beneficial to present the choices hierarchically. *)

type 'a menu_tree =
  | Leafs of (string * 'a) list
  | Branches of (string * 'a menu_tree) list

val submenu_tree : ('a -> unit) -> 'a menu_tree -> GMenu.menu
val tree_of_nested_lists : ('a -> string) -> (string * 'a list) list -> 'a menu_tree

class virtual ['a] menu_button : Gtk.button Gtk.obj * GMisc.label ->
  ('a -> string) -> 'a -> 'a menu_tree ->
    object
      inherit ['a] ThoGButton.stateful_button
      method virtual set_menu : 'a menu_tree -> unit
    end

class type ['a] menu_button_type = 
  object
    inherit ['a] menu_button
    method set_menu : 'a menu_tree -> unit
  end

class ['a] menu_button_immediate : Gtk.button Gtk.obj * GMisc.label ->
  ('a -> string) -> 'a -> 'a menu_tree -> ['a] menu_button_type

class ['a] menu_button_delayed : Gtk.button Gtk.obj * GMisc.label ->
  ('a -> string) -> 'a -> 'a menu_tree -> ['a] menu_button_type

val menu_button : ('a -> string) -> 'a -> 'a menu_tree ->
  ?border_width:int -> ?width:int -> ?height:int ->
    ?packing:(GObj.widget -> unit) -> ?show:bool -> unit ->
      'a menu_button_delayed

class ['a] tensor_menu : ('a -> string) -> 'a -> 'a menu_tree -> int ->
  ?label:string -> ?tooltip_maker:(int -> string) ->
    ?border_width:'b -> ?width:int -> ?height:int ->
      ?packing:(GObj.widget -> unit) -> ?show:bool -> unit ->
        object
          val mutable active : int
          val mutable buttons : 'a menu_button array
          val frame : GBin.frame
          method frame : GBin.frame
          method set_active : int -> unit
          method set_menu : 'a menu_tree -> unit
          method states : 'a array
        end

(* This is the same as [GMenu.factory] but with the ability to
   add right justified menus; for Motif-style `Help' menus, for
   example. *)

class ['a] factory : ?accel_group:Gtk.accel_group ->
  ?accel_modi:Gdk.Tags.modifier list ->
    ?accel_flags:Gtk.Tags.accel_flag list -> 'a ->
      object
	inherit ['a] GMenu.factory
	method add_submenu_right :
            ?key:Gdk.keysym -> string -> GMenu.menu
      end

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
