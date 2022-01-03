(* thoGMenu.ml --

   Copyright (C) 1999-2022 by

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

let rec submenu_tree accept = function
  | Leafs choices ->
      let menu = GMenu.menu () in
      List.iter (fun (label, choice) ->
        let item = GMenu.menu_item ~label ~packing:menu#append () in
        ignore (item#connect#activate
                  ~callback:(fun () -> accept choice))) choices;
      menu
  | Branches choices ->
      let menu = GMenu.menu () in
      List.iter (fun (label, choices') ->
        let item = GMenu.menu_item ~label ~packing:menu#append () in
        item#set_submenu (submenu_tree accept choices')) choices;
      menu

let tree_of_nested_lists format nested =
  Branches (List.map (fun (label, sub_menus) ->
    (label, Leafs (List.map (fun o -> (format o, o)) sub_menus))) nested)

(* We can either build the menus at startup (or immediately after
   model selection) or build them when the button is clicked.  There
   appears to be no noticeable performance difference. *)

class virtual ['a] menu_button widgets format state menu =
  object (self)
    inherit ['a] ThoGButton.stateful_button widgets format state
    method virtual set_menu : 'a menu_tree -> unit
    initializer self#set_menu menu
  end

class type ['a] menu_button_type = 
  object
    inherit ['a] menu_button
    method set_menu : 'a menu_tree -> unit
  end

(* \begin{dubious}
     [class type ['a] menu_button_type = ['a] ThoGMenu.menu_button_type] does
     \emph{not} work!
   \end{dubious} *)

class ['a] menu_button_immediate widgets format inistate menu =
  object (self)
    inherit ['a] menu_button widgets format inistate menu
    method set_menu menu =
      let m = submenu_tree self#set_state menu in
      self#connect#clicked ~callback:(fun () -> m#popup ~button:3 ~time:0);
      ()
  end

class ['a] menu_button_delayed widgets format state menu =
  object (self)
    inherit ['a] menu_button widgets format state menu
    method set_menu menu =
      self#connect#clicked ~callback:(fun () ->
        let m = submenu_tree self#set_state menu in
        m#popup ~button:3 ~time:0);
      ()
  end

let menu_button format state menu
    ?border_width ?width ?height ?packing ?show () =
  new menu_button_delayed (ThoGButton.mutable_button_raw
                             ?border_width ?width ?height ?packing ?show ())
    format state menu

(* Select tuples of similar objects.  *)

class ['a] tensor_menu format state menu n ?label ?tooltip_maker
    ?border_width ?width ?height ?packing ?show () =
  let frame = GBin.frame ?label ?packing ?show () in  
  let hbox = GPack.hbox ~packing:frame#add ?show () in
  let tooltips =
    match tooltip_maker with
    | None -> None
    | Some maker -> Some (GData.tooltips (), maker) in
  let buttons =
    Array.init n (fun i ->
      let mb = menu_button format state menu
          ?width ?height ~packing:(hbox#pack ~expand:false) ?show () in
      begin match tooltips with
      | None -> ()
      | Some (widget, maker) -> widget#set_tip mb#coerce ~text:(maker i)
      end;
      mb) in
  object (self)
    val frame = frame
    val mutable buttons : 'a menu_button array = buttons
    val mutable active = n
    method frame = frame
    method set_menu menu =
      Array.iter (fun b -> b#set_menu menu) buttons
    method set_active n =
      active <- n;
      Array.iteri (fun i b -> b#misc#set_sensitive (i < active)) buttons
    method states =
      Array.map (fun b -> b#state) (Array.sub buttons 0 active)
  end

class ['a] factory ?accel_group ?accel_modi ?accel_flags menu_shell =
  object (self)
    inherit ['a] GMenu.factory
        ?accel_group ?accel_modi ?accel_flags menu_shell
    method add_submenu_right ?key label =
      let item = GMenu.menu_item ~label () in
      item#right_justify ();
      self#bind item ?key;
      GMenu.menu ~packing:item#set_submenu ()
end

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
