(* thoGButton.ml --

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

(* \begin{dubious}
     Multiple inheritance from [GButton.button] and [GMisc.label] won't
     typecheck because [GButton.button_signals] and [GObj.widget_signals]
     don't match.
   \end{dubious}
   \begin{dubious}
     Instead of [GtkBase.Object.try_cast], we could use
     [GtkBase.Object.unsafe_cast]
   \end{dubious} *)

class mutable_button (button, label) =
  object (self)
    inherit GButton.button button
    val label : GMisc.label = label
    method set_text = label#set_text
  end

(* It remains to provide the semantics.  Ask \texttt{GTK+} to create a
   pair consisting of a button and \emph{included} label. *)

let mutable_button_raw ?text ?border_width ?width ?height ?packing ?show () =
  let button = GButton.button ?border_width ?width ?height ?packing ?show () in
  let hbox = GPack.hbox ~packing:button#add () in
  let label = GMisc.label ?text ~packing:(hbox#pack ~expand:true) () in
  (GtkBase.Object.unsafe_cast button#as_widget, label)

(* Finally, wrap it in the object. *)

let mutable_button ?text ?border_width ?width ?height ?packing ?show () =
  new mutable_button
    (mutable_button_raw
       ?text ?border_width ?width ?height ?packing ?show ())

(* If we need more state then just a changing label, we can do this
   polymorphically by inheritance.  *)

class ['a] stateful_button widgets format state =
  object (self)
    inherit mutable_button widgets
    val mutable state : 'a = state
    method private update_text = self#set_text (format state)
    method state = state
    method set_state s = (state <- s; self#update_text)
    initializer self#update_text
  end

let stateful_button format state
    ?text ?border_width ?width ?height ?packing ?show () =
  new stateful_button (mutable_button_raw
                         ?text ?border_width ?width ?height ?packing ?show ())
    format state

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
