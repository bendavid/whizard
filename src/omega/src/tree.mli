(* $Id: tree.mli 3670 2012-01-21 19:33:07Z jr_reuter $

   Copyright (C) 1999-2012 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
       Christian Speckner <christian.speckner@physik.uni-freiburg.de>

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

(* This module provides utilities for generic decorated trees, such as
   FeynMF output. *)

(* \thocwmodulesection{Abstract Data Type} *)
type ('n, 'l) t

(* [leaf n l] returns a tree consisting of a single leaf of type [n]
   connected to [l]. *)
val leaf : 'n -> 'l -> ('n, 'l) t

(* [cons n ch] returns a tree node. *)
val cons : 'n -> ('n, 'l) t list -> ('n, 'l) t

(* [node t] returns the top node of the tree [t]. *)
val node : ('n, 'l) t -> 'n

(* [leafs t] returns a list of all leafs \textit{in order}. *)
val leafs : ('n, 'l) t -> 'l list

(* [nodes t] returns a list of all nodes in post-order. This guarantees
   that the root node can be stripped from the result by [List.tl]. *)
val nodes :  ('n, 'l) t -> 'n list

(* [fuse conjg root contains_root trees] joins the [trees], using
   the leaf [root] in one of the trees as root of the new tree.
   [contains_root] guides the search for the subtree containing [root]
   as a leaf. [fun t -> List.mem root (leafs t)] is acceptable, but more
   efficient solutions could be available in special circumstances.  *)
val fuse : ('n -> 'n) -> 'l -> (('n, 'l) t -> bool) -> ('n, 'l) t list -> ('n, 'l) t

(* [sort lesseq t] return a sorted copy of the tree~[t]: node
   labels are ignored and nodes are according to the supremum of the
   leaf labels in the corresponding subtree. *)
val sort : ('l -> 'l -> bool) -> ('n, 'l) t -> ('n, 'l) t

(* \thocwmodulesection{Homomorphisms} *)
val map : ('n1 -> 'n2) -> ('l1 -> 'l2) -> ('n1, 'l1) t -> ('n2, 'l2) t
val fold : ('n -> 'l -> 'a) -> ('n -> 'a list -> 'a) -> ('n, 'l) t -> 'a
val fan : ('n -> 'l -> 'a list) -> ('n -> 'a list -> 'a list) ->
  ('n, 'l) t -> 'a list

(* \thocwmodulesection{Output} *)
val to_string : (string, string) t -> string

(* \thocwmodulesubsection{Feynmf} *)
(* \begin{dubious}
      [style : (string * string) option] should be replaced by
      [style : string option; tex_label : string option]
   \end{dubious} *)
type feynmf =
    { style : (string * string) option;
      rev : bool;
      label : string option;
      tension : float option } 
val vanilla : feynmf
val sty : (string * string) * bool * string -> feynmf

(* [to_feynmf file to_string i2 t] write the trees in the
   list~[t] to the file named~[file].  The leaf~[i2] is used as
   the second incoming particle and~[to_string] is use to convert
   leaf labels to \LaTeX-strings. *)
val to_feynmf : bool ref -> string -> ('l -> string) -> 'l -> (feynmf, 'l) t list -> unit

(* \thocwmodulesubsection{Least Squares Layout} *)

(* A general graph with edges of type~['e], internal nodes of type~['n],
   and external nodes of type ['ext].  *)
type ('e, 'n, 'ext) graph
val graph_of_tree : ('n -> 'n -> 'e) -> ('n -> 'n) ->
  'n -> ('n, 'n) t -> ('e, 'n, 'n) graph

(* A general graph with the layout of the external nodes fixed.  *)
type ('e, 'n, 'ext) ext_layout
val left_to_right : int -> ('e, 'n, 'ext) graph -> ('e, 'n, 'ext) ext_layout

(* A general graph with the layout of all nodes fixed.  *)
type ('e, 'n, 'ext) layout
val layout : ('e, 'n, 'ext) ext_layout -> ('e, 'n, 'ext) layout

val dump : ('e, 'n, 'ext) layout -> unit
val iter_edges : ('e -> float * float -> float * float -> unit) ->
  ('e, 'n, 'ext) layout -> unit
val iter_internal : (float * float -> unit) ->
  ('e, 'n, 'ext) layout -> unit
val iter_incoming : ('ext * float * float -> unit) ->
  ('e, 'n, 'ext) layout -> unit
val iter_outgoing : ('ext * float * float -> unit) ->
  ('e, 'n, 'ext) layout -> unit

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
