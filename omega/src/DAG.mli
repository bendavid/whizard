(* DAG.mli --

   Copyright (C) 1999-2025 by

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

(* This datastructure describes large collections of trees with
   many shared nodes.  The sharing of nodes is semantically irrelevant,
   but can turn a factorial complexity to exponential complexity.
   Note that [DAG] implements only a very specialized subset of Directed
   Acyclical Graphs (DAGs). *)

(* \thocwmodulesection{Forests} *)

(* A forest is a set of trees and we want to represent
   it efficiently by a DAG.  However, we will not handle arbitrary
   forests here, but only such forests, where \emph{all} subtrees of
   trees in the forest are also members of the forest.

   In this case, we can represent
   a forest~$F$ over a set of nodes and a set of edges
   as a map from the set of nodes~$N$ to the direct product
   of the set of edges~$E$ and the set
   \begin{equation}
    t(N) = \bigcup_{n=0}^\infty N^{\times n}
         = \emptyset \cup N \cup N\times N \cup N\times N\times N \cup \ldots
   \end{equation}
   of tuples of nodes augmented
   by a special element~$\bot$ (``bottom'').
   \begin{equation}
     \begin{aligned}
        F: N &\to (E \times t(N)) \cup \{\bot\} \\
           n &\mapsto \begin{cases}
                         \left(e, (n'_1,n'_2,\ldots)\right) \\
                         \bot
                      \end{cases}
     \end{aligned}
   \end{equation}
   Nodes that are mapped to~$\bot$ are called \emph{leaf} nodes and
   nodes that do not appear in any~$F(n)$ are called \emph{root}
   nodes.  There are as many trees in a given forest~$F$ as there are
   nodes.
   Our trees are Feynman tree diagrams and each forest~$F$
   consists of one diagram and its subdiagrams.

   For convenience, we require edges and nodes to be members of ordered
   sets.  If the nodes are ordered, cycles can be detected easily
   \begin{equation}
     \forall n\in N:
       \Bigl( \bigl( F(n) = (e, x) \bigr) \Rightarrow
              \bigl( \forall n'\in x: n > n' \bigr) \Bigr)\,.
   \end{equation}
   Note that this requirement does \emph{not} exclude any trees.
   Even if we consider only topological equivalence classes with
   anonymous nodes and edges, we can always construct a canonical
   labeling and order from the children of the nodes. E.\,g.~the depth
   of the tree beneath a node provides a suitable labeling for
   \emph{all} forests. However, in practical
   applications, we will often have more efficient labelings and
   orders at our disposal.

   The sematics of [compare] is expected to be compatible
   with [Pervasives.compare]
   (i.\,e.~[Stdlib.compare] on O'Caml 4.08 and later):
   \begin{equation}
      \ocwlowerid{compare}(x,y) =
        \begin{cases}
          -1 & \text{for $x<y$} \\
          0 & \text{for $x=y$} \\
          1 & \text{for $x>y$}
        \end{cases}
   \end{equation} *)

module type Ord =
  sig
    type t
    val compare : t -> t -> int
  end

module type Forest =
  sig

    module Nodes : Ord
    type node = Nodes.t
    type edge

(* A tuple of nodes.  The most general realization is
   [type children = node list], but we use a [Tuple.Mono]
   or [Tuple.Poly] module for more specific
   implementations, where the number of nodes is bounded from
   below or above.  For example to two for binary trees,
   as in~$\phi^3$ or QED. We can also have mixed arities
   (e.\,g.~two and three for QCD) or even arbitrary arities.  However,
   in most cases, there will be at least two children. *)
    type children

(* This type abbreviation and order allow to apply the [Set.Make]
   functor to $E\times t(N)$. *)
    type t = edge * children

(* In our implementation, we order by [children] and if they
   agree, we disambiguate by [edge]. *)
    val compare : t -> t -> int

(* Test a predicate for \emph{all} children. *)
    val for_all : (node -> bool) -> t -> bool

(* [fold f (_, children) acc] will calculate
   \begin{equation}
     f (x_1, f(x_2, \cdots f(x_n,\ocwlowerid{acc})))
   \end{equation}
   where the [children] are $\{x_1,x_2,\ldots,x_n\}$.
   There are slightly more efficient alternatives for fixed arity
   (in particular binary), but we want to be general. *)
    val fold : (node -> 'a -> 'a) -> t -> 'a -> 'a

  end

(* We will use modules from [Tuple] to implement arity constraints
   for [Forest.children]. *)

module Forest : functor (PT : Tuple.Poly) ->
  functor (N : Ord) -> functor (E : Ord) ->
      Forest with module Nodes = N and type edge = E.t
      and type node = N.t and type children = N.t PT.t

(* \thocwmodulesection{DAGs} *)

(* A DAG will describe the recursive construction of one particle
   off-shell wave functions (1POW).  The nodes are therefore the 1POWs
   and can be specified by a flavor and a momentum or a sum of external
   momenta.  Just as in [Forest], the edges are couplings and the leaf
   nodes are external on-shell wave functions.  However, each node can
   now have more than one offspring, i.\,e.~combination of edge and children
   or coupling and tuple of 1POWs.  This factorizes the forest and
   optimizes the code by common subexpression elimination.

   If~$T(n,D)$ denotes the set of all binary trees with root~$n$
   encoded in the DAG~$D$, while
   \begin{equation}
     O(n,D)=\{(e_1,n_1,n_1'), \ldots, (e_k,n_k,n_k')\}
   \end{equation}
   denotes the set of all~\emph{offspring} of~$n$ in~$D$,
   and~$\text{tree}(e,t,t')$ denotes the binary tree formed by
   joining the binary trees~$t$ and~$t'$ with the label~$e$, then
   \begin{equation}
     T(n,D) = \left\{ \text{tree}(e_i,t_i,t_i')\,\bigl|\,
      (e_i,t_i,t_i')\in
         \bigcup_{i=1}^k \{e_i\}\times T(n_i,D)\times T(n_i',D)\right\}
   \end{equation}
   is the recursive definition of the binary trees encoded by the DAG~$D$.
   It is obvious how this definitions translates to $n$-ary trees
   (including trees with mixed arity). *)

module type T =
  sig

(* When implementing modules of type [T], the type [node] will
   be a [Ord.t] that allows us use [Map.Make] and [Set.Make] to
   construct maps.  In a functor [Forest -> T], the order from
   [Forest.Node] will be used for ordering [node] in [T].
   In particular, the equality of nodes in [add_node], [add_offspring],
   [harvest], etc.{} below will be determined by [Forest.Node.compare]. *)
    type node

(* For [edge], we need no additional structure. *)
    type edge

(* In the description of the function we assume for definiteness DAGs of
   binary trees with [type children = node * node]. However, we will
   also have implementations with [type children = node list] below. *)

(* Other possibilities include
   [type children = V3 of node * node | V4 of node * node * node].
   There's probable never a need to use sets with logarithmic
   access, but it would be easy to add.  *)

    type children
    type t

(* The empty DAG. *)
    val empty : t

(* [add_node n dag] returns the DAG [dag] with the node [n].
   If the node [n] already exists in [dag], [dag] is returned
   unchanged.  Otherwise [n] is added without offspring. *)
    val add_node : node -> t -> t

(* [add_offspring n (e, (n1, n2)) dag] returns the DAG [dag]
   with the node [n] and its offspring [n1] and [n2] with edge
   label [e].  Each node can have an arbitrary number of offspring,
   but identical offspring are added only once.  In order
   to prevent cycles, [add_offspring] requires both [n>n1] and
   [n>n2] in the ordering of [node]s in the [Forest] that the
   DAG represents.  The nodes [n1] and [n2] are
   added as by [add_node].  NB: Adding the nodes [n1] and [n2] even
   if they are sterile is not necessary for our applications.
   But even though it slows down the code by a few percent, it is desirable
   for consistency and allows much more concise implementations of
   [iter_nodes] and [fold_nodes] below. *)
    val add_offspring : node -> edge * children -> t -> t
    exception Cycle

(* Just like [add_offspring], but does not check for potential cycles.  *)
    val add_offspring_unsafe : node -> edge * children -> t -> t

(* [is_node n dag] returns [true] iff [n] is a node in [dag]. *)
    val is_node : node -> t -> bool

(* [is_sterile n dag] returns [true] iff [n] is a node in [dag], but
   has no offspring. *)
    val is_sterile : node -> t -> bool

(* [is_offspring n (e, (n1, n2)) dag] returns [true] iff [n1] and [n2]
   are offspring of [n] with label [e] in [dag]. *)
    val is_offspring : node -> edge * children -> t -> bool

(* There is no function [val offspring : node -> (edge * children) list]
   to extract the structure of the DAG explicitely.  Instead, we export
   a functional interface that allows us to transform a DAG and
   to evaluate the expression encoded by the DAG. *)

(* Note that the following functions can run into infinite
   recursion if the DAG given as argument contains cycles. *)

(* The usual functionals for processing all nodes (including sterile)
   \ldots{} *)
    val iter_nodes : (node -> unit) -> t -> unit
    val map_nodes : (node -> node) -> t -> t
    val fold_nodes : (node -> 'a -> 'a) -> t -> 'a -> 'a

(* \ldots{} and all parent/offspring relations.  Note that [map] requires
   \emph{two} functions: one for the nodes and one for
   the edges and children.  This is so because a change in the
   definition of node is \emph{not} propagated automatically to where
   it is used as a child.  *)
    val iter : (node -> edge * children -> unit) -> t -> unit
    val map : (node -> node) ->
      (node -> edge * children -> edge * children) -> t -> t
    val fold : (node -> edge * children -> 'a -> 'a) -> t -> 'a -> 'a

(* \begin{dubious} 
     Note that in it's current incarnation,
     [fold add_offspring dag empty] copies \emph{only} the fertile nodes, while
     [fold add_offspring dag (fold_nodes add_node dag empty)]
     includes sterile ones, as does
     [map (fun n -> n) (fun n ec -> ec) dag].
   \end{dubious} *)

(* Return the DAG as a list of lists. *)
    val lists : t -> (node * (edge * children) list) list

(* [dependencies dag node] returns a canonically sorted [Tree2.t] of all
   nodes reachable from [node]. *)
    val dependencies : t -> node -> (node, edge) Tree2.t

(* [harvest dag n roots] returns the DAG [roots]
   enlarged by all nodes in [dag] reachable from [n].  *)
    val harvest : t -> node -> t -> t

(* [harvest_list dag nodes] returns the part of the DAG [dag]
   that is reachable from the [nodes]. *)
    val harvest_list : t -> node list -> t

(* [size dag] returns the number of nodes in the DAG [dag]. *)
    val size : t -> int

(* [eval f mul_edge mul_nodes add null unit root dag]
   interprets the part of [dag] beneath [root] as an algebraic
   expression:
   \begin{itemize}
     \item each node is evaluated by [f: node -> 'a]
     \item each set of children is evaluated by iterating the
       binary
       [mul_nodes: 'a -> 'c -> 'c] on the values of the nodes,
       starting from [unit: 'c]
     \item each offspring relation $(node, (edge, children))$
       is evaluated by applying
       [mul_edge: node -> edge -> 'c -> 'd] to [node], [edge]
       and the evaluation of [children].
     \item all offspring relations of a [node] are combined by
       iterating the binary 
       [add: 'd -> 'a -> 'a] starting from [null: 'a]
   \end{itemize}
   In our applications, we will always have ['a = 'c = 'd], but
   the more general type is useful for documenting the relationships.
   The memoizing variant
   [eval_memoized f mul_edge mul_nodes add null unit root dag] 
   requires some overhead, but can be more efficient for
   complex operations. *)
    val eval : (node -> 'a) -> (node -> edge -> 'c -> 'd) ->
      ('a -> 'c -> 'c) -> ('d -> 'a -> 'a) -> 'a -> 'c -> node -> t -> 'a
    val eval_memoized : (node -> 'a) -> (node -> edge -> 'c -> 'd) ->
      ('a -> 'c -> 'c) -> ('d -> 'a -> 'a) -> 'a -> 'c -> node -> t -> 'a

(* [forest root dag] expands the [dag] beneath [root] into the
   equivalent list of trees [Tree.t].  [children] are represented
   as list of nodes.
   \begin{dubious}
     A sterile node~[n] is represented as [Tree.Leaf ((n, None), n)],
     cf.~page~\pageref{Tree.Leaf}.  There might be a better way, but
     we need to change the interface and semantics of [Tree] for this.
   \end{dubious} *)
    val forest : node -> t -> (node * edge option, node) Tree.t list
    val forest_memoized : node -> t -> (node * edge option, node) Tree.t list

(* [count_trees n dag] returns the number of trees with root [n] encoded
    in the DAG [dag], i.\,e.~$|T(n,D)|$.  NB: the current
    implementation is very naive and can take a \emph{very} long
    time for moderately sized DAGs that encode a large set of
    trees. *)
    val count_trees : node -> t -> int

   end

module Make (F : Forest) :
    T with type node = F.node and type edge = F.edge
    and type children = F.children

(* \thocwmodulesection{Graded Sets, Forests \&{} DAGs} *)

(* A graded ordered\footnote{We don't appear to have use for graded unordered
   sets.} set is an ordered set with a map [rank]
   into another ordered set (often the
   non-negative integers).  Note that it is \emph{not} required
   that the grading respects the ordering,
   i.\,e.~[x<y] $\not\Rightarrow$ [rank x < rank y].  *)

(* \begin{dubious}
     Conceptionally, there is some overlap with [Bundle]
     (cf.~section~\ref{sec:bundle}), if we interpret the
     set of ranks as the base of the bundle and [rank] as the
     projection $\pi$.  We might want to unify the structures.
     But note that in the case of [Bundle], the intuition is
     that the base is a subset of the bundle, i.\,e.~each element
     of the base is an element of a fiber.  In the case of
     a grading, the set of ranks can be completely disjoint from
     the original set.
   \end{dubious} *)

module type Graded_Ord =
  sig
    include Ord
    module G : Ord
    val rank : t -> G.t
  end

(* For all ordered sets, there are two canonical gradings: a [Chaotic] grading
   that assigns the same rank (e.\,g.~[unit]) to all elements and the [Discrete]
   grading that uses the identity map as grading. *)

module type Grader = functor (O : Ord) -> Graded_Ord with type t = O.t
module Chaotic : Grader
module Discrete : Grader

(* A graded forest is just a forest in which the nodes form a graded ordered set.
   \begin{dubious}
     Module type substitions for avoiding the repetition
     here will come with O'Caml 4.13.
     Until then, we're lucky that the signature is short \ldots
   \end{dubious}  *)

module type Graded_Forest =
  sig
    module Nodes : Graded_Ord
    type node = Nodes.t
    type edge
    type children
    type t = edge * children
    val compare : t -> t -> int
    val for_all : (node -> bool) -> t -> bool
    val fold : (node -> 'a -> 'a) -> t -> 'a -> 'a
  end

module type Forest_Grader = functor (G : Grader) -> functor (F : Forest) ->
  Graded_Forest with type Nodes.t = F.node
  and type node = F.node
  and type edge = F.edge
  and type children = F.children
  and type t = F.t

module Grade_Forest : Forest_Grader

(* Finally, a graded DAG is a DAG in which the nodes form a graded ordered set
   and the subsets with a given rank can be accessed cheaply.  *)

module type Graded =
  sig
    include T
    type rank
    val rank : node -> rank
    val ranks : t -> rank list
    val min_max_rank : t -> rank * rank
    val ranked : rank -> t -> node list
  end

module Graded (F : Graded_Forest) :
    Graded with type node = F.node and type edge = F.edge
    and type children = F.children and type rank = F.Nodes.G.t

module Test : sig val suite : OUnit.test end
