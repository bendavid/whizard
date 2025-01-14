(* orders.mli --

   Copyright (C) 2023-2025 by

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


(* \thocwmodulesection{Conditions} *)

(* The function [of_strings] parses a small domain specific language.
   The list of strings can be understood as multiple command line options
   or as lines in a file:
   \begin{itemize}
     \item except for newlines, white space is \emph{not} significant.
     \item newlines are only significant as terminator for comments that start
       with a~["#"].
     \item [coupling_order]s are represented as unquoted strings, taken from
       the codomain of the model's [coupling_order_to_string] function.
       Strings outside of the codomain trigger a non-terminal error message and are ignored.
     \item sets of [coupling_order]s are written as comma separated lists,
       enclosed in matching braces, e.\,.g.~["{QED,QCD}"].
     \item the braces are optional for single element sets, i.\,e.~["QED"]
       and~["{QED}"] are equivalent.
     \item the empty set is represented by ["{}"].
     \item\relax ["~"] denotes the set complement with respect to the model's
       [all_coupling_orders ()].  In particular, ["~{}"] denotes
       [all_coupling_orders ()] and ["~{QED,QCD}"] all coupling orders
       except~["QED"] and~["QCD"].
     \item set difference is denoted by \texttt{\textbackslash},
       i.\,e.~["{QED,QCD} \ QCD"] is just~["QED"] and~["~{} \ QED"]
       is a complicated way to write ["~QED"].
       NB: as long as there are no variables for sets, the set difference is probably only
       useful as syntactic sugar for very few cases.
       Typical applications can be expressed as set complements.  Set union and intersection
       would be trivial, but appear to be even less useful.
     \item ranges of orders come as
       \begin{itemize}
         \item slices~["{2..3}"] and
         \item intervals~["[2..3]"].
       \end{itemize}
       In the case of slices, code for amplitudes at all orders in the range is generated,
       while in the case of intervals, code for the sum of these is generated.  If there
       is only one order in the range, the notations~["{3..3}"] or~["{3}"]
       and~["[3..3]"] or~["[3]"] produce equivalent physics, of course, but the interface
       code for the generated amplitudes are slightly different of course.  In the case of a
       slice~["{3..3}"], the order~3 will be exposed, while it will not be visible
       in the case of an interval~["[3..3]"].
       The abbreviation by a single integer, ["3"], behaves exactly as the
       slice~["{3..3}"] or~["{3}"].
       If the systematic expansion is performed in the
       squared matrix element, slices are more useful than intervals.
     \item ranges can be limited on one side or on both sides: in the former case,
       ["[..3]"] is equivalent to~["[0..3]"],
       while~["[0..]"] is equivalent to no limit at all.
     \item ranges for sets of coupling constants are set with an equal sign, as
       in~["{QED,QCD} = {2..4}"].  Note that the range~["0"] need not
       be spelled out: ["~{QCD}"] is equivalent to~["~{QCD} = 0"]
       and switches off all couplings with a positive QCD coupling order.
     \item specifications can be combined by a logical AND~["&&"] or logical
       OR~["||"] both operators associate to the left and
       parentheses ["("] and [")"] can be used for grouping
       (the support for logical OR is limited, but might be extended in
       the future to fill a gap in [Cascade]).
     \item combining conditions by a semicolon [";"] or as
       separate strings corresponds to a logical AND.  For example, the following
       \begin{itemize}
         \item [of_strings ["QED = {..4}; QCD = {..2}"]]
         \item [of_strings ["QED = {..4} && QCD = {..2}"]]
         \item [of_strings ["QED = {..4}"; "QCD = {..2}"]]
       \end{itemize}
       are equivalent ways to select upto and including second order in QCD
       and fourth order in QED
     \item a logical AND translates to set intersection for coupling orders,
       e.\,g.~["QCD = {2,4}; QCD = {3,5}"] is equivalent to~["QCD = {3,4}"].
       In the case of mixed types, the result will be a slice, if at least one
       of the sets is a slice.
     \item a natural consequence is that an empty intersection corresponds to
       switching off the coupling order completely e.\,g.~["QCD = 2; QCD = 4"]
       is equivalent to~["QCD"] or~["QCD=0"]
     \item for convenience, there is one exception to this rule: in a logical AND,
       if one set is~["{0}"], it is ignored and the result is the other set,
       e.\,g.~["~{}; QCD = 3"] is equivalent to the more verbose~["~{QCD}; QCD = 3"].
     \item since logical AND associates to the left, the above rules imply
     that~["QCD = 2; QCD = 4; QCD = 6"] is equivalent to~["QCD = 0; QCD = 6"]
     and finally to~["QCD = 6"].
   \end{itemize}
   The powers of all the coupling orders that are neither set to zero nor summed over
   will be encoded into the variable names for the shell wave functions.   If there are
   to many of these, we will run into the target language's limits on variable names.
   In models like typical SMEFT implementations, that define many different coupling orders,
   one can not ask for ["~{QED,QCD} = [..1]"] in order get all first order new physics
   contributions.  The list of all new physics coupling orders is just too long.
   Instead one needs to select a specfic coupling or a small set like in
   ["~{QED,QCD}; NP = [..1]"] *)

module type Conditions =
  sig

    (* This is the same as [coupling_order] from [Model.T]. *)
    type coupling_order

    (* Orders is just an abbreviation to make the interface more readable. *)
    type orders = (coupling_order * int) list

    (* This type collects the conditions on the orders of coupling constants
       and will be used by the functions below to select coupling constants,
       fusions and brakets. *)
    type t

    (* Keep all orders and sum them. *)
    val trivial : t

    (* Parse a list of strings as described above.*)
    val of_strings : string list -> t

    (* Return a human readable textual representation that can be inserted
       into the output source code for documentation.  *)
    val to_strings : t -> string list

    (* The following three predicates test whether coupling orders
       \begin{itemize}
         \item have been switched off completely ([constant])
         \item still can be added to ([fusion])
         \item satisfy the overall condition ([braket]).
       \end{itemize} *)

    (* [constant condition (M.coupling_orders c)] checks that none of the
       [coupling_order]s of the coupling constant [c] is non-zero and
       switched off in [condition] at the same time.   If not, the corresponding
       fusion or braket can be discarded immediately. *)

    (* \begin{dubious}
         NB: this can be used very early, before colorization or even during
         the model definition to avoid constructing pieces that will eventually be
         discarded anyways.
       \end{dubious} *)
    val constant : t -> orders -> bool

    (* Check that none of the [coupling_order]s exceeds the limits.  They can
       be below the lower bounds, since additional fusions might add more powers. *)

    val fusion : t -> orders -> bool

    (* Check that all of the [coupling_order]s are inside the limits.
       Return only the [coupling_order]s corresponding to
       slices.  This performs the sum over intervals implicitely. *)
    val braket : t -> orders -> orders option

    (* The list of coupling orders that is neither set to zero nor summed
       over without constraints. *)
    val exclusive_fusion : t -> coupling_order list

    (* The list of coupling orders with fixed powers. *)
    val exclusive_braket : t -> coupling_order list

    (* Compute the coupling order conditions on the scattering amplitude
       that allow to compute the squared amplitude to the given order.
       Note that intervals must be converted to slices, to be able to compute
       the interferences.  For example
       \begin{equation}
          \left| \mathcal{M}_{\text{SM}} + \lambda \mathcal{M}_{\text{BSM}} \right|^2
            = \mathcal{M}_{\text{SM}}^* \mathcal{M}_{\text{SM}}
            + \lambda \mathcal{M}_{\text{SM}}^* \mathcal{M}_{\text{BSM}}
            + \lambda \mathcal{M}_{\text{BSM}}^* \mathcal{M}_{\text{SM}}
            + \mathcal{O}(\lambda^2)
       \end{equation} *)
                                  
    (* For the general case, we arrange $n$ coupling orders~$\{c_k\}_{k=1,\ldots,n}$ in a sequence
       \begin{equation}
         c=(c_1,c_2,\ldots,c_n)\,,
       \end{equation}
       so that we can introduce a multi index notation for the powers
       \begin{equation}
         i=(i_1,i_2,\ldots,i_n)
       \end{equation}
       and write
       \begin{equation}
         c^i = \prod_{k=1}^n c_k^{i_k}\,.
       \end{equation}
       The matrix element is then
       \begin{equation}
         \mathcal{M}_\chi = \sum_{i} \chi(i) c^i \mathcal{M}_i\,,
       \end{equation}
       where the function~$\chi:\mathbf{N}_0^n\to\{0,1\}$ encodes the conditions on the
       coupling orders.
       For the squared matrix element with the condition~$\chi_2:\mathbf{N}_0^n\to\{0,1\}$
       we must find all~$\mathcal{M}_i$ that contribute to the sum
       \begin{equation}
         \left|\mathcal{M}\right|^2_{\chi_2}
            = \sum_{i,j} \chi_2(i+j) c^{i+j} \mathcal{M}^*_i \mathcal{M}_j\,.
       \end{equation}
       This means, that we need to find a function~$\chi$ such that
       \begin{equation}
        \forall i,j\in\mathrm{N}_0^n: \chi_2(i+j) = 1 \Rightarrow \chi(i)=\chi(j)=1\,.
       \end{equation}
       There are infinitely many of such~$\chi$, of course, and we want the function
       that is non-zero for the smallest possible subset of~$\mathrm{N}_0^n$. *)

    (* If~$\chi_2$ is non-zero for only one~$\hat\imath$, it is straightforward to
       construct a corresponding set~$I=\{i\}$ for which~$\chi$ doesn't vanish as a cartesian
       product
       \begin{equation}
         I = \times_{k=1}^n \{0,1,\ldots \hat\imath_{k}\}\,.
       \end{equation}
       If there is a larger set of~$i$ for which~$\chi_2(i)=1$, we can form the union by
       selecting the maximum order for each coupling order independently.  This can be implemented
       easily by replacing each slice and interval by the slice running from 0 to the
       upper limit. *)

    (* Infortunately, this will in general \emph{not} be the smallest such set
       for a given amplitude, because not all coupling order combinations can contribute.
       Therefore, only \emph{after} constructing the sliced amplitude, we can find all matching
       pairs. *)

    (* \begin{dubious}
         In addition, we should provide the Fortran code with the combinations
         of coupling orders to be multiplied an summed.
       \end{dubious} *)
    val square_root : t -> t

    (* Return a compact textual representation that can be parsed again by [of_strings].
       This is useful for testing and debugging. *)
    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
  end

(* A projection of [Model.T] containing only coupling constants
   and coupling orders.  This is useful for testing without having
   to link real models. *)
module type Model_CO =
  sig
    type constant 
    type coupling_order
    val all_coupling_orders : unit -> coupling_order list
    val coupling_order_to_string : coupling_order -> string
    val coupling_orders : constant -> (coupling_order * int) list
  end

module Conditions (M : Model_CO (* $\subset$ [Model.T] *)) : Conditions
       with type coupling_order = M.coupling_order

(* \thocwmodulesection{Slicing} *)

(* The idea is to slice a [DAG.t] representing an amplitude into
   pieces that correspond to given orders in a set of coupling
   constants.  This allows to assign a fixed order to all brakets
   and to write the corresponding amplitude.

   The mapping from one amplitude to many amplitudes is analogous
   to colorization and can be implemented as such.

   \begin{dubious}
     There is a certain co-product vibe to this, but I don't know if
     it is useful to investigate the analogy further.  First get a
     working prototype.
   \end{dubious}

   \begin{dubious}
     It is not obvious whether it is more efficient to
     \begin{enumerate}
       \item slice first, colorize later
       \item colorize first, slice later
     \end{enumerate}
     In the first case, we have to slice a smaller [DAG.t], but
     subsequently colorize a more complicated [DAG.].  In the second
     case, we have to colorize a smaller [DAG.t], but subsequently slice a
     more complicated [DAG.].  Probably, this varies from amplitude to
     amplitude and doesn't matter.  For the moment we choose route of slicing
     the colorized [DAG.t], because we don't have to touch the [Colorize.It()]
     functor.
   \end{dubious} *)

module Slice (CM : Model.Colorized) : Model.Sliced_by_Orders
       with type flavor_all_orders = CM.flavor
        and type flavor_sans_color = CM.flavor_sans_color
        and type constant = CM.constant
        and type coupling_order = CM.coupling_order
        and type orders = (CM.coupling_order * int) list

(* \thocwmodulesection{Tests} *)

module Test : sig val suite : OUnit.test end

