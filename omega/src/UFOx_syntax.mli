(* vertex_syntax.mli --

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

(* \thocwmodulesection{UFO Extensions}

   We accept the following extensions to the UFO format:
   \begin{enumerate}
     \item Young tableaux: they are representated as a list of lists
       of integers using ``\texttt{,}'' as separators.  E.\,g.
       \ytableausetup{centertableaux,smalltableaux}
       \begin{equation}
         \ytableaushort{13,2}
       \end{equation}
       is written as \texttt{\lbrack\lbrack1,3\rbrack,\lbrack2\rbrack\rbrack}.
       The contents of cells in a Young tableau for the representation of a
       particle must be consecutive positive integers starting with 1.
       The representation for the anti particle has all integers negated,
       e.\,g.~\texttt{\lbrack\lbrack-1,-3\rbrack,\lbrack-2\rbrack\rbrack}.
     \item Young tableaux for particles and anti particles can appear in
       the \emph{new} optional attribute \texttt{color\_young}.
       If \texttt{color\_young} is present, \texttt{color} should be set to the
       non-standard value~$0$.
     \item Young tableaux for particles (but not for anti particles!)
       can also appear in the \texttt{color} attribute of vertices as
       the first argument of the new tensors \texttt{Delta} and \texttt{TY},
       representing the Kronecker-$\delta$ and the generator~$T_a$ in the
       given representation.  The gauge vertex in the above representation
       would be written
       \begin{center}
         \texttt{color = \lbrack 'TY(\lbrack\lbrack1,3\rbrack,\lbrack2\rbrack\rbrack,3,1,2)'\rbrack}
       \end{center}
       where the gluon would be at position 3, the particle at position 1
       and the anti particle at position 2.  The numbers in the Young tableau
       and the numbers denoting the position of the particles are
       completely unrelated, of course.
   \end{enumerate}
   Note that the cells in the Young tableaux used internally by O'Mega start
   from~0.  Using this in the UFO files would have required to introduce
   even more special syntax for charge conjugation. *)


(* \thocwmodulesection{Abstract Syntax} *)

exception Syntax_Error of string * Lexing.position * Lexing.position

type expr =
  | Integer of int
  | Float of float
  | Variable of string
  | Quoted of string
  | Young_Tableau of int Young.tableau
  | Sum of expr * expr
  | Difference of expr * expr
  | Product of expr * expr
  | Quotient of expr * expr
  | Power of expr * expr
  | Application of string * expr list

val integer : int -> expr
val float : float -> expr
val variable : string -> expr
val quoted : string -> expr
val young_tableau : int Young.tableau -> expr
val add : expr -> expr -> expr
val subtract : expr -> expr -> expr
val multiply : expr -> expr -> expr
val divide : expr -> expr -> expr
val power : expr -> expr -> expr
val apply : string -> expr list -> expr

(* Return the sets of variable and function names referenced
   in the expression. *)
val variables : expr -> Sets.String_Caseless.t
val functions : expr -> Sets.String_Caseless.t
