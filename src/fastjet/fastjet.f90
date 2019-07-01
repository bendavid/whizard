!$Id: fastjet.f90 6133 2014-09-17 14:42:33Z kilian $

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     with contributions from
!     Christian Speckner <cnspeckn@googlemail.com>
!
! WHIZARD is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by 
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
!
! WHIZARD is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module fastjet

  use, intrinsic :: iso_c_binding
  use kinds
  use lorentz
  use cpp_strings

  implicit none
  private

  ! Public types
  public :: pseudojet_t
  public :: pseudojet_vector
  public :: pseudojet_vector_t
  public :: jet_definition_t
  public :: cluster_sequence_t

  ! Public functions and operations
  public :: fastjet_available
  public :: print_banner
  public :: sorted_by_pt
  public :: assignment(=)
  
  ! Public parameters
  public :: kt_algorithm
  public :: cambridge_algorithm
  public :: antikt_algorithm
  public :: genkt_algorithm
  public :: cambridge_for_passive_algorithm
  public :: genkt_for_passive_algorithm
  public :: ee_kt_algorithm
  public :: ee_genkt_algorithm
  public :: plugin_algorithm
  public :: undefined_jet_algorithm
  enum, bind (C)
     enumerator :: kt_algorithm = 0
     enumerator :: cambridge_algorithm = 1
     enumerator :: antikt_algorithm = 2
     enumerator :: genkt_algorithm = 3
     enumerator :: cambridge_for_passive_algorithm = 11
     enumerator :: genkt_for_passive_algorithm = 13
     enumerator :: ee_kt_algorithm = 50
     enumerator :: ee_genkt_algorithm = 53
     enumerator :: plugin_algorithm = 99
     enumerator :: undefined_jet_algorithm = 999  
  end enum
  integer, parameter :: jet_algorithm_kind = c_int


  ! Type definitions
  type :: pseudojet_t
     private
     type(c_ptr) :: cptr
   contains
     generic :: init => pseudojet_init, pseudojet_init_vector4
     procedure :: pseudojet_init
     procedure :: pseudojet_init_vector4
     procedure :: final => pseudojet_final
     procedure :: e => pseudojet_e
     procedure :: px => pseudojet_px
     procedure :: py => pseudojet_py
     procedure :: pz => pseudojet_pz
     procedure :: perp => pseudojet_perp
     procedure :: rap => pseudojet_rap
     procedure :: phi => pseudojet_phi
     procedure :: constituents => pseudojet_constituents
     procedure :: contains => pseudojet_contains_prt
  end type pseudojet_t

  type :: pseudojet_vector_t
     private
     type(c_ptr) :: cptr
   contains
     procedure :: init => pseudojet_vector_init
     procedure :: final => pseudojet_vector_final
     procedure :: size => pseudojet_vector_size
     procedure :: get => pseudojet_vector_get
  end type pseudojet_vector_t

  type :: jet_definition_t
     private
     type(c_ptr) :: cptr
     type(cpp_string_t) :: description_str
     integer :: description_strlen = 0
   contains
     procedure :: init => jet_definition_init
     procedure :: final => jet_definition_final
     procedure :: description => jet_definition_description
  end type jet_definition_t

  type :: cluster_sequence_t
     private
     type(c_ptr) :: cptr
   contains
     procedure :: init => cluster_sequence_init
     procedure :: final => cluster_sequence_final
     procedure :: inclusive_jets => cluster_sequence_inclusive_jets
     procedure :: assign_jet_indices => cluster_sequence_assign_jet_indices
  end type cluster_sequence_t


  ! Interfaces for generic operators
  interface assignment(=)
     module procedure pseudojet_array_from_vector
  end interface assignment(=)


  ! Interfaces for FastJet C wrapper functions
  interface
     subroutine fastjet_print_banner () bind (C)
     end subroutine fastjet_print_banner
  end interface

  interface
     function fastjet_available () bind (C) result (flag)
       import
       logical(c_bool) :: flag
     end function fastjet_available
  end interface

  interface
     function new_pseudojet (px, py, pz, e) bind (C) result (j)
       import
       real(c_double), intent(in), value :: px, py, pz, e
       type(c_ptr) :: j
     end function new_pseudojet
  end interface

  interface
     subroutine pseudojet_delete (j) bind (C)
       import
       type(c_ptr), value :: j
     end subroutine pseudojet_delete
  end interface

  interface
     function pseudojet_get_e (j) bind (C) result (e)
       import
       type(c_ptr), intent(in), value :: j
       real(c_double) :: e
     end function pseudojet_get_e
  end interface

  interface
     function pseudojet_get_px (j) bind (C) result (px)
       import
       type(c_ptr), intent(in), value :: j
       real(c_double) :: px
     end function pseudojet_get_px
  end interface

  interface
     function pseudojet_get_py (j) bind (C) result (py)
       import
       type(c_ptr), intent(in), value :: j
       real(c_double) :: py
     end function pseudojet_get_py
  end interface

  interface
     function pseudojet_get_pz (j) bind (C) result (pz)
       import
       type(c_ptr), intent(in), value :: j
       real(c_double) :: pz
     end function pseudojet_get_pz
  end interface

  interface
     function pseudojet_get_perp (j) bind (C) result (p)
       import
       type(c_ptr), intent(in), value :: j
       real(c_double) :: p
     end function pseudojet_get_perp
  end interface

  interface
     function pseudojet_get_rap (j) bind (C) result (p)
       import
       type(c_ptr), intent(in), value :: j
       real(c_double) :: p
     end function pseudojet_get_rap
  end interface

  interface
     function pseudojet_get_phi (j) bind (C) result (p)
       import
       type(c_ptr), intent(in), value :: j
       real(c_double) :: p
     end function pseudojet_get_phi
  end interface

  interface
     function pseudojet_get_constituents (j) bind (C) result (cv)
       import
       type(c_ptr), intent(in), value :: j
       type(c_ptr) :: cv
     end function pseudojet_get_constituents
  end interface

  interface
     function pseudojet_contains (j, p) bind (C) result (flag)
       import
       type(c_ptr), intent(in), value :: j, p
       logical(c_bool) :: flag
     end function pseudojet_contains
  end interface


  interface
     function new_pseudojet_vector (j, n) bind (C) result (jv)
       import
       type(c_ptr), dimension(*), intent(in) :: j
       integer(c_int), intent(in), value :: n
       type(c_ptr) :: jv
     end function new_pseudojet_vector
  end interface

  interface
     subroutine pseudojet_vector_delete (jv) bind (C)
       import
       type(c_ptr), value :: jv
     end subroutine pseudojet_vector_delete
  end interface

  interface
     function pseudojet_vector_get_size (jv) bind (C) result (n)
       import
       type(c_ptr), intent(in), value :: jv
       integer(c_int) :: n
     end function pseudojet_vector_get_size
  end interface

  interface
     function pseudojet_vector_get_jet (jv, i) bind (C) result (cptr)
       import
       type(c_ptr), intent(in), value :: jv
       integer(c_int), intent(in), value :: i
       type(c_ptr) :: cptr
     end function pseudojet_vector_get_jet
  end interface

  interface
     function pseudojet_vector_sorted_by_pt (jets) bind (C) result (sorted_jets)
       import
       type(c_ptr), intent(in), value :: jets
       type(c_ptr) :: sorted_jets
     end function pseudojet_vector_sorted_by_pt
  end interface

  interface
     function new_jet_definition (jet_alg, r) bind (C) result (jet_def)
       import
       integer(jet_algorithm_kind), intent(in), value :: jet_alg
       real(c_double), intent(in), value :: r
       type(c_ptr) :: jet_def
     end function new_jet_definition
  end interface

  interface
     subroutine jet_definition_delete (jet_def) bind (C)
       import
       type(c_ptr), value :: jet_def
     end subroutine jet_definition_delete
  end interface

  interface
     function jet_definition_get_description (jet_def) bind (C) result (str)
       import
       type(c_ptr), intent(in), value :: jet_def
       type(c_ptr) :: str
     end function jet_definition_get_description
  end interface

  interface
     function new_cluster_sequence (jv, jet_def) bind (C) result (cs)
       import
       type(c_ptr), intent(in), value :: jv
       type(c_ptr), intent(in), value :: jet_def
       type(c_ptr) :: cs
     end function new_cluster_sequence
  end interface

  interface
     subroutine cluster_sequence_delete (cs) bind (C)
       import
       type(c_ptr), value :: cs
     end subroutine cluster_sequence_delete
  end interface

  interface
     function cluster_sequence_get_inclusive_jets (cs) bind (C) result (jets)
       import
       type(c_ptr), intent(in), value :: cs
       type(c_ptr) :: jets
     end function cluster_sequence_get_inclusive_jets
  end interface

  interface
     function cluster_sequence_get_jet_indices (cs, jets) bind (C) result (idx)
       import
       type(c_ptr), intent(in), value :: cs, jets
       type(c_ptr) :: idx
     end function cluster_sequence_get_jet_indices
  end interface

  interface
     function int_vector_get (iv, i) bind (C) result (j)
       import
       type(c_ptr), intent(in), value :: iv
       integer(c_int), intent(in), value :: i
       integer(c_int) :: j
     end function int_vector_get
  end interface

  interface
     subroutine int_vector_delete (iv) bind (C)
       import
       type(c_ptr), value :: iv
     end subroutine int_vector_delete
  end interface

contains

  ! Fastjet banner, print explicitly to control order of output
  subroutine print_banner ()
    call fastjet_print_banner ()
  end subroutine print_banner

  ! Procedures for pseudojets
  subroutine pseudojet_init (j, px, py, pz, E)
    class(pseudojet_t), intent(out) :: j
    real(default), intent(in) :: px, py, pz, E
    real(c_double) :: jx = 0
    real(c_double) :: jy = 0
    real(c_double) :: jz = 0
    real(c_double) :: je = 0
    jx = px
    jy = py
    jz = pz
    je = E
    j%cptr = new_pseudojet (jx, jy, jz, je)
  end subroutine pseudojet_init

  subroutine pseudojet_init_vector4 (j, p)
    class(pseudojet_t), intent(out) :: j
    type(vector4_t), intent(in) :: p
    real(c_double) :: jx = 0
    real(c_double) :: jy = 0
    real(c_double) :: jz = 0
    real(c_double) :: je = 0
    jx = vector4_get_component (p, 1)
    jy = vector4_get_component (p, 2)
    jz = vector4_get_component (p, 3)
    je = vector4_get_component (p, 0)
    j%cptr = new_pseudojet (jx, jy, jz, je)
  end subroutine pseudojet_init_vector4

  subroutine pseudojet_final (j)
    class(pseudojet_t), intent(inout) :: j
    call pseudojet_delete (j%cptr)
    j%cptr = c_null_ptr
  end subroutine pseudojet_final

  function pseudojet_e (j) result (e)
    class(pseudojet_t), intent(in) :: j
    real(default) :: e
    e = pseudojet_get_e (j%cptr)
  end function pseudojet_e

  function pseudojet_px (j) result (p)
    class(pseudojet_t), intent(in) :: j
    real(default) :: p
    p = pseudojet_get_px (j%cptr)
  end function pseudojet_px

  function pseudojet_py (j) result (p)
    class(pseudojet_t), intent(in) :: j
    real(default) :: p
    p = pseudojet_get_py (j%cptr)
  end function pseudojet_py

  function pseudojet_pz (j) result (p)
    class(pseudojet_t), intent(in) :: j
    real(default) :: p
    p = pseudojet_get_pz (j%cptr)
  end function pseudojet_pz

  function pseudojet_perp (j) result (p)
    class(pseudojet_t), intent(in) :: j
    real(default) :: p
    p = pseudojet_get_perp (j%cptr)
  end function pseudojet_perp

  function pseudojet_rap (j) result (p)
    class(pseudojet_t), intent(in) :: j
    real(default) :: p
    p = pseudojet_get_rap (j%cptr)
  end function pseudojet_rap

  function pseudojet_phi (j) result (p)
    class(pseudojet_t), intent(in) :: j
    real(default) :: p
    p = pseudojet_get_phi (j%cptr)
  end function pseudojet_phi

  function pseudojet_constituents (j) result (prt)
    class(pseudojet_t), intent(in) :: j
    type(pseudojet_vector_t) :: prt
    prt%cptr = pseudojet_get_constituents (j%cptr)
  end function 

  function pseudojet_contains_prt (j, prt) result (flag)
    class(pseudojet_t), intent(in) :: j
    type(pseudojet_t), intent(in) :: prt
    logical :: flag
    flag = pseudojet_contains (j%cptr, prt%cptr)
  end function pseudojet_contains_prt


  ! Procedures for pseudojet vectors
  function pseudojet_vector (j) result (jv)
    type(pseudojet_t), dimension(:), intent(in) :: j
    type(pseudojet_vector_t) :: jv
    call jv%init (j)
  end function pseudojet_vector

  subroutine pseudojet_vector_init (jv, j)
    class(pseudojet_vector_t), intent(out) :: jv
    type(pseudojet_t), dimension(:), intent(in) :: j
    type(c_ptr), dimension(:), allocatable :: cptr
    allocate (cptr (size (j)))
    cptr = j%cptr
    jv%cptr = new_pseudojet_vector (cptr, size (j))
  end subroutine pseudojet_vector_init

  subroutine pseudojet_vector_final (jv)
    class(pseudojet_vector_t), intent(inout) :: jv
    call pseudojet_vector_delete (jv%cptr)
    jv%cptr = c_null_ptr
  end subroutine pseudojet_vector_final

  function pseudojet_vector_size (jv) result (n)
    class(pseudojet_vector_t), intent(in) :: jv
    integer :: n
    n = pseudojet_vector_get_size (jv%cptr)
  end function pseudojet_vector_size

  function pseudojet_vector_get (jv, i) result (j)
    class(pseudojet_vector_t), intent(in) :: jv
    integer, intent(in) :: i
    type(pseudojet_t) :: j
    j%cptr = pseudojet_vector_get_jet (jv%cptr, int(i-1, c_int))
  end function pseudojet_vector_get

  subroutine pseudojet_array_from_vector (j, jv)
    type(pseudojet_t), dimension(:), allocatable, intent(out) :: j
    type(pseudojet_vector_t), intent(in) :: jv
    integer :: i, n
    n = jv%size ()
    allocate (j (n))
    do i = 1, n
       j(i) = jv%get (i)
    end do
  end subroutine pseudojet_array_from_vector

  function sorted_by_pt (jets) result (sorted_jets)
    class(pseudojet_vector_t), intent(in) :: jets
    type(pseudojet_vector_t) :: sorted_jets
    sorted_jets%cptr = pseudojet_vector_sorted_by_pt (jets%cptr)
  end function sorted_by_pt


  ! Procedures for jet definitions
  subroutine jet_definition_init (jet_def, jet_alg, r)
    class(jet_definition_t), intent(out) :: jet_def
    integer(jet_algorithm_kind), intent(in) :: jet_alg
    real(default), intent(in) :: r
    type(cpp_string_t) :: description_str
    jet_def%cptr = new_jet_definition (jet_alg, real (r, c_double))
    call jet_def%description_str%init &
         (jet_definition_get_description (jet_def%cptr))
    jet_def%description_strlen = len (jet_def%description_str)
  end subroutine jet_definition_init

  subroutine jet_definition_final (jet_def)
    class(jet_definition_t), intent(inout) :: jet_def
    call jet_def%description_str%final ()
    jet_def%description_strlen = 0
    call jet_definition_delete (jet_def%cptr)
    jet_def%cptr = c_null_ptr
  end subroutine jet_definition_final

  function jet_definition_description (jet_def) result (string)
    class(jet_definition_t), intent(in) :: jet_def
    character(len=jet_def%description_strlen) :: string
    string = char (jet_def%description_str)
  end function jet_definition_description


  ! Procedures for cluster sequences
  subroutine cluster_sequence_init (cs, jv, jet_def)
    class(cluster_sequence_t), intent(out) :: cs
    type(pseudojet_vector_t), intent(in) :: jv
    type(jet_definition_t), intent(in) :: jet_def
    cs%cptr = new_cluster_sequence (jv%cptr, jet_def%cptr)
  end subroutine cluster_sequence_init

  subroutine cluster_sequence_final (cs)
    class(cluster_sequence_t), intent(inout) :: cs
    call cluster_sequence_delete (cs%cptr)
    cs%cptr = c_null_ptr
  end subroutine cluster_sequence_final

  function cluster_sequence_inclusive_jets (cs) result (jets)
    class(cluster_sequence_t), intent(in) :: cs
    type(pseudojet_vector_t) :: jets
    jets%cptr = cluster_sequence_get_inclusive_jets (cs%cptr)
  end function cluster_sequence_inclusive_jets

  subroutine cluster_sequence_assign_jet_indices (cs, jets, idx)
    class(cluster_sequence_t), intent(in) :: cs
    type(pseudojet_vector_t), intent(in) :: jets
    integer, dimension(:), intent(out) :: idx
    type(c_ptr) :: idx_cptr
    integer :: i
    idx_cptr = cluster_sequence_get_jet_indices (cs%cptr, jets%cptr)
    do i = 1, size (idx)
       idx(i) = int_vector_get (idx_cptr, int(i-1, c_int)) + 1
    end do
    call int_vector_delete (idx_cptr)
  end subroutine cluster_sequence_assign_jet_indices

end module fastjet

