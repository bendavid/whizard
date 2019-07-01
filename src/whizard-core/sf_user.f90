! WHIZARD 2.1.0 June 15 2012
! 
! Copyright (C) 1999-2012 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     Christian Speckner <christian.speckner@physik.uni-freiburg.de>
!     with contributions by Sebastian Schmidt, Daniel Wiesler, Felix Braam
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
! This file has been stripped of most comments.  For documentation, refer
! to the source 'whizard.nw'

module sf_user

  use iso_c_binding !NODEP!
  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use c_particles !NODEP!
  use lorentz !NODEP!
  use subevents
  use user_code_interface
  use models
  use flavors
  use helicities
  use colors
  use quantum_numbers
  use state_matrices
  use polarizations
  use interactions
  use sf_aux

  implicit none
  private

  public :: sf_user_data_t
  public :: sf_user_data_init
  public :: sf_user_data_write
  public :: sf_user_data_get_name
  public :: sf_user_data_get_n_in
  public :: sf_user_data_get_n_out
  public :: sf_user_data_get_n_tot
  public :: sf_user_data_get_n_dim
  public :: sf_user_data_get_n_var
  public :: interaction_init_sf_user
  public :: interaction_set_kinematics_sf_user
  public :: interaction_apply_sf_user

  type :: sf_user_data_t
     private
     type(string_t) :: name
     integer :: n_in
     integer :: n_out
     integer :: n_tot
     integer :: n_states
     integer :: n_col
     integer :: n_dim
     integer :: n_var
     integer, dimension(2) :: pdg_in
     type(model_t), pointer :: model => null ()
     procedure(user_int_info), nopass, pointer :: info => null ()
     procedure(user_int_mask), nopass, pointer :: mask => null ()
     procedure(user_int_state), nopass, pointer :: state => null ()
     procedure(user_int_kinematics), nopass, pointer :: kinematics => null ()
     procedure(user_int_evaluate), nopass, pointer :: evaluate => null ()
  end type sf_user_data_t


contains

  subroutine sf_user_data_init (data, name, flv, model)
    type(sf_user_data_t), intent(out) :: data
    type(string_t), intent(in) :: name
    type(flavor_t), dimension(2), intent(in) :: flv
    type(model_t), intent(in), target :: model
    integer(c_int) :: n_in
    integer(c_int) :: n_out
    integer(c_int) :: n_states
    integer(c_int) :: n_col
    integer(c_int) :: n_dim
    integer(c_int) :: n_var
    data%name = name
    data%pdg_in = flavor_get_pdg (flv)
    data%model => model
    call c_f_procpointer (user_code_find_proc (name // "_info"), data%info)
    call c_f_procpointer (user_code_find_proc (name // "_mask"), data%mask)
    call c_f_procpointer (user_code_find_proc (name // "_state"), data%state)
    call c_f_procpointer &
         (user_code_find_proc (name // "_kinematics"), data%kinematics)
    call c_f_procpointer &
         (user_code_find_proc (name // "_evaluate"), data%evaluate)
    n_in = 1
    n_out = 2
    n_states = 1
    n_col = 2
    n_dim = 1
    n_var = 1
    call data%info (n_in, n_out, n_states, n_col, n_dim, n_var)
    data%n_in = n_in
    data%n_out = n_out
    data%n_tot = n_in + n_out
    data%n_states = n_states
    data%n_col = n_col
    data%n_dim = n_dim
    data%n_var = n_var
  end subroutine sf_user_data_init

  subroutine sf_user_data_write (data, unit, md5)
    type(sf_user_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    integer :: u
    logical, intent(in), optional :: md5
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) "User structure function: ", char (data%name)
  end subroutine sf_user_data_write

  function sf_user_data_get_name (data) result (name)
    type(string_t) :: name
    type(sf_user_data_t), intent(in) :: data
    name = data%name
  end function sf_user_data_get_name

  function sf_user_data_get_n_in (data) result (n_in)
    integer :: n_in
    type(sf_user_data_t), intent(in) :: data
    n_in = data%n_in
  end function sf_user_data_get_n_in

   function sf_user_data_get_n_out (data) result (n_out)
    integer :: n_out
    type(sf_user_data_t), intent(in) :: data
    n_out = data%n_out
  end function sf_user_data_get_n_out

  function sf_user_data_get_n_tot (data) result (n_tot)
    integer :: n_tot
    type(sf_user_data_t), intent(in) :: data
    n_tot = data%n_tot
  end function sf_user_data_get_n_tot

  function sf_user_data_get_n_dim (data) result (n_dim)
    integer :: n_dim
    type(sf_user_data_t), intent(in) :: data
    n_dim = data%n_dim
  end function sf_user_data_get_n_dim

 function sf_user_data_get_n_var (data) result (n_var)
    integer :: n_var
    type(sf_user_data_t), intent(in) :: data
    n_var = data%n_var
  end function sf_user_data_get_n_var

  subroutine interaction_init_sf_user (inter, data)
    type(interaction_t), intent(out) :: inter
    type(sf_user_data_t), intent(in) :: data
    type(quantum_numbers_mask_t), dimension(:), allocatable :: mask
    integer, dimension(:), allocatable :: hel_lock
    integer(c_int) :: m_flv, m_hel, m_col, i_lock
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    integer(c_int) :: f, h
    integer(c_int), dimension(:), allocatable :: c
    type(flavor_t) :: flv
    type(helicity_t) :: hel
    type(color_t) :: col
    integer :: i, s
    integer(c_int) :: i_prt, i_state
    allocate (mask (data%n_tot))
    allocate (hel_lock (data%n_tot))
    allocate (qn (data%n_tot))
    allocate (c (data%n_col))
    do i = 1, size (mask)
       i_prt = i
       m_flv = 0;  m_col = 0;  m_hel = 0;  i_lock = 0
       call data%mask (i_prt, m_flv, m_col, m_hel, i_lock)
       mask(i) = new_quantum_numbers_mask (m_flv /= 0, m_col /= 0, m_hel /= 0)
       hel_lock(i) = i_lock
    end do
    call interaction_init &
         (inter, data%n_in, 0, data%n_out, mask=mask, hel_lock=hel_lock, &
          set_relations=.true.)
    do s = 1, data%n_states
       i_state = s
       do i = 1, data%n_tot
          i_prt = i
          f = 0;  h = 0;  c = 0
          call data%state (i_state, i_prt, f, h, c)
          if (m_flv == 0) then
             call flavor_init (flv, int (f), data%model)
          else
             call flavor_init (flv)
          end if
          if (m_hel == 0) then
             call helicity_init (hel, int (h))
          else
             call helicity_init (hel)
          end if
          if (m_col == 0) then
             call color_init_from_array (col, int (c))
          else
             call color_init (col)
          end if
          call quantum_numbers_init (qn(i), flv, col, hel)
       end do
       call interaction_add_state (inter, qn)
    end do
    call interaction_freeze (inter)
  end subroutine interaction_init_sf_user

  subroutine interaction_set_kinematics_sf_user (int, x, r, data)
    type(interaction_t), intent(inout) :: int
    real(default), dimension(:), intent(out) :: x
    real(default), dimension(:), intent(in) :: r
    type(sf_user_data_t), intent(in) :: data
    type(vector4_t), dimension(data%n_in) :: p_in
    type(vector4_t), dimension(data%n_out) :: p_out
    type(c_prt_t), dimension(data%n_in) :: prt_in
    type(c_prt_t), dimension(data%n_out) :: prt_out
    real(c_double), dimension(data%n_var) :: xval
    call interaction_get_momenta_sub (int, p_in, outgoing=.false.)
    prt_in = vector4_to_c_prt (p_in)
    prt_in%type = PRT_INCOMING
    call data%kinematics (prt_in, real (r, c_double), prt_out, xval)
    x = xval
    p_out = vector4_from_c_prt (prt_out)
    call interaction_set_momenta (int, p_out, outgoing=.true.)
  end subroutine interaction_set_kinematics_sf_user

  subroutine interaction_apply_sf_user (int, scale, x, data)
    type(interaction_t), intent(inout) :: int
    real(default), intent(in) :: scale
    real(default), dimension(:), intent(in) :: x
    type(sf_user_data_t), intent(in) :: data
    real(c_double), dimension(data%n_states) :: fval
    complex(default), dimension(data%n_states) :: fc
    call data%evaluate (real (x, c_double), real (scale, c_double), fval)
    fc = fval
    call interaction_set_matrix_element (int, fc)
  end subroutine interaction_apply_sf_user


end module sf_user
