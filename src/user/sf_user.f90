! WHIZARD 2.2.5 Feb 27 2015
! 
! Copyright (C) 1999-2015 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, Daniel Wiesler 
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

  use, intrinsic :: iso_c_binding !NODEP!
  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_defs, only: FMT_17
  use diagnostics
  use c_particles
  use lorentz
  use subevents
  use user_code_interface
  use pdg_arrays
  use model_data
  use flavors
  use helicities
  use colors
  use quantum_numbers
  use state_matrices
  use polarizations
  use interactions
  use sf_aux
  use sf_base
  
  implicit none
  private

  public :: user_data_t

  type, extends(sf_data_t) :: user_data_t
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
     class(model_data_t), pointer :: model => null ()
     procedure(user_int_info), nopass, pointer :: info => null ()
     procedure(user_int_mask), nopass, pointer :: mask => null ()
     procedure(user_int_state), nopass, pointer :: state => null ()
     procedure(user_int_kinematics), nopass, pointer :: kinematics => null ()
     procedure(user_int_evaluate), nopass, pointer :: evaluate => null ()
   contains
     procedure :: write => user_data_write
     procedure :: allocate_sf_int => user_data_allocate_sf_int
     procedure :: get_n_par => user_data_get_n_par
     procedure :: get_pdg_out => user_data_get_pdg_out  
  end type user_data_t

  !!! JRR: WK please check (#529)
  type, extends (sf_int_t) :: user_t
     type(user_data_t), pointer :: data => null ()
     real(default) :: x = 0
     real(default) :: q = 0
   contains
     procedure :: init => user_init
     procedure :: type_string => user_type_string
     procedure :: write => user_write
     procedure :: complete_kinematics => user_complete_kinematics
     procedure :: inverse_kinematics => user_inverse_kinematics
     procedure :: apply => user_apply
  end type user_t 
  

contains

  subroutine user_data_write (data, unit, verbose) 
    class(user_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose        
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A,A)") "User structure function: ", char (data%name)
  end subroutine user_data_write

  subroutine user_init (sf_int, data)
    !!! JRR: WK please check (#529)
    class(user_t), intent(out) :: sf_int
    class(sf_data_t), intent(in), target :: data
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
    select type (data)
    type is (user_data_t)   
       allocate (mask (data%n_tot))
       allocate (hel_lock (data%n_tot))
       allocate (qn (data%n_tot))
       allocate (c (data%n_col))
       do i = 1, size (mask)
          i_prt = i
          m_flv = 0;  m_col = 0;  m_hel = 0;  i_lock = 0
          call data%mask (i_prt, m_flv, m_col, m_hel, i_lock)
          mask(i) = &
               quantum_numbers_mask (m_flv /= 0, m_col /= 0, m_hel /= 0)
          hel_lock(i) = i_lock
       end do
       !!! JRR: WK please check (#529)
       !!! Will have to be filled in later.
       ! call sf_int%base_init (mask, &
       !      hel_lock = hel_lock)
       call interaction_init &
            (sf_int%interaction_t, data%n_in, 0, data%n_out, mask=mask, &
            hel_lock=hel_lock, set_relations=.true.)       
       do s = 1, data%n_states
          i_state = s
          do i = 1, data%n_tot
             i_prt = i
             f = 0;  h = 0;  c = 0
             call data%state (i_state, i_prt, f, h, c)
             if (m_flv == 0) then
                call flv%init (int (f), data%model)
             else
                call flv%init ()
             end if
             if (m_hel == 0) then
                call hel%init (int (h))
             else
                call hel%init ()
             end if
             if (m_col == 0) then
                call color_init_from_array (col, int (c))
             else
                call col%init ()
             end if
             call qn(i)%init (flv, col, hel)
          end do
          call interaction_add_state (sf_int%interaction_t, qn)
       end do
       call interaction_freeze (sf_int%interaction_t)
       !!! JRR: WK please check (#529)
       !!! What has to be inserted here?
       ! call sf_int%set_incoming (??)
       ! call sf_int%set_radiated (??)
       ! call sf_int%set_outgoing (??)
       sf_int%status = SF_INITIAL
    end select
  end subroutine user_init

  subroutine user_data_allocate_sf_int (data, sf_int)
    class(user_data_t), intent(in) :: data
    class(sf_int_t), intent(inout), allocatable :: sf_int
    allocate (user_t :: sf_int)
  end subroutine user_data_allocate_sf_int
  
  function user_data_get_n_par (data) result (n)
    class(user_data_t), intent(in) :: data
    integer :: n
    n = data%n_var
  end function user_data_get_n_par
  
  subroutine user_data_get_pdg_out (data, pdg_out)
    class(user_data_t), intent(in) :: data
    type(pdg_array_t), dimension(:), intent(inout) :: pdg_out
    !!! JRR: WK please check (#529)
    !!! integer :: n, np, i
    !!! n = count (data%mask)
    !!! np = 0;  if (data%has_photon .and. data%mask_photon)  np = 1
    !!! allocate (pdg_out (n + np))
    !!! pdg_out(1:n) = pack ([(i, i = -6, 6)], data%mask)
    !!! if (np == 1)  pdg_out(n+np) = PHOTON
  end subroutine user_data_get_pdg_out
  
  function user_type_string (object) result (string)
    class(user_t), intent(in) :: object
    type(string_t) :: string
    if (associated (object%data)) then
       string = "User structure function: " // object%data%name
    else
       string = "User structure function: [undefined]"
    end if
  end function user_type_string
  
  subroutine user_write (object, unit, testflag)
    !!! JRR: WK please check (#529)
    !!! Guess these variables do not exist for user strfun (?)
    class(user_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = given_output_unit (unit)
    if (associated (object%data)) then
       call object%data%write (u)
       if (object%status >= SF_DONE_KINEMATICS) then
          write (u, "(1x,A)")  "SF parameters:"
          write (u, "(3x,A," // FMT_17 // ")")  "x =", object%x
          if (object%status >= SF_FAILED_EVALUATION) then
             write (u, "(3x,A," // FMT_17 // ")")  "Q =", object%q
          end if
       end if
       call object%base_write (u, testflag)
    else
       write (u, "(1x,A)")  "User structure function data: [undefined]"
    end if
  end subroutine user_write
    
  subroutine user_complete_kinematics (sf_int, x, f, r, rb, map)
    !!! JRR: WK please check (#529)
    !!! This cannot be correct, as the CIRCE1 structure function has 
    !!! twice the variables (2->4 instead of 1->2 splitting)
    class(user_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: r
    real(default), dimension(:), intent(in) :: rb
    logical, intent(in) :: map
    real(default) :: xb1
    if (map) then
       call msg_fatal ("User structure function: map flag not supported")
    else
       x(1) = r(1)
       f = 1
    end if
    xb1 = 1 - x(1)
    call sf_int%split_momentum (x, xb1)
    select case (sf_int%status)
    case (SF_DONE_KINEMATICS)
       sf_int%x = x(1)
    case (SF_FAILED_KINEMATICS)
       sf_int%x = 0
       f = 0
    end select
  end subroutine user_complete_kinematics

  subroutine user_inverse_kinematics (sf_int, x, f, r, rb, map, set_momenta)
    !!! JRR: WK please check (#529)
    !!! This cannot be correct, as the CIRCE1 structure function has 
    !!! twice the variables (2->4 instead of 1->2 splitting)
    class(user_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(in) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: r
    real(default), dimension(:), intent(out) :: rb
    logical, intent(in) :: map
    logical, intent(in), optional :: set_momenta
    real(default) :: xb1
    logical :: set_mom
    set_mom = .false.;  if (present (set_momenta))  set_mom = set_momenta
    if (map) then
       call msg_fatal ("User structure function: map flag not supported")
    else
       r(1) = x(1)
       f = 1
    end if
    xb1 = 1 - x(1)
    rb = 1 - r
    if (set_mom) then
       call sf_int%split_momentum (x, xb1)
       select case (sf_int%status)
       case (SF_DONE_KINEMATICS)
          sf_int%x = x(1)
       case (SF_FAILED_KINEMATICS)
          sf_int%x = 0
          f = 0
       end select
    end if
  end subroutine user_inverse_kinematics

  subroutine user_apply (sf_int, scale) !, x, data)    
    !!! JRR: WK please check (#529)
    class(user_t), intent(inout) :: sf_int
    real(default), intent(in) :: scale
    real(default), dimension(:), allocatable :: x
    real(c_double), dimension(sf_int%data%n_states) :: fval
    complex(default), dimension(sf_int%data%n_states) :: fc
    associate (data => sf_int%data)
      !!! This is wrong, has to be replaced
      ! allocate (x, size (sf_int%x)))
      x = sf_int%x
      call data%evaluate (real (x, c_double), real (scale, c_double), fval)
      fc = fval
      call interaction_set_matrix_element (sf_int%interaction_t, fc)
    end associate
    sf_int%status = SF_EVALUATED
  end subroutine user_apply


end module sf_user
