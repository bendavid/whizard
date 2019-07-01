! WHIZARD 2.2.6 May 02 2015
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

module sf_escan

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_defs, only: FMT_12
  use unit_tests
  use diagnostics
  use lorentz
  use physics_defs, only: ELECTRON
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

  public :: escan_data_t
  public :: sf_escan_test

  type, extends(sf_data_t) :: escan_data_t
     private
     type(flavor_t), dimension(:,:), allocatable :: flv_in
     integer, dimension(2) :: n_flv = 0
     real(default) :: norm = 1
   contains
     procedure :: init => escan_data_init
     procedure :: write => escan_data_write
     procedure :: get_n_par => escan_data_get_n_par
     procedure :: get_pdg_out => escan_data_get_pdg_out
     procedure :: allocate_sf_int => escan_data_allocate_sf_int  
  end type escan_data_t

  type, extends (sf_int_t) :: escan_t
     type(escan_data_t), pointer :: data => null ()
   contains
     procedure :: type_string => escan_type_string
     procedure :: write => escan_write
     procedure :: init => escan_init
     procedure :: complete_kinematics => escan_complete_kinematics
     procedure :: recover_x => escan_recover_x
     procedure :: inverse_kinematics => escan_inverse_kinematics
     procedure :: apply => escan_apply
  end type escan_t 
  

contains

  subroutine escan_data_init (data, model, pdg_in, norm)
    class(escan_data_t), intent(out) :: data
    class(model_data_t), intent(in), target :: model
    type(pdg_array_t), dimension(2), intent(in) :: pdg_in
    real(default), intent(in), optional :: norm
    real(default), dimension(2) :: m2
    integer :: i, j
    data%n_flv = pdg_array_get_length (pdg_in)
    allocate (data%flv_in (maxval (data%n_flv), 2))
    do i = 1, 2
       do j = 1, data%n_flv(i)
          call data%flv_in(j, i)%init (pdg_array_get (pdg_in(i), j), model)
       end do
    end do
    m2 = data%flv_in(1,:)%get_mass ()
    do i = 1, 2
       if (any (data%flv_in(1:data%n_flv(i),i)%get_mass () /= m2(i))) then
          call msg_fatal ("Energy scan: incoming particle mass must be uniform")
       end if
    end do
    if (present (norm))  data%norm = norm
  end subroutine escan_data_init

  subroutine escan_data_write (data, unit, verbose) 
    class(escan_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    integer :: u, i, j
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A)") "Energy-scan data:"
    write (u, "(3x,A)", advance="no")  "prt_in = "
    do i = 1, 2
       if (i > 1)  write (u, "(',',1x)", advance="no")
       do j = 1, data%n_flv(i)
          if (j > 1)  write (u, "(':')", advance="no")
          write (u, "(A)", advance="no")  char (data%flv_in(j,i)%get_name ())
       end do
    end do
    write (u, *)
    write (u, "(3x,A," // FMT_12 // ")")  "norm   =", data%norm
  end subroutine escan_data_write

  function escan_data_get_n_par (data) result (n)
    class(escan_data_t), intent(in) :: data
    integer :: n
    n = 1
  end function escan_data_get_n_par
  
  subroutine escan_data_get_pdg_out (data, pdg_out)
    class(escan_data_t), intent(in) :: data
    type(pdg_array_t), dimension(:), intent(inout) :: pdg_out
    integer :: i, n
    n = 2
    do i = 1, n
       pdg_out(i) = data%flv_in(1:data%n_flv(i),i)%get_pdg ()
    end do
  end subroutine escan_data_get_pdg_out
  
  subroutine escan_data_allocate_sf_int (data, sf_int)
    class(escan_data_t), intent(in) :: data
    class(sf_int_t), intent(inout), allocatable :: sf_int
    allocate (escan_t :: sf_int)
  end subroutine escan_data_allocate_sf_int
  
  function escan_type_string (object) result (string)
    class(escan_t), intent(in) :: object
    type(string_t) :: string
    if (associated (object%data)) then
       string = "Escan: energy scan" 
    else
       string = "Escan: [undefined]"
    end if
  end function escan_type_string
  
  subroutine escan_write (object, unit, testflag)
    class(escan_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = given_output_unit (unit)
    if (associated (object%data)) then
       call object%data%write (u)
       call object%base_write (u, testflag)
    else
       write (u, "(1x,A)")  "Energy scan data: [undefined]"
    end if
  end subroutine escan_write
    
  subroutine escan_init (sf_int, data)
    class(escan_t), intent(out) :: sf_int
    class(sf_data_t), intent(in), target :: data
    type(quantum_numbers_mask_t), dimension(4) :: mask
    integer, dimension(4) :: hel_lock
    real(default), dimension(2) :: m2
    real(default), dimension(0) :: mr2
    type(quantum_numbers_t), dimension(4) :: qn_fc, qn_hel, qn
    type(polarization_t) :: pol1, pol2
    type(state_iterator_t) :: it_hel1, it_hel2
    integer :: j1, j2
    select type (data)
    type is (escan_data_t)
       hel_lock = [3, 4, 1, 2]
       m2 = data%flv_in(1,:)%get_mass ()
       call sf_int%base_init (mask, m2, mr2, m2, hel_lock = hel_lock)
       sf_int%data => data       
       do j1 = 1, data%n_flv(1)
          call qn_fc(1)%init ( &
               flv = data%flv_in(j1,1), &
               col = color_from_flavor (data%flv_in(j1,1)))
          call qn_fc(3)%init ( &
               flv = data%flv_in(j1,1), &
               col = color_from_flavor (data%flv_in(j1,1)))
          call polarization_init_generic (pol1, data%flv_in(j1,1))
          do j2 = 1, data%n_flv(2)
             call qn_fc(2)%init ( &
                  flv = data%flv_in(j2,2), &
                  col = color_from_flavor (data%flv_in(j2,2)))
             call qn_fc(4)%init ( &
                  flv = data%flv_in(j2,2), &
                  col = color_from_flavor (data%flv_in(j2,2)))
             call polarization_init_generic (pol2, data%flv_in(j2,2))
             call it_hel1%init (pol1%state)
             do while (it_hel1%is_valid ())
                qn_hel(1:1) = it_hel1%get_quantum_numbers ()
                qn_hel(3:3) = it_hel1%get_quantum_numbers ()
                call it_hel2%init (pol2%state)
                do while (it_hel2%is_valid ())
                   qn_hel(2:2) = it_hel2%get_quantum_numbers ()
                   qn_hel(4:4) = it_hel2%get_quantum_numbers ()
                   qn = qn_hel .merge. qn_fc
                   call sf_int%add_state (qn)
                   call it_hel2%advance ()
                end do
                call it_hel1%advance ()
             end do
             call polarization_final (pol2)
          end do
          call polarization_final (pol1)
       end do
       call sf_int%set_incoming ([1,2])
       call sf_int%set_outgoing ([3,4])
       call sf_int%freeze ()
       sf_int%status = SF_INITIAL
    end select
  end subroutine escan_init
    
  subroutine escan_complete_kinematics (sf_int, x, f, r, rb, map)
    class(escan_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: x
    real(default), intent(out) :: f
    real(default) :: sqrt_x
    real(default), dimension(:), intent(in) :: r
    real(default), dimension(:), intent(in) :: rb
    logical, intent(in) :: map
    x = r
    sqrt_x = sqrt (x(1))
    if (sqrt_x > 0) then
       f = 1 / (2 * sqrt_x)
    else
       f = 0
       sf_int%status = SF_FAILED_KINEMATICS
       return
    end if
    call sf_int%reduce_momenta ([sqrt_x, sqrt_x])
  end subroutine escan_complete_kinematics

  subroutine escan_recover_x (sf_int, x, x_free)
    class(escan_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: x
    real(default), intent(inout), optional :: x_free
    real(default), dimension(2) :: xi
    call sf_int%base_recover_x (xi, x_free)
    x = product (xi)
  end subroutine escan_recover_x
  
  subroutine escan_inverse_kinematics (sf_int, x, f, r, rb, map, set_momenta)
    class(escan_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(in) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: r
    real(default), dimension(:), intent(out) :: rb
    logical, intent(in) :: map
    logical, intent(in), optional :: set_momenta
    real(default) :: sqrt_x
    logical :: set_mom
    set_mom = .false.;  if (present (set_momenta))  set_mom = set_momenta
    sqrt_x = sqrt (x(1))
    if (sqrt_x > 0) then
       f = 1 / (2 * sqrt_x)
    else
       f = 0
       sf_int%status = SF_FAILED_KINEMATICS
       return
    end if
    r = x
    rb = 1 - r
    if (set_mom) then
       call sf_int%reduce_momenta ([sqrt_x, sqrt_x])
    end if
  end subroutine escan_inverse_kinematics

  subroutine escan_apply (sf_int, scale)
    class(escan_t), intent(inout) :: sf_int
    real(default), intent(in) :: scale
    real(default) :: f
    associate (data => sf_int%data)
      f = data%norm
    end associate
    call sf_int%set_matrix_element (cmplx (f, kind=default))    
    sf_int%status = SF_EVALUATED
  end subroutine escan_apply


  subroutine sf_escan_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (sf_escan_1, "sf_escan_1", &
         "structure function configuration", &
         u, results)
    call test (sf_escan_2, "sf_escan_2", &
         "generate event", &
         u, results)
  end subroutine sf_escan_test
  
  subroutine sf_escan_1 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(pdg_array_t), dimension(2) :: pdg_in
    type(pdg_array_t), dimension(2) :: pdg_out
    integer, dimension(:), allocatable :: pdg1, pdg2
    class(sf_data_t), allocatable :: data
    
    write (u, "(A)")  "* Test output: sf_escan_1"
    write (u, "(A)")  "*   Purpose: initialize and display &
         &energy-scan structure function data"
    write (u, "(A)")
    
    call model%init_qed_test ()
    pdg_in(1) = ELECTRON
    pdg_in(2) = -ELECTRON

    allocate (escan_data_t :: data)
    select type (data)
    type is (escan_data_t)
       call data%init (model, pdg_in, norm = 2._default)
    end select

    call data%write (u)

    write (u, "(A)")

    write (u, "(1x,A)")  "Outgoing particle codes:"
    call data%get_pdg_out (pdg_out)
    pdg1 = pdg_out(1)
    pdg2 = pdg_out(2)
    write (u, "(2x,99(1x,I0))")  pdg1, pdg2

    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_escan_1"

  end subroutine sf_escan_1

  subroutine sf_escan_2 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(flavor_t), dimension(2) :: flv
    type(pdg_array_t), dimension(2) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    class(sf_int_t), allocatable :: sf_int
    type(vector4_t) :: k1, k2
    real(default) :: E
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: x_free, f
    
    write (u, "(A)")  "* Test output: sf_escan_2"
    write (u, "(A)")  "*   Purpose: initialize and display &
         &beam-events structure function data"
    write (u, "(A)")
    
    call model%init_qed_test ()
    call flv(1)%init (ELECTRON, model)
    call flv(2)%init (-ELECTRON, model)
    pdg_in(1) = ELECTRON
    pdg_in(2) = -ELECTRON

    call reset_interaction_counter ()
    
    allocate (escan_data_t :: data)
    select type (data)
    type is (escan_data_t)
       call data%init (model, pdg_in)
    end select
       
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1,2])

    write (u, "(A)")  "* Initialize incoming momentum with E=500"
    write (u, "(A)")
    E = 250
    k1 = vector4_moving (E, sqrt (E**2 - flv(1)%get_mass ()**2), 3)
    k2 = vector4_moving (E,-sqrt (E**2 - flv(2)%get_mass ()**2), 3)
    call vector4_write (k1, u)
    call vector4_write (k2, u)
    call sf_int%seed_kinematics ([k1, k2])

    write (u, "(A)")
    write (u, "(A)")  "* Set dummy parameters and generate x"
    write (u, "(A)")

    allocate (r (data%get_n_par ()))
    allocate (rb(size (r)))
    allocate (x (size (r)))

    r  = 0.8
    rb = 1 - r
    x_free = 1

    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)

    write (u, "(A,9(1x,F10.7))")  "r =", r
    write (u, "(A,9(1x,F10.7))")  "rb=", rb
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "f =", f
    write (u, "(A,9(1x,F10.7))")  "xf=", x_free

    write (u, "(A)")
    write (u, "(A)")  "* Inverse kinematics"
    write (u, "(A)")

    call sf_int%recover_x (x, x_free)
    call sf_int%inverse_kinematics (x, f, r, rb, map=.false.)

    write (u, "(A,9(1x,F10.7))")  "r =", r
    write (u, "(A,9(1x,F10.7))")  "rb=", rb
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "f =", f
    write (u, "(A,9(1x,F10.7))")  "xf=", x_free

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate"
    write (u, "(A)")

    call sf_int%apply (scale = 0._default)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call sf_int%final ()
    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_escan_2"

  end subroutine sf_escan_2


end module sf_escan
