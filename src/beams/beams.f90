! WHIZARD 2.2.3 Nov 30 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

module beams

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use constants
  use format_defs, only: FMT_19
  use unit_tests
  use diagnostics
  use md5
  use lorentz
  use model_data
  use flavors
  use colors
  use quantum_numbers
  use state_matrices
  use interactions
  use polarizations
  use beam_structures

  implicit none
  private

  public :: beam_data_t
  public :: beam_data_final
  public :: beam_data_write
  public :: beam_data_are_valid
  public :: beam_data_check_scattering
  public :: beam_data_get_n_in
  public :: beam_data_get_flavor
  public :: beam_data_get_energy
  public :: beam_data_get_sqrts
  public :: beam_data_cm_frame
  public :: beam_data_get_md5sum
  public :: beam_data_init_structure
  public :: beam_data_init_sqrts
  public :: beam_data_init_momenta
  public :: beam_data_init_decay
  public :: beam_data_masses_are_consistent
  public :: beam_t
  public :: beam_init
  public :: beam_final
  public :: beam_write
  public :: assignment(=)
  public :: interaction_set_source_link
  public :: beam_get_int_ptr
  public :: beam_set_momenta
  public :: beam_test

  type :: beam_data_t
     logical :: initialized = .false.
     integer :: n = 0
     type(flavor_t), dimension(:), allocatable :: flv
     real(default), dimension(:), allocatable :: mass
     type(pmatrix_t), dimension(:), allocatable :: pmatrix
     logical :: lab_is_cm_frame = .true.
     type(vector4_t), dimension(:), allocatable :: p_cm
     type(vector4_t), dimension(:), allocatable :: p
     type(lorentz_transformation_t), allocatable  :: L_cm_to_lab
     real(default) :: sqrts = 0
     character(32) :: md5sum = ""
   contains
     procedure :: write => beam_data_write
  end type beam_data_t

  type :: beam_t
     private
     type(interaction_t) :: int
  end type beam_t


  interface assignment(=)
     module procedure beam_assign
  end interface

  interface interaction_set_source_link
     module procedure interaction_set_source_link_beam
  end interface

contains

  subroutine beam_data_init (beam_data, n)
    type(beam_data_t), intent(out) :: beam_data
    integer, intent(in) :: n
    beam_data%n = n
    allocate (beam_data%flv (n))
    allocate (beam_data%mass (n))
    allocate (beam_data%pmatrix (n))
    allocate (beam_data%p_cm (n))
    allocate (beam_data%p (n))
    beam_data%initialized = .true.
  end subroutine beam_data_init

  subroutine beam_data_final (beam_data)
    type(beam_data_t), intent(inout) :: beam_data
    beam_data%initialized = .false.
  end subroutine beam_data_final

  subroutine beam_data_write (beam_data, unit, verbose, write_md5sum)
    class(beam_data_t), intent(in) :: beam_data
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, write_md5sum
    integer :: prt_name_len
    logical :: verb, write_md5
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    verb = .false.;  if (present (verbose))  verb = verbose
    write_md5 = verb;  if (present (write_md5sum)) write_md5 = write_md5sum
    if (.not. beam_data%initialized) then
       write (u, "(1x,A)") "Beam data: [undefined]"
       return
    end if
    prt_name_len = maxval (len (flavor_get_name (beam_data%flv)))
    select case (beam_data%n)
    case (1)
       write (u, "(1x,A)") "Beam data (decay):"
       if (verb) then
          call write_prt (1)
          call beam_data%pmatrix(1)%write (u)          
          write (u, *) "R.f. momentum:"
          call vector4_write (beam_data%p_cm(1), u)
          write (u, *) "Lab momentum:"
          call vector4_write (beam_data%p(1), u)
       else
          call write_prt (1)
       end if
    case (2)
       write (u, "(1x,A)") "Beam data (collision):"
       if (verb) then
          call write_prt (1)
          call beam_data%pmatrix(1)%write (u)          
          call write_prt (2)
          call beam_data%pmatrix(2)%write (u)
          call write_sqrts
          write (u, *) "C.m. momenta:"
          call vector4_write (beam_data%p_cm(1), u)
          call vector4_write (beam_data%p_cm(2), u)
          write (u, *) "Lab momenta:"
          call vector4_write (beam_data%p(1), u)
          call vector4_write (beam_data%p(2), u)
       else
          call write_prt (1)
          call write_prt (2)
          call write_sqrts
       end if
    end select
    if (allocated (beam_data%L_cm_to_lab)) then
       if (verb) then
          call lorentz_transformation_write (beam_data%L_cm_to_lab, u)
       else
          write (u, "(1x,A)")  "Beam structure: lab and c.m. frame differ"
       end if
    end if
    if (write_md5) then
       write (u, *) "MD5 sum: ", beam_data%md5sum
    end if
  contains
    subroutine write_sqrts
      character(80) :: sqrts_str
      write (sqrts_str, "(" // FMT_19 // ")")  beam_data%sqrts
      write (u, "(3x,A)")  "sqrts = " // trim (adjustl (sqrts_str)) // " GeV"
    end subroutine write_sqrts
    subroutine write_prt (i)
      integer, intent(in) :: i
      character(80) :: name_str, mass_str
      write (name_str, "(A)")  char (flavor_get_name (beam_data%flv(i)))
      write (mass_str, "(ES13.7)")  beam_data%mass(i)
      write (u, "(3x,A)", advance="no") &
           name_str(:prt_name_len) // "  (mass = " &
           // trim (adjustl (mass_str)) // " GeV)"
      if (beam_data%pmatrix(i)%is_polarized ()) then
         write (u, "(2x,A)")  "polarized"
      else
         write (u, *)
      end if
    end subroutine write_prt
  end subroutine beam_data_write

  function beam_data_are_valid (beam_data) result (flag)
    logical :: flag
    type(beam_data_t), intent(in) :: beam_data
    flag = beam_data%initialized
  end function beam_data_are_valid

  subroutine beam_data_check_scattering (beam_data, sqrts)
    type(beam_data_t), intent(in) :: beam_data
    real(default), intent(in), optional :: sqrts
    if (beam_data_are_valid (beam_data)) then
       if (present (sqrts)) then
          if (sqrts /= beam_data%sqrts) then
             call msg_error ("Current setting of sqrts is inconsistent " &
                  // "with beam setup (ignored).")
          end if
       end if
    else
       call msg_bug ("Beam setup: invalid beam data")
    end if
  end subroutine beam_data_check_scattering

  function beam_data_get_n_in (beam_data) result (n_in)
    integer :: n_in
    type(beam_data_t), intent(in) :: beam_data
    n_in = beam_data%n
  end function beam_data_get_n_in

  function beam_data_get_flavor (beam_data) result (flv)
    type(flavor_t), dimension(:), allocatable :: flv
    type(beam_data_t), intent(in) :: beam_data
    allocate (flv (beam_data%n))
    flv = beam_data%flv
  end function beam_data_get_flavor

  function beam_data_get_energy (beam_data) result (e)
    real(default), dimension(:), allocatable :: e
    type(beam_data_t), intent(in) :: beam_data
    allocate (e (beam_data%n))
    if (beam_data%initialized) then
       e = energy (beam_data%p)
    else
       e = 0
    end if
  end function beam_data_get_energy

  function beam_data_get_sqrts (beam_data) result (sqrts)
    real(default) :: sqrts
    type(beam_data_t), intent(in) :: beam_data
    sqrts = beam_data%sqrts
  end function beam_data_get_sqrts
  
  function beam_data_cm_frame (beam_data) result (flag)
    type(beam_data_t), intent(in) :: beam_data
    logical :: flag
    flag = beam_data%lab_is_cm_frame
  end function beam_data_cm_frame
  
  function beam_data_get_md5sum (beam_data, sqrts) result (md5sum_beams)
    type(beam_data_t), intent(in) :: beam_data
    real(default), intent(in) :: sqrts
    character(32) :: md5sum_beams
    character(80) :: buffer
    if (beam_data%md5sum /= "") then
       md5sum_beams = beam_data%md5sum
    else
       write (buffer, *)  sqrts
       md5sum_beams = md5sum (buffer)
    end if
  end function beam_data_get_md5sum

  subroutine beam_data_init_structure &
       (beam_data, structure, sqrts, model, decay_rest_frame)
    type(beam_data_t), intent(out) :: beam_data
    type(beam_structure_t), intent(in) :: structure
    integer :: n_beam
    real(default), intent(in) :: sqrts
    class(model_data_t), intent(in), target :: model
    logical, intent(in), optional :: decay_rest_frame
    type(flavor_t), dimension(:), allocatable :: flv
    n_beam = structure%get_n_beam ()
    allocate (flv (n_beam))
    call flavor_init (flv, structure%get_prt (), model)
    if (structure%asymmetric ()) then
       if (structure%polarized ()) then
          call beam_data_init_momenta (beam_data, &
               structure%get_momenta (), flv, &
               structure%get_smatrix (), structure%get_pol_f ())
       else
          call beam_data_init_momenta (beam_data, &
               structure%get_momenta (), flv)
       end if
    else
       select case (n_beam)
       case (1)
          if (structure%polarized ()) then
             call beam_data_init_decay (beam_data, flv, &
                  structure%get_smatrix (), structure%get_pol_f (), &
                  rest_frame = decay_rest_frame)
          else
             call beam_data_init_decay (beam_data, flv, &
                  rest_frame = decay_rest_frame)
          end if
       case (2)
          if (structure%polarized ()) then
             call beam_data_init_sqrts (beam_data, sqrts, flv, &
                  structure%get_smatrix (), structure%get_pol_f ())
          else
             call beam_data_init_sqrts (beam_data, sqrts, flv)
          end if
       case default
          call msg_bug ("Beam data: invalid beam structure object")
       end select
    end if
  end subroutine beam_data_init_structure
    
  subroutine beam_data_init_sqrts (beam_data, sqrts, flv, smatrix, pol_f)
    type(beam_data_t), intent(out) :: beam_data
    real(default), intent(in) :: sqrts
    type(flavor_t), dimension(:), intent(in) :: flv
    type(smatrix_t), dimension(:), intent(in), optional :: smatrix
    real(default), dimension(:), intent(in), optional :: pol_f
    real(default), dimension(size(flv)) :: E, p
    call beam_data_init (beam_data, size (flv))
    beam_data%sqrts = sqrts
    beam_data%lab_is_cm_frame = .true.
    select case (beam_data%n)
    case (1)
       E = sqrts;  p = 0
       beam_data%p_cm = vector4_moving (E, p, 3)
       beam_data%p = beam_data%p_cm
    case (2)
       beam_data%p_cm = colliding_momenta (sqrts, flavor_get_mass (flv))
       beam_data%p = colliding_momenta (sqrts, flavor_get_mass (flv))
    end select
    call beam_data_finish_initialization (beam_data, flv, smatrix, pol_f)
  end subroutine beam_data_init_sqrts

  subroutine beam_data_init_momenta (beam_data, p3, flv, smatrix, pol_f)
    type(beam_data_t), intent(out) :: beam_data
    type(vector3_t), dimension(:), intent(in) :: p3
    type(flavor_t), dimension(:), intent(in) :: flv
    type(smatrix_t), dimension(:), intent(in), optional :: smatrix
    real(default), dimension(:), intent(in), optional :: pol_f
    type(vector4_t) :: p0
    type(vector4_t), dimension(:), allocatable :: p, p_cm_rot
    real(default), dimension(size(p3)) :: e
    real(default), dimension(size(flv)) :: m
    type(lorentz_transformation_t) :: L_boost, L_rot
    call beam_data_init (beam_data, size (flv))
    m = flavor_get_mass (flv)
    e = sqrt (p3 ** 2 + m ** 2)
    allocate (p (beam_data%n))
    p = vector4_moving (e, p3)
    p0 = sum (p)
    beam_data%p = p
    beam_data%lab_is_cm_frame = .false.
    beam_data%sqrts = p0 ** 1
    L_boost = boost (p0, beam_data%sqrts)
    allocate (p_cm_rot (beam_data%n))
    p_cm_rot = inverse (L_boost) * p
    allocate (beam_data%L_cm_to_lab)
    select case (beam_data%n)
    case (1)
       beam_data%L_cm_to_lab = L_boost
       beam_data%p_cm = vector4_at_rest (beam_data%sqrts)
    case (2)
       L_rot = rotation_to_2nd (3, space_part (p_cm_rot(1)))
       beam_data%L_cm_to_lab = L_boost * L_rot
       beam_data%p_cm = &
            colliding_momenta (beam_data%sqrts, flavor_get_mass (flv))
    end select
    call beam_data_finish_initialization (beam_data, flv, smatrix, pol_f)
  end subroutine beam_data_init_momenta
    
  subroutine beam_data_finish_initialization (beam_data, flv, smatrix, pol_f)
    type(beam_data_t), intent(inout) :: beam_data
    type(flavor_t), dimension(:), intent(in) :: flv
    type(smatrix_t), dimension(:), intent(in), optional :: smatrix
    real(default), dimension(:), intent(in), optional :: pol_f
    integer :: i
    do i = 1, beam_data%n
       beam_data%flv(i) = flv(i)
       beam_data%mass(i) = flavor_get_mass (flv(i))
       if (present (smatrix)) then
          if (size (smatrix) /= beam_data%n) &
               call msg_fatal ("Beam data: &
               &polarization density array has wrong dimension")
          beam_data%pmatrix(i) = smatrix(i)
          if (present (pol_f)) then
             if (size (pol_f) /= size (smatrix)) &
                  call msg_fatal ("Beam data: &
                  &polarization fraction array has wrong dimension")
             call beam_data%pmatrix(i)%normalize (flv(i), pol_f(i))
          else
             call beam_data%pmatrix(i)%normalize (flv(i), 1._default)
          end if
       else
          call beam_data%pmatrix(i)%init (2, 0)
          call beam_data%pmatrix(i)%normalize (flv(i), 0._default)
       end if
    end do
    call beam_data_compute_md5sum (beam_data)
  end subroutine beam_data_finish_initialization

  subroutine beam_data_compute_md5sum (beam_data)
    type(beam_data_t), intent(inout) :: beam_data
    integer :: unit
    unit = free_unit ()
    open (unit = unit, status = "scratch", action = "readwrite")
    call beam_data_write (beam_data, unit, write_md5sum = .false., &
       verbose = .true.)
    rewind (unit)
    beam_data%md5sum = md5sum (unit)
    close (unit)
  end subroutine beam_data_compute_md5sum

  subroutine beam_data_init_decay (beam_data, flv, smatrix, pol_f, rest_frame)
    type(beam_data_t), intent(out) :: beam_data
    type(flavor_t), dimension(1), intent(in) :: flv
    type(smatrix_t), dimension(1), intent(in), optional :: smatrix
    real(default), dimension(:), intent(in), optional :: pol_f
    logical, intent(in), optional :: rest_frame
    real(default), dimension(1) :: m
    m = flavor_get_mass (flv)
    if (present (smatrix)) then
       call beam_data_init_sqrts (beam_data, m(1), flv, smatrix, pol_f)
    else
       call beam_data_init_sqrts (beam_data, m(1), flv, smatrix, pol_f)
    end if
    if (present (rest_frame))  beam_data%lab_is_cm_frame = rest_frame
  end subroutine beam_data_init_decay

  function beam_data_masses_are_consistent (beam_data) result (flag)
    logical :: flag
    type(beam_data_t), intent(in) :: beam_data
    flag = all (beam_data%mass == flavor_get_mass (beam_data%flv))
  end function beam_data_masses_are_consistent

  subroutine beam_init (beam, beam_data)
    type(beam_t), intent(out) :: beam
    type(beam_data_t), intent(in), target :: beam_data
    type(quantum_numbers_mask_t), dimension(beam_data%n) :: mask
    type(state_matrix_t), target :: state_hel, state_fc, state_tmp
    type(state_iterator_t) :: it_hel, it_tmp
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    type(polarization_t), dimension(:), allocatable :: pol
    integer :: i
    mask = new_quantum_numbers_mask (.false., .false., &
         .not. beam_data%pmatrix%is_polarized (), &
         mask_hd = beam_data%pmatrix%is_diagonal ())
    call interaction_init &
         (beam%int, 0, 0, beam_data%n, mask=mask, store_values=.true.)
    allocate (pol (beam_data%n))
    do i = 1, size (pol)
       call polarization_init_pmatrix (pol(i), beam_data%pmatrix(i))
    end do
    call combine_polarization_states (pol, state_hel)
    do i = 1, size (pol)
       call polarization_final (pol(i))
    end do
    allocate (qn (beam_data%n))
    call quantum_numbers_init &
         (qn, beam_data%flv, color_from_flavor (beam_data%flv, 1))
    call state_matrix_init (state_fc)
    call state_matrix_add_state (state_fc, qn)
    call merge_state_matrices (state_hel, state_fc, state_tmp)
    call state_iterator_init (it_hel, state_hel)
    call state_iterator_init (it_tmp, state_tmp)
    do while (state_iterator_is_valid (it_hel))
       call interaction_add_state (beam%int, &
            state_iterator_get_quantum_numbers (it_tmp), &
            value=state_iterator_get_matrix_element (it_hel))
       call state_iterator_advance (it_hel)
       call state_iterator_advance (it_tmp)
    end do
    call interaction_freeze (beam%int)
    call interaction_set_momenta &
         (beam%int, beam_data%p, outgoing = .true.)
    call state_matrix_final (state_hel)
    call state_matrix_final (state_fc)
    call state_matrix_final (state_tmp)
  end subroutine beam_init

  elemental subroutine beam_final (beam)
    type(beam_t), intent(inout) :: beam
    call interaction_final (beam%int)
  end subroutine beam_final

  subroutine beam_write (beam, unit, verbose, show_momentum_sum, show_mass)
    type(beam_t), intent(in) :: beam
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, show_momentum_sum, show_mass
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    select case (interaction_get_n_out (beam%int))
    case (1);  write (u, *) "Decaying particle:"
    case (2);  write (u, *) "Colliding beams:"
    end select
    call interaction_write &
         (beam%int, unit, verbose = verbose, show_momentum_sum = &
            show_momentum_sum, show_mass = show_mass)
  end subroutine beam_write

  subroutine beam_assign (beam_out, beam_in)
    type(beam_t), intent(out) :: beam_out
    type(beam_t), intent(in) :: beam_in
    beam_out%int = beam_in%int
  end subroutine beam_assign

  subroutine interaction_set_source_link_beam (int, i, beam1, i1)
    type(interaction_t), intent(inout) :: int
    type(beam_t), intent(in), target :: beam1
    integer, intent(in) :: i, i1
    call interaction_set_source_link (int, i, beam1%int, i1)
  end subroutine interaction_set_source_link_beam

  function beam_get_int_ptr (beam) result (int)
    type(interaction_t), pointer :: int
    type(beam_t), intent(in), target :: beam
    int => beam%int
  end function beam_get_int_ptr

  subroutine beam_set_momenta (beam, p)
    type(beam_t), intent(inout) :: beam
    type(vector4_t), dimension(:), intent(in) :: p
    call interaction_set_momenta (beam%int, p)
  end subroutine beam_set_momenta

  subroutine beam_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (beam_1, "beam_1", &
         "check basic beam setup", &
         u, results)
    call test (beam_2, "beam_2", &
         "beam initialization", &
         u, results)
    call test (beam_3, "beam_3", &
         "generic beam momenta", &
         u, results)
  end subroutine beam_test


  subroutine beam_1 (u)
    integer, intent(in) :: u
    type(beam_data_t), target :: beam_data
    type(beam_t) :: beam
    real(default) :: sqrts
    type(flavor_t), dimension(2) :: flv
    type(smatrix_t), dimension(2) :: smatrix
    real(default), dimension(2) :: pol_f
    type(model_data_t), target :: model
    
    write (u, "(A)")  "* Test output: beam_1"
    write (u, "(A)")  "*   Purpose: test basic beam setup"
    write (u, "(A)")      
        
    write (u, "(A)")  "* Reading model file"
    write (u, "(A)") 

    call reset_interaction_counter ()

    call model%init_sm_test ()

    write (u, "(A)")  "* 1: Scattering process"
    write (u, "(A)")
    
    sqrts = 500
    call flavor_init (flv, [1,-1], model)

    call smatrix(1)%init (2, 1)
    call smatrix(1)%set_entry (1, [1,1], (1._default, 0._default))
    pol_f(1) = 0.5_default

    !!! 2.1 version:
    ! call polarization_init_circular (pol(1), flv(1), 0.5_default)

    call smatrix(2)%init (2, 3)
    call smatrix(2)%set_entry (1, [1,1], (1._default, 0._default))
    call smatrix(2)%set_entry (2, [-1,-1], (1._default, 0._default))
    call smatrix(2)%set_entry (3, [-1,1], (1._default, 0._default))
    pol_f(2) = 1._default

    !!! 2.1 version:
    ! call polarization_init_transversal (pol(2), flv(2), 0._default, 1._default)
    call beam_data_init_sqrts (beam_data, sqrts, flv, smatrix, pol_f)
    call beam_data_write (beam_data, u)
    write (u, "(A)")
    call beam_init (beam, beam_data)
    call beam_write (beam, u)
    call beam_final (beam)
    call beam_data_final (beam_data)
    
    write (u, "(A)")
    write (u, "(A)")  "* 2: Decay"
    write (u, "(A)")
    call flavor_init (flv(1), 23, model)
    call smatrix(1)%init (2, 1)
    call smatrix(1)%set_entry (1, [0,0], (1._default, 0._default))
    pol_f(1) = 0.4_default

    !!! 2.1 version:
    ! call polarization_init_longitudinal (pol(1), flv(1), 0.4_default)
    call beam_data_init_decay (beam_data, flv(1:1), smatrix(1:1), pol_f(1:1))
    call beam_data_write (beam_data, u)
    write (u, "(A)")
    call beam_init (beam, beam_data)
    call beam_write (beam, u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"            
       
    call beam_final (beam)
    call beam_data_final (beam_data)

    call model%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: beam_1"        
    
  end subroutine beam_1

  subroutine beam_2 (u)
    integer, intent(in) :: u
    type(beam_data_t), target :: beam_data
    type(beam_t) :: beam
    real(default) :: sqrts
    type(flavor_t), dimension(2) :: flv
    integer, dimension(0) :: no_records
    type(beam_structure_t) :: beam_structure
    type(model_data_t), target :: model
    
    write (u, "(A)")  "* Test output: beam_2"
    write (u, "(A)")  "*   Purpose: transfer beam polarization using &
         &beam structure"
    write (u, "(A)")      
        
    write (u, "(A)")  "* Reading model file"
    write (u, "(A)") 

    call reset_interaction_counter ()

    call model%init_sm_test ()

    write (u, "(A)")  "* 1: Scattering process"
    write (u, "(A)")
    
    sqrts = 500
    call flavor_init (flv, [1,-1], model)
    call beam_structure%init_sf (flavor_get_name (flv), no_records)

    call beam_structure%init_pol (2)

    call beam_structure%init_smatrix (1, 1)
    call beam_structure%set_sentry (1, 1, [1,1], (1._default, 0._default))

    call beam_structure%init_smatrix (2, 3)
    call beam_structure%set_sentry (2, 1, [1,1], (1._default, 0._default))
    call beam_structure%set_sentry (2, 2, [-1,-1], (1._default, 0._default))
    call beam_structure%set_sentry (2, 3, [-1,1], (1._default, 0._default))

    call beam_structure%set_pol_f ([0.5_default, 1._default])
    call beam_structure%write (u)
    write (u, *)
    
    call beam_data_init_structure (beam_data, beam_structure, sqrts, model)
    call beam_data_write (beam_data, u)
    write (u, *)
 
    call beam_init (beam, beam_data)
    call beam_write (beam, u)

    call beam_final (beam)
    call beam_data_final (beam_data)
    call beam_structure%final_pol ()
    call beam_structure%final_sf ()
    
    write (u, "(A)")
    write (u, "(A)")  "* 2: Decay"
    write (u, "(A)")

    call flavor_init (flv(1), 23, model)
    call beam_structure%init_sf ([flavor_get_name (flv(1))], no_records)

    call beam_structure%init_pol (1)

    call beam_structure%init_smatrix (1, 1)
    call beam_structure%set_sentry (1, 1, [0,0], (1._default, 0._default))
    call beam_structure%set_pol_f ([0.4_default])
    call beam_structure%write (u)
    write (u, *)
    
    call beam_data_init_structure (beam_data, beam_structure, sqrts, model)
    call beam_data_write (beam_data, u)
    write (u, "(A)")
    call beam_init (beam, beam_data)
    call beam_write (beam, u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"            
       
    call beam_final (beam)
    call beam_data_final (beam_data)

    call model%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: beam_2"        
    
  end subroutine beam_2

  subroutine beam_3 (u)
    integer, intent(in) :: u
    type(beam_data_t), target :: beam_data
    type(beam_t) :: beam
    type(flavor_t), dimension(2) :: flv
    integer, dimension(0) :: no_records
    type(model_data_t), target :: model
    type(beam_structure_t) :: beam_structure
    type(vector3_t), dimension(2) :: p3
    type(vector4_t), dimension(2) :: p
    
    write (u, "(A)")  "* Test output: beam_3"
    write (u, "(A)")  "*   Purpose: set up beams with generic momenta"
    write (u, "(A)")      
        
    write (u, "(A)")  "* Reading model file"
    write (u, "(A)") 

    call reset_interaction_counter ()

    call model%init_sm_test ()

    write (u, "(A)")  "* 1: Scattering process"
    write (u, "(A)")
    
    call flavor_init (flv, [2212,2212], model)

    p3(1) = vector3_moving ([5._default, 0._default, 10._default])
    p3(2) = -vector3_moving ([1._default, 1._default, -10._default])

    call beam_structure%init_sf (flavor_get_name (flv), no_records)
    call beam_structure%set_momentum (p3 ** 1)
    call beam_structure%set_theta (polar_angle (p3))
    call beam_structure%set_phi (azimuthal_angle (p3))
    call beam_structure%write (u)
    write (u, *)

    call beam_data_init_structure (beam_data, beam_structure, 0._default, model)
    call beam_data_write (beam_data, u, verbose = .true.)
    write (u, *)
 
    write (u, "(1x,A)")  "Beam momenta reconstructed from LT:"
    p = beam_data%L_cm_to_lab * beam_data%p_cm
    call pacify (p, 1e-12_default)
    call vector4_write (p(1), u)
    call vector4_write (p(2), u)
    write (u, "(A)")

    call beam_init (beam, beam_data)
    call beam_write (beam, u)

    call beam_final (beam)
    call beam_data_final (beam_data)
    call beam_structure%final_sf ()
    call beam_structure%final_mom ()
    
    write (u, "(A)")
    write (u, "(A)")  "* 2: Decay"
    write (u, "(A)")

    call flavor_init (flv(1), 23, model)
    p3(1) = vector3_moving ([10._default, 5._default, 50._default])
    
    call beam_structure%init_sf ([flavor_get_name (flv(1))], no_records)
    call beam_structure%set_momentum ([p3(1) ** 1])
    call beam_structure%set_theta ([polar_angle (p3(1))])
    call beam_structure%set_phi ([azimuthal_angle (p3(1))])
    call beam_structure%write (u)
    write (u, *)

    call beam_data_init_structure (beam_data, beam_structure, 0._default, model)
    call beam_data_write (beam_data, u, verbose = .true.)
    write (u, "(A)")

    write (u, "(1x,A)")  "Beam momentum reconstructed from LT:"
    p(1) = beam_data%L_cm_to_lab * beam_data%p_cm(1)
    call pacify (p(1), 1e-12_default)
    call vector4_write (p(1), u)
    write (u, "(A)")

    call beam_init (beam, beam_data)
    call beam_write (beam, u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"            
       
    call beam_final (beam)
    call beam_data_final (beam_data)
    call beam_structure%final_sf ()
    call beam_structure%final_mom ()

    call model%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: beam_3"        
    
  end subroutine beam_3


end module beams
