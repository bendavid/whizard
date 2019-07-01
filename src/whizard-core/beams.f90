! WHIZARD 2.1.1 September 18 2012
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

module beams

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use constants !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use lorentz !NODEP!
  use md5
  use models
  use flavors
  use colors
  use polarizations
  use quantum_numbers
  use state_matrices
  use interactions

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
  public :: beam_data_get_md5sum
  public :: beam_data_init_sqrts
  public :: beam_data_init_momenta
  public :: beam_data_init_decay
  public :: beam_data_set_polarization
  public :: beam_data_kill_polarization
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
     type(polarization_t), dimension(:), allocatable :: pol
     logical :: lab_is_cm_frame = .true.
     type(vector4_t), dimension(:), allocatable :: p_cm
     type(vector4_t), dimension(:), allocatable :: p
     type(lorentz_transformation_t), pointer  :: L_cm_to_lab => null ()
     real(default) :: sqrts = 0
     character(32) :: md5sum = ""
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
    allocate (beam_data%pol (n))
    allocate (beam_data%p_cm (n))
    allocate (beam_data%p (n))
    beam_data%initialized = .true.
  end subroutine beam_data_init

  subroutine beam_data_final (beam_data)
    type(beam_data_t), intent(inout) :: beam_data
    beam_data%initialized = .false.
    if (allocated (beam_data%pol))  call polarization_final (beam_data%pol)
    if (associated (beam_data%L_cm_to_lab))  deallocate (beam_data%L_cm_to_lab)
  end subroutine beam_data_final

  subroutine beam_data_write (beam_data, unit, verbose, write_md5sum)
    type(beam_data_t), intent(in) :: beam_data
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, write_md5sum
    integer :: prt_name_len
    logical :: verb, write_md5
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    verb = .false.;  if (present (verbose))  verb = verbose
    write_md5 = verb;  if (present (write_md5sum)) write_md5 = write_md5sum
    if (.not. beam_data%initialized) then
       write (u, "(A)") "Beam data: [undefined]"
       return
    end if
    prt_name_len = maxval (len (flavor_get_name (beam_data%flv)))
    select case (beam_data%n)
    case (1)
       write (u, "(A)") "Beam data (decay):"
       if (verb) then
          call write_prt (1)
          call polarization_write (beam_data%pol(1), u)
          write (u, *) "R.f. momentum:"
          call vector4_write (beam_data%p_cm(1), u)
          write (u, *) "Lab momentum:"
          call vector4_write (beam_data%p(1), u)
       else
          call write_prt (1)
       end if
    case (2)
       write (u, "(A)") "Beam data (collision):"
       if (verb) then
          call write_prt (1)
          call polarization_write (beam_data%pol(1), u)
          call write_prt (2)
          call polarization_write (beam_data%pol(2), u)
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
    if (associated (beam_data%L_cm_to_lab)) &
         call lorentz_transformation_write (beam_data%L_cm_to_lab, u)
    if (write_md5) then
       write (u, *) "MD5 sum: ", beam_data%md5sum
    end if
  contains
    subroutine write_sqrts
      character(80) :: sqrts_str
      write (sqrts_str, "(1PG21.15)")  beam_data%sqrts
      write (u, "(1x,A)")  "sqrts = " // trim (adjustl (sqrts_str)) // " GeV"
    end subroutine write_sqrts
    subroutine write_prt (i)
      integer, intent(in) :: i
      character(80) :: name_str, mass_str
      write (name_str, "(A)")  char (flavor_get_name (beam_data%flv(i)))
      write (mass_str, "(1PG15.8)")  beam_data%mass(i)
      write (u, "(1x,A)")  name_str(:prt_name_len) // "  (mass = " &
           // trim (adjustl (mass_str)) // " GeV)"
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

  subroutine beam_data_init_sqrts (beam_data, sqrts, flv, pol, p_cm, theta, phi)
    type(beam_data_t), intent(out) :: beam_data
    real(default), intent(in) :: sqrts
    type(flavor_t), dimension(:), intent(in) :: flv
    type(polarization_t), dimension(:), intent(in), optional :: pol
    real(default), intent(in), optional :: p_cm, theta, phi
    real(default), dimension(size(flv)) :: E, p
    call beam_data_init (beam_data, size (flv))
    beam_data%sqrts = sqrts
    beam_data%lab_is_cm_frame = &
         .not. present (p_cm) .and. .not. present (theta)
    select case (beam_data%n)
    case (1)
       if (present (p_cm)) then
          E = sqrt (sqrts**2 + p_cm**2)
          p = p_cm
       else
          E = sqrts;  p = 0
       end if
       beam_data%p_cm = vector4_moving (E, p, 3)
       beam_data%p = beam_data%p_cm
    case (2)
       beam_data%p_cm = colliding_momenta (sqrts, flavor_get_mass (flv))
       beam_data%p = colliding_momenta (sqrts, flavor_get_mass (flv), p_cm)
    end select
    call beam_data_finish_initialization (beam_data, flv, pol, theta, phi)
  end subroutine beam_data_init_sqrts

  subroutine beam_data_init_momenta (beam_data, p, flv, pol, alpha, theta, phi)
    type(beam_data_t), intent(out) :: beam_data
    real(default), dimension(2), intent(in) :: p
    type(flavor_t), dimension(2), intent(in) :: flv
    type(polarization_t), dimension(2), intent(in), optional :: pol
    real(default), intent(in), optional :: alpha, theta, phi
    real(default), dimension(2) :: m, e
    real(default) :: ca, sa
    call beam_data_init (beam_data, 2)
    m = flavor_get_mass (flv)
    e = sqrt (p**2 + m**2)
    beam_data%p(1) = vector4_moving (e(1), p(1), 3)
    beam_data%p(2) = vector4_moving (e(2),-p(2), 3)
    if (present (alpha)) then
       ca = cos (alpha / 2)
       sa = sin (alpha / 2)
       beam_data%p(1) = rotation (ca, sa, 2) * beam_data%p(1) 
       beam_data%p(2) = rotation (ca,-sa, 2) * beam_data%p(2) 
       beam_data%sqrts = &
            sqrt (2 * (e(1)*e(2) + p(1)*p(2)*cos(alpha)) + m(1)**2 + m(2)**2)
    else
       beam_data%sqrts = &
            sqrt (2 * (e(1)*e(2) + p(1)*p(2)) + m(1)**2 + m(2)**2)
    end if
    beam_data%p_cm = colliding_momenta (beam_data%sqrts, m)
    call beam_data_finish_initialization (beam_data, flv, pol, theta, phi)
  end subroutine beam_data_init_momenta

  subroutine beam_data_finish_initialization (beam_data, flv, pol, theta, phi)
    type(beam_data_t), intent(inout) :: beam_data
    type(flavor_t), dimension(:), intent(in) :: flv
    type(polarization_t), dimension(:), intent(in), optional :: pol
    real(default), intent(in), optional :: theta, phi
    integer :: i
    if (present (theta)) then
       beam_data%p = rotation (theta, 2) * beam_data%p
       if (present (phi)) then
          beam_data%p = rotation (phi, 3) * beam_data%p
       end if
    end if
    do i = 1, beam_data%n
       beam_data%flv(i) = flv(i)
       beam_data%mass(i) = flavor_get_mass (flv(i))
       if (present (pol)) then
          beam_data%pol(i) = pol(i)
       else
          call polarization_init_unpolarized (beam_data%pol(i), flv(i))
       end if
    end do
    call beam_data_compute_md5sum (beam_data)
    call beam_data_write (beam_data)
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

  subroutine beam_data_init_decay &
       (beam_data, flv, pol, p_cm, theta, phi)
    type(beam_data_t), intent(out) :: beam_data
    type(flavor_t), dimension(1), intent(in) :: flv
    type(polarization_t), dimension(1), intent(in), optional :: pol
    real(default), intent(in), optional :: p_cm, theta, phi
    real(default), dimension(1) :: m
    type(polarization_t), dimension(1) :: polarization
    m = flavor_get_mass (flv)
    if (present (pol)) then
       call beam_data_init_sqrts &
            (beam_data, m(1), flv, pol, p_cm, theta, phi)
    else
       call polarization_init_trivial (polarization(1), flv(1))
       call beam_data_init_sqrts &
            (beam_data, m(1), flv, polarization, p_cm, theta, phi)
    end if
  end subroutine beam_data_init_decay

  subroutine beam_data_set_polarization (beam_data, pol)
     type(beam_data_t), intent(inout) :: beam_data
     type(polarization_t), dimension(:), intent(in) :: pol
     integer :: i
     if (size (pol) /= beam_data%n) call msg_bug ( &
        "beam_data_set_polarization_pol: initial state multiplicty mismatch")
     !do i = 1, beam_data%n
     !   call polarization_final (beam_data%pol(i))
     !end do
     beam_data%pol = pol
     call beam_data_compute_md5sum (beam_data)
  end subroutine beam_data_set_polarization

  subroutine beam_data_kill_polarization (beam_data)
    type(beam_data_t), intent(inout) :: beam_data
    if (beam_data%n == 1) then
       call polarization_init_trivial (beam_data%pol(1), beam_data%flv(1))
    else
       call polarization_init_unpolarized (beam_data%pol(1), beam_data%flv(1))
       call polarization_init_unpolarized (beam_data%pol(2), beam_data%flv(2))
    end if
  end subroutine beam_data_kill_polarization

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
    mask = new_quantum_numbers_mask (.false., .false., &
         .not. polarization_is_polarized (beam_data%pol), &
         mask_hd = polarization_is_diagonal (beam_data%pol))
    call interaction_init &
         (beam%int, 0, 0, beam_data%n, mask=mask, store_values=.true.)
    call combine_polarization_states (beam_data%pol, state_hel)
    allocate (qn (beam_data%n))
    call quantum_numbers_init &
         (qn, beam_data%flv, color_from_flavor (beam_data%flv))
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
    u = output_unit (unit);  if (u < 0)  return
    select case (interaction_get_n_out (beam%int))
    case (1);  write (u, *) "Decaying particle:"
    case (2);  write (u, *) "Colliding beams:"
    end select
    call interaction_write &
         (beam%int, unit, verbose, show_momentum_sum, show_mass)
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

  subroutine beam_test ()
    use os_interface, only: os_data_t
    type(os_data_t) :: os_data
    type(beam_data_t), target :: beam_data
    type(beam_t) :: beam
    real(default) :: sqrts
    type(flavor_t), dimension(2) :: flv
    type(polarization_t), dimension(2) :: pol
    type(model_t), pointer :: model
    print *, "*** Read model file"
    call syntax_model_file_init ()
    call model_list_read_model &
         (var_str("SM"), var_str("SM.mdl"), os_data, model)
    call syntax_model_file_final ()
    print *
    print *, "*** Scattering"
    sqrts = 500
    call flavor_init (flv, ((/1,-1/)), model)
    call polarization_init_circular (pol(1), flv(1), 0.5_default)
    call polarization_init_transversal (pol(2), flv(2), 0._default, 1._default)
    call beam_data_init_sqrts (beam_data, sqrts, flv, pol)
    call beam_data_write (beam_data)
    print *
    call beam_init (beam, beam_data)
    call beam_write (beam)
    call beam_final (beam)
    call beam_data_final (beam_data)
    print *
    print *, "*** Decay"
    call flavor_init (flv(1), 23, model)
    call polarization_init_longitudinal (pol(1), flv(1), 0.4_default)
    call beam_data_init_decay (beam_data, flv(1:1), pol(1:1))
    call beam_data_write (beam_data)
    print *
    call beam_init (beam, beam_data)
    call beam_write (beam)
    call beam_final (beam)
    call beam_data_final (beam_data)
  end subroutine beam_test


end module beams
