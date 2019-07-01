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

module sf_beam_events

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use lorentz !NODEP!
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

  public :: beam_events_data_t
  public :: beam_events_data_init
  public :: beam_events_data_write
  public :: beam_events_data_open
  public :: beam_events_data_close
  public :: interaction_init_beam_events
  public :: interaction_apply_beam_events

  type :: beam_events_data_t
     private
     logical, dimension(2) :: affects_beam
     type(flavor_t), dimension(2) :: flv
     real(default), dimension(2) :: mass = 0
     type(string_t) :: file
     logical :: warn_eof = .true.
     integer :: unit = 0
  end type beam_events_data_t


contains

  subroutine beam_events_data_init (data, affects_beam, flv_in, file, warn_eof)
    type(beam_events_data_t), intent(out) :: data
    logical, dimension(2), intent(in) :: affects_beam
    type(flavor_t), dimension(2), intent(in) :: flv_in
    type(string_t), intent(in) :: file
    logical, intent(in) :: warn_eof
    data%affects_beam = affects_beam
    data%flv = flv_in
    data%mass = flavor_get_mass (data%flv)
    data%file = file
    data%warn_eof = warn_eof
  end subroutine beam_events_data_init

  subroutine beam_events_data_write (data, unit, md5)
    type(beam_events_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    integer :: u
    logical, intent(in), optional :: md5
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) "Beam-event file data:"
    write (u, *) "  prt_in  = ", char (flavor_get_name (data%flv(1))), &
         ", ", char (flavor_get_name (data%flv(2)))    
    write (u, *) "  file = '", char (data%file), "'"
    write (u, *) "  warn_eof = ", data%warn_eof
    write (u, *) "  unit = ", data%unit
  end subroutine beam_events_data_write

  subroutine beam_events_data_open (data)
    type(beam_events_data_t), intent(inout) :: data
    if (data%unit == 0) then
       data%unit = free_unit ()
       open (unit = data%unit, file = char (data%file))
    else
       call msg_fatal ("Reading beam events: file '" &
         // char (data%file) // "' is already open.")
    end if
    call msg_message ("Reading beam events from file '" &
         // char (data%file) // "'")
  end subroutine beam_events_data_open

  subroutine beam_events_data_close (data)
    type(beam_events_data_t), intent(inout) :: data
    if (data%unit /= 0) then
       close (data%unit)
       data%unit = 0
    end if
  end subroutine beam_events_data_close

  subroutine interaction_init_beam_events (int, data)
    type(interaction_t), intent(out) :: int
    type(beam_events_data_t), intent(in) :: data
    type(quantum_numbers_mask_t), dimension(4) :: mask
    integer, dimension(4) :: hel_lock
    type(quantum_numbers_t), dimension(4) :: qn_fc, qn_hel, qn
    type(polarization_t) :: pol1, pol2
    type(state_iterator_t) :: it_hel1, it_hel2
    integer :: i
    if (all (data%affects_beam)) then
       hel_lock = (/ 3, 4, 1, 2 /)
       call interaction_init &
            (int, 2, 0, 2, mask=mask, hel_lock=hel_lock, set_relations=.true.)
    else if (data%affects_beam(1)) then
       hel_lock = (/ 2, 1, 0, 0 /)
       call interaction_init &
            (int, 1, 0, 1, mask=mask(1:2), hel_lock=hel_lock(1:2), set_relations=.true.)
    else if (data%affects_beam(2)) then
       hel_lock = (/ 2, 1, 0, 0 /)
       call interaction_init &
            (int, 1, 0, 1, mask=mask(1:2), hel_lock=hel_lock(1:2), set_relations=.true.)
    end if
    do i = 1, 2
       call quantum_numbers_init (qn_fc(i), &
            flv = data%flv(i), col = color_from_flavor (data%flv(i)))
       call quantum_numbers_init (qn_fc(i+2), &
            flv = data%flv(i), col = color_from_flavor (data%flv(i)))
    end do
    call polarization_init_generic (pol1, data%flv(1))
    call state_iterator_init (it_hel1, pol1%state)
    do while (state_iterator_is_valid (it_hel1))
       qn_hel(1:1) = state_iterator_get_quantum_numbers (it_hel1)
       qn_hel(3:3) = state_iterator_get_quantum_numbers (it_hel1)
       call polarization_init_generic (pol2, data%flv(2))
       call state_iterator_init (it_hel2, pol2%state)
       do while (state_iterator_is_valid (it_hel2))
          qn_hel(2:2) = state_iterator_get_quantum_numbers (it_hel2)
          qn_hel(4:4) = state_iterator_get_quantum_numbers (it_hel2)
          qn = qn_hel .merge. qn_fc
          if (all (data%affects_beam)) then
             call interaction_add_state (int, qn)
          else if (data%affects_beam(1)) then
             call interaction_add_state (int, qn((/1,3/)))
          else if (data%affects_beam(2)) then
             call interaction_add_state (int, qn((/2,4/)))
          end if
          call state_iterator_advance (it_hel2)
       end do
       call polarization_final (pol2)
       call state_iterator_advance (it_hel1)
    end do
    call polarization_final (pol2)
    call interaction_freeze (int)
  end subroutine interaction_init_beam_events
    
  subroutine read_x (x, data)
    real(default), dimension(:), intent(out) :: x
    type(beam_events_data_t), intent(in) :: data
    read (unit=data%unit, fmt=*, end=1)  x
    return
1   if (data%warn_eof)  &
         call msg_warning ("Reading beam event file: EOF reached, rewinding.")
    rewind (data%unit)
    read (unit=data%unit, fmt=*)  x
  end subroutine read_x

  subroutine interaction_apply_beam_events (int, data)
    type(interaction_t), intent(inout), target :: int
    type(beam_events_data_t), intent(in) :: data
    type(vector4_t), dimension(2) :: k
    real(default), dimension(:), allocatable :: x
    type(splitting_data_t), dimension(2) :: sd
    type(vector4_t), dimension(4) :: q
    complex(default), parameter :: c_one = 1
    k(1) = interaction_get_momentum (int, 1)
    if (all (data%affects_beam)) then
       k(2) = interaction_get_momentum (int, 2)
    else
       k(2) = k(1)
    end if
    allocate (x (count (data%affects_beam)))
    call read_x (x, data)
    call interaction_set_matrix_element (int, c_one)
    sd = new_splitting_data (k, data%mass**2, 0._default, data%mass)
    call splitting_set_t_bounds (sd, x, 1-x)
    call splitting_set_collinear (sd)
    q((/1, 3/)) = split_momentum (k(1), sd(1))
    q((/2, 4/)) = split_momentum (k(2), sd(2))
    if (all (data%affects_beam)) then
       call interaction_set_momenta (int, q(3:4), outgoing=.true.)
    else if (data%affects_beam(1)) then
       call interaction_set_momenta (int, q(3:3), outgoing=.true.)
    else if (data%affects_beam(2)) then
       call interaction_set_momenta (int, q(4:4), outgoing=.true.)
    end if
  end subroutine interaction_apply_beam_events


end module sf_beam_events
