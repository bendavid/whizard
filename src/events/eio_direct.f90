! WHIZARD 2.6.4 Aug 23 2018
!
! Copyright (C) 1999-2018 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     cf. main AUTHORS file
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

module eio_direct

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use diagnostics
  use cputime
  use lorentz, only: vector4_t
  use particles, only: particle_set_t
  use model_data, only: model_data_t
  use event_base
  use eio_data
  use eio_base

  implicit none
  private

  public :: eio_direct_t

  type, extends (eio_t) :: eio_direct_t
     private
     logical :: i_evt_set = .false.
     integer :: i_evt = 0
     integer :: i_prc = 0
     integer :: i_mci = 0
     integer :: i_term = 0
     integer :: channel = 0
     logical :: passed_set = .false.
     logical :: passed = .true.
     type(particle_set_t) :: pset
   contains
     procedure :: write => eio_direct_write
     procedure :: final => eio_direct_final
     procedure :: init_out => eio_direct_init_out
     procedure :: init_in => eio_direct_init_in
     procedure :: switch_inout => eio_direct_switch_inout
     procedure :: output => eio_direct_output
     procedure :: input_i_prc => eio_direct_input_i_prc
     procedure :: input_event => eio_direct_input_event
     procedure :: skip => eio_direct_skip
     procedure :: has_event_index => eio_direct_has_event_index
     procedure :: get_event_index => eio_direct_get_event_index
     procedure :: passed_known => eio_direct_passed_known
     procedure :: has_passed => eio_direct_has_passed
     procedure :: get_n_in => eio_direct_get_n_in
     procedure :: get_n_out => eio_direct_get_n_out
     procedure :: get_n_tot => eio_direct_get_n_tot
     procedure :: get_momentum_array => eio_direct_get_momentum_array
     procedure :: init_direct => eio_direct_init_direct
     procedure :: set_event_index => eio_direct_set_event_index
     procedure :: reset_event_index => eio_direct_reset_event_index
     procedure :: set_selection_indices => eio_direct_set_selection_indices
     generic :: set_momentum => set_momentum_single
     generic :: set_momentum => set_momentum_all
     procedure :: set_momentum_single => eio_direct_set_momentum_single
     procedure :: set_momentum_all => eio_direct_set_momentum_all
  end type eio_direct_t


contains

  subroutine eio_direct_write (object, unit)
    class(eio_direct_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "Event direct access:"
    if (object%i_evt_set) then
       write (u, "(3x,A,1x,I0)")  "i_evt =", object%i_evt
    else
       write (u, "(3x,A)")  "i_evt = [undefined]"
    end if
    write (u, "(3x,A,1x,I0)")  "i_prc =", object%i_prc
    write (u, "(3x,A,1x,I0)")  "i_mci =", object%i_prc
    write (u, "(3x,A,1x,I0)")  "i_term =", object%i_prc
    write (u, "(3x,A,1x,I0)")  "channel =", object%i_prc
    if (object%passed_set) then
       write (u, "(3x,A,1x,L1)")  "passed =", object%passed
    else
       write (u, "(3x,A)")  "passed = [N/A]"
    end if
    call object%pset%write (u)
  end subroutine eio_direct_write

  subroutine eio_direct_final (object)
    class(eio_direct_t), intent(inout) :: object
    call object%pset%final ()
  end subroutine eio_direct_final

  subroutine eio_direct_init_out (eio, sample, data, success, extension)
    class(eio_direct_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(string_t), intent(in), optional :: extension
    type(event_sample_data_t), intent(in), optional :: data
    logical, intent(out), optional :: success
    if (present (success))  success = .true.
  end subroutine eio_direct_init_out

  subroutine eio_direct_init_in (eio, sample, data, success, extension)
    class(eio_direct_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(string_t), intent(in), optional :: extension
    type(event_sample_data_t), intent(inout), optional :: data
    logical, intent(out), optional :: success
    if (present (success))  success = .true.
  end subroutine eio_direct_init_in

  subroutine eio_direct_switch_inout (eio, success)
    class(eio_direct_t), intent(inout) :: eio
    logical, intent(out), optional :: success
    if (present (success))  success = .true.
  end subroutine eio_direct_switch_inout

  subroutine eio_direct_output (eio, event, i_prc, reading, passed, pacify)
    class(eio_direct_t), intent(inout) :: eio
    class(generic_event_t), intent(in), target :: event
    integer, intent(in) :: i_prc
    logical, intent(in), optional :: reading, passed, pacify
    type(particle_set_t), pointer :: pset_ptr
    call eio%pset%final ()
    if (event%has_index ()) then
       call eio%set_event_index (event%get_index ())
    else
       call eio%reset_event_index ()
    end if
    if (present (passed)) then
       eio%passed = passed
       eio%passed_set = .true.
    else
       eio%passed_set = .false.
    end if
    pset_ptr => event%get_particle_set_ptr ()
    if (associated (pset_ptr)) then
       eio%i_prc = i_prc
       eio%pset = pset_ptr
    end if
  end subroutine eio_direct_output

  subroutine eio_direct_input_i_prc (eio, i_prc, iostat)
    class(eio_direct_t), intent(inout) :: eio
    integer, intent(out) :: i_prc
    integer, intent(out) :: iostat
    i_prc = eio%i_prc
    iostat = 0
  end subroutine eio_direct_input_i_prc

  subroutine eio_direct_input_event (eio, event, iostat)
    class(eio_direct_t), intent(inout) :: eio
    class(generic_event_t), intent(inout), target :: event
    integer, intent(out) :: iostat
    call event%select (eio%i_mci, eio%i_term, eio%channel)
    if (eio%has_event_index ()) then
       call event%set_index (eio%get_event_index ())
    else
       call event%reset_index ()
    end if
    call event%set_hard_particle_set (eio%pset)
  end subroutine eio_direct_input_event

  subroutine eio_direct_skip (eio, iostat)
    class(eio_direct_t), intent(inout) :: eio
    integer, intent(out) :: iostat
    iostat = 0
  end subroutine eio_direct_skip

  function eio_direct_has_event_index (eio) result (flag)
    class(eio_direct_t), intent(in) :: eio
    logical :: flag
    flag = eio%i_evt_set
  end function eio_direct_has_event_index

  function eio_direct_get_event_index (eio) result (index)
    class(eio_direct_t), intent(in) :: eio
    integer :: index
    if (eio%has_event_index ()) then
       index = eio%i_evt
    else
       index = 0
    end if
  end function eio_direct_get_event_index

  function eio_direct_passed_known (eio) result (flag)
    class(eio_direct_t), intent(in) :: eio
    logical :: flag
    flag = eio%passed_set
  end function eio_direct_passed_known

  function eio_direct_has_passed (eio) result (flag)
    class(eio_direct_t), intent(in) :: eio
    logical :: flag
    if (eio%passed_known ()) then
       flag = eio%passed
    else
       flag = .true.
    end if
  end function eio_direct_has_passed

  function eio_direct_get_n_in (eio) result (n_in)
    class(eio_direct_t), intent(in) :: eio
    integer :: n_in
    n_in = eio%pset%get_n_in ()
  end function eio_direct_get_n_in

  function eio_direct_get_n_out (eio) result (n_out)
    class(eio_direct_t), intent(in) :: eio
    integer :: n_out
    n_out = eio%pset%get_n_out ()
  end function eio_direct_get_n_out

  function eio_direct_get_n_tot (eio) result (n_tot)
    class(eio_direct_t), intent(in) :: eio
    integer :: n_tot
    n_tot = eio%pset%get_n_tot ()
  end function eio_direct_get_n_tot

  subroutine eio_direct_get_momentum_array (eio, p)
    class(eio_direct_t), intent(in) :: eio
    type(vector4_t), dimension(:), allocatable, intent(out) :: p
    integer :: n
    n = eio%get_n_tot ()
    allocate (p (n))
    p(:) = eio%pset%get_momenta ()
  end subroutine eio_direct_get_momentum_array

  subroutine eio_direct_init_direct &
       (eio, n_beam, n_in, n_rem, n_vir, n_out, pdg, model)
    class(eio_direct_t), intent(out) :: eio
    integer, intent(in) :: n_beam
    integer, intent(in) :: n_in
    integer, intent(in) :: n_rem
    integer, intent(in) :: n_vir
    integer, intent(in) :: n_out
    integer, dimension(:), intent(in) :: pdg
    class(model_data_t), intent(in), target :: model
    call eio%pset%init_direct (n_beam, n_in, n_rem, n_vir, n_out, pdg, model)
  end subroutine eio_direct_init_direct

  subroutine eio_direct_set_event_index (eio, index)
    class(eio_direct_t), intent(inout) :: eio
    integer, intent(in) :: index
    eio%i_evt = index
    eio%i_evt_set = .true.
  end subroutine eio_direct_set_event_index
  
  subroutine eio_direct_reset_event_index (eio)
    class(eio_direct_t), intent(inout) :: eio
    eio%i_evt_set = .false.
  end subroutine eio_direct_reset_event_index
  
  subroutine eio_direct_set_selection_indices &
       (eio, i_prc, i_mci, i_term, channel)
    class(eio_direct_t), intent(inout) :: eio
    integer, intent(in) :: i_prc
    integer, intent(in) :: i_mci
    integer, intent(in) :: i_term
    integer, intent(in) :: channel
    eio%i_prc = i_prc
    eio%i_mci = i_mci
    eio%i_term = i_term
    eio%channel = channel
  end subroutine eio_direct_set_selection_indices

  subroutine eio_direct_set_momentum_single (eio, i, p, p2, on_shell)
    class(eio_direct_t), intent(inout) :: eio
    integer, intent(in) :: i
    type(vector4_t), intent(in) :: p
    real(default), intent(in), optional :: p2
    logical, intent(in), optional :: on_shell
    call eio%pset%set_momentum (i, p, p2, on_shell)
  end subroutine eio_direct_set_momentum_single

  subroutine eio_direct_set_momentum_all (eio, p, p2, on_shell)
    class(eio_direct_t), intent(inout) :: eio
    type(vector4_t), dimension(:), intent(in) :: p
    real(default), dimension(:), intent(in), optional :: p2
    logical, intent(in), optional :: on_shell
    call eio%pset%set_momentum (p, p2, on_shell)
  end subroutine eio_direct_set_momentum_all


end module eio_direct
