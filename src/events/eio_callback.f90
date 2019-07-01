! WHIZARD 2.3.0 July 21 2016
! 
! Copyright (C) 1999-2016 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Soyoung Shim <soyoung.shim@desy.de>
!     Florian Staub <florian.staub@cern.ch>  
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, So-young Shim, Daniel Wiesler 
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

module eio_callback
  
  use kinds, only: i64
  use iso_varying_string, string_t => varying_string
  use io_units
  use diagnostics
  use cputime
  use event_base
  use eio_data
  use eio_base

  implicit none
  private

  public :: eio_callback_t

  type, extends (eio_t) :: eio_callback_t
     class(event_callback_t), allocatable :: callback
     integer(i64) :: i_evt = 0
     integer :: i_interval = 0
     integer :: n_interval = 0
!      type(timer_t) :: timer
   contains
     procedure :: set_parameters => eio_callback_set_parameters
     procedure :: write => eio_callback_write
     procedure :: final => eio_callback_final
     procedure :: init_out => eio_callback_init_out
     procedure :: init_in => eio_callback_init_in
     procedure :: switch_inout => eio_callback_switch_inout
     procedure :: output => eio_callback_output
     procedure :: input_i_prc => eio_callback_input_i_prc
     procedure :: input_event => eio_callback_input_event
     procedure :: skip => eio_callback_skip
  end type eio_callback_t
  

contains
  
  subroutine eio_callback_set_parameters (eio, callback, count_interval)
    class(eio_callback_t), intent(inout) :: eio
    class(event_callback_t), intent(in) :: callback
    integer, intent(in) :: count_interval
    allocate (eio%callback, source = callback)
    eio%n_interval = count_interval
  end subroutine eio_callback_set_parameters
  
  subroutine eio_callback_write (object, unit)
    class(eio_callback_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "Event-sample callback:"
    write (u, "(3x,A,I0)")  "interval  = ", object%n_interval
    write (u, "(3x,A,I0)")  "evt count = ", object%i_evt
!        call object%timer%write (u)
  end subroutine eio_callback_write
  
  subroutine eio_callback_final (object)
    class(eio_callback_t), intent(inout) :: object
  end subroutine eio_callback_final
  
  subroutine eio_callback_init_out (eio, sample, data, success, extension)
    class(eio_callback_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(string_t), intent(in), optional :: extension
    type(event_sample_data_t), intent(in), optional :: data
    logical, intent(out), optional :: success
    eio%i_evt = 0
    eiO%i_interval = 0
    if (present (success))  success = .true.
  end subroutine eio_callback_init_out
    
  subroutine eio_callback_init_in (eio, sample, data, success, extension)
    class(eio_callback_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(string_t), intent(in), optional :: extension
    type(event_sample_data_t), intent(inout), optional :: data
    logical, intent(out), optional :: success
    call msg_bug ("Event callback: event input not supported")
    if (present (success))  success = .false.
  end subroutine eio_callback_init_in
    
  subroutine eio_callback_switch_inout (eio, success)
    class(eio_callback_t), intent(inout) :: eio
    logical, intent(out), optional :: success
    call msg_bug ("Event callback: in-out switch not supported")
    if (present (success))  success = .false.
  end subroutine eio_callback_switch_inout
  
  subroutine eio_callback_output (eio, event, i_prc, reading, passed, pacify)
    class(eio_callback_t), intent(inout) :: eio
    class(generic_event_t), intent(in), target :: event
    integer, intent(in) :: i_prc
    logical, intent(in), optional :: reading, passed, pacify
    logical :: rd
    eio%i_evt = eio%i_evt + 1
    if (eio%n_interval > 0) then
       eio%i_interval = eio%i_interval + 1
       if (eio%i_interval >= eio%n_interval) then
          call eio%callback%proc (eio%i_evt, event)
          eio%i_interval = 0
       end if
    end if
  end subroutine eio_callback_output

  subroutine eio_callback_input_i_prc (eio, i_prc, iostat)
    class(eio_callback_t), intent(inout) :: eio
    integer, intent(out) :: i_prc
    integer, intent(out) :: iostat
    call msg_bug ("Event callback: event input not supported")
    i_prc = 0
    iostat = 1
  end subroutine eio_callback_input_i_prc

  subroutine eio_callback_input_event (eio, event, iostat)
    class(eio_callback_t), intent(inout) :: eio
    class(generic_event_t), intent(inout), target :: event
    integer, intent(out) :: iostat
    call msg_bug ("Event callback: event input not supported")
    iostat = 1
  end subroutine eio_callback_input_event

  subroutine eio_callback_skip (eio, iostat)
    class(eio_callback_t), intent(inout) :: eio
    integer, intent(out) :: iostat
    iostat = 0
  end subroutine eio_callback_skip


end module eio_callback
