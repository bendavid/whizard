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

module eio_checkpoints
  
  use kinds
  use io_units
  use iso_varying_string, string_t => varying_string
  use unit_tests
  use diagnostics

  use cputime
  use model_data
  use processes
  use events
  use eio_data
  use eio_base

  implicit none
  private

  public :: eio_checkpoints_t
  public :: eio_checkpoints_test

  character(*), parameter :: &
     checkpoint_head = "| % complete | events generated | events remaining &
     &| time remaining"
  character(*), parameter :: &
     checkpoint_bar  = "|==================================================&
     &=================|"
  character(*), parameter :: &
     checkpoint_fmt  = "('   ',F5.1,T16,I9,T35,I9,T58,A)"

  type, extends (eio_t) :: eio_checkpoints_t
     logical :: active = .false.
     logical :: running = .false.
     integer :: val = 0
     integer :: n_events = 0
     integer :: n_read = 0
     integer :: i_evt = 0
     logical :: blank = .false.
     type(timer_t) :: timer
   contains
     procedure :: set_parameters => eio_checkpoints_set_parameters
     procedure :: write => eio_checkpoints_write
     procedure :: final => eio_checkpoints_final
     procedure :: init_out => eio_checkpoints_init_out
     procedure :: init_in => eio_checkpoints_init_in
     procedure :: switch_inout => eio_checkpoints_switch_inout
     procedure :: output => eio_checkpoints_output
     procedure :: startup => eio_checkpoints_startup
     procedure :: message => eio_checkpoints_message
     procedure :: shutdown => eio_checkpoints_shutdown
     procedure :: input_i_prc => eio_checkpoints_input_i_prc
     procedure :: input_event => eio_checkpoints_input_event
  end type eio_checkpoints_t
  

contains
  
  subroutine eio_checkpoints_set_parameters (eio, checkpoint, blank)
    class(eio_checkpoints_t), intent(inout) :: eio
    integer, intent(in) :: checkpoint
    logical, intent(in), optional :: blank
    eio%val = checkpoint
    if (present (blank))  eio%blank = blank
  end subroutine eio_checkpoints_set_parameters
  
  subroutine eio_checkpoints_write (object, unit)
    class(eio_checkpoints_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    if (object%active) then
       write (u, "(1x,A)")  "Event-sample checkpoints:  active"
       write (u, "(3x,A,I0)")  "interval  = ", object%val
       write (u, "(3x,A,I0)")  "n_events  = ", object%n_events
       write (u, "(3x,A,I0)")  "n_read    = ", object%n_read
       write (u, "(3x,A,I0)")  "n_current = ", object%i_evt
       write (u, "(3x,A,L1)")  "blanking  = ", object%blank
       call object%timer%write (u)
    else
       write (u, "(1x,A)")  "Event-sample checkpoints:  off"
    end if
  end subroutine eio_checkpoints_write
  
  subroutine eio_checkpoints_final (object)
    class(eio_checkpoints_t), intent(inout) :: object
    object%active = .false.
  end subroutine eio_checkpoints_final
  
  subroutine eio_checkpoints_init_out &
       (eio, sample, process_ptr, data, success, extension)
    class(eio_checkpoints_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(string_t), intent(in), optional :: extension
    type(process_ptr_t), dimension(:), intent(in) :: process_ptr
    type(event_sample_data_t), intent(in), optional :: data
    logical, intent(out), optional :: success
    if (present (data)) then
       if (eio%val > 0) then
          eio%active = .true.
          eio%i_evt = 0
          eio%n_read = 0
          eio%n_events = data%n_evt
       end if
    end if
    if (present (success))  success = .true.
  end subroutine eio_checkpoints_init_out
    
  subroutine eio_checkpoints_init_in &
       (eio, sample, process_ptr, data, success, extension)
    class(eio_checkpoints_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(string_t), intent(in), optional :: extension
    type(process_ptr_t), dimension(:), intent(in) :: process_ptr
    type(event_sample_data_t), intent(inout), optional :: data
    logical, intent(out), optional :: success
    call msg_bug ("Event checkpoints: event input not supported")
    if (present (success))  success = .false.
  end subroutine eio_checkpoints_init_in
    
  subroutine eio_checkpoints_switch_inout (eio, success)
    class(eio_checkpoints_t), intent(inout) :: eio
    logical, intent(out), optional :: success
    call msg_bug ("Event checkpoints: in-out switch not supported")
    if (present (success))  success = .false.
  end subroutine eio_checkpoints_switch_inout
  
  subroutine eio_checkpoints_output (eio, event, i_prc, reading)
    class(eio_checkpoints_t), intent(inout) :: eio
    type(event_t), intent(in), target :: event
    integer, intent(in) :: i_prc
    logical, intent(in), optional :: reading
    logical :: rd
    rd = .false.;  if (present (reading))  rd = reading
    if (eio%active) then
       if (.not. eio%running)  call eio%startup ()
       if (eio%running) then
          eio%i_evt = eio%i_evt + 1
          if (rd) then
             eio%n_read = eio%n_read + 1
          else if (mod (eio%i_evt, eio%val) == 0) then
             call eio%message (eio%blank)
          end if
          if (eio%i_evt == eio%n_events)  call eio%shutdown ()
       end if
    end if
  end subroutine eio_checkpoints_output

  subroutine eio_checkpoints_startup (eio)
    class(eio_checkpoints_t), intent(inout) :: eio
    if (eio%active .and. eio%i_evt < eio%n_events) then
       call msg_message ("")
       call msg_message (checkpoint_bar)
       call msg_message (checkpoint_head)
       call msg_message (checkpoint_bar)
       write (msg_buffer, checkpoint_fmt) 0., 0, eio%n_events - eio%i_evt, "???"
       call msg_message ()
       eio%running = .true.
       call eio%timer%start ()
    end if
  end subroutine eio_checkpoints_startup
  
  subroutine eio_checkpoints_message (eio, testflag)
    class(eio_checkpoints_t), intent(inout) :: eio
    logical, intent(in), optional :: testflag
    real :: t
    type(time_t) :: time_remaining
    type(string_t) :: time_string
    call eio%timer%stop ()
    t = eio%timer
    call eio%timer%restart ()
    time_remaining = &
         nint (t / (eio%i_evt - eio%n_read) * (eio%n_events - eio%i_evt))
    time_string = time_remaining%to_string_ms (blank = testflag)
    write (msg_buffer, checkpoint_fmt) &
         100 * (eio%i_evt - eio%n_read) / real (eio%n_events - eio%n_read), &
         eio%i_evt - eio%n_read, &
         eio%n_events - eio%i_evt, &
         char (time_string)
    call msg_message ()
  end subroutine eio_checkpoints_message

  subroutine eio_checkpoints_shutdown (eio)
    class(eio_checkpoints_t), intent(inout) :: eio
    if (mod (eio%i_evt, eio%val) /= 0) then
       write (msg_buffer, checkpoint_fmt) &
            100., eio%i_evt - eio%n_read, 0, "0m:00s"
       call msg_message ()
    end if
    call msg_message (checkpoint_bar)
    call msg_message ("")
    eio%running = .false.
  end subroutine eio_checkpoints_shutdown

  subroutine eio_checkpoints_input_i_prc (eio, i_prc, iostat)
    class(eio_checkpoints_t), intent(inout) :: eio
    integer, intent(out) :: i_prc
    integer, intent(out) :: iostat
    call msg_bug ("Event checkpoints: event input not supported")
    i_prc = 0
    iostat = 1
  end subroutine eio_checkpoints_input_i_prc

  subroutine eio_checkpoints_input_event (eio, event, iostat)
    class(eio_checkpoints_t), intent(inout) :: eio
    type(event_t), intent(inout), target :: event
    integer, intent(out) :: iostat
    call msg_bug ("Event checkpoints: event input not supported")
    iostat = 1
  end subroutine eio_checkpoints_input_event


  subroutine eio_checkpoints_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (eio_checkpoints_1, "eio_checkpoints_1", &
         "read and write event contents", &
         u, results)
  end subroutine eio_checkpoints_test
  
  subroutine eio_checkpoints_1 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(event_t), allocatable, target :: event
    type(process_t), allocatable, target :: process
    type(process_ptr_t) :: process_ptr
    type(process_instance_t), allocatable, target :: process_instance
    class(eio_t), allocatable :: eio
    type(event_sample_data_t) :: data
    type(string_t) :: sample
    integer :: i, n_events

    write (u, "(A)")  "* Test output: eio_checkpoints_1"
    write (u, "(A)")  "*   Purpose: generate a number of events &
         &with screen output"
    write (u, "(A)")

    call model%init_test ()

    write (u, "(A)")  "* Initialize test process"
 
    allocate (process)
    process_ptr%ptr => process
    allocate (process_instance)
    call prepare_test_process (process, process_instance, model)
    call process_instance%setup_event_data ()
 
    allocate (event)
    call event%basic_init ()
    call event%connect (process_instance, process%get_model_ptr ())
    
    write (u, "(A)")
    write (u, "(A)")  "* Generate events"
    write (u, "(A)")
 
    sample = "eio_checkpoints_1"
 
    allocate (eio_checkpoints_t :: eio)

    n_events = 10
    call data%init (1, 0)
    data%n_evt = n_events

    select type (eio)
    type is (eio_checkpoints_t)
       call eio%set_parameters (checkpoint = 4)
    end select

    call eio%init_out (sample, [process_ptr], data)

    do i = 1, n_events
       call event%generate (1, [0._default, 0._default])
       call eio%output (event, i_prc = 0)
    end do
    
    write (u, "(A)")  "* Checkpointing status"
    write (u, "(A)")

    call eio%write (u)
    call eio%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
 
    call event%final ()
    deallocate (event)
 
    call cleanup_test_process (process, process_instance)
    deallocate (process_instance)
    deallocate (process)

    call model%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: eio_checkpoints_1"
    
  end subroutine eio_checkpoints_1
  

end module eio_checkpoints
