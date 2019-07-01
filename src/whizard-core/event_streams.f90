! WHIZARD 2.2.0 May 18 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Christian Speckner <cnspeckn@googlemail.com> 
!     and  Fabian Bach, Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

module event_streams
  
  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use unit_tests
  use variables
  use models
  use processes
  use events
  use eio_data
  use eio_base
  use rt_data
  use dispatch
  
  implicit none
  private

  public :: event_stream_array_t
  public :: event_streams_test

  type :: event_stream_entry_t
     class(eio_t), allocatable :: eio
  end type event_stream_entry_t
  
  type :: event_stream_array_t
     type(event_stream_entry_t), dimension(:), allocatable :: entry
     integer :: i_in = 0
   contains
     procedure :: write => event_stream_array_write
     procedure :: final => event_stream_array_final
     procedure :: init => event_stream_array_init
     procedure :: switch_inout => event_stream_array_switch_inout
     procedure :: output => event_stream_array_output
     procedure :: input_i_prc => event_stream_array_input_i_prc
     procedure :: input_event => event_stream_array_input_event
     procedure :: has_input => event_stream_array_has_input
  end type event_stream_array_t
  

contains

  subroutine event_streams_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (event_streams_1, "event_streams_1", &
         "empty event stream array", &
         u, results)
    call test (event_streams_2, "event_streams_2", &
         "nontrivial event stream array", &
         u, results)
    call test (event_streams_3, "event_streams_3", &
         "switch input/output", &
         u, results)
    call test (event_streams_4, "event_streams_4", &
         "check MD5 sum", &
         u, results)
  end subroutine event_streams_test
  
  subroutine event_streams_1 (u)
    integer, intent(in) :: u
    type(event_stream_array_t) :: es_array
    type(rt_data_t) :: global
    type(event_t) :: event
    type(string_t) :: sample
    type(string_t), dimension(0) :: empty_string_array
    type(process_ptr_t), dimension(0) :: empty_process_ptr_array

    write (u, "(A)")  "* Test output: event_streams_1"
    write (u, "(A)")  "*   Purpose: handle empty event stream array"
    write (u, "(A)")

    sample = "event_streams_1"

    call es_array%init &
         (sample, empty_string_array, empty_process_ptr_array, global)
    call es_array%output (event, 42, 1)
    call es_array%write (u)
    call es_array%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: event_streams_1"
    
  end subroutine event_streams_1
  
  subroutine event_streams_2 (u)
    integer, intent(in) :: u
    type(event_stream_array_t) :: es_array
    type(rt_data_t) :: global
    type(event_t), allocatable, target :: event
    type(process_t), allocatable, target :: process
    type(process_instance_t), allocatable, target :: process_instance
    type(process_ptr_t) :: process_ptr
    type(string_t) :: sample
    type(string_t), dimension(0) :: empty_string_array
    integer :: i_prc, iostat

    write (u, "(A)")  "* Test output: event_streams_2"
    write (u, "(A)")  "*   Purpose: handle empty event stream array"
    write (u, "(A)")

    call syntax_model_file_init ()
    call global%global_init ()

    write (u, "(A)")  "* Generate test process event"
    write (u, "(A)")

    allocate (process)
    process_ptr%ptr => process
    allocate (process_instance)
    call prepare_test_process (process, process_instance, global%model_list)
    call process_instance%setup_event_data ()

    allocate (event)
    call event%basic_init ()
    call event%connect (process_instance, process%get_model_ptr ())
    call event%generate (1, [0.4_default, 0.4_default])
    call event%evaluate_expressions ()
    call event%write (u)

    write (u, "(A)")
    write (u, "(A)") "* Allocate raw eio stream and write event to file"
    write (u, "(A)")

    sample = "event_streams_2"

    call es_array%init (sample, [var_str ("raw")], [process_ptr], global)
    call es_array%output (event, 1, 1)
    call es_array%write (u)
    call es_array%final ()
    
    write (u, "(A)")
    write (u, "(A)") "* Reallocate raw eio stream for reading"
    write (u, "(A)")

    sample = "foo"
    call es_array%init (sample, empty_string_array, [process_ptr], global, &
         input = var_str ("raw"), input_sample = var_str ("event_streams_2"))
    call es_array%write (u)

    write (u, "(A)")
    write (u, "(A)") "* Reread event"
    write (u, "(A)")
    
    call es_array%input_i_prc (i_prc, iostat)
    
    write (u, "(1x,A,I0)")  "i_prc = ", i_prc
    write (u, "(A)")
    call es_array%input_event (event, iostat)
    call es_array%final ()
    
    call event%write (u)
    
    call global%final ()
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: event_streams_2"
    
  end subroutine event_streams_2
  
  subroutine event_streams_3 (u)
    integer, intent(in) :: u
    type(event_stream_array_t) :: es_array
    type(rt_data_t) :: global
    type(event_t), allocatable, target :: event
    type(process_t), allocatable, target :: process
    type(process_instance_t), allocatable, target :: process_instance
    type(process_ptr_t) :: process_ptr
    type(string_t) :: sample
    type(string_t), dimension(0) :: empty_string_array
    integer :: i_prc, iostat

    write (u, "(A)")  "* Test output: event_streams_3"
    write (u, "(A)")  "*   Purpose: handle in/out switching"
    write (u, "(A)")

    call syntax_model_file_init ()
    call global%global_init ()

    write (u, "(A)")  "* Generate test process event"
    write (u, "(A)")

    allocate (process)
    process_ptr%ptr => process
    allocate (process_instance)
    call prepare_test_process (process, process_instance, global%model_list)
    call process_instance%setup_event_data ()

    allocate (event)
    call event%basic_init ()
    call event%connect (process_instance, process%get_model_ptr ())
    call event%generate (1, [0.4_default, 0.4_default])
    call event%evaluate_expressions ()

    write (u, "(A)") "* Allocate raw eio stream and write event to file"
    write (u, "(A)")

    sample = "event_streams_3"

    call es_array%init (sample, [var_str ("raw")], [process_ptr], global)
    call es_array%output (event, 1, 1)
    call es_array%write (u)
    call es_array%final ()
    
    write (u, "(A)")
    write (u, "(A)") "* Reallocate raw eio stream for reading"
    write (u, "(A)")

    call es_array%init (sample, empty_string_array, [process_ptr], global, &
         input = var_str ("raw"))
    call es_array%write (u)

    write (u, "(A)")
    write (u, "(A)") "* Reread event"
    write (u, "(A)")
    
    call es_array%input_i_prc (i_prc, iostat)
    call es_array%input_event (event, iostat)

    write (u, "(A)") "* Attempt to read another event (fail), then generate"
    write (u, "(A)")
    
    call es_array%input_i_prc (i_prc, iostat)
    if (iostat < 0) then
       call es_array%switch_inout ()
       call event%generate (1, [0.3_default, 0.3_default])
       call event%evaluate_expressions ()
       call es_array%output (event, 1, 2)
    end if
    call es_array%write (u)
    call es_array%final ()
    
    write (u, "(A)")
    call event%write (u)

    write (u, "(A)")
    write (u, "(A)") "* Reallocate raw eio stream for reading"
    write (u, "(A)")

    call es_array%init (sample, empty_string_array, [process_ptr], global, &
         input = var_str ("raw"))
    call es_array%write (u)

    write (u, "(A)")
    write (u, "(A)") "* Reread two events and display 2nd event"
    write (u, "(A)")
    
    call es_array%input_i_prc (i_prc, iostat)
    call es_array%input_event (event, iostat)
    call es_array%input_i_prc (i_prc, iostat)
    
    call es_array%input_event (event, iostat)
    call es_array%final ()

    call event%write (u)
    
    call global%final ()
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: event_streams_3"
    
  end subroutine event_streams_3
  
  subroutine event_streams_4 (u)
    integer, intent(in) :: u
    type(event_stream_array_t) :: es_array
    type(rt_data_t) :: global
    type(process_t), allocatable, target :: process
    type(process_ptr_t) :: process_ptr
    type(string_t) :: sample
    type(string_t), dimension(0) :: empty_string_array
    type(event_sample_data_t) :: data

    write (u, "(A)")  "* Test output: event_streams_4"
    write (u, "(A)")  "*   Purpose: handle in/out switching"
    write (u, "(A)")

    write (u, "(A)")  "* Generate test process event"
    write (u, "(A)")

    call global%global_init ()
    call var_list_set_log (global%var_list, var_str ("?check_event_file"), &
         .true., is_known = .true.)

    allocate (process)
    process_ptr%ptr => process

    write (u, "(A)") "* Allocate raw eio stream for writing"
    write (u, "(A)")

    sample = "event_streams_4"
    data%md5sum_cfg = "1234567890abcdef1234567890abcdef"

    call es_array%init &
         (sample, [var_str ("raw")], [process_ptr], global, data)
    call es_array%write (u)
    call es_array%final ()
    
    write (u, "(A)")
    write (u, "(A)") "* Reallocate raw eio stream for reading"
    write (u, "(A)")

    call es_array%init (sample, empty_string_array, [process_ptr], global, &
         data, input = var_str ("raw"))
    call es_array%write (u)
    call es_array%final ()

    write (u, "(A)")
    write (u, "(A)") "* Reallocate modified raw eio stream for reading (fail)"
    write (u, "(A)")

    data%md5sum_cfg = "1234567890______1234567890______"
    call es_array%init (sample, empty_string_array, [process_ptr], global, &
         data, input = var_str ("raw"))
    call es_array%write (u)
    call es_array%final ()
    
    write (u, "(A)")
    write (u, "(A)") "* Repeat ignoring checksum"
    write (u, "(A)")

    call var_list_set_log (global%var_list, var_str ("?check_event_file"), &
         .false., is_known = .true.)
    call es_array%init (sample, empty_string_array, [process_ptr], global, &
         data, input = var_str ("raw"))
    call es_array%write (u)
    call es_array%final ()
    
    call global%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: event_streams_4"
    
  end subroutine event_streams_4
  

  subroutine event_stream_array_write (object, unit)
    class(event_stream_array_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, i
    u = output_unit (unit)
    write (u, "(1x,A)")  "Event stream array:"
    if (allocated (object%entry)) then
       select case (size (object%entry))
       case (0)
          write (u, "(3x,A)")  "[empty]"
       case default
          do i = 1, size (object%entry)
             if (i == object%i_in)  write (u, "(1x,A)")  "Input stream:"
             call object%entry(i)%eio%write (u)
          end do
       end select
    else
       write (u, "(3x,A)")  "[undefined]"
    end if
  end subroutine event_stream_array_write

  subroutine event_stream_array_final (es_array)
    class(event_stream_array_t), intent(inout) :: es_array
    integer :: i
    do i = 1, size (es_array%entry)
       call es_array%entry(i)%eio%final ()
    end do
  end subroutine event_stream_array_final

  subroutine event_stream_array_init &
       (es_array, sample, stream_fmt, process_ptr, global, &
       data, input, input_sample, input_data, allow_switch, checkpoint, &
       error)
    class(event_stream_array_t), intent(out) :: es_array
    type(string_t), intent(in) :: sample
    type(string_t), dimension(:), intent(in) :: stream_fmt
    type(process_ptr_t), dimension(:), intent(in) :: process_ptr
    type(rt_data_t), intent(in) :: global
    type(event_sample_data_t), intent(inout), optional :: data
    type(string_t), intent(in), optional :: input
    type(string_t), intent(in), optional :: input_sample
    type(event_sample_data_t), intent(inout), optional :: input_data
    logical, intent(in), optional :: allow_switch
    integer, intent(in), optional :: checkpoint
    logical, intent(out), optional :: error
    type(string_t) :: sample_in
    integer :: n, i
    logical :: success, switch
    if (present (input)) then
       n = size (stream_fmt) + 1
    else
       n = size (stream_fmt)
    end if
    if (present (input_sample)) then
       sample_in = input_sample
    else
       sample_in = sample
    end if
    if (present (allow_switch)) then
       switch = allow_switch
    else
       switch = .true.
    end if
    if (present (error)) then
       error = .false.
    end if
    if (present (checkpoint)) then
       allocate (es_array%entry (n + 1))
       call dispatch_eio &
            (es_array%entry(n+1)%eio, var_str ("checkpoint"), global)
       call es_array%entry(n+1)%eio%init_out (sample, process_ptr, data)
    else
       allocate (es_array%entry (n))
    end if
    if (present (input)) then
       call dispatch_eio (es_array%entry(n)%eio, input, global)
       if (present (input_data)) then
          call es_array%entry(n)%eio%init_in &
               (sample_in, process_ptr, input_data, success)
       else
          call es_array%entry(n)%eio%init_in &
               (sample_in, process_ptr, data, success)
       end if
       if (success) then
          es_array%i_in = n
       else if (present (input_sample)) then
          if (present (error)) then
             error = .true.
          else
             call msg_fatal ("Events: &
                  &parameter mismatch in input, aborting")
          end if
       else
          call msg_message ("Events: &
               &parameter mismatch, discarding old event set")
          call es_array%entry(n)%eio%final ()
          if (switch) then
             call msg_message ("Events: generating new events")
             call es_array%entry(n)%eio%init_out &
                  (sample, process_ptr, data)
          end if
       end if
    end if
    do i = 1, size (stream_fmt)
       call dispatch_eio (es_array%entry(i)%eio, stream_fmt(i), global)
       call es_array%entry(i)%eio%init_out (sample, process_ptr, data)
    end do
  end subroutine event_stream_array_init
  
  subroutine event_stream_array_switch_inout (es_array)
    class(event_stream_array_t), intent(inout) :: es_array
    integer :: n
    if (es_array%has_input ()) then
       n = es_array%i_in
       call es_array%entry(n)%eio%switch_inout ()
       es_array%i_in = 0
    else
       call msg_bug ("Reading events: switch_inout: no input stream selected")
    end if
  end subroutine event_stream_array_switch_inout
  
  subroutine event_stream_array_output (es_array, event, i_prc, event_index)
    class(event_stream_array_t), intent(inout) :: es_array
    type(event_t), intent(in), target :: event
    integer, intent(in) :: i_prc, event_index
    integer :: i
    do i = 1, size (es_array%entry)
       if (i /= es_array%i_in) then
          associate (eio => es_array%entry(i)%eio)
            if (eio%split) then
               if (event_index > 1 .and. &
                    mod (event_index, eio%split_n_evt) == 1) then
                  call eio%split_out ()
               end if
            end if
            call eio%output (event, i_prc, reading = es_array%i_in /= 0)
          end associate
       end if
    end do
  end subroutine event_stream_array_output
  
  subroutine event_stream_array_input_i_prc (es_array, i_prc, iostat)
    class(event_stream_array_t), intent(inout) :: es_array
    integer, intent(out) :: i_prc
    integer, intent(out) :: iostat
    integer :: n
    if (es_array%has_input ()) then
       n = es_array%i_in
       call es_array%entry(n)%eio%input_i_prc (i_prc, iostat)
    else
       call msg_fatal ("Reading events: no input stream selected")
    end if
  end subroutine event_stream_array_input_i_prc
  
  subroutine event_stream_array_input_event (es_array, event, iostat)
    class(event_stream_array_t), intent(inout) :: es_array
    type(event_t), intent(inout), target :: event
    integer, intent(out) :: iostat
    integer :: n
    if (es_array%has_input ()) then
       n = es_array%i_in
       call es_array%entry(n)%eio%input_event (event, iostat)
    else
       call msg_fatal ("Reading events: no input stream selected")
    end if
  end subroutine event_stream_array_input_event
  
  function event_stream_array_has_input (es_array) result (flag)
    class(event_stream_array_t), intent(in) :: es_array
    logical :: flag
    flag = es_array%i_in /= 0
  end function event_stream_array_has_input
  

end module event_streams
