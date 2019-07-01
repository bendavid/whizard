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

module event_streams
  
  use iso_varying_string, string_t => varying_string
  use io_units
  use diagnostics
  use events
  use eio_data
  use eio_base
  use rt_data

  use dispatch, only: dispatch_eio
  
  implicit none
  private

  public :: event_stream_array_t

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
     procedure :: skip_eio_entry => event_stream_array_skip_eio_entry
     procedure :: has_input => event_stream_array_has_input
  end type event_stream_array_t
  

contains

  subroutine event_stream_array_write (object, unit)
    class(event_stream_array_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit)
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
       (es_array, sample, stream_fmt, global, &
       data, input, input_sample, input_data, allow_switch, &
       checkpoint, callback, &
       error)
    class(event_stream_array_t), intent(out) :: es_array
    type(string_t), intent(in) :: sample
    type(string_t), dimension(:), intent(in) :: stream_fmt
    type(rt_data_t), intent(in) :: global
    type(event_sample_data_t), intent(inout), optional :: data
    type(string_t), intent(in), optional :: input
    type(string_t), intent(in), optional :: input_sample
    type(event_sample_data_t), intent(inout), optional :: input_data
    logical, intent(in), optional :: allow_switch
    integer, intent(in), optional :: checkpoint
    integer, intent(in), optional :: callback
    logical, intent(out), optional :: error
    type(string_t) :: sample_in
    integer :: n, i, n_output, i_input, i_checkpoint, i_callback
    logical :: success, switch
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
    n = size (stream_fmt)
    n_output = n
    if (present (input)) then
       n = n + 1
       i_input = n
    else
       i_input = 0
    end if
    if (present (checkpoint)) then
       n = n + 1
       i_checkpoint = n
    else
       i_checkpoint = 0
    end if
    if (present (callback)) then
       n = n + 1
       i_callback = n
    else
       i_callback = 0
    end if
    allocate (es_array%entry (n))
    if (i_checkpoint > 0) then
       call dispatch_eio &
            (es_array%entry(i_checkpoint)%eio, var_str ("checkpoint"), global)
       call es_array%entry(i_checkpoint)%eio%init_out (sample, data)
    end if
    if (i_callback > 0) then
       call dispatch_eio &
            (es_array%entry(i_callback)%eio, var_str ("callback"), global)
       call es_array%entry(i_callback)%eio%init_out (sample, data)
    end if
    if (i_input > 0) then
       call dispatch_eio (es_array%entry(i_input)%eio, input, global)
       if (present (input_data)) then
          call es_array%entry(i_input)%eio%init_in &
               (sample_in, input_data, success)
       else
          call es_array%entry(i_input)%eio%init_in &
               (sample_in, data, success)
       end if
       if (success) then
          es_array%i_in = i_input
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
          call es_array%entry(i_input)%eio%final ()
          if (switch) then
             call msg_message ("Events: generating new events")
             call es_array%entry(i_input)%eio%init_out (sample, data)
          end if
       end if
    end if
    do i = 1, n_output
       call dispatch_eio (es_array%entry(i)%eio, stream_fmt(i), global)
       call es_array%entry(i)%eio%init_out (sample, data)
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
  
  subroutine event_stream_array_output (es_array, event, i_prc, &
                                        event_index, passed, pacify)
    class(event_stream_array_t), intent(inout) :: es_array
    type(event_t), intent(in), target :: event
    integer, intent(in) :: i_prc, event_index
    logical, intent(in), optional :: passed, pacify
    logical :: increased
    integer :: i
    do i = 1, size (es_array%entry)
       if (i /= es_array%i_in) then
          associate (eio => es_array%entry(i)%eio)
            if (eio%split) then
               if (eio%split_n_evt > 0 &
                    .and. event_index > 1 &
                    .and. mod (event_index, eio%split_n_evt) == 1) then
                  call eio%split_out ()
               else if (eio%split_n_kbytes > 0) then
                  call eio%update_split_count (increased)
                  if (increased)  call eio%split_out ()
               end if
            end if
            call eio%output (event, i_prc, reading = es_array%i_in /= 0, &
                 passed = passed, &
                 pacify = pacify)
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
  
  subroutine event_stream_array_skip_eio_entry (es_array, iostat)
    class(event_stream_array_t), intent(inout) :: es_array
    integer, intent(out) :: iostat
    integer :: n
    if (es_array%has_input ()) then
       n = es_array%i_in
       call es_array%entry(n)%eio%skip (iostat)
    else
       call msg_fatal ("Reading events: no input stream selected")
    end if
  end subroutine event_stream_array_skip_eio_entry

  function event_stream_array_has_input (es_array) result (flag)
    class(event_stream_array_t), intent(in) :: es_array
    logical :: flag
    flag = es_array%i_in /= 0
  end function event_stream_array_has_input
  

end module event_streams
