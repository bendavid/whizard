! WHIZARD 2.2.4 Feb 06 2015
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

module eio_raw
  
  use kinds
  use io_units
  use iso_varying_string, string_t => varying_string
  use unit_tests
  use diagnostics

  use lorentz
  use variables
  use model_data
  use particles
  use event_base
  use eio_data
  use eio_base
  use events

  implicit none
  private

  public :: eio_raw_t
  public :: eio_raw_test

  integer, parameter :: CURRENT_FILE_VERSION = 2


  type, extends (eio_t) :: eio_raw_t
     logical :: reading = .false.
     logical :: writing = .false.
     integer :: unit = 0
     integer :: norm_mode = NORM_UNDEFINED
     real(default) :: sigma = 1
     integer :: n = 1
     integer :: n_alt = 0
     logical :: check = .false.
     integer :: file_version = CURRENT_FILE_VERSION
   contains
     procedure :: write => eio_raw_write
     procedure :: final => eio_raw_final
     procedure :: set_parameters => eio_raw_set_parameters
     procedure :: init_out => eio_raw_init_out
     procedure :: init_in => eio_raw_init_in
     procedure :: switch_inout => eio_raw_switch_inout
     procedure :: output => eio_raw_output
     procedure :: input_i_prc => eio_raw_input_i_prc
     procedure :: input_event => eio_raw_input_event
  end type eio_raw_t
  

contains
  
  subroutine eio_raw_write (object, unit)
    class(eio_raw_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "Raw event stream:"
    write (u, "(3x,A,L1)")  "Check MD5 sum     = ", object%check
    if (object%n_alt > 0) then
       write (u, "(3x,A,I0)")  "Alternate weights = ", object%n_alt
    end if
    if (object%reading) then
       write (u, "(3x,A,A)")  "Reading from file = ", char (object%filename)
    else if (object%writing) then
       write (u, "(3x,A,A)")  "Writing to file   = ", char (object%filename)
    else
       write (u, "(3x,A)")  "[closed]"
    end if
  end subroutine eio_raw_write
  
  subroutine eio_raw_final (object)
    class(eio_raw_t), intent(inout) :: object
    if (object%reading .or. object%writing) then
       write (msg_buffer, "(A,A,A)")  "Events: closing raw file '", &
            char (object%filename), "'"
       call msg_message ()
       close (object%unit)
       object%reading = .false.
       object%writing = .false.
    end if
  end subroutine eio_raw_final
  
  subroutine eio_raw_set_parameters (eio, check, version_string, extension)
    class(eio_raw_t), intent(inout) :: eio
    logical, intent(in), optional :: check
    type(string_t), intent(in), optional :: version_string
    type(string_t), intent(in), optional :: extension 
    if (present (check))  eio%check = check
    if (present (version_string)) then
       select case (char (version_string))
       case ("", "2.2.4")
          eio%file_version = CURRENT_FILE_VERSION
       case ("2.2")
          eio%file_version = 1
       case default
          call msg_fatal ("Raw event I/O: unsupported version '" &
               // char (version_string) // "'")
          eio%file_version = 0
       end select
    end if
    if (present (extension)) then
       eio%extension = extension
    else
       eio%extension = "evx"
    end if
  end subroutine eio_raw_set_parameters
    
  subroutine eio_raw_init_out (eio, sample, data, success, extension)
    class(eio_raw_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(event_sample_data_t), intent(in), optional :: data
    logical, intent(out), optional :: success
    type(string_t), intent(in), optional :: extension
    character(32) :: md5sum_prc, md5sum_cfg
    character(32), dimension(:), allocatable :: md5sum_alt
    integer :: i
    if (present (extension)) then
       eio%extension  = extension
    else
       eio%extension = "evx"
    end if
    eio%filename = sample // "." // eio%extension
    eio%unit = free_unit ()
    write (msg_buffer, "(A,A,A)")  "Events: writing to raw file '", &
         char (eio%filename), "'"
    call msg_message ()
    eio%writing = .true.
    if (present (data)) then
       md5sum_prc = data%md5sum_prc
       md5sum_cfg = data%md5sum_cfg
       eio%norm_mode = data%norm_mode
       eio%sigma = data%total_cross_section
       eio%n = data%n_evt
       eio%n_alt = data%n_alt
       if (eio%n_alt > 0) then
          allocate (md5sum_alt (data%n_alt), source = data%md5sum_alt)
       end if
    else
       md5sum_prc = ""
       md5sum_cfg = ""
    end if
    open (eio%unit, file = char (eio%filename), form = "unformatted", &
         action = "write", status = "replace")
    select case (eio%file_version)
    case (2:);  write (eio%unit)  eio%file_version
    end select
    write (eio%unit)  md5sum_prc
    write (eio%unit)  md5sum_cfg
    write (eio%unit)  eio%norm_mode
    write (eio%unit)  eio%n_alt
    do i = 1, eio%n_alt
       write (eio%unit)  md5sum_alt(i)
    end do
    if (present (success))  success = .true.
  end subroutine eio_raw_init_out
    
  subroutine eio_raw_init_in (eio, sample, data, success, extension)
    class(eio_raw_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(event_sample_data_t), intent(inout), optional :: data
    logical, intent(out), optional :: success
    type(string_t), intent(in), optional :: extension
    character(32) :: md5sum_prc, md5sum_cfg
    character(32), dimension(:), allocatable :: md5sum_alt
    integer :: i, file_version
    if (present (success))  success = .true.
    if (present (extension)) then
       eio%extension = extension
    else
       eio%extension = "evx"
    end if
    eio%filename = sample // "." // eio%extension
    eio%unit = free_unit ()
    if (present (data)) then
       eio%sigma = data%total_cross_section
       eio%n = data%n_evt
    end if
    write (msg_buffer, "(A,A,A)")  "Events: reading from raw file '", &
         char (eio%filename), "'"
    call msg_message ()
    eio%reading = .true.
    open (eio%unit, file = char (eio%filename), form = "unformatted", &
         action = "read", status = "old")
    select case (eio%file_version)
    case (2:);  read (eio%unit)  file_version
    case default;  file_version = 1
    end select
    if (file_version /= eio%file_version) then
       call msg_error ("Reading event file: raw-file version mismatch.")
       if (present (success))  success = .false.
       return
    else if (file_version /= CURRENT_FILE_VERSION) then
       call msg_warning ("Reading event file: compatibility mode.")
    end if
    read (eio%unit)  md5sum_prc
    read (eio%unit)  md5sum_cfg
    read (eio%unit)  eio%norm_mode
    read (eio%unit)  eio%n_alt
    if (present (data)) then
       if (eio%n_alt /= data%n_alt) then
          if (present (success))  success = .false. !
          return
       end if
    end if
    allocate (md5sum_alt (eio%n_alt))
    do i = 1, eio%n_alt
       read (eio%unit)  md5sum_alt(i)
    end do
    if (present (success)) then
       if (present (data)) then
          if (eio%check) then
             if (data%md5sum_prc /= "") then
                success = success .and. md5sum_prc == data%md5sum_prc
             end if
             if (data%md5sum_cfg /= "") then
                success = success .and. md5sum_cfg == data%md5sum_cfg
             end if
             do i = 1, eio%n_alt
                if (data%md5sum_alt(i) /= "") then
                   success = success .and. md5sum_alt(i) == data%md5sum_alt(i)
                end if
             end do
          else
             call msg_warning ("Reading event file: MD5 sum check disabled")
          end if
       end if
    end if
  end subroutine eio_raw_init_in
    
  subroutine eio_raw_switch_inout (eio, success)
    class(eio_raw_t), intent(inout) :: eio
    logical, intent(out), optional :: success
    write (msg_buffer, "(A,A,A)")  "Events: appending to raw file '", &
         char (eio%filename), "'"
    call msg_message ()
    close (eio%unit, status = "keep")
    eio%reading = .false.
    open (eio%unit, file = char (eio%filename), form = "unformatted", &
         action = "write", position = "append", status = "old")
    eio%writing = .true.
    if (present (success))  success = .true.
  end subroutine eio_raw_switch_inout
  
  subroutine eio_raw_output (eio, event, i_prc, reading, pacify)
    class(eio_raw_t), intent(inout) :: eio
    class(generic_event_t), intent(in), target :: event
    logical, intent(in), optional :: reading, pacify
    integer, intent(in) :: i_prc
    type(particle_set_t), pointer :: pset
    integer :: i
    if (eio%writing) then
       if (event%has_valid_particle_set ()) then
          select type (event)
          type is (event_t)
             write (eio%unit)  i_prc
             write (eio%unit)  event%get_i_mci ()
             write (eio%unit)  event%get_i_term ()
             write (eio%unit)  event%get_channel ()
             write (eio%unit)  event%expr%weight_prc
             write (eio%unit)  event%expr%excess_prc
             write (eio%unit)  event%expr%sqme_prc
             do i = 1, eio%n_alt
                write (eio%unit)  event%expr%weight_alt(i)
                write (eio%unit)  event%expr%sqme_alt(i)
             end do
             allocate (pset)
             call event%get_hard_particle_set (pset)
             call particle_set_write_raw (pset, eio%unit)
             call particle_set_final (pset)
             deallocate (pset)
             select case (eio%file_version)
             case (2:)
                if (event%has_transform ()) then
                   write (eio%unit)  .true.
                   pset => event%get_particle_set_ptr ()
                   call particle_set_write_raw (pset, eio%unit)
                else
                   write (eio%unit)  .false.
                end if
             end select
          class default
             call msg_bug ("Event: write raw: defined only for full event_t")
          end select
       else
          call msg_bug ("Event: write raw: particle set is undefined")
       end if
    else
       call eio%write ()
       call msg_fatal ("Raw event file is not open for writing")
    end if
  end subroutine eio_raw_output

  subroutine eio_raw_input_i_prc (eio, i_prc, iostat)
    class(eio_raw_t), intent(inout) :: eio
    integer, intent(out) :: i_prc
    integer, intent(out) :: iostat
    if (eio%reading) then
       read (eio%unit, iostat = iostat)  i_prc
    else
       call eio%write ()
       call msg_fatal ("Raw event file is not open for reading")
    end if
  end subroutine eio_raw_input_i_prc

  subroutine eio_raw_input_event (eio, event, iostat)
    class(eio_raw_t), intent(inout) :: eio
    class(generic_event_t), intent(inout), target :: event
    integer, intent(out) :: iostat
    integer :: i_mci, i_term, channel, i
    real(default) :: weight, excess, sqme
    real(default), dimension(:), allocatable :: weight_alt, sqme_alt
    logical :: has_transform
    type(particle_set_t), pointer :: pset
    class(model_data_t), pointer :: model
    if (eio%reading) then
       select type (event)
       type is (event_t)
          read (eio%unit, iostat = iostat)  i_mci
          if (iostat /= 0)  return
          read (eio%unit, iostat = iostat)  i_term
          if (iostat /= 0)  return
          read (eio%unit, iostat = iostat)  channel
          if (iostat /= 0)  return
          read (eio%unit, iostat = iostat)  weight
          if (iostat /= 0)  return
          read (eio%unit, iostat = iostat)  excess
          if (iostat /= 0)  return
          read (eio%unit, iostat = iostat)  sqme
          if (iostat /= 0)  return
          call event%reset ()
          call event%select (i_mci, i_term, channel)
          if (eio%norm_mode /= NORM_UNDEFINED) then
             call event_normalization_update (weight, &
                  eio%sigma, eio%n, event%get_norm_mode (), eio%norm_mode)
             call event_normalization_update (excess, &
                  eio%sigma, eio%n, event%get_norm_mode (), eio%norm_mode)
          end if
          call event%set (sqme_ref = sqme, weight_ref = weight, &
               excess_prc = excess)
          if (eio%n_alt /= 0) then
             allocate (sqme_alt (eio%n_alt), weight_alt (eio%n_alt))
             do i = 1, eio%n_alt
                read (eio%unit, iostat = iostat)  weight_alt(i)
                if (iostat /= 0)  return
                read (eio%unit, iostat = iostat)  sqme_alt(i)
                if (iostat /= 0)  return
             end do
             call event%set (sqme_alt = sqme_alt, weight_alt = weight_alt)
          end if
          model => null ()
          if (associated (event%process)) then
             model => event%process%get_model_ptr ()
          end if
          allocate (pset)
          call particle_set_read_raw (pset, eio%unit, iostat)
          if (iostat /= 0)  return
          if (associated (model))  call particle_set_set_model (pset, model)
          call event%set_hard_particle_set (pset)
          call particle_set_final (pset)
          deallocate (pset)
          select case (eio%file_version)
          case (2:)
             read (eio%unit, iostat = iostat)  has_transform
             if (iostat /= 0)  return
             if (has_transform) then
                allocate (pset)
                call particle_set_read_raw (pset, eio%unit, iostat)
                if (iostat /= 0)  return
                if (associated (model)) &
                     call particle_set_set_model (pset, model)
                call event%link_particle_set (pset)
             end if
          end select
       class default
          call msg_bug ("Event: read raw: defined only for full event_t")
       end select
    else
       call eio%write ()
       call msg_fatal ("Raw event file is not open for reading")
    end if
  end subroutine eio_raw_input_event


  subroutine eio_raw_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (eio_raw_1, "eio_raw_1", &
         "read and write event contents", &
         u, results)
    call test (eio_raw_2, "eio_raw_2", &
         "handle multiple weights", &
         u, results)
  end subroutine eio_raw_test
  
  subroutine eio_raw_1 (u)
    use processes
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(event_t), allocatable, target :: event
    type(process_t), allocatable, target :: process
    type(process_instance_t), allocatable, target :: process_instance
    class(eio_t), allocatable :: eio
    integer :: i_prc, iostat
    type(string_t) :: sample

    write (u, "(A)")  "* Test output: eio_raw_1"
    write (u, "(A)")  "*   Purpose: generate and read/write an event"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize test process"
 
    call model%init_test ()

    allocate (process)
    allocate (process_instance)
    call prepare_test_process (process, process_instance, model)
    call process_instance%setup_event_data ()
 
    allocate (event)
    call event%basic_init ()
    call event%connect (process_instance, process%get_model_ptr ())
    
    write (u, "(A)")
    write (u, "(A)")  "* Generate and write an event"
    write (u, "(A)")
 
    sample = "eio_raw_1"
 
    allocate (eio_raw_t :: eio)
    
    call eio%init_out (sample)
    call event%generate (1, [0._default, 0._default])
    call event%evaluate_expressions ()
    call event%write (u)
    write (u, "(A)")

    call eio%output (event, i_prc = 42)
    call eio%write (u)
    call eio%final ()

    call event%final ()
    deallocate (event)
    call process_instance%final ()
    deallocate (process_instance)
    
    write (u, "(A)")
    write (u, "(A)")  "* Re-read the event"
    write (u, "(A)")
    
    call eio%init_in (sample)

    allocate (process_instance)
    call process_instance%init (process)
    call process_instance%setup_event_data ()
    allocate (event)
    call event%basic_init ()
    call event%connect (process_instance, process%get_model_ptr ())
    
    call eio%input_i_prc (i_prc, iostat)
    if (iostat /= 0)  write (u, "(A,I0)")  "I/O error (i_prc):", iostat
    call eio%input_event (event, iostat)
    if (iostat /= 0)  write (u, "(A,I0)")  "I/O error (event):", iostat
    call eio%write (u)
    
    write (u, "(A)")
    write (u, "(1x,A,I0)")  "i_prc = ", i_prc
    write (u, "(A)")
    call event%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Generate and append another event"
    write (u, "(A)")
    
    call eio%switch_inout ()
    call event%generate (1, [0._default, 0._default])
    call event%evaluate_expressions ()
    call event%write (u)
    write (u, "(A)")

    call eio%output (event, i_prc = 5)
    call eio%write (u)
    call eio%final ()
    
    call event%final ()
    deallocate (event)
    call process_instance%final ()
    deallocate (process_instance)
    
    write (u, "(A)")
    write (u, "(A)")  "* Re-read both events"
    write (u, "(A)")
    
    call eio%init_in (sample)

    allocate (process_instance)
    call process_instance%init (process)
    call process_instance%setup_event_data ()
    allocate (event)
    call event%basic_init ()
    call event%connect (process_instance, process%get_model_ptr ())
    
    call eio%input_i_prc (i_prc, iostat)
    if (iostat /= 0)  write (u, "(A,I0)")  "I/O error (i_prc/1):", iostat
    call eio%input_event (event, iostat)
    if (iostat /= 0)  write (u, "(A,I0)")  "I/O error (event/1):", iostat
    call eio%input_i_prc (i_prc, iostat)
    if (iostat /= 0)  write (u, "(A,I0)")  "I/O error (i_prc/2):", iostat
    call eio%input_event (event, iostat)
    if (iostat /= 0)  write (u, "(A,I0)")  "I/O error (event/2):", iostat
    call eio%write (u)
    
    write (u, "(A)")
    write (u, "(1x,A,I0)")  "i_prc = ", i_prc
    write (u, "(A)")
    call event%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
 
    call eio%final ()
    deallocate (eio)
 
    call event%final ()
    deallocate (event)
 
    call cleanup_test_process (process, process_instance)
    deallocate (process_instance)
    deallocate (process)
    
    call model%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: eio_raw_1"
    
  end subroutine eio_raw_1
  
  subroutine eio_raw_2 (u)
    use processes
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(var_list_t) :: var_list
    type(event_t), allocatable, target :: event
    type(process_t), allocatable, target :: process
    type(process_instance_t), allocatable, target :: process_instance
    type(event_sample_data_t) :: data
    class(eio_t), allocatable :: eio
    integer :: i_prc, iostat
    type(string_t) :: sample

    write (u, "(A)")  "* Test output: eio_raw_2"
    write (u, "(A)")  "*   Purpose: generate and read/write an event"
    write (u, "(A)")  "*            with multiple weights"
    write (u, "(A)")

    call model%init_test ()

    write (u, "(A)")  "* Initialize test process"
 
    allocate (process)
    allocate (process_instance)
    call prepare_test_process (process, process_instance, model)
    call process_instance%setup_event_data ()
 
    call data%init (n_proc = 1, n_alt = 2)

    call var_list_append_log (var_list, var_str ("?unweighted"), .false., &
         intrinsic = .true.)
    call var_list_append_string (var_list, var_str ("$sample_normalization"), &
         var_str ("auto"), intrinsic = .true.)
    call var_list_append_real (var_list, var_str ("safety_factor"), &
         1._default, intrinsic = .true.)

    allocate (event)
    call event%basic_init (var_list, n_alt = 2)
    call event%connect (process_instance, process%get_model_ptr ())
    
    write (u, "(A)")
    write (u, "(A)")  "* Generate and write an event"
    write (u, "(A)")
 
    sample = "eio_raw_2"
 
    allocate (eio_raw_t :: eio)
    
    call eio%init_out (sample, data)
    call event%generate (1, [0._default, 0._default])
    call event%evaluate_expressions ()
    call event%set (sqme_alt = [2._default, 3._default])
    call event%set (weight_alt = &
         [2 * event%get_weight_ref (), 3 * event%get_weight_ref ()])
    call event%store_alt_values ()
    call event%check ()

    call event%write (u)
    write (u, "(A)")

    call eio%output (event, i_prc = 42)
    call eio%write (u)
    call eio%final ()

    call event%final ()
    deallocate (event)
    call process_instance%final ()
    deallocate (process_instance)
    
    write (u, "(A)")
    write (u, "(A)")  "* Re-read the event"
    write (u, "(A)")
    
    call eio%init_in (sample, data)

    allocate (process_instance)
    call process_instance%init (process)
    call process_instance%setup_event_data ()
    allocate (event)
    call event%basic_init (var_list, n_alt = 2)
    call event%connect (process_instance, process%get_model_ptr ())
    
    call eio%input_i_prc (i_prc, iostat)
    if (iostat /= 0)  write (u, "(A,I0)")  "I/O error (i_prc):", iostat
    call eio%input_event (event, iostat)
    if (iostat /= 0)  write (u, "(A,I0)")  "I/O error (event):", iostat
    call eio%write (u)
    
    write (u, "(A)")
    write (u, "(1x,A,I0)")  "i_prc = ", i_prc
    write (u, "(A)")
    call event%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
 
    call eio%final ()
    deallocate (eio)
 
    call event%final ()
    deallocate (event)
 
    call cleanup_test_process (process, process_instance)
    deallocate (process_instance)
    deallocate (process)
    
    call model%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: eio_raw_2"
    
  end subroutine eio_raw_2
  

end module eio_raw
