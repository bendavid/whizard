! WHIZARD 2.2.1 June 3 2014
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

module eio_stdhep
  
  use kinds !NODEP!
  use file_utils !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use diagnostics !NODEP!
  use unit_tests

  use lorentz !NODEP!
  use models
  use particles
  use beams
  use processes
  use events
  use eio_data
  use eio_base
  use hep_common
  use hep_events

  implicit none
  private

  public :: eio_stdhep_t
  public :: eio_stdhep_hepevt_t
  public :: eio_stdhep_hepeup_t
  public :: stdhep_init
  public :: stdhep_write 
  public :: stdhep_end  
  public :: eio_stdhep_test

  type, abstract, extends (eio_t) :: eio_stdhep_t
     logical :: writing = .false.
     integer :: unit = 0
     logical :: keep_beams = .false.     
     integer(i64) :: n_events_expected = 0
   contains
     procedure :: set_parameters => eio_stdhep_set_parameters
     procedure :: write => eio_stdhep_write
     procedure :: final => eio_stdhep_final
     procedure :: split_out => eio_stdhep_split_out
     procedure :: init_out => eio_stdhep_init_out
     procedure :: init_in => eio_stdhep_init_in
     procedure :: switch_inout => eio_stdhep_switch_inout
     procedure :: output => eio_stdhep_output
     procedure :: input_i_prc => eio_stdhep_input_i_prc
     procedure :: input_event => eio_stdhep_input_event
  end type eio_stdhep_t
  
  type, extends (eio_stdhep_t) :: eio_stdhep_hepevt_t
  end type eio_stdhep_hepevt_t
  
  type, extends (eio_stdhep_t) :: eio_stdhep_hepeup_t
  end type eio_stdhep_hepeup_t
  

  integer, save :: istr, lok
  integer, parameter :: &
       STDHEP_HEPEVT = 1, STDHEP_HEPEUP = 11, STDHEP_HEPRUP = 12

contains
  
  subroutine eio_stdhep_set_parameters (eio, keep_beams, extension)
    class(eio_stdhep_t), intent(inout) :: eio
    logical, intent(in), optional :: keep_beams
    type(string_t), intent(in), optional :: extension
    if (present (keep_beams))  eio%keep_beams = keep_beams
    if (present (extension)) then
       eio%extension = extension
    else
       select type (eio)
       type is (eio_stdhep_hepevt_t)
          eio%extension = "stdhep"
       type is (eio_stdhep_hepeup_t)
          eio%extension = "up.stdhep"
       end select
    end if
  end subroutine eio_stdhep_set_parameters
  
  subroutine eio_stdhep_write (object, unit)
    class(eio_stdhep_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A)")  "STDHEP event stream:"
    if (object%writing) then
       write (u, "(3x,A,A)")  "Writing to file   = ", char (object%filename)
    else
       write (u, "(3x,A)")  "[closed]"
    end if
    write (u, "(3x,A,L1)")    "Keep beams        = ", object%keep_beams
  end subroutine eio_stdhep_write
  
  subroutine eio_stdhep_final (object)
    class(eio_stdhep_t), intent(inout) :: object
    if (object%writing) then
       write (msg_buffer, "(A,A,A)")  "Events: closing STDHEP file '", &
            char (object%filename), "'"
       call msg_message ()
       call stdhep_end
       object%writing = .false.
    end if
  end subroutine eio_stdhep_final
  
  subroutine eio_stdhep_split_out (eio)
    class(eio_stdhep_t), intent(inout) :: eio
    if (eio%split) then
       eio%split_index = eio%split_index + 1
       call eio%set_filename ()
       write (msg_buffer, "(A,A,A)")  "Events: writing to STDHEP file '", &
            char (eio%filename), "'"
       call msg_message ()
       call stdhep_end
       select type (eio)
       type is (eio_stdhep_hepeup_t)
          call stdhep_init (char (eio%filename), &
               "WHIZARD event sample", eio%n_events_expected)
          call stdhep_write (STDHEP_HEPRUP)
       type is (eio_stdhep_hepevt_t)
          call stdhep_init (char (eio%filename), &
               "WHIZARD event sample", eio%n_events_expected) 
       end select
    end if
  end subroutine eio_stdhep_split_out
  
  subroutine eio_stdhep_init_out (eio, sample, process_ptr, data, success)
    class(eio_stdhep_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(process_ptr_t), dimension(:), intent(in) :: process_ptr
    type(event_sample_data_t), intent(in), optional :: data
    logical, intent(out), optional :: success
    integer :: i
    if (.not. present (data)) &
         call msg_bug ("STDHEP initialization: missing data")
    if (data%n_beam /= 2) &
         call msg_fatal ("STDHEP: defined for scattering processes only")
    call eio%set_splitting (data)
    eio%sample = sample
    select type (eio)
    type is (eio_stdhep_hepevt_t)
       eio%extension = "stdhep"
    type is (eio_stdhep_hepeup_t)
       eio%extension = "up.stdhep"
    end select
    call eio%set_filename ()
    eio%n_events_expected = data%n_evt
    eio%unit = free_unit ()
    write (msg_buffer, "(A,A,A)")  "Events: writing to STDHEP file '", &
         char (eio%filename), "'"
    call msg_message ()
    eio%writing = .true.
    select type (eio)
    type is (eio_stdhep_hepeup_t)
       call heprup_init &
            (data%pdg_beam, &
            data%energy_beam, &
            n_processes = data%n_proc, &
            unweighted = data%unweighted, &
            negative_weights = data%negative_weights)           
       do i = 1, data%n_proc
          call heprup_set_process_parameters (i = i, &
               process_id = data%proc_num_id(i), &
               cross_section = data%cross_section(i), &
               error = data%error(i))          
       end do
       call stdhep_init (char (eio%filename), &
            "WHIZARD event sample", eio%n_events_expected)
       call stdhep_write (STDHEP_HEPRUP)
    type is (eio_stdhep_hepevt_t)
       call stdhep_init (char (eio%filename), &
            "WHIZARD event sample", eio%n_events_expected) 
    end select
    if (present (success))  success = .true.
  end subroutine eio_stdhep_init_out
    
  subroutine eio_stdhep_init_in &
       (eio, sample, process_ptr, data, success, extension)
    class(eio_stdhep_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(string_t), intent(in), optional :: extension
    type(process_ptr_t), dimension(:), intent(in) :: process_ptr
    type(event_sample_data_t), intent(inout), optional :: data
    logical, intent(out), optional :: success
    call msg_bug ("STDHEP: event input not supported")
    if (present (success))  success = .false.
  end subroutine eio_stdhep_init_in
    
  subroutine eio_stdhep_switch_inout (eio, success)
    class(eio_stdhep_t), intent(inout) :: eio
    logical, intent(out), optional :: success
    call msg_bug ("STDHEP: in-out switch not supported")
    if (present (success))  success = .false.
  end subroutine eio_stdhep_switch_inout
  
  subroutine eio_stdhep_output (eio, event, i_prc, reading)
    class(eio_stdhep_t), intent(inout) :: eio
    type(event_t), intent(in), target :: event
    integer, intent(in) :: i_prc
    logical, intent(in), optional :: reading
    if (eio%writing) then
       select type (eio)
       type is (eio_stdhep_hepeup_t)
          call hepeup_from_event (event, &
               process_index = i_prc, &
               keep_beams = eio%keep_beams)
          call stdhep_write (STDHEP_HEPEUP)
       type is (eio_stdhep_hepevt_t)
          call hepevt_from_event (event, &
               i_evt = event%expr%index, &                         
               keep_beams = eio%keep_beams)
          call stdhep_write (STDHEP_HEPEVT)                              
       end select       
    else
       call eio%write ()
       call msg_fatal ("STDHEP file is not open for writing")
    end if
  end subroutine eio_stdhep_output

  subroutine eio_stdhep_input_i_prc (eio, i_prc, iostat)
    class(eio_stdhep_t), intent(inout) :: eio
    integer, intent(out) :: i_prc
    integer, intent(out) :: iostat
    call msg_bug ("STDHEP: event input not supported")
    i_prc = 0
    iostat = 1
  end subroutine eio_stdhep_input_i_prc

  subroutine eio_stdhep_input_event (eio, event, iostat)
    class(eio_stdhep_t), intent(inout) :: eio
    type(event_t), intent(inout), target :: event
    integer, intent(out) :: iostat
    call msg_bug ("STDHEP: event input not supported")
    iostat = 1
  end subroutine eio_stdhep_input_event

  subroutine stdhep_init (file, title, nevt)
    character(len=*), intent(in) :: file, title
    integer(i64), intent(in) :: nevt
    integer(i32) :: nevt32
    external stdxwinit, stdxwrt
    nevt32 = min (nevt, int (huge (1_i32), i64))
    call stdxwinit (file, title, nevt32, istr, lok)
    call stdxwrt (100, istr, lok)
  end subroutine stdhep_init

  subroutine stdhep_write (ilbl)
    integer, intent(in) :: ilbl
    external stdxwrt
    call stdxwrt (ilbl, istr, lok)
  end subroutine stdhep_write

  subroutine stdhep_end
    external stdxend
    call stdxend (istr)
  end subroutine stdhep_end  
  

  subroutine eio_stdhep_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (eio_stdhep_1, "eio_stdhep_1", &
         "read and write event contents, format [stdhep]", &
         u, results)
    call test (eio_stdhep_2, "eio_stdhep_2", &
         "read and write event contents, format [stdhep]", &
         u, results)
  end subroutine eio_stdhep_test
  
  subroutine eio_stdhep_1 (u)
    integer, intent(in) :: u
    type(model_list_t) :: model_list
    type(event_t), allocatable, target :: event
    type(process_t), allocatable, target :: process
    type(process_ptr_t) :: process_ptr
    type(process_instance_t), allocatable, target :: process_instance
    type(event_sample_data_t) :: data
    class(eio_t), allocatable :: eio
    type(string_t) :: sample
    integer :: u_file, iostat
    character(80) :: buffer

    write (u, "(A)")  "* Test output: eio_stdhep_1"
    write (u, "(A)")  "*   Purpose: generate an event in STDHEP HEPEVT format"
    write (u, "(A)")  "*      and write weight to file"
    write (u, "(A)")

    call syntax_model_file_init ()

    write (u, "(A)")  "* Initialize test process"
 
    allocate (process)
    process_ptr%ptr => process
    allocate (process_instance)
    call prepare_test_process (process, process_instance, model_list)
    call process_instance%setup_event_data ()
 
    allocate (event)
    call event%basic_init ()
    call event%connect (process_instance, process%get_model_ptr ())
    
    call data%init (1)
    data%n_evt = 1
    data%n_beam = 2
    data%pdg_beam = 25
    data%energy_beam = 500
    data%proc_num_id = [42]
    data%cross_section(1) = 100
    data%error(1) = 1
    data%total_cross_section = sum (data%cross_section)

    write (u, "(A)")
    write (u, "(A)")  "* Generate and write an event"
    write (u, "(A)")
 
    sample = "eio_stdhep_1"
 
    allocate (eio_stdhep_hepevt_t :: eio)
    
    call eio%init_out (sample, [process_ptr], data)
    call event%generate (1, [0._default, 0._default])
    call event%evaluate_expressions ()

    call eio%output (event, i_prc = 1)
    call eio%write (u)
    call eio%final ()

    write (u, "(A)")
    write (u, "(A)")  "* File contents:"
    write (u, "(A)")

    u_file = free_unit ()
    open (u_file, file = char (sample // ".stdhep"), &
         action = "read", status = "old")
    do
       read (u_file, "(A)", iostat = iostat)  buffer
       if (buffer(1:21) == "  <generator_version>")  buffer = "[...]"
       if (iostat /= 0)  exit
    end do
    !!! JRR: WK please check: should be replaced by a XDR reader? (#530)
    !  write (u, "(A)") trim (buffer) 
    !  The number of lines is system-dependent!
    write (u, "(A)") "     Successfully read STDHEP HEPEVT file"    
    close (u_file)
    
    write (u, "(A)")
    write (u, "(A)")  "* Reset data"
    write (u, "(A)")
 
    deallocate (eio)
    allocate (eio_stdhep_hepevt_t :: eio)
    
    select type (eio)
    type is (eio_stdhep_hepevt_t)
       call eio%set_parameters (keep_beams = .true.)
    end select
    call eio%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
 
    call event%final ()
    deallocate (event)
 
    call cleanup_test_process (process, process_instance)
    deallocate (process_instance)
    deallocate (process)

    call model_list%final ()
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: eio_stdhep_1"
    
  end subroutine eio_stdhep_1
  
  subroutine eio_stdhep_2 (u)
    integer, intent(in) :: u
    type(model_list_t) :: model_list
    type(event_t), allocatable, target :: event
    type(process_t), allocatable, target :: process
    type(process_ptr_t) :: process_ptr
    type(process_instance_t), allocatable, target :: process_instance
    type(event_sample_data_t) :: data
    class(eio_t), allocatable :: eio
    type(string_t) :: sample
    integer :: u_file, iostat
    character(80) :: buffer

    write (u, "(A)")  "* Test output: eio_stdhep_2"
    write (u, "(A)")  "*   Purpose: generate an event in STDHEP HEPEUP format"
    write (u, "(A)")  "*      and write weight to file"
    write (u, "(A)")

    call syntax_model_file_init ()

    write (u, "(A)")  "* Initialize test process"
 
    allocate (process)
    process_ptr%ptr => process
    allocate (process_instance)
    call prepare_test_process (process, process_instance, model_list)
    call process_instance%setup_event_data ()
 
    allocate (event)
    call event%basic_init ()
    call event%connect (process_instance, process%get_model_ptr ())
    
    call data%init (1)
    data%n_evt = 1
    data%n_beam = 2
    data%pdg_beam = 25
    data%energy_beam = 500
    data%proc_num_id = [42]
    data%cross_section(1) = 100
    data%error(1) = 1
    data%total_cross_section = sum (data%cross_section)

    write (u, "(A)")
    write (u, "(A)")  "* Generate and write an event"
    write (u, "(A)")
 
    sample = "eio_stdhep_2"
 
    allocate (eio_stdhep_hepeup_t :: eio)
    
    call eio%init_out (sample, [process_ptr], data)
    call event%generate (1, [0._default, 0._default])
    call event%evaluate_expressions ()

    call eio%output (event, i_prc = 1)
    call eio%write (u)
    call eio%final ()

    write (u, "(A)")
    write (u, "(A)")  "* File contents:"
    write (u, "(A)")

    u_file = free_unit ()
    open (u_file, file = char(sample // ".up.stdhep"), &
         action = "read", status = "old")
    do
       read (u_file, "(A)", iostat = iostat)  buffer
       if (buffer(1:21) == "  <generator_version>")  buffer = "[...]"
       if (iostat /= 0)  exit
    end do
    !!! JRR: WK please check: should be replaced by a XDR reader? (#530)
    !  write (u, "(A)") trim (buffer)
    !  The number of lines is system-dependent!    
    write (u, "(A)") "     Successfully read STDHEP HEPEUP file"    
    close (u_file)
    
    write (u, "(A)")
    write (u, "(A)")  "* Reset data"
    write (u, "(A)")
 
    deallocate (eio)
    allocate (eio_stdhep_hepeup_t :: eio)
    
    select type (eio)
    type is (eio_stdhep_hepeup_t)
       call eio%set_parameters (keep_beams = .true.)
    end select
    call eio%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
 
    call event%final ()
    deallocate (event)
 
    call cleanup_test_process (process, process_instance)
    deallocate (process_instance)
    deallocate (process)

    call model_list%final ()
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: eio_stdhep_2"
    
  end subroutine eio_stdhep_2
  

end module eio_stdhep
