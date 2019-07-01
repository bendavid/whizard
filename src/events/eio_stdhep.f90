! WHIZARD 2.2.5 Feb 27 2015
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

module eio_stdhep
  
  use kinds
  use io_units
  use iso_varying_string, string_t => varying_string
  use string_utils
  use unit_tests
  use diagnostics

  use lorentz
  use model_data
  use particles
  use event_base
  use hep_common
  use hep_events
  use xdr_stdhep
  use eio_data
  use eio_base

  implicit none
  private

  public :: eio_stdhep_t
  public :: eio_stdhep_hepevt_t
  public :: eio_stdhep_hepeup_t
  public :: stdhep_init_out
  public :: stdhep_init_in
  public :: stdhep_write 
  public :: stdhep_end  
  public :: eio_stdhep_test

  type, abstract, extends (eio_t) :: eio_stdhep_t
     logical :: writing = .false.
     logical :: reading = .false.
     integer :: unit = 0
     logical :: keep_beams = .false.     
     logical :: keep_remnants = .true.
     logical :: recover_beams = .false.
     logical :: use_alpha_s_from_file = .false.
     logical :: use_scale_from_file = .false.
     integer, dimension(:), allocatable :: proc_num_id     
     integer(i64) :: n_events_expected = 0
   contains
     procedure :: set_parameters => eio_stdhep_set_parameters
     procedure :: write => eio_stdhep_write
     procedure :: final => eio_stdhep_final
     procedure :: common_init => eio_stdhep_common_init
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
  
  subroutine eio_stdhep_set_parameters (eio, &
       keep_beams, keep_remnants, recover_beams, use_alpha_s_from_file, &
       use_scale_from_file, extension)
    class(eio_stdhep_t), intent(inout) :: eio
    logical, intent(in), optional :: keep_beams
    logical, intent(in), optional :: keep_remnants
    logical, intent(in), optional :: recover_beams
    logical, intent(in), optional :: use_alpha_s_from_file
    logical, intent(in), optional :: use_scale_from_file
    type(string_t), intent(in), optional :: extension
    if (present (keep_beams))  eio%keep_beams = keep_beams
    if (present (keep_remnants))  eio%keep_remnants = keep_remnants
    if (present (recover_beams))  eio%recover_beams = recover_beams
    if (present (use_alpha_s_from_file)) &
         eio%use_alpha_s_from_file = use_alpha_s_from_file
    if (present (use_scale_from_file))  &
         eio%use_scale_from_file = use_scale_from_file
    if (present (extension)) then
       eio%extension = extension
    else
       select type (eio)
       type is (eio_stdhep_hepevt_t)
          eio%extension = "hep"
       type is (eio_stdhep_hepeup_t)
          eio%extension = "up.hep"
       end select
    end if
  end subroutine eio_stdhep_set_parameters
  
  subroutine eio_stdhep_write (object, unit)
    class(eio_stdhep_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "STDHEP event stream:"
    if (object%writing) then
       write (u, "(3x,A,A)")  "Writing to file   = ", char (object%filename)
    else if (object%reading) then
       write (u, "(3x,A,A)")  "Reading from file = ", char (object%filename)
    else
       write (u, "(3x,A)")  "[closed]"
    end if
    write (u, "(3x,A,L1)")    "Keep beams        = ", object%keep_beams
    write (u, "(3x,A,L1)")    "Keep remnants     = ", object%keep_remnants
    write (u, "(3x,A,L1)")    "Recover beams     = ", object%recover_beams
    write (u, "(3x,A,L1)")    "Alpha_s from file = ", &
         object%use_alpha_s_from_file
    write (u, "(3x,A,L1)")    "Scale from file   = ", &
         object%use_scale_from_file    
    if (allocated (object%proc_num_id)) then
       write (u, "(3x,A)")  "Numerical process IDs:"
       do i = 1, size (object%proc_num_id)
          write (u, "(5x,I0,': ',I0)")  i, object%proc_num_id(i)
       end do
    end if      
  end subroutine eio_stdhep_write
  
  subroutine eio_stdhep_final (object)
    class(eio_stdhep_t), intent(inout) :: object
    if (allocated (object%proc_num_id))  deallocate (object%proc_num_id)
    if (object%writing) then
       write (msg_buffer, "(A,A,A)")  "Events: closing STDHEP file '", &
            char (object%filename), "'"
       call msg_message ()
       call stdhep_write (200)
       call stdhep_end ()
       object%writing = .false.
    else if (object%reading) then
       write (msg_buffer, "(A,A,A)")  "Events: closing STDHEP file '", &
            char (object%filename), "'"
       call msg_message ()
       object%reading = .false.
    end if
  end subroutine eio_stdhep_final
  
  subroutine eio_stdhep_common_init (eio, sample, data, extension)
    class(eio_stdhep_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(string_t), intent(in), optional :: extension
    type(event_sample_data_t), intent(in), optional :: data
    if (.not. present (data)) &
         call msg_bug ("STDHEP initialization: missing data")
    if (data%n_beam /= 2) &
         call msg_fatal ("STDHEP: defined for scattering processes only")
    if (present (extension)) then
       eio%extension = extension
    end if
    eio%sample = sample    
    call eio%set_filename ()    
    eio%unit = free_unit () 
    allocate (eio%proc_num_id (data%n_proc), source = data%proc_num_id)
  end subroutine eio_stdhep_common_init

  subroutine eio_stdhep_split_out (eio)
    class(eio_stdhep_t), intent(inout) :: eio
    if (eio%split) then
       eio%split_index = eio%split_index + 1
       call eio%set_filename ()
       write (msg_buffer, "(A,A,A)")  "Events: writing to STDHEP file '", &
            char (eio%filename), "'"
       call msg_message ()
       call stdhep_write (200)
       call stdhep_end ()
       select type (eio)
       type is (eio_stdhep_hepeup_t)
          call stdhep_init_out (char (eio%filename), &
               "WHIZARD 2.2.5", eio%n_events_expected)
          call stdhep_write (100)
          call stdhep_write (STDHEP_HEPRUP)
       type is (eio_stdhep_hepevt_t)
          call stdhep_init_out (char (eio%filename), &
               "WHIZARD 2.2.5", eio%n_events_expected) 
          call stdhep_write (100)
       end select
    end if
  end subroutine eio_stdhep_split_out
  
  subroutine eio_stdhep_init_out (eio, sample, data, success, extension)
    class(eio_stdhep_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(string_t), intent(in), optional :: extension
    type(event_sample_data_t), intent(in), optional :: data
    logical, intent(out), optional :: success
    integer :: i
    if (.not. present (data)) &
         call msg_bug ("STDHEP initialization: missing data")        
    call eio%set_splitting (data)    
    call eio%common_init (sample, data, extension)
    eio%n_events_expected = data%n_evt
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
       call stdhep_init_out (char (eio%filename), &
            "WHIZARD 2.2.5", eio%n_events_expected)
       call stdhep_write (100)
       call stdhep_write (STDHEP_HEPRUP)
    type is (eio_stdhep_hepevt_t)
       call stdhep_init_out (char (eio%filename), &
            "WHIZARD 2.2.5", eio%n_events_expected) 
       call stdhep_write (100)
    end select    
    if (present (success))  success = .true.
  end subroutine eio_stdhep_init_out
    
  subroutine eio_stdhep_init_in (eio, sample, data, success, extension)
    class(eio_stdhep_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(string_t), intent(in), optional :: extension
    type(event_sample_data_t), intent(inout), optional :: data
    logical, intent(out), optional :: success
    integer :: ilbl, lok
    logical :: exist
    call eio%common_init (sample, data, extension)
    write (msg_buffer, "(A,A,A)")  "Events: reading from STDHEP file '", &
         char (eio%filename), "'"
    call msg_message ()
    inquire (file = char (eio%filename), exist = exist)
    if (.not. exist)  call msg_fatal ("Events: STDHEP file not found.")
    eio%reading = .true.
    call stdhep_init_in (char (eio%filename), eio%n_events_expected)
    call stdhep_read (ilbl, lok)
    if (lok /= 0) then
       call stdhep_end () 
       write (msg_buffer, "(A)")  "Events: STDHEP file appears to" // &
            " be empty."
       call msg_message ()      
    end if
    if (ilbl == 100) then
       write (msg_buffer, "(A)")  "Events: reading in STDHEP events"
       call msg_message ()
    end if 
    if (present (success))  success = .false.
  end subroutine eio_stdhep_init_in
    
  subroutine eio_stdhep_switch_inout (eio, success)
    class(eio_stdhep_t), intent(inout) :: eio
    logical, intent(out), optional :: success
    call msg_bug ("STDHEP: in-out switch not supported")
    if (present (success))  success = .false.
  end subroutine eio_stdhep_switch_inout
  
  subroutine eio_stdhep_output (eio, event, i_prc, reading, passed, pacify)
    class(eio_stdhep_t), intent(inout) :: eio
    class(generic_event_t), intent(in), target :: event
    integer, intent(in) :: i_prc
    logical, intent(in), optional :: reading, passed, pacify
    if (present (passed)) then
       if (.not. passed)  return
    end if
    if (eio%writing) then
       select type (eio)
       type is (eio_stdhep_hepeup_t)
          call hepeup_from_event (event, &
               process_index = eio%proc_num_id (i_prc), &
               keep_beams = eio%keep_beams, &
               keep_remnants = eio%keep_remnants)
          call stdhep_write (STDHEP_HEPEUP)
       type is (eio_stdhep_hepevt_t)
          call hepevt_from_event (event, &
               i_evt = event%get_index (), &                         
               keep_beams = eio%keep_beams, &
               keep_remnants = eio%keep_remnants)
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
    integer :: i, ilbl, proc_num_id
    iostat = 0
    select type (eio)
    type is (eio_stdhep_hepevt_t)
       if (size (eio%proc_num_id) > 1) then
          call msg_fatal ("Events: only single processes allowed " // &
               "with the STDHEP HEPEVT format.")
       else
          proc_num_id = eio%proc_num_id (1)
          call stdhep_read (ilbl, lok)
       end if
    type is (eio_stdhep_hepeup_t)
       call stdhep_read (ilbl, lok)
       if (lok /= 0)  call msg_error ("Events: STDHEP appears to be " // &
            "empty or corrupted.")
       if (ilbl == 12) then
          call stdhep_read (ilbl, lok)
       end if
       if (ilbl == 11) then
          proc_num_id = IDPRUP
       end if       
    end select
    FIND_I_PRC: do i = 1, size (eio%proc_num_id)
       if (eio%proc_num_id(i) == proc_num_id) then
          i_prc = i
          exit FIND_I_PRC
       end if
    end do FIND_I_PRC
    if (i_prc == 0)  call err_index
  contains
    subroutine err_index
      call msg_error ("STDHEP: reading events: undefined process ID " &
           // char (str (proc_num_id)) // ", aborting read")
      iostat = 1
    end subroutine err_index    
  end subroutine eio_stdhep_input_i_prc

  subroutine eio_stdhep_input_event (eio, event, iostat)
    class(eio_stdhep_t), intent(inout) :: eio
    class(generic_event_t), intent(inout), target :: event
    integer, intent(out) :: iostat
    call event%reset ()
    call event%select (1, 1, 1)
    call hepeup_to_event (event, eio%fallback_model, &
         recover_beams = eio%recover_beams, &
         use_alpha_s = eio%use_alpha_s_from_file, &
         use_scale = eio%use_scale_from_file)    
  end subroutine eio_stdhep_input_event

  subroutine stdhep_init_out (file, title, nevt)
    character(len=*), intent(in) :: file, title
    integer(i64), intent(in) :: nevt
    integer(i32) :: nevt32
    external stdxwinit, stdxwrt
    nevt32 = min (nevt, int (huge (1_i32), i64))
    call stdxwinit (file, title, nevt32, istr, lok)    
  end subroutine stdhep_init_out

  subroutine stdhep_init_in (file, nevt)
    character(len=*), intent(in) :: file
    integer(i64), intent(out) :: nevt
    integer(i32) :: nevt32
    external stdxrinit, stdxrd
    call stdxrinit (file, nevt32, istr, lok)
    if (lok /= 0)  call msg_fatal ("STDHEP: error in reading file '" // &
         file // "'.")
    nevt = int (nevt32, i64)
  end subroutine stdhep_init_in
  
  subroutine stdhep_write (ilbl)
    integer, intent(in) :: ilbl
    external stdxwrt
    call stdxwrt (ilbl, istr, lok)
  end subroutine stdhep_write

  subroutine stdhep_read (ilbl, lok)
    integer, intent(out) :: ilbl, lok
    external stdxrd
    call stdxrd (ilbl, istr, lok)
    if (lok /= 0)  return
  end subroutine stdhep_read
  
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
    call test (eio_stdhep_3, "eio_stdhep_3", &
         "read StdHep file, HEPEVT block", &
         u, results)
    call test (eio_stdhep_4, "eio_stdhep_4", &
         "read StdHep file, HEPRUP/HEPEUP block", &
         u, results)
  end subroutine eio_stdhep_test
  
  subroutine eio_stdhep_1 (u)
    integer, intent(in) :: u
    class(generic_event_t), pointer :: event
    type(event_sample_data_t) :: data
    class(eio_t), allocatable :: eio
    type(string_t) :: sample
    integer :: u_file, iostat
    character(215) :: buffer

    write (u, "(A)")  "* Test output: eio_stdhep_1"
    write (u, "(A)")  "*   Purpose: generate an event in STDHEP HEPEVT format"
    write (u, "(A)")  "*      and write weight to file"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize test process"
 
    call eio_prepare_test (event)
    
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
    select type (eio)
    type is (eio_stdhep_hepevt_t)
       call eio%set_parameters ()
    end select
    
    call eio%init_out (sample, data)
    call event%generate (1, [0._default, 0._default])
    call event%evaluate_expressions ()
    call event%pacify_particle_set ()
    
    call eio%output (event, i_prc = 1)
    call eio%write (u)
    call eio%final ()
    
    write (u, "(A)") 
    write (u, "(A)")  "* Write STDHEP file contents to ASCII file"
    write (u, "(A)")

    call write_stdhep_event &
         (sample // ".hep", var_str ("test_1.hep"), 1)
        
    write (u, "(A)") 
    write (u, "(A)")  "* Read in ASCII contents of STDHEP file"
    write (u, "(A)")    
    
    u_file = free_unit ()
    open (u_file, file = "test_1.hep", &
         action = "read", status = "old")
    do
       read (u_file, "(A)", iostat = iostat)  buffer
       if (iostat /= 0)  exit
       if (trim (buffer) == "")  cycle
       if (buffer(1:18) == "    total blocks: ")  &
            buffer = "    total blocks: [...]"
       if (buffer(1:25) == "           title: WHIZARD")  &
            buffer = "           title: WHIZARD [version]"
       if (buffer(1:17) == "            date:")  &
            buffer = "            date: [...]"
       if (buffer(1:17) == "    closing date:")  &
            buffer = "    closing date: [...]"       
       write (u, "(A)") trim (buffer)
    end do
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
 
    call eio_cleanup_test (event)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: eio_stdhep_1"
    
  end subroutine eio_stdhep_1
  
  subroutine eio_stdhep_2 (u)
    integer, intent(in) :: u
    class(generic_event_t), pointer :: event
    type(event_sample_data_t) :: data
    class(model_data_t), pointer :: fallback_model
    class(eio_t), allocatable :: eio
    type(string_t) :: sample
    integer :: u_file, iostat
    character(215) :: buffer

    write (u, "(A)")  "* Test output: eio_stdhep_2"
    write (u, "(A)")  "*   Purpose: generate an event in STDHEP HEPEUP format"
    write (u, "(A)")  "*      and write weight to file"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize test process"
 
    call eio_prepare_fallback_model (fallback_model)
    call eio_prepare_test (event, unweighted = .false.)
    
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
    select type (eio)
    type is (eio_stdhep_hepeup_t)
       call eio%set_parameters ()
    end select  
    call eio%set_fallback_model (fallback_model)
    
    call eio%init_out (sample, data)
    call event%generate (1, [0._default, 0._default])
    call event%evaluate_expressions ()

    call eio%output (event, i_prc = 1)
    call eio%write (u)
    call eio%final ()

    write (u, "(A)") 
    write (u, "(A)")  "* Write STDHEP file contents to ASCII file"
    write (u, "(A)")

    call write_stdhep_event & 
         (sample // ".up.hep", var_str ("test_2.hep"), 2)
        
    write (u, "(A)") 
    write (u, "(A)")  "* Read in ASCII contents of STDHEP file"
    write (u, "(A)")    
    
    u_file = free_unit ()
    open (u_file, file = "test_2.hep", &
         action = "read", status = "old")
    do
       read (u_file, "(A)", iostat = iostat)  buffer
       if (iostat /= 0)  exit
       if (trim (buffer) == "")  cycle
       if (buffer(1:18) == "    total blocks: ")  &
            buffer = "    total blocks: [...]"       
       if (buffer(1:25) == "           title: WHIZARD")  &
            buffer = "           title: WHIZARD [version]"
       if (buffer(1:17) == "            date:")  &
            buffer = "            date: [...]"
       if (buffer(1:17) == "    closing date:")  &
            buffer = "    closing date: [...]"       
       write (u, "(A)") trim (buffer)
    end do
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
 
    call eio_cleanup_test (event)
    call eio_cleanup_fallback_model (fallback_model)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: eio_stdhep_2"
    
  end subroutine eio_stdhep_2
  
  subroutine eio_stdhep_3 (u)
    integer, intent(in) :: u
    class(model_data_t), pointer :: fallback_model
    class(generic_event_t), pointer :: event
    type(event_sample_data_t) :: data
    class(eio_t), allocatable :: eio
    type(string_t) :: sample
    integer :: u_file, iostat, i_prc

    write (u, "(A)")  "* Test output: eio_stdhep_3"
    write (u, "(A)")  "*   Purpose: read a StdHep file, HEPEVT block"
    write (u, "(A)")

    write (u, "(A)")  "* Write a StdHep data file, HEPEVT block"
    write (u, "(A)")
 
    call eio_prepare_fallback_model (fallback_model)    
    call eio_prepare_test (event)
    
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
 
    sample = "eio_stdhep_3"
 
    allocate (eio_stdhep_hepevt_t :: eio)
    select type (eio)
    type is (eio_stdhep_hepevt_t)
       call eio%set_parameters ()
    end select  
    call eio%set_fallback_model (fallback_model)
    
    call eio%init_out (sample, data)
    call event%generate (1, [0._default, 0._default])
    call event%evaluate_expressions ()

    call eio%output (event, i_prc = 1)
    call eio%write (u)
    call eio%final ()

    call eio_cleanup_test (event)
    call eio_cleanup_fallback_model (fallback_model)    
    deallocate (eio)
    
    write (u, "(A)")  "* Initialize test process"
    write (u, "(A)")

    call eio_prepare_fallback_model (fallback_model)
    call eio_prepare_test (event, unweighted = .false.)

    allocate (eio_stdhep_hepevt_t :: eio)
    select type (eio)
    type is (eio_stdhep_hepevt_t)
       call eio%set_parameters (recover_beams = .false.)
    end select
    call eio%set_fallback_model (fallback_model)
    
    call data%init (1)
    data%n_beam = 2
    data%unweighted = .true.
    data%norm_mode = NORM_UNIT
    data%pdg_beam = 25
    data%energy_beam = 500
    data%proc_num_id = [42]
    call data%write (u)
    write (u, *)

    write (u, "(A)")  "* Initialize"
    write (u, "(A)")

    call eio%init_in (sample, data)
    call eio%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Read event"
    write (u, "(A)")

    call eio%input_i_prc (i_prc, iostat)
    
    select type (eio)
    type is (eio_stdhep_hepevt_t)
       write (u, "(A,I0,A,I0)")  "Found process #", i_prc, &
            " with ID = ", eio%proc_num_id(i_prc)
    end select

    call eio%input_event (event, iostat)

    call event%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Read closing"
    write (u, "(A)")

    call eio%input_i_prc (i_prc, iostat)
    write (u, "(A,I0)")  "iostat = ", iostat
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
 
    call eio%final ()

    call eio_cleanup_test (event)
    call eio_cleanup_fallback_model (fallback_model)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: eio_stdhep_3"
    
  end subroutine eio_stdhep_3
  
  subroutine eio_stdhep_4 (u)
    integer, intent(in) :: u
    class(model_data_t), pointer :: fallback_model
    class(generic_event_t), pointer :: event
    type(event_sample_data_t) :: data
    class(eio_t), allocatable :: eio
    type(string_t) :: sample
    integer :: u_file, iostat, i_prc

    write (u, "(A)")  "* Test output: eio_stdhep_3"
    write (u, "(A)")  "*   Purpose: read a StdHep file, HEPRUP/HEPEUP block"
    write (u, "(A)")

    write (u, "(A)")  "* Write a StdHep data file, HEPRUP/HEPEUP block"
    write (u, "(A)")
 
    call eio_prepare_fallback_model (fallback_model)    
    call eio_prepare_test (event)
    
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
    write (u, "(A)")  "* Generate and write an event, HEPEUP/HEPRUP"
    write (u, "(A)")
 
    sample = "eio_stdhep_4"
 
    allocate (eio_stdhep_hepeup_t :: eio)
    select type (eio)
    type is (eio_stdhep_hepeup_t)
       call eio%set_parameters ()
    end select  
    call eio%set_fallback_model (fallback_model)
    
    call eio%init_out (sample, data)
    call event%generate (1, [0._default, 0._default])
    call event%evaluate_expressions ()
    call event%pacify_particle_set ()    

    call eio%output (event, i_prc = 1)
    call eio%write (u)
    call eio%final ()

    call eio_cleanup_test (event)
    call eio_cleanup_fallback_model (fallback_model)    
    deallocate (eio)
    
    write (u, "(A)")  "* Initialize test process"
    write (u, "(A)")

    call eio_prepare_fallback_model (fallback_model)
    call eio_prepare_test (event, unweighted = .false.)

    allocate (eio_stdhep_hepeup_t :: eio)
    select type (eio)
    type is (eio_stdhep_hepeup_t)
       call eio%set_parameters (recover_beams = .false.)
    end select
    call eio%set_fallback_model (fallback_model)
    
    call data%init (1)
    data%n_beam = 2
    data%unweighted = .true.
    data%norm_mode = NORM_UNIT
    data%pdg_beam = 25
    data%energy_beam = 500
    data%proc_num_id = [42]
    call data%write (u)
    write (u, *)

    write (u, "(A)")  "* Initialize"
    write (u, "(A)")

    call eio%init_in (sample, data)
    call eio%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Read event"
    write (u, "(A)")

    call eio%input_i_prc (i_prc, iostat)
    
    select type (eio)
    type is (eio_stdhep_hepeup_t)
       write (u, "(A,I0,A,I0)")  "Found process #", i_prc, &
            " with ID = ", eio%proc_num_id(i_prc)
    end select

    call eio%input_event (event, iostat)

    call event%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Read closing"
    write (u, "(A)")

    call eio%input_i_prc (i_prc, iostat)
    write (u, "(A,I0)")  "iostat = ", iostat
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
 
    call eio%final ()

    call eio_cleanup_test (event)
    call eio_cleanup_fallback_model (fallback_model)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: eio_stdhep_4"
    
  end subroutine eio_stdhep_4
  

end module eio_stdhep
