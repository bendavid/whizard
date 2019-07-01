! WHIZARD 2.2.6 May 02 2015
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

module eio_lcio
  
  use kinds
  use io_units
  use iso_varying_string, string_t => varying_string
  use string_utils
  use unit_tests
  use diagnostics
  use os_interface
  use lorentz
  use model_data
  use particles
  use subevents
  use event_base
  use hep_events
  use eio_data
  use eio_base
  use lcio_interface

  implicit none
  private

  public :: eio_lcio_t
  public :: eio_lcio_test

  type, extends (eio_t) :: eio_lcio_t
     logical :: writing = .false.
     logical :: reading = .false.
     logical :: recover_beams = .false.
     logical :: use_alpha_s_from_file = .false.
     logical :: use_scale_from_file = .false.
     type(lcio_writer_t) :: lcio_writer
     type(lcio_reader_t) :: lcio_reader
     type(lcio_run_header_t) :: lcio_run_hdr
     type(lcio_event_t) :: lcio_event
     integer, dimension(:), allocatable :: proc_num_id
   contains
     procedure :: set_parameters => eio_lcio_set_parameters
     procedure :: write => eio_lcio_write
     procedure :: final => eio_lcio_final
     procedure :: split_out => eio_lcio_split_out
     procedure :: common_init => eio_lcio_common_init
     procedure :: init_out => eio_lcio_init_out
     procedure :: init_in => eio_lcio_init_in
     procedure :: switch_inout => eio_lcio_switch_inout
     procedure :: output => eio_lcio_output
     procedure :: input_i_prc => eio_lcio_input_i_prc
     procedure :: input_event => eio_lcio_input_event
  end type eio_lcio_t
  



contains
  
  subroutine eio_lcio_set_parameters &
       (eio, recover_beams, use_alpha_s_from_file, use_scale_from_file, &
       extension)
    class(eio_lcio_t), intent(inout) :: eio
    logical, intent(in), optional :: recover_beams 
    logical, intent(in), optional :: use_alpha_s_from_file
    logical, intent(in), optional :: use_scale_from_file
    type(string_t), intent(in), optional :: extension    
    if (present (recover_beams))  eio%recover_beams = recover_beams
    if (present (use_alpha_s_from_file)) &
         eio%use_alpha_s_from_file = use_alpha_s_from_file
    if (present (use_scale_from_file)) &
         eio%use_scale_from_file = use_scale_from_file
    if (present (extension)) then
       eio%extension = extension
    else
       eio%extension = "slcio"
    end if
  end subroutine eio_lcio_set_parameters
  
  subroutine eio_lcio_write (object, unit)
    class(eio_lcio_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "LCIO event stream:"
    if (object%writing) then
       write (u, "(3x,A,A)")  "Writing to file   = ", char (object%filename)
    else if (object%reading) then
       write (u, "(3x,A,A)")  "Reading from file = ", char (object%filename)
    else
       write (u, "(3x,A)")  "[closed]"
    end if
    write (u, "(3x,A,L1)")    "Recover beams     = ", object%recover_beams
    write (u, "(3x,A,L1)")    "Alpha_s from file = ", &
         object%use_alpha_s_from_file
    write (u, "(3x,A,L1)")    "Scale from file   = ", &
         object%use_scale_from_file
    write (u, "(3x,A,A,A)")     "File extension    = '", &
         char (object%extension), "'"
    if (allocated (object%proc_num_id)) then
       write (u, "(3x,A)")  "Numerical process IDs:"
       do i = 1, size (object%proc_num_id)
          write (u, "(5x,I0,': ',I0)")  i, object%proc_num_id(i)
       end do
    end if    
  end subroutine eio_lcio_write
  
  subroutine eio_lcio_final (object)
    class(eio_lcio_t), intent(inout) :: object
    if (allocated (object%proc_num_id))  deallocate (object%proc_num_id)
    if (object%writing) then
       write (msg_buffer, "(A,A,A)")  "Events: closing LCIO file '", &
            char (object%filename), "'"
       call msg_message ()
       call lcio_writer_close (object%lcio_writer)
       object%writing = .false.
    else if (object%reading) then
       write (msg_buffer, "(A,A,A)")  "Events: closing LCIO file '", &
            char (object%filename), "'"
       call msg_message ()
       call lcio_reader_close (object%lcio_reader)
       object%reading = .false.
    end if
  end subroutine eio_lcio_final
  
  subroutine eio_lcio_split_out (eio)
    class(eio_lcio_t), intent(inout) :: eio
    if (eio%split) then
       eio%split_index = eio%split_index + 1
       call eio%set_filename ()
       write (msg_buffer, "(A,A,A)")  "Events: writing to LCIO file '", &
            char (eio%filename), "'"
       call msg_message ()
       call lcio_writer_close (eio%lcio_writer)
       call lcio_writer_open_out (eio%lcio_writer, eio%filename)       
    end if
  end subroutine eio_lcio_split_out
  
  subroutine eio_lcio_common_init (eio, sample, data, extension)
    class(eio_lcio_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(string_t), intent(in), optional :: extension
    type(event_sample_data_t), intent(in), optional :: data
    if (.not. present (data)) &
         call msg_bug ("LCIO initialization: missing data")
    if (data%n_beam /= 2) &
         call msg_fatal ("LCIO: defined for scattering processes only")    
    if (data%unweighted) then
       select case (data%norm_mode)
       case (NORM_UNIT)
       case default; call msg_fatal &
            ("LCIO: normalization for unweighted events must be '1'")
       end select
    else
       call msg_fatal ("LCIO: events must be unweighted")    
    end if
    eio%sample = sample    
    if (present (extension)) then
       eio%extension = extension
    end if
    call eio%set_filename ()
    allocate (eio%proc_num_id (data%n_proc), source = data%proc_num_id)
  end subroutine eio_lcio_common_init
  
  subroutine eio_lcio_init_out (eio, sample, data, success, extension)
    class(eio_lcio_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(string_t), intent(in), optional :: extension
    type(event_sample_data_t), intent(in), optional :: data
    logical, intent(out), optional :: success
    call eio%set_splitting (data)    
    call eio%common_init (sample, data, extension)
    write (msg_buffer, "(A,A,A)")  "Events: writing to LCIO file '", &
         char (eio%filename), "'"
    call msg_message ()
    eio%writing = .true.
    call lcio_writer_open_out (eio%lcio_writer, eio%filename)
    call lcio_run_header_init (eio%lcio_run_hdr)
    call lcio_run_header_write (eio%lcio_writer, eio%lcio_run_hdr)
    if (present (success))  success = .true.
  end subroutine eio_lcio_init_out
    
  subroutine eio_lcio_init_in (eio, sample, data, success, extension)
    class(eio_lcio_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(string_t), intent(in), optional :: extension
    type(event_sample_data_t), intent(inout), optional :: data
    logical, intent(out), optional :: success
    logical :: exist
    eio%split = .false.
    call eio%common_init (sample, data, extension)
    write (msg_buffer, "(A,A,A)")  "Events: reading from LCIO file '", &
         char (eio%filename), "'"
    call msg_message ()
    inquire (file = char (eio%filename), exist = exist)
    if (.not. exist)  call msg_fatal ("Events: LCIO file not found.")
    eio%reading = .true.
    call lcio_open_file (eio%lcio_reader, eio%filename)
    if (present (success))  success = .true.
  end subroutine eio_lcio_init_in
    
  subroutine eio_lcio_switch_inout (eio, success)
    class(eio_lcio_t), intent(inout) :: eio
    logical, intent(out), optional :: success
    call msg_bug ("LCIO: in-out switch not supported")
    if (present (success))  success = .false.
  end subroutine eio_lcio_switch_inout
  
  subroutine eio_lcio_output (eio, event, i_prc, reading, passed, pacify)
    class(eio_lcio_t), intent(inout) :: eio
    class(generic_event_t), intent(in), target :: event
    integer, intent(in) :: i_prc
    logical, intent(in), optional :: reading, passed, pacify
    type(particle_set_t), pointer :: pset_ptr
    if (present (passed)) then
       if (.not. passed)  return
    end if
    if (eio%writing) then
       pset_ptr => event%get_particle_set_ptr ()
       call lcio_event_init (eio%lcio_event, &
             proc_id = eio%proc_num_id (i_prc), &
             event_id = event%get_index ())
       call lcio_event_from_particle_set (eio%lcio_event, pset_ptr)
       call lcio_event_set_scale (eio%lcio_event, event%get_fac_scale ())
       call lcio_event_set_alpha_qcd (eio%lcio_event, event%get_alpha_s ())
       call lcio_event_write (eio%lcio_writer, eio%lcio_event)
       call lcio_event_final (eio%lcio_event)
    else
       call eio%write ()
       call msg_fatal ("LCIO file is not open for writing")
    end if
  end subroutine eio_lcio_output

  subroutine eio_lcio_input_i_prc (eio, i_prc, iostat)
    class(eio_lcio_t), intent(inout) :: eio
    integer, intent(out) :: i_prc
    integer, intent(out) :: iostat
    logical :: ok
    integer :: i, proc_num_id
    iostat = 0
    call lcio_read_event (eio%lcio_reader, eio%lcio_event, ok)
    if (.not. ok) then 
       iostat = -1 
       return
    end if
    proc_num_id = lcio_event_get_process_id (eio%lcio_event)    
    i_prc = 0
    FIND_I_PRC: do i = 1, size (eio%proc_num_id)
       if (eio%proc_num_id(i) == proc_num_id) then
          i_prc = i
          exit FIND_I_PRC
       end if
    end do FIND_I_PRC
    if (i_prc == 0)  call err_index
  contains
    subroutine err_index
      call msg_error ("LCIO: reading events: undefined process ID " &
           // char (str (proc_num_id)) // ", aborting read")
      iostat = 1
    end subroutine err_index
  end subroutine eio_lcio_input_i_prc

  subroutine eio_lcio_input_event (eio, event, iostat)
    class(eio_lcio_t), intent(inout) :: eio
    class(generic_event_t), intent(inout), target :: event
    integer, intent(out) :: iostat
    type(particle_set_t), pointer :: pset
    logical :: ok    
    iostat = 0
    call event%reset ()
    call event%select (1, 1, 1)
    call lcio_to_event (event, eio%lcio_event, eio%fallback_model, &
         recover_beams = eio%recover_beams, &
         use_alpha_s = eio%use_alpha_s_from_file, &
         use_scale = eio%use_scale_from_file) 
    call lcio_event_final (eio%lcio_event)
  end subroutine eio_lcio_input_event


  subroutine eio_lcio_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (eio_lcio_1, "eio_lcio_1", &
         "write event contents", &
         u, results)
    call test (eio_lcio_2, "eio_lcio_2", &
         "read event contents", &
         u, results)
  end subroutine eio_lcio_test
  
  subroutine eio_lcio_1 (u)
    integer, intent(in) :: u
    class(generic_event_t), pointer :: event
    type(event_sample_data_t) :: data
    class(eio_t), allocatable :: eio
    type(particle_set_t), pointer :: pset_ptr    
    type(string_t) :: sample
    integer :: u_file, iostat
    character(215) :: buffer

    write (u, "(A)")  "* Test output: eio_lcio_1"
    write (u, "(A)")  "*   Purpose: write a LCIO file"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize test process"
 
    call eio_prepare_test (event)

    call data%init (1)
    data%n_beam = 2
    data%unweighted = .true.
    data%norm_mode = NORM_UNIT
    data%pdg_beam = 25
    data%energy_beam = 500
    data%proc_num_id = [42]
    data%cross_section(1) = 100
    data%error(1) = 1
    data%total_cross_section = sum (data%cross_section)

    write (u, "(A)")
    write (u, "(A)")  "* Generate and write an event"
    write (u, "(A)")
 
    sample = "eio_lcio_1"
 
    allocate (eio_lcio_t :: eio)
    select type (eio)
    type is (eio_lcio_t)
       call eio%set_parameters ()
    end select

    call eio%init_out (sample, data)
    
    call event%generate (1, [0._default, 0._default])
    call event%pacify_particle_set ()
    
    call eio%output (event, i_prc = 1)    
    call eio%write (u)
    call eio%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Reset data"
    write (u, "(A)")
 
    deallocate (eio)
    allocate (eio_lcio_t :: eio)
    
    select type (eio)
    type is (eio_lcio_t)
       call eio%set_parameters ()
    end select
    call eio%write (u)

    write (u, "(A)") 
    write (u, "(A)")  "* Write LCIO file contents to ASCII file"
    write (u, "(A)")

    select type (eio)    
    type is (eio_lcio_t)       
       call lcio_event_init (eio%lcio_event, &
            proc_id = 42, &
            event_id = event%get_index ())
       pset_ptr => event%get_particle_set_ptr ()
       call lcio_event_from_particle_set &
            (eio%lcio_event,  pset_ptr)
       call write_lcio_event (eio%lcio_event, var_str ("test_file.slcio"))
       call lcio_event_final (eio%lcio_event)       
    end select
    
    write (u, "(A)") 
    write (u, "(A)")  "* Read in ASCII contents of LCIO file"
    write (u, "(A)")    
    
    u_file = free_unit ()
    open (u_file, file = "test_file.slcio", &
         action = "read", status = "old")
    do
       read (u_file, "(A)", iostat = iostat)  buffer
       if (iostat /= 0)  exit
       if (trim (buffer) == "")  cycle
       if (buffer(1:12) == " - timestamp")  cycle
       if (buffer(1:6) == " date:")  cycle       
       write (u, "(A)") trim (buffer)
    end do
    close (u_file)    
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
 
    call eio_cleanup_test (event)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: eio_lcio_1"

  end subroutine eio_lcio_1
  
  subroutine eio_lcio_2 (u)
    integer, intent(in) :: u
    class(model_data_t), pointer :: fallback_model
    class(generic_event_t), pointer :: event
    type(event_sample_data_t) :: data
    class(eio_t), allocatable :: eio
    type(string_t) :: sample
    integer :: u_file, iostat, i_prc

    write (u, "(A)")  "* Test output: eio_lcio_2"
    write (u, "(A)")  "*   Purpose: read a LCIO event"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize test process" 
    
    call eio_prepare_fallback_model (fallback_model)
    call eio_prepare_test (event)
    
    call data%init (1)
    data%n_beam = 2
    data%unweighted = .true.
    data%norm_mode = NORM_UNIT
    data%pdg_beam = 25
    data%energy_beam = 500
    data%proc_num_id = [42]
    data%cross_section(1) = 100
    data%error(1) = 1
    data%total_cross_section = sum (data%cross_section)    

    write (u, "(A)")
    write (u, "(A)")  "* Generate and write an event"
    write (u, "(A)")
 
    sample = "eio_lcio_2"
 
    allocate (eio_lcio_t :: eio)
    select type (eio)
    type is (eio_lcio_t)
       call eio%set_parameters (recover_beams = .false.)
    end select            
    call eio%set_fallback_model (fallback_model)
    
    call eio%init_out (sample, data)
    call event%generate (1, [0._default, 0._default])
    call event%evaluate_expressions ()
    call event%pacify_particle_set ()

    call eio%output (event, i_prc = 1)
    call eio%write (u)
    call eio%final ()
    deallocate (eio)
    
    write (u, "(A)")    
    write (u, "(A)")  "* Initialize"
    write (u, "(A)")
             
    allocate (eio_lcio_t :: eio)
    select type (eio)
    type is (eio_lcio_t)
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
    type is (eio_lcio_t)
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
    write (u, "(A)")  "* Test output end: eio_lcio_2"
    
  end subroutine eio_lcio_2
  

end module eio_lcio
