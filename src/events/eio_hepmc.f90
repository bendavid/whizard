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

module eio_hepmc
  
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
  use hepmc_interface

  implicit none
  private

  public :: eio_hepmc_t
  public :: eio_hepmc_test

  type, extends (eio_t) :: eio_hepmc_t
     logical :: writing = .false.
     logical :: reading = .false.
     ! logical :: keep_beams = .false.
     logical :: recover_beams = .false.
     logical :: use_alpha_s_from_file = .false.
     logical :: use_scale_from_file = .false.
     type(hepmc_iostream_t) :: iostream
     type(hepmc_event_t) :: hepmc_event
     integer, dimension(:), allocatable :: proc_num_id
   contains
     procedure :: set_parameters => eio_hepmc_set_parameters
     procedure :: write => eio_hepmc_write
     procedure :: final => eio_hepmc_final
     procedure :: split_out => eio_hepmc_split_out
     procedure :: common_init => eio_hepmc_common_init
     procedure :: init_out => eio_hepmc_init_out
     procedure :: init_in => eio_hepmc_init_in
     procedure :: switch_inout => eio_hepmc_switch_inout
     procedure :: output => eio_hepmc_output
     procedure :: input_i_prc => eio_hepmc_input_i_prc
     procedure :: input_event => eio_hepmc_input_event
  end type eio_hepmc_t
  



contains
  
  ! subroutine eio_hepmc_set_parameters (eio, keep_beams, recover_beams, extension)
  subroutine eio_hepmc_set_parameters &
       (eio, recover_beams, use_alpha_s_from_file, use_scale_from_file, &
       extension)
    class(eio_hepmc_t), intent(inout) :: eio
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
       eio%extension = "hepmc"
    end if
  end subroutine eio_hepmc_set_parameters
  
  subroutine eio_hepmc_write (object, unit)
    class(eio_hepmc_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "HepMC event stream:"
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
  end subroutine eio_hepmc_write
  
  subroutine eio_hepmc_final (object)
    class(eio_hepmc_t), intent(inout) :: object
    if (allocated (object%proc_num_id))  deallocate (object%proc_num_id)
    if (object%writing) then
       write (msg_buffer, "(A,A,A)")  "Events: closing HepMC file '", &
            char (object%filename), "'"
       call msg_message ()
       call hepmc_iostream_close (object%iostream)
       object%writing = .false.
    else if (object%reading) then
       write (msg_buffer, "(A,A,A)")  "Events: closing HepMC file '", &
            char (object%filename), "'"
       call msg_message ()
       call hepmc_iostream_close (object%iostream)
       object%reading = .false.
    end if
  end subroutine eio_hepmc_final
  
  subroutine eio_hepmc_split_out (eio)
    class(eio_hepmc_t), intent(inout) :: eio
    if (eio%split) then
       eio%split_index = eio%split_index + 1
       call eio%set_filename ()
       write (msg_buffer, "(A,A,A)")  "Events: writing to HepMC file '", &
            char (eio%filename), "'"
       call msg_message ()
       call hepmc_iostream_close (eio%iostream)
       call hepmc_iostream_open_out (eio%iostream, eio%filename)
    end if
  end subroutine eio_hepmc_split_out
  
  subroutine eio_hepmc_common_init (eio, sample, data, extension)
    class(eio_hepmc_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(string_t), intent(in), optional :: extension
    type(event_sample_data_t), intent(in), optional :: data
    if (.not. present (data)) &
         call msg_bug ("HepMC initialization: missing data")
    if (data%n_beam /= 2) &
         call msg_fatal ("HepMC: defined for scattering processes only")    
    if (data%unweighted) then
       select case (data%norm_mode)
       case (NORM_UNIT)
       case default; call msg_fatal &
            ("HepMC: normalization for unweighted events must be '1'")
       end select
    else
       call msg_fatal ("HepMC: events must be unweighted")    
    end if
    eio%sample = sample    
    if (present (extension)) then
       eio%extension = extension
    end if
    call eio%set_filename ()
    allocate (eio%proc_num_id (data%n_proc), source = data%proc_num_id)
  end subroutine eio_hepmc_common_init
  
  subroutine eio_hepmc_init_out (eio, sample, data, success, extension)
    class(eio_hepmc_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(string_t), intent(in), optional :: extension
    type(event_sample_data_t), intent(in), optional :: data
    logical, intent(out), optional :: success
    call eio%set_splitting (data)    
    call eio%common_init (sample, data, extension)
    write (msg_buffer, "(A,A,A)")  "Events: writing to HepMC file '", &
         char (eio%filename), "'"
    call msg_message ()
    eio%writing = .true.
    call hepmc_iostream_open_out (eio%iostream, eio%filename)
    if (present (success))  success = .true.
  end subroutine eio_hepmc_init_out
    
  subroutine eio_hepmc_init_in (eio, sample, data, success, extension)
    class(eio_hepmc_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(string_t), intent(in), optional :: extension
    type(event_sample_data_t), intent(inout), optional :: data
    logical, intent(out), optional :: success
    logical :: exist
    eio%split = .false.
    call eio%common_init (sample, data, extension)
    write (msg_buffer, "(A,A,A)")  "Events: reading from HepMC file '", &
         char (eio%filename), "'"
    call msg_message ()
    inquire (file = char (eio%filename), exist = exist)
    if (.not. exist)  call msg_fatal ("Events: HepMC file not found.")
    eio%reading = .true.
    call hepmc_iostream_open_in (eio%iostream, eio%filename)
    if (present (success))  success = .true.
  end subroutine eio_hepmc_init_in
    
  subroutine eio_hepmc_switch_inout (eio, success)
    class(eio_hepmc_t), intent(inout) :: eio
    logical, intent(out), optional :: success
    call msg_bug ("HepMC: in-out switch not supported")
    if (present (success))  success = .false.
  end subroutine eio_hepmc_switch_inout
  
  subroutine eio_hepmc_output (eio, event, i_prc, reading, passed, pacify)
    class(eio_hepmc_t), intent(inout) :: eio
    class(generic_event_t), intent(in), target :: event
    integer, intent(in) :: i_prc
    logical, intent(in), optional :: reading, passed, pacify
    type(particle_set_t), pointer :: pset_ptr
    if (present (passed)) then
       if (.not. passed)  return
    end if
    if (eio%writing) then
       pset_ptr => event%get_particle_set_ptr ()
       call hepmc_event_init (eio%hepmc_event, &
            proc_id = eio%proc_num_id (i_prc), &
            event_id = event%get_index ())
       call hepmc_event_from_particle_set (eio%hepmc_event, pset_ptr)
       call hepmc_event_set_scale (eio%hepmc_event, event%get_fac_scale ())
       call hepmc_event_set_alpha_qcd (eio%hepmc_event, event%get_alpha_s ())
       call hepmc_iostream_write_event (eio%iostream, eio%hepmc_event)
       call hepmc_event_final (eio%hepmc_event)
    else
       call eio%write ()
       call msg_fatal ("HepMC file is not open for writing")
    end if
  end subroutine eio_hepmc_output

  subroutine eio_hepmc_input_i_prc (eio, i_prc, iostat)
    class(eio_hepmc_t), intent(inout) :: eio
    integer, intent(out) :: i_prc
    integer, intent(out) :: iostat
    logical :: ok
    integer :: i, proc_num_id
    iostat = 0
    call hepmc_event_init (eio%hepmc_event)
    call hepmc_iostream_read_event (eio%iostream, eio%hepmc_event, ok)
    proc_num_id = hepmc_event_get_process_id (eio%hepmc_event)
    if (.not. ok) then 
       iostat = -1 
       return
    end if
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
      call msg_error ("HepMC: reading events: undefined process ID " &
           // char (str (proc_num_id)) // ", aborting read")
      iostat = 1
    end subroutine err_index
  end subroutine eio_hepmc_input_i_prc

  subroutine eio_hepmc_input_event (eio, event, iostat)
    class(eio_hepmc_t), intent(inout) :: eio
    class(generic_event_t), intent(inout), target :: event
    integer, intent(out) :: iostat
    type(particle_set_t), pointer :: pset
    logical :: ok    
    iostat = 0
    call event%reset ()
    call event%select (1, 1, 1)
    call hepmc_to_event (event, eio%hepmc_event, eio%fallback_model, &
         recover_beams = eio%recover_beams, &
         use_alpha_s = eio%use_alpha_s_from_file, &
         use_scale = eio%use_scale_from_file) 
    call hepmc_event_final (eio%hepmc_event)
  end subroutine eio_hepmc_input_event


  subroutine eio_hepmc_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (eio_hepmc_1, "eio_hepmc_1", &
         "write event contents", &
         u, results)
    call test (eio_hepmc_2, "eio_hepmc_2", &
         "read event contents", &
         u, results)
  end subroutine eio_hepmc_test
  
  subroutine eio_hepmc_1 (u)
    integer, intent(in) :: u
    class(generic_event_t), pointer :: event
    type(event_sample_data_t) :: data
    class(eio_t), allocatable :: eio
    type(string_t) :: sample
    integer :: u_file, iostat
    character(116) :: buffer

    write (u, "(A)")  "* Test output: eio_hepmc_1"
    write (u, "(A)")  "*   Purpose: write a HepMC file"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize test process"
 
    call eio_prepare_test (event, unweighted=.false.)

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
 
    sample = "eio_hepmc_1"
 
    allocate (eio_hepmc_t :: eio)
    select type (eio)
    type is (eio_hepmc_t)
       call eio%set_parameters ()
    end select
    
    call eio%init_out (sample, data)
    call event%generate (1, [0._default, 0._default])

    call eio%output (event, i_prc = 1)
    call eio%write (u)
    call eio%final ()

    write (u, "(A)")
    write (u, "(A)")  "* File contents (blanking out last two digits):"
    write (u, "(A)")

    u_file = free_unit ()
    open (u_file, file = char (sample // ".hepmc"), &
         action = "read", status = "old")
    do
       read (u_file, "(A)", iostat = iostat)  buffer
       if (iostat /= 0)  exit
       if (trim (buffer) == "")  cycle
       if (buffer(1:14) == "HepMC::Version")  cycle
       if (buffer(1:10) == "P 10001 25") &
            call buffer_blanker (buffer, 32, 55, 78)
       if (buffer(1:10) == "P 10002 25") &
            call buffer_blanker (buffer, 33, 56, 79)
       if (buffer(1:10) == "P 10003 25") &
            call buffer_blanker (buffer, 29, 53, 78, 101)
       if (buffer(1:10) == "P 10004 25") &
            call buffer_blanker (buffer, 28, 51, 76, 99)       
       write (u, "(A)") trim (buffer)
    end do
    close (u_file)
    
    write (u, "(A)")
    write (u, "(A)")  "* Reset data"
    write (u, "(A)")
 
    deallocate (eio)
    allocate (eio_hepmc_t :: eio)
    
    select type (eio)
    type is (eio_hepmc_t)
       call eio%set_parameters ()
    end select
    call eio%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
 
    call eio_cleanup_test (event)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: eio_hepmc_1"
    
  contains
    
    subroutine buffer_blanker (buf, pos1, pos2, pos3, pos4)
      character(len=*), intent(inout) :: buf
      integer, intent(in) :: pos1, pos2, pos3
      integer, intent(in), optional :: pos4
      type(string_t) :: line
      line = var_str (trim (buf))
      line = replace (line, pos1, "XX")
      line = replace (line, pos2, "XX")
      line = replace (line, pos3, "XX")
      if (present (pos4)) then
         line = replace (line, pos4, "XX")
      end if
      line = replace (line, "4999999999999", "5000000000000")
      buf = char (line)
    end subroutine buffer_blanker
    
  end subroutine eio_hepmc_1
  
  subroutine eio_hepmc_2 (u)
    integer, intent(in) :: u
    class(model_data_t), pointer :: fallback_model
    class(generic_event_t), pointer :: event
    type(event_sample_data_t) :: data
    class(eio_t), allocatable :: eio
    type(string_t) :: sample
    integer :: u_file, iostat, i_prc

    write (u, "(A)")  "* Test output: eio_hepmc_2"
    write (u, "(A)")  "*   Purpose: read a HepMC event"
    write (u, "(A)")

    write (u, "(A)")  "* Write a HepMC data file"
    write (u, "(A)")
    
    u_file = free_unit ()
    sample = "eio_hepmc_2"
    open (u_file, file = char (sample // ".hepmc"), &
         status = "replace", action = "readwrite")
    
    write (u_file, "(A)")  "HepMC::Version 2.06.09"
    write (u_file, "(A)")  "HepMC::IO_GenEvent-START_EVENT_LISTING"
    write (u_file, "(A)")  "E 0 -1 -1.0000000000000000e+00 &
         &-1.0000000000000000e+00 &
         &-1.0000000000000000e+00 42 0 1 10001 10002 0 0"
    write (u_file, "(A)")  "U GEV MM"
    write (u_file, "(A)")  "V -1 0 0 0 0 0 2 2 0"
    write (u_file, "(A)")  "P 10001 25 0 0 4.8412291827592713e+02 &
         &5.0000000000000000e+02 &
         &1.2499999999999989e+02 3 0 0 -1 0"
    write (u_file, "(A)")  "P 10002 25 0 0 -4.8412291827592713e+02 &
         &5.0000000000000000e+02 &
         &1.2499999999999989e+02 3 0 0 -1 0"
    write (u_file, "(A)")  "P 10003 25 -1.4960220911365536e+02 &
         &-4.6042825611414656e+02 &
         &0 5.0000000000000000e+02 1.2500000000000000e+02 1 0 0 0 0"
    write (u_file, "(A)")  "P 10004 25 1.4960220911365536e+02 &
         &4.6042825611414656e+02 &
         &0 5.0000000000000000e+02 1.2500000000000000e+02 1 0 0 0 0"
    write (u_file, "(A)")  "HepMC::IO_GenEvent-END_EVENT_LISTING"
    close (u_file)

    write (u, "(A)")  "* Initialize test process" 
    write (u, "(A)")
    
    call eio_prepare_fallback_model (fallback_model)
    call eio_prepare_test (event, unweighted=.false.)

    allocate (eio_hepmc_t :: eio)
    select type (eio)
    type is (eio_hepmc_t)
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

    write (u, "(A)")    
    write (u, "(A)")  "* Initialize"
    write (u, "(A)")
             
    call eio%init_in (sample, data)
    call eio%write (u)    
    
    write (u, "(A)")
    write (u, "(A)")  "* Read event"
    write (u, "(A)")
 
    call eio%input_i_prc (i_prc, iostat)

    select type (eio)
    type is (eio_hepmc_t)
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
    write (u, "(A)")  "* Test output end: eio_hepmc_2"
    
  end subroutine eio_hepmc_2
  

end module eio_hepmc
