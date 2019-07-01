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

module eio_hepmc
  
  use kinds !NODEP!
  use file_utils !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use diagnostics !NODEP!
  use unit_tests

  use lorentz !NODEP!
  use models
  use particles
  use subevents
  use beams
  use processes
  use events
  use eio_data
  use eio_base
  use hepmc_interface

  implicit none
  private

  public :: eio_hepmc_t
  public :: eio_hepmc_test

  type, extends (eio_t) :: eio_hepmc_t
     logical :: writing = .false.
     logical :: keep_beams = .false.
     type(hepmc_iostream_t) :: iostream
   contains
     procedure :: set_parameters => eio_hepmc_set_parameters
     procedure :: write => eio_hepmc_write
     procedure :: final => eio_hepmc_final
     procedure :: split_out => eio_hepmc_split_out
     procedure :: init_out => eio_hepmc_init_out
     procedure :: init_in => eio_hepmc_init_in
     procedure :: switch_inout => eio_hepmc_switch_inout
     procedure :: output => eio_hepmc_output
     procedure :: input_i_prc => eio_hepmc_input_i_prc
     procedure :: input_event => eio_hepmc_input_event
  end type eio_hepmc_t
  

contains
  
  subroutine eio_hepmc_set_parameters (eio, keep_beams, extension)
    class(eio_hepmc_t), intent(inout) :: eio
    logical, intent(in), optional :: keep_beams
    type(string_t), intent(in), optional :: extension
    if (present (keep_beams))  eio%keep_beams = keep_beams
    if (present (extension)) then
       eio%extension = extension
    else
       eio%extension = "hepmc"
    end if
  end subroutine eio_hepmc_set_parameters
  
  subroutine eio_hepmc_write (object, unit)
    class(eio_hepmc_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A)")  "HepMC event stream:"
    if (object%writing) then
       write (u, "(3x,A,A)")  "Writing to file   = ", char (object%filename)
    else
       write (u, "(3x,A)")  "[closed]"
    end if
    write (u, "(3x,A,L1)")    "Keep beams        = ", object%keep_beams
  end subroutine eio_hepmc_write
  
  subroutine eio_hepmc_final (object)
    class(eio_hepmc_t), intent(inout) :: object
    if (object%writing) then
       write (msg_buffer, "(A,A,A)")  "Events: closing HepMC file '", &
            char (object%filename), "'"
       call msg_message ()
       call hepmc_iostream_close (object%iostream)
       object%writing = .false.
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
  
  subroutine eio_hepmc_init_out (eio, sample, process_ptr, data, success)
    class(eio_hepmc_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(process_ptr_t), dimension(:), intent(in) :: process_ptr
    type(event_sample_data_t), intent(in), optional :: data
    logical, intent(out), optional :: success
    if (.not. present (data)) &
         call msg_bug ("HepMC initialization: missing data")
    if (data%n_beam /= 2) &
         call msg_fatal ("HepMC: defined for scattering processes only")
    if (.not. data%unweighted) &
         call msg_fatal ("HepMC: events must be unweighted")
    call eio%set_splitting (data)
    eio%sample = sample
    eio%extension = "hepmc"
    call eio%set_filename ()
    write (msg_buffer, "(A,A,A)")  "Events: writing to HepMC file '", &
         char (eio%filename), "'"
    call msg_message ()
    eio%writing = .true.
    call hepmc_iostream_open_out (eio%iostream, eio%filename)
    if (present (success))  success = .true.
  end subroutine eio_hepmc_init_out
    
  subroutine eio_hepmc_init_in & 
       (eio, sample, process_ptr, data, success, extension)
    class(eio_hepmc_t), intent(inout) :: eio
    type(string_t), intent(in) :: sample
    type(string_t), intent(in), optional :: extension
    type(process_ptr_t), dimension(:), intent(in) :: process_ptr
    type(event_sample_data_t), intent(inout), optional :: data
    logical, intent(out), optional :: success
    call msg_bug ("HepMC: event input not supported")
    if (present (success))  success = .false.
  end subroutine eio_hepmc_init_in
    
  subroutine eio_hepmc_switch_inout (eio, success)
    class(eio_hepmc_t), intent(inout) :: eio
    logical, intent(out), optional :: success
    call msg_bug ("HepMC: in-out switch not supported")
    if (present (success))  success = .false.
  end subroutine eio_hepmc_switch_inout
  
  subroutine eio_hepmc_output (eio, event, i_prc, reading)
    class(eio_hepmc_t), intent(inout) :: eio
    type(event_t), intent(in), target :: event
    integer, intent(in) :: i_prc
    logical, intent(in), optional :: reading
    type(hepmc_event_t) :: hepmc_event
    type(particle_set_t), pointer :: pset_ptr
    if (eio%writing) then
       pset_ptr => event%get_particle_set_ptr ()
       call hepmc_event_init (hepmc_event, proc_id = i_prc)
       call hepmc_event_from_particle_set (hepmc_event, pset_ptr)
       call hepmc_iostream_write_event (eio%iostream, hepmc_event)
       call hepmc_event_final (hepmc_event)
    else
       call eio%write ()
       call msg_fatal ("HepMC file is not open for writing")
    end if
  end subroutine eio_hepmc_output

  subroutine eio_hepmc_input_i_prc (eio, i_prc, iostat)
    class(eio_hepmc_t), intent(inout) :: eio
    integer, intent(out) :: i_prc
    integer, intent(out) :: iostat
    call msg_bug ("HepMC: event input not supported")
    i_prc = 0
    iostat = 1
  end subroutine eio_hepmc_input_i_prc

  subroutine eio_hepmc_input_event (eio, event, iostat)
    class(eio_hepmc_t), intent(inout) :: eio
    type(event_t), intent(inout), target :: event
    integer, intent(out) :: iostat
    call msg_bug ("HepMC: event input not supported")
    iostat = 1
  end subroutine eio_hepmc_input_event


  subroutine eio_hepmc_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (eio_hepmc_1, "eio_hepmc_1", &
         "read and write event contents", &
         u, results)
  end subroutine eio_hepmc_test
  
  subroutine eio_hepmc_1 (u)
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
    character(116) :: buffer

    write (u, "(A)")  "* Test output: eio_hepmc_1"
    write (u, "(A)")  "*   Purpose: generate an event and write weight to file"
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
    data%n_beam = 2
    data%pdg_beam = 25
    data%energy_beam = 500
    data%cross_section(1) = 100
    data%error(1) = 1
    data%total_cross_section = sum (data%cross_section)

    write (u, "(A)")
    write (u, "(A)")  "* Generate and write an event"
    write (u, "(A)")
 
    sample = "eio_hepmc_1"
 
    allocate (eio_hepmc_t :: eio)
    
    call eio%init_out (sample, [process_ptr], data)
    call event%generate (1, [0._default, 0._default])

    call eio%output (event, i_prc = 42)
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
  

end module eio_hepmc
