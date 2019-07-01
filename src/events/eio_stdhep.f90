! WHIZARD 2.6.1 Nov 03 2017
!
! Copyright (C) 1999-2017 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     cf. main AUTHORS file
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

  use kinds, only: i32, i64
  use iso_varying_string, string_t => varying_string
  use io_units
  use string_utils
  use diagnostics
  use event_base
  use hep_common
  use hep_events
  use eio_data
  use eio_base

  implicit none
  private

  public :: eio_stdhep_t
  public :: eio_stdhep_hepevt_t
  public :: eio_stdhep_hepeup_t
  public :: eio_stdhep_hepev4_t
  public :: stdhep_init_out
  public :: stdhep_init_in
  public :: stdhep_write
  public :: stdhep_end

  type, abstract, extends (eio_t) :: eio_stdhep_t
     logical :: writing = .false.
     logical :: reading = .false.
     integer :: unit = 0
     logical :: keep_beams = .false.
     logical :: keep_remnants = .true.
     logical :: ensure_order = .false.
     logical :: recover_beams = .false.
     logical :: use_alphas_from_file = .false.
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
     procedure :: skip => eio_stdhep_skip
  end type eio_stdhep_t

  type, extends (eio_stdhep_t) :: eio_stdhep_hepevt_t
  end type eio_stdhep_hepevt_t

  type, extends (eio_stdhep_t) :: eio_stdhep_hepeup_t
  end type eio_stdhep_hepeup_t

  type, extends (eio_stdhep_t) :: eio_stdhep_hepev4_t
  end type eio_stdhep_hepev4_t


  integer, save :: istr, lok
  integer, parameter :: &
       STDHEP_HEPEVT = 1, STDHEP_HEPEV4 = 4, &
       STDHEP_HEPEUP = 11, STDHEP_HEPRUP = 12

contains

  subroutine eio_stdhep_set_parameters (eio, &
       keep_beams, keep_remnants, ensure_order, recover_beams, &
       use_alphas_from_file, use_scale_from_file, extension)
    class(eio_stdhep_t), intent(inout) :: eio
    logical, intent(in), optional :: keep_beams
    logical, intent(in), optional :: keep_remnants
    logical, intent(in), optional :: ensure_order
    logical, intent(in), optional :: recover_beams
    logical, intent(in), optional :: use_alphas_from_file
    logical, intent(in), optional :: use_scale_from_file
    type(string_t), intent(in), optional :: extension
    if (present (keep_beams))  eio%keep_beams = keep_beams
    if (present (keep_remnants))  eio%keep_remnants = keep_remnants
    if (present (ensure_order))  eio%ensure_order = ensure_order
    if (present (recover_beams))  eio%recover_beams = recover_beams
    if (present (use_alphas_from_file)) &
         eio%use_alphas_from_file = use_alphas_from_file
    if (present (use_scale_from_file))  &
         eio%use_scale_from_file = use_scale_from_file
    if (present (extension)) then
       eio%extension = extension
    else
       select type (eio)
       type is (eio_stdhep_hepevt_t)
          eio%extension = "hep"
       type is (eio_stdhep_hepev4_t)
          eio%extension = "ev4.hep"
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
         object%use_alphas_from_file
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
               "WHIZARD 2.6.1", eio%n_events_expected)
          call stdhep_write (100)
          call stdhep_write (STDHEP_HEPRUP)
       type is (eio_stdhep_hepevt_t)
          call stdhep_init_out (char (eio%filename), &
               "WHIZARD 2.6.1", eio%n_events_expected)
          call stdhep_write (100)
       type is (eio_stdhep_hepev4_t)
          call stdhep_init_out (char (eio%filename), &
               "WHIZARD 2.6.1", eio%n_events_expected)
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
            "WHIZARD 2.6.1", eio%n_events_expected)
       call stdhep_write (100)
       call stdhep_write (STDHEP_HEPRUP)
    type is (eio_stdhep_hepevt_t)
       call stdhep_init_out (char (eio%filename), &
            "WHIZARD 2.6.1", eio%n_events_expected)
       call stdhep_write (100)
    type is (eio_stdhep_hepev4_t)
       call stdhep_init_out (char (eio%filename), &
            "WHIZARD 2.6.1", eio%n_events_expected)
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
               keep_remnants = eio%keep_remnants, &
               ensure_order = eio%ensure_order)
          call stdhep_write (STDHEP_HEPEVT)
       type is (eio_stdhep_hepev4_t)
          call hepevt_from_event (event, &
               process_index = eio%proc_num_id (i_prc), &
               i_evt = event%get_index (), &
               keep_beams = eio%keep_beams, &
               keep_remnants = eio%keep_remnants, &
               ensure_order = eio%ensure_order, &
               fill_hepev4 = .true.)
          call stdhep_write (STDHEP_HEPEV4)
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
    type is (eio_stdhep_hepev4_t)
       call stdhep_read (ilbl, lok)
       proc_num_id = idruplh
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
    iostat = 0
    call event%reset ()
    call event%select (1, 1, 1)
    call hepeup_to_event (event, eio%fallback_model, &
         recover_beams = eio%recover_beams, &
         use_alpha_s = eio%use_alphas_from_file, &
         use_scale = eio%use_scale_from_file)
  end subroutine eio_stdhep_input_event

  subroutine eio_stdhep_skip (eio, iostat)
    class(eio_stdhep_t), intent(inout) :: eio
    integer, intent(out) :: iostat
    if (eio%reading) then
       read (eio%unit, iostat = iostat)
    else
       call eio%write ()
       call msg_fatal ("Raw event file is not open for reading")
    end if
  end subroutine eio_stdhep_skip

  subroutine stdhep_init_out (file, title, nevt)
    character(len=*), intent(in) :: file, title
    integer(i64), intent(in) :: nevt
    integer(i32) :: nevt32
    nevt32 = min (nevt, int (huge (1_i32), i64))
    call stdxwinit (file, title, nevt32, istr, lok)
  end subroutine stdhep_init_out

  subroutine stdhep_init_in (file, nevt)
    character(len=*), intent(in) :: file
    integer(i64), intent(out) :: nevt
    integer(i32) :: nevt32
    call stdxrinit (file, nevt32, istr, lok)
    if (lok /= 0)  call msg_fatal ("STDHEP: error in reading file '" // &
         file // "'.")
    nevt = int (nevt32, i64)
  end subroutine stdhep_init_in

  subroutine stdhep_write (ilbl)
    integer, intent(in) :: ilbl
    call stdxwrt (ilbl, istr, lok)
  end subroutine stdhep_write

  subroutine stdhep_read (ilbl, lok)
    integer, intent(out) :: ilbl, lok
    call stdxrd (ilbl, istr, lok)
    if (lok /= 0)  return
  end subroutine stdhep_read

  subroutine stdhep_end
    call stdxend (istr)
  end subroutine stdhep_end


end module eio_stdhep
