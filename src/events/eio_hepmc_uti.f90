! WHIZARD 2.4.1 Mar 24 2017
!
! Copyright (C) 1999-2017 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com>
!     So Young Shim <soyoung.shim@desy.de>
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

module eio_hepmc_uti

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use model_data
  use event_base
  use eio_data
  use eio_base

  use eio_hepmc

  use eio_base_ut, only: eio_prepare_test, eio_cleanup_test
  use eio_base_ut, only: eio_prepare_fallback_model, eio_cleanup_fallback_model

  implicit none
  private

  public :: eio_hepmc_1
  public :: eio_hepmc_2

contains

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


end module eio_hepmc_uti
