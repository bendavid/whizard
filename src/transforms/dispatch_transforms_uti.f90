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

module dispatch_transforms_uti

  use iso_varying_string, string_t => varying_string
  use format_utils, only: write_separator
  use variables
  use event_base, only: event_callback_t
  use models, only: model_t, model_list_t
  use models, only: syntax_model_file_init, syntax_model_file_final
  use beam_structures, only: beam_structure_t
  use eio_base, only: eio_t
  use os_interface, only: os_data_t, os_data_init
  use event_transforms, only: evt_t
  use dispatch_transforms

  implicit none
  private

  public :: dispatch_transforms_1
  public :: dispatch_transforms_2

contains

  subroutine dispatch_transforms_1 (u)
    integer, intent(in) :: u
    type(var_list_t) :: var_list
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    type(os_data_t) :: os_data
    class(event_callback_t), allocatable :: event_callback
    class(eio_t), allocatable :: eio

    write (u, "(A)")  "* Test output: dispatch_transforms_1"
    write (u, "(A)")  "*   Purpose: allocate an event I/O (eio) stream"
    write (u, "(A)")

    call var_list%init_defaults (0)
    call os_data_init (os_data)
    call syntax_model_file_init ()
    call model_list%read_model (var_str ("SM_hadrons"), &
         var_str ("SM_hadrons.mdl"), os_data, model)

    write (u, "(A)")  "* Allocate as raw"
    write (u, "(A)")

    call dispatch_eio (eio, var_str ("raw"), var_list, &
         model, event_callback)

    call eio%write (u)

    call eio%final ()
    deallocate (eio)

    write (u, "(A)")
    write (u, "(A)")  "* Allocate as checkpoints:"
    write (u, "(A)")

    call dispatch_eio (eio, var_str ("checkpoint"), var_list, &
         model, event_callback)

    call eio%write (u)

    call eio%final ()
    deallocate (eio)

    write (u, "(A)")
    write (u, "(A)")  "* Allocate as LHEF:"
    write (u, "(A)")

    call var_list%set_string (var_str ("$lhef_extension"), &
         var_str ("lhe_custom"), is_known = .true.)
    call dispatch_eio (eio, var_str ("lhef"), var_list, &
         model, event_callback)

    call eio%write (u)

    call eio%final ()
    deallocate (eio)

    write (u, "(A)")
    write (u, "(A)")  "* Allocate as HepMC:"
    write (u, "(A)")

    call dispatch_eio (eio, var_str ("hepmc"), var_list, &
         model, event_callback)

    call eio%write (u)

    call eio%final ()
    deallocate (eio)

    write (u, "(A)")
    write (u, "(A)")  "* Allocate as weight_stream"
    write (u, "(A)")

    call dispatch_eio (eio, var_str ("weight_stream"), var_list, &
         model, event_callback)

    call eio%write (u)

    call eio%final ()
    deallocate (eio)

    write (u, "(A)")
    write (u, "(A)")  "* Allocate as debug format"
    write (u, "(A)")

    call var_list%set_log (var_str ("?debug_verbose"), &
         .false., is_known = .true.)
    call dispatch_eio (eio, var_str ("debug"), var_list, &
         model, event_callback)

    call eio%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call eio%final ()
    call var_list%final ()
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: dispatch_transforms_1"

  end subroutine dispatch_transforms_1

  subroutine dispatch_transforms_2 (u)
    integer, intent(in) :: u
    type(var_list_t) :: var_list
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    type(os_data_t) :: os_data
    type(beam_structure_t) :: beam_structure
    class(evt_t), pointer :: evt

    write (u, "(A)")  "* Test output: dispatch_transforms_2"
    write (u, "(A)")  "*   Purpose: configure event transform"
    write (u, "(A)")

    call syntax_model_file_init ()
    call var_list%init_defaults (0)
    call os_data_init (os_data)
    call model_list%read_model (var_str ("SM_hadrons"), &
         var_str ("SM_hadrons.mdl"), os_data, model)

    write (u, "(A)")  "* Partonic decays"
    write (u, "(A)")

    call dispatch_evt_decay (evt, var_list)
    call evt%write (u, verbose = .true., more_verbose = .true.)

    call evt%final ()
    deallocate (evt)

    write (u, "(A)")
    write (u, "(A)")  "* Shower"
    write (u, "(A)")

    call var_list%set_log (var_str ("?allow_shower"), .true., &
         is_known = .true.)
    call var_list%set_string (var_str ("$shower_method"), &
         var_str ("WHIZARD"), is_known = .true.)
    call dispatch_evt_shower (evt, var_list, model, &
         model, os_data, beam_structure)
    call evt%write (u)
    call write_separator (u, 2)

    call evt%final ()
    deallocate (evt)

    call var_list%final ()
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: dispatch_transforms_2"

  end subroutine dispatch_transforms_2


end module dispatch_transforms_uti
