! WHIZARD 2.2.7 Aug 11 2015
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

module shower_uti

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use format_utils, only: write_separator
  use os_interface
  use sm_qcd
  use model_data
  use state_matrices, only: FM_IGNORE_HELICITY
  use process_libraries
  use rng_base
  use rng_tao
  use mci_base
  use mci_midpoint
  use phs_base
  use phs_single
  use prc_core
  use prc_omega
  use variables
  use models
  use processes
  use event_transforms

  use pdf
  use shower_base
  use shower_core

  use shower

  implicit none
  private

  public :: shower_1
  public :: shower_2

contains

  subroutine setup_testbed &
       (prefix, os_data, lib, model_list, process, process_instance)
    type(string_t), intent(in) :: prefix
    type(os_data_t), intent(out) :: os_data
    type(process_library_t), intent(out), target :: lib
    type(model_list_t), intent(out) :: model_list
    class(model_data_t), pointer :: model
    type(model_t), pointer :: model_tmp
    type(process_t), target, intent(out) :: process
    type(process_instance_t), target, intent(out) :: process_instance
    type(var_list_t), pointer :: model_vars
    type(string_t) :: model_name, libname, procname, run_id
    type(process_def_entry_t), pointer :: entry
    type(string_t), dimension(:), allocatable :: prt_in, prt_out
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    class(prc_core_t), allocatable :: core_template
    class(mci_t), allocatable :: mci_template
    class(phs_config_t), allocatable :: phs_config_template
    real(default) :: sqrts

    model_name = "SM"
    libname = prefix // "_lib"
    procname = prefix // "p"
    run_id = "1"

    call os_data_init (os_data)
    allocate (rng_tao_factory_t :: rng_factory)
    allocate (model_tmp)
    call model_list%read_model (model_name, model_name // ".mdl", &
         os_data, model_tmp)
    model_vars => model_tmp%get_var_list_ptr ()
    call model_vars%set_real (var_str ("me"), 0._default, &
         is_known = .true.)
    model => model_tmp

    call lib%init (libname)

    allocate (prt_in (2), source = [var_str ("e-"), var_str ("e+")])
    allocate (prt_out (2), source = [var_str ("d"), var_str ("dbar")])

    allocate (entry)
    call entry%init (procname, model, n_in = 2, n_components = 1)
    call omega_make_process_component (entry, 1, &
         model_name, prt_in, prt_out, &
         report_progress=.true.)
    call lib%append (entry)

    call lib%configure (os_data)
    call lib%write_makefile (os_data, force = .true.)
    call lib%clean (os_data, distclean = .false.)
    call lib%write_driver (force = .true.)
    call lib%load (os_data)

    call process%init (procname, run_id, lib, os_data, &
         qcd, rng_factory, model)

    allocate (prc_omega_t :: core_template)
    allocate (mci_midpoint_t :: mci_template)
    allocate (phs_single_config_t :: phs_config_template)

    model => process%get_model_ptr ()

    select type (core_template)
    type is (prc_omega_t)
       call core_template%set_parameters (model = model)
    end select
    call process%init_component &
         (1, core_template, mci_template, phs_config_template)

    sqrts = 1000
    call process%setup_beams_sqrts (sqrts)
    call process%configure_phs ()
    call process%setup_mci ()
    call process%setup_terms ()

    call process_instance%init (process)
    call process%integrate (process_instance, 1, 1, 1000)
    call process%final_integration (1)

    call process_instance%setup_event_data ()
    call process_instance%init_simulation (1)
    call process%generate_weighted_event (process_instance, 1)
    call process_instance%evaluate_event_data ()

  end subroutine setup_testbed

  subroutine shower_1 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(process_library_t), target :: lib
    type(model_list_t) :: model_list
    class(model_data_t), pointer :: model
    type(model_t), pointer :: model_hadrons
    type(process_t), target :: process
    type(process_instance_t), target :: process_instance
    type(pdf_data_t) :: pdf_data
    integer :: factorization_mode
    logical :: keep_correlations
    class(evt_t), allocatable, target :: evt_trivial
    class(evt_t), allocatable, target :: evt_shower
    type(shower_settings_t) :: settings

    write (u, "(A)")  "* Test output: shower_1"
    write (u, "(A)")  "*   Purpose: Two-jet event with disabled shower"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize environment"
    write (u, "(A)")

    call syntax_model_file_init ()
    call os_data_init (os_data)
    call model_list%read_model &
         (var_str ("SM_hadrons"), var_str ("SM_hadrons.mdl"), &
         os_data, model_hadrons)
    call setup_testbed (var_str ("shower_1"), &
         os_data, lib, model_list, process, process_instance)

    write (u, "(A)")  "* Set up trivial transform"
    write (u, "(A)")

    allocate (evt_trivial_t :: evt_trivial)
    model => process%get_model_ptr ()
    call evt_trivial%connect (process_instance, model)
    call evt_trivial%prepare_new_event (1, 1)
    call evt_trivial%generate_unweighted ()

    factorization_mode = FM_IGNORE_HELICITY
    keep_correlations = .false.
    call evt_trivial%make_particle_set (factorization_mode, keep_correlations)

    select type (evt_trivial)
    type is (evt_trivial_t)
       call evt_trivial%write (u)
       call write_separator (u, 2)
    end select

    write (u, "(A)")
    write (u, "(A)")  "* Set up shower event transform"
    write (u, "(A)")

    allocate (evt_shower_t :: evt_shower)
    select type (evt_shower)
    type is (evt_shower_t)
       call evt_shower%init (model_hadrons, os_data)
       allocate (shower_t :: evt_shower%shower)
       call evt_shower%shower%init (settings, pdf_data)
       call evt_shower%connect (process_instance, model)
    end select

    evt_trivial%next => evt_shower
    evt_shower%previous => evt_trivial

    call evt_shower%prepare_new_event (1, 1)
    call evt_shower%generate_unweighted ()
    call evt_shower%make_particle_set (factorization_mode, keep_correlations)

    select type (evt_shower)
    type is (evt_shower_t)
       call evt_shower%write (u)
       call write_separator (u, 2)
    end select

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call evt_shower%final ()
    call evt_trivial%final ()
    call process_instance%final ()
    call process%final ()
    call lib%final ()
    call model_hadrons%final ()
    deallocate (model_hadrons)
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: shower_1"

  end subroutine shower_1

  subroutine shower_2 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(process_library_t), target :: lib
    type(model_list_t) :: model_list
    type(model_t), pointer :: model_hadrons
    class(model_data_t), pointer :: model
    type(process_t), target :: process
    type(process_instance_t), target :: process_instance
    integer :: factorization_mode
    logical :: keep_correlations
    type(pdf_data_t) :: pdf_data
    class(evt_t), allocatable, target :: evt_trivial
    class(evt_t), allocatable, target :: evt_shower
    type(shower_settings_t) :: settings

    write (u, "(A)")  "* Test output: shower_2"
    write (u, "(A)")  "*   Purpose: Two-jet event with FSR shower"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize environment"
    write (u, "(A)")

    call syntax_model_file_init ()
    call os_data_init (os_data)
    call model_list%read_model &
         (var_str ("SM_hadrons"), var_str ("SM_hadrons.mdl"), &
         os_data, model_hadrons)
    call setup_testbed (var_str ("shower_2"), &
         os_data, lib, model_list, process, process_instance)
    model => process%get_model_ptr ()

    write (u, "(A)")  "* Set up trivial transform"
    write (u, "(A)")

    allocate (evt_trivial_t :: evt_trivial)
    call evt_trivial%connect (process_instance, model)
    call evt_trivial%prepare_new_event (1, 1)
    call evt_trivial%generate_unweighted ()

    factorization_mode = FM_IGNORE_HELICITY
    keep_correlations = .false.
    call evt_trivial%make_particle_set (factorization_mode, keep_correlations)

    select type (evt_trivial)
    type is (evt_trivial_t)
       call evt_trivial%write (u)
       call write_separator (u, 2)
    end select

    write (u, "(A)")
    write (u, "(A)")  "* Set up shower event transform"
    write (u, "(A)")

    settings%fsr_active = .true.

    allocate (evt_shower_t :: evt_shower)
    select type (evt_shower)
    type is (evt_shower_t)
       call evt_shower%init (model_hadrons, os_data)
       allocate (shower_t :: evt_shower%shower)
       call evt_shower%shower%init (settings, pdf_data)
       call evt_shower%connect (process_instance, model)
    end select

    evt_trivial%next => evt_shower
    evt_shower%previous => evt_trivial

    call evt_shower%prepare_new_event (1, 1)
    call evt_shower%generate_unweighted ()
    call evt_shower%make_particle_set (factorization_mode, keep_correlations)

    select type (evt_shower)
    type is (evt_shower_t)
       call evt_shower%write (u, testflag = .true.)
       call write_separator (u, 2)
    end select

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call evt_shower%final ()
    call evt_trivial%final ()
    call process_instance%final ()
    call process%final ()
    call lib%final ()
    call model_hadrons%final ()
    deallocate (model_hadrons)
    call syntax_model_file_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: shower_2"

  end subroutine shower_2


end module shower_uti

