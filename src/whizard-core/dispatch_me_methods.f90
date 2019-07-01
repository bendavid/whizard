! WHIZARD 2.5.0 May 06 2017
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

module dispatch_me_methods

  use iso_varying_string, string_t => varying_string
  use physics_defs, only: BORN
  use diagnostics
  use sm_qcd
  use variables, only: var_list_t
  use models
  use model_data

  use prc_core_def
  use prc_core
  use prc_test_core
  use prc_template_me
  use prc_test
  use prc_omega
  use prc_user_defined
  use prc_gosam
  use prc_openloops
  use prc_recola
  use prc_threshold

  implicit none
  private

  public :: dispatch_core_def
  public :: dispatch_core
  public :: dispatch_core_update
  public :: dispatch_core_restore

contains

  subroutine dispatch_core_def (core_def, prt_in, prt_out, &
       model, var_list, id, nlo_type)
    class(prc_core_def_t), allocatable, intent(inout) :: core_def
    type(string_t), dimension(:), intent(in) :: prt_in
    type(string_t), dimension(:), intent(in) :: prt_out
    type(model_t), pointer, intent(in) :: model
    type(var_list_t), intent(in) :: var_list
    type(string_t), intent(in), optional :: id
    integer, intent(in), optional :: nlo_type
    type(string_t) :: method
    type(string_t) :: model_name
    type(string_t) :: ufo_path
    type(string_t) :: restrictions
    logical :: ufo
    logical :: cms_scheme
    logical :: openmp_support
    logical :: report_progress
    logical :: diags, diags_color
    type(string_t) :: extra_options
    integer :: nlo
    nlo = BORN;  if (present (nlo_type))  nlo = nlo_type
    method = var_list%get_sval (var_str ("$method"))
    if (associated (model)) then
       model_name = model%get_name ()
       cms_scheme = model%get_scheme () == "Complex_Mass_Scheme"
       ufo = model%is_ufo_model ()
       ufo_path = model%get_ufo_path ()
    else
       model_name = ""
       cms_scheme = .false.
       ufo = .false.
    end if
    restrictions = var_list%get_sval (&
         var_str ("$restrictions"))
    diags = var_list%get_lval (&
         var_str ("?vis_diags"))
    diags_color = var_list%get_lval (&
         var_str ("?vis_diags_color"))
    openmp_support = var_list%get_lval (&
         var_str ("?omega_openmp"))
    report_progress = var_list%get_lval (&
         var_str ("?report_progress"))
    extra_options = var_list%get_sval (&
         var_str ("$omega_flags"))
    select case (char (method))
    case ("unit_test")
       allocate (prc_test_def_t :: core_def)
       select type (core_def)
       type is (prc_test_def_t)
          call core_def%init (model_name, prt_in, prt_out)
       end select
    case ("template")
       allocate (template_me_def_t :: core_def)
       select type (core_def)
       type is (template_me_def_t)
          call core_def%init (model, prt_in, prt_out, unity = .false.)
       end select
    case ("template_unity")
       allocate (template_me_def_t :: core_def)
       select type (core_def)
       type is (template_me_def_t)
          call core_def%init (model, prt_in, prt_out, unity = .true.)
       end select
    case ("omega")
       allocate (omega_def_t :: core_def)
       select type (core_def)
       type is (omega_def_t)
          call core_def%init (model_name, prt_in, prt_out, &
               .false., ufo, ufo_path, &
               restrictions, cms_scheme, &
               openmp_support, report_progress, extra_options, diags, diags_color)
       end select
    case ("ovm")
       allocate (omega_def_t :: core_def)
       select type (core_def)
       type is (omega_def_t)
          call core_def%init (model_name, prt_in, prt_out, &
               .true., .false., var_str (""), &
               restrictions, cms_scheme, &
               openmp_support, report_progress, extra_options, diags, diags_color)
       end select
    case ("gosam")
      allocate (gosam_def_t :: core_def)
      select type (core_def)
      type is (gosam_def_t)
        if (present (id)) then
           call core_def%init (id, model_name, prt_in, &
              prt_out, nlo, var_list)
        else
           call msg_fatal ("Dispatch GoSam def: No id!")
        end if
      end select
    case ("openloops")
       allocate (openloops_def_t :: core_def)
       select type (core_def)
       type is (openloops_def_t)
          if (present (id)) then
             call core_def%init (id, model_name, prt_in, &
                prt_out, nlo, var_list)
          else
             call msg_fatal ("Dispatch OpenLoops def: No id!")
          end if
       end select
    case ("recola")
       call abort_if_recola_not_active ()
       allocate (recola_def_t :: core_def)
       select type (core_def)
       type is (recola_def_t)
          if (present (id)) then
             call core_def%init (id, model_name, prt_in, &
                prt_out, nlo)
          else
             call msg_fatal ("Dispatch RECOLA def: No id!")
          end if
       end select
    case ("dummy")
       allocate (user_defined_test_def_t :: core_def)
       select type (core_def)
       type is (user_defined_test_def_t)
          if (present (id)) then
             call core_def%init (id, model_name, prt_in, prt_out)
          else
             call msg_fatal ("Dispatch User-Defined Test def: No id!")
          end if
       end select
    case ("threshold")
       allocate (threshold_def_t :: core_def)
       select type (core_def)
       type is (threshold_def_t)
          if (present (id)) then
             call core_def%init (id, model_name, prt_in, prt_out, &
                  nlo, restrictions)
          else
             call msg_fatal ("Dispatch Threshold def: No id!")
          end if
       end select
    case default
       call msg_fatal ("Process configuration: method '" &
            // char (method) // "' not implemented")
    end select
  end subroutine dispatch_core_def

  subroutine dispatch_core (core, core_def, model, &
       helicity_selection, qcd, use_color_factors)
    class(prc_core_t), allocatable, intent(inout) :: core
    class(prc_core_def_t), intent(in) :: core_def
    class(model_data_t), intent(in), target, optional :: model
    type(helicity_selection_t), intent(in), optional :: helicity_selection
    type(qcd_t), intent(in), optional :: qcd
    logical, intent(in), optional :: use_color_factors
    select type (core_def)
    type is (prc_test_def_t)
       allocate (test_t :: core)
    type is (template_me_def_t)
       allocate (prc_template_me_t :: core)
       select type (core)
       type is (prc_template_me_t)
          call core%set_parameters (model)
       end select
    class is (omega_def_t)
       if (.not. allocated (core)) allocate (prc_omega_t :: core)
       select type (core)
       type is (prc_omega_t)
          call core%set_parameters (model, &
               helicity_selection, qcd, use_color_factors)
       end select
    type is (gosam_def_t)
      if (.not. allocated (core)) allocate (prc_gosam_t :: core)
      select type (core)
      type is (prc_gosam_t)
        call core%set_parameters (qcd)
      end select
    type is (openloops_def_t)
      if (.not. allocated (core)) allocate (prc_openloops_t :: core)
      select type (core)
      type is (prc_openloops_t)
         call core%set_parameters (qcd)
      end select
    type is (recola_def_t)
      if (.not. allocated (core)) allocate (prc_recola_t :: core)
      select type (core)
      type is (prc_recola_t)
         call core%set_parameters (qcd, model)
      end select
    type is (user_defined_test_def_t)
      if (.not. allocated (core)) allocate (prc_user_defined_test_t :: core)
      select type (core)
      type is (prc_user_defined_test_t)
         call core%set_parameters (qcd, model)
      end select
    type is (threshold_def_t)
      if (.not. allocated (core)) allocate (prc_threshold_t :: core)
      select type (core)
      type is (prc_threshold_t)
         call core%set_parameters (qcd, model)
      end select
    class default
       call msg_bug ("Process core: unexpected process definition type")
    end select
  end subroutine dispatch_core

  subroutine dispatch_core_update &
       (core, model, helicity_selection, qcd, saved_core)

    class(prc_core_t), allocatable, intent(inout) :: core
    class(model_data_t), intent(in), optional, target :: model
    type(helicity_selection_t), intent(in), optional :: helicity_selection
    type(qcd_t), intent(in), optional :: qcd
    class(prc_core_t), allocatable, intent(inout), optional :: saved_core

    if (present (saved_core)) then
       allocate (saved_core, source = core)
    end if
    select type (core)
    type is (test_t)
    type is (prc_omega_t)
       call core%set_parameters (model, helicity_selection, qcd)
       call core%activate_parameters ()
    class is (prc_user_defined_base_t)
      call msg_message ("Updating user defined cores is not implemented yet.")
    class default
       call msg_bug ("Process core update: unexpected process definition type")
    end select
  end subroutine dispatch_core_update

  subroutine dispatch_core_restore (core, saved_core)

    class(prc_core_t), allocatable, intent(inout) :: core
    class(prc_core_t), allocatable, intent(inout) :: saved_core

    call move_alloc (from = saved_core, to = core)
    select type (core)
    type is (test_t)
    type is (prc_omega_t)
       call core%activate_parameters ()
    class default
       call msg_bug ("Process core restore: unexpected process definition type")
    end select
  end subroutine dispatch_core_restore


end module dispatch_me_methods
