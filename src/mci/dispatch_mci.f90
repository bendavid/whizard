! WHIZARD 2.4.0 Nov 28 2016
! 
! Copyright (C) 1999-2016 by 
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

module dispatch_mci

  use iso_varying_string, string_t => varying_string
  use diagnostics
  use variables

  use mci_base
  use mci_midpoint
  use mci_vamp

  implicit none
  private

  public :: dispatch_mci_s

contains

  subroutine dispatch_mci_s (mci, var_list, process_id, is_nlo)
    class(mci_t), allocatable, intent(inout) :: mci
    logical, intent(in), optional :: is_nlo
    type(var_list_t), intent(in) :: var_list
    type(string_t), intent(in) :: process_id
    type(string_t) :: run_id
    type(string_t) :: integration_method
    type(grid_parameters_t) :: grid_par
    type(history_parameters_t) :: history_par
    logical :: rebuild_grids, check_grid_file, negative_weights, verbose
    logical :: dispatch_nlo = .false.
    if (present (is_nlo)) dispatch_nlo = is_nlo
    integration_method = &
         var_list%get_sval (var_str ("$integration_method"))
    select case (char (integration_method))
    case ("midpoint")
       allocate (mci_midpoint_t :: mci)
    case ("vamp", "default")
       call unpack_options ()
       allocate (mci_vamp_t :: mci)
       select type (mci)
       type is (mci_vamp_t)
          call mci%set_grid_parameters (grid_par)
          if (run_id /= "") then
             call mci%set_grid_filename (process_id, run_id)
          else
             call mci%set_grid_filename (process_id)
          end if
          call mci%set_history_parameters (history_par)
          call mci%set_rebuild_flag (rebuild_grids, check_grid_file)
          mci%negative_weights = negative_weights
          mci%verbose = verbose
       end select
    case default
       call msg_fatal ("Integrator '" &
            // char (integration_method) // "' not implemented")
    end select
  contains
      subroutine unpack_options()
        grid_par%threshold_calls = &
             var_list%get_ival (var_str ("threshold_calls"))
        grid_par%min_calls_per_channel = &
             var_list%get_ival (var_str ("min_calls_per_channel"))
        grid_par%min_calls_per_bin = &
             var_list%get_ival (var_str ("min_calls_per_bin"))
        grid_par%min_bins = &
             var_list%get_ival (var_str ("min_bins"))
        grid_par%max_bins = &
             var_list%get_ival (var_str ("max_bins"))
        grid_par%stratified = &
             var_list%get_lval (var_str ("?stratified"))
        if (.not. dispatch_nlo) then
           grid_par%use_vamp_equivalences = &
                var_list%get_lval (var_str ("?use_vamp_equivalences"))
        else
           grid_par%use_vamp_equivalences = .false.
        end if
        grid_par%channel_weights_power = &
             var_list%get_rval (var_str ("channel_weights_power"))
        grid_par%accuracy_goal = &
             var_list%get_rval (var_str ("accuracy_goal"))
        grid_par%error_goal = &
             var_list%get_rval (var_str ("error_goal"))
        grid_par%rel_error_goal = &
             var_list%get_rval (var_str ("relative_error_goal"))
        history_par%global = &
             var_list%get_lval (var_str ("?vamp_history_global"))
        history_par%global_verbose = &
             var_list%get_lval (var_str ("?vamp_history_global_verbose"))
        history_par%channel = &
             var_list%get_lval (var_str ("?vamp_history_channels"))
        history_par%channel_verbose = &
             var_list%get_lval (var_str ("?vamp_history_channels_verbose"))
        verbose = &
             var_list%get_lval (var_str ("?vamp_verbose"))
        check_grid_file = &
             var_list%get_lval (var_str ("?check_grid_file"))
        run_id = &
             var_list%get_sval (var_str ("$run_id"))
        rebuild_grids = &
             var_list%get_lval (var_str ("?rebuild_grids"))
        negative_weights = &
             var_list%get_lval (var_str ("?negative_weights")) .or. dispatch_nlo
      end subroutine unpack_options

  end subroutine dispatch_mci_s
  

end module dispatch_mci
