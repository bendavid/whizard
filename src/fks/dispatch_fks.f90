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

module dispatch_fks

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use string_utils, only: split_string
  use variables, only: var_list_t
  use nlo_data, only: fks_template_t, FKS_DEFAULT, FKS_RESONANCES

  implicit none
  private

  public :: dispatch_fks_s

contains

  subroutine dispatch_fks_s (fks_template, var_list)
    type(fks_template_t), intent(inout) :: fks_template
    type(var_list_t), intent(in) :: var_list
    real(default) :: fks_dij_exp1, fks_dij_exp2
    type(string_t) :: fks_mapping_type
    logical :: subtraction_disabled
    type(string_t) :: exclude_from_resonance
    fks_dij_exp1 = &
       var_list%get_rval (var_str ("fks_dij_exp1"))
    fks_dij_exp2 = &
       var_list%get_rval (var_str ("fks_dij_exp2"))
    fks_mapping_type = &
       var_list%get_sval (var_str ("$fks_mapping_type"))
    subtraction_disabled = &
       var_list%get_lval (var_str ("?disable_subtraction"))
    exclude_from_resonance = &
       var_list%get_sval (var_str ("$resonances_exclude_particles"))
    if (exclude_from_resonance /= var_str ("default")) &
       call split_string (exclude_from_resonance, var_str (":"), &
       fks_template%excluded_resonances)
    call fks_template%set_dij_exp (fks_dij_exp1, fks_dij_exp2)
    call fks_template%set_xi_and_y_bounds &
         (var_list%get_rval (var_str ("fks_xi_min")), &
         var_list%get_rval (var_str ("fks_y_max")))
    select case (char (fks_mapping_type))
    case ("default")
       call fks_template%set_mapping_type (FKS_DEFAULT)
    case ("resonances")
       call fks_template%set_mapping_type (FKS_RESONANCES)
    end select
    fks_template%subtraction_disabled = subtraction_disabled
  end subroutine dispatch_fks_s


end module dispatch_fks
