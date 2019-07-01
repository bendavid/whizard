! WHIZARD 2.2.4 Feb 06 2015
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

module blha_driver

  use iso_varying_string, string_t => varying_string
  use io_units
  use constants
  use diagnostics
  use sm_physics
  use md5
  use lorentz
  use model_data
  use flavors
  use quantum_numbers
  use interactions
  use evaluators
  use particles
  use quantum_numbers
  use blha_config
  use blha_interface

  implicit none
  private

  public :: blha_test

contains

  subroutine blha_test (model)
    class(model_data_t), pointer :: model
    type (blha_configuration_t) :: cfg
    logical :: ok
    call blha_config_test (model, cfg, ok)
    if (ok) call blha_interface_test (cfg, ok)
  end subroutine blha_test


end module blha_driver
