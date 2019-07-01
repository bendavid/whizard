! WHIZARD 2.2.8 Nov 22 2015
! 
! Copyright (C) 1999-2015 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Soyoung Shim <soyoung.shim@desy.de>
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

module powheg_matching_uti

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use constants, only: zero, one
  use lorentz
  use physics_defs, only: LAMBDA_QCD_REF
  use sm_qcd
  use subevents, only: PRT_INCOMING, PRT_OUTGOING
  use model_data
  use particles
  use rng_base
  use variables
  use processes
  use shower_base
  use shower_core

  use powheg_matching

  use rng_base_ut, only: rng_test_factory_t

  implicit none
  private

  public :: powheg_1

contains

  subroutine powheg_1 (u)
    integer, intent(in) :: u
    type(powheg_matching_t) :: powheg
    type(powheg_settings_t) :: powheg_settings
    type(powheg_testing_t) :: powheg_testing
    type(process_instance_t), target :: process_instance
    class(shower_base_t), allocatable, target :: shower
    type(model_data_t), target :: model
    type(particle_set_t) :: particle_set
    class(rng_factory_t), allocatable :: rng_factory
    class(rng_t), allocatable :: rng
    type(string_t) :: process_name
    type(vector4_t), dimension(4) :: born_momenta
    type(qcd_t), target :: qcd
    type(var_list_t) :: var_list

    allocate (shower_t :: shower)
    allocate (rng_test_factory_t :: rng_factory)
    call rng_factory%make (rng)
    allocate (alpha_qcd_from_lambda_t :: qcd%alpha)
    select type (alpha => qcd%alpha)
    type is (alpha_qcd_from_lambda_t)
       alpha%order = 2
    end select
    process_name = "test_powheg_1"
    powheg_settings%n_init = 1000
    powheg_settings%size_grid_xi = 2
    powheg_settings%size_grid_y = 2
    powheg_settings%pt2_min = one
    powheg_settings%lambda = LAMBDA_QCD_REF
    powheg_testing%n_alr = 3
    powheg_testing%n_in = 2
    powheg_testing%n_out_born = 2
    powheg_testing%n_out_real = 3
    powheg_testing%sqme_born = one
    powheg_testing%active = .true.
    born_momenta(1) = [50._default, zero, zero, 50._default]
    born_momenta(2) = [50._default, zero, zero, - 50._default]
    born_momenta(3) = [50._default, zero, zero, 50._default]
    born_momenta(4) = [50._default, zero, zero, - 50._default]
    particle_set%n_tot = 4
    particle_set%n_in = 2
    particle_set%n_out = 2
    call particle_set%set_momenta (born_momenta)
    call particle_set%prt(1)%set_status (PRT_INCOMING)
    call particle_set%prt(2)%set_status (PRT_INCOMING)
    call particle_set%prt(3)%set_status (PRT_OUTGOING)
    call particle_set%prt(4)%set_status (PRT_OUTGOING)

    write (u, "(A)")  "* Test output: powheg_1"
    write (u, "(A)")  "*   Purpose: Initialization"
    write (u, "(A)")

    call powheg%init (var_list, process_name)
    powheg%testing = powheg_testing
    powheg%settings = powheg_settings
    powheg%qcd => qcd

    allocate (pcm_instance_nlo_t :: process_instance%pcm)
    
    call powheg%import_rng (rng)
    call powheg%connect (process_instance, model, shower)
    call powheg%prepare_for_events ()
    call powheg%update (particle_set)
    ! TODO: (bcn 2015-05-04) put this write somewhere useful
    call powheg%grid%compute_and_write_mean_and_max (u)
    !!! Needs some more thought: if we just set R = 1, B = 1 the grid
    !!! setup will fail
    !!! call powheg%generate_emission (particle_set)
    call powheg%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: powheg_1"
  end subroutine powheg_1


end module powheg_matching_uti
