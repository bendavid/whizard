! WHIZARD 2.3.1 Aug 25 2016
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

module muli_momentum
  
  use kinds, only: default
  use constants
  use muli_base
  
  implicit none
  private
  
  public :: transverse_mom_t
  public :: qcd_2_2_class

  type, extends (ser_class_t) :: transverse_mom_t
     private
     real(default), dimension(0:4) :: momentum = [0, 0, 0, 0, 0]
   contains
     procedure :: mom_write_to_marker => transverse_mom_write_to_marker
     procedure :: write_to_marker => transverse_mom_write_to_marker
     procedure :: mom_read_from_marker => transverse_mom_read_from_marker
     procedure :: read_from_marker => transverse_mom_read_from_marker
     procedure :: mom_print_to_unit => transverse_mom_print_to_unit
     procedure :: print_to_unit => transverse_mom_print_to_unit
     procedure, nopass :: get_type => transverse_mom_get_type    
     procedure :: get_gev_initial_cme => transverse_mom_get_gev_initial_cme
     procedure :: get_gev_max_scale => transverse_mom_get_gev_max_scale
     procedure :: get_gev2_max_scale => transverse_mom_get_gev2_max_scale
     procedure :: get_gev_scale => transverse_mom_get_gev_scale
     procedure :: get_gev2_scale => transverse_mom_get_gev2_scale
     procedure :: get_unit_scale => transverse_mom_get_unit_scale
     procedure :: get_unit2_scale => transverse_mom_get_unit2_scale
     procedure :: set_gev_initial_cme => transverse_mom_set_gev_initial_cme
     procedure :: set_gev_max_scale => transverse_mom_set_gev_max_scale
     procedure :: set_gev2_max_scale => transverse_mom_set_gev2_max_scale
     procedure :: set_gev_scale => transverse_mom_set_gev_scale
     procedure :: set_gev2_scale => transverse_mom_set_gev2_scale
     procedure :: set_unit_scale => transverse_mom_set_unit_scale
     procedure :: set_unit2_scale => transverse_mom_set_unit2_scale
     generic :: initialize => transverse_mom_initialize
     procedure :: transverse_mom_initialize  
  end type transverse_mom_t
  
  type, extends (transverse_mom_t), abstract :: qcd_2_2_class
   contains
     procedure(qcd_get_int), deferred :: get_process_id
     procedure(qcd_get_int), deferred :: get_integrand_id
     procedure(qcd_get_int), deferred :: get_diagram_kind
     procedure(qcd_get_int_4), deferred :: get_lha_flavors
     procedure(qcd_get_int_4), deferred :: get_pdg_flavors
     procedure(qcd_get_int_by_int), deferred :: get_parton_id
     procedure(qcd_get_int_2), deferred :: get_parton_kinds
     procedure(qcd_get_int_2), deferred :: get_pdf_int_kinds
     procedure(qcd_get_real), deferred :: get_momentum_boost
     ! procedure(qcd_get_real_3),deferred :: get_parton_in_momenta
     procedure(qcd_get_real_2), deferred :: get_remnant_momentum_fractions
     procedure(qcd_get_real_2), deferred :: get_total_momentum_fractions    
  end type qcd_2_2_class


  abstract interface
     subroutine qcd_none (this)
       import qcd_2_2_class
       class(qcd_2_2_class), target, intent(in) :: this
     end subroutine qcd_none
  end interface
  ! abstract interface
  !    subroutine qcd_get_beam (this, beam)
  !      import qcd_2_2_class
  !      import pp_remnant_class
  !      class(qcd_2_2_class),target, intent(in) :: this
  !      class(pp_remnant_class),pointer, intent(out) :: beam
  !     end subroutine qcd_get_beam
  ! end interface
  abstract interface
     elemental function qcd_get_real (this)
       import 
       class(qcd_2_2_class), intent(in) :: this
       real(default) :: qcd_get_real
     end function qcd_get_real
  end interface
  abstract interface
     pure function qcd_get_real_2 (this)
       import 
       class(qcd_2_2_class), intent(in) :: this
       real(default), dimension(2) :: qcd_get_real_2
     end function qcd_get_real_2
  end interface
  abstract interface
     pure function qcd_get_real_3 (this)
       import 
       class(qcd_2_2_class), intent(in) :: this
       real(default), dimension(3) :: qcd_get_real_3
     end function qcd_get_real_3
  end interface
  abstract interface
     elemental function qcd_get_int (this)
       import 
       class(qcd_2_2_class), intent(in) :: this
       integer :: qcd_get_int
     end function qcd_get_int
  end interface
  abstract interface
     pure function qcd_get_int_by_int (this, n)
       import 
       class(qcd_2_2_class), intent(in) :: this
       integer, intent(in) :: n
       integer :: qcd_get_int_by_int
     end function qcd_get_int_by_int
  end interface
  abstract interface
     pure function qcd_get_int_2 (this)
       import 
       class(qcd_2_2_class), intent(in) :: this
       integer, dimension(2) :: qcd_get_int_2
     end function qcd_get_int_2
  end interface
  abstract interface
     pure function qcd_get_int_4 (this)
       import 
       class(qcd_2_2_class), intent(in) :: this
       integer, dimension(4) :: qcd_get_int_4
     end function qcd_get_int_4
  end interface


contains

  subroutine transverse_mom_write_to_marker (this, marker, status)
    class(transverse_mom_t), intent(in) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    call marker%mark_begin ("transverse_mom_t")
    call marker%mark ("gev_momenta", this%momentum(0:1))
    call marker%mark_end ("transverse_mom_t")
  end subroutine transverse_mom_write_to_marker

  subroutine transverse_mom_read_from_marker (this, marker, status)
    class(transverse_mom_t), intent(out) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    call marker%pick_begin ("transverse_mom_t", status=status)
    call marker%pick ("gev_momenta", this%momentum(0:1), status)
    this%momentum(2:4) = [ this%momentum(1)**2, &
                           this%momentum(1) / this%momentum(0), &
                           (this%momentum(1)/this%momentum(0))**2 ]
    call marker%pick_end ("transverse_mom_t", status=status)
  end subroutine transverse_mom_read_from_marker

  subroutine transverse_mom_print_to_unit &
       (this, unit, parents, components, peers)
    class(transverse_mom_t), intent(in) :: this
    integer, intent(in) :: unit
    integer(dik), intent(in) :: parents, components, peers
    write (unit, "(1x,A)")  "Components of transverse_mom_t:"
    write (unit, "(3x,A)")  "Actual energy scale:"
    write (unit, "(A,E20.10)")  "Max scale (MeV)   :", this%momentum(0)
    write (unit, "(A,E20.10)")  "Scale (MeV)       :", this%momentum(1)
    write (unit, "(A,E20.10)")  "Scale^2 (MeV^2)   :", this%momentum(2)
    write (unit, "(A,E20.10)")  "Scale normalized  :", this%momentum(3)
    write (unit, "(A,E20.10)")  "Scale^2 normalized:", this%momentum(4)
  end subroutine transverse_mom_print_to_unit
    
  pure subroutine transverse_mom_get_type (type)
    character(:), allocatable, intent(out) :: type
    allocate (type, source="transverse_mom_t")
  end subroutine transverse_mom_get_type
  
  elemental function transverse_mom_get_gev_initial_cme (this) result(scale)
    class(transverse_mom_t), intent(in) :: this
    real(default) :: scale
    scale = this%momentum(0) * 2D0
  end function transverse_mom_get_gev_initial_cme

  elemental function transverse_mom_get_gev_max_scale (this) result (scale)
    class(transverse_mom_t), intent(in) :: this
    real(default) :: scale
    scale = this%momentum(0)
  end function transverse_mom_get_gev_max_scale

  elemental function transverse_mom_get_gev2_max_scale (this) result (scale)
    class(transverse_mom_t), intent(in) :: this
    real(default) :: scale
    scale = this%momentum(0)**2
  end function transverse_mom_get_gev2_max_scale

  elemental function transverse_mom_get_gev_scale(this) result(scale)
    class(transverse_mom_t), intent(in) :: this
    real(default) :: scale
    scale = this%momentum(1)
  end function transverse_mom_get_gev_scale

  elemental function transverse_mom_get_gev2_scale (this) result (scale)
    class(transverse_mom_t), intent(in) :: this
    real(default) :: scale
    scale = this%momentum(2)
  end function transverse_mom_get_gev2_scale

  pure function transverse_mom_get_unit_scale (this) result (scale)
    class(transverse_mom_t), intent(in) :: this
    real(default) :: scale
    scale = this%momentum(3)
  end function transverse_mom_get_unit_scale

  pure function transverse_mom_get_unit2_scale (this) result (scale)
    class(transverse_mom_t), intent(in) :: this
    real(default) :: scale
    scale = this%momentum(4)
  end function transverse_mom_get_unit2_scale

  subroutine transverse_mom_set_gev_initial_cme (this, new_gev_initial_cme)
    class(transverse_mom_t), intent(inout) :: this
    real(default), intent(in)  ::  new_gev_initial_cme
    this%momentum(0) = new_gev_initial_cme / 2D0
    this%momentum(3) = this%momentum(1) / this%momentum(0)
    this%momentum(4) = this%momentum(3)**2
  end subroutine transverse_mom_set_gev_initial_cme

  subroutine transverse_mom_set_gev_max_scale (this, new_gev_max_scale)
    class(transverse_mom_t), intent(inout) :: this
    real(default), intent(in)  ::  new_gev_max_scale
    this%momentum(0) = new_gev_max_scale
    this%momentum(3) = this%momentum(1) / this%momentum(0)
    this%momentum(4) = this%momentum(3)**2
  end subroutine transverse_mom_set_gev_max_scale
  
  subroutine transverse_mom_set_gev2_max_scale (this, new_gev2_max_scale)
    class(transverse_mom_t), intent(inout) :: this
    real(default), intent(in)  ::  new_gev2_max_scale
    this%momentum(0) = sqrt (new_gev2_max_scale)
    this%momentum(3) = this%momentum(1) / this%momentum(0)
    this%momentum(4) = this%momentum(3)**2
  end subroutine transverse_mom_set_gev2_max_scale

  subroutine transverse_mom_set_gev_scale (this, new_gev_scale)
    class(transverse_mom_t), intent(inout) :: this
    real(default), intent(in)  ::  new_gev_scale
    this%momentum(1) = new_gev_scale
    this%momentum(2) = new_gev_scale**2
    this%momentum(3) = new_gev_scale / this%momentum(0)
    this%momentum(4) = this%momentum(3)**2
  end subroutine transverse_mom_set_gev_scale

  subroutine transverse_mom_set_gev2_scale (this, new_gev2_scale)
    class(transverse_mom_t), intent(inout) :: this
    real(default), intent(in) :: new_gev2_scale
    this%momentum(1) = sqrt (new_gev2_scale)
    this%momentum(2) = new_gev2_scale
    this%momentum(3) = this%momentum(1) / this%momentum(0)
    this%momentum(4) = this%momentum(3)**2
  end subroutine transverse_mom_set_gev2_scale

  subroutine transverse_mom_set_unit_scale (this, new_unit_scale)
    class(transverse_mom_t), intent(inout)::this
    real(default), intent(in) :: new_unit_scale
    this%momentum(1) = new_unit_scale * this%momentum(0)
    this%momentum(2) = this%momentum(1)**2
    this%momentum(3) = new_unit_scale
    this%momentum(4) = this%momentum(3)**2
  end subroutine transverse_mom_set_unit_scale

  subroutine transverse_mom_set_unit2_scale (this, new_unit2_scale)
    class(transverse_mom_t), intent(inout)::this
    real(default), intent(in) :: new_unit2_scale
    this%momentum(3) = sqrt (new_unit2_scale)
    this%momentum(4) = new_unit2_scale
    this%momentum(1) = this%momentum(3) * this%momentum(0)
    this%momentum(2) = this%momentum(1)**2
  end subroutine transverse_mom_set_unit2_scale

  subroutine transverse_mom_initialize (this, gev2_s)
    class(transverse_mom_t), intent(out) :: this
    real(default), intent(in) :: gev2_s
    real(default) :: gev_s
    gev_s = sqrt (gev2_s)
    this%momentum = [gev_s/2, gev_s/2, gev2_s/4, one, one]
  end subroutine transverse_mom_initialize
    
    
end module muli_momentum

