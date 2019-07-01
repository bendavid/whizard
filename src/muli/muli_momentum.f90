!!! module: muli_momentum
!!! This code is part of my Ph.D studies.
!!! 
!!! Copyright (C) 2011 Hans-Werner Boschmann <boschmann@tp1.physik.uni-siegen.de>
!!! 
!!! This program is free software; you can redistribute it and/or modify it
!!! under the terms of the GNU General Public License as published by the Free 
!!! Software Foundation; either version 3 of the License, or (at your option) 
!!! any later version.
!!! 
!!! This program is distributed in the hope that it will be useful, but WITHOUT
!!! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
!!! FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
!!! more details.
!!! 
!!! You should have received a copy of the GNU General Public License along
!!! with this program; if not, see <http://www.gnu.org/licenses/>.
!!! 
!!! Latest Change: 2011-06-09 13:26:25 CEST(+0200)
!!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!! This file contains the module "muli_momentum". It's purpose is to store the
!!! actual value of the evolution parameter pt^2 in a convenient way. I use the
!!! normalized value pt^2/ptmax^2 for generating the next value of the scale,
!!! also need the square root of both pt^2 and pt^2/ptmax^2 for other purposes.
!!! That's why I store all four combinations together with ptmax in an array.

module muli_momentum
  use muli_basic
  implicit none
  type,extends(serializable_class)::transversal_momentum_type
     private
     real(kind=drk),dimension(0:4)::momentum=[0D0,0D0,0D0,0D0,0D0]
   contains
     ! overridden serializable_class procedures
     procedure,public::write_to_marker=>transversal_momentum_write_to_marker
     procedure,public::read_from_marker=>transversal_momentum_read_from_marker
     procedure,public::print_to_unit=>transversal_momentum_print_to_unit
     procedure,public,nopass::get_type=>transversal_momentum_get_type
     ! new type-bound-procedures
     procedure,public::get_gev_initial_cme=>transversal_momentum_get_gev_initial_cme
     procedure,public::get_gev_max_scale=>transversal_momentum_get_gev_max_scale
     procedure,public::get_gev2_max_scale=>transversal_momentum_get_gev2_max_scale
     procedure,public::get_gev_scale=>transversal_momentum_get_gev_scale
     procedure,public::get_gev2_scale=>transversal_momentum_get_gev2_scale
     procedure,public::get_unit_scale=>transversal_momentum_get_unit_scale
     procedure,public::get_unit2_scale=>transversal_momentum_get_unit2_scale
     procedure,public::set_gev_initial_cme=>transversal_momentum_set_gev_initial_cme
     procedure,public::set_gev_max_scale=>transversal_momentum_set_gev_max_scale
     procedure,public::set_gev2_max_scale=>transversal_momentum_set_gev2_max_scale
     procedure,public::set_gev_scale=>transversal_momentum_set_gev_scale
     procedure,public::set_gev2_scale=>transversal_momentum_set_gev2_scale
     procedure,public::set_unit_scale=>transversal_momentum_set_unit_scale
     procedure,public::set_unit2_scale=>transversal_momentum_set_unit2_scale
     procedure,public::transversal_momentum_initialize
     generic,public::initialize=>transversal_momentum_initialize
  end type transversal_momentum_type

  type,extends(transversal_momentum_type),abstract::qcd_2_2_class
   contains
     procedure(qcd_get_int),deferred::get_process_id
     procedure(qcd_get_int),deferred::get_integrand_id
     procedure(qcd_get_int),deferred::get_diagram_kind
     procedure(qcd_get_int_4),deferred::get_lha_flavors
     procedure(qcd_get_int_4),deferred::get_pdg_flavors
     procedure(qcd_get_int_by_int),deferred::get_parton_id
     procedure(qcd_get_int_2),deferred::get_parton_kinds
     procedure(qcd_get_int_2),deferred::get_pdf_int_kinds
     procedure(qcd_get_drk),deferred::get_momentum_boost
!     procedure(qcd_get_drk_3),deferred::get_parton_in_momenta
     procedure(qcd_get_drk_2),deferred::get_remnant_momentum_fractions
     procedure(qcd_get_drk_2),deferred::get_total_momentum_fractions
  end type qcd_2_2_class

    abstract interface
     subroutine qcd_none(this)
       import qcd_2_2_class
       class(qcd_2_2_class),target,intent(in)::this
     end subroutine qcd_none
!!$     subroutine qcd_get_beam(this,beam)
!!$       import qcd_2_2_class
!!$       import pp_remnant_class
!!$       class(qcd_2_2_class),target,intent(in)::this
!!$       class(pp_remnant_class),pointer,intent(out)::beam
!!$     end subroutine qcd_get_beam
     elemental function qcd_get_drk(this)
       use muli_basic, only: drk
       import qcd_2_2_class
       class(qcd_2_2_class),intent(in)::this
       real(kind=drk)::qcd_get_drk
     end function qcd_get_drk
     pure function qcd_get_drk_2(this)
       use muli_basic, only: drk
       import qcd_2_2_class
       class(qcd_2_2_class),intent(in)::this
       real(kind=drk),dimension(2)::qcd_get_drk_2
     end function qcd_get_drk_2
     pure function qcd_get_drk_3(this)
       use muli_basic, only: drk
       import qcd_2_2_class
       class(qcd_2_2_class),intent(in)::this
       real(kind=drk),dimension(3)::qcd_get_drk_3
     end function qcd_get_drk_3
     elemental function qcd_get_int(this)
       use muli_basic, only: drk
       import qcd_2_2_class
       class(qcd_2_2_class),intent(in)::this
       integer::qcd_get_int
     end function qcd_get_int
     elemental function qcd_get_int_by_int(this,n)
       use muli_basic, only: drk
       import qcd_2_2_class
       class(qcd_2_2_class),intent(in)::this
       integer,intent(in)::n
       integer::qcd_get_int_by_int
     end function qcd_get_int_by_int
     pure function qcd_get_int_2(this)
       use muli_basic, only: drk
       import qcd_2_2_class
       class(qcd_2_2_class),intent(in)::this
       integer,dimension(2)::qcd_get_int_2
     end function qcd_get_int_2
     pure function qcd_get_int_4(this)
       use muli_basic, only: drk
       import qcd_2_2_class
       class(qcd_2_2_class),intent(in)::this
       integer,dimension(4)::qcd_get_int_4
     end function qcd_get_int_4
  end interface

contains

  subroutine transversal_momentum_write_to_marker(this,marker,status)
    class(transversal_momentum_type),intent(in)::this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    call marker%mark_begin("transversal_momentum_type")
    call marker%mark("gev_momenta",this%momentum(0:1))
    call marker%mark_end("transversal_momentum_type")
  end subroutine transversal_momentum_write_to_marker

  subroutine transversal_momentum_read_from_marker(this,marker,status)
    class(transversal_momentum_type),intent(out)::this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    call marker%pick_begin("transversal_momentum_type",status=status)
    call marker%pick("gev_momenta",this%momentum(0:1),status)
    this%momentum(2:4)=[&
         this%momentum(1)**2,&
         this%momentum(1)/this%momentum(0),&
         (this%momentum(1)/this%momentum(0))**2]
    call marker%pick_end("transversal_momentum_type",status=status)
  end subroutine transversal_momentum_read_from_marker

  subroutine transversal_momentum_print_to_unit(this,unit,parents,components,peers)
    class(transversal_momentum_type),intent(in)::this
    integer,intent(in)::unit
    integer(kind=dik),intent(in)::parents,components,peers
    write(unit,'("Components of transversal_momentum_type:")')
    write(unit,fmt='("Actual energy scale:")')
    write(unit,fmt='("Max scale (MeV)   :",E20.10)')this%momentum(0)
    write(unit,fmt='("Scale (MeV)       :",E20.10)')this%momentum(1)
    write(unit,fmt='("Scale^2 (MeV^2)   :",E20.10)')this%momentum(2)
    write(unit,fmt='("Scale normalized  :",E20.10)')this%momentum(3)
    write(unit,fmt='("Scale^2 normalized:",E20.10)')this%momentum(4)
  end subroutine transversal_momentum_print_to_unit
  
  pure subroutine transversal_momentum_get_type(type)
    character(:),allocatable,intent(out)::type
    allocate(type,source="transversal_momentum_type")
  end subroutine transversal_momentum_get_type

  
  elemental function transversal_momentum_get_gev_initial_cme(this) result(scale)
    class(transversal_momentum_type),intent(in)::this
    real(kind=drk)::scale
    scale=this%momentum(0)*2D0
  end function transversal_momentum_get_gev_initial_cme

  elemental function transversal_momentum_get_gev_max_scale(this) result(scale)
    class(transversal_momentum_type),intent(in)::this
    real(kind=drk)::scale
    scale=this%momentum(0)
  end function transversal_momentum_get_gev_max_scale

  elemental function transversal_momentum_get_gev2_max_scale(this) result(scale)
    class(transversal_momentum_type),intent(in)::this
    real(kind=drk)::scale
    scale=this%momentum(0)**2
  end function transversal_momentum_get_gev2_max_scale

  elemental function transversal_momentum_get_gev_scale(this) result(scale)
    class(transversal_momentum_type),intent(in)::this
    real(kind=drk)::scale
    scale=this%momentum(1)
  end function transversal_momentum_get_gev_scale

  elemental function transversal_momentum_get_gev2_scale(this) result(scale)
    class(transversal_momentum_type),intent(in)::this
    real(kind=drk)::scale
    scale=this%momentum(2)
  end function transversal_momentum_get_gev2_scale

  elemental function transversal_momentum_get_unit_scale(this) result(scale)
    class(transversal_momentum_type),intent(in)::this
    real(kind=drk)::scale
    scale=this%momentum(3)
  end function transversal_momentum_get_unit_scale

  elemental function transversal_momentum_get_unit2_scale(this) result(scale)
    class(transversal_momentum_type),intent(in)::this
    real(kind=drk)::scale
    scale=this%momentum(4)
  end function transversal_momentum_get_unit2_scale

  subroutine transversal_momentum_set_gev_initial_cme(this,new_gev_initial_cme)
    class(transversal_momentum_type),intent(inout)::this
    real(kind=drk),intent(in) :: new_gev_initial_cme
    this%momentum(0) = new_gev_initial_cme/2D0
    this%momentum(3) = this%momentum(1)/this%momentum(0)
    this%momentum(4) = this%momentum(3)**2
  end subroutine transversal_momentum_set_gev_initial_cme

  subroutine transversal_momentum_set_gev_max_scale(this,new_gev_max_scale)
    class(transversal_momentum_type),intent(inout)::this
    real(kind=drk),intent(in) :: new_gev_max_scale
    this%momentum(0) = new_gev_max_scale
    this%momentum(3) = this%momentum(1)/this%momentum(0)
    this%momentum(4) = this%momentum(3)**2
  end subroutine transversal_momentum_set_gev_max_scale

  subroutine transversal_momentum_set_gev2_max_scale(this,new_gev2_max_scale)
    class(transversal_momentum_type),intent(inout)::this
    real(kind=drk),intent(in) :: new_gev2_max_scale
    this%momentum(0) = sqrt(new_gev2_max_scale)
    this%momentum(3) = this%momentum(1)/this%momentum(0)
    this%momentum(4) = this%momentum(3)**2
  end subroutine transversal_momentum_set_gev2_max_scale

  subroutine transversal_momentum_set_gev_scale(this,new_gev_scale)
    class(transversal_momentum_type),intent(inout)::this
    real(kind=drk),intent(in) :: new_gev_scale
    this%momentum(1) = new_gev_scale
    this%momentum(2) = new_gev_scale**2
    this%momentum(3) = new_gev_scale/this%momentum(0)
    this%momentum(4) = this%momentum(3)**2
  end subroutine transversal_momentum_set_gev_scale

  subroutine transversal_momentum_set_gev2_scale(this,new_gev2_scale)
    class(transversal_momentum_type),intent(inout)::this
    real(kind=drk),intent(in) :: new_gev2_scale
    this%momentum(1) = sqrt(new_gev2_scale)
    this%momentum(2) = new_gev2_scale
    this%momentum(3) = this%momentum(1)/this%momentum(0)
    this%momentum(4) = this%momentum(3)**2
  end subroutine transversal_momentum_set_gev2_scale

  subroutine transversal_momentum_set_unit_scale(this,new_unit_scale)
    class(transversal_momentum_type),intent(inout)::this
    real(kind=drk),intent(in) :: new_unit_scale
    this%momentum(1) = new_unit_scale*this%momentum(0)
    this%momentum(2) = this%momentum(1)**2
    this%momentum(3) = new_unit_scale
    this%momentum(4) = this%momentum(3)**2
  end subroutine transversal_momentum_set_unit_scale

  subroutine transversal_momentum_set_unit2_scale(this,new_unit2_scale)
    class(transversal_momentum_type),intent(inout)::this
    real(kind=drk),intent(in) :: new_unit2_scale
    this%momentum(3) = sqrt(new_unit2_scale)
    this%momentum(4) = new_unit2_scale
    this%momentum(1) = this%momentum(3)*this%momentum(0)
    this%momentum(2) = this%momentum(1)**2
  end subroutine transversal_momentum_set_unit2_scale

  subroutine transversal_momentum_initialize(this,gev2_s)
    class(transversal_momentum_type),intent(out)::this
    real(kind=drk),intent(in)::gev2_s
    real(kind=drk)::gev_s
    gev_s=sqrt(gev2_s)
    this%momentum=[gev_s/2D0,gev_s/2D0,gev2_s/4D0,1D0,1D0]
  end subroutine transversal_momentum_initialize

end module muli_momentum
