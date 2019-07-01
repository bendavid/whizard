!!! module: muli_remnant
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
!!! Latest Change: 2011-08-03 14:20:01 CEST(+0200)
!!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!! This file contains the module "muli_remnant". All bookkeeping of the proton
!!! remnants and twin quarks is done here. Furthermore reweighting of the
!!! pdfs to derive remnant pdfs is done here.

module muli_remnant
  use, intrinsic :: iso_fortran_env
  use pdf_builtin !NODEP!
  use tao_random_numbers !NODEP!
  use muli_basic
  use muli_interactions
  use muli_momentum
!  use sf_lhapdf!NODEP!
  implicit none
  integer,parameter::nx=10000000
  integer,parameter::nq=60
  integer,public::remnant_weight_model=2
  integer::gluon_exp=4


  type,extends(serializable_class)::pdfnorm_type
     real(kind=double)::qmin,qmax,dq
     real(kind=double),dimension(-6:6,0:nq)::pdf_int
     real(kind=double),dimension(0:4,0:nq)::pdf_norm
   contains
     ! overridden serializable_class procedures
     procedure::write_to_marker=>pdfnorm_write_to_marker
     procedure::read_from_marker=>pdfnorm_read_from_marker
     procedure::print_to_unit=>pdfnorm_print_to_unit
     procedure,nopass::get_type=>pdfnorm_get_type
     procedure,nopass::verify_type=>pdfnorm_verify_type
     ! new procedures
     procedure::scan=>pdfnorm_scan
     procedure::get_norm=>pdfnorm_get_norm
  end type pdfnorm_type

  type,extends(serializable_class)::parton_type
     private
     integer::id=-1
     integer::lha_flavor
     real(kind=double)::momentum=-1D0
     class(parton_type),pointer::twin=>null()
     class(parton_type),pointer::next=>null()
   contains
     ! overridden serializable_class procedures
     procedure::write_to_marker=>parton_write_to_marker
     procedure::read_from_marker=>parton_read_from_marker
     procedure::print_to_unit=>parton_print_to_unit
     procedure,nopass::get_type=>parton_get_type
     ! new procedures
     procedure::unweighted_pdf=>twin_unweighted_pdf
     procedure::deallocate=>twin_deallocate
     procedure::push=>parton_push
     procedure::pop_by_id=>parton_pop_by_id
     procedure::pop_by_association=>parton_pop_by_association
     generic::  pop=>pop_by_id,pop_by_association
  end type parton_type

  type,extends(serializable_class)::proton_remnant_type
     integer,dimension(2)::valence_content=[1,2]
     integer::n_twins=0
     ![gluon,sea quark,valence down,valence up,twin]
     real(kind=drk),dimension(5)::pdf_int_weight=[1D0,1D0,1D0,1D0,0D0]
     real(kind=drk)::momentum_fraction=1D0
     real(kind=double)::twin_norm=0D0
     type(parton_type)::twin_partons
     type(parton_type)::is_partons
     type(parton_type)::fs_partons
     ! these pointers shall not be allocated, deallocated, serialized or deserialized explicitly.
     class(pdfnorm_type),pointer::pdf_norm=>null()
   contains
     ! manipulating parton content
     procedure::remove_valence_quark=>proton_remnant_remove_valence_quark
     procedure::remove_sea_quark=>proton_remnant_remove_sea_quark
     procedure::remove_gluon=>proton_remnant_remove_gluon
     procedure::remove_valence_up_quark=>proton_remnant_remove_valence_up_quark
     procedure::remove_valence_down_quark=>proton_remnant_remove_valence_down_quark
     procedure::remove_twin=>proton_remnant_remove_twin
     ! getting pdf
     procedure::momentum_twin_pdf=>proton_remnant_momentum_twin_pdf
     procedure::momentum_twin_pdf_array=>proton_remnant_momentum_twin_pdf_array
     procedure::momentum_kind_pdf=>proton_remnant_momentum_kind_pdf
     procedure::momentum_flavor_pdf=>proton_remnant_momentum_flavor_pdf
     procedure::momentum_kind_pdf_array=>proton_remnant_momentum_kind_pdf_array
     procedure::momentum_flavor_pdf_array=>proton_remnant_momentum_flavor_pdf_array
     procedure::parton_twin_pdf=>proton_remnant_parton_twin_pdf
     procedure::parton_twin_pdf_array=>proton_remnant_parton_twin_pdf_array
     procedure::parton_kind_pdf=>proton_remnant_parton_kind_pdf
     procedure::parton_flavor_pdf=>proton_remnant_parton_flavor_pdf
     procedure::parton_kind_pdf_array=>proton_remnant_parton_kind_pdf_array
     procedure::parton_flavor_pdf_array=>proton_remnant_parton_flavor_pdf_array
     ! getting components
     procedure::get_pdf_int_weight=>proton_remnant_get_pdf_int_weight
     procedure::get_valence_down_weight=>proton_remnant_get_valence_down_weight
     procedure::get_valence_up_weight=>proton_remnant_get_valence_up_weight
     procedure::get_valence_weight=>proton_remnant_get_valence_weight
     procedure::get_gluon_weight=>proton_remnant_get_gluon_weight
     procedure::get_sea_weight=>proton_remnant_get_sea_weight
     procedure::get_twin_weight=>proton_remnant_get_twin_weight
     procedure::get_valence_content=>proton_remnant_get_valence_content
     procedure::get_momentum_fraction=>proton_remnant_get_momentum_fraction
     ! misc
     procedure::deallocate=>proton_remnant_deallocate
     procedure::initialize=>proton_remnant_initialize
     procedure::finalize=>proton_remnant_finalize
     procedure::apply_initial_splitting=>proton_remnant_apply_initial_splitting
     procedure::reset=>proton_remnant_reset
     ! private
     procedure,private::calculate_weight=>proton_remnant_calculate_weight
     procedure,private::push_is_parton=>proton_remnant_push_is_parton
     procedure,private::push_twin=>proton_remnant_push_twin
     procedure,private::calculate_twin_norm=>proton_remnant_calculate_twin_norm
     procedure,private::replace_is_parton=>proton_remnant_replace_is_parton
     ! overridden serializable_class procedures
     procedure::write_to_marker=>proton_remnant_write_to_marker
     procedure::read_from_marker=>proton_remnant_read_from_marker
     procedure::print_to_unit=>proton_remnant_print_to_unit
     procedure,nopass::get_type=>proton_remnant_get_type
     ! plots
     procedure::gnuplot_momentum_kind_pdf_array=>proton_remnant_gnuplot_momentum_kind_pdf_array
  end type proton_remnant_type

  type,extends(serializable_class)::pp_remnant_type
     logical::initialized=.false.
     real(kind=double),private::gev_initial_cme = gev_cme_tot
     real(kind=double),private::X=1D0
     type(proton_remnant_type),dimension(2)::proton
     class(pdfnorm_type),pointer,private::pdf_norm
   contains
     ! overridden serializable_class procedures
     procedure::write_to_marker=>pp_remnant_write_to_marker
     procedure::read_from_marker=>pp_remnant_read_from_marker
     procedure::print_to_unit=>pp_remnant_print_to_unit
     procedure,nopass::get_type=>pp_remnant_get_type
     ! init /final
     procedure::initialize=>pp_remnant_initialize
     procedure::finalize=>pp_remnant_finalize
     procedure::apply_initial_interaction=>pp_remnant_apply_initial_interaction
     procedure::reset=>pp_remnant_reset
     ! 
     procedure::replace_parton=>pp_remnant_replace_parton
     procedure::momentum_pdf=>pp_remnant_momentum_pdf
     procedure::parton_pdf=>pp_remnant_parton_pdf
     procedure::apply_interaction=>pp_remnant_apply_interaction
     procedure::get_pdf_int_weights=>pp_remnant_get_pdf_int_weights
     procedure::get_pdf_int_weight=>pp_remnant_get_pdf_int_weight
     procedure::set_pdf_weight=>pp_remnant_set_pdf_weight
     procedure::get_gev_initial_cme=>pp_remnant_get_gev_initial_cme
     procedure::get_gev_actual_cme=>pp_remnant_get_gev_actual_cme
     procedure::get_cme_fraction=>pp_remnant_get_cme_fraction
     procedure::get_proton_remnant_momentum_fractions=>pp_remnant_get_proton_remnant_momentum_fractions
     procedure::get_proton_remnants=>pp_remnant_get_proton_remnants
     procedure::get_remnant_parton_flavor_pdf_arrays=>pp_remnant_get_remnant_parton_flavor_pdf_arrays
     
  end type pp_remnant_type

  interface 
     subroutine getxmin (mem, xmin)
       integer, intent(in) :: mem
       double precision, intent(out) :: xmin
     end subroutine getxmin
  end interface

  interface 
     subroutine getxmax (mem, xmax)
       integer, intent(in) :: mem
       double precision, intent(out) :: xmax
     end subroutine getxmax
  end interface

  interface
     subroutine getq2min (mem, q2min)
       integer, intent(in) :: mem
       double precision, intent(out) :: q2min
     end subroutine getq2min
  end interface

  interface
     subroutine getq2max (mem, q2max)
       integer, intent(in) :: mem
       double precision, intent(out) :: q2max
     end subroutine getq2max
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! type bound procedures for pdfnorm_type !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine pdfnorm_write_to_marker(this,marker,status)
    class(pdfnorm_type),intent(in)::this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    call marker%mark_begin("pdfnorm_type")
    call marker%mark("qmin",this%qmin)
    call marker%mark("qmax",this%qmax)
    call marker%mark("dq",this%dq)
    call marker%mark("pdf_int",this%pdf_int)
    call marker%mark("pdf_norm",this%pdf_norm)
    call marker%mark_end("pdfnorm_type")
  end subroutine pdfnorm_write_to_marker

  subroutine pdfnorm_read_from_marker(this,marker,status)
    class(pdfnorm_type),intent(out)::this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    character(:),allocatable::name
    call marker%pick_begin("pdfnorm_type",status=status)
    call marker%pick("qmin",this%qmin,status)
    call marker%pick("qmax",this%qmax,status)
    call marker%pick("dq",this%dq,status)
    call marker%pick("pdf_int",this%pdf_int,status)
    call marker%pick("pdf_norm",this%pdf_norm,status)
    call marker%pick_end("pdfnorm_type",status=status)
  end subroutine pdfnorm_read_from_marker

  recursive subroutine pdfnorm_print_to_unit(this,unit,parents,components,peers)
    class(pdfnorm_type),intent(in)::this
    integer,intent(in)::unit
    integer(kind=dik),intent(in)::parents,components,peers
    write(unit,'("Components of pdfnorm_type:")')
    write(unit,'("qmin:    ",F7.6)')this%qmin
    write(unit,'("qmax:    ",F7.6)')this%qmax
    write(unit,'("dq:      ",F7.6)')this%dq
    if(components>0)then
       write(unit,'("pdf_int:  ",13(F8.6," "))')this%pdf_int
       write(unit,'("pdf_norm: ",5(F8.6," "))')this%pdf_norm
    else
       write(unit,'("Skipping pdf_int")')
       write(unit,'("Skipping pdf_norm")')
    end if
  end subroutine pdfnorm_print_to_unit

  pure subroutine pdfnorm_get_type(type)
    character(:),allocatable,intent(out)::type
    allocate(type,source="pdfnorm_type")
  end subroutine pdfnorm_get_type

  elemental logical function pdfnorm_verify_type(type) result(match)
    character(*),intent(in)::type
    match=type=="pdfnorm_type"
  end function pdfnorm_verify_type

  subroutine pdfnorm_scan(this)
    class(pdfnorm_type),intent(out)::this
    integer::ix,iq
    real(kind=double)::xmin,xmax,dx
    real(kind=double)::q,q2min,q2max
    real(kind=double),dimension(-6:6)::f
    real(kind=double),dimension(0:2)::x
    call getxmin(0,xmin)
    call getxmax(0,xmax)
    call getq2min(0,q2min)
    call getq2max(0,q2max)
    this%qmin=sqrt(sqrt(q2min))
    this%qmax=sqrt(sqrt(q2max))
    this%dq=(this%qmax-this%qmin)/nq
    xmin=sqrt(xmin)
    xmax=sqrt(xmax)   
    dx=(xmax-xmin)/nx
    do iq=0,nq
       print *,"iq=",iq,"/",nq
       q=(this%qmin+iq*this%dq)**2
       x(0)=xmin**2
       x(1)=(xmin+dx)**2
       call evolvePDF(x(0),q,f)
       f(1)=f(1)-f(-1)
       f(2)=f(2)-f(-2)
       this%pdf_int(:,iq)=(x(1)-x(0))*f
       do ix=2,nx
          x(2)=(xmin+ix*dx)**2
          call evolvePDF(x(1),q,f)
          f(1)=f(1)-f(-1)
          f(2)=f(2)-f(-2)
          this%pdf_int(:,iq)=this%pdf_int(:,iq)+f*(x(2)-x(0))
          x(0)=x(1)
          x(1)=x(2)
       end do
       call evolvePDF(x(1),q,f)
       f(1)=f(1)-f(-1)
       f(2)=f(2)-f(-2)
       this%pdf_int(:,iq)=(this%pdf_int(:,iq)+f*(x(1)-x(0)))/2D0
       this%pdf_norm(4,iq)=this%pdf_int(2,iq)
       this%pdf_norm(3,iq)=this%pdf_int(1,iq)
       this%pdf_int(2,iq)=this%pdf_int(2,iq)+this%pdf_int(-2,iq)
       this%pdf_int(1,iq)=this%pdf_int(1,iq)+this%pdf_int(-1,iq)
       this%pdf_norm(1,iq)=this%pdf_int(0,iq)
       this%pdf_norm(2,iq)=sum(this%pdf_int(-6:-1,iq))+sum(this%pdf_int(-2:-1,iq))+sum(this%pdf_int(3:6,iq))
       this%pdf_norm(0,iq)=sum(this%pdf_int(:,iq))
       this%pdf_norm(1,iq)=this%pdf_norm(1,iq)/this%pdf_norm(0,iq)
       this%pdf_norm(2,iq)=this%pdf_norm(2,iq)/this%pdf_norm(0,iq)
       this%pdf_norm(3,iq)=this%pdf_norm(3,iq)/this%pdf_norm(0,iq)
       this%pdf_norm(4,iq)=this%pdf_norm(4,iq)/this%pdf_norm(0,iq)
       !print *,this%pdf_norm(0,iq)-1D0
    end do
  end subroutine pdfnorm_scan

  subroutine pdfnorm_get_norm(this,gev_q,dim,kind,norm)
    class(pdfnorm_type),intent(in)::this
    real(kind=double),intent(in)::gev_q
    integer,intent(in)::dim,kind
    real(kind=double),intent(out)::norm
    integer::iq
    real(kind=double)::x,q,z0,z1,z2,z3,z4
    norm=-1D0
    q=sqrt(gev_q)-this%qmin
    iq=floor(q/this%dq)
    x=q/this%dq-iq
    if(iq<0)then
       print *,"pdfnorm_getnorm: q < q_min ",gev_q,this%qmin**2
       norm=this%pdf_norm(kind,0)
    else
       if(iq>=nq)then
          print *,"pdfnorm_getnorm: q >= q_max ",gev_q,this%qmax**2
          norm=this%pdf_norm(kind,nq)
       else
          select case(dim)
          case(0)
             norm=this%pdf_norm(kind,iq)
          case(1)
             norm=this%pdf_norm(kind,iq)*(1D0-x)+this%pdf_norm(kind,iq+1)*x
          case(2)
             x=x+mod(iq,2)
             iq=iq-mod(iq,2)
             z0=this%pdf_norm(kind,iq)
             z1=this%pdf_norm(kind,iq+1)
             z2=this%pdf_norm(kind,iq+2)
             norm=((z0-2D0*z1+z2)*x-(3D0*z0-4D0*z1+z2))*x/2D0+z0
          case(3)
             x=x+mod(iq,3)
             iq=iq-mod(iq,3)
             z0=this%pdf_norm(kind,iq)
             z1=this%pdf_norm(kind,iq+1)
             z2=this%pdf_norm(kind,iq+2)
             z3=this%pdf_norm(kind,iq+3)
             norm=((-(z0-3*z1+3*z2-z3)*x+3*(2*z0-5*z1+4*z2-z3))*x-(11*z0-18*z1+9*z2-2*z3))*x/6D0+z0
          case(4)
             x=x+mod(iq,4)
             iq=iq-mod(iq,4)
             z0=this%pdf_norm(kind,iq)
             z1=this%pdf_norm(kind,iq+1)
             z2=this%pdf_norm(kind,iq+2)
             z3=this%pdf_norm(kind,iq+3)
             z4=this%pdf_norm(kind,iq+4)
             norm=(((((z0-4*z1+6*z2-4*z3+z4)*x&
                  -2*(5*z0-18*z1+24*z2-14*z3+3*z4))*x&
                  +(35*z0-104*z1+114*z2-56*z3+11*z4))*x&
                  -2*(25*z0-48*z1+36*z2-16*z3+3*z4))*x)/24D0&
                  +z0
          case default
             norm=this%pdf_norm(kind,iq)*(1D0-x)+this%pdf_norm(kind,iq+1)*x
          end select
          !          print *,iq,x,norm
       end if
    end if
  end subroutine pdfnorm_get_norm

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! type bound procedures for parton_type !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine parton_write_to_marker(this,marker,status)
    class(parton_type),intent(in)::this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    call marker%mark_begin("parton_type")
    call marker%mark("id",this%id)
    call marker%mark("lha",this%lha_flavor)
    call marker%mark("momentum",this%momentum)
    call marker%mark_end("parton_type")
  end subroutine parton_write_to_marker

  subroutine parton_read_from_marker(this,marker,status)
    class(parton_type),intent(out)::this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    character(:),allocatable::name
    call marker%pick_begin("parton_type",status=status)
    call marker%pick("id",this%id,status)
    call marker%pick("lha",this%lha_flavor,status)
    call marker%pick("momentum",this%momentum,status)
    call marker%pick_end("parton_type",status=status)
  end subroutine parton_read_from_marker

  recursive subroutine parton_print_to_unit(this,unit,parents,components,peers)
    class(parton_type),intent(in)::this
    integer,intent(in)::unit
    integer(kind=dik),intent(in)::parents,components,peers
    class(serializable_class),pointer::ser
    write(unit,'("Components of parton_type:")')
    write(unit,'("id:         ",I7)')this%id
    write(unit,'("lha flavor: ",I7)')this%lha_flavor
    write(unit,'("momentum:   ",F7.6)')this%momentum
    ser=>this%next
    call serialize_print_peer_pointer(ser,unit,parents,components,peers-one,"next")
    ser=>this%twin
    call serialize_print_comp_pointer(ser,unit,parents,components,peers-one,"twin")
  end subroutine parton_print_to_unit

  pure subroutine parton_get_type(type)
    character(:),allocatable,intent(out)::type
    allocate(type,source="parton_type")
  end subroutine parton_get_type

  pure function twin_unweighted_pdf(this,momentum_fraction) result(pdf)
    !parton pdf
    class(parton_type),intent(in)::this
    real(kind=double),intent(in)::momentum_fraction
    real(kind=double)::pdf
    if(momentum_fraction+this%twin%momentum<1D0)then
       pdf=remnant_twin_pdf_p(momentum_fraction,this%twin%momentum,gluon_exp)
    else
       pdf=0D0
    end if
  end function twin_unweighted_pdf

  recursive subroutine twin_deallocate(this)
    class(parton_type)::this
    if(associated(this%next))then
       call this%next%deallocate
       deallocate(this%next)
    end if
  end subroutine twin_deallocate

  subroutine parton_push(this,parton)
    class(parton_type),intent(inout)::this
    class(parton_type),intent(inout),pointer::parton
!    print *,"parton_push ",parton%id
    parton%next=>this%next
    this%next=>parton
  end subroutine parton_push

  subroutine parton_pop_by_id(this,id,parton)
    class(parton_type),target,intent(inout)::this
    integer,intent(in)::id
    class(parton_type),intent(out),pointer::parton
    class(parton_type),pointer::tmp_parton    
    tmp_parton=>this
    do while(associated(tmp_parton%next))
       if(tmp_parton%next%id==id)exit
       tmp_parton=>tmp_parton%next
    end do
    if(associated(tmp_parton%next))then
       parton=>tmp_parton%next
       tmp_parton%next=>parton%next
       nullify(parton%next)
!       print *,"parton_pop ",id,parton%id
    else
       nullify(parton)
       print *,"parton_pop ",id,"NULL"
    end if
  end subroutine parton_pop_by_id

  subroutine parton_pop_by_association(this,parton)
    class(parton_type),target,intent(inout)::this
    class(parton_type),intent(inout),target::parton
    class(parton_type),pointer::tmp_parton    
    tmp_parton=>this
    do while(associated(tmp_parton%next))
       if(associated(tmp_parton%next,parton))exit
       tmp_parton=>tmp_parton%next
    end do
    if(associated(tmp_parton%next))then
       tmp_parton%next=>parton%next
       nullify(parton%next)
!       print *,"parton_pop ",parton%id
    else
       print *,"parton_pop NULL"
    end if
  end subroutine parton_pop_by_association


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! type bound procedures for proton_remnant_type !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
  ! manipulating parton content 

  subroutine proton_remnant_remove_valence_quark(this,id,GeV_scale,momentum_fraction,lha_flavor)
    class(proton_remnant_type),intent(inout)::this
    integer,intent(in)::id
    real(kind=double),intent(in)::GeV_scale,momentum_fraction
    integer,intent(in)::lha_flavor  !d=1 u=2
    if(lha_flavor==1.or.lha_flavor==2)then
       associate(q=>this%valence_content(lha_flavor))
         if(q>0)then
            q=q-1
            call this%push_is_parton(id,lha_flavor,momentum_fraction)
            this%momentum_fraction=this%momentum_fraction*(1D0-momentum_fraction)
            call this%calculate_weight(GeV_scale)
         else
            print('("proton_remnant_remove_valence_quark: Cannot remove parton ",I2,": &
                 &There are no such partons left.")'),lha_flavor
            call this%print_all()
         end if
       end associate
    else
       print('("proton_remnant_remove_valence_quark: Cannot remove parton ",I2,": &
            &There are no such valence partons.")'),lha_flavor
    end if
  end subroutine proton_remnant_remove_valence_quark

  subroutine proton_remnant_remove_valence_up_quark(this,id,GeV_scale,momentum_fraction)
    class(proton_remnant_type),intent(inout)::this
    integer,intent(in)::id
    real(kind=double),intent(in)::GeV_scale,momentum_fraction
    associate(q=>this%valence_content(lha_flavor_u))
      if(q>0)then
         q=q-1
         call this%push_is_parton(id,lha_flavor_u,momentum_fraction)
         this%momentum_fraction=this%momentum_fraction*(1D0-momentum_fraction)
         call this%calculate_weight(GeV_scale)
      else
         print('("proton_remnant_remove_valence_up_quark: Cannot remove parton ",I2,": &
              &There are no such partons left.")'),lha_flavor_u
         call this%print_all
      end if
    end associate
  end subroutine proton_remnant_remove_valence_up_quark

  subroutine proton_remnant_remove_valence_down_quark(this,id,GeV_scale,momentum_fraction)
    class(proton_remnant_type),intent(inout)::this
    integer,intent(in)::id
    real(kind=double),intent(in)::GeV_scale,momentum_fraction
    associate(q=>this%valence_content(lha_flavor_d))
      if(q>0)then
         q=q-1
         call this%push_is_parton(id,lha_flavor_d,momentum_fraction)
         this%momentum_fraction=this%momentum_fraction*(1D0-momentum_fraction)
         call this%calculate_weight(GeV_scale)
      else
         print('("proton_remnant_remove_valence_down_quark: Cannot remove&
              & parton ",I2,": There are no such partons left.")')&
              &,lha_flavor_d
         call this%print_all
      end if
    end associate
  end subroutine proton_remnant_remove_valence_down_quark

  subroutine proton_remnant_remove_sea_quark(this,id,GeV_scale,momentum_fraction&
       &,lha_flavor)
    integer,intent(in)::id
    class(proton_remnant_type),intent(inout)::this
    real(kind=double),intent(in)::GeV_scale,momentum_fraction
    integer,intent(in)::lha_flavor
    !print *,"proton_remnant_remove_sea_quark",momentum_fraction
    if(lha_flavor>-6.and.lha_flavor<6.and.(lha_flavor.ne.0))then
       this%momentum_fraction=this%momentum_fraction*(1D0-momentum_fraction)
       call this%push_twin(id,lha_flavor,momentum_fraction,GeV_scale)
    end if
  end subroutine proton_remnant_remove_sea_quark

  subroutine proton_remnant_remove_gluon(this,id,GeV_scale,momentum_fraction)
    class(proton_remnant_type),intent(inout)::this
    integer,intent(in)::id
    real(kind=double),intent(in)::GeV_scale,momentum_fraction
    this%momentum_fraction=this%momentum_fraction*(1D0-momentum_fraction)
    call this%push_is_parton(id,lha_flavor_g,momentum_fraction)
  end subroutine proton_remnant_remove_gluon

  subroutine proton_remnant_remove_twin(this,id,GeV_scale)
    class(proton_remnant_type),intent(inout)::this
    integer,intent(in)::id
    real(kind=double),intent(in)::GeV_scale
    class(parton_type),pointer::twin
    call this%twin_partons%pop(id,twin)
    call this%fs_partons%push(twin)
    this%twin_norm=this%twin_norm-twin%momentum
    this%n_twins=this%n_twins-1
    call this%calculate_weight(GeV_scale)
  end subroutine proton_remnant_remove_twin

  ! getting pdf
  
  subroutine proton_remnant_parton_twin_pdf(this,lha_flavor,momentum_fraction,pdf)
    class(proton_remnant_type),intent(in)::this
    integer,intent(in)::lha_flavor
    real(kind=double),intent(in)::momentum_fraction
    real(kind=double)::pdf
    class(parton_type),pointer::tmp_twin
    pdf=0D0
    tmp_twin=>this%twin_partons%next
    do while(associated(tmp_twin))
       if(tmp_twin%lha_flavor==lha_flavor)pdf=pdf+tmp_twin%unweighted_pdf(momentum_fraction)
       tmp_twin=>tmp_twin%next
    end do
    pdf=pdf*this%get_twin_weight()
  end subroutine proton_remnant_parton_twin_pdf

  subroutine proton_remnant_parton_twin_pdf_array(this,momentum_fraction,pdf)
    class(proton_remnant_type),intent(in)::this
    real(kind=double),intent(in)::momentum_fraction
    real(kind=double),dimension(this%n_twins),intent(out)::pdf
    class(parton_type),pointer::tmp_twin
    integer::l
    tmp_twin=>this%twin_partons%next
    l=0
    do while(associated(tmp_twin))
       l=l+1
       pdf(l)=tmp_twin%unweighted_pdf(momentum_fraction)*this%twin_norm
       tmp_twin=>tmp_twin%next
    end do
  end subroutine proton_remnant_parton_twin_pdf_array

  subroutine proton_remnant_momentum_twin_pdf(this,lha_flavor,momentum_fraction,pdf)
    class(proton_remnant_type),intent(in)::this
    integer,intent(in)::lha_flavor
    real(kind=double),intent(in)::momentum_fraction
    real(kind=double),intent(out)::pdf
    call this%parton_twin_pdf(lha_flavor,momentum_fraction,pdf)
    pdf=pdf*momentum_fraction
  end subroutine proton_remnant_momentum_twin_pdf

  subroutine proton_remnant_momentum_twin_pdf_array(this,momentum_fraction,pdf)
    class(proton_remnant_type),intent(in)::this
    real(kind=double),intent(in)::momentum_fraction
    real(kind=double),dimension(this%n_twins),intent(out)::pdf
    call this%parton_twin_pdf_array(momentum_fraction,pdf)
    pdf=pdf*momentum_fraction
  end subroutine proton_remnant_momentum_twin_pdf_array
  
  subroutine proton_remnant_momentum_kind_pdf(this,GeV_scale,momentum_fraction&
       &,lha_flavor,valence_pdf,sea_pdf,twin_pdf)
    class(proton_remnant_type),intent(in)::this
    real(kind=double),intent(in)::GeV_scale,momentum_fraction
    integer,intent(in)::lha_flavor                   !g,u,d,etc.
    real(kind=double),intent(out)::valence_pdf,sea_pdf,twin_pdf
    real(kind=double),dimension(-6:6)::pdf_array
    call evolvePDF(momentum_fraction,GeV_scale,pdf_array)
    select case (lha_flavor)
    case(0) !gluon
       valence_pdf=0D0
       sea_pdf=pdf_array(0)
    case(1) !down
       valence_pdf=this%get_valence_down_weight()*(pdf_array(1)-pdf_array(-1))
       sea_pdf=pdf_array(-1)
    case(2) !up
       valence_pdf=this%get_valence_up_weight()*(pdf_array(2)-pdf_array(-2))
       sea_pdf=pdf_array(-2)
    case default
       valence_pdf=0D0
       sea_pdf=pdf_array(lha_flavor)
    end select
    sea_pdf=sea_pdf*this%get_sea_weight()
    call this%momentum_twin_pdf(lha_flavor,momentum_fraction,twin_pdf)
  end subroutine proton_remnant_momentum_kind_pdf

  subroutine proton_remnant_momentum_flavor_pdf(this,GeV_scale,momentum_fraction&
       &,lha_flavor,pdf)
    class(proton_remnant_type),intent(in)::this
    real(kind=double),intent(in)::GeV_scale,momentum_fraction
    integer,intent(in)::lha_flavor     !g,u,d,etc.
    real(kind=double),intent(out)::pdf
    real(kind=double)::valence_pdf,sea_pdf,twin_pdf
    call proton_remnant_momentum_kind_pdf(this,GeV_scale,momentum_fraction,lha_flavor&
         &,valence_pdf,sea_pdf,twin_pdf)
    pdf=valence_pdf+sea_pdf+twin_pdf
  end subroutine proton_remnant_momentum_flavor_pdf
  
  subroutine proton_remnant_momentum_flavor_pdf_array(this,GeV_scale,momentum_fraction&
       &,pdf)
    class(proton_remnant_type),intent(in)::this
    real(kind=double),intent(in)::GeV_scale,momentum_fraction
    real(kind=double),dimension(-6:6),intent(out)::pdf
    real(kind=double),dimension(2)::valence_pdf
    call this%momentum_kind_pdf_array(GeV_scale,momentum_fraction,valence_pdf,pdf)
    pdf(1:2)=pdf(1:2)+valence_pdf
    ! no twin yet
  end subroutine proton_remnant_momentum_flavor_pdf_array

  subroutine proton_remnant_momentum_kind_pdf_array(this,GeV_scale,momentum_fraction&
       &,valence_pdf,sea_pdf)
    class(proton_remnant_type),intent(in)::this
    real(kind=double),intent(in)::GeV_scale,momentum_fraction
    real(kind=double),dimension(2),intent(out)::valence_pdf
    real(kind=double),dimension(-6:6),intent(out)::sea_pdf
    call evolvePDF(momentum_fraction,GeV_scale,sea_pdf)
    valence_pdf(1)=(sea_pdf(1)-sea_pdf(-1))*this%pdf_int_weight(pdf_int_kind_val_down)
    valence_pdf(2)=(sea_pdf(2)-sea_pdf(-2))*this%pdf_int_weight(pdf_int_kind_val_up)
    sea_pdf(1)=sea_pdf(-1)
    sea_pdf(2)=sea_pdf(-2)
    sea_pdf=sea_pdf*this%get_sea_weight()
    ! no twin yet
  end subroutine PROTON_REMNANT_MOMENTUM_KIND_PDF_ARRAY

  subroutine proton_remnant_parton_kind_pdf(this,GeV_scale,momentum_fraction&
       &,lha_flavor,valence_pdf,sea_pdf,twin_pdf)
    class(proton_remnant_type),intent(in)::this
    real(kind=double),intent(in)::GeV_scale,momentum_fraction
    integer,intent(in)::lha_flavor                   !g,u,d,etc.
    real(kind=double),intent(out)::valence_pdf,sea_pdf,twin_pdf
    call this%momentum_kind_pdf(GeV_scale,momentum_fraction,lha_flavor,valence_pdf&
         &,sea_pdf,twin_pdf)
    valence_pdf=valence_pdf/momentum_fraction
    sea_pdf=sea_pdf/momentum_fraction
    twin_pdf=twin_pdf/momentum_fraction
  end subroutine proton_remnant_parton_kind_pdf

  subroutine proton_remnant_parton_flavor_pdf(this,GeV_scale,momentum_fraction&
       &,lha_flavor,pdf)
    class(proton_remnant_type),intent(in)::this
    real(kind=double),intent(in)::GeV_scale,momentum_fraction
    integer,intent(in)::lha_flavor     !g,u,d,etc.
    real(kind=double),intent(out)::pdf
    call this%momentum_flavor_pdf(GeV_scale,momentum_fraction,lha_flavor,pdf)
    pdf=pdf/momentum_fraction
  end subroutine proton_remnant_parton_flavor_pdf
  
  subroutine proton_remnant_parton_kind_pdf_array(this,GeV_scale,momentum_fraction&
       &,valence_pdf,sea_pdf)
    class(proton_remnant_type),intent(in)::this
    real(kind=double),intent(in)::GeV_scale,momentum_fraction
    real(kind=double),dimension(2),intent(out)::valence_pdf
    real(kind=double),dimension(-6:6),intent(out)::sea_pdf
    call evolvePDF(momentum_fraction,GeV_scale,sea_pdf)
    sea_pdf=sea_pdf/momentum_fraction
    valence_pdf(1)=(sea_pdf(1)-sea_pdf(-1))*this%valence_content(1)
    valence_pdf(2)=(sea_pdf(2)-sea_pdf(-2))*(this%valence_content(2)/2D0)
    sea_pdf(1)=sea_pdf(-1)
    sea_pdf(2)=sea_pdf(-2)
    valence_pdf=valence_pdf*this%get_valence_weight()
    sea_pdf=sea_pdf*this%get_sea_weight()
    ! no twin yet
  end subroutine proton_remnant_parton_kind_pdf_array

  subroutine proton_remnant_parton_flavor_pdf_array(this,GeV_scale,momentum_fraction&
       &,pdf)
    class(proton_remnant_type),intent(in)::this
    real(kind=double),intent(in)::GeV_scale,momentum_fraction
    real(kind=double),dimension(-6:6),intent(out)::pdf
    real(kind=double),dimension(2)::valence_pdf
    real(kind=double),dimension(-6:6)::twin_pdf
    print('("proton_remnant_flavor_pdf_array: Not yet implemented.")')
  end subroutine proton_remnant_parton_flavor_pdf_array

  ! getting components

  pure function proton_remnant_get_pdf_int_weight(this) result(weight)
    class(proton_remnant_type),intent(in)::this
    real(kind=double),dimension(5)::weight
    weight=this%pdf_int_weight
  end function proton_remnant_get_pdf_int_weight

  pure function proton_remnant_get_valence_weight(this) result(weight)
    class(proton_remnant_type),intent(in)::this
    real(kind=double),dimension(2)::weight
    weight=this%pdf_int_weight(3:4)
  end function proton_remnant_get_valence_weight
  
  elemental function proton_remnant_get_valence_down_weight(this) result(weight)
    class(proton_remnant_type),intent(in)::this
    real(kind=double)::weight
    weight=this%pdf_int_weight(pdf_int_kind_val_down)
  end function proton_remnant_get_valence_down_weight

  elemental function proton_remnant_get_valence_up_weight(this) result(weight)
    class(proton_remnant_type),intent(in)::this
    real(kind=double)::weight
    weight=this%pdf_int_weight(pdf_int_kind_val_up)
  end function proton_remnant_get_valence_up_weight

  elemental function proton_remnant_get_sea_weight(this) result(weight)
    class(proton_remnant_type),intent(in)::this
    real(kind=double)::weight
    weight=this%pdf_int_weight(pdf_int_kind_sea)
  end function proton_remnant_get_sea_weight

  elemental function proton_remnant_get_gluon_weight(this) result(weight)
    class(proton_remnant_type),intent(in)::this
    real(kind=double)::weight
    weight=this%pdf_int_weight(pdf_int_kind_gluon)
  end function proton_remnant_get_gluon_weight

  elemental function proton_remnant_get_twin_weight(this) result(weight)
    class(proton_remnant_type),intent(in)::this
    real(kind=double)::weight
    weight=this%pdf_int_weight(pdf_int_kind_twin)
  end function proton_remnant_get_twin_weight

  pure function proton_remnant_get_valence_content(this) result(valence)
    class(proton_remnant_type),intent(in)::this
    integer,dimension(2)::valence
    valence=this%valence_content
  end function proton_remnant_get_valence_content
  
  elemental function proton_remnant_get_momentum_fraction(this) result(momentum)
    class(proton_remnant_type),intent(in)::this
    real(kind=double)::momentum
    momentum=this%momentum_fraction
  end function proton_remnant_get_momentum_fraction

  ! misc

  subroutine proton_remnant_deallocate(this)
    class(proton_remnant_type),intent(inout)::this
    call this%is_partons%deallocate
    call this%fs_partons%deallocate
    call this%twin_partons%deallocate
    this%twin_norm=0D0
    this%n_twins=0
  end subroutine proton_remnant_deallocate

  subroutine proton_remnant_initialize(this,pdf_norm)
    class(proton_remnant_type),intent(out)::this
    class(pdfnorm_type),target,intent(in)::pdf_norm
    this%pdf_norm=>pdf_norm
  end subroutine proton_remnant_initialize
  
  subroutine proton_remnant_finalize(this)
    class(proton_remnant_type),intent(inout)::this
    call this%deallocate()
    nullify(this%pdf_norm)
  end subroutine proton_remnant_finalize

  subroutine proton_remnant_apply_initial_splitting(this,id,pdg_flavor,x,gev_scale,rnd)
    class(proton_remnant_type),intent(inout)::this
    integer,intent(in)::id,pdg_flavor
    real(kind=double),intent(in)::x,gev_scale,rnd
    real(kind=double)::valence_pdf,sea_pdf,twin_pdf
    select case(pdg_flavor)
    case(pdg_flavor_g)
       call this%remove_gluon(id,gev_scale,x)
    case(pdg_flavor_u)
       call this%parton_kind_pdf(gev_scale,x&
            &,pdg_flavor,valence_pdf,sea_pdf,twin_pdf)
       if(valence_pdf/(valence_pdf+sea_pdf)<rnd)then
          call this%remove_sea_quark(id,gev_scale,x,pdg_flavor)
       else
          call this%remove_valence_up_quark(id,gev_scale,x)
       end if
    case(pdg_flavor_d)
       call this%parton_kind_pdf(gev_scale,x&
            &,pdg_flavor,valence_pdf,sea_pdf,twin_pdf)
       if(valence_pdf/(valence_pdf+sea_pdf)<rnd)then
          call this%remove_sea_quark(id,gev_scale,x,pdg_flavor)
       else
          call this%remove_valence_down_quark(id,gev_scale,x)
       end if
    case default
       call this%remove_sea_quark(id,gev_scale,x,pdg_flavor)
    end select
    this%momentum_fraction=(1D0-x)
  end subroutine proton_remnant_apply_initial_splitting

  subroutine proton_remnant_reset(this)
    class(proton_remnant_type),intent(inout)::this
    call this%deallocate()
    this%valence_content=[1,2]
    this%pdf_int_weight=[1D0,1D0,1D0,1D0,1D0]
    this%momentum_fraction=1D0
  end subroutine proton_remnant_reset

  ! private

  subroutine proton_remnant_push_is_parton(this,id,lha_flavor,momentum_fraction)
    class(proton_remnant_type),intent(inout)::this
    integer,intent(in)::id,lha_flavor
    real(kind=double),intent(in)::momentum_fraction
    class(parton_type),pointer::tmp_parton
    allocate(tmp_parton)
    tmp_parton%id=id
    tmp_parton%lha_flavor=lha_flavor
    tmp_parton%momentum=momentum_fraction
    call this%is_partons%push(tmp_parton)
  end subroutine proton_remnant_push_is_parton

  subroutine proton_remnant_push_twin(this,id,lha_flavor,momentum_fraction,gev_scale)
    class(proton_remnant_type),intent(inout)::this
    integer,intent(in)::id,lha_flavor !of IS parton
    real(kind=double),intent(in)::momentum_fraction !of IS parton
    real(kind=double),intent(in)::GeV_scale
    class(parton_type),pointer::new_is,new_twin
    real(kind=double)::norm
    !print *,"proton_remnant_push_twin",momentum_fraction
    allocate(new_is)    
    allocate(new_twin)
    !IS initialization
    new_is%id=id
    new_is%lha_flavor=lha_flavor
    new_is%momentum=momentum_fraction
    new_is%twin=>new_twin
    !twin initialization
    new_twin%id=-id
    new_twin%lha_flavor=-lha_flavor
    new_twin%momentum=remnant_twin_momentum_4(momentum_fraction)
    new_twin%twin=>new_is
    !remnant update
    this%n_twins=this%n_twins+1
    this%twin_norm=this%twin_norm+new_twin%momentum
    call this%is_partons%push(new_is)
    call this%twin_partons%push(new_twin)
    call this%calculate_weight(GeV_scale)
  end subroutine proton_remnant_push_twin

  subroutine proton_remnant_calculate_twin_norm(this)
    class(proton_remnant_type),intent(inout)::this
    class(parton_type),pointer::twin
    integer::n
    if(associated(this%twin_partons%next))then
       this%twin_norm=0D0
       twin=>this%twin_partons%next
       do while(associated(twin))
          this%twin_norm=this%twin_norm+twin%momentum
          twin=>twin%next
       end do
    else
       this%twin_norm=0D0
    end if
  end subroutine proton_remnant_calculate_twin_norm

  subroutine proton_remnant_replace_is_parton(this,old_id,new_id,pdg_f,x_proton,gev_scale)
    class(proton_remnant_type),intent(inout)::this
    integer,intent(in)::old_id,new_id,pdg_f
    real(kind=double),intent(in)::x_proton,gev_scale
    class(parton_type),pointer::old_is_parton
    integer::lha_flavor
    real(kind=double)::momentum_fraction
    momentum_fraction=x_proton/this%momentum_fraction
    ! convert pdg flavor numbers to lha flavor numbers
    if(pdg_f==pdg_flavor_g)then
       lha_flavor=lha_flavor_g
    else
       lha_flavor=pdg_f
    end if
    ! we remove the old initial state parton from initial state stack.
    call this%is_partons%pop(old_id,old_is_parton)
    ! this check has no physical meaning, it's just a check for consistency.
    if(associated(old_is_parton))then
       ! do we emit a gluon?
       if(lha_flavor==old_is_parton%lha_flavor)then
          ! has the old initial state parton been a sea quark?
          if(associated(old_is_parton%twin))then
             ! the connection of the old is parton with it's twin was provisional. We remove it now
             call this%twin_partons%pop(old_is_parton%twin)
             call this%fs_partons%push(old_is_parton%twin)
             this%n_twins=this%n_twins-1
             ! and generate a new initial state parton - twin pair.
             call this%push_twin(new_id,lha_flavor,momentum_fraction,gev_scale)
          else
             ! there is no twin, so we just insert the new initial state parton.
             call this%push_is_parton(new_id,lha_flavor,momentum_fraction)
          end if
       else
          ! we emit a quark. is this a g->qqbar splitting?
          if(lha_flavor==lha_flavor_g)then
             ! we insert the new initial state gloun.
             call this%push_is_parton(new_id,lha_flavor,momentum_fraction)
             ! has the old initial state quark got a twin?
             if(associated(old_is_parton%twin))then
                ! we assume that this twin is the second splitting particle. so the twin becomes
                ! a final state particle now and must be removed from the is stack.
                call this%remove_twin(-old_id,GeV_scale)
             else
                ! the old initial state quark has been a valence quark. what should we do now? is this
                ! splitting sensible at all? we don't know but allow these splittings. The most trivial
                ! treatment is to restore the former valence quark.
                this%valence_content(old_is_parton%lha_flavor)=&
                     this%valence_content(old_is_parton%lha_flavor)+1
             end if
          else
             ! this is a q->qg splitting. the new initial state quark emits the preceding initial state
             ! gluon. yeah, backward evolution is confusing!
             ! the new initial state quark is not part of the proton remnant any longer. how do we remove
             ! a quark from the remnant? we add a conjugated twin parton and assume, that this twin is
             ! created in a not yet resolved g->qqbar splitting.
             call this%push_twin(new_id,lha_flavor,momentum_fraction,gev_scale)
          end if
       end if
       ! everything is done. what shall we do with the old initial state parton? we don't need it any more
       ! but we store it anyway.
       call this%fs_partons%push(old_is_parton)
       ! the new initial state parton has taken away momentum, so we update the remnant momentum fraction.
       this%momentum_fraction=this%momentum_fraction*(1-momentum_fraction)/(1-old_is_parton%momentum)
    else
       ! this is a bug.
       print *,"proton_remnant_replace_is_parton: parton #",old_id," not found on ISR stack."
       if(associated(this%is_partons%next))then
          print *,"actual content of isr stack:"
          call this%is_partons%next%print_peers()
       else
          print *,"isr stack is not associated."
       end if
       STOP
    end if
  end subroutine proton_remnant_replace_is_parton
 
  subroutine proton_remnant_calculate_weight(this,GeV_scale)
    class(proton_remnant_type),intent(inout)::this
    real(kind=double),intent(in)::GeV_scale
    real(kind=double)::all,gluon,sea,vu,vd,valence,twin,weight
    call this%pdf_norm%get_norm(GeV_scale,1,0,all)
    call this%pdf_norm%get_norm(GeV_scale,1,pdf_int_kind_gluon,gluon)
    call this%pdf_norm%get_norm(GeV_scale,1,pdf_int_kind_sea,sea)
    call this%pdf_norm%get_norm(GeV_scale,1,pdf_int_kind_val_down,vd)
    call this%pdf_norm%get_norm(GeV_scale,1,pdf_int_kind_val_up,vu)
    valence=&
         vd*this%valence_content(lha_flavor_d)+&
         vu*this%valence_content(lha_flavor_u)/2D0
    twin=this%twin_norm/all
    !print *,all,gluon+sea+valence+twin,gluon,sea,valence,twin
    !pdf_rescale=(1D0-n_d_valence*mean_d1-n_u_valence*mean_u2)/(1D0-1*mean_d1-2*mean_u2) !pythia
    select case(remnant_weight_model)
    case(0) ! no reweighting
       this%pdf_int_weight=[1D0,1D0,1D0,1D0,1D0]
    case(2) !pythia-like, only sea
       weight=(1D0-valence-twin)&
            &/(sea+gluon)
       this%pdf_int_weight=[weight,weight,1D0,1D0,1D0]
    case(3) !only valence and twin
       weight=(1D0-sea-gluon)&
            &/(valence+twin)
       this%pdf_int_weight=[1D0,1D0,weight,weight,weight]
    case(4) !only sea and twin
       weight=(1D0-valence)&
            &/(sea+gluon+twin)
       this%pdf_int_weight=[1D0,weight,1D0,1D0,weight]
    case default !equal weight
       weight=1D0/(valence+sea+gluon+twin)
       this%pdf_int_weight=[weight,weight,weight,weight,weight]
    end select
    this%pdf_int_weight(pdf_int_kind_val_down)=this%pdf_int_weight(pdf_int_kind_val_down)*this%valence_content(1)
    this%pdf_int_weight(pdf_int_kind_val_up)=this%pdf_int_weight(pdf_int_kind_val_up)*this%valence_content(2)*5D-1
!    print('("New rescale factors are: ",2(I10),7(E14.7))'),&
!         this%valence_content,&
!         this%pdf_int_weight,&
!         sea_norm,&
!         valence_norm,&
!         this%twin_norm
  end subroutine proton_remnant_calculate_weight
  
  ! overridden

  subroutine proton_remnant_write_to_marker(this,marker,status)
    class(proton_remnant_type),intent(in)::this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    call marker%mark_begin("proton_remnant_type")
    call marker%mark("valence_content",this%valence_content)
    call marker%mark("momentum_fraction",this%momentum_fraction)
    call marker%mark("pdf_int_weight",this%pdf_int_weight)
    call marker%mark_end("proton_remnant_type")
  end subroutine proton_remnant_write_to_marker

  subroutine proton_remnant_read_from_marker(this,marker,status)
    class(proton_remnant_type),intent(out)::this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    character(:),allocatable::name
    call marker%pick_begin("proton_remnant_type",status=status)
    call marker%pick("valence_content",this%valence_content,status)
    call marker%pick("momentum_fraction",this%momentum_fraction,status)
    call marker%pick("pdf_int_weight",this%pdf_int_weight,status)
    call marker%pick_end("proton_remnant_type",status=status)
  end subroutine proton_remnant_read_from_marker

  subroutine proton_remnant_print_to_unit(this,unit,parents,components,peers)
    class(proton_remnant_type),intent(in)::this
    integer,intent(in)::unit
    integer(kind=dik),intent(in)::parents,components,peers
    write(unit,'("Components of proton_remnant_type:")')
    write(unit,'("Valence Content:           ",I1,":",I1)')this&
         &%valence_content
    write(unit,'("N Twins:                   ",I1)')this%n_twins
    write(unit,'("INT weights [g,s,d,u,t]    ",5(F7.3))')this%pdf_int_weight
    write(unit,'("Total Momentum Fraction:   ",F7.3)')this%momentum_fraction
    write(unit,'("Twin Norm:            ",F7.3)')this%twin_norm
  end subroutine proton_remnant_print_to_unit
  
  pure subroutine proton_remnant_get_type(type)
    character(:),allocatable,intent(out)::type
    allocate(type,source="proton_remnant_type")
  end subroutine proton_remnant_get_type

  subroutine proton_remnant_gnuplot_momentum_kind_pdf_array(this,momentum_unit,parton_unit,GeV_scale)
    class(proton_remnant_type),intent(in)::this
    integer,intent(in)::momentum_unit,parton_unit
    real(kind=double),intent(in)::GeV_scale
    real(kind=double),dimension(2)::valence_pdf
    real(kind=double),dimension(-6:6)::sea_pdf
    real(kind=double),dimension(this%n_twins)::twin_pdf
    integer::x
    real(kind=double)::momentum_fraction
    do x=1,100
       momentum_fraction=x*1D-2
       call this%momentum_kind_pdf_array(GeV_scale,momentum_fraction&
            &,valence_pdf,sea_pdf)
       call this%momentum_twin_pdf_array(momentum_fraction,twin_pdf)
       write(momentum_unit,fmt=*)momentum_fraction,&
            sum(valence_pdf)+sum(sea_pdf)+sum(twin_pdf),&
            valence_pdf,&
            sea_pdf,&
            twin_pdf
       call this%parton_kind_pdf_array(GeV_scale,momentum_fraction&
            &,valence_pdf,sea_pdf)
       call this%parton_twin_pdf_array(momentum_fraction,twin_pdf)
       write(parton_unit,fmt=*)momentum_fraction,&
            sum(valence_pdf)+sum(sea_pdf)+sum(twin_pdf),&
            valence_pdf,&
            sea_pdf,&
            twin_pdf       
    end do
  end subroutine proton_remnant_gnuplot_momentum_kind_pdf_array

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! type bound procedures for pp_remnant_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine pp_remnant_initialize(&
       this,&
       muli_dir,&
       lhapdf_dir,&
       lhapdf_file,&
       lhapdf_member)
    class(pp_remnant_type),intent(out)::this
    character(*),intent(in)::muli_dir,lhapdf_dir,lhapdf_file
    integer,intent(in)::lhapdf_member
    logical::exist
    allocate(this%pdf_norm)
!    call InitPDFset(lhapdf_dir//lhapdf_file)
!    call InitPDF(lhapdf_member)
    print *,"looking for previously generated pdf integrals..."
    inquire(file=muli_dir//"/pdf_norm_"//lhapdf_file//".xml",exist=exist)
    if(exist)then
       print *,"found. Starting deserialization..."
       call this%pdf_norm%deserialize(&
            name="pdf_norm_"//lhapdf_file,&
            file=muli_dir//"/pdf_norm_"//lhapdf_file//".xml")
       print *,"done."
    else
       print *,"No integrals found. Starting generation..."
       call this%pdf_norm%scan()
       print *,"done."
       call this%pdf_norm%serialize(&
            name="pdf_norm_"//lhapdf_file,&
            file=muli_dir//"/pdf_norm_"//lhapdf_file//".xml")
    end if
    call this%proton(1)%initialize(this%pdf_norm)
    call this%proton(2)%initialize(this%pdf_norm)
    this%initialized=.true.
    !call this%print_all()
  end subroutine pp_remnant_initialize

  subroutine pp_remnant_finalize(this)
    class(pp_remnant_type),intent(inout)::this
    call this%proton(1)%finalize()
    call this%proton(2)%finalize()
    deallocate(this%pdf_norm)
  end subroutine pp_remnant_finalize

  subroutine pp_remnant_apply_initial_interaction(this,gev_cme,x1,x2,pdg_f1,pdg_f2,n1,n2,gev_scale,rnd1,rnd2)
    class(pp_remnant_type),intent(inout)::this
    real(kind=double),intent(in)::gev_cme,x1,x2,gev_scale,rnd1,rnd2
    integer,intent(in)::pdg_f1,pdg_f2,n1,n2
    if(this%initialized)then
       call this%proton(1)%apply_initial_splitting(n1,pdg_f1,x1,gev_scale,rnd1)
       call this%proton(2)%apply_initial_splitting(n2,pdg_f2,x2,gev_scale,rnd2)
       this%X=(1D0-x1)*(1D0-x2)
       this%gev_initial_cme=gev_cme
       !call this%print_all()
    else
       print *,"pp_remnant_apply_initial_interaction: Not yet initialized, call pp_remnant_initialize first!"
       stop
    end if
  end subroutine pp_remnant_apply_initial_interaction
  
  subroutine pp_remnant_replace_parton(this,proton_id,old_id,new_id,pdg_f,x_proton,gev_scale)
    class(pp_remnant_type),intent(inout)::this
    integer,intent(in)::proton_id,old_id,new_id,pdg_f
    real(kind=double),intent(in)::x_proton,gev_scale
    call this%proton(proton_id)%replace_is_parton(old_id,new_id,pdg_f,x_proton,gev_scale)
  end subroutine pp_remnant_replace_parton

  subroutine pp_remnant_momentum_pdf(this,x_proton,gev2_scale,n,pdg_f,pdf)
    class(pp_remnant_type),intent(in)::this
    real(kind=double),intent(in)::x_proton,gev2_scale
    integer,intent(in)::n,pdg_f
    real(kind=double),intent(out)::pdf
    if(n==1.or.n==2)then
       if(x_proton<=this%proton(n)%momentum_fraction)then
          if(pdg_f==pdg_flavor_g)then
             call this%proton(n)%momentum_flavor_pdf(&
                  sqrt(GeV2_scale),x_proton/this%proton(n)%momentum_fraction,lha_flavor_g,pdf&
                  )
          else
             call this%proton(n)%momentum_flavor_pdf(&
                  sqrt(GeV2_scale),x_proton/this%proton(n)%momentum_fraction,pdg_f,pdf&
                  )
          end if
          pdf=pdf*this%proton(n)%momentum_fraction
       else
          pdf=0D0
       end if
    else
       print *,"pp_remnant_momentum_pdf: n must be either 1 or 2, but it is ",n
       stop
    end if
  end subroutine pp_remnant_momentum_pdf

 subroutine pp_remnant_parton_pdf(this,x_proton,gev2_scale,n,pdg_f,pdf)
    class(pp_remnant_type),intent(in)::this
    real(kind=double),intent(in)::x_proton,gev2_scale
    integer,intent(in)::n,pdg_f
    real(kind=double),intent(out)::pdf
    if(n==1.or.n==2)then
       if(x_proton<=this%proton(n)%momentum_fraction)then
          if(pdg_f==pdg_flavor_g)then
             call this%proton(n)%parton_flavor_pdf(&
                  sqrt(GeV2_scale),x_proton/this%proton(n)%momentum_fraction,lha_flavor_g,pdf&
                  )
          else
             call this%proton(n)%parton_flavor_pdf(&
                  sqrt(GeV2_scale),x_proton/this%proton(n)%momentum_fraction,pdg_f,pdf&
                  )
          end if
          pdf=pdf*this%proton(n)%momentum_fraction
       else
          pdf=0D0
       end if
    else
       print *,"pp_remnant_parton_pdf: n must be either 1 or 2, but it is ",n
       stop
    end if
  end subroutine pp_remnant_parton_pdf

  subroutine pp_remnant_apply_interaction(this,qcd_2_2)
    class(pp_remnant_type),intent(inout)::this
    class(qcd_2_2_class),intent(in)::qcd_2_2
    integer,dimension(4)::lha_f
    integer,dimension(2)::int_k
    real(kind=double)::gev_pt
    real(kind=double),dimension(2)::mom_f
    integer::n
    mom_f=qcd_2_2%get_remnant_momentum_fractions()
    lha_f=qcd_2_2%get_lha_flavors()
    int_k=qcd_2_2%get_pdf_int_kinds()
    gev_pt=qcd_2_2%get_gev_scale()
    !print *,"pp_remnant_apply_interaction",mom_f,qcd_2_2%get_parton_id(1),qcd_2_2%get_parton_id(2),lha_f
    do n=1,2
       select case (int_k(n))
       case(pdf_int_kind_val_down)
          call this%proton(n)%remove_valence_down_quark(qcd_2_2%get_parton_id(n),gev_pt,mom_f(n))
       case(pdf_int_kind_val_up)
          call this%proton(n)%remove_valence_up_quark(qcd_2_2%get_parton_id(n),gev_pt,mom_f(n))
       case(pdf_int_kind_sea)
          call this%proton(n)%remove_sea_quark(qcd_2_2%get_parton_id(n),gev_pt,mom_f(n),lha_f(n))
       case(pdf_int_kind_gluon)
          call this%proton(n)%remove_gluon(qcd_2_2%get_parton_id(n),gev_pt,mom_f(n))
       end select
    end do
    this%X=this%proton(1)%momentum_fraction*this%proton(2)%momentum_fraction
  end subroutine pp_remnant_apply_interaction

  subroutine pp_remnant_reset(this)
    class(pp_remnant_type),intent(inout)::this
    call this%proton(1)%reset()
    call this%proton(2)%reset()
    this%X=1D0
  end subroutine pp_remnant_reset

  pure function pp_remnant_get_pdf_int_weights(this,pdf_int_kinds) result(weight)
    class(pp_remnant_type),intent(in)::this
    real(kind=double)::weight
    integer,dimension(2),intent(in)::pdf_int_kinds ! pdf_int_kind
    weight=this%proton(1)%pdf_int_weight(pdf_int_kinds(1))*this%proton(2)%pdf_int_weight(pdf_int_kinds(2))!*((this%x)**2)
  end function pp_remnant_get_pdf_int_weights

  elemental function pp_remnant_get_pdf_int_weight(this,kind1,kind2) result(weight)
    class(pp_remnant_type),intent(in)::this
    real(kind=double)::weight
    integer,intent(in)::kind1,kind2 ! pdf_int_kind
    weight=this%proton(1)%pdf_int_weight(kind1)*this%proton(2)%pdf_int_weight(kind2)!*((this%x)**2)
  end function pp_remnant_get_pdf_int_weight

  subroutine pp_remnant_set_pdf_weight(this,weights)
    class(pp_remnant_type),intent(inout)::this
    real(kind=double),dimension(10),intent(in)::weights
    this%proton(1)%pdf_int_weight=weights(1:5)
    this%proton(2)%pdf_int_weight=weights(6:10)
  end subroutine pp_remnant_set_pdf_weight

  elemental function pp_remnant_get_gev_initial_cme(this) result(cme)
    class(pp_remnant_type),intent(in)::this
    real(kind=double)::cme
    cme=this%gev_initial_cme
  end function pp_remnant_get_gev_initial_cme

  elemental function pp_remnant_get_gev_actual_cme(this) result(cme)
    class(pp_remnant_type),intent(in)::this
    real(kind=double)::cme
    cme=this%gev_initial_cme*this%X
  end function pp_remnant_get_gev_actual_cme

  elemental function pp_remnant_get_cme_fraction(this) result(cme)
    class(pp_remnant_type),intent(in)::this
    real(kind=double)::cme
    cme=this%X
  end function pp_remnant_get_cme_fraction

  pure function pp_remnant_get_proton_remnant_momentum_fractions(this) result(fractions)
    class(pp_remnant_type),intent(in)::this
    real(kind=double),dimension(2)::fractions
    fractions=[this%proton(1)%get_momentum_fraction(),this%proton(2)%get_momentum_fraction()]
  end function pp_remnant_get_proton_remnant_momentum_fractions

  subroutine pp_remnant_get_proton_remnants(this,proton1,proton2)
    class(pp_remnant_type),target,intent(in)::this
    class(proton_remnant_type),intent(out),pointer::proton1,proton2
    proton1=>this%proton(1)
    proton2=>this%proton(2)
  end subroutine pp_remnant_get_proton_remnants

  subroutine pp_remnant_get_remnant_parton_flavor_pdf_arrays(this,GeV_scale,momentum1,momentum2,pdf1,pdf2)
    class(pp_remnant_type),intent(in)::this
    real(kind=double),intent(in)::GeV_scale,momentum1,momentum2
    real(kind=double),dimension(-6:6),intent(out)::pdf1,pdf2
    call this%proton(1)%parton_flavor_pdf_array(GeV_scale,momentum1,pdf1)
    call this%proton(2)%parton_flavor_pdf_array(GeV_scale,momentum2,pdf2)
  end subroutine pp_remnant_get_remnant_parton_flavor_pdf_arrays

  !overridden procedures

  subroutine pp_remnant_write_to_marker(this,marker,status)
    class(pp_remnant_type),intent(in)::this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    call marker%mark_begin("pp_remnant_type")
    call marker%mark("gev_initial_cme",this%gev_initial_cme)
    call marker%mark("X",this%X)
    call this%proton(1)%write_to_marker(marker,status)
    call this%proton(2)%write_to_marker(marker,status)
    call marker%mark_end("pp_remnant_type")
  end subroutine pp_remnant_write_to_marker

  subroutine pp_remnant_read_from_marker(this,marker,status)
    class(pp_remnant_type),intent(out)::this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    character(:),allocatable::name
    call marker%pick_begin("pp_remnant_type",status=status)
    call marker%pick("gev_initial_cme",this%gev_initial_cme,status)
    call marker%pick("X",this%X,status)
    call this%proton(1)%read_from_marker(marker,status)
    call this%proton(2)%read_from_marker(marker,status)
    call marker%pick_end("pp_remnant_type",status=status)
  end subroutine pp_remnant_read_from_marker

  subroutine pp_remnant_print_to_unit(this,unit,parents,components,peers)
    class(pp_remnant_type),intent(in)::this
    integer,intent(in)::unit
    integer(kind=dik),intent(in)::parents,components,peers
    write(unit,'("Components of pp_remnant_type:")')
    write(unit,'("Initial center of mass energy: ",F10.3)')this%gev_initial_cme
    write(unit,'("Actual center of mass energy:  ",F10.3)')this%get_gev_actual_cme()
    write(unit,'("Total Momentum Fraction is:    ",F10.3)')this%X
    if(components>0)then
       write(unit,'("Proton 1:")')
       call this%proton(1)%print_to_unit(unit,parents,components-1,peers)
       write(unit,'("Proton 2:")')
       call this%proton(2)%print_to_unit(unit,parents,components-1,peers)
    end if
!    write(unit,'("Total Momentum Fraction:   ",F7.2)')this%momentum_fraction
  end subroutine pp_remnant_print_to_unit

  pure subroutine pp_remnant_get_type(type)
    character(:),allocatable,intent(out)::type
    allocate(type,source="pp_remnant_type")
  end subroutine pp_remnant_get_type

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Non type bound module procedures !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  pure function remnant_dglap_splitting_gqq(z) result(p)
    real(kind=double)::p
    real(kind=double),intent(in)::z
    p=(z**2+(1-z)**2)/2D0
  end function remnant_dglap_splitting_gqq
  
  pure function remnant_gluon_pdf_approx(x,p) result(g)
    real(kind=double)::g
    integer,intent(in)::p
    real(kind=double),intent(in)::x
    g=((1-x)**p)/x
  end function remnant_gluon_pdf_approx

  pure function remnant_norm_0(xs) result(c0)
    real(kind=double)::c0
    real(kind=double),intent(in)::xs
    c0=6*xs/(2-xs*(3-3*xs+2*xs**2))
  end function remnant_norm_0

  pure function remnant_norm_1(xs) result(c1)
    real(kind=double)::c1
    real(kind=double),intent(in)::xs
    c1=3*xs/(2-xs**2*(3-xs)+3*xs*log(xs))
  end function remnant_norm_1

  pure function remnant_norm_4(xs) result(c4)
    real(kind=double)::c4
    real(kind=double),intent(in)::xs
    real(kind=double)::y
    if((1D0-xs)>1D-3)then
       c4=3*xs/(1 + 11*xs + 6*xs*log(xs) + 12*xs**3*log(xs) + 18*xs**2*log(xs) + 9*xs**2 - 19*xs**3 - 2*xs**4)
    else
       y=1D0/(1D0-xs)
       c4=&
            &1130D0/11907D0&
            & -10D0 *y**5&
            & -40D0 *y**4/3D0&
            & -160D0*y**3/63D0&
            & +50D0 *y**2/189D0&
            & -565D0*y   /3969D0&
            & -186170D0*(1D0-xs)/2750517D0
    end if
  end function remnant_norm_4

  pure function remnant_norm(xs,p) result(c)
    real(kind=double)::c
    real(kind=double),intent(in)::xs
    integer,intent(in)::p
    select case (p)
    case(0)
       c=remnant_norm_0(xs)
    case(1)
       c=remnant_norm_1(xs)
    case default
       c=remnant_norm_4(xs)
    end select
  end function remnant_norm

  pure function remnant_twin_pdf_p(x,xs,p) result(qc)
    real(kind=double)::qc
    real(kind=double),intent(in)::x,xs
    integer,intent(in)::p
    qc=remnant_norm(xs,p)*remnant_gluon_pdf_approx(xs+x,p)*remnant_dglap_splitting_gqq(xs/(xs+x))/(xs+x)
  end function remnant_twin_pdf_p
  
  elemental function remnant_twin_momentum_4(xs) result(p)
    real(kind=double)::p
    real(kind=double),intent(in)::xs
    if(xs<0.99D0)then
       p=(-9*(-1+xs)*xs*(1+xs)*(5+xs*(24+xs))+12*xs*(1+2*xs)*(1+2*xs*(5+2*xs))*Log(xs))/&
            (8*(1+2*xs)*((-1+xs)*(1+xs*(10+xs))-6*xs*(1+xs)*Log(xs)))
    else
       p=(1-xs)/6-(5*(-1+xs)**2)/63+(5*(-1+xs)**3)/216
    end if
  end function remnant_twin_momentum_4

  subroutine gnuplot_integrated_pdf(this,momentum_unit,parton_unit)
    class(proton_remnant_type),intent(in)::this
    integer,intent(in)::momentum_unit,parton_unit
!    real(kind=double),intent(in)::gev_scale
    integer,parameter::x_grid=1000000
    integer,parameter::q_grid=100
    integer::n,m,mem
    real(kind=double)::x,q,dx,dq,overall_sum,xmin,xmax,q2min,q2max,qmin,qmax
    real(kind=double),dimension(-6:6)::sea_pdf,sea_momentum_pdf_sum,sea_parton_pdf_sum
    real(kind=double),dimension(2)::valence_pdf,valence_momentum_pdf_sum,valence_parton_pdf_sum
    real(kind=double),allocatable,dimension(:)::twin_momentum_pdf_sum
    class(parton_type),pointer::tmp_twin
    mem=1
    call GetXmin(mem,xmin)
    call GetXmax(mem,xmax)
    call GetQ2max(mem,q2max)
    call GetQ2min(mem,q2min)
    qmin=sqrt(q2min)
    qmax=sqrt(q2max)
    print *,"qmin=",qmin,"GeV"
    print *,"qmax=",qmax,"GeV"
    dx=(xmax-xmin)/x_grid
    dq=(qmax-qmin)/q_grid
    q=qmin+dq/2D0
    tmp_twin=>this%twin_partons%next
    n=0
    if(this%n_twins>0)then
       allocate(twin_momentum_pdf_sum(this%n_twins))
       do while(associated(tmp_twin))
          n=n+1
          twin_momentum_pdf_sum(n)=tmp_twin%momentum
          tmp_twin=>tmp_twin%next
       end do
    end if
    do m=1,q_grid
       valence_momentum_pdf_sum=[0D0,0D0]
       valence_parton_pdf_sum=[0D0,0D0]
       sea_momentum_pdf_sum=[0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0]    
       sea_parton_pdf_sum=[0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0]    
       x=xmin+dx/2D0
       do n=1,x_grid
          call this%parton_kind_pdf_array(Q,x,valence_pdf,sea_pdf)
          valence_parton_pdf_sum=valence_parton_pdf_sum+valence_pdf
          sea_parton_pdf_sum=sea_parton_pdf_sum+sea_pdf
          call this%momentum_kind_pdf_array(Q,x,valence_pdf,sea_pdf)
          valence_momentum_pdf_sum=valence_momentum_pdf_sum+valence_pdf
          sea_momentum_pdf_sum=sea_momentum_pdf_sum+sea_pdf
          x=x+dx
       end do
       valence_parton_pdf_sum=valence_parton_pdf_sum*dx
       sea_parton_pdf_sum=sea_parton_pdf_sum*dx
       valence_momentum_pdf_sum=valence_momentum_pdf_sum*dx
       sea_momentum_pdf_sum=sea_momentum_pdf_sum*dx
       if(this%n_twins>0)then
          write(momentum_unit,fmt=*)q,&
               sum(valence_momentum_pdf_sum)+sum(sea_momentum_pdf_sum)+sum(twin_momentum_pdf_sum),&
               valence_momentum_pdf_sum,&
               sea_momentum_pdf_sum,&
               twin_momentum_pdf_sum
       else
          write(momentum_unit,fmt=*)q,&
               sum(valence_momentum_pdf_sum)+sum(sea_momentum_pdf_sum),&
               valence_momentum_pdf_sum,&
               sea_momentum_pdf_sum
       end if
       write(parton_unit,fmt=*)q,&
            sum(valence_parton_pdf_sum)+sum(sea_parton_pdf_sum),&
            valence_parton_pdf_sum,&
            sea_parton_pdf_sum
       q=q+dq
    end do
  end subroutine gnuplot_integrated_pdf

end module muli_remnant
