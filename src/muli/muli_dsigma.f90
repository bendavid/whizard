!!! module: aqd_sigma_module
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
!!! Latest Change: 2011-06-28 11:53:48 CEST(+0200)
!!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!! This file contains the module "muli_dsigma". It's only type
!!! "muli_dsigma_type" provides an integrand to aq_class. The actual integrand
!!! is the normalized differential cross section of a qcd 2->2 process
!!! 1/sigma_0 * d^3 sigma / ( d pt^2  d x_1  d x_2 )
!!! I need a root function of this integrand in terms of pt, so I have to
!!! integrate out x_1 and x_2 and have to approximate the root function of the
!!! left coordinate pt.
!!! Integration of x_1 and x_2 is done by cuba, the root function is
!!! approximated by muli_aq.

module muli_dsigma
  use kinds !NODEP!
  use muli_momentum
  use muli_interactions
  use muli_basic
  use muli_cuba
  use muli_aq
  implicit none
  integer,parameter,private::dim_f=17
  
  type,public,extends(aq_class) :: muli_dsigma_type
     private
     type(transversal_momentum_type)::pt
     type(cuba_divonne_type) :: cuba_int
   contains
     ! overridden serializable_class procedures
     procedure::write_to_marker=>muli_dsigma_write_to_marker
     procedure::read_from_marker=>muli_dsigma_read_from_marker
     procedure::print_to_unit=>muli_dsigma_print_to_unit
     procedure,nopass::get_type=>muli_dsigma_get_type
     ! new procedures
     procedure :: generate => muli_dsigma_generate
     procedure :: evaluate => muli_dsigma_evaluate
     procedure :: muli_dsigma_initialize
     generic   :: initialize => muli_dsigma_initialize
!     procedure :: reset => muli_dsigma_reset
  end type muli_dsigma_type

contains
  
  subroutine muli_dsigma_write_to_marker(this,marker,status)
    class(muli_dsigma_type), intent(in) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik), intent(out) :: status
    ! local variables
    class(serializable_class),pointer::ser
    call marker%mark_begin("muli_dsigma_type")
    call aq_write_to_marker(this,marker,status)
    call this%cuba_int%serialize(marker,"cuba_int")
    call marker%mark_end("muli_dsigma_type")
  end subroutine muli_dsigma_write_to_marker

  subroutine muli_dsigma_read_from_marker(this,marker,status)
    class(muli_dsigma_type), intent(out) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik), intent(out) :: status
    ! local variables
    call marker%pick_begin("muli_dsigma_type",status=status)
    call aq_read_from_marker(this,marker,status)
    call this%cuba_int%deserialize("cuba_int",marker)
    call marker%pick_end("muli_dsigma_type",status)
  end subroutine muli_dsigma_read_from_marker

  subroutine muli_dsigma_print_to_unit(this,unit,parents,components,peers)
    class(muli_dsigma_type),intent(in)::this
    integer,intent(in)::unit
    integer(kind=dik),intent(in)::parents,components,peers
    integer::ite
    if(parents>0)call aq_print_to_unit(this,unit,parents-1,components,peers)
    write(unit,'("Components of muli_dsigma_type")')
    if(components>0)then
       write(unit,fmt=*)"Printing components of cuba_int:"
       call this%cuba_int%print_to_unit(unit,parents,components-1,peers)
    else
       write(unit,fmt=*)"Skipping components of cuba_int:"
    end if
  end subroutine muli_dsigma_print_to_unit
  
  pure subroutine muli_dsigma_get_type(type)
    character(:),allocatable,intent(out)::type
    allocate(type,source="muli_dsigma_type")
  end subroutine muli_dsigma_get_type

  subroutine muli_dsigma_generate(this,gev2_scale_cutoff,gev2_s,int_tree)
    class(muli_dsigma_type),intent(inout)::this
    real(kind=drk),intent(in)::gev2_scale_cutoff,gev2_s
    type(muli_trapezium_tree_type),intent(out)::int_tree
    real(kind=drk),dimension(ceiling(log(gev2_s/gev2_scale_cutoff)/2D0))::initial_values
    integer::n
    print *,gev2_s/gev2_scale_cutoff,ceiling(log(gev2_s/gev2_scale_cutoff)/2D0)
!    allocate(initial_values(ceiling(-log(gev2_scale_cutoff))/2))
!    allocate(real(kind=drk),dimension(ceiling(log(gev2_scale_cutoff))/2)::initial_values)
    initial_values(1)=sqrt(gev2_scale_cutoff/gev2_s)*2D0
    do n=2,size(initial_values)-1
       initial_values(n)=initial_values(n-1)*euler
    end do
    initial_values(n)=1D0
    print *,initial_values
!    STOP
    call identified_initialize(this,one,"dsigma")
    call this%pt%initialize(gev2_s)
    this%abs_error_goal = 0D0
    this%rel_error_goal=scale(1D0,-12)!-12
    this%max_nodes=1000
    call this%cuba_int%set_common(&
         &dim_f=dim_f,&
         &dim_x=2,&
         &eps_rel=scale(this%rel_error_goal,-8),&!-8
         &flags = 0)
    call this%cuba_int%set_deferred(xgiven_flat=[1D-2,5D-1+epsilon(1D0),1D-2,5D-1-epsilon(1D0)])
    print *,"muli_dsigma_generate:"
!    print *,"Cuba Error Goal:    ",this%cuba_int%eps_rel
    print *,"Overall Error Goal: ",this%rel_error_goal
    call this%init_error_tree(dim_f,initial_values)
    call this%run()
    call this%integrate(int_tree)
    call this%err_tree%deallocate_all()
    deallocate(this%err_tree)
    nullify(this%int_list)
  end subroutine muli_dsigma_generate

  subroutine muli_dsigma_evaluate(this,x,y)
    class(muli_dsigma_type),intent(inout) :: this
    real(kind=double), intent(in) :: x
    real(kind=double), intent(out),dimension(:):: y
    call this%pt%set_unit_scale(x)
!    print *,"muli_dsigma_evaluate x=",x
!    call this%cuba_int%integrate_userdata(&
!         interactions_proton_proton_integrand_param_17_reg,this%pt)
    !      if(this%cuba_int%fail==0)then
!    call this%cuba_int%print_all()
    call this%cuba_int%get_integral_array(y)
    !      else
    !         print *,"muli_dsigma_evaluate: failed."
    !         stop
    !      end if
  end subroutine muli_dsigma_evaluate

    subroutine muli_dsigma_initialize(this,id,name,goal,max_nodes,dim,cuba_goal)
      class(muli_dsigma_type),intent(inout) :: this
      integer(kind=dik),intent(in)::id,max_nodes
      integer,intent(in)::dim
      character(*),intent(in)::name
      real(kind=double),intent(in)::goal,cuba_goal
      call identified_initialize(this,id,name)
      this%rel_error_goal = goal!1d-4
      this%max_nodes=max_nodes
      call this%cuba_int%set_common(&
           &dim_f=dim,&
           &dim_x=2,&
           &eps_rel=cuba_goal,&!1d-6
           &flags = 0)
      call this%cuba_int%set_deferred(xgiven_flat=[1D-2,5D-1+epsilon(1D0),1D-2,5D-1-epsilon(1D0)])
!      call aq_initialize(this,id,name,d_goal,max_nodes,dim_f,(/8D-1/7D3,2D-3,1D-2,1D-1,1D0/))
      call this%init_error_tree(dim,(/8D-1/7D3,2D-3,1D-2,1D-1,1D0/))
      this%is_deferred_initialised = .true.
    end subroutine muli_dsigma_initialize

!!$    subroutine muli_dsigma_reset(this)
!!$      class(muli_dsigma_type),intent(inout) :: this
!!$      call aq_reset(this)
!!$      call this%initialize(id,name,d_goal,max_nodes,dim_f,init,cuba_goal)
!!$    end subroutine muli_dsigma_reset

  end module muli_dsigma


