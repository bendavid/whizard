!!! module: muli_cuba_types
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
!!! Latest Change: 2011-06-20 11:26:03 CEST(+0200)
!!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!! This file contains the module "muli_cuba", a wrapper for the cuba
!!! integration library. It is not very useful for the final state of my
!!! project, but I have toyed a lot with different algorithms and different
!!! settings to find a combination that suits my demands best. That's what 
!!! I made this wrapper for.

module muli_cuba
  use muli_momentum
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Parameter Definition !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  integer, parameter :: max_maxeval = huge(1)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Derived Type Definitions !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  type,extends(serializable_class), abstract :: cuba_class  
     ! private
     real(kind=drk) :: start_time=0D0
     real(kind=drk) :: stop_time=0D0
     real(kind=drk) :: run_time=0D0
     ! common input
     integer :: dim_x = 2
     integer :: dim_f = 1
     type(transversal_momentum_type) :: userdata
     real(kind=drk) :: eps_rel = 1D-3
     real(kind=drk) :: eps_abs = 0D0
     integer :: flags = 0
     integer :: seed = 1
     integer :: min_eval = 0
     integer :: max_eval = max_maxeval
     ! common output
     integer :: neval = 0
     integer,public :: fail = -1
     integer :: nregions = 0
     real(kind=drk), dimension(:), allocatable :: integral
     real(kind=drk), dimension(:), allocatable :: error
     real(kind=drk), dimension(:), allocatable :: prob
     !
     procedure(integrand_interface),nopass,pointer::integrand
   contains
     ! overridden serializable procedures
     procedure::write_to_marker=>cuba_write_to_marker
     procedure::read_from_marker=>cuba_read_from_marker
     procedure::print_to_unit=>cuba_print_to_unit
     ! new procedures
     procedure :: get_integral_array => cuba_get_integral_array
     procedure :: get_integral_1 => cuba_get_integral_1
     generic   :: get_integral=>get_integral_array,get_integral_1!FC = nagfor
     procedure :: copy_common => cuba_copy_common

     procedure :: set_common => cuba_set_common
     procedure :: set_dim_f => cuba_set_dim_f
     procedure :: set_dim_x => cuba_set_dim_x
     procedure :: reset_timer=>cuba_reset_timer

     procedure :: integrate_with_timer => cuba_integrate_with_timer
     procedure :: integrate_associated => cuba_integrate_associated
     procedure(integrate_interface), deferred :: integrate_nd
     procedure(integrate_userdata_interface), deferred :: integrate_userdata
     procedure(cuba_copy_interface), deferred :: copy

     procedure :: dealloc_dim_f => cuba_dealloc_dim_f
     procedure :: alloc_dim_f => cuba_alloc_dim_f    
     procedure :: dealloc => cuba_dealloc
     procedure :: alloc => cuba_alloc     
     generic :: integrate=>integrate_nd,integrate_userdata
  end type cuba_class

!!$  type, extends(cuba_class) :: cuba_cuhre_type
!!$     private
!!$     integer :: key = 13
!!$   contains
!!$     ! overridden serializable procedures
!!$     procedure::write_to_marker=>cuba_cuhre_write_to_marker
!!$     procedure::read_from_marker=>cuba_cuhre_read_from_marker
!!$     procedure::print_to_unit=>cuba_cuhre_print_to_unit
!!$     procedure,nopass::get_type=>cuba_cuhre_get_type
!!$     ! overridden cuba procedures
!!$     procedure :: integrate_nd => integrate_cuhre
!!$     procedure :: integrate_userdata => integrate_cuhre_userdata
!!$     procedure :: copy=>cuba_cuhre_copy
!!$     procedure :: set_deferred => cuba_cuhre_set_deferred
!!$  end type cuba_cuhre_type

!!$  type, extends(cuba_class) :: cuba_suave_type
!!$     private
!!$     integer :: nnew = 10000 !1000
!!$     integer :: flatness = 5 !50
!!$   contains
!!$     ! overridden serializable procedures
!!$     procedure::write_to_marker=>cuba_suave_write_to_marker
!!$     procedure::read_from_marker=>cuba_suave_read_from_marker
!!$     procedure::print_to_unit=>cuba_suave_print_to_unit
!!$     procedure,nopass::get_type=>cuba_suave_get_type
!!$     ! overridden cuba procedures
!!$     procedure :: integrate_nd => integrate_suave
!!$     procedure :: integrate_userdata => integrate_suave_userdata
!!$     procedure :: copy=>cuba_suave_copy
!!$  end type cuba_suave_type

  type, extends(cuba_class) :: cuba_divonne_type
     private
     integer :: key1 = 13
     integer :: key2 = 13
     integer :: key3 = 13
     integer :: maxpass = 2
     real(kind=drk) :: border = 0D0
     real(kind=drk) :: maxchisq = 10D0
     real(kind=drk) :: mindeviation = .25D0
     integer :: ngiven = 0
     integer :: ldxgiven = 0
     !     real(kind=drk), dimension(ldxgiven,ngiven) :: &
     !          & xgiven = reshape( source = (/0.0,0.0/), shape = (/2,1/))
     real(kind=drk),dimension(:,:),allocatable :: xgiven
     !     real(kind=drk),dimension(2) :: xgiven = [1d-1,5d-1]
     integer :: nextra = 0
   contains
     ! overridden serializable procedures
     procedure::write_to_marker=>cuba_divonne_write_to_marker
     procedure::read_from_marker=>cuba_divonne_read_from_marker
     procedure::print_to_unit=>cuba_divonne_print_to_unit
     procedure,nopass::get_type=>cuba_divonne_get_type
     ! overridden cuba procedures
     procedure :: integrate_nd => integrate_divonne
     procedure :: integrate_userdata => integrate_divonne_userdata
     procedure :: copy=>cuba_divonne_copy
     procedure :: set_deferred => cuba_divonne_set_deferred
  end type cuba_divonne_type

!!$  type, extends(cuba_class) :: cuba_vegas_type
!!$     private
!!$     integer :: nstart = 500
!!$     integer :: nincrease = 1000
!!$     integer :: nbatch = 1000
!!$     integer :: gridno = 0
!!$     character(len=8),pointer :: statefile => null()
!!$   contains
!!$     ! overridden serializable procedures
!!$     procedure::write_to_marker=>cuba_vegas_write_to_marker
!!$     procedure::read_from_marker=>cuba_vegas_read_from_marker
!!$     procedure::print_to_unit=>cuba_vegas_print_to_unit
!!$     procedure,nopass::get_type=>cuba_vegas_get_type
!!$     ! overridden cuba procedures
!!$     procedure :: integrate_nd => integrate_vegas
!!$     procedure :: integrate_userdata => integrate_vegas_userdata
!!$     procedure :: copy=>cuba_vegas_copy
!!$     procedure :: set_deferred => cuba_vegas_set_deferred
!!$  end type cuba_vegas_type

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Interface Definitions !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  interface
     subroutine integrand_interface(dim_x, x, dim_f, f,userdata)
       use kinds !NODEP!
       use muli_momentum
       integer, intent(in) :: dim_x, dim_f
       real(kind=drk), dimension(dim_x), intent(in) :: x
       real(kind=drk), dimension(dim_f), intent(out) :: f
       class(transversal_momentum_type), intent(in) :: userdata
     end subroutine integrand_interface
  end interface
  interface
     subroutine cuba_copy_interface(this,source)
       import :: cuba_class
       class(cuba_class),intent(out)::this
       class(cuba_class),intent(in)::source
     end subroutine cuba_copy_interface
     subroutine ca_plain(this)
       import :: cuba_class
       class(cuba_class) :: this 
     end subroutine ca_plain
     subroutine integrate_interface(this, integrand)
       import :: cuba_class
       class(cuba_class),intent(inout) :: this  
       interface
          subroutine integrand(dim_x, x, dim_f, f,userdata)
            use kinds !NODEP!
            use muli_momentum
            integer, intent(in) :: dim_x, dim_f
            real(kind=drk), dimension(dim_x), intent(in) :: x
            real(kind=drk), dimension(dim_f), intent(out) :: f
            class(transversal_momentum_type), intent(in) :: userdata
          end subroutine integrand
       end interface
     end subroutine integrate_interface
  end interface
  interface
     subroutine integrate_userdata_interface(this, integrand,userdata)
       use muli_momentum
       import :: cuba_class
       class(cuba_class),intent(inout) :: this  
       interface
          subroutine integrand(dim_x, x, dim_f, f,userdata)
            use kinds !NODEP!
            use muli_momentum
            integer, intent(in) :: dim_x, dim_f
            real(kind=drk), dimension(dim_x), intent(in) :: x
            real(kind=drk), dimension(dim_f), intent(out) :: f
            class(transversal_momentum_type), intent(in) :: userdata
          end subroutine integrand
       end interface
       class(transversal_momentum_type),intent(in)::userdata
     end subroutine integrate_userdata_interface
  end interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Module Procedure Definition !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Type Bound Procedures for cuba_class !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine cuba_write_to_marker(this,marker,status)
    class(cuba_class),intent(in)::this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    call marker%mark_begin("cuba_class")
    call marker%mark("dim_x",this%dim_x)
    call marker%mark("dim_f",this%dim_f)    
    call marker%mark("eps_rel",this%eps_rel)
    call marker%mark("eps_abs",this%eps_abs)
    call marker%mark("flags",this%flags)    
    call marker%mark("min_eval",this%min_eval)
    call marker%mark("max_eval",this%max_eval)
    call marker%mark("neval",this%neval)
    call marker%mark("fail",this%fail)
    call marker%mark("nregions",this%nregions)
    if(allocated(this%integral))then
       call marker%mark("integral",this%integral)
    else
       call marker%mark_null("integral")
    end if
    if(allocated(this%error))then
       call marker%mark("error",this%error)
    else
       call marker%mark_null("error")

    end if
    if(allocated(this%prob))then
       call marker%mark("prob",this%prob)
    else
       call marker%mark_null("prob")
    end if    
    call marker%mark_null("cuba_class")
  end subroutine cuba_write_to_marker

  subroutine cuba_read_from_marker(this,marker,status)
    class(cuba_class),intent(out) :: this
    class(marker_type), intent(inout) :: marker
    integer(kind=dik),intent(out)::status
    call marker%pick_begin("CUBA_CLASS",status=status)
    call marker%pick("dim_x",this%dim_x,status)
    call marker%pick("dim_f",this%dim_f,status)
    call marker%pick("eps_rel",this%eps_rel,status)
    call marker%pick("eps_abs",this%eps_abs,status)
    call marker%pick("flags",this%flags,status)
    call marker%pick("min_eval",this%min_eval,status)
    call marker%pick("max_eval",this%max_eval,status)
    call marker%pick("neval",this%neval,status)
    call marker%pick("fail",this%fail,status)
    call marker%pick("nregions",this%nregions,status)
    call marker%verify_nothing("integral",status)
    if(allocated(this%integral))deallocate(this%integral)
    if(status==serialize_ok)then
       allocate(this%integral(this%dim_f))
       call marker%pick("integral",this%integral,status)
    end if
    call marker%verify_nothing("error",status)
    if(allocated(this%error))deallocate(this%error)
    if(status==serialize_ok)then
       allocate(this%error(this%dim_f))
       call marker%pick("error",this%error,status)
    end if
    call marker%verify_nothing("prob",status)
    if(allocated(this%prob))deallocate(this%prob)
    if(status==serialize_ok)then
       allocate(this%prob(this%dim_f))
       call marker%pick("prob",this%prob,status)
    end if
    call marker%pick_end("cuba_class",status)
  END SUBROUTINE cuba_read_from_marker

  subroutine cuba_print_to_unit(this,unit,parents,components,peers)
    class(cuba_class),intent(in) :: this
    INTEGER, INTENT(IN) :: unit
    integer(kind=dik),intent(in)::parents,components,peers
    character(11)::n
    write(n,'("(",I2,"(E12.4))")')this%dim_f
    write(unit,'("Components of cuba_class:")')
    write(unit,'("Parameters:")')
    write(unit,'("dim_f:     ",I10)')   this%dim_f
    write(unit,'("dim_x:     ",I10)')   this%dim_x
    call this%userdata%print_to_unit(unit,parents,components-1,peers)
    write(unit,'("eps_rel:   ",E10.4)') this%eps_rel
    write(unit,'("eps_abs:   ",E10.4)') this%eps_abs
    write(unit,'("flags:     ",I10)')   this%flags
    write(unit,'("seed:      ",I10)')   this%seed
    write(unit,'("min_eval:  ",I10)')   this%min_eval
    write(unit,'("max_eval:  ",I10)')   this%max_eval  
    write(unit,'("Results:")')
    write(unit,'("neval:     ",I10)')   this%neval
    write(unit,'("fail:      ",I10)')   this%fail
    write(unit,'("integral:  ")',advance="no")
    write(unit,fmt=n)this%integral
    write(unit,'("error:     ")',advance="no")
    write(unit,fmt=n)this%error
    write(unit,'("prob:      ")',advance="no")
    write(unit,fmt=n)this%prob
    write(unit,'("time:      ",E10.4)') this%stop_time-this%start_time
    !    write(unit,'("time:      ",E10.4)') this%run_time
  end subroutine cuba_print_to_unit

! new procedures
  
  subroutine cuba_integrate_associated(this)
    class(cuba_class),intent(inout)::this
    call cuba_integrate_with_timer(this,this%integrand)
  end subroutine cuba_integrate_associated

  subroutine cuba_integrate_with_timer(this,integrand)
    class(cuba_class),intent(inout)::this
    procedure(integrand_interface)::integrand
    call cpu_time(this%start_time)
    call this%integrate(integrand)
    call cpu_time(this%stop_time)
    this%run_time=this%run_time+this%stop_time-this%start_time
  end subroutine cuba_integrate_with_timer

  subroutine cuba_reset_timer(this)
    class(cuba_class),intent(inout)::this
    this%start_time=0D0
    this%stop_time=0D0
    this%run_time=0D0
  end subroutine cuba_reset_timer

  subroutine cuba_get_integral_array(this,integral)
    class(cuba_class) :: this
    real(kind=drk),intent(out),dimension(:) :: integral
    integral=this%integral
  end subroutine cuba_get_integral_array

  subroutine cuba_get_integral_1(this,integral)
    class(cuba_class) :: this
    real(kind=drk),intent(out) :: integral
    integral=this%integral(1)
  end subroutine cuba_get_integral_1

  subroutine cuba_dealloc_dim_f(this)
    class(cuba_class) :: this
    !      print '("cuba_dealloc_dim_f...")'
    if (allocated(this%integral)) then
       deallocate(this%integral)
    end if
    if (allocated(this%error)) then
       deallocate(this%error)
    end if
    if (allocated(this%prob)) then
       deallocate(this%prob)
    end if
    !      print '("done")'
  end subroutine cuba_dealloc_dim_f

  subroutine cuba_dealloc(this)
    class(cuba_class) :: this
    call this%dealloc_dim_f
  end subroutine cuba_dealloc

  subroutine cuba_alloc_dim_f(this)
    class(cuba_class) :: this
    call this%dealloc_dim_f()
    allocate(this%integral(this%dim_f))
    allocate(this%error(this%dim_f))
    allocate(this%prob(this%dim_f))
  end subroutine cuba_alloc_dim_f

  subroutine cuba_alloc(this)
    class(cuba_class) :: this
    call this%alloc_dim_f
  end subroutine cuba_alloc

  subroutine cuba_set_common(this,dim_x,dim_f,eps_rel,eps_abs,flags,seed,min_eval,max_eval,integrand,userdata)
    class(cuba_class),intent(inout) :: this
    integer,intent(in),optional :: dim_x,dim_f,flags,min_eval,max_eval,seed
    real(kind=drk),intent(in),optional :: eps_rel,eps_abs
    type(transversal_momentum_type),intent(in),optional :: userdata
    procedure(integrand_interface),optional::integrand
    if(present(dim_x))then
       call this%set_dim_x(dim_x)
    end if
    if(present(dim_f))then
       call this%set_dim_f(dim_f)
    end if
    if(present(flags))then
       this%flags=flags
    end if
    if(present(seed))then
       this%seed=seed
    end if    
    if(present(min_eval))then
       this%min_eval=min_eval
    end if
    if(present(max_eval))then
       if(max_eval<max_maxeval)then
          this%max_eval=max_eval
       else
          print '("cuba_set_common: Value of max_eval is too large.")'
          this%max_eval=max_maxeval
       end if
    end if
    if(present(eps_rel))then
       this%eps_rel=eps_rel
    end if
    if(present(eps_abs))then
       this%eps_abs=eps_abs
    end if
    if(present(integrand))this%integrand=>integrand
    if(present(userdata))this%userdata=userdata
  end subroutine cuba_set_common

  subroutine cuba_set_dim_f(this,new_dim_f)
    class(cuba_class) :: this
    integer,intent(in) :: new_dim_f
    if (new_dim_f>0) then
       this%dim_f = new_dim_f
       call this%alloc_dim_f
    else
       write (*,'("cuba_set_dim_f: New value for dim_f is negative. dim_f is not set.")')
    end if
  end subroutine cuba_set_dim_f

  subroutine cuba_set_dim_x(this,new_dim_x)
    class(cuba_class) :: this
    integer,intent(in) :: new_dim_x
    if (new_dim_x>0) then
       this%dim_x = new_dim_x
    else
       write (*,'("cuba_set_dim_x: New value for dim_x is negative. dim_x is not set.")')
    end if
  end subroutine cuba_set_dim_x

  subroutine cuba_copy_common(this,source)
    class(cuba_class),intent(out) :: this
    class(cuba_class),intent(in) :: source
    this%dim_x = source%dim_x
    this%dim_f = source%dim_f
    this%eps_rel = source%eps_rel
    this%eps_abs = source%eps_abs
    this%flags = source%flags
    this%min_eval = source%min_eval
    this%max_eval = source%max_eval
    call this%alloc()
  end subroutine cuba_copy_common

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! type bound procedures for vegas_type !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!$  subroutine cuba_vegas_write_to_marker(this,marker,status)
!!$    class(cuba_vegas_type),intent(in) :: this
!!$    class(marker_type), intent(inout) :: marker
!!$    integer(kind=dik),intent(out)::status
!!$    call marker%mark_begin("cuba_vegas_type")
!!$    call cuba_write_to_marker(this,marker,status)
!!$    call marker%mark("nstart",this%nstart)
!!$    call marker%mark("nincrease",this%nincrease)
!!$    call marker%mark_null("cuba_vegas_type")
!!$  end subroutine cuba_vegas_write_to_marker
!!$
!!$  subroutine cuba_vegas_read_from_marker(this,marker,status)
!!$    class(cuba_vegas_type),intent(out) :: this
!!$    class(marker_type), intent(inout) :: marker
!!$    integer(kind=dik),intent(out)::status
!!$    call marker%pick_begin("cuba_vegas_type",status=status)
!!$    call cuba_read_from_marker(this,marker,status)
!!$    call marker%pick("nstart",this%nstart,status)
!!$    call marker%pick("nincrease",this%nincrease,status)
!!$    call marker%pick_end("cuba_vegas_type",status)
!!$  end subroutine cuba_vegas_read_from_marker
!!$
!!$  subroutine cuba_vegas_print_to_unit(this,unit,parents,components,peers)
!!$    class(cuba_vegas_type),intent(in) :: this
!!$    INTEGER, INTENT(IN) :: unit
!!$    integer(kind=dik),intent(in)::parents,components,peers
!!$    if(parents>0)call cuba_print_to_unit(this,unit,parents-1,components,peers)
!!$    write(unit,'("Components of cuba_vegas_type:")')
!!$    write(unit,'("nstart:    ",I10)')   this%nstart
!!$    write(unit,'("nincrease: ",I10)')   this%nincrease
!!$    write(unit,'("nbatch:    ",I10)')   this%nbatch
!!$    write(unit,'("gridno:    ",I10)')   this%gridno
!!$    if(associated(this%statefile))then
!!$       write(unit,'("statefile:",a)')   this%statefile
!!$    else
!!$       write(unit,'("statefile:",a)')   "not associated"
!!$    end if
!!$  end subroutine cuba_vegas_print_to_unit
!!$
!!$  pure subroutine cuba_vegas_get_type(type)
!!$    character(:),allocatable,intent(out)::type
!!$    allocate(type,source="cuba_vegas_type")
!!$  end subroutine cuba_vegas_get_type
!!$
!!$  subroutine cuba_vegas_set_deferred(this,n_start,n_increase,nbatch,gridno,statefile)
!!$    class(cuba_vegas_type),intent(inout) :: this  
!!$    integer,intent(in),optional :: n_start,n_increase,nbatch,gridno
!!$    character(len=*),intent(in),target,optional::statefile
!!$    if(present(n_start))this%nstart=n_start
!!$    if(present(n_increase))this%nincrease=n_increase
!!$    if(present(nbatch))this%nbatch=nbatch
!!$    if(present(gridno))this%gridno=gridno
!!$    if(present(statefile))this%statefile=>statefile
!!$  end subroutine cuba_vegas_set_deferred
!!$
!!$  subroutine cuba_vegas_copy(this,source)
!!$    class(cuba_vegas_type),intent(out) :: this
!!$    class(cuba_class),intent(in) :: source
!!$    select type(source)
!!$    class is (cuba_vegas_type)
!!$       call this%copy_common(source)
!!$       this%nstart=source%nstart
!!$       this%nincrease=source%nincrease
!!$    class default
!!$       print *,"cuba_vegas_copy: type of source is not type compatible with cuba_vegas_type."
!!$    end select
!!$  end subroutine cuba_vegas_copy
!!$
!!$  subroutine integrate_vegas(this,integrand)
!!$    class(cuba_vegas_type),intent(inout) :: this
!!$    procedure(integrand_interface)::integrand
!!$    !      print '("vegas")'
!!$    call vegas(&
!!$         this%dim_x, &
!!$         this%dim_f, &
!!$         integrand, &
!!$         this%userdata, &
!!$         this%eps_rel, &
!!$         this%eps_abs, &
!!$         this%flags, &
!!$         this%seed, &
!!$         this%min_eval, &
!!$         this%max_eval, &
!!$         this%nstart, &
!!$         this%nincrease, &
!!$         this%nbatch, &
!!$         this%gridno, &
!!$         this%statefile, &
!!$         this%neval, &
!!$         this%fail, &
!!$         this%integral, &
!!$         this%error, &
!!$         this%prob)
!!$  end subroutine integrate_vegas
!!$
!!$  subroutine integrate_vegas_userdata(this,integrand,userdata)
!!$    class(cuba_vegas_type),intent(inout) :: this
!!$    procedure(integrand_interface)::integrand
!!$    class(transversal_momentum_type),intent(in)::userdata
!!$    !      print '("vegas")'
!!$    call vegas(&
!!$         this%dim_x, &
!!$         this%dim_f, &
!!$         integrand, &
!!$         userdata, &
!!$         this%eps_rel, &
!!$         this%eps_abs, &
!!$         this%flags, &
!!$         this%seed, &
!!$         this%min_eval, &
!!$         this%max_eval, &
!!$         this%nstart, &
!!$         this%nincrease, &
!!$         this%nbatch, &
!!$         this%gridno, &
!!$         this%statefile, &
!!$         this%neval, &
!!$         this%fail, &
!!$         this%integral, &
!!$         this%error, &
!!$         this%prob)
!!$  end subroutine integrate_vegas_userdata

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! type bound procedures for cuba_suave_type !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!$  subroutine cuba_suave_write_to_marker(this,marker,status)
!!$    class(cuba_suave_type),intent(in) :: this
!!$    class(marker_type), intent(inout) :: marker
!!$    integer(kind=dik),intent(out)::status
!!$    call marker%mark_begin("cuba_suave_type")
!!$    call cuba_write_to_marker(this,marker,status)
!!$    call marker%mark("nnew",this%nnew)
!!$    call marker%mark("flatness",this%flatness)
!!$    call marker%mark_null("cuba_suave_type")
!!$  end subroutine cuba_suave_write_to_marker
!!$
!!$  subroutine cuba_suave_read_from_marker(this,marker,status)
!!$    class(cuba_suave_type),intent(out) :: this
!!$    class(marker_type), intent(inout) :: marker
!!$    integer(kind=dik),intent(out)::status
!!$    call marker%pick_begin("cuba_suave_type",status=status)
!!$    call cuba_read_from_marker(this,marker,status)
!!$    call marker%pick("nnew",this%nnew,status)
!!$    call marker%pick("flatnes",this%flatness,status)
!!$    call marker%pick_end("cuba_suave_type",status)
!!$  end subroutine cuba_suave_read_from_marker
!!$
!!$  subroutine cuba_suave_print_to_unit(this,unit,parents,components,peers)
!!$    class(cuba_suave_type),intent(in) :: this
!!$    INTEGER, INTENT(IN) :: unit
!!$    integer(kind=dik),intent(in)::parents,components,peers
!!$    if(parents>0)call cuba_print_to_unit(this,unit,parents-1,components,peers)
!!$    write(unit,'("Components of cuba_suave_type:")')
!!$    write(unit,'("nnew:      ",I10)')   this%nnew
!!$    write(unit,'("flatness:  ",I10)')   this%flatness
!!$  end subroutine cuba_suave_print_to_unit
!!$
!!$  pure subroutine cuba_suave_get_type(type)
!!$    character(:),allocatable,intent(out)::type
!!$    allocate(type,source="cuba_suave_type")
!!$  end subroutine cuba_suave_get_type
!!$
!!$  subroutine integrate_suave(this,integrand)
!!$    class(cuba_suave_type),intent(inout) :: this  
!!$    procedure(integrand_interface)::integrand
!!$    !      print '("suave")'
!!$    call suave(&
!!$         this%dim_x, &
!!$         this%dim_f, &
!!$         integrand, &
!!$         this%userdata, &
!!$         this%eps_rel, &
!!$         this%eps_abs, &
!!$         this%flags, &
!!$         this%seed, &
!!$         this%min_eval, &
!!$         this%max_eval, &
!!$         this%nnew, &
!!$         this%flatness, &
!!$         this%nregions, &
!!$         this%neval, &
!!$         this%fail, &
!!$         this%integral, &
!!$         this%error, &
!!$         this%prob)
!!$  end subroutine integrate_suave
!!$
!!$   subroutine integrate_suave_userdata(this,integrand,userdata)
!!$    class(cuba_suave_type),intent(inout) :: this  
!!$    procedure(integrand_interface)::integrand
!!$    class(transversal_momentum_type),intent(in)::userdata
!!$    !      print '("suave")'
!!$    call suave(&
!!$         this%dim_x, &
!!$         this%dim_f, &
!!$         integrand, &
!!$         userdata, &
!!$         this%eps_rel, &
!!$         this%eps_abs, &
!!$         this%flags, &
!!$         this%seed, &
!!$         this%min_eval, &
!!$         this%max_eval, &
!!$         this%nnew, &
!!$         this%flatness, &
!!$         this%nregions, &
!!$         this%neval, &
!!$         this%fail, &
!!$         this%integral, &
!!$         this%error, &
!!$         this%prob)
!!$  end subroutine integrate_suave_userdata
!!$
!!$  subroutine cuba_suave_copy(this,source)
!!$    class(cuba_suave_type),intent(out) :: this
!!$    class(cuba_class),intent(in) :: source
!!$    select type(source)
!!$    class is (cuba_suave_type)
!!$       call this%copy_common(source)
!!$       this%nnew = source%nnew
!!$       this%flatness = source%flatness
!!$    class default
!!$       print *,"cuba_suave_copy: type of source is not type compatible with cuba_suave_type."
!!$    end select
!!$  end subroutine cuba_suave_copy


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! type bound procedures for cuba_divonne_type !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine cuba_divonne_write_to_marker(this,marker,status)
    class(cuba_divonne_type),intent(in) :: this
    class(marker_type), intent(inout) :: marker
    integer(kind=dik),intent(out)::status
    call marker%mark_begin("cuba_divonne_type")
    call cuba_write_to_marker(this,marker,status)
    call marker%mark("key1",this%key1)
    call marker%mark("key2",this%key2)
    call marker%mark("key3",this%key3)
    call marker%mark("maxpass",this%maxpass)
    call marker%mark("border",this%border)
    call marker%mark("maxchisq",this%maxchisq)
    call marker%mark("mindeviation",this%mindeviation)
    call marker%mark("ngiven",this%ngiven)
    call marker%mark("ldxgiven",this%ldxgiven)
    call marker%mark("nextra",this%nextra)
    call marker%mark("xgiven",this%xgiven)
    call marker%mark_null("cuba_divonne_type")
  end subroutine cuba_divonne_write_to_marker

  subroutine cuba_divonne_read_from_marker(this,marker,status)
    class(cuba_divonne_type),intent(out) :: this
    class(marker_type), intent(inout) :: marker
    integer(kind=dik),intent(out)::status
    call marker%pick_begin("cuba_divonne_type",status=status)
    call cuba_read_from_marker(this,marker,status)
    call marker%pick("key1",this%key1,status)
    call marker%pick("key2",this%key2,status)
    call marker%pick("key3",this%key3,status)
    call marker%pick("maxpass",this%maxpass,status)
    call marker%pick("border",this%border,status)
    call marker%pick("maxchisq",this%maxchisq,status)
    call marker%pick("mindeviation",this%mindeviation,status)
    call marker%pick("ngiven",this%ngiven,status)
    call marker%pick("ldxgiven",this%ldxgiven,status)
    call marker%pick("nextra",this%nextra,status)
    if(allocated(this%xgiven))deallocate(this%xgiven)
    allocate(this%xgiven(this%ldxgiven,this%ngiven))
    call marker%pick("xgiven",this%xgiven,status)
    call marker%pick_end("cuba_divonne_type",status)
  end subroutine cuba_divonne_read_from_marker

  subroutine cuba_divonne_print_to_unit(this,unit,parents,components,peers)
    class(cuba_divonne_type),intent(in) :: this
    INTEGER, INTENT(IN) :: unit
    integer(kind=dik),intent(in)::parents,components,peers
    if(parents>0)call cuba_print_to_unit(this,unit,parents-1,components,peers)
    write(unit,'("Components of cuba_divonne_type:")')
    write(unit,'("key1:      ",I10)')   this%key1
    write(unit,'("key2:      ",I10)')   this%key2
    write(unit,'("key3:      ",I10)')   this%key3
    write(unit,'("maxpass:   ",I10)')   this%maxpass
    write(unit,'("ngiven:    ",I10)')   this%ngiven
    write(unit,'("ldxgiven:  ",I10)')   this%ldxgiven
    write(unit,'("nextra:    ",I10)')   this%nextra
    write(unit,'("border:    ",E10.4)') this%border
    write(unit,'("maxchisq:  ",E10.4)') this%maxchisq
    write(unit,'("mindeviation:",E10.4)') this%mindeviation
    write(unit,'("xgiven:    ",2(E10.4))') this%xgiven
  end subroutine cuba_divonne_print_to_unit

  pure subroutine cuba_divonne_get_type(type)
    character(:),allocatable,intent(out)::type
    allocate(type,source="cuba_divonne_type")
  end subroutine cuba_divonne_get_type

  subroutine integrate_divonne(this,integrand)
    class(cuba_divonne_type),intent(inout) :: this  
    procedure(integrand_interface)::integrand
    !      call this%reset_output()
    !    print '("divonne")'
    !call divonne(&
    !     & this%dim_x, &
    !     & this%dim_f, &
    !     & integrand, &
    !     & this%userdata,&
    !     & this%eps_rel, &
    !     & this%eps_abs, &
    !     & this%flags, &
    !     & this%seed, &
    !     & this%min_eval, &
    !     & this%max_eval, &
    !     & this%key1, &
    !     & this%key2, &
    !     & this%key3, &
    !     & this%maxpass, &
    !     & this%border, &
    !     & this%maxchisq, &
    !     & this%mindeviation, &
    !     & this%ngiven, &
    !     & this%ldxgiven, &
    !     & this%xgiven, &
    !     & this%nextra, &
    !                            !         & this%peakfinder, &
    !     & 0,&
    !     & this%nregions, &
    !     & this%neval, &
    !     & this%fail, &
    !     & this%integral, &
    !     & this%error, &
    !     & this%prob)
  end subroutine integrate_divonne

subroutine integrate_divonne_userdata(this,integrand,userdata)
    class(cuba_divonne_type),intent(inout) :: this  
    procedure(integrand_interface)::integrand
    class(transversal_momentum_type),intent(in)::userdata
    !      call this%reset_output()
    !    print '("divonne")'
    !call divonne(&
    !     & this%dim_x, &
    !     & this%dim_f, &
    !     & integrand, &
    !     & userdata,&
    !     & this%eps_rel, &
    !     & this%eps_abs, &
    !     & this%flags, &
    !     & this%seed, &
    !     & this%min_eval, &
    !     & this%max_eval, &
    !     & this%key1, &
    !     & this%key2, &
    !     & this%key3, &
    !     & this%maxpass, &
    !     & this%border, &
    !     & this%maxchisq, &
    !     & this%mindeviation, &
    !     & this%ngiven, &
    !     & this%ldxgiven, &
    !     & this%xgiven, &
    !     & this%nextra, &
    !                            !         & this%peakfinder, &
    !     & 0,&
    !     & this%nregions, &
    !     & this%neval, &
    !     & this%fail, &
    !     & this%integral, &
    !     & this%error, &
    !     & this%prob)
  end subroutine integrate_divonne_userdata

  subroutine cuba_divonne_copy(this,source)
    class(cuba_divonne_type),intent(out) :: this
    class(cuba_class),intent(in) :: source
    select type(source)
    class is (cuba_divonne_type)
       call this%copy_common(source)
       call this%set_deferred(&
       &source%key1,&
       &source%key2,&
       &source%key3,&
       &source%maxpass,&
       &source%border,&
       &source%maxchisq,&
       &source%mindeviation,&
       &source%xgiven&
       &)
    class default
       print *,"cuba_divonne_copy: type of source is not type compatible with cuba_divonne_type."
    end select
  end subroutine cuba_divonne_copy

  subroutine cuba_divonne_set_deferred(this,key1,key2,key3,maxpass,border,maxchisq,mindeviation,xgiven,xgiven_flat)
    class(cuba_divonne_type) :: this
    integer,optional,intent(in)::key1,key2,key3,maxpass
    real(kind=drk),optional,intent(in)::border,maxchisq,mindeviation
    real(kind=drk),dimension(:,:),optional,intent(in)::xgiven
    real(kind=drk),dimension(:),optional,intent(in)::xgiven_flat
    integer,dimension(2)::s
    if(present(key1))this%key1=key1
    if(present(key2))this%key2=key2
    if(present(key3))this%key3=key3
    if(present(maxpass))this%maxpass=maxpass
    if(present(border))this%border=border
    if(present(maxchisq))this%maxchisq=maxchisq
    if(present(mindeviation))this%mindeviation=mindeviation
    if(present(xgiven))then
       if(allocated(this%xgiven))deallocate(this%xgiven)
       s=shape(xgiven)
       if(s(1)==this%dim_x)then
          allocate(this%xgiven(s(1),s(2)),source=xgiven)
          this%ldxgiven=s(1)
          this%ngiven=s(2)
       else
          print *,"cuba_divonne_set_deferred: shape of xgiven is not [dim_x,:]."
          this%ngiven=0
       end if
    end if
    if(present(xgiven_flat))then
       if(allocated(this%xgiven))deallocate(this%xgiven)
       if(mod(size(xgiven_flat),this%dim_x)==0)then
          this%ngiven=size(xgiven_flat)/this%dim_x
          this%ldxgiven=this%dim_x
          allocate(this%xgiven(this%ldxgiven,this%ngiven))
          this%xgiven = reshape(xgiven_flat,[this%ldxgiven,this%ngiven])
       else
          print *,"cuba_divonne_set_deferred: size of xgiven_flat is no multiple of dim_x."
          this%ngiven=0
       end if
    end if
  end subroutine cuba_divonne_set_deferred

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! type bound procedures for cuba_cuhre_type !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!$  subroutine cuba_cuhre_write_to_marker(this,marker,status)
!!$    class(cuba_cuhre_type),intent(in) :: this
!!$    class(marker_type), intent(inout) :: marker
!!$    integer(kind=dik),intent(out)::status
!!$    call marker%mark_begin("cuba_cuhre_type")
!!$    call cuba_write_to_marker(this,marker,status)
!!$    call marker%mark("key",this%key)
!!$    call marker%pick_end("cuba_cuhre_type",status)
!!$  end subroutine cuba_cuhre_write_to_marker
!!$
!!$  subroutine cuba_cuhre_read_from_marker(this,marker,status)
!!$    class(cuba_cuhre_type),intent(out) :: this
!!$    class(marker_type), intent(inout) :: marker
!!$    integer(kind=dik),intent(out)::status
!!$    call marker%pick_begin("cuba_cuhre_type",status=status)
!!$    call cuba_read_from_marker(this,marker,status)
!!$    call marker%pick("key",this%key,status)
!!$    call marker%pick_end("cuba_cuhre_type",status)
!!$  end subroutine cuba_cuhre_read_from_marker
!!$
!!$  subroutine cuba_cuhre_print_to_unit(this,unit,parents,components,peers)
!!$    class(cuba_cuhre_type),intent(in) :: this
!!$    integer, intent(in) :: unit
!!$    integer(kind=dik),intent(in)::parents,components,peers
!!$    if(parents>0)call cuba_print_to_unit(this,unit,parents-1,components,peers)
!!$    write(unit,'("Components of cuba_cuhre_type:")')
!!$    write(unit,'("key:       ",I10)')   this%key
!!$  end subroutine cuba_cuhre_print_to_unit
!!$
!!$  pure subroutine cuba_cuhre_get_type(type)
!!$    character(:),allocatable,intent(out)::type
!!$    allocate(type,source="cuba_cuhre_type")
!!$  end subroutine cuba_cuhre_get_type
!!$
!!$  subroutine integrate_cuhre(this,integrand)
!!$    class(cuba_cuhre_type),intent(inout) :: this  
!!$    procedure(integrand_interface)::integrand
!!$    !c      print '("cuhre")'
!!$    call cuhre(&
!!$         this%dim_x, &
!!$         this%dim_f, &
!!$         integrand, &
!!$         this%userdata, &
!!$         this%eps_rel, &
!!$         this%eps_abs, &
!!$         this%flags, &
!!$!         this%seed, &
!!$         this%min_eval, &
!!$         this%max_eval, &
!!$         this%key, &
!!$         this%nregions, &
!!$         this%neval, &
!!$         this%fail, &
!!$         this%integral, &
!!$         this%error, &
!!$         this%prob)
!!$  end subroutine integrate_cuhre
!!$
!!$  subroutine integrate_cuhre_userdata(this,integrand,userdata)
!!$    class(cuba_cuhre_type),intent(inout) :: this
!!$    procedure(integrand_interface)::integrand
!!$    class(transversal_momentum_type),intent(in)::userdata
!!$    !c      print '("cuhre")'
!!$    call cuhre(&
!!$         this%dim_x, &
!!$         this%dim_f, &
!!$         integrand, &
!!$         userdata, &
!!$         this%eps_rel, &
!!$         this%eps_abs, &
!!$         this%flags, &
!!$!         this%seed, &
!!$         this%min_eval, &
!!$         this%max_eval, &
!!$         this%key, &
!!$         this%nregions, &
!!$         this%neval, &
!!$         this%fail, &
!!$         this%integral, &
!!$         this%error, &
!!$         this%prob)
!!$  end subroutine integrate_cuhre_userdata
!!$
!!$  subroutine cuba_cuhre_copy(this,source)
!!$    class(cuba_cuhre_type),intent(out) :: this
!!$    class(cuba_class),intent(in) :: source
!!$    select type(source)
!!$    class is (cuba_cuhre_type)
!!$       call this%copy_common(source)
!!$       this%key=source%key
!!$    class default
!!$       print *,"cuba_cuhre_copy: type of source is not type compatible with cuba_cuhre_type."
!!$    end select
!!$  end subroutine cuba_cuhre_copy
!!$
!!$  subroutine cuba_cuhre_set_deferred(this,key)
!!$    class(cuba_cuhre_type),intent(inout) :: this  
!!$    integer, intent(in) :: key
!!$    this%key = key
!!$  end subroutine cuba_cuhre_set_deferred

end module muli_cuba

