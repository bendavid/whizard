!!! module: muli_aq
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
!!! Latest Change: 2011-06-28 11:18:42 CEST(+0200)
!!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module muli_aq
  use kinds !NODEP!
  use muli_basic
  use muli_cuba
  use muli_trapezium
  use muli_fibonacci_tree
  implicit none

!!! This file contains the module "muli_aq" which is an acronym for adaptive
!!! quadrature. All you have to do is to extend the abstract type "aq_class"
!!! and define the deferred procedure "evaluate". muli_aq calls evaluate to
!!! evaluate the integrand at any point in the given range. I have tried to
!!! use a procedure pointer instead of the deferred tbp, but no compiler was
!!! able to handle procedure pointers plus cuba was not able to handle 
!!! parameters, to wit dimensions of the integrand that should not get
!!! integrated, so I switched to this odd way of using inheritance. Meanwhile
!!! these problems got solved and I could go for a more straight forward
!!! solution, but it works fine as it is.
!!! 
!!! aq_class uses muli_trapezium to approximate the integral. It still has to
!!! do the subdivision of segments and has to check whether the precision goal
!!! is reached.
!!!
!!! Finally the result is written do disc using the serialization framework 
!!! defined in muli_basic. Since QCD is not expected to change frequently,
!!! the only reason to regenerate this function is a change of the used pdf set.
!!! Then you can read the integral from disc each time you run a simulation with
!!! the same pdf set.

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Derived Type Definitions !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  type,extends(identified_type),abstract :: aq_class
     ! private
     logical :: is_deferred_initialised = .false.
     logical :: is_error_tree_initialised = .false.
     logical :: is_goal_set = .false.
     logical :: is_initialised = .false.
     logical :: is_run = .false.
     logical :: is_goal_reached = .false.
     logical :: is_integrated = .false.
     integer(kind=dik) :: n_nodes = 0
     integer(kind=dik) :: max_nodes = 10000
     integer :: dim_integral = 1
     real(kind=double) :: abs_error_goal = 0D0
     real(kind=double) :: rel_error_goal = 0.1D0
     real(kind=double) :: scaled_error_goal = 0.0D0
     real(kind=double) :: integral = 1D0
     real(kind=double) :: integral_error = 0D0
     real(kind=double),dimension(2) :: region = (/0D0,1D0/)
     real(kind=double),dimension(:,:),allocatable :: convergence
!time stamps
     real(kind=double) :: total_time = 0
     real(kind=double) :: loop_time = 0
     real(kind=double) :: int_time = 0
     real(kind=double) :: cuba_time = 0
     real(kind=double) :: init_time = 0
     real(kind=double) :: cpu_time = 0
!these variables *must* be initialised before the main loop can be called
!additionaly the nodes and segments should be preprocessed by first_run before the main loop is called
     real(kind=double) :: error_goal = 0D0
     class(fibonacci_root_type),pointer :: err_tree => null()
     class(muli_trapezium_list_type),pointer :: int_list => null()
   contains
     ! overridden serializable_class procedures
     procedure::write_to_marker=>aq_write_to_marker
     procedure::read_from_marker=>aq_read_from_marker
     procedure::print_to_unit=>aq_print_to_unit
     procedure,nopass::get_type=>aq_get_type
     procedure::deserialize_from_marker=>aq_deserialize_from_marker
     ! new procedures
     procedure :: aq_initialize
     generic   :: initialize => aq_initialize
     procedure :: print_times => aq_print_times
     procedure :: write_convergence => aq_write_convergence
     ! init/ de-init
     procedure :: reset => aq_reset
     procedure :: dealloc_trees => aq_dealloc_trees
     procedure :: finalize => aq_dealloc_trees
     procedure :: init_error_tree => aq_init_error_tree
     procedure :: set_rel_goal => aq_set_rel_goal
     procedure :: set_abs_goal => aq_set_abs_goal
     procedure :: set_goal => aq_set_goal
     procedure :: check_init => aq_check_init
     ! calculation
     procedure :: main_loop => aq_main_loop
     procedure :: run => aq_run
     procedure :: integrate => aq_integrate
     ! deferred
     procedure(evaluate_if),deferred :: evaluate
!       procedure(evaluate_ratios_if),deferred :: evaluate_ratios
  end type aq_class

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Interface Definitions !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  interface
     subroutine evaluate_if(this,x,y)
       use kinds!NODEP!
       import aq_class
       class(aq_class),intent(inout) :: this
       real(kind=double), intent(in) :: x
       real(kind=double), intent(out) ,dimension(:):: y
     end subroutine evaluate_if

!!$     subroutine evaluate_ratios_if(this,cont)
!!$       use kinds
!!$       use lin_approx_tree_module,only:muli_trapezium_type
!!$       import aq_class
!!$       class(aq_class) :: this
!!$       class(muli_trapezium_type),intent(inout),pointer :: cont
!!$     end subroutine evaluate_ratios_if
  end interface

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Type Bound Procedures for aq_class !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !overridden serializable_class procedures

  subroutine aq_write_to_marker(this,marker,status)
    class(aq_class), intent(in) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status  
    class(serializable_class),pointer::ser
    call marker%mark_begin("aq_class")
    call identified_write_to_marker(this,marker,status)
    call marker%mark("is_deferred_initialised",this&
         &%is_deferred_initialised)
    call marker%mark("is_error_tree_initialised",this&
         &%is_error_tree_initialised)
    call marker%mark("is_goal_set",this%is_goal_set)
    call marker%mark("is_initialised",this%is_initialised)
    call marker%mark("is_run",this%is_run)
    call marker%mark("is_goal_reached",this%is_goal_reached)
    call marker%mark("is_integrated",this%is_integrated)
    call marker%mark("n_nodes",this%n_nodes)
    call marker%mark("max_nodes",this%max_nodes)
    call marker%mark("dim_integral",this%dim_integral)    
    call marker%mark("abs_error_goal",this%abs_error_goal)
    call marker%mark("rel_error_goal",this%rel_error_goal)
    call marker%mark("scaled_error_goal",this%scaled_error_goal)
    call marker%mark("error_goal",this%error_goal)
    call marker%mark("integral",this%integral)
    call marker%mark("integral_error",this%integral_error)
    call marker%mark("region",this%region(1:2))
    ser=>this%err_tree
    call marker%mark_pointer("err_tree",ser)
    ser=>this%int_list
    call marker%mark_pointer("int_list",ser)
    call marker%mark_end("aq_class")
  end subroutine aq_write_to_marker

  subroutine aq_read_from_marker(this,marker,status)
    class(aq_class), intent(out) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status  
    class(serializable_class),pointer::ser
    call marker%pick_begin("aq_class",status=status)
    call identified_read_from_marker(this,marker,status)
    call marker%pick("is_deferred_initialised",this%is_deferred_initialised&
         &,status)
    call marker%pick("is_error_tree_initialised",this&
         &%is_error_tree_initialised,status)
    call marker%pick("is_goal_set",this%is_goal_set,status)
    call marker%pick("is_initialised",this%is_initialised,status)
    call marker%pick("is_run",this%is_run,status)
    call marker%pick("is_goal_reached",this%is_goal_reached,status)
    call marker%pick("is_integrated",this%is_integrated,status)
    call marker%pick("n_nodes",this%n_nodes,status)
    call marker%pick("max_nodes",this%max_nodes,status)
    call marker%pick("dim_integral",this%dim_integral,status)    
    call marker%pick("abs_error_goal",this%abs_error_goal,status)
    call marker%pick("rel_error_goal",this%rel_error_goal,status)
    call marker%pick("scaled_error_goal",this%scaled_error_goal,status)
    call marker%pick("error_goal",this%error_goal,status)
    call marker%pick("integral",this%integral,status)
    call marker%pick("integral_error",this%integral_error,status)
    call marker%pick("region",this%region(1:2),status)
    call marker%pick_pointer("err_tree",ser)
    if(associated(ser))then
       select type(ser)
       class is (fibonacci_root_type)
          this%err_tree=>ser
       class default
          nullify(this%err_tree)
       end select
    end if
    call marker%pick_pointer("int_list",ser)
    if(associated(ser))then
       select type(ser)
       class is (muli_trapezium_list_type)
          this%int_list=>ser
       class default
          nullify(this%int_list)
       end select
    end if
    call marker%pick_end("aq_class",status)
  end subroutine aq_read_from_marker

  subroutine aq_print_to_unit(this,unit,parents,components,peers)
    class(aq_class),intent(in)::this
    integer,intent(in)::unit
    integer(kind=dik),intent(in)::parents,components,peers
    integer::ite
    class(serializable_class),pointer::ser
    if(parents>0)call identified_print_to_unit(this,unit,parents-1,components&
         &,peers)
    write(unit,'("Components of aq_class")')
    write(unit,'(a,L1)')"Deferred class initialised: ",this&
         &%is_deferred_initialised
    write(unit,'(a,L1)')"Error tree initialised:     ",this&
         &%is_error_tree_initialised
    write(unit,'(a,L1)')"Accuracy goal set:          ",this%is_goal_set
    write(unit,'(a,L1)')"Ready for run:              ",this%is_initialised
    write(unit,'(a,L1)')"Is run:                     ",this%is_run
    write(unit,'(a,L1)')"Accuracy goal reached:      ",this%is_goal_reached
    write(unit,'(a,L1)')"Integral calculated:        ",this%is_integrated
    write(unit,'(a,I10)')"Number of nodes:            ",this%n_nodes
    write(unit,'(a,I10)')"Maximal number of nodes:    ",this%max_nodes
    write(unit,'(a,I10)')"Dimension of integral:      ",this%dim_integral
    write(unit,'(a,E20.10)')"Given abs. error goal: ",this%abs_error_goal
    write(unit,'(a,E20.10)')"Given rel. error goal: ",this%rel_error_goal
    write(unit,'(a,E20.10)')"Guessed abs error goal:",this%scaled_error_goal
    write(unit,'(a,E20.10)')"Actual abs error goal: ",this%error_goal
    write(unit,'(a,E20.10)')"Integral               ",this%integral
    write(unit,'(a,E20.10)')"Estimated abs. error:  ",this%integral_error
!    if(this%integral==0D0)then
!       write(unit,'(a,E20.10)')"Estimated rel. error:  ",this%integral_error&
!            &/this%integral
!    else
!       write(unit,'(a,E20.10)')"Estimated rel. error:  INF"
!    end if
    write(unit,'(a,E10.5,a,E10.5,a)')"Integration region =  (",this%region(1)&
         &," : ",this%region(2),")"
    ser=>this%err_tree
    call serialize_print_comp_pointer(ser,unit,parents,components,peers&
         &,"error tree")
    ser=>this%int_list
    call serialize_print_comp_pointer(ser,unit,parents,components,peers&
         &,"integral list")
  end subroutine aq_print_to_unit

  pure subroutine aq_get_type(type)
    character(:),allocatable,intent(out)::type
    allocate(type,source="aq_type")
  end subroutine aq_get_type
  
  subroutine aq_deserialize_from_marker(this,name,marker)
    class(aq_class),intent(out)::this
    character(*),intent(in)::name
    class(marker_type),intent(inout)::marker
    class(serializable_class),pointer::ser
    allocate(muli_trapezium_type::ser)
    call marker%push_reference(ser)
    allocate(fibonacci_root_type::ser)
    call marker%push_reference(ser)
    allocate(fibonacci_leave_type::ser)
    call marker%push_reference(ser)
    allocate(fibonacci_node_type::ser)
    call marker%push_reference(ser)
    call serializable_deserialize_from_marker(this,name,marker)
    call marker%pop_reference(ser)
    deallocate(ser)
    call marker%pop_reference(ser)
    deallocate(ser)
    call marker%pop_reference(ser)
    deallocate(ser)
    call marker%pop_reference(ser)
    deallocate(ser)
  end subroutine aq_deserialize_from_marker

  ! new procedures
  
  subroutine aq_initialize(this,id,name,goal,max_nodes,dim,init)
    class(aq_class),intent(out) :: this
    integer(kind=dik),intent(in)::id,max_nodes
    integer,intent(in)::dim
    character,intent(in)::name
    real(kind=double)::goal
    real(kind=double),dimension(:),intent(in)::init
    call identified_initialize(this,id,name)
    this%rel_error_goal = goal!1d-4
    this%max_nodes=max_nodes
    call this%init_error_tree(dim,init)
  end subroutine aq_initialize

  subroutine aq_print_times(this)
    class(aq_class),intent(in) :: this
    print '(a,E20.10)',"Initialization time:  ",this%init_time
    print '(a,E20.10)',"Main loop time:       ",this%loop_time
    print '(a,E20.10)',"Integration time:     ",this%int_time
    print '(a,E20.10)',"Overall run time:     ",this%total_time
    print '(a,E20.10)',"Cuba integration time:",this%cuba_time
  end subroutine aq_print_times

  subroutine aq_write_convergence(this,unit)
    class(aq_class),intent(in) :: this
    integer,intent(in)::unit
    integer,dimension(2)::s
    integer::node
    if(allocated(this%convergence))then
       s=shape(this%convergence)
       do node=1,s(2)
          write(unit,fmt=*)node,this%convergence(1:2,node)
       end do
    end if
  end subroutine aq_write_convergence

  ! init/ de-init

  subroutine aq_reset(this)
    class(aq_class) :: this
    this%is_deferred_initialised = .false.
    this%is_error_tree_initialised = .false.
    this%is_goal_set = .false.
    this%is_initialised = .false.
    this%is_run = .false.
    this%is_goal_reached = .false.
    this%is_integrated = .false.
    this%n_nodes = 0
    this%max_nodes = 10000
    this%dim_integral=1
    this%abs_error_goal = 1D0
    this%rel_error_goal = 0.1D0
    this%scaled_error_goal = 0.0D0
    this%error_goal = 0.0D0
    this%integral = 0D0
    this%integral_error = 0D0
    this%region = (/0D0,1D0/)
    this%total_time = 0
    this%loop_time = 0
    this%int_time = 0
    this%init_time = 0
    call this%dealloc_trees()
  end subroutine aq_reset

  subroutine aq_check_init(this)
    class(aq_class) :: this
    this%is_initialised = this%is_error_tree_initialised .and. this%is_deferred_initialised
  end subroutine aq_check_init

  subroutine aq_dealloc_trees(this)
    class(aq_class) :: this
    if(associated(this%err_tree))then
       call this%err_tree%deallocate_all()
       deallocate(this%err_tree)
    end if
    if(associated(this%int_list))then
       call this%int_list%finalize()
       deallocate(this%int_list)
    end if
  end subroutine aq_dealloc_trees

  subroutine aq_init_error_tree(this,dim_integral,x_array)
    class(aq_class) :: this
    integer,intent(in)::dim_integral
    real(kind=double), dimension(:), intent(in) :: x_array
    real(kind=double) :: center
    real(kind=double), dimension(:),allocatable::l_val,c_val,r_val
    class(muli_trapezium_type),pointer :: left_node => null()
    class(muli_trapezium_type),pointer :: right_node => null()
    integer :: x_size,pos
!    print '("Entemarker aq_init_error_tree...")'
    call cpu_time(this%init_time)
    this%is_initialised=.false.
    this%integral=0D0
    this%dim_integral=dim_integral
    x_size = size(x_array)
    if (x_size<2) then
         write (*,'("aq_init_error_tree: I need at least two real values")')
    else
       allocate(l_val(0:dim_integral-1))
       allocate(c_val(0:dim_integral-1))
       allocate(r_val(0:dim_integral-1))
       this%region=(/x_array(1),x_array(x_size)/)
       if (x_size<3) then
          center=(x_array(2)-x_array(1))/2D0
          call this%evaluate(x_array(1),l_val)
          call this%evaluate(center,    c_val)
          call this%evaluate(x_array(2),r_val)
          allocate(left_node)
          call left_node%initialize(&
               &dim=dim_integral,&
               &r_position=center,&
               &d_position=center-x_array(1))
          call left_node%set_r_value(c_val)
          call left_node%set_d_value(c_val-l_val)
          allocate(right_node)
          call right_node%initialize(&
               &dim=dim_integral,&
               &r_position=x_array(2),&
               &d_position=x_array(2)-center)
          call right_node%set_r_value(r_val)
          call right_node%set_d_value(r_val-c_val)
       else
          call this%evaluate(x_array(1),l_val)
          call this%evaluate(x_array(2),c_val)
          call this%evaluate(x_array(3),r_val)
          allocate(left_node)
          call left_node%initialize(&
               &dim=dim_integral,&
               &r_position=x_array(2),&
               &d_position=x_array(2)-x_array(1))
          call left_node%set_r_value(c_val)
          call left_node%set_d_value(c_val-l_val)
          allocate(right_node)
          call right_node%initialize(&
               &dim=dim_integral,&
               &r_position=x_array(3),&
               &d_position=x_array(3)-x_array(2))
          call right_node%set_r_value(r_val)
          call right_node%set_d_value(r_val-c_val)
       end if
       call left_node%update()
       call right_node%update()
       this%integral=sum(left_node%get_d_integral()+right_node%get_d_integral())
       if (.not. associated(this%err_tree)) then
          allocate(this%err_tree)
       end if
       print *,left_node%measure()
       print *,right_node%measure()
       call this%err_tree%init_by_content(left_node,right_node)
!       call this%err_tree%write_pstricks(11)
       if (x_size > 3) then
          do pos=4,x_size
             print *,"aq_init_error_tree",pos,"/",x_size
             l_val=right_node%get_r_value_array()
             call this%evaluate(x_array(pos),r_val)
             c_val=r_val-l_val
             allocate(right_node)
             call right_node%initialize(&
                  &dim=dim_integral,&
                  &r_position=x_array(pos),&
                  &d_position=x_array(pos)-x_array(pos-1))
             call right_node%set_r_value(r_val)
             call right_node%set_d_value(c_val)
             call right_node%update()
             call this%err_tree%push_by_content(right_node)
!             call this%err_tree%write_pstricks(11)
             this%integral=this%integral+sum(right_node%get_d_integral())
          end do
          this%n_nodes = x_size
       end if
       this%is_error_tree_initialised=.true.
    end if
    call this%set_goal()
    this%is_initialised=.true.
    call cpu_time(this%cpu_time)
    this%init_time=this%cpu_time-this%init_time
    this%cuba_time=this%init_time
    allocate(this%convergence(2,this%n_nodes:this%max_nodes))
  end subroutine aq_init_error_tree

  subroutine aq_set_abs_goal(this,goal)
    class(aq_class) :: this
    real(kind=double) :: goal
    this%abs_error_goal = goal
    call this%set_goal
  end subroutine aq_set_abs_goal

  subroutine aq_set_rel_goal(this,goal)
    class(aq_class) :: this
    real(kind=double) :: goal
    this%rel_error_goal = goal
    call this%set_goal
  end subroutine aq_set_rel_goal

  subroutine aq_set_goal(this)
    class(aq_class) :: this
    this%scaled_error_goal = this%rel_error_goal*abs(this%integral)
    if ((this%scaled_error_goal==0D0).and.(this%abs_error_goal==0D0)) then
       this%is_goal_set = .false.
       this%error_goal = 0D0
    else
       if (this%scaled_error_goal == 0D0) then
          this%error_goal = this%abs_error_goal
       else
          if (this%abs_error_goal == 0D0) then
             this%error_goal = this%scaled_error_goal
          else
             this%error_goal = max(this%scaled_error_goal,this%abs_error_goal)
          end if
       end if
       if (this%error_goal > 0D0) then
          this%is_goal_set = .true.
       else
          this%is_goal_set = .false.
       end if
    end if
  end subroutine aq_set_goal

  ! calculation

  subroutine aq_main_loop(this)
    ! unsafe, when n_nodes < 4
    class(aq_class) :: this
    class(fibonacci_leave_type), pointer :: rightmost
    class(measurable_class), pointer :: content
    class(muli_trapezium_type),pointer :: new_node!,debug
    logical :: limit = .false.
    real(kind=double) :: center
    real(kind=double),dimension(:),allocatable::c_val
    allocate(c_val(0:this%dim_integral-1))
    loop:do
       call this%err_tree%pop_right(rightmost)
       if (rightmost < this%error_goal/this%n_nodes) then
          this%is_goal_reached = .true.
          exit loop
       else
          call rightmost%get_content(content)
          select type (content)
          class is (muli_trapezium_type)
             print ('("nodes: ",I5," error: ",E14.7," goal: ",E14.7," node at: ",E14.7,"-",E14.7)'),&
                  this%n_nodes,&
                  rightmost%measure()*this%n_nodes,&
                  this%error_goal,&
                  content%get_l_position(),&
                  content%get_r_position()
             this%convergence(1,this%n_nodes)=this%error_goal/this%n_nodes
             this%convergence(2,this%n_nodes)=rightmost%measure()
             center = content%get_r_position()-content%get_d_position()/2D0
             call cpu_time(this%cpu_time)
             this%cuba_time=this%cuba_time-this%cpu_time
             call this%evaluate(center,c_val)
             call cpu_time(this%cpu_time)
             this%cuba_time=this%cuba_time+this%cpu_time
             call content%split(c_val,center,new_node)
             call this%err_tree%push_by_leave(rightmost)
             call this%err_tree%push_by_content(new_node)
          end select
          this%n_nodes=this%n_nodes+1
          if (this%n_nodes > this%max_nodes) then
             limit = .true.             
             exit loop
          end if
       end if
    end do loop
    call this%err_tree%push_by_leave(rightmost)
  end subroutine aq_main_loop

  subroutine aq_run(this)
    class(aq_class) :: this
    call cpu_time(this%total_time)
    if (.not. this%is_error_tree_initialised) then
       call this%init_error_tree(this%dim_integral,this%region)
    end if
    this%is_run = .false.
    this%is_goal_reached = .false.
    call aq_main_loop(this)
    this%is_run = .true.
    call cpu_time(this%cpu_time)
    this%total_time=this%cpu_time-this%total_time
  end subroutine aq_run

  subroutine aq_integrate(this,int_tree)
    class(aq_class) :: this
    class(muli_trapezium_node_class),pointer :: node
    type(muli_trapezium_tree_type),intent(out)::int_tree
    real(kind=double) :: sum
    this%is_integrated=.false.
    this%integral_error=0D0
    if (this%is_run) then
    call cpu_time(this%int_time)
       call fibonacci_tree_resort_and_convert_to_trapezium_list(this%err_tree,this%int_list)
!       call this%int_list%print_all()
!       call this%int_list%integrate(this%integral,this%integral_error)
       call muli_trapezium_list_integrate(this%int_list,this%integral,this%integral_error)
       call this%int_list%to_tree(int_tree)
       this%is_integrated=.true.
       call cpu_time(this%cpu_time)
       this%int_time=this%cpu_time-this%int_time
    end if
  end subroutine aq_integrate

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Non Type Bound Module Procedures !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine fibonacci_tree_resort_and_convert_to_trapezium_list(fib_tree,lin_list)
    ! usually, the tree is sorted by the sum of errors. now it shall be sorted by the right position.
    class(fibonacci_node_type),intent(in) :: fib_tree
    class(fibonacci_node_type),pointer :: leave
    class(muli_trapezium_list_type),pointer,intent(out) :: lin_list
    class(muli_trapezium_list_type),pointer :: left_list,right_list
    class(muli_trapezium_node_class),pointer :: left_node,right_node,last_node
    class(measurable_class),pointer :: content
    ! When at least one branch of the tree is itself a tree, i.e. each branch has got at least two leaves, then process each branch and merge the results.
    if (fib_tree%depth>1) then
       !print *,"3A"
       call fibonacci_tree_resort_and_convert_to_trapezium_list(fib_tree%left,left_list)
       call fibonacci_tree_resort_and_convert_to_trapezium_list(fib_tree%right,right_list)
       ! Now we got two sortet lists. Which one's leftmost node has got the lowest value of "r_position"?
       ! That one shall be the beginning of the merged list "lin_list".
       if(left_list%is_left_of(right_list))then
          lin_list => left_list
          call left_list%get_right(left_node)
          right_node=>right_list
       else
          lin_list => right_list
          left_node=>left_list
          call right_list%get_right(right_node)
       end if
       last_node=>lin_list
       ! Everything is prepared for the algorithm: lin_list id the beginning of the sorted list, last_node is it's end. left_node and right_node are the leftmost nodes of the remainders of left_list and right_list. The latter will get stripped from left to right, until one of them ends.
       do while(associated(left_node).and.associated(right_node))          
          if (left_node%is_left_of(right_node)) then
             call last_node%append(left_node)
             call last_node%get_right(last_node)
             call left_node%get_right(left_node)
          else
             call last_node%append(right_node)
             call last_node%get_right(last_node)
             call right_node%get_right(right_node)
          end if
       end do
       ! Either left_list or right_list is completely merged into lin_list. The other one gets appended to lin_list.
       if (associated(left_node)) then
          call last_node%append(left_node)
       else
          call last_node%append(right_node)
       end if
       ! It's done.
       !print *,"3E"
    else
       ! The tree has got two leaves at most. Is it more than one?
       if (fib_tree%depth == 0) then
          !print *,"1A"
          ! Here fib_tree is a single leave with an allocated "content" componet of type muli_trapezium_type. If "content" is not type compatible with muli_trapezium_type, then this whole conversion cannot succeed. 
          ! We allocate a new node of type muli_trapezium_list_type. This list does not contain the content of fib_tree, it *IS* a copy of the content, for muli_trapezium_list_type is an extension of muli_trapezium_type.
          select type (fib_tree)
          class is (fibonacci_leave_type)
             call fib_tree%get_content(content)
             select type (content)
             class is (muli_trapezium_type)
                call muli_trapezium_to_node(content,content%get_r_position(),list=lin_list)
             class default
                print *,"fibonacci_tree_resort_and_convert_to_trapezium_list: &
                     &Content of fibonacci_tree is not type compatible to muli_trapezium_type"
             end select
          end select
          !print *,"1E"
       else
          !print *,"2A"
          ! Each branch of fib_tree is a single leave. We could call this soubroutine for each branch, but we do copy and paste for each branch instead.
          leave=>fib_tree%left
          select type (leave)
          class is (fibonacci_leave_type)
             call leave%get_content(content)
             select type (content)
             class is (muli_trapezium_type)
                call muli_trapezium_to_node(content,content%get_r_position(),list=left_list)
             class default
                print *,"fibonacci_tree_resort_and_convert_to_trapezium_list: &
                     &Content of fibonacci_tree is not type compatible to muli_trapezium_type"
             end select
          end select
          leave=>fib_tree%right
          select type (leave)
          class is (fibonacci_leave_type)
             call leave%get_content(content)
             select type (content)
             class is (muli_trapezium_type)
                call muli_trapezium_to_node(content,content%get_r_position(),list=right_list)
             class default
                print *,"fibonacci_tree_resort_and_convert_to_trapezium_list: &
                     &Content of fibonacci_tree is not type compatible to muli_trapezium_type"
             end select
          end select
          ! Finally we append one list to the other, the lowest value of "r_position" comes first.
          if (left_list%is_left_of(right_list)) then
             call left_list%append(right_list)
             lin_list=>left_list
          else
             call right_list%append(left_list)
             lin_list=>right_list
          end if
          !print *,"2E"
       end if
    end if
    !    call lin_list%print_all()
    !    call lin_list%check()
  end subroutine fibonacci_tree_resort_and_convert_to_trapezium_list

end module muli_aq

