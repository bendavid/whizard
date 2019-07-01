!!! module: fibonacci_tree_module
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
!!! Latest Change: 2011-06-29 09:07:26 CEST(+0200)
!!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!! This file contains the module "muli_fibonacci_tree". A fibonacci tree is a
!!! self balancing binary tree. "Balanced" means that the depth of the left
!!! successor may differ from the depth of the right successor in any node by
!!! maximal one. The maximal unbalanced tree is maximal unbalanced in every
!!! node. So the number of leaves of a maximal unbalanced fibonacci tree of
!!! depth n is precisely the nth fibonacci number.
!!!
!!! I use this tree for the adaptive quadrature. In every iteration of the
!!! algorithm, I have to pick to segment with largest integration error, cut it
!!! into pieces and calculate the new integration error for the pieces.
!!! A binary tree is obvioulsly a good choise for soring these segments. The 
!!! problem is, that I always pick leaves from the same side of the tree. So 
!!! the tree must decline at one side and grow at the other side. Sorting gets
!!! less efficient and finally most of the overall cpu time gets wasted for
!!! sorting.
!!! This tree outperforms a naive binary tree significntly in this particular
!!! job.

module muli_fibonacci_tree
  use muli_basic
  use kinds!NODEP!
  implicit none

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Parameter Definition !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!

  character(*),parameter,private :: no_par = "edge=\noparent"
  character(*),parameter,private :: no_ret = "edge=\noreturn"
  character(*),parameter,private :: no_kid = "edge=\nochild"
  character(*),parameter,private :: le_kid = "edge=\childofleave"
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Derived Type Module Component Declaration !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!  class(serializable_ref_type),pointer,private::ref_list

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Derived Type Definitions !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  type,extends(measurable_class) :: fibonacci_node_type
!     private
     class(fibonacci_node_type), pointer :: up => null()
     class(measurable_class), pointer :: down => null()
     class(fibonacci_node_type), pointer :: left => null()
     class(fibonacci_node_type), pointer :: right => null()
     integer :: depth = 0
!     real(kind=double) :: value
   contains
     ! overridden serializable_class procedures
     procedure::write_to_marker=>fibonacci_node_write_to_marker
     procedure::read_from_marker=>fibonacci_node_read_from_marker
     procedure::read_target_from_marker=>fibonacci_node_read_target_from_marker
     procedure::print_to_unit=>fibonacci_node_print_to_unit
     procedure,nopass::get_type=>fibonacci_node_get_type
     procedure::deserialize_from_marker=>fibonacci_node_deserialize_from_marker
     ! overridden measurable_class procedures
     procedure::measure=>fibonacci_node_measure    
     ! init/final
     procedure,public :: deallocate_tree => fibonacci_node_deallocate_tree
     procedure,public :: deallocate_all => fibonacci_node_deallocate_all
!     interface
     procedure,public :: get_depth => fibonacci_node_get_depth
     procedure,public :: count_leaves => fibonacci_node_count_leaves
! public tests
     procedure,public,nopass :: is_leave => fibonacci_node_is_leave
     procedure,public,nopass :: is_root => fibonacci_node_is_root
     procedure,public,nopass :: is_inner => fibonacci_node_is_inner
! print methods
     procedure,public :: write_association => fibonacci_node_write_association
     procedure,public :: write_contents => fibonacci_node_write_contents
     procedure,public :: write_values => fibonacci_node_write_values
     procedure,public :: write_leaves => fibonacci_node_write_leaves
     !procedure,public :: write => fibonacci_node_write_contents
! write methods
     procedure,public :: write_pstricks => fibonacci_node_write_pstricks
! elaborated functions
     procedure,public :: copy_node => fibonacci_node_copy_node
     procedure,public :: find_root => fibonacci_node_find_root
     procedure,public :: find_leftmost => fibonacci_node_find_leftmost
     procedure,public :: find_rightmost => fibonacci_node_find_rightmost
     procedure,public :: find => fibonacci_node_find
     procedure,public :: find_left_leave => fibonacci_node_find_left_leave
     procedure,public :: find_right_leave => fibonacci_node_find_right_leave
     procedure,public :: apply_to_leaves => fibonacci_node_apply_to_leaves
     procedure,public :: apply_to_leaves_rl => fibonacci_node_apply_to_leaves_rl
! private procedures: these are unsafe!
     procedure :: set_depth => fibonacci_node_set_depth
     procedure :: append_left => fibonacci_node_append_left
     procedure :: append_right => fibonacci_node_append_right
     procedure :: replace => fibonacci_node_replace
     procedure :: swap => fibonacci_node_swap_nodes
     procedure :: flip => fibonacci_node_flip_children
     procedure :: rip => fibonacci_node_rip
     procedure :: remove_and_keep_parent => fibonacci_node_remove_and_keep_parent
     procedure :: remove_and_keep_twin => fibonacci_node_remove_and_keep_twin
     procedure :: rotate_left => fibonacci_node_rotate_left
     procedure :: rotate_right => fibonacci_node_rotate_right
     procedure :: rotate => fibonacci_node_rotate
     procedure :: balance_node => fibonacci_node_balance_node
     procedure :: update_depth_save => fibonacci_node_update_depth_save
     procedure :: update_depth_unsave => fibonacci_node_update_depth_unsave
     procedure :: repair => fibonacci_node_repair
! tests: these are save when type is fibonacci_node_type and else unsafe.
     procedure :: is_left_short => fibonacci_node_is_left_short
     procedure :: is_right_short => fibonacci_node_is_right_short
     procedure :: is_unbalanced => fibonacci_node_is_unbalanced
     procedure :: is_left_too_short => fibonacci_node_is_left_too_short
     procedure :: is_right_too_short => fibonacci_node_is_right_too_short
     procedure :: is_too_unbalanced => fibonacci_node_is_too_unbalanced
     procedure :: is_left_child => fibonacci_node_is_left_child
     procedure :: is_right_child => fibonacci_node_is_right_child
     ! user
     ! node
     ! tree
!     procedure :: balance
!     procedure :: sort
!     procedure :: merge
!     procedure :: split
  end type fibonacci_node_type

  type,extends(fibonacci_node_type) :: fibonacci_leave_type
!     class(measurable_class),pointer :: content
  contains
     ! overridden serializable_class procedures
     !procedure::write_to_marker=>fibonacci_leave_write_to_marker
     !procedure::read_from_marker=>fibonacci_leave_read_from_marker
     procedure::print_to_unit=>fibonacci_leave_print_to_unit
     procedure,nopass::get_type=>fibonacci_leave_get_type
     procedure,public :: deallocate_all => fibonacci_leave_deallocate_all
     ! new procedures
     procedure,public :: pick => fibonacci_leave_pick
     procedure,public :: get_left => fibonacci_leave_get_left
     procedure,public :: get_right => fibonacci_leave_get_right
     procedure,public :: write_pstricks => fibonacci_leave_write_pstricks
     procedure,public :: copy_content => fibonacci_leave_copy_content
     procedure,public :: set_content => fibonacci_leave_set_content
     procedure,public :: get_content => fibonacci_leave_get_content
     procedure,public,nopass :: is_inner => fibonacci_leave_is_inner
     procedure,public,nopass :: is_leave => fibonacci_leave_is_leave
     procedure :: insert_leave_by_node => fibonacci_leave_insert_leave_by_node
     procedure :: is_left_short => fibonacci_leave_is_left_short
     procedure :: is_right_short => fibonacci_leave_is_right_short
     procedure :: is_unbalanced => fibonacci_leave_is_unbalanced
     procedure :: is_left_too_short => fibonacci_leave_is_left_too_short
     procedure :: is_right_too_short => fibonacci_leave_is_right_too_short
     procedure :: is_too_unbalanced => fibonacci_leave_is_too_unbalanced
  end type fibonacci_leave_type

  type,extends(fibonacci_node_type) :: fibonacci_root_type
     logical::is_valid_c=.false.
     class(fibonacci_leave_type),pointer :: leftmost => null()
     class(fibonacci_leave_type),pointer :: rightmost => null()
  contains
     ! overridden serializable_class procedures
     procedure::write_to_marker=>fibonacci_root_write_to_marker
     procedure::read_target_from_marker=>fibonacci_root_read_target_from_marker
     procedure::print_to_unit=>fibonacci_root_print_to_unit
     procedure,nopass::get_type=>fibonacci_root_get_type
     ! new procedures
     procedure::get_leftmost=>fibonacci_root_get_leftmost
     procedure::get_rightmost=>fibonacci_root_get_rightmost
! public tests
     procedure,public,nopass :: is_root => fibonacci_root_is_root
     procedure,public,nopass :: is_inner => fibonacci_root_is_inner
     procedure,public :: is_valid => fibonacci_root_is_valid
     procedure,public :: count_leaves => fibonacci_root_count_leaves
     procedure,public :: write_pstricks => fibonacci_root_write_pstricks
     procedure,public :: copy_root => fibonacci_root_copy_root
     procedure,public :: push_by_content => fibonacci_root_push_by_content
     procedure,public :: push_by_leave => fibonacci_root_push_by_leave
     procedure,public :: pop_left => fibonacci_root_pop_left
     procedure,public :: pop_right => fibonacci_root_pop_right
     procedure,public :: merge => fibonacci_root_merge
     procedure,public :: set_leftmost => fibonacci_root_set_leftmost
     procedure,public :: set_rightmost => fibonacci_root_set_rightmost
     procedure,public :: init_by_leave => fibonacci_root_init_by_leave
     procedure,public :: init_by_content => fibonacci_root_init_by_content
     procedure,public :: reset => fibonacci_root_reset
     ! init/final
     procedure,public :: deallocate_tree => fibonacci_root_deallocate_tree
     procedure,public :: deallocate_all => fibonacci_root_deallocate_all
     procedure :: is_left_child => fibonacci_root_is_left_child
     procedure :: is_right_child => fibonacci_root_is_right_child
  end type fibonacci_root_type

  type,extends(fibonacci_root_type) :: fibonacci_stub_type
   contains
     ! overridden serializable_class procedures
!!$     procedure::write_to_marker=>fibonacci_stub_write_to_marker
!!$     procedure::read_from_marker=>fibonacci_stub_read_from_marker
!!$     procedure::print_to_unit=>fibonacci_stub_print_to_unit
     procedure,nopass::get_type=>fibonacci_stub_get_type
     ! overridden fibonacci_root_type procedures
     procedure,public :: push_by_content => fibonacci_stub_push_by_content
     procedure,public :: push_by_leave => fibonacci_stub_push_by_leave
     procedure,public :: pop_left => fibonacci_stub_pop_left
     procedure,public :: pop_right => fibonacci_stub_pop_right
  end type fibonacci_stub_type

  type fibonacci_leave_list_type
     class(fibonacci_leave_type),pointer :: leave => null()
     class(fibonacci_leave_list_type),pointer :: next => null()
  end type fibonacci_leave_list_type

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Type Bound Procedures for fibonacci_node_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! overridden serializable_class procedures
  
 recursive  subroutine fibonacci_node_write_to_marker(this,marker,status)
    class(fibonacci_node_type), intent(in) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
! local variables
    class(serializable_class),pointer::ser
    call marker%mark_begin("fibonacci_node_type")
    ser=>this%left
    call marker%mark_pointer("left",ser)
    ser=>this%right
    call marker%mark_pointer("right",ser)
    ser=>this%down
    call marker%mark_pointer("down",ser)
    call marker%mark_end("fibonacci_node_type")
  end subroutine fibonacci_node_write_to_marker

  recursive subroutine fibonacci_node_read_from_marker (this,marker,status)
    class(fibonacci_node_type), intent(out) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
    print *,"fibonacci_node_read_from_marker: You cannot deserialize a list with this subroutine."
    print *,"Use fibonacci_node_read_target_from_marker instead."    
  end subroutine fibonacci_node_read_from_marker

recursive subroutine fibonacci_node_read_target_from_marker(this,marker,status)
    class(fibonacci_node_type),target,intent(out) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
! local variables
    class(serializable_class),pointer::ser
    call marker%pick_begin("fibonacci_node_type",status=status)
    call marker%pick_pointer("left",ser)
    if(status==0)then
       select type(ser)
       class is (fibonacci_node_type)
          this%left=>ser
          this%left%up=>this
       end select
    end if
    call marker%pick_pointer("right",ser)
    if(status==0)then
       select type(ser)
       class is (fibonacci_node_type)
          this%right=>ser
          this%right%up=>this
       end select
    end if
    call marker%pick_pointer("down",ser)
    if(status==0)then
       select type(ser)
       class is (measurable_class)
          this%down=>ser
       end select
    end if
    call marker%pick_end("fibonacci_node_type",status)
  end subroutine fibonacci_node_read_target_from_marker

  pure subroutine fibonacci_node_get_type(type)
    character(:),allocatable,intent(out)::type
    allocate(type,source="fibonacci_node_type")
  end subroutine fibonacci_node_get_type
  
  subroutine fibonacci_node_deserialize_from_marker(this,name,marker)
    class(fibonacci_node_type),intent(out)::this
    character(*),intent(in)::name
    class(marker_type),intent(inout)::marker
    class(serializable_class),pointer::ser
    allocate(fibonacci_leave_type::ser)
    call marker%push_reference(ser)
    allocate(fibonacci_node_type::ser)
    call marker%push_reference(ser)
    call serializable_deserialize_from_marker(this,name,marker)
    call marker%pop_reference(ser)
    deallocate(ser)
    call marker%pop_reference(ser)
    deallocate(ser)    
  end subroutine fibonacci_node_deserialize_from_marker
    
  recursive subroutine fibonacci_node_print_to_unit(this,unit,parents,components,peers)
    class(fibonacci_node_type),intent(in)::this
    integer,intent(in)::unit
    integer(kind=dik),intent(in)::parents,components,peers
    class(serializable_class),pointer::ser
    write(unit,'("Components of fibonacci_node_type:")')
    write(unit,'("Depth:   ",I22)')this%depth
    write(unit,'("Value:   ",E23.16)')this%measure()
    ser=>this%up
    call serialize_print_comp_pointer(ser,unit,parents,-one,-one,"Up:     ")
    ser=>this%left
    call serialize_print_peer_pointer(ser,unit,parents,components,peers,"Left:   ")
    ser=>this%right
    call serialize_print_peer_pointer(ser,unit,parents,components,peers,"Right:  ")
  end subroutine fibonacci_node_print_to_unit

  elemental function fibonacci_node_measure(this)
    class(fibonacci_node_type),intent(in)::this
    real(kind=double)::fibonacci_node_measure
    fibonacci_node_measure=this%down%measure()
  end function fibonacci_node_measure

  ! init/final

  recursive subroutine fibonacci_node_deallocate_tree(this)
    class(fibonacci_node_type),intent(inout) :: this
    if (associated(this%left)) then
       call this%left%deallocate_tree()
       deallocate(this%left)
    end if
    if (associated(this%right)) then
       call this%right%deallocate_tree()
       deallocate(this%right)
    end if
    call this%set_depth(0)
  end subroutine fibonacci_node_deallocate_tree

  recursive subroutine fibonacci_node_deallocate_all(this)
    class(fibonacci_node_type),intent(inout) :: this
    if (associated(this%left)) then
       call this%left%deallocate_all()
       deallocate(this%left)
    end if
    if (associated(this%right)) then
       call this%right%deallocate_all()
       deallocate(this%right)
    end if
    call this%set_depth(0)
  end subroutine fibonacci_node_deallocate_all

  subroutine fibonacci_node_set_depth(this,depth)
    class(fibonacci_node_type),intent(inout) :: this
    integer,intent(in) :: depth
    this%depth=depth
  end subroutine fibonacci_node_set_depth

  elemental function fibonacci_node_get_depth(this)
    class(fibonacci_node_type),intent(in) :: this
    integer :: fibonacci_node_get_depth
    fibonacci_node_get_depth = this%depth
  end function fibonacci_node_get_depth

  elemental function fibonacci_node_is_leave()
    logical :: fibonacci_node_is_leave
    fibonacci_node_is_leave = .false.
  end function fibonacci_node_is_leave

  elemental function fibonacci_node_is_root()
    logical :: fibonacci_node_is_root
    fibonacci_node_is_root = .false.
  end function fibonacci_node_is_root

  elemental function fibonacci_node_is_inner()
    logical :: fibonacci_node_is_inner
    fibonacci_node_is_inner = .true.
  end function fibonacci_node_is_inner

  subroutine fibonacci_node_write_leaves(this,unit)
    class(fibonacci_node_type),intent(in),target :: this
    integer,intent(in),optional :: unit
    call this%apply_to_leaves(fibonacci_leave_write,unit)
  end subroutine fibonacci_node_write_leaves

  subroutine fibonacci_node_write_contents(this,unit)
    class(fibonacci_node_type),intent(in),target :: this
    integer,intent(in),optional :: unit
    call this%apply_to_leaves(fibonacci_leave_write_content,unit)
  end subroutine fibonacci_node_write_contents

  subroutine fibonacci_node_write_values(this,unit)
    class(fibonacci_node_type),intent(in),target :: this
    integer,intent(in),optional :: unit
    call this%apply_to_leaves(fibonacci_leave_write_value,unit)
  end subroutine fibonacci_node_write_values

  subroutine fibonacci_node_write_association(this,that)
    class(fibonacci_node_type),intent(in),target :: this
    class(fibonacci_node_type),intent(in),target :: that
    if (associated(that%left,this)) then
       write(*,'("this is left child of that")')
    end if
    if (associated(that%right,this)) then
       write(*,'("this is right child of that")')
    end if
    if (associated(that%up,this)) then
       write(*,'("this is parent of that")')
    end if    
    if (associated(this%left,that)) then
       write(*,'("that is left child of this")')
    end if
    if (associated(this%right,that)) then
       write(*,'("that is right child of this")')
    end if
    if (associated(this%up,that)) then
       write(*,'("that is parent of this")')
    end if
  end subroutine fibonacci_node_write_association
  
  recursive subroutine fibonacci_node_write_pstricks(this,unitnr)
    class(fibonacci_node_type),intent(in),target :: this
    integer,intent(in) :: unitnr
    if (associated(this%up)) then
       if (associated(this%up%left,this).neqv.(associated(this%up%right,this))) then
!          write(unitnr,'("\begin{psTree}{\Toval{$",i3,"$}}")') int(this%depth)
          write(unitnr,'("\begin{psTree}{\Toval{\node{",i3,"}{",f9.3,"}}}")') int(this%depth),this%measure()
       else
          write(unitnr,'("\begin{psTree}{\Toval[",a,"]{\node{",i3,"}{",f9.3,"}}}")') no_ret,int(this%depth),this%measure()
       end if
    else
       write(unitnr,'("\begin{psTree}{\Toval[",a,"]{\node{",i3,"}{",f9.3,"}}}")') no_par,int(this%depth),this%measure()
    end if
    if (associated(this%left)) then
       call this%left%write_pstricks(unitnr)
    else
       write(unitnr,'("\Tr[edge=brokenline]{}")')
    end if
    if (associated(this%right)) then
       call this%right%write_pstricks(unitnr)
    else
       write(unitnr,'("\Tr[edge=brokenline]{}")')
    end if
    write(unitnr,'("\end{psTree}")')
  end subroutine fibonacci_node_write_pstricks

  subroutine fibonacci_node_copy_node(this,primitive)
    class(fibonacci_node_type),intent(out) :: this
    class(fibonacci_node_type),intent(in) :: primitive
    this%up => primitive%up
    this%left => primitive%left
    this%right => primitive%right
    this%depth = primitive%depth
    this%down=> primitive%down
  end subroutine fibonacci_node_copy_node

  subroutine fibonacci_node_find_root(this,root)
    class(fibonacci_node_type),intent(in),target  :: this
    class(fibonacci_root_type),pointer,intent(out) :: root
    class(fibonacci_node_type),pointer :: node
    node=>this
    do while(associated(node%up))
       node=>node%up
    end do
    select type (node)
    class is (fibonacci_root_type)
       root=>node
    class default
       nullify(root)
       print *,"fibonacci_node_find_root: root is not type compatible to fibonacci_root_type. Retured NULL()."
    end select
  end subroutine fibonacci_node_find_root

  subroutine fibonacci_node_find_leftmost(this,leave)
    class(fibonacci_node_type),intent(in), target  :: this
    class(fibonacci_leave_type),pointer,intent(out) :: leave
    class(fibonacci_node_type), pointer :: node
    node=>this
    do while(associated(node%left))
       node=>node%left
    end do
    select type (node)
    class is (fibonacci_leave_type)
       leave => node
    class default
       leave => null()
    end select
  end subroutine fibonacci_node_find_leftmost

  subroutine fibonacci_node_find_rightmost(this,leave)
    class(fibonacci_node_type),intent(in), target  :: this
    class(fibonacci_leave_type),pointer,intent(out) :: leave
    class(fibonacci_node_type), pointer :: node
    node=>this
    do while(associated(node%right))
       node=>node%right
    end do
    select type (node)
    class is (fibonacci_leave_type)
       leave => node
    class default
       leave => null()
    end select
  end subroutine fibonacci_node_find_rightmost

  subroutine fibonacci_node_find(this,value,leave)
    class(fibonacci_node_type),intent(in),target  :: this
    real(kind=double),intent(in) :: value
    class(fibonacci_leave_type),pointer,intent(out) :: leave
    class(fibonacci_node_type), pointer :: node
    node=>this
    do
       if (node>=value) then
          if (associated(node%left)) then
             node=>node%left
          else
             print *,"fibonacci_node_find: broken tree!"
             leave => null()
             return
          end if
       else
          if (associated(node%right)) then
             node=>node%right
          else
             print *,"fibonacci_node_find: broken tree!"
             leave => null()
             return
          end if
       end if
       select type (node)
       class is (fibonacci_leave_type)
          leave => node
          exit
       end select
    end do
  end subroutine fibonacci_node_find

  subroutine fibonacci_node_find_left_leave(this,leave)
    class(fibonacci_node_type),intent(in),target  :: this
    class(fibonacci_node_type),pointer  :: node
    class(fibonacci_leave_type),pointer,intent(out)  :: leave
    nullify(leave)
    node=>this
    do while (associated(node%up))
       if (associated(node%up%right,node)) then
          node=>node%up%left
          do while (associated(node%right))
             node=>node%right
          end do
          select type (node)
          class is (fibonacci_leave_type)
          leave=>node
          end select
          exit
       end if
       node=>node%up
    end do
  end subroutine fibonacci_node_find_left_leave

  subroutine fibonacci_node_find_right_leave(this,leave)
    class(fibonacci_node_type),intent(in),target  :: this
    class(fibonacci_node_type),pointer  :: node
    class(fibonacci_leave_type),pointer,intent(out)  :: leave
    nullify(leave)
    node=>this
    do while (associated(node%up))
       if (associated(node%up%left,node)) then
          node=>node%up%right
          do while (associated(node%left))
             node=>node%left
          end do
          select type (node)
          class is (fibonacci_leave_type)
          leave=>node
          end select
          exit
       end if
       node=>node%up
    end do
  end subroutine fibonacci_node_find_right_leave

  subroutine fibonacci_node_replace(this,old_node)
    class(fibonacci_node_type),intent(inout),target :: this
    class(fibonacci_node_type),target :: old_node
    if (associated(old_node%up)) then
       if (old_node%is_left_child()) then
          old_node%up%left => this
       else
          if (old_node%is_right_child()) then
             old_node%up%right => this
          end if
       end if
       this%up => old_node%up
    else
       nullify(this%up)
    end if
  end subroutine fibonacci_node_replace

  subroutine fibonacci_node_swap_nodes(left,right)
    class(fibonacci_node_type),target,intent(inout) :: left,right
    class(fibonacci_node_type),pointer :: left_left,right_right
    class(measurable_class),pointer::down
    ! swap branches
    left_left  =>left%left
    right_right=>right%right
    left%left  =>right%right
    right%right=>left_left
    ! repair up components
    right_right%up=>left
    left_left%up  =>right
    ! repair down components
          down =>  left%down
     left%down => right%down
    right%down =>       down
  end subroutine fibonacci_node_swap_nodes

!  subroutine fibonacci_node_swap_nodes(this,that)
!    class(fibonacci_node_type),target :: this
!    class(fibonacci_node_type),pointer,intent(in) :: that
!    class(fibonacci_node_type),pointer :: par_i,par_a
!    par_i => this%up
!    par_a => that%up
!    if (associated(par_i%left,this)) then
!       par_i%left => that
!    else
!       par_i%right => that
!    end if
!    if (associated(par_a%left,that)) then
!       par_a%left => this
!    else
!       par_a%right => this
!    end if
!    this%up => par_a
!    that%up => par_i
!  end subroutine fibonacci_node_swap_nodes
  
  subroutine fibonacci_node_flip_children(this)
    class(fibonacci_node_type),intent(inout) :: this
    class(fibonacci_node_type),pointer :: child
    child => this%left
    this%left=>this%right
    this%right => child
  end subroutine fibonacci_node_flip_children

  subroutine fibonacci_node_rip(this)
    class(fibonacci_node_type),intent(inout),target :: this
    if (this%is_left_child()) then
       nullify(this%up%left)
    end if
    if (this%is_right_child()) then
       nullify(this%up%right)
    end if
    nullify(this%up)
  end subroutine fibonacci_node_rip
 
  subroutine fibonacci_node_remove_and_keep_twin(this,twin)
    class(fibonacci_node_type),intent(inout),target  :: this
    class(fibonacci_node_type),intent(out),pointer :: twin
    class(fibonacci_node_type),pointer :: pa
    if (.not. (this%is_root())) then
       pa=>this%up
       if (.not. pa%is_root()) then
          if (this%is_left_child()) then         
             twin => pa%right
          else
             twin => pa%left
          end if
          if (pa%is_left_child()) then
             pa%up%left => twin
          else
             pa%up%right => twin
          end if
       end if
       twin%up => pa%up
       if(associated(this%right))then
          this%right%left=>this%left
       end if
       if(associated(this%left))then
          this%left%right=>this%right
       end if
       nullify(this%left)
       nullify(this%right)
       nullify(this%up)
       deallocate(pa)
    end if
  end subroutine fibonacci_node_remove_and_keep_twin

  subroutine fibonacci_node_remove_and_keep_parent(this,pa)
    class(fibonacci_node_type),intent(inout),target  :: this
    class(fibonacci_node_type),intent(out),pointer :: pa
    class(fibonacci_node_type),pointer :: twin
    if (.not. (this%is_root())) then
       pa=>this%up
       if (this%is_left_child()) then         
          twin => pa%right
       else
          twin => pa%left
       end if
       twin%up=>pa%up
       if (associated(twin%left)) then
          twin%left%up => pa
       end if
       if (associated(twin%right)) then
          twin%right%up => pa
       end if
       call pa%copy_node(twin)
       select type(pa)
       class is (fibonacci_root_type)
          call pa%set_leftmost()
          call pa%set_rightmost()
       end select
       if(associated(this%right))then
          this%right%left=>this%left
       end if
       if(associated(this%left))then
          this%left%right=>this%right
       end if
       nullify(this%left)
       nullify(this%right)
       nullify(this%up)
       deallocate(twin)
    else
       pa=>this
    end if
  end subroutine fibonacci_node_remove_and_keep_parent

  subroutine fibonacci_leave_pick(this)
    class(fibonacci_leave_type),target,intent(inout) :: this
    class(fibonacci_node_type),pointer :: other
    class(fibonacci_root_type),pointer :: root
!    call this%up%print_parents()
    call this%find_root(root)
    if(associated(this%up,root))then
       if(this%up%depth<2)then
          print *,"fibonacci_leave_pick: Cannot pick leave. Tree must have at least three leaves."
          return
       else
          call this%remove_and_keep_parent(other)
          call other%repair()
       end if
    else
       call this%remove_and_keep_twin(other)
       call other%up%repair()
    end if
    if(associated(root%leftmost,this))call root%set_leftmost()
    if(associated(root%rightmost,this))call root%set_rightmost()
  end subroutine fibonacci_leave_pick

  subroutine fibonacci_node_append_left(this,new_branch)
    class(fibonacci_node_type),target :: this
    class(fibonacci_node_type),target :: new_branch
    this%left => new_branch
    new_branch%up => this
  end subroutine fibonacci_node_append_left
  
  subroutine fibonacci_node_append_right(this,new_branch)
    class(fibonacci_node_type),intent(inout),target :: this
    class(fibonacci_node_type),target :: new_branch
    this%right => new_branch
    new_branch%up => this
  end subroutine fibonacci_node_append_right

  subroutine fibonacci_node_rotate_left(this)
    class(fibonacci_node_type),intent(inout),target  :: this
    call this%swap(this%right)
    call this%right%flip()
    call this%right%update_depth_unsave()
    call this%flip()
!    value = this%value
!    this%value = this%left%value
!    this%left%value = value
  end subroutine fibonacci_node_rotate_left

  subroutine fibonacci_node_rotate_right(this)
    class(fibonacci_node_type),intent(inout),target  :: this
    call this%left%swap(this)
    call this%left%flip()
    call this%left%update_depth_unsave()
    call this%flip()
!    value = this%value
!    this%value = this%right%value
!    this%right%value = value
  end subroutine fibonacci_node_rotate_right

  subroutine fibonacci_node_rotate(this)
    class(fibonacci_node_type),intent(inout),target  :: this
    if (this%is_left_short()) then
       call this%rotate_left()
    else
       if (this%is_right_short()) then
          call this%rotate_right()
       end if
    end if
  end subroutine fibonacci_node_rotate

  subroutine fibonacci_node_balance_node(this,changed)
    class(fibonacci_node_type),intent(inout),target  :: this
    logical,intent(out) :: changed
    changed=.false.
    if (this%is_left_too_short()) then
       if (this%right%is_right_short()) then
          call this%right%rotate_right
       end if
       call this%rotate_left()
       changed=.true.
    else
       if (this%is_right_too_short()) then
          if (this%left%is_left_short()) then
             call this%left%rotate_left
          end if
          call this%rotate_right()
          changed=.true.
       end if
    end if
  end subroutine fibonacci_node_balance_node

  subroutine fibonacci_node_update_depth_unsave(this)
    class(fibonacci_node_type),intent(inout)  :: this
    this%depth=max(this%left%depth+1,this%right%depth+1)
  end subroutine fibonacci_node_update_depth_unsave

  subroutine fibonacci_node_update_depth_save(this,updated)
    class(fibonacci_node_type),intent(inout)  :: this
    logical,intent(out) :: updated
    integer :: left,right,new_depth
    if (associated(this%left)) then
       left=this%left%depth+1
    else
       left=-1
    end if
    if (associated(this%right)) then
       right=this%right%depth+1
    else
       right=-1
    end if
    new_depth=max(left,right)
    if (this%depth == new_depth) then
       updated = .false.
    else
       this%depth=new_depth
       updated = .true.
    end if
  end subroutine fibonacci_node_update_depth_save

  subroutine fibonacci_node_repair(this)
    class(fibonacci_node_type),intent(inout),target :: this
    class(fibonacci_node_type),pointer:: node
    logical :: new_depth,new_balance
    new_depth = .true.
    node=>this
    do while((new_depth .or. new_balance) .and. (associated(node)))
       call node%balance_node(new_balance)
       call node%update_depth_save(new_depth)
       node=>node%up
    end do
  end subroutine fibonacci_node_repair

!!$  subroutine fibonacci_node_update_value(this,right_value)
!!$    class(fibonacci_node_type),target :: this
!!$    class(fibonacci_node_type),pointer:: node
!!$    real(kind=double),intent(in) :: right_value
!!$    if (associated(this%left) .and. associated(this%right)) then
!!$       node=>this
!!$!       node%value = node%left%value
!!$!       right_value = node%right%value
!!$       inner: do while(associated(node%up))
!!$          if (node%is_right_child()) then
!!$             node=>node%up
!!$          else
!!$             node%up%value=right_value
!!$             exit
!!$          end if
!!$       end do inner
!!$    end if
!!$  end subroutine fibonacci_node_update_value

  elemental logical function fibonacci_node_is_left_short(this)
    class(fibonacci_node_type),intent(in) :: this
    fibonacci_node_is_left_short = (this%left%depth<this%right%depth)
  end function fibonacci_node_is_left_short

  elemental logical function fibonacci_node_is_right_short(this)
    class(fibonacci_node_type),intent(in) :: this
    fibonacci_node_is_right_short = (this%right%depth<this%left%depth)
  end function fibonacci_node_is_right_short

  elemental logical function fibonacci_node_is_unbalanced(this)
    class(fibonacci_node_type),intent(in) :: this
    fibonacci_node_is_unbalanced = (this%is_left_short() .or. this%is_right_short())
  end function fibonacci_node_is_unbalanced

  elemental logical function fibonacci_node_is_left_too_short(this)
    class(fibonacci_node_type),intent(in) :: this
    fibonacci_node_is_left_too_short = (this%left%depth+1<this%right%depth)
  end function fibonacci_node_is_left_too_short

  elemental logical function fibonacci_node_is_right_too_short(this)
    class(fibonacci_node_type),intent(in) :: this
    fibonacci_node_is_right_too_short = (this%right%depth+1<this%left%depth)
  end function fibonacci_node_is_right_too_short

  elemental logical function fibonacci_node_is_too_unbalanced(this)
    class(fibonacci_node_type),intent(in) :: this
    fibonacci_node_is_too_unbalanced = (this%is_left_too_short() .or. this%is_right_too_short())
  end function fibonacci_node_is_too_unbalanced

  elemental logical function fibonacci_node_is_left_child(this)
    class(fibonacci_node_type),intent(in),target :: this
    fibonacci_node_is_left_child = associated(this%up%left,this)
  end function fibonacci_node_is_left_child

  elemental logical function fibonacci_node_is_right_child(this)
    class(fibonacci_node_type),intent(in),target :: this
    fibonacci_node_is_right_child = associated(this%up%right,this)
  end function fibonacci_node_is_right_child

  recursive subroutine fibonacci_node_apply_to_leaves(node,func,unit)
    class(fibonacci_node_type),intent(in),target :: node
    interface
       subroutine func(this,unit)
         import fibonacci_leave_type
         class(fibonacci_leave_type),intent(in),target :: this
         integer,intent(in),optional :: unit
       end subroutine func
    end interface
    integer,intent(in),optional :: unit
    select type (node)
    class is (fibonacci_leave_type)
       call func(node,unit)
    class default 
       call node%left%apply_to_leaves(func,unit)
       call node%right%apply_to_leaves(func,unit)
    end select
  end subroutine fibonacci_node_apply_to_leaves

  recursive subroutine fibonacci_node_apply_to_leaves_RL(node,func,unit)
    class(fibonacci_node_type),intent(in),target :: node
    interface
       subroutine func(this,unit)
         import fibonacci_leave_type
         class(fibonacci_leave_type),intent(in),target :: this
         integer,intent(in),optional :: unit
       end subroutine func
    end interface
    integer,intent(in),optional :: unit
    select type (node)
    class is (fibonacci_leave_type)
       call func(node,unit)
    class default 
       call node%right%apply_to_leaves_rl(func,unit)
       call node%left%apply_to_leaves_rl(func,unit)
    end select
  end subroutine fibonacci_node_apply_to_leaves_RL
  
  recursive subroutine fibonacci_node_count_leaves(this,n)
    class(fibonacci_node_type),intent(in) :: this
    integer,intent(out) :: n
    integer::n1,n2
    if(associated(this%left).and.associated(this%right)) then
       call fibonacci_node_count_leaves(this%left,n1)
       call fibonacci_node_count_leaves(this%right,n2)
       n=n1+n2
    else
       n=1
    end if
  end subroutine fibonacci_node_count_leaves

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Type Bound Procedures for fibonacci_root_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE fibonacci_root_write_to_marker(this,marker,status)
    CLASS(fibonacci_root_type), INTENT(IN) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
!    call marker%mark_begin("FIBONACCI_ROOT_TYPE")
    call fibonacci_node_write_to_marker(this,marker,status)
!    marker%mark_end("FIBONACCI_ROOT_TYPE")
  end SUBROUTINE fibonacci_root_write_to_marker

  SUBROUTINE fibonacci_root_read_target_from_marker(this,marker,status)
    CLASS(fibonacci_root_type),target,INTENT(out) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(out)::status
!    call marker%pick_begin("FIBONACCI_ROOT_TYPE",status)
    call fibonacci_node_read_from_marker(this,marker,status)
    call this%find_leftmost(this%leftmost)
    call this%find_rightmost(this%rightmost)
!    call marker%pick_end("FIBONACCI_ROOT_TYPE",status)
  end SUBROUTINE fibonacci_root_read_target_from_marker

  subroutine fibonacci_root_print_to_unit(this,unit,parents,components,peers)
    class(fibonacci_root_type),intent(in)::this
    integer,intent(in)::unit
    integer(kind=dik),intent(in)::parents,components,peers
    class(serializable_class),pointer::ser
    if(parents>0)call fibonacci_node_print_to_unit(this,unit,parents-1,components,peers)
    write(unit,'("Components of fibonacci_root_type:")')
    ser=>this%leftmost
    call serialize_print_peer_pointer(ser,unit,parents,components,min(peers,one),"Leftmost: ")
    ser=>this%rightmost
    call serialize_print_peer_pointer(ser,unit,parents,components,min(peers,one),"Rightmost:")
  end subroutine fibonacci_root_print_to_unit

  pure subroutine fibonacci_root_get_type(type)
    character(:),allocatable,intent(out)::type
    allocate(type,source="fibonacci_root_type")
  end subroutine fibonacci_root_get_type

  subroutine fibonacci_root_get_leftmost(this,leftmost)
    class(fibonacci_root_type),intent(in)::this
    class(fibonacci_leave_type),pointer::leftmost
    leftmost=>this%leftmost
  end subroutine fibonacci_root_get_leftmost

  subroutine fibonacci_root_get_rightmost(this,rightmost)
    class(fibonacci_root_type),intent(in)::this
    class(fibonacci_leave_type),pointer::rightmost
    rightmost=>this%rightmost
  end subroutine fibonacci_root_get_rightmost

  elemental function fibonacci_root_is_inner()
    logical::fibonacci_root_is_inner
    fibonacci_root_is_inner=.false.
  end function fibonacci_root_is_inner

  elemental function fibonacci_root_is_root()
    logical::fibonacci_root_is_root
    fibonacci_root_is_root=.true.
  end function fibonacci_root_is_root

  elemental function fibonacci_root_is_valid(this)
    class(fibonacci_root_type),intent(in) :: this
    logical :: fibonacci_root_is_valid
    fibonacci_root_is_valid=this%is_valid_c
  end function fibonacci_root_is_valid

  subroutine fibonacci_root_count_leaves(this,n)
    class(fibonacci_root_type),intent(in) :: this
    integer,intent(out) :: n
    n=0
    call fibonacci_node_count_leaves(this,n)
  end subroutine fibonacci_root_count_leaves

!!$  subroutine fibonacci_root_copy_node(this,primitive)
!!$    class(fibonacci_root_type),intent(out) :: this
!!$    type(fibonacci_node_type),intent(in) :: primitive
!!$    call fibonacci_node_copy_node(this,primitive)
!!$    call primitive%find_leftmost(this%leftmost)
!!$    call primitive%find_rightmost(this%rightmost)
!!$  end subroutine fibonacci_root_copy_node

  subroutine fibonacci_root_write_pstricks(this,unitnr)
    class(fibonacci_root_type),intent(in),target :: this
    integer,intent(in) :: unitnr
    logical :: is_opened
    character :: is_sequential,is_formatted,is_writeable
    print *,"pstricks"
    inquire(unitnr,opened=is_opened,&
         &sequential=is_sequential,formatted=is_formatted,write=is_writeable)
    if (is_opened) then
       if (is_sequential=="Y" .and. is_formatted=="Y" .and. is_writeable=="Y") then
!          write(unitnr,'("\begin{psTree}{\Toval[linecolor=blue]{$",i3,"$}}")') int(this%depth)
          write(unitnr,'("\begin{psTree}{\Toval[linecolor=blue]{\node{",i3,"}{",f9.3,"}}}")') this%depth,this%measure()
          if (associated(this%leftmost)) then
             call this%leftmost%write_pstricks(unitnr)
          else
             write(unitnr,'("\Tr[",a,"]{}")') no_kid
          end if
          if (associated(this%left)) then
             call this%left%write_pstricks(unitnr)
          else
             write(unitnr,'("\Tr[",a,"]{}")') no_kid
          end if
          if (associated(this%right)) then
             call this%right%write_pstricks(unitnr)
          else
             write(unitnr,'("\Tr[",a,"]{}")') no_kid
          end if
          if (associated(this%rightmost)) then
             call this%rightmost%write_pstricks(unitnr)
          else
             write(unitnr,'("\Tr[",a,"]{}")') no_kid
          end if
          write(unitnr,'("\end{psTree}")')
          write(unitnr,'("\\")')
       else
          print '("fibonacci_node_write_pstricks: Unit ",I2," is not opened properly.")',unitnr
          print '("No output is written to unit.")'
       end if
    else
       print '("fibonacci_node_write_pstricks: Unit ",I2," is not opened.")',unitnr
       print '("No output is written to unit.")' 
    end if
  end subroutine fibonacci_root_write_pstricks

  subroutine fibonacci_root_copy_root(this,primitive)
    class(fibonacci_root_type),intent(out) :: this
    class(fibonacci_root_type),intent(in) :: primitive
    call fibonacci_node_copy_node(this,primitive)
    this%leftmost => primitive%leftmost
    this%rightmost => primitive%rightmost
  end subroutine fibonacci_root_copy_root

!!$  subroutine fibonacci_root_push_by_node(this,new_leave)
!!$    class(fibonacci_root_type),target,intent(inout)  :: this
!!$    class(fibonacci_leave_type),pointer,intent(inout) :: new_leave
!!$    class(fibonacci_leave_type),pointer :: old_leave
!!$    if (new_leave<=this%leftmost) then
!!$       old_leave=>this%leftmost
!!$       this%leftmost=>new_leave
!!$    else
!!$       if (new_leave>this%rightmost) then
!!$          old_leave=>this%rightmost
!!$          this%rightmost=>new_leave
!!$       else
!!$          call this%find(new_leave%measure(),old_leave)
!!$       end if
!!$    end if
!!$!    call old_leave%insert_leave_by_node(new_leave)
!!$    call fibonacci_leave_insert_leave_by_node(old_leave,new_leave)
!!$    call new_leave%up%repair()
!!$!    call new_leave%up%update_value()
!!$  end subroutine fibonacci_root_push_by_node

  subroutine fibonacci_root_push_by_content(this,content)
    class(fibonacci_root_type),target,intent(inout)  :: this
    class(measurable_class),target,intent(in)::content
    class(fibonacci_leave_type),pointer :: node
!    print *,"fibonacci_root_push_by_content: ",content%measure()
    allocate(node)
    node%down=>content
    call this%push_by_leave(node)
  end subroutine fibonacci_root_push_by_content
  
  ! this is a workaround for BUG 44696. This subroutine is a merge of 
  ! fibonacci_tree_push_by_node
  ! fibonacci_node_find
  ! fibonacci_leave_insert_leave_by_node
  subroutine fibonacci_root_push_by_leave(this,new_leave)
    class(fibonacci_root_type),target,intent(inout)   :: this
    class(fibonacci_leave_type),pointer,intent(inout) :: new_leave
    class(fibonacci_leave_type),pointer :: old_leave
    class(fibonacci_node_type), pointer :: node,new_node,leave_c
    !write(11,fmt=*)"push by leave(",new_leave%measure(),")\\"!PSTRICKS
    !flush(11)!PSTRICKS
    if (new_leave<=this%leftmost) then
       old_leave=>this%leftmost
       this%leftmost=>new_leave
       node=>old_leave%up
       call fibonacci_node_spawn(new_node,new_leave,old_leave,old_leave%left,old_leave%right)
       call node%append_left(new_node)
    else
       if (new_leave>this%rightmost) then
          old_leave=>this%rightmost
          this%rightmost=>new_leave
          node=>old_leave%up
          call fibonacci_node_spawn(new_node,old_leave,new_leave,old_leave%left,old_leave%right)
          call node%append_right(new_node)
       else
          node=>this
          do
             if (new_leave<=node) then
                leave_c=>node%left
                select type (leave_c)
                class is (fibonacci_leave_type)
                   if(new_leave<=leave_c)then
!                      print *,"left left"
                      call fibonacci_node_spawn(new_node,new_leave,leave_c,leave_c%left,leave_c%right)
                   else
!                      print *,"left right"
                      call fibonacci_node_spawn(new_node,leave_c,new_leave,leave_c%left,leave_c%right)
                   end if
                   call node%append_left(new_node)
                   exit
                class default
!                   print *,"left"
                   node=>node%left
                end select
             else
                leave_c=>node%right
                select type (leave_c)
                class is (fibonacci_leave_type)          
                   if(new_leave<=leave_c)then
!                      print *,"right left"
                      call fibonacci_node_spawn(new_node,new_leave,leave_c,leave_c%left,leave_c%right)
                   else
!                      print *,"right right"
                      call fibonacci_node_spawn(new_node,leave_c,new_leave,leave_c%left,leave_c%right)
                   end if
                   call node%append_right(new_node)
                   exit
                class default
!                   print *,"right"
                   node=>node%right
                end select
             end if
          end do
       end if
    end if
    !call this%write_pstricks(11)!PSTRICKS
    !flush(11)!PSTRICKS
    !write(11,fmt=*)"repair\\"!PSTRICKS
    call node%repair()
    !call this%write_pstricks(11)!PSTRICKS
    !flush(11)!PSTRICKS
!    call node%update_value(right_value)
!    call this%write_pstricks(11)
!    print *,new_node%value,new_node%left%value,new_node%right%value
  end subroutine fibonacci_root_push_by_leave

  subroutine fibonacci_root_pop_left(this,leave)
    class(fibonacci_root_type),intent(inout),target  :: this
    class(fibonacci_leave_type),pointer,intent(out) :: leave
    class(fibonacci_node_type),pointer  :: parent,grand
    !write(11,fmt=*)"fibonacci root pop left\\"!PSTRICKS
    !flush(11)!PSTRICKS
    leave => this%leftmost
    if (this%left%depth>=1) then
       parent => leave%up
       grand=>parent%up
       grand%left => parent%right
       parent%right%up=>grand
       deallocate(parent)
       parent=>grand%left
       if (.not.parent%is_leave())then
          parent=>parent%left
       end if
       select type (parent)
       class is (fibonacci_leave_type)
          this%leftmost => parent
       class default
          print *,"fibonacci_root_pop_left: ERROR: leftmost is no leave."
          call parent%print_all()
          STOP
       end select
       !call this%write_pstricks(11)!PSTRICKS
       !flush(11)!PSTRICKS
       !write(11,fmt=*)"fibonacci node repair\\"!PSTRICKS
       !flush(11)!PSTRICKS
       call grand%repair()
    else
       if (this%left%depth==0.and.this%right%depth==1) then
          parent => this%right
          parent%right%up=>this
          parent%left%up=>this
          this%left=>parent%left
          this%right=>parent%right
          this%depth=1
          deallocate(parent)
          parent=>this%left
          select type (parent)
          class is (fibonacci_leave_type)
          this%leftmost => parent
          end select
          this%down=>this%leftmost%down
       end if
    end if
    nullify(leave%right%left)
    nullify(leave%up)
    nullify(leave%right)
    nullify(this%leftmost%left)
    !call this%write_pstricks(11)!PSTRICKS
    !flush(11)!PSTRICKS
  end subroutine fibonacci_root_pop_left

  subroutine fibonacci_root_pop_right(this,leave)
    class(fibonacci_root_type),intent(inout),target  :: this
    class(fibonacci_leave_type),pointer,intent(out) :: leave
    class(fibonacci_node_type),pointer  :: parent,grand
    leave => this%rightmost
    if (this%right%depth>=1) then
       parent => leave%up
       grand=>parent%up
       grand%right => parent%left
       parent%left%up=>grand
       deallocate(parent)
       parent=>grand%right
       if (.not.parent%is_leave())then
          parent=>parent%right
       end if
       select type (parent)
       class is (fibonacci_leave_type)
          this%rightmost => parent
       class default
          print *,"fibonacci_root_pop_left: ERROR: leftmost is no leave."
          call parent%print_all()
          STOP
       end select
       call grand%repair()
    else
       if (this%right%depth==0.and.this%left%depth==1) then
          parent => this%left
          parent%left%up=>this
          parent%right%up=>this
          this%right=>parent%right
          this%left=>parent%left
          this%depth=1
          deallocate(parent)
          parent=>this%right
          select type (parent)
          class is (fibonacci_leave_type)
          this%rightmost => parent
          end select
          this%down=>this%rightmost%down
       end if
    end if
  end subroutine fibonacci_root_pop_right

  subroutine fibonacci_root_merge(this_tree,that_tree,merge_tree)
    ! I neither used nor revised this procedure for a long time, so it might be broken.
    class(fibonacci_root_type),intent(in) :: this_tree
    class(fibonacci_root_type),intent(in) :: that_tree
    class(fibonacci_root_type),pointer,intent(out) :: merge_tree
    class(fibonacci_leave_type),pointer :: this_leave,that_leave,old_leave
    type(fibonacci_leave_list_type),target :: leave_list
    class(fibonacci_leave_list_type),pointer :: last_leave
    integer :: n_leaves
    if (associated(this_tree%leftmost).and.associated(that_tree%leftmost)) then
       n_leaves=1
       this_leave=>this_tree%leftmost
       that_leave=>that_tree%leftmost
       if (this_leave < that_leave) then
          allocate(leave_list%leave,source=this_leave)
          call this_leave%find_right_leave(this_leave)
       else
          allocate(leave_list%leave,source=that_leave)
          call that_leave%find_right_leave(that_leave)
       end if
       last_leave=>leave_list
       do while (associated(this_leave).and.associated(that_leave))
          if (this_leave < that_leave) then
             old_leave=>this_leave
             call this_leave%find_right_leave(this_leave)
          else
             old_leave=>that_leave
             call that_leave%find_right_leave(that_leave)
          end if
          allocate(last_leave%next)
          last_leave=>last_leave%next
          allocate(last_leave%leave,source=old_leave)
          n_leaves=n_leaves+1
       end do
       if (associated(this_leave)) then
          old_leave=>this_leave
       else
          old_leave=>that_leave
       end if
       do while (associated(old_leave))
          allocate(last_leave%next)
          last_leave=>last_leave%next
          allocate(last_leave%leave,source=old_leave)
          n_leaves=n_leaves+1
          call old_leave%find_right_leave(old_leave)
       end do
       allocate(merge_tree)
       call fibonacci_root_list_to_tree(merge_tree,n_leaves,leave_list)
    else
       n_leaves=0
    end if
    if(associated(leave_list%next)) then
       last_leave=>leave_list%next
       do while (associated(last_leave%next))
          leave_list%next=>last_leave%next
          deallocate(last_leave)
          last_leave=>leave_list%next
       end do
       deallocate(last_leave)
    end if
  end subroutine fibonacci_root_merge

  subroutine fibonacci_root_list_to_tree(this,n_leaves,leave_list_target)
    class(fibonacci_root_type),target :: this
    integer,intent(in) :: n_leaves
    type(fibonacci_leave_list_type),target,intent(in) :: leave_list_target
!    class(fibonacci_root_type),pointer,intent(out) :: tree
    integer:: depth,n_deep,n_merge
    class(fibonacci_node_type),pointer :: node
    class(fibonacci_leave_list_type),pointer :: leave_list
    class(fibonacci_leave_type),pointer::content
    real(kind=double) :: up_value
    leave_list=>leave_list_target
    call ilog2(n_leaves,depth,n_deep)
    n_deep=n_deep*2
    n_merge=0
    this%depth=depth
    node=>this
    outer: do
       do while(depth>1)
          depth=depth-1
          allocate(node%left)
          node%left%up=>node
          node=>node%left
          node%depth=depth
       end do
       node%left=>leave_list%leave
       node%down=>leave_list%leave%down
       leave_list=>leave_list%next
       node%right=>leave_list%leave
       content => leave_list%leave
       leave_list=>leave_list%next
       n_merge=n_merge+2
       inner: do
          if (associated(node%up)) then
             if (node%is_left_child()) then
                if (n_merge==n_deep.and.depth==1) then
                   node=>node%up
                   node%right=>leave_list%leave
                   node%right%up=>node
                   node%down=>content%down
                   content=>leave_list%leave
                   leave_list=>leave_list%next
                   n_merge=n_merge+1
                   cycle
                end if
                exit inner
             else
                node=>node%up
                depth=depth+1
             end if
          else
             exit outer
          end if
       end do inner
       node=>node%up
       node%down=>content%down
       allocate(node%right)
       node%right%up => node
       node=>node%right
       if (n_deep==n_merge) then
          depth=depth-1
       end if
       node%depth=depth
    end do outer
    call this%set_leftmost
    call this%set_rightmost
  end subroutine fibonacci_root_list_to_tree

  subroutine fibonacci_root_set_leftmost(this)
    class(fibonacci_root_type) :: this
    call this%find_leftmost(this%leftmost)
  end subroutine fibonacci_root_set_leftmost

  subroutine fibonacci_root_set_rightmost(this)
    class(fibonacci_root_type) :: this
    call this%find_rightmost(this%rightmost)
  end subroutine fibonacci_root_set_rightmost

  subroutine fibonacci_root_init_by_leave(this,left_leave,right_leave)
    class(fibonacci_root_type),target,intent(out) :: this
    class(fibonacci_leave_type),target,intent(in) :: left_leave,right_leave
    if (left_leave <= right_leave) then
       this%left  =>  left_leave
       this%right => right_leave
       this%leftmost => left_leave
       this%rightmost => right_leave
    else
       this%left  => right_leave
       this%right  => left_leave
       this%leftmost => right_leave
       this%rightmost => left_leave
    end if
    this%left%up => this
    this%right%up => this
    this%down=>this%leftmost%down
    this%depth = 1
    this%leftmost%right=>this%rightmost
    this%rightmost%left=>this%leftmost
    this%is_valid_c=.true.
  end subroutine fibonacci_root_init_by_leave

  subroutine fibonacci_root_init_by_content(this,left_content,right_content)
    class(fibonacci_root_type),target,intent(out) :: this
    class(measurable_class),intent(in),target :: left_content,right_content
    call fibonacci_root_reset(this)
    print *,"fibonacci_root_init_by_content: ",left_content%measure(),right_content%measure()
    if (left_content<right_content) then
       call this%leftmost%set_content(left_content)
       call this%rightmost%set_content(right_content)
    else
       call this%leftmost%set_content(right_content)
       call this%rightmost%set_content(left_content)
    end if
    this%down=>this%leftmost%down
    this%is_valid_c=.true.
  end subroutine fibonacci_root_init_by_content

  subroutine fibonacci_root_reset(this)
    class(fibonacci_root_type),target,intent(inout) :: this
    call fibonacci_root_deallocate_tree(this)
    allocate (this%leftmost)
    allocate (this%rightmost) 
    this%depth=1
    this%leftmost%depth=0
    this%rightmost%depth=0
    this%left=>this%leftmost
    this%right=>this%rightmost
    this%left%up=>this
    this%right%up=>this
    this%leftmost%right=>this%rightmost
    this%rightmost%left=>this%leftmost
  end subroutine fibonacci_root_reset

  recursive subroutine fibonacci_root_deallocate_tree(this)
    class(fibonacci_root_type),intent(inout) :: this
    call fibonacci_node_deallocate_tree(this)
    nullify(this%leftmost)
    nullify(this%rightmost)
  end subroutine fibonacci_root_deallocate_tree

  recursive subroutine fibonacci_root_deallocate_all(this)
    class(fibonacci_root_type),intent(inout) :: this
    call fibonacci_node_deallocate_all(this)
    nullify(this%leftmost)
    nullify(this%rightmost)
  end subroutine fibonacci_root_deallocate_all

  elemental logical function fibonacci_root_is_left_child(this)
    class(fibonacci_root_type),target,intent(in) :: this
    fibonacci_root_is_left_child = .false.
  end function fibonacci_root_is_left_child

  elemental logical function fibonacci_root_is_right_child(this)
    class(fibonacci_root_type),target,intent(in) :: this
    fibonacci_root_is_right_child = .false.
  end function fibonacci_root_is_right_child

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Type Bound Procedures for fibonacci_stub_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE fibonacci_stub_write_to_marker(this,marker,status)
    CLASS(fibonacci_stub_type), INTENT(IN) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(inout)::status
  end SUBROUTINE fibonacci_stub_write_to_marker

  SUBROUTINE fibonacci_stub_read_target_from_marker(this,marker,status)
    CLASS(fibonacci_stub_type),target,INTENT(out) :: this
    class(marker_type),intent(inout)::marker
    integer(kind=dik),intent(inout)::status
  end SUBROUTINE fibonacci_stub_read_target_from_marker

  pure subroutine fibonacci_stub_get_type(type)
    character(:),allocatable,intent(out)::type
    allocate(type,source="fibonacci_stub_type")
  end subroutine fibonacci_stub_get_type

  subroutine fibonacci_stub_push_by_content(this,content)
    class(fibonacci_stub_type),target,intent(inout)  :: this
    class(measurable_class),target,intent(in)::content
    class(fibonacci_leave_type),pointer::leave
    allocate(leave)
    call leave%set_content(content)
    call this%push_by_leave(leave)
  end subroutine fibonacci_stub_push_by_content
  
  subroutine fibonacci_stub_push_by_leave(this,new_leave)
    class(fibonacci_stub_type),target,intent(inout)   :: this
    class(fibonacci_leave_type),pointer,intent(inout) :: new_leave
    class(fibonacci_leave_type),pointer::old_leave
    if(this%depth<1)then
       if(associated(this%leftmost))then     
          old_leave=>this%leftmost
          call this%init_by_leave(old_leave,new_leave)
       else
          this%leftmost=>new_leave
       end if
    else
       call fibonacci_root_push_by_leave(this,new_leave)
    end if
  end subroutine fibonacci_stub_push_by_leave

  subroutine fibonacci_stub_pop_left(this,leave)
    class(fibonacci_stub_type),intent(inout),target  :: this
    class(fibonacci_leave_type),pointer,intent(out) :: leave
    if(this%depth<2)then
       if(this%depth==1)then
          leave=>this%leftmost
          this%leftmost=>this%rightmost
          nullify(this%rightmost)
          nullify(this%right)
          nullify(this%left)
          this%depth=0
          this%is_valid_c=.false.
       else
          if(associated(this%leftmost))then
             leave=>this%leftmost
             nullify(this%leftmost)
          end if
       end if
    else
       call fibonacci_root_pop_left(this,leave)
    end if
  end subroutine fibonacci_stub_pop_left

  subroutine fibonacci_stub_pop_right(this,leave)
    class(fibonacci_stub_type),intent(inout),target  :: this
    class(fibonacci_leave_type),pointer,intent(out) :: leave
    if(this%depth<2)then
       if(this%depth==1)then
          this%is_valid_c=.false.
          if(associated(this%rightmost))then
             leave=>this%rightmost
             nullify(this%rightmost)
             nullify(this%right)
          else
             if(associated(this%leftmost))then
                leave=>this%leftmost
                nullify(this%leftmost)
                nullify(this%left)
             else
                nullify(leave)
             end if
          end if
       end if
    else
       call fibonacci_root_pop_right(this,leave)
    end if
  end subroutine fibonacci_stub_pop_right

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Type Bound Procedures for fibonacci_leave_type !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  pure subroutine fibonacci_leave_get_type(type)
    character(:),allocatable,intent(out)::type
    allocate(type,source="fibonacci_leave_type")
  end subroutine fibonacci_leave_get_type

  subroutine fibonacci_leave_print_to_unit(this,unit,parents,components,peers)
    class(fibonacci_leave_type),intent(in)::this
    integer,intent(in)::unit
    integer(kind=dik),intent(in)::parents,components,peers
    class(serializable_class),pointer::ser
    if(parents>0)call fibonacci_node_print_to_unit(this,unit,parents-one,components,-one)
    write(unit,'("Components of fibonacci_leave_type:")')
    ser=>this%down
    call serialize_print_comp_pointer(ser,unit,parents,components,peers,"Content:")
  end subroutine fibonacci_leave_print_to_unit

  subroutine fibonacci_leave_get_left(this,leave)
    class(fibonacci_leave_type),intent(in) :: this
    class(fibonacci_leave_type),intent(out),pointer :: leave
    class(fibonacci_node_type),pointer::node
    node=>this%left
    select type(node)
    class is (fibonacci_leave_type)
       leave=>node
    end select
  end subroutine fibonacci_leave_get_left
  
  subroutine fibonacci_leave_get_right(this,leave)
    class(fibonacci_leave_type),intent(in) :: this
    class(fibonacci_leave_type),intent(out),pointer :: leave
    class(fibonacci_node_type),pointer::node
!    print *,"fibonacci_leave_get_right"
!    call this%down%print_little
    if(associated(this%right))then
       node=>this%right
!       call node%down%print_little
       select type(node)
       class is (fibonacci_leave_type)
          leave=>node
       end select
    else
!       print *,"no right leave"
       nullify(leave)
    end if
  end subroutine fibonacci_leave_get_right
  
  subroutine fibonacci_leave_deallocate_all(this)
    class(fibonacci_leave_type),intent(inout) :: this
    if (associated(this%down)) then
       deallocate(this%down)
    end if
  end subroutine fibonacci_leave_deallocate_all

  subroutine fibonacci_leave_write_pstricks(this,unitnr)
    class(fibonacci_leave_type),intent(in),target :: this
    integer,intent(in) :: unitnr
    write(unitnr,'("\begin{psTree}{\Toval[linecolor=green]{\node{",i3,"}{",f9.3,"}}}")') this%depth,this%measure()
    if (associated(this%left)) then
       write(unitnr,'("\Tr[",a,"]{}")') le_kid
    end if
    if (associated(this%right)) then
       write(unitnr,'("\Tr[",a,"]{}")') le_kid
    end if
    write(unitnr,'("\end{psTree}")')
  end subroutine fibonacci_leave_write_pstricks

  subroutine fibonacci_leave_insert_leave_by_node(this,new_leave)
    class(fibonacci_leave_type),target,intent(inout) :: this,new_leave
    class(fibonacci_node_type),pointer :: parent,new_node
    parent=>this%up
    !print *,associated(this%left),associated(this%right)
    if(this<new_leave)then
       call fibonacci_node_spawn(new_node,this,new_leave,this%left,this%right)
       !print *,"Repair! ",this%measure(),new_leave%measure()
    else
       call fibonacci_node_spawn(new_node,new_leave,this,this%left,this%right)
    end if
    if(associated(parent%left,this))then
       call parent%append_left(new_node)
    else
       call parent%append_right(new_node)
    end if
    call parent%repair()
  end subroutine fibonacci_leave_insert_leave_by_node

  subroutine fibonacci_leave_copy_content(this,content)
    class(fibonacci_leave_type) :: this
    class(measurable_class),intent(in) :: content
    allocate(this%down,source=content)
  end subroutine fibonacci_leave_copy_content

  subroutine fibonacci_leave_set_content(this,content)
    class(fibonacci_leave_type) :: this
    class(measurable_class),target,intent(in) :: content
    this%down => content
  end subroutine fibonacci_leave_set_content

  subroutine fibonacci_leave_get_content(this,content)
    class(fibonacci_leave_type),intent(in) :: this
    class(measurable_class),pointer :: content
    content => this%down
  end subroutine fibonacci_leave_get_content

  elemental logical function fibonacci_leave_is_inner()
    fibonacci_leave_is_inner = .false.
  end function fibonacci_leave_is_inner

  elemental logical function fibonacci_leave_is_leave()
    fibonacci_leave_is_leave = .true.
  end function fibonacci_leave_is_leave

  elemental logical function fibonacci_leave_is_left_short(this)
    class(fibonacci_leave_type),intent(in) :: this
    fibonacci_leave_is_left_short = .false.
  end function fibonacci_leave_is_left_short

  elemental logical function fibonacci_leave_is_right_short(this)
    class(fibonacci_leave_type),intent(in) :: this
    fibonacci_leave_is_right_short = .false.
  end function fibonacci_leave_is_right_short

  elemental logical function fibonacci_leave_is_unbalanced(this)
    class(fibonacci_leave_type),intent(in) :: this
    fibonacci_leave_is_unbalanced = .false.
  end function fibonacci_leave_is_unbalanced

  elemental logical function fibonacci_leave_is_left_too_short(this)
    class(fibonacci_leave_type),intent(in) :: this
    fibonacci_leave_is_left_too_short = .false.
  end function fibonacci_leave_is_left_too_short

  elemental logical function fibonacci_leave_is_right_too_short(this)
    class(fibonacci_leave_type),intent(in) :: this
    fibonacci_leave_is_right_too_short = .false.
  end function fibonacci_leave_is_right_too_short

  elemental logical function fibonacci_leave_is_too_unbalanced(this)
    class(fibonacci_leave_type),intent(in) :: this
    fibonacci_leave_is_too_unbalanced = .false.
  end function fibonacci_leave_is_too_unbalanced

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! Non Type Bound Procedures !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine fibonacci_leave_write_content(this,unit)
    class(fibonacci_leave_type),intent(in),target :: this
    integer,optional,intent(in)::unit
    call this%down%print_all(unit)
  end subroutine fibonacci_leave_write_content

  subroutine fibonacci_leave_write(this,unit)
    class(fibonacci_leave_type),intent(in),target :: this
    integer,optional,intent(in)::unit
    call this%print_all(unit)
  end subroutine fibonacci_leave_write

  subroutine fibonacci_leave_write_value(this,unit)
    class(fibonacci_leave_type),intent(in),target :: this
    integer,intent(in),optional::unit
    if(present(unit))then
       write(unit,fmt=*)this%measure()
    else
       print *,this%measure()
    end if
!    call this%print_little(unit)
  end subroutine fibonacci_leave_write_value

  subroutine fibonacci_node_spawn(new_node,left_leave,right_leave,left_left_leave,right_right_leave)
    class(fibonacci_node_type),pointer,intent(out) :: new_node
    class(fibonacci_leave_type),target,intent(inout) :: left_leave,right_leave
    class(fibonacci_node_type),pointer,intent(inout) :: left_left_leave,right_right_leave
    allocate(new_node)
    new_node%depth=1
    if(associated(left_left_leave))then
       left_left_leave%right=>left_leave
       left_leave%left=>left_left_leave
    else
       nullify(left_leave%left)
    end if
    if(associated(right_right_leave))then
       right_right_leave%left=>right_leave
       right_leave%right=>right_right_leave
    else
       nullify(right_leave%right)
    end if
    new_node%left=>left_leave
    new_node%right=>right_leave
    new_node%down=>left_leave%down
    new_node%depth=1
    left_leave%up=>new_node
    right_leave%up=>new_node
    left_leave%right=>right_leave
    right_leave%left=>left_leave
  end subroutine fibonacci_node_spawn

end module muli_fibonacci_tree

