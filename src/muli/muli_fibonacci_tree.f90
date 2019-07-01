
module muli_fibonacci_tree

  use kinds, only: default
  use diagnostics
  use muli_base

  implicit none
  private

  public :: fibonacci_node_t
  public :: fibonacci_leave_t
  public :: fibonacci_root_t
  public :: fibonacci_leave_list_t

  character(*), parameter :: no_par = "edge=\noparent"
  character(*), parameter :: no_ret = "edge=\noreturn"
  character(*), parameter :: no_kid = "edge=\nochild"
  character(*), parameter :: le_kid = "edge=\childofleave"


  type, extends (measure_class_t) :: fibonacci_node_t
     ! private
     class(fibonacci_node_t), pointer :: up => null()
     class(measure_class_t), pointer :: down => null()
     class(fibonacci_node_t), pointer :: left => null()
     class(fibonacci_node_t), pointer :: right => null()
     integer :: depth = 0
     ! real(default) :: value
   contains
     procedure :: write_to_marker => fibonacci_node_write_to_marker
     procedure :: read_from_marker => fibonacci_node_read_from_marker
     procedure :: read_target_from_marker => fibonacci_node_read_target_from_marker
     procedure :: print_to_unit => fibonacci_node_print_to_unit
     procedure, nopass :: get_type => fibonacci_node_get_type
     procedure :: deserialize_from_marker => fibonacci_node_deserialize_from_marker
     procedure :: measure => fibonacci_node_measure
     procedure :: deallocate_tree => fibonacci_node_deallocate_tree
     procedure :: deallocate_all => fibonacci_node_deallocate_all
     procedure :: get_depth => fibonacci_node_get_depth
     procedure :: count_leaves => fibonacci_node_count_leaves
     procedure,public,nopass :: is_leave => fibonacci_node_is_leave
     procedure,public,nopass :: is_root => fibonacci_node_is_root
     procedure,public,nopass :: is_inner => fibonacci_node_is_inner
     procedure :: write_association => fibonacci_node_write_association
     procedure :: write_contents => fibonacci_node_write_contents
     procedure :: write_values => fibonacci_node_write_values
     procedure :: write_leaves => fibonacci_node_write_leaves
     ! procedure :: write => fibonacci_node_write_contents
     procedure :: write_pstricks => fibonacci_node_write_pstricks
     procedure :: copy_node => fibonacci_node_copy_node
     procedure :: find_root => fibonacci_node_find_root
     procedure :: find_leftmost => fibonacci_node_find_leftmost
     procedure :: find_rightmost => fibonacci_node_find_rightmost
     procedure :: find => fibonacci_node_find
     procedure :: find_left_leave => fibonacci_node_find_left_leave
     procedure :: find_right_leave => fibonacci_node_find_right_leave
     procedure :: apply_to_leaves => fibonacci_node_apply_to_leaves
     procedure :: apply_to_leaves_rl => fibonacci_node_apply_to_leaves_rl
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
     ! procedure :: balance
     ! procedure :: sort
     ! procedure :: merge
     ! procedure :: split
  end type fibonacci_node_t

  type, extends (fibonacci_node_t) :: fibonacci_leave_t
     ! class(measure_class_t), pointer :: content
  contains
    ! procedure :: write_to_marker => fibonacci_leave_write_to_marker
    ! procedure :: read_from_marker => fibonacci_leave_read_from_marker
    procedure :: print_to_unit => fibonacci_leave_print_to_unit
    procedure, nopass :: get_type => fibonacci_leave_get_type
    procedure :: deallocate_all => fibonacci_leave_deallocate_all
    procedure :: pick => fibonacci_leave_pick
    procedure :: get_left => fibonacci_leave_get_left
    procedure :: get_right => fibonacci_leave_get_right
    procedure :: write_pstricks => fibonacci_leave_write_pstricks
    procedure :: copy_content => fibonacci_leave_copy_content
    procedure :: set_content => fibonacci_leave_set_content
    procedure :: get_content => fibonacci_leave_get_content
    procedure, nopass :: is_inner => fibonacci_leave_is_inner
    procedure, nopass :: is_leave => fibonacci_leave_is_leave
    procedure :: insert_leave_by_node => fibonacci_leave_insert_leave_by_node
    procedure :: is_left_short => fibonacci_leave_is_left_short
    procedure :: is_right_short => fibonacci_leave_is_right_short
    procedure :: is_unbalanced => fibonacci_leave_is_unbalanced
    procedure :: is_left_too_short => fibonacci_leave_is_left_too_short
    procedure :: is_right_too_short => fibonacci_leave_is_right_too_short
    procedure :: is_too_unbalanced => fibonacci_leave_is_too_unbalanced
  end type fibonacci_leave_t

  type, extends (fibonacci_node_t) :: fibonacci_root_t
     logical::is_valid_c=.false.
     class(fibonacci_leave_t),pointer :: leftmost => null()
     class(fibonacci_leave_t),pointer :: rightmost => null()
  contains
    procedure :: write_to_marker => fibonacci_root_write_to_marker
    procedure :: read_target_from_marker => fibonacci_root_read_target_from_marker
    procedure :: print_to_unit => fibonacci_root_print_to_unit
    procedure, nopass :: get_type => fibonacci_root_get_type
    procedure :: get_leftmost=>fibonacci_root_get_leftmost
    procedure :: get_rightmost=>fibonacci_root_get_rightmost
    procedure, nopass :: is_root => fibonacci_root_is_root
    procedure, nopass :: is_inner => fibonacci_root_is_inner
    procedure :: is_valid => fibonacci_root_is_valid
    procedure :: count_leaves => fibonacci_root_count_leaves
    procedure :: write_pstricks => fibonacci_root_write_pstricks
    procedure :: copy_root => fibonacci_root_copy_root
    procedure :: push_by_content => fibonacci_root_push_by_content
    procedure :: push_by_leave => fibonacci_root_push_by_leave
    procedure :: pop_left => fibonacci_root_pop_left
    procedure :: pop_right => fibonacci_root_pop_right
    procedure :: list_to_tree => fibonacci_root_list_to_tree
    procedure :: merge => fibonacci_root_merge
    procedure :: set_leftmost => fibonacci_root_set_leftmost
    procedure :: set_rightmost => fibonacci_root_set_rightmost
    procedure :: init_by_leave => fibonacci_root_init_by_leave
    procedure :: init_by_content => fibonacci_root_init_by_content
    procedure :: reset => fibonacci_root_reset
    procedure :: deallocate_tree => fibonacci_root_deallocate_tree
    procedure :: deallocate_all => fibonacci_root_deallocate_all
     procedure :: is_left_child => fibonacci_root_is_left_child
     procedure :: is_right_child => fibonacci_root_is_right_child
  end type fibonacci_root_t

  ! class(serializable_ref_type), pointer :: ref_list
  type, extends (fibonacci_root_t) :: fibonacci_stub_t
   contains
     procedure :: write_to_marker => fibonacci_stub_write_to_marker
     procedure :: read_target_from_marker => fibonacci_stub_read_target_from_marker
     ! procedure :: print_to_unit => fibonacci_stub_print_to_unit
     procedure, nopass :: get_type => fibonacci_stub_get_type
     procedure :: push_by_content => fibonacci_stub_push_by_content
     procedure :: push_by_leave => fibonacci_stub_push_by_leave
     procedure :: pop_left => fibonacci_stub_pop_left
     procedure :: pop_right => fibonacci_stub_pop_right
  end type fibonacci_stub_t

  type fibonacci_leave_list_t
     class(fibonacci_leave_t), pointer :: leave => null()
     class(fibonacci_leave_list_t), pointer :: next => null()
  end type fibonacci_leave_list_t


contains

  recursive  subroutine fibonacci_node_write_to_marker (this, marker, status)
    class(fibonacci_node_t), intent(in) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    class(ser_class_t), pointer :: ser
    call marker%mark_begin ("fibonacci_node_t")
    ser => this%left
    call marker%mark_pointer ("left", ser)
    ser => this%right
    call marker%mark_pointer ("right", ser)
    ser => this%down
    call marker%mark_pointer ("down", ser)
    call marker%mark_end ("fibonacci_node_t")
  end subroutine fibonacci_node_write_to_marker

  recursive subroutine fibonacci_node_read_from_marker (this, marker, status)
    class(fibonacci_node_t), intent(out) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    call msg_warning ("fibonacci_node_read_from_marker: You cannot " // &
         "deserialize a list with this subroutine.")
    call msg_error ("Use fibonacci_node_read_target_from_marker instead.")
  end subroutine fibonacci_node_read_from_marker

  recursive subroutine fibonacci_node_read_target_from_marker &
       (this, marker, status)
    class(fibonacci_node_t), target, intent(out) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    class(ser_class_t), pointer :: ser
    call marker%pick_begin ("fibonacci_node_t", status=status)
    call marker%pick_pointer ("left", ser)
    if (status == 0) then
       select type (ser)
       class is (fibonacci_node_t)
          this%left => ser
          this%left%up => this
       end select
    end if
    call marker%pick_pointer ("right", ser)
    if (status == 0) then
       select type (ser)
       class is (fibonacci_node_t)
          this%right => ser
          this%right%up => this
       end select
    end if
    call marker%pick_pointer ("down", ser)
    if (status == 0) then
       select type (ser)
       class is (measure_class_t)
          this%down => ser
       end select
    end if
    call marker%pick_end ("fibonacci_node_t", status)
  end subroutine fibonacci_node_read_target_from_marker

  recursive subroutine fibonacci_node_print_to_unit &
       (this, unit, parents, components, peers)
    class(fibonacci_node_t), intent(in) :: this
    integer, intent(in) :: unit
    integer(dik), intent(in) :: parents, components, peers
    class(ser_class_t), pointer :: ser
    write (unit, "(1x,A)")        "Components of fibonacci_node_t:"
    write (unit, "(3x,A,I22)")    "Depth:   ", this%depth
    write (unit, "(3x,A,E23.16)") "Value:   ", this%measure ()
    ser => this%up
    call serialize_print_comp_pointer &
         (ser, unit, parents, -i_one, -i_one, "Up:     ")
    ser => this%left
    call serialize_print_peer_pointer &
         (ser, unit, parents, components, peers, "Left:   ")
    ser => this%right
    call serialize_print_peer_pointer &
         (ser, unit, parents, components, peers, "Right:  ")
  end subroutine fibonacci_node_print_to_unit

  pure subroutine fibonacci_node_get_type (type)
    character(:), allocatable, intent(out) :: type
    allocate (type, source="fibonacci_node_t")
  end subroutine fibonacci_node_get_type

  subroutine fibonacci_node_deserialize_from_marker (this, name, marker)
    class(fibonacci_node_t), intent(out) :: this
    character(*), intent(in) :: name
    class(marker_t), intent(inout) :: marker
    class(ser_class_t), pointer :: ser
    allocate (fibonacci_leave_t :: ser)
    call marker%push_reference (ser)
    allocate (fibonacci_node_t :: ser)
    call marker%push_reference (ser)
    call serializable_deserialize_from_marker (this, name, marker)
    call marker%pop_reference (ser)
    deallocate (ser)
    call marker%pop_reference (ser)
    deallocate (ser)
  end subroutine fibonacci_node_deserialize_from_marker

  elemental function fibonacci_node_measure (this)
    class(fibonacci_node_t), intent(in) :: this
    real(default) :: fibonacci_node_measure
    fibonacci_node_measure = this%down%measure ()
  end function fibonacci_node_measure

  recursive subroutine fibonacci_node_deallocate_tree (this)
    class(fibonacci_node_t), intent(inout) :: this
    if (associated (this%left)) then
       call this%left%deallocate_tree ()
       deallocate (this%left)
    end if
    if (associated (this%right)) then
       call this%right%deallocate_tree ()
       deallocate (this%right)
    end if
    call this%set_depth (0)
  end subroutine fibonacci_node_deallocate_tree

  recursive subroutine fibonacci_node_deallocate_all (this)
    class(fibonacci_node_t), intent(inout) :: this
    if (associated (this%left)) then
       call this%left%deallocate_all ()
       deallocate (this%left)
    end if
    if (associated (this%right)) then
       call this%right%deallocate_all ()
       deallocate (this%right)
    end if
    call this%set_depth (0)
  end subroutine fibonacci_node_deallocate_all

  elemental function fibonacci_node_get_depth (this)
    class(fibonacci_node_t), intent(in) :: this
    integer :: fibonacci_node_get_depth
    fibonacci_node_get_depth = this%depth
  end function fibonacci_node_get_depth

  recursive subroutine fibonacci_node_count_leaves (this, n)
    class(fibonacci_node_t), intent(in) :: this
    integer, intent(out) :: n
    integer :: n1, n2
    if (associated (this%left) .and. associated (this%right)) then
       call fibonacci_node_count_leaves (this%left, n1)
       call fibonacci_node_count_leaves (this%right, n2)
       n = n1 + n2
    else
       n = 1
    end if
  end subroutine fibonacci_node_count_leaves

  elemental function fibonacci_node_is_leave ()
    logical :: fibonacci_node_is_leave
    fibonacci_node_is_leave = .false.
  end function fibonacci_node_is_leave

  elemental function fibonacci_node_is_root ()
    logical :: fibonacci_node_is_root
    fibonacci_node_is_root = .false.
  end function fibonacci_node_is_root

  elemental function fibonacci_node_is_inner ()
    logical :: fibonacci_node_is_inner
    fibonacci_node_is_inner = .true.
  end function fibonacci_node_is_inner

  subroutine fibonacci_node_write_association (this, that)
    class(fibonacci_node_t), intent(in), target :: this
    class(fibonacci_node_t), intent(in), target :: that
    if (associated (that%left, this)) then
       write(*, "(A)")  "This is left child of that"
    end if
    if (associated (that%right, this)) then
       write(*, "(A)")  "This is right child of that"
    end if
    if (associated (that%up, this)) then
       write(*, "(A)")  "This is parent of that"
    end if
    if (associated (this%left, that)) then
       write(*, "(A)")  "That is left child of this"
    end if
    if (associated (this%right, that)) then
       write(*, "(A)")  "That is right child of this"
    end if
    if (associated (this%up, that)) then
       write(*, "(A)")  "That is parent of this"
    end if
  end subroutine fibonacci_node_write_association

  subroutine fibonacci_node_write_contents (this, unit)
    class(fibonacci_node_t), intent(in), target :: this
    integer, intent(in), optional :: unit
    call this%apply_to_leaves (fibonacci_leave_write_content, unit)
  end subroutine fibonacci_node_write_contents

  subroutine fibonacci_node_write_values (this, unit)
    class(fibonacci_node_t), intent(in), target :: this
    integer, intent(in), optional :: unit
    call this%apply_to_leaves (fibonacci_leave_write_value, unit)
  end subroutine fibonacci_node_write_values

  subroutine fibonacci_node_write_leaves (this, unit)
    class(fibonacci_node_t), intent(in), target :: this
    integer, intent(in),optional :: unit
    call this%apply_to_leaves (fibonacci_leave_write, unit)
  end subroutine fibonacci_node_write_leaves

  recursive subroutine fibonacci_node_write_pstricks (this, unitnr)
    class(fibonacci_node_t), intent(in), target :: this
    integer, intent(in) :: unitnr
    if (associated (this%up)) then
       if (associated (this%up%left, this) .neqv. &
          (associated (this%up%right, this))) then
          ! write (unitnr,'("\begin{psTree}{\Toval{$",i3,"$}}")') int(this%depth)
          write (unitnr, &
               '("\begin{psTree}{\Toval{\node{",i3,"}{",f9.3,"}}}")') &
               int(this%depth), this%measure()
       else
          write (unitnr, &
               '("\begin{psTree}{\Toval[",a,"]{\node{",i3,"}{",f9.3,"}}}")') &
               no_ret, int(this%depth), this%measure()
       end if
    else
       write (unitnr, &
            '("\begin{psTree}{\Toval[",a,"]{\node{",i3,"}{",f9.3,"}}}")') &
            no_par, int(this%depth), this%measure()
    end if
    if (associated (this%left)) then
       call this%left%write_pstricks (unitnr)
    else
       write (unitnr,'("\Tr[edge=brokenline]{}")')
    end if
    if (associated (this%right)) then
       call this%right%write_pstricks (unitnr)
    else
       write (unitnr, '("\Tr[edge=brokenline]{}")')
    end if
    write (unitnr, '("\end{psTree}")')
  end subroutine fibonacci_node_write_pstricks

  subroutine fibonacci_node_copy_node (this, primitive)
    class(fibonacci_node_t), intent(out) :: this
    class(fibonacci_node_t), intent(in) :: primitive
    this%up => primitive%up
    this%left => primitive%left
    this%right => primitive%right
    this%depth = primitive%depth
    this%down => primitive%down
  end subroutine fibonacci_node_copy_node

  subroutine fibonacci_node_find_root (this, root)
    class(fibonacci_node_t), intent(in), target :: this
    class(fibonacci_root_t), pointer, intent(out) :: root
    class(fibonacci_node_t), pointer :: node
    node => this
    do while (associated (node%up))
       node => node%up
    end do
    select type (node)
    class is (fibonacci_root_t)
       root => node
    class default
       nullify (root)
       call msg_error ("fibonacci_node_find_root: root is not type " // &
            "compatible to fibonacci_root_t. Retured NULL().")
    end select
  end subroutine fibonacci_node_find_root

  subroutine fibonacci_node_find_leftmost (this, leave)
    class(fibonacci_node_t), intent(in), target :: this
    class(fibonacci_leave_t), pointer, intent(out) :: leave
    class(fibonacci_node_t),  pointer :: node
    node => this
    do while (associated (node%left))
       node => node%left
    end do
    select type (node)
    class is (fibonacci_leave_t)
       leave => node
    class default
       leave => null()
    end select
  end subroutine fibonacci_node_find_leftmost

  subroutine fibonacci_node_find_rightmost (this, leave)
    class(fibonacci_node_t), intent(in), target :: this
    class(fibonacci_leave_t), pointer, intent(out) :: leave
    class(fibonacci_node_t), pointer :: node
    node => this
    do while (associated (node%right))
       node => node%right
    end do
    select type (node)
    class is (fibonacci_leave_t)
       leave => node
    class default
       leave => null()
    end select
  end subroutine fibonacci_node_find_rightmost

  subroutine fibonacci_node_find (this, value, leave)
    class(fibonacci_node_t), intent(in), target :: this
    real(default), intent(in) :: value
    class(fibonacci_leave_t), pointer, intent(out) :: leave
    class(fibonacci_node_t), pointer :: node
    node => this
    do
       if (node >= value) then
          if (associated (node%left)) then
             node => node%left
          else
             call msg_warning ("fibonacci_node_find: broken tree!")
             leave => null()
             return
          end if
       else
          if (associated (node%right)) then
             node => node%right
          else
             call msg_warning ("fibonacci_node_find: broken tree!")
             leave => null()
             return
          end if
       end if
       select type (node)
       class is (fibonacci_leave_t)
          leave => node
          exit
       end select
    end do
  end subroutine fibonacci_node_find

  subroutine fibonacci_node_find_left_leave (this, leave)
    class(fibonacci_node_t), intent(in), target :: this
    class(fibonacci_node_t), pointer :: node
    class(fibonacci_leave_t), pointer, intent(out) :: leave
    nullify(leave)
    node => this
    do while (associated (node%up))
       if (associated (node%up%right, node)) then
          node => node%up%left
          do while (associated (node%right))
             node => node%right
          end do
          select type (node)
          class is (fibonacci_leave_t)
          leave => node
          end select
          exit
       end if
       node => node%up
    end do
  end subroutine fibonacci_node_find_left_leave

  subroutine fibonacci_node_find_right_leave (this, leave)
    class(fibonacci_node_t), intent(in), target :: this
    class(fibonacci_node_t), pointer :: node
    class(fibonacci_leave_t), pointer, intent(out) :: leave
    nullify (leave)
    node => this
    do while (associated (node%up))
       if (associated (node%up%left, node)) then
          node => node%up%right
          do while (associated (node%left))
             node => node%left
          end do
          select type (node)
          class is (fibonacci_leave_t)
          leave => node
          end select
          exit
       end if
       node => node%up
    end do
  end subroutine fibonacci_node_find_right_leave

  recursive subroutine fibonacci_node_apply_to_leaves (node, func, unit)
    class(fibonacci_node_t), intent(in), target :: node
    interface
       subroutine func (this, unit)
         import fibonacci_leave_t
         class(fibonacci_leave_t), intent(in), target :: this
         integer, intent(in), optional :: unit
       end subroutine func
    end interface
    integer, intent(in), optional :: unit
    select type (node)
    class is (fibonacci_leave_t)
       call func (node, unit)
    class default
       call node%left%apply_to_leaves (func, unit)
       call node%right%apply_to_leaves (func, unit)
    end select
  end subroutine fibonacci_node_apply_to_leaves

  recursive subroutine fibonacci_node_apply_to_leaves_rl (node, func, unit)
    class(fibonacci_node_t), intent(in), target :: node
    interface
       subroutine func (this, unit)
         import fibonacci_leave_t
         class(fibonacci_leave_t), intent(in), target :: this
         integer, intent(in), optional :: unit
       end subroutine func
    end interface
    integer, intent(in), optional :: unit
    select type (node)
    class is (fibonacci_leave_t)
       call func (node, unit)
    class default
       call node%right%apply_to_leaves_rl (func, unit)
       call node%left%apply_to_leaves_rl (func, unit)
    end select
  end subroutine fibonacci_node_apply_to_leaves_rl

  subroutine fibonacci_node_set_depth (this, depth)
    class(fibonacci_node_t), intent(inout) :: this
    integer, intent(in) :: depth
    this%depth = depth
  end subroutine fibonacci_node_set_depth

  subroutine fibonacci_node_append_left(this,new_branch)
    class(fibonacci_node_t),target :: this
    class(fibonacci_node_t),target :: new_branch
    this%left => new_branch
    new_branch%up => this
  end subroutine fibonacci_node_append_left

  subroutine fibonacci_node_append_right (this, new_branch)
    class(fibonacci_node_t), intent(inout), target :: this
    class(fibonacci_node_t), target :: new_branch
    this%right => new_branch
    new_branch%up => this
  end subroutine fibonacci_node_append_right

  subroutine fibonacci_node_replace (this, old_node)
    class(fibonacci_node_t), intent(inout), target :: this
    class(fibonacci_node_t), target :: old_node
    if (associated (old_node%up)) then
       if (old_node%is_left_child ()) then
          old_node%up%left => this
       else
          if (old_node%is_right_child ()) then
             old_node%up%right => this
          end if
       end if
       this%up => old_node%up
    else
       nullify (this%up)
    end if
  end subroutine fibonacci_node_replace

  subroutine fibonacci_node_swap_nodes (left, right)
    class(fibonacci_node_t), target, intent(inout) :: left, right
    class(fibonacci_node_t), pointer :: left_left, right_right
    class(measure_class_t), pointer::down
    ! swap branches
    left_left => left%left
    right_right => right%right
    left%left => right%right
    right%right => left_left
    ! repair up components
    right_right%up => left
    left_left%up => right
    ! repair down components
    down => left%down
    left%down => right%down
    right%down => down
  end subroutine fibonacci_node_swap_nodes

!  subroutine fibonacci_node_swap_nodes (this, that)
!    class(fibonacci_node_t),target :: this
!    class(fibonacci_node_t), pointer, intent(in) :: that
!    class(fibonacci_node_t), pointer :: par_i, par_a
!    par_i => this%up
!    par_a => that%up
!    if (associated (par_i%left, this)) then
!       par_i%left => that
!    else
!       par_i%right => that
!    end if
!    if (associated (par_a%left, that)) then
!       par_a%left => this
!    else
!       par_a%right => this
!    end if
!    this%up => par_a
!    that%up => par_i
!  end subroutine fibonacci_node_swap_nodes

  subroutine fibonacci_node_flip_children (this)
    class(fibonacci_node_t), intent(inout) :: this
    class(fibonacci_node_t), pointer :: child
    child => this%left
    this%left => this%right
    this%right => child
  end subroutine fibonacci_node_flip_children

  subroutine fibonacci_node_rip (this)
    class(fibonacci_node_t), intent(inout), target :: this
    if (this%is_left_child ()) then
       nullify (this%up%left)
    end if
    if (this%is_right_child ()) then
       nullify (this%up%right)
    end if
    nullify (this%up)
  end subroutine fibonacci_node_rip

  subroutine fibonacci_node_remove_and_keep_parent (this, pa)
    class(fibonacci_node_t), intent(inout), target :: this
    class(fibonacci_node_t), intent(out), pointer :: pa
    class(fibonacci_node_t), pointer :: twin
    if (.not. (this%is_root ())) then
       pa => this%up
       if (this%is_left_child ()) then
          twin => pa%right
       else
          twin => pa%left
       end if
       twin%up => pa%up
       if (associated (twin%left)) then
          twin%left%up => pa
       end if
       if (associated (twin%right)) then
          twin%right%up => pa
       end if
       call pa%copy_node (twin)
       select type (pa)
       class is (fibonacci_root_t)
          call pa%set_leftmost ()
          call pa%set_rightmost ()
       end select
       if (associated (this%right)) then
          this%right%left => this%left
       end if
       if (associated (this%left)) then
          this%left%right => this%right
       end if
       nullify (this%left)
       nullify (this%right)
       nullify (this%up)
       deallocate (twin)
    else
       pa => this
    end if
  end subroutine fibonacci_node_remove_and_keep_parent

  subroutine fibonacci_node_remove_and_keep_twin (this, twin)
    class(fibonacci_node_t), intent(inout), target :: this
    class(fibonacci_node_t), intent(out), pointer :: twin
    class(fibonacci_node_t), pointer :: pa
    if (.not. (this%is_root ())) then
       pa => this%up
       if (.not. pa%is_root ()) then
          if (this%is_left_child ()) then
             twin => pa%right
          else
             twin => pa%left
          end if
          if (pa%is_left_child ()) then
             pa%up%left => twin
          else
             pa%up%right => twin
          end if
       end if
       twin%up => pa%up
       if (associated (this%right)) then
          this%right%left => this%left
       end if
       if (associated (this%left)) then
          this%left%right => this%right
       end if
       nullify (this%left)
       nullify (this%right)
       nullify (this%up)
       deallocate (pa)
    end if
  end subroutine fibonacci_node_remove_and_keep_twin

  subroutine fibonacci_node_rotate_left (this)
    class(fibonacci_node_t), intent(inout), target :: this
    call this%swap (this%right)
    call this%right%flip ()
    call this%right%update_depth_unsave ()
    call this%flip ()
    ! value = this%value
    ! this%value = this%left%value
    ! this%left%value = value
  end subroutine fibonacci_node_rotate_left

  subroutine fibonacci_node_rotate_right (this)
    class(fibonacci_node_t), intent(inout), target :: this
    call this%left%swap (this)
    call this%left%flip ()
    call this%left%update_depth_unsave ()
    call this%flip ()
    ! value = this%value
    ! this%value = this%right%value
    ! this%right%value = value
  end subroutine fibonacci_node_rotate_right

  subroutine fibonacci_node_rotate (this)
    class(fibonacci_node_t), intent(inout), target :: this
    if (this%is_left_short ()) then
       call this%rotate_left ()
    else
       if (this%is_right_short ()) then
          call this%rotate_right ()
       end if
    end if
  end subroutine fibonacci_node_rotate

  subroutine fibonacci_node_balance_node (this, changed)
    class(fibonacci_node_t), intent(inout), target :: this
    logical, intent(out) :: changed
    changed = .false.
    if (this%is_left_too_short ()) then
       if (this%right%is_right_short ()) then
          call this%right%rotate_right
       end if
       call this%rotate_left ()
       changed = .true.
    else
       if (this%is_right_too_short ()) then
          if (this%left%is_left_short ()) then
             call this%left%rotate_left
          end if
          call this%rotate_right ()
          changed = .true.
       end if
    end if
  end subroutine fibonacci_node_balance_node

  subroutine fibonacci_node_update_depth_save (this, updated)
    class(fibonacci_node_t), intent(inout) :: this
    logical, intent(out) :: updated
    integer :: left, right, new_depth
    if (associated (this%left)) then
       left = this%left%depth + 1
    else
       left = -1
    end if
    if (associated (this%right)) then
       right = this%right%depth + 1
    else
       right = -1
    end if
    new_depth = max(left, right)
    if (this%depth == new_depth) then
       updated = .false.
    else
       this%depth = new_depth
       updated = .true.
    end if
  end subroutine fibonacci_node_update_depth_save

  subroutine fibonacci_node_update_depth_unsave (this)
    class(fibonacci_node_t), intent(inout) :: this
    this%depth = max (this%left%depth+1, this%right%depth+1)
  end subroutine fibonacci_node_update_depth_unsave

  subroutine fibonacci_node_repair (this)
    class(fibonacci_node_t), intent(inout), target :: this
    class(fibonacci_node_t), pointer:: node
    logical :: new_depth, new_balance
    new_depth = .true.
    node => this
    do while ((new_depth .or. new_balance) .and. (associated (node)))
       call node%balance_node (new_balance)
       call node%update_depth_save (new_depth)
       node => node%up
    end do
  end subroutine fibonacci_node_repair

  elemental logical function fibonacci_node_is_left_short(this)
    class(fibonacci_node_t), intent(in) :: this
    fibonacci_node_is_left_short = (this%left%depth < this%right%depth)
  end function fibonacci_node_is_left_short

  elemental logical function fibonacci_node_is_right_short (this)
    class(fibonacci_node_t), intent(in) :: this
    fibonacci_node_is_right_short = (this%right%depth < this%left%depth)
  end function fibonacci_node_is_right_short

  elemental logical function fibonacci_node_is_unbalanced (this)
    class(fibonacci_node_t), intent(in) :: this
    fibonacci_node_is_unbalanced = &
         (this%is_left_short () .or. this%is_right_short ())
  end function fibonacci_node_is_unbalanced

  elemental logical function fibonacci_node_is_left_too_short (this)
    class(fibonacci_node_t), intent(in) :: this
    fibonacci_node_is_left_too_short = (this%left%depth+1 < this%right%depth)
  end function fibonacci_node_is_left_too_short

  elemental logical function fibonacci_node_is_right_too_short (this)
    class(fibonacci_node_t), intent(in) :: this
    fibonacci_node_is_right_too_short = (this%right%depth+1 < this%left%depth)
  end function fibonacci_node_is_right_too_short

  elemental logical function fibonacci_node_is_too_unbalanced (this)
    class(fibonacci_node_t), intent(in) :: this
    fibonacci_node_is_too_unbalanced = &
         (this%is_left_too_short() .or. this%is_right_too_short())
  end function fibonacci_node_is_too_unbalanced

  elemental logical function fibonacci_node_is_left_child (this)
    class(fibonacci_node_t), intent(in),target :: this
    fibonacci_node_is_left_child = associated (this%up%left, this)
  end function fibonacci_node_is_left_child

  elemental logical function fibonacci_node_is_right_child (this)
    class(fibonacci_node_t), intent(in),target :: this
    fibonacci_node_is_right_child = associated (this%up%right, this)
  end function fibonacci_node_is_right_child

  subroutine fibonacci_leave_print_to_unit &
       (this, unit, parents, components, peers)
    class(fibonacci_leave_t), intent(in) :: this
    integer, intent(in) :: unit
    integer(dik), intent(in) :: parents, components, peers
    class(ser_class_t), pointer :: ser
    if (parents > 0)  call fibonacci_node_print_to_unit &
         (this, unit, parents-i_one, components, -i_one)
    write(unit, "(A)")  "Components of fibonacci_leave_t:"
    ser => this%down
    call serialize_print_comp_pointer &
         (ser, unit, parents, components, peers, "Content:")
  end subroutine fibonacci_leave_print_to_unit

  pure subroutine fibonacci_leave_get_type (type)
    character(:), allocatable, intent(out) :: type
    allocate (type, source="fibonacci_leave_t")
  end subroutine fibonacci_leave_get_type

  subroutine fibonacci_leave_deallocate_all (this)
    class(fibonacci_leave_t), intent(inout) :: this
    if (associated (this%down)) then
       deallocate (this%down)
    end if
  end subroutine fibonacci_leave_deallocate_all

  subroutine fibonacci_leave_pick (this)
    class(fibonacci_leave_t), target, intent(inout) :: this
    class(fibonacci_node_t), pointer :: other
    class(fibonacci_root_t), pointer :: root
    ! call this%up%print_parents()
    call this%find_root (root)
    if (associated (this%up, root)) then
       if (this%up%depth < 2) then
          call msg_error ("fibonacci_leave_pick: Cannot pick leave. " // &
               "Tree must have at least three leaves.")
       else
          call this%remove_and_keep_parent (other)
          call other%repair ()
       end if
    else
       call this%remove_and_keep_twin (other)
       call other%up%repair ()
    end if
    if (associated (root%leftmost, this))  call root%set_leftmost ()
    if (associated (root%rightmost, this))  call root%set_rightmost ()
  end subroutine fibonacci_leave_pick

  subroutine fibonacci_leave_get_left (this, leave)
    class(fibonacci_leave_t), intent(in) :: this
    class(fibonacci_leave_t), intent(out), pointer :: leave
    class(fibonacci_node_t), pointer :: node
    node => this%left
    select type (node)
    class is (fibonacci_leave_t)
       leave => node
    end select
  end subroutine fibonacci_leave_get_left

  subroutine fibonacci_leave_get_right (this, leave)
    class(fibonacci_leave_t), intent(in) :: this
    class(fibonacci_leave_t), intent(out), pointer :: leave
    class(fibonacci_node_t), pointer :: node
    ! print *,"fibonacci_leave_get_right"
    ! call this%down%print_little
    if (associated (this%right)) then
       node => this%right
       ! call node%down%print_little
       select type (node)
       class is (fibonacci_leave_t)
          leave => node
       end select
    else
       ! print *,"no right leave"
       nullify (leave)
    end if
  end subroutine fibonacci_leave_get_right

  subroutine fibonacci_leave_write_pstricks (this, unitnr)
    class(fibonacci_leave_t), intent(in), target :: this
    integer, intent(in) :: unitnr
    write (unitnr, "(A,I3,A,F9.3,A)")  &
         "\begin{psTree}{\Toval[linecolor=green]{\node{", this%depth, "}{", &
         this%measure(), "}}}"
    if (associated (this%left)) then
       write (unitnr, "(A,A,A)")  "\Tr[", le_kid, "]{}"
    end if
    if (associated (this%right)) then
       write (unitnr, "(A,A,A)")  "\Tr[", le_kid, "]{}"
    end if
    write (unitnr, "(A)")  "\end{psTree}"
  end subroutine fibonacci_leave_write_pstricks

  subroutine fibonacci_leave_copy_content (this, content)
    class(fibonacci_leave_t) :: this
    class(measure_class_t), intent(in) :: content
    allocate (this%down, source=content)
  end subroutine fibonacci_leave_copy_content

  subroutine fibonacci_leave_set_content (this, content)
    class(fibonacci_leave_t) :: this
    class(measure_class_t), target, intent(in) :: content
    this%down => content
  end subroutine fibonacci_leave_set_content

  subroutine fibonacci_leave_get_content (this, content)
    class(fibonacci_leave_t), intent(in) :: this
    class(measure_class_t), pointer :: content
    content => this%down
  end subroutine fibonacci_leave_get_content

  elemental logical function fibonacci_leave_is_inner ()
    fibonacci_leave_is_inner = .false.
  end function fibonacci_leave_is_inner

  elemental logical function fibonacci_leave_is_leave ()
    fibonacci_leave_is_leave = .true.
  end function fibonacci_leave_is_leave

  subroutine fibonacci_leave_insert_leave_by_node (this, new_leave)
    class(fibonacci_leave_t), target, intent(inout) :: this,new_leave
    class(fibonacci_node_t), pointer :: parent, new_node
    parent => this%up
    !print *, associated (this%left), associated (this%right)
    if (this < new_leave) then
       call fibonacci_node_spawn (new_node, this, new_leave, this%left, this%right)
       ! print *,"Repair! ",this%measure(),new_leave%measure()
    else
       call fibonacci_node_spawn (new_node, new_leave, this, this%left, this%right)
    end if
    if (associated (parent%left, this)) then
       call parent%append_left (new_node)
    else
       call parent%append_right (new_node)
    end if
    call parent%repair ()
  end subroutine fibonacci_leave_insert_leave_by_node

  elemental logical function fibonacci_leave_is_left_short (this)
    class(fibonacci_leave_t), intent(in) :: this
    fibonacci_leave_is_left_short = .false.
  end function fibonacci_leave_is_left_short

  elemental logical function fibonacci_leave_is_right_short (this)
    class(fibonacci_leave_t), intent(in) :: this
    fibonacci_leave_is_right_short = .false.
  end function fibonacci_leave_is_right_short

  elemental logical function fibonacci_leave_is_unbalanced (this)
    class(fibonacci_leave_t), intent(in) :: this
    fibonacci_leave_is_unbalanced = .false.
  end function fibonacci_leave_is_unbalanced

  elemental logical function fibonacci_leave_is_left_too_short (this)
    class(fibonacci_leave_t), intent(in) :: this
    fibonacci_leave_is_left_too_short = .false.
  end function fibonacci_leave_is_left_too_short

  elemental logical function fibonacci_leave_is_right_too_short (this)
    class(fibonacci_leave_t), intent(in) :: this
    fibonacci_leave_is_right_too_short = .false.
  end function fibonacci_leave_is_right_too_short

  elemental logical function fibonacci_leave_is_too_unbalanced (this)
    class(fibonacci_leave_t), intent(in) :: this
    fibonacci_leave_is_too_unbalanced = .false.
  end function fibonacci_leave_is_too_unbalanced

  subroutine fibonacci_root_write_to_marker (this, marker, status)
    class(fibonacci_root_t), intent(in) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    ! call marker%mark_begin ("fibonacci_root_t")
    call fibonacci_node_write_to_marker (this, marker, status)
    ! marker%mark_end ("fibonacci_root_t")
  end subroutine fibonacci_root_write_to_marker

  subroutine fibonacci_root_read_target_from_marker (this, marker, status)
    class(fibonacci_root_t), target, intent(out) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
    ! call marker%pick_begin ("fibonacci_root_t", status)
    call fibonacci_node_read_from_marker (this, marker, status)
    call this%find_leftmost (this%leftmost)
    call this%find_rightmost (this%rightmost)
    ! call marker%pick_end ("fibonacci_root_t", status)
  end subroutine fibonacci_root_read_target_from_marker

  subroutine fibonacci_root_print_to_unit (this, unit, parents, components, peers)
    class(fibonacci_root_t), intent(in) :: this
    integer, intent(in) :: unit
    integer(dik), intent(in) :: parents, components, peers
    class(ser_class_t), pointer :: ser
    if (parents > 0)  call fibonacci_node_print_to_unit &
         (this, unit, parents-1, components, peers)
    write (unit, "(A)")  "Components of fibonacci_root_t:"
    ser => this%leftmost
    call serialize_print_peer_pointer &
         (ser, unit, parents, components, min(peers, i_one), "Leftmost: ")
    ser => this%rightmost
    call serialize_print_peer_pointer &
         (ser, unit, parents, components, min(peers, i_one), "Rightmost:")
  end subroutine fibonacci_root_print_to_unit

  elemental logical function fibonacci_root_is_left_child (this)
    class(fibonacci_root_t),target, intent(in) :: this
    fibonacci_root_is_left_child = .false.
  end function fibonacci_root_is_left_child

  elemental logical function fibonacci_root_is_right_child (this)
    class(fibonacci_root_t),target, intent(in) :: this
    fibonacci_root_is_right_child = .false.
  end function fibonacci_root_is_right_child

  pure subroutine fibonacci_root_get_type (type)
    character(:),allocatable, intent(out) :: type
    allocate (type, source="fibonacci_root_t")
  end subroutine fibonacci_root_get_type

  subroutine fibonacci_root_get_leftmost (this, leftmost)
    class(fibonacci_root_t), intent(in) :: this
    class(fibonacci_leave_t), pointer :: leftmost
    leftmost => this%leftmost
  end subroutine fibonacci_root_get_leftmost

  subroutine fibonacci_root_get_rightmost (this, rightmost)
    class(fibonacci_root_t), intent(in) :: this
    class(fibonacci_leave_t),pointer :: rightmost
    rightmost => this%rightmost
  end subroutine fibonacci_root_get_rightmost

  elemental function fibonacci_root_is_root ()
    logical::fibonacci_root_is_root
    fibonacci_root_is_root = .true.
  end function fibonacci_root_is_root

  elemental function fibonacci_root_is_inner ()
    logical::fibonacci_root_is_inner
    fibonacci_root_is_inner = .false.
  end function fibonacci_root_is_inner

  elemental function fibonacci_root_is_valid (this)
    class(fibonacci_root_t), intent(in) :: this
    logical :: fibonacci_root_is_valid
    fibonacci_root_is_valid = this%is_valid_c
  end function fibonacci_root_is_valid

  subroutine fibonacci_root_count_leaves (this, n)
    class(fibonacci_root_t), intent(in) :: this
    integer, intent(out) :: n
    n = 0
    call fibonacci_node_count_leaves (this, n)
  end subroutine fibonacci_root_count_leaves

  subroutine fibonacci_root_write_pstricks (this, unitnr)
    class(fibonacci_root_t), intent(in), target :: this
    integer, intent(in) :: unitnr
    logical :: is_opened
    character :: is_sequential, is_formatted, is_writeable
    print *,"pstricks"
    inquire (unitnr, opened=is_opened, sequential=is_sequential, &
         formatted=is_formatted, write=is_writeable)
    if (is_opened) then
       if (is_sequential == "Y" .and. is_formatted == "Y " &
            .and. is_writeable == "Y") then
          ! write (unitnr, "(A,I3,A)")  &
          !     "\begin{psTree}{\Toval[linecolor=blue]{$", int(this%depth), "$}}"
          write (unitnr, "(A,I3,A,F9.3,A)")  &
               "\begin{psTree}{\Toval[linecolor=blue]{\node{", this%depth, &
               "}{", this%measure(), "}}}"
          if (associated (this%leftmost)) then
             call this%leftmost%write_pstricks (unitnr)
          else
             write (unitnr, "(A,A,A)") "\Tr[", no_kid, "]{}"
          end if
          if (associated (this%left)) then
             call this%left%write_pstricks (unitnr)
          else
             write (unitnr, "(A,A,A)") "\Tr[", no_kid, "]{}"
          end if
          if (associated (this%right)) then
             call this%right%write_pstricks (unitnr)
          else
             write (unitnr, "(A,A,A)") "\Tr[", no_kid, "]{}"
          end if
          if (associated (this%rightmost)) then
             call this%rightmost%write_pstricks (unitnr)
          else
             write(unitnr,'("\Tr[",a,"]{}")') no_kid
          end if
          write (unitnr, "(A)")  "\end{psTree}"
          write (unitnr, "(A)")  "\\"
       else
          write (*, "(A,I2,A)") &
             "fibonacci_node_write_pstricks: Unit ", unitnr, &
               " is not opened properly."
          write (*, "(A)")  "No output is written to unit."
       end if
    else
       write (*, "(A,I2,A)") &
          "fibonacci_node_write_pstricks: Unit ", unitnr, &
            " is not opened."
       write (*, "(A)")  "No output is written to unit."
    end if
  end subroutine fibonacci_root_write_pstricks

  subroutine fibonacci_root_copy_root (this, primitive)
    class(fibonacci_root_t), intent(out) :: this
    class(fibonacci_root_t), intent(in) :: primitive
    call fibonacci_node_copy_node (this, primitive)
    this%leftmost => primitive%leftmost
    this%rightmost => primitive%rightmost
  end subroutine fibonacci_root_copy_root

  subroutine fibonacci_root_push_by_content (this, content)
    class(fibonacci_root_t), target, intent(inout) :: this
    class(measure_class_t), target, intent(in) :: content
    class(fibonacci_leave_t), pointer :: node
    ! print *,"fibonacci_root_push_by_content: ",content%measure()
    allocate (node)
    node%down => content
    call this%push_by_leave (node)
  end subroutine fibonacci_root_push_by_content

  subroutine fibonacci_root_push_by_leave (this, new_leave)
    class(fibonacci_root_t), target, intent(inout) :: this
    class(fibonacci_leave_t), pointer, intent(inout) :: new_leave
    class(fibonacci_leave_t), pointer :: old_leave
    class(fibonacci_node_t), pointer :: node, new_node, leave_c
    ! write (11, fmt=*)  "push by leave(", new_leave%measure(), ")\\" !PSTRICKS
    ! flush(11)  !PSTRICKS
    if (new_leave <= this%leftmost) then
       old_leave => this%leftmost
       this%leftmost => new_leave
       node => old_leave%up
       call fibonacci_node_spawn &
            (new_node, new_leave, old_leave, old_leave%left, old_leave%right)
       call node%append_left (new_node)
    else
       if (new_leave > this%rightmost) then
          old_leave => this%rightmost
          this%rightmost => new_leave
          node => old_leave%up
          call fibonacci_node_spawn &
               (new_node, old_leave, new_leave, old_leave%left, old_leave%right)
          call node%append_right (new_node)
       else
          node => this
          do
             if (new_leave <= node) then
                leave_c => node%left
                select type (leave_c)
                class is (fibonacci_leave_t)
                   if (new_leave <= leave_c) then
                      ! print *,"left left"
                      call fibonacci_node_spawn (new_node, new_leave, &
                           leave_c, leave_c%left, leave_c%right)
                   else
                      ! print *,"left right"
                      call fibonacci_node_spawn (new_node, leave_c, &
                           new_leave, leave_c%left, leave_c%right)
                   end if
                   call node%append_left (new_node)
                   exit
                class default
                   ! print *,"left"
                   node => node%left
                end select
             else
                leave_c => node%right
                select type (leave_c)
                class is (fibonacci_leave_t)
                   if (new_leave <= leave_c) then
                      ! print *,"right left"
                      call fibonacci_node_spawn (new_node, new_leave, &
                           leave_c, leave_c%left, leave_c%right)
                   else
                      ! print *,"right right"
                      call fibonacci_node_spawn (new_node, leave_c, &
                           new_leave, leave_c%left, leave_c%right)
                   end if
                   call node%append_right (new_node)
                   exit
                class default
                   ! print *,"right"
                   node => node%right
                end select
             end if
          end do
       end if
    end if
    ! call this%write_pstricks(11) ! PSTRICKS
    ! flush(11) ! PSTRICKS
    ! write(11,fmt=*)"repair\\" ! PSTRICKS
    call node%repair ()
    ! call this%write_pstricks (11) !PSTRICKS
    ! flush(11) !PSTRICKS
    ! call node%update_value (right_value)
    ! call this%write_pstricks (11)
    ! print *, new_node%value, new_node%left%value, new_node%right%value
  end subroutine fibonacci_root_push_by_leave

  subroutine fibonacci_root_pop_left (this, leave)
    class(fibonacci_root_t), intent(inout), target :: this
    class(fibonacci_leave_t), pointer, intent(out) :: leave
    class(fibonacci_node_t), pointer :: parent, grand
    ! write (11,fmt=*) "fibonacci root pop left\\"  ! PSTRICKS
    ! flush (11)   ! PSTRICKS
    leave => this%leftmost
    if (this%left%depth >= 1) then
       parent => leave%up
       grand => parent%up
       grand%left => parent%right
       parent%right%up => grand
       deallocate (parent)
       parent => grand%left
       if (.not. parent%is_leave ()) then
          parent => parent%left
       end if
       select type (parent)
       class is (fibonacci_leave_t)
          this%leftmost => parent
       class default
          call parent%print_all()
          call msg_fatal ("fibonacci_root_pop_left: ERROR: leftmost is no leave.")
       end select
       ! call this%write_pstricks (11)   ! PSTRICKS
       ! flush (11)   ! PSTRICKS
       ! write (11,fmt=*)  "fibonacci node repair\\"   ! PSTRICKS
       ! flush (11)   ! PSTRICKS
       call grand%repair ()
    else
       if (this%left%depth == 0 .and. this%right%depth == 1) then
          parent => this%right
          parent%right%up => this
          parent%left%up => this
          this%left => parent%left
          this%right => parent%right
          this%depth = 1
          deallocate (parent)
          parent => this%left
          select type (parent)
          class is (fibonacci_leave_t)
          this%leftmost => parent
          end select
          this%down => this%leftmost%down
       end if
    end if
    nullify (leave%right%left)
    nullify (leave%up)
    nullify (leave%right)
    nullify (this%leftmost%left)
    ! call this%write_pstricks (11)   ! PSTRICKS
    ! flush (11)   ! PSTRICKS
  end subroutine fibonacci_root_pop_left

  subroutine fibonacci_root_pop_right (this, leave)
    class(fibonacci_root_t), intent(inout), target :: this
    class(fibonacci_leave_t), pointer, intent(out) :: leave
    class(fibonacci_node_t), pointer :: parent, grand
    leave => this%rightmost
    if (this%right%depth >= 1) then
       parent => leave%up
       grand => parent%up
       grand%right => parent%left
       parent%left%up => grand
       deallocate (parent)
       parent => grand%right
       if (.not. parent%is_leave ()) then
          parent => parent%right
       end if
       select type (parent)
       class is (fibonacci_leave_t)
          this%rightmost => parent
       class default
          call parent%print_all()
          call msg_fatal ("fibonacci_root_pop_left: ERROR: leftmost is no leave.")
       end select
       call grand%repair ()
    else
       if (this%right%depth == 0 .and. this%left%depth == 1) then
          parent => this%left
          parent%left%up => this
          parent%right%up => this
          this%right => parent%right
          this%left => parent%left
          this%depth = 1
          deallocate (parent)
          parent => this%right
          select type (parent)
          class is (fibonacci_leave_t)
          this%rightmost => parent
          end select
          this%down => this%rightmost%down
       end if
    end if
  end subroutine fibonacci_root_pop_right

  subroutine fibonacci_root_list_to_tree (this, n_leaves, leave_list_target)
    class(fibonacci_root_t), target, intent(inout) :: this
    integer, intent(in) :: n_leaves
    type(fibonacci_leave_list_t), target, intent(in) :: leave_list_target
    ! class(fibonacci_root_t), pointer, intent(out) :: tree
    integer :: depth, n_deep, n_merge
    class(fibonacci_node_t), pointer :: node
    class(fibonacci_leave_list_t), pointer :: leave_list
    class(fibonacci_leave_t), pointer :: content
    real(default) :: up_value
    leave_list => leave_list_target
    call ilog2 (n_leaves, depth, n_deep)
    n_deep = n_deep * 2
    n_merge = 0
    this%depth = depth
    node => this
    OUTER: do
       do while (depth > 1)
          depth = depth - 1
          allocate (node%left)
          node%left%up => node
          node => node%left
          node%depth = depth
       end do
       node%left => leave_list%leave
       node%down => leave_list%leave%down
       leave_list => leave_list%next
       node%right => leave_list%leave
       content => leave_list%leave
       leave_list => leave_list%next
       n_merge = n_merge + 2
       INNER: do
          if (associated (node%up)) then
             if (node%is_left_child ()) then
                if (n_merge == n_deep .and. depth == 1) then
                   node => node%up
                   node%right => leave_list%leave
                   node%right%up => node
                   node%down => content%down
                   content => leave_list%leave
                   leave_list => leave_list%next
                   n_merge = n_merge + 1
                   cycle
                end if
                exit INNER
             else
                node => node%up
                depth = depth + 1
             end if
          else
             exit OUTER
          end if
       end do INNER
       node => node%up
       node%down => content%down
       allocate (node%right)
       node%right%up => node
       node => node%right
       if (n_deep == n_merge) then
          depth = depth - 1
       end if
       node%depth = depth
    end do OUTER
    call this%set_leftmost
    call this%set_rightmost
  end subroutine fibonacci_root_list_to_tree

  subroutine fibonacci_root_merge(this_tree,that_tree,merge_tree)
    class(fibonacci_root_t), intent(in) :: this_tree
    class(fibonacci_root_t), intent(in) :: that_tree
    class(fibonacci_root_t), pointer, intent(out) :: merge_tree
    class(fibonacci_leave_t), pointer :: this_leave, that_leave, old_leave
    type(fibonacci_leave_list_t), target :: leave_list
    class(fibonacci_leave_list_t), pointer :: last_leave
    integer :: n_leaves
    if (associated (this_tree%leftmost) .and. associated (that_tree%leftmost)) then
       n_leaves = 1
       this_leave => this_tree%leftmost
       that_leave => that_tree%leftmost
       if (this_leave < that_leave) then
          allocate (leave_list%leave, source=this_leave)
          call this_leave%find_right_leave (this_leave)
       else
          allocate (leave_list%leave, source=that_leave)
          call that_leave%find_right_leave (that_leave)
       end if
       last_leave => leave_list
       do while (associated (this_leave) .and. associated (that_leave))
          if (this_leave < that_leave) then
             old_leave => this_leave
             call this_leave%find_right_leave (this_leave)
          else
             old_leave=>that_leave
             call that_leave%find_right_leave (that_leave)
          end if
          allocate (last_leave%next)
          last_leave => last_leave%next
          allocate (last_leave%leave, source=old_leave)
          n_leaves = n_leaves + 1
       end do
       if (associated (this_leave)) then
          old_leave => this_leave
       else
          old_leave => that_leave
       end if
       do while (associated (old_leave))
          allocate (last_leave%next)
          last_leave => last_leave%next
          allocate (last_leave%leave, source=old_leave)
          n_leaves = n_leaves + 1
          call old_leave%find_right_leave (old_leave)
       end do
       allocate (merge_tree)
       call merge_tree%list_to_tree (n_leaves, leave_list)
    else
       n_leaves = 0
    end if
    if (associated (leave_list%next)) then
       last_leave => leave_list%next
       do while (associated (last_leave%next))
          leave_list%next => last_leave%next
          deallocate (last_leave)
          last_leave => leave_list%next
       end do
       deallocate (last_leave)
    end if
  end subroutine fibonacci_root_merge

  subroutine fibonacci_root_set_leftmost (this)
    class(fibonacci_root_t) :: this
    call this%find_leftmost (this%leftmost)
  end subroutine fibonacci_root_set_leftmost

  subroutine fibonacci_root_set_rightmost (this)
    class(fibonacci_root_t) :: this
    call this%find_rightmost (this%rightmost)
  end subroutine fibonacci_root_set_rightmost

  subroutine fibonacci_root_init_by_leave (this, left_leave, right_leave)
    class(fibonacci_root_t), target, intent(out) :: this
    class(fibonacci_leave_t), target, intent(in) :: left_leave, right_leave
    if (left_leave <= right_leave) then
       this%left => left_leave
       this%right => right_leave
       this%leftmost => left_leave
       this%rightmost => right_leave
    else
       this%left => right_leave
       this%right => left_leave
       this%leftmost => right_leave
       this%rightmost => left_leave
    end if
    this%left%up => this
    this%right%up => this
    this%down => this%leftmost%down
    this%depth = 1
    this%leftmost%right => this%rightmost
    this%rightmost%left => this%leftmost
    this%is_valid_c = .true.
  end subroutine fibonacci_root_init_by_leave

  subroutine fibonacci_root_init_by_content (this, left_content, right_content)
    class(fibonacci_root_t), target, intent(out) :: this
    class(measure_class_t), intent(in), target :: left_content, right_content
    call this%reset ()
    print *, "fibonacci_root_init_by_content: ", left_content%measure (), &
         right_content%measure ()
    if (left_content < right_content) then
       call this%leftmost%set_content (left_content)
       call this%rightmost%set_content (right_content)
    else
       call this%leftmost%set_content (right_content)
       call this%rightmost%set_content (left_content)
    end if
    this%down => this%leftmost%down
    this%is_valid_c = .true.
  end subroutine fibonacci_root_init_by_content

  subroutine fibonacci_root_reset (this)
    class(fibonacci_root_t), target, intent(inout) :: this
    call this%deallocate_tree ()
    allocate (this%leftmost)
    allocate (this%rightmost)
    this%depth = 1
    this%leftmost%depth = 0
    this%rightmost%depth = 0
    this%left => this%leftmost
    this%right => this%rightmost
    this%left%up => this
    this%right%up => this
    this%leftmost%right => this%rightmost
    this%rightmost%left => this%leftmost
  end subroutine fibonacci_root_reset

  recursive subroutine fibonacci_root_deallocate_tree (this)
    class(fibonacci_root_t), intent(inout) :: this
    call this%deallocate_tree ()
    nullify (this%leftmost)
    nullify (this%rightmost)
  end subroutine fibonacci_root_deallocate_tree

  recursive subroutine fibonacci_root_deallocate_all (this)
    class(fibonacci_root_t), intent(inout) :: this
    call this%deallocate_all ()
    nullify (this%leftmost)
    nullify (this%rightmost)
  end subroutine fibonacci_root_deallocate_all

  subroutine fibonacci_stub_write_to_marker (this, marker, status)
    class(fibonacci_stub_t), intent(in) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
  end subroutine fibonacci_stub_write_to_marker

  subroutine fibonacci_stub_read_target_from_marker (this, marker, status)
    class(fibonacci_stub_t), target, intent(out) :: this
    class(marker_t), intent(inout) :: marker
    integer(dik), intent(out) :: status
  end subroutine fibonacci_stub_read_target_from_marker

  pure subroutine fibonacci_stub_get_type (type)
    character(:), allocatable, intent(out) :: type
    allocate (type, source="fibonacci_stub_t")
  end subroutine fibonacci_stub_get_type

  subroutine fibonacci_stub_push_by_content (this, content)
    class(fibonacci_stub_t), target, intent(inout) :: this
    class(measure_class_t), target, intent(in) :: content
    class(fibonacci_leave_t), pointer :: leave
    allocate (leave)
    call leave%set_content (content)
    call this%push_by_leave (leave)
  end subroutine fibonacci_stub_push_by_content

  subroutine fibonacci_stub_push_by_leave (this, new_leave)
    class(fibonacci_stub_t), target, intent(inout) :: this
    class(fibonacci_leave_t), pointer, intent(inout) :: new_leave
    class(fibonacci_leave_t), pointer :: old_leave
    if (this%depth < 1) then
       if (associated (this%leftmost)) then
          old_leave => this%leftmost
          call this%init_by_leave (old_leave, new_leave)
       else
          this%leftmost => new_leave
       end if
    else
       call fibonacci_root_push_by_leave (this, new_leave)
    end if
  end subroutine fibonacci_stub_push_by_leave

  subroutine fibonacci_stub_pop_left (this, leave)
    class(fibonacci_stub_t), intent(inout), target :: this
    class(fibonacci_leave_t), pointer, intent(out) :: leave
    if (this%depth < 2) then
       if (this%depth == 1) then
          leave => this%leftmost
          this%leftmost => this%rightmost
          nullify (this%rightmost)
          nullify (this%right)
          nullify (this%left)
          this%depth = 0
          this%is_valid_c = .false.
       else
          if (associated (this%leftmost)) then
             leave => this%leftmost
             nullify (this%leftmost)
          end if
       end if
    else
       call fibonacci_root_pop_left (this, leave)
    end if
  end subroutine fibonacci_stub_pop_left

  subroutine fibonacci_stub_pop_right (this, leave)
    class(fibonacci_stub_t), intent(inout), target :: this
    class(fibonacci_leave_t), pointer, intent(out) :: leave
    if (this%depth < 2) then
       if (this%depth == 1) then
          this%is_valid_c = .false.
          if (associated (this%rightmost)) then
             leave => this%rightmost
             nullify (this%rightmost)
             nullify (this%right)
          else
             if (associated (this%leftmost)) then
                leave => this%leftmost
                nullify (this%leftmost)
                nullify (this%left)
             else
                nullify (leave)
             end if
          end if
       end if
    else
       call fibonacci_root_pop_right (this, leave)
    end if
  end subroutine fibonacci_stub_pop_right

!  subroutine fibonacci_node_update_value (this, right_value)
!    class(fibonacci_node_t), target :: this
!    class(fibonacci_node_t), pointer:: node
!    real(default), intent(in) :: right_value
!    if (associated (this%left) .and. associated (this%right)) then
!       node => this
!       ! node%value = node%left%value
!       ! right_value = node%right%value
!       INNER: do while (associated (node%up))
!          if (node%is_right_child ()) then
!             node => node%up
!          else
!             node%up%value = right_value
!             exit
!          end if
!       end do INNER
!    end if
!  end subroutine fibonacci_node_update_value

!  subroutine fibonacci_root_copy_node (this, primitive)
!    class(fibonacci_root_t), intent(out) :: this
!    type(fibonacci_node_t), intent(in) :: primitive
!    call fibonacci_node_copy_node (this, primitive)
!    call primitive%find_leftmost (this%leftmost)
!    call primitive%find_rightmost (this%rightmost)
!  end subroutine fibonacci_root_copy_node

!  subroutine fibonacci_root_push_by_node (this, new_leave)
!    class(fibonacci_root_t), target, intent(inout) :: this
!    class(fibonacci_leave_t), pointer, intent(inout) :: new_leave
!    class(fibonacci_leave_t), pointer :: old_leave
!    if (new_leave <= this%leftmost) then
!       old_leave => this%leftmost
!       this%leftmost => new_leave
!    else
!       if (new_leave > this%rightmost) then
!          old_leave => this%rightmost
!          this%rightmost => new_leave
!       else
!          call this%find (new_leave%measure(), old_leave)
!       end if
!    end if
!    ! call old_leave%insert_leave_by_node (new_leave)
!    call fibonacci_leave_insert_leave_by_node (old_leave, new_leave)
!    call new_leave%up%repair ()
!    ! call new_leave%up%update_value ()
!  end subroutine fibonacci_root_push_by_node

  subroutine fibonacci_leave_write_content (this, unit)
    class(fibonacci_leave_t), intent(in), target :: this
    integer,optional, intent(in) :: unit
    call this%down%print_all (unit)
  end subroutine fibonacci_leave_write_content

  subroutine fibonacci_leave_write (this, unit)
    class(fibonacci_leave_t), intent(in), target :: this
    integer,optional, intent(in) :: unit
    call this%print_all (unit)
  end subroutine fibonacci_leave_write

  subroutine fibonacci_leave_write_value (this, unit)
    class(fibonacci_leave_t), intent(in), target :: this
    integer, intent(in), optional :: unit
    if (present (unit)) then
       write(unit, fmt=*)  this%measure ()
    else
       print *, this%measure ()
    end if
    ! call this%print_little (unit)
  end subroutine fibonacci_leave_write_value

  subroutine fibonacci_node_spawn (new_node, left_leave, right_leave, &
       left_left_leave, right_right_leave)
    class(fibonacci_node_t), pointer, intent(out) :: new_node
    class(fibonacci_leave_t), target, intent(inout) :: left_leave, right_leave
    class(fibonacci_node_t), pointer, intent(inout) :: left_left_leave, &
         right_right_leave
    allocate (new_node)
    new_node%depth = 1
    if (associated (left_left_leave)) then
       left_left_leave%right => left_leave
       left_leave%left => left_left_leave
    else
       nullify (left_leave%left)
    end if
    if (associated (right_right_leave)) then
       right_right_leave%left => right_leave
       right_leave%right => right_right_leave
    else
       nullify (right_leave%right)
    end if
    new_node%left => left_leave
    new_node%right => right_leave
    new_node%down => left_leave%down
    new_node%depth = 1
    left_leave%up => new_node
    right_leave%up => new_node
    left_leave%right => right_leave
    right_leave%left => left_leave
  end subroutine fibonacci_node_spawn


end module muli_fibonacci_tree

