! WHIZARD 2.2.6 May 02 2015
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

module sindarin_parser

  use iso_varying_string, string_t => varying_string
  use unit_tests
  use format_utils
  use io_units
  use diagnostics

  use ifiles
  use syntax_rules
  use lexers
  use parser
  
  use codes
  use builders

  use object_base
  use object_builder
  use object_expr
  use object_logical
  use object_integer

  implicit none
  private

  public :: syntax_sindarin
  public :: syntax_sindarin_init
  public :: syntax_sindarin_final
  public :: syntax_sindarin_write
  public :: sindarin_parser_test

  type, extends (builder_t), abstract :: pn_decoder_t
     private
     type(parse_node_t), pointer :: pn => null ()
     type(parse_node_t), pointer :: pn_next => null ()
     type(syntax_rule_t), pointer :: rule => null ()
     type(string_t), dimension(:), pointer :: prototypes => null ()
     type(position_t) :: position
     class(pn_decoder_t), pointer :: previous => null ()
   contains
     procedure :: final => pn_decoder_final
     procedure :: write => pn_decoder_write
     procedure :: init => pn_decoder_init
     procedure :: build => pn_decoder_build
     procedure :: get_next_node => pn_decoder_get_next_node
     procedure :: get_part => pn_decoder_get_part
  end type pn_decoder_t
  
  type, extends (builder_t) :: sindarin_decoder_t
     private
     type(string_t), dimension(:), pointer :: prototypes => null ()
     type(parse_node_t), pointer :: pn_root => null ()
     class(pn_decoder_t), pointer :: pn_decoder => null ()
   contains
     procedure :: final => sindarin_decoder_final
     procedure :: write => sindarin_decoder_write
     procedure :: write_prototypes => sindarin_decoder_write_prototypes
     procedure :: write_tree => sindarin_decoder_write_tree
     procedure :: write_stack => sindarin_decoder_write_stack
     procedure :: init => sindarin_decoder_init
     procedure, private :: push => sindarin_decoder_push
     procedure, private :: pop => sindarin_decoder_pop
     procedure :: decode => sindarin_decoder_decode
     procedure :: build => sindarin_decoder_build
     procedure :: make_pn_decoder => sindarin_decoder_make_pn_decoder
  end type sindarin_decoder_t
     
  type, extends (pn_decoder_t) :: pnd_script_t
     private
     integer :: n_declarations = 0
     integer :: n_commands = 0
     integer, dimension(:), allocatable :: dec
     integer, dimension(:), allocatable :: com
   contains
     procedure :: decode => decode_script
  end type pnd_script_t
  
  type, extends (pn_decoder_t) :: pnd_declaration_t
     private
   contains
     procedure :: decode => decode_declaration
  end type pnd_declaration_t
  
  type, extends (pn_decoder_t) :: pnd_assignment_t
     private
     type(string_t) :: var_name
     type(parse_node_t), pointer :: pn_rhs => null ()
   contains
     procedure :: decode => decode_assignment
  end type pnd_assignment_t
  
  type, extends (pn_decoder_t) :: pnd_atom_t
     private
   contains
     procedure :: decode => decode_atom
  end type pnd_atom_t
  
  type, extends (pn_decoder_t) :: pnd_logical_expr_t
     private
     logical :: unary = .false.
   contains
     procedure :: decode => decode_logical_expr
  end type pnd_logical_expr_t
  
  type, extends (pn_decoder_t) :: pnd_logical_literal_t
     private
     logical :: value
   contains
     procedure :: decode => decode_logical_literal
  end type pnd_logical_literal_t
  

  type(syntax_t), target, save :: syntax_sindarin
  

contains

  subroutine make_sindarin_repository (repository)
    type(repository_t), intent(inout) :: repository
    class(object_t), pointer :: core
    class(object_t), pointer :: logical
    class(object_t), pointer :: not, and, or
    class(object_t), pointer :: assignment
    integer :: i
    integer, parameter :: n_members = 5
    call repository%init (name = var_str ("repository"), n_members = n_members)
    ! Elementary data types
    ! logical
    allocate (composite_t :: logical)
    select type (logical)
    type is (composite_t)
       allocate (logical_t :: core)
       call logical%init (var_str ("logical"))
       call logical%import_core (core)
    end select
    ! Logical operators
    ! not
    allocate (not_t :: not)
    select type (not)
    type is (not_t);  call not%init (logical)
    end select
    ! and
    allocate (and_t :: and)
    select type (and)
    type is (and_t);  call and%init (logical)
    end select
    ! or
    allocate (or_t :: or)
    select type (or)
    type is (or_t);  call or%init (logical)
    end select
    ! Statements
    ! Assignment
    allocate (assignment_t :: assignment)
    select type (assignment)
    type is (assignment_t);  call assignment%init ()
    end select
    i = 0
    i = i + 1;  call repository%import_member (i, logical)
    i = i + 1;  call repository%import_member (i, not)
    i = i + 1;  call repository%import_member (i, and)
    i = i + 1;  call repository%import_member (i, or)
    i = i + 1;  call repository%import_member (i, assignment)
    if (i /= n_members) then
       write (msg_buffer, "(A,I0,A,I0,A)")  "Sindarin: prototype repository&
            & has size ", n_members, ", but ", i, " items are given."
       call msg_bug ()
    end if
  end subroutine make_sindarin_repository
    
  subroutine syntax_sindarin_init ()
    type(ifile_t) :: ifile
    call sindarin_syntax_setup (ifile)
    call syntax_init (syntax_sindarin, ifile)
    call ifile%final ()
  end subroutine syntax_sindarin_init

  subroutine syntax_sindarin_final ()
    call syntax_final (syntax_sindarin)
  end subroutine syntax_sindarin_final

  subroutine syntax_sindarin_write (unit)
    integer, intent(in), optional :: unit
    call syntax_write (syntax_sindarin, unit)
  end subroutine syntax_sindarin_write

  subroutine sindarin_syntax_setup (ifile)
    type(ifile_t), intent(inout) :: ifile
    call ifile%append ("SEQ script = command*")
    call ifile%append ("ALT command = declaration | assignment")
    call ifile%append ("SEQ declaration =&
         & builtin_type var_name assignment_clause?")
    call ifile%append ("SEQ assignment = var_name assignment_clause")
    call ifile%append ("ALT builtin_type = logical")
    call ifile%append ("KEY logical")
    call ifile%append ("IDE var_name")
    call ifile%append ("SEQ assignment_clause = '=' expr")
    call ifile%append ("KEY '='")
    call ifile%append ("ALT expr = logical_expr")
    call ifile%append ("SEQ logical_expr = logical_term or_clause*")
    call ifile%append ("SEQ logical_term = logical_value and_clause*")
    call ifile%append ("ALT logical_value =&
         & not_clause | logical_literal | group | atom")
    call ifile%append ("SEQ or_clause = or logical_term")
    call ifile%append ("SEQ and_clause = and logical_value")
    call ifile%append ("SEQ not_clause = not logical_value")
    call ifile%append ("ALT logical_literal = true | false")
    call ifile%append ("KEY or")
    call ifile%append ("KEY and")
    call ifile%append ("KEY not")
    call ifile%append ("KEY true")
    call ifile%append ("KEY false")
    call ifile%append ("GRO group = ( expr )")
    call ifile%append ("IDE atom")
  end subroutine sindarin_syntax_setup

  subroutine pn_decoder_final (builder)
    class(pn_decoder_t), intent(inout) :: builder
  end subroutine pn_decoder_final
  
  subroutine pn_decoder_write (builder, unit)
    class(pn_decoder_t), intent(in) :: builder
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(2x)", advance="no")
    call builder%rule%write (u, short=.true., key_only=.true., advance=.false.)
    write (u, "(':',1x)", advance="no")
    call builder%position%write (u)
    write (u, *)
  end subroutine pn_decoder_write

  subroutine pn_decoder_init (builder, parse_node, rule, prototypes)
    class(pn_decoder_t), intent(inout) :: builder
    type(parse_node_t), intent(in), target :: parse_node
    type(syntax_rule_t), intent(in), target :: rule
    type(string_t), dimension(:), target :: prototypes
    builder%pn => parse_node
    builder%rule => rule
    builder%prototypes => prototypes
  end subroutine pn_decoder_init
  
  subroutine pn_decoder_build (builder, code, success)
    class(pn_decoder_t), intent(inout) :: builder
    type(code_t), intent(in) :: code
    logical, intent(out) :: success
    success = .false.
    call msg_bug ("Parse-node decoder: build method not implemented")
  end subroutine pn_decoder_build
    
  subroutine pn_decoder_get_next_node (builder, parse_node)
    class(pn_decoder_t), intent(in) :: builder
    type(parse_node_t), pointer, intent(out) :: parse_node
    parse_node => builder%pn_next
  end subroutine pn_decoder_get_next_node
  
  function pn_decoder_get_part (builder) result (part)
    class(pn_decoder_t), intent(in) :: builder
    integer :: part
    part = builder%position%part
  end function pn_decoder_get_part
  
  subroutine sindarin_decoder_final (builder)
    class(sindarin_decoder_t), intent(inout) :: builder
    class(pn_decoder_t), pointer :: pn_decoder
    if (associated (builder%prototypes))  deallocate (builder%prototypes)
    do while (associated (builder%pn_decoder))
       pn_decoder => builder%pn_decoder
       builder%pn_decoder => pn_decoder%previous
       call pn_decoder%final ()
       deallocate (pn_decoder)
    end do
  end subroutine sindarin_decoder_final
  
  subroutine sindarin_decoder_write (builder, unit)
    class(sindarin_decoder_t), intent(in) :: builder
    integer, intent(in), optional :: unit
    class(pn_decoder_t), pointer :: pn_decoder
    integer :: u
    u = given_output_unit (unit)
    write (u, "(A)")  "Parse-tree decoder state:"
    call builder%write_prototypes (u)
    call builder%write_tree (u)
    call builder%write_stack (u)
  end subroutine sindarin_decoder_write

  subroutine sindarin_decoder_write_prototypes (builder, unit)
    class(sindarin_decoder_t), intent(in) :: builder
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit)
    if (associated (builder%prototypes)) then
       write (u, "(A)")  "Prototype names:"
       do i = 1, size (builder%prototypes)
          write (u, "(1x,I0,1x,A)")  i, char (builder%prototypes(i))
       end do
    else
       write (u, "(A)")  "[No prototype array]"
    end if
  end subroutine sindarin_decoder_write_prototypes

  subroutine sindarin_decoder_write_tree (builder, unit)
    class(sindarin_decoder_t), intent(in) :: builder
    integer, intent(in), optional :: unit
    class(pn_decoder_t), pointer :: pn_decoder
    integer :: u
    u = given_output_unit (unit)
    if (associated (builder%pn_root)) then
       write (u, "(A)")  "Parse tree:"
       call builder%pn_root%write (unit, short=.true., depth = 1)
    else
       write (u, "(A)")  "[No parse tree]"
    end if
  end subroutine sindarin_decoder_write_tree

  subroutine sindarin_decoder_write_stack (builder, unit)
    class(sindarin_decoder_t), intent(in) :: builder
    integer, intent(in), optional :: unit
    class(pn_decoder_t), pointer :: pn_decoder
    integer :: u
    u = given_output_unit (unit)
    if (associated (builder%pn_decoder)) then
       write (u, "(A)")  "Node handler stack:"
       pn_decoder => builder%pn_decoder
       do while (associated (pn_decoder))
          call pn_decoder%write (u)
          pn_decoder => pn_decoder%previous
       end do
    else
       write (u, "(A)")  "[No pointer stack]"
    end if
  end subroutine sindarin_decoder_write_stack

  subroutine sindarin_decoder_init (decoder, parse_tree, prototype_names)
    class(sindarin_decoder_t), intent(out) :: decoder
    type(parse_tree_t), intent(in) :: parse_tree
    type(string_t), dimension(:), intent(in) :: prototype_names
    class(pn_decoder_t), pointer :: pn_decoder
    integer :: i
    allocate (decoder%prototypes (size (prototype_names)))
    decoder%prototypes = prototype_names
    decoder%pn_root => parse_tree%get_root_ptr ()
    if (associated (decoder%pn_root)) then
       call decoder%push (decoder%pn_root, 0)
    end if
  end subroutine sindarin_decoder_init
  
  subroutine sindarin_decoder_push (builder, parse_node, context_part)
    class(sindarin_decoder_t), intent(inout) :: builder
    type(parse_node_t), intent(in), target :: parse_node
    integer, intent(in) :: context_part
    class(pn_decoder_t), pointer :: pn_decoder
    call builder%make_pn_decoder (parse_node, context_part, pn_decoder)
    pn_decoder%previous => builder%pn_decoder
    builder%pn_decoder => pn_decoder
  end subroutine sindarin_decoder_push
    
  subroutine sindarin_decoder_pop (builder)
    class(sindarin_decoder_t), intent(inout) :: builder
    class(pn_decoder_t), pointer :: pn_decoder
    pn_decoder => builder%pn_decoder
    builder%pn_decoder => pn_decoder%previous
    call pn_decoder%final ()
    deallocate (pn_decoder)
  end subroutine sindarin_decoder_pop
    
  subroutine sindarin_decoder_decode (builder, code, success)
    class(sindarin_decoder_t), intent(inout) :: builder
    type(code_t), intent(out) :: code
    logical, intent(out) :: success
    type(parse_node_t), pointer :: pn_next
    integer :: context_part
    do while (associated (builder%pn_decoder))
       call builder%pn_decoder%decode (code, success)
       if (success) then
          call builder%pn_decoder%get_next_node (pn_next)
          if (associated (pn_next)) then
             context_part = builder%pn_decoder%get_part ()
             call builder%push (pn_next, context_part)
          end if
          if (code%cat /= 0) then
             return
          end if
       else
          call builder%pop ()
       end if
    end do
    success = .false.
  end subroutine sindarin_decoder_decode

  subroutine sindarin_decoder_build (builder, code, success)
    class(sindarin_decoder_t), intent(inout) :: builder
    type(code_t), intent(in) :: code
    logical, intent(out) :: success
    success = .false.
    call msg_bug ("Sindarin decoder: build method not implemented")
  end subroutine sindarin_decoder_build
    
  subroutine sindarin_decoder_make_pn_decoder &
       (builder, parse_node, context_part, pn_decoder)
    class(sindarin_decoder_t), intent(inout) :: builder
    type(parse_node_t), intent(in), target :: parse_node
    integer, intent(in) :: context_part
    class(pn_decoder_t), intent(out), pointer :: pn_decoder
    type(syntax_rule_t), pointer :: rule
    integer :: prototype_index
    rule => parse_node%get_rule_ptr ()
    prototype_index = 0
    select case (char (rule%get_key ()))
       case ("script")
          allocate (pnd_script_t :: pn_decoder)
       case ("declaration")
          select case (context_part)
          case (POS_MEMBER)
             allocate (pnd_declaration_t :: pn_decoder)
          case (POS_PRIMER)
             allocate (pnd_assignment_t :: pn_decoder)
          end select
       case ("assignment")
          allocate (pnd_assignment_t :: pn_decoder)
       case ("atom")
          allocate (pnd_atom_t :: pn_decoder)
       case ("logical_expr", "logical_term")
          allocate (pnd_logical_expr_t :: pn_decoder)
       case ("not_clause")
          allocate (pnd_logical_expr_t :: pn_decoder)
          select type (pn_decoder)
          type is (pnd_logical_expr_t)
             pn_decoder%unary = .true.
          end select
       case ("true")
          allocate (pnd_logical_literal_t :: pn_decoder)
          select type (pn_decoder)
          type is (pnd_logical_literal_t)
             pn_decoder%value = .true.
          end select
       case ("false")
          allocate (pnd_logical_literal_t :: pn_decoder)
          select type (pn_decoder)
          type is (pnd_logical_literal_t)
             pn_decoder%value = .false.
          end select
    case default
       call msg_bug ("Sindarin decoder: rule '" &
            // char (rule%get_key ()) // "' not supported")
    end select
    call pn_decoder%init (parse_node, rule, builder%prototypes)
  end subroutine sindarin_decoder_make_pn_decoder
    
  function find_index (array, name) result (i_prototype)
    type(string_t), dimension(:), intent(in) :: array
    type(string_t), intent(in) :: name
    integer :: i_prototype
    integer :: i
    i_prototype = 0
    do i = 1, size (array)
       if (name == array(i)) then
          i_prototype = i
          return
       end if
    end do
  end function find_index

  recursive subroutine decode_script (builder, code, success)
    class(pnd_script_t), intent(inout) :: builder
    type(code_t), intent(out) :: code
    logical, intent(out) :: success
    type(parse_node_t), pointer :: pn_com, pn_asg
    type(syntax_rule_t), pointer :: rule, rule_asg
    logical, dimension(:), allocatable :: mask_dec, mask_com
    integer :: i, n_sub, n_dec, n_com
    success = .true.
    select case (builder%position%part)
    case (POS_HERE)
       n_sub = builder%pn%get_n_sub ()
       allocate (mask_dec (n_sub))
       allocate (mask_com (n_sub))
       pn_com => builder%pn%get_sub_ptr ()
       do i = 1, n_sub
          rule => pn_com%get_rule_ptr ()
          select case (char (rule%get_key ()))
          case ("declaration")
             mask_dec(i) = .true.
             pn_asg => pn_com%get_sub_ptr (3)
             mask_com(i) = associated (pn_asg)
          case default
             mask_dec(i) = .false.
             mask_com(i) = .true.
          end select
          pn_com => pn_com%get_next_ptr ()
       end do
       n_dec = count (mask_dec)
       n_com = count (mask_com)
       code%cat = CAT_COMPOSITE
       code%natt = 6
       code%att(4) = n_dec
       code%att(6) = n_com
       call code%create_string_val (builder%rule%get_key ())
       builder%n_declarations = n_dec
       builder%n_commands = n_com
       builder%dec = pack ([(i, i=1, n_sub)], mask_dec)
       builder%com = pack ([(i, i=1, n_sub)], mask_com)
       if (n_dec > 0) then
          builder%position%part = POS_MEMBER
          i = 1
          builder%pn_next => builder%pn%get_sub_ptr (builder%dec(1))
       else if (n_com > 0) then
          builder%position%part = POS_PRIMER
          i = 1
          builder%pn_next => builder%pn%get_sub_ptr (builder%com(1))
       else
          builder%position%part = POS_NONE
          i = 0
          builder%pn_next => null ()
       end if
    case (POS_MEMBER)
       i = builder%position%i
       if (i < builder%n_declarations) then
          i = i + 1
          builder%pn_next => builder%pn%get_sub_ptr (builder%dec(i))
       else if (builder%n_commands > 0) then
          builder%position%part = POS_PRIMER
          i = 1
          builder%pn_next => builder%pn%get_sub_ptr (builder%com(1))
       else
          builder%position%part = POS_NONE
          i = 0
          builder%pn_next => null ()
       end if
       builder%position%i = i
    case (POS_PRIMER)
       i = builder%position%i
       if (i < builder%n_commands) then
          i = i + 1
          builder%pn_next => builder%pn%get_sub_ptr (builder%com(i))
       else
          builder%position%part = POS_NONE
          i = 0
          builder%pn_next => null ()
       end if
    case default
       builder%pn_next => null ()
       success = .false.
    end select
    builder%position%i = i
  end subroutine decode_script
  
  recursive subroutine decode_declaration (builder, code, success)
    class(pnd_declaration_t), intent(inout) :: builder
    type(code_t), intent(out) :: code
    logical, intent(out) :: success
    type(parse_node_t), pointer :: pn_prototype, pn_var_name
    type(syntax_rule_t), pointer :: rule
    type(string_t) :: var_name
    success = .true.
    select case (builder%position%part)
    case (POS_HERE)
       pn_prototype => builder%pn%get_sub_ptr ()
       rule => pn_prototype%get_rule_ptr ()
       pn_var_name => pn_prototype%get_next_ptr ()
       var_name = pn_var_name%get_string ()
       code%cat = CAT_COMPOSITE
       code%natt = 2
       code%att(1) = find_index (builder%prototypes, rule%get_key ())
       code%att(2) = MODE_VARIABLE
       call code%create_string_val (var_name)
       builder%position%part = POS_CORE
       builder%pn_next => null ()
    case (POS_CORE)
       code%cat = CAT_VALUE
       builder%position%part = POS_NONE
       builder%pn_next => null ()
    case default
       builder%pn_next => null ()
       success = .false.
    end select
  end subroutine decode_declaration
  
  recursive subroutine decode_assignment (builder, code, success)
    class(pnd_assignment_t), intent(inout) :: builder
    type(code_t), intent(out) :: code
    logical, intent(out) :: success
    type(parse_node_t), pointer :: pn_lhs, pn_asg
    success = .true.
    select case (builder%position%part)
    case (POS_HERE)
       select case (char (builder%rule%get_key ()))
       case ("assignment")
          pn_lhs => builder%pn%get_sub_ptr ()
       case ("declaration")
          pn_lhs => builder%pn%get_sub_ptr (2)
       end select
       pn_asg => pn_lhs%get_next_ptr ()
       builder%pn_rhs => pn_asg%get_sub_ptr (2)
       builder%var_name = pn_lhs%get_string ()
       code%cat = CAT_COMPOSITE
       code%natt = 1
       code%att(1) = find_index (builder%prototypes, var_str ("assignment"))
       builder%position%part = POS_ID
       builder%pn_next => null ()
    case (POS_ID)
       code%cat = CAT_ID
       call code%create_string_val (builder%var_name)
       builder%position%part = POS_MEMBER
       builder%pn_next => null ()
    case (POS_MEMBER)
       builder%position%part = POS_NONE
       builder%pn_next => builder%pn_rhs
    case default
       builder%pn_next => null ()
       success = .false.
    end select
  end subroutine decode_assignment
  
  recursive subroutine decode_atom (builder, code, success)
    class(pnd_atom_t), intent(inout) :: builder
    type(code_t), intent(out) :: code
    logical, intent(out) :: success
    success = .true.
    select case (builder%position%part)
    case (POS_HERE)
       code%cat = CAT_REFERENCE
       builder%position%part = POS_ID
       builder%pn_next => null ()
    case (POS_ID)
       code%cat = CAT_ID
       call code%create_string_val (builder%pn%get_string ())
       builder%position%part = POS_NONE
       builder%pn_next => null ()
    case default
       builder%pn_next => null ()
       success = .false.
    end select
  end subroutine decode_atom
  
  recursive subroutine decode_logical_expr (builder, code, success)
    class(pnd_logical_expr_t), intent(inout) :: builder
    type(code_t), intent(out) :: code
    logical, intent(out) :: success
    type(parse_node_t), pointer :: pn_arg, pn_op
    type(syntax_rule_t), pointer :: rule
    integer :: i, n_terms
    success = .true.
    select case (builder%position%part)
    case (POS_HERE)
       if (builder%unary) then
          pn_arg => null ()
          pn_op => builder%pn
       else
          pn_arg => builder%pn%get_sub_ptr ()
          pn_op => builder%pn%get_sub_ptr (2)
       end if
       n_terms = builder%pn%get_n_sub ()
       if (associated (pn_op)) then
          rule => pn_op%get_rule_ptr ()
          code%cat = CAT_COMPOSITE
          code%natt = 5
          select case (char (rule%get_key ()))
          case ("not_clause")
             code%att(1) = find_index (builder%prototypes, var_str ("not"))
             code%att(2) = MODE_CONSTANT
             code%att(4) = 1
             code%att(5) = 1
          case ("and_clause")
             code%att(1) = find_index (builder%prototypes, var_str ("and"))
             code%att(2) = MODE_CONSTANT
             code%att(4) = n_terms
             code%att(5) = n_terms
          case ("or_clause")
             code%att(1) = find_index (builder%prototypes, var_str ("or"))
             code%att(2) = MODE_CONSTANT
             code%att(4) = n_terms
             code%att(5) = n_terms
          end select
          builder%position%part = POS_MEMBER
          builder%position%i = 1
          builder%pn_next => pn_arg
       else
          builder%position%part = POS_NONE
          builder%pn_next => pn_arg
       end if
    case (POS_MEMBER)
       i = builder%position%i
       i = i + 1
       if (builder%unary) then
          pn_op => builder%pn
       else
          pn_op => builder%pn%get_sub_ptr (i)
       end if
       pn_arg => pn_op%get_sub_ptr (2)
       builder%position%part = POS_CORE
       builder%pn_next => pn_arg
    case (POS_CORE)
       code%cat = CAT_VALUE
       builder%position%part = POS_NONE
       builder%pn_next => null ()
    case default
       builder%pn_next => null ()
       success = .false.
    end select
  end subroutine decode_logical_expr
  
  recursive subroutine decode_logical_literal (builder, code, success)
    class(pnd_logical_literal_t), intent(inout) :: builder
    type(code_t), intent(out) :: code
    logical, intent(out) :: success
    type(parse_node_t), pointer :: pn1, pn_op
    success = .true.
    select case (builder%position%part)
    case (POS_HERE)
       code%cat = CAT_COMPOSITE
       code%natt = 2
       code%att(1) = find_index (builder%prototypes, var_str ("logical"))
       code%att(2) = MODE_CONSTANT
       builder%position%part = POS_CORE
       builder%pn_next => null ()
    case (POS_CORE)
       code%cat = CAT_VALUE
       call code%create_logical_val (builder%value)
       builder%position%part = POS_NONE
       builder%pn_next => null ()
    case default
       builder%pn_next => null ()
       success = .false.
    end select
  end subroutine decode_logical_literal
  
  subroutine sindarin_parser_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (sindarin_parser_1, "sindarin_parser_1", &
         "syntax table", &
         u, results)
    call test (sindarin_parser_2, "sindarin_parser_2", &
         "repository", &
         u, results)
    call test (sindarin_parser_3, "sindarin_parser_3", &
         "parse script", &
         u, results)  
  end subroutine sindarin_parser_test
  

  subroutine sindarin_parser_1 (u)
    integer, intent(in) :: u

    write (u, "(A)")  "* Test output: sindarin_parser_1"
    write (u, "(A)")  "*   Purpose: build syntax table"
    write (u, "(A)")      
    
    call syntax_sindarin_init ()
    call syntax_sindarin_write (u)
    call syntax_sindarin_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sindarin_parser_1"
    
    end subroutine sindarin_parser_1

  subroutine sindarin_parser_2 (u)
    integer, intent(in) :: u
    type(repository_t) :: repository
    type(string_t), dimension(:), allocatable :: name
    integer :: i

    write (u, "(A)")  "* Test output: sindarin_parser_2"
    write (u, "(A)")  "*   Purpose: build Sindarin repository"
    write (u, "(A)")      
    
    call make_sindarin_repository (repository)
    call repository%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Name array"
    write (u, "(A)")
    
    call repository%get_prototype_names (name)
    do i = 1, size (name)
       write (u, "(I0,1x,A)")  i, char (name (i))
    end do

    call repository%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sindarin_parser_2"
    
    end subroutine sindarin_parser_2

  subroutine sindarin_parser_3 (u)
    integer, intent(in) :: u
    type(lexer_t) :: lexer
    type(stream_t), target :: stream
    type(parse_tree_t) :: parse_tree
    type(sindarin_decoder_t) :: decoder
    type(repository_t), allocatable :: repository
    type(string_t), dimension(:), allocatable :: prototype_names
    type(object_builder_t) :: builder
    type(code_t) :: code
    class(object_t), pointer :: main
    logical :: success
    integer :: u_sin, u_code, iostat
    character(80) :: buffer

    write (u, "(A)")  "* Test output: sindarin_parser_3"
    write (u, "(A)")  "*   Purpose: parse a simple script"
    write (u, "(A)")      
    
    call syntax_sindarin_init ()

    allocate (repository)
    call make_sindarin_repository (repository)

    write (u, "(A)")  "* Create script"
    write (u, "(A)")      

    u_sin = free_unit ()
    open (u_sin, status="scratch")
    
    write (u_sin, "(A)")  "logical a"
    write (u_sin, "(A)")  "logical b = true"
    write (u_sin, "(A)")  "a = b and not false"          ! = true 
    write (u_sin, "(A)")  "b = (a or true) and (not b)"  ! = false
    
    rewind (u_sin)
    do
       read (u_sin, "(A)", end=1)  buffer
       write (u, "(A)") trim (buffer)
    end do
1   continue
    
    rewind (u_sin)

    write (u, "(A)")      
    write (u, "(A)")  "* Parse script"
    write (u, "(A)")      

    call lexer%init ( &
         comment_chars = "", &
         quote_chars = '', &
         quote_match = '', &
         single_chars = "()", &
         special_class = [ "=" ] , &
         keyword_list = syntax_get_keyword_list_ptr (syntax_sindarin))

    call stream%init (u_sin)
    call lexer%assign_stream (stream)

    call parse_tree%parse (syntax_sindarin, lexer)

    write (u, "(A)")  "* Setup decoder"
    write (u, "(A)")
    
    call repository%get_prototype_names (prototype_names)

    call decoder%init (parse_tree, prototype_names)
    call decoder%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Decode (see 'sindarin_parser_3.dat')"
    write (u, "(A)")
    
    u_code = free_unit ()
    open (u_code, file="sindarin_parser_3.dat", &
         status="replace", action="readwrite")
    
    do
       call decoder%decode (code, success)
       if (.not. success)  exit
       call code%write (u_code)
    end do
    
    write (u, "(A)")  "* Create object tree"
    write (u, "(A)")
    
    rewind (u_code)

    call builder%import_repository (repository)
    call builder%init_empty ()

    do
       call code%read (u_code, iostat=iostat)
       if (iostat /= 0)  exit
       call builder%build (code, success)
       if (.not. success)  exit
    end do
    close (u_code)

    call builder%export (main)
    
    call main%write (u)
    call remove_object (main)

    write (u, "(A)")      
    write (u, "(A)")  "* Cleanup"
    
    call builder%final ()
    call parse_tree%final ()

    close (u_sin)
    close (u_code)
    call stream%final ()
    call lexer%final ()

    call syntax_sindarin_final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sindarin_parser_3"
    
    end subroutine sindarin_parser_3


end module sindarin_parser
