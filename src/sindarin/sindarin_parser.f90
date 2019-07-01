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

module sindarin_parser

  use iso_varying_string, string_t => varying_string

  use io_units
  use diagnostics
  use ifiles
  use syntax_rules
  use parser
  use codes
  use builders
  use object_base
  use object_expr
  use object_logical
  use object_integer
  use object_container
  use object_comparison
  use object_conditional

  implicit none
  private

  public :: make_sindarin_repository
  public :: syntax_sindarin
  public :: syntax_sindarin_init
  public :: syntax_sindarin_final
  public :: syntax_sindarin_write
  public :: sindarin_decoder_t

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
     generic :: get_index => &
          pn_decoder_get_index_string, &
          pn_decoder_get_index_char
     procedure, private :: pn_decoder_get_index_string
     procedure, private :: pn_decoder_get_index_char
     procedure :: get_part => pn_decoder_get_part
     procedure :: next_part => pn_decoder_next_part
     procedure :: next_index => pn_decoder_next_index
     procedure :: get_next_node => pn_decoder_get_next_node
     generic :: get_n_terms => &
          pn_decoder_get_n_terms, &
          pn_decoder_get_n_terms_index, &
          pn_decoder_get_n_terms_array
     procedure, private :: pn_decoder_get_n_terms
     procedure, private :: pn_decoder_get_n_terms_index
     procedure, private :: pn_decoder_get_n_terms_array
     generic :: get_pn => get_pn0, get_pn1, get_pn2
     procedure :: get_pn0 => pn_decoder_get_pn
     procedure :: get_pn1 => pn_decoder_get_pn_index
     procedure :: get_pn2 => pn_decoder_get_pn_array
     generic :: get_key => &
          pn_decoder_get_key, &
          pn_decoder_get_key_index, &
          pn_decoder_get_key_array
     procedure, private :: pn_decoder_get_key
     procedure, private :: pn_decoder_get_key_index
     procedure, private :: pn_decoder_get_key_array
     generic :: get_integer => &
          pn_decoder_get_integer, &
          pn_decoder_get_integer_index, &
          pn_decoder_get_integer_array
     procedure, private :: pn_decoder_get_integer
     procedure, private :: pn_decoder_get_integer_index
     procedure, private :: pn_decoder_get_integer_array
     generic :: has_term => &
          pn_decoder_has_term, &
          pn_decoder_has_term_index, &
          pn_decoder_has_term_array
     procedure, private :: pn_decoder_has_term
     procedure, private :: pn_decoder_has_term_index
     procedure, private :: pn_decoder_has_term_array
     generic :: goto => &
          pn_decoder_goto_self, &
          pn_decoder_goto_index, &
          pn_decoder_goto_array
     procedure, private :: pn_decoder_goto_self
     procedure, private :: pn_decoder_goto_index
     procedure, private :: pn_decoder_goto_array
     procedure :: stay => pn_decoder_stay
     procedure :: done => pn_decoder_done
     procedure :: fail => pn_decoder_fail
     procedure (pn_decoder_create_node_code), deferred :: create_node_code
     procedure :: create_core_code => pn_decoder_create_empty_value
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
     integer :: n_dec = 0
     integer :: n_com = 0
     integer, dimension(:), allocatable :: dec
     integer, dimension(:), allocatable :: com
   contains
     procedure :: setup => setup_script
     procedure :: decode => decode_script
     procedure :: create_node_code => create_node_code_script
  end type pnd_script_t
  
  type, extends (pn_decoder_t) :: pnd_declaration_t
     private
     type(string_t) :: key
     type(string_t) :: var_name
   contains
     procedure :: setup => setup_declaration
     procedure :: decode => decode_declaration
     procedure :: create_node_code => create_node_code_declaration
  end type pnd_declaration_t
  
  type, extends (pn_decoder_t) :: pnd_assignment_t
     private
     integer :: i_lhs = 0
     type(string_t) :: var_name
   contains
     procedure :: setup => setup_assignment
     procedure :: decode => decode_assignment
     procedure :: create_node_code => create_node_code_assignment
     procedure :: create_id_code => create_id_code_assignment
  end type pnd_assignment_t
  
  type, extends (pn_decoder_t) :: pnd_atom_t
     private
   contains
     procedure :: decode => decode_atom
     procedure :: create_node_code => create_node_code_atom
     procedure :: create_id_code => create_id_code_atom
  end type pnd_atom_t
  
  type, extends (pn_decoder_t), abstract :: pnd_expr_t
     private
     type(syntax_rule_t), pointer :: rule_orig => null ()
     integer :: n_sub = 0
     integer :: n_terms = 0
     logical, dimension(:), allocatable :: is_master_op
     type(string_t), dimension(:), allocatable :: key
     logical :: minus_sign = .false.
     type(syntax_rule_t), pointer :: rule_sign => null ()
     logical :: use_pn_sub = .false.
     logical, dimension(:), allocatable :: new_pn
     type(parse_node_p), dimension(:), allocatable :: pn_sub
   contains
     procedure :: final => final_expr
     procedure :: get_pn1 => expr_get_pn_index
     procedure :: get_pn2 => expr_get_pn_array
     procedure :: setup => setup_expr
     procedure :: decode => decode_expr
  end type pnd_expr_t
  
  type, extends (pn_decoder_t) :: pnd_positive_wrapper_t
     private
   contains
     procedure :: decode => decode_positive_wrapper
     procedure :: create_node_code => create_node_code_positive_wrapper
  end type pnd_positive_wrapper_t
  
  type, extends (pn_decoder_t) :: pnd_negative_wrapper_t
     private
   contains
     procedure :: decode => decode_negative_wrapper
     procedure :: create_node_code => create_node_code_negative_wrapper
  end type pnd_negative_wrapper_t
  
  type, extends (pnd_expr_t) :: pnd_container_expr_t
     private
   contains
     procedure :: create_node_code => create_node_code_container_expr
     procedure :: create_core_code => create_core_code_container_expr
  end type pnd_container_expr_t
  
  type, extends (pnd_expr_t) :: pnd_logical_expr_t
     private
   contains
     procedure :: create_node_code => create_node_code_logical_expr
  end type pnd_logical_expr_t
  
  type, extends (pn_decoder_t) :: pnd_logical_not_t
     private
   contains
     procedure :: decode => decode_logical_not
     procedure :: create_node_code => create_node_code_logical_not
  end type pnd_logical_not_t
  
  type, extends (pn_decoder_t) :: pnd_logical_literal_t
     private
     logical :: value
   contains
     procedure :: decode => decode_logical_literal
     procedure :: create_node_code => create_node_code_logical_literal
     procedure :: create_core_code => create_core_code_logical_literal
  end type pnd_logical_literal_t
  
  type, extends (pn_decoder_t) :: pnd_comparison_expr_t
     private
     integer :: n_terms = 0
   contains
     procedure :: setup => setup_comparison_expr
     procedure :: decode => decode_comparison_expr
     procedure :: create_node_code => create_node_code_comparison_expr
  end type pnd_comparison_expr_t
  
  type, extends (pnd_expr_t) :: pnd_integer_expr_t
     private
   contains
     procedure :: create_node_code => create_node_code_integer_expr
  end type pnd_integer_expr_t
  
  type, extends (pn_decoder_t), abstract :: pnd_integer_literal_t
     private
     integer :: value
   contains
     procedure (setup_integer_literal), deferred :: setup
     procedure :: decode => decode_integer_literal
     procedure :: create_node_code => create_node_code_integer_literal
     procedure :: create_core_code => create_core_code_integer_literal
  end type pnd_integer_literal_t
  
  type, extends (pnd_integer_literal_t) :: pnd_positive_integer_t
     private
   contains
     procedure :: setup => setup_positive_integer
  end type pnd_positive_integer_t
  
  type, extends (pnd_integer_literal_t) :: pnd_negative_integer_t
     private
   contains
     procedure :: setup => setup_negative_integer
  end type pnd_negative_integer_t
  
  type, extends (pn_decoder_t) :: pnd_conditional_expr_t
     private
     integer :: n_branches = 2
     integer :: n_terms = 3
     integer :: n_elsif = 0
     logical :: has_else = .false.
   contains
     procedure :: setup => setup_conditional_expr
     procedure :: decode => decode_conditional_expr
     procedure :: create_node_code => create_node_code_conditional_expr
     procedure :: create_else_code => create_else_code_conditional_expr
  end type pnd_conditional_expr_t
  

  abstract interface
     subroutine pn_decoder_create_node_code (builder, code)
       import
       class(pn_decoder_t), intent(in) :: builder
       type(code_t), intent(out) :: code
     end subroutine pn_decoder_create_node_code
  end interface
  
  abstract interface
     subroutine setup_integer_literal (builder)
       import
       class(pnd_integer_literal_t), intent(inout) :: builder
     end subroutine setup_integer_literal
  end interface
     

  type(syntax_t), target, save :: syntax_sindarin
  

contains

  subroutine make_sindarin_repository (repository)
    type(repository_t), intent(inout) :: repository
    class(object_t), pointer :: core
    class(object_t), pointer :: logical
    class(object_t), pointer :: integer
    class(object_t), pointer :: container
    class(object_t), pointer :: not, and, or
    class(object_t), pointer :: compare
    class(object_t), pointer :: minus, multiply, add
    class(object_t), pointer :: conditional
    class(object_t), pointer :: assignment
    integer :: i
    integer, parameter :: n_members = 12
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
    ! integer
    allocate (composite_t :: integer)
    select type (integer)
    type is (composite_t)
       allocate (integer_t :: core)
       call integer%init (var_str ("integer"))
       call integer%import_core (core)
    end select
    ! Containers
    allocate (container_t :: container)
    select type (container)
    type is (container_t);  call container%init (CT_LIST)
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
    ! Comparison operator
    ! compare
    allocate (compare_t :: compare)
    select type (compare)
    type is (compare_t);  call compare%init (logical)
    end select
    ! Integer operators
    ! minus
    allocate (minus_t :: minus)
    select type (minus)
    type is (minus_t);  call minus%init (integer)
    end select
    ! multiply
    allocate (multiply_t :: multiply)
    select type (multiply)
    type is (multiply_t);  call multiply%init (integer)
    end select
    ! add
    allocate (add_t :: add)
    select type (add)
    type is (add_t);  call add%init (integer)
    end select
    ! Structured expressions
    ! conditional
    allocate (conditional_t :: conditional)
    select type (conditional)
    type is (conditional_t);  call conditional%init (logical, integer)
    end select
    ! Statements
    ! Assignment
    allocate (assignment_t :: assignment)
    select type (assignment)
    type is (assignment_t);  call assignment%init ()
    end select
    i = 0
    i = i + 1;  call repository%import_member (i, logical)
    i = i + 1;  call repository%import_member (i, integer)
    i = i + 1;  call repository%import_member (i, container)
    i = i + 1;  call repository%import_member (i, not)
    i = i + 1;  call repository%import_member (i, and)
    i = i + 1;  call repository%import_member (i, or)
    i = i + 1;  call repository%import_member (i, compare)
    i = i + 1;  call repository%import_member (i, minus)
    i = i + 1;  call repository%import_member (i, multiply)
    i = i + 1;  call repository%import_member (i, add)
    i = i + 1;  call repository%import_member (i, conditional)
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
    call ifile%append ("ALT builtin_type = logical | integer")
    call ifile%append ("KEY logical")
    call ifile%append ("KEY integer")
    call ifile%append ("IDE var_name")
    call ifile%append ("SEQ assignment_clause = '=' generic_expr")
    call ifile%append ("KEY '='")
    call ifile%append ("ALT generic_expr = signed_expr | expr")
    call ifile%append ("SEQ signed_expr = sign expr")
    call ifile%append ("ALT sign = '+' | '-'")
    call ifile%append ("SEQ expr = term operator_clause*")
    call ifile%append ("ALT operator_clause =&
         & separator_clause |&
         & logical_operation |&
         & comparison |&
         & arithmetic_operation")
    call ifile%append ("SEQ separator_clause = separator term")
    call ifile%append ("ALT separator = ':' | ',' | '=>'")
    call ifile%append ("KEY ':'")
    call ifile%append ("KEY ','")
    call ifile%append ("KEY '=>'")
    call ifile%append ("SEQ logical_operation = logical_operator term")
    call ifile%append ("ALT logical_operator = or | and")
    call ifile%append ("KEY or")
    call ifile%append ("KEY and")
    call ifile%append ("SEQ comparison = comparison_operator term")
    call ifile%append ("ALT comparison_operator =&
         & '==' | '<>' | '<' | '>' | '<=' | '>='")
    call ifile%append ("KEY '=='")
    call ifile%append ("KEY '<>'")
    call ifile%append ("KEY '<'")
    call ifile%append ("KEY '>'")
    call ifile%append ("KEY '<='")
    call ifile%append ("KEY '>='")
    call ifile%append ("SEQ arithmetic_operation = arithmetic_operator term")
    call ifile%append ("ALT arithmetic_operator =&
         & '+' | '-' | '*' | '/' | '^'")
    call ifile%append ("KEY '+'")
    call ifile%append ("KEY '-'")
    call ifile%append ("KEY '*'")
    call ifile%append ("KEY '/'")
    call ifile%append ("KEY '^'")
    call ifile%append ("ALT term =&
         & not_clause | logical_literal |&
         & integer_literal |&
         & conditional_expr |&
         & group | atom")
    call ifile%append ("SEQ not_clause = not logical_expr")
    call ifile%append ("ALT logical_expr = term comparison*")
    call ifile%append ("ALT logical_literal = true | false")
    call ifile%append ("KEY not")
    call ifile%append ("KEY true")
    call ifile%append ("KEY false")
    call ifile%append ("INT integer_literal")
    call ifile%append ("SEQ conditional_expr =&
         & if_expr elsif_expr_part else_expr_part endif")
    call ifile%append ("SEQ if_expr = if expr then expr")
    call ifile%append ("SEQ elsif_expr_part = elsif_expr*")
    call ifile%append ("SEQ elsif_expr = elsif expr then expr")
    call ifile%append ("SEQ else_expr_part = else_expr?") 
    call ifile%append ("SEQ else_expr = else expr")
    call ifile%append ("KEY if")
    call ifile%append ("KEY then")
    call ifile%append ("KEY elsif")
    call ifile%append ("KEY else")
    call ifile%append ("KEY endif")
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
    
  function pn_decoder_get_index_string (builder, key) result (i)
    class(pn_decoder_t), intent(in) :: builder
    type(string_t), intent(in) :: key
    integer :: i
    i = find_index (builder%prototypes, key)
  end function pn_decoder_get_index_string
  
  function pn_decoder_get_index_char (builder, key) result (i)
    class(pn_decoder_t), intent(in) :: builder
    character(len=*), intent(in) :: key
    integer :: i
    i = find_index (builder%prototypes, var_str (key))
  end function pn_decoder_get_index_char
  
  function find_index (array, key) result (i_prototype)
    type(string_t), dimension(:), intent(in) :: array
    type(string_t), intent(in) :: key
    integer :: i_prototype
    integer :: i
    i_prototype = 0
    do i = 1, size (array)
       if (key == array(i)) then
          i_prototype = i
          return
       end if
    end do
  end function find_index

  function pn_decoder_get_part (builder) result (part)
    class(pn_decoder_t), intent(in) :: builder
    integer :: part
    part = builder%position%part
  end function pn_decoder_get_part
  
  subroutine pn_decoder_next_part (builder, part)
    class(pn_decoder_t), intent(inout) :: builder
    integer, intent(in) :: part
    builder%position%part = part
    builder%position%i = 1
  end subroutine pn_decoder_next_part
  
  subroutine pn_decoder_next_index (builder)
    class(pn_decoder_t), intent(inout) :: builder
    builder%position%i = builder%position%i + 1
  end subroutine pn_decoder_next_index

  subroutine pn_decoder_get_next_node (builder, parse_node)
    class(pn_decoder_t), intent(in) :: builder
    type(parse_node_t), pointer, intent(out) :: parse_node
    parse_node => builder%pn_next
  end subroutine pn_decoder_get_next_node
  
  function pn_decoder_get_n_terms (builder) result (n)
    class(pn_decoder_t), intent(in) :: builder
    type(parse_node_t), pointer :: pn
    integer :: n
    call builder%get_pn0 (pn)
    n = pn%get_n_sub ()
  end function pn_decoder_get_n_terms
  
  function pn_decoder_get_n_terms_index (builder, i) result (n)
    class(pn_decoder_t), intent(in) :: builder
    integer, intent(in) :: i
    type(parse_node_t), pointer :: pn
    integer :: n
    call builder%get_pn1 (i, pn)
    if (associated (pn)) then
       n = pn%get_n_sub ()
    else
       n = 0
    end if
  end function pn_decoder_get_n_terms_index
  
  function pn_decoder_get_n_terms_array (builder, ii) result (n)
    class(pn_decoder_t), intent(in) :: builder
    integer, dimension(:), intent(in) :: ii
    type(parse_node_t), pointer :: pn
    integer :: n
    call builder%get_pn2 (ii, pn)
    if (associated (pn)) then
       n = pn%get_n_sub ()
    else
       n = 0
    end if
  end function pn_decoder_get_n_terms_array
  
  subroutine pn_decoder_get_pn (builder, pn, key)
    class(pn_decoder_t), intent(in) :: builder
    type(parse_node_t), intent(out), pointer :: pn
    type(string_t), intent(out), optional :: key
    pn => builder%pn
    if (present (key)) then
       key = builder%rule%get_key ()
    end if
  end subroutine pn_decoder_get_pn
          
  subroutine pn_decoder_get_pn_index (builder, i, pn, key)
    class(pn_decoder_t), intent(in) :: builder
    integer, intent(in) :: i
    type(parse_node_t), intent(out), pointer :: pn
    type(string_t), intent(out), optional :: key
    type(syntax_rule_t), pointer :: rule
    pn => builder%pn%get_sub_ptr (i)
    if (present (key)) then
       if (associated (pn)) then
          rule => pn%get_rule_ptr ()
          key = rule%get_key ()
       else
          key = ""
       end if
    end if
  end subroutine pn_decoder_get_pn_index
          
  subroutine pn_decoder_get_pn_array (builder, ii, pn, key)
    class(pn_decoder_t), intent(in) :: builder
    integer, dimension(:), intent(in) :: ii
    type(parse_node_t), intent(out), pointer :: pn
    type(string_t), intent(out), optional :: key
    type(syntax_rule_t), pointer :: rule
    integer :: k
    pn => builder%pn
    do k = 1, size (ii)
       if (associated (pn)) then
          pn => pn%get_sub_ptr (ii(k))
       else
          exit
       end if
    end do
    if (present (key)) then
       if (associated (pn)) then
          rule => pn%get_rule_ptr ()
          key = rule%get_key ()
       else
          key = ""
       end if
    end if
  end subroutine pn_decoder_get_pn_array
          
  function pn_decoder_get_key (builder) result (key)
    class(pn_decoder_t), intent(in) :: builder
    type(string_t) :: key
    type(parse_node_t), pointer :: pn
    call builder%get_pn0 (pn, key)
  end function pn_decoder_get_key
          
  function pn_decoder_get_key_index (builder, i) result (key)
    class(pn_decoder_t), intent(in) :: builder
    integer, intent(in) :: i
    type(string_t) :: key
    type(parse_node_t), pointer :: pn
    call builder%get_pn1 (i, pn, key)
  end function pn_decoder_get_key_index
          
  function pn_decoder_get_key_array (builder, ii) result (key)
    class(pn_decoder_t), intent(in) :: builder
    integer, dimension(:), intent(in) :: ii
    type(string_t) :: key
    type(parse_node_t), pointer :: pn
    call builder%get_pn2 (ii, pn, key)
  end function pn_decoder_get_key_array
          
  function pn_decoder_get_integer (builder) result (value)
    class(pn_decoder_t), intent(in) :: builder
    integer :: value
    type(parse_node_t), pointer :: pn
    call builder%get_pn0 (pn)
    value = pn%get_integer ()
  end function pn_decoder_get_integer
          
  function pn_decoder_get_integer_index (builder, i) result (value)
    class(pn_decoder_t), intent(in) :: builder
    integer, intent(in) :: i
    integer :: value
    type(parse_node_t), pointer :: pn
    call builder%get_pn1 (i, pn)
    value = pn%get_integer ()
  end function pn_decoder_get_integer_index
          
  function pn_decoder_get_integer_array (builder, ii) result (value)
    class(pn_decoder_t), intent(in) :: builder
    integer, dimension(:), intent(in) :: ii
    integer :: value
    type(parse_node_t), pointer :: pn
    call builder%get_pn2 (ii, pn)
    value = pn%get_integer ()
  end function pn_decoder_get_integer_array
          
  function pn_decoder_has_term (builder) result (flag)
    class(pn_decoder_t), intent(in) :: builder
    logical :: flag
    type(parse_node_t), pointer :: pn
    call builder%get_pn0 (pn)
    flag = associated (pn)
  end function pn_decoder_has_term
          
  function pn_decoder_has_term_index (builder, i) result (flag)
    class(pn_decoder_t), intent(in) :: builder
    integer, intent(in) :: i
    logical :: flag
    type(parse_node_t), pointer :: pn
    call builder%get_pn1 (i, pn)
    flag = associated (pn)
  end function pn_decoder_has_term_index
          
  function pn_decoder_has_term_array (builder, ii) result (flag)
    class(pn_decoder_t), intent(in) :: builder
    integer, dimension(:), intent(in) :: ii
    logical :: flag
    type(parse_node_t), pointer :: pn
    call builder%get_pn2 (ii, pn)
    flag = associated (pn)
  end function pn_decoder_has_term_array
          
  subroutine pn_decoder_goto_self (builder)
    class(pn_decoder_t), intent(inout) :: builder
    call builder%get_pn0 (builder%pn_next)
  end subroutine pn_decoder_goto_self

  subroutine pn_decoder_goto_index (builder, i)
    class(pn_decoder_t), intent(inout) :: builder
    integer, intent(in) :: i
    call builder%get_pn1 (i, builder%pn_next)
  end subroutine pn_decoder_goto_index

  subroutine pn_decoder_goto_array (builder, ii)
    class(pn_decoder_t), intent(inout) :: builder
    integer, dimension(:), intent(in) :: ii
    call builder%get_pn2 (ii, builder%pn_next)
  end subroutine pn_decoder_goto_array

  subroutine pn_decoder_stay (builder)
    class(pn_decoder_t), intent(inout) :: builder
    builder%pn_next => null ()
  end subroutine pn_decoder_stay
  
  subroutine pn_decoder_done (builder)
    class(pn_decoder_t), intent(inout) :: builder
    builder%position%part = POS_NONE
    builder%position%i = 0
    builder%pn_next => null ()
  end subroutine pn_decoder_done
  
  subroutine pn_decoder_fail (builder, success)
    class(pn_decoder_t), intent(inout) :: builder
    logical, intent(out) :: success
    call builder%done ()
    success = .false.
  end subroutine pn_decoder_fail
  
  subroutine pn_decoder_create_empty_value (builder, code)
    class(pn_decoder_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    call code%set (CAT_VALUE)
  end subroutine pn_decoder_create_empty_value

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
          if (code%get_cat () /= 0) then
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
    type (parse_node_t), pointer :: pn
    type(syntax_rule_t), pointer :: rule
    pn => parse_node
    rule => parse_node%get_rule_ptr ()
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
       case ("expr")
          call allocate_expr_decoder (pn_decoder, pn, signed=.false.)
       case ("signed_expr")
          call allocate_expr_decoder (pn_decoder, pn, signed=.true.)
       case ("container_expr")
          allocate (pnd_container_expr_t :: pn_decoder)
       case ("not_clause")
          allocate (pnd_logical_not_t :: pn_decoder)
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
       case ("comparison_expr")
          allocate (pnd_comparison_expr_t :: pn_decoder)
       case ("integer_literal")
          allocate (pnd_positive_integer_t :: pn_decoder)
       case ("conditional_expr")
          allocate (pnd_conditional_expr_t :: pn_decoder)
    case default
       call msg_bug ("Sindarin decoder: rule '" &
            // char (rule%get_key ()) // "' not supported")
    end select
    call pn_decoder%init (pn, rule, builder%prototypes)
  end subroutine sindarin_decoder_make_pn_decoder
    
  subroutine setup_script (builder)
    class(pnd_script_t), intent(inout) :: builder
    logical, dimension(:), allocatable :: mask_dec, mask_com
    integer :: i, n_terms
    n_terms = builder%get_n_terms ()
    allocate (mask_dec (n_terms))
    allocate (mask_com (n_terms))
    do i = 1, n_terms
       select case (char (builder%get_key (i)))
       case ("declaration")
          mask_dec(i) = .true.
          mask_com(i) = builder%has_term ([i,3])
       case default
          mask_dec(i) = .false.
          mask_com(i) = .true.
       end select
    end do
    builder%n_dec = count (mask_dec)
    builder%n_com = count (mask_com)
    allocate (builder%dec (builder%n_dec))
    allocate (builder%com (builder%n_com))    
    builder%dec = pack ([(i, i=1, n_terms)], mask_dec)
    builder%com = pack ([(i, i=1, n_terms)], mask_com)
  end subroutine setup_script
    
  recursive subroutine decode_script (builder, code, success)
    class(pnd_script_t), intent(inout) :: builder
    type(code_t), intent(out) :: code
    logical, intent(out) :: success
    integer :: i
    success = .true.
    select case (builder%position%part)
    case (POS_HERE)
       call builder%setup ()
       call builder%create_node_code (code)
       if (builder%n_dec > 0) then
          call builder%next_part (POS_MEMBER)
          call builder%goto (builder%dec(1))
       else if (builder%n_com > 0) then
          call builder%next_part (POS_PRIMER)
          call builder%goto (builder%com(1))
       else
          call builder%done ()
       end if
    case (POS_MEMBER)
       i = builder%position%i
       if (i < builder%n_dec) then
          call builder%next_index ()
          call builder%goto (builder%dec(i+1))
       else if (builder%n_com > 0) then
          call builder%next_part (POS_PRIMER)
          call builder%goto (builder%com(1))
       else
          call builder%done ()
       end if
    case (POS_PRIMER)
       i = builder%position%i
       if (i < builder%n_com) then
          call builder%next_index ()
          call builder%goto (builder%com(i+1))
       else
          call builder%done ()
       end if
    case default
       call builder%fail (success)
    end select
  end subroutine decode_script
  
  subroutine create_node_code_script (builder, code)
    class(pnd_script_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    call code%set (CAT_COMPOSITE, &
         [0, 0, 0, builder%n_dec, 0, builder%n_com])
    call code%create_string_val ([builder%rule%get_key ()])
  end subroutine create_node_code_script
  
  subroutine setup_declaration (builder)
    class(pnd_declaration_t), intent(inout) :: builder
    type(parse_node_t), pointer :: pn_var
    builder%key = builder%get_key (1)
    call builder%get_pn ([2], pn_var)
    builder%var_name = pn_var%get_string ()
  end subroutine setup_declaration
    
  recursive subroutine decode_declaration (builder, code, success)
    class(pnd_declaration_t), intent(inout) :: builder
    type(code_t), intent(out) :: code
    logical, intent(out) :: success
    success = .true.
    select case (builder%position%part)
    case (POS_HERE)
       call builder%setup ()
       call builder%create_node_code (code)
       call builder%next_part (POS_CORE)
       call builder%stay ()
    case (POS_CORE)
       call builder%create_core_code (code)
       call builder%done ()
    case default
       call builder%fail (success)
    end select
  end subroutine decode_declaration
  
  subroutine create_node_code_declaration (builder, code)
    class(pnd_declaration_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    call code%set (CAT_COMPOSITE, &
         [builder%get_index (builder%key), MODE_VARIABLE])
    call code%create_string_val ([builder%var_name])
  end subroutine create_node_code_declaration
  
  subroutine setup_assignment (builder)
    class(pnd_assignment_t), intent(inout) :: builder
    type(parse_node_t), pointer :: pn_lhs
    integer :: i_lhs
    select case (char (builder%get_key ()))
    case ("assignment");  i_lhs = 1
    case ("declaration"); i_lhs = 2
    end select
    call builder%get_pn (i_lhs, pn_lhs)
    builder%i_lhs = i_lhs
    builder%var_name = pn_lhs%get_string ()
  end subroutine setup_assignment
    
  recursive subroutine decode_assignment (builder, code, success)
    class(pnd_assignment_t), intent(inout) :: builder
    type(code_t), intent(out) :: code
    logical, intent(out) :: success
    success = .true.
    select case (builder%position%part)
    case (POS_HERE)
       call builder%setup ()
       call builder%create_node_code (code)
       call builder%next_part (POS_ID)
       call builder%stay ()
    case (POS_ID)
       call builder%create_id_code (code)
       call builder%next_part (POS_MEMBER)
       call builder%stay ()
    case (POS_MEMBER)
       call builder%next_part (POS_NONE)
       call builder%goto ([builder%i_lhs+1, 2])
    case default
       call builder%fail (success)
    end select
  end subroutine decode_assignment
  
  subroutine create_node_code_assignment (builder, code)
    class(pnd_assignment_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    call code%set (CAT_COMPOSITE, [builder%get_index ("assignment")])
  end subroutine create_node_code_assignment
  
  subroutine create_id_code_assignment (builder, code)
    class(pnd_assignment_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    call code%set (CAT_ID)
    call code%create_string_val ([builder%var_name])
  end subroutine create_id_code_assignment
  
  recursive subroutine decode_atom (builder, code, success)
    class(pnd_atom_t), intent(inout) :: builder
    type(code_t), intent(out) :: code
    logical, intent(out) :: success
    success = .true.
    select case (builder%position%part)
    case (POS_HERE)
       call builder%create_node_code (code)
       call builder%next_part (POS_ID)
       call builder%stay ()
    case (POS_ID)
       call builder%create_id_code (code)
       call builder%done ()
    case default
       call builder%fail (success)
    end select
  end subroutine decode_atom
  
  subroutine create_node_code_atom (builder, code)
    class(pnd_atom_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    call code%set (CAT_REFERENCE)
  end subroutine create_node_code_atom
  
  subroutine create_id_code_atom (builder, code)
    class(pnd_atom_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    call code%set (CAT_ID)
    call code%create_string_val ([builder%pn%get_string ()])
  end subroutine create_id_code_atom
  
  subroutine allocate_expr_decoder (builder, pn, signed)
    class(pn_decoder_t), intent(inout), pointer :: builder
    type(parse_node_t), intent(inout), pointer :: pn
    logical, intent(in) :: signed
    logical :: minus_sign
    type(syntax_rule_t), pointer :: rule_sign
    type(parse_node_t), pointer :: pn_sub
    logical, dimension(:), allocatable :: is_master_op
    type(string_t), dimension(:), allocatable :: key, master_key
    type(string_t) :: select_key
    integer :: n_sub
    !    print *
    !    print *, "allocate expr decoder"
    !    call pn%write ()
    if (signed) then
       call get_sign (pn, minus_sign)
       pn_sub => pn%get_sub_ptr ()
       rule_sign => pn_sub%get_rule_ptr ()
       pn => pn%get_sub_ptr (2)
    else
       rule_sign => null ()
       minus_sign = .false.
    end if
    n_sub = pn%get_n_sub ()
    if (n_sub == 1) then
       pn => pn%get_sub_ptr ()
       if (minus_sign) then
          select case (char (pn%get_rule_key ()))
          case ("integer_literal")
             allocate (pnd_negative_integer_t :: builder)
          case default
             allocate (pnd_negative_wrapper_t :: builder)
          end select
       else
          select case (char (pn%get_rule_key ()))
          case ("integer_literal")
             allocate (pnd_positive_integer_t :: builder)
          case default
             allocate (pnd_positive_wrapper_t :: builder)
          end select
       end if
    else
       call get_all_opkeys (pn, n_sub, minus_sign, key)
       call identify_master_expression (key, minus_sign, is_master_op)
       if (signed .and. is_master_op(1)) then
          if (minus_sign) then
             allocate (pnd_negative_wrapper_t :: builder)
          else
             allocate (pnd_positive_wrapper_t :: builder)
          end if
       else
          is_master_op(1) = .true.
          ! master_key = pack (key, is_master_op)
          call pack_array (key, is_master_op, master_key)
          select_key = master_key(2)
          select case (char (select_key))
          case (":", ",", "=>")
             allocate (pnd_container_expr_t :: builder)
          case ("and", "or")
             allocate (pnd_logical_expr_t :: builder)
          case ("==", "<>", "<", ">", "<=", ">=")
             allocate (pnd_comparison_expr_t :: builder)
          case ("+", "-", "*", "/", "^")
             allocate (pnd_integer_expr_t :: builder)
          case default
             call msg_bug ("Sindarin parser: expr decoder: illegal key '" &
                  // char (select_key) // "'")
          end select
       end if
       select type (builder)
       class is (pnd_expr_t)
          builder%n_sub = n_sub
          allocate (builder%is_master_op (size (is_master_op)))
          builder%is_master_op = is_master_op
          builder%minus_sign = minus_sign
          builder%rule_sign => rule_sign
          builder%n_terms = count (is_master_op)
          allocate (builder%key (builder%n_terms))
          builder%key(1) = ""
          builder%key(2:) = master_key(2:)
       end select
    end if
  end subroutine allocate_expr_decoder
  
  subroutine pack_array (s_in, mask, s_out)
    type(string_t), dimension(:), intent(in) :: s_in
    logical, dimension(:), intent(in) :: mask
    type(string_t), dimension(:), allocatable, intent(inout) :: s_out
    integer :: i, k
    allocate (s_out (count (mask)))
    k = 1
    do i = 1, size (s_in)
       if (mask(i)) then
          s_out(k) = s_in(i)
          k = k + 1
       end if
    end do
  end subroutine pack_array
    
  subroutine get_sign (pn, minus_sign)
    class(parse_node_t), intent(in), target :: pn
    logical, intent(out) :: minus_sign
    type(parse_node_t), pointer :: pn_key
    pn_key => pn%get_sub_ptr ()
    minus_sign = pn_key%get_rule_key () == "-"
  end subroutine get_sign
  
  subroutine get_all_opkeys (pn, n_sub, minus_sign, key)
    type(parse_node_t), intent(in), target :: pn
    integer, intent(in) :: n_sub
    logical, intent(in) :: minus_sign
    type(string_t), dimension(:), intent(inout), allocatable :: key
    type(parse_node_t), pointer :: pn_op_clause, pn_key
    integer :: i
    allocate (key (n_sub))
    if (minus_sign) then
       key(1) = "-"
    else
       key(1) = ""
    end if
    do i = 2, n_sub
       pn_op_clause => pn%get_sub_ptr (i)
       pn_key => pn_op_clause%get_sub_ptr ()
       key(i) = pn_key%get_rule_key ()
    end do
  end subroutine get_all_opkeys
  
  subroutine identify_master_expression (key, minus_sign, is_master_op)
    type(string_t), dimension(:), intent(in) :: key
    logical, intent(in) :: minus_sign
    logical, dimension(:), intent(inout), allocatable :: is_master_op
    integer, dimension(:), allocatable :: prio
    integer :: i, n_sub, min_prio
    n_sub = size (key)
    allocate (prio (n_sub), is_master_op (n_sub))
    prio(1) = unary_priority (key(1))
    do i = 2, n_sub
       prio(i) = binary_priority (key(i))
    end do
    min_prio = minval (prio)
    is_master_op = prio == min_prio
    if (minus_sign) then
       if (any (is_master_op(2:)))  is_master_op(1) = .false.
    end if
  end subroutine identify_master_expression
  
  subroutine final_expr (builder)
    class(pnd_expr_t), intent(inout) :: builder
    integer :: j, k, n_sub
    type(parse_node_t), pointer :: pn_branch, pn_sub
    if (allocated (builder%pn_sub)) then
       do k = 1, size (builder%pn_sub)
          if (.not. builder%new_pn(k))  cycle
          pn_branch => builder%pn_sub(k)%ptr
          n_sub = pn_branch%get_n_sub () 
          if (n_sub > 1) then
             do j = n_sub, 1, -1
                pn_sub => pn_branch%get_sub_ptr (j)
                deallocate (pn_sub)
             end do
             deallocate (pn_branch)
          end if
       end do
    end if
  end subroutine final_expr
  
  subroutine expr_get_pn_index (builder, i, pn, key)
    class(pnd_expr_t), intent(in) :: builder
    integer, intent(in) :: i
    type(parse_node_t), intent(out), pointer :: pn
    type(string_t), intent(out), optional :: key
    type(syntax_rule_t), pointer :: rule
    if (builder%use_pn_sub) then
       pn => builder%pn_sub(i)%ptr
    else
       pn => builder%pn%get_sub_ptr (i)
    end if
    if (present (key)) then
       if (associated (pn)) then
          rule => pn%get_rule_ptr ()
          key = rule%get_key ()
       else
          key = ""
       end if
    end if
  end subroutine expr_get_pn_index
          
  subroutine expr_get_pn_array (builder, ii, pn, key)
    class(pnd_expr_t), intent(in) :: builder
    integer, dimension(:), intent(in) :: ii
    type(parse_node_t), intent(out), pointer :: pn
    type(string_t), intent(out), optional :: key
    type(syntax_rule_t), pointer :: rule
    integer :: k
    if (builder%use_pn_sub) then
       pn => builder%pn_sub(ii(1))%ptr
    else
       pn => builder%pn%get_sub_ptr (ii(1))
    end if
    do k = 2, size (ii)
       if (associated (pn)) then
          pn => pn%get_sub_ptr (ii(k))
       else
          exit
       end if
    end do
    if (present (key)) then
       if (associated (pn)) then
          rule => pn%get_rule_ptr ()
          key = rule%get_key ()
       else
          key = ""
       end if
    end if
  end subroutine expr_get_pn_array
          
  subroutine setup_expr (builder)
    class(pnd_expr_t), intent(inout) :: builder

   !   print *
   !   print *, "setup master expr"
   !   call builder%pn%write ()
    if (builder%n_terms < builder%n_sub .or. builder%minus_sign) then
       call create_fake_expr_node ()
       builder%use_pn_sub = .true.
    end if
  contains
    subroutine create_fake_expr_node ()
      integer :: i_start, k
      allocate (builder%pn_sub (builder%n_terms))
      allocate (builder%new_pn (builder%n_terms), source=.false.)
      i_start = 1
      ! print *, "create expr"
      do k = 1, builder%n_terms
         call create_fake_subexpr_node (builder%pn_sub(k)%ptr, k, i_start)
         !    print *, "subexpr", k
         !    call builder%pn_sub(k)%ptr%write ()
         ! print *, "k = ", k
         ! call builder%pn_sub(k)%ptr%write ()
      end do
    end subroutine create_fake_expr_node
    subroutine create_fake_subexpr_node (pn, k, i_start)
      type(parse_node_t), pointer, intent(inout) :: pn
      integer, intent(in) :: k
      integer, intent(inout) :: i_start
      logical, dimension(:), allocatable :: mask
      integer, dimension(:), allocatable :: ii
      type(parse_node_t), pointer :: pn_src, pn_sub
      integer :: i, j, m
      allocate (mask (builder%n_sub), source = .false.)
      mask(i_start) = .true.
      do j = i_start + 1, builder%n_sub
         if (.not. allocated (builder%is_master_op)) exit
         if (builder%is_master_op(j)) exit
         mask(j) = .true.
      end do
      allocate (ii (count (mask)))
      ii = pack ([(i, i=1, builder%n_sub)], mask)
      ! print *, "ii = ", ii
      if (size (ii) == 1) then
         call builder%get_pn (ii(1), pn)
      else
         !  call builder%pn%write ()
         call parse_node_create_branch (pn, builder%pn%get_rule_ptr ())
         if (ii(1) == 1) then
            call builder%get_pn (1, pn_src)
         else
            call builder%get_pn ([ii(1),2], pn_src)
         end if
         !   print *, "pn_src = "
         !   call pn_src%write ()
         call pn_src%copy (pn_sub)   ! shallow copy
         call pn%append_sub (pn_sub)
         do m = 2, size (ii)
            call builder%get_pn (ii(m), pn_src)
            call pn_src%copy (pn_sub)   ! shallow copy
            call pn%append_sub (pn_sub)
         end do
         call parse_node_freeze_branch (pn)
         if (builder%minus_sign .and. ii(1) == 1) then
            call insert_signed_expr_node (pn)
         end if
         builder%new_pn(k) = .true.
      end if
      if (j > builder%n_sub)  return
      i_start = j
    end subroutine create_fake_subexpr_node
    subroutine insert_signed_expr_node (pn)
      type(parse_node_t), intent(inout), pointer :: pn
      type(parse_node_t), pointer :: pn_signed, pn_sign
      allocate (pn_signed)
      call parse_node_create_branch (pn_signed, builder%rule)
      allocate (pn_sign)
      call parse_node_create_key (pn_sign, builder%rule_sign)
      call pn_signed%append_sub (pn_sign)
      call pn_signed%append_sub (pn)
      call parse_node_freeze_branch (pn_signed)
      pn => pn_signed
    end subroutine insert_signed_expr_node
  end subroutine setup_expr
    
  recursive subroutine decode_expr (builder, code, success)
    class(pnd_expr_t), intent(inout) :: builder
    type(code_t), intent(out) :: code
    logical, intent(out) :: success
    integer :: i
    success = .true.
    select case (builder%position%part)
    case (POS_HERE)
       call builder%setup ()
       call builder%create_node_code (code)
       call builder%next_part (POS_MEMBER)
       call builder%goto (1)
          !   print *, "goto 1"
    case (POS_MEMBER)
       i = builder%position%i
       if (i < builder%n_terms) then
          call builder%next_index ()
          if (builder%use_pn_sub) then
             if (builder%new_pn(i+1)) then
                call builder%goto (i+1)
             else
                call builder%goto ([i+1,2])          
             end if
          else
             call builder%goto ([i+1,2])          
          end if
          !   print *, "goto next"
          !   call builder%pn_next%write ()
       else
          call builder%next_part (POS_CORE)
          call builder%stay ()
          !   print *, "goto core"
       end if
    case (POS_CORE)
       call builder%create_core_code (code)
       call builder%next_part (POS_NONE)
       call builder%done ()
    case default
       call builder%fail (success)
    end select
  end subroutine decode_expr
  
  function binary_priority (op) result (p)
    type(string_t), intent(in) :: op
    integer :: p
    select case (char (op))
    case (":");    p = PRIO_COLON
    case (",");    p = PRIO_COMMA
    case ("=>");   p = PRIO_ARROW
    case ("*");    p = PRIO_MULTIPLY
    case ("/");    p = PRIO_MULTIPLY
    case ("+");    p = PRIO_ADD
    case ("-");    p = PRIO_ADD
    case ("==");   p = PRIO_COMPARE
    case ("<>");   p = PRIO_COMPARE
    case ("<");    p = PRIO_COMPARE
    case (">");    p = PRIO_COMPARE
    case ("<=");   p = PRIO_COMPARE
    case (">=");   p = PRIO_COMPARE
    case ("and");  p = PRIO_AND
    case ("or");   p = PRIO_OR
    case default
       p = 0
    end select
  end function binary_priority
  
  function unary_priority (op) result (p)
    type(string_t), intent(in) :: op
    integer :: p
    select case (char (op))
    case ("+");    p = PRIO_MINUS
    case ("-");    p = PRIO_MINUS
    case ("not");  p = PRIO_NOT
    case default
       p = 0
    end select
  end function unary_priority
  
  recursive subroutine decode_positive_wrapper (builder, code, success)
    class(pnd_positive_wrapper_t), intent(inout) :: builder
    type(code_t), intent(out) :: code
    logical, intent(out) :: success
    success = .true.
    select case (builder%position%part)
    case (POS_HERE)
       call builder%next_part (POS_NONE)
       call builder%goto ()
    case default
       call builder%fail (success)
    end select
  end subroutine decode_positive_wrapper
  
  recursive subroutine decode_negative_wrapper (builder, code, success)
    class(pnd_negative_wrapper_t), intent(inout) :: builder
    type(code_t), intent(out) :: code
    logical, intent(out) :: success
    success = .true.
    select case (builder%position%part)
    case (POS_HERE)
       call builder%create_node_code (code)
       call builder%next_part (POS_CORE)
       call builder%goto ()
    case (POS_CORE)
       call builder%create_core_code (code)
       call builder%next_part (POS_NONE)
       call builder%done ()
    case default
       call builder%fail (success)
    end select
  end subroutine decode_negative_wrapper
  
  subroutine create_node_code_positive_wrapper (builder, code)
    class(pnd_positive_wrapper_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    call msg_bug ("Sindarin parser: positive-sign code requested")
  end subroutine create_node_code_positive_wrapper
  
  subroutine create_node_code_negative_wrapper (builder, code)
    class(pnd_negative_wrapper_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    call code%set (CAT_COMPOSITE, &
         [builder%get_index ("minus"), MODE_CONSTANT, &
         0, 1, 1])
  end subroutine create_node_code_negative_wrapper
  
  subroutine create_node_code_container_expr (builder, code)
    class(pnd_container_expr_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    integer :: n_terms
    n_terms = builder%n_terms
    call code%set (CAT_COMPOSITE, &
         [builder%get_index ("container"), MODE_CONSTANT, &
         0, n_terms, n_terms])
    call code%create_integer_val ([separator_code (builder%key(2))])
  end subroutine create_node_code_container_expr
    
  function separator_code (key) result (code)
    type(string_t), intent(in) :: key
    integer :: code
    select case (char (key))
    case (":");  code = CT_TUPLE
    case (",");  code = CT_LIST
    case ("=>"); code = CT_SEQUENCE
    case default
       code = 0
    end select
  end function separator_code

  subroutine create_core_code_container_expr (builder, code)
    class(pnd_container_expr_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    call code%set (CAT_REF_ARRAY, [builder%n_terms])
  end subroutine create_core_code_container_expr
    
  subroutine create_node_code_logical_expr (builder, code)
    class(pnd_logical_expr_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    integer :: n_terms
    n_terms = builder%n_terms
    select case (char (builder%key (2)))
    case ("and")
       call code%set (CAT_COMPOSITE, &
            [builder%get_index ("and"), MODE_CONSTANT, &
            0, n_terms, n_terms])
    case ("or")
       call code%set (CAT_COMPOSITE, &
            [builder%get_index ("or"), MODE_CONSTANT, &
            0, n_terms, n_terms])
    end select
  end subroutine create_node_code_logical_expr
    
  recursive subroutine decode_logical_not (builder, code, success)
    class(pnd_logical_not_t), intent(inout) :: builder
    type(code_t), intent(out) :: code
    logical, intent(out) :: success
    success = .true.
    select case (builder%position%part)
    case (POS_HERE)
       call builder%create_node_code (code)
       call builder%next_part (POS_MEMBER)
       call builder%stay ()
    case (POS_MEMBER)
       call builder%next_part (POS_CORE)
       call builder%goto (2)
    case (POS_CORE)
       call builder%create_core_code (code)
       call builder%done ()
    case default
       call builder%fail (success)
    end select
  end subroutine decode_logical_not
  
  subroutine create_node_code_logical_not (builder, code)
    class(pnd_logical_not_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    call code%set (CAT_COMPOSITE, &
         [builder%get_index ("not"), MODE_CONSTANT, &
         0, 1, 1])
  end subroutine create_node_code_logical_not
    
  recursive subroutine decode_logical_literal (builder, code, success)
    class(pnd_logical_literal_t), intent(inout) :: builder
    type(code_t), intent(out) :: code
    logical, intent(out) :: success
    success = .true.
    select case (builder%position%part)
    case (POS_HERE)
       call builder%create_node_code (code)
       call builder%next_part (POS_CORE)
       call builder%stay ()
    case (POS_CORE)
       call builder%create_core_code (code)
       call builder%done ()
    case default
       call builder%fail (success)
    end select
  end subroutine decode_logical_literal
  
  subroutine create_node_code_logical_literal (builder, code)
    class(pnd_logical_literal_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    call code%set (CAT_COMPOSITE, &
         [builder%get_index ("logical"), MODE_CONSTANT])
  end subroutine create_node_code_logical_literal
    
  subroutine create_core_code_logical_literal (builder, code)
    class(pnd_logical_literal_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    call code%set (CAT_VALUE)
    call code%create_logical_val ([builder%value])
  end subroutine create_core_code_logical_literal
    
  subroutine setup_comparison_expr (builder)
    class(pnd_comparison_expr_t), intent(inout) :: builder
    builder%n_terms = builder%get_n_terms ()
  end subroutine setup_comparison_expr
    
  recursive subroutine decode_comparison_expr (builder, code, success)
    class(pnd_comparison_expr_t), intent(inout) :: builder
    type(code_t), intent(out) :: code
    logical, intent(out) :: success
    integer :: i, n_terms
    success = .true.
    select case (builder%position%part)
    case (POS_HERE)
       call builder%setup ()
       n_terms = builder%n_terms
       if (n_terms == 1) then
          call builder%next_part (POS_NONE)
          call builder%goto (1)
       else
          call builder%create_node_code (code)
          call builder%next_part (POS_MEMBER)
          call builder%goto (1)
       end if
    case (POS_MEMBER)
       i = builder%position%i
       if (i < builder%n_terms) then
          call builder%next_index ()
          call builder%goto ([i+1,2])
       else
          call builder%next_part (POS_CORE)
          call builder%stay ()
       end if
    case (POS_CORE)
       call builder%create_core_code (code)
       call builder%done ()
    case default
       call builder%fail (success)
    end select
  end subroutine decode_comparison_expr
  
  subroutine create_node_code_comparison_expr (builder, code)
    class(pnd_comparison_expr_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    integer, dimension(:), allocatable :: cmp_code
    integer :: n_terms, i
    n_terms = builder%n_terms
    call code%set (CAT_COMPOSITE, &
         [builder%get_index ("compare"), MODE_CONSTANT, &
         0, n_terms, n_terms])
    allocate (cmp_code (builder%n_terms), source = CMP_NONE)
    do i = 2, builder%n_terms
       select case (char (builder%get_key ([i,1])))
       case ("==");  cmp_code(i) = CMP_EQ
       case ("<>");  cmp_code(i) = CMP_NE
       case ("<");   cmp_code(i) = CMP_LT
       case (">");   cmp_code(i) = CMP_GT
       case ("<=");  cmp_code(i) = CMP_LE
       case (">=");  cmp_code(i) = CMP_GE
       end select
    end do
    call code%create_integer_val (cmp_code)
  end subroutine create_node_code_comparison_expr
    
  subroutine create_node_code_integer_expr (builder, code)
    class(pnd_integer_expr_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    logical, dimension(:), allocatable :: inverse
    integer :: n_terms, i
    n_terms = builder%n_terms
    select case (char (builder%key(2)))
    case ("*", "/")
       call code%set (CAT_COMPOSITE, &
            [builder%get_index ("multiply"), MODE_CONSTANT, &
            0, n_terms, n_terms])
    case ("+", "-")
       call code%set (CAT_COMPOSITE, &
            [builder%get_index ("add"), MODE_CONSTANT, &
            0, n_terms, n_terms])
    case default
       call msg_bug ("Sindarin: decode integer expr: illegal key")
    end select
    allocate (inverse (builder%n_terms), source = .false.)
    do i = 2, builder%n_terms
       select case (char (builder%get_key ([i,1])))
       case ("-", "/");  inverse(i) = .true.
       end select
    end do
    call code%create_logical_val (inverse)
  end subroutine create_node_code_integer_expr
    
  subroutine setup_positive_integer (builder)
    class(pnd_positive_integer_t), intent(inout) :: builder
    builder%value = builder%get_integer ()
  end subroutine setup_positive_integer
  
  subroutine setup_negative_integer (builder)
    class(pnd_negative_integer_t), intent(inout) :: builder
    builder%value = - builder%get_integer ()
  end subroutine setup_negative_integer
  
  recursive subroutine decode_integer_literal (builder, code, success)
    class(pnd_integer_literal_t), intent(inout) :: builder
    type(code_t), intent(out) :: code
    logical, intent(out) :: success
    success = .true.
    select case (builder%position%part)
    case (POS_HERE)
       call builder%setup ()
       call builder%create_node_code (code)
       call builder%next_part (POS_CORE)
       call builder%stay ()
    case (POS_CORE)
       call builder%create_core_code (code)
       call code%set (CAT_VALUE)
       call code%create_integer_val ([builder%value])
       call builder%done ()
    case default
       call builder%fail (success)
    end select
  end subroutine decode_integer_literal
  
  subroutine create_node_code_integer_literal (builder, code)
    class(pnd_integer_literal_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    call code%set (CAT_COMPOSITE, &
         [builder%get_index ("integer"), MODE_CONSTANT])
  end subroutine create_node_code_integer_literal
    
  subroutine create_core_code_integer_literal (builder, code)
    class(pnd_integer_literal_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    call code%set (CAT_VALUE)
    call code%create_integer_val ([builder%value])
  end subroutine create_core_code_integer_literal
    
  subroutine setup_conditional_expr (builder)
    class(pnd_conditional_expr_t), intent(inout) :: builder
    integer :: i
    do i = 2, 3
       select case (char (builder%get_key (i)))
       case ("elsif_expr_part")
          builder%n_elsif = builder%get_n_terms (i)
       case ("else_expr_part")
          builder%has_else = .true.
       end select
    end do
    builder%n_branches = 2 + builder%n_elsif
    builder%n_terms = 2 * builder%n_branches - 1
  end subroutine setup_conditional_expr
  
  recursive subroutine decode_conditional_expr (builder, code, success)
    class(pnd_conditional_expr_t), intent(inout) :: builder
    type(code_t), intent(out) :: code
    logical, intent(out) :: success
    integer :: i
    success = .true.
    select case (builder%position%part)
    case (POS_HERE)
       call builder%setup ()
       call builder%create_node_code (code)
       call builder%next_part (POS_MEMBER)
       call builder%goto ([1,4])         ! if-value
    case (POS_MEMBER)
       i = builder%position%i
       if (i == 1) then
          call builder%next_index ()
          call builder%goto ([1,2])      ! if-condition
       else if (i < 2 + 2 * builder%n_elsif .and. mod (i, 2) == 0) then
          call builder%next_index ()
          call builder%goto ([2,i/2,4])  ! elsif-value #i/2
       else if (i < 2 + 2 * builder%n_elsif .and. mod (i, 2) == 1) then
          call builder%next_index ()
          call builder%goto ([2,i/2,2])  ! elsif-condition #i/2
       else if (builder%has_else) then
          call builder%next_part (POS_CORE)
          if (builder%n_elsif > 0) then
             call builder%goto ([3,1,2]) ! else-value
          else
             call builder%goto ([2,1,2])
          end if
       else if (i < builder%n_terms) then
          call builder%create_else_code (code)
          call builder%next_index ()     ! fake else-value
          call builder%stay ()
       else
          call code%set (CAT_VALUE)      ! fake else-value core
          call builder%next_part (POS_CORE)
          call builder%stay ()
       end if
    case (POS_CORE)
       call builder%create_core_code (code)
       call builder%done ()
    case default
       call builder%fail (success)
    end select
  end subroutine decode_conditional_expr
  
  subroutine create_node_code_conditional_expr (builder, code)
    class(pnd_conditional_expr_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    integer :: n_terms
    n_terms = builder%n_terms
    call code%set (CAT_COMPOSITE, &
         [builder%get_index ("conditional_expr"), MODE_CONSTANT, &
         0, n_terms, n_terms])
  end subroutine create_node_code_conditional_expr
    
  subroutine create_else_code_conditional_expr (builder, code)
    class(pnd_conditional_expr_t), intent(in) :: builder
    type(code_t), intent(out) :: code
    call code%set (CAT_COMPOSITE, &
         [builder%get_index ("integer"), MODE_CONSTANT])
  end subroutine create_else_code_conditional_expr
    

end module sindarin_parser
