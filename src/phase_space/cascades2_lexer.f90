! WHIZARD 2.6.2 Dec 13 2017
!
! Copyright (C) 1999-2017 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     cf. main AUTHORS file
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

module cascades2_lexer

  use kinds, only: default
  use kinds, only: TC, i8

  implicit none
  private

  public :: dag_token_t
  public :: dag_string_t
  public :: dag_chain_t
  public :: assignment (=)
  public :: operator (//)
  public :: operator (==)
  public :: operator (/=)
  public :: char

  character(len=1), parameter, public :: BACKSLASH_CHAR = "\\"
  character(len=1), parameter :: BLANC_CHAR = " "
  integer, parameter, public :: NEW_LINE_TK = -2
  integer, parameter :: BLANC_SPACE_TK = -1
  integer, parameter :: EMPTY_TK = 0
  integer, parameter, public :: NODE_TK = 1
  integer, parameter, public :: DAG_NODE_TK = 2
  integer, parameter, public :: DAG_OPTIONS_TK = 3
  integer, parameter, public :: DAG_COMBINATION_TK = 4
  integer, parameter, public :: COLON_TK = 11
  integer, parameter, public :: COMMA_TK = 12
  integer, parameter, public :: VERTICAL_BAR_TK = 13
  integer, parameter, public :: OPEN_PAR_TK = 21
  integer, parameter, public :: CLOSED_PAR_TK = 22
  integer, parameter, public :: OPEN_CURLY_TK = 31
  integer, parameter, public :: CLOSED_CURLY_TK = 32


  type :: dag_token_t
     integer :: type = EMPTY_TK
     integer :: char_len = 0
     integer :: bincode = 0
     character (len=1), dimension (:), allocatable :: particle_name
     integer :: index = 0
   contains
       procedure :: init_dag_object_token => dag_token_init_dag_object_token
  end type dag_token_t

  type :: dag_string_t
     integer :: char_len = 0
     type (dag_token_t), dimension(:), allocatable :: t
     type (dag_string_t), pointer :: next => null ()
   contains
       procedure :: clean => dag_string_clean
       procedure :: update_char_len => dag_string_update_char_len
       procedure :: final => dag_string_final
  end type dag_string_t

  type :: dag_chain_t
     integer :: char_len = 0
     integer :: t_size = 0
     type (dag_string_t), pointer :: first => null ()
     type (dag_string_t), pointer :: last => null ()
   contains
       procedure :: append => dag_chain_append_string
       procedure :: compress => dag_chain_compress
       procedure :: final => dag_chain_final
  end type dag_chain_t


  interface assignment (=)
     module procedure dag_token_assign_from_char_string
     module procedure dag_token_assign_from_dag_token
     module procedure dag_string_assign_from_dag_token
     module procedure dag_string_assign_from_char_string
     module procedure dag_string_assign_from_dag_string
     module procedure dag_string_assign_from_dag_token_array
  end interface assignment (=)

  interface operator (//)
     module procedure concat_dag_token_dag_token
     module procedure concat_dag_string_dag_token
     module procedure concat_dag_token_dag_string
     module procedure concat_dag_string_dag_string
  end interface operator (//)

  interface operator (==)
     module procedure dag_token_eq_dag_token
     module procedure dag_string_eq_dag_string
     module procedure dag_token_eq_dag_string
     module procedure dag_string_eq_dag_token
     module procedure dag_token_eq_char_string
     module procedure char_string_eq_dag_token
     module procedure dag_string_eq_char_string
     module procedure char_string_eq_dag_string
  end interface operator (==)

  interface operator (/=)
     module procedure dag_token_ne_dag_token
     module procedure dag_string_ne_dag_string
     module procedure dag_token_ne_dag_string
     module procedure dag_string_ne_dag_token
     module procedure dag_token_ne_char_string
     module procedure char_string_ne_dag_token
     module procedure dag_string_ne_char_string
     module procedure char_string_ne_dag_string
  end interface operator (/=)

  interface char
     module procedure char_dag_token
     module procedure char_dag_string
  end interface char


contains

  subroutine dag_token_init_dag_object_token (dag_token, type, index)
    class (dag_token_t), intent (out) :: dag_token
    integer, intent (in) :: index
    integer :: type
    dag_token%type = type
    dag_token%char_len = integer_n_dec_digits (index) + 3
    dag_token%index = index
  contains
    function integer_n_dec_digits (number) result (n_digits)
      integer, intent (in) :: number
      integer :: n_digits
      integer :: div_number
      n_digits = 0
      div_number = number
      do
         div_number = div_number / 10
         n_digits = n_digits + 1
         if (div_number == 0) exit
      enddo
    end function integer_n_dec_digits
  end subroutine dag_token_init_dag_object_token

  elemental subroutine dag_token_assign_from_char_string (dag_token, char_string)
    type (dag_token_t), intent (out) :: dag_token
    character (len=*), intent (in) :: char_string
    integer :: i, j
    logical :: set_bincode
    integer :: bit_pos
    character (len=10) :: index_char
    dag_token%char_len = len (char_string)
    if (dag_token%char_len == 1) then
       select case (char_string(1:1))
       case (BACKSLASH_CHAR)
          dag_token%type = NEW_LINE_TK
       case (" ")
          dag_token%type = BLANC_SPACE_TK
       case (":")
          dag_token%type = COLON_TK
       case (",")
          dag_token%type = COMMA_TK
       case ("|")
          dag_token%type = VERTICAL_BAR_TK
       case ("(")
          dag_token%type = OPEN_PAR_TK
       case (")")
          dag_token%type = CLOSED_PAR_TK
       case ("{")
          dag_token%type = OPEN_CURLY_TK
       case ("}")
          dag_token%type = CLOSED_CURLY_TK
       end select
    else if (char_string(1:1) == "<") then
       select case (char_string(2:2))
          case ("N")
             dag_token%type = DAG_NODE_TK
          case ("O")
             dag_token%type = DAG_OPTIONS_TK
          case ("C")
             dag_token%type = DAG_COMBINATION_TK
       end select
       read(char_string(3:dag_token%char_len-1), fmt="(I10)") dag_token%index
    else
       dag_token%bincode = 0
       set_bincode = .false.
       do i=1, dag_token%char_len
          select case (char_string(i:i))
          case ("[")
             dag_token%type = NODE_TK
             if (i > 1) then
                allocate (dag_token%particle_name(i-1))
                do j = 1, i - 1
                   dag_token%particle_name(j) = char_string(j:j)
                enddo
             endif
             set_bincode = .true.
          case ("]")
             set_bincode = .false.
          case default
             dag_token%type = NODE_TK
             if (set_bincode) then
                select case (char_string(i:i))
                case ("1", "2", "3", "4", "5", "6", "7", "8", "9")
                   read (char_string(i:i), fmt="(I1)") bit_pos
                case ("A")
                   bit_pos = 10
                case ("B")
                   bit_pos = 11
                case ("C")
                   bit_pos = 12
                end select
                dag_token%bincode = ibset(dag_token%bincode, bit_pos - 1)
             endif
          end select
          if (dag_token%type /= NODE_TK) exit
       enddo
    endif
  end subroutine dag_token_assign_from_char_string

  elemental subroutine dag_token_assign_from_dag_token (token_out, token_in)
    type (dag_token_t), intent (out) :: token_out
    type (dag_token_t), intent (in) :: token_in
    token_out%type = token_in%type
    token_out%char_len = token_in%char_len
    token_out%bincode = token_in%bincode
    if (allocated (token_in%particle_name)) then
       allocate (token_out%particle_name(size(token_in%particle_name)))
       token_out%particle_name = token_in%particle_name
    endif
    token_out%index = token_in%index
  end subroutine dag_token_assign_from_dag_token

  elemental subroutine dag_string_assign_from_dag_token (dag_string, dag_token)
    type (dag_string_t), intent (out) :: dag_string
    type (dag_token_t), intent (in) :: dag_token
    allocate (dag_string%t(1))
    dag_string%t(1) = dag_token
    dag_string%char_len = dag_token%char_len
  end subroutine dag_string_assign_from_dag_token

  subroutine dag_string_assign_from_dag_token_array (dag_string, dag_token)
    type (dag_string_t), intent (out) :: dag_string
    type (dag_token_t), dimension(:), intent (in) :: dag_token
    allocate (dag_string%t(size(dag_token)))
    dag_string%t = dag_token
    dag_string%char_len = sum(dag_token%char_len)
  end subroutine dag_string_assign_from_dag_token_array

  elemental subroutine dag_string_assign_from_char_string (dag_string, char_string)
    type (dag_string_t), intent (out) :: dag_string
    character (len=*), intent (in) :: char_string
    type (dag_token_t), dimension(:), allocatable :: token
    integer :: token_pos
    integer :: i
    character (len=len(char_string)) :: node_char
    integer :: node_char_len
    node_char = ""
    dag_string%char_len = len (char_string)
    if (dag_string%char_len > 0) then
       allocate (token(dag_string%char_len))
       token_pos = 0
       node_char_len = 0
       do i=1, dag_string%char_len
          select case (char_string(i:i))
          case (BACKSLASH_CHAR, " ", ":", ",", "|", "(", ")", "{", "}")
             if (node_char_len > 0) then
                token_pos = token_pos + 1
                token(token_pos) = node_char(:node_char_len)
                node_char_len = 0
             endif
             token_pos = token_pos + 1
             token(token_pos) = char_string(i:i)
          case default
             node_char_len = node_char_len + 1
             node_char(node_char_len:node_char_len) = char_string(i:i)
          end select
       enddo
       if (node_char_len > 0) then
          token_pos = token_pos + 1
          token(token_pos) = node_char(:node_char_len)
       endif
       if (token_pos > 0) then
          allocate (dag_string%t(token_pos))
          dag_string%t = token(:token_pos)
          deallocate (token)
       endif
    end if
  end subroutine dag_string_assign_from_char_string

  elemental subroutine dag_string_assign_from_dag_string (string_out, string_in)
    type (dag_string_t), intent (out) :: string_out
    type (dag_string_t), intent (in) :: string_in
    if (allocated (string_in%t)) then
       allocate (string_out%t (size(string_in%t)))
       string_out%t = string_in%t
    endif
    string_out%char_len = string_in%char_len
  end subroutine dag_string_assign_from_dag_string

  function concat_dag_token_dag_token (token1, token2) result (res_string)
    type (dag_token_t), intent (in) :: token1, token2
    type (dag_string_t) :: res_string
    if (token1%type == EMPTY_TK) then
       res_string = token2
    else if (token2%type == EMPTY_TK) then
       res_string = token1
    else
       allocate (res_string%t(2))
       res_string%t(1) = token1
       res_string%t(2) = token2
       res_string%char_len = token1%char_len + token2%char_len
    endif
  end function concat_dag_token_dag_token

  function concat_dag_string_dag_token (dag_string, dag_token) result (res_string)
    type (dag_string_t), intent (in) :: dag_string
    type (dag_token_t), intent (in) :: dag_token
    type (dag_string_t) :: res_string
    integer :: t_size
    if (dag_string%char_len == 0) then
       res_string = dag_token
    else if (dag_token%type == EMPTY_TK) then
       res_string = dag_string
    else
       t_size = size (dag_string%t)
       allocate (res_string%t(t_size+1))
       res_string%t(:t_size) = dag_string%t
       res_string%t(t_size+1) = dag_token
       res_string%char_len = dag_string%char_len + dag_token%char_len
    endif
  end function concat_dag_string_dag_token

  function concat_dag_token_dag_string (dag_token, dag_string) result (res_string)
    type (dag_token_t), intent (in) :: dag_token
    type (dag_string_t), intent (in) :: dag_string
    type (dag_string_t) :: res_string
    integer :: t_size
    if (dag_token%type == EMPTY_TK) then
       res_string = dag_string
    else if (dag_string%char_len == 0) then
       res_string = dag_token
    else
       t_size = size (dag_string%t)
       allocate (res_string%t(t_size+1))
       res_string%t(2:t_size+1) = dag_string%t
       res_string%t(1) = dag_token
       res_string%char_len = dag_token%char_len + dag_string%char_len
    endif
  end function concat_dag_token_dag_string

  function concat_dag_string_dag_string (string1, string2) result (res_string)
    type (dag_string_t), intent (in) :: string1, string2
    type (dag_string_t) :: res_string
    integer :: t1_size, t2_size, t_size
    if (string1%char_len == 0) then
       res_string = string2
    else if (string2%char_len == 0) then
       res_string = string1
    else
       t1_size = size (string1%t)
       t2_size = size (string2%t)
       t_size = t1_size + t2_size
       if (t_size > 0) then
          allocate (res_string%t(t_size))
          res_string%t(:t1_size) = string1%t
          res_string%t(t1_size+1:) = string2%t
          res_string%char_len = string1%char_len + string2%char_len
       endif
    endif
  end function concat_dag_string_dag_string

  elemental function dag_token_eq_dag_token (token1, token2) result (flag)
    type (dag_token_t), intent (in) :: token1, token2
    logical :: flag
    flag = (token1%type == token2%type) .and. &
         (token1%char_len == token2%char_len) .and. &
         (token1%bincode == token2%bincode) .and. &
         (token1%index == token2%index) .and. &
         (allocated (token1%particle_name) .eqv. allocated (token2%particle_name))
    if (flag) then
       if (allocated (token1%particle_name)) flag = all (token1%particle_name == token2%particle_name)
    endif
  end function dag_token_eq_dag_token

  elemental function dag_string_eq_dag_string (string1, string2) result (flag)
    type (dag_string_t), intent (in) :: string1, string2
    logical :: flag
    flag = (string1%char_len == string2%char_len) .and. &
         (allocated (string1%t) .eqv. allocated (string2%t))
    if (flag) then
       if (allocated (string1%t)) flag = all (string1%t == string2%t)
    endif
  end function dag_string_eq_dag_string

  elemental function dag_token_eq_dag_string (dag_token, dag_string) result (flag)
    type (dag_token_t), intent (in) :: dag_token
    type (dag_string_t), intent (in) :: dag_string
    logical :: flag
    flag = size (dag_string%t) == 1 .and. &
         dag_string%char_len == dag_token%char_len
    if (flag) flag = (dag_string%t(1) == dag_token)
  end function dag_token_eq_dag_string

  elemental function dag_string_eq_dag_token (dag_string, dag_token) result (flag)
    type (dag_token_t), intent (in) :: dag_token
    type (dag_string_t), intent (in) :: dag_string
    logical :: flag
    flag = (dag_token == dag_string)
  end function dag_string_eq_dag_token

  elemental function dag_token_eq_char_string (dag_token, char_string) result (flag)
    type (dag_token_t), intent (in) :: dag_token
    character (len=*), intent (in) :: char_string
    logical :: flag
    flag = (char (dag_token) == char_string)
  end function dag_token_eq_char_string

  elemental function char_string_eq_dag_token (char_string, dag_token) result (flag)
    type (dag_token_t), intent (in) :: dag_token
    character (len=*), intent (in) :: char_string
    logical :: flag
    flag = (char (dag_token) == char_string)
  end function char_string_eq_dag_token

  elemental function dag_string_eq_char_string (dag_string, char_string) result (flag)
    type (dag_string_t), intent (in) :: dag_string
    character (len=*), intent (in) :: char_string
    logical :: flag
    flag = (char (dag_string) == char_string)
  end function dag_string_eq_char_string

  elemental function char_string_eq_dag_string (char_string, dag_string) result (flag)
    type (dag_string_t), intent (in) :: dag_string
    character (len=*), intent (in) :: char_string
    logical :: flag
    flag = (char (dag_string) == char_string)
  end function char_string_eq_dag_string

  elemental function dag_token_ne_dag_token (token1, token2) result (flag)
    type (dag_token_t), intent (in) :: token1, token2
    logical :: flag
    flag = .not. (token1 == token2)
  end function dag_token_ne_dag_token

  elemental function dag_string_ne_dag_string (string1, string2) result (flag)
    type (dag_string_t), intent (in) :: string1, string2
    logical :: flag
    flag = .not. (string1 == string2)
  end function dag_string_ne_dag_string

  elemental function dag_token_ne_dag_string (dag_token, dag_string) result (flag)
    type (dag_token_t), intent (in) :: dag_token
    type (dag_string_t), intent (in) :: dag_string
    logical :: flag
    flag = .not. (dag_token == dag_string)
  end function dag_token_ne_dag_string

  elemental function dag_string_ne_dag_token (dag_string, dag_token) result (flag)
    type (dag_token_t), intent (in) :: dag_token
    type (dag_string_t), intent (in) :: dag_string
    logical :: flag
    flag = .not. (dag_string == dag_token)
  end function dag_string_ne_dag_token

  elemental function dag_token_ne_char_string (dag_token, char_string) result (flag)
    type (dag_token_t), intent (in) :: dag_token
    character (len=*), intent (in) :: char_string
    logical :: flag
    flag = .not. (dag_token == char_string)
  end function dag_token_ne_char_string

  elemental function char_string_ne_dag_token (char_string, dag_token) result (flag)
    type (dag_token_t), intent (in) :: dag_token
    character (len=*), intent (in) :: char_string
    logical :: flag
    flag = .not. (char_string == dag_token)
  end function char_string_ne_dag_token

  elemental function dag_string_ne_char_string (dag_string, char_string) result (flag)
    type (dag_string_t), intent (in) :: dag_string
    character (len=*), intent (in) :: char_string
    logical :: flag
    flag = .not. (dag_string == char_string)
  end function dag_string_ne_char_string

  elemental function char_string_ne_dag_string (char_string, dag_string) result (flag)
    type (dag_string_t), intent (in) :: dag_string
    character (len=*), intent (in) :: char_string
    logical :: flag
    flag = .not. (char_string == dag_string)
  end function char_string_ne_dag_string

  pure function char_dag_token (dag_token) result (char_string)
    type (dag_token_t), intent (in) :: dag_token
    character (dag_token%char_len) :: char_string
    integer :: i
    integer :: name_len
    integer :: bc_pos
    integer :: n_digits
    character (len=9) :: fmt_spec
    select case (dag_token%type)
    case (EMPTY_TK)
       char_string = ""
    case (NEW_LINE_TK)
       char_string = BACKSLASH_CHAR
    case (BLANC_SPACE_TK)
       char_string = " "
    case (COLON_TK)
       char_string = ":"
    case (COMMA_TK)
       char_string = ","
    case (VERTICAL_BAR_TK)
       char_string = "|"
    case (OPEN_PAR_TK)
       char_string = "("
    case (CLOSED_PAR_TK)
       char_string = ")"
    case (OPEN_CURLY_TK)
       char_string = "{"
    case (CLOSED_CURLY_TK)
       char_string = "}"
    case (DAG_NODE_TK, DAG_OPTIONS_TK, DAG_COMBINATION_TK)
       n_digits = dag_token%char_len - 3
       fmt_spec = ""
       if (n_digits > 9) then
          write (fmt_spec, fmt="(A,I2,A)") "(A,I", n_digits, ",A)"
       else
          write (fmt_spec, fmt="(A,I1,A)") "(A,I", n_digits, ",A)"
       endif
       select case (dag_token%type)
          case (DAG_NODE_TK)
             write (char_string, fmt=fmt_spec) "<N", dag_token%index, ">"
          case (DAG_OPTIONS_TK)
             write (char_string, fmt=fmt_spec) "<O", dag_token%index, ">"
          case (DAG_COMBINATION_TK)
             write (char_string, fmt=fmt_spec) "<C", dag_token%index, ">"
          end select
    case (NODE_TK)
       name_len = size (dag_token%particle_name)
       do i=1, name_len
          char_string(i:i) = dag_token%particle_name(i)
       enddo
       bc_pos = name_len + 1
       char_string(bc_pos:bc_pos) = "["
       do i=0, bit_size (dag_token%bincode) - 1
          if (btest (dag_token%bincode, i)) then
             bc_pos = bc_pos + 1
             select case (i)
             case (0, 1, 2, 3, 4, 5, 6, 7, 8)
                write (char_string(bc_pos:bc_pos), fmt="(I1)") i + 1
             case (9)
                write (char_string(bc_pos:bc_pos), fmt="(A1)") "A"
             case (10)
                write (char_string(bc_pos:bc_pos), fmt="(A1)") "B"
             case (11)
                write (char_string(bc_pos:bc_pos), fmt="(A1)") "C"
             end select
             bc_pos = bc_pos + 1
             if (bc_pos == dag_token%char_len) then
                write (char_string(bc_pos:bc_pos), fmt="(A1)") "]"
                return
             else
                write (char_string(bc_pos:bc_pos), fmt="(A1)") "/"
             endif
          endif
       enddo
    end select
  end function char_dag_token

  pure function char_dag_string (dag_string) result (char_string)
    type (dag_string_t), intent (in) :: dag_string
    character (dag_string%char_len) :: char_string
    integer :: pos
    integer :: i
    char_string = ""
    pos = 0
    do i=1, size(dag_string%t)
       char_string(pos+1:pos+dag_string%t(i)%char_len) = char (dag_string%t(i))
       pos = pos + dag_string%t(i)%char_len
    enddo
  end function char_dag_string

  subroutine dag_string_clean (dag_string)
    class (dag_string_t), intent (inout) :: dag_string
    type (dag_token_t), dimension(:), allocatable :: tmp_token
    integer :: n_keep
    integer :: i
    n_keep = 0
    dag_string%char_len = 0
    allocate (tmp_token (size(dag_string%t)))
    do i=1, size (dag_string%t)
       select case (dag_string%t(i)%type)
       case(NEW_LINE_TK, BLANC_SPACE_TK, EMPTY_TK)
       case default
          n_keep = n_keep + 1
          tmp_token(n_keep) = dag_string%t(i)
          dag_string%char_len = dag_string%char_len + dag_string%t(i)%char_len
       end select
    enddo
    deallocate (dag_string%t)
    allocate (dag_string%t(n_keep))
    dag_string%t = tmp_token(:n_keep)
  end subroutine dag_string_clean

  subroutine dag_string_update_char_len (dag_string)
    class (dag_string_t), intent (inout) :: dag_string
    integer :: char_len
    integer :: i
    char_len = 0
    if (allocated (dag_string%t)) then
       do i=1, size (dag_string%t)
          char_len = char_len + dag_string%t(i)%char_len
       enddo
    endif
    dag_string%char_len = char_len
  end subroutine dag_string_update_char_len

  subroutine dag_chain_append_string (dag_chain, char_string)
    class (dag_chain_t), intent (inout) :: dag_chain
    character (len=*), intent (in) :: char_string
    if (.not. associated (dag_chain%first)) then
       allocate (dag_chain%first)
       dag_chain%last => dag_chain%first
    else
       allocate (dag_chain%last%next)
       dag_chain%last => dag_chain%last%next
    endif
    dag_chain%last = char_string
    dag_chain%char_len = dag_chain%char_len + dag_chain%last%char_len
    dag_chain%t_size = dag_chain%t_size + size (dag_chain%last%t)
  end subroutine dag_chain_append_string

  subroutine dag_chain_compress (dag_chain)
    class (dag_chain_t), intent (inout) :: dag_chain
    type (dag_string_t), pointer :: current
    type (dag_string_t), pointer :: remove
    integer :: filled_t
    current => dag_chain%first
    dag_chain%first => null ()
    allocate (dag_chain%first)
    dag_chain%last => dag_chain%first
    dag_chain%first%char_len = dag_chain%char_len
    allocate (dag_chain%first%t (dag_chain%t_size))
    filled_t = 0
    do while (associated (current))
       dag_chain%first%t(filled_t+1:filled_t+size(current%t)) = current%t
       filled_t = filled_t + size (current%t)
       remove => current
       current => current%next
       deallocate (remove)
    enddo
  end subroutine dag_chain_compress

  subroutine dag_string_final (dag_string)
    class (dag_string_t), intent (inout) :: dag_string
    deallocate (dag_string%t)
    dag_string%next => null ()
  end subroutine dag_string_final

  subroutine dag_chain_final (dag_chain)
    class (dag_chain_t), intent (inout) :: dag_chain
    type (dag_string_t), pointer :: current
    current => dag_chain%first
    do while (associated (current))
       dag_chain%first => dag_chain%first%next
       call current%final ()
       deallocate (current)
       current => dag_chain%first
    enddo
    dag_chain%last => null ()
  end subroutine dag_chain_final


end module cascades2_lexer

