! WHIZARD 2.2.7 Aug 11 2015
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

module codes
  
  use iso_varying_string, string_t => varying_string
  use io_units

  implicit none
  private

  public :: position_t
  public :: code_t
  public :: val_logical_t
  public :: val_string_t
  public :: val_integer_t

  integer, parameter, public :: POS_NONE = -1
  integer, parameter, public :: POS_HERE = 0
  integer, parameter, public :: POS_ID = 1
  integer, parameter, public :: POS_CORE = 2
  integer, parameter, public :: POS_MEMBER = 3
  integer, parameter, public :: POS_PRIMER = 4

  integer, parameter, public :: CAT_TAG = 1
  integer, parameter, public :: CAT_VALUE = 2
  integer, parameter, public :: CAT_ID = 3

  integer, parameter, public :: CAT_REFERENCE = 99
  integer, parameter, public :: CAT_REF_ARRAY = 90

  integer, parameter, public :: CAT_COMPOSITE = 100

  integer, parameter, public :: CMP_NONE = 0
  integer, parameter, public :: CMP_EQ = 1
  integer, parameter, public :: CMP_NE = 2
  integer, parameter, public :: CMP_LT = 3
  integer, parameter, public :: CMP_GT = 4
  integer, parameter, public :: CMP_LE = 5
  integer, parameter, public :: CMP_GE = 6
  
  integer, parameter, public :: CT_TUPLE = 201
  integer, parameter, public :: CT_LIST = 202
  integer, parameter, public :: CT_SEQUENCE = 203

  integer, parameter, public :: CT_ADD = 210
  integer, parameter, public :: CT_SUB = 211
  integer, parameter, public :: CT_MUL = 212
  integer, parameter, public :: CT_DIV = 213
  integer, parameter, public :: CT_LIN = 214
  integer, parameter, public :: CT_LOG = 215
  
  integer, parameter, public :: PRIO_POWER = -71
  integer, parameter, public :: PRIO_MULTIPLY = -72
  integer, parameter, public :: PRIO_MINUS = -73 
  integer, parameter, public :: PRIO_ADD = -74
  
  integer, parameter, public :: PRIO_COMPARE = -95

  integer, parameter, public :: PRIO_NOT = -100
  integer, parameter, public :: PRIO_AND = -101
  integer, parameter, public :: PRIO_OR  = -102
  
  integer, parameter, public :: PRIO_COLON = -80
  integer, parameter, public :: PRIO_COMMA = -121
  integer, parameter, public :: PRIO_ARROW = -122
  
  integer, parameter, public :: PRIO_CONDITIONAL = -200

  integer, parameter :: NATT_MAX = 16
  integer, parameter :: NAME_LEN_MAX = 256
  
  integer, parameter, public :: VT_LOGICAL = 1
  integer, parameter, public :: VT_STRING = 2
  integer, parameter, public :: VT_INTEGER = 3

  type :: position_t
     integer :: part = POS_HERE
     integer :: i = 0
   contains
     procedure :: write => position_write
  end type position_t

  type, abstract :: val_t
     private
   contains
     procedure(val_get_type), deferred :: get_type
     procedure(val_init), deferred :: init
     procedure(val_get_nval), deferred :: get_nval
     procedure(val_read), deferred :: read
     procedure(val_write), deferred :: write
  end type val_t
  
  type :: code_t
     private
     integer :: cat = 0
     integer :: natt = 0
     integer, dimension(NATT_MAX) :: att = 0
     class(val_t), allocatable :: val
   contains
     procedure :: read => code_read
     procedure :: write => code_write
     procedure :: set
     procedure :: get_cat
     procedure :: get_n_att
     procedure :: get_att
     procedure :: create_logical_val
     procedure :: get_logical_array
     procedure :: get_logical
     procedure :: create_string_val
     procedure :: get_string_array
     procedure :: get_string
     procedure :: create_integer_val
     procedure :: get_integer_array
     procedure :: get_integer
     procedure, nopass :: create_val
  end type code_t
     
  type, extends (val_t) :: val_logical_t
     private
     logical, dimension(:), allocatable :: x
   contains
     procedure :: get_type => val_logical_get_type
     procedure :: init => val_logical_init
     procedure :: get_nval => val_logical_get_nval
     procedure :: read => val_logical_read
     procedure :: write => val_logical_write
  end type val_logical_t
     
  type, extends (val_t) :: val_string_t
     private
     type(string_t), dimension(:), allocatable :: x
   contains
     procedure :: get_type => val_string_get_type
     procedure :: init => val_string_init
     procedure :: get_nval => val_string_get_nval
     procedure :: read => val_string_read
     procedure :: write => val_string_write
  end type val_string_t
     
  type, extends (val_t) :: val_integer_t
     private
     integer, dimension(:), allocatable :: x
   contains
     procedure :: get_type => val_integer_get_type
     procedure :: init => val_integer_init
     procedure :: get_nval => val_integer_get_nval
     procedure :: read => val_integer_read
     procedure :: write => val_integer_write
  end type val_integer_t
     
  
  abstract interface
     function val_get_type (val) result (t)
       import
       class(val_t), intent(in) :: val
       integer :: t
     end function val_get_type
  end interface
  
  abstract interface
     subroutine val_init (val, nval)
       import
       class(val_t), intent(out) :: val
       integer, intent(in) :: nval
     end subroutine val_init
  end interface
  
  abstract interface
     function val_get_nval (val) result (nval)
       import
       class(val_t), intent(in) :: val
       integer :: nval
     end function val_get_nval
  end interface
  
  abstract interface
     subroutine val_read (val, unit, i, iostat)
       import
       class(val_t), intent(inout) :: val
       integer, intent(in) :: unit, i
       integer, intent(out), optional :: iostat
     end subroutine val_read
  end interface
       
  abstract interface
     subroutine val_write (val, unit, i)
       import
       class(val_t), intent(in) :: val
       integer, intent(in) :: unit, i
     end subroutine val_write
  end interface
       

contains
  
  subroutine position_write (position, unit)
    class(position_t), intent(in) :: position
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    select case (position%part)
    case (POS_NONE)
       write (u, "(1x,'-')", advance="no")
    case (POS_HERE)
       write (u, "(1x,'H')", advance="no")
    case (POS_ID)
       write (u, "(1x,'I')", advance="no")
    case (POS_CORE)
       write (u, "(1x,'C')", advance="no")
    case (POS_MEMBER)
       write (u, "(1x,'M',I0)", advance="no")  position%i
    case (POS_PRIMER)
       write (u, "(1x,'P',I0)", advance="no")  position%i
    case default
       write (u, "(1x,'?')", advance="no")
    end select
  end subroutine position_write

  subroutine code_read (code, unit, iostat)
    class(code_t), intent(out) :: code
    integer, intent(in) :: unit
    integer, intent(out), optional :: iostat
    logical :: err
    integer :: cat, vt, nval, natt, i
    err = .false.
    if (present (iostat)) then
       read (unit, *, iostat=iostat) &
            cat, vt, nval, natt, (code%att(i), i = 1, natt)
       call check;  if (err)  return
    else
       read (unit, *) &
            cat, vt, nval, natt, (code%att(i), i = 1, natt)
    end if
    code%cat = cat
    code%natt = natt
    if (vt > 0) then
       call code%create_val (code%val, vt, nval)
       do i = 1, nval
          call code%val%read (unit, i, iostat=iostat)
          if (present (iostat))  call check;  if (err)  return
       end do
    end if
  contains
    subroutine check
      err = iostat /= 0
    end subroutine check
  end subroutine code_read
    
  subroutine code_write (code, unit, iostat, verbose)
    class(code_t), intent(in) :: code
    integer, intent(in), optional :: unit
    integer, intent(out), optional :: iostat
    logical, intent(in), optional :: verbose
    logical :: verb
    integer :: natt, nval, i, u
    u = given_output_unit (unit)
    verb = .false.; if (present (verbose))  verb = verbose
    if (verb)  write (u, "(1x,A,1x)", advance="no")  "c="
    write (u, "(I0)", advance="no")  code%cat
    if (verb)  write (u, "(2x,A)", advance="no")  "vt="
    if (allocated (code%val)) then
       write (u, "(1x,I0)", advance="no")  code%val%get_type ()
       nval = code%val%get_nval ()
    else
       write (u, "(1x,I0)", advance="no")  0
       nval = 0
    end if
    if (verb)  write (u, "(2x,A)", advance="no")  "nv="
    write (u, "(1x,I0)", advance="no")  nval
    natt = code%natt
    if (.not. verb)  write (u, "(1x,I0)", advance="no")  natt
    if (natt > 0) then
       if (verb)  write (u, "(2x,A)", advance="no")  "att="
       write (u, "(*(1x,I0,:))")  code%att(1:natt)
    else
       write (u, *)
    end if
    do i = 1, nval
       if (verb)  write (u, "(5x)", advance="no")
       call code%val%write (u, i)
    end do
    if (present (iostat))  iostat = 0
  end subroutine code_write
    
  subroutine set (code, cat, att)
    class(code_t), intent(inout) :: code
    integer, intent(in) :: cat
    integer, dimension(:), optional, intent(in) :: att
    code%cat = cat
    if (present (att)) then
       code%natt = size (att)
       code%att(1:code%natt) = att
    else
       code%natt = 0
    end if
  end subroutine set
   
  function get_cat (code) result (cat)
    class(code_t), intent(in) :: code
    integer :: cat
    cat = code%cat
  end function get_cat

  function get_n_att (code) result (n)
    class(code_t), intent(in) :: code
    integer :: n
    n = code%natt
  end function get_n_att
  
  function get_att (code, i) result (att)
    class(code_t), intent(in) :: code
    integer, intent(in) :: i
    integer :: att
    if (i <= code%natt) then
       att = code%att(i)
    else
       att = 0
    end if
  end function get_att
  
  function val_logical_get_type (val) result (type)
    class(val_logical_t), intent(in) :: val
    integer :: type
    type = VT_LOGICAL
  end function val_logical_get_type
  
  subroutine val_logical_init (val, nval)
    class(val_logical_t), intent(out) :: val
    integer, intent(in) :: nval
    allocate (val%x (nval))
  end subroutine val_logical_init
  
  function val_logical_get_nval (val) result (nval)
    class(val_logical_t), intent(in) :: val
    integer :: nval
    if (allocated (val%x)) then
       nval = size (val%x)
    else
       nval = 0
    end if
  end function val_logical_get_nval
  
  subroutine val_logical_read (val, unit, i, iostat)
    class(val_logical_t), intent(inout) :: val
    integer, intent(in) :: unit, i
    integer, intent(out), optional :: iostat
    if (present (iostat)) then
       read (unit, *, iostat=iostat)  val%x(i)
    else
       read (unit, *)  val%x(i)
    end if
  end subroutine val_logical_read
       
  subroutine val_logical_write (val, unit, i)
    class(val_logical_t), intent(in) :: val
    integer, intent(in) :: unit, i
    write (unit, "(L1)")  val%x(i)
  end subroutine val_logical_write
       
  subroutine create_logical_val (code, item)
    class(code_t), intent(inout) :: code
    logical, dimension(:), intent(in) :: item
    call create_val (code%val, VT_LOGICAL, size (item))
    select type (val => code%val)
    type is (val_logical_t);  val%x = item
    end select
  end subroutine create_logical_val
  
  subroutine get_logical_array (code, item, success)
    class(code_t), intent(in) :: code
    logical, dimension(:), intent(out), allocatable :: item
    logical, intent(out) :: success
    if (allocated (code%val)) then
       select type (val => code%val)
       type is (val_logical_t)
          allocate (item (size (val%x)))
          item = val%x
          success = .true.;  return
       end select
    end if
    success = .false.
  end subroutine get_logical_array
  
  subroutine get_logical (code, item, success)
    class(code_t), intent(in) :: code
    logical, intent(out) :: item
    logical, intent(out) :: success
    if (allocated (code%val)) then
       select type (val => code%val)
       type is (val_logical_t)
          if (size (val%x) == 1) then
             item = val%x(1)
             success = .true.;  return
          end if
       end select
    end if
    success = .false.
  end subroutine get_logical
  
  function val_string_get_type (val) result (type)
    class(val_string_t), intent(in) :: val
    integer :: type
    type = VT_STRING
  end function val_string_get_type
  
  subroutine val_string_init (val, nval)
    class(val_string_t), intent(out) :: val
    integer, intent(in) :: nval
    allocate (val%x (nval))
  end subroutine val_string_init
  
  function val_string_get_nval (val) result (nval)
    class(val_string_t), intent(in) :: val
    integer :: nval
    if (allocated (val%x)) then
       nval = size (val%x)
    else
       nval = 0
    end if
  end function val_string_get_nval
  
  subroutine val_string_read (val, unit, i, iostat)
    class(val_string_t), intent(inout) :: val
    integer, intent(in) :: unit, i
    integer, intent(out), optional :: iostat
    character(NAME_LEN_MAX) :: buffer
    if (present (iostat)) then
       read (unit, *, iostat=iostat)  buffer
    else
       read (unit, *)  buffer
    end if
    val%x(i) = trim (adjustl (buffer))
  end subroutine val_string_read
       
  subroutine val_string_write (val, unit, i)
    class(val_string_t), intent(in) :: val
    integer, intent(in) :: unit, i
    write (unit, "(A)")  char (val%x(i))
  end subroutine val_string_write
       
  subroutine create_string_val (code, item)
    class(code_t), intent(inout) :: code
    type(string_t), dimension(:), intent(in) :: item
    call create_val (code%val, VT_STRING, size (item))
    select type (val => code%val)
    type is (val_string_t);  val%x = item
    end select
  end subroutine create_string_val
  
  subroutine get_string_array (code, item, success)
    class(code_t), intent(in) :: code
    type(string_t), dimension(:), intent(out), allocatable :: item
    logical, intent(out) :: success
    if (allocated (code%val)) then
       select type (val => code%val)
       type is (val_string_t)
          allocate (item (size (val%x)))
          item = val%x
          success = .true.;  return
       end select
    end if
    success = .false.
  end subroutine get_string_array
  
  subroutine get_string (code, item, success)
    class(code_t), intent(in) :: code
    type(string_t), intent(out) :: item
    logical, intent(out) :: success
    if (allocated (code%val)) then
       select type (val => code%val)
       type is (val_string_t)
          if (size (val%x) == 1) then
             item = val%x(1)
             success = .true.;  return
          end if
       end select
    end if
    success = .false.
  end subroutine get_string
  
  function val_integer_get_type (val) result (type)
    class(val_integer_t), intent(in) :: val
    integer :: type
    type = VT_INTEGER
  end function val_integer_get_type
  
  subroutine val_integer_init (val, nval)
    class(val_integer_t), intent(out) :: val
    integer, intent(in) :: nval
    allocate (val%x (nval))
  end subroutine val_integer_init
  
  function val_integer_get_nval (val) result (nval)
    class(val_integer_t), intent(in) :: val
    integer :: nval
    if (allocated (val%x)) then
       nval = size (val%x)
    else
       nval = 0
    end if
  end function val_integer_get_nval
  
  subroutine val_integer_read (val, unit, i, iostat)
    class(val_integer_t), intent(inout) :: val
    integer, intent(in) :: unit, i
    integer, intent(out), optional :: iostat
    if (present (iostat)) then
       read (unit, *, iostat=iostat)  val%x(i)
    else
       read (unit, *)  val%x(i)
    end if
  end subroutine val_integer_read
       
  subroutine val_integer_write (val, unit, i)
    class(val_integer_t), intent(in) :: val
    integer, intent(in) :: unit, i
    write (unit, "(I0)")  val%x(i)
  end subroutine val_integer_write
       
  subroutine create_integer_val (code, item)
    class(code_t), intent(inout) :: code
    integer, dimension(:), intent(in) :: item
    call create_val (code%val, VT_INTEGER, size (item))
    select type (val => code%val)
    type is (val_integer_t);  val%x = item
    end select
  end subroutine create_integer_val
  
  subroutine get_integer_array (code, item, success)
    class(code_t), intent(in) :: code
    integer, dimension(:), intent(out), allocatable :: item
    logical, intent(out) :: success
    if (allocated (code%val)) then
       select type (val => code%val)
       type is (val_integer_t)
          allocate (item (size (val%x)))
          item = val%x
          success = .true.;  return
       end select
    end if
    success = .false.
  end subroutine get_integer_array
  
  subroutine get_integer (code, item, success)
    class(code_t), intent(in) :: code
    integer, intent(out) :: item
    logical, intent(out) :: success
    if (allocated (code%val)) then
       select type (val => code%val)
       type is (val_integer_t)
          if (size (val%x) == 1) then
             item = val%x(1)
             success = .true.;  return
          end if
       end select
    end if
    success = .false.
  end subroutine get_integer
  
  subroutine create_val (val, vt, nval)
    class(val_t), allocatable, intent(out) :: val
    integer, intent(in) :: vt, nval
    if (allocated (val))  deallocate (val)    
    select case (vt)
    case (VT_LOGICAL);  allocate (val_logical_t :: val)
    case (VT_STRING);  allocate (val_string_t :: val)
    case (VT_INTEGER);  allocate (val_integer_t :: val)
    end select
    call val%init (nval)
  end subroutine create_val
  

end module codes
