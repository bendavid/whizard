! WHIZARD 2.3.0 July 21 2016
! 
! Copyright (C) 1999-2016 by 
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

module particle_specifiers

  use iso_varying_string, string_t => varying_string
  use io_units
  use diagnostics

  implicit none
  private

  public :: prt_expr_t
  public :: prt_spec_t
  public :: prt_spec_write
  public :: prt_spec_read
  public :: new_prt_spec
  public :: prt_spec_list_t
  public :: prt_spec_sum_t

  type, abstract :: prt_spec_expr_t
   contains
     procedure (prt_spec_expr_to_string), deferred :: to_string
     procedure (prt_spec_expr_expand_sub), deferred :: expand_sub
  end type prt_spec_expr_t
  
  type :: prt_expr_t
     class(prt_spec_expr_t), allocatable :: x
   contains
     procedure :: to_string => prt_expr_to_string
     procedure :: init_spec => prt_expr_init_spec
     procedure :: init_list => prt_expr_init_list
     procedure :: init_sum => prt_expr_init_sum
     procedure :: get_n_terms => prt_expr_get_n_terms
     procedure :: term_to_array => prt_expr_term_to_array
     procedure :: expand => prt_expr_expand
  end type prt_expr_t
  
  type, extends (prt_spec_expr_t) :: prt_spec_t
     private
     type(string_t) :: name
     logical :: polarized = .false.
     type(string_t), dimension(:), allocatable :: decay
   contains
     procedure :: get_name => prt_spec_get_name
     procedure :: to_string => prt_spec_to_string
     procedure :: is_polarized => prt_spec_is_polarized
     procedure :: is_unstable => prt_spec_is_unstable
     procedure :: get_n_decays => prt_spec_get_n_decays
     procedure :: get_decays => prt_spec_get_decays
     procedure :: expand_sub => prt_spec_expand_sub
  end type prt_spec_t
  
  type, extends (prt_spec_expr_t) :: prt_spec_list_t
     type(prt_expr_t), dimension(:), allocatable :: expr
   contains
     procedure :: to_string => prt_spec_list_to_string
     procedure :: flatten => prt_spec_list_flatten
     procedure :: expand_sub => prt_spec_list_expand_sub
  end type prt_spec_list_t
  
  type, extends (prt_spec_expr_t) :: prt_spec_sum_t
     type(prt_expr_t), dimension(:), allocatable :: expr
   contains
     procedure :: to_string => prt_spec_sum_to_string
     procedure :: flatten => prt_spec_sum_flatten
     procedure :: expand_sub => prt_spec_sum_expand_sub
  end type prt_spec_sum_t
  

  abstract interface
     function prt_spec_expr_to_string (object) result (string)
       import
       class(prt_spec_expr_t), intent(in) :: object
       type(string_t) :: string
     end function prt_spec_expr_to_string
  end interface
  
  abstract interface
     subroutine prt_spec_expr_expand_sub (object)
       import
       class(prt_spec_expr_t), intent(inout) :: object
     end subroutine prt_spec_expr_expand_sub
  end interface

  interface prt_spec_write
     module procedure prt_spec_write1
     module procedure prt_spec_write2
  end interface prt_spec_write
  interface prt_spec_read
     module procedure prt_spec_read1
     module procedure prt_spec_read2
  end interface prt_spec_read
  interface new_prt_spec
     module procedure new_prt_spec
     module procedure new_prt_spec_polarized
     module procedure new_prt_spec_unstable
  end interface new_prt_spec

contains

  recursive function prt_expr_to_string (object) result (string)
    class(prt_expr_t), intent(in) :: object
    type(string_t) :: string
    if (allocated (object%x)) then
       string = object%x%to_string ()
    else
       string = ""
    end if
  end function prt_expr_to_string
  
  subroutine prt_expr_init_spec (object, spec)
    class(prt_expr_t), intent(out) :: object
    type(prt_spec_t), intent(in) :: spec
    allocate (prt_spec_t :: object%x)
    select type (x => object%x)
    type is (prt_spec_t)
       x = spec
    end select
  end subroutine prt_expr_init_spec
  
  subroutine prt_expr_init_list (object, n)
    class(prt_expr_t), intent(out) :: object
    integer, intent(in) :: n
    allocate (prt_spec_list_t :: object%x)
    select type (x => object%x)
    type is (prt_spec_list_t)
       allocate (x%expr (n))
    end select
  end subroutine prt_expr_init_list
  
  subroutine prt_expr_init_sum (object, n)
    class(prt_expr_t), intent(out) :: object
    integer, intent(in) :: n
    allocate (prt_spec_sum_t :: object%x)
    select type (x => object%x)
    type is (prt_spec_sum_t)
       allocate (x%expr (n))
    end select
  end subroutine prt_expr_init_sum
  
  function prt_expr_get_n_terms (object) result (n)
    class(prt_expr_t), intent(in) :: object
    integer :: n
    if (allocated (object%x)) then
       select type (x => object%x)
       type is (prt_spec_sum_t)
          n = size (x%expr)
       class default
          n = 1
       end select
    else
       n = 0
    end if
  end function prt_expr_get_n_terms
  
  recursive subroutine prt_expr_term_to_array (object, array, i)
    class(prt_expr_t), intent(in) :: object
    type(prt_spec_t), dimension(:), intent(inout), allocatable :: array
    integer, intent(in) :: i
    integer :: j
    if (allocated (array))  deallocate (array)
    select type (x => object%x)
    type is (prt_spec_t)
       allocate (array (1))
       array(1) = x
    type is (prt_spec_list_t)
       allocate (array (size (x%expr)))
       do j = 1, size (array)
          select type (y => x%expr(j)%x)
          type is (prt_spec_t)
             array(j) = y
          end select
       end do
    type is (prt_spec_sum_t)
       call x%expr(i)%term_to_array (array, 1)
    end select
  end subroutine prt_expr_term_to_array
  
  subroutine prt_spec_write1 (object, unit, advance)
    type(prt_spec_t), intent(in) :: object
    integer, intent(in), optional :: unit
    character(len=*), intent(in), optional :: advance
    character(3) :: adv
    integer :: u
    u = given_output_unit (unit)
    adv = "yes";  if (present (advance))  adv = advance
    write (u, "(A)", advance = adv)  char (object%to_string ())
  end subroutine prt_spec_write1
       
  subroutine prt_spec_write2 (prt_spec, unit, advance)
    type(prt_spec_t), dimension(:), intent(in) :: prt_spec
    integer, intent(in), optional :: unit
    character(len=*), intent(in), optional :: advance
    character(3) :: adv
    integer :: u, i
    u = given_output_unit (unit)
    adv = "yes";  if (present (advance))  adv = advance
    do i = 1, size (prt_spec)
       if (i > 1)  write (u, "(A)", advance="no")  ", "
       call prt_spec_write (prt_spec(i), u, advance="no")
    end do
    write (u, "(A)", advance = adv)
  end subroutine prt_spec_write2
       
  pure subroutine prt_spec_read1 (prt_spec, string)
    type(prt_spec_t), intent(out) :: prt_spec
    type(string_t), intent(in) :: string
    type(string_t) :: arg, buffer
    integer :: b1, b2, c, n, i
    b1 = scan (string, "(")
    b2 = scan (string, ")")
    if (b1 == 0) then
       prt_spec%name = trim (adjustl (string))
    else
       prt_spec%name = trim (adjustl (extract (string, 1, b1-1)))
       arg = trim (adjustl (extract (string, b1+1, b2-1)))
       if (arg == "*") then
          prt_spec%polarized = .true.
       else
          n = 0
          buffer = arg
          do
             if (verify (buffer, " ") == 0)  exit
             n = n + 1
             c = scan (buffer, "+")
             if (c == 0)  exit
             buffer = extract (buffer, c+1)
          end do
          allocate (prt_spec%decay (n))
          buffer = arg
          do i = 1, n
             c = scan (buffer, "+")
             if (c == 0)  c = len (buffer) + 1
             prt_spec%decay(i) = trim (adjustl (extract (buffer, 1, c-1)))
             buffer = extract (buffer, c+1)
          end do
       end if
    end if
  end subroutine prt_spec_read1

  pure subroutine prt_spec_read2 (prt_spec, string)
    type(prt_spec_t), dimension(:), intent(out), allocatable :: prt_spec
    type(string_t), intent(in) :: string
    type(string_t) :: buffer
    integer :: c, i, n
    n = 0
    buffer = string
    do
       n = n + 1
       c = scan (buffer, ",")
       if (c == 0)  exit
       buffer = extract (buffer, c+1)
    end do
    allocate (prt_spec (n))
    buffer = string
    do i = 1, size (prt_spec)
       c = scan (buffer, ",")
       if (c == 0)  c = len (buffer) + 1
       call prt_spec_read (prt_spec(i), &
            trim (adjustl (extract (buffer, 1, c-1))))
       buffer = extract (buffer, c+1)
    end do
  end subroutine prt_spec_read2
  
  elemental function new_prt_spec (name) result (prt_spec)
    type(string_t), intent(in) :: name
    type(prt_spec_t) :: prt_spec
    prt_spec%name = name
  end function new_prt_spec

  elemental function new_prt_spec_polarized (name, polarized) result (prt_spec)
    type(string_t), intent(in) :: name
    logical, intent(in) :: polarized
    type(prt_spec_t) :: prt_spec
    prt_spec%name = name
    prt_spec%polarized = polarized
  end function new_prt_spec_polarized

  pure function new_prt_spec_unstable (name, decay) result (prt_spec)
    type(string_t), intent(in) :: name
    type(string_t), dimension(:), intent(in) :: decay
    type(prt_spec_t) :: prt_spec
    prt_spec%name = name
    allocate (prt_spec%decay (size (decay)))
    prt_spec%decay = decay
  end function new_prt_spec_unstable

  elemental function prt_spec_get_name (prt_spec) result (name)
    class(prt_spec_t), intent(in) :: prt_spec
    type(string_t) :: name
    name = prt_spec%name
  end function prt_spec_get_name
  
  function prt_spec_to_string (object) result (string)
    class(prt_spec_t), intent(in) :: object
    type(string_t) :: string
    integer :: i
    string = object%name
    if (allocated (object%decay)) then
       string = string // "("
       do i = 1, size (object%decay)
          if (i > 1)  string = string // " + "
          string = string // object%decay(i)
       end do
       string = string // ")"
    else if (object%polarized) then
       string = string // "(*)"
    end if
  end function prt_spec_to_string
  
  elemental function prt_spec_is_polarized (prt_spec) result (flag)
    class(prt_spec_t), intent(in) :: prt_spec
    logical :: flag
    flag = prt_spec%polarized
  end function prt_spec_is_polarized
  
  elemental function prt_spec_is_unstable (prt_spec) result (flag)
    class(prt_spec_t), intent(in) :: prt_spec
    logical :: flag
    flag = allocated (prt_spec%decay)
  end function prt_spec_is_unstable
  
  elemental function prt_spec_get_n_decays (prt_spec) result (n)
    class(prt_spec_t), intent(in) :: prt_spec
    integer :: n
    if (allocated (prt_spec%decay)) then
       n = size (prt_spec%decay)
    else
       n = 0
    end if
  end function prt_spec_get_n_decays
  
  subroutine prt_spec_get_decays (prt_spec, decay)
    class(prt_spec_t), intent(in) :: prt_spec
    type(string_t), dimension(:), allocatable, intent(out) :: decay
    if (allocated (prt_spec%decay)) then
       allocate (decay (size (prt_spec%decay)))
       decay = prt_spec%decay
    else
       allocate (decay (0))
    end if
  end subroutine prt_spec_get_decays
  
  subroutine prt_spec_expand_sub (object)
    class(prt_spec_t), intent(inout) :: object
  end subroutine prt_spec_expand_sub

  recursive function prt_spec_list_to_string (object) result (string)
    class(prt_spec_list_t), intent(in) :: object
    type(string_t) :: string
    integer :: i
    string = ""
    if (allocated (object%expr)) then
       do i = 1, size (object%expr)
          if (i > 1)  string = string // ", "
          select type (x => object%expr(i)%x)
          type is (prt_spec_list_t)
             string = string // "(" // x%to_string () // ")"
          class default
             string = string // x%to_string ()
          end select
       end do
    end if
  end function prt_spec_list_to_string

  subroutine prt_spec_list_flatten (object)
    class(prt_spec_list_t), intent(inout) :: object
    type(prt_expr_t), dimension(:), allocatable :: tmp_expr
    integer :: i, n_flat, i_flat
    n_flat = 0
    do i = 1, size (object%expr)
       select type (y => object%expr(i)%x)
       type is (prt_spec_list_t)
          n_flat = n_flat + size (y%expr)
       class default
          n_flat = n_flat + 1
       end select
    end do
    if (n_flat > size (object%expr)) then
       allocate (tmp_expr (n_flat))
       i_flat = 0
       do i = 1, size (object%expr)
          select type (y => object%expr(i)%x)
          type is (prt_spec_list_t)
             tmp_expr (i_flat + 1 : i_flat + size (y%expr)) = y%expr
             i_flat = i_flat + size (y%expr)
          class default
             tmp_expr (i_flat + 1) = object%expr(i)
             i_flat = i_flat + 1
          end select
       end do
    end if
    if (allocated (tmp_expr)) &
         call move_alloc (from = tmp_expr, to = object%expr)
  end subroutine prt_spec_list_flatten
    
  subroutine distribute_prt_spec_list (object)
    class(prt_spec_expr_t), intent(inout), allocatable :: object
    class(prt_spec_expr_t), allocatable :: new_object
    integer, dimension(:), allocatable :: n, ii
    integer :: k, n_expr, n_terms, i_term
    select type (object)
    type is (prt_spec_list_t)
       n_expr = size (object%expr)
       allocate (n (n_expr), source = 1)
       allocate (ii (n_expr), source = 1)
       do k = 1, size (object%expr)
          select type (y => object%expr(k)%x)
          type is (prt_spec_sum_t)
             n(k) = size (y%expr)
          end select
       end do
       n_terms = product (n)
       if (n_terms > 1) then
          allocate (prt_spec_sum_t :: new_object)
          select type (new_object)
          type is (prt_spec_sum_t)
             allocate (new_object%expr (n_terms))
             do i_term = 1, n_terms
                allocate (prt_spec_list_t :: new_object%expr(i_term)%x)
                select type (x => new_object%expr(i_term)%x)
                type is (prt_spec_list_t)
                   allocate (x%expr (n_expr))
                   do k = 1, n_expr
                      select type (y => object%expr(k)%x)
                      type is (prt_spec_sum_t)
                         x%expr(k) = y%expr(ii(k))
                      class default
                         x%expr(k) = object%expr(k)
                      end select
                   end do
                end select
                INCR_INDEX: do k = n_expr, 1, -1
                   if (ii(k) < n(k)) then
                      ii(k) = ii(k) + 1
                      exit INCR_INDEX
                   else
                      ii(k) = 1
                   end if
                end do INCR_INDEX
             end do
          end select
       end if
    end select
    if (allocated (new_object)) call move_alloc (from = new_object, to = object)
  end subroutine distribute_prt_spec_list
    
  recursive subroutine prt_spec_list_expand_sub (object)
    class(prt_spec_list_t), intent(inout) :: object
    integer :: i
    if (allocated (object%expr)) then
       do i = 1, size (object%expr)
          call object%expr(i)%expand ()
       end do
    end if
  end subroutine prt_spec_list_expand_sub

  recursive function prt_spec_sum_to_string (object) result (string)
    class(prt_spec_sum_t), intent(in) :: object
    type(string_t) :: string
    integer :: i
    string = ""
    if (allocated (object%expr)) then
       do i = 1, size (object%expr)
          if (i > 1)  string = string // " + "
          select type (x => object%expr(i)%x)
          type is (prt_spec_list_t)
             string = string // "(" // x%to_string () // ")"
          type is (prt_spec_sum_t)
             string = string // "(" // x%to_string () // ")"
          class default
             string = string // x%to_string ()
          end select
       end do
    end if
  end function prt_spec_sum_to_string

  subroutine prt_spec_sum_flatten (object)
    class(prt_spec_sum_t), intent(inout) :: object
    type(prt_expr_t), dimension(:), allocatable :: tmp_expr
    integer :: i, n_flat, i_flat
    n_flat = 0
    do i = 1, size (object%expr)
       select type (y => object%expr(i)%x)
       type is (prt_spec_sum_t)
          n_flat = n_flat + size (y%expr)
          class default
          n_flat = n_flat + 1
       end select
    end do
    if (n_flat > size (object%expr)) then
       allocate (tmp_expr (n_flat))
       i_flat = 0
       do i = 1, size (object%expr)
          select type (y => object%expr(i)%x)
          type is (prt_spec_sum_t)
             tmp_expr (i_flat + 1 : i_flat + size (y%expr)) = y%expr
             i_flat = i_flat + size (y%expr)
          class default
             tmp_expr (i_flat + 1) = object%expr(i)
             i_flat = i_flat + 1
          end select
       end do
    end if
    if (allocated (tmp_expr)) &
         call move_alloc (from = tmp_expr, to = object%expr)
  end subroutine prt_spec_sum_flatten
    
  recursive subroutine prt_spec_sum_expand_sub (object)
    class(prt_spec_sum_t), intent(inout) :: object
    integer :: i
    if (allocated (object%expr)) then
       do i = 1, size (object%expr)
          call object%expr(i)%expand ()
       end do
    end if
  end subroutine prt_spec_sum_expand_sub

  recursive subroutine prt_expr_expand (expr)
    class(prt_expr_t), intent(inout) :: expr
    if (allocated (expr%x)) then
       call distribute_prt_spec_list (expr%x)
       call expr%x%expand_sub ()
       select type (x => expr%x)
       type is (prt_spec_list_t)
          call x%flatten ()
       type is (prt_spec_sum_t)
          call x%flatten ()
       end select
    end if
  end subroutine prt_expr_expand


end module particle_specifiers
