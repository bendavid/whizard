! WHIZARD 2.2.2 July 6 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Christian Speckner <cnspeckn@googlemail.com> 
!     and  Fabian Bach, Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

module sf_mappings

  use kinds, only: default !NODEP!
  use kinds, only: double !NODEP!
  use file_utils !NODEP!
  use limits, only: FMT_12, FMT_13, FMT_14, FMT_15, FMT_16 !NODEP!
  use diagnostics !NODEP!
  use constants, only: pi !NODEP!
  use unit_tests

  implicit none
  private

  public :: sf_mapping_t
  public :: sf_s_mapping_t
  public :: sf_res_mapping_t
  public :: sf_os_mapping_t
  public :: sf_ep_mapping_t
  public :: sf_epr_mapping_t
  public :: sf_epo_mapping_t
  public :: sf_ip_mapping_t
  public :: sf_ipr_mapping_t
  public :: sf_ipo_mapping_t
  public :: sf_ei_mapping_t
  public :: sf_eir_mapping_t
  public :: sf_eio_mapping_t
  public :: log_prec
  public :: map_power_1
  public :: map_power_inverse_1
  public :: sf_channel_t
  public :: allocate_sf_channels
  public :: any_sf_channel_has_mapping
  public :: sf_mappings_test

  integer, parameter :: SFMAP_NONE = 0
  integer, parameter :: SFMAP_SINGLE = 1
  integer, parameter :: SFMAP_MULTI_S = 2
  integer, parameter :: SFMAP_MULTI_RES = 3
  integer, parameter :: SFMAP_MULTI_ONS = 4
  integer, parameter :: SFMAP_MULTI_EP = 5
  integer, parameter :: SFMAP_MULTI_EPR = 6
  integer, parameter :: SFMAP_MULTI_EPO = 7
  integer, parameter :: SFMAP_MULTI_IP = 8
  integer, parameter :: SFMAP_MULTI_IPR = 9
  integer, parameter :: SFMAP_MULTI_IPO = 10
  integer, parameter :: SFMAP_MULTI_EI = 11


  type, abstract :: sf_mapping_t
     integer, dimension(:), allocatable :: i
   contains
     procedure (sf_mapping_write), deferred :: write
     procedure :: base_init => sf_mapping_base_init
     procedure :: set_index => sf_mapping_set_index
     procedure :: get_n_dim => sf_mapping_get_n_dim
     procedure (sf_mapping_compute), deferred :: compute
     procedure (sf_mapping_inverse), deferred :: inverse
     procedure :: check => sf_mapping_check
     procedure :: integral => sf_mapping_integral
  end type sf_mapping_t
     
  type, extends (sf_mapping_t) :: sf_s_mapping_t
     logical :: power_set = .false.
     real(default) :: power = 1
   contains
     procedure :: write => sf_s_mapping_write
     procedure :: init => sf_s_mapping_init
     procedure :: compute => sf_s_mapping_compute
     procedure :: inverse => sf_s_mapping_inverse
  end type sf_s_mapping_t
  
  type, extends (sf_mapping_t) :: sf_res_mapping_t
     real(default) :: m = 0
     real(default) :: w = 0
   contains
     procedure :: write => sf_res_mapping_write
     procedure :: init => sf_res_mapping_init
     procedure :: compute => sf_res_mapping_compute
     procedure :: inverse => sf_res_mapping_inverse
  end type sf_res_mapping_t
  
  type, extends (sf_mapping_t) :: sf_os_mapping_t
     real(default) :: m = 0
     real(default) :: lm2 = 0
   contains
     procedure :: write => sf_os_mapping_write
     procedure :: init => sf_os_mapping_init
     procedure :: compute => sf_os_mapping_compute
     procedure :: inverse => sf_os_mapping_inverse
  end type sf_os_mapping_t
  
  type, extends (sf_mapping_t) :: sf_ep_mapping_t
     real(default) :: a = 1
   contains
     procedure :: write => sf_ep_mapping_write
     procedure :: init => sf_ep_mapping_init
     procedure :: compute => sf_ep_mapping_compute
     procedure :: inverse => sf_ep_mapping_inverse
  end type sf_ep_mapping_t
  
  type, extends (sf_mapping_t) :: sf_epr_mapping_t
     real(default) :: a = 1
     real(default) :: m = 0
     real(default) :: w = 0
     logical :: resonance = .true.
   contains
     procedure :: write => sf_epr_mapping_write
     procedure :: init => sf_epr_mapping_init
     procedure :: compute => sf_epr_mapping_compute
     procedure :: inverse => sf_epr_mapping_inverse
  end type sf_epr_mapping_t
  
  type, extends (sf_mapping_t) :: sf_epo_mapping_t
     real(default) :: a = 1
     real(default) :: m = 0
     real(default) :: lm2 = 0
   contains
     procedure :: write => sf_epo_mapping_write
     procedure :: init => sf_epo_mapping_init
     procedure :: compute => sf_epo_mapping_compute
     procedure :: inverse => sf_epo_mapping_inverse
  end type sf_epo_mapping_t
  
  type, extends (sf_mapping_t) :: sf_ip_mapping_t
     real(default) :: eps = 0
   contains
     procedure :: write => sf_ip_mapping_write
     procedure :: init => sf_ip_mapping_init
     procedure :: compute => sf_ip_mapping_compute
     procedure :: inverse => sf_ip_mapping_inverse
  end type sf_ip_mapping_t
  
  type, extends (sf_mapping_t) :: sf_ipr_mapping_t
     real(default) :: eps = 0
     real(default) :: m = 0
     real(default) :: w = 0
     logical :: resonance = .true.
   contains
     procedure :: write => sf_ipr_mapping_write
     procedure :: init => sf_ipr_mapping_init
     procedure :: compute => sf_ipr_mapping_compute
     procedure :: inverse => sf_ipr_mapping_inverse
  end type sf_ipr_mapping_t
  
  type, extends (sf_mapping_t) :: sf_ipo_mapping_t
     real(default) :: eps = 0
     real(default) :: m = 0
   contains
     procedure :: write => sf_ipo_mapping_write
     procedure :: init => sf_ipo_mapping_init
     procedure :: compute => sf_ipo_mapping_compute
     procedure :: inverse => sf_ipo_mapping_inverse
  end type sf_ipo_mapping_t
  
  type, extends (sf_mapping_t) :: sf_ei_mapping_t
     type(sf_ep_mapping_t) :: ep
     type(sf_ip_mapping_t) :: ip
   contains
     procedure :: write => sf_ei_mapping_write
     procedure :: init => sf_ei_mapping_init
     procedure :: set_index => sf_ei_mapping_set_index
     procedure :: compute => sf_ei_mapping_compute
     procedure :: inverse => sf_ei_mapping_inverse
  end type sf_ei_mapping_t
  
  type, extends (sf_mapping_t) :: sf_eir_mapping_t
     type(sf_res_mapping_t) :: res
     type(sf_epr_mapping_t) :: ep
     type(sf_ipr_mapping_t) :: ip
   contains
     procedure :: write => sf_eir_mapping_write
     procedure :: init => sf_eir_mapping_init
     procedure :: set_index => sf_eir_mapping_set_index
     procedure :: compute => sf_eir_mapping_compute
     procedure :: inverse => sf_eir_mapping_inverse
  end type sf_eir_mapping_t
  
  type, extends (sf_mapping_t) :: sf_eio_mapping_t
     type(sf_os_mapping_t) :: os
     type(sf_epr_mapping_t) :: ep
     type(sf_ipr_mapping_t) :: ip
   contains
     procedure :: write => sf_eio_mapping_write
     procedure :: init => sf_eio_mapping_init
     procedure :: set_index => sf_eio_mapping_set_index
     procedure :: compute => sf_eio_mapping_compute
     procedure :: inverse => sf_eio_mapping_inverse
  end type sf_eio_mapping_t
  
  type :: sf_channel_t
     integer, dimension(:), allocatable :: map_code
     class(sf_mapping_t), allocatable :: multi_mapping
   contains
     procedure :: write => sf_channel_write
     procedure :: init => sf_channel_init
     generic :: assignment (=) => sf_channel_assign
     procedure :: sf_channel_assign
     procedure :: activate_mapping => sf_channel_activate_mapping
     procedure :: set_s_mapping => sf_channel_set_s_mapping
     procedure :: set_res_mapping => sf_channel_set_res_mapping
     procedure :: set_os_mapping => sf_channel_set_os_mapping
     procedure :: set_ep_mapping => sf_channel_set_ep_mapping
     procedure :: set_epr_mapping => sf_channel_set_epr_mapping
     procedure :: set_epo_mapping => sf_channel_set_epo_mapping
     procedure :: set_ip_mapping => sf_channel_set_ip_mapping
     procedure :: set_ipr_mapping => sf_channel_set_ipr_mapping
     procedure :: set_ipo_mapping => sf_channel_set_ipo_mapping
     procedure :: set_ei_mapping => sf_channel_set_ei_mapping
     procedure :: set_eir_mapping => sf_channel_set_eir_mapping
     procedure :: set_eio_mapping => sf_channel_set_eio_mapping
     procedure :: is_single_mapping => sf_channel_is_single_mapping
     procedure :: is_multi_mapping => sf_channel_is_multi_mapping
     procedure :: set_par_index => sf_channel_set_par_index
  end type sf_channel_t
  

  abstract interface
     subroutine sf_mapping_write (object, unit)
       import
       class(sf_mapping_t), intent(in) :: object
       integer, intent(in), optional :: unit
     end subroutine sf_mapping_write
  end interface

  abstract interface
     subroutine sf_mapping_compute (mapping, r, rb, f, p, pb, x_free)
       import
       class(sf_mapping_t), intent(inout) :: mapping
       real(default), dimension(:), intent(out) :: r, rb
       real(default), intent(out) :: f
       real(default), dimension(:), intent(in) :: p, pb
       real(default), intent(inout), optional :: x_free
     end subroutine sf_mapping_compute
  end interface
  
  abstract interface
     subroutine sf_mapping_inverse (mapping, r, rb, f, p, pb, x_free)
       import
       class(sf_mapping_t), intent(inout) :: mapping
       real(default), dimension(:), intent(in) :: r, rb
       real(default), intent(out) :: f
       real(default), dimension(:), intent(out) :: p, pb
       real(default), intent(inout), optional :: x_free
     end subroutine sf_mapping_inverse
  end interface
  

contains

  subroutine sf_mapping_base_init (mapping, n_par)
    class(sf_mapping_t), intent(out) :: mapping
    integer, intent(in) :: n_par
    allocate (mapping%i (n_par))
    mapping%i = 0
  end subroutine sf_mapping_base_init
    
  subroutine sf_mapping_set_index (mapping, j, i)
    class(sf_mapping_t), intent(inout) :: mapping
    integer, intent(in) :: j, i
    mapping%i(j) = i
  end subroutine sf_mapping_set_index
    
  function sf_mapping_get_n_dim (mapping) result (n)
    class(sf_mapping_t), intent(in) :: mapping
    integer :: n
    n = size (mapping%i)
  end function sf_mapping_get_n_dim
  
  subroutine sf_mapping_check (mapping, u, p_in, pb_in, fmt_p, fmt_f)
    class(sf_mapping_t), intent(inout) :: mapping
    integer, intent(in) :: u
    real(default), dimension(:), intent(in) :: p_in, pb_in
    character(*), intent(in) :: fmt_p
    character(*), intent(in), optional :: fmt_f
    real(default), dimension(size(p_in)) :: p, pb, r, rb
    real(default) :: f, tolerance
    tolerance = 1.5E-17
    p = p_in
    pb= pb_in
    call mapping%compute (r, rb, f, p, pb)
    call pacify (p, tolerance)  
    call pacify (pb, tolerance) 
    call pacify (r, tolerance)  
    call pacify (rb, tolerance) 
    write (u, "(3x,A,9(1x," // fmt_p // "))")  "p =", p
    write (u, "(3x,A,9(1x," // fmt_p // "))")  "pb=", pb
    write (u, "(3x,A,9(1x," // fmt_p // "))")  "r =", r
    write (u, "(3x,A,9(1x," // fmt_p // "))")  "rb=", rb
    if (present (fmt_f)) then
       write (u, "(3x,A,9(1x," // fmt_f // "))")  "f =", f
    else
       write (u, "(3x,A,9(1x," // fmt_p // "))")  "f =", f
    end if
    write (u, *)
    call mapping%inverse (r, rb, f, p, pb)
    call pacify (p, tolerance)  
    call pacify (pb, tolerance) 
    call pacify (r, tolerance)  
    call pacify (rb, tolerance) 
    write (u, "(3x,A,9(1x," // fmt_p // "))")  "p =", p
    write (u, "(3x,A,9(1x," // fmt_p // "))")  "pb=", pb
    write (u, "(3x,A,9(1x," // fmt_p // "))")  "r =", r
    write (u, "(3x,A,9(1x," // fmt_p // "))")  "rb=", rb
    if (present (fmt_f)) then
       write (u, "(3x,A,9(1x," // fmt_f // "))")  "f =", f
    else
       write (u, "(3x,A,9(1x," // fmt_p // "))")  "f =", f
    end if
    write (u, *)
    write (u, "(3x,A,9(1x," // fmt_p // "))")  "*r=", product (r)
  end subroutine sf_mapping_check
    
  function sf_mapping_integral (mapping, n_calls) result (integral)
    class(sf_mapping_t), intent(inout) :: mapping
    integer, intent(in) :: n_calls
    real(default) :: integral
    integer :: n_dim, n_bin, k
    real(default), dimension(:), allocatable :: p, pb, r, rb
    integer, dimension(:), allocatable :: ii
    real(default) :: dx, f, s

    n_dim = mapping%get_n_dim ()
    allocate (p (n_dim))
    allocate (pb(n_dim))
    allocate (r (n_dim))
    allocate (rb(n_dim))
    allocate (ii(n_dim))
    n_bin = nint (real (n_calls, default) ** (1._default / n_dim))
    dx = 1._default / n_bin
    s = 0
    ii = 1

    SAMPLE: do
       do k = 1, n_dim
          p(k)  = ii(k) * dx - dx/2
          pb(k) = (n_bin - ii(k)) * dx + dx/2
       end do
       call mapping%compute (r, rb, f, p, pb)
       s = s + f
       INCR: do k = 1, n_dim
          ii(k) = ii(k) + 1
          if (ii(k) <= n_bin) then
             exit INCR
          else if (k < n_dim) then
             ii(k) = 1
          else
             exit SAMPLE
          end if
       end do INCR
    end do SAMPLE

    integral = s / real (n_bin, default) ** n_dim

  end function sf_mapping_integral

  subroutine sf_s_mapping_write (object, unit)
    class(sf_s_mapping_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A)", advance="no")  "map"
    if (any (object%i /= 0)) then
       write (u, "('(',I0,',',I0,')')", advance="no")  object%i
    end if
    write (u, "(A,F7.5,A)")  ": standard (", object%power, ")"
  end subroutine sf_s_mapping_write
  
  subroutine sf_s_mapping_init (mapping, power)
    class(sf_s_mapping_t), intent(out) :: mapping
    real(default), intent(in), optional :: power
    call mapping%base_init (2)
    if (present (power)) then
       mapping%power_set = .true.
       mapping%power = power
    end if
  end subroutine sf_s_mapping_init
    
  subroutine sf_s_mapping_compute (mapping, r, rb, f, p, pb, x_free)
    class(sf_s_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(out) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(2) :: r2
    integer :: j
    if (mapping%power_set) then
       call map_unit_square (r2, f, p(mapping%i), mapping%power)
    else
       call map_unit_square (r2, f, p(mapping%i))
    end if
    r = p
    rb= pb
    do j = 1, 2
       r (mapping%i(j)) = r2(j)
       rb(mapping%i(j)) = 1 - r2(j)
    end do
  end subroutine sf_s_mapping_compute

  subroutine sf_s_mapping_inverse (mapping, r, rb, f, p, pb, x_free)
    class(sf_s_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(in) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(2) :: p2
    integer :: j
    if (mapping%power_set) then
       call map_unit_square_inverse (r(mapping%i), f, p2, mapping%power)
    else
       call map_unit_square_inverse (r(mapping%i), f, p2)
    end if
    p = r
    pb= rb
    do j = 1, 2
       p (mapping%i(j)) = p2(j)
       pb(mapping%i(j)) = 1 - p2(j)
    end do
  end subroutine sf_s_mapping_inverse

  subroutine sf_res_mapping_write (object, unit)
    class(sf_res_mapping_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A)", advance="no")  "map"
    if (any (object%i /= 0)) then
       write (u, "('(',I0,',',I0,')')", advance="no")  object%i
    end if
    write (u, "(A,F7.5,', ',F7.5,A)")  ": resonance (", object%m, object%w, ")"
  end subroutine sf_res_mapping_write
  
  subroutine sf_res_mapping_init (mapping, m, w)
    class(sf_res_mapping_t), intent(out) :: mapping
    real(default), intent(in) :: m, w
    call mapping%base_init (2)
    mapping%m = m
    mapping%w = w
  end subroutine sf_res_mapping_init
    
  subroutine sf_res_mapping_compute (mapping, r, rb, f, p, pb, x_free)
    class(sf_res_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(out) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(2) :: r2, p2
    real(default) :: fbw, f2, p1m
    integer :: j
    p2 = p(mapping%i)
    call map_breit_wigner &
         (p1m, fbw, p2(1), mapping%m, mapping%w, x_free)
    call map_unit_square (r2, f2, [p1m, p2(2)])
    f = fbw * f2
    r = p
    rb= pb
    do j = 1, 2
       r (mapping%i(j)) = r2(j)
       rb(mapping%i(j)) = 1 - r2(j)
    end do
  end subroutine sf_res_mapping_compute

  subroutine sf_res_mapping_inverse (mapping, r, rb, f, p, pb, x_free)
    class(sf_res_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(in) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(2) :: p2
    real(default) :: fbw, f2, p1m
    call map_unit_square_inverse (r(mapping%i), f2, p2)
    call map_breit_wigner_inverse &
         (p2(1), fbw, p1m, mapping%m, mapping%w, x_free)
    p = r
    pb= rb
    p (mapping%i(1)) = p1m
    pb(mapping%i(1)) = 1 - p1m
    p (mapping%i(2)) = p2(2)
    pb(mapping%i(2)) = 1 - p2(2)
    f = fbw * f2
  end subroutine sf_res_mapping_inverse

  subroutine sf_os_mapping_write (object, unit)
    class(sf_os_mapping_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A)", advance="no")  "map"
    if (any (object%i /= 0)) then
       write (u, "('(',I0,',',I0,')')", advance="no")  object%i
    end if
    write (u, "(A,F7.5,A)")  ": on-shell (", object%m, ")"
  end subroutine sf_os_mapping_write
  
  subroutine sf_os_mapping_init (mapping, m)
    class(sf_os_mapping_t), intent(out) :: mapping
    real(default), intent(in) :: m
    call mapping%base_init (2)
    mapping%m = m
    mapping%lm2 = abs (2 * log (mapping%m))
  end subroutine sf_os_mapping_init
    
  subroutine sf_os_mapping_compute (mapping, r, rb, f, p, pb, x_free)
    class(sf_os_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(out) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(2) :: r2, p2
    integer :: j
    p2 = p(mapping%i)
    call map_on_shell (r2, f, p2, mapping%lm2, x_free)
    r = p
    rb= pb
    do j = 1, 2
       r (mapping%i(j)) = r2(j)
       rb(mapping%i(j)) = 1 - r2(j)
    end do
  end subroutine sf_os_mapping_compute

  subroutine sf_os_mapping_inverse (mapping, r, rb, f, p, pb, x_free)
    class(sf_os_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(in) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(2) :: p2, r2
    r2 = r(mapping%i)
    call map_on_shell_inverse (r2, f, p2, mapping%lm2, x_free)
    p = r
    pb= rb
    p (mapping%i(1)) = p2(1)
    pb(mapping%i(1)) = 1 - p2(1)
    p (mapping%i(2)) = p2(2)
    pb(mapping%i(2)) = 1 - p2(2)
  end subroutine sf_os_mapping_inverse

  subroutine sf_ep_mapping_write (object, unit)
    class(sf_ep_mapping_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A)", advance="no")  "map"
    if (any (object%i /= 0)) then
       write (u, "('(',I0,',',I0,')')", advance="no")  object%i
    end if
    write (u, "(A,ES12.5,A)")  ": endpoint (a =", object%a, ")"
  end subroutine sf_ep_mapping_write
  
  subroutine sf_ep_mapping_init (mapping, a)
    class(sf_ep_mapping_t), intent(out) :: mapping
    real(default), intent(in), optional :: a
    call mapping%base_init (2)
    if (present (a))  mapping%a = a
  end subroutine sf_ep_mapping_init
    
  subroutine sf_ep_mapping_compute (mapping, r, rb, f, p, pb, x_free)
    class(sf_ep_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(out) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(2) :: px, r2
    real(default) :: f1, f2
    integer :: j
    call map_endpoint_1 (px(1), f1, p(mapping%i(1)), mapping%a)
    call map_endpoint_01 (px(2), f2, p(mapping%i(2)), mapping%a)
    call map_unit_square (r2, f, px)
    f = f * f1 * f2
    r = p
    rb= pb
    do j = 1, 2
       r (mapping%i(j)) = r2(j)
       rb(mapping%i(j)) = 1 - r2(j)
    end do
  end subroutine sf_ep_mapping_compute

  subroutine sf_ep_mapping_inverse (mapping, r, rb, f, p, pb, x_free)
    class(sf_ep_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(in) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(2) :: r2, px, p2
    real(default) :: f1, f2
    integer :: j
    do j = 1, 2
       r2(j) = r(mapping%i(j))
    end do
    call map_unit_square_inverse (r2, f, px)
    call map_endpoint_inverse_1 (px(1), f1, p2(1), mapping%a)
    call map_endpoint_inverse_01 (px(2), f2, p2(2), mapping%a)
    f = f * f1 * f2
    p = r
    pb= rb
    do j = 1, 2
       p (mapping%i(j)) = p2(j)
       pb(mapping%i(j)) = 1 - p2(j)
    end do
  end subroutine sf_ep_mapping_inverse

  subroutine sf_epr_mapping_write (object, unit)
    class(sf_epr_mapping_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A)", advance="no")  "map"
    if (any (object%i /= 0)) then
       write (u, "('(',I0,',',I0,')')", advance="no")  object%i
    end if
    if (object%resonance) then
       write (u, "(A,F7.5,A,F7.5,', ',F7.5,A)")  ": ep/res (a = ", object%a, &
            " | ", object%m, object%w, ")"
    else
       write (u, "(A,F7.5,A)")  ": ep/nores (a = ", object%a, ")"
    end if
  end subroutine sf_epr_mapping_write
  
  subroutine sf_epr_mapping_init (mapping, a, m, w)
    class(sf_epr_mapping_t), intent(out) :: mapping
    real(default), intent(in) :: a
    real(default), intent(in), optional :: m, w
    call mapping%base_init (2)
    mapping%a = a
    if (present (m) .and. present (w)) then
       mapping%m = m
       mapping%w = w
    else
       mapping%resonance = .false.
    end if
  end subroutine sf_epr_mapping_init
    
  subroutine sf_epr_mapping_compute (mapping, r, rb, f, p, pb, x_free)
    class(sf_epr_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(out) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(2) :: px, r2
    real(default) :: f1, f2
    integer :: j
    if (mapping%resonance) then
       call map_breit_wigner &
            (px(1), f1, p(mapping%i(1)), mapping%m, mapping%w, x_free)
    else
       px(1) = p(mapping%i(1))
       f1 = 1
    end if
    call map_endpoint_01 (px(2), f2, p(mapping%i(2)), mapping%a)
    call map_unit_square (r2, f, px)
    f = f * f1 * f2
    r = p
    rb= pb
    do j = 1, 2
       r (mapping%i(j)) = r2(j)
       rb(mapping%i(j)) = 1 - r2(j)
    end do
  end subroutine sf_epr_mapping_compute

  subroutine sf_epr_mapping_inverse (mapping, r, rb, f, p, pb, x_free)
    class(sf_epr_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(in) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(2) :: px, p2
    real(default) :: f1, f2
    integer :: j
    call map_unit_square_inverse (r(mapping%i), f, px)
    if (mapping%resonance) then
       call map_breit_wigner_inverse &
            (px(1), f1, p2(1), mapping%m, mapping%w, x_free)
    else
       p2(1) = px(1)
       f1 = 1
    end if
    call map_endpoint_inverse_01 (px(2), f2, p2(2), mapping%a)
    f = f * f1 * f2
    p = r
    pb= rb
    do j = 1, 2
       p (mapping%i(j)) = p2(j)
       pb(mapping%i(j)) = 1 - p2(j)
    end do
  end subroutine sf_epr_mapping_inverse

  subroutine sf_epo_mapping_write (object, unit)
    class(sf_epo_mapping_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A)", advance="no")  "map"
    if (any (object%i /= 0)) then
       write (u, "('(',I0,',',I0,')')", advance="no")  object%i
    end if
    write (u, "(A,F7.5,A,F7.5,A)")  ": ep/on-shell (a = ", object%a, &
         " | ", object%m, ")"
  end subroutine sf_epo_mapping_write
  
  subroutine sf_epo_mapping_init (mapping, a, m)
    class(sf_epo_mapping_t), intent(out) :: mapping
    real(default), intent(in) :: a, m
    call mapping%base_init (2)
    mapping%a = a
    mapping%m = m
    mapping%lm2 = abs (2 * log (mapping%m))
  end subroutine sf_epo_mapping_init
    
  subroutine sf_epo_mapping_compute (mapping, r, rb, f, p, pb, x_free)
    class(sf_epo_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(out) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(2) :: px, r2
    real(default) :: f2
    integer :: j
    px(1) = 0
    call map_endpoint_01 (px(2), f2, p(mapping%i(2)), mapping%a)
    call map_on_shell (r2, f, px, mapping%lm2)
    f = f * f2
    r = p
    rb= pb
    do j = 1, 2
       r (mapping%i(j)) = r2(j)
       rb(mapping%i(j)) = 1 - r2(j)
    end do
  end subroutine sf_epo_mapping_compute

  subroutine sf_epo_mapping_inverse (mapping, r, rb, f, p, pb, x_free)
    class(sf_epo_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(in) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(2) :: px, p2
    real(default) :: f2
    integer :: j
    call map_on_shell_inverse (r(mapping%i), f, px, mapping%lm2)
    p2(1) = 0
    call map_endpoint_inverse_01 (px(2), f2, p2(2), mapping%a)
    f = f * f2
    p = r
    pb= rb
    do j = 1, 2
       p (mapping%i(j)) = p2(j)
       pb(mapping%i(j)) = 1 - p2(j)
    end do
  end subroutine sf_epo_mapping_inverse

  subroutine sf_ip_mapping_write (object, unit)
    class(sf_ip_mapping_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A)", advance="no")  "map"
    if (any (object%i /= 0)) then
       write (u, "('(',I0,',',I0,')')", advance="no")  object%i
    end if
    write (u, "(A,ES12.5,A)")  ": isr (eps =", object%eps, ")"
  end subroutine sf_ip_mapping_write
  
  subroutine sf_ip_mapping_init (mapping, eps)
    class(sf_ip_mapping_t), intent(out) :: mapping
    real(default), intent(in), optional :: eps
    call mapping%base_init (2)
    if (present (eps))  mapping%eps = eps
    if (mapping%eps <= 0) &
         call msg_fatal ("ISR mapping: regulator epsilon must not be zero")
  end subroutine sf_ip_mapping_init
    
  subroutine sf_ip_mapping_compute (mapping, r, rb, f, p, pb, x_free)
    class(sf_ip_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(out) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(2) :: px, pxb, r2, r2b
    real(default) :: f1, f2, xb, y, yb
    integer :: j
    call map_power_1 (xb, f1, pb(mapping%i(1)), 2 * mapping%eps)
    call map_power_01 (y, yb, f2, pb(mapping%i(2)), mapping%eps)
    px(1)  = 1 - xb
    pxb(1) = xb
    px(2)  = y
    pxb(2) = yb
    call map_unit_square_prec (r2, r2b, f, px, pxb)
    f = f * f1 * f2
    r = p
    rb= pb
    do j = 1, 2
       r (mapping%i(j)) = r2 (j)
       rb(mapping%i(j)) = r2b(j)
    end do
  end subroutine sf_ip_mapping_compute

  subroutine sf_ip_mapping_inverse (mapping, r, rb, f, p, pb, x_free)
    class(sf_ip_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(in) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(2) :: r2, r2b, px, pxb, p2, p2b
    real(default) :: f1, f2, xb, y, yb
    integer :: j
    do j = 1, 2
       r2 (j) = r (mapping%i(j))
       r2b(j) = rb(mapping%i(j))
    end do
    call map_unit_square_inverse_prec (r2, r2b, f, px, pxb)
    xb = pxb(1)
    if (px(1) > 0) then
       y  = px(2)
       yb = pxb(2)
    else
       y  = 0.5_default
       yb = 0.5_default
    end if
    call map_power_inverse_1 (xb, f1, p2b(1), 2 * mapping%eps)
    call map_power_inverse_01 (y, yb, f2, p2b(2), mapping%eps)
    p2 = 1 - p2b
    f = f * f1 * f2
    p = r
    pb= rb
    do j = 1, 2
       p (mapping%i(j)) = p2(j)
       pb(mapping%i(j)) = p2b(j)
    end do
  end subroutine sf_ip_mapping_inverse

  subroutine sf_ipr_mapping_write (object, unit)
    class(sf_ipr_mapping_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A)", advance="no")  "map"
    if (any (object%i /= 0)) then
       write (u, "('(',I0,',',I0,')')", advance="no")  object%i
    end if
    if (object%resonance) then
       write (u, "(A,F7.5,A,F7.5,', ',F7.5,A)")  ": isr/res (eps = ", &
            object%eps, " | ", object%m, object%w, ")"
    else
       write (u, "(A,F7.5,A)")  ": isr/res (eps = ", object%eps, ")"
    end if
  end subroutine sf_ipr_mapping_write
  
  subroutine sf_ipr_mapping_init (mapping, eps, m, w)
    class(sf_ipr_mapping_t), intent(out) :: mapping
    real(default), intent(in), optional :: eps, m, w
    call mapping%base_init (2)
    if (present (eps))  mapping%eps = eps
    if (mapping%eps <= 0) &
         call msg_fatal ("ISR mapping: regulator epsilon must not be zero")
    if (present (m) .and. present (w)) then
       mapping%m = m
       mapping%w = w
    else
       mapping%resonance = .false.
    end if
  end subroutine sf_ipr_mapping_init
    
  subroutine sf_ipr_mapping_compute (mapping, r, rb, f, p, pb, x_free)
    class(sf_ipr_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(out) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(2) :: px, pxb, r2, r2b
    real(default) :: f1, f2, y, yb
    integer :: j
    if (mapping%resonance) then
       call map_breit_wigner &
            (px(1), f1, p(mapping%i(1)), mapping%m, mapping%w, x_free)
    else
       px(1) = p(mapping%i(1))
       f1 = 1
    end if
    call map_power_01 (y, yb, f2, pb(mapping%i(2)), mapping%eps)
    pxb(1) = 1 - px(1)
    px(2)  = y
    pxb(2) = yb
    call map_unit_square_prec (r2, r2b, f, px, pxb)
    f = f * f1 * f2
    r = p
    rb= pb
    do j = 1, 2
       r (mapping%i(j)) = r2 (j)
       rb(mapping%i(j)) = r2b(j)
    end do
  end subroutine sf_ipr_mapping_compute

  subroutine sf_ipr_mapping_inverse (mapping, r, rb, f, p, pb, x_free)
    class(sf_ipr_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(in) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(2) :: r2, r2b, px, pxb, p2, p2b
    real(default) :: f1, f2, y, yb
    integer :: j
    do j = 1, 2
       r2 (j) = r (mapping%i(j))
       r2b(j) = rb(mapping%i(j))
    end do
    call map_unit_square_inverse_prec (r2, r2b, f, px, pxb)
    if (px(1) > 0) then
       y  = px(2)
       yb = pxb(2)
    else
       y  = 0.5_default
       yb = 0.5_default
    end if
    if (mapping%resonance) then
       call map_breit_wigner_inverse &
            (px(1), f1, p2(1), mapping%m, mapping%w, x_free)
    else
       p2(1) = px(1)
       f1 = 1
    end if
    call map_power_inverse_01 (y, yb, f2, p2b(2), mapping%eps)
    p2b(1) = 1 - p2(1)
    p2 (2) = 1 - p2b(2)
    f = f * f1 * f2
    p = r
    pb= rb
    do j = 1, 2
       p (mapping%i(j)) = p2(j)
       pb(mapping%i(j)) = p2b(j)
    end do
  end subroutine sf_ipr_mapping_inverse

  subroutine sf_ipo_mapping_write (object, unit)
    class(sf_ipo_mapping_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A)", advance="no")  "map"
    if (any (object%i /= 0)) then
       write (u, "('(',I0,',',I0,')')", advance="no")  object%i
    end if
    write (u, "(A,F7.5,A,F7.5,A)")  ": isr/os (eps = ", object%eps, &
         " | ", object%m, ")"
  end subroutine sf_ipo_mapping_write
  
  subroutine sf_ipo_mapping_init (mapping, eps, m)
    class(sf_ipo_mapping_t), intent(out) :: mapping
    real(default), intent(in), optional :: eps, m
    call mapping%base_init (2)
    if (present (eps))  mapping%eps = eps
    if (mapping%eps <= 0) &
         call msg_fatal ("ISR mapping: regulator epsilon must not be zero")
    mapping%m = m
  end subroutine sf_ipo_mapping_init
    
  subroutine sf_ipo_mapping_compute (mapping, r, rb, f, p, pb, x_free)
    class(sf_ipo_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(out) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(2) :: px, pxb, r2, r2b
    real(default) :: f1, f2, y, yb
    integer :: j
    call map_power_01 (y, yb, f2, pb(mapping%i(2)), mapping%eps)
    px(1)  = mapping%m ** 2
    if (present (x_free))  px(1) = px(1) / x_free
    pxb(1) = 1 - px(1)
    px(2)  = y
    pxb(2) = yb
    call map_unit_square_prec (r2, r2b, f1, px, pxb)
    f = f1 * f2
    r = p
    rb= pb
    do j = 1, 2
       r (mapping%i(j)) = r2 (j)
       rb(mapping%i(j)) = r2b(j)
    end do
  end subroutine sf_ipo_mapping_compute

  subroutine sf_ipo_mapping_inverse (mapping, r, rb, f, p, pb, x_free)
    class(sf_ipo_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(in) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(2) :: r2, r2b, px, pxb, p2, p2b
    real(default) :: f1, f2, y, yb
    integer :: j
    do j = 1, 2
       r2 (j) = r (mapping%i(j))
       r2b(j) = rb(mapping%i(j))
    end do
    call map_unit_square_inverse_prec (r2, r2b, f1, px, pxb)
    y  = px(2)
    yb = pxb(2)
    call map_power_inverse_01 (y, yb, f2, p2b(2), mapping%eps)
    p2(1) = 0
    p2b(1)= 1
    p2(2) = 1 - p2b(2)
    f = f1 * f2
    p = r
    pb= rb
    do j = 1, 2
       p (mapping%i(j)) = p2(j)
       pb(mapping%i(j)) = p2b(j)
    end do
  end subroutine sf_ipo_mapping_inverse

  subroutine sf_ei_mapping_write (object, unit)
    class(sf_ei_mapping_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A)", advance="no")  "map"
    if (any (object%i /= 0)) then
       write (u, "('(',I0,3(',',I0),')')", advance="no")  object%i
    end if
    write (u, "(A,ES12.5,A,ES12.5,A)")  ": ep/isr (a =", object%ep%a, &
         ", eps =", object%ip%eps, ")"
  end subroutine sf_ei_mapping_write
  
  subroutine sf_ei_mapping_init (mapping, a, eps)
    class(sf_ei_mapping_t), intent(out) :: mapping
    real(default), intent(in), optional :: a, eps
    call mapping%base_init (4)
    call mapping%ep%init (a)
    call mapping%ip%init (eps)
  end subroutine sf_ei_mapping_init
    
  subroutine sf_ei_mapping_set_index (mapping, j, i)
    class(sf_ei_mapping_t), intent(inout) :: mapping
    integer, intent(in) :: j, i
    mapping%i(j) = i
    select case (j)
    case (1:2);  call mapping%ep%set_index (j, i)
    case (3:4);  call mapping%ip%set_index (j-2, i)
    end select
  end subroutine sf_ei_mapping_set_index
    
  subroutine sf_ei_mapping_compute (mapping, r, rb, f, p, pb, x_free)
    class(sf_ei_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(out) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(size(p)) :: q, qb
    real(default) :: f1, f2
    call mapping%ep%compute (q, qb, f1, p, pb, x_free)
    call mapping%ip%compute (r, rb, f2, q, qb, x_free)
    f = f1 * f2
  end subroutine sf_ei_mapping_compute

  subroutine sf_ei_mapping_inverse (mapping, r, rb, f, p, pb, x_free)
    class(sf_ei_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(in) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(size(p)) :: q, qb
    real(default) :: f1, f2
    call mapping%ip%inverse (r, rb, f2, q, qb, x_free)
    call mapping%ep%inverse (q, qb, f1, p, pb, x_free)
    f = f1 * f2
  end subroutine sf_ei_mapping_inverse

  subroutine sf_eir_mapping_write (object, unit)
    class(sf_eir_mapping_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A)", advance="no")  "map"
    if (any (object%i /= 0)) then
       write (u, "('(',I0,3(',',I0),')')", advance="no")  object%i
    end if
    write (u, "(A,F7.5,A,F7.5,A,F7.5,', ',F7.5,A)")  &
         ": ep/isr/res (a =", object%ep%a, &
         ", eps =", object%ip%eps, " | ", object%res%m, object%res%w, ")"
  end subroutine sf_eir_mapping_write
  
  subroutine sf_eir_mapping_init (mapping, a, eps, m, w)
    class(sf_eir_mapping_t), intent(out) :: mapping
    real(default), intent(in) :: a, eps, m, w
    call mapping%base_init (4)
    call mapping%res%init (m, w)
    call mapping%ep%init (a)
    call mapping%ip%init (eps)
  end subroutine sf_eir_mapping_init
    
  subroutine sf_eir_mapping_set_index (mapping, j, i)
    class(sf_eir_mapping_t), intent(inout) :: mapping
    integer, intent(in) :: j, i
    mapping%i(j) = i
    select case (j)
    case (1);  call mapping%res%set_index (1, i)
    case (3);  call mapping%res%set_index (2, i)
    end select
    select case (j)
    case (1:2);  call mapping%ep%set_index (j, i)
    case (3:4);  call mapping%ip%set_index (j-2, i)
    end select
  end subroutine sf_eir_mapping_set_index
    
  subroutine sf_eir_mapping_compute (mapping, r, rb, f, p, pb, x_free)
    class(sf_eir_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(out) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(size(p)) :: px, pxb, q, qb
    real(default) :: f0, f1, f2
    call mapping%res%compute (px, pxb, f0, p, pb, x_free)
    call mapping%ep%compute (q, qb, f1, px, pxb, x_free)
    call mapping%ip%compute (r, rb, f2, q, qb, x_free)
    f = f0 * f1 * f2
  end subroutine sf_eir_mapping_compute

  subroutine sf_eir_mapping_inverse (mapping, r, rb, f, p, pb, x_free)
    class(sf_eir_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(in) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(size(p)) :: px, pxb, q, qb
    real(default) :: f0, f1, f2
    call mapping%ip%inverse (r, rb, f2, q, qb, x_free)
    call mapping%ep%inverse (q, qb, f1, px, pxb, x_free)
    call mapping%res%inverse (px, pxb, f0, p, pb, x_free)
    f = f0 * f1 * f2
  end subroutine sf_eir_mapping_inverse

  subroutine sf_eio_mapping_write (object, unit)
    class(sf_eio_mapping_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A)", advance="no")  "map"
    if (any (object%i /= 0)) then
       write (u, "('(',I0,3(',',I0),')')", advance="no")  object%i
    end if
    write (u, "(A,F7.5,A,F7.5,A,F7.5,A)")  ": ep/isr/os (a =", object%ep%a, &
         ", eps =", object%ip%eps, " | ", object%os%m, ")"
  end subroutine sf_eio_mapping_write
  
  subroutine sf_eio_mapping_init (mapping, a, eps, m)
    class(sf_eio_mapping_t), intent(out) :: mapping
    real(default), intent(in), optional :: a, eps, m
    call mapping%base_init (4)
    call mapping%os%init (m)
    call mapping%ep%init (a)
    call mapping%ip%init (eps)
  end subroutine sf_eio_mapping_init
    
  subroutine sf_eio_mapping_set_index (mapping, j, i)
    class(sf_eio_mapping_t), intent(inout) :: mapping
    integer, intent(in) :: j, i
    mapping%i(j) = i
    select case (j)
    case (1);  call mapping%os%set_index (1, i)
    case (3);  call mapping%os%set_index (2, i)
    end select
    select case (j)
    case (1:2);  call mapping%ep%set_index (j, i)
    case (3:4);  call mapping%ip%set_index (j-2, i)
    end select
  end subroutine sf_eio_mapping_set_index
    
  subroutine sf_eio_mapping_compute (mapping, r, rb, f, p, pb, x_free)
    class(sf_eio_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(out) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(size(p)) :: px, pxb, q, qb
    real(default) :: f0, f1, f2
    call mapping%os%compute (px, pxb, f0, p, pb, x_free)
    call mapping%ep%compute (q, qb, f1, px, pxb, x_free)
    call mapping%ip%compute (r, rb, f2, q, qb, x_free)
    f = f0 * f1 * f2
  end subroutine sf_eio_mapping_compute

  subroutine sf_eio_mapping_inverse (mapping, r, rb, f, p, pb, x_free)
    class(sf_eio_mapping_t), intent(inout) :: mapping
    real(default), dimension(:), intent(in) :: r, rb
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: p, pb
    real(default), intent(inout), optional :: x_free
    real(default), dimension(size(p)) :: px, pxb, q, qb
    real(default) :: f0, f1, f2
    call mapping%ip%inverse (r, rb, f2, q, qb, x_free)
    call mapping%ep%inverse (q, qb, f1, px, pxb, x_free)
    call mapping%os%inverse (px, pxb, f0, p, pb, x_free)
    f = f0 * f1 * f2
  end subroutine sf_eio_mapping_inverse

  subroutine map_unit_square (r, factor, p, power)
    real(default), dimension(2), intent(out) :: r
    real(default), intent(out) :: factor
    real(default), dimension(2), intent(in) :: p
    real(default), intent(in), optional :: power
    real(default) :: xx, yy
    factor = 1
    xx = p(1)
    yy = p(2)
    if (present(power)) then
       if (p(1) > 0 .and. power > 1) then
          xx = p(1)**power
          factor = factor * power * xx / p(1)
       end if
    end if
    if (xx /= 0) then
       r(1) = xx ** yy
       r(2) = xx / r(1)
       factor = factor * abs (log (xx))
    else
       r = 0
    end if
  end subroutine map_unit_square

  subroutine map_unit_square_inverse (r, factor, p, power)
    real(kind=default), dimension(2), intent(in) :: r
    real(kind=default), intent(out) :: factor
    real(kind=default), dimension(2), intent(out) :: p
    real(kind=default), intent(in), optional :: power
    real(kind=default) :: lg, xx, yy
    factor = 1
    xx = r(1) * r(2)
    if (xx /= 0) then
       lg = log (xx)
       if (lg /= 0) then
          yy = log (r(1)) / lg
       else
          yy = 0
       end if
       p(2) = yy
       factor = factor * abs (lg)
       if (present(power)) then
          p(1) = xx**(1._default/power)
          factor = factor * power * xx / p(1)
       else
          p(1) = xx
       end if
    else
       p = 0
    end if
  end subroutine map_unit_square_inverse

  subroutine map_unit_square_prec (r, rb, factor, p, pb)
    real(default), dimension(2), intent(out) :: r
    real(default), dimension(2), intent(out) :: rb
    real(default), intent(out) :: factor
    real(default), dimension(2), intent(in) :: p
    real(default), dimension(2), intent(in) :: pb
    if (p(1) > 0.5_default) then
       call compute_prec_xy_1 (r(1), rb(1), p(1), pb(1), p (2))
       call compute_prec_xy_1 (r(2), rb(2), p(1), pb(1), pb(2))
       factor = - log_prec (p(1), pb(1))
    else if (p(1) > 0) then
       call compute_prec_xy_0 (r(1), rb(1), p(1), pb(1), p (2))
       call compute_prec_xy_0 (r(2), rb(2), p(1), pb(1), pb(2))
       factor = - log_prec (p(1), pb(1))
    else
       r  = 0
       rb = 1
       factor = 0
    end if
  end subroutine map_unit_square_prec

  subroutine map_unit_square_inverse_prec (r, rb, factor, p, pb)
    real(default), dimension(2), intent(in) :: r
    real(default), dimension(2), intent(in) :: rb
    real(default), intent(out) :: factor
    real(default), dimension(2), intent(out) :: p
    real(default), dimension(2), intent(out) :: pb
    call inverse_prec_x (r, rb, p(1), pb(1))
    if (all (r > 0)) then
       if (rb(1) < rb(2)) then
          call inverse_prec_y (r, rb, p(2), pb(2))
       else
          call inverse_prec_y ([r(2),r(1)], [rb(2),rb(1)], pb(2), p(2))
       end if
       factor = - log_prec (p(1), pb(1))
    else
       p(1)  = 0
       pb(1) = 1
       p(2)  = 0.5_default
       pb(2) = 0.5_default
       factor = 0
    end if
  end subroutine map_unit_square_inverse_prec

  subroutine compute_prec_xy_1 (z, zb, x, xb, y)
    real(default), intent(out) :: z, zb
    real(default), intent(in) :: x, xb, y
    real(default) :: a1, a2, a3
    a1 = y * xb
    a2 = a1 * (1 - y) * xb / 2
    a3 = a2 * (2 - y) * xb / 3
    if (abs (a3) < epsilon (a3)) then
       zb = a1 + a2 + a3
       z = 1 - zb
    else
       z = x ** y
       zb = 1 - z
    end if
  end subroutine compute_prec_xy_1
   
  subroutine compute_prec_xy_0 (z, zb, x, xb, y)
    real(default), intent(out) :: z, zb
    real(default), intent(in) :: x, xb, y
    real(default) :: a1, a2, a3, lx
    lx = -log (x)
    a1 = y * lx
    a2 = a1 * y * lx / 2
    a3 = a2 * y * lx / 3
    if (abs (a3) < epsilon (a3)) then
       zb = a1 + a2 + a3
       z = 1 - zb
    else
       z = x ** y
       zb = 1 - z
    end if
  end subroutine compute_prec_xy_0
   
  subroutine inverse_prec_x (r, rb, x, xb)
    real(default), dimension(2), intent(in) :: r, rb
    real(default), intent(out) :: x, xb
    real(default) :: a0, a1
    a0 = rb(1) + rb(2)
    a1 = rb(1) * rb(2)
    if (a0 > 0.5_default) then
       xb = a0 - a1
       x = 1 - xb
    else
       x = r(1) * r(2)
       xb = 1 - x
    end if
  end subroutine inverse_prec_x
    
  subroutine inverse_prec_y (r, rb, y, yb)
    real(default), dimension(2), intent(in) :: r, rb
    real(default), intent(out) :: y, yb
    real(default) :: log1, log2, a1, a2, a3
    log1 = log_prec (r(1), rb(1))
    log2 = log_prec (r(2), rb(2))
    a1 = - rb(1) / log2
    a2 = - rb(1) ** 2 * (1 / log2**2 + 1 / (2 * log2))
    a3 = - rb(1) ** 3 * (1 / log2**3 + 1 / log2**2 + 1 / (3 * log2))
    if (abs (a3) < epsilon (a3)) then
       y  = a1 + a2 + a3
       yb = 1 - y
    else
       y  = 1 / (1 + log2/log1)
       yb = 1 / (1 + log1/log2)
    end if
  end subroutine inverse_prec_y
  
  function log_prec (x, xb) result (lx)
    real(default), intent(in) :: x, xb
    real(default) :: a1, a2, a3, lx
    a1 = xb
    a2 = a1 * xb / 2
    a3 = a2 * xb * 2 / 3
    if (abs (a3) < epsilon (a3)) then
       lx = - a1 - a2 - a3
    else
       lx = log (x)
    end if
  end function log_prec
  
  subroutine map_on_shell (r, factor, p, lm2, x_free)
    real(default), dimension(2), intent(out) :: r
    real(default), intent(out) :: factor
    real(default), dimension(2), intent(in) :: p
    real(default), intent(in) :: lm2
    real(default), intent(in), optional :: x_free
    real(default) :: lx
    lx = lm2;  if (present (x_free))  lx = lx + log (x_free)
    r(1) = exp (- p(2) * lx)
    r(2) = exp (- (1 - p(2)) * lx)
    factor = lx
  end subroutine map_on_shell

  subroutine map_on_shell_inverse (r, factor, p, lm2, x_free)
    real(default), dimension(2), intent(in) :: r
    real(default), intent(out) :: factor
    real(default), dimension(2), intent(out) :: p
    real(default), intent(in) :: lm2
    real(default), intent(in), optional :: x_free
    real(default) :: lx
    lx = lm2;  if (present (x_free))  lx = lx + log (x_free)
    p(1) = 0
    p(2) = abs (log (r(1))) / lx
    factor = lx
  end subroutine map_on_shell_inverse
    
  subroutine map_breit_wigner (r, factor, p, m, w, x_free)
    real(default), intent(out) :: r
    real(default), intent(out) :: factor
    real(default), intent(in) :: p
    real(default), intent(in) :: m
    real(default), intent(in) :: w
    real(default), intent(in), optional :: x_free
    real(default) :: m2, mw, a1, a2, a3, z, tmp
    m2 = m ** 2
    mw = m * w
    if (present (x_free)) then
       m2 = m2 / x_free
       mw = mw / x_free
    end if
    a1 = atan (- m2 / mw)
    a2 = atan ((1 - m2) / mw)
    a3 = (a2 - a1) * mw
    z = (1-p) * a1 + p * a2
    if (-pi/2 < z .and. z < pi/2) then
       tmp = tan (z)
       r = max (m2 + mw * tmp, 0._default)
       factor = a3 * (1 + tmp ** 2) 
    else
       r = 0
       factor = 0
    end if
  end subroutine map_breit_wigner
    
  subroutine map_breit_wigner_inverse (r, factor, p, m, w, x_free)
    real(default), intent(in) :: r
    real(default), intent(out) :: factor
    real(default), intent(out) :: p
    real(default), intent(in) :: m
    real(default), intent(in) :: w
    real(default) :: m2, mw, a1, a2, a3, tmp
    real(default), intent(in), optional :: x_free
    m2 = m ** 2
    mw = m * w
    if (present (x_free)) then
       m2 = m2 / x_free
       mw = mw / x_free
    end if
    a1 = atan (- m2 / mw)
    a2 = atan ((1 - m2) / mw)
    a3 = (a2 - a1) * mw
    tmp = (r - m2) / mw
    p = (atan (tmp) - a1) / (a2 - a1)
    factor = a3 * (1 + tmp ** 2)
  end subroutine map_breit_wigner_inverse

  subroutine map_endpoint_1 (x3, factor, x1, a)
    real(default), intent(out) :: x3, factor
    real(default), intent(in) :: x1
    real(default), intent(in) :: a
    real(default) :: x2
    if (abs (x1) < 1) then
       x2 = tan (x1 * pi / 2)
       x3 = tanh (a * x2)
       factor = a * pi/2 * (1 + x2 ** 2) * (1 - x3 ** 2)
    else
       x3 = x1
       factor = 0
    end if
  end subroutine map_endpoint_1

  subroutine map_endpoint_inverse_1 (x3, factor, x1, a)
    real(default), intent(in) :: x3
    real(default), intent(out) :: x1, factor
    real(default), intent(in) :: a
    real(default) :: x2
    if (abs (x3) < 1) then
       x2 = atanh (x3) / a
       x1 = 2 / pi * atan (x2)
       factor = a * pi/2 * (1 + x2 ** 2) * (1 - x3 ** 2)
    else
       x1 = x3
       factor = 0
    end if
  end subroutine map_endpoint_inverse_1

  subroutine map_endpoint_01 (x4, factor, x0, a)
    real(default), intent(out) :: x4, factor
    real(default), intent(in) :: x0
    real(default), intent(in) :: a
    real(default) :: x1, x3
    x1 = 2 * x0 - 1
    call map_endpoint_1 (x3, factor, x1, a)
    x4 = (x3 + 1) / 2
  end subroutine map_endpoint_01

  subroutine map_endpoint_inverse_01 (x4, factor, x0, a)
    real(default), intent(in) :: x4
    real(default), intent(out) :: x0, factor
    real(default), intent(in) :: a
    real(default) :: x1, x3
    x3 = 2 * x4 - 1
    call map_endpoint_inverse_1 (x3, factor, x1, a)
    x0 = (x1 + 1) / 2
  end subroutine map_endpoint_inverse_01

  subroutine map_power_1 (xb, factor, rb, eps)
    real(default), intent(out) :: xb, factor
    real(default), intent(in) :: rb
    real(double) :: rb_db, factor_db, eps_db, xb_db
    real(default), intent(in) :: eps
    rb_db = real (rb, kind=double)
    eps_db = real (eps, kind=double)
    xb_db = rb_db ** (1 / eps_db)
    if (rb_db > 0) then
       factor_db = xb_db / rb_db / eps_db
       factor = real (factor_db, kind=default)
    else
       factor = 0
    end if
    xb = real (xb_db, kind=default)
  end subroutine map_power_1
    
  subroutine map_power_inverse_1 (xb, factor, rb, eps)
    real(default), intent(in) :: xb
    real(default), intent(out) :: rb, factor
    real(double) :: xb_db, factor_db, eps_db, rb_db
    real(default), intent(in) :: eps
    xb_db = real (xb, kind=double)
    eps_db = real (eps, kind=double)
    rb_db = xb_db ** eps_db
    if (xb_db > 0) then
       factor_db = xb_db / rb_db / eps_db
       factor = real (factor_db, kind=default)
    else
       factor = 0
    end if
    rb = real (rb_db, kind=default)
  end subroutine map_power_inverse_1
    
  subroutine map_power_01 (y, yb, factor, r, eps)
    real(default), intent(out) :: y, yb, factor
    real(default), intent(in) :: r
    real(default), intent(in) :: eps
    real(default) :: u, ub, zp, zm
    u = 2 * r - 1
    if (u > 0) then
       ub = 2 * (1 - r)
       call map_power_1 (zm, factor, ub, eps)
       zp = 2 - zm
    else if (u < 0) then
       ub = 2 * r
       call map_power_1 (zp, factor, ub, eps)
       zm = 2 - zp
    else
       factor = 1 / eps
       zp = 1
       zm = 1
    end if
    y  = zp / 2
    yb = zm / 2
  end subroutine map_power_01
    
  subroutine map_power_inverse_01 (y, yb, factor, r, eps)
    real(default), intent(in) :: y, yb
    real(default), intent(out) :: r, factor
    real(default), intent(in) :: eps
    real(default) :: ub, zp, zm
    zp = 2 * y
    zm = 2 * yb
    if (zm < zp) then
       call map_power_inverse_1 (zm, factor, ub, eps)
       r = 1 - ub / 2
    else if (zp < zm) then
       call map_power_inverse_1 (zp, factor, ub, eps)
       r = ub / 2
    else
       factor = 1 / eps
       ub = 1
       r = ub / 2
    end if
  end subroutine map_power_inverse_01
    
  subroutine sf_channel_write (object, unit)
    class(sf_channel_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, i
    u = output_unit (unit)
    if (allocated (object%map_code)) then
       do i = 1, size (object%map_code)
          select case (object%map_code (i))
          case (SFMAP_NONE)
             write (u, "(1x,A)", advance="no") "-"
          case (SFMAP_SINGLE)
             write (u, "(1x,A)", advance="no") "+"
          case (SFMAP_MULTI_S)
             write (u, "(1x,A)", advance="no") "s"
          case (SFMAP_MULTI_RES)
             write (u, "(1x,A)", advance="no") "r"
          case (SFMAP_MULTI_ONS)
             write (u, "(1x,A)", advance="no") "o"
          case (SFMAP_MULTI_EP)
             write (u, "(1x,A)", advance="no") "e"
          case (SFMAP_MULTI_EPR)
             write (u, "(1x,A)", advance="no") "p"
          case (SFMAP_MULTI_EPO)
             write (u, "(1x,A)", advance="no") "q"
          case (SFMAP_MULTI_IP)
             write (u, "(1x,A)", advance="no") "i"
          case (SFMAP_MULTI_IPR)
             write (u, "(1x,A)", advance="no") "i"
          case (SFMAP_MULTI_IPO)
             write (u, "(1x,A)", advance="no") "i"
          case (SFMAP_MULTI_EI)
             write (u, "(1x,A)", advance="no") "i"
          end select
       end do
    else
       write (u, "(1x,A)", advance="no") "-"
    end if
    if (allocated (object%multi_mapping)) then
       write (u, "(1x,'/')", advance="no")
       call object%multi_mapping%write (u)
    else
       write (u, *)
    end if
  end subroutine sf_channel_write
       
  subroutine sf_channel_init (channel, n_strfun)
    class(sf_channel_t), intent(out) :: channel
    integer, intent(in) :: n_strfun
    allocate (channel%map_code (n_strfun))
    channel%map_code = SFMAP_NONE
  end subroutine sf_channel_init
    
  subroutine sf_channel_assign (copy, original)
    class(sf_channel_t), intent(out) :: copy
    type(sf_channel_t), intent(in) :: original
    allocate (copy%map_code (size (original%map_code)))
    copy%map_code = original%map_code
    if (allocated (original%multi_mapping)) then
       allocate (copy%multi_mapping, source = original%multi_mapping)
    end if
  end subroutine sf_channel_assign
  
  subroutine allocate_sf_channels (channel, n_channel, n_strfun)
    type(sf_channel_t), dimension(:), intent(out), allocatable :: channel
    integer, intent(in) :: n_channel
    integer, intent(in) :: n_strfun
    integer :: c
    allocate (channel (n_channel))
    do c = 1, n_channel
       call channel(c)%init (n_strfun)
    end do
  end subroutine allocate_sf_channels

  subroutine sf_channel_activate_mapping (channel, i_sf)
    class(sf_channel_t), intent(inout) :: channel
    integer, dimension(:), intent(in) :: i_sf
    channel%map_code(i_sf) = SFMAP_SINGLE
  end subroutine sf_channel_activate_mapping
  
  subroutine sf_channel_set_s_mapping (channel, i_sf, power)
    class(sf_channel_t), intent(inout) :: channel
    integer, dimension(:), intent(in) :: i_sf
    real(default), intent(in), optional :: power
    channel%map_code(i_sf) = SFMAP_MULTI_S
    allocate (sf_s_mapping_t :: channel%multi_mapping)
    select type (mapping => channel%multi_mapping)
    type is (sf_s_mapping_t)
       call mapping%init (power)
    end select
  end subroutine sf_channel_set_s_mapping

  subroutine sf_channel_set_res_mapping (channel, i_sf, m, w)
    class(sf_channel_t), intent(inout) :: channel
    integer, dimension(:), intent(in) :: i_sf
    real(default), intent(in) :: m, w
    channel%map_code(i_sf) = SFMAP_MULTI_RES
    allocate (sf_res_mapping_t :: channel%multi_mapping)
    select type (mapping => channel%multi_mapping)
    type is (sf_res_mapping_t)
       call mapping%init (m, w)
    end select
  end subroutine sf_channel_set_res_mapping

  subroutine sf_channel_set_os_mapping (channel, i_sf, m)
    class(sf_channel_t), intent(inout) :: channel
    integer, dimension(:), intent(in) :: i_sf
    real(default), intent(in) :: m
    channel%map_code(i_sf) = SFMAP_MULTI_ONS
    allocate (sf_os_mapping_t :: channel%multi_mapping)
    select type (mapping => channel%multi_mapping)
    type is (sf_os_mapping_t)
       call mapping%init (m)
    end select
  end subroutine sf_channel_set_os_mapping

  subroutine sf_channel_set_ep_mapping (channel, i_sf, a)
    class(sf_channel_t), intent(inout) :: channel
    integer, dimension(:), intent(in) :: i_sf
    real(default), intent(in), optional :: a
    channel%map_code(i_sf) = SFMAP_MULTI_EP
    allocate (sf_ep_mapping_t :: channel%multi_mapping)
    select type (mapping => channel%multi_mapping)
    type is (sf_ep_mapping_t)
       call mapping%init (a = a)
    end select
  end subroutine sf_channel_set_ep_mapping

  subroutine sf_channel_set_epr_mapping (channel, i_sf, a, m, w)
    class(sf_channel_t), intent(inout) :: channel
    integer, dimension(:), intent(in) :: i_sf
    real(default), intent(in) :: a, m, w
    channel%map_code(i_sf) = SFMAP_MULTI_EPR
    allocate (sf_epr_mapping_t :: channel%multi_mapping)
    select type (mapping => channel%multi_mapping)
    type is (sf_epr_mapping_t)
       call mapping%init (a, m, w)
    end select
  end subroutine sf_channel_set_epr_mapping

  subroutine sf_channel_set_epo_mapping (channel, i_sf, a, m)
    class(sf_channel_t), intent(inout) :: channel
    integer, dimension(:), intent(in) :: i_sf
    real(default), intent(in) :: a, m
    channel%map_code(i_sf) = SFMAP_MULTI_EPO
    allocate (sf_epo_mapping_t :: channel%multi_mapping)
    select type (mapping => channel%multi_mapping)
    type is (sf_epo_mapping_t)
       call mapping%init (a, m)
    end select
  end subroutine sf_channel_set_epo_mapping

  subroutine sf_channel_set_ip_mapping (channel, i_sf, eps)
    class(sf_channel_t), intent(inout) :: channel
    integer, dimension(:), intent(in) :: i_sf
    real(default), intent(in), optional :: eps
    channel%map_code(i_sf) = SFMAP_MULTI_IP
    allocate (sf_ip_mapping_t :: channel%multi_mapping)
    select type (mapping => channel%multi_mapping)
    type is (sf_ip_mapping_t)
       call mapping%init (eps)
    end select
  end subroutine sf_channel_set_ip_mapping

  subroutine sf_channel_set_ipr_mapping (channel, i_sf, eps, m, w)
    class(sf_channel_t), intent(inout) :: channel
    integer, dimension(:), intent(in) :: i_sf
    real(default), intent(in), optional :: eps, m, w
    channel%map_code(i_sf) = SFMAP_MULTI_IPR
    allocate (sf_ipr_mapping_t :: channel%multi_mapping)
    select type (mapping => channel%multi_mapping)
    type is (sf_ipr_mapping_t)
       call mapping%init (eps, m, w)
    end select
  end subroutine sf_channel_set_ipr_mapping

  subroutine sf_channel_set_ipo_mapping (channel, i_sf, eps, m)
    class(sf_channel_t), intent(inout) :: channel
    integer, dimension(:), intent(in) :: i_sf
    real(default), intent(in), optional :: eps, m
    channel%map_code(i_sf) = SFMAP_MULTI_IPO
    allocate (sf_ipo_mapping_t :: channel%multi_mapping)
    select type (mapping => channel%multi_mapping)
    type is (sf_ipo_mapping_t)
       call mapping%init (eps, m)
    end select
  end subroutine sf_channel_set_ipo_mapping

  subroutine sf_channel_set_ei_mapping (channel, i_sf, a, eps)
    class(sf_channel_t), intent(inout) :: channel
    integer, dimension(:), intent(in) :: i_sf
    real(default), intent(in), optional :: a, eps
    channel%map_code(i_sf) = SFMAP_MULTI_EI
    allocate (sf_ei_mapping_t :: channel%multi_mapping)
    select type (mapping => channel%multi_mapping)
    type is (sf_ei_mapping_t)
       call mapping%init (a, eps)
    end select
  end subroutine sf_channel_set_ei_mapping

  subroutine sf_channel_set_eir_mapping (channel, i_sf, a, eps, m, w)
    class(sf_channel_t), intent(inout) :: channel
    integer, dimension(:), intent(in) :: i_sf
    real(default), intent(in), optional :: a, eps, m, w
    channel%map_code(i_sf) = SFMAP_MULTI_EI
    allocate (sf_eir_mapping_t :: channel%multi_mapping)
    select type (mapping => channel%multi_mapping)
    type is (sf_eir_mapping_t)
       call mapping%init (a, eps, m, w)
    end select
  end subroutine sf_channel_set_eir_mapping

  subroutine sf_channel_set_eio_mapping (channel, i_sf, a, eps, m)
    class(sf_channel_t), intent(inout) :: channel
    integer, dimension(:), intent(in) :: i_sf
    real(default), intent(in), optional :: a, eps, m
    channel%map_code(i_sf) = SFMAP_MULTI_EI
    allocate (sf_eio_mapping_t :: channel%multi_mapping)
    select type (mapping => channel%multi_mapping)
    type is (sf_eio_mapping_t)
       call mapping%init (a, eps, m)
    end select
  end subroutine sf_channel_set_eio_mapping

  function sf_channel_is_single_mapping (channel, i_sf) result (flag)
    class(sf_channel_t), intent(in) :: channel
    integer, intent(in) :: i_sf
    logical :: flag
    flag = channel%map_code(i_sf) == SFMAP_SINGLE
  end function sf_channel_is_single_mapping 

  function sf_channel_is_multi_mapping (channel, i_sf) result (flag)
    class(sf_channel_t), intent(in) :: channel
    integer, intent(in) :: i_sf
    logical :: flag
    select case (channel%map_code(i_sf))
    case (SFMAP_NONE, SFMAP_SINGLE)
       flag = .false.
    case default
       flag = .true.
    end select
  end function sf_channel_is_multi_mapping 

  function any_sf_channel_has_mapping (channel) result (flag)
    type(sf_channel_t), dimension(:), intent(in) :: channel
    logical :: flag
    integer :: c
    flag = .false.
    do c = 1, size (channel)
       flag = flag .or. any (channel(c)%map_code /= SFMAP_NONE)
    end do
  end function any_sf_channel_has_mapping
  
  subroutine sf_channel_set_par_index (channel, j, i_par)
    class(sf_channel_t), intent(inout) :: channel
    integer, intent(in) :: j
    integer, intent(in) :: i_par
    call channel%multi_mapping%set_index (j, i_par)
  end subroutine sf_channel_set_par_index
    

  subroutine sf_mappings_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (sf_mappings_1, "sf_mappings_1", &
         "standard pair mapping", &
         u, results)
    call test (sf_mappings_2, "sf_mappings_2", &
         "structure-function mapping channels", &
         u, results)
    call test (sf_mappings_3, "sf_mappings_3", &
         "resonant pair mapping", &
         u, results)
    call test (sf_mappings_4, "sf_mappings_4", &
         "on-shell pair mapping", &
         u, results)
    call test (sf_mappings_5, "sf_mappings_5", &
         "endpoint pair mapping", &
         u, results)
    call test (sf_mappings_6, "sf_mappings_6", &
         "endpoint resonant mapping", &
         u, results)
    call test (sf_mappings_7, "sf_mappings_7", &
         "endpoint on-shell mapping", &
         u, results)
    call test (sf_mappings_8, "sf_mappings_8", &
         "power pair mapping", &
         u, results)
    call test (sf_mappings_9, "sf_mappings_9", &
         "power resonance mapping", &
         u, results)
    call test (sf_mappings_10, "sf_mappings_10", &
         "power on-shell mapping", &
         u, results)
    call test (sf_mappings_11, "sf_mappings_11", &
         "endpoint/power combined mapping", &
         u, results)
    call test (sf_mappings_12, "sf_mappings_12", &
         "endpoint/power resonant combined mapping", &
         u, results)
    call test (sf_mappings_13, "sf_mappings_13", &
         "endpoint/power on-shell combined mapping", &
         u, results)
    call test (sf_mappings_14, "sf_mappings_14", &
         "rescaled on-shell mapping", &
         u, results)
  end subroutine sf_mappings_test
  
  subroutine sf_mappings_1 (u)
    integer, intent(in) :: u
    class(sf_mapping_t), allocatable :: mapping
    real(default), dimension(2) :: p
    
    write (u, "(A)")  "* Test output: sf_mappings_1"
    write (u, "(A)")  "*   Purpose: probe standard mapping"
    write (u, "(A)")
    
    allocate (sf_s_mapping_t :: mapping)
    select type (mapping)
    type is (sf_s_mapping_t)
       call mapping%init ()
       call mapping%set_index (1, 1)
       call mapping%set_index (2, 2)
    end select
       
    call mapping%write (u)
    
    write (u, *)
    write (u, "(A)")  "Probe at (0,0):"
    p = [0._default, 0._default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.5,0.5):"
    p = [0.5_default, 0.5_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.1,0.5):"
    p = [0.1_default, 0.5_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.1,0.1):"
    p = [0.1_default, 0.1_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Compute integral:"
    write (u, "(3x,A,1x,F7.5)") "I =", mapping%integral (100000)

    deallocate (mapping)
    allocate (sf_s_mapping_t :: mapping)
    select type (mapping)
    type is (sf_s_mapping_t)
       call mapping%init (power=2._default)
       call mapping%set_index (1, 1)
       call mapping%set_index (2, 2)
    end select
       
    write (u, *)
    call mapping%write (u)
    
    write (u, *)
    write (u, "(A)")  "Probe at (0,0):"
    p = [0._default, 0._default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.5,0.5):"
    p = [0.5_default, 0.5_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.1,0.5):"
    p = [0.1_default, 0.5_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.1,0.1):"
    p = [0.1_default, 0.1_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Compute integral:"
    write (u, "(3x,A,1x,F7.5)")  "I =", mapping%integral (100000)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_mappings_1"

  end subroutine sf_mappings_1
  
  subroutine sf_mappings_2 (u)
    integer, intent(in) :: u
    type(sf_channel_t), dimension(:), allocatable :: channel
    integer :: c
    
    write (u, "(A)")  "* Test output: sf_mappings_2"
    write (u, "(A)")  "*   Purpose: construct and display &
         &mapping-channel objects"
    write (u, "(A)")
    
    call allocate_sf_channels (channel, n_channel = 6, n_strfun = 2)
    call channel(2)%activate_mapping ([1])
    call channel(3)%set_s_mapping ([1,2])
    call channel(4)%set_s_mapping ([1,2], power=2._default)
    call channel(5)%set_res_mapping ([1,2], m = 0.5_default, w = 0.1_default)
    call channel(6)%set_os_mapping ([1,2], m = 0.5_default)
    
    call channel(3)%set_par_index (1, 1)
    call channel(3)%set_par_index (2, 4)

    call channel(4)%set_par_index (1, 1)
    call channel(4)%set_par_index (2, 4)

    call channel(5)%set_par_index (1, 1)
    call channel(5)%set_par_index (2, 3)

    call channel(6)%set_par_index (1, 1)
    call channel(6)%set_par_index (2, 2)

    do c = 1, size (channel)
       write (u, "(I0,':')", advance="no")  c
       call channel(c)%write (u)
    end do

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_mappings_2"

  end subroutine sf_mappings_2
  
  subroutine sf_mappings_3 (u)
    integer, intent(in) :: u
    class(sf_mapping_t), allocatable :: mapping
    real(default), dimension(2) :: p
    
    write (u, "(A)")  "* Test output: sf_mappings_3"
    write (u, "(A)")  "*   Purpose: probe resonance pair mapping"
    write (u, "(A)")
    
    allocate (sf_res_mapping_t :: mapping)
    select type (mapping)
    type is (sf_res_mapping_t)
       call mapping%init (0.5_default, 0.1_default)
       call mapping%set_index (1, 1)
       call mapping%set_index (2, 2)
    end select
       
    call mapping%write (u)
    
    write (u, *)
    write (u, "(A)")  "Probe at (0,0):"
    p = [0._default, 0._default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.5,0.5):"
    p = [0.5_default, 0.5_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.1,0.5):"
    p = [0.1_default, 0.5_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.1,0.1):"
    p = [0.1_default, 0.1_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Compute integral:"
    write (u, "(3x,A,1x,F7.5)") "I =", mapping%integral (100000)

    deallocate (mapping)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_mappings_3"

  end subroutine sf_mappings_3
  
  subroutine sf_mappings_4 (u)
    integer, intent(in) :: u
    class(sf_mapping_t), allocatable :: mapping
    real(default), dimension(2) :: p
    
    write (u, "(A)")  "* Test output: sf_mappings_4"
    write (u, "(A)")  "*   Purpose: probe on-shell pair mapping"
    write (u, "(A)")
    
    allocate (sf_os_mapping_t :: mapping)
    select type (mapping)
    type is (sf_os_mapping_t)
       call mapping%init (0.5_default)
       call mapping%set_index (1, 1)
       call mapping%set_index (2, 2)
    end select
       
    call mapping%write (u)
    
    write (u, *)
    write (u, "(A)")  "Probe at (0,0):"
    p = [0._default, 0._default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.5,0.5):"
    p = [0.5_default, 0.5_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0,0.1):"
    p = [0._default, 0.1_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0,1.0):"
    p = [0._default, 1.0_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Compute integral:"
    write (u, "(3x,A,1x,F7.5)") "I =", mapping%integral (100000)

    deallocate (mapping)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_mappings_4"

  end subroutine sf_mappings_4
  
  subroutine sf_mappings_5 (u)
    integer, intent(in) :: u
    class(sf_mapping_t), allocatable :: mapping
    real(default), dimension(2) :: p
    
    write (u, "(A)")  "* Test output: sf_mappings_5"
    write (u, "(A)")  "*   Purpose: probe endpoint pair mapping"
    write (u, "(A)")
    
    allocate (sf_ep_mapping_t :: mapping)
    select type (mapping)
    type is (sf_ep_mapping_t)
       call mapping%init ()
       call mapping%set_index (1, 1)
       call mapping%set_index (2, 2)
    end select
       
    call mapping%write (u)
    
    write (u, *)
    write (u, "(A)")  "Probe at (0,0):"
    p = [0._default, 0._default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.5,0.5):"
    p = [0.5_default, 0.5_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.1,0.5):"
    p = [0.1_default, 0.5_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.7,0.2):"
    p = [0.7_default, 0.2_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Compute integral:"
    write (u, "(3x,A,1x,F7.5)") "I =", mapping%integral (100000)

    deallocate (mapping)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_mappings_5"

  end subroutine sf_mappings_5
  
  subroutine sf_mappings_6 (u)
    integer, intent(in) :: u
    class(sf_mapping_t), allocatable :: mapping
    real(default), dimension(2) :: p
    
    write (u, "(A)")  "* Test output: sf_mappings_6"
    write (u, "(A)")  "*   Purpose: probe endpoint resonant mapping"
    write (u, "(A)")
    
    allocate (sf_epr_mapping_t :: mapping)
    select type (mapping)
    type is (sf_epr_mapping_t)
       call mapping%init (a = 1._default, m = 0.5_default, w = 0.1_default)
       call mapping%set_index (1, 1)
       call mapping%set_index (2, 2)
    end select
       
    call mapping%write (u)
    
    write (u, *)
    write (u, "(A)")  "Probe at (0,0):"
    p = [0._default, 0._default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.5,0.5):"
    p = [0.5_default, 0.5_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.1,0.5):"
    p = [0.1_default, 0.5_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.7,0.2):"
    p = [0.7_default, 0.2_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Compute integral:"
    write (u, "(3x,A,1x,F7.5)") "I =", mapping%integral (100000)

    deallocate (mapping)

    write (u, "(A)")
    write (u, "(A)")  "* Same mapping without resonance:"
    write (u, "(A)")

    allocate (sf_epr_mapping_t :: mapping)
    select type (mapping)
    type is (sf_epr_mapping_t)
       call mapping%init (a = 1._default)
       call mapping%set_index (1, 1)
       call mapping%set_index (2, 2)
    end select
       
    call mapping%write (u)
    
    write (u, *)
    write (u, "(A)")  "Probe at (0,0):"
    p = [0._default, 0._default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.5,0.5):"
    p = [0.5_default, 0.5_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.1,0.5):"
    p = [0.1_default, 0.5_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.7,0.2):"
    p = [0.7_default, 0.2_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Compute integral:"
    write (u, "(3x,A,1x,F7.5)") "I =", mapping%integral (100000)

    deallocate (mapping)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_mappings_6"

  end subroutine sf_mappings_6
  
  subroutine sf_mappings_7 (u)
    integer, intent(in) :: u
    class(sf_mapping_t), allocatable :: mapping
    real(default), dimension(2) :: p
    
    write (u, "(A)")  "* Test output: sf_mappings_7"
    write (u, "(A)")  "*   Purpose: probe endpoint on-shell mapping"
    write (u, "(A)")
    
    allocate (sf_epo_mapping_t :: mapping)
    select type (mapping)
    type is (sf_epo_mapping_t)
       call mapping%init (a = 1._default, m = 0.5_default)
       call mapping%set_index (1, 1)
       call mapping%set_index (2, 2)
    end select
       
    call mapping%write (u)
    
    write (u, *)
    write (u, "(A)")  "Probe at (0,0):"
    p = [0._default, 0._default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.5,0.5):"
    p = [0.5_default, 0.5_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.1,0.5):"
    p = [0.1_default, 0.5_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Probe at (0.7,0.2):"
    p = [0.7_default, 0.2_default]
    call mapping%check (u, p, 1-p, "F7.5")

    write (u, *)
    write (u, "(A)")  "Compute integral:"
    write (u, "(3x,A,1x,F7.5)") "I =", mapping%integral (100000)

    deallocate (mapping)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_mappings_7"

  end subroutine sf_mappings_7
  
  subroutine sf_mappings_8 (u)
    integer, intent(in) :: u
    class(sf_mapping_t), allocatable :: mapping
    real(default), dimension(2) :: p, pb
    
    write (u, "(A)")  "* Test output: sf_mappings_8"
    write (u, "(A)")  "*   Purpose: probe power pair mapping"
    write (u, "(A)")
    
    allocate (sf_ip_mapping_t :: mapping)
    select type (mapping)
    type is (sf_ip_mapping_t)
       call mapping%init (eps = 0.1_default)
       call mapping%set_index (1, 1)
       call mapping%set_index (2, 2)
    end select
       
    call mapping%write (u)
    
    write (u, *)
    write (u, "(A)")  "Probe at (0,0.5):"
    p = [0._default, 0.5_default]
    pb= [1._default, 0.5_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0.5,0.5):"
    p = [0.5_default, 0.5_default]
    pb= [0.5_default, 0.5_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0.9,0.5):"
    p = [0.9_default, 0.5_default]
    pb= [0.1_default, 0.5_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0.7,0.2):"
    p = [0.7_default, 0.2_default]
    pb= [0.3_default, 0.8_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0.7,0.8):"
    p = [0.7_default, 0.8_default]
    pb= [0.3_default, 0.2_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0.99,0.02):"
    p = [0.99_default, 0.02_default]
    pb= [0.01_default, 0.98_default]
    call mapping%check (u, p, pb, FMT_14, FMT_12)

    write (u, *)
    write (u, "(A)")  "Probe at (0.99,0.98):"
    p = [0.99_default, 0.98_default]
    pb= [0.01_default, 0.02_default]
    call mapping%check (u, p, pb, FMT_14, FMT_12)

    write (u, *)
    write (u, "(A)")  "Compute integral:"
    write (u, "(3x,A,1x,F7.5)") "I =", mapping%integral (100000)

    deallocate (mapping)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_mappings_8"

  end subroutine sf_mappings_8
  
  subroutine sf_mappings_9 (u)
    integer, intent(in) :: u
    class(sf_mapping_t), allocatable :: mapping
    real(default), dimension(2) :: p, pb
    
    write (u, "(A)")  "* Test output: sf_mappings_9"
    write (u, "(A)")  "*   Purpose: probe power resonant pair mapping"
    write (u, "(A)")
    
    allocate (sf_ipr_mapping_t :: mapping)
    select type (mapping)
    type is (sf_ipr_mapping_t)
       call mapping%init (eps = 0.1_default, m = 0.5_default, w = 0.1_default)
       call mapping%set_index (1, 1)
       call mapping%set_index (2, 2)
    end select
       
    call mapping%write (u)
    
    write (u, *)
    write (u, "(A)")  "Probe at (0,0.5):"
    p = [0._default, 0.5_default]
    pb= [1._default, 0.5_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0.5,0.5):"
    p = [0.5_default, 0.5_default]
    pb= [0.5_default, 0.5_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0.9,0.5):"
    p = [0.9_default, 0.5_default]
    pb= [0.1_default, 0.5_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0.7,0.2):"
    p = [0.7_default, 0.2_default]
    pb= [0.3_default, 0.8_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0.7,0.8):"
    p = [0.7_default, 0.8_default]
    pb= [0.3_default, 0.2_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0.9999,0.02):"
    p = [0.9999_default, 0.02_default]
    pb= [0.0001_default, 0.98_default]
    call mapping%check (u, p, pb, FMT_15, FMT_12)

    write (u, *)
    write (u, "(A)")  "Probe at (0.9999,0.98):"
    p = [0.9999_default, 0.98_default]
    pb= [0.0001_default, 0.02_default]
    call mapping%check (u, p, pb, FMT_15, FMT_12)

    write (u, *)
    write (u, "(A)")  "Compute integral:"
    write (u, "(3x,A,1x,F7.5)") "I =", mapping%integral (100000)

    deallocate (mapping)

    write (u, "(A)")
    write (u, "(A)")  "* Same mapping without resonance:"
    write (u, "(A)")
    
    allocate (sf_ipr_mapping_t :: mapping)
    select type (mapping)
    type is (sf_ipr_mapping_t)
       call mapping%init (eps = 0.1_default)
       call mapping%set_index (1, 1)
       call mapping%set_index (2, 2)
    end select
       
    call mapping%write (u)
    
    write (u, *)
    write (u, "(A)")  "Probe at (0,0.5):"
    p = [0._default, 0.5_default]
    pb= [1._default, 0.5_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0.5,0.5):"
    p = [0.5_default, 0.5_default]
    pb= [0.5_default, 0.5_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0.9,0.5):"
    p = [0.9_default, 0.5_default]
    pb= [0.1_default, 0.5_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0.7,0.2):"
    p = [0.7_default, 0.2_default]
    pb= [0.3_default, 0.8_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0.7,0.8):"
    p = [0.7_default, 0.8_default]
    pb= [0.3_default, 0.2_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Compute integral:"
    write (u, "(3x,A,1x,F7.5)") "I =", mapping%integral (100000)

    deallocate (mapping)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_mappings_9"

  end subroutine sf_mappings_9
  
  subroutine sf_mappings_10 (u)
    integer, intent(in) :: u
    class(sf_mapping_t), allocatable :: mapping
    real(default), dimension(2) :: p, pb
    
    write (u, "(A)")  "* Test output: sf_mappings_10"
    write (u, "(A)")  "*   Purpose: probe power on-shell mapping"
    write (u, "(A)")
    
    allocate (sf_ipo_mapping_t :: mapping)
    select type (mapping)
    type is (sf_ipo_mapping_t)
       call mapping%init (eps = 0.1_default, m = 0.5_default)
       call mapping%set_index (1, 1)
       call mapping%set_index (2, 2)
    end select
       
    call mapping%write (u)
    
    write (u, *)
    write (u, "(A)")  "Probe at (0,0.5):"
    p = [0._default, 0.5_default]
    pb= [1._default, 0.5_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0,0.02):"
    p = [0._default, 0.02_default]
    pb= [1._default, 0.98_default]
    call mapping%check (u, p, pb, FMT_15, FMT_12)

    write (u, *)
    write (u, "(A)")  "Probe at (0,0.98):"
    p = [0._default, 0.98_default]
    pb= [1._default, 0.02_default]
    call mapping%check (u, p, pb, FMT_15, FMT_12)

    write (u, *)
    write (u, "(A)")  "Compute integral:"
    write (u, "(3x,A,1x,F7.5)") "I =", mapping%integral (100000)

    deallocate (mapping)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_mappings_10"

  end subroutine sf_mappings_10
  
  subroutine sf_mappings_11 (u)
    integer, intent(in) :: u
    class(sf_mapping_t), allocatable :: mapping
    real(default), dimension(4) :: p, pb
    
    write (u, "(A)")  "* Test output: sf_mappings_11"
    write (u, "(A)")  "*   Purpose: probe power pair mapping"
    write (u, "(A)")
    
    allocate (sf_ei_mapping_t :: mapping)
    select type (mapping)
    type is (sf_ei_mapping_t)
       call mapping%init (eps = 0.1_default)
       call mapping%set_index (1, 1)
       call mapping%set_index (2, 2)
       call mapping%set_index (3, 3)
       call mapping%set_index (4, 4)
    end select
       
    call mapping%write (u)
    
    write (u, *)
    write (u, "(A)")  "Probe at (0.5, 0.5, 0.5, 0.5):"
    p = [0.5_default, 0.5_default, 0.5_default, 0.5_default]
    pb= [0.5_default, 0.5_default, 0.5_default, 0.5_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0.7, 0.2, 0.4, 0.8):"
    p = [0.7_default, 0.2_default, 0.4_default, 0.8_default]
    pb= [0.3_default, 0.8_default, 0.6_default, 0.2_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0.9, 0.06, 0.95, 0.1):"
    p = [0.9_default, 0.06_default, 0.95_default, 0.1_default]
    pb= [0.1_default, 0.94_default, 0.05_default, 0.9_default]
    call mapping%check (u, p, pb, FMT_13, FMT_12)

    write (u, *)
    write (u, "(A)")  "Compute integral:"
    write (u, "(3x,A,1x,F7.5)") "I =", mapping%integral (100000)

    deallocate (mapping)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_mappings_11"

  end subroutine sf_mappings_11
  
  subroutine sf_mappings_12 (u)
    integer, intent(in) :: u
    class(sf_mapping_t), allocatable :: mapping
    real(default), dimension(4) :: p, pb
    
    write (u, "(A)")  "* Test output: sf_mappings_12"
    write (u, "(A)")  "*   Purpose: probe resonant combined mapping"
    write (u, "(A)")
    
    allocate (sf_eir_mapping_t :: mapping)
    select type (mapping)
    type is (sf_eir_mapping_t)
       call mapping%init (a = 1._default, &
            eps = 0.1_default, m = 0.5_default, w = 0.1_default)
       call mapping%set_index (1, 1)
       call mapping%set_index (2, 2)
       call mapping%set_index (3, 3)
       call mapping%set_index (4, 4)
    end select
       
    call mapping%write (u)
    
    write (u, *)
    write (u, "(A)")  "Probe at (0.5, 0.5, 0.5, 0.5):"
    p = [0.5_default, 0.5_default, 0.5_default, 0.5_default]
    pb= [0.5_default, 0.5_default, 0.5_default, 0.5_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0.7, 0.2, 0.4, 0.8):"
    p = [0.7_default, 0.2_default, 0.4_default, 0.8_default]
    pb= [0.3_default, 0.8_default, 0.6_default, 0.2_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0.9, 0.06, 0.95, 0.1):"
    p = [0.9_default, 0.06_default, 0.95_default, 0.1_default]
    pb= [0.1_default, 0.94_default, 0.05_default, 0.9_default]
    call mapping%check (u, p, pb, FMT_15, FMT_12)

    write (u, *)
    write (u, "(A)")  "Compute integral:"
    write (u, "(3x,A,1x,F7.5)") "I =", mapping%integral (100000)

    deallocate (mapping)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_mappings_12"

  end subroutine sf_mappings_12
  
  subroutine sf_mappings_13 (u)
    integer, intent(in) :: u
    class(sf_mapping_t), allocatable :: mapping
    real(default), dimension(4) :: p, pb
    
    write (u, "(A)")  "* Test output: sf_mappings_13"
    write (u, "(A)")  "*   Purpose: probe on-shell combined mapping"
    write (u, "(A)")
    
    allocate (sf_eio_mapping_t :: mapping)
    select type (mapping)
    type is (sf_eio_mapping_t)
       call mapping%init (a = 1._default, eps = 0.1_default, m = 0.5_default)
       call mapping%set_index (1, 1)
       call mapping%set_index (2, 2)
       call mapping%set_index (3, 3)
       call mapping%set_index (4, 4)
    end select
       
    call mapping%write (u)
    
    write (u, *)
    write (u, "(A)")  "Probe at (0.5, 0.5, 0.5, 0.5):"
    p = [0.5_default, 0.5_default, 0.5_default, 0.5_default]
    pb= [0.5_default, 0.5_default, 0.5_default, 0.5_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0.7, 0.2, 0.4, 0.8):"
    p = [0.7_default, 0.2_default, 0.4_default, 0.8_default]
    pb= [0.3_default, 0.8_default, 0.6_default, 0.2_default]
    call mapping%check (u, p, pb, FMT_16)

    write (u, *)
    write (u, "(A)")  "Probe at (0.9, 0.06, 0.95, 0.1):"
    p = [0.9_default, 0.06_default, 0.95_default, 0.1_default]
    pb= [0.1_default, 0.94_default, 0.05_default, 0.9_default]
    call mapping%check (u, p, pb, FMT_14, FMT_12)

    write (u, *)
    write (u, "(A)")  "Compute integral:"
    write (u, "(3x,A,1x,F7.5)") "I =", mapping%integral (100000)

    deallocate (mapping)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_mappings_13"

  end subroutine sf_mappings_13
  
  subroutine sf_mappings_14 (u)
    integer, intent(in) :: u
    real(default), dimension(2) :: p, r
    real(default) :: f, x_free, m2
    
    write (u, "(A)")  "* Test output: sf_mappings_14"
    write (u, "(A)")  "*   Purpose: probe rescaling in os mapping"
    write (u, "(A)")
    
    p = [0.1_default, 0.2_default]
    x_free = 0.9_default
    m2 = 0.5_default
    
    call map_on_shell (r, f, p, -log (m2), x_free)
    
    write (u, "(A,9(1x," // FMT_14 // "))")  "p =", p
    write (u, "(A,9(1x," // FMT_14 // "))")  "r =", r
    write (u, "(A,9(1x," // FMT_14 // "))")  "f =", f
    write (u, "(A,9(1x," // FMT_14 // "))")  "*r=", x_free * product (r)

    write (u, *)

    call map_on_shell_inverse (r, f, p, -log (m2), x_free)
    
    write (u, "(A,9(1x," // FMT_14 // "))")  "p =", p
    write (u, "(A,9(1x," // FMT_14 // "))")  "r =", r
    write (u, "(A,9(1x," // FMT_14 // "))")  "f =", f
    write (u, "(A,9(1x," // FMT_14 // "))")  "*r=", x_free * product (r)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_mappings_14"

  end subroutine sf_mappings_14
  

end module sf_mappings
