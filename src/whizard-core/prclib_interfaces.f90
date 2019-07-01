! WHIZARD 2.0.5 Tue May 10 2011
! 
! Copyright (C) 1999-2011 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     Christian Speckner <christian.speckner@physik.uni-freiburg.de>
!     with contributions by Sebastian Schmidt, Daniel Wiesler, Felix Braam
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

module prclib_interfaces

  use iso_varying_string, string_t => varying_string !NODEP!
  use iso_c_binding !NODEP!
  use kinds !NODEP!

  implicit none
  private

  public :: prc_get_n_processes
  public :: prc_get_stringptr
  public :: prc_get_int
  public :: prc_get_log
  public :: prc_set_int_tab1
  public :: prc_set_int_tab2
  public :: prc_set_cf_tab
  public :: prc_init
  public :: prc_final
  public :: prc_update_alpha_s
  public :: prc_reset_helicity_selection
  public :: prc_new_event
  public :: prc_is_allowed
  public :: prc_get_amplitude
  public :: prc_get_fptr
  public :: prclib_unload_hook
  public :: prclib_reload_hook

  abstract interface
     function prc_get_n_processes () result (n) bind(C)
       import
       integer(c_int) :: n
     end function prc_get_n_processes
  end interface
  abstract interface
     subroutine prc_get_stringptr (i, cptr, len) bind(C)
       import
       integer(c_int), intent(in) :: i
       type(c_ptr), intent(out) :: cptr
       integer(c_int), intent(out) :: len
     end subroutine prc_get_stringptr
  end interface
  abstract interface
     function prc_get_int (pid) result (n) bind(C)
       import
       integer(c_int), intent(in) :: pid
       integer(c_int) :: n
     end function prc_get_int
  end interface
  abstract interface
     function prc_get_log (pid) result (l) bind(C)
       import
       integer(c_int), intent(in) :: pid
       logical(c_bool) :: l
     end function prc_get_log
  end interface
  abstract interface
     subroutine prc_set_int_tab1 (pid, cptr, shape) bind(C)
       import
       integer(c_int), intent(in) :: pid
       type(c_ptr), intent(in) :: cptr
       integer(c_int), dimension(2), intent(in) :: shape
     end subroutine prc_set_int_tab1
  end interface
  abstract interface
     subroutine prc_set_int_tab2 (pid, cptr, shape, lcptr, lshape) bind(C)
       import
       integer(c_int), intent(in) :: pid
       type(c_ptr), intent(in) :: cptr
       integer(c_int), dimension(3), intent(in) :: shape
       type(c_ptr), intent(in) :: lcptr
       integer(c_int), dimension(2), intent(in) :: lshape
     end subroutine prc_set_int_tab2
  end interface

  abstract interface
     subroutine prc_set_cf_tab (pid, iptr1, iptr2, cptr, shape) bind(C)
       import
       integer(c_int), intent(in) :: pid
       type(c_ptr), intent(in) :: iptr1, iptr2, cptr
       integer(c_int), dimension(1), intent(in) :: shape
     end subroutine prc_set_cf_tab
  end interface

  abstract interface
     subroutine prc_init (par) bind(C)
       import
       real(c_default_float), dimension(*), intent(in) :: par
     end subroutine prc_init
  end interface
  abstract interface
     subroutine prc_final () bind(C)
     end subroutine prc_final
  end interface
 interface
     subroutine prc_update_alpha_s (alpha_s) bind(C)
       import
       real(c_default_float), intent(in) :: alpha_s
     end subroutine prc_update_alpha_s
  end interface
  abstract interface
     subroutine prc_reset_helicity_selection (threshold, cutoff) bind(C)
       import
       real(c_default_float), intent(in) :: threshold
       integer(c_int), intent(in) :: cutoff
     end subroutine prc_reset_helicity_selection
  end interface
  abstract interface
     subroutine prc_new_event (p) bind(C)
       import
       real(c_default_float), dimension(0:3,*), intent(in) :: p
     end subroutine prc_new_event
  end interface
  abstract interface
     function prc_is_allowed (flv, hel, col) result (is_allowed) bind(C)
       import
       logical(c_bool) :: is_allowed
       integer(c_int), intent(in) :: flv, hel, col
     end function prc_is_allowed
  end interface
  abstract interface
     function prc_get_amplitude (flv, hel, col) result (amp) bind(C)
       import
       complex(c_default_complex) :: amp
       integer(c_int), intent(in) :: flv, hel, col
     end function prc_get_amplitude
  end interface

  abstract interface
     subroutine prc_get_fptr (pid, fptr) bind(C)
       import
       integer(c_int), intent(in) :: pid
       type(c_funptr), intent(out) :: fptr
     end subroutine prc_get_fptr
  end interface
  abstract interface
     subroutine prclib_unload_hook (libname)
       import
       type(string_t), intent(in) :: libname
     end subroutine prclib_unload_hook

     subroutine prclib_reload_hook (libname)
       import
       type(string_t), intent(in) :: libname
     end subroutine prclib_reload_hook
  end interface

end module prclib_interfaces
