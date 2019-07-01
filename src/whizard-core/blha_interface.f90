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

module blha_interface

  use iso_varying_string, string_t => varying_string !NODEP!
  use constants !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use sm_physics !NODEP!
  use md5
  use lorentz !NODEP!
  use models
  use flavors
  use quantum_numbers
  use interactions
  use evaluators
  use particles
  use quantum_numbers
  use blha_config
  use, intrinsic :: iso_c_binding !NODEP!
  use os_interface

  implicit none
  private

  public :: blha_olp_t
  public :: blha_olp_init
  public :: blha_olp_final
  public :: blha_interface_test

  type :: blha_olp_t
     private
     type(blha_configuration_t) :: cfg
     type(string_t) :: library
     integer :: n_in, n_out, n_flv, n_hel, n_col
     integer, dimension(:,:), allocatable :: flv_state
     logical :: color_summed = .true., flavor_summed = .true.
     logical :: loaded = .false.
     type(dlaccess_t) :: lib_handle
     procedure(ext_olp_start), pointer, nopass :: olp_start => null ()
     procedure(ext_olp_evalsubprocess), pointer, nopass :: &
        olp_evalsubprocess => null ()
     procedure(ext_olp_finalize), pointer, nopass :: olp_finalize => null ()
     procedure(ext_olp_option), pointer, nopass :: olp_option => null ()
  end type blha_olp_t


  abstract interface
     subroutine ext_olp_start (file, status) bind(c)
       import
       character(c_char), dimension(*), intent(in) :: file
       integer(c_int), intent(out) :: status
     end subroutine ext_olp_Start

     subroutine ext_olp_evalsubprocess &
          (label, momenta, scale, parameters, amp) bind(c)
       import
       integer(c_int), intent(in), value :: label
       real(c_double), dimension(*), intent(in) :: momenta
       real(c_double), intent(in), value :: scale
       real(c_double), dimension(*), intent(in) :: parameters
       real(c_double), dimension(*), intent(out) :: amp
     end subroutine ext_olp_evalsubprocess

     subroutine ext_olp_finalize () bind(c)
     end subroutine ext_olp_finalize

     subroutine ext_olp_option (assignment, status) bind(c)
       import
       character(c_char), dimension(*), intent(in) :: assignment
       integer(c_int), intent(out) :: status
     end subroutine ext_olp_option
  end interface


contains

  subroutine blha_olp_init (olp, cfg, library, success)
    type(blha_olp_t), intent(out) :: olp
    type(string_t), intent(in), optional :: library
    type(blha_configuration_t), intent(in) :: cfg
    logical, intent(out), optional :: success
    type(blha_cfg_process_node_t), pointer :: node
    type(string_t) :: prefix, libname
    type(c_funptr) :: fptr
    integer :: olp_status
    success = .true.
    node => cfg%processes
    if (.not. associated (node)) then
       call error ("blha_interface_init: empty process list")
       return
    end if
    olp%n_in = size (node%pdg_in)
    olp%n_out = size (node%pdg_out)
    do while (associated (node))
       if ((olp%n_in /= size (node%pdg_in)) .or. &
             (olp%n_out /= size (node%pdg_out))) then
          call error ("blha_interface_init: inconsistent process list")
          return
       end if
       node => node%next
    end do
    if (present (library)) then
       olp%library = library
    else
       olp%library = cfg%name // ".so"
    end if
    if (char (extract (olp%library, 1, 1)) == "/") then
       prefix = ""
       libname = extract (olp%library, 2)
    else
       prefix = "."
       libname = olp%library
    end if
    call dlaccess_init (olp%lib_handle, prefix, libname)
    if (dlaccess_has_error (olp%lib_handle)) then
       call error ("blha_interface_init: error opening library: " // &
          char (dlaccess_get_error (olp%lib_handle)))
       call dlaccess_final (olp%lib_handle)
       return
    end if
    fptr = dlaccess_get_c_funptr (olp%lib_handle, var_str ("OLP_Start"))
    if (.not. check_dlstate ()) return
    call c_f_procpointer (fptr, olp%olp_start)
    fptr = dlaccess_get_c_funptr (olp%lib_handle, var_str ("OLP_EvalSubProcess"))
    if (.not. check_dlstate ()) return
    call c_f_procpointer (fptr, olp%olp_evalsubprocess)
    if (olp%cfg%mode == BLHA_MODE_GOSAM) then
       fptr = dlaccess_get_c_funptr (olp%lib_handle, var_str ("OLP_Finalize"))
       if (.not. check_dlstate ()) return
       call c_f_procpointer (fptr, olp%olp_finalize)
       fptr = dlaccess_get_c_funptr (olp%lib_handle, var_str ("OLP_Option"))
       if (.not. check_dlstate ()) return
       call c_f_procpointer (fptr, olp%olp_option)
    end if
    call olp%olp_start (string_f2c (cfg%model_file), olp_status)
    if (olp_status /= 1) then
       call error ("blha_interface_init: OLP initialization failed")
       call dlaccess_final (olp%lib_handle)
    end if
    success = .true.
    olp%loaded = .true.

  contains

    function check_dlstate () result (ok)
      logical :: ok
      ok = .not. dlaccess_has_error (olp%lib_handle)
      if (.not. ok) then
         call error ("blha_interface_init: error loading library: " // &
              char (dlaccess_get_error (olp%lib_handle)))
         call dlaccess_final (olp%lib_handle)
      end if
    end function check_dlstate
   
    subroutine error (msg)
      character(*), intent(in) :: msg
      if (present (success)) then
         call msg_error (msg)
         success = .false.
      else
         call msg_fatal (msg)
      end if
    end subroutine error

  end subroutine blha_olp_init

  subroutine blha_olp_final (olp)
    type(blha_olp_t), intent(inout) :: olp
    if (.not. olp%loaded) return
    if (associated (olp%olp_finalize)) call olp%olp_finalize
    call dlaccess_final (olp%lib_handle)
    olp%loaded = .false.
  end subroutine blha_olp_final

  subroutine blha_interface_test (cfg, ok)
    type(blha_configuration_t), intent(inout) :: cfg
    type(blha_olp_t) :: olp
    logical, intent(out) :: ok
    call blha_olp_init (olp, cfg, library=var_str ("blha_test.so"), success=ok)
    print *, "loading OLP library: success?", ok
    call blha_olp_final (olp)
  end subroutine blha_interface_test


end module blha_interface
