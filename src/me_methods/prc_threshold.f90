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
module prc_threshold

  use, intrinsic :: iso_c_binding !NODEP!

  use kinds
  use constants
  use io_units
  use iso_varying_string, string_t => varying_string
  use system_defs, only: TAB
  use diagnostics
  use os_interface
  use lorentz
  use interactions
  use sm_qcd

  use prclib_interfaces
  use process_libraries
  use prc_core_def
  use prc_core
  use prc_user_defined
 
  implicit none
  private

  public :: threshold_writer_t
  public :: threshold_get_amp_squared
  public :: threshold_init
  public :: threshold_driver_t
  public :: threshold_def_t
  public :: prc_threshold_t

  interface
    subroutine threshold_get_amp_squared (amp2, p) bind(C)
      import
      real(c_default_float), intent(out) :: amp2
      real(c_default_float), dimension(0:3,*), intent(in) :: p
    end subroutine threshold_get_amp_squared
  end interface

  interface
   subroutine threshold_init (par) bind(C)
      import
      real(c_default_float), dimension(*), intent(in) :: par
    end subroutine threshold_init
  end interface


  type, extends (prc_user_defined_writer_t) :: threshold_writer_t
  contains
    procedure :: write_makefile_code => threshold_writer_write_makefile_code
    procedure, nopass :: type_name => threshold_writer_type_name
  end type threshold_writer_t

  type, extends (user_defined_driver_t) :: threshold_driver_t
    procedure(threshold_get_amp_squared), nopass, pointer :: &
         get_amp_squared => null ()
    procedure(threshold_init), nopass, pointer :: &
         init => null ()
  contains
    procedure, nopass :: type_name => threshold_driver_type_name
    procedure :: load => threshold_driver_load
  end type threshold_driver_t

  type, extends (user_defined_def_t) :: threshold_def_t
  contains
    procedure :: init => threshold_def_init
    procedure, nopass :: type_string => threshold_def_type_string
    procedure :: write => threshold_def_write
    procedure :: read => threshold_def_read
    procedure :: allocate_driver => threshold_def_allocate_driver
    procedure :: connect => threshold_def_connect
  end type threshold_def_t

  type, extends (prc_user_defined_base_t) :: prc_threshold_t
  contains
    procedure :: write => prc_threshold_write
    procedure :: compute_amplitude => prc_threshold_compute_amplitude
    procedure :: allocate_workspace => prc_threshold_allocate_workspace
    procedure :: compute_sqme => prc_threshold_compute_sqme
    procedure :: init => prc_threshold_init
    procedure :: activate_parameters => prc_threshold_activate_parameters
  end type prc_threshold_t


contains

  subroutine threshold_writer_write_makefile_code (writer, unit, id, os_data, testflag)
    class(threshold_writer_t), intent(in) :: writer
    integer, intent(in) :: unit
    type(string_t), intent(in) :: id
    type(os_data_t), intent(in) :: os_data
    logical, intent(in), optional :: testflag
    type(string_t) :: f90in, f90, lo
    call writer%base_write_makefile_code (unit, id, os_data, testflag)
    f90 = id // "_threshold.f90"
    f90in = f90 // ".in"
    lo = id // "_threshold.lo"
    write (unit, "(A)") "OBJECTS += " // char (lo)
    write (unit, "(A)") char (f90in) // ":"
    write (unit, "(A)") TAB // "if ! test -f " // char (f90in) // &
         "; then cp " // char (os_data%whizard_sharepath) // &
         "/SM_tt_threshold_data/threshold.f90 " // &
         char (f90in) // "; fi"
    write (unit, "(A)") char(f90) // ": " // char (f90in)
    write (unit, "(A)") TAB // "sed 's/@ID@/" // char (id) // "/' " // &
         char (f90in) // " > " // char (f90)
    write (unit, "(5A)")  "CLEAN_SOURCES += ", char (f90)
    write (unit, "(5A)")  "CLEAN_OBJECTS += ", char (f90in)
    write (unit, "(5A)")  "CLEAN_OBJECTS += ", char (id), "_threshold.mod"
    write (unit, "(5A)")  "CLEAN_OBJECTS += ", char (lo)
    write (unit, "(A)") char(lo) // ": " // char (f90) // " " // &
         char(id) // ".f90"
    write (unit, "(5A)")  TAB, "$(LTFCOMPILE) $<"
  end subroutine threshold_writer_write_makefile_code

  function threshold_writer_type_name () result (string)
    type(string_t) :: string
    string = "Threshold"
  end function threshold_writer_type_name

  function threshold_driver_type_name () result (type)
    type(string_t) :: type
    type = "Threshold"
  end function threshold_driver_type_name

  subroutine threshold_driver_load (threshold_driver, dlaccess)
    class(threshold_driver_t), intent(inout) :: threshold_driver
    type(dlaccess_t), intent(inout) :: dlaccess
    logical :: success
    type(c_funptr) :: c_fptr
    ! TODO: (bcn 2015-08-24) use the id in the function name to avoid clashes
    c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("threshold_get_amp_squared"))
    call c_f_procpointer (c_fptr, threshold_driver%get_amp_squared)
    success = .not. dlaccess_has_error (dlaccess)
    c_fptr = dlaccess_get_c_funptr (dlaccess, var_str ("threshold_init"))
    call c_f_procpointer (c_fptr, threshold_driver%init)
    success = success .and. .not. dlaccess_has_error (dlaccess)
    if (.not. success) then
       call msg_fatal ("Loading of extra threshold functions has failed!")
    else
       call msg_message ("Loaded extra threshold functions")
    end if
  end subroutine threshold_driver_load

  subroutine threshold_def_init (object, basename, model_name, &
       prt_in, prt_out, restrictions)
    class(threshold_def_t), intent(inout) :: object
    type(string_t), intent(in) :: basename, model_name
    type(string_t), dimension(:), intent(in) :: prt_in, prt_out
    type(string_t), intent(in), optional :: restrictions
    object%basename = basename
    allocate (threshold_writer_t :: object%writer)
    select type (writer => object%writer)
    type is (threshold_writer_t)
       call writer%init (model_name, prt_in, prt_out, restrictions)
    end select
  end subroutine threshold_def_init

  function threshold_def_type_string () result (string)
    type(string_t) :: string
    string = "threshold computation"
  end function threshold_def_type_string

  subroutine threshold_def_write (object, unit)
    class(threshold_def_t), intent(in) :: object
    integer, intent(in) :: unit
  end subroutine threshold_def_write

  subroutine threshold_def_read (object, unit)
    class(threshold_def_t), intent(out) :: object
    integer, intent(in) :: unit
  end subroutine threshold_def_read

  subroutine threshold_def_allocate_driver (object, driver, basename)
    class(threshold_def_t), intent(in) :: object
    class(prc_core_driver_t), intent(out), allocatable :: driver
    type(string_t), intent(in) :: basename
    if (.not. allocated (driver)) allocate (threshold_driver_t :: driver)
  end subroutine threshold_def_allocate_driver

  subroutine threshold_def_connect (def, lib_driver, i, proc_driver)
    class(threshold_def_t), intent(in) :: def
    class(prclib_driver_t), intent(in) :: lib_driver
    integer, intent(in) :: i
    class(prc_core_driver_t), intent(inout) :: proc_driver
    type(dlaccess_t) :: dlaccess
    call def%omega_connect (lib_driver, i, proc_driver)
    select type (lib_driver)
    class is (prclib_driver_dynamic_t)
       dlaccess = lib_driver%dlaccess
    end select
    select type (proc_driver)
    class is (threshold_driver_t)
       call proc_driver%load (dlaccess)
    end select
  end subroutine threshold_def_connect

  subroutine prc_threshold_write (object, unit)
    class(prc_threshold_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    call msg_message ("Supply amplitudes squared for threshold computation")
  end subroutine prc_threshold_write

  function prc_threshold_compute_amplitude &
       (object, j, p, f, h, c, fac_scale, ren_scale, alpha_qcd_forced, &
       core_state)  result (amp)
    class(prc_threshold_t), intent(in) :: object
    integer, intent(in) :: j
    type(vector4_t), dimension(:), intent(in) :: p
    integer, intent(in) :: f, h, c
    real(default), intent(in) :: fac_scale, ren_scale
    real(default), intent(in), allocatable :: alpha_qcd_forced
    class(prc_core_state_t), intent(inout), allocatable, optional :: core_state
    complex(default) :: amp
    select type (core_state)
    class is (user_defined_test_state_t)
       core_state%alpha_qcd = object%qcd%alpha%get (fac_scale)
    end select
    amp = 1.0
  end function prc_threshold_compute_amplitude

  subroutine prc_threshold_allocate_workspace (object, core_state)
    class(prc_threshold_t), intent(in) :: object
    class(prc_core_state_t), intent(inout), allocatable :: core_state
    allocate (user_defined_test_state_t :: core_state)
  end subroutine prc_threshold_allocate_workspace

  function prc_threshold_compute_sqme (object, i_flv, p) result (sqme)
     real(default) :: sqme
     class(prc_threshold_t), intent(in) :: object
     integer, intent(in) :: i_flv
     type(vector4_t), dimension(:), intent(in) :: p
     integer :: n_tot, i
     real(c_default_float), dimension(:,:), allocatable, save :: parray
     n_tot = size (p)
     if (.not. allocated (parray)) then
        allocate (parray (0:3, n_tot))
     end if
     forall (i = 1:n_tot)  parray(:,i) = p(i)%p
     select type (driver => object%driver)
     class is (threshold_driver_t)
        call driver%get_amp_squared (sqme, parray)
     end select
  end function prc_threshold_compute_sqme

  subroutine prc_threshold_init (object, def, lib, id, i_component)
    class(prc_threshold_t), intent(inout) :: object
    class(prc_core_def_t), intent(in), target :: def
    type(process_library_t), intent(in), target :: lib
    type(string_t), intent(in) :: id
    integer, intent(in) :: i_component
    call object%base_init (def, lib, id, i_component)
    call object%activate_parameters ()
  end subroutine prc_threshold_init
    
  subroutine prc_threshold_activate_parameters (object)
    class (prc_threshold_t), intent(inout) :: object
    if (allocated (object%driver)) then
       if (allocated (object%par)) then
          select type (driver => object%driver)
          type is (threshold_driver_t)
             if (associated (driver%init))  call driver%init (object%par)
          end select
       else
          call msg_bug ("prc_threshold_activate: parameter set is not allocated")
       end if
    else
       call msg_bug ("prc_threshold_activate: driver is not allocated")
    end if
  end subroutine prc_threshold_activate_parameters
    

end module prc_threshold
