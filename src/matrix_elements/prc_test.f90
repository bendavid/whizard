! WHIZARD 2.2.4 Feb 06 2015
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

module prc_test
  
  use iso_c_binding !NODEP!
  use kinds
  use iso_varying_string, string_t => varying_string
  use unit_tests
  use os_interface

  use process_constants
  use prclib_interfaces
  use prc_core_def
  use particle_specifiers
  use process_libraries

  implicit none
  private

  public :: prc_test_def_t
  public :: prc_test_t
  public :: prc_test_create_library
  public :: prc_test_test

  type, extends (prc_core_def_t) :: prc_test_def_t
     type(string_t) :: model_name
     type(string_t), dimension(:), allocatable :: prt_in
     type(string_t), dimension(:), allocatable :: prt_out
   contains
     procedure, nopass :: type_string => prc_test_def_type_string
     procedure, nopass :: get_features => prc_test_def_get_features
     procedure :: init => prc_test_def_init
     procedure :: write => prc_test_def_write
     procedure :: read => prc_test_def_read
     procedure :: allocate_driver => prc_test_def_allocate_driver
     procedure :: connect => prc_test_def_connect
  end type prc_test_def_t
  
  type, extends (process_driver_internal_t) :: prc_test_t
     type(string_t) :: id
     type(string_t) :: model_name
     logical :: scattering = .true.
   contains
     procedure, nopass :: get_amplitude => prc_test_get_amplitude
     procedure, nopass :: type_name => prc_test_type_name
     procedure :: fill_constants => prc_test_fill_constants
  end type prc_test_t


contains
  
  function prc_test_def_type_string () result (string)
    type(string_t) :: string
    string = "test_me"
  end function prc_test_def_type_string

  subroutine prc_test_def_get_features (features)
    type(string_t), dimension(:), allocatable, intent(out) :: features
    allocate (features (0))
  end subroutine prc_test_def_get_features

  subroutine prc_test_def_init (object, model_name, prt_in, prt_out)
    class(prc_test_def_t), intent(out) :: object
    type(string_t), intent(in) :: model_name
    type(string_t), dimension(:), intent(in) :: prt_in
    type(string_t), dimension(:), intent(in) :: prt_out
    object%model_name = model_name
    allocate (object%prt_in (size (prt_in)))
    object%prt_in = prt_in
    allocate (object%prt_out (size (prt_out)))
    object%prt_out = prt_out
  end subroutine prc_test_def_init

  subroutine prc_test_def_write (object, unit)
    class(prc_test_def_t), intent(in) :: object
    integer, intent(in) :: unit
  end subroutine prc_test_def_write
  
  subroutine prc_test_def_read (object, unit)
    class(prc_test_def_t), intent(out) :: object
    integer, intent(in) :: unit
  end subroutine prc_test_def_read
  
  subroutine prc_test_def_allocate_driver (object, driver, basename)
    class(prc_test_def_t), intent(in) :: object
    class(prc_core_driver_t), intent(out), allocatable :: driver
    type(string_t), intent(in) :: basename
    allocate (prc_test_t :: driver)
    select type (driver)
    type is (prc_test_t)
       driver%id = basename
       driver%model_name = object%model_name
       select case (size (object%prt_in))
       case (1);  driver%scattering = .false.
       case (2);  driver%scattering = .true.
       end select
    end select
  end subroutine prc_test_def_allocate_driver
  
  subroutine prc_test_def_connect (def, lib_driver, i, proc_driver)
    class(prc_test_def_t), intent(in) :: def
    class(prclib_driver_t), intent(in) :: lib_driver
    integer, intent(in) :: i
    class(prc_core_driver_t), intent(inout) :: proc_driver
  end subroutine prc_test_def_connect

  function prc_test_get_amplitude (p) result (amp)
    complex(default) :: amp
    real(default), dimension(:,:), intent(in) :: p
    amp = 1
  end function prc_test_get_amplitude

  function prc_test_type_name () result (string)
    type(string_t) :: string
    string = "test_me"
  end function prc_test_type_name

  subroutine prc_test_fill_constants (driver, data)
    class(prc_test_t), intent(in) :: driver
    type(process_constants_t), intent(out) :: data
    data%id = driver%id
    data%model_name = driver%model_name
    if (driver%scattering) then
       data%n_in  = 2
       data%n_out = 2
       data%n_flv = 1
       data%n_hel = 1
       data%n_col = 1
       data%n_cin = 2
       data%n_cf  = 1
       allocate (data%flv_state (4, 1))
       data%flv_state = 25
       allocate (data%hel_state (4, 1))
       data%hel_state = 0
       allocate (data%col_state (2, 4, 1))
       data%col_state = 0
       allocate (data%ghost_flag (4, 1))
       data%ghost_flag = .false.
       allocate (data%color_factors (1))
       data%color_factors = 1
       allocate (data%cf_index (2, 1))
       data%cf_index = 1
    else
       data%n_in  = 1
       data%n_out = 2
       data%n_flv = 1
       data%n_hel = 2
       data%n_col = 1
       data%n_cin = 2
       data%n_cf  = 1
       allocate (data%flv_state (3, 1))
       data%flv_state(:,1) = [25, 6, -6]
       allocate (data%hel_state (3, 2))
       data%hel_state(:,1) = [0, 1,-1]
       data%hel_state(:,2) = [0,-1, 1]
       allocate (data%col_state (2, 3, 1))
       data%col_state = reshape ([0,0, 1,0, 0,-1], [2,3,1])
       allocate (data%ghost_flag (3, 1))
       data%ghost_flag = .false.
       allocate (data%color_factors (1))
       data%color_factors = 3
       allocate (data%cf_index (2, 1))
       data%cf_index = 1
    end if
  end subroutine prc_test_fill_constants
  
  subroutine prc_test_create_library &
       (libname, lib, scattering, decay, procname1, procname2)
    type(string_t), intent(in) :: libname
    type(process_library_t), intent(out) :: lib
    logical, intent(in), optional :: scattering, decay
    type(string_t), intent(in), optional :: procname1, procname2
    type(string_t) :: model_name, procname
    type(string_t), dimension(:), allocatable :: prt_in, prt_out
    class(prc_core_def_t), allocatable :: def
    type(process_def_entry_t), pointer :: entry
    type(os_data_t) :: os_data
    logical :: sca, dec
    sca = .true.;   if (present (scattering))  sca = scattering
    dec = .false.;  if (present (decay))       dec = decay

    call os_data_init (os_data)
    call lib%init (libname)
    model_name = "Test"

    if (sca) then
       if (present (procname1)) then
          procname = procname1
       else
          procname = libname
       end if
       allocate (prt_in (2), prt_out (2))
       prt_in  = [var_str ("s"), var_str ("s")]
       prt_out = [var_str ("s"), var_str ("s")]
       allocate (prc_test_def_t :: def)
       select type (def)
       type is (prc_test_def_t)
          call def%init (model_name, prt_in, prt_out)
       end select
       allocate (entry)
       call entry%init (procname, model_name = model_name, &
            n_in = 2, n_components = 1)
       call entry%import_component (1, n_out = size (prt_out), &
            prt_in  = new_prt_spec (prt_in), &
            prt_out = new_prt_spec (prt_out), &
            method  = var_str ("test_me"), &
            variant = def)
       call lib%append (entry)
    end if

    if (dec) then
       if (present (procname2)) then
          procname = procname2
       else
          procname = libname
       end if
       if (allocated (prt_in))  deallocate (prt_in, prt_out)
       allocate (prt_in (1), prt_out (2))
       prt_in  = [var_str ("s")]
       prt_out = [var_str ("f"), var_str ("fbar")]
       allocate (prc_test_def_t :: def)
       select type (def)
       type is (prc_test_def_t)
          call def%init (model_name, prt_in, prt_out)
       end select
       allocate (entry)
       call entry%init (procname, model_name = model_name, &
            n_in = 1, n_components = 1)
       call entry%import_component (1, n_out = size (prt_out), &
            prt_in  = new_prt_spec (prt_in), &
            prt_out = new_prt_spec (prt_out), &
            method  = var_str ("test_decay"), &
            variant = def)
       call lib%append (entry)
    end if
    
    call lib%configure (os_data)
    call lib%load (os_data)
  end subroutine prc_test_create_library
  

  subroutine prc_test_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (prc_test_1, "prc_test_1", &
         "build and load trivial process", &
         u, results)
    call test (prc_test_2, "prc_test_2", &
         "build and load trivial process using shortcut", &
         u, results)
    call test (prc_test_3, "prc_test_3", &
         "build and load trivial decay", &
         u, results)
    call test (prc_test_4, "prc_test_4", &
         "build and load trivial decay using shortcut", &
         u, results)
end subroutine prc_test_test

  subroutine prc_test_1 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(process_library_t) :: lib
    class(prc_core_def_t), allocatable :: def
    type(process_def_entry_t), pointer :: entry
    type(string_t) :: model_name
    type(string_t), dimension(:), allocatable :: prt_in, prt_out
    type(process_constants_t) :: data
    class(prc_core_driver_t), allocatable :: driver
    real(default), dimension(0:3,4) :: p
    integer :: i
    
    write (u, "(A)")  "* Test output: prc_test_1"
    write (u, "(A)")  "*   Purpose: create a trivial process"
    write (u, "(A)")  "*            build a library and &
         &access the matrix element"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize a process library with one entry"
    write (u, "(A)")
    call os_data_init (os_data)
    call lib%init (var_str ("prc_test1"))

    model_name = "Test"
    allocate (prt_in (2), prt_out (2))
    prt_in  = [var_str ("s"), var_str ("s")]
    prt_out = [var_str ("s"), var_str ("s")]
    
    allocate (prc_test_def_t :: def)
    select type (def)
    type is (prc_test_def_t)
       call def%init (model_name, prt_in, prt_out)
    end select
    allocate (entry)
    call entry%init (var_str ("prc_test1_a"), model_name = model_name, &
         n_in = 2, n_components = 1)
    call entry%import_component (1, n_out = size (prt_out), &
         prt_in  = new_prt_spec (prt_in), &
         prt_out = new_prt_spec (prt_out), &
         method  = var_str ("test_me"), &
         variant = def)
    call lib%append (entry)
    
    write (u, "(A)")  "* Configure library"
    write (u, "(A)")
    call lib%configure (os_data)
    
    write (u, "(A)")  "* Load library"
    write (u, "(A)")
    call lib%load (os_data)

    call lib%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Probe library API:"
    write (u, "(A)")
       
    write (u, "(1x,A,L1)")  "is active                 = ", &
         lib%is_active ()
    write (u, "(1x,A,I0)")  "n_processes               = ", &
         lib%get_n_processes ()

    write (u, "(A)")
    write (u, "(A)")  "* Constants of prc_test1_a_i1:"
    write (u, "(A)")

    call lib%connect_process (var_str ("prc_test1_a"), 1, data, driver)

    write (u, "(1x,A,A)")  "component ID     = ", char (data%id)
    write (u, "(1x,A,A)")  "model name       = ", char (data%model_name)
    write (u, "(1x,A,A,A)")  "md5sum           = '", data%md5sum, "'"
    write (u, "(1x,A,L1)") "openmp supported = ", data%openmp_supported
    write (u, "(1x,A,I0)") "n_in  = ", data%n_in
    write (u, "(1x,A,I0)") "n_out = ", data%n_out
    write (u, "(1x,A,I0)") "n_flv = ", data%n_flv
    write (u, "(1x,A,I0)") "n_hel = ", data%n_hel
    write (u, "(1x,A,I0)") "n_col = ", data%n_col
    write (u, "(1x,A,I0)") "n_cin = ", data%n_cin
    write (u, "(1x,A,I0)") "n_cf  = ", data%n_cf
    write (u, "(1x,A,10(1x,I0))") "flv state =", data%flv_state
    write (u, "(1x,A,10(1x,I2))") "hel state =", data%hel_state(:,1)
    write (u, "(1x,A,10(1x,I0))") "col state =", data%col_state
    write (u, "(1x,A,10(1x,L1))") "ghost flag =", data%ghost_flag
    write (u, "(1x,A,10(1x,F5.3))") "color factors =", data%color_factors
    write (u, "(1x,A,10(1x,I0))") "cf index =", data%cf_index

    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics:"
    write (u, "(A)")
    
    p = reshape ([ &
         1.0_default, 0.0_default, 0.0_default, 1.0_default, &
         1.0_default, 0.0_default, 0.0_default,-1.0_default, &
         1.0_default, 1.0_default, 0.0_default, 0.0_default, &
         1.0_default,-1.0_default, 0.0_default, 0.0_default &
         ], [4,4])
    do i = 1, 4
       write (u, "(2x,A,I0,A,4(1x,F7.4))")  "p", i, " =", p(:,i)
    end do

    write (u, "(A)")
    write (u, "(A)")  "* Compute matrix element:"
    write (u, "(A)")

    select type (driver)
    type is (prc_test_t)
       write (u, "(1x,A,1x,E11.4)") "|amp| =", abs (driver%get_amplitude (p))
    end select

    call lib%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: prc_test_1"
    
  end subroutine prc_test_1
  
  subroutine prc_test_2 (u)
    integer, intent(in) :: u
    type(process_library_t) :: lib
    class(prc_core_driver_t), allocatable :: driver
    type(process_constants_t) :: data
    real(default), dimension(0:3,4) :: p
    
    write (u, "(A)")  "* Test output: prc_test_2"
    write (u, "(A)")  "*   Purpose: create a trivial process"
    write (u, "(A)")  "*            build a library and &
         &access the matrix element"
    write (u, "(A)")

    write (u, "(A)")  "* Build and load a process library with one entry"

    call prc_test_create_library (var_str ("prc_test2"), lib)
    call lib%connect_process (var_str ("prc_test2"), 1, data, driver)

    p = reshape ([ &
         1.0_default, 0.0_default, 0.0_default, 1.0_default, &
         1.0_default, 0.0_default, 0.0_default,-1.0_default, &
         1.0_default, 1.0_default, 0.0_default, 0.0_default, &
         1.0_default,-1.0_default, 0.0_default, 0.0_default &
         ], [4,4])

    write (u, "(A)")
    write (u, "(A)")  "* Compute matrix element:"
    write (u, "(A)")

    select type (driver)
    type is (prc_test_t)
       write (u, "(1x,A,1x,E11.4)") "|amp| =", abs (driver%get_amplitude (p))
    end select

    call lib%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: prc_test_2"
    
  end subroutine prc_test_2
  
  subroutine prc_test_3 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(process_library_t) :: lib
    class(prc_core_def_t), allocatable :: def
    type(process_def_entry_t), pointer :: entry
    type(string_t) :: model_name
    type(string_t), dimension(:), allocatable :: prt_in, prt_out
    type(process_constants_t) :: data
    class(prc_core_driver_t), allocatable :: driver
    real(default), dimension(0:3,3) :: p
    integer :: i
    
    write (u, "(A)")  "* Test output: prc_test_3"
    write (u, "(A)")  "*   Purpose: create a trivial decay process"
    write (u, "(A)")  "*            build a library and &
         &access the matrix element"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize a process library with one entry"
    write (u, "(A)")
    call os_data_init (os_data)
    call lib%init (var_str ("prc_test3"))

    model_name = "Test"
    allocate (prt_in (2), prt_out (2))
    prt_in  = [var_str ("s")]
    prt_out = [var_str ("f"), var_str ("F")]
    
    allocate (prc_test_def_t :: def)
    select type (def)
    type is (prc_test_def_t)
       call def%init (model_name, prt_in, prt_out)
    end select
    allocate (entry)
    call entry%init (var_str ("prc_test3_a"), model_name = model_name, &
         n_in = 1, n_components = 1)
    call entry%import_component (1, n_out = size (prt_out), &
         prt_in  = new_prt_spec (prt_in), &
         prt_out = new_prt_spec (prt_out), &
         method  = var_str ("test_me"), &
         variant = def)
    call lib%append (entry)
    
    write (u, "(A)")  "* Configure library"
    write (u, "(A)")
    call lib%configure (os_data)
    
    write (u, "(A)")  "* Load library"
    write (u, "(A)")
    call lib%load (os_data)

    call lib%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Probe library API:"
    write (u, "(A)")
       
    write (u, "(1x,A,L1)")  "is active                 = ", &
         lib%is_active ()
    write (u, "(1x,A,I0)")  "n_processes               = ", &
         lib%get_n_processes ()

    write (u, "(A)")
    write (u, "(A)")  "* Constants of prc_test3_a_i1:"
    write (u, "(A)")

    call lib%connect_process (var_str ("prc_test3_a"), 1, data, driver)

    write (u, "(1x,A,A)")  "component ID     = ", char (data%id)
    write (u, "(1x,A,A)")  "model name       = ", char (data%model_name)
    write (u, "(1x,A,A,A)")  "md5sum           = '", data%md5sum, "'"
    write (u, "(1x,A,L1)") "openmp supported = ", data%openmp_supported
    write (u, "(1x,A,I0)") "n_in  = ", data%n_in
    write (u, "(1x,A,I0)") "n_out = ", data%n_out
    write (u, "(1x,A,I0)") "n_flv = ", data%n_flv
    write (u, "(1x,A,I0)") "n_hel = ", data%n_hel
    write (u, "(1x,A,I0)") "n_col = ", data%n_col
    write (u, "(1x,A,I0)") "n_cin = ", data%n_cin
    write (u, "(1x,A,I0)") "n_cf  = ", data%n_cf
    write (u, "(1x,A,10(1x,I0))") "flv state =", data%flv_state
    write (u, "(1x,A,10(1x,I2))") "hel state =", data%hel_state(:,1)
    write (u, "(1x,A,10(1x,I2))") "hel state =", data%hel_state(:,2)
    write (u, "(1x,A,10(1x,I0))") "col state =", data%col_state
    write (u, "(1x,A,10(1x,L1))") "ghost flag =", data%ghost_flag
    write (u, "(1x,A,10(1x,F5.3))") "color factors =", data%color_factors
    write (u, "(1x,A,10(1x,I0))") "cf index =", data%cf_index

    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics:"
    write (u, "(A)")
    
    p = reshape ([ &
         125._default, 0.0_default, 0.0_default, 0.0_default, &
         62.5_default, 0.0_default, 0.0_default, 62.5_default, &
         62.5_default, 0.0_default, 0.0_default,-62.5_default &
         ], [4,3])
    do i = 1, 3
       write (u, "(2x,A,I0,A,4(1x,F8.4))")  "p", i, " =", p(:,i)
    end do

    write (u, "(A)")
    write (u, "(A)")  "* Compute matrix element:"
    write (u, "(A)")

    select type (driver)
    type is (prc_test_t)
       write (u, "(1x,A,1x,E11.4)") "|amp| =", abs (driver%get_amplitude (p))
    end select

    call lib%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: prc_test_3"
    
  end subroutine prc_test_3
  
  subroutine prc_test_4 (u)
    integer, intent(in) :: u
    type(process_library_t) :: lib
    class(prc_core_driver_t), allocatable :: driver
    type(process_constants_t) :: data
    real(default), dimension(0:3,3) :: p
    
    write (u, "(A)")  "* Test output: prc_test_4"
    write (u, "(A)")  "*   Purpose: create a trivial decay process"
    write (u, "(A)")  "*            build a library and &
         &access the matrix element"
    write (u, "(A)")

    write (u, "(A)")  "* Build and load a process library with one entry"

    call prc_test_create_library (var_str ("prc_test4"), lib, &
         scattering=.false., decay=.true.)
    call lib%connect_process (var_str ("prc_test4"), 1, data, driver)

    p = reshape ([ &
         125._default, 0.0_default, 0.0_default, 0.0_default, &
         62.5_default, 0.0_default, 0.0_default, 62.5_default, &
         62.5_default, 0.0_default, 0.0_default,-62.5_default &
         ], [4,3])

    write (u, "(A)")
    write (u, "(A)")  "* Compute matrix element:"
    write (u, "(A)")

    select type (driver)
    type is (prc_test_t)
       write (u, "(1x,A,1x,E11.4)") "|amp| =", abs (driver%get_amplitude (p))
    end select

    call lib%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: prc_test_4"
    
  end subroutine prc_test_4
  

end module prc_test
