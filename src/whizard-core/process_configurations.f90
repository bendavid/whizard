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

module process_configurations
  
  use iso_varying_string, string_t => varying_string
  use unit_tests
  use diagnostics

  use variables
  use models
  use prc_core_def
  use particle_specifiers
  use process_libraries
  use prclib_stacks
  use prc_test
  use prc_omega
  use rt_data
  use dispatch

  use prc_gosam
  
  implicit none
  private

  public :: process_configuration_t
  public :: process_configurations_test
  public :: prepare_test_library

  type :: process_configuration_t
     type(process_def_entry_t), pointer :: entry => null ()
     type(string_t) :: id
     integer :: num_id = 0
   contains
     procedure :: init => process_configuration_init
     procedure :: setup_component => process_configuration_setup_component
     procedure :: set_component_associations => &
                      process_configuration_set_component_associations
     procedure :: record => process_configuration_record
  end type process_configuration_t
  

contains
  
  subroutine process_configuration_init &
       (config, prc_name, n_in, n_components, global)
    class(process_configuration_t), intent(out) :: config
    type(string_t), intent(in) :: prc_name
    integer, intent(in) :: n_in
    integer, intent(in) :: n_components 
    type(rt_data_t), intent(in) :: global
    type(model_t), pointer :: model
    model => global%model
    config%id = prc_name
    allocate (config%entry)
    if (global%var_list%is_known (var_str ("process_num_id"))) then
       config%num_id = &
            global%var_list%get_ival (var_str ("process_num_id"))
       call config%entry%init (prc_name, &
            model = model, n_in = n_in, n_components = n_components, &
            num_id = config%num_id, nlo_process = global%nlo_calculation)
    else
       call config%entry%init (prc_name, &
            model = model, n_in = n_in, n_components = n_components, &
            nlo_process = global%nlo_calculation)
    end if
  end subroutine process_configuration_init
    
  subroutine process_configuration_setup_component &
       (config, i_component, prt_in, prt_out, global, &
        nlo_type, active_in)
    class(process_configuration_t), intent(inout) :: config
    integer, intent(in) :: i_component
    type(prt_spec_t), dimension(:), intent(in) :: prt_in
    type(prt_spec_t), dimension(:), intent(in) :: prt_out
    type(rt_data_t), intent(inout) :: global
    type(string_t), intent(in), optional :: nlo_type
    logical, intent(in), optional :: active_in
    type(string_t), dimension(:), allocatable :: prt_str_in
    type(string_t), dimension(:), allocatable :: prt_str_out
    class(prc_core_def_t), allocatable :: core_def
    type(string_t) :: method
    integer :: i
    logical :: active

    allocate (prt_str_in  (size (prt_in)))
    allocate (prt_str_out (size (prt_out)))
    forall (i = 1:size (prt_in))  prt_str_in(i)  = prt_in(i)% get_name ()
    forall (i = 1:size (prt_out)) prt_str_out(i) = prt_out(i)%get_name ()
    if (present (active_in)) then
      active = active_in
    else
      active = .true.
    end if

    call dispatch_core_def (core_def, prt_str_in, prt_str_out, &
                            global, config%id, nlo_type)
    method = &
         global%var_list%get_sval (var_str ("$method"))
    call config%entry%import_component (i_component, &
       n_out = size (prt_out), &
       prt_in = prt_in, &
       prt_out = prt_out, &
       method = method, &
       variant = core_def, &
       nlo_type = nlo_type, &
       active = active)
  end subroutine process_configuration_setup_component
  
  subroutine process_configuration_set_component_associations &
       (config, i_list)
    class(process_configuration_t), intent(inout) :: config
    integer, intent(in), dimension(4) :: i_list 
    integer :: i_component
    do i_component = 1, config%entry%get_n_components ()
       if (any (i_list == i_component)) then
          call config%entry%set_associated_components (i_component, &
                 i_list(1), i_list(2), i_list(3), i_list(4))
       end if
    end do
  end subroutine process_configuration_set_component_associations

  subroutine process_configuration_record (config, global)
    class(process_configuration_t), intent(inout) :: config
    type(rt_data_t), intent(inout) :: global
    if (associated (global%prclib)) then
       call global%prclib%open ()
       call global%prclib%append (config%entry)
       if (config%num_id /= 0) then
          write (msg_buffer, "(5A,I0,A)") "Process library '", &
               char (global%prclib%get_name ()), &
               "': recorded process '", char (config%id), "' (", &
               config%num_id, ")"
       else
          write (msg_buffer, "(5A)") "Process library '", &
               char (global%prclib%get_name ()), &
               "': recorded process '", char (config%id), "'"          
       end if
       call msg_message ()
    else
       call msg_fatal ("Recording process '" // char (config%id) &
            // "': active process library undefined")
    end if
  end subroutine process_configuration_record
  

  subroutine process_configurations_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (process_configurations_1, "process_configurations_1", &
         "test processes", &
         u, results)
    call test (process_configurations_2, "process_configurations_2", &
         "omega options", &
         u, results)
  end subroutine process_configurations_test

  subroutine prepare_test_library (global, libname, mode, procname)
    type(rt_data_t), intent(inout), target :: global
    type(string_t), intent(in) :: libname
    integer, intent(in) :: mode
    type(string_t), intent(in), dimension(:), optional :: procname
    type(prclib_entry_t), pointer :: lib
    type(string_t) :: prc_name
    type(string_t), dimension(:), allocatable :: prt_in, prt_out
    integer :: n_components
    type(process_configuration_t) :: prc_config

    if (.not. associated (global%prclib_stack%get_first_ptr ())) then
       allocate (lib)
       call lib%init (libname)
       call global%add_prclib (lib)
    end if

    if (btest (mode, 0)) then

       call global%select_model (var_str ("Test"))

       if (present (procname)) then
          prc_name = procname(1)
       else
          prc_name = "prc_config_a"
       end if
       n_components = 1
       allocate (prt_in (2), prt_out (2))
       prt_in = [var_str ("s"), var_str ("s")]
       prt_out = [var_str ("s"), var_str ("s")]

       call var_list_set_string (global%var_list, var_str ("$method"),&
            var_str ("unit_test"), is_known = .true.)

       call prc_config%init (prc_name, size (prt_in), n_components, global)
       call prc_config%setup_component (1, &
            new_prt_spec (prt_in), new_prt_spec (prt_out), global)
       call prc_config%record (global)

       deallocate (prt_in, prt_out)
       
    end if

    if (btest (mode, 1)) then

       call global%select_model (var_str ("QED"))

       if (present (procname)) then
          prc_name = procname(2)
       else
          prc_name = "prc_config_b"
       end if
       n_components = 1
       allocate (prt_in (2), prt_out (2))
       prt_in = [var_str ("e+"), var_str ("e-")]
       prt_out = [var_str ("m+"), var_str ("m-")]

       call var_list_set_string (global%var_list, var_str ("$method"),&
            var_str ("omega"), is_known = .true.)

       call prc_config%init (prc_name, size (prt_in), n_components, global)
       call prc_config%setup_component (1, &
            new_prt_spec (prt_in), new_prt_spec (prt_out), global)
       call prc_config%record (global)

       deallocate (prt_in, prt_out)
       
    end if
    
    if (btest (mode, 2)) then

       call global%select_model (var_str ("Test"))

       if (present (procname)) then
          prc_name = procname(1)
       else
          prc_name = "prc_config_a"
       end if
       n_components = 1
       allocate (prt_in (1), prt_out (2))
       prt_in = [var_str ("s")]
       prt_out = [var_str ("f"), var_str ("fbar")]

       call var_list_set_string (global%var_list, var_str ("$method"),&
            var_str ("unit_test"), is_known = .true.)

       call prc_config%init (prc_name, size (prt_in), n_components, global)
       call prc_config%setup_component (1, &
            new_prt_spec (prt_in), new_prt_spec (prt_out), global)
       call prc_config%record (global)

       deallocate (prt_in, prt_out)
       
    end if

  end subroutine prepare_test_library
    
  subroutine process_configurations_1 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global
    
    write (u, "(A)")  "* Test output: process_configurations_1"
    write (u, "(A)")  "*   Purpose: configure test processes"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()
    call var_list_set_log (global%var_list, var_str ("?omega_openmp"), &
         .false., is_known = .true.)

    write (u, "(A)")  "* Configure processes as prc_test, model Test"
    write (u, "(A)")  "*                     and omega, model QED"
    write (u, *)

    call var_list_set_int (global%var_list, var_str ("process_num_id"), &
         42, is_known = .true.)
    call prepare_test_library (global, var_str ("prc_config_lib_1"), 3)

    global%os_data%fc = "Fortran-compiler"
    global%os_data%fcflags = "Fortran-flags"

    call global%write_libraries (u)

    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: process_configurations_1"
    
  end subroutine process_configurations_1
  
  subroutine process_configurations_2 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global
    
    type(string_t) :: libname
    type(prclib_entry_t), pointer :: lib
    type(string_t) :: prc_name
    type(string_t), dimension(:), allocatable :: prt_in, prt_out
    integer :: n_components
    type(process_configuration_t) :: prc_config

    write (u, "(A)")  "* Test output: process_configurations_2"
    write (u, "(A)")  "*   Purpose: configure test processes with options"
    write (u, "(A)")

    call syntax_model_file_init ()

    call global%global_init ()
    
    write (u, "(A)")  "* Configure processes as omega, model QED"
    write (u, *)

    libname = "prc_config_lib_2"
    
    allocate (lib)
    call lib%init (libname)
    call global%add_prclib (lib)

    call global%select_model (var_str ("QED"))

    prc_name = "prc_config_c"
    n_components = 2
    allocate (prt_in (2), prt_out (2))
    prt_in = [var_str ("e+"), var_str ("e-")]
    prt_out = [var_str ("m+"), var_str ("m-")]

    call var_list_set_string (global%var_list, var_str ("$method"),&
         var_str ("omega"), is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?omega_openmp"), &
         .false., is_known = .true.)

    call prc_config%init (prc_name, size (prt_in), n_components, global)

    call var_list_set_log (global%var_list, var_str ("?report_progress"), &
         .true., is_known = .true.)
    call prc_config%setup_component (1, &
         new_prt_spec (prt_in), new_prt_spec (prt_out), global)

    call var_list_set_log (global%var_list, var_str ("?report_progress"), &
         .false., is_known = .true.)
    call var_list_set_log (global%var_list, var_str ("?omega_openmp"), &
         .true., is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$restrictions"),&
         var_str ("3+4~A"), is_known = .true.)
    call var_list_set_string (global%var_list, var_str ("$omega_flags"), &
         var_str ("-fusion:progress_file omega_prc_config.log"), &
         is_known = .true.)
    call prc_config%setup_component (2, &
         new_prt_spec (prt_in), new_prt_spec (prt_out), global)
    
    call prc_config%record (global)

    deallocate (prt_in, prt_out)
    
    global%os_data%fc = "Fortran-compiler"
    global%os_data%fcflags = "Fortran-flags"

    call global%write_vars (u, [ &
         var_str ("$model_name"), &
         var_str ("$method"), &
         var_str ("?report_progress"), &
         var_str ("$restrictions"), &
         var_str ("$omega_flags")])
    write (u, "(A)")
    call global%write_libraries (u)

    call global%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: process_configurations_2"
    
  end subroutine process_configurations_2
  

end module process_configurations
