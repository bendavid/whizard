! WHIZARD 2.3.1 Aug 25 2016
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

module process_configurations
  
  use iso_varying_string, string_t => varying_string
  use diagnostics
  use models
  use prc_core_def
  use particle_specifiers
  use process_libraries
  use rt_data
  use variables

  use dispatch, only: dispatch_core_def
  
  implicit none
  private

  public :: process_configuration_t

  type :: process_configuration_t
     type(process_def_entry_t), pointer :: entry => null ()
     type(string_t) :: id
     integer :: num_id = 0
   contains
     procedure :: init => process_configuration_init
     procedure :: setup_component => process_configuration_setup_component
     procedure :: set_fixed_emitter => process_configuration_set_fixed_emitter
     procedure :: set_coupling_powers => process_configuration_set_coupling_powers
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
    logical :: nlo_process
    model => global%model
    config%id = prc_name
    nlo_process = global%nlo_fixed_order
    allocate (config%entry)
    if (global%var_list%is_known (var_str ("process_num_id"))) then
       config%num_id = &
            global%var_list%get_ival (var_str ("process_num_id"))
       call config%entry%init (prc_name, &
            model = model, n_in = n_in, n_components = n_components, &
            num_id = config%num_id, nlo_process = nlo_process)
    else
       call config%entry%init (prc_name, &
            model = model, n_in = n_in, n_components = n_components, &
            nlo_process = nlo_process)
    end if
  end subroutine process_configuration_init
    
  subroutine process_configuration_setup_component &
       (config, i_component, prt_in, prt_out, model, var_list, &
        nlo_type, active_in)
    class(process_configuration_t), intent(inout) :: config
    integer, intent(in) :: i_component
    type(prt_spec_t), dimension(:), intent(in) :: prt_in
    type(prt_spec_t), dimension(:), intent(in) :: prt_out
    type(model_t), pointer, intent(in) :: model
    type(var_list_t), intent(in) :: var_list
    integer, intent(in), optional :: nlo_type
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
                            model, var_list, config%id, nlo_type)
    method = var_list%get_sval (var_str ("$method"))
    call config%entry%import_component (i_component, &
       n_out = size (prt_out), &
       prt_in = prt_in, &
       prt_out = prt_out, &
       method = method, &
       variant = core_def, &
       nlo_type = nlo_type, &
       active = active)
  end subroutine process_configuration_setup_component
  
  subroutine process_configuration_set_fixed_emitter (config, i, emitter)
     class(process_configuration_t), intent(inout) :: config
     integer, intent(in) :: i, emitter
     call config%entry%set_fixed_emitter (i, emitter)
  end subroutine process_configuration_set_fixed_emitter

  subroutine process_configuration_set_coupling_powers (config, alpha_power, alphas_power)
    class(process_configuration_t), intent(inout) :: config
    integer, intent(in) :: alpha_power, alphas_power
    call config%entry%set_coupling_powers (alpha_power, alphas_power)
  end subroutine process_configuration_set_coupling_powers

  subroutine process_configuration_set_component_associations &
         (config, i_list, pdf, damping, mismatch)
    class(process_configuration_t), intent(inout) :: config
    integer, intent(in), dimension(:) :: i_list
    logical, intent(in) :: pdf, damping, mismatch
    integer :: i_component
    do i_component = 1, config%entry%get_n_components ()
       if (any (i_list == i_component)) then
          if (pdf) then
             call config%entry%set_associated_components (i_component, &
                    i_list(1), i_list(2), i_list(3), i_list(4), i_pdf = i_list(5))
          else if (mismatch) then
             !!! TODO: (bcn 2016-07-21) this is not a typo. mismatch
             !!!               seems to need no extra component type
             call config%entry%set_associated_components (i_component, &
                    i_list(1), i_list(2), i_list(3), i_list(4), i_pdf = i_list(5))
          else if (damping) then
             call config%entry%set_associated_components (i_component, &
                    i_list(1), i_list(2), i_list(3), i_list(4), i_rfin = i_list(5))
          else
             call config%entry%set_associated_components (i_component, &
                    i_list(1), i_list(2), i_list(3), i_list(4))
          end if
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
  

end module process_configurations
