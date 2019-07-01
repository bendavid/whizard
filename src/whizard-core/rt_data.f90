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

module rt_data

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use system_dependencies !NODEP!
  use diagnostics !NODEP!
  use unit_tests
  use pdf_builtin !NODEP!
  use sf_lhapdf !NODEP!
  use os_interface
  use ifiles
  use lexers
  use parser
  use models
  use flavors
  use jets
  use variables
  use expressions
  use polarizations
  use beams
  use process_libraries
  use prclib_stacks
  use prc_core
  use beam_structures
  use user_files
  use process_stacks
  use iterations

  implicit none
  private

  public :: rt_data_t
  public :: rt_data_test

  type :: rt_parse_nodes_t
     type(parse_node_t), pointer :: cuts_lexpr => null ()
     type(parse_node_t), pointer :: scale_expr => null ()     
     type(parse_node_t), pointer :: fac_scale_expr => null ()
     type(parse_node_t), pointer :: ren_scale_expr => null ()     
     type(parse_node_t), pointer :: weight_expr => null ()
     type(parse_node_t), pointer :: selection_lexpr => null ()
     type(parse_node_t), pointer :: reweight_expr => null ()
     type(parse_node_t), pointer :: analysis_lexpr => null ()
     type(parse_node_p), dimension(:), allocatable :: alt_setup
   contains
     procedure :: clear => rt_parse_nodes_clear
     procedure :: write => rt_parse_nodes_write
     procedure :: show => rt_parse_nodes_show
  end type rt_parse_nodes_t
     
  type :: rt_particle_entry_t
     integer :: pdg = 0
     logical :: stable = .true.
     logical :: isotropic = .false.
     logical :: diagonal = .false.
     logical :: polarized = .false.
     type(rt_particle_entry_t), pointer :: next => null ()
  end type rt_particle_entry_t
  
  type :: rt_particle_stack_t
     type(model_t), pointer :: model => null ()
     type(rt_particle_entry_t), pointer :: first => null ()
   contains
     procedure :: final => rt_particle_stack_final
     procedure :: write => rt_particle_stack_write
     procedure :: init => rt_particle_stack_init
     procedure :: reset => rt_particle_stack_reset
     procedure :: push => rt_particle_stack_push
     procedure :: is_empty => rt_particle_stack_is_empty
     procedure :: contains => rt_particle_stack_contains
     procedure :: restore_model => rt_particle_stack_restore_model
  end type rt_particle_stack_t
  
  type :: rt_data_t
     type(lexer_t), pointer :: lexer => null ()
     type(var_list_t) :: var_list
     type(iterations_list_t) :: it_list
     type(os_data_t) :: os_data
     type(model_list_t) :: model_list
     type(model_t), pointer :: model => null ()
     type(model_t), pointer :: fallback_model => null ()
     type(rt_particle_stack_t) :: particle_stack
     type(prclib_stack_t) :: prclib_stack
     type(process_library_t), pointer :: prclib => null ()
     type(beam_structure_t) :: beam_structure
     type(pdf_builtin_status_t) :: pdf_builtin_status
     type(lhapdf_status_t) :: lhapdf_status
     type(rt_parse_nodes_t) :: pn
     type(process_stack_t) :: process_stack
     type(string_t), dimension(:), allocatable :: sample_fmt
     type(file_list_t), pointer :: out_files => null ()
     logical :: quit = .false.
     integer :: quit_code = 0
     type(string_t) :: logfile 
   contains
     procedure :: write => rt_data_write
     procedure :: write_vars => rt_data_write_vars
     procedure :: write_model_list => rt_data_write_model_list
     procedure :: write_libraries => rt_data_write_libraries
     procedure :: write_beams => rt_data_write_beams
     procedure :: write_expr => rt_data_write_expr
     procedure :: write_process_stack => rt_data_write_process_stack
     procedure :: clear_beams => rt_data_clear_beams
     procedure :: global_init => rt_data_global_init
     procedure :: local_init => rt_data_local_init
     procedure :: copy_globals => rt_data_copy_globals
     procedure :: init_pointer_variables => rt_data_init_pointer_variables
     procedure :: link => rt_data_link
     procedure :: restore => rt_data_restore
     procedure :: restore_globals => rt_data_restore_globals
     procedure :: final => rt_data_global_final
     procedure :: local_final => rt_data_local_final
     procedure :: init_fallback_model => rt_data_init_fallback_model
     procedure :: read_model => rt_data_read_model
     procedure :: select_model => rt_data_select_model
     procedure :: modify_particle => rt_data_modify_particle
     procedure :: add_prclib => rt_data_add_prclib
     procedure :: update_prclib => rt_data_update_prclib
     procedure :: get_helicity_selection => rt_data_get_helicity_selection
     procedure :: show_beams => rt_data_show_beams
     procedure :: get_sqrts => rt_data_get_sqrts
     procedure :: pacify => rt_data_pacify
     procedure :: fix_system_dependencies => rt_data_fix_system_dependencies
  end type rt_data_t


contains

  subroutine rt_parse_nodes_clear (rt_pn, name)
    class(rt_parse_nodes_t), intent(inout) :: rt_pn
    type(string_t), intent(in) :: name
    select case (char (name))
    case ("cuts")
       rt_pn%cuts_lexpr => null ()
    case ("scale")
       rt_pn%scale_expr => null ()
    case ("factorization_scale")
       rt_pn%fac_scale_expr => null ()
    case ("renormalization_scale")
       rt_pn%ren_scale_expr => null ()
    case ("weight")
       rt_pn%weight_expr => null ()
    case ("selection")
       rt_pn%selection_lexpr => null ()
    case ("reweight")
       rt_pn%reweight_expr => null ()
    case ("analysis")
       rt_pn%analysis_lexpr => null ()
    end select
  end subroutine rt_parse_nodes_clear
  
  subroutine rt_parse_nodes_write (object, unit)
    class(rt_parse_nodes_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, i
    u = output_unit (unit)
    call wrt ("Cuts", object%cuts_lexpr)
    call write_separator (u)
    call wrt ("Scale", object%scale_expr)
    call write_separator (u)
    call wrt ("Factorization scale", object%fac_scale_expr)
    call write_separator (u)
    call wrt ("Renormalization scale", object%ren_scale_expr)
    call write_separator (u)
    call wrt ("Weight", object%weight_expr)
    call write_separator_double (u)
    call wrt ("Event selection", object%selection_lexpr)
    call write_separator (u)
    call wrt ("Event reweighting factor", object%reweight_expr)
    call write_separator (u)
    call wrt ("Event analysis", object%analysis_lexpr)
    if (allocated (object%alt_setup)) then
       call write_separator_double (u)
       write (u, "(1x,A,':')")  "Alternative setups"
       do i = 1, size (object%alt_setup)
          call write_separator (u)
          call wrt ("Commands", object%alt_setup(i)%ptr)
       end do
    end if
  contains
    subroutine wrt (title, pn)
      character(*), intent(in) :: title
      type(parse_node_t), intent(in), pointer :: pn
      if (associated (pn)) then
         write (u, "(1x,A,':')")  title
         call write_separator (u)
         call parse_node_write_rec (pn, u)
      else
         write (u, "(1x,A,':',1x,A)")  title, "[undefined]"
      end if
    end subroutine wrt
  end subroutine rt_parse_nodes_write
    
  subroutine rt_parse_nodes_show (rt_pn, name, unit)
    class(rt_parse_nodes_t), intent(in) :: rt_pn
    type(string_t), intent(in) :: name
    integer, intent(in), optional :: unit
    type(parse_node_t), pointer :: pn
    integer :: u
    u = output_unit (unit)
    select case (char (name))
    case ("cuts")
       pn => rt_pn%cuts_lexpr
    case ("scale")
       pn => rt_pn%scale_expr
    case ("factorization_scale")
       pn => rt_pn%fac_scale_expr
    case ("renormalization_scale")
       pn => rt_pn%ren_scale_expr
    case ("weight")
       pn => rt_pn%weight_expr
    case ("selection")
       pn => rt_pn%selection_lexpr
    case ("reweight")
       pn => rt_pn%reweight_expr
    case ("analysis")
       pn => rt_pn%analysis_lexpr
    end select
    if (associated (pn)) then
       write (u, "(A,1x,A,1x,A)")  "Expression:", char (name), "(parse tree):"
       call parse_node_write_rec (pn, u)
    else
       write (u, "(A,1x,A,A)")  "Expression:", char (name), ": [undefined]"
    end if
  end subroutine rt_parse_nodes_show
  
  subroutine rt_particle_stack_final (object)
    class(rt_particle_stack_t), intent(inout) :: object
    type(rt_particle_entry_t), pointer :: entry
    do while (associated (object%first))
       entry => object%first
       object%first => entry%next
       deallocate (entry)
    end do
    object%model => null ()
  end subroutine rt_particle_stack_final

  subroutine rt_particle_stack_write (object, unit)
    class(rt_particle_stack_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    type(rt_particle_entry_t), pointer :: entry
    u = output_unit (unit)
    if (.not. object%is_empty ()) then
       write (u, "(1x,A)")  "Stored particle data"
       if (associated (object%model)) then
          write (u, "(3x,A,A)") "Model = ", char (object%model%get_name ())
       else
          write (u, "(3x,A,A)") "Model = [undefined]"
       end if
       entry => object%first
       do while (associated (entry))
          write (u, "(3x,I0,':',4(2x,A,L1))")  entry%pdg, &
               "stable = ", entry%stable, &
               "isotropic = ", entry%isotropic, &
               "diagonal = ", entry%diagonal, &
               "polarized = ", entry%polarized
          entry => entry%next
       end do
    end if
  end subroutine rt_particle_stack_write

  subroutine rt_particle_stack_init (stack, model)
    class(rt_particle_stack_t), intent(out) :: stack
    type(model_t), intent(in), target :: model
    stack%model => model
  end subroutine rt_particle_stack_init
    
  subroutine rt_particle_stack_reset (stack, model)
    class(rt_particle_stack_t), intent(inout) :: stack
    type(model_t), intent(in), target :: model
    if (associated (stack%model)) then
       if (model%get_name () /= stack%model%get_name ()) then
          call stack%final ()
          call stack%init (model)
       end if
    else
       call stack%init (model)
    end if
  end subroutine rt_particle_stack_reset

  subroutine rt_particle_stack_push (stack, pdg)
    class(rt_particle_stack_t), intent(inout) :: stack
    integer, intent(in) :: pdg
    type(rt_particle_entry_t), pointer :: entry
    type(particle_data_t), pointer :: prt_data
    logical :: anti
    allocate (entry)
    entry%pdg = pdg
    anti = pdg < 0
    prt_data => model_get_particle_ptr (stack%model, pdg)
    entry%stable = particle_data_is_stable (prt_data, anti)
    entry%polarized = particle_data_is_polarized (prt_data, anti)
    entry%isotropic = particle_data_decays_isotropically (prt_data, anti)
    entry%diagonal = particle_data_decays_diagonal (prt_data, anti)
    entry%next => stack%first
    stack%first => entry
  end subroutine rt_particle_stack_push
    
  function rt_particle_stack_is_empty (stack) result (flag)
    class(rt_particle_stack_t), intent(in) :: stack
    logical :: flag
    flag = .not. associated (stack%first)
  end function rt_particle_stack_is_empty
  
  function rt_particle_stack_contains (stack, pdg) result (flag)
    class(rt_particle_stack_t), intent(in) :: stack
    integer, intent(in) :: pdg
    logical :: flag
    type(rt_particle_entry_t), pointer :: entry
    flag = .false.
    entry => stack%first
    do while (associated (entry))
       if (entry%pdg == pdg) then
          flag = .true.;  return
       end if
       entry => entry%next
    end do
  end function rt_particle_stack_contains
  
  subroutine rt_particle_stack_restore_model (stack)
    class(rt_particle_stack_t), intent(inout) :: stack
    type(rt_particle_entry_t), pointer :: entry
    type(particle_data_t), pointer :: prt_data
    if (associated (stack%model)) then
       entry => stack%first
       do while (associated (entry))
          prt_data => model_get_particle_ptr (stack%model, entry%pdg)
          if (entry%pdg > 0) then
             call particle_data_set (prt_data, &
                  p_is_stable = entry%stable, &
                  p_polarized = entry%polarized, &
                  p_decays_isotropically = entry%isotropic, &
                  p_decays_diagonal = entry%diagonal)
          else
             call particle_data_set (prt_data, &
                  a_is_stable = entry%stable, &
                  a_polarized = entry%polarized, &
                  a_decays_isotropically = entry%isotropic, &
                  a_decays_diagonal = entry%diagonal)
          end if
          entry => entry%next
       end do
    end if
    call stack%final ()
  end subroutine rt_particle_stack_restore_model
    
  subroutine rt_data_write (object, unit, vars, pacify)
    class(rt_data_t), intent(in) :: object
    integer, intent(in), optional :: unit
    type(string_t), dimension(:), intent(in), optional :: vars
    logical, intent(in), optional :: pacify
    integer :: u, i
    u = output_unit (unit)
    call write_separator_double (u)
    write (u, "(1x,A)")  "Runtime data:"
    if (present (vars)) then
       if (size (vars) /= 0) then
          call write_separator_double (u)
          write (u, "(1x,A)")  "Selected variables:"
          call write_separator (u)
          call object%write_vars (u, vars)
       end if
    else
       call write_separator_double (u)
       call var_list_write (object%var_list, u, follow_link=.true.)
    end if
    if (object%it_list%get_n_pass () > 0) then
       call write_separator_double (u)
       write (u, "(1x)", advance="no")
       call object%it_list%write (u)
    end if
    if (associated (object%model)) then
       call write_separator_double (u)
       call object%model_list%write (u)
       if (.not. object%particle_stack%is_empty ()) then
          call write_separator (u)
          call object%particle_stack%write (u)
       end if
    end if
    call object%prclib_stack%write (u)
    call object%beam_structure%write (u)
    call write_separator_double (u)
    call object%pn%write (u)
    if (allocated (object%sample_fmt)) then
       call write_separator (u)
       write (u, "(1x,A)", advance="no")  "Event sample formats = "
       do i = 1, size (object%sample_fmt)
          if (i > 1)  write (u, "(A,1x)", advance="no")  ","
          write (u, "(A)", advance="no")  char (object%sample_fmt(i))
       end do
       write (u, "(A)")
    end if
    call object%process_stack%write (u, pacify)
    write (u, "(1x,A,1x,L1)")  "quit     :", object%quit
    write (u, "(1x,A,1x,I0)")  "quit_code:", object%quit_code
    call write_separator_double (u)
    write (u, "(1x,A,1x,A)")   "Logfile  :", "'" // trim (char (object%logfile)) // "'"
    call write_separator_double (u)
  end subroutine rt_data_write
  
  subroutine rt_data_write_vars (object, unit, vars)
    class(rt_data_t), intent(in) :: object
    integer, intent(in), optional :: unit
    type(string_t), dimension(:), intent(in), optional :: vars
    integer :: u, i
    u = output_unit (unit)
    if (present (vars)) then
       do i = 1, size (vars)
          call var_list_write_var (object%var_list, vars(i), unit = u, &
               follow_link = .true.)
       end do
    end if
  end subroutine rt_data_write_vars
  
  subroutine rt_data_write_model_list (object, unit)
    class(rt_data_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    call object%model_list%write (u)
  end subroutine rt_data_write_model_list

  subroutine rt_data_write_libraries (object, unit, libpath)
    class(rt_data_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: libpath
    integer :: u
    u = output_unit (unit)
    call object%prclib_stack%write (u, libpath)
  end subroutine rt_data_write_libraries

  subroutine rt_data_write_beams (object, unit)
    class(rt_data_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    call write_separator_double (u)
    call object%beam_structure%write (u)
    call write_separator_double (u)
  end subroutine rt_data_write_beams

  subroutine rt_data_write_expr (object, unit)
    class(rt_data_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    call write_separator_double (u)
    call object%pn%write (u)
    call write_separator_double (u)
  end subroutine rt_data_write_expr
  
  subroutine rt_data_write_process_stack (object, unit)
    class(rt_data_t), intent(in) :: object
    integer, intent(in), optional :: unit
    call object%process_stack%write (unit)
  end subroutine rt_data_write_process_stack
  
  subroutine rt_data_clear_beams (global)
    class(rt_data_t), intent(inout) :: global
    call global%beam_structure%final_sf ()
    call global%beam_structure%final_pol ()
    call global%beam_structure%final_mom ()
  end subroutine rt_data_clear_beams
  
  subroutine rt_data_global_init (global, paths, logfile)
    class(rt_data_t), intent(out), target :: global
    type(paths_t), intent(in), optional :: paths
    type(string_t), intent(in), optional :: logfile
    logical, target, save :: known = .true.
    integer :: seed
    real(default), parameter :: real_specimen = 1.
    call os_data_init (global%os_data, paths)
    if (present (logfile)) then
       global%logfile = logfile
    else
       global%logfile = ""
    end if
    allocate (global%out_files)
    call system_clock (seed)
    call var_list_append_log_ptr &
         (global%var_list, var_str ("?logging"), logging, known, &
         intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("seed"), seed, &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$model_name"), &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("process_num_id"), &
         intrinsic=.true.)        
    call var_list_append_string &
         (global%var_list, var_str ("$method"), var_str ("omega"), &
         intrinsic=.true.)        
    call var_list_append_log &
         (global%var_list, var_str ("?report_progress"), .true., &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$restrictions"), var_str (""), &
         intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$omega_flags"), var_str (""), &
         intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?read_color_factors"), .true., &
          intrinsic=.true.)
!!! JRR: WK please check (#529)    
!     call var_list_append_string &
!          (global%var_list, var_str ("$user_procs_cut"), var_str (""), &
!           intrinsic=.true.)     
!     call var_list_append_string &
!          (global%var_list, var_str ("$user_procs_event_shape"), var_str (""), &
!           intrinsic=.true.)     
!     call var_list_append_string &
!          (global%var_list, var_str ("$user_procs_obs1"), var_str (""), &
!           intrinsic=.true.)     
!     call var_list_append_string &
!          (global%var_list, var_str ("$user_procs_obs2"), var_str (""), &
!           intrinsic=.true.)     
!     call var_list_append_string &
!          (global%var_list, var_str ("$user_procs_sf"), var_str (""), &
!           intrinsic=.true.)     
    call var_list_append_log &
         (global%var_list, var_str ("?slha_read_input"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?slha_read_spectrum"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?slha_read_decays"), .false., &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$library_name"), &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("sqrts"), &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("luminosity"), 0._default, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?sf_trace"), .false., &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$sf_trace_file"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?sf_allow_s_mapping"), .true., &
          intrinsic=.true.)
    if (present (paths)) then
       call var_list_append_string &
            (global%var_list, var_str ("$lhapdf_dir"), paths%lhapdfdir, &
             intrinsic=.true.)
    else
       call var_list_append_string &
            (global%var_list, var_str ("$lhapdf_dir"), var_str(""), &
             intrinsic=.true.)
    end if 
    call var_list_append_string &
         (global%var_list, var_str ("$lhapdf_file"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$lhapdf_photon_file"), var_str (""), &
          intrinsic=.true.)    
    call var_list_append_int &
         (global%var_list, var_str ("lhapdf_member"), 0, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("lhapdf_photon_scheme"), 0, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?hoppet_b_matching"), .false., &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("isr_alpha"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("isr_q_max"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("isr_mass"), 0._default, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("isr_order"), 3, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?isr_recoil"), .false., &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("epa_alpha"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("epa_x_min"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("epa_q_min"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("epa_e_max"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("epa_mass"), 0._default, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?epa_recoil"), .false., &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("ewa_x_min"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("ewa_pt_max"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("ewa_mass"), 0._default, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?ewa_keep_momentum"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?ewa_keep_energy"), .false., &
          intrinsic=.true.)               
    call var_list_append_log &
         (global%var_list, var_str ("?circe1_photon1"), .false., &
          intrinsic=.true.)     
    call var_list_append_log &
         (global%var_list, var_str ("?circe1_photon2"), .false., &
          intrinsic=.true.)     
    call var_list_append_real &
         (global%var_list, var_str ("circe1_sqrts"), &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?circe1_generate"), .true., &
          intrinsic=.true.)               
    call var_list_append_log &
         (global%var_list, var_str ("?circe1_map"), .true., &
          intrinsic=.true.)     
    call var_list_append_real &
         (global%var_list, var_str ("circe1_mapping_slope"), 2._default, &
          intrinsic=.true.)     
    call var_list_append_real &
         (global%var_list, var_str ("circe1_eps"), 1e-5_default, &
          intrinsic=.true.)               
    call var_list_append_int &
         (global%var_list, var_str ("circe1_ver"), 0, intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("circe1_rev"), 0, intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$circe1_acc"), var_str ("SBAND"), &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("circe1_chat"), 0, intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?circe2_polarized"), .true., &
          intrinsic=.true.)               
    call var_list_append_string &    
         (global%var_list, var_str ("$circe2_file"), &
          intrinsic=.true.)      
    call var_list_append_string &    
         (global%var_list, var_str ("$circe2_design"), var_str ("*"), &
          intrinsic=.true.)               
    call var_list_append_string &    
         (global%var_list, var_str ("$beam_events_file"), &
          intrinsic=.true.)      
    call var_list_append_log &
         (global%var_list, var_str ("?beam_events_warn_eof"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?energy_scan_normalize"), .false., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?alpha_s_is_fixed"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?alpha_s_from_lhapdf"), .false., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?alpha_s_from_pdf_builtin"), .false., &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("alpha_s_order"), 0, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("alpha_s_nf"), 5, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?alpha_s_from_mz"), .false., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?alpha_s_from_lambda_qcd"), .false., &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("lambda_qcd"), 200.e-3_default, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?fatal_beam_decay"), .true., &
          intrinsic=.true.)          
    call var_list_append_log &
         (global%var_list, var_str ("?helicity_selection_active"), .true., &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("helicity_selection_threshold"), &
          1E10_default, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("helicity_selection_cutoff"), 1000, &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$rng_method"), var_str ("tao"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$integration_method"), var_str ("vamp"), &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("threshold_calls"), 10, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("min_calls_per_channel"), 10, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("min_calls_per_bin"), 10, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("min_bins"), 3, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("max_bins"), 20, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?stratified"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?use_vamp_equivalences"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?vamp_verbose"), .false., &
         intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?vamp_history_global"), &
         .true., intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?vamp_history_global_verbose"), &
         .false., intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?vamp_history_channels"), &
         .false., intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?vamp_history_channels_verbose"), &
         .false., intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("channel_weights_power"), 0.25_default, &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$phs_method"), var_str ("default"), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?vis_channels"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?check_phs_file"), .true., &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$phs_file"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?phs_only"), .false., &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("phs_threshold_s"), 50._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("phs_threshold_t"), 100._default, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("phs_off_shell"), 2, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("phs_t_channel"), 6, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("phs_e_scale"), 10._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("phs_m_scale"), 10._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("phs_q_scale"), 10._default, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?phs_keep_nonresonant"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?phs_step_mapping"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?phs_step_mapping_exp"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?phs_s_mapping"), .true., &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$run_id"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("n_calls_test"), 0, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?integration_timer"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?check_grid_file"), .true., &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("accuracy_goal"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("error_goal"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("relative_error_goal"), 0._default, &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("error_threshold"), &
         0._default, intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?vis_history"), .true., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?diags"), .false., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?diags_color"), .false., &
          intrinsic=.true.)    
    call var_list_append_log &
         (global%var_list, var_str ("?check_event_file"), .true., &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$event_file_version"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("n_events"), 0, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?unweighted"), .true., &
          intrinsic=.true.)       
    call var_list_append_real &
         (global%var_list, var_str ("safety_factor"), 1._default, &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?negative_weights"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?keep_beams"), .false., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?recover_beams"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?update_event"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?update_sqme"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?update_weight"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?allow_decays"), .true., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?auto_decays"), .false., &
          intrinsic=.true.)       
    call var_list_append_int &
         (global%var_list, var_str ("auto_decays_multiplicity"), 2, &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?auto_decays_radiative"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?decay_rest_frame"), .false., &
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?isotropic_decay"), .false., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?diagonal_decay"), .false., &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$sample"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$sample_normalization"), var_str ("auto"),&
          intrinsic=.true.)       
    call var_list_append_log &
         (global%var_list, var_str ("?sample_pacify"), .false., &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("sample_max_tries"), 10000, &
         intrinsic = .true.)
    call var_list_append_int &
         (global%var_list, var_str ("sample_split_n_evt"), 0, &
         intrinsic = .true.)
    call var_list_append_int &
         (global%var_list, var_str ("sample_split_index"), 0, &
         intrinsic = .true.)
    call var_list_append_string &
         (global%var_list, var_str ("$rescan_input_format"), var_str ("raw"), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?read_raw"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?write_raw"), .true., &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_raw"), var_str ("evx"), &
         intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_default"), var_str ("evt"), &
         intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$debug_extension"), var_str ("debug"), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?debug_process"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?debug_transforms"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?debug_decay"), .true., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?debug_verbose"), .true., &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_hepevt"), var_str ("hepevt"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_ascii_short"), &
          var_str ("short.evt"), intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_ascii_long"), &
          var_str ("long.evt"), intrinsic=.true.)        
    call var_list_append_string &
         (global%var_list, var_str ("$extension_athena"), &
          var_str ("athena.evt"), intrinsic=.true.) 
    call var_list_append_string &
          (global%var_list, var_str ("$extension_mokka"), &
           var_str ("mokka.evt"), intrinsic=.true.)       
    call var_list_append_string &
         (global%var_list, var_str ("$lhef_version"), var_str ("2.0"), &
         intrinsic = .true.)
    call var_list_append_string &
         (global%var_list, var_str ("$lhef_extension"), var_str ("lhe"), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?lhef_write_sqme_prc"), .true., &
         intrinsic = .true.)
    call var_list_append_log &
         (global%var_list, var_str ("?lhef_write_sqme_ref"), .false., &
         intrinsic = .true.)
    call var_list_append_log &
         (global%var_list, var_str ("?lhef_write_sqme_alt"), .true., &
         intrinsic = .true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_lha"), var_str ("lha"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_hepmc"), var_str ("hepmc"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_stdhep"), var_str ("hep"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_stdhep_up"), &
          var_str ("up.hep"), intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_hepevt_verb"), &
          var_str ("hepevt.verb"), intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$extension_lha_verb"), &
          var_str ("lha.verb"), intrinsic=.true.)
    call var_list_append_int (global%var_list, &
         var_str ("n_bins"), 20, &
         intrinsic=.true.)
    call var_list_append_log (global%var_list, &
         var_str ("?normalize_bins"), .false., &
         intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$obs_label"), var_str (""), &
         intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$obs_unit"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$title"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$description"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$x_label"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string (global%var_list, &
         var_str ("$y_label"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("graph_width_mm"), 130, &
          intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("graph_height_mm"), 90, &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?y_log"), .false., &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?x_log"), .false., &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("x_min"),  &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("x_max"),  &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("y_min"),  &
          intrinsic=.true.)
    call var_list_append_real &
         (global%var_list, var_str ("y_max"),  &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$gmlcode_bg"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$gmlcode_fg"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?draw_histogram"), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?draw_base"), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?draw_piecewise"), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?fill_curve"), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?draw_curve"), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?draw_errors"), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?draw_symbols"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$fill_options"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$draw_options"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$err_options"), &
          intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$symbol"), &
          intrinsic=.true.)
    call var_list_append_real (global%var_list, &
         var_str ("tolerance"), 0._default, &
          intrinsic=.true.)
    call var_list_append_int (global%var_list, &
         var_str ("checkpoint"), 0, &
         intrinsic = .true.)
    call var_list_append_log &
         (global%var_list, var_str ("?pacify"), .false., &
         intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$out_file"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?out_advance"), .true., &
          intrinsic=.true.)
!!! JRR: WK please check (#542)    
!     call var_list_append_log &
!          (global%var_list, var_str ("?out_custom"), .false., &
!           intrinsic=.true.)
!     call var_list_append_string &
!          (global%var_list, var_str ("$out_comment"), var_str ("# "), &
!           intrinsic=.true.)
!     call var_list_append_log &
!          (global%var_list, var_str ("?out_header"), .true., &
!           intrinsic=.true.)
!     call var_list_append_log &
!          (global%var_list, var_str ("?out_yerr"), .true., &
!           intrinsic=.true.)
!     call var_list_append_log &
!          (global%var_list, var_str ("?out_xerr"), .true., &
!           intrinsic=.true.)
    call var_list_append_int (global%var_list, var_str ("real_range"), &
         range (real_specimen), intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, var_str ("real_precision"), &
         precision (real_specimen), intrinsic = .true., locked = .true.)
    call var_list_append_real (global%var_list, var_str ("real_epsilon"), &
         epsilon (real_specimen), intrinsic = .true., locked = .true.)
    call var_list_append_real (global%var_list, var_str ("real_tiny"), &
         tiny (real_specimen), intrinsic = .true., locked = .true.)
    !!! FastJet parameters
    call var_list_append_int (global%var_list, &
         var_str ("kt_algorithm"), &
         kt_algorithm, &
         intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("cambridge_algorithm"), &
         cambridge_algorithm, intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("antikt_algorithm"), &
         antikt_algorithm, &
         intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("genkt_algorithm"), &
         genkt_algorithm, &
         intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("cambridge_for_passive_algorithm"), &
         cambridge_for_passive_algorithm, &
         intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("genkt_for_passive_algorithm"), &
         genkt_for_passive_algorithm, &
         intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("ee_kt_algorithm"), &
         ee_kt_algorithm, &
         intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("ee_genkt_algorithm"), &
         ee_genkt_algorithm, &
         intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("plugin_algorithm"), &
         plugin_algorithm, &
         intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("undefined_jet_algorithm"), &
         undefined_jet_algorithm, &
         intrinsic = .true., locked = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("jet_algorithm"), undefined_jet_algorithm, &
         intrinsic = .true.)
    call var_list_append_real (global%var_list, &
         var_str ("jet_r"), 0._default, &
         intrinsic = .true.)
    call var_list_append_log &
         (global%var_list, var_str ("?polarized_events"), .false., &
            intrinsic=.true.)
    !!! Default settings for shower
    call var_list_append_log &
         (global%var_list, var_str ("?allow_shower"), .true., &
            intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?ps_fsr_active"), .false., &
            intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?ps_use_PYTHIA_shower"), .false., &
            intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?ps_PYTHIA_verbose"), .false., &
            intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$ps_PYTHIA_PYGIVE"), var_str (""), &
          intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?ps_isr_active"), .false., &
            intrinsic=.true.)
    call var_list_append_real (global%var_list, &
         var_str ("ps_mass_cutoff"), 1._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, &
         var_str ("ps_fsr_lambda"), 0.29_default, intrinsic = .true.)
    call var_list_append_real (global%var_list, &
         var_str ("ps_isr_lambda"), 0.29_default, intrinsic = .true.)
    call var_list_append_int (global%var_list, &
         var_str ("ps_max_n_flavors"), 5, intrinsic = .true.)
    call var_list_append_log &
         (global%var_list, var_str ("?ps_isr_alpha_s_running"), .true., &
            intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?ps_fsr_alpha_s_running"), .true., &
            intrinsic=.true.)
    call var_list_append_real (global%var_list, var_str ("ps_fixed_alpha_s"), &
         0._default, intrinsic = .true.)
    call var_list_append_log &
         (global%var_list, var_str ("?ps_isr_pt_ordered"), .false., &
            intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?ps_isr_angular_ordered"), .true., &
            intrinsic=.true.)
    call var_list_append_real (global%var_list, var_str &
         ("ps_isr_primordial_kt_width"), 0._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str &
         ("ps_isr_primordial_kt_cutoff"), 5._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str &
         ("ps_isr_z_cutoff"), 0.999_default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str &
         ("ps_isr_minenergy"), 1._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str &
         ("ps_isr_tscalefactor"), 1._default, intrinsic = .true.)
    call var_list_append_log (global%var_list, var_str &
         ("?ps_isr_only_onshell_emitted_partons"), .false., intrinsic=.true.)
    !!! Default settings for hadronization
    call var_list_append_log &
         (global%var_list, var_str ("?hadronization_active"), .false., &
            intrinsic=.true.)
    !!! Setting for mlm matching
    call var_list_append_log &
         (global%var_list, var_str ("?mlm_matching"), .false., &
            intrinsic=.true.)
    call var_list_append_real (global%var_list, var_str &
         ("mlm_Qcut_ME"), 0._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str &
         ("mlm_Qcut_PS"), 0._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str &
         ("mlm_ptmin"), 0._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str &
         ("mlm_etamax"), 0._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str &
         ("mlm_Rmin"), 0._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str &
         ("mlm_Emin"), 0._default, intrinsic = .true.)
    call var_list_append_int (global%var_list, var_str &
         ("mlm_nmaxMEjets"), 0, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str &
         ("mlm_ETclusfactor"), 0.2_default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str &
         ("mlm_ETclusminE"), 5._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str &
         ("mlm_etaclusfactor"), 1._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str &
         ("mlm_Rclusfactor"), 1._default, intrinsic = .true.)
    call var_list_append_real (global%var_list, var_str &
         ("mlm_Eclusfactor"), 1._default, intrinsic = .true.)
    call var_list_append_log &
         (global%var_list, var_str ("?ckkw_matching"), .false., &
            intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?muli_active"), .false., &
            intrinsic=.true.)
    call var_list_append_string &
         (global%var_list, var_str ("$pdf_builtin_set"), var_str ("CTEQ6L"), &
         intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?omega_openmp"), &
         openmp_is_active (), &
         intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?openmp_is_active"), &
         openmp_is_active (), &
         locked=.true., intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("openmp_num_threads_default"), &
         openmp_get_default_max_threads (), &
         locked=.true., intrinsic=.true.)
    call var_list_append_int &
         (global%var_list, var_str ("openmp_num_threads"), &        
         openmp_get_max_threads (), &
         intrinsic=.true.)
    call var_list_append_log &
         (global%var_list, var_str ("?openmp_logging"), &
         .true., intrinsic=.true.)    
    call global%init_pointer_variables ()
    call global%process_stack%init_var_list (global%var_list)
  end subroutine rt_data_global_init

  subroutine rt_data_local_init (local, global, env)
    class(rt_data_t), intent(inout), target :: local
    class(rt_data_t), intent(in), target :: global
    integer, intent(in), optional :: env
    call var_list_link (local%var_list, global%var_list)
    if (associated (global%model)) then
       call var_list_init_copies (local%var_list, &
            model_get_var_list_ptr (global%model), &
            derived_only = .true.)
    end if
    call local%init_pointer_variables ()
    local%fallback_model => global%fallback_model
    local%os_data = global%os_data
    local%logfile = global%logfile
  end subroutine rt_data_local_init

  subroutine rt_data_copy_globals (global, local)
    class(rt_data_t), intent(in) :: global
    class(rt_data_t), intent(inout) :: local
    local%model_list = global%model_list
    local%prclib_stack = global%prclib_stack
    local%process_stack = global%process_stack
  end subroutine rt_data_copy_globals
 
  subroutine rt_data_init_pointer_variables (local)
    class(rt_data_t), intent(inout), target :: local
    logical, target, save :: known = .true.
    call var_list_append_string_ptr &
         (local%var_list, var_str ("$fc"), local%os_data%fc, known, &
          intrinsic=.true.)
    call var_list_append_string_ptr &
         (local%var_list, var_str ("$fcflags"), local%os_data%fcflags, known, &
         intrinsic=.true.)
  end subroutine rt_data_init_pointer_variables

  subroutine rt_data_link (local, global)
    class(rt_data_t), intent(inout), target :: local
    class(rt_data_t), intent(in), target :: global
    local%lexer => global%lexer
    call global%copy_globals (local)
    local%os_data = global%os_data
    local%logfile = global%logfile
    call var_list_link (local%var_list, global%var_list)
    if (associated (global%model)) then
       local%model => &
            local%model_list%get_model_ptr (global%model%get_name ())
       call var_list_synchronize (local%var_list, &
            model_get_var_list_ptr (local%model), reset_pointers = .true.)
       call local%particle_stack%init (local%model)
    end if
    if (associated (global%prclib)) then
       local%prclib => &
            local%prclib_stack%get_library_ptr (global%prclib%get_name ())
    end if
    local%it_list = global%it_list
    local%beam_structure = global%beam_structure
    local%lhapdf_status = global%lhapdf_status
    local%pn = global%pn
    if (allocated (local%sample_fmt))  deallocate (local%sample_fmt)
    if (allocated (global%sample_fmt)) then
       allocate (local%sample_fmt (size (global%sample_fmt)), &
            source = global%sample_fmt)
    end if
    local%out_files => global%out_files
  end subroutine rt_data_link

  subroutine rt_data_restore (global, local, keep_model_vars, keep_local)
    class(rt_data_t), intent(inout) :: global
    class(rt_data_t), intent(inout) :: local
    logical, intent(in), optional :: keep_model_vars, keep_local
    logical :: same_model, restore, delete
    delete = .true.;  if (present (keep_local))  delete = .not. keep_local
    if (delete) then
       call var_list_undefine (local%var_list, follow_link=.false.)
    else
       if (associated (local%model)) then
          call model_pointer_to_instance (local%model)
          call var_list_synchronize (local%var_list, &
               model_get_var_list_ptr (local%model), reset_pointers = .true.)
       end if
    end if
    if (associated (global%model)) then 
       call local%particle_stack%restore_model ()
       same_model = &
            global%model%get_name () == local%model%get_name ()
       if (present (keep_model_vars) .and. same_model) then
          restore = .not. keep_model_vars
       else
          if (.not. same_model)  call msg_message ("Restoring model '" // &
               char (global%model%get_name ()) // "'")
          restore = .true.
       end if
       if (restore) then
          call var_list_restore (global%var_list)
       else
          call var_list_synchronize &
               (global%var_list, model_get_var_list_ptr (global%model))
       end if
    end if
    call global%restore_globals (local)
  end subroutine rt_data_restore

  subroutine rt_data_restore_globals (global, local)
    class(rt_data_t), intent(inout) :: global
    class(rt_data_t), intent(in) :: local
    global%model_list = local%model_list
    global%prclib_stack = local%prclib_stack
    global%process_stack = local%process_stack
  end subroutine rt_data_restore_globals
 
  subroutine rt_data_global_final (global)
    class(rt_data_t), intent(inout) :: global
    call global%process_stack%final ()
    call global%prclib_stack%final ()
    call global%particle_stack%final ()
    call global%model_list%final ()
    call var_list_final (global%var_list)
    if (associated (global%out_files)) then
       call file_list_final (global%out_files)
       deallocate (global%out_files)
    end if
  end subroutine rt_data_global_final

  subroutine rt_data_local_final (local)
    class(rt_data_t), intent(inout) :: local
    call var_list_final (local%var_list)
  end subroutine rt_data_local_final

  subroutine rt_data_init_fallback_model (global, name, filename)
    class(rt_data_t), intent(inout) :: global
    type(string_t), intent(in) :: name, filename
    call global%model_list%read_model &
         (name, filename, global%os_data, global%fallback_model)
  end subroutine rt_data_init_fallback_model
  
  subroutine rt_data_read_model (global, name, filename, synchronize)
    class(rt_data_t), intent(inout) :: global
    type(string_t), intent(in) :: name, filename
    type(var_list_t), pointer :: model_vars
    logical, intent(in), optional :: synchronize
    logical :: sync
    sync = .true.;  if (present (synchronize))  sync = synchronize
    call global%model_list%read_model &
         (name, filename, global%os_data, global%model)
    if (associated (global%model)) then
       call var_list_set_string (global%var_list, var_str ("$model_name"), &
            name, is_known = .true.)
       model_vars => model_get_var_list_ptr (global%model)
       call var_list_init_copies (global%var_list, model_vars)
       if (sync) then
          call var_list_synchronize (global%var_list, model_vars, &
               reset_pointers = .true.)
       end if
       call global%particle_stack%reset (global%model)
    end if
  end subroutine rt_data_read_model
    
  subroutine rt_data_select_model (global, name)
    class(rt_data_t), intent(inout) :: global
    type(string_t), intent(in) :: name
    type(var_list_t), pointer :: model_vars
    global%model => global%model_list%get_model_ptr (name)
    if (associated (global%model)) then
       call var_list_set_string (global%var_list, var_str ("$model_name"), &
            name, is_known = .true.)
       model_vars => model_get_var_list_ptr (global%model)
       call var_list_synchronize (global%var_list, model_vars, &
            reset_pointers = .true.)
       call global%particle_stack%reset (global%model)
    end if
  end subroutine rt_data_select_model
  
  subroutine rt_data_modify_particle &
       (global, pdg, polarized, stable, decay, isotropic_decay, diagonal_decay)
    class(rt_data_t), intent(inout) :: global
    integer, intent(in) :: pdg
    logical, intent(in), optional :: polarized, stable
    logical, intent(in), optional :: isotropic_decay, diagonal_decay
    type(string_t), dimension(:), intent(in), optional :: decay
    if (.not. global%particle_stack%contains (pdg)) then
       call global%particle_stack%push (pdg)
    end if
    if (present (polarized)) then
       if (polarized) then
          call model_set_polarized (global%model, pdg)
       else
          call model_set_unpolarized (global%model, pdg)
       end if
    end if
    if (present (stable)) then
       if (stable) then
          call model_set_stable (global%model, pdg)
       else if (present (decay)) then
          call model_set_unstable &
               (global%model, pdg, decay, isotropic_decay, diagonal_decay)
       else
          call msg_bug ("Setting particle unstable: missing decay processes")
       end if
    end if
  end subroutine rt_data_modify_particle

  subroutine rt_data_add_prclib (global, prclib_entry)
    class(rt_data_t), intent(inout) :: global
    type(prclib_entry_t), intent(inout), pointer :: prclib_entry
    call global%prclib_stack%push (prclib_entry)
    call global%update_prclib (global%prclib_stack%get_first_ptr ())
  end subroutine rt_data_add_prclib
  
  subroutine rt_data_update_prclib (global, lib)
    class(rt_data_t), intent(inout) :: global
    type(process_library_t), intent(in), target :: lib
    type(var_entry_t), pointer :: var
    global%prclib => lib
    var => var_list_get_var_ptr (global%var_list, &
         var_str ("$library_name"), follow_link = .false.)
    if (associated (var)) then
       call var_entry_set_string (var, &
            global%prclib%get_name (), is_known=.true.)
    else
       call var_list_append_string (global%var_list, &
            var_str ("$library_name"), global%prclib%get_name (), &
            intrinsic = .true.)
    end if
  end subroutine rt_data_update_prclib

  function rt_data_get_helicity_selection (rt_data) result (helicity_selection)
    class(rt_data_t), intent(in) :: rt_data
    type(helicity_selection_t) :: helicity_selection
    associate (var_list => rt_data%var_list)
      helicity_selection%active = var_list_get_lval (var_list, &
           var_str ("?helicity_selection_active"))
      if (helicity_selection%active) then
         helicity_selection%threshold = var_list_get_rval (var_list, &
              var_str ("helicity_selection_threshold"))
         helicity_selection%cutoff = var_list_get_ival (var_list, &
              var_str ("helicity_selection_cutoff"))
      end if
    end associate
  end function rt_data_get_helicity_selection

  subroutine rt_data_show_beams (rt_data, unit)
    class(rt_data_t), intent(in) :: rt_data
    integer, intent(in), optional :: unit
    type(string_t) :: s
    integer :: u
    u = output_unit (unit)
    associate (beams => rt_data%beam_structure, var_list => rt_data%var_list)
      call beams%write (u)
      if (.not. beams%asymmetric () .and. beams%get_n_beam () == 2) then
         write (u, "(2x,A,ES19.12,1x,'GeV')") "sqrts =", &
              var_list_get_rval (var_list, var_str ("sqrts"))         
      end if
      if (beams%contains ("pdf_builtin")) then
         s = var_list_get_sval (var_list, var_str ("$pdf_builtin_set"))
         if (s /= "") then
            write (u, "(2x,A,1x,3A)")  "PDF set =", '"', char (s), '"'
         else
            write (u, "(2x,A,1x,A)")  "PDF set =", "[undefined]"
         end if
      end if
      if (beams%contains ("lhapdf")) then
         s = var_list_get_sval (var_list, var_str ("$lhapdf_dir"))
         if (s /= "") then
            write (u, "(2x,A,1x,3A)")  "LHAPDF dir    =", '"', char (s), '"'
         end if
         s = var_list_get_sval (var_list, var_str ("$lhapdf_file"))
         if (s /= "") then
            write (u, "(2x,A,1x,3A)")  "LHAPDF file   =", '"', char (s), '"'
            write (u, "(2x,A,1x,I0)") "LHAPDF member =", &
                 var_list_get_ival (var_list, var_str ("lhapdf_member"))
         else
            write (u, "(2x,A,1x,A)")  "LHAPDF file   =", "[undefined]"
         end if
      end if
      if (beams%contains ("lhapdf_photon")) then
         s = var_list_get_sval (var_list, var_str ("$lhapdf_dir"))
         if (s /= "") then
            write (u, "(2x,A,1x,3A)")  "LHAPDF dir    =", '"', char (s), '"'
         end if
         s = var_list_get_sval (var_list, var_str ("$lhapdf_photon_file"))
         if (s /= "") then
            write (u, "(2x,A,1x,3A)")  "LHAPDF file   =", '"', char (s), '"'
            write (u, "(2x,A,1x,I0)") "LHAPDF member =", &
                 var_list_get_ival (var_list, var_str ("lhapdf_member"))
            write (u, "(2x,A,1x,I0)") "LHAPDF scheme =", &
                 var_list_get_ival (var_list, &
                 var_str ("lhapdf_photon_scheme"))
         else
            write (u, "(2x,A,1x,A)")  "LHAPDF file   =", "[undefined]"
         end if
      end if
      if (beams%contains ("isr")) then
         write (u, "(2x,A,ES19.12)") "ISR alpha =", &
              var_list_get_rval (var_list, var_str ("isr_alpha"))
         write (u, "(2x,A,ES19.12)") "ISR Q max =", &
              var_list_get_rval (var_list, var_str ("isr_q_max"))
         write (u, "(2x,A,ES19.12)") "ISR mass  =", &
              var_list_get_rval (var_list, var_str ("isr_mass"))
         write (u, "(2x,A,1x,I0)") "ISR order  =", &
              var_list_get_ival (var_list, var_str ("isr_order"))
         write (u, "(2x,A,1x,L1)") "ISR recoil =", &
              var_list_get_lval (var_list, var_str ("?isr_recoil"))
      end if
      if (beams%contains ("epa")) then
         write (u, "(2x,A,ES19.12)") "EPA alpha  =", &
              var_list_get_rval (var_list, var_str ("epa_alpha"))
         write (u, "(2x,A,ES19.12)") "EPA x min  =", &
              var_list_get_rval (var_list, var_str ("epa_x_min"))
         write (u, "(2x,A,ES19.12)") "EPA Q min  =", &
              var_list_get_rval (var_list, var_str ("epa_q_min"))
         write (u, "(2x,A,ES19.12)") "EPA E max  =", &
              var_list_get_rval (var_list, var_str ("epa_e_max"))
         write (u, "(2x,A,ES19.12)") "EPA mass   =", &
              var_list_get_rval (var_list, var_str ("epa_mass"))
         write (u, "(2x,A,1x,L1)") "EPA recoil =", &
              var_list_get_lval (var_list, var_str ("?epa_recoil"))
      end if
      if (beams%contains ("ewa")) then
         write (u, "(2x,A,ES19.12)") "EWA x min       =", &
              var_list_get_rval (var_list, var_str ("ewa_x_min"))
         write (u, "(2x,A,ES19.12)") "EWA Pt max      =", &
              var_list_get_rval (var_list, var_str ("ewa_pt_max"))
         write (u, "(2x,A,ES19.12)") "EWA mass        =", &
              var_list_get_rval (var_list, var_str ("ewa_mass"))
         write (u, "(2x,A,1x,L1)") "EWA mom cons.   =", &
              var_list_get_lval (var_list, &
              var_str ("?ewa_keep_momentum"))
         write (u, "(2x,A,1x,L1)") "EWA energ. cons. =", &
              var_list_get_lval (var_list, &
              var_str ("ewa_keep_energy"))
      end if
      if (beams%contains ("circe1")) then
         write (u, "(2x,A,1x,I0)") "CIRCE1 version    =", &
              var_list_get_ival (var_list, var_str ("circe1_ver"))
         write (u, "(2x,A,1x,I0)") "CIRCE1 revision   =", &
              var_list_get_ival (var_list, var_str ("circe1_rev")) 
         s = var_list_get_sval (var_list, var_str ("$circe1_acc"))
         write (u, "(2x,A,1x,A)") "CIRCE1 acceler.   =", char (s)
         write (u, "(2x,A,1x,I0)") "CIRCE1 chattin.   =", &
              var_list_get_ival (var_list, var_str ("circe1_chat"))
         write (u, "(2x,A,ES19.12)") "CIRCE1 sqrts      =", &
              var_list_get_rval (var_list, var_str ("circe1_sqrts"))
         write (u, "(2x,A,ES19.12)") "CIRCE1 epsil.     =", &
              var_list_get_rval (var_list, var_str ("circe1_eps"))
         write (u, "(2x,A,1x,L1)") "CIRCE1 phot. 1  =", &
              var_list_get_lval (var_list, var_str ("?circe1_photon1"))
         write (u, "(2x,A,1x,L1)") "CIRCE1 phot. 2  =", &
              var_list_get_lval (var_list, var_str ("?circe1_photon2"))
         write (u, "(2x,A,1x,L1)") "CIRCE1 generat. =", &
              var_list_get_lval (var_list, var_str ("?circe1_generate"))
         write (u, "(2x,A,1x,L1)") "CIRCE1 mapping  =", &
              var_list_get_lval (var_list, var_str ("?circe1_map"))
         write (u, "(2x,A,ES19.12)") "CIRCE1 map. slope =", &
              var_list_get_rval (var_list, var_str ("circe1_mapping_slope"))
      end if
      if (beams%contains ("circe2")) then
         s = var_list_get_sval (var_list, var_str ("$circe2_design"))
         write (u, "(2x,A,1x,A)") "CIRCE2 design   =", char (s) 
         s = var_list_get_sval (var_list, var_str ("$circe2_file"))
         write (u, "(2x,A,1x,A)") "CIRCE2 file     =", char (s)
         write (u, "(2x,A,1x,L1)") "CIRCE2 polarized =", &
              var_list_get_lval (var_list, var_str ("?circe2_polarized"))
      end if
      if (beams%contains ("beam_events")) then
         s = var_list_get_sval (var_list, var_str ("$beam_events_file"))
         write (u, "(2x,A,1x,A)") "Beam events file     =", char (s)
         write (u, "(2x,A,1x,L1)") "Beam events EOF warn =", &
              var_list_get_lval (var_list, var_str ("?beam_events_warn_eof"))
      end if
    end associate
  end subroutine rt_data_show_beams
  
  function rt_data_get_sqrts (rt_data) result (sqrts)
    class(rt_data_t), intent(in) :: rt_data
    real(default) :: sqrts
    sqrts = var_list_get_rval (rt_data%var_list, var_str ("sqrts"))
  end function rt_data_get_sqrts
    
  subroutine rt_data_pacify (rt_data, efficiency_reset, error_reset)
    class(rt_data_t), intent(inout) :: rt_data
    logical, intent(in), optional :: efficiency_reset, error_reset
    type(process_entry_t), pointer :: process
    process => rt_data%process_stack%first
    do while (associated (process))
       call process%pacify (efficiency_reset, error_reset)
       process => process%next
    end do    
  end subroutine rt_data_pacify


  subroutine rt_data_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (rt_data_1, "rt_data_1", &
         "initialize", &
         u, results)
    call test (rt_data_2, "rt_data_2", &
         "fill", &
         u, results)
    call test (rt_data_3, "rt_data_3", &
         "save/restore", &
         u, results)
    call test (rt_data_4, "rt_data_4", &
         "show variables", &
         u, results)
    call test (rt_data_5, "rt_data_5", &
         "show parts", &
         u, results)
    call test (rt_data_6, "rt_data_6", &
         "local model", &
         u, results)
    call test (rt_data_7, "rt_data_7", &
         "result variables", &
         u, results)
    call test (rt_data_8, "rt_data_8", &
         "beam energy", &
         u, results)
  end subroutine rt_data_test

  subroutine rt_data_fix_system_dependencies (rt_data)
    class(rt_data_t), intent(inout), target :: rt_data
    type(var_list_t), pointer :: var_list
    type(var_entry_t), pointer :: var
    var_list => rt_data%var_list

    call var_list_set_log (var_list, &
         var_str ("?omega_openmp"), .false., is_known = .true.)

    var => var_list_get_var_ptr (var_list, &
         var_str ("?openmp_is_active"), V_LOG)
    call var_entry_set_log (var, .false., is_known = .true.)
    var => var_list_get_var_ptr (var_list, &
         var_str ("openmp_num_threads_default"), V_INT)
    call var_entry_set_int (var, 1, is_known = .true.)
    call var_list_set_int (var_list, &
         var_str ("openmp_num_threads"), 1, is_known = .true.)        
    var => var_list_get_var_ptr (var_list, &
         var_str ("real_range"), V_INT)
    call var_entry_set_int (var, 307, is_known = .true.)
    var => var_list_get_var_ptr (var_list, &
         var_str ("real_precision"), V_INT)
    call var_entry_set_int (var, 15, is_known = .true.)    
    var => var_list_get_var_ptr (var_list, &
         var_str ("real_epsilon"), V_REAL)
    call var_entry_set_real (var, 1.e-16_default, is_known = .true.)
    var => var_list_get_var_ptr (var_list, &
         var_str ("real_tiny"), V_REAL)
    call var_entry_set_real (var, 1.e-300_default, is_known = .true.)     
    
    rt_data%os_data%fc = "Fortran-compiler"
    rt_data%os_data%fcflags = "Fortran-flags"
        
  end subroutine rt_data_fix_system_dependencies
  
  subroutine rt_data_1 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: rt_data
    
    write (u, "(A)")  "* Test output: rt_data_1"
    write (u, "(A)")  "*   Purpose: initialize global runtime data"
    write (u, "(A)")

    call rt_data%global_init (logfile = var_str ("rt_data.log"))

    call rt_data%fix_system_dependencies ()
    call var_list_set_int (rt_data%var_list, var_str ("seed"), &
         0, is_known=.true.)            

    call rt_data%it_list%init ([2, 3], [5000, 20000])

    call rt_data%write (u)

    call rt_data%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: rt_data_1"
    
  end subroutine rt_data_1
  
  subroutine rt_data_2 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: rt_data
    type(flavor_t), dimension(2) :: flv
    type(string_t) :: cut_expr_text
    type(ifile_t) :: ifile
    type(stream_t) :: stream
    type(parse_tree_t) :: parse_tree
    
    write (u, "(A)")  "* Test output: rt_data_2"
    write (u, "(A)")  "*   Purpose: initialize global runtime data &
         &and fill contents"
    write (u, "(A)")

    call syntax_model_file_init ()

    call rt_data%global_init ()
    call rt_data%fix_system_dependencies ()

    call rt_data%read_model (var_str ("Test"), var_str ("Test.mdl"))

    call var_list_set_real (rt_data%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)
    call var_list_set_int (rt_data%var_list, var_str ("seed"), &
         0, is_known=.true.)        
    call flavor_init (flv, [25,25], rt_data%model)
    
    call var_list_set_string (rt_data%var_list, var_str ("$run_id"), &
         var_str ("run1"), is_known = .true.)
    call var_list_set_real (rt_data%var_list, var_str ("luminosity"), &
         33._default, is_known = .true.)
    
    call syntax_pexpr_init ()
    cut_expr_text = "all Pt > 100 [s]"
    call ifile_append (ifile, cut_expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_lexpr (parse_tree, stream, .true.)
    rt_data%pn%cuts_lexpr => parse_tree_get_root_ptr (parse_tree)
    
    allocate (rt_data%sample_fmt (2))
    rt_data%sample_fmt(1) = "foo_fmt"
    rt_data%sample_fmt(2) = "bar_fmt"
    
    call rt_data%write (u)

    call parse_tree_final (parse_tree)
    call stream_final (stream)
    call ifile_final (ifile)
    call syntax_pexpr_final ()

    call rt_data%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: rt_data_2"
    
  end subroutine rt_data_2
  
  subroutine rt_data_3 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: rt_data, local
    type(var_list_t), pointer :: model_vars
    type(var_entry_t), pointer :: var
    type(flavor_t), dimension(2) :: flv
    type(string_t) :: cut_expr_text
    type(ifile_t) :: ifile
    type(stream_t) :: stream
    type(parse_tree_t) :: parse_tree
    type(prclib_entry_t), pointer :: lib
    
    write (u, "(A)")  "* Test output: rt_data_3"
    write (u, "(A)")  "*   Purpose: initialize global runtime data &
         &and fill contents;"
    write (u, "(A)")  "*            copy to local block and back"
    write (u, "(A)")

    write (u, "(A)")  "* Init global data"
    write (u, "(A)")

    call syntax_model_file_init ()

    call rt_data%global_init ()
    call rt_data%fix_system_dependencies ()
    call var_list_set_int (rt_data%var_list, var_str ("seed"), &
         0, is_known=.true.)        

    call rt_data%read_model (var_str ("Test"), var_str ("Test.mdl"))

    call var_list_set_real (rt_data%var_list, var_str ("sqrts"),&
         1000._default, is_known = .true.)
    call flavor_init (flv, [25,25], rt_data%model)
    
    call rt_data%beam_structure%init_sf (flavor_get_name (flv), [1])
    call rt_data%beam_structure%set_sf (1, 1, var_str ("pdf_builtin"))

    call var_list_set_string (rt_data%var_list, var_str ("$run_id"), &
         var_str ("run1"), is_known = .true.)
    call var_list_set_real (rt_data%var_list, var_str ("luminosity"), &
         33._default, is_known = .true.)
    
    call syntax_pexpr_init ()
    cut_expr_text = "all Pt > 100 [s]"
    call ifile_append (ifile, cut_expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_lexpr (parse_tree, stream, .true.)
    rt_data%pn%cuts_lexpr => parse_tree_get_root_ptr (parse_tree)
    
    allocate (rt_data%sample_fmt (2))
    rt_data%sample_fmt(1) = "foo_fmt"
    rt_data%sample_fmt(2) = "bar_fmt"

    allocate (lib)
    call lib%init (var_str ("library_1"))
    call rt_data%add_prclib (lib)

    write (u, "(A)")  "* Init and modify local data"
    write (u, "(A)")

    call local%local_init (rt_data)
    call local%link (rt_data)

    write (u, "(1x,A,L1)")  "model associated   = ", associated (local%model)
    write (u, "(1x,A,L1)")  "library associated = ", associated (local%prclib)
    write (u, *)

    var => var_list_get_var_ptr (local%var_list, var_str ("ms"))
    if (var_entry_is_copy (var)) then
       call var_list_init_copy (local%var_list, var, user=.true.)
       model_vars => model_get_var_list_ptr (local%model)
       call var_list_set_original_pointer (local%var_list, var_str ("ms"), &
            model_vars)
       call var_list_set_real (local%var_list, var_str ("ms"), &
         150._default, is_known = .true., model_name = var_str ("Test"))
       call model_parameters_update (local%model)
       call var_list_synchronize (local%var_list, model_vars)
    end if

    call var_list_append_string (local%var_list, &
         var_str ("$integration_method"), intrinsic = .true., user = .true.)
    call var_list_set_string (local%var_list, var_str ("$integration_method"), &
         var_str ("midpoint"), is_known = .true.)
    
    call var_list_append_string (local%var_list, &
         var_str ("$phs_method"), intrinsic = .true., user = .true.)
    call var_list_set_string (local%var_list, var_str ("$phs_method"), &
         var_str ("single"), is_known = .true.)

    local%os_data%fc = "Local compiler"
    
    allocate (lib)
    call lib%init (var_str ("library_2"))
    call local%add_prclib (lib)

    call local%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Restore global data"
    write (u, "(A)")
    
    call rt_data%restore (local)

    write (u, "(1x,A,L1)")  "model associated   = ", associated (rt_data%model)
    write (u, "(1x,A,L1)")  "library associated = ", associated (rt_data%prclib)
    write (u, *)

    call rt_data%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call parse_tree_final (parse_tree)
    call stream_final (stream)
    call ifile_final (ifile)
    call syntax_pexpr_final ()

    call rt_data%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: rt_data_3"
    
  end subroutine rt_data_3
  
  subroutine rt_data_4 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: rt_data
    
    type(string_t), dimension(0) :: empty_string_array

    write (u, "(A)")  "* Test output: rt_data_4"
    write (u, "(A)")  "*   Purpose: display selected variables"
    write (u, "(A)")

    call rt_data%global_init ()

    write (u, "(A)")  "* No variables:"
    write (u, "(A)")

    call rt_data%write_vars (u, empty_string_array)

    write (u, "(A)")  "* Two variables:"
    write (u, "(A)")

    call rt_data%write_vars (u, &
         [var_str ("?unweighted"), var_str ("$phs_method")])
    
    write (u, "(A)")
    write (u, "(A)")  "* Display whole record with selected variables"
    write (u, "(A)")

    call rt_data%write (u, &
         vars = [var_str ("?unweighted"), var_str ("$phs_method")])

    call rt_data%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: rt_data_4"
    
  end subroutine rt_data_4
  
  subroutine rt_data_5 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: rt_data
    
    write (u, "(A)")  "* Test output: rt_data_5"
    write (u, "(A)")  "*   Purpose: display parts of rt data"
    write (u, "(A)")

    call rt_data%global_init ()
    call rt_data%write_libraries (u)

    write (u, "(A)")

    call rt_data%write_beams (u)

    write (u, "(A)")

    call rt_data%write_process_stack (u)

    call rt_data%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: rt_data_5"
    
  end subroutine rt_data_5
  
  function is_stable (pdg, rt_data) result (flag)
    integer, intent(in) :: pdg
    type(rt_data_t), intent(in) :: rt_data
    logical :: flag
    type(flavor_t) :: flv
    call flavor_init (flv, pdg, rt_data%model)
    flag = flavor_is_stable (flv)
  end function is_stable
   
  function is_polarized (pdg, rt_data) result (flag)
    integer, intent(in) :: pdg
    type(rt_data_t), intent(in) :: rt_data
    logical :: flag
    type(flavor_t) :: flv
    call flavor_init (flv, pdg, rt_data%model)
    flag = flavor_is_polarized (flv)
  end function is_polarized
    
  subroutine rt_data_6 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: rt_data, local
    type(var_list_t), pointer :: model_vars
    type(var_entry_t), pointer :: var_entry
    type(string_t) :: var_name

    write (u, "(A)")  "* Test output: rt_data_6"
    write (u, "(A)")  "*   Purpose: apply and keep local modifications to model"
    write (u, "(A)")

    call syntax_model_file_init ()

    call rt_data%global_init ()
    call rt_data%read_model (var_str ("Test"), var_str ("Test.mdl"), &
         synchronize=.true.)
    
    write (u, "(A)")  "* Original model"
    write (u, "(A)")

    call rt_data%write_model_list (u)
    write (u, *)
    write (u, "(A,L1)")  "s is stable    = ", is_stable (25, rt_data)
    write (u, "(A,L1)")  "f is polarized = ", is_polarized (6, rt_data)

    write (u, *)

    var_name = "ff"

    write (u, "(A)", advance="no")  "Global model variable: "
    model_vars => model_get_var_list_ptr (rt_data%model)
    call var_list_write_var (model_vars, var_name, u)

    write (u, "(A)", advance="no")  "Global variable: "
    call var_list_write_var (rt_data%var_list, var_name, u)

    write (u, "(A)")
    write (u, "(A)")  "* Apply local modifications: unstable"
    write (u, "(A)")

    call local%local_init (rt_data)
    call local%link (rt_data)

    var_entry => var_list_get_var_ptr &
       (rt_data%var_list, var_name, V_REAL, follow_link=.false.)
    call var_list_init_copy (local%var_list, var_entry, user=.true.)

    call var_list_set_original_pointer (local%var_list, var_name, &
         model_get_var_list_ptr (local%model))
    call var_list_set_real (local%var_list, var_name, 0.4_default, &
         is_known = .true., verbose = .true., model_name = var_str ("Test"))
    call var_list_restore (local%var_list)
    call model_parameters_update (local%model)

    call local%modify_particle (25, stable = .false., decay = [var_str ("d1")])
    call local%modify_particle (6, stable = .false., &
         decay = [var_str ("f1")], isotropic_decay = .true.)
    call local%modify_particle (-6, stable = .false., &
         decay = [var_str ("f2"), var_str ("f3")], diagonal_decay = .true.)

    call model_write (local%model, u)

    write (u, "(A)")
    write (u, "(A)")  "* Further modifications"
    write (u, "(A)")

    call local%modify_particle (6, stable = .false., &
         decay = [var_str ("f1")], &
         diagonal_decay = .true., isotropic_decay = .false.)
    call local%modify_particle (-6, stable = .false., &
         decay = [var_str ("f2"), var_str ("f3")], &
         diagonal_decay = .false., isotropic_decay = .true.)
    call model_write (local%model, u)

    write (u, "(A)")
    write (u, "(A)")  "* Further modifications: f stable but polarized"
    write (u, "(A)")

    call local%modify_particle (6, stable = .true., polarized = .true.)
    call local%modify_particle (-6, stable = .true.)
    call model_write (local%model, u)

    write (u, *)
    
    call local%particle_stack%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Global model"
    write (u, "(A)")

    call model_write (local%model, u)
    write (u, *)
    write (u, "(A,L1)")  "s is stable    = ", is_stable (25, rt_data)
    write (u, "(A,L1)")  "f is polarized = ", is_polarized (6, rt_data)

    write (u, "(A)")
    write (u, "(A)")  "* Local model"
    write (u, "(A)")

    call model_write (local%model, u)
    write (u, *)
    write (u, "(A,L1)")  "s is stable    = ", is_stable (25, local)
    write (u, "(A,L1)")  "f is polarized = ", is_polarized (6, local)

    write (u, *)

    write (u, "(A)", advance="no")  "Global model variable: "
    model_vars => model_get_var_list_ptr (rt_data%model)
    call var_list_write_var (model_vars, var_name, u)

    write (u, "(A)", advance="no")  "Local model variable: "
    call var_list_write_var (model_get_var_list_ptr (local%model), &
         var_name, u)

    write (u, "(A)", advance="no")  "Global variable: "
    call var_list_write_var (rt_data%var_list, var_name, u)

    write (u, "(A)", advance="no")  "Local variable: "
    call var_list_write_var (local%var_list, var_name, u)

    write (u, "(A)")
    write (u, "(A)")  "* Restore global"

    call rt_data%restore (local, keep_local = .true.)

    write (u, "(A)")
    write (u, "(A)")  "* Global model"
    write (u, "(A)")

    call model_write (rt_data%model, u)
    write (u, *)
    write (u, "(A,L1)")  "s is stable    = ", is_stable (25, rt_data)
    write (u, "(A,L1)")  "f is polarized = ", is_polarized (6, rt_data)

    write (u, "(A)")
    write (u, "(A)")  "* Local model"
    write (u, "(A)")

    call model_write (local%model, u)
    write (u, *)
    write (u, "(A,L1)")  "s is stable    = ", is_stable (25, local)
    write (u, "(A,L1)")  "f is polarized = ", is_polarized (6, local)

    write (u, *)

    write (u, "(A)", advance="no")  "Global model variable: "
    model_vars => model_get_var_list_ptr (rt_data%model)
    call var_list_write_var (model_vars, var_name, u)

    write (u, "(A)", advance="no")  "Local model variable: "
    call var_list_write_var (model_get_var_list_ptr (local%model), &
         var_name, u)

    write (u, "(A)", advance="no")  "Global variable: "
    call var_list_write_var (rt_data%var_list, var_name, u)

    write (u, "(A)", advance="no")  "Local variable: "
    call var_list_write_var (local%var_list, var_name, u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call model_final (local%model)
    deallocate (local%model)
    
    call rt_data%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: rt_data_6"
    
  end subroutine rt_data_6
  
  subroutine rt_data_7 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: rt_data

    write (u, "(A)")  "* Test output: rt_data_7"
    write (u, "(A)")  "*   Purpose: set and access result variables"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize process variables"
    write (u, "(A)")

    call rt_data%global_init ()
    call rt_data%process_stack%init_result_vars (var_str ("testproc"))
    
    call var_list_write_var (rt_data%var_list, &
         var_str ("integral(testproc)"), u)
    call var_list_write_var (rt_data%var_list, &
         var_str ("error(testproc)"), u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call rt_data%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: rt_data_7"
    
  end subroutine rt_data_7
  
  subroutine rt_data_8 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: rt_data

    write (u, "(A)")  "* Test output: rt_data_8"
    write (u, "(A)")  "*   Purpose: get correct collision energy"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize"
    write (u, "(A)")

    call rt_data%global_init ()

    write (u, "(A)")  "* Set sqrts"
    write (u, "(A)")

    call var_list_set_real (rt_data%var_list, var_str ("sqrts"), &
         1000._default, is_known = .true.)
    write (u, "(1x,A,ES19.12)")  "sqrts =", rt_data%get_sqrts ()

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call rt_data%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: rt_data_8"
    
  end subroutine rt_data_8
  

end module rt_data
