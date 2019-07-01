! WHIZARD 2.2.1 June 3 2014
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
module subevt_expr

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use limits, only: FMT_12 !NODEP!
  use diagnostics !NODEP!
  use unit_tests
  use os_interface

  use ifiles
  use lexers
  use parser

  use lorentz !NODEP!
  use subevents
  use variables
  use expressions
  use models
  use flavors
  use interactions
  use particles

  implicit none
  private

  public :: parton_expr_t
  public :: event_expr_t
  public :: subevt_expr_test

  type, extends (subevt_t), abstract :: subevt_expr_t
     logical :: subevt_filled = .false.
     type(var_list_t) :: var_list
     real(default) :: sqrts_hat = 0
     integer :: n_in = 0
     integer :: n_out = 0
     integer :: n_tot = 0
     logical :: has_selection = .false.
     type(eval_tree_t) :: selection
   contains
     procedure :: base_write => subevt_expr_write
     procedure (subevt_expr_final), deferred :: final
     procedure :: base_final => subevt_expr_final
     procedure (subevt_expr_setup_vars), deferred :: setup_vars
     procedure :: base_setup_vars => subevt_expr_setup_vars
     procedure :: link_var_list => subevt_expr_link_var_list
     procedure :: setup_selection => subevt_expr_setup_selection
     procedure :: reset => subevt_expr_reset
     procedure :: base_reset => subevt_expr_reset
     procedure :: base_evaluate => subevt_expr_evaluate
  end type subevt_expr_t
  
  type, extends (subevt_expr_t) :: parton_expr_t
     integer, dimension(:), allocatable :: i_beam
     integer, dimension(:), allocatable :: i_in
     integer, dimension(:), allocatable :: i_out
     logical :: has_scale = .false.
     logical :: has_fac_scale = .false.
     logical :: has_ren_scale = .false.
     logical :: has_weight = .false.
     type(eval_tree_t) :: scale
     type(eval_tree_t) :: fac_scale
     type(eval_tree_t) :: ren_scale
     type(eval_tree_t) :: weight
   contains
     procedure :: final => parton_expr_final
     procedure :: write => parton_expr_write
     procedure :: setup_vars => parton_expr_setup_vars
     procedure :: setup_scales => parton_expr_setup_scales
     procedure :: setup_weight => parton_expr_setup_weight
     procedure :: setup_subevt => parton_expr_setup_subevt
     procedure :: fill_subevt => parton_expr_fill_subevt
     procedure :: evaluate => parton_expr_evaluate
     procedure :: get_beam_index => parton_expr_get_beam_index
     procedure :: get_in_index => parton_expr_get_in_index
  end type parton_expr_t
     
  type, extends (subevt_expr_t) :: event_expr_t
     logical :: has_reweight = .false.
     logical :: has_analysis = .false.
     type(eval_tree_t) :: reweight
     type(eval_tree_t) :: analysis
     logical :: has_id = .false.
     type(string_t) :: id
     logical :: has_num_id = .false.
     integer :: num_id = 0
     logical :: has_index = .false.
     integer :: index = 0
     logical :: has_sqme_ref = .false.
     real(default) :: sqme_ref = 0
     logical :: has_sqme_prc = .false.
     real(default) :: sqme_prc = 0
     logical :: has_weight_ref = .false.
     real(default) :: weight_ref = 0
     logical :: has_weight_prc = .false.
     real(default) :: weight_prc = 0
     logical :: has_excess_prc = .false.
     real(default) :: excess_prc = 0
     integer :: n_alt = 0
     logical :: has_sqme_alt = .false.
     real(default), dimension(:), allocatable :: sqme_alt
     logical :: has_weight_alt = .false.
     real(default), dimension(:), allocatable :: weight_alt
   contains
     procedure :: final => event_expr_final
     procedure :: write => event_expr_write
     procedure :: init => event_expr_init
     procedure :: setup_vars => event_expr_setup_vars
     procedure :: setup_analysis => event_expr_setup_analysis
     procedure :: setup_reweight => event_expr_setup_reweight
     procedure :: set_process_id => event_expr_set_process_id
     procedure :: set_process_num_id => event_expr_set_process_num_id
     procedure :: reset => event_expr_reset
     procedure :: set => event_expr_set
     procedure :: fill_subevt => event_expr_fill_subevt
     procedure :: evaluate => event_expr_evaluate
  end type event_expr_t
     

contains
  
  subroutine subevt_expr_write (object, unit)
    class(subevt_expr_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(1x,A)")  "Local variables:"
    call write_separator (u)
    call var_list_write (object%var_list, u, follow_link=.false.)
    call write_separator (u)
    if (object%subevt_filled) then
       call object%subevt_t%write (u)
       if (object%has_selection) then
          call write_separator (u)
          write (u, "(1x,A)")  "Selection expression:"
          call write_separator (u)
          call eval_tree_write (object%selection, u)
       end if
    else
       write (u, "(1x,A)")  "subevt: [undefined]"
    end if
  end subroutine subevt_expr_write
    
  subroutine subevt_expr_final (object)
    class(subevt_expr_t), intent(inout) :: object
    call var_list_final (object%var_list)
    if (object%has_selection) then
       call eval_tree_final (object%selection)
    end if
  end subroutine subevt_expr_final
  
  subroutine subevt_expr_setup_vars (expr, sqrts)
    class(subevt_expr_t), intent(inout), target :: expr
    real(default), intent(in) :: sqrts
    call var_list_final (expr%var_list)
    call var_list_append_real (expr%var_list, &
         var_str ("sqrts"), sqrts, &
         locked = .true., verbose = .false., intrinsic = .true.)
    call var_list_append_real_ptr (expr%var_list, &
         var_str ("sqrts_hat"), expr%sqrts_hat, &
         is_known = expr%subevt_filled, &
         locked = .true., verbose = .false., intrinsic = .true.)
    call var_list_append_int_ptr (expr%var_list, &
         var_str ("n_in"), expr%n_in, &
         is_known = expr%subevt_filled, &
         locked = .true., verbose = .false., intrinsic = .true.)
    call var_list_append_int_ptr (expr%var_list, &
         var_str ("n_out"), expr%n_out, &
         is_known = expr%subevt_filled, &
         locked = .true., verbose = .false., intrinsic = .true.)
    call var_list_append_int_ptr (expr%var_list, &
         var_str ("n_tot"), expr%n_tot, &
         is_known = expr%subevt_filled, &
         locked = .true., verbose = .false., intrinsic = .true.)
  end subroutine subevt_expr_setup_vars
    
  subroutine subevt_expr_link_var_list (expr, var_list)
    class(subevt_expr_t), intent(inout) :: expr
    type(var_list_t), intent(in), target :: var_list
    call var_list_link (expr%var_list, var_list)
  end subroutine subevt_expr_link_var_list

  subroutine subevt_expr_setup_selection (expr, pn_selection)
    class(subevt_expr_t), intent(inout), target :: expr
    type(parse_node_t), intent(in), pointer :: pn_selection
    if (associated (pn_selection)) then
       call eval_tree_init_lexpr (expr%selection, &
            pn_selection, expr%var_list, expr%subevt_t)
       expr%has_selection = .true.
    end if
  end subroutine subevt_expr_setup_selection

  subroutine subevt_expr_reset (expr)
    class(subevt_expr_t), intent(inout) :: expr
    expr%subevt_filled = .false.
  end subroutine subevt_expr_reset
  
  subroutine subevt_expr_evaluate (expr, passed)
    class(subevt_expr_t), intent(inout) :: expr
    logical, intent(out) :: passed 
    if (expr%has_selection) then
       call eval_tree_evaluate (expr%selection)
       if (eval_tree_result_is_known (expr%selection)) then
          passed = eval_tree_get_log (expr%selection)
       else
          call msg_error ("Evaluate selection expression: result undefined")
          passed = .false.
       end if
    else
       passed = .true.
    end if
  end subroutine subevt_expr_evaluate
  
  subroutine parton_expr_final (object)
    class(parton_expr_t), intent(inout) :: object
    call object%base_final ()
    if (object%has_scale) then
       call eval_tree_final (object%scale)
    end if
    if (object%has_fac_scale) then
       call eval_tree_final (object%fac_scale)
    end if
    if (object%has_ren_scale) then
       call eval_tree_final (object%ren_scale)
    end if
    if (object%has_weight) then
       call eval_tree_final (object%weight)
    end if
  end subroutine parton_expr_final

  subroutine parton_expr_write (object, unit, prefix)
    class(parton_expr_t), intent(in) :: object
    integer, intent(in), optional :: unit
    character(*), intent(in), optional :: prefix
    integer :: u
    u = output_unit (unit)
    call object%base_write (u)
    if (object%subevt_filled) then
       if (object%has_scale) then
          call write_separator (u)
          write (u, "(1x,A)")  "Scale expression:"
          call write_separator (u)
          call eval_tree_write (object%scale, u)
       end if
       if (object%has_fac_scale) then
          call write_separator (u)
          write (u, "(1x,A)")  "Factorization scale expression:"
          call write_separator (u)
          call eval_tree_write (object%fac_scale, u)
       end if
       if (object%has_ren_scale) then
          call write_separator (u)
          write (u, "(1x,A)")  "Renormalization scale expression:"
          call write_separator (u)
          call eval_tree_write (object%ren_scale, u)
       end if
       if (object%has_weight) then
          call write_separator (u)
          write (u, "(1x,A)")  "Weight expression:"
          call write_separator (u)
          call eval_tree_write (object%weight, u)
       end if
    end if
  end subroutine parton_expr_write
    
  subroutine parton_expr_setup_vars (expr, sqrts)
    class(parton_expr_t), intent(inout), target :: expr
    real(default), intent(in) :: sqrts
    call expr%base_setup_vars (sqrts)
  end subroutine parton_expr_setup_vars

  subroutine parton_expr_setup_scales &
       (expr, pn_scale, pn_fac_scale, pn_ren_scale)
    class(parton_expr_t), intent(inout), target :: expr
    type(parse_node_t), intent(in), pointer :: pn_scale
    type(parse_node_t), intent(in), pointer :: pn_fac_scale, pn_ren_scale
    if (associated (pn_scale)) then
       call eval_tree_init_expr (expr%scale, &
            pn_scale, expr%var_list, expr%subevt_t)
       expr%has_scale = .true.
    end if
    if (associated (pn_fac_scale)) then
       call eval_tree_init_expr (expr%fac_scale, &
            pn_fac_scale, expr%var_list, expr%subevt_t)
       expr%has_fac_scale = .true.
    end if
    if (associated (pn_ren_scale)) then
       call eval_tree_init_expr (expr%ren_scale, &
            pn_ren_scale, expr%var_list, expr%subevt_t)
       expr%has_ren_scale = .true.
    end if
  end subroutine parton_expr_setup_scales

  subroutine parton_expr_setup_weight (expr, pn_weight)
    class(parton_expr_t), intent(inout), target :: expr
    type(parse_node_t), intent(in), pointer :: pn_weight
    if (associated (pn_weight)) then
       call eval_tree_init_expr (expr%weight, &
            pn_weight, expr%var_list, expr%subevt_t)
       expr%has_weight = .true.
    end if
  end subroutine parton_expr_setup_weight

  subroutine parton_expr_setup_subevt (expr, int, &
       i_beam, i_in, i_out, f_beam, f_in, f_out)
    class(parton_expr_t), intent(inout) :: expr
    type(interaction_t), intent(in), target :: int
    integer, dimension(:), intent(in) :: i_beam, i_in, i_out
    type(flavor_t), dimension(:), intent(in) :: f_beam, f_in, f_out
    allocate (expr%i_beam (size (i_beam)))
    allocate (expr%i_in (size (i_in)))
    allocate (expr%i_out (size (i_out)))
    expr%i_beam = i_beam
    expr%i_in = i_in
    expr%i_out = i_out
    call interaction_to_subevt (int, &
         expr%i_beam, expr%i_in, expr%i_out, expr%subevt_t)
    call subevt_set_pdg_beam     (expr%subevt_t, flavor_get_pdg (f_beam))
    call subevt_set_pdg_incoming (expr%subevt_t, flavor_get_pdg (f_in))
    call subevt_set_pdg_outgoing (expr%subevt_t, flavor_get_pdg (f_out))
    call subevt_set_p2_beam     (expr%subevt_t, flavor_get_mass (f_beam) ** 2)
    call subevt_set_p2_incoming (expr%subevt_t, flavor_get_mass (f_in)   ** 2)
    call subevt_set_p2_outgoing (expr%subevt_t, flavor_get_mass (f_out)  ** 2)
    expr%n_in  = size (i_in)
    expr%n_out = size (i_out)
    expr%n_tot = expr%n_in + expr%n_out
  end subroutine parton_expr_setup_subevt

  subroutine parton_expr_fill_subevt (expr, int)
    class(parton_expr_t), intent(inout) :: expr
    type(interaction_t), intent(in), target :: int
    call interaction_momenta_to_subevt (int, &
         expr%i_beam, expr%i_in, expr%i_out, expr%subevt_t)
    expr%sqrts_hat = subevt_get_sqrts_hat (expr%subevt_t)
    expr%subevt_filled = .true.
  end subroutine parton_expr_fill_subevt
    
  subroutine parton_expr_evaluate &
       (expr, passed, scale, fac_scale, ren_scale, weight)
    class(parton_expr_t), intent(inout) :: expr
    logical, intent(out) :: passed
    real(default), intent(out) :: scale
    real(default), intent(out) :: fac_scale
    real(default), intent(out) :: ren_scale
    real(default), intent(out) :: weight
    call expr%base_evaluate (passed)
    if (passed) then
       if (expr%has_scale) then
          call eval_tree_evaluate (expr%scale)
          if (eval_tree_result_is_known (expr%scale)) then
             scale = eval_tree_get_real (expr%scale)
          else
             call msg_error ("Evaluate scale expression: result undefined")
             scale = 0
          end if
       else
          scale = expr%sqrts_hat
       end if
       if (expr%has_fac_scale) then
          call eval_tree_evaluate (expr%fac_scale)
          if (eval_tree_result_is_known (expr%fac_scale)) then
             fac_scale = eval_tree_get_real (expr%fac_scale)
          else
             call msg_error ("Evaluate factorization scale expression: &
                  &result undefined")
             fac_scale = 0
          end if
       else
          fac_scale = scale
       end if
       if (expr%has_ren_scale) then
          call eval_tree_evaluate (expr%ren_scale)
          if (eval_tree_result_is_known (expr%ren_scale)) then
             ren_scale = eval_tree_get_real (expr%ren_scale)
          else
             call msg_error ("Evaluate renormalization scale expression: &
                  &result undefined")
             ren_scale = 0
          end if
       else
          ren_scale = scale
       end if
       if (expr%has_weight) then
          call eval_tree_evaluate (expr%weight)
          if (eval_tree_result_is_known (expr%weight)) then
             weight = eval_tree_get_real (expr%weight)
          else
             call msg_error ("Evaluate weight expression: result undefined")
             weight = 0
          end if
       else
          weight = 1
       end if
    end if
  end subroutine parton_expr_evaluate
  
  subroutine parton_expr_get_beam_index (expr, i_beam)
    class(parton_expr_t), intent(in) :: expr
    integer, dimension(:), intent(out) :: i_beam
    i_beam = expr%i_beam
  end subroutine parton_expr_get_beam_index
  
  subroutine parton_expr_get_in_index (expr, i_in)
    class(parton_expr_t), intent(in) :: expr
    integer, dimension(:), intent(out) :: i_in
    i_in = expr%i_in
  end subroutine parton_expr_get_in_index
  
  subroutine event_expr_final (object)
    class(event_expr_t), intent(inout) :: object
    call object%base_final ()
    if (object%has_reweight) then
       call eval_tree_final (object%reweight)
    end if
    if (object%has_analysis) then
       call eval_tree_final (object%analysis)
    end if
  end subroutine event_expr_final

  subroutine event_expr_write (object, unit, prefix)
    class(event_expr_t), intent(in) :: object
    integer, intent(in), optional :: unit
    character(*), intent(in), optional :: prefix
    integer :: u
    u = output_unit (unit)
    call object%base_write (u)
    if (object%subevt_filled) then
       if (object%has_reweight) then
          call write_separator (u)
          write (u, "(1x,A)")  "Reweighting expression:"
          call write_separator (u)
          call eval_tree_write (object%reweight, u)
       end if
       if (object%has_analysis) then
          call write_separator (u)
          write (u, "(1x,A)")  "Analysis expression:"
          call write_separator (u)
          call eval_tree_write (object%analysis, u)
       end if
    end if
  end subroutine event_expr_write
    
  subroutine event_expr_init (expr, n_alt)
    class(event_expr_t), intent(out) :: expr
    integer, intent(in), optional :: n_alt
    if (present (n_alt)) then
       expr%n_alt = n_alt
       allocate (expr%sqme_alt (n_alt), source = 0._default)
       allocate (expr%weight_alt (n_alt), source = 0._default)
    end if
  end subroutine event_expr_init
  
  subroutine event_expr_setup_vars (expr, sqrts)
    class(event_expr_t), intent(inout), target :: expr
    real(default), intent(in) :: sqrts
    call expr%base_setup_vars (sqrts)
    call var_list_append_string_ptr (expr%var_list, &
         var_str ("$process_id"), expr%id, &
         is_known = expr%has_id, &
         locked = .true., verbose = .false., intrinsic = .true.)
    call var_list_append_int_ptr (expr%var_list, &
         var_str ("process_num_id"), expr%num_id, &
         is_known = expr%has_num_id, &
         locked = .true., verbose = .false., intrinsic = .true.)
    call var_list_append_real_ptr (expr%var_list, &
         var_str ("sqme"), expr%sqme_prc, &
         is_known = expr%has_sqme_prc, &
         locked = .true., verbose = .false., intrinsic = .true.)
    call var_list_append_real_ptr (expr%var_list, &
         var_str ("sqme_ref"), expr%sqme_ref, &
         is_known = expr%has_sqme_ref, &
         locked = .true., verbose = .false., intrinsic = .true.)
    call var_list_append_int_ptr (expr%var_list, &
         var_str ("event_index"), expr%index, &
         is_known = expr%has_index, &
         locked = .true., verbose = .false., intrinsic = .true.)
    call var_list_append_real_ptr (expr%var_list, &
         var_str ("event_weight"), expr%weight_prc, &
         is_known = expr%has_weight_prc, &
         locked = .true., verbose = .false., intrinsic = .true.)
    call var_list_append_real_ptr (expr%var_list, &
         var_str ("event_weight_ref"), expr%weight_ref, &
         is_known = expr%has_weight_ref, &
         locked = .true., verbose = .false., intrinsic = .true.)
    call var_list_append_real_ptr (expr%var_list, &
         var_str ("event_excess"), expr%excess_prc, &
         is_known = expr%has_excess_prc, &
         locked = .true., verbose = .false., intrinsic = .true.)
  end subroutine event_expr_setup_vars

  subroutine event_expr_setup_analysis (expr, pn_analysis)
    class(event_expr_t), intent(inout), target :: expr
    type(parse_node_t), intent(in), pointer :: pn_analysis
    if (associated (pn_analysis)) then
       call eval_tree_init_lexpr (expr%analysis, &
            pn_analysis, expr%var_list, expr%subevt_t)
       expr%has_analysis = .true.
    end if
  end subroutine event_expr_setup_analysis

  subroutine event_expr_setup_reweight (expr, pn_reweight)
    class(event_expr_t), intent(inout), target :: expr
    type(parse_node_t), intent(in), pointer :: pn_reweight
    if (associated (pn_reweight)) then
       call eval_tree_init_expr (expr%reweight, &
            pn_reweight, expr%var_list, expr%subevt_t)
       expr%has_reweight = .true.
    end if
  end subroutine event_expr_setup_reweight

  subroutine event_expr_set_process_id (expr, id)
    class(event_expr_t), intent(inout) :: expr
    type(string_t), intent(in) :: id
    expr%id = id
    expr%has_id = .true.
  end subroutine event_expr_set_process_id
    
  subroutine event_expr_set_process_num_id (expr, num_id)
    class(event_expr_t), intent(inout) :: expr
    integer, intent(in) :: num_id
    expr%num_id = num_id
    expr%has_num_id = .true.
  end subroutine event_expr_set_process_num_id
    
  subroutine event_expr_reset (expr)
    class(event_expr_t), intent(inout) :: expr
    call expr%base_reset ()
    expr%has_sqme_ref = .false.
    expr%has_sqme_prc = .false.
    expr%has_sqme_alt = .false.
    expr%has_weight_ref = .false.
    expr%has_weight_prc = .false.
    expr%has_weight_alt = .false.
    expr%has_excess_prc = .false.
  end subroutine event_expr_reset
  
  subroutine event_expr_set (expr, &
       weight_ref, weight_prc, weight_alt, &
       excess_prc, &
       sqme_ref, sqme_prc, sqme_alt)
    class(event_expr_t), intent(inout) :: expr
    real(default), intent(in), optional :: weight_ref, weight_prc
    real(default), intent(in), optional :: excess_prc
    real(default), intent(in), optional :: sqme_ref, sqme_prc
    real(default), dimension(:), intent(in), optional :: sqme_alt, weight_alt
    if (present (sqme_ref)) then
       expr%has_sqme_ref = .true.
       expr%sqme_ref = sqme_ref
    end if
    if (present (sqme_prc)) then
       expr%has_sqme_prc = .true.
       expr%sqme_prc = sqme_prc
    end if 
    if (present (sqme_alt)) then
       expr%has_sqme_alt = .true.
       expr%sqme_alt = sqme_alt
    end if
    if (present (weight_ref)) then
       expr%has_weight_ref = .true.
       expr%weight_ref = weight_ref
    end if
    if (present (weight_prc)) then
       expr%has_weight_prc = .true.
       expr%weight_prc = weight_prc
    end if
    if (present (weight_alt)) then
       expr%has_weight_alt = .true.
       expr%weight_alt = weight_alt
    end if
    if (present (excess_prc)) then
       expr%has_excess_prc = .true.
       expr%excess_prc = excess_prc
    end if
  end subroutine event_expr_set
  
  subroutine event_expr_fill_subevt (expr, particle_set)
    class(event_expr_t), intent(inout) :: expr
    type(particle_set_t), intent(in) :: particle_set
    call particle_set_to_subevt (particle_set, expr%subevt_t)
    expr%sqrts_hat = subevt_get_sqrts_hat (expr%subevt_t)
    expr%n_in  = particle_set_get_n_in  (particle_set)
    expr%n_out = particle_set_get_n_out (particle_set)
    expr%n_tot = expr%n_in + expr%n_out
    expr%subevt_filled = .true.
    if (expr%has_index) then
       expr%index = expr%index + 1
    else
       expr%index = 1
       expr%has_index = .true.
    end if
  end subroutine event_expr_fill_subevt
  
  subroutine event_expr_evaluate (expr, passed, reweight, analysis_flag)
    class(event_expr_t), intent(inout) :: expr
    logical, intent(out) :: passed
    real(default), intent(out) :: reweight
    logical, intent(out) :: analysis_flag
    call expr%base_evaluate (passed)
    if (passed) then
       if (expr%has_reweight) then
          call eval_tree_evaluate (expr%reweight)
          if (eval_tree_result_is_known (expr%reweight)) then
             reweight = eval_tree_get_real (expr%reweight)
          else
             call msg_error ("Evaluate reweight expression: &
                  &result undefined")
             reweight = 0
          end if
       else
          reweight = 1
       end if
       if (expr%has_analysis) then
          call eval_tree_evaluate (expr%analysis)
          if (eval_tree_result_is_known (expr%analysis)) then
             analysis_flag = eval_tree_get_log (expr%analysis)
          else
             call msg_error ("Evaluate analysis expression: &
                  &result undefined")
             analysis_flag = .false.
          end if
       else
          analysis_flag = .true.
       end if
    end if
  end subroutine event_expr_evaluate
  

  subroutine subevt_expr_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (subevt_expr_1, "subevt_expr_1", &
         "parton-event expressions", &
         u, results)
    call test (subevt_expr_2, "subevt_expr_2", &
         "parton-event expressions", &
         u, results)
end subroutine subevt_expr_test

  subroutine subevt_expr_1 (u)
    integer, intent(in) :: u
    type(string_t) :: expr_text
    type(ifile_t) :: ifile
    type(stream_t) :: stream
    type(parse_tree_t) :: pt_cuts, pt_scale, pt_fac_scale, pt_ren_scale
    type(parse_tree_t) :: pt_weight
    type(parse_node_t), pointer :: pn_cuts, pn_scale, pn_fac_scale, pn_ren_scale
    type(parse_node_t), pointer :: pn_weight
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model => null ()
    type(parton_expr_t), target :: expr
    real(default) :: E, Ex, m
    type(vector4_t), dimension(6) :: p
    integer :: i, pdg
    logical :: passed
    real(default) :: scale, fac_scale, ren_scale, weight
    
    write (u, "(A)")  "* Test output: subevt_expr_1"
    write (u, "(A)")  "*   Purpose: Set up a subevt and associated &
         &process-specific expressions"
    write (u, "(A)")

    call syntax_pexpr_init ()
    call syntax_model_file_init ()
    call os_data_init (os_data)
    call model_list%read_model (var_str ("Test"), &
         var_str ("Test.mdl"), os_data, model)

    write (u, "(A)")  "* Expression texts"
    write (u, "(A)")


    expr_text = "all Pt > 100 [s]"
    write (u, "(A,A)")  "cuts = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_lexpr (pt_cuts, stream, .true.)
    call stream_final (stream)
    pn_cuts => parse_tree_get_root_ptr (pt_cuts)

    expr_text = "sqrts"
    write (u, "(A,A)")  "scale = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_expr (pt_scale, stream, .true.)
    call stream_final (stream)
    pn_scale => parse_tree_get_root_ptr (pt_scale)

    expr_text = "sqrts_hat"
    write (u, "(A,A)")  "fac_scale = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_expr (pt_fac_scale, stream, .true.)
    call stream_final (stream)
    pn_fac_scale => parse_tree_get_root_ptr (pt_fac_scale)

    expr_text = "100"
    write (u, "(A,A)")  "ren_scale = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_expr (pt_ren_scale, stream, .true.)
    call stream_final (stream)
    pn_ren_scale => parse_tree_get_root_ptr (pt_ren_scale)

    expr_text = "n_tot - n_in - n_out"
    write (u, "(A,A)")  "weight = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_expr (pt_weight, stream, .true.)
    call stream_final (stream)
    pn_weight => parse_tree_get_root_ptr (pt_weight)

    call ifile_final (ifile)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize process expr"
    write (u, "(A)")

    call expr%setup_vars (1000._default)
    call var_list_append_real (expr%var_list, var_str ("tolerance"), 0._default)
    call expr%link_var_list (model_get_var_list_ptr (model))

    call expr%setup_selection (pn_cuts)
    call expr%setup_scales (pn_scale, pn_fac_scale, pn_ren_scale)
    call expr%setup_weight (pn_weight)

    call write_separator (u)
    call expr%write (u)
    call write_separator (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Fill subevt and evaluate expressions"
    write (u, "(A)")

    call subevt_init (expr%subevt_t, 6)
    E = 500._default
    Ex = 400._default
    m = 125._default
    pdg = 25
    p(1) = vector4_moving (E, sqrt (E**2 - m**2), 3)
    p(2) = vector4_moving (E, -sqrt (E**2 - m**2), 3)
    p(3) = vector4_moving (Ex, sqrt (Ex**2 - m**2), 3)
    p(4) = vector4_moving (Ex, -sqrt (Ex**2 - m**2), 3)
    p(5) = vector4_moving (Ex, sqrt (Ex**2 - m**2), 1)
    p(6) = vector4_moving (Ex, -sqrt (Ex**2 - m**2), 1)

    call expr%reset ()
    do i = 1, 2
       call subevt_set_beam (expr%subevt_t, i, pdg, p(i), m**2)
    end do
    do i = 3, 4
       call subevt_set_incoming (expr%subevt_t, i, pdg, p(i), m**2)
    end do
    do i = 5, 6
       call subevt_set_outgoing (expr%subevt_t, i, pdg, p(i), m**2)
    end do
    expr%sqrts_hat = subevt_get_sqrts_hat (expr%subevt_t)
    expr%n_in = 2
    expr%n_out = 2
    expr%n_tot = 4
    expr%subevt_filled = .true.

    call expr%evaluate (passed, scale, fac_scale, ren_scale, weight)
    
    write (u, "(A,L1)")      "Event has passed      = ", passed
    write (u, "(A," // FMT_12 // ")")  "Scale                 = ", scale
    write (u, "(A," // FMT_12 // ")")  "Factorization scale   = ", fac_scale
    write (u, "(A," // FMT_12 // ")")  "Renormalization scale = ", ren_scale
    write (u, "(A," // FMT_12 // ")")  "Weight                = ", weight
    write (u, "(A)")
    
    call write_separator (u)
    call expr%write (u)
    call write_separator (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call expr%final ()

    call model_list%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: subevt_expr_1"
    
  end subroutine subevt_expr_1
  
  subroutine subevt_expr_2 (u)
    integer, intent(in) :: u
    type(string_t) :: expr_text
    type(ifile_t) :: ifile
    type(stream_t) :: stream
    type(parse_tree_t) :: pt_selection
    type(parse_tree_t) :: pt_reweight, pt_analysis
    type(parse_node_t), pointer :: pn_selection
    type(parse_node_t), pointer :: pn_reweight, pn_analysis
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model => null ()
    type(event_expr_t), target :: expr
    real(default) :: E, Ex, m
    type(vector4_t), dimension(6) :: p
    integer :: i, pdg
    logical :: passed
    real(default) :: reweight
    logical :: analysis_flag
    
    write (u, "(A)")  "* Test output: subevt_expr_2"
    write (u, "(A)")  "*   Purpose: Set up a subevt and associated &
         &process-specific expressions"
    write (u, "(A)")

    call syntax_pexpr_init ()
    call syntax_model_file_init ()
    call os_data_init (os_data)
    call model_list%read_model (var_str ("Test"), &
         var_str ("Test.mdl"), os_data, model)

    write (u, "(A)")  "* Expression texts"
    write (u, "(A)")


    expr_text = "all Pt > 100 [s]"
    write (u, "(A,A)")  "selection = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_lexpr (pt_selection, stream, .true.)
    call stream_final (stream)
    pn_selection => parse_tree_get_root_ptr (pt_selection)

    expr_text = "n_tot - n_in - n_out"
    write (u, "(A,A)")  "reweight = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_expr (pt_reweight, stream, .true.)
    call stream_final (stream)
    pn_reweight => parse_tree_get_root_ptr (pt_reweight)

    expr_text = "true"
    write (u, "(A,A)")  "analysis = ", char (expr_text)
    call ifile_clear (ifile)
    call ifile_append (ifile, expr_text)
    call stream_init (stream, ifile)
    call parse_tree_init_lexpr (pt_analysis, stream, .true.)
    call stream_final (stream)
    pn_analysis => parse_tree_get_root_ptr (pt_analysis)

    call ifile_final (ifile)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize process expr"
    write (u, "(A)")

    call expr%setup_vars (1000._default)
    call expr%link_var_list (model_get_var_list_ptr (model))
    call var_list_append_real (expr%var_list, var_str ("tolerance"), 0._default)

    call expr%setup_selection (pn_selection)
    call expr%setup_analysis (pn_analysis)
    call expr%setup_reweight (pn_reweight)

    call write_separator (u)
    call expr%write (u)
    call write_separator (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Fill subevt and evaluate expressions"
    write (u, "(A)")

    call subevt_init (expr%subevt_t, 6)
    E = 500._default
    Ex = 400._default
    m = 125._default
    pdg = 25
    p(1) = vector4_moving (E, sqrt (E**2 - m**2), 3)
    p(2) = vector4_moving (E, -sqrt (E**2 - m**2), 3)
    p(3) = vector4_moving (Ex, sqrt (Ex**2 - m**2), 3)
    p(4) = vector4_moving (Ex, -sqrt (Ex**2 - m**2), 3)
    p(5) = vector4_moving (Ex, sqrt (Ex**2 - m**2), 1)
    p(6) = vector4_moving (Ex, -sqrt (Ex**2 - m**2), 1)

    call expr%reset ()
    do i = 1, 2
       call subevt_set_beam (expr%subevt_t, i, pdg, p(i), m**2)
    end do
    do i = 3, 4
       call subevt_set_incoming (expr%subevt_t, i, pdg, p(i), m**2)
    end do
    do i = 5, 6
       call subevt_set_outgoing (expr%subevt_t, i, pdg, p(i), m**2)
    end do
    expr%sqrts_hat = subevt_get_sqrts_hat (expr%subevt_t)
    expr%n_in = 2
    expr%n_out = 2
    expr%n_tot = 4
    expr%subevt_filled = .true.

    call expr%evaluate (passed, reweight, analysis_flag)
    
    write (u, "(A,L1)")      "Event has passed      = ", passed
    write (u, "(A," // FMT_12 // ")")  "Reweighting factor    = ", reweight
    write (u, "(A,L1)")      "Analysis flag         = ", analysis_flag
    write (u, "(A)")
    
    call write_separator (u)
    call expr%write (u)
    call write_separator (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call expr%final ()

    call model_list%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: subevt_expr_2"
    
  end subroutine subevt_expr_2
  

end module subevt_expr
