! WHIZARD 2.2.0 May 18 2014
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
module parton_states

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use parser
  use lorentz !NODEP!
  use subevents
  use variables
  use expressions
  use models
  use flavors
  use helicities
  use colors
  use quantum_numbers
  use state_matrices
  use polarizations
  use interactions
  use evaluators

  use beams
  use sf_base
  use process_constants
  use prc_core
  use subevt_expr

  implicit none
  private

  public :: isolated_state_t

  type, abstract :: parton_state_t
     logical :: has_trace = .false.
     logical :: has_matrix = .false.
     logical :: has_flows = .false.
     type(evaluator_t) :: trace
     type(evaluator_t) :: matrix
     type(evaluator_t) :: flows
   contains
     procedure :: write => parton_state_write
     procedure :: final => parton_state_final
     procedure :: receive_kinematics => parton_state_receive_kinematics
     procedure :: send_kinematics => parton_state_send_kinematics
     procedure :: evaluate_trace => parton_state_evaluate_trace
     procedure :: evaluate_event_data => parton_state_evaluate_event_data
     procedure :: normalize_matrix_by_trace => &
          parton_state_normalize_matrix_by_trace
     procedure :: get_trace_int_ptr => parton_state_get_trace_int_ptr
     procedure :: get_matrix_int_ptr => parton_state_get_matrix_int_ptr
     procedure :: get_flows_int_ptr => parton_state_get_flows_int_ptr
  end type parton_state_t

  type, extends (parton_state_t) :: isolated_state_t
     logical :: sf_chain_is_allocated = .false.
     type(sf_chain_instance_t), pointer :: sf_chain_eff => null ()
     logical :: int_is_allocated = .false.
     type(interaction_t), pointer :: int_eff => null ()
   contains
     procedure :: init => isolated_state_init_pointers
     procedure :: setup_square_trace => isolated_state_setup_square_trace
     procedure :: setup_square_matrix => isolated_state_setup_square_matrix
     procedure :: setup_square_flows => isolated_state_setup_square_flows
     procedure :: evaluate_sf_chain => isolated_state_evaluate_sf_chain
  end type isolated_state_t

  public :: connected_state_t
  type, extends (parton_state_t) :: connected_state_t
     logical :: has_flows_sf = .false.
     type(evaluator_t) :: flows_sf
     logical :: has_expr = .false.
     type(parton_expr_t) :: expr
   contains
     procedure :: setup_connected_trace => connected_state_setup_connected_trace
     procedure :: setup_connected_matrix => connected_state_setup_connected_matrix
     procedure :: setup_connected_flows => connected_state_setup_connected_flows
     procedure :: setup_subevt => connected_state_setup_subevt
     procedure :: setup_var_list => connected_state_setup_var_list
     procedure :: setup_expressions => connected_state_setup_expressions
     procedure :: reset_expressions => connected_state_reset_expressions
     procedure :: evaluate_expressions => connected_state_evaluate_expressions
     procedure :: get_beam_index => connected_state_get_beam_index
     procedure :: get_in_index => connected_state_get_in_index
  end type connected_state_t
     

contains
  
  subroutine parton_state_write (state, unit, testflag)
    class(parton_state_t), intent(in) :: state
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = output_unit (unit)
    select type (state)
    class is (isolated_state_t)
       if (state%sf_chain_is_allocated) then
          call write_separator (u)
          call state%sf_chain_eff%write (u)
       end if
       if (state%int_is_allocated) then
          call write_separator (u)
          write (u, "(1x,A)") &
               "Effective interaction:"
          call write_separator (u)
          call interaction_write (state%int_eff, u, testflag = testflag)
       end if
    class is (connected_state_t)
       if (state%has_flows_sf) then
          call write_separator (u)
          write (u, "(1x,A)") &
               "Evaluator (extension of the beam evaluator &
               &with color contractions):"
          call write_separator (u)
          call state%flows_sf%write (u, testflag = testflag)
       end if
    end select
    if (state%has_trace) then
       call write_separator (u)
       write (u, "(1x,A)") &
            "Evaluator (trace of the squared transition matrix):"
       call write_separator (u)
       call state%trace%write (u, testflag = testflag)
    end if
    if (state%has_matrix) then
       call write_separator (u)
       write (u, "(1x,A)") &
            "Evaluator (squared transition matrix):"
       call write_separator (u)
       call state%matrix%write (u, testflag = testflag)
    end if
    if (state%has_flows) then
       call write_separator (u)
       write (u, "(1x,A)") &
            "Evaluator (squared color-flow matrix):"
       call write_separator (u)
       call state%flows%write (u, testflag = testflag)
    end if
    select type (state)
    class is (connected_state_t)
       if (state%has_expr) then
          call write_separator (u)
          call state%expr%write (u)
       end if
    end select
  end subroutine parton_state_write
    
  subroutine parton_state_final (state)
    class(parton_state_t), intent(inout) :: state
    if (state%has_flows) then
       call evaluator_final (state%flows)
       state%has_flows = .false.
    end if
    if (state%has_matrix) then
       call evaluator_final (state%matrix)
       state%has_matrix = .false.
    end if
    if (state%has_trace) then
       call evaluator_final (state%trace)
       state%has_trace = .false.
    end if
    select type (state)
    class is (connected_state_t)
       if (state%has_flows_sf) then
          call evaluator_final (state%flows_sf)
          state%has_flows_sf = .false.
       end if
       call state%expr%final ()
    class is (isolated_state_t)
       if (state%int_is_allocated) then
          call interaction_final (state%int_eff)
          deallocate (state%int_eff)
          state%int_is_allocated = .false.
       end if
       if (state%sf_chain_is_allocated) then
          call state%sf_chain_eff%final ()
       end if
    end select
  end subroutine parton_state_final
    
  subroutine isolated_state_init_pointers (state, sf_chain, int)
    class(isolated_state_t), intent(out) :: state
    type(sf_chain_instance_t), intent(in), target :: sf_chain
    type(interaction_t), intent(in), target :: int
    state%sf_chain_eff => sf_chain
    state%int_eff => int
  end subroutine isolated_state_init_pointers
    
  subroutine isolated_state_setup_square_trace (state, core, qn_mask_in, &
       col)
    class(isolated_state_t), intent(inout), target :: state
    class(prc_core_t), intent(in) :: core
    type(quantum_numbers_mask_t), dimension(:), intent(in) :: qn_mask_in
    integer, dimension(:), intent(in) :: col
    type(quantum_numbers_mask_t), dimension(:), allocatable :: qn_mask    
    associate (data => core%data)
      allocate (qn_mask (data%n_in + data%n_out))
      qn_mask(:data%n_in) = &
              new_quantum_numbers_mask (.false., .true., .false.) &
              .or. qn_mask_in
      qn_mask(data%n_in+1:) = &
           new_quantum_numbers_mask (.true., .true., .true.)
    if (core%use_color_factors) then
       call evaluator_init_square (state%trace, &
            state%int_eff, qn_mask, &
            data%cf_index, data%color_factors, col, nc=core%nc)
    else
       call evaluator_init_square (state%trace, &
            state%int_eff, qn_mask, nc=core%nc)
    end if
    end associate
    state%has_trace = .true.
  end subroutine isolated_state_setup_square_trace
    
  subroutine isolated_state_setup_square_matrix &
       (state, core, model, qn_mask_in, col)
    class(isolated_state_t), intent(inout), target :: state
    class(prc_core_t), intent(in) :: core
    type(model_t), intent(in), target :: model
    type(quantum_numbers_mask_t), dimension(:), intent(in) :: qn_mask_in
    integer, dimension(:), intent(in) :: col
    type(quantum_numbers_mask_t), dimension(:), allocatable :: qn_mask
    type(flavor_t), dimension(:), allocatable :: flv
    integer :: i
    logical :: helmask, helmask_hd
    associate (data => core%data)
      allocate (qn_mask (data%n_in + data%n_out))
      allocate (flv (data%n_flv))
      do i = 1, data%n_in + data%n_out      
         call flavor_init (flv, data%flv_state(i,:), model)
         if ((data%n_in == 1 .or. i > data%n_in) &
              .and. any (.not. flavor_is_stable (flv))) then
            helmask = all (flavor_decays_isotropically (flv))
            helmask_hd = all (flavor_decays_diagonal (flv))
            qn_mask(i) = new_quantum_numbers_mask (.false., .true., helmask, &
                 mask_hd = helmask_hd)
         else if (i > data%n_in) then
            helmask = all (.not. flavor_is_polarized (flv))
            qn_mask(i) = new_quantum_numbers_mask (.false., .true., helmask)
         else
            qn_mask(i) = new_quantum_numbers_mask (.false., .true., .false.) &
              .or. qn_mask_in(i)
         end if
      end do
    if (core%use_color_factors) then
       call evaluator_init_square (state%matrix, &
            state%int_eff, qn_mask, &
            data%cf_index, data%color_factors, col, nc=core%nc)
    else
       call evaluator_init_square (state%matrix, state%int_eff, &
            qn_mask, nc=core%nc)
    end if
    end associate
    state%has_matrix = .true.
  end subroutine isolated_state_setup_square_matrix

  subroutine isolated_state_setup_square_flows (state, core, model, qn_mask_in)
    class(isolated_state_t), intent(inout), target :: state
    class(prc_core_t), intent(in) :: core
    type(model_t), intent(in), target :: model
    type(quantum_numbers_mask_t), dimension(:), intent(in) :: qn_mask_in
    type(quantum_numbers_mask_t), dimension(:), allocatable :: qn_mask
    type(flavor_t), dimension(:), allocatable :: flv
    integer :: i
    logical :: helmask, helmask_hd
    associate (data => core%data)
      allocate (qn_mask (data%n_in + data%n_out))
      allocate (flv (data%n_flv))
      do i = 1, data%n_in + data%n_out
         call flavor_init (flv, data%flv_state(i,:), model)
         if ((data%n_in == 1 .or. i > data%n_in) &
              .and. any (.not. flavor_is_stable (flv))) then
            helmask = all (flavor_decays_isotropically (flv))
            helmask_hd = all (flavor_decays_diagonal (flv))
            qn_mask(i) = new_quantum_numbers_mask (.false., .false., helmask, &
                 mask_hd = helmask_hd)
         else if (i > data%n_in) then
            helmask = all (.not. flavor_is_polarized (flv))
            qn_mask(i) = new_quantum_numbers_mask (.false., .false., helmask)
         else
            qn_mask(i) = new_quantum_numbers_mask (.false., .false., .false.) &
              .or. qn_mask_in(i)
         end if
      end do
      call evaluator_init_square (state%flows, state%int_eff, qn_mask, &
           expand_color_flows = .true.)
    end associate
    state%has_flows = .true.
  end subroutine isolated_state_setup_square_flows

  subroutine connected_state_setup_connected_trace (state, isolated, int)
    class(connected_state_t), intent(inout), target :: state
    type(isolated_state_t), intent(in), target :: isolated
    type(interaction_t), intent(in), optional, target :: int
    type(quantum_numbers_mask_t) :: mask
    type(interaction_t), pointer :: src_int
    mask = new_quantum_numbers_mask (.true., .true., .true.)
    if (present (int)) then
       src_int => int
    else
       src_int => isolated%sf_chain_eff%get_out_int_ptr ()
    end if
    call evaluator_init_product &
         (state%trace, src_int, isolated%trace, mask, mask)
    state%has_trace = .true.
  end subroutine connected_state_setup_connected_trace
    
  subroutine connected_state_setup_connected_matrix (state, isolated, int)
    class(connected_state_t), intent(inout), target :: state
    type(isolated_state_t), intent(in), target :: isolated
    type(interaction_t), intent(in), optional, target :: int
    type(quantum_numbers_mask_t) :: mask
    type(interaction_t), pointer :: src_int
    mask = new_quantum_numbers_mask (.false., .true., .true.)
    if (present (int)) then
       src_int => int
    else
       src_int => isolated%sf_chain_eff%get_out_int_ptr ()
    end if
    call evaluator_init_product &
         (state%matrix, src_int, isolated%matrix, mask)
    state%has_matrix = .true.
  end subroutine connected_state_setup_connected_matrix
  
  subroutine connected_state_setup_connected_flows (state, isolated, int)
    class(connected_state_t), intent(inout), target :: state
    type(isolated_state_t), intent(in), target :: isolated
    type(interaction_t), intent(in), optional, target :: int
    type(quantum_numbers_mask_t) :: mask
    type(interaction_t), pointer :: src_int
    mask = new_quantum_numbers_mask (.false., .false., .true.)
    if (present (int)) then
       src_int => int
    else
       src_int => isolated%sf_chain_eff%get_out_int_ptr ()
       call evaluator_init_color_contractions (state%flows_sf, src_int)
       state%has_flows_sf = .true.
       src_int => evaluator_get_int_ptr (state%flows_sf)
    end if
    call evaluator_init_product &
         (state%flows, src_int, isolated%flows, mask)
    state%has_flows = .true.
  end subroutine connected_state_setup_connected_flows
  
  subroutine connected_state_setup_subevt (state, sf_chain, f_beam, f_in, f_out)
    class(connected_state_t), intent(inout), target :: state
    type(sf_chain_instance_t), intent(in), target :: sf_chain
    type(flavor_t), dimension(:), intent(in) :: f_beam, f_in, f_out
    integer :: n_beam, n_in, n_out, n_vir, n_tot, i, j
    integer, dimension(:), allocatable :: i_beam, i_in, i_out
    integer :: sf_out_i
    type(interaction_t), pointer :: int, sf_int
    int => evaluator_get_int_ptr (state%trace)
    sf_int => sf_chain%get_out_int_ptr ()
    n_beam = size (f_beam)
    n_in = size (f_in)
    n_out = size (f_out)
    n_vir = interaction_get_n_vir (int)
    n_tot = interaction_get_n_tot (int)
    allocate (i_beam (n_beam), i_in (n_in), i_out (n_out))
    i_beam = [(i, i = 1, n_beam)]
    do j = 1, n_in
       sf_out_i = sf_chain%get_out_i (j)
       i_in(j) = interaction_find_link (int, sf_int, sf_out_i)
    end do
    i_out = [(i, i = n_vir + 1, n_tot)]
    call state%expr%setup_subevt (int, &
         i_beam, i_in, i_out, f_beam, f_in, f_out)
    state%has_expr = .true.
  end subroutine connected_state_setup_subevt

  subroutine connected_state_setup_var_list (state, process_var_list, beam_data)
    class(connected_state_t), intent(inout), target :: state
    type(var_list_t), intent(in), target :: process_var_list
    type(beam_data_t), intent(in) :: beam_data
    call state%expr%setup_vars (beam_data_get_sqrts (beam_data))
    call state%expr%link_var_list (process_var_list)
  end subroutine connected_state_setup_var_list
  
  subroutine connected_state_setup_expressions (state, &
       pn_cuts, pn_scale, pn_fac_scale, pn_ren_scale, pn_weight)
    class(connected_state_t), intent(inout), target :: state
    type(parse_node_t), intent(in), pointer :: pn_cuts
    type(parse_node_t), intent(in), pointer :: pn_scale
    type(parse_node_t), intent(in), pointer :: pn_fac_scale
    type(parse_node_t), intent(in), pointer :: pn_ren_scale
    type(parse_node_t), intent(in), pointer :: pn_weight
    call state%expr%setup_selection (pn_cuts)
    call state%expr%setup_scales (pn_scale, pn_fac_scale, pn_ren_scale)
    call state%expr%setup_weight (pn_weight)
  end subroutine connected_state_setup_expressions
    
  subroutine connected_state_reset_expressions (state)
    class(connected_state_t), intent(inout) :: state
    if (state%has_expr)  call state%expr%reset ()
  end subroutine connected_state_reset_expressions
  
  subroutine parton_state_receive_kinematics (state)
    class(parton_state_t), intent(inout), target :: state
    type(interaction_t), pointer :: int
    if (state%has_trace) then
       call evaluator_receive_momenta (state%trace)
       select type (state)
       class is (connected_state_t)
          if (state%has_expr) then
             int => evaluator_get_int_ptr (state%trace)
             call state%expr%fill_subevt (int)
          end if
       end select
    end if
  end subroutine parton_state_receive_kinematics

  subroutine parton_state_send_kinematics (state)
    class(parton_state_t), intent(inout), target :: state
    type(interaction_t), pointer :: int
    if (state%has_trace) then
       call evaluator_send_momenta (state%trace)
       select type (state)
       class is (connected_state_t)
          int => evaluator_get_int_ptr (state%trace)
          call state%expr%fill_subevt (int)
       end select
    end if
  end subroutine parton_state_send_kinematics

  subroutine connected_state_evaluate_expressions (state, passed, &
       scale, fac_scale, ren_scale, weight)
    class(connected_state_t), intent(inout) :: state
    logical, intent(out) :: passed
    real(default), intent(out) :: scale, fac_scale, ren_scale, weight
    if (state%has_expr) then
       call state%expr%evaluate (passed, scale, fac_scale, ren_scale, weight)
    end if
  end subroutine connected_state_evaluate_expressions
    
  subroutine isolated_state_evaluate_sf_chain (state, fac_scale)
    class(isolated_state_t), intent(inout) :: state
    real(default), intent(in) :: fac_scale
    if (state%sf_chain_is_allocated) then
       call state%sf_chain_eff%evaluate (fac_scale)
    end if
  end subroutine isolated_state_evaluate_sf_chain
  
  subroutine parton_state_evaluate_trace (state)
    class(parton_state_t), intent(inout) :: state
    if (state%has_trace) then
       call state%trace%evaluate ()
    end if
  end subroutine parton_state_evaluate_trace

  subroutine parton_state_evaluate_event_data (state)
    class(parton_state_t), intent(inout) :: state
    select type (state)
    type is (connected_state_t)
       if (state%has_flows_sf) then
          call evaluator_receive_momenta (state%flows_sf)
          call state%flows_sf%evaluate ()
       end if
    end select
    if (state%has_matrix) then
       call evaluator_receive_momenta (state%matrix)
       call state%matrix%evaluate ()
    end if
    if (state%has_flows) then
       call evaluator_receive_momenta (state%flows)
       call state%flows%evaluate ()
    end if
  end subroutine parton_state_evaluate_event_data

  subroutine parton_state_normalize_matrix_by_trace (state)
    class(parton_state_t), intent(inout) :: state
    if (state%has_matrix) then
       call evaluator_normalize_by_trace (state%matrix)
    end if
  end subroutine parton_state_normalize_matrix_by_trace
  
  function parton_state_get_trace_int_ptr (state) result (ptr)
    class(parton_state_t), intent(in), target :: state
    type(interaction_t), pointer :: ptr
    if (state%has_trace) then
       ptr => evaluator_get_int_ptr (state%trace)
    else
       ptr => null ()
    end if
  end function parton_state_get_trace_int_ptr
  
  function parton_state_get_matrix_int_ptr (state) result (ptr)
    class(parton_state_t), intent(in), target :: state
    type(interaction_t), pointer :: ptr
    if (state%has_matrix) then
       ptr => evaluator_get_int_ptr (state%matrix)
    else
       ptr => null ()
    end if
  end function parton_state_get_matrix_int_ptr
  
  function parton_state_get_flows_int_ptr (state) result (ptr)
    class(parton_state_t), intent(in), target :: state
    type(interaction_t), pointer :: ptr
    if (state%has_flows) then
       ptr => evaluator_get_int_ptr (state%flows)
    else
       ptr => null ()
    end if
  end function parton_state_get_flows_int_ptr
  
  subroutine connected_state_get_beam_index (state, i_beam)
    class(connected_state_t), intent(in) :: state
    integer, dimension(:), intent(out) :: i_beam
    call state%expr%get_beam_index (i_beam)
  end subroutine connected_state_get_beam_index
  
  subroutine connected_state_get_in_index (state, i_in)
    class(connected_state_t), intent(in) :: state
    integer, dimension(:), intent(out) :: i_in
    call state%expr%get_in_index (i_in)
  end subroutine connected_state_get_in_index
  

end module parton_states
