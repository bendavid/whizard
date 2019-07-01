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

module decays

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_utils, only: write_indent, write_separator
  use unit_tests
  use diagnostics
  use os_interface
  use sm_qcd
  use flavors
  use quantum_numbers
  use state_matrices
  use interactions
  use evaluators
  use variables
  use model_data
  use rng_base
  use selectors
  use prc_core
  use prc_test
  use process_libraries
  use mci_base
  use mci_midpoint
  use phs_base
  use phs_single
  use parton_states
  use processes
  use process_stacks
  use event_transforms
  
  implicit none
  private

  public :: evt_decay_t
  public :: pacify
  public :: decays_test
  public :: prepare_testbed

  type, abstract :: any_config_t
   contains
     procedure (any_config_final), deferred :: final
     procedure (any_config_write), deferred :: write
  end type any_config_t

  type :: particle_config_t
     class(any_config_t), allocatable :: c
  end type particle_config_t
  
  type, abstract :: any_t
   contains
     procedure (any_final), deferred :: final
     procedure (any_write), deferred :: write
  end type any_t
  
  type :: particle_out_t
     class(any_t), allocatable :: c
  end type particle_out_t
  
  type :: decay_term_config_t
     type(particle_config_t), dimension(:), allocatable :: prt
   contains
     procedure :: final => decay_term_config_final
     procedure :: write => decay_term_config_write
     procedure :: init => decay_term_config_init
     procedure :: compute => decay_term_config_compute
  end type decay_term_config_t
  
  type :: decay_term_t
     type(decay_term_config_t), pointer :: config => null ()
     type(particle_out_t), dimension(:), allocatable :: particle_out
   contains
     procedure :: final => decay_term_final
     procedure :: write => decay_term_write
     procedure :: write_process_instances => decay_term_write_process_instances
     procedure :: init => decay_term_init
     procedure :: make_rng => decay_term_make_rng
     procedure :: link_interactions => decay_term_link_interactions
     procedure :: select_chain => decay_term_select_chain
     procedure :: generate => decay_term_generate
  end type decay_term_t

  type :: decay_root_config_t
     type(string_t) :: process_id
     type(process_t), pointer :: process => null ()
     class(model_data_t), pointer :: model => null ()
     type(decay_term_config_t), dimension(:), allocatable :: term_config
   contains
     procedure :: final => decay_root_config_final
     procedure :: write => decay_root_config_write
     procedure :: write_header => decay_root_config_write_header
     procedure :: write_terms => decay_root_config_write_terms
     procedure :: init => decay_root_config_init
     procedure :: init_term => decay_root_config_init_term
     procedure :: connect => decay_root_config_connect
     procedure :: compute => decay_root_config_compute
  end type decay_root_config_t
  
  type, abstract :: decay_gen_t
     type(decay_term_t), dimension(:), allocatable :: term
     type(process_instance_t), pointer :: process_instance => null ()
     integer :: selected_mci = 0
     integer :: selected_term = 0
   contains
     procedure :: base_final => decay_gen_final
     procedure :: write_process_instances => decay_gen_write_process_instances
     procedure :: base_init => decay_gen_init
     procedure :: make_term_rng => decay_gen_make_term_rng
     procedure :: link_term_interactions => decay_gen_link_term_interactions
  end type decay_gen_t
  
  type, extends (decay_gen_t) :: decay_root_t
     type(decay_root_config_t), pointer :: config => null ()
   contains
     procedure :: final => decay_root_final
     procedure :: write => decay_root_write
     procedure :: init => decay_root_init
     procedure :: select_chain => decay_root_select_chain
     procedure :: generate => decay_root_generate
  end type decay_root_t
  
  type, extends (decay_root_config_t) :: decay_config_t
     real(default) :: weight = 0
     real(default) :: integral = 0
     real(default) :: abs_error = 0
     real(default) :: rel_error = 0
     type(selector_t) :: mci_selector
   contains
     procedure :: write => decay_config_write
     procedure :: connect => decay_config_connect
     procedure :: compute => decay_config_compute
  end type decay_config_t
  
  type, extends (decay_gen_t) :: decay_t
     type(decay_config_t), pointer :: config => null ()
     class(rng_t), allocatable :: rng
   contains
     procedure :: final => decay_final
     procedure :: write => decay_write
     procedure :: init => decay_init
     procedure :: link_interactions => decay_link_interactions
     procedure :: select_chain => decay_select_chain
     procedure :: generate => decay_generate
  end type decay_t
     
  type, extends (any_config_t) :: stable_config_t
     type(flavor_t), dimension(:), allocatable :: flv
   contains
     procedure :: final => stable_config_final
     procedure :: write => stable_config_write
     procedure :: init => stable_config_init
  end type stable_config_t

  type, extends (any_t) :: stable_t
     type(stable_config_t), pointer :: config => null ()
   contains
     procedure :: final => stable_final
     procedure :: write => stable_write
     procedure :: init => stable_init
  end type stable_t

  type, extends (any_config_t) :: unstable_config_t
     type(flavor_t) :: flv
     real(default) :: integral = 0
     real(default) :: abs_error = 0
     real(default) :: rel_error = 0
     type(selector_t) :: selector
     type(decay_config_t), dimension(:), allocatable :: decay_config
   contains
     procedure :: final => unstable_config_final
     procedure :: write => unstable_config_write
     procedure :: init => unstable_config_init
     procedure :: init_decays => unstable_config_init_decays
     procedure :: compute => unstable_config_compute
  end type unstable_config_t
  
  type, extends (any_t) :: unstable_t
     type(unstable_config_t), pointer :: config => null ()
     class(rng_t), allocatable :: rng
     integer :: selected_decay = 0
     type(decay_t), dimension(:), allocatable :: decay
   contains
     procedure :: final => unstable_final
     procedure :: write => unstable_write
     procedure :: write_process_instances => unstable_write_process_instances
     procedure :: init => unstable_init
     procedure :: link_interactions => unstable_link_interactions
     procedure :: import_rng => unstable_import_rng
     procedure :: select_chain => unstable_select_chain
     procedure :: generate => unstable_generate
  end type unstable_t
  
  type, extends (connected_state_t) :: decay_chain_entry_t
     integer :: index = 0
     type(decay_config_t), pointer :: config => null ()
     integer :: selected_mci = 0
     integer :: selected_term = 0
     type(decay_chain_entry_t), pointer :: previous => null ()
  end type decay_chain_entry_t
     
  type :: decay_chain_t
     type(process_instance_t), pointer :: process_instance => null ()
     integer :: selected_term = 0
     type(evaluator_t) :: correlated_trace
     type(decay_chain_entry_t), pointer :: last => null ()
   contains
     procedure :: final => decay_chain_final
     procedure :: write => decay_chain_write
     procedure :: build => decay_chain_build
     procedure :: build_term_entries => decay_chain_build_term_entries
     procedure :: build_decay_entries => decay_chain_build_decay_entries
     procedure :: evaluate => decay_chain_evaluate
     procedure :: get_probability => decay_chain_get_probability
  end type decay_chain_t
  
  type, extends (evt_t) :: evt_decay_t
     type(decay_root_config_t) :: decay_root_config
     type(decay_root_t) :: decay_root
     type(decay_chain_t) :: decay_chain
   contains
     procedure :: write => evt_decay_write
     procedure :: connect => evt_decay_connect
     procedure :: prepare_new_event => evt_decay_prepare_new_event
     procedure :: generate_weighted => evt_decay_generate_weighted
     procedure :: make_particle_set => evt_decay_make_particle_set
  end type evt_decay_t
  

  interface
     subroutine any_config_final (object)
       import
       class(any_config_t), intent(inout) :: object
     end subroutine any_config_final
  end interface
  
  interface
     subroutine any_config_write (object, unit, indent, verbose)
       import
       class(any_config_t), intent(in) :: object
       integer, intent(in), optional :: unit, indent
       logical, intent(in), optional :: verbose
     end subroutine any_config_write
  end interface

  interface
     subroutine any_final (object)
       import
       class(any_t), intent(inout) :: object
     end subroutine any_final
  end interface
  
  interface
     subroutine any_write (object, unit, indent)
       import
       class(any_t), intent(in) :: object
       integer, intent(in), optional :: unit, indent
     end subroutine any_write
  end interface

  interface pacify
     module procedure pacify_decay
  end interface pacify

contains

  recursive subroutine decay_term_config_final (object)
    class(decay_term_config_t), intent(inout) :: object
    integer :: i
    if (allocated (object%prt)) then
       do i = 1, size (object%prt)
          if (allocated (object%prt(i)%c))  call object%prt(i)%c%final ()
       end do
    end if
  end subroutine decay_term_config_final
  
  recursive subroutine decay_term_config_write (object, unit, indent, verbose)
    class(decay_term_config_t), intent(in) :: object
    integer, intent(in), optional :: unit, indent
    logical, intent(in), optional :: verbose
    integer :: i, j, u, ind
    logical :: verb
    u = given_output_unit (unit)
    ind = 0;  if (present (indent))  ind = indent
    verb = .true.;  if (present (verbose))  verb = verbose
    call write_indent (u, ind)
    write (u, "(1x,A)", advance="no")  "Final state:"
    do i = 1, size (object%prt)
       select type (prt_config => object%prt(i)%c)
       type is (stable_config_t)
          write (u, "(1x,A)", advance="no") &
               char (flavor_get_name (prt_config%flv(1)))
          do j = 2, size (prt_config%flv)
             write (u, "(':',A)", advance="no") &
                  char (flavor_get_name (prt_config%flv(j)))
          end do
       type is (unstable_config_t)
          write (u, "(1x,A)", advance="no") &
               char (flavor_get_name (prt_config%flv))
       end select
    end do
    write (u, *)
    if (verb) then
       do i = 1, size (object%prt)
          call object%prt(i)%c%write (u, ind)
       end do
    end if
  end subroutine decay_term_config_write

  recursive subroutine decay_term_config_init &
       (term, flv, stable, model, process_stack)
    class(decay_term_config_t), intent(out) :: term
    type(flavor_t), dimension(:,:), intent(in) :: flv
    logical, dimension(:), intent(in) :: stable
    class(model_data_t), intent(in), target :: model
    type(process_stack_t), intent(in), optional :: process_stack
    type(string_t), dimension(:), allocatable :: decay
    integer :: i
    allocate (term%prt (size (flv, 1)))
    do i = 1, size (flv, 1)
       associate (prt => term%prt(i))
         if (stable(i)) then
            allocate (stable_config_t :: prt%c)
         else
            allocate (unstable_config_t :: prt%c)
         end if
         select type (prt_config => prt%c)
         type is (stable_config_t)
            call prt_config%init (flv(i,:))
         type is (unstable_config_t)
            if (all (flv(i,:) == flv(i,1))) then
               call prt_config%init (flv(i,1))
               call flavor_get_decays (flv(i,1), decay)
               call prt_config%init_decays (decay, model, process_stack)
            else
               call prt_config%write ()
               call msg_fatal ("Decay configuration: &
                    &unstable product must be unique")
            end if
         end select
       end associate
    end do
  end subroutine decay_term_config_init
  
  recursive subroutine decay_term_config_compute (term)
    class(decay_term_config_t), intent(inout) :: term
    integer :: i
    do i = 1, size (term%prt)
       select type (unstable_config => term%prt(i)%c)
       type is (unstable_config_t)
          call unstable_config%compute ()
       end select
    end do
  end subroutine decay_term_config_compute
  
  recursive subroutine decay_term_final (object)
    class(decay_term_t), intent(inout) :: object
    integer :: i
    if (allocated (object%particle_out)) then
       do i = 1, size (object%particle_out)
          call object%particle_out(i)%c%final ()
       end do
    end if
  end subroutine decay_term_final
  
  recursive subroutine decay_term_write (object, unit, indent)
    class(decay_term_t), intent(in) :: object
    integer, intent(in), optional :: unit, indent
    integer :: i, u, ind
    u = given_output_unit (unit)
    ind = 0;  if (present (indent))  ind = indent
    call object%config%write (u, ind, verbose = .false.)
    do i = 1, size (object%particle_out)
       call object%particle_out(i)%c%write (u, ind)
    end do
  end subroutine decay_term_write

  recursive subroutine decay_term_write_process_instances (term, unit, verbose)
    class(decay_term_t), intent(in) :: term
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    integer :: i
    do i = 1, size (term%particle_out)
       select type (unstable => term%particle_out(i)%c)
       type is (unstable_t)
          call unstable%write_process_instances (unit, verbose)
       end select
    end do
  end subroutine decay_term_write_process_instances
  
  recursive subroutine decay_term_init (term, config)
    class(decay_term_t), intent(out) :: term
    type(decay_term_config_t), intent(in), target :: config
    integer :: i
    term%config => config
    allocate (term%particle_out (size (config%prt)))
    do i = 1, size (config%prt)
       select type (prt_config => config%prt(i)%c)
       type is (stable_config_t)
          allocate (stable_t :: term%particle_out(i)%c)
          select type (stable => term%particle_out(i)%c)
          type is (stable_t)
             call stable%init (prt_config)
          end select
       type is (unstable_config_t)
          allocate (unstable_t :: term%particle_out(i)%c)
          select type (unstable => term%particle_out(i)%c)
          type is (unstable_t)
             call unstable%init (prt_config)
          end select
       end select
    end do
  end subroutine decay_term_init

  subroutine decay_term_make_rng (term, process)
    class(decay_term_t), intent(inout) :: term
    type(process_t), intent(inout) :: process
    class(rng_t), allocatable :: rng
    integer :: i
    do i = 1, size (term%particle_out)
       select type (unstable => term%particle_out(i)%c)
       type is (unstable_t)
          call process%make_rng (rng)
          call unstable%import_rng (rng)
       end select
    end do
  end subroutine decay_term_make_rng
    
  recursive subroutine decay_term_link_interactions (term, trace)
    class(decay_term_t), intent(inout) :: term
    type(interaction_t), intent(in), target :: trace
    integer :: i
    do i = 1, size (term%particle_out)
       select type (unstable => term%particle_out(i)%c)
       type is (unstable_t)
          call unstable%link_interactions (i, trace)
       end select
    end do
  end subroutine decay_term_link_interactions
  
  recursive subroutine decay_term_select_chain (term)
    class(decay_term_t), intent(inout) :: term
    integer :: i
    do i = 1, size (term%particle_out)
       select type (unstable => term%particle_out(i)%c)
       type is (unstable_t)
          call unstable%select_chain ()
       end select
    end do
  end subroutine decay_term_select_chain

  recursive subroutine decay_term_generate (term)
    class(decay_term_t), intent(inout) :: term
    integer :: i
    do i = 1, size (term%particle_out)
       select type (unstable => term%particle_out(i)%c)
       type is (unstable_t)
          call unstable%generate ()
       end select
    end do
  end subroutine decay_term_generate

  recursive subroutine decay_root_config_final (object)
    class(decay_root_config_t), intent(inout) :: object
    integer :: i
    if (allocated (object%term_config)) then
       do i = 1, size (object%term_config)
          call object%term_config(i)%final ()
       end do
    end if
  end subroutine decay_root_config_final
  
  recursive subroutine decay_root_config_write (object, unit, indent, verbose)
    class(decay_root_config_t), intent(in) :: object
    integer, intent(in), optional :: unit, indent
    logical, intent(in), optional :: verbose
    integer :: u, ind
    u = given_output_unit (unit)
    ind = 0;  if (present (indent))  ind = indent
    call write_indent (u, ind)
    write (u, "(1x,A)")  "Final-state decay tree:"
    call object%write_header (unit, indent)
    call object%write_terms (unit, indent, verbose)
  end subroutine decay_root_config_write

  subroutine decay_root_config_write_header (object, unit, indent)
    class(decay_root_config_t), intent(in) :: object
    integer, intent(in), optional :: unit, indent
    integer :: u, ind
    u = given_output_unit (unit)
    ind = 0;  if (present (indent))  ind = indent
    call write_indent (u, ind)
    if (associated (object%process)) then
       write (u, 3)  "process ID      =", char (object%process_id), "*"
    else
       write (u, 3)  "process ID      =", char (object%process_id)
    end if
3   format (3x,A,2(1x,A))
  end subroutine decay_root_config_write_header
    
  recursive subroutine decay_root_config_write_terms &
       (object, unit, indent, verbose)
    class(decay_root_config_t), intent(in) :: object
    integer, intent(in), optional :: unit, indent
    logical, intent(in), optional :: verbose
    integer :: i, u, ind
    logical :: verb
    u = given_output_unit (unit)
    ind = 0;  if (present (indent))  ind = indent
    verb = .true.;  if (present (verbose))  verb = verbose
    if (verb .and. allocated (object%term_config)) then
       do i = 1, size (object%term_config)
          call object%term_config(i)%write (u, ind + 1)
       end do
    end if
  end subroutine decay_root_config_write_terms
    
  subroutine decay_root_config_init (decay, model, process_id, n_terms)
    class(decay_root_config_t), intent(out) :: decay
    class(model_data_t), intent(in), target :: model
    type(string_t), intent(in) :: process_id
    integer, intent(in), optional :: n_terms
    decay%model => model
    decay%process_id = process_id
    if (present (n_terms)) then
       allocate (decay%term_config (n_terms))
    end if
  end subroutine decay_root_config_init
       
  recursive subroutine decay_root_config_init_term &
       (decay, i, flv, stable, model, process_stack)
    class(decay_root_config_t), intent(inout) :: decay
    integer, intent(in) :: i
    type(flavor_t), dimension(:,:), intent(in) :: flv
    logical, dimension(:), intent(in) :: stable
    class(model_data_t), intent(in), target :: model
    type(process_stack_t), intent(in), optional :: process_stack
    call decay%term_config(i)%init (flv, stable, model, process_stack)
  end subroutine decay_root_config_init_term
  
  recursive subroutine decay_root_config_connect &
       (decay, process, model, process_stack, process_instance)
    class(decay_root_config_t), intent(out) :: decay
    type(process_t), intent(in), target :: process
    class(model_data_t), intent(in), target :: model
    type(process_stack_t), intent(in), optional :: process_stack
    type(process_instance_t), intent(in), optional, target :: process_instance
    type(connected_state_t), pointer :: connected_state
    type(interaction_t), pointer :: int
    type(flavor_t), dimension(:,:), allocatable :: flv
    logical, dimension(:), allocatable :: stable
    integer :: i
    call decay%init (model, process%get_id (), process%get_n_terms ())
    do i = 1, size (decay%term_config)
       if (present (process_instance)) then
          connected_state => process_instance%get_connected_state_ptr (i)
          int => connected_state%get_matrix_int_ptr ()
          call interaction_get_flv_out (int, flv)
       else
          call process%get_term_flv_out (i, flv)
       end if
       call flavor_set_model (flv, model)
       allocate (stable (size (flv, 1)))
       stable = flavor_is_stable (flv(:,1))
       call decay%init_term (i, flv, stable, model, process_stack)
       deallocate (flv, stable)
    end do
    decay%process => process
  end subroutine decay_root_config_connect

  recursive subroutine decay_root_config_compute (decay)
    class(decay_root_config_t), intent(inout) :: decay
    integer :: i
    do i = 1, size (decay%term_config)
       call decay%term_config(i)%compute ()
    end do
  end subroutine decay_root_config_compute
  
  recursive subroutine decay_gen_final (object)
    class(decay_gen_t), intent(inout) :: object
    integer :: i
    if (allocated (object%term)) then
       do i = 1, size (object%term)
          call object%term(i)%final ()
       end do
    end if
  end subroutine decay_gen_final    
  
  subroutine decay_root_final (object)
    class(decay_root_t), intent(inout) :: object
    call object%base_final ()
  end subroutine decay_root_final    
  
  subroutine decay_root_write (object, unit)
    class(decay_root_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    if (associated (object%config)) then
       call object%config%write (unit, verbose = .false.)
    else
       write (u, "(1x,A)")  "Final-state decay tree: [not configured]"
    end if
    if (object%selected_mci > 0) then
       write (u, "(3x,A,I0)")  "Selected MCI    = ", object%selected_mci
    else
       write (u, "(3x,A)")  "Selected MCI    = [undefined]"
    end if
    if (object%selected_term > 0) then
       write (u, "(3x,A,I0)")  "Selected term   = ", object%selected_term
       call object%term(object%selected_term)%write (u, 1)
    else
       write (u, "(3x,A)")  "Selected term   = [undefined]"
    end if
  end subroutine decay_root_write

  recursive subroutine decay_gen_write_process_instances (decay, unit, verbose)
    class(decay_gen_t), intent(in) :: decay
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    logical :: verb
    verb = .true.;  if (present (verbose))  verb = verbose
    if (associated (decay%process_instance)) then
       if (verb) then
          call decay%process_instance%write (unit)
       else
          call decay%process_instance%write_header (unit)
       end if
    end if
    if (decay%selected_term > 0) then
       call decay%term(decay%selected_term)%write_process_instances (unit, verb)
    end if
  end subroutine decay_gen_write_process_instances
    
  recursive subroutine decay_gen_init (decay, term_config)
    class(decay_gen_t), intent(out) :: decay
    type(decay_term_config_t), dimension(:), intent(in), target :: term_config
    integer :: i
    allocate (decay%term (size (term_config)))
    do i = 1, size (decay%term)
       call decay%term(i)%init (term_config(i))
    end do
  end subroutine decay_gen_init

  subroutine decay_root_init (decay_root, config, process_instance)
    class(decay_root_t), intent(out) :: decay_root
    type(decay_root_config_t), intent(in), target :: config
    type(process_instance_t), intent(in), target :: process_instance
    call decay_root%base_init (config%term_config)
    decay_root%config => config
    decay_root%process_instance => process_instance
    call decay_root%make_term_rng (config%process)
    call decay_root%link_term_interactions ()
  end subroutine decay_root_init

  subroutine decay_gen_make_term_rng (decay, process)
    class(decay_gen_t), intent(inout) :: decay
    type(process_t), intent(in), pointer :: process
    integer :: i
    do i = 1, size (decay%term)
       call decay%term(i)%make_rng (process)
    end do
  end subroutine decay_gen_make_term_rng
    
  recursive subroutine decay_gen_link_term_interactions (decay)
    class(decay_gen_t), intent(inout) :: decay
    integer :: i, i_term
    type(interaction_t), pointer :: trace
    associate (instance => decay%process_instance)
      do i = 1, size (decay%term)
         i_term = i
         trace => instance%get_trace_int_ptr (i_term)
         call decay%term(i_term)%link_interactions (trace)
      end do
    end associate
  end subroutine decay_gen_link_term_interactions

  subroutine decay_root_select_chain (decay_root)
    class(decay_root_t), intent(inout) :: decay_root
    if (decay_root%selected_term > 0) then
       call decay_root%term(decay_root%selected_term)%select_chain ()
    else
       call msg_bug ("Decays: no term selected for parent process")
    end if
  end subroutine decay_root_select_chain

  subroutine decay_root_generate (decay_root)
    class(decay_root_t), intent(inout) :: decay_root
    type(connected_state_t), pointer :: connected_state
    if (decay_root%selected_term > 0) then
       connected_state => decay_root%process_instance%get_connected_state_ptr &
            (decay_root%selected_term)
       call connected_state%normalize_matrix_by_trace ()
       call decay_root%term(decay_root%selected_term)%generate ()
    else
       call msg_bug ("Decays: no term selected for parent process")
    end if
  end subroutine decay_root_generate

  recursive subroutine decay_config_write (object, unit, indent, verbose)
    class(decay_config_t), intent(in) :: object
    integer, intent(in), optional :: unit, indent
    logical, intent(in), optional :: verbose
    integer :: u, ind
    u = given_output_unit (unit)
    ind = 0;  if (present (indent))  ind = indent
    call write_indent (u, ind)
    write (u, "(1x,A)")  "Decay:"
    call object%write_header (unit, indent)
    call write_indent (u, ind)
    write (u, 2)  "branching ratio =", object%weight * 100
    call write_indent (u, ind)
    write (u, 1)  "partial width   =", object%integral
    call write_indent (u, ind)
    write (u, 1)  "error (abs)     =", object%abs_error
    call write_indent (u, ind)
    write (u, 1)  "error (rel)     =", object%rel_error
1   format (3x,A,ES19.12)
2   format (3x,A,F11.6,1x,'%')
    call object%write_terms (unit, indent, verbose)
  end subroutine decay_config_write
    
  recursive subroutine decay_config_connect &
       (decay, process, model, process_stack, process_instance)
    class(decay_config_t), intent(out) :: decay
    type(process_t), intent(in), target :: process
    class(model_data_t), intent(in), target :: model
    type(process_stack_t), intent(in), optional :: process_stack
    type(process_instance_t), intent(in), optional, target :: process_instance
    real(default), dimension(:), allocatable :: integral_mci
    integer :: i, n_mci
    call decay%decay_root_config_t%connect (process, model, process_stack)
    decay%integral = process%get_integral ()
    decay%abs_error = process%get_error ()
    if (process%cm_frame ()) then
       call msg_fatal ("Decay process " // char (process%get_id ()) &
            // ": unusable because rest frame is fixed.")
    end if
    n_mci = process%get_n_mci ()
    allocate (integral_mci (n_mci))
    do i = 1, n_mci
       integral_mci(i) = process%get_integral_mci (i)
    end do
    call decay%mci_selector%init (integral_mci)
  end subroutine decay_config_connect

  recursive subroutine decay_config_compute (decay)
    class(decay_config_t), intent(inout) :: decay
    call decay%decay_root_config_t%compute ()
    if (decay%integral == 0) then
       call decay%write ()
       call msg_fatal ("Decay configuration: partial width is zero")
    end if
    decay%rel_error = decay%abs_error / decay%integral
  end subroutine decay_config_compute
  
  recursive subroutine decay_final (object)
    class(decay_t), intent(inout) :: object
    integer :: i
    call object%base_final ()
    do i = 1, object%config%process%get_n_mci ()
       call object%process_instance%final_simulation (i)
    end do
    call object%process_instance%final ()
    deallocate (object%process_instance)
  end subroutine decay_final
  
  recursive subroutine decay_write (object, unit, indent, recursive)
    class(decay_t), intent(in) :: object
    integer, intent(in), optional :: unit, indent, recursive
    integer :: u, ind
    u = given_output_unit (unit)
    ind = 0;  if (present (indent))  ind = indent
    call object%config%write (unit, indent, verbose = .false.)
    if (allocated (object%rng)) then
       call object%rng%write (u, ind + 1)
    end if
    call write_indent (u, ind)
    if (object%selected_mci > 0) then
       write (u, "(3x,A,I0)")  "Selected MCI    = ", object%selected_mci
    else
       write (u, "(3x,A)")  "Selected MCI    = [undefined]"
    end if
    call write_indent (u, ind)
    if (object%selected_term > 0) then
       write (u, "(3x,A,I0)")  "Selected term   = ", object%selected_term
       call object%term(object%selected_term)%write (u, ind + 1)
    else
       write (u, "(3x,A)")  "Selected term   = [undefined]"
    end if
  end subroutine decay_write

  recursive subroutine decay_init (decay, config)
    class(decay_t), intent(out) :: decay
    type(decay_config_t), intent(in), target :: config
    integer :: i
    call decay%base_init (config%term_config)
    decay%config => config
    allocate (decay%process_instance)
    call decay%process_instance%init (decay%config%process)
    call decay%process_instance%setup_event_data (decay%config%model)
    do i = 1, decay%config%process%get_n_mci ()
       call decay%process_instance%init_simulation (i)
    end do
    call decay%config%process%make_rng (decay%rng)
    call decay%make_term_rng (decay%config%process)
  end subroutine decay_init

  recursive subroutine decay_link_interactions (decay, i_prt, trace)
    class(decay_t), intent(inout) :: decay
    integer, intent(in) :: i_prt
    type(interaction_t), intent(in), target :: trace
    type(interaction_t), pointer :: beam_int
    integer :: n_in, n_vir
    beam_int => decay%process_instance%get_beam_int_ptr ()
    n_in = interaction_get_n_in (trace)
    n_vir = interaction_get_n_vir (trace)
    call interaction_set_source_link (beam_int, 1, trace, &
         n_in + n_vir + i_prt)
    call decay%link_term_interactions ()
  end subroutine decay_link_interactions
    
  recursive subroutine decay_select_chain (decay)
    class(decay_t), intent(inout) :: decay
    real(default) :: x
    integer :: i
    call decay%rng%generate (x)
    decay%selected_mci = decay%config%mci_selector%select (x)
    call decay%process_instance%choose_mci (decay%selected_mci)
    call decay%process_instance%select_i_term (decay%selected_term)
    do i = 1, size (decay%term)
       call decay%term(i)%select_chain ()
    end do
  end subroutine decay_select_chain
  
  recursive subroutine decay_generate (decay)
    class(decay_t), intent(inout) :: decay
    type(isolated_state_t), pointer :: isolated_state
    integer :: i
    call decay%process_instance%receive_beam_momenta ()
    call decay%config%process%generate_unweighted_event &
         (decay%process_instance, decay%selected_mci)
    if (signal_is_pending ())  return
    call decay%process_instance%evaluate_event_data ()
    isolated_state => &
         decay%process_instance%get_isolated_state_ptr (decay%selected_term)
    call isolated_state%normalize_matrix_by_trace ()
    do i = 1, size (decay%term)
       call decay%term(i)%generate ()
       if (signal_is_pending ())  return
    end do
  end subroutine decay_generate
    
  subroutine stable_config_final (object)
    class(stable_config_t), intent(inout) :: object
  end subroutine stable_config_final
  
  recursive subroutine stable_config_write (object, unit, indent, verbose)
    class(stable_config_t), intent(in) :: object
    integer, intent(in), optional :: unit, indent
    logical, intent(in), optional :: verbose
    integer :: u, i, ind
    u = given_output_unit (unit)
    ind = 0;  if (present (indent))  ind = indent
    call write_indent (u, ind)
    write (u, "(1x,'+',1x,A)", advance = "no")  "Stable:"
    write (u, "(1x,A)", advance = "no")  char (flavor_get_name (object%flv(1)))
    do i = 2, size (object%flv)
       write (u, "(':',A)", advance = "no") &
            char (flavor_get_name (object%flv(i)))
    end do
    write (u, *)
  end subroutine stable_config_write
  
  subroutine stable_config_init (config, flv)
    class(stable_config_t), intent(out) :: config
    type(flavor_t), dimension(:), intent(in) :: flv
    integer, dimension (size (flv)) :: pdg
    logical, dimension (size (flv)) :: mask
    integer :: i
    pdg = flavor_get_pdg (flv)
    mask(1) = .true.
    forall (i = 2 : size (pdg))
       mask(i) = all (pdg(i) /= pdg(1:i-1))
    end forall
    allocate (config%flv (count (mask)))
    config%flv = pack (flv, mask)
  end subroutine stable_config_init
  
  subroutine stable_final (object)
    class(stable_t), intent(inout) :: object
  end subroutine stable_final

  subroutine stable_write (object, unit, indent)
    class(stable_t), intent(in) :: object
    integer, intent(in), optional :: unit, indent
    call object%config%write (unit, indent)
  end subroutine stable_write
  
  subroutine stable_init (stable, config)
    class(stable_t), intent(out) :: stable
    type(stable_config_t), intent(in), target :: config
    stable%config => config
  end subroutine stable_init
  
  recursive subroutine unstable_config_final (object)
    class(unstable_config_t), intent(inout) :: object
    integer :: i
    if (allocated (object%decay_config)) then
       do i = 1, size (object%decay_config)
          call object%decay_config(i)%final ()
       end do
    end if
  end subroutine unstable_config_final

  recursive subroutine unstable_config_write (object, unit, indent, verbose)
    class(unstable_config_t), intent(in) :: object
    integer, intent(in), optional :: unit, indent
    logical, intent(in), optional :: verbose
    integer :: u, i, ind
    logical :: verb
    u = given_output_unit (unit)
    ind = 0;  if (present (indent))  ind = indent
    verb = .true.;  if (present (verbose))  verb = verbose
    call write_indent (u, ind)
    write (u, "(1x,'+',1x,A,1x,A)")  "Unstable:", &
         char (flavor_get_name (object%flv))
    call write_indent (u, ind)
    write (u, 1)  "total width =", object%integral
    call write_indent (u, ind)
    write (u, 1)  "error (abs) =", object%abs_error
    call write_indent (u, ind)
    write (u, 1)  "error (rel) =", object%rel_error
1   format (5x,A,ES19.12)
    if (verb .and. allocated (object%decay_config)) then
       do i = 1, size (object%decay_config)
          call object%decay_config(i)%write (u, ind + 1)
       end do
    end if
  end subroutine unstable_config_write
  
  subroutine unstable_config_init (config, flv)
    class(unstable_config_t), intent(out) :: config
    type(flavor_t), intent(in) :: flv
    config%flv = flv
  end subroutine unstable_config_init
  
  recursive subroutine unstable_config_init_decays &
       (unstable, decay_id, model, process_stack)
    class(unstable_config_t), intent(inout) :: unstable
    type(string_t), dimension(:), intent(in) :: decay_id
    class(model_data_t), intent(in), target :: model
    type(process_stack_t), intent(in), optional :: process_stack
    integer :: i
    allocate (unstable%decay_config (size (decay_id)))
    do i = 1, size (decay_id)
       associate (decay => unstable%decay_config(i))
         if (present (process_stack)) then
            call decay%connect (process_stack%get_process_ptr (decay_id(i)), &
                 model, process_stack)
         else
            call decay%init (model, decay_id(i))
         end if
       end associate
    end do
  end subroutine unstable_config_init_decays
  
  recursive subroutine unstable_config_compute (unstable)
    class(unstable_config_t), intent(inout) :: unstable
    integer :: i
    do i = 1, size (unstable%decay_config)
       call unstable%decay_config(i)%compute ()
    end do
    unstable%integral = sum (unstable%decay_config%integral)
    if (unstable%integral <= 0) then
       call unstable%write ()
       call msg_fatal ("Decay configuration: computed total width is zero")
    end if
    unstable%abs_error = sqrt (sum (unstable%decay_config%abs_error ** 2))
    unstable%rel_error = unstable%abs_error / unstable%integral
    call unstable%selector%init (unstable%decay_config%integral)
    do i = 1, size (unstable%decay_config)
       unstable%decay_config(i)%weight &
            = unstable%selector%get_weight (i)
    end do
  end subroutine unstable_config_compute
    
  recursive subroutine unstable_final (object)
    class(unstable_t), intent(inout) :: object
    integer :: i
    if (allocated (object%decay)) then
       do i = 1, size (object%decay)
          call object%decay(i)%final ()
       end do
    end if
  end subroutine unstable_final
  
  recursive subroutine unstable_write (object, unit, indent)
    class(unstable_t), intent(in) :: object
    integer, intent(in), optional :: unit, indent
    integer :: u, ind
    u = given_output_unit (unit)
    ind = 0;  if (present (indent))  ind = indent
    call object%config%write (u, ind, verbose=.false.)
    if (allocated (object%rng)) then
       call object%rng%write (u, ind + 2)
    end if
    call write_indent (u, ind)
    if (object%selected_decay > 0) then
       write (u, "(5x,A,I0)") "Sel. decay  = ", object%selected_decay
       call object%decay(object%selected_decay)%write (u, ind + 1)
    else
       write (u, "(5x,A)")  "Sel. decay  = [undefined]"
    end if
  end subroutine unstable_write
    
  recursive subroutine unstable_write_process_instances &
       (unstable, unit, verbose)
    class(unstable_t), intent(in) :: unstable
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    if (unstable%selected_decay > 0) then
       call unstable%decay(unstable%selected_decay)% &
            write_process_instances (unit, verbose)
    end if
  end subroutine unstable_write_process_instances
  
  recursive subroutine unstable_init (unstable, config)
    class(unstable_t), intent(out) :: unstable
    type(unstable_config_t), intent(in), target :: config
    integer :: i
    unstable%config => config
    allocate (unstable%decay (size (config%decay_config)))
    do i = 1, size (config%decay_config)
       call unstable%decay(i)%init (config%decay_config(i))
    end do
  end subroutine unstable_init
  
  recursive subroutine unstable_link_interactions (unstable, i_prt, trace)
    class(unstable_t), intent(inout) :: unstable
    integer, intent(in) :: i_prt
    type(interaction_t), intent(in), target :: trace
    integer :: i
    do i = 1, size (unstable%decay)
       call unstable%decay(i)%link_interactions (i_prt, trace)
    end do
  end subroutine unstable_link_interactions
    
  subroutine unstable_import_rng (unstable, rng)
    class(unstable_t), intent(inout) :: unstable
    class(rng_t), intent(inout), allocatable :: rng
    call move_alloc (from = rng, to = unstable%rng)
  end subroutine unstable_import_rng
  
  recursive subroutine unstable_select_chain (unstable)
    class(unstable_t), intent(inout) :: unstable
    real(default) :: x
    call unstable%rng%generate (x)
    unstable%selected_decay = unstable%config%selector%select (x)
    call unstable%decay(unstable%selected_decay)%select_chain ()
  end subroutine unstable_select_chain
    
  recursive subroutine unstable_generate (unstable)
    class(unstable_t), intent(inout) :: unstable
    call unstable%decay(unstable%selected_decay)%generate ()
  end subroutine unstable_generate
    
  subroutine decay_chain_final (object)
    class(decay_chain_t), intent(inout) :: object
    type(decay_chain_entry_t), pointer :: entry
    do while (associated (object%last))
       entry => object%last
       object%last => entry%previous
       call entry%final ()
       deallocate (entry)
    end do
    call evaluator_final (object%correlated_trace)
  end subroutine decay_chain_final
  
  subroutine decay_chain_write (object, unit)
    class(decay_chain_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    call write_separator (u, 2)
    write (u, "(1x,A)")  "Decay chain:"
    call write_entries (object%last)
    call write_separator (u, 2)
    write (u, "(1x,A)")  "Evaluator (correlated trace of the decay chain):"
    call write_separator (u)
    call object%correlated_trace%write (u)
    call write_separator (u, 2)
  contains
    recursive subroutine write_entries (entry)
      type(decay_chain_entry_t), intent(in), pointer :: entry
      if (associated (entry)) then
         call write_entries (entry%previous)
         call write_separator (u, 2)
         write (u, "(1x,A,I0)")  "Decay #", entry%index
         call entry%config%write_header (u)
         write (u, "(3x,A,I0)")  "Selected MCI    = ", entry%selected_mci
         write (u, "(3x,A,I0)")  "Selected term   = ", entry%selected_term
         call entry%config%term_config(entry%selected_term)%write (u, indent=1)
         call entry%write (u)
      end if
    end subroutine write_entries
  end subroutine decay_chain_write
    
  subroutine decay_chain_build (chain, decay_root)
    class(decay_chain_t), intent(inout), target :: chain
    type(decay_root_t), intent(in) :: decay_root
    type(quantum_numbers_mask_t), dimension(:), allocatable :: qn_mask
    type(interaction_t), pointer :: int_last_decay
    call chain%final ()
    if (decay_root%selected_term > 0) then
       chain%process_instance => decay_root%process_instance
       chain%selected_term = decay_root%selected_term
       call chain%build_term_entries (decay_root%term(decay_root%selected_term))
    end if
    int_last_decay => chain%last%get_matrix_int_ptr ()
    allocate (qn_mask (interaction_get_n_tot (int_last_decay)))
    call quantum_numbers_mask_init (qn_mask, &
         mask_f = .true., mask_c = .true., mask_h = .true.)
    call evaluator_init_qn_sum (chain%correlated_trace, int_last_decay, qn_mask)
  end subroutine decay_chain_build
    
  recursive subroutine decay_chain_build_term_entries (chain, term)
    class(decay_chain_t), intent(inout) :: chain
    type(decay_term_t), intent(in) :: term
    integer :: i
    do i = 1, size (term%particle_out)
       select type (unstable => term%particle_out(i)%c)
       type is (unstable_t)
          if (unstable%selected_decay > 0) then
             call chain%build_decay_entries &
                  (unstable%decay(unstable%selected_decay))
          end if
       end select
    end do
  end subroutine decay_chain_build_term_entries
  
  recursive subroutine decay_chain_build_decay_entries (chain, decay)
    class(decay_chain_t), intent(inout) :: chain
    type(decay_t), intent(in) :: decay
    type(decay_chain_entry_t), pointer :: entry
    type(connected_state_t), pointer :: previous_state
    type(isolated_state_t), pointer :: current_decay
    allocate (entry)
    if (associated (chain%last)) then
       entry%previous => chain%last
       entry%index = entry%previous%index + 1
       previous_state => entry%previous%connected_state_t
    else
       entry%index = 1
       previous_state => &
            chain%process_instance%get_connected_state_ptr (chain%selected_term)
    end if
    entry%config => decay%config
    entry%selected_mci = decay%selected_mci
    entry%selected_term = decay%selected_term
    current_decay => decay%process_instance%get_isolated_state_ptr &
         (decay%selected_term)
    call entry%setup_connected_trace &
         (current_decay, previous_state%get_trace_int_ptr (), resonant=.true.)
    call entry%setup_connected_matrix &
         (current_decay, previous_state%get_matrix_int_ptr (), resonant=.true.)
    call entry%setup_connected_flows &
         (current_decay, previous_state%get_flows_int_ptr (), resonant=.true.)
    chain%last => entry
    call chain%build_term_entries (decay%term(decay%selected_term))
  end subroutine decay_chain_build_decay_entries
    
  subroutine decay_chain_evaluate (chain)
    class(decay_chain_t), intent(inout) :: chain
    call evaluate (chain%last)
    call evaluator_receive_momenta (chain%correlated_trace)
    call chain%correlated_trace%evaluate ()
  contains
    recursive subroutine evaluate (entry)
      type(decay_chain_entry_t), intent(inout), pointer :: entry
      if (associated (entry)) then
         call evaluate (entry%previous)
         call entry%receive_kinematics ()
         call entry%evaluate_trace ()
         call entry%evaluate_event_data ()
      end if
    end subroutine evaluate
  end subroutine decay_chain_evaluate
  
  function decay_chain_get_probability (chain) result (x)
    class(decay_chain_t), intent(in) :: chain
    real(default) :: x
    x = evaluator_get_matrix_element (chain%correlated_trace, 1)
  end function decay_chain_get_probability
  
  subroutine evt_decay_write (object, unit, &
       show_decay_tree, &
       show_processes, &
       verbose)
    class(evt_decay_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: show_decay_tree, show_processes, verbose
    logical :: dec, prc, verb
    integer :: u
    u = given_output_unit (unit)
    dec = .true.;  if (present (show_decay_tree))  dec = show_decay_tree
    prc = .false.;  if (present (show_processes))  prc = show_processes
    verb = .false.;  if (present (verbose))  verb = verbose
    call write_separator (u, 2)
    write (u, "(1x,A)")  "Event transform: partonic decays"
    call write_separator (u, 2)
    call object%base_write (u)
    if (dec) then
       call write_separator (u)
       call object%decay_root%write (u)
       if (verb) then
          call object%decay_chain%write (u)
       end if
       if (prc) then
          call object%decay_root%write_process_instances (u, verb)
       end if
    else
       call write_separator (u, 2)
    end if
  end subroutine evt_decay_write
    
  subroutine evt_decay_connect (evt, process_instance, model, process_stack)
    class(evt_decay_t), intent(inout), target :: evt
    type(process_instance_t), intent(in), target :: process_instance
    class(model_data_t), intent(in), target :: model
    type(process_stack_t), intent(in), optional :: process_stack
    call evt%base_connect (process_instance, model)
    call evt%decay_root_config%connect (process_instance%process, &
         model, process_stack, process_instance)
    call evt%decay_root_config%compute ()
    call evt%decay_root%init (evt%decay_root_config, evt%process_instance)
  end subroutine evt_decay_connect
  
  subroutine evt_decay_prepare_new_event (evt, i_mci, i_term)
    class(evt_decay_t), intent(inout) :: evt
    integer, intent(in) :: i_mci, i_term
    call evt%reset ()
    evt%decay_root%selected_mci = i_mci
    evt%decay_root%selected_term = i_term
    call evt%decay_root%select_chain ()
    call evt%decay_chain%build (evt%decay_root)
  end subroutine evt_decay_prepare_new_event
  
  subroutine evt_decay_generate_weighted (evt, probability)
    class(evt_decay_t), intent(inout) :: evt
    real(default), intent(out) :: probability
    call evt%decay_root%generate ()
    if (signal_is_pending ())  return
    call evt%decay_chain%evaluate ()
    probability = evt%decay_chain%get_probability ()
  end subroutine evt_decay_generate_weighted
  
  subroutine evt_decay_make_particle_set &
       (evt, factorization_mode, keep_correlations, r)
    class(evt_decay_t), intent(inout) :: evt
    integer, intent(in) :: factorization_mode
    logical, intent(in) :: keep_correlations
    real(default), dimension(:), intent(in), optional :: r
    type(interaction_t), pointer :: int_matrix, int_flows
    type(decay_chain_entry_t), pointer :: last_entry
    last_entry => evt%decay_chain%last
    int_matrix => last_entry%get_matrix_int_ptr ()
    int_flows  => last_entry%get_flows_int_ptr ()
    call evt%factorize_interactions (int_matrix, int_flows, &
         factorization_mode, keep_correlations, r)
    call evt%tag_incoming ()
  end subroutine evt_decay_make_particle_set
    
  subroutine pacify_decay (evt)
    class(evt_decay_t), intent(inout) :: evt
    call pacify_decay_gen (evt%decay_root)
  contains
    recursive subroutine pacify_decay_gen (decay)
      class(decay_gen_t), intent(inout) :: decay
      if (associated (decay%process_instance)) then
         call pacify (decay%process_instance)
      end if
      if (decay%selected_term > 0) then
         call pacify_term (decay%term(decay%selected_term))
      end if
    end subroutine pacify_decay_gen
    recursive subroutine pacify_term (term)
      class(decay_term_t), intent(inout) :: term
      integer :: i
      do i = 1, size (term%particle_out)
         select type (unstable => term%particle_out(i)%c)
         type is (unstable_t);  call pacify_unstable (unstable)
         end select
      end do
    end subroutine pacify_term
    recursive subroutine pacify_unstable (unstable)
      class(unstable_t), intent(inout) :: unstable
      if (unstable%selected_decay > 0) then
         call pacify_decay_gen (unstable%decay(unstable%selected_decay))
      end if
    end subroutine pacify_unstable
  end subroutine pacify_decay
  

  subroutine decays_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (decays_1, "decays_1", &
         "branching and decay configuration", &
         u, results)
    call test (decays_2, "decays_2", &
         "cascade decay configuration", &
         u, results)
    call test (decays_3, "decays_3", &
         "associate process", &
         u, results)
    call test (decays_4, "decays_4", &
         "decay instance", &
         u, results)
    call test (decays_5, "decays_5", &
         "parent process and decay", &
         u, results)
    call test (decays_6, "decays_6", &
         "evt_decay object", &
         u, results)
  end subroutine decays_test
  
  subroutine prepare_testbed &
       (lib, process_stack, prefix, os_data, &
        scattering, decay, decay_rest_frame)
    type(process_library_t), intent(out), target :: lib
    type(process_stack_t), intent(out) :: process_stack
    type(string_t), intent(in) :: prefix
    type(os_data_t), intent(in) :: os_data
    logical, intent(in) :: scattering, decay
    logical, intent(in), optional :: decay_rest_frame

    type(model_data_t), target :: model
    class(model_data_t), pointer :: model_copy
    type(string_t) :: libname, procname1, procname2, run_id
    type(qcd_t) :: qcd
    class(rng_factory_t), allocatable :: rng_factory
    type(process_entry_t), pointer :: process
    type(process_instance_t), allocatable, target :: process_instance
    class(prc_core_t), allocatable :: core_template
    class(mci_t), allocatable :: mci_template
    class(phs_config_t), allocatable :: phs_config_template
    type(field_data_t), pointer :: field_data
    real(default) :: sqrts

    libname = prefix // "_lib"
    procname1 = prefix // "_p"
    procname2 = prefix // "_d"
    run_id = prefix

    call model%init_test ()
    call model%set_par (var_str ("ff"), 0.4_default)
    call model%set_par (var_str ("mf"), &
         model%get_real (var_str ("ff")) * model%get_real (var_str ("ms")))

    if (scattering .and. decay) then
       field_data => model%get_field_ptr (25)
       call field_data%set (p_is_stable = .false.)
    end if
    
    call prc_test_create_library (libname, lib, &
         scattering = .true., decay = .true., &
         procname1 = procname1, procname2 = procname2)

    call reset_interaction_counter ()

    allocate (test_t :: core_template)
    allocate (mci_midpoint_t :: mci_template)
    allocate (phs_single_config_t :: phs_config_template)

    if (scattering) then

       allocate (rng_test_factory_t :: rng_factory)
       allocate (model_copy)
       call model_copy%init (model%get_name (), &
            model%get_n_real (), &
            model%get_n_complex (), &
            model%get_n_field (), &
            model%get_n_vtx ())
       call model_copy%copy_from (model)

       allocate (process)
       call process%init (procname1, &
            run_id, lib, os_data, qcd, rng_factory, model_copy)
       call process%init_component &
            (1, core_template, mci_template, phs_config_template)
       sqrts = 1000
       call process%setup_beams_sqrts (sqrts)
       call process%configure_phs ()
       call process%setup_mci ()
       call process%setup_terms ()

       allocate (process_instance)
       call process_instance%init (process%process_t)
       call process%integrate (process_instance, 1, n_it=1, n_calls=100)
       call process%final_integration (1)
       call process_instance%final ()
       deallocate (process_instance)

       call process%prepare_simulation (1)
       call process_stack%push (process)
    end if
    
    if (decay) then
       allocate (rng_test_factory_t :: rng_factory)
       allocate (model_copy)
       call model_copy%init (model%get_name (), &
            model%get_n_real (), &
            model%get_n_complex (), &
            model%get_n_field (), &
            model%get_n_vtx ())
       call model_copy%copy_from (model)

       allocate (process)
       call process%init (procname2, &
            run_id, lib, os_data, qcd, rng_factory, model_copy)
       call process%init_component &
            (1, core_template, mci_template, phs_config_template)
       if (present (decay_rest_frame)) then
          call process%setup_beams_decay (rest_frame = decay_rest_frame)
       else
          call process%setup_beams_decay (rest_frame = .not. scattering)
       end if
       call process%configure_phs ()
       call process%setup_mci ()
       call process%setup_terms ()

       allocate (process_instance)
       call process_instance%init (process%process_t)
       call process%integrate (process_instance, 1, n_it=1, n_calls=100)
       call process%final_integration (1)
       call process_instance%final ()
       deallocate (process_instance)

       call process%prepare_simulation (1)
       call process_stack%push (process)
    end if
    
    call model%final ()

  end subroutine prepare_testbed

  subroutine decays_1 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_data_t), target :: model
!    type(model_t), pointer :: model
!    type(model_list_t) :: model_list
    type(flavor_t) :: flv_h
    type(flavor_t), dimension(2,1) :: flv_hbb, flv_hgg
    type(unstable_config_t), allocatable :: unstable

    write (u, "(A)")  "* Test output: decays_1"
    write (u, "(A)")  "*   Purpose: Set up branching and decay configuration"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize environment"
    write (u, "(A)")

    call os_data_init (os_data)
    call model%init_sm_test ()
!    call model_list%read_model (var_str ("SM"), &
!         var_str ("SM.mdl"), os_data, model)

    call flavor_init (flv_h, 25, model)
    call flavor_init (flv_hbb(:,1), [5, -5], model)
    call flavor_init (flv_hgg(:,1), [22, 22], model)

    write (u, "(A)")  "* Set up branching and decay"
    write (u, "(A)")

    allocate (unstable)
    unstable%flv = flv_h
    call unstable%init_decays ([var_str ("h_bb"), var_str ("h_gg")], model)
    
    associate (decay => unstable%decay_config(1))
      allocate (decay%term_config (1))
      call decay%init_term (1, flv_hbb, stable = [.true., .true.], model=model)
      decay%integral = 1.234e-3_default
      decay%abs_error = decay%integral * .02_default
    end associate
    
    associate (decay => unstable%decay_config(2))
      allocate (decay%term_config (1))
      call decay%init_term (1, flv_hgg, stable = [.true., .true.], model=model)
      decay%integral = 3.085e-4_default
      decay%abs_error = decay%integral * .08_default
    end associate
    
    call unstable%compute ()
    call unstable%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call unstable%final ()
    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: decays_1"
    
  end subroutine decays_1
  
  subroutine decays_2 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
!    type(model_t), pointer :: model
!    type(model_list_t) :: model_list
    type(model_data_t), target :: model
    type(flavor_t) :: flv_h, flv_wp, flv_wm
    type(flavor_t), dimension(2,1) :: flv_hww, flv_wud, flv_wen
    type(unstable_config_t), allocatable :: unstable
    type(string_t), dimension(:), allocatable :: decay

    write (u, "(A)")  "* Test output: decays_2"
    write (u, "(A)")  "*   Purpose: Set up cascade branching"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize environment"
    write (u, "(A)")

!    call syntax_model_file_init ()
    call os_data_init (os_data)
!    call model_list%read_model (var_str ("SM"), &
!         var_str ("SM.mdl"), os_data, model)
    call model%init_sm_test ()

    call model%set_unstable (25, [var_str ("h_ww")])
    call model%set_unstable (24, [var_str ("w_ud"), var_str ("w_en")])

    call flavor_init (flv_h, 25, model)
    call flavor_init (flv_hww(:,1), [24, -24], model)
    call flavor_init (flv_wp, 24, model)
    call flavor_init (flv_wm,-24, model)
    call flavor_init (flv_wud(:,1), [2, -1], model)
    call flavor_init (flv_wen(:,1), [-11, 12], model)
    

    write (u, "(A)")  "* Set up branching and decay"
    write (u, "(A)")

    allocate (unstable)
    unstable%flv = flv_h
    call flavor_get_decays (unstable%flv, decay)
    call unstable%init_decays (decay, model)
    
    associate (decay => unstable%decay_config(1))

      decay%integral = 1.e-3_default
      decay%abs_error = decay%integral * .01_default

      allocate (decay%term_config (1))
      call decay%init_term (1, flv_hww, stable = [.false., .true.], model=model)

      select type (w => decay%term_config(1)%prt(1)%c)
      type is (unstable_config_t)

         associate (w_decay => w%decay_config(1))
           w_decay%integral = 2._default
           allocate (w_decay%term_config (1))
           call w_decay%init_term (1, flv_wud, stable = [.true., .true.], &
                model=model)
         end associate
         associate (w_decay => w%decay_config(2))
           w_decay%integral = 1._default
           allocate (w_decay%term_config (1))
           call w_decay%init_term (1, flv_wen, stable = [.true., .true.], &
                model=model)
         end associate
         call w%compute ()

      end select
      
    end associate
    
    call unstable%compute ()
    call unstable%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call unstable%final ()
    call model%final ()
!    call model_list%final ()
!    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: decays_2"
    
  end subroutine decays_2
  
  subroutine decays_3 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    class(model_data_t), pointer :: model
    type(process_library_t), target :: lib
    type(string_t) :: prefix
    type(string_t) :: procname2
    type(process_stack_t) :: process_stack
    type(process_t), pointer :: process
    type(unstable_config_t), allocatable :: unstable

    write (u, "(A)")  "* Test output: decays_3"
    write (u, "(A)")  "*   Purpose: Connect a decay configuration &
         &with a process"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize environment and integrate process"
    write (u, "(A)")

    call os_data_init (os_data)

    prefix = "decays_3"
    call prepare_testbed &
         (lib, process_stack, prefix, os_data, &
         scattering=.false., decay=.true., decay_rest_frame=.false.)

    procname2 = prefix // "_d"
    process => process_stack%get_process_ptr (procname2)
    model => process%get_model_ptr ()
    call process%write (.false., u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Set up branching and decay"
    write (u, "(A)")

    allocate (unstable)
    call flavor_init (unstable%flv, 25, model)
    call unstable%init_decays ([procname2], model)
    
    write (u, "(A)")  "* Connect decay with process object"
    write (u, "(A)")

    associate (decay => unstable%decay_config(1))
      call decay%connect (process, model)
    end associate
    
    call unstable%compute ()
    call unstable%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call unstable%final ()
    call process_stack%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: decays_3"
    
  end subroutine decays_3

  subroutine decays_4 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    class(model_data_t), pointer :: model
    type(process_library_t), target :: lib
    type(string_t) :: prefix, procname2
    class(rng_t), allocatable :: rng
    type(process_stack_t) :: process_stack
    type(process_t), pointer :: process
    type(unstable_config_t), allocatable, target :: unstable
    type(unstable_t), allocatable :: instance

    write (u, "(A)")  "* Test output: decays_4"
    write (u, "(A)")  "*   Purpose: Create a decay process and evaluate &
         &an instance"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize environment, process, &
         &and decay configuration"
    write (u, "(A)")

    call os_data_init (os_data)

    prefix = "decays_4"
    call prepare_testbed &
         (lib, process_stack, prefix, os_data, &
         scattering=.false., decay=.true., decay_rest_frame = .false.)

    procname2 = prefix // "_d"
    process => process_stack%get_process_ptr (procname2)
    model => process%get_model_ptr ()

    allocate (unstable)
    call flavor_init (unstable%flv, 25, model)
    call unstable%init_decays ([procname2], model)
    
    call model%set_unstable (25, [procname2])

    associate (decay => unstable%decay_config(1))
      call decay%connect (process, model)
    end associate
    
    call unstable%compute ()

    allocate (rng_test_t :: rng)

    allocate (instance)
    call instance%init (unstable)
    call instance%import_rng (rng)

    call instance%select_chain ()
    call instance%generate ()
    call instance%write (u)

    write (u, *)
    call instance%write_process_instances (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call instance%final ()
    call process_stack%final ()
    call unstable%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: decays_4"
    
  end subroutine decays_4

  subroutine decays_5 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    class(model_data_t), pointer :: model
    type(process_library_t), target :: lib
    type(string_t) :: prefix, procname1, procname2
    type(process_stack_t) :: process_stack
    type(process_t), pointer :: process
    type(process_instance_t), allocatable, target :: process_instance
    type(decay_root_config_t), target :: decay_root_config
    type(decay_root_t) :: decay_root
    type(decay_chain_t) :: decay_chain
    integer :: i

    write (u, "(A)")  "* Test output: decays_5"
    write (u, "(A)")  "*   Purpose: Handle a process with subsequent decays"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize environment and parent process"
    write (u, "(A)")

    call os_data_init (os_data)

    prefix = "decays_5"
    procname1 = prefix // "_p"
    procname2 = prefix // "_d"
    call prepare_testbed &
         (lib, process_stack, prefix, os_data, &
         scattering=.true., decay=.true.)

    write (u, "(A)")  "* Initialize decay process"
    write (u, "(A)")

    process => process_stack%get_process_ptr (procname1)
    model => process%get_model_ptr ()
    call model%set_unstable (25, [procname2])

    write (u, "(A)")  "* Initialize decay tree configuration"
    write (u, "(A)")

    call decay_root_config%connect (process, model, process_stack)
    call decay_root_config%compute ()
    call decay_root_config%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize decay tree"

    allocate (process_instance)
    call process_instance%init (process)
    call process_instance%setup_event_data ()
    call process_instance%init_simulation (1)

    call decay_root%init (decay_root_config, process_instance)

    write (u, "(A)")
    write (u, "(A)")  "* Select decay chain"
    write (u, "(A)")

    decay_root%selected_mci = 1
    !!! Not yet implemented; there is only one term anyway:
    ! call process_instance%select_i_term (decay_root%selected_term)
    decay_root%selected_term = 1
    call decay_root%select_chain ()

    call decay_chain%build (decay_root)
    
    call decay_root%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Generate event"
    write (u, "(A)")

    call process%generate_unweighted_event (process_instance, &
         decay_root%selected_mci)
    call process_instance%evaluate_event_data ()
    
    call decay_root%generate ()
    
    associate (term => decay_root%term(1))
      do i = 1, 2
         select type (unstable => term%particle_out(i)%c)
         type is (unstable_t)
            associate (decay => unstable%decay(1))
              call pacify (decay%process_instance)
            end associate
         end select
      end do
    end associate
    
    write (u, "(A)")  "* Process instances"
    write (u, "(A)")
    
    call decay_root%write_process_instances (u)

    write (u, "(A)")
    write (u, "(A)")  "* Generate decay chain"
    write (u, "(A)")
    
    call decay_chain%evaluate ()
    call decay_chain%write (u)

    write (u, *)
    write (u, "(A,ES19.12)")  "chain probability =", &
         decay_chain%get_probability ()

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call decay_chain%final ()
    call decay_root%final ()
    call decay_root_config%final ()
    call process_instance%final ()
    deallocate (process_instance)
    
    call process%final ()
    call process_stack%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: decays_5"
    
  end subroutine decays_5

  subroutine decays_6 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    class(model_data_t), pointer :: model
    type(process_library_t), target :: lib
    type(string_t) :: prefix, procname1, procname2
    type(process_stack_t) :: process_stack
    type(process_t), pointer :: process
    type(process_instance_t), allocatable, target :: process_instance
    type(evt_decay_t), target :: evt_decay
    integer :: factorization_mode
    logical :: keep_correlations

    write (u, "(A)")  "* Test output: decays_6"
    write (u, "(A)")  "*   Purpose: Handle a process with subsequent decays"
    write (u, "(A)")

    write (u, "(A)")  "* Initialize environment and parent process"
    write (u, "(A)")

    call os_data_init (os_data)

    prefix = "decays_6"
    procname1 = prefix // "_p"
    procname2 = prefix // "_d"
    call prepare_testbed &
         (lib, process_stack, prefix, os_data, &
         scattering=.true., decay=.true.)

    write (u, "(A)")  "* Initialize decay process"

    process => process_stack%get_process_ptr (procname1)
    model => process%get_model_ptr ()
    call model%set_unstable (25, [procname2])

    allocate (process_instance)
    call process_instance%init (process)
    call process_instance%setup_event_data ()
    call process_instance%init_simulation (1)
    
    write (u, "(A)")
    write (u, "(A)")  "* Initialize decay object"

    call evt_decay%connect (process_instance, model, process_stack)

    write (u, "(A)")
    write (u, "(A)")  "* Generate scattering event"

    call process%generate_unweighted_event (process_instance, 1)
    call process_instance%evaluate_event_data ()

    write (u, "(A)")
    write (u, "(A)")  "* Select decay chain and generate event"
    write (u, "(A)")

    call evt_decay%prepare_new_event (1, 1)
    call evt_decay%generate_unweighted ()

    factorization_mode = FM_IGNORE_HELICITY
    keep_correlations = .false.
    call evt_decay%make_particle_set (factorization_mode, keep_correlations)

    call evt_decay%write (u, verbose = .true.)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call evt_decay%final ()
    call process_instance%final ()
    deallocate (process_instance)
    
    call process_stack%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: decays_6"
    
  end subroutine decays_6


end module decays

