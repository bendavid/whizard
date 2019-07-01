! WHIZARD 2.0.4 Tue Oct 26 2010
! 
! (C) 1999-2010 by 
!     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
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

module hard_interactions

  use iso_c_binding !NODEP!
  use kinds !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use lorentz !NODEP!
  use os_interface
  use models
  use flavors
  use helicities
  use colors
  use quantum_numbers
  use state_matrices
  use interactions
  use evaluators
  use particles
  use prclib_interfaces
  use process_libraries

  implicit none
  private

  public :: hard_interaction_t
  public :: hard_interaction_init
  public :: hard_interaction_unload
  public :: hard_interaction_reload
  public :: hard_interaction_update_parameters
  public :: hard_interaction_final
  public :: hard_interaction_write
  public :: assignment(=)
  public :: hard_interaction_is_valid
  public :: hard_interaction_get_id
  public :: hard_interaction_get_model_ptr
  public :: hard_interaction_get_n_in
  public :: hard_interaction_get_n_out
  public :: hard_interaction_get_n_tot
  public :: hard_interaction_get_n_flv
  public :: hard_interaction_get_n_col
  public :: hard_interaction_get_n_hel
  public :: hard_interaction_get_flv_states
  public :: hard_interaction_get_n_cf
  public :: hard_interaction_get_first_pdg_in
  public :: hard_interaction_get_first_pdg_out
  public :: hard_interaction_get_unstable_products
  public :: hard_interaction_init_trace
  public :: hard_interaction_init_sqme
  public :: hard_interaction_init_flows
  public :: hard_interaction_final_sqme
  public :: hard_interaction_final_flows
  public :: hard_interaction_update_alpha_s
  public :: hard_interaction_reset_helicity_selection
  public :: hard_interaction_evaluate
  public :: hard_interaction_evaluate_sqme
  public :: hard_interaction_evaluate_flows
  public :: hard_interaction_compute_sqme_sum
  public :: hard_interaction_get_int_ptr
  public :: hard_interaction_get_eval_trace_ptr
  public :: hard_interaction_get_eval_sqme_ptr
  public :: hard_interaction_get_eval_flows_ptr
  public :: hard_interaction_recover_kinematics
  public :: hard_interaction_write_state_summary
  public :: hard_interaction_test

  type :: hard_interaction_data_t
     type(string_t) :: id
     type(model_t), pointer :: model => null ()
     integer :: n_tot = 0
     integer :: n_in = 0
     integer :: n_out = 0
     integer :: n_flv = 0
     integer :: n_hel = 0
     integer :: n_col = 0
     integer :: n_cin = 0
     integer :: n_cf  = 0
     real(default), dimension(:), allocatable :: par
     integer, dimension(:,:), allocatable :: flv_state, hel_state
     integer, dimension(:,:,:), allocatable :: col_state
     logical, dimension(:,:), allocatable :: ghost_flag
     integer, dimension(:,:), allocatable :: col_flow_index
     complex(default), dimension(:), allocatable :: col_factor
     procedure(prc_init), nopass, pointer :: init => null ()
     procedure(prc_final), nopass, pointer :: final => null ()
     procedure(prc_update_alpha_s), nopass, pointer :: update_alpha_s => null ()
     procedure(prc_reset_helicity_selection), nopass, pointer :: &
          reset_helicity_selection => null ()
     procedure(prc_new_event), nopass, pointer :: new_event => null ()
     procedure(prc_is_allowed), nopass, pointer :: is_allowed => null ()
     procedure(prc_get_amplitude), nopass, pointer :: get_amplitude => null ()
  end type hard_interaction_data_t

  type :: hard_interaction_t
     private
     logical :: initialized = .false.
     type(hard_interaction_data_t) :: data
     integer :: n_values = 0
     integer, dimension(:), allocatable :: flv, hel, col
     type(interaction_t) :: int
     type(evaluator_t) :: eval_trace
     type(evaluator_t) :: eval_sqme
     type(evaluator_t) :: eval_flows
  end type hard_interaction_t


  interface assignment(=)
     module procedure hard_interaction_assign
  end interface


contains

  subroutine hard_interaction_data_unload (data)
    type(hard_interaction_data_t), intent(inout) :: data
    call data%final
    nullify (data%init)
    nullify (data%final)
    nullify (data%update_alpha_s)
    nullify (data%reset_helicity_selection)
    nullify (data%new_event)
    nullify (data%is_allowed)
    nullify (data%get_amplitude)
  end subroutine hard_interaction_data_unload

  subroutine hard_interaction_data_reload (data, prc_lib, pid)
    type(hard_interaction_data_t), intent(inout) :: data
    type(process_library_t), intent(in) :: prc_lib
    integer, optional :: pid
    integer :: the_pid
    type(c_funptr) :: fptr
    if (present (pid)) then
       the_pid = pid
    else
       the_pid = process_library_get_process_pid (prc_lib, data%id)
       if (the_pid <= 0) call msg_bug &
          ("Invalid process ID '" // char (data%id) // "'")
    end if
    call prc_lib% init_get_fptr (the_pid, fptr)
    call c_f_procpointer (fptr, data% init)
    call prc_lib% final_get_fptr (the_pid, fptr)
    call c_f_procpointer (fptr, data% final)
    call prc_lib% update_alpha_s_get_fptr (the_pid, fptr)
    call c_f_procpointer (fptr, data% update_alpha_s)
    call prc_lib% reset_helicity_selection_get_fptr (the_pid, fptr)
    call c_f_procpointer (fptr, data% reset_helicity_selection)
    call prc_lib% new_event_get_fptr (the_pid, fptr)
    call c_f_procpointer (fptr, data% new_event)
    call prc_lib% is_allowed_get_fptr (the_pid, fptr)
    call c_f_procpointer (fptr, data% is_allowed)
    call prc_lib% get_amplitude_get_fptr (the_pid, fptr)
    call c_f_procpointer (fptr, data% get_amplitude)
  end subroutine hard_interaction_data_reload
    
  subroutine hard_interaction_data_init &
       (data, prc_lib, process_index, process_id, model)
    type(hard_interaction_data_t), intent(out) :: data
    type(process_library_t), intent(in) :: prc_lib
    integer, intent(in) :: process_index
    type(string_t), intent(in) :: process_id
    type(model_t), intent(in), target :: model
    integer(c_int) :: pid
    type(string_t) :: model_name
    integer(c_int), dimension(:,:), allocatable, target :: flv_state, hel_state
    integer(c_int), dimension(:,:,:), allocatable, target :: col_state
    logical(c_bool), dimension(:,:), allocatable, target :: ghost_flag
    integer(c_int), dimension(:), allocatable, target :: cf_index1, cf_index2
    complex(c_default_complex), dimension(:), allocatable, target :: col_factor
    integer :: c, i
    if (.not. associated (prc_lib% get_process_id)) then
       call msg_fatal ("Process library '" // char (prc_lib%basename) // "':" &
            // " procedures unavailable (missing compile command?)")
       data%id = ""
       return
    end if
    pid = process_index
    data%id = process_library_get_process_id (prc_lib, pid)
    if (data%id /= process_id) then
       call msg_bug ("Process ID mismatch: requested '" &
            // char (process_id) // "' but found '" // char (data%id) // "'")
    end if
    data%model => model
    model_name = process_library_get_process_model_name (prc_lib, pid)
    if (model_get_name (data%model) /= model_name) then
       call msg_warning ("Process '" // char (process_id) // "': " &
            // "temporarily resetting model from '" &
            // char (model_get_name (data%model)) // "' to '" &
            // char (model_name) // "'")
       data%model => model_list_get_model_ptr (model_name)
       if (.not. associated (data%model)) then
          call msg_fatal ("Model '" // char (model_name) &
               // "' is not initialized")
       end if
    end if
    data%n_in  = prc_lib% get_n_in  (pid)
    data%n_out = prc_lib% get_n_out (pid)
    data%n_tot = data%n_in + data%n_out
    data%n_flv = prc_lib% get_n_flv (pid)
    data%n_hel = prc_lib% get_n_hel (pid)
    data%n_col = prc_lib% get_n_col (pid)
    data%n_cin = prc_lib% get_n_cin (pid)
    data%n_cf  = prc_lib% get_n_cf  (pid)
    if (data%n_flv == 0) then
       call msg_warning ("Process '" // char (process_id) // "': " &
            // "matrix element vanishes.")
    end if
    call model_parameters_to_array (data%model, data%par)
    allocate (data%flv_state (data%n_tot, data%n_flv))
    allocate (data%hel_state (data%n_tot, data%n_hel))
    allocate (data%col_state (data%n_cin, data%n_tot, data%n_col))
    allocate (data%ghost_flag (data%n_tot, data%n_col))
    allocate (data%col_flow_index (2, data%n_cf))
    allocate (data%col_factor (data%n_cf))
    allocate (flv_state (data%n_tot, data%n_flv))
    allocate (hel_state (data%n_tot, data%n_hel))
    allocate (col_state (data%n_cin, data%n_tot, data%n_col))
    allocate (ghost_flag (data%n_tot, data%n_col))
    allocate (cf_index1 (data%n_cf))
    allocate (cf_index2 (data%n_cf))
    allocate (col_factor (data%n_cf))
    call prc_lib% set_flv_state (pid, &
         c_loc (flv_state), &
         int((/data%n_tot, data%n_flv/), kind=c_int))
    data%flv_state = flv_state
    call prc_lib% set_hel_state (pid, &
         c_loc (hel_state), &
         int((/data%n_tot, data%n_hel/), kind=c_int))
    data%hel_state = hel_state
    call prc_lib% set_col_state (pid, &
         c_loc (col_state), &
         int((/data%n_cin, data%n_tot, data%n_col/), kind=c_int), &
         c_loc (ghost_flag), &
         int((/data%n_tot, data%n_col/), kind=c_int))
    if (data%n_cin /= 2)  &
         call msg_bug ("Process library '" // char (prc_lib%basename) // "':" &
              // " number of color indices must be two")
    forall (c = 1:2, i = 1:data%n_in)
       data%col_state(c,i,:) = - col_state(3-c,i,:)
    end forall
    forall (i = data%n_in+1:data%n_tot)
       data%col_state(:,i,:) = col_state(:,i,:)
    end forall
    data%ghost_flag = ghost_flag
    call prc_lib% set_cf_table (pid, &
         c_loc (cf_index1), c_loc (cf_index2), c_loc (col_factor), &
         int ((/data%n_cf/), kind=c_int))
    data%col_flow_index(1,:) = cf_index1
    data%col_flow_index(2,:) = cf_index2
    data%col_factor = col_factor
    call hard_interaction_data_reload (data, prc_lib, pid=pid)
  end subroutine hard_interaction_data_init

  subroutine hard_interaction_data_write (data, unit)
    type(hard_interaction_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    integer :: f, h, c, n, i
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) "Process '", char (trim (data%id)), "'"
    write (u, *) "n_tot = ", data%n_tot
    write (u, *) "n_in  = ", data%n_in
    write (u, *) "n_out = ", data%n_out
    write (u, *) "n_flv = ", data%n_flv
    write (u, *) "n_hel = ", data%n_hel
    write (u, *) "n_col = ", data%n_col
    write (u, *) "n_cin = ", data%n_cin
    write (u, *) "n_cf  = ", data%n_cf
    write (u, *) "Model parameters:"
    do i = 1, size (data%par)
       write (u, *) i, data%par(i)
    end do
    write (u, *) "Flavor states:"
    do f = 1, data%n_flv
       write (u, *) f, ":", data%flv_state (:,f)
    end do
    write (u, *) "Helicity states:"
    do h = 1, data%n_hel
       write (u, *) h, ":", data%hel_state (:,h)
    end do
    write (u, *) "Color states:"
    do c = 1, data%n_col
       write (u, "(I5,A)", advance="no") c, ":"
       do n = 1, data%n_tot
          write (u, "('/')", advance="no")
          if (data%ghost_flag (n, c))  write (u, "('*')", advance="no")
          do i = 1, data%n_cin
             if (data%col_state(i,n,c) == 0)  cycle
             write (u, "(I3)", advance="no")  data%col_state(i,n,c)
          end do
       end do
       write (u, "('/')")
    end do
    write (u, *) "Color factors:"
    do c = 1, data%n_cf
       write (u, "(I5,A,2(I4,1x))", advance="no") c, ":", &
            data%col_flow_index(:,c)
       write (u, *) data%col_factor(c)
    end do
  end subroutine hard_interaction_data_write

  subroutine hard_interaction_init &
       (hi, prc_lib, process_index, process_id, model)
    type(hard_interaction_t), intent(out), target :: hi
    type(process_library_t), intent(in) :: prc_lib
    integer, intent(in) :: process_index
    type(string_t), intent(in) :: process_id
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(:), allocatable :: flv
    type(color_t), dimension(:), allocatable :: col
    type(helicity_t), dimension(:), allocatable :: hel
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    integer :: f, h, c, i, n
    call hard_interaction_data_init &
         (hi%data, prc_lib, process_index, process_id, model)
    if (hi%data%id == "") return
    call hi%data% init (real (hi%data%par, c_default_float))
    call interaction_init &
         (hi%int, hi%data%n_in, 0, hi%data%n_out, set_relations=.true.)
    call hard_interaction_reset_helicity_selection (hi, 0._default, 0)
    n = 0
    do f = 1, hi%data%n_flv
       do h = 1, hi%data%n_hel
          do c = 1, hi%data%n_col
             if (hi%data%is_allowed (f, h, c))  n = n + 1
          end do
       end do
    end do
    hi%n_values = n
    allocate (hi%flv (n), hi%hel (n), hi%col (n))
    allocate (flv (hi%data%n_tot), col (hi%data%n_tot), hel (hi%data%n_tot))
    allocate (qn (hi%data%n_tot))
    i = 0
    do f = 1, hi%data%n_flv
       do h = 1, hi%data%n_hel
          do c = 1, hi%data%n_col
             if (hi%data%is_allowed (f, h, c)) then
                i = i + 1
                hi%flv(i) = f
                hi%hel(i) = h
                hi%col(i) = c
                call flavor_init (flv, hi%data%flv_state(:,f), hi%data%model)
                call color_init_from_array (col, hi%data%col_state(:,:,c), &
                                                 hi%data%ghost_flag(:,c))
                call helicity_init (hel, hi%data%hel_state(:,h))
                call quantum_numbers_init (qn, flv, col, hel)
                call interaction_add_state (hi%int, qn)
             end if
          end do
       end do
    end do
    call interaction_freeze (hi%int)
    hi%initialized = .true.
  end subroutine hard_interaction_init

  subroutine hard_interaction_unload (hi)
    type(hard_interaction_t), intent(inout), target :: hi
    if (.not. associated (hi%data%final)) return
    call hard_interaction_data_unload (hi%data)
  end subroutine hard_interaction_unload

  subroutine hard_interaction_reload (hi, prc_lib)
    type(hard_interaction_t), intent(inout), target :: hi
    type(process_library_t), intent(in) :: prc_lib
    if (associated (hi%data%init)) return
    call hard_interaction_data_reload (hi%data, prc_lib)
    call hi%data% init (real (hi%data%par, c_default_float))
  end subroutine hard_interaction_reload

  subroutine hard_interaction_update_parameters (hi)
    type(hard_interaction_t), intent(inout), target :: hi
    call model_parameters_to_array (hi%data%model, hi%data%par)
    call hi%data% init (real (hi%data%par, c_default_float))
  end subroutine hard_interaction_update_parameters

  subroutine hard_interaction_final (hi)
    type(hard_interaction_t), intent(inout) :: hi
    hi%initialized = .false.
    if (associated (hi%data% final))  call hi%data% final ()
    call interaction_final (hi%int)
    call evaluator_final (hi%eval_trace)
    call evaluator_final (hi%eval_flows)
    call evaluator_final (hi%eval_sqme)
    hi%n_values = 0
    if (allocated (hi%flv))  deallocate (hi%flv)
    if (allocated (hi%hel))  deallocate (hi%hel)
    if (allocated (hi%col))  deallocate (hi%col)
  end subroutine hard_interaction_final

  subroutine hard_interaction_write &
       (hi, unit, verbose, show_momentum_sum, show_mass, write_comb)
    type(hard_interaction_t), intent(in) :: hi
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, show_momentum_sum, show_mass
    logical, intent(in), optional :: write_comb
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A)")  "Hard interaction:"
    call hard_interaction_data_write (hi%data, u)
    if (present (write_comb)) then
       if (write_comb .and. hi%n_values /= 0) then
          write (u, "(1x,A)")  "Allowed f/h/c index combinations:"
          do i = 1, hi%n_values
             write (u, *)  i, ":", hi%flv(i), hi%hel(i), hi%col(i)
          end do
       end if
    end if
    write (u, *)
    call interaction_write &
         (hi%int, unit, verbose, show_momentum_sum, show_mass)
    write (u, *) repeat ("- ", 36)
    write (u, "(A)") "Trace including color factors (hard interaction)"
    call evaluator_write &
         (hi%eval_trace, unit, verbose, show_momentum_sum, show_mass)
    write (u, *) repeat ("- ", 36)
    write (u, "(A)") "Exclusive sqme including color factors (hard interaction)"
    call evaluator_write &
         (hi%eval_sqme, unit, verbose, show_momentum_sum, show_mass)
    write (u, *) repeat ("- ", 36)
    write (u, "(A)") "Color flow coefficients (hard interaction)"
    call evaluator_write &
         (hi%eval_flows, unit, verbose, show_momentum_sum, show_mass)
  end subroutine hard_interaction_write

  subroutine hard_interaction_assign (hi_out, hi_in)
    type(hard_interaction_t), intent(out) :: hi_out
    type(hard_interaction_t), intent(in) :: hi_in
    hi_out%initialized = hi_in%initialized
    hi_out%data = hi_in%data
    hi_out%n_values = hi_in%n_values
    if (allocated (hi_in%flv)) then
       allocate (hi_out%flv (size (hi_in%flv)))
       hi_out%flv = hi_in%flv
    end if
    if (allocated (hi_in%hel)) then
       allocate (hi_out%hel (size (hi_in%hel)))
       hi_out%hel = hi_in%hel
    end if
    if (allocated (hi_in%col)) then
       allocate (hi_out%col (size (hi_in%col)))
       hi_out%col = hi_in%col
    end if
    hi_out%int = hi_in%int
    hi_out%eval_trace = hi_in%eval_trace
    hi_out%eval_sqme = hi_in%eval_sqme
    hi_out%eval_flows = hi_in%eval_flows
  end subroutine hard_interaction_assign

  function hard_interaction_is_valid (hi) result (flag)
    logical :: flag
    type(hard_interaction_t), intent(in) :: hi
    flag = hi%initialized
  end function hard_interaction_is_valid

  function hard_interaction_get_id (hi) result (id)
    type(string_t) :: id
    type(hard_interaction_t), intent(in) :: hi
    id = hi%data%id
  end function hard_interaction_get_id

  function hard_interaction_get_model_ptr (hi) result (model)
    type(model_t), pointer :: model
    type(hard_interaction_t), intent(in) :: hi
    model => hi%data%model
  end function hard_interaction_get_model_ptr

  pure function hard_interaction_get_n_in (hi) result (n_in)
    integer :: n_in
    type(hard_interaction_t), intent(in) :: hi
    n_in = hi%data%n_in
  end function hard_interaction_get_n_in

  pure function hard_interaction_get_n_out (hi) result (n_out)
    integer :: n_out
    type(hard_interaction_t), intent(in) :: hi
    n_out = hi%data%n_out
  end function hard_interaction_get_n_out

  pure function hard_interaction_get_n_tot (hi) result (n_tot)
    integer :: n_tot
    type(hard_interaction_t), intent(in) :: hi
    n_tot = hi%data%n_tot
  end function hard_interaction_get_n_tot
 
  pure function hard_interaction_get_n_flv (hi) result (n_flv)
    integer :: n_flv
    type(hard_interaction_t), intent(in) :: hi
    n_flv = hi%data%n_flv
  end function hard_interaction_get_n_flv

  pure function hard_interaction_get_n_col (hi) result (n_col)
    integer :: n_col
    type(hard_interaction_t), intent(in) :: hi
    n_col = hi%data%n_col
  end function hard_interaction_get_n_col

  pure function hard_interaction_get_n_hel (hi) result (n_hel)
    integer :: n_hel
    type(hard_interaction_t), intent(in) :: hi
    n_hel = hi%data%n_hel
  end function hard_interaction_get_n_hel

  function hard_interaction_get_flv_states (hi) result (flv_state)
    integer, dimension(:,:), allocatable :: flv_state
    type(hard_interaction_t), intent(in) :: hi
    allocate (flv_state (size (hi%data%flv_state, 1), &
                         size (hi%data%flv_state, 2)))
    flv_state = hi%data%flv_state
  end function hard_interaction_get_flv_states

  pure function hard_interaction_get_n_cf (hi) result (n_cf)
    integer :: n_cf
    type(hard_interaction_t), intent(in) :: hi
    n_cf = hi%data%n_cf
  end function hard_interaction_get_n_cf

  function hard_interaction_get_first_pdg_in (hi) result (pdg)
    integer, dimension(:), allocatable :: pdg
    type(hard_interaction_t), intent(in) :: hi
    allocate (pdg (hi%data%n_in))
    if (hi%data%n_flv > 0) then
       pdg = hi%data%flv_state (:hi%data%n_in, 1)
    else
       pdg = 0
    end if
  end function hard_interaction_get_first_pdg_in

  function hard_interaction_get_first_pdg_out (hi) result (pdg)
    integer, dimension(:), allocatable :: pdg
    type(hard_interaction_t), intent(in) :: hi
    allocate (pdg (hi%data%n_out))
    if (hi%data%n_flv > 0) then
       pdg = hi%data%flv_state (hi%data%n_in+1:hi%data%n_tot, 1)
    else
       pdg = 0
    end if
  end function hard_interaction_get_first_pdg_out

  subroutine hard_interaction_get_unstable_products (hi, flv_unstable)
    type(hard_interaction_t), intent(in) :: hi
    type(flavor_t), dimension(:), intent(out), allocatable :: flv_unstable
    type(model_t), pointer :: model
    integer, dimension(hi%data%n_out) :: pdg_out
    type(flavor_t) :: flv
    integer :: i
    model => hi%data%model
    if (associated (model) .and. size (hi%data%flv_state, 2) /= 0) then
       pdg_out = hi%data%flv_state(hi%data%n_in+1:,1)
       do i = 1, size (pdg_out)
          if (pdg_out(i) /= 0) then
             call flavor_init (flv, pdg_out(i), model)
             if (flavor_is_stable (flv)) then
                where (pdg_out(i:) == pdg_out(i))  pdg_out(i:) = 0
             else
                where (pdg_out(i+1:) == pdg_out(i))  pdg_out(i+1:) = 0
             end if
          end if
       end do
       allocate (flv_unstable (count (pdg_out /= 0)))
       call flavor_init &
            (flv_unstable, pack (pdg_out, pdg_out /= 0), model)
    else
       allocate (flv_unstable (0))
    end if
  end subroutine hard_interaction_get_unstable_products

  subroutine hard_interaction_init_trace &
       (hi, qn_mask_in, use_hi_color_factors, nc)
    type(hard_interaction_t), intent(inout), target :: hi
    type(quantum_numbers_mask_t), dimension(:), intent(in) :: qn_mask_in
    logical, intent(in), optional :: use_hi_color_factors
    integer, intent(in), optional :: nc
    logical :: use_hi_cf
    type(quantum_numbers_mask_t), dimension(:), allocatable :: qn_mask
    if (present (use_hi_color_factors)) then
       use_hi_cf = use_hi_color_factors
    else
       use_hi_cf = .false.
    end if
    allocate (qn_mask (hi%data%n_tot))
    qn_mask(:hi%data%n_in) = &
         new_quantum_numbers_mask (.false., .true., .false.) &
         .or. qn_mask_in
    qn_mask(hi%data%n_in+1:) = &
         new_quantum_numbers_mask (.true., .true., .true.)
    if (use_hi_cf) then
       call evaluator_init_square (hi%eval_trace, hi%int, qn_mask, &
            hi%data%col_flow_index, hi%data%col_factor, hi%col, nc=nc)
    else
       call evaluator_init_square (hi%eval_trace, hi%int, qn_mask, nc=nc)
    end if
  end subroutine hard_interaction_init_trace

  subroutine hard_interaction_init_sqme &
       (hi, qn_mask_in, use_hi_color_factors, nc)
    type(hard_interaction_t), intent(inout), target :: hi
    type(quantum_numbers_mask_t), dimension(:), intent(in) :: qn_mask_in
    logical, intent(in), optional :: use_hi_color_factors
    integer, intent(in), optional :: nc
    logical :: use_hi_cf
    type(quantum_numbers_mask_t), dimension(:), allocatable :: qn_mask
    type(flavor_t), dimension(:), allocatable :: flv
    integer :: i
    logical :: helmask, helmask_hd
    if (present (use_hi_color_factors)) then
       use_hi_cf = use_hi_color_factors
    else
       use_hi_cf = .false.
    end if
    allocate (qn_mask (hi%data%n_tot), flv (hi%data%n_flv))
    qn_mask(:hi%data%n_in) = &
         new_quantum_numbers_mask (.false., .true., .false.) &
         .or. qn_mask_in
    do i = hi%data%n_in + 1, hi%data%n_tot
       call flavor_init (flv, hi%data%flv_state(i,:), hi%data%model)
       if (.not. all (flavor_is_stable (flv))) then
          helmask = all (flavor_decays_isotropically (flv))
          helmask_hd = all (flavor_decays_diagonal (flv))
       else
          helmask = all (.not. flavor_is_polarized (flv))
          helmask_hd = .true.
       end if
       qn_mask(i) = new_quantum_numbers_mask (.false., .true., &
              helmask, mask_hd = helmask_hd)
    end do
    if (use_hi_cf) then
       call evaluator_init_square (hi%eval_sqme, hi%int, qn_mask, &
            hi%data%col_flow_index, hi%data%col_factor, hi%col, nc=nc)
    else
       call evaluator_init_square (hi%eval_sqme, hi%int, qn_mask, nc=nc)
    end if
  end subroutine hard_interaction_init_sqme

  subroutine hard_interaction_init_flows (hi, qn_mask_in)
    type(hard_interaction_t), intent(inout), target :: hi
    type(quantum_numbers_mask_t), dimension(:), intent(in) :: qn_mask_in
    type(quantum_numbers_mask_t), dimension(:), allocatable :: qn_mask
    type(flavor_t), dimension(:), allocatable :: flv
    integer :: i
    logical :: helmask, helmask_hd
    allocate (qn_mask (hi%data%n_tot), flv (hi%data%n_flv))
    qn_mask(:hi%data%n_in) = &
         new_quantum_numbers_mask (.false., .false., .false.) &
         .or. qn_mask_in
    do i = hi%data%n_in + 1, hi%data%n_tot
       call flavor_init (flv, hi%data%flv_state(i,:), hi%data%model)
       if (.not. all (flavor_is_stable (flv))) then
          helmask = all (flavor_decays_isotropically (flv))
          helmask_hd = all (flavor_decays_diagonal (flv))
       else
          helmask = all (.not. flavor_is_polarized (flv))
          helmask_hd = .true.
       end if
       qn_mask(i) = new_quantum_numbers_mask (.false., .false., &
              helmask, mask_hd = helmask_hd)
    end do
    call evaluator_init_square (hi%eval_flows, hi%int, qn_mask, &
         expand_color_flows = .true.)
  end subroutine hard_interaction_init_flows

  subroutine hard_interaction_final_sqme (hi)
    type(hard_interaction_t), intent(inout) :: hi
    call evaluator_final (hi%eval_sqme)
  end subroutine hard_interaction_final_sqme

  subroutine hard_interaction_final_flows (hi)
    type(hard_interaction_t), intent(inout) :: hi
    call evaluator_final (hi%eval_flows)
  end subroutine hard_interaction_final_flows

  subroutine hard_interaction_update_alpha_s (hi, alpha_s)
    type(hard_interaction_t), intent(inout) :: hi
    real(default), intent(in) :: alpha_s
    real(c_default_float) :: c_alpha_s
    c_alpha_s = alpha_s
    call hi%data% update_alpha_s (c_alpha_s)
  end subroutine hard_interaction_update_alpha_s
    
  subroutine hard_interaction_reset_helicity_selection (hi, threshold, cutoff)
    type(hard_interaction_t), intent(inout) :: hi
    real(default), intent(in) :: threshold
    integer, intent(in) :: cutoff
    real(c_default_float) :: c_threshold
    integer(c_int) :: c_cutoff
    c_threshold = threshold
    c_cutoff = cutoff
    call hi%data% reset_helicity_selection (c_threshold, c_cutoff)
  end subroutine hard_interaction_reset_helicity_selection
    
  subroutine hard_interaction_evaluate (hi)
    type(hard_interaction_t), intent(inout), target :: hi
    integer :: i
    complex(default) :: val
    call hi%data% new_event &
         (array_from_vector4 (interaction_get_momenta (hi%int)))
!     forall (i = 1:hi%n_values)
!        hi%me(i) = hi%data% get_amplitude (hi%flv(i), hi%hel(i), hi%col(i))
!     end forall
    do i = 1, hi%n_values
       val = hi%data% get_amplitude (hi%flv(i), hi%hel(i), hi%col(i))
       call interaction_set_matrix_element (hi%int, i, val)
    end do
    call evaluator_evaluate (hi%eval_trace)
  end subroutine hard_interaction_evaluate

  subroutine hard_interaction_evaluate_sqme (hi)
    type(hard_interaction_t), intent(inout), target :: hi
    call evaluator_receive_momenta (hi%eval_sqme)
    call evaluator_evaluate (hi%eval_sqme)
  end subroutine hard_interaction_evaluate_sqme

  subroutine hard_interaction_evaluate_flows (hi)
    type(hard_interaction_t), intent(inout), target :: hi
    call evaluator_receive_momenta (hi%eval_flows)
    call evaluator_evaluate (hi%eval_flows)
  end subroutine hard_interaction_evaluate_flows

  function hard_interaction_compute_sqme_sum (hi, p) result (sqme)
    real(default) :: sqme
    type(hard_interaction_t), intent(inout), target :: hi
    type(vector4_t), dimension(:), intent(in) :: p
    call interaction_set_momenta (hi%int, p)
    call hard_interaction_evaluate (hi)
    sqme = evaluator_sum (hi%eval_trace)
  end function hard_interaction_compute_sqme_sum

  function hard_interaction_get_int_ptr (hi) result (int)
    type(interaction_t), pointer :: int
    type(hard_interaction_t), intent(in), target :: hi
    int => hi%int
  end function hard_interaction_get_int_ptr

  function hard_interaction_get_eval_trace_ptr (hi) result (eval)
    type(evaluator_t), pointer :: eval
    type(hard_interaction_t), intent(in), target :: hi
    eval => hi%eval_trace
  end function hard_interaction_get_eval_trace_ptr

  function hard_interaction_get_eval_sqme_ptr (hi) result (eval)
    type(evaluator_t), pointer :: eval
    type(hard_interaction_t), intent(in), target :: hi
    eval => hi%eval_sqme
  end function hard_interaction_get_eval_sqme_ptr

  function hard_interaction_get_eval_flows_ptr (hi) result (eval)
    type(evaluator_t), pointer :: eval
    type(hard_interaction_t), intent(in), target :: hi
    eval => hi%eval_flows
  end function hard_interaction_get_eval_flows_ptr

  subroutine hard_interaction_recover_kinematics (hi, pset)
    type(hard_interaction_t), intent(inout) :: hi
    type(particle_set_t), intent(in) :: pset
    call particle_set_extract_interaction (pset, hi%int, hi%data%flv_state)
  end subroutine hard_interaction_recover_kinematics

  subroutine hard_interaction_write_state_summary (hi, unit)
    type(hard_interaction_t), intent(in), target :: hi
    integer, intent(in), optional :: unit
    type(state_iterator_t) :: it
    integer :: u, i, f, h, c
    character(1) :: sgn
    u = output_unit (unit)
    call state_iterator_init (it, interaction_get_state_matrix_ptr (hi%int))
    do while (state_iterator_is_valid (it))
       i = state_iterator_get_me_index (it)
       f = hi%flv(i)
       h = hi%hel(i)
       c = hi%col(i)
       if (hi%data% is_allowed (f, h, c)) then
          sgn = "+"
       else
          sgn = " "
       end if
       write (u, "(1x,A1,1x,I0,2x)", advance="no")  sgn, i
       call quantum_numbers_write (state_iterator_get_quantum_numbers (it), u)
       write (u, *)
       call state_iterator_advance (it)
    end do
  end subroutine hard_interaction_write_state_summary

  subroutine hard_interaction_test (model)
    type(model_t), pointer :: model
    type(process_library_t) :: prc_lib
    type(os_data_t) :: os_data
    type(hard_interaction_t), target :: hi
    type(vector4_t), dimension(4) :: p
    type(quantum_numbers_mask_t), dimension(2) :: qn_mask_in
    type(quantum_numbers_mask_t), dimension(4) :: qn_mask
    real(default) :: sqme, mh
    call os_data_init (os_data)
    call msg_message ("*** Load library 'qedtest'")
    call msg_message ("    [must exist and contain process 'eemm' (whizard.sin.qedtest)]")
    call process_library_init (prc_lib, var_str("qedtest"), os_data)
    call process_library_load (prc_lib, os_data)
    call msg_message ()
    call msg_message ("*** Create hard interaction")
    call hard_interaction_init (hi, prc_lib, 1, var_str ("eemm"), model)
    qn_mask_in = new_quantum_numbers_mask (.true., .true., .true.)
    call hard_interaction_init_trace (hi, qn_mask_in)
    print *, "Interaction: n_values = ", interaction_get_n_matrix_elements (hi%int)
    qn_mask_in = new_quantum_numbers_mask (.false., .false., .false., .true.)
    call hard_interaction_init_sqme (hi, qn_mask_in)
    call hard_interaction_init_flows (hi, qn_mask_in)
    p(1) = vector4_moving (250._default, 250._default, 3)
    p(2) = vector4_moving (250._default,-250._default, 3)
    p(3) = rotation (1._default, 1) * p(1)
    p(4) = p(1) + p(2) - p(3)
    call msg_message ()
    call msg_message ("*** Evaluate new event")
    sqme = hard_interaction_compute_sqme_sum (hi, p)
    call hard_interaction_evaluate_sqme (hi)
    call hard_interaction_evaluate_flows (hi)
    call hard_interaction_write (hi)
    print *
    print *, "sqme sum =", sqme
    print *
    print *, "*** Cleanup"
    call hard_interaction_final (hi)
    call process_library_final (prc_lib)
  end subroutine hard_interaction_test


end module hard_interactions
