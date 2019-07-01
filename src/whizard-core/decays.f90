! WHIZARD 2.0.6 Wed Dec 7 2011
! 
! Copyright (C) 1999-2011 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
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

module decays

  use kinds, only: default !NODEP!
  use kinds, only: double !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use limits, only: MAX_TRIES_FOR_DECAY_CHAIN !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use lorentz !NODEP!
  use tao_random_numbers !NODEP!
  use md5
  use models
  use flavors
  use quantum_numbers
  use processes
  use interactions
  use evaluators

  implicit none
  private

  public :: decay_configuration_t
  public :: decay_configuration_set_channel
  public :: decay_configuration_write
  public :: decay_configuration_get_n_channels
  public :: decay_store_final
  public :: decay_store_write
  public :: decay_store_get_md5sum
  public :: decay_store_append_decay
  public :: decay_store_recheck_final_state
  public :: decay_tree_t
  public :: decay_tree_init
  public :: decay_tree_final
  public :: decay_tree_write
  public :: decay_tree_generate_event
  public :: decay_tree_get_eval_sqme_ptr
  public :: decay_tree_get_eval_flows_ptr

  type :: decay_channel_t
     private
     type(process_t), pointer :: process => null ()
     real(default) :: br = 0
     type(flavor_t), dimension(:), allocatable :: unstable_products
     logical, dimension(:), allocatable :: isotropic
     logical, dimension(:), allocatable :: diagonal
  end type decay_channel_t

  type :: decay_configuration_t
     private
     type(flavor_t) :: flv
     type(model_t), pointer :: model => null ()
     real(default) :: width = 0
     logical :: isotropic = .false.
     logical :: diagonal = .false.
     type(decay_channel_t), dimension(:), allocatable :: channel
     type(string_t), dimension(:), allocatable :: process_id
     type(decay_configuration_t), pointer :: next => null ()
  end type decay_configuration_t

  type :: decay_store_t
     private
     integer :: n = 0
     type(decay_configuration_t), pointer :: first => null ()
     type(decay_configuration_t), pointer :: last => null ()
  end type decay_store_t

  type :: decay_t
     private
     logical :: initialized = .false.
     type(process_t), pointer :: process => null ()
     type(evaluator_t) :: eval_sqme
     type(evaluator_t) :: eval_flows
     type(decay_node_t), pointer :: next_node => null ()
  end type decay_t

  type :: decay_node_t
     private
     type(decay_configuration_t), pointer :: configuration => null ()
     integer :: current_channel = 0
     type(decay_t), dimension(:), allocatable :: decay
  end type decay_node_t

  type :: decay_tree_t
     private
     integer :: tries = 0
     real(default) :: acceptance_probability = 0
     type(process_t), pointer :: hard_process => null ()
     type(evaluator_t), pointer :: eval_sqme_in => null ()
     type(evaluator_t), pointer :: eval_flows_in => null ()
     type(decay_node_t), pointer :: root => null ()
     type(evaluator_t), pointer :: eval_sqme => null ()
     type(evaluator_t), pointer :: eval_flows => null ()
  end type decay_tree_t


  type(decay_store_t), save :: store


contains

  subroutine decay_configuration_init &
       (conf, flv, model, width, n_channels, isotropic, diagonal)
    type(decay_configuration_t), intent(out) :: conf
    type(flavor_t), intent(in) :: flv
    type(model_t), intent(in), target :: model
    real(default), intent(in) :: width
    integer, intent(in) :: n_channels
    logical, intent(in) :: isotropic, diagonal
    conf%flv = flv
    conf%model => model
    conf%width = width
    conf%isotropic = isotropic
    conf%diagonal = diagonal
    allocate (conf%channel (n_channels))
    allocate (conf%process_id (n_channels))
  end subroutine decay_configuration_init

  function decay_configuration_get_next_ptr (conf) result (ptr)
    type(decay_configuration_t), pointer :: ptr
    type(decay_configuration_t), intent(in) :: conf
    ptr => conf%next
  end function decay_configuration_get_next_ptr

  subroutine decay_configuration_set_next_ptr (conf, ptr)
    type(decay_configuration_t), intent(inout) :: conf
    type(decay_configuration_t), pointer :: ptr
    conf%next => ptr
  end subroutine decay_configuration_set_next_ptr

  subroutine decay_configuration_set_channel (conf, i, process, br)
    type(decay_configuration_t), intent(inout) :: conf
    integer, intent(in) :: i
    type(process_t), intent(in), target :: process
    real(default), intent(in) :: br
    integer :: n_unstable_products
    conf%channel(i)%process => process
    conf%channel(i)%br = br
    conf%process_id(i) = process_get_id (process)
    call process_get_unstable_products &
         (conf%channel(i)%process, conf%channel(i)%unstable_products)
    n_unstable_products = size (conf%channel(i)%unstable_products)
    if (allocated (conf%channel(i)%isotropic)) &
         deallocate (conf%channel(i)%isotropic)
    if (allocated (conf%channel(i)%diagonal)) &
         deallocate (conf%channel(i)%diagonal)
    allocate (conf%channel(i)%isotropic (n_unstable_products))
    allocate (conf%channel(i)%diagonal (n_unstable_products))
    if (n_unstable_products /= 0) then
       conf%channel(i)%isotropic = &
            flavor_decays_isotropically (conf%channel(i)%unstable_products)
       conf%channel(i)%diagonal = &
            flavor_decays_diagonal (conf%channel(i)%unstable_products)
    end if
  end subroutine decay_configuration_set_channel

  subroutine decay_configuration_recheck_final_state (conf, verbose)
    type(decay_configuration_t), intent(inout) :: conf
    logical, intent(in), optional :: verbose
    type(flavor_t), dimension(:), allocatable :: flv_unstable
    logical, dimension(:), allocatable :: isotropic, diagonal
    logical :: modified, verb
    integer :: u, i, n_unstable_products
    u = logfile_unit ()
    verb = .false.;  if (present (verbose))  verb = verbose
    if (flavor_is_stable (conf%flv))  return
    do i = 1, size (conf%channel)
       call process_get_unstable_products &
          (conf%channel(i)%process, flv_unstable)
       n_unstable_products = size (flv_unstable)
       allocate (isotropic (n_unstable_products))
       allocate (diagonal (n_unstable_products))
       isotropic = flavor_decays_isotropically (flv_unstable)
       diagonal = flavor_decays_diagonal (flv_unstable)
       if (n_unstable_products == size (conf%channel(i)%unstable_products)) &
            then
          modified = &
               any (flv_unstable /= conf%channel(i)%unstable_products) &
               .or. &
               any (isotropic .neqv. conf%channel(i)%isotropic) &
               .or. &
               any (diagonal .neqv. conf%channel(i)%diagonal)
       else
          modified = .true.
          deallocate (conf%channel(i)%unstable_products)
          deallocate (conf%channel(i)%isotropic)
          deallocate (conf%channel(i)%diagonal)
          allocate (conf%channel(i)%unstable_products (n_unstable_products))
          allocate (conf%channel(i)%isotropic (n_unstable_products))
          allocate (conf%channel(i)%diagonal (n_unstable_products))
       end if
       if (modified) then
          conf%channel(i)%unstable_products = flv_unstable
          conf%channel(i)%isotropic = isotropic
          conf%channel(i)%diagonal = diagonal
          call process_setup_event_generation (conf%channel(i)%process, &
               qn_mask_in = new_quantum_numbers_mask (.false., .false., &
                    mask_h =  conf%isotropic, mask_hd = conf%diagonal))
          if (verb) then
             call msg_message ("Further modified decay configuration:")
             call decay_configuration_write (conf)
             call decay_configuration_write (conf, u)
          end if
       end if
       deallocate (flv_unstable, isotropic, diagonal)
    end do
  end subroutine decay_configuration_recheck_final_state

  subroutine decay_configuration_write (conf, unit)
    type(decay_configuration_t), intent(in) :: conf
    integer, intent(in), optional :: unit
    character(12) :: fmt
    integer :: n_channels, proc_id_len
    integer :: u, i, j
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(A)")  "Decay configuration of particle '" &
         // char (flavor_get_name (conf%flv)) // "' in model '" &
         // char (model_get_name (conf%model)) // "':"
    write (u, *) " Computed total width = ", conf%width, " GeV"
    if (conf%isotropic) then
       write (u, *) " Isotropic decays requested for simulation."
    end if
    if (conf%diagonal) then
       write (u, *) " Diagonal density matrix in decays " &
            // "requested for simulation."
    end if
    write (u, *) " Branching ratios:"
    n_channels = decay_configuration_get_n_channels (conf)
    if (n_channels /= 0) then
       proc_id_len = maxval (len (conf%process_id))
       do i = 1, n_channels
          write (u, "(F12.7,1x,A)", advance="no") 100 * conf%channel(i)%br, "%"
          write (fmt, "(2x,A,I0,A)")  "(4x,A", proc_id_len + 1, ")"
          write (u, fmt, advance="no")  char (conf%process_id(i))
          if (allocated (conf%channel(i)%unstable_products)) then
             if (size (conf%channel(i)%unstable_products) /= 0) then
                write (u, "(1x,A)", advance="no") "   -> unstable:"
                do j = 1, size (conf%channel(i)%unstable_products)
                   write (u, "(1x,A)", advance="no")  char (flavor_get_name &
                         (conf%channel(i)%unstable_products(j)))
                   if (conf%channel(i)%isotropic(j)) then
                      write (u, "(A)", advance="no")  "[I]"
                   else if (conf%channel(i)%diagonal(j)) then
                      write (u, "(A)", advance="no")  "[D]"
                   end if
                end do
             end if
          end if
          write (u, *)
       end do
    else
       write (u, *) " [undefined]"
    end if
  end subroutine decay_configuration_write

  function decay_configuration_get_n_channels (conf) result (n)
    integer :: n
    type(decay_configuration_t), intent(in) :: conf
    if (allocated (conf%channel)) then
       n = size (conf%channel)
    else
       n = 0
    end if
  end function decay_configuration_get_n_channels

  function decay_configuration_select_channel (conf, rng) result (channel)
    integer :: channel
    type(decay_configuration_t), intent(in) :: conf
    type(tao_random_state), intent(inout) :: rng
    real(default) :: x
    real(default) :: x_sum
    call tao_random_number (rng, x)
    x_sum = 0
    do channel = 1, size (conf%channel)
       x_sum = x_sum + conf%channel(channel)%br
       if (x < x_sum)  return
    end do
    channel = size (conf%channel)
  end function decay_configuration_select_channel
    
  function decay_configuration_get_process_ptr (conf, channel) &
       result (process)
    type(process_t), pointer :: process
    type(decay_configuration_t), intent(in) :: conf
    integer, intent(in) :: channel
    process => conf%channel(channel)%process
  end function decay_configuration_get_process_ptr

  subroutine decay_store_final ()
    type(decay_configuration_t), pointer :: current
    store%last => null ()
    do while (associated (store%first))
       current => store%first
       store%first => current%next
       deallocate (current)
    end do
    store%n = 0
  end subroutine decay_store_final

  subroutine decay_store_write (unit)
    integer, intent(in), optional :: unit
    type(decay_configuration_t), pointer :: decay
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(A)")  "Decay configuration for unstable particles:"
    decay => store%first
    do while (associated (decay))
       if (.not. flavor_is_stable (decay%flv)) &
            call decay_configuration_write (decay, unit)
       decay => decay%next
    end do
  end subroutine decay_store_write

  function decay_store_get_md5sum () result (md5sum_decays)
    character(32) :: md5sum_decays
    integer :: u
    if (associated (store%first)) then
       u = free_unit ()
       open (u, status="scratch")
       call decay_store_write (u)
       rewind (u)
       md5sum_decays = md5sum (u)
    else
       md5sum_decays = ""
    end if
  end function decay_store_get_md5sum

  subroutine decay_store_append_decay &
      (flv, model, width, n_channels, isotropic, diagonal, decay)
    type(flavor_t), intent(in) :: flv
    type(model_t), intent(in), target :: model
    real(default), intent(in) :: width
    integer, intent(in) :: n_channels
    logical, intent(in) :: isotropic, diagonal
    type(decay_configuration_t), pointer :: decay
    type(decay_configuration_t), pointer :: next_decay
    decay => store%first
    do while (associated (decay))
       if (decay%flv == flv) then
          next_decay => decay_configuration_get_next_ptr (decay)
          call decay_configuration_init &
              (decay, flv, model, width, n_channels, isotropic, diagonal)
          call decay_configuration_set_next_ptr (decay, next_decay)
          return
       end if
       decay => decay%next
    end do
    allocate (decay)
    call decay_configuration_init &
        (decay, flv, model, width, n_channels, isotropic, diagonal)
    if (associated (store%first)) then
       store%last%next => decay
    else
       store%first => decay
    end if
    store%last => decay
  end subroutine decay_store_append_decay

  function decay_store_get_decay_configuration_ptr (flv) result (config)
    type(decay_configuration_t), pointer :: config
    type(flavor_t), intent(in) :: flv
    config => store%first
    SCAN_PARTICLES: do while (associated (config))
       if (config%flv == flv)  exit SCAN_PARTICLES
       config => config%next
    end do SCAN_PARTICLES
  end function decay_store_get_decay_configuration_ptr

  subroutine decay_store_recheck_final_state (verbose)
    logical, intent(in), optional :: verbose
    logical :: modified
    type(decay_configuration_t), pointer :: config
    config => store%first
    do while (associated (config))
       call decay_configuration_recheck_final_state (config, verbose)
       config => config%next
    end do
  end subroutine decay_store_recheck_final_state

  subroutine decay_init (decay, process, eval_sqme, eval_flows, i)
    type(decay_t), intent(out), target :: decay
    type(process_t), intent(inout), target :: process
    type(evaluator_t), intent(in), target :: eval_sqme, eval_flows
    integer, intent(in) :: i
    type(interaction_t), pointer :: prc_int
    type(evaluator_t), pointer :: prc_eval_sqme, prc_eval_flows
    integer :: n_tot
    logical, dimension(:), allocatable :: ignore_hel
    type(quantum_numbers_mask_t), dimension(:), allocatable :: &
         mask_hel, mask_sqme, mask_flows
    type(quantum_numbers_mask_t) :: mask_conn
    call process_request_copy (process, decay%process)
    call process_mark_as_cascade_decay (decay%process)
    call process_setup_cuts (decay%process)
    call process_setup_weight (decay%process)
    call process_setup_scale (decay%process)
    call process_setup_fac_scale (decay%process)
    call process_setup_ren_scale (decay%process)
    prc_int => process_get_hi_int_ptr (decay%process)
    prc_eval_sqme => process_get_hi_eval_sqme_ptr (decay%process)
    prc_eval_flows => process_get_hi_eval_flows_ptr (decay%process)
    n_tot = evaluator_get_n_tot (prc_eval_sqme)
    allocate (ignore_hel (n_tot))
    ignore_hel(1) = .true.
    ignore_hel(2:) = .false.
    allocate (mask_hel (n_tot), mask_sqme (n_tot), mask_flows (n_tot))
    call quantum_numbers_mask_set_helicity (mask_hel, ignore_hel)
    mask_sqme = evaluator_get_mask (prc_eval_sqme) .or. mask_hel
    mask_flows = evaluator_get_mask (prc_eval_flows) .or. mask_hel
    mask_conn = new_quantum_numbers_mask (.false., .false., .true.)
    call evaluator_set_source_link (prc_eval_sqme, 1, eval_sqme, i)
    call evaluator_set_source_link (prc_eval_flows, 1, eval_flows, i)
    call evaluator_init_product (decay%eval_sqme, &
         eval_sqme, prc_eval_sqme, mask_conn, &
         connections_are_resonant=.true.)
    call evaluator_init_product (decay%eval_flows, &
         eval_flows, prc_eval_flows, mask_conn, &
         connections_are_resonant=.true.)
    call evaluator_set_source_link (prc_eval_sqme, 1, prc_int, 1)
    call evaluator_set_source_link (prc_eval_flows, 1, prc_int, 1)
    allocate (decay%next_node)
    decay%initialized = .true.
  end subroutine decay_init

  recursive subroutine decay_final (decay)
    type(decay_t), intent(inout) :: decay
    if (decay%initialized) then
       if (associated (decay%next_node)) &
            call decay_node_final (decay%next_node)
       call evaluator_final (decay%eval_sqme)
       call evaluator_final (decay%eval_flows)
    end if
  end subroutine decay_final

  subroutine decay_write (decay, unit)
    type(decay_t), intent(in) :: decay
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(A)")  repeat ("=", 72)
    write (u, "(A)")  "Decay process:"
    call process_write (decay%process, unit)
    write (u, "(A)")  repeat ("=", 72)
    write (u, "(A)")  "Combined sqme including color factors " &
         // "(process + decay):"
    call evaluator_write (decay%eval_sqme, unit)
    write (u, "(A)")  repeat ("-", 72)
    write (u, "(A)")  "Combined color flow coefficients " &
         // "(process + decay):"
    call evaluator_write (decay%eval_flows, unit)
  end subroutine decay_write

  subroutine decay_generate (decay, rng, flv, p)
    type(decay_t), intent(inout) :: decay
    type(tao_random_state), intent(inout) :: rng
    type(flavor_t), intent(in) :: flv
    type(vector4_t), intent(in) :: p
    real(default) :: excess
    type(evaluator_t), pointer :: process_eval_sqme
    call process_set_beam_momenta (decay%process, (/ p /))
    call process_tag_as_working_copy (decay%process)
    call process_generate_unweighted_event (decay%process, rng, excess=excess)
    process_eval_sqme => process_get_eval_sqme_ptr (decay%process)
    call evaluator_normalize_by_trace (process_eval_sqme)
    call evaluator_receive_momenta (decay%eval_sqme)
    call evaluator_receive_momenta (decay%eval_flows)
    call evaluator_evaluate (decay%eval_sqme)
    call evaluator_evaluate (decay%eval_flows)
  end subroutine decay_generate

  subroutine decay_node_init (node, flv)
    type(decay_node_t), intent(out) :: node
    type(flavor_t), intent(in) :: flv
    node%configuration => decay_store_get_decay_configuration_ptr (flv)
    if (associated (node%configuration)) then
       allocate (node%decay &
            (decay_configuration_get_n_channels (node%configuration)))
    else
       call msg_bug ("Particle '" // char (flavor_get_name (flv)) &
            // "': Missing decay configuration")
    end if
  end subroutine decay_node_init

  recursive subroutine decay_node_final (node)
    type(decay_node_t), intent(inout) :: node
    integer :: i
    if (allocated (node%decay)) then
       do i = 1, size (node%decay)
          call decay_final (node%decay(i))
       end do
       deallocate (node%decay)
    end if
  end subroutine decay_node_final
    
  subroutine decay_node_write (node, unit)
    type(decay_node_t), intent(in) :: node
    integer, intent(in), optional :: unit
    integer :: channel, u
    u = output_unit (unit)
    write (u, "(A)")  "|" // repeat ("=", 79)
    if (associated (node%configuration)) then
       call decay_configuration_write (node%configuration, unit)
       channel = node%current_channel
       if (channel /= 0) then
          write (u, "(1x,A)", advance="no")  "Decay node: "
          write (u, *) "current channel = ", channel
          call decay_write (node%decay(channel), unit)
       else
          write (u, *)  "Decay node: [no channel selected]"
       end if
    else
       write (u, *)  "Decay configuration: [undefined]"
    end if
  end subroutine decay_node_write

  function decay_node_get_next_ptr (node) result (ptr)
    type(decay_node_t), pointer :: ptr
    type(decay_node_t), intent(in) :: node
    if (node%current_channel /= 0) then
       ptr => node%decay(node%current_channel)%next_node
    else
       ptr => null ()
    end if
  end function decay_node_get_next_ptr

  subroutine decay_tree_init (decay_tree, process)
    type(decay_tree_t), intent(out) :: decay_tree
    type(process_t), intent(in), target :: process
    decay_tree%hard_process => process
    decay_tree%eval_sqme_in => process_get_eval_sqme_ptr (process)
    decay_tree%eval_flows_in => process_get_eval_flows_ptr (process)
    allocate (decay_tree%root)
  end subroutine decay_tree_init

  subroutine decay_tree_final (decay_tree)
    type(decay_tree_t), intent(inout) :: decay_tree
    if (associated (decay_tree%root)) then
       call decay_node_final (decay_tree%root)
       deallocate (decay_tree%root)
    end if
  end subroutine decay_tree_final
    
  subroutine decay_tree_write (decay_tree, unit)
    type(decay_tree_t), intent(in) :: decay_tree
    integer, intent(in), optional :: unit
    type(decay_node_t), pointer :: decay_node
    integer :: u
    u = output_unit (unit)
    write (u, "(A)") "|" // repeat ("=", 79)
    write (u, *) "Decay tree:"
    write (u, *) "  tries = ", decay_tree%tries
    write (u, *) "  acceptance probability = ", &
          decay_tree%acceptance_probability
    write (u, "(A)") "|" // repeat ("=", 79)
    write (u, "(1x,A)", advance="no") "Mother process = "
    if (associated (decay_tree%hard_process)) then
       write (u, "(A)")  "'" &
           // char (process_get_id (decay_tree%hard_process)) &
           // "'"
    else
       write (u, "(A)")  "[undefined]"
    end if
    write (u, "(A)") "|" // repeat ("=", 79)
    decay_node => decay_tree%root
    if (associated (decay_node)) then
       write (u, *) "Decay chain:"
       do while (associated (decay_node))   
          call decay_node_write (decay_node, unit)
          decay_node => decay_node_get_next_ptr (decay_node)
       end do
    else
       write (u, *) "[No decays]"
    end if
    write (u, "(A)") "|" // repeat ("=", 79)
    write (u, "(1x,A)") "Evaluator: " &
          // "Color-summed including all decays"
    if (associated (decay_tree%eval_sqme)) then
       call evaluator_write (decay_tree%eval_sqme, unit)
    else
       write (u, "(A)")  "[undefined]"
    end if
    write (u, "(A)") "|" // repeat ("=", 79)
    write (u, "(1x,A)") "Evaluator: " &
          // "Color flow components including all decays"
    if (associated (decay_tree%eval_flows)) then
       call evaluator_write (decay_tree%eval_flows, unit)
    else
       write (u, "(A)")  "[undefined]"
    end if
    write (u, "(A)") "|" // repeat ("=", 79)
  end subroutine decay_tree_write

  subroutine decay_tree_generate_event (decay_tree, rng)
    type(decay_tree_t), intent(inout) :: decay_tree
    type(tao_random_state), intent(inout) :: rng
    real(default) :: x_decay
    real(default) :: x
    integer :: i
    logical :: decay_occurs
    call evaluator_normalize_by_max (decay_tree%eval_sqme_in)
    decay_occurs = .false.
    REJECTION: do i = 1, MAX_TRIES_FOR_DECAY_CHAIN
       decay_tree%tries = i
       decay_tree%eval_sqme => decay_tree%eval_sqme_in
       decay_tree%eval_flows => decay_tree%eval_flows_in
       call decay_node_generate_event (decay_tree%root, decay_occurs)
       if (decay_occurs) then
          x_decay = evaluator_sum (decay_tree%eval_sqme)
          decay_tree%acceptance_probability = x_decay
          call tao_random_number (rng, x)
          if (x <= x_decay)  return
       else
          return
       end if
    end do REJECTION
    write (msg_buffer, "(A,I0,A)") "Failed to generate a decay chain " &
         // "after ", MAX_TRIES_FOR_DECAY_CHAIN, " tries"
    call msg_fatal ()
  contains
    recursive subroutine decay_node_generate_event (node, decay_occurs)
      type(decay_node_t), intent(inout), target :: node
      logical, intent(inout) :: decay_occurs
      type(flavor_t) :: flv
      type(vector4_t) :: p
      integer :: i, channel
      type(process_t), pointer :: process
      call evaluator_get_unstable_particle (decay_tree%eval_sqme, flv, p, i)
      if (flavor_is_defined (flv)) then
         decay_occurs = .true.
         if (.not. associated (node%configuration)) &
              call decay_node_init (node, flv)
         channel = decay_configuration_select_channel (node%configuration, rng)
         node%current_channel = channel
         if (.not. node%decay(channel)%initialized) then
            process => decay_configuration_get_process_ptr &
                 (node%configuration, channel)
            call decay_init (node%decay(channel), &
                 process, decay_tree%eval_sqme, decay_tree%eval_flows, i)
         end if
         call decay_generate (node%decay(channel), rng, flv, p)
         decay_tree%eval_sqme => node%decay(channel)%eval_sqme
         decay_tree%eval_flows => node%decay(channel)%eval_flows
         call decay_node_generate_event &
              (node%decay(channel)%next_node, decay_occurs)
      end if
    end subroutine decay_node_generate_event
  end subroutine decay_tree_generate_event

  function decay_tree_get_eval_sqme_ptr (decay_tree) result (eval)
    type(evaluator_t), pointer :: eval
    type(decay_tree_t), intent(in), target :: decay_tree
    eval => decay_tree%eval_sqme
  end function decay_tree_get_eval_sqme_ptr

  function decay_tree_get_eval_flows_ptr (decay_tree) result (eval)
    type(evaluator_t), pointer :: eval 
    type(decay_tree_t), intent(in), target :: decay_tree
    eval => decay_tree%eval_flows
  end function decay_tree_get_eval_flows_ptr


end module decays
