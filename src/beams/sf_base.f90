! WHIZARD 2.2.3 Nov 30 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

module sf_base

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_utils, only: write_separator
  use format_defs, only: FMT_17, FMT_19
  use unit_tests
  use diagnostics
  use lorentz
  use model_data
  use flavors
  use helicities
  use quantum_numbers
  use state_matrices
  use interactions
  use evaluators
  use pdg_arrays
  use particles
  use beams
  use sf_aux
  use sf_mappings

  implicit none
  private

  public :: sf_data_t
  public :: sf_config_t
  public :: sf_int_t
  public :: sf_chain_t
  public :: sf_chain_instance_t
  public :: sf_base_test
  public :: sf_test_data_t

  integer, parameter, public :: SF_UNDEFINED = 0
  integer, parameter, public :: SF_INITIAL = 1
  integer, parameter, public :: SF_DONE_LINKS = 2
  integer, parameter, public :: SF_FAILED_MASK = 3
  integer, parameter, public :: SF_DONE_MASK = 4
  integer, parameter, public :: SF_FAILED_CONNECTIONS = 5
  integer, parameter, public :: SF_DONE_CONNECTIONS = 6
  integer, parameter, public :: SF_SEED_KINEMATICS = 10
  integer, parameter, public :: SF_FAILED_KINEMATICS = 11
  integer, parameter, public :: SF_DONE_KINEMATICS = 12
  integer, parameter, public :: SF_FAILED_EVALUATION = 13
  integer, parameter, public :: SF_EVALUATED = 20


  type, abstract :: sf_data_t
   contains
     procedure (sf_data_write), deferred :: write
     procedure :: is_generator => sf_data_is_generator
     procedure (sf_data_get_int), deferred :: get_n_par
     procedure (sf_data_get_pdg_out), deferred :: get_pdg_out
     procedure (sf_data_allocate_sf_int), deferred :: allocate_sf_int
     procedure :: get_pdf_set => sf_data_get_pdf_set
  end type sf_data_t

  type :: sf_config_t
     integer, dimension(:), allocatable :: i
     class(sf_data_t), allocatable :: data
   contains
     procedure :: write => sf_config_write
     procedure :: init => sf_config_init
     procedure :: get_pdf_set => sf_config_get_pdf_set
  end type sf_config_t
  
  type, abstract, extends (interaction_t) :: sf_int_t
     integer :: status = SF_UNDEFINED
     real(default), dimension(:), allocatable :: mi2
     real(default), dimension(:), allocatable :: mr2
     real(default), dimension(:), allocatable :: mo2
     integer :: on_shell_mode = KEEP_ENERGY
     logical :: qmin_defined = .false.
     logical :: qmax_defined = .false.
     real(default), dimension(:), allocatable :: qmin
     real(default), dimension(:), allocatable :: qmax
     integer, dimension(:), allocatable :: beam_index
     integer, dimension(:), allocatable :: incoming
     integer, dimension(:), allocatable :: radiated
     integer, dimension(:), allocatable :: outgoing
     integer, dimension(:), allocatable :: par_index
     integer, dimension(:), allocatable :: par_primary
   contains
     procedure :: final => sf_int_final
     procedure :: base_write => sf_int_base_write
     procedure (sf_int_type_string), deferred :: type_string
     procedure (sf_int_write), deferred :: write
     procedure :: base_init => sf_int_base_init
     procedure :: set_incoming => sf_int_set_incoming
     procedure :: set_radiated => sf_int_set_radiated
     procedure :: set_outgoing => sf_int_set_outgoing
     procedure (sf_int_init), deferred :: init
     procedure :: setup_constants => sf_int_setup_constants
     procedure :: set_beam_index => sf_int_set_beam_index
     procedure :: set_par_index => sf_int_set_par_index
     generic :: seed_kinematics => sf_int_receive_momenta
     generic :: seed_kinematics => sf_int_seed_momenta
     generic :: seed_kinematics => sf_int_seed_energies
     procedure :: sf_int_receive_momenta
     procedure :: sf_int_seed_momenta
     procedure :: sf_int_seed_energies
     procedure :: is_generator => sf_int_is_generator
     procedure :: generate_free => sf_int_generate_free
     procedure (sf_int_complete_kinematics), deferred :: complete_kinematics
     procedure (sf_int_inverse_kinematics), deferred :: inverse_kinematics
     procedure :: split_momentum => sf_int_split_momentum
     procedure :: split_momenta => sf_int_split_momenta
     procedure :: reduce_momenta => sf_int_reduce_momenta
     procedure :: recover_x => sf_int_recover_x
     procedure :: base_recover_x => sf_int_recover_x
     procedure (sf_int_apply), deferred :: apply
     procedure :: get_n_in => sf_int_get_n_in
     procedure :: get_n_rad => sf_int_get_n_rad
     procedure :: get_n_out => sf_int_get_n_out
     procedure :: get_n_states => sf_int_get_n_states
     procedure :: get_state => sf_int_get_state
     procedure :: get_values => sf_int_get_values
     procedure :: compute_values => sf_int_compute_values
     procedure :: compute_value => sf_int_compute_value
  end type sf_int_t
  
  type :: sf_instance_t
     class(sf_int_t), allocatable :: int
     type(evaluator_t) :: eval
     real(default), dimension(:,:), allocatable :: r
     real(default), dimension(:,:), allocatable :: rb
     real(default), dimension(:), allocatable :: f
     logical, dimension(:), allocatable :: m
     real(default), dimension(:), allocatable :: x
  end type sf_instance_t
  
  type, extends (beam_t) :: sf_chain_t
     type(beam_data_t), pointer :: beam_data => null ()
     integer :: n_in = 0
     integer :: n_strfun = 0
     integer :: n_par = 0
     integer :: n_bound = 0
     type(sf_instance_t), dimension(:), allocatable :: sf
     logical :: trace_enable = .false.
     integer :: trace_unit = 0
   contains
     procedure :: final => sf_chain_final
     procedure :: write => sf_chain_write
     procedure :: init => sf_chain_init
     procedure :: receive_beam_momenta => sf_chain_receive_beam_momenta
     procedure :: set_beam_momenta => sf_chain_set_beam_momenta
     procedure :: set_strfun => sf_chain_set_strfun
     procedure :: get_n_par => sf_chain_get_n_par
     procedure :: get_n_bound => sf_chain_get_n_bound
     procedure :: get_beam_int_ptr => sf_chain_get_beam_int_ptr
     procedure :: setup_tracing => sf_chain_setup_tracing
     procedure :: final_tracing => sf_chain_final_tracing
     procedure :: write_trace_header => sf_chain_write_trace_header
     procedure :: trace => sf_chain_trace
  end type sf_chain_t
     
  type, extends (beam_t) :: sf_chain_instance_t
     type(sf_chain_t), pointer :: config => null ()
     integer :: status = SF_UNDEFINED
     type(sf_instance_t), dimension(:), allocatable :: sf
     integer, dimension(:), allocatable :: out_sf
     integer, dimension(:), allocatable :: out_sf_i
     integer :: out_eval = 0
     integer, dimension(:), allocatable :: out_eval_i
     integer :: selected_channel = 0
     real(default), dimension(:,:), allocatable :: p, pb
     real(default), dimension(:,:), allocatable :: r, rb
     real(default), dimension(:), allocatable :: f
     real(default), dimension(:), allocatable :: x
     logical, dimension(:), allocatable :: bound
     real(default) :: x_free = 1
     type(sf_channel_t), dimension(:), allocatable :: channel
   contains
     procedure :: final => sf_chain_instance_final
     procedure :: write => sf_chain_instance_write
     procedure :: init => sf_chain_instance_init
     procedure :: select_channel => sf_chain_instance_select_channel
     procedure :: set_channel => sf_chain_instance_set_channel
     procedure :: link_interactions => sf_chain_instance_link_interactions
     procedure :: exchange_mask => sf_chain_exchange_mask
     procedure :: init_evaluators => sf_chain_instance_init_evaluators
     procedure :: compute_kinematics => sf_chain_instance_compute_kinematics
     procedure :: inverse_kinematics => sf_chain_instance_inverse_kinematics
     procedure :: recover_kinematics => sf_chain_instance_recover_kinematics
     procedure :: return_beam_momenta => sf_chain_instance_return_beam_momenta
     procedure :: evaluate => sf_chain_instance_evaluate
     procedure :: get_out_momenta => sf_chain_instance_get_out_momenta
     procedure :: get_out_int_ptr => sf_chain_instance_get_out_int_ptr
     procedure :: get_out_i => sf_chain_instance_get_out_i
     procedure :: get_out_mask => sf_chain_instance_get_out_mask
     procedure :: get_mcpar => sf_chain_instance_get_mcpar
     procedure :: get_f => sf_chain_instance_get_f
     procedure :: get_status => sf_chain_instance_get_status
  end type sf_chain_instance_t
     

  abstract interface
     subroutine sf_data_write (data, unit, verbose)
       import
       class(sf_data_t), intent(in) :: data
       integer, intent(in), optional :: unit
       logical, intent(in), optional :: verbose
     end subroutine sf_data_write
  end interface
  
  abstract interface
     function sf_data_get_int (data) result (n)
       import
       class(sf_data_t), intent(in) :: data
       integer :: n
     end function sf_data_get_int
  end interface

  abstract interface
     subroutine sf_data_get_pdg_out (data, pdg_out)
       import
       class(sf_data_t), intent(in) :: data
       type(pdg_array_t), dimension(:), intent(inout) :: pdg_out
     end subroutine sf_data_get_pdg_out
  end interface
  
  abstract interface
     subroutine sf_data_allocate_sf_int (data, sf_int)
       import
       class(sf_data_t), intent(in) :: data
       class(sf_int_t), intent(inout), allocatable :: sf_int
     end subroutine sf_data_allocate_sf_int
  end interface

  abstract interface
     function sf_int_type_string (object) result (string)
       import
       class(sf_int_t), intent(in) :: object
       type(string_t) :: string
     end function sf_int_type_string
  end interface
  
  abstract interface
     subroutine sf_int_write (object, unit, testflag)
       import
       class(sf_int_t), intent(in) :: object
       integer, intent(in), optional :: unit
       logical, intent(in), optional :: testflag
     end subroutine sf_int_write
  end interface
  
  abstract interface
     subroutine sf_int_init (sf_int, data)
       import
       class(sf_int_t), intent(out) :: sf_int
       class(sf_data_t), intent(in), target :: data
     end subroutine sf_int_init
  end interface
     
  abstract interface
     subroutine sf_int_complete_kinematics (sf_int, x, f, r, rb, map)
       import
       class(sf_int_t), intent(inout) :: sf_int
       real(default), dimension(:), intent(out) :: x
       real(default), intent(out) :: f
       real(default), dimension(:), intent(in) :: r
       real(default), dimension(:), intent(in) :: rb
       logical, intent(in) :: map
     end subroutine sf_int_complete_kinematics
  end interface
  
  abstract interface
     subroutine sf_int_inverse_kinematics (sf_int, x, f, r, rb, map, &
          set_momenta)
       import
       class(sf_int_t), intent(inout) :: sf_int
       real(default), dimension(:), intent(in) :: x
       real(default), intent(out) :: f
       real(default), dimension(:), intent(out) :: r
       real(default), dimension(:), intent(out) :: rb
       logical, intent(in) :: map
       logical, intent(in), optional :: set_momenta
     end subroutine sf_int_inverse_kinematics
  end interface
  
  abstract interface
     subroutine sf_int_apply (sf_int, scale)
       import
       class(sf_int_t), intent(inout) :: sf_int
       real(default), intent(in) :: scale
     end subroutine sf_int_apply
  end interface


  type, extends (sf_data_t) :: sf_test_data_t
     class(model_data_t), pointer :: model => null ()
     integer :: mode = 0
     type(flavor_t) :: flv_in
     type(flavor_t) :: flv_out
     type(flavor_t) :: flv_rad
     real(default) :: m = 0
     logical :: collinear = .true.
     real(default), dimension(:), allocatable :: qbounds
   contains
     procedure :: write => sf_test_data_write
     procedure :: init => sf_test_data_init
     procedure :: get_n_par => sf_test_data_get_n_par
     procedure :: get_pdg_out => sf_test_data_get_pdg_out
     procedure :: allocate_sf_int => sf_test_data_allocate_sf_int
  end type sf_test_data_t
  
  type, extends (sf_int_t) :: sf_test_t
     type(sf_test_data_t), pointer :: data => null ()
     real(default) :: x = 0
   contains
     procedure :: type_string => sf_test_type_string
     procedure :: write => sf_test_write
     procedure :: init => sf_test_init
     procedure :: complete_kinematics => sf_test_complete_kinematics
     procedure :: inverse_kinematics => sf_test_inverse_kinematics
     procedure :: apply => sf_test_apply
  end type sf_test_t
  
  type, extends (sf_data_t) :: sf_test_spectrum_data_t
     class(model_data_t), pointer :: model => null ()
     type(flavor_t) :: flv_in
     type(flavor_t) :: flv_out
     type(flavor_t) :: flv_rad
     logical :: with_radiation = .true.
     real(default) :: m = 0
   contains
     procedure :: write => sf_test_spectrum_data_write
     procedure :: init => sf_test_spectrum_data_init
     procedure :: get_n_par => sf_test_spectrum_data_get_n_par
     procedure :: get_pdg_out => sf_test_spectrum_data_get_pdg_out
     procedure :: allocate_sf_int => &
          sf_test_spectrum_data_allocate_sf_int
  end type sf_test_spectrum_data_t
  
  type, extends (sf_int_t) :: sf_test_spectrum_t
     type(sf_test_spectrum_data_t), pointer :: data => null ()
   contains
     procedure :: type_string => sf_test_spectrum_type_string
     procedure :: write => sf_test_spectrum_write
     procedure :: init => sf_test_spectrum_init
     procedure :: complete_kinematics => sf_test_spectrum_complete_kinematics
     procedure :: inverse_kinematics => sf_test_spectrum_inverse_kinematics
     procedure :: apply => sf_test_spectrum_apply
  end type sf_test_spectrum_t
  
  type, extends (sf_data_t) :: sf_test_generator_data_t
     class(model_data_t), pointer :: model => null ()
     type(flavor_t) :: flv_in
     type(flavor_t) :: flv_out
     type(flavor_t) :: flv_rad
     real(default) :: m = 0
   contains
     procedure :: write => sf_test_generator_data_write
     procedure :: init => sf_test_generator_data_init
     procedure :: is_generator => sf_test_generator_data_is_generator
     procedure :: get_n_par => sf_test_generator_data_get_n_par
     procedure :: get_pdg_out => sf_test_generator_data_get_pdg_out
     procedure :: allocate_sf_int => &
          sf_test_generator_data_allocate_sf_int
  end type sf_test_generator_data_t
  
  type, extends (sf_int_t) :: sf_test_generator_t
     type(sf_test_generator_data_t), pointer :: data => null ()
   contains
     procedure :: type_string => sf_test_generator_type_string
     procedure :: write => sf_test_generator_write
     procedure :: init => sf_test_generator_init
     procedure :: is_generator => sf_test_generator_is_generator
     procedure :: generate_free => sf_test_generator_generate_free
     procedure :: recover_x => sf_test_generator_recover_x
     procedure :: complete_kinematics => sf_test_generator_complete_kinematics
     procedure :: inverse_kinematics => sf_test_generator_inverse_kinematics
     procedure :: apply => sf_test_generator_apply
  end type sf_test_generator_t
  

contains

  function sf_data_is_generator (data) result (flag)
    class(sf_data_t), intent(in) :: data
    logical :: flag
    flag = .false.
  end function sf_data_is_generator
  
  function sf_data_get_pdf_set (data) result (pdf_set)
    class(sf_data_t), intent(in) :: data
    integer :: pdf_set
    pdf_set = 0
  end function sf_data_get_pdf_set
  
  subroutine sf_config_write (object, unit)
    class(sf_config_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    if (allocated (object%i)) then
       write (u, "(1x,A,2(1x,I0))")  "Structure-function configuration: &
            &beam(s)", object%i
       if (allocated (object%data))  call object%data%write (u)
    else
       write (u, "(1x,A)")  "Structure-function configuration: [undefined]"
    end if
  end subroutine sf_config_write
       
  subroutine sf_config_init (sf_config, i_beam, sf_data)
    class(sf_config_t), intent(out) :: sf_config
    integer, dimension(:), intent(in) :: i_beam
    class(sf_data_t), intent(in) :: sf_data
    allocate (sf_config%i (size (i_beam)), source = i_beam)
    allocate (sf_config%data, source = sf_data)
  end subroutine sf_config_init
  
  function sf_config_get_pdf_set (sf_config) result (pdf_set)
    class(sf_config_t), intent(in) :: sf_config
    integer :: pdf_set
    pdf_set = sf_config%data%get_pdf_set ()
  end function sf_config_get_pdf_set
  
  subroutine write_sf_status (status, u)
    integer, intent(in) :: status
    integer, intent(in) :: u
    select case (status)
    case (SF_UNDEFINED)
       write (u, "(1x,'[',A,']')")  "undefined"
    case (SF_INITIAL)
       write (u, "(1x,'[',A,']')")  "initialized"
    case (SF_DONE_LINKS)
       write (u, "(1x,'[',A,']')")  "links set"
    case (SF_FAILED_MASK)
       write (u, "(1x,'[',A,']')")  "mask mismatch"
    case (SF_DONE_MASK)
       write (u, "(1x,'[',A,']')")  "mask set"
    case (SF_FAILED_CONNECTIONS)
       write (u, "(1x,'[',A,']')")  "connections failed"
    case (SF_DONE_CONNECTIONS)
       write (u, "(1x,'[',A,']')")  "connections set"
    case (SF_SEED_KINEMATICS)
       write (u, "(1x,'[',A,']')")  "incoming momenta set"
    case (SF_FAILED_KINEMATICS)
       write (u, "(1x,'[',A,']')")  "kinematics failed"
    case (SF_DONE_KINEMATICS)
       write (u, "(1x,'[',A,']')")  "kinematics set"
    case (SF_FAILED_EVALUATION)
       write (u, "(1x,'[',A,']')")  "evaluation failed"
    case (SF_EVALUATED)
       write (u, "(1x,'[',A,']')")  "evaluated"
    end select
  end subroutine write_sf_status

  subroutine sf_int_final (object)
    class(sf_int_t), intent(inout) :: object
    call interaction_final (object%interaction_t)
  end subroutine sf_int_final

  subroutine sf_int_base_write (object, unit, testflag)
    class(sf_int_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)", advance="no")  "SF instance:"
    call write_sf_status (object%status, u)
    if (allocated (object%beam_index)) &
         write (u, "(3x,A,2(1x,I0))")  "beam      =", object%beam_index
    if (allocated (object%incoming)) &
         write (u, "(3x,A,2(1x,I0))")  "incoming  =", object%incoming
    if (allocated (object%radiated)) &
         write (u, "(3x,A,2(1x,I0))")  "radiated  =", object%radiated
    if (allocated (object%outgoing)) &
         write (u, "(3x,A,2(1x,I0))")  "outgoing  =", object%outgoing
    if (allocated (object%par_index)) &
         write (u, "(3x,A,2(1x,I0))")  "parameter =", object%par_index
    if (object%qmin_defined) &
         write (u, "(3x,A,1x," // FMT_19 // ")")  "q_min     =", object%qmin
    if (object%qmax_defined) &
         write (u, "(3x,A,1x," // FMT_19 // ")")  "q_max     =", object%qmax
    call interaction_write (object%interaction_t, u, testflag = testflag)
  end subroutine sf_int_base_write
  
  subroutine sf_int_base_init &
       (sf_int, mask, mi2, mr2, mo2, qmin, qmax, hel_lock)
    class(sf_int_t), intent(out) :: sf_int
    type (quantum_numbers_mask_t), dimension(:), intent(in) :: mask
    real(default), dimension(:), intent(in) :: mi2, mr2, mo2
    real(default), dimension(:), intent(in), optional :: qmin, qmax
    integer, dimension(:), intent(in), optional :: hel_lock
    allocate (sf_int%mi2 (size (mi2)))
    sf_int%mi2 = mi2
    allocate (sf_int%mr2 (size (mr2)))
    sf_int%mr2 = mr2
    allocate (sf_int%mo2 (size (mo2)))
    sf_int%mo2 = mo2
    if (present (qmin)) then
       sf_int%qmin_defined = .true.
       allocate (sf_int%qmin (size (qmin)))
       sf_int%qmin = qmin
    end if
    if (present (qmax)) then
       sf_int%qmax_defined = .true.
       allocate (sf_int%qmax (size (qmax)))
       sf_int%qmax = qmax
    end if
    call interaction_init (sf_int%interaction_t, &
         size (mi2), 0, size (mr2) + size (mo2), &
         mask = mask, hel_lock = hel_lock, set_relations = .true.)
  end subroutine sf_int_base_init
    
  subroutine sf_int_set_incoming (sf_int, incoming)
    class(sf_int_t), intent(inout) :: sf_int
    integer, dimension(:), intent(in) :: incoming
    allocate (sf_int%incoming (size (incoming)))
    sf_int%incoming = incoming
  end subroutine sf_int_set_incoming

  subroutine sf_int_set_radiated (sf_int, radiated)
    class(sf_int_t), intent(inout) :: sf_int
    integer, dimension(:), intent(in) :: radiated
    allocate (sf_int%radiated (size (radiated)))
    sf_int%radiated = radiated
  end subroutine sf_int_set_radiated

  subroutine sf_int_set_outgoing (sf_int, outgoing)
    class(sf_int_t), intent(inout) :: sf_int
    integer, dimension(:), intent(in) :: outgoing
    allocate (sf_int%outgoing (size (outgoing)))
    sf_int%outgoing = outgoing
  end subroutine sf_int_set_outgoing

  subroutine sf_int_setup_constants (sf_int)
    class(sf_int_t), intent(inout) :: sf_int
  end subroutine sf_int_setup_constants
  
  subroutine sf_int_set_beam_index (sf_int, beam_index)
    class(sf_int_t), intent(inout) :: sf_int
    integer, dimension(:), intent(in) :: beam_index
    allocate (sf_int%beam_index (size (beam_index)))
    sf_int%beam_index = beam_index
  end subroutine sf_int_set_beam_index

  subroutine sf_int_set_par_index (sf_int, par_index)
    class(sf_int_t), intent(inout) :: sf_int
    integer, dimension(:), intent(in) :: par_index
    allocate (sf_int%par_index (size (par_index)))
    sf_int%par_index = par_index
  end subroutine sf_int_set_par_index

  subroutine sf_int_receive_momenta (sf_int)
    class(sf_int_t), intent(inout) :: sf_int
    if (sf_int%status >= SF_INITIAL) then
       call interaction_receive_momenta (sf_int%interaction_t)
       sf_int%status = SF_SEED_KINEMATICS
    end if
  end subroutine sf_int_receive_momenta

  subroutine sf_int_seed_momenta (sf_int, k)
    class(sf_int_t), intent(inout) :: sf_int
    type(vector4_t), dimension(:), intent(in) :: k
    if (sf_int%status >= SF_INITIAL) then
       call interaction_set_momenta (sf_int%interaction_t, k, &
            outgoing=.false.)
       sf_int%status = SF_SEED_KINEMATICS
    end if
  end subroutine sf_int_seed_momenta
  
  subroutine sf_int_seed_energies (sf_int, E)
    class(sf_int_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(in) :: E
    type(vector4_t), dimension(:), allocatable :: k
    integer :: j
    if (sf_int%status >= SF_INITIAL) then
       allocate (k (size (E)))
       if (all (E**2 >= sf_int%mi2)) then
          do j = 1, size (E)
             k(j) = vector4_moving (E(j), &
                  (3-2*j) * sqrt (E(j)**2 - sf_int%mi2(j)), 3)
          end do
          call sf_int%seed_kinematics (k)
       end if
    end if
  end subroutine sf_int_seed_energies
  
  function sf_int_is_generator (sf_int) result (flag)
    class(sf_int_t), intent(in) :: sf_int
    logical :: flag
    flag = .false.
  end function sf_int_is_generator

  subroutine sf_int_generate_free (sf_int, r, rb,  x_free)
    class(sf_int_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: r, rb
    real(default), intent(inout) :: x_free
    r = 0
    rb= 1
  end subroutine sf_int_generate_free
    
  subroutine sf_int_split_momentum (sf_int, x, xb1)
    class(sf_int_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(in) :: x
    real(default), intent(in) :: xb1
    type(vector4_t) :: k
    type(vector4_t), dimension(2) :: q
    type(splitting_data_t) :: sd
    real(default) :: E1, E2
    logical :: fail
    if (sf_int%status >= SF_SEED_KINEMATICS) then
       k = interaction_get_momentum (sf_int%interaction_t, 1)
       call sd%init (k, &
            sf_int%mi2(1), sf_int%mr2(1), sf_int%mo2(1), &
            collinear = size (x) == 1)
       call sd%set_t_bounds (x(1), xb1)
       select case (size (x))
       case (1)
       case (3)
          if (sf_int%qmax_defined) then
             if (sf_int%qmin_defined) then
                call sd%sample_t (x(2), &
                     t0 = - sf_int%qmax(1) ** 2, t1 = - sf_int%qmin(1) ** 2)
             else
                call sd%sample_t (x(2), &
                     t0 = - sf_int%qmax(1) ** 2)
             end if
          else
             if (sf_int%qmin_defined) then
                call sd%sample_t (x(2), t1 = - sf_int%qmin(1) ** 2)
             else
                call sd%sample_t (x(2))
             end if
          end if
          call sd%sample_phi (x(3))
       case default
          call msg_bug ("Structure function: impossible number of parameters")
       end select
       q = sd%split_momentum (k)
       call on_shell (q, [sf_int%mr2, sf_int%mo2], &
            sf_int%on_shell_mode)
       call interaction_set_momenta (sf_int%interaction_t, &
            q, outgoing=.true.)
       E1 = energy (q(1))
       E2 = energy (q(2))
       fail = E1 < 0 .or. E2 < 0 &
            .or. E1 ** 2 < sf_int%mr2(1) &
            .or. E2 ** 2 < sf_int%mo2(1)
       if (fail) then
          sf_int%status = SF_FAILED_KINEMATICS
       else
          sf_int%status = SF_DONE_KINEMATICS
       end if
    end if
  end subroutine sf_int_split_momentum
    
  subroutine sf_int_split_momenta (sf_int, x, xb1)
    class(sf_int_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(in) :: x
    real(default), dimension(:), intent(in) :: xb1
    type(vector4_t), dimension(2) :: k
    type(vector4_t), dimension(4) :: q
    real(default), dimension(4) :: E
    logical :: fail
    if (sf_int%status >= SF_SEED_KINEMATICS) then
       select case (size (x))
       case (2)
       case default
          call msg_bug ("Pair structure function: recoil requested &
               &but not implemented yet")
       end select
       k(1) = interaction_get_momentum (sf_int%interaction_t, 1)
       k(2) = interaction_get_momentum (sf_int%interaction_t, 2)
       q(1:2) = xb1 * k
       q(3:4) = x * k
       select case (size (sf_int%mr2))
       case (2)
          call on_shell (q, &
               [sf_int%mr2(1), sf_int%mr2(2), &
               sf_int%mo2(1), sf_int%mo2(2)], &
               sf_int%on_shell_mode)
          call interaction_set_momenta (sf_int%interaction_t, &
               q, outgoing=.true.)
          E = energy (q)
          fail = any (E < 0) &
               .or. any (E(1:2) ** 2 < sf_int%mr2) &
               .or. any (E(3:4) ** 2 < sf_int%mo2)
       case default;  call msg_bug ("split momenta: incorrect use")
       end select
       if (fail) then
          sf_int%status = SF_FAILED_KINEMATICS
       else
          sf_int%status = SF_DONE_KINEMATICS
       end if
    end if
  end subroutine sf_int_split_momenta
    
  subroutine sf_int_reduce_momenta (sf_int, x)
    class(sf_int_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(in) :: x
    type(vector4_t), dimension(2) :: k
    type(vector4_t), dimension(2) :: q
    real(default), dimension(2) :: E
    logical :: fail
    if (sf_int%status >= SF_SEED_KINEMATICS) then
       select case (size (x))
       case (2)
       case default
          call msg_bug ("Pair spectrum: recoil requested &
               &but not implemented yet")
       end select
       k(1) = interaction_get_momentum (sf_int%interaction_t, 1)
       k(2) = interaction_get_momentum (sf_int%interaction_t, 2)
       q = x * k
       call on_shell (q, &
            [sf_int%mo2(1), sf_int%mo2(2)], &
            sf_int%on_shell_mode)
       call interaction_set_momenta (sf_int%interaction_t, &
            q, outgoing=.true.)
       E = energy (q)
       fail = any (E < 0) &
            .or. any (E ** 2 < sf_int%mo2)
       if (fail) then
          sf_int%status = SF_FAILED_KINEMATICS
       else
          sf_int%status = SF_DONE_KINEMATICS
       end if
    end if
  end subroutine sf_int_reduce_momenta
    
  subroutine sf_int_recover_x (sf_int, x, x_free)
    class(sf_int_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: x
    real(default), intent(inout), optional :: x_free
    type(vector4_t), dimension(:), allocatable :: k
    type(vector4_t), dimension(:), allocatable :: q
    type(splitting_data_t) :: sd
    if (sf_int%status >= SF_SEED_KINEMATICS) then
       allocate (k (interaction_get_n_in (sf_int%interaction_t)))
       allocate (q (interaction_get_n_out (sf_int%interaction_t)))
       k = interaction_get_momenta (sf_int%interaction_t, outgoing=.false.)
       q = interaction_get_momenta (sf_int%interaction_t, outgoing=.true.)
       select case (size (k))
       case (1)
          call sd%init (k(1), &
               sf_int%mi2(1), sf_int%mr2(1), sf_int%mo2(1), &
               collinear = size (x) == 1) 
          call sd%recover (k(1), q(2), sf_int%on_shell_mode)
          x(1) = sd%get_x ()
          select case (size (x))
          case (1)
          case (3)
             if (sf_int%qmax_defined) then
                if (sf_int%qmin_defined) then
                   call sd%inverse_t (x(2), &
                        t0 = - sf_int%qmax(1) ** 2, t1 = - sf_int%qmin(1) ** 2)
                else
                   call sd%inverse_t (x(2), &
                        t0 = - sf_int%qmax(1) ** 2)
                end if
             else
                if (sf_int%qmin_defined) then
                   call sd%inverse_t (x(2), t1 = - sf_int%qmin(1) ** 2)
                else
                   call sd%inverse_t (x(2)) 
                end if
             end if
             call sd%inverse_phi (x(3))
          case default
             call msg_bug ("Structure function: impossible number &
                  &of parameters")
          end select
       case (2)
          select case (size (x))
          case (2)
          case default
             call msg_bug ("Pair structure function: recoil requested &
                  &but not implemented yet")
          end select
          select case (sf_int%on_shell_mode)
          case (KEEP_ENERGY)
             select case (size (q))
             case (4)
                x = energy (q(3:4)) / energy (k)
             case (2)
                x = energy (q) / energy (k)
             end select
          case (KEEP_MOMENTUM)
             select case (size (q))
             case (4)
                x = longitudinal_part (q(3:4)) / longitudinal_part (k)
             case (2)
                x = longitudinal_part (q) / longitudinal_part (k)
             end select
          end select
       end select
    end if
  end subroutine sf_int_recover_x
  
  function sf_int_get_n_in (sf_int) result (n_in)
    class(sf_int_t), intent(in) :: sf_int
    integer :: n_in
    n_in = interaction_get_n_in (sf_int%interaction_t)
  end function sf_int_get_n_in
  
  function sf_int_get_n_rad (sf_int) result (n_rad)
    class(sf_int_t), intent(in) :: sf_int
    integer :: n_rad
    n_rad = interaction_get_n_out (sf_int%interaction_t) &
         - interaction_get_n_in (sf_int%interaction_t)
  end function sf_int_get_n_rad
  
  function sf_int_get_n_out (sf_int) result (n_out)
    class(sf_int_t), intent(in) :: sf_int
    integer :: n_out
    n_out = interaction_get_n_in (sf_int%interaction_t)
  end function sf_int_get_n_out
  
  function sf_int_get_n_states (sf_int) result (n_states)
    class(sf_int_t), intent(in) :: sf_int
    integer :: n_states
    n_states = interaction_get_n_matrix_elements (sf_int%interaction_t)
  end function sf_int_get_n_states
  
  function sf_int_get_state (sf_int, i) result (qn)
    class(sf_int_t), intent(in) :: sf_int
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    integer, intent(in) :: i
    allocate (qn (interaction_get_n_tot (sf_int%interaction_t)))
    qn = interaction_get_quantum_numbers (sf_int%interaction_t, i)
  end function sf_int_get_state

  subroutine sf_int_get_values (sf_int, value)
    class(sf_int_t), intent(in) :: sf_int
    real(default), dimension(:), intent(out) :: value
    integer :: i
    if (sf_int%status >= SF_EVALUATED) then
       do i = 1, size (value)
          value(i) = interaction_get_matrix_element &
               (sf_int%interaction_t, i)
       end do
    else
       value = 0
    end if
  end subroutine sf_int_get_values

  subroutine sf_int_compute_values (sf_int, value, x, xb, scale, E)
    class(sf_int_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: value
    real(default), dimension(:), intent(in) :: x
    real(default), dimension(:), intent(in) :: xb
    real(default), intent(in) :: scale
    real(default), dimension(:), intent(in), optional :: E
    real(default), dimension(size (x)) :: xx
    real(default) :: f
    if (present (E))  call sf_int%seed_kinematics (E)
    if (sf_int%status >= SF_SEED_KINEMATICS) then
       call sf_int%complete_kinematics (xx, f, x, xb, map=.false.)
       call sf_int%apply (scale)
       call sf_int%get_values (value)
       value = value * f
    else
       value = 0
    end if
  end subroutine sf_int_compute_values

  subroutine sf_int_compute_value &
       (sf_int, i_state, value, x, xb, scale, E)
    class(sf_int_t), intent(inout) :: sf_int
    integer, intent(in) :: i_state
    real(default), intent(out) :: value
    real(default), dimension(:), intent(in) :: x
    real(default), dimension(:), intent(in) :: xb
    real(default), intent(in) :: scale
    real(default), dimension(:), intent(in), optional :: E
    real(default), dimension(:), allocatable :: value_array
    if (sf_int%status >= SF_INITIAL) then
       allocate (value_array (sf_int%get_n_states ()))
       call sf_int%compute_values (value_array, x, xb, scale, E)
       value = value_array(i_state)
    else
       value = 0
    end if
  end subroutine sf_int_compute_value

  subroutine sf_chain_final (object)
    class(sf_chain_t), intent(inout) :: object
    integer :: i
    call object%final_tracing ()
    if (allocated (object%sf)) then 
       do i = 1, size (object%sf, 1)
          associate (sf => object%sf(i))
            if (allocated (sf%int)) then
               call sf%int%final ()
            end if
          end associate
       end do
    end if
    call beam_final (object%beam_t)
  end subroutine sf_chain_final

  subroutine sf_chain_write (object, unit)
    class(sf_chain_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "Incoming particles / structure-function chain:"
    if (associated (object%beam_data)) then
       write (u, "(3x,A,I0)")  "n_in      = ", object%n_in
       write (u, "(3x,A,I0)")  "n_strfun  = ", object%n_strfun
       write (u, "(3x,A,I0)")  "n_par     = ", object%n_par
       if (object%n_par /= object%n_bound) then
          write (u, "(3x,A,I0)")  "n_bound   = ", object%n_bound
       end if
       call beam_data_write (object%beam_data, u)
       call write_separator (u)
       call beam_write (object%beam_t, u)
       if (allocated (object%sf)) then
          do i = 1, object%n_strfun
             associate (sf => object%sf(i))
               call write_separator (u)
               if (allocated (sf%int)) then
                  call sf%int%write (u)
               else
                  write (u, "(1x,A)")  "SF instance: [undefined]"
               end if
             end associate
          end do
       end if
    else
       write (u, "(3x,A)")  "[undefined]"
    end if
  end subroutine sf_chain_write
  
  subroutine sf_chain_init (sf_chain, beam_data, sf_config)
    class(sf_chain_t), intent(out) :: sf_chain
    type(beam_data_t), intent(in), target :: beam_data
    type(sf_config_t), dimension(:), intent(in), optional, target :: sf_config
    integer :: i
    sf_chain%beam_data => beam_data
    sf_chain%n_in = beam_data_get_n_in (beam_data)
    call beam_init (sf_chain%beam_t, beam_data)
    if (present (sf_config)) then
       sf_chain%n_strfun = size (sf_config)
       allocate (sf_chain%sf (sf_chain%n_strfun))
       do i = 1, sf_chain%n_strfun
          call sf_chain%set_strfun (i, sf_config(i)%i, sf_config(i)%data)
       end do
    end if
  end subroutine sf_chain_init
  
  subroutine sf_chain_receive_beam_momenta (sf_chain)
    class(sf_chain_t), intent(inout), target :: sf_chain
    type(interaction_t), pointer :: beam_int
    beam_int => sf_chain%get_beam_int_ptr ()
    call interaction_receive_momenta (beam_int)
  end subroutine sf_chain_receive_beam_momenta
  
  subroutine sf_chain_set_beam_momenta (sf_chain, p)
    class(sf_chain_t), intent(inout) :: sf_chain
    type(vector4_t), dimension(:), intent(in) :: p
    call beam_set_momenta (sf_chain%beam_t, p)
  end subroutine sf_chain_set_beam_momenta

  subroutine sf_chain_set_strfun (sf_chain, i, beam_index, data)
    class(sf_chain_t), intent(inout) :: sf_chain
    integer, intent(in) :: i
    integer, dimension(:), intent(in) :: beam_index
    class(sf_data_t), intent(in), target :: data
    integer :: n_par, j
    n_par = data%get_n_par ()
    call data%allocate_sf_int (sf_chain%sf(i)%int)
    associate (sf_int => sf_chain%sf(i)%int)
      call sf_int%init (data)
      call sf_int%set_beam_index (beam_index)
      call sf_int%set_par_index &
           ([(j, j = sf_chain%n_par + 1, sf_chain%n_par + n_par)])
      sf_chain%n_par = sf_chain%n_par + n_par
      if (.not. data%is_generator ()) then
         sf_chain%n_bound = sf_chain%n_bound + n_par
      end if
    end associate
  end subroutine sf_chain_set_strfun
    
  function sf_chain_get_n_par (sf_chain) result (n)
    class(sf_chain_t), intent(in) :: sf_chain
    integer :: n
    n = sf_chain%n_par
  end function sf_chain_get_n_par
  
  function sf_chain_get_n_bound (sf_chain) result (n)
    class(sf_chain_t), intent(in) :: sf_chain
    integer :: n
    n = sf_chain%n_bound
  end function sf_chain_get_n_bound
  
  function sf_chain_get_beam_int_ptr (sf_chain) result (int)
    class(sf_chain_t), intent(in), target :: sf_chain
    type(interaction_t), pointer :: int
    int => beam_get_int_ptr (sf_chain%beam_t)
  end function sf_chain_get_beam_int_ptr
  
  subroutine sf_chain_setup_tracing (sf_chain, file)
    class(sf_chain_t), intent(inout) :: sf_chain
    type(string_t), intent(in) :: file
    if (sf_chain%n_strfun > 0) then
       sf_chain%trace_enable = .true.
       sf_chain%trace_unit = free_unit ()
       open (sf_chain%trace_unit, file = char (file), action = "write", &
            status = "replace")
       call sf_chain%write_trace_header ()
    else
       call msg_error ("Beam structure: no structure functions, tracing &
            &disabled")
    end if
  end subroutine sf_chain_setup_tracing

  subroutine sf_chain_final_tracing (sf_chain)
    class(sf_chain_t), intent(inout) :: sf_chain
    if (sf_chain%trace_enable) then
       close (sf_chain%trace_unit)
       sf_chain%trace_enable = .false.
    end if
  end subroutine sf_chain_final_tracing

  subroutine sf_chain_write_trace_header (sf_chain)
    class(sf_chain_t), intent(in) :: sf_chain
    integer :: u
    if (sf_chain%trace_enable) then
       u = sf_chain%trace_unit
       write (u, "('# ',A)")  "WHIZARD output: &
            &structure-function sampling data"
       write (u, "('# ',A,1x,I0)")  "Number of sf records:", sf_chain%n_strfun
       write (u, "('# ',A,1x,I0)")  "Number of parameters:", sf_chain%n_par
       write (u, "('# ',A)")  "Columns: channel, p(n_par), x(n_par), f, Jac * f"
    end if
  end subroutine sf_chain_write_trace_header
    
  subroutine sf_chain_trace (sf_chain, c_sel, p, x, f, sf_sum)
    class(sf_chain_t), intent(in) :: sf_chain
    integer, intent(in) :: c_sel
    real(default), dimension(:,:), intent(in) :: p
    real(default), dimension(:), intent(in) :: x
    real(default), dimension(:), intent(in) :: f
    real(default), intent(in) :: sf_sum
    integer :: u, i
    if (sf_chain%trace_enable) then
       u = sf_chain%trace_unit
       write (u, "(1x,I0)", advance="no")  c_sel
       write (u, "(2x)", advance="no")
       do i = 1, sf_chain%n_par
          write (u, "(1x," // FMT_17 // ")", advance="no")  p(i,c_sel)
       end do
       write (u, "(2x)", advance="no")
       do i = 1, sf_chain%n_par
          write (u, "(1x," // FMT_17 // ")", advance="no")  x(i)
       end do
       write (u, "(2x)", advance="no")
       write (u, "(2(1x," // FMT_17 // "))")  sf_sum, f(c_sel) * sf_sum
    end if
  end subroutine sf_chain_trace
  
  subroutine sf_chain_instance_final (object)
    class(sf_chain_instance_t), intent(inout) :: object
    integer :: i
    if (allocated (object%sf)) then
       do i = 1, size (object%sf, 1)
          associate (sf => object%sf(i))
            if (allocated (sf%int)) then
               call evaluator_final (sf%eval)
               call sf%int%final ()
            end if
          end associate
       end do
    end if
    call beam_final (object%beam_t)
  end subroutine sf_chain_instance_final

  subroutine sf_chain_instance_write (object, unit)
    class(sf_chain_instance_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, i, c
    u = given_output_unit (unit)
    write (u, "(1x,A)", advance="no")  "Structure-function chain instance:"
    call write_sf_status (object%status, u)
    if (allocated (object%out_sf)) then
       write (u, "(3x,A)", advance="no")  "outgoing (interactions) ="
       do i = 1, size (object%out_sf)
          write (u, "(1x,I0,':',I0)", advance="no") &
               object%out_sf(i), object%out_sf_i(i)
       end do
       write (u, *)
    end if
    if (object%out_eval /= 0) then
       write (u, "(3x,A)", advance="no")  "outgoing (evaluators)   ="
       do i = 1, size (object%out_sf)
          write (u, "(1x,I0,':',I0)", advance="no") &
               object%out_eval, object%out_eval_i(i)
       end do
       write (u, *)
    end if
    if (allocated (object%sf)) then
       if (size (object%sf) /= 0) then
          write (u, "(1x,A)")  "Structure-function parameters:"
          do c = 1, size (object%f)
             write (u, "(1x,A,I0,A)", advance="no")  "Channel #", c, ":"
             if (c == object%selected_channel) then
                write (u, "(1x,A)")  "[selected]"
             else
                write (u, *)
             end if
             write (u, "(3x,A,9(1x,F9.7))")  "p =", object%p(:,c)
             write (u, "(3x,A,9(1x,F9.7))")  "r =", object%r(:,c)
             write (u, "(3x,A,9(1x,ES13.7))")  "f =", object%f(c)
             write (u, "(3x,A)", advance="no") "m ="
             call object%channel(c)%write (u)
          end do
          write (u, "(3x,A,9(1x,F9.7))")  "x =", object%x
          if (.not. all (object%bound)) then
             write (u, "(3x,A,9(1x,L1))")  "bound =", object%bound
          end if
       end if
    end if
    call write_separator (u)
    call beam_write (object%beam_t, u)
    if (allocated (object%sf)) then
       do i = 1, size (object%sf)
          associate (sf => object%sf(i))
            call write_separator (u)
            if (allocated (sf%int)) then
               if (allocated (sf%r)) then
                  write (u, "(1x,A)")  "Structure-function parameters:"
                  do c = 1, size (sf%f)
                     write (u, "(1x,A,I0,A)", advance="no")  "Channel #", c, ":"
                     if (c == object%selected_channel) then
                        write (u, "(1x,A)")  "[selected]"
                     else
                        write (u, *)
                     end if
                     write (u, "(3x,A,9(1x,F9.7))")  "r =", sf%r(:,c)
                     write (u, "(3x,A,9(1x,ES13.7))")  "f =", sf%f(c)
                     write (u, "(3x,A,9(1x,L1,7x))") "m =", sf%m(c)
                  end do
                  write (u, "(3x,A,9(1x,F9.7))")  "x =", sf%x
               end if
               call sf%int%write (u)
               if (.not. evaluator_is_empty (sf%eval)) then
                     call sf%eval%write (u)
               end if
            end if
          end associate
       end do
    end if
  end subroutine sf_chain_instance_write
  
  subroutine sf_chain_instance_init (chain, config, n_channel)
    class(sf_chain_instance_t), intent(out), target :: chain
    type(sf_chain_t), intent(in), target :: config
    integer, intent(in) :: n_channel
    integer :: i, j
    integer :: n_par_tot, n_par, n_strfun
    chain%config => config
    n_strfun = config%n_strfun
    chain%beam_t = config%beam_t
    allocate (chain%out_sf (config%n_in), chain%out_sf_i (config%n_in))
    allocate (chain%out_eval_i (config%n_in))
    chain%out_sf = 0
    chain%out_sf_i = [(i, i = 1, config%n_in)]
    chain%out_eval_i = chain%out_sf_i
    n_par_tot = 0
    if (n_strfun /= 0) then
       allocate (chain%sf (n_strfun))
       do i = 1, n_strfun
          associate (sf => chain%sf(i))
            allocate (sf%int, source=config%sf(i)%int)
            sf%int%interaction_t = config%sf(i)%int%interaction_t
            n_par = size (sf%int%par_index)
            allocate (sf%r (n_par, n_channel));  sf%r = 0
            allocate (sf%rb(n_par, n_channel));  sf%rb= 0
            allocate (sf%f (n_channel));         sf%f = 0
            allocate (sf%m (n_channel));         sf%m = .false.
            allocate (sf%x (n_par));             sf%x = 0
            n_par_tot = n_par_tot + n_par
          end associate
       end do
       allocate (chain%p (n_par_tot, n_channel));  chain%p = 0
       allocate (chain%pb(n_par_tot, n_channel));  chain%pb= 0
       allocate (chain%r (n_par_tot, n_channel));  chain%r = 0
       allocate (chain%rb(n_par_tot, n_channel));  chain%rb= 0
       allocate (chain%f (n_channel));             chain%f = 0
       allocate (chain%x (n_par_tot));             chain%x = 0
       call allocate_sf_channels &
            (chain%channel, n_channel=n_channel, n_strfun=n_strfun)
    end if
    allocate (chain%bound (n_par_tot), source = .true.)
    do i = 1, n_strfun
       associate (sf => chain%sf(i))
         if (sf%int%is_generator ()) then
            do j = 1, size (sf%int%par_index)
               chain%bound(sf%int%par_index(j)) = .false.
            end do
         end if
       end associate
    end do
    chain%status = SF_INITIAL
  end subroutine sf_chain_instance_init
  
  subroutine sf_chain_instance_select_channel (chain, channel)
    class(sf_chain_instance_t), intent(inout) :: chain
    integer, intent(in), optional :: channel
    if (present (channel)) then
       chain%selected_channel = channel
    else
       chain%selected_channel = 0
    end if
  end subroutine sf_chain_instance_select_channel
  
  subroutine sf_chain_instance_set_channel (chain, c, channel)
    class(sf_chain_instance_t), intent(inout) :: chain
    integer, intent(in) :: c
    type(sf_channel_t), intent(in) :: channel
    integer :: i, j, k
    if (chain%status >= SF_INITIAL) then
       chain%channel(c) = channel
       j = 0
       do i = 1, chain%config%n_strfun
          associate (sf => chain%sf(i))
            sf%m(c) = channel%is_single_mapping (i)
            if (channel%is_multi_mapping (i)) then
               do k = 1, size (sf%int%beam_index)
                  j = j + 1
                  call chain%channel(c)%set_par_index (j, sf%int%par_index(k))
               end do
            end if
          end associate
       end do
       chain%status = SF_INITIAL
    end if
  end subroutine sf_chain_instance_set_channel
  
  subroutine sf_chain_instance_link_interactions (chain)
    class(sf_chain_instance_t), intent(inout), target :: chain
    type(interaction_t), pointer :: int
    integer :: i, j, b
    if (chain%status >= SF_INITIAL) then
       do b = 1, chain%config%n_in
          int => beam_get_int_ptr (chain%beam_t)
          call interaction_set_source_link (int, b, &
               chain%config%beam_t, b)
       end do
       if (allocated (chain%sf)) then
          do i = 1, size (chain%sf)
             associate (sf_int => chain%sf(i)%int)
               do j = 1, size (sf_int%beam_index)
                  b = sf_int%beam_index(j)
                  call link (sf_int%interaction_t, b, sf_int%incoming(j))
                  chain%out_sf(b) = i
                  chain%out_sf_i(b) = sf_int%outgoing(j)
               end do
             end associate
          end do
       end if
       chain%status = SF_DONE_LINKS
    end if
  contains
    subroutine link (int, b, in_index)
      type(interaction_t), intent(inout) :: int
      integer, intent(in) :: b, in_index
      integer :: i
      i = chain%out_sf(b)
      select case (i)
      case (0)
         call interaction_set_source_link (int, in_index, &
              chain%beam_t, chain%out_sf_i(b))
      case default
         call interaction_set_source_link (int, in_index, &
              chain%sf(i)%int%interaction_t, chain%out_sf_i(b))
      end select
    end subroutine link
  end subroutine sf_chain_instance_link_interactions
  
  subroutine sf_chain_exchange_mask (chain)
    class(sf_chain_instance_t), intent(inout), target :: chain
    type(interaction_t), pointer :: int
    type(quantum_numbers_mask_t), dimension(:), allocatable :: mask
    integer :: i
    if (chain%status >= SF_DONE_LINKS) then
       if (allocated (chain%sf)) then
          int => beam_get_int_ptr (chain%beam_t)
          allocate (mask (interaction_get_n_out (int)))
          mask = interaction_get_mask (int)
          if (size (chain%sf) /= 0) then
             do i = 1, size (chain%sf) - 1
                call interaction_exchange_mask (chain%sf(i)%int%interaction_t)
             end do
             do i = size (chain%sf), 1, -1
                call interaction_exchange_mask (chain%sf(i)%int%interaction_t)
             end do
             if (any (mask .neqv. interaction_get_mask (int))) then
                chain%status = SF_FAILED_MASK
                return
             end if
             do i = 1, size (chain%sf)
                call chain%sf(i)%int%setup_constants ()
             end do
          end if
       end if
       chain%status = SF_DONE_MASK
    end if
  end subroutine sf_chain_exchange_mask
  
  subroutine sf_chain_instance_init_evaluators (chain)
    class(sf_chain_instance_t), intent(inout), target :: chain
    type(interaction_t), pointer :: int
    type(quantum_numbers_mask_t) :: mask
    integer :: i
    if (chain%status >= SF_DONE_MASK) then
       if (allocated (chain%sf)) then
          if (size (chain%sf) /= 0) then
             mask = new_quantum_numbers_mask (.false., .false., .true.)
             int => beam_get_int_ptr (chain%beam_t)
             do i = 1, size (chain%sf)
                associate (sf => chain%sf(i))
                  call evaluator_init_product (sf%eval, &
                       int, sf%int%interaction_t, &
                       mask)
                  if (evaluator_is_empty (sf%eval)) then
                     chain%status = SF_FAILED_CONNECTIONS
                     return
                  end if
                  int => evaluator_get_int_ptr (sf%eval)
                end associate
             end do
             call find_outgoing_particles ()
          end if
       end if
       chain%status = SF_DONE_CONNECTIONS
    end if
  contains
    subroutine find_outgoing_particles ()
      type(interaction_t), pointer :: int, int_next
      integer :: i, j, out_sf, out_i
      chain%out_eval = size (chain%sf)
      do j = 1, size (chain%out_eval_i)
         out_sf = chain%out_sf(j)
         out_i = chain%out_sf_i(j)
         if (out_sf == 0) then
            int => beam_get_int_ptr (chain%beam_t)
            out_sf = 1
         else
            int => chain%sf(out_sf)%int%interaction_t
         end if
         do i = out_sf, chain%out_eval
            int_next => evaluator_get_int_ptr (chain%sf(i)%eval)
            out_i = interaction_find_link (int_next, int, out_i)
            int => int_next
         end do
         chain%out_eval_i(j) = out_i
      end do
    end subroutine find_outgoing_particles
  end subroutine sf_chain_instance_init_evaluators
  
  subroutine sf_chain_instance_compute_kinematics (chain, c_sel, p_in)
    class(sf_chain_instance_t), intent(inout), target :: chain
    integer, intent(in) :: c_sel
    real(default), dimension(:), intent(in) :: p_in
    type(interaction_t), pointer :: int
    real(default) :: f_mapping
    logical, dimension(size (chain%bound)) :: bound
    integer :: i, j, c
    if (chain%status >= SF_DONE_CONNECTIONS) then
       call chain%select_channel (c_sel)
       int => beam_get_int_ptr (chain%beam_t)
       call interaction_receive_momenta (int)
       if (allocated (chain%sf)) then
          if (size (chain%sf) /= 0) then
             forall (i = 1:size (chain%sf))  chain%sf(i)%int%status = SF_INITIAL
             !!! Bug in nagfor 5.3.1(907), fixed in 5.3.1(982)
             ! chain%p (:,c_sel) = unpack (p_in, chain%bound, 0._default)
             !!! Workaround:
             bound = chain%bound
             chain%p (:,c_sel) = unpack (p_in, bound, 0._default)
             chain%pb(:,c_sel) = 1 - chain%p(:,c_sel)
             chain%f = 1
             chain%x_free = 1
             do i = 1, size (chain%sf)
                associate (sf => chain%sf(i))
                  call sf%int%generate_free (sf%r(:,c_sel), sf%rb(:,c_sel), &
                       chain%x_free)
                  do j = 1, size (sf%x)
                     if (.not. chain%bound(sf%int%par_index(j))) then
                        chain%p (sf%int%par_index(j),c_sel) = sf%r (j,c_sel)
                        chain%pb(sf%int%par_index(j),c_sel) = sf%rb(j,c_sel)
                     end if
                  end do
                end associate
             end do
             if (allocated (chain%channel(c_sel)%multi_mapping)) then
                call chain%channel(c_sel)%multi_mapping%compute &
                     (chain%r(:,c_sel), chain%rb(:,c_sel), &
                      f_mapping, &
                      chain%p(:,c_sel), chain%pb(:,c_sel), &
                      chain%x_free)
                chain%f(c_sel) = f_mapping
             else
                chain%r (:,c_sel) = chain%p (:,c_sel)
                chain%rb(:,c_sel) = chain%pb(:,c_sel)
                chain%f(c_sel) = 1
             end if
             do i = 1, size (chain%sf)
                associate (sf => chain%sf(i))
                  call sf%int%seed_kinematics ()
                  do j = 1, size (sf%x)
                     sf%r (j,c_sel) = chain%r (sf%int%par_index(j),c_sel)
                     sf%rb(j,c_sel) = chain%rb(sf%int%par_index(j),c_sel)
                  end do
                  call sf%int%complete_kinematics &
                       (sf%x, sf%f(c_sel), sf%r(:,c_sel), sf%rb(:,c_sel), &
                        sf%m(c_sel))
                  do j = 1, size (sf%x)
                     chain%x(sf%int%par_index(j)) = sf%x(j)
                  end do
                  if (sf%int%status <= SF_FAILED_KINEMATICS) then
                     chain%status = SF_FAILED_KINEMATICS
                     return
                  end if
                  do c = 1, size (sf%f)
                     if (c /= c_sel) then
                        call sf%int%inverse_kinematics &
                             (sf%x, sf%f(c), sf%r(:,c), sf%rb(:,c), sf%m(c))
                        do j = 1, size (sf%x)
                           chain%r (sf%int%par_index(j),c) = sf%r (j,c)
                           chain%rb(sf%int%par_index(j),c) = sf%rb(j,c)
                        end do
                     end if
                     chain%f(c) = chain%f(c) * sf%f(c)
                  end do
                  if (.not. evaluator_is_empty (sf%eval)) then
                     call evaluator_receive_momenta (sf%eval)
                  end if
                end associate
             end do
             do c = 1, size (chain%f)
                if (c /= c_sel) then
                   if (allocated (chain%channel(c)%multi_mapping)) then
                      call chain%channel(c)%multi_mapping%inverse &
                           (chain%r(:,c), chain%rb(:,c), &
                            f_mapping, &
                            chain%p(:,c), chain%pb(:,c), &
                            chain%x_free)
                      chain%f(c) = chain%f(c) * f_mapping
                   else
                      chain%p (:,c) = chain%r (:,c)
                      chain%pb(:,c) = chain%rb(:,c)
                   end if
                end if
             end do
          end if
       end if
       chain%status = SF_DONE_KINEMATICS
    end if
  end subroutine sf_chain_instance_compute_kinematics
  
  subroutine sf_chain_instance_inverse_kinematics (chain, x)
    class(sf_chain_instance_t), intent(inout), target :: chain
    real(default), dimension(:), intent(in) :: x
    type(interaction_t), pointer :: int
    real(default) :: f_mapping
    integer :: i, j, c
    if (chain%status >= SF_DONE_CONNECTIONS) then
       call chain%select_channel ()
       int => beam_get_int_ptr (chain%beam_t)
       call interaction_receive_momenta (int)
       if (allocated (chain%sf)) then
          chain%f = 1
          if (size (chain%sf) /= 0) then
             forall (i = 1:size (chain%sf))  chain%sf(i)%int%status = SF_INITIAL
             chain%x = x
             do i = 1, size (chain%sf)
                associate (sf => chain%sf(i))
                  call sf%int%seed_kinematics ()
                  do j = 1, size (sf%x)
                     sf%x(j) = chain%x(sf%int%par_index(j))
                  end do
                  do c = 1, size (sf%f)
                     call sf%int%inverse_kinematics &
                          (sf%x, sf%f(c), sf%r(:,c), sf%rb(:,c), sf%m(c), c==1)
                     chain%f(c) = chain%f(c) * sf%f(c)
                     do j = 1, size (sf%x)
                        chain%r (sf%int%par_index(j),c) = sf%r (j,c)
                        chain%rb(sf%int%par_index(j),c) = sf%rb(j,c)
                     end do
                  end do
                  if (.not. evaluator_is_empty (sf%eval)) then
                     call evaluator_receive_momenta (sf%eval)
                  end if
                end associate
             end do
             do c = 1, size (chain%f)
                if (allocated (chain%channel(c)%multi_mapping)) then
                   call chain%channel(c)%multi_mapping%inverse &
                        (chain%r(:,c), chain%rb(:,c), &
                        f_mapping, &
                        chain%p(:,c), chain%pb(:,c), &
                        chain%x_free)
                   chain%f(c) = chain%f(c) * f_mapping
                else
                   chain%p (:,c) = chain%r (:,c)
                   chain%pb(:,c) = chain%rb(:,c)
                end if
             end do
          end if
       end if
       chain%status = SF_DONE_KINEMATICS
    end if
  end subroutine sf_chain_instance_inverse_kinematics
  
  subroutine sf_chain_instance_recover_kinematics (chain, c_sel)
    class(sf_chain_instance_t), intent(inout), target :: chain
    integer, intent(in) :: c_sel
    real(default) :: f_mapping
    integer :: i, j, c
    if (chain%status >= SF_DONE_CONNECTIONS) then
       call chain%select_channel (c_sel)
       if (allocated (chain%sf)) then
          do i = size (chain%sf), 1, -1
             associate (sf => chain%sf(i))
               if (.not. evaluator_is_empty (sf%eval)) then
                  call evaluator_send_momenta (sf%eval)
               end if
             end associate
          end do
          chain%f = 1
          if (size (chain%sf) /= 0) then
             forall (i = 1:size (chain%sf))  chain%sf(i)%int%status = SF_INITIAL
             chain%x_free = 1
             do i = 1, size (chain%sf)
                associate (sf => chain%sf(i))
                  call sf%int%seed_kinematics ()
                  call sf%int%recover_x (sf%x, chain%x_free)
                  do j = 1, size (sf%x)
                     chain%x(sf%int%par_index(j)) = sf%x(j)
                  end do
                  do c = 1, size (sf%f)
                     call sf%int%inverse_kinematics &
                          (sf%x, sf%f(c), sf%r(:,c), sf%rb(:,c), sf%m(c), c==1)
                     chain%f(c) = chain%f(c) * sf%f(c)
                     do j = 1, size (sf%x)
                        chain%r (sf%int%par_index(j),c) = sf%r (j,c)
                        chain%rb(sf%int%par_index(j),c) = sf%rb(j,c)
                     end do
                  end do
                end associate
             end do
             do c = 1, size (chain%f)
                if (allocated (chain%channel(c)%multi_mapping)) then
                   call chain%channel(c)%multi_mapping%inverse &
                        (chain%r(:,c), chain%rb(:,c), &
                        f_mapping, &
                        chain%p(:,c), chain%pb(:,c), &
                        chain%x_free)
                   chain%f(c) = chain%f(c) * f_mapping
                else
                   chain%p (:,c) = chain%r (:,c)
                   chain%pb(:,c) = chain%rb(:,c)
                end if
             end do
          end if
       end if
       chain%status = SF_DONE_KINEMATICS
    end if
  end subroutine sf_chain_instance_recover_kinematics

  subroutine sf_chain_instance_return_beam_momenta (chain)
    class(sf_chain_instance_t), intent(in), target :: chain
    type(interaction_t), pointer :: int
    if (chain%status >= SF_DONE_KINEMATICS) then
       int => beam_get_int_ptr (chain%beam_t)
       call interaction_send_momenta (int)
    end if
  end subroutine sf_chain_instance_return_beam_momenta

  subroutine sf_chain_instance_evaluate (chain, scale)
    class(sf_chain_instance_t), intent(inout), target :: chain
    real(default), intent(in) :: scale
    type(interaction_t), pointer :: out_int
    real(default) :: sf_sum
    integer :: i
    if (chain%status >= SF_DONE_KINEMATICS) then
       if (allocated (chain%sf)) then
          if (size (chain%sf) /= 0) then
             do i = 1, size (chain%sf)
                associate (sf => chain%sf(i))
                  call sf%int%apply (scale)
                  if (sf%int%status <= SF_FAILED_EVALUATION) then
                     chain%status = SF_FAILED_EVALUATION
                     return
                  end if
                  if (.not. evaluator_is_empty (sf%eval)) then
                     call sf%eval%evaluate ()
                  end if
                end associate
             end do
             out_int => chain%get_out_int_ptr ()
             sf_sum = interaction_sum (out_int)
             call chain%config%trace &
                  (chain%selected_channel, chain%p, chain%x, chain%f, sf_sum)
          end if
       end if
       chain%status = SF_EVALUATED
    end if
  end subroutine sf_chain_instance_evaluate
  
  subroutine sf_chain_instance_get_out_momenta (chain, p)
    class(sf_chain_instance_t), intent(in), target :: chain
    type(vector4_t), dimension(:), intent(out) :: p
    type(interaction_t), pointer :: int
    integer :: i, j
    if (chain%status >= SF_DONE_KINEMATICS) then
       do j = 1, size (chain%out_sf)
          i = chain%out_sf(j)
          select case (i)
          case (0)
             int => beam_get_int_ptr (chain%beam_t)
          case default
             int => chain%sf(i)%int%interaction_t
          end select
          p(j) = interaction_get_momentum (int, chain%out_sf_i(j))
       end do
    end if
  end subroutine sf_chain_instance_get_out_momenta
       
  function sf_chain_instance_get_out_int_ptr (chain) result (int)
    class(sf_chain_instance_t), intent(in), target :: chain
    type(interaction_t), pointer :: int
    if (chain%out_eval == 0) then
       int => beam_get_int_ptr (chain%beam_t)
    else
       int => evaluator_get_int_ptr (chain%sf(chain%out_eval)%eval)
    end if
  end function sf_chain_instance_get_out_int_ptr

  function sf_chain_instance_get_out_i (chain, j) result (i)
    class(sf_chain_instance_t), intent(in) :: chain
    integer, intent(in) :: j
    integer :: i
    i = chain%out_eval_i(j)
  end function sf_chain_instance_get_out_i
    
  function sf_chain_instance_get_out_mask (chain) result (mask)
    class(sf_chain_instance_t), intent(in), target :: chain
    type(quantum_numbers_mask_t), dimension(:), allocatable :: mask
    type(interaction_t), pointer :: int
    allocate (mask (chain%config%n_in))
    int => chain%get_out_int_ptr ()
    mask = interaction_get_mask (int, chain%out_eval_i)
  end function sf_chain_instance_get_out_mask
    
  subroutine sf_chain_instance_get_mcpar (chain, c, r)
    class(sf_chain_instance_t), intent(in) :: chain
    integer, intent(in) :: c
    real(default), dimension(:), intent(out) :: r
    if (allocated (chain%p))  r = pack (chain%p(:,c), chain%bound)
  end subroutine sf_chain_instance_get_mcpar
  
  function sf_chain_instance_get_f (chain, c) result (f)
    class(sf_chain_instance_t), intent(in) :: chain
    integer, intent(in) :: c
    real(default) :: f
    if (allocated (chain%f)) then
       f = chain%f(c)
    else
       f = 1
    end if
  end function sf_chain_instance_get_f
  
  function sf_chain_instance_get_status (chain) result (status)
    class(sf_chain_instance_t), intent(in) :: chain
    integer :: status
    status = chain%status
  end function sf_chain_instance_get_status
  
  subroutine sf_test_data_write (data, unit, verbose)
    class(sf_test_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "SF test data:"
    write (u, "(3x,A,A)") "model     = ", char (data%model%get_name ())
    write (u, "(3x,A)", advance="no") "incoming  = "
    call flavor_write (data%flv_in, u);  write (u, *)
    write (u, "(3x,A)", advance="no") "outgoing  = "
    call flavor_write (data%flv_out, u);  write (u, *)
    write (u, "(3x,A)", advance="no") "radiated  = "
    call flavor_write (data%flv_rad, u);  write (u, *)
    write (u, "(3x,A," // FMT_19 // ")")  "mass      = ", data%m
    write (u, "(3x,A,L1)")  "collinear = ", data%collinear
    if (.not. data%collinear .and. allocated (data%qbounds)) then
       write (u, "(3x,A," // FMT_19 // ")")  "qmin      = ", data%qbounds(1)
       write (u, "(3x,A," // FMT_19 // ")")  "qmax      = ", data%qbounds(2)
    end if
  end subroutine sf_test_data_write
    
  subroutine sf_test_data_init (data, model, pdg_in, collinear, qbounds, mode)
    class(sf_test_data_t), intent(out) :: data
    class(model_data_t), intent(in), target :: model
    type(pdg_array_t), intent(in) :: pdg_in
    logical, intent(in), optional :: collinear
    real(default), dimension(2), intent(in), optional :: qbounds
    integer, intent(in), optional :: mode
    data%model => model
    if (present (mode))  data%mode = mode
    if (pdg_array_get (pdg_in, 1) /= 25) then
       call msg_fatal ("Test spectrum function: input flavor must be 's'")
    end if
    call flavor_init (data%flv_in, 25, model)
    data%m = flavor_get_mass (data%flv_in)
    if (present (collinear))  data%collinear = collinear
    call flavor_init (data%flv_out, 25, model)
    call flavor_init (data%flv_rad, 25, model)
    if (present (qbounds)) then
       allocate (data%qbounds (2))
       data%qbounds = qbounds
    end if
  end subroutine sf_test_data_init
  
  function sf_test_data_get_n_par (data) result (n)
    class(sf_test_data_t), intent(in) :: data
    integer :: n
    if (data%collinear) then
       n = 1
    else
       n = 3
    end if
  end function sf_test_data_get_n_par
  
  subroutine sf_test_data_get_pdg_out (data, pdg_out)
    class(sf_test_data_t), intent(in) :: data
    type(pdg_array_t), dimension(:), intent(inout) :: pdg_out
    pdg_out(1) = 25
  end subroutine sf_test_data_get_pdg_out
  
  subroutine sf_test_data_allocate_sf_int (data, sf_int)
    class(sf_test_data_t), intent(in) :: data
    class(sf_int_t), intent(inout), allocatable :: sf_int
    allocate (sf_test_t :: sf_int)
  end subroutine sf_test_data_allocate_sf_int
    
  function sf_test_type_string (object) result (string)
    class(sf_test_t), intent(in) :: object
    type(string_t) :: string
    string = "Test"
  end function sf_test_type_string
  
  subroutine sf_test_write (object, unit, testflag)
    class(sf_test_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = given_output_unit (unit)
    if (associated (object%data)) then
       call object%data%write (u)
       call object%base_write (u, testflag)
    else
       write (u, "(1x,A)")  "SF test data: [undefined]"
    end if
  end subroutine sf_test_write
    
  subroutine sf_test_init (sf_int, data)
    class(sf_test_t), intent(out) :: sf_int
    class(sf_data_t), intent(in), target :: data
    type(quantum_numbers_mask_t), dimension(3) :: mask
    type(helicity_t) :: hel0
    type(quantum_numbers_t), dimension(3) :: qn
    mask = new_quantum_numbers_mask (.false., .false., .false.)
    select type (data)
    type is (sf_test_data_t)
       if (allocated (data%qbounds)) then
          call sf_int%base_init (mask, &
               [data%m**2], [0._default], [data%m**2], &
               [data%qbounds(1)], [data%qbounds(2)])
       else
          call sf_int%base_init (mask, &
               [data%m**2], [0._default], [data%m**2])
       end if
       sf_int%data => data
       call helicity_init (hel0, 0)
       call quantum_numbers_init (qn(1), data%flv_in,  hel0)
       call quantum_numbers_init (qn(2), data%flv_rad, hel0)
       call quantum_numbers_init (qn(3), data%flv_out, hel0)
       call interaction_add_state (sf_int%interaction_t, qn)
       call interaction_freeze (sf_int%interaction_t)
       call sf_int%set_incoming ([1])
       call sf_int%set_radiated ([2])
       call sf_int%set_outgoing ([3])
    end select
    sf_int%status = SF_INITIAL
  end subroutine sf_test_init

  subroutine sf_test_complete_kinematics (sf_int, x, f, r, rb, map)
    class(sf_test_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: r
    real(default), dimension(:), intent(in) :: rb
    logical, intent(in) :: map
    real(default) :: xb1
    if (map) then
       x(1) = r(1)**2
       f = 2 * r(1)
    else
       x(1) = r(1)
       f = 1
    end if
    xb1 = 1 - x(1)
    if (size (x) == 3)  x(2:3) = r(2:3)
    call sf_int%split_momentum (x, xb1)
    sf_int%x = x(1)
    select case (sf_int%status)
    case (SF_FAILED_KINEMATICS);  f = 0
    end select
  end subroutine sf_test_complete_kinematics

  subroutine sf_test_inverse_kinematics (sf_int, x, f, r, rb, map, set_momenta)
    class(sf_test_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(in) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: r
    real(default), dimension(:), intent(out) :: rb
    logical, intent(in) :: map
    logical, intent(in), optional :: set_momenta
    real(default) :: xb1
    logical :: set_mom
    set_mom = .false.;  if (present (set_momenta))  set_mom = set_momenta
    if (map) then
       r(1) = sqrt (x(1))
       f = 2 * r(1)
    else
       r(1) = x(1)
       f = 1
    end if
    xb1 = 1 - x(1)
    if (size (x) == 3)  r(2:3) = x(2:3)
    rb = 1 - r
    sf_int%x = x(1)
    if (set_mom) then
       call sf_int%split_momentum (x, xb1)
       select case (sf_int%status)
       case (SF_FAILED_KINEMATICS);  f = 0
       end select
    end if
  end subroutine sf_test_inverse_kinematics

  subroutine sf_test_apply (sf_int, scale)
    class(sf_test_t), intent(inout) :: sf_int
    real(default), intent(in) :: scale
    select case (sf_int%data%mode)
    case (0)
       call interaction_set_matrix_element (sf_int%interaction_t, &
            cmplx (1._default, kind=default))
    case (1)
       call interaction_set_matrix_element (sf_int%interaction_t, &
            cmplx (sf_int%x, kind=default))
    end select
    sf_int%status = SF_EVALUATED
  end subroutine sf_test_apply

  subroutine sf_test_spectrum_data_write (data, unit, verbose)
    class(sf_test_spectrum_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "SF test spectrum data:"
    write (u, "(3x,A,A)") "model     = ", char (data%model%get_name ())
    write (u, "(3x,A)", advance="no") "incoming  = "
    call flavor_write (data%flv_in, u);  write (u, *)
    write (u, "(3x,A)", advance="no") "outgoing  = "
    call flavor_write (data%flv_out, u);  write (u, *)
    write (u, "(3x,A)", advance="no") "radiated  = "
    call flavor_write (data%flv_rad, u);  write (u, *)
    write (u, "(3x,A," // FMT_19 // ")")  "mass      = ", data%m
  end subroutine sf_test_spectrum_data_write
    
  subroutine sf_test_spectrum_data_init (data, model, pdg_in, with_radiation)
    class(sf_test_spectrum_data_t), intent(out) :: data
    class(model_data_t), intent(in), target :: model
    type(pdg_array_t), intent(in) :: pdg_in
    logical, intent(in) :: with_radiation
    data%model => model
    data%with_radiation = with_radiation
    if (pdg_array_get (pdg_in, 1) /= 25) then
       call msg_fatal ("Test structure function: input flavor must be 's'")
    end if
    call flavor_init (data%flv_in, 25, model)
    data%m = flavor_get_mass (data%flv_in)
    call flavor_init (data%flv_out, 25, model)
    if (with_radiation) then
       call flavor_init (data%flv_rad, 25, model)
    end if
  end subroutine sf_test_spectrum_data_init
  
  function sf_test_spectrum_data_get_n_par (data) result (n)
    class(sf_test_spectrum_data_t), intent(in) :: data
    integer :: n
    n = 2
  end function sf_test_spectrum_data_get_n_par
  
  subroutine sf_test_spectrum_data_get_pdg_out (data, pdg_out)
    class(sf_test_spectrum_data_t), intent(in) :: data
    type(pdg_array_t), dimension(:), intent(inout) :: pdg_out
    pdg_out(1) = 25
    pdg_out(2) = 25
  end subroutine sf_test_spectrum_data_get_pdg_out
  
  subroutine sf_test_spectrum_data_allocate_sf_int (data, sf_int)
    class(sf_test_spectrum_data_t), intent(in) :: data
    class(sf_int_t), intent(inout), allocatable :: sf_int
    allocate (sf_test_spectrum_t :: sf_int)
  end subroutine sf_test_spectrum_data_allocate_sf_int
    
  function sf_test_spectrum_type_string (object) result (string)
    class(sf_test_spectrum_t), intent(in) :: object
    type(string_t) :: string
    string = "Test Spectrum"
  end function sf_test_spectrum_type_string
  
  subroutine sf_test_spectrum_write (object, unit, testflag)
    class(sf_test_spectrum_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = given_output_unit (unit)
    if (associated (object%data)) then
       call object%data%write (u)
       call object%base_write (u, testflag)
    else
       write (u, "(1x,A)")  "SF test spectrum data: [undefined]"
    end if
  end subroutine sf_test_spectrum_write
    
  subroutine sf_test_spectrum_init (sf_int, data)
    class(sf_test_spectrum_t), intent(out) :: sf_int
    class(sf_data_t), intent(in), target :: data
    type(quantum_numbers_mask_t), dimension(6) :: mask
    type(helicity_t) :: hel0
    type(quantum_numbers_t), dimension(6) :: qn
    mask = new_quantum_numbers_mask (.false., .false., .false.)
    select type (data)
    type is (sf_test_spectrum_data_t)
       if (data%with_radiation) then
          call sf_int%base_init (mask(1:6), &
               [data%m**2, data%m**2], &
               [0._default, 0._default], &
               [data%m**2, data%m**2])
          sf_int%data => data
          call helicity_init (hel0, 0)
          call quantum_numbers_init (qn(1), data%flv_in,  hel0)
          call quantum_numbers_init (qn(2), data%flv_in,  hel0)
          call quantum_numbers_init (qn(3), data%flv_rad, hel0)
          call quantum_numbers_init (qn(4), data%flv_rad, hel0)
          call quantum_numbers_init (qn(5), data%flv_out, hel0)
          call quantum_numbers_init (qn(6), data%flv_out, hel0)
          call interaction_add_state (sf_int%interaction_t, qn(1:6))
          call sf_int%set_incoming ([1,2])
          call sf_int%set_radiated ([3,4])
          call sf_int%set_outgoing ([5,6])
       else
          call sf_int%base_init (mask(1:4), &
               [data%m**2, data%m**2], &
               [real(default) :: ], &
               [data%m**2, data%m**2])
          sf_int%data => data
          call helicity_init (hel0, 0)
          call quantum_numbers_init (qn(1), data%flv_in,  hel0)
          call quantum_numbers_init (qn(2), data%flv_in,  hel0)
          call quantum_numbers_init (qn(3), data%flv_out, hel0)
          call quantum_numbers_init (qn(4), data%flv_out, hel0)
          call interaction_add_state (sf_int%interaction_t, qn(1:4))
          call sf_int%set_incoming ([1,2])
          call sf_int%set_outgoing ([3,4])
       end if
       call interaction_freeze (sf_int%interaction_t)
    end select
    sf_int%status = SF_INITIAL
  end subroutine sf_test_spectrum_init

  subroutine sf_test_spectrum_complete_kinematics (sf_int, x, f, r, rb, map)
    class(sf_test_spectrum_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: r
    real(default), dimension(:), intent(in) :: rb
    logical, intent(in) :: map
    real(default), dimension(2) :: xb1
    if (map) then
       x = r**2
       f = 4 * r(1) * r(2)
    else
       x = r
       f = 1
    end if
    if (sf_int%data%with_radiation) then
       xb1 = 1 - x
       call sf_int%split_momenta (x, xb1)
    else
       call sf_int%reduce_momenta (x)
    end if
    select case (sf_int%status)
    case (SF_FAILED_KINEMATICS);  f = 0
    end select
  end subroutine sf_test_spectrum_complete_kinematics

  subroutine sf_test_spectrum_inverse_kinematics &
       (sf_int, x, f, r, rb, map, set_momenta)
    class(sf_test_spectrum_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(in) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: r
    real(default), dimension(:), intent(out) :: rb
    logical, intent(in) :: map
    logical, intent(in), optional :: set_momenta
    real(default), dimension(2) :: xb1
    logical :: set_mom
    set_mom = .false.;  if (present (set_momenta))  set_mom = set_momenta
    if (map) then
       r = sqrt (x)
       f = 4 * r(1) * r(2)
    else
       r = x
       f = 1
    end if
    rb = 1 - r
    if (set_mom)  then
       if (sf_int%data%with_radiation) then
          xb1 = 1 - x
          call sf_int%split_momenta (x, xb1)
       else
          call sf_int%reduce_momenta (x)
       end if
       select case (sf_int%status)
       case (SF_FAILED_KINEMATICS);  f = 0
       end select
    end if
  end subroutine sf_test_spectrum_inverse_kinematics

  subroutine sf_test_spectrum_apply (sf_int, scale)
    class(sf_test_spectrum_t), intent(inout) :: sf_int
    real(default), intent(in) :: scale
    call interaction_set_matrix_element (sf_int%interaction_t, &
         cmplx (1._default, kind=default))
    sf_int%status = SF_EVALUATED
  end subroutine sf_test_spectrum_apply

  subroutine sf_test_generator_data_write (data, unit, verbose)
    class(sf_test_generator_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "SF test generator data:"
    write (u, "(3x,A,A)") "model     = ", char (data%model%get_name ())
    write (u, "(3x,A)", advance="no") "incoming  = "
    call flavor_write (data%flv_in, u);  write (u, *)
    write (u, "(3x,A)", advance="no") "outgoing  = "
    call flavor_write (data%flv_out, u);  write (u, *)
    write (u, "(3x,A," // FMT_19 // ")")  "mass      = ", data%m
  end subroutine sf_test_generator_data_write
    
  subroutine sf_test_generator_data_init (data, model, pdg_in)
    class(sf_test_generator_data_t), intent(out) :: data
    class(model_data_t), intent(in), target :: model
    type(pdg_array_t), intent(in) :: pdg_in
    data%model => model
    if (pdg_array_get (pdg_in, 1) /= 25) then
       call msg_fatal ("Test generator: input flavor must be 's'")
    end if
    call flavor_init (data%flv_in, 25, model)
    data%m = flavor_get_mass (data%flv_in)
    call flavor_init (data%flv_out, 25, model)
  end subroutine sf_test_generator_data_init
  
  function sf_test_generator_data_is_generator (data) result (flag)
    class(sf_test_generator_data_t), intent(in) :: data
    logical :: flag
    flag = .true.
  end function sf_test_generator_data_is_generator
  
  function sf_test_generator_data_get_n_par (data) result (n)
    class(sf_test_generator_data_t), intent(in) :: data
    integer :: n
    n = 2
  end function sf_test_generator_data_get_n_par
  
  subroutine sf_test_generator_data_get_pdg_out (data, pdg_out)
    class(sf_test_generator_data_t), intent(in) :: data
    type(pdg_array_t), dimension(:), intent(inout) :: pdg_out
    pdg_out(1) = 25
    pdg_out(2) = 25
  end subroutine sf_test_generator_data_get_pdg_out
  
  subroutine sf_test_generator_data_allocate_sf_int (data, sf_int)
    class(sf_test_generator_data_t), intent(in) :: data
    class(sf_int_t), intent(inout), allocatable :: sf_int
    allocate (sf_test_generator_t :: sf_int)
  end subroutine sf_test_generator_data_allocate_sf_int
    
  function sf_test_generator_type_string (object) result (string)
    class(sf_test_generator_t), intent(in) :: object
    type(string_t) :: string
    string = "Test Generator"
  end function sf_test_generator_type_string
  
  subroutine sf_test_generator_write (object, unit, testflag)
    class(sf_test_generator_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = given_output_unit (unit)
    if (associated (object%data)) then
       call object%data%write (u)
       call object%base_write (u, testflag)
    else
       write (u, "(1x,A)")  "SF test generator data: [undefined]"
    end if
  end subroutine sf_test_generator_write
    
  subroutine sf_test_generator_init (sf_int, data)
    class(sf_test_generator_t), intent(out) :: sf_int
    class(sf_data_t), intent(in), target :: data
    type(quantum_numbers_mask_t), dimension(4) :: mask
    type(helicity_t) :: hel0
    type(quantum_numbers_t), dimension(4) :: qn
    mask = new_quantum_numbers_mask (.false., .false., .false.)
    select type (data)
    type is (sf_test_generator_data_t)
       call sf_int%base_init (mask(1:4), &
            [data%m**2, data%m**2], &
            [real(default) :: ], &
            [data%m**2, data%m**2])
       sf_int%data => data
       call helicity_init (hel0, 0)
       call quantum_numbers_init (qn(1), data%flv_in,  hel0)
       call quantum_numbers_init (qn(2), data%flv_in,  hel0)
       call quantum_numbers_init (qn(3), data%flv_out, hel0)
       call quantum_numbers_init (qn(4), data%flv_out, hel0)
       call interaction_add_state (sf_int%interaction_t, qn(1:4))
       call sf_int%set_incoming ([1,2])
       call sf_int%set_outgoing ([3,4])
       call interaction_freeze (sf_int%interaction_t)
    end select
    sf_int%status = SF_INITIAL
  end subroutine sf_test_generator_init

  function sf_test_generator_is_generator (sf_int) result (flag)
    class(sf_test_generator_t), intent(in) :: sf_int
    logical :: flag
    flag = sf_int%data%is_generator ()
  end function sf_test_generator_is_generator
  
  subroutine sf_test_generator_generate_free (sf_int, r, rb,  x_free)
    class(sf_test_generator_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: r, rb
    real(default), intent(inout) :: x_free
    r = [0.8, 0.5]
    rb= 1 - r
    x_free = x_free * product (r)
  end subroutine sf_test_generator_generate_free
    
  subroutine sf_test_generator_recover_x (sf_int, x, x_free)
    class(sf_test_generator_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: x
    real(default), intent(inout), optional :: x_free
    call sf_int%base_recover_x (x)
    if (present (x_free))  x_free = x_free * product (x)
  end subroutine sf_test_generator_recover_x
  
  subroutine sf_test_generator_complete_kinematics (sf_int, x, f, r, rb, map)
    class(sf_test_generator_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: r
    real(default), dimension(:), intent(in) :: rb
    logical, intent(in) :: map
    x = r
    f = 1
    call sf_int%reduce_momenta (x)
  end subroutine sf_test_generator_complete_kinematics

  subroutine sf_test_generator_inverse_kinematics &
       (sf_int, x, f, r, rb, map, set_momenta)
    class(sf_test_generator_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(in) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: r
    real(default), dimension(:), intent(out) :: rb
    logical, intent(in) :: map
    logical, intent(in), optional :: set_momenta
    logical :: set_mom
    set_mom = .false.;  if (present (set_momenta))  set_mom = set_momenta
    r = x
    rb= 1 - x
    f = 1
    if (set_mom)  call sf_int%reduce_momenta (x)
  end subroutine sf_test_generator_inverse_kinematics

  subroutine sf_test_generator_apply (sf_int, scale)
    class(sf_test_generator_t), intent(inout) :: sf_int
    real(default), intent(in) :: scale
    call interaction_set_matrix_element (sf_int%interaction_t, &
         cmplx (1._default, kind=default))
    sf_int%status = SF_EVALUATED
  end subroutine sf_test_generator_apply


  subroutine sf_base_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (sf_base_1, "sf_base_1", &
         "structure function configuration", &
         u, results)
    call test (sf_base_2, "sf_base_2", &
         "structure function instance", &
         u, results)
    call test (sf_base_3, "sf_base_3", &
         "alternatives for collinear kinematics", &
         u, results)
    call test (sf_base_4, "sf_base_4", &
         "alternatives for non-collinear kinematics", &
         u, results)
    call test (sf_base_5, "sf_base_5", &
         "pair spectrum with radiation", &
         u, results)
    call test (sf_base_6, "sf_base_6", &
         "pair spectrum without radiation", &
         u, results)
    call test (sf_base_7, "sf_base_7", &
         "direct access", &
         u, results)
    call test (sf_base_8, "sf_base_8", &
         "structure function chain configuration", &
         u, results)
    call test (sf_base_9, "sf_base_9", &
         "structure function chain instance", &
         u, results)
   call test (sf_base_10, "sf_base_10", &
        "structure function chain mapping", &
        u, results)
    call test (sf_base_11, "sf_base_11", &
         "structure function chain evaluation", &
         u, results)
    call test (sf_base_12, "sf_base_12", &
         "multi-channel structure function chain", &
         u, results)
    call test (sf_base_13, "sf_base_13", &
         "pair spectrum generator", &
         u, results)
    call test (sf_base_14, "sf_base_14", &
         "structure function generator evaluation", &
         u, results)
  end subroutine sf_base_test
  
  subroutine sf_base_1 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(pdg_array_t) :: pdg_in
    type(pdg_array_t), dimension(1) :: pdg_out
    integer, dimension(:), allocatable :: pdg1
    class(sf_data_t), allocatable :: data
    
    write (u, "(A)")  "* Test output: sf_base_1"
    write (u, "(A)")  "*   Purpose: initialize and display &
         &test structure function data"
    write (u, "(A)")
    
    call model%init_test ()
    pdg_in = 25

    allocate (sf_test_data_t :: data)
    select type (data)
    type is (sf_test_data_t)
       call data%init (model, pdg_in)
    end select
       
    call data%write (u)

    write (u, "(A)") 

    write (u, "(1x,A)")  "Outgoing particle code:"
    call data%get_pdg_out (pdg_out)
    pdg1 = pdg_out(1)
    write (u, "(2x,99(1x,I0))")  pdg1
    
    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_base_1"

  end subroutine sf_base_1

  subroutine sf_base_2 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(flavor_t) :: flv
    type(pdg_array_t) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    class(sf_int_t), allocatable :: sf_int
    type(vector4_t) :: k
    type(vector4_t), dimension(2) :: q
    real(default) :: E
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: f
    
    write (u, "(A)")  "* Test output: sf_base_2"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &test structure function object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call model%init_test ()
    pdg_in = 25
    call flavor_init (flv, 25, model)

    call reset_interaction_counter ()
    
    allocate (sf_test_data_t :: data)
    select type (data)
    type is (sf_test_data_t)
       call data%init (model, pdg_in)
    end select
       
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1])
    
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize incoming momentum with E=500"
    write (u, "(A)")
    E = 500
    k = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    call vector4_write (k, u)
    call sf_int%seed_kinematics ([k])

    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0"
    write (u, "(A)")

    allocate (r (data%get_n_par ()))
    allocate (rb(size (r)))
    allocate (x (size (r)))

    r = 0
    rb = 1 - r
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "f =", f

    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=1"
    write (u, "(A)")

    r = 1
    rb = 1 - r
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "f =", f

    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.5"
    write (u, "(A)")

    r = 0.5_default
    rb = 1 - r
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "f =", f

    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics with mapping for r=0.8"
    write (u, "(A)")

    r = 0.8_default
    rb = 1 - r
    call sf_int%complete_kinematics (x, f, r, rb, map=.true.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "f =", f

    write (u, "(A)")
    write (u, "(A)")  "* Recover x from momenta"
    write (u, "(A)")

    q = interaction_get_momenta (sf_int%interaction_t, outgoing=.true.)
    call sf_int%final ()
    deallocate (sf_int)

    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1])

    call sf_int%seed_kinematics ([k])
    call interaction_set_momenta (sf_int%interaction_t, q, outgoing=.true.)
    call sf_int%recover_x (x)

    write (u, "(A,9(1x,F10.7))")  "x =", x

    write (u, "(A)")
    write (u, "(A)")  "* Compute inverse kinematics for x=0.64 and evaluate"
    write (u, "(A)")

    x = 0.64_default
    call sf_int%inverse_kinematics (x, f, r, rb, map=.true.)
    call sf_int%apply (scale=0._default)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A,9(1x,F10.7))")  "r =", r
    write (u, "(A,9(1x,F10.7))")  "f =", f

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call sf_int%final ()
    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_base_2"

  end subroutine sf_base_2

  subroutine sf_base_3 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(pdg_array_t) :: pdg_in
    type(flavor_t) :: flv
    class(sf_data_t), allocatable, target :: data
    class(sf_int_t), allocatable :: sf_int
    type(vector4_t) :: k
    real(default) :: E
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: f
    
    write (u, "(A)")  "* Test output: sf_base_3"
    write (u, "(A)")  "*   Purpose: check various kinematical setups"
    write (u, "(A)")  "*            for collinear structure-function splitting."
    write (u, "(A)")  "             (two masses equal, one zero)"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call model%init_test ()
    pdg_in = 25
    call flavor_init (flv, 25, model)

    call reset_interaction_counter ()
    
    allocate (sf_test_data_t :: data)
    select type (data)
    type is (sf_test_data_t)
       call data%init (model, pdg_in)
    end select
       
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    
    call sf_int%write (u)

    allocate (r (data%get_n_par ()))
    allocate (rb(size (r)))
    allocate (x (size (r)))

    write (u, "(A)")
    write (u, "(A)")  "* Initialize incoming momentum with E=500"

    E = 500
    k = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    call sf_int%seed_kinematics ([k])

    write (u, "(A)")
    write (u, "(A)")  "* Set radiated mass to zero"

    sf_int%mr2 = 0
    sf_int%mo2 = sf_int%mi2
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.5, keeping energy"
    write (u, "(A)")

    r = 0.5_default
    rb = 1 - r
    sf_int%on_shell_mode = KEEP_ENERGY
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover x and r"
    write (u, "(A)")

    call sf_int%recover_x (x)
    call sf_int%inverse_kinematics (x, f, r, rb, map=.false.)
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "r =", r
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.5, keeping momentum"
    write (u, "(A)")

    r = 0.5_default
    rb = 1 - r
    sf_int%on_shell_mode = KEEP_MOMENTUM
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover x and r"
    write (u, "(A)")

    call sf_int%recover_x (x)
    call sf_int%inverse_kinematics (x, f, r, rb, map=.false.)
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "r =", r
    
    write (u, "(A)")
    write (u, "(A)")  "* Set outgoing mass to zero"

    sf_int%mr2 = sf_int%mi2
    sf_int%mo2 = 0
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.5, keeping energy"
    write (u, "(A)")

    r = 0.5_default
    rb = 1 - r
    sf_int%on_shell_mode = KEEP_ENERGY
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover x and r"
    write (u, "(A)")

    call sf_int%recover_x (x)
    call sf_int%inverse_kinematics (x, f, r, rb, map=.false.)
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "r =", r
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.5, keeping momentum"
    write (u, "(A)")

    r = 0.5_default
    rb = 1 - r
    sf_int%on_shell_mode = KEEP_MOMENTUM
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover x and r"
    write (u, "(A)")

    call sf_int%recover_x (x)
    call sf_int%inverse_kinematics (x, f, r, rb, map=.false.)
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "r =", r
    
    write (u, "(A)")
    write (u, "(A)")  "* Set incoming mass to zero"

    k = vector4_moving (E, E, 3)
    call sf_int%seed_kinematics ([k])

    sf_int%mr2 = sf_int%mi2
    sf_int%mo2 = sf_int%mi2
    sf_int%mi2 = 0
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.5, keeping energy"
    write (u, "(A)")

    r = 0.5_default
    rb = 1 - r
    sf_int%on_shell_mode = KEEP_ENERGY
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover x and r"
    write (u, "(A)")

    call sf_int%recover_x (x)
    call sf_int%inverse_kinematics (x, f, r, rb, map=.false.)
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "r =", r
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.5, keeping momentum"
    write (u, "(A)")

    r = 0.5_default
    rb = 1 - r
    sf_int%on_shell_mode = KEEP_MOMENTUM
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover x and r"
    write (u, "(A)")

    call sf_int%recover_x (x)
    call sf_int%inverse_kinematics (x, f, r, rb, map=.false.)
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "r =", r
    
    write (u, "(A)")
    write (u, "(A)")  "* Set all masses to zero"

    sf_int%mr2 = 0
    sf_int%mo2 = 0
    sf_int%mi2 = 0
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.5, keeping energy"
    write (u, "(A)")

    r = 0.5_default
    rb = 1 - r
    sf_int%on_shell_mode = KEEP_ENERGY
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover x and r"
    write (u, "(A)")

    call sf_int%recover_x (x)
    call sf_int%inverse_kinematics (x, f, r, rb, map=.false.)
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "r =", r
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.5, keeping momentum"
    write (u, "(A)")

    r = 0.5_default
    rb = 1 - r
    sf_int%on_shell_mode = KEEP_MOMENTUM
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover x and r"
    write (u, "(A)")

    call sf_int%recover_x (x)
    call sf_int%inverse_kinematics (x, f, r, rb, map=.false.)
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "r =", r
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call sf_int%final ()
    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_base_3"

  end subroutine sf_base_3

  subroutine sf_base_4 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(pdg_array_t) :: pdg_in
    type(flavor_t) :: flv
    class(sf_data_t), allocatable, target :: data
    class(sf_int_t), allocatable :: sf_int
    type(vector4_t) :: k
    real(default) :: E
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: f
    
    write (u, "(A)")  "* Test output: sf_base_4"
    write (u, "(A)")  "*   Purpose: check various kinematical setups"
    write (u, "(A)")  "*            for free structure-function splitting."
    write (u, "(A)")  "             (two masses equal, one zero)"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call model%init_test ()
    pdg_in = 25
    call flavor_init (flv, 25, model)

    call reset_interaction_counter ()
    
    allocate (sf_test_data_t :: data)
    select type (data)
    type is (sf_test_data_t)
       call data%init (model, pdg_in, collinear=.false.)
    end select
       
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    
    call sf_int%write (u)

    allocate (r (data%get_n_par ()))
    allocate (rb(size (r)))
    allocate (x (size (r)))

    write (u, "(A)")
    write (u, "(A)")  "* Initialize incoming momentum with E=500"

    E = 500
    k = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    call sf_int%seed_kinematics ([k])

    write (u, "(A)")
    write (u, "(A)")  "* Set radiated mass to zero"

    sf_int%mr2 = 0
    sf_int%mo2 = sf_int%mi2
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.5/0.5/0.125, keeping energy"
    write (u, "(A)")

    r = [0.5_default, 0.5_default, 0.125_default]
    rb = 1 - r
    sf_int%on_shell_mode = KEEP_ENERGY
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover x and r"
    write (u, "(A)")

    call sf_int%recover_x (x)
    call sf_int%inverse_kinematics (x, f, r, rb, map=.false.)
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "r =", r
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.5/0.5/0.125, keeping momentum"
    write (u, "(A)")

    r = [0.5_default, 0.5_default, 0.125_default]
    rb = 1 - r
    sf_int%on_shell_mode = KEEP_MOMENTUM
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover x and r"
    write (u, "(A)")

    call sf_int%recover_x (x)
    call sf_int%inverse_kinematics (x, f, r, rb, map=.false.)
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "r =", r
    
    write (u, "(A)")
    write (u, "(A)")  "* Set outgoing mass to zero"

    sf_int%mr2 = sf_int%mi2
    sf_int%mo2 = 0
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.5/0.5/0.125, keeping energy"
    write (u, "(A)")

    r = [0.5_default, 0.5_default, 0.125_default]
    rb = 1 - r
    sf_int%on_shell_mode = KEEP_ENERGY
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover x and r"
    write (u, "(A)")

    call sf_int%recover_x (x)
    call sf_int%inverse_kinematics (x, f, r, rb, map=.false.)
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "r =", r
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.5/0.5/0.125, keeping momentum"
    write (u, "(A)")

    r = [0.5_default, 0.5_default, 0.125_default]
    rb = 1 - r
    sf_int%on_shell_mode = KEEP_MOMENTUM
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover x and r"
    write (u, "(A)")

    call sf_int%recover_x (x)
    call sf_int%inverse_kinematics (x, f, r, rb, map=.false.)
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "r =", r
    
    write (u, "(A)")
    write (u, "(A)")  "* Set incoming mass to zero"

    k = vector4_moving (E, E, 3)
    call sf_int%seed_kinematics ([k])

    sf_int%mr2 = sf_int%mi2
    sf_int%mo2 = sf_int%mi2
    sf_int%mi2 = 0
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.5/0.5/0.125, keeping energy"
    write (u, "(A)")

    r = [0.5_default, 0.5_default, 0.125_default]
    rb = 1 - r
    sf_int%on_shell_mode = KEEP_ENERGY
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover x and r"
    write (u, "(A)")

    call sf_int%recover_x (x)
    call sf_int%inverse_kinematics (x, f, r, rb, map=.false.)
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "r =", r
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.5/0.5/0.125, keeping momentum"
    write (u, "(A)")

    r = [0.5_default, 0.5_default, 0.125_default]
    rb = 1 - r
    sf_int%on_shell_mode = KEEP_MOMENTUM
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover x and r"
    write (u, "(A)")

    call sf_int%recover_x (x)
    call sf_int%inverse_kinematics (x, f, r, rb, map=.false.)
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "r =", r
    
    write (u, "(A)")
    write (u, "(A)")  "* Set all masses to zero"

    sf_int%mr2 = 0
    sf_int%mo2 = 0
    sf_int%mi2 = 0
    
    write (u, "(A)")
    write (u, "(A)")  "* Re-Initialize structure-function object with Q bounds"
    
    call reset_interaction_counter ()
    
    select type (data)
    type is (sf_test_data_t)
       call data%init (model, pdg_in, collinear=.false., &
            qbounds = [1._default, 100._default])
    end select
       
    call sf_int%init (data)
    call sf_int%seed_kinematics ([k])

    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.5/0.5/0.125, keeping energy"
    write (u, "(A)")

    r = [0.5_default, 0.5_default, 0.125_default]
    rb = 1 - r
    sf_int%on_shell_mode = KEEP_ENERGY
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover x and r"
    write (u, "(A)")

    call sf_int%recover_x (x)
    call sf_int%inverse_kinematics (x, f, r, rb, map=.false.)
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "r =", r
    
    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.5/0.5/0.125, keeping momentum"
    write (u, "(A)")

    r = [0.5_default, 0.5_default, 0.125_default]
    rb = 1 - r
    sf_int%on_shell_mode = KEEP_MOMENTUM
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover x and r"
    write (u, "(A)")

    call sf_int%recover_x (x)
    call sf_int%inverse_kinematics (x, f, r, rb, map=.false.)
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "r =", r
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call sf_int%final ()
    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_base_4"

  end subroutine sf_base_4

  subroutine sf_base_5 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(pdg_array_t) :: pdg_in
    type(pdg_array_t), dimension(2) :: pdg_out
    integer, dimension(:), allocatable :: pdg1, pdg2
    type(flavor_t) :: flv
    class(sf_data_t), allocatable, target :: data
    class(sf_int_t), allocatable :: sf_int
    type(vector4_t), dimension(2) :: k
    type(vector4_t), dimension(4) :: q
    real(default) :: E
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: f
    
    write (u, "(A)")  "* Test output: sf_base_5"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &a pair spectrum object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call model%init_test ()
    call flavor_init (flv, 25, model)
    pdg_in = 25

    call reset_interaction_counter ()
    
    allocate (sf_test_spectrum_data_t :: data)
    select type (data)
    type is (sf_test_spectrum_data_t)
       call data%init (model, pdg_in, with_radiation=.true.)
    end select
       
    write (u, "(1x,A)")  "Outgoing particle codes:"
    call data%get_pdg_out (pdg_out)
    pdg1 = pdg_out(1)
    pdg2 = pdg_out(2)
    write (u, "(2x,99(1x,I0))")  pdg1, pdg2
    
    write (u, "(A)") 
    write (u, "(A)")  "* Initialize spectrum object"
    write (u, "(A)")
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize incoming momenta with sqrts=1000"

    E = 500
    k(1) = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    k(2) = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    call sf_int%seed_kinematics (k)

    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.4,0.8"
    write (u, "(A)")

    allocate (r (data%get_n_par ()))
    allocate (rb(size (r)))
    allocate (x (size (r)))

    r = [0.4_default, 0.8_default]
    rb = 1 - r
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "f =", f

    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics with mapping for r=0.6,0.8"
    write (u, "(A)")

    r = [0.6_default, 0.8_default]
    rb = 1 - r
    call sf_int%complete_kinematics (x, f, r, rb, map=.true.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "f =", f

    write (u, "(A)")
    write (u, "(A)")  "* Recover x from momenta"
    write (u, "(A)")

    q = interaction_get_momenta (sf_int%interaction_t, outgoing=.true.)
    call sf_int%final ()
    deallocate (sf_int)

    call reset_interaction_counter ()
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)

    call sf_int%seed_kinematics (k)
    call interaction_set_momenta (sf_int%interaction_t, q, outgoing=.true.)
    call sf_int%recover_x (x)
    write (u, "(A,9(1x,F10.7))")  "x =", x

    write (u, "(A)")
    write (u, "(A)")  "* Compute inverse kinematics for x=0.36,0.64 &
         &and evaluate"
    write (u, "(A)")

    x = [0.36_default, 0.64_default]
    call sf_int%inverse_kinematics (x, f, r, rb, map=.true.)
    call sf_int%apply (scale=0._default)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A,9(1x,F10.7))")  "r =", r
    write (u, "(A,9(1x,F10.7))")  "f =", f

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call sf_int%final ()
    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_base_5"

  end subroutine sf_base_5

  subroutine sf_base_6 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(pdg_array_t) :: pdg_in
    type(flavor_t) :: flv
    class(sf_data_t), allocatable, target :: data
    class(sf_int_t), allocatable :: sf_int
    type(vector4_t), dimension(2) :: k
    type(vector4_t), dimension(2) :: q
    real(default) :: E
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: f
    
    write (u, "(A)")  "* Test output: sf_base_6"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &a pair spectrum object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call model%init_test ()
    call flavor_init (flv, 25, model)
    pdg_in = 25

    call reset_interaction_counter ()
    
    allocate (sf_test_spectrum_data_t :: data)
    select type (data)
    type is (sf_test_spectrum_data_t)
       call data%init (model, pdg_in, with_radiation=.false.)
    end select
       
    write (u, "(A)")  "* Initialize spectrum object"
    write (u, "(A)")
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    
    write (u, "(A)")  "* Initialize incoming momenta with sqrts=1000"

    E = 500
    k(1) = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    k(2) = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    call sf_int%seed_kinematics (k)

    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.4,0.8"
    write (u, "(A)")

    allocate (r (data%get_n_par ()))
    allocate (rb(size (r)))
    allocate (x (size (r)))

    r = [0.4_default, 0.8_default]
    rb = 1 - r
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "f =", f

    write (u, "(A)")
    write (u, "(A)")  "* Recover x from momenta"
    write (u, "(A)")

    q = interaction_get_momenta (sf_int%interaction_t, outgoing=.true.)
    call sf_int%final ()
    deallocate (sf_int)

    call reset_interaction_counter ()
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)

    call sf_int%seed_kinematics (k)
    call interaction_set_momenta (sf_int%interaction_t, q, outgoing=.true.)
    call sf_int%recover_x (x)
    write (u, "(A,9(1x,F10.7))")  "x =", x

    write (u, "(A)")
    write (u, "(A)")  "* Compute inverse kinematics for x=0.4,0.8 &
         &and evaluate"
    write (u, "(A)")

    x = [0.4_default, 0.8_default]
    call sf_int%inverse_kinematics (x, f, r, rb, map=.false.)
    call sf_int%apply (scale=0._default)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A,9(1x,F10.7))")  "r =", r
    write (u, "(A,9(1x,F10.7))")  "f =", f

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call sf_int%final ()
    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_base_6"

  end subroutine sf_base_6

  subroutine sf_base_7 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(pdg_array_t) :: pdg_in
    type(flavor_t) :: flv
    class(sf_data_t), allocatable, target :: data
    class(sf_int_t), allocatable :: sf_int
    real(default), dimension(:), allocatable :: value
    
    write (u, "(A)")  "* Test output: sf_base_7"
    write (u, "(A)")  "*   Purpose: check direct access method"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call model%init_test ()
    call flavor_init (flv, 25, model)
    pdg_in = 25

    call reset_interaction_counter ()
    
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")
    
    allocate (sf_test_data_t :: data)
    select type (data)
    type is (sf_test_data_t)
       call data%init (model, pdg_in)
    end select
       
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)

    write (u, "(A)")  "* Probe structure function: states"
    write (u, "(A)")
    
    write (u, "(A,I0)")  "n_states = ", sf_int%get_n_states ()
    write (u, "(A,I0)")  "n_in     = ", sf_int%get_n_in ()
    write (u, "(A,I0)")  "n_rad    = ", sf_int%get_n_rad ()
    write (u, "(A,I0)")  "n_out    = ", sf_int%get_n_out ()
    write (u, "(A)")
    write (u, "(A)", advance="no")  "state(1)  = "
    call quantum_numbers_write (sf_int%get_state (1), u)
    write (u, *)
    
    allocate (value (sf_int%get_n_states ()))
    call sf_int%compute_values (value, &
         E=[500._default], x=[0.5_default], xb=[0.5_default], scale=0._default)

    write (u, "(A)")
    write (u, "(A)", advance="no")  "value (E=500, x=0.5) ="
    write (u, "(9(1x," // FMT_19 // "))")  value

    call sf_int%compute_values (value, &
         x=[0.1_default], xb=[0.9_default], scale=0._default)

    write (u, "(A)")
    write (u, "(A)", advance="no")  "value (E=500, x=0.1) ="
    write (u, "(9(1x," // FMT_19 // "))")  value


    write (u, "(A)")
    write (u, "(A)")  "* Initialize spectrum object"
    write (u, "(A)")
    
    deallocate (value)
    call sf_int%final ()
    deallocate (sf_int)
    deallocate (data)
    
    allocate (sf_test_spectrum_data_t :: data)
    select type (data)
    type is (sf_test_spectrum_data_t)
       call data%init (model, pdg_in, with_radiation=.false.)
    end select
       
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)

    write (u, "(A)")  "* Probe spectrum: states"
    write (u, "(A)")
    
    write (u, "(A,I0)")  "n_states = ", sf_int%get_n_states ()
    write (u, "(A,I0)")  "n_in     = ", sf_int%get_n_in ()
    write (u, "(A,I0)")  "n_rad    = ", sf_int%get_n_rad ()
    write (u, "(A,I0)")  "n_out    = ", sf_int%get_n_out ()
    write (u, "(A)")
    write (u, "(A)", advance="no")  "state(1)  = "
    call quantum_numbers_write (sf_int%get_state (1), u)
    write (u, *)
    
    allocate (value (sf_int%get_n_states ()))
    call sf_int%compute_value (1, value(1), &
         E = [500._default, 500._default], &
         x = [0.5_default, 0.6_default], &
         xb= [0.5_default, 0.4_default], &
         scale = 0._default)

    write (u, "(A)")
    write (u, "(A)", advance="no")  "value (E=500,500, x=0.5,0.6) ="
    write (u, "(9(1x," // FMT_19 // "))")  value

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call sf_int%final ()
    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_base_7"

  end subroutine sf_base_7

  subroutine sf_base_8 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(flavor_t) :: flv
    type(pdg_array_t) :: pdg_in
    type(beam_data_t), target :: beam_data
    class(sf_data_t), allocatable, target :: data_strfun
    class(sf_data_t), allocatable, target :: data_spectrum
    type(sf_config_t), dimension(:), allocatable :: sf_config
    type(sf_chain_t) :: sf_chain
    
    write (u, "(A)")  "* Test output: sf_base_8"
    write (u, "(A)")  "*   Purpose: set up a structure-function chain"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call model%init_test ()
    call flavor_init (flv, 25, model)
    pdg_in = 25

    call reset_interaction_counter ()
    
    call beam_data_init_sqrts (beam_data, &
         1000._default, [flv, flv])

    allocate (sf_test_data_t :: data_strfun)
    select type (data_strfun)
    type is (sf_test_data_t)
       call data_strfun%init (model, pdg_in)
    end select
       
    allocate (sf_test_spectrum_data_t :: data_spectrum)
    select type (data_spectrum)
    type is (sf_test_spectrum_data_t)
       call data_spectrum%init (model, pdg_in, with_radiation=.true.)
    end select
       
    write (u, "(A)")  "* Set up chain with beams only"
    write (u, "(A)")
    
    call sf_chain%init (beam_data)
    call write_separator (u, 2)
    call sf_chain%write (u)
    call write_separator (u, 2)
    call sf_chain%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Set up chain with structure function"
    write (u, "(A)")
    
    allocate (sf_config (1))
    call sf_config(1)%init ([1], data_strfun)
    call sf_chain%init (beam_data, sf_config)

    call write_separator (u, 2)
    call sf_chain%write (u)
    call write_separator (u, 2)
    call sf_chain%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Set up chain with spectrum and structure function"
    write (u, "(A)")
    
    deallocate (sf_config)
    allocate (sf_config (2))
    call sf_config(1)%init ([1,2], data_spectrum)
    call sf_config(2)%init ([2], data_strfun)
    call sf_chain%init (beam_data, sf_config)

    call write_separator (u, 2)
    call sf_chain%write (u)
    call write_separator (u, 2)
    call sf_chain%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_base_8"

  end subroutine sf_base_8

  subroutine sf_base_9 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(flavor_t) :: flv
    type(pdg_array_t) :: pdg_in
    type(beam_data_t), target :: beam_data
    class(sf_data_t), allocatable, target :: data_strfun
    class(sf_data_t), allocatable, target :: data_spectrum
    type(sf_config_t), dimension(:), allocatable, target :: sf_config
    type(sf_chain_t), target :: sf_chain
    type(sf_chain_instance_t), target :: sf_chain_instance
    type(sf_channel_t), dimension(2) :: sf_channel
    type(vector4_t), dimension(2) :: p
    integer :: j
    
    write (u, "(A)")  "* Test output: sf_base_9"
    write (u, "(A)")  "*   Purpose: set up a structure-function chain &
         &and create an instance"
    write (u, "(A)")  "*            compute kinematics"
    write (u, "(A)")
   
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call model%init_test ()
    call flavor_init (flv, 25, model)
    pdg_in = 25

    call reset_interaction_counter ()
    
    call beam_data_init_sqrts (beam_data, &
         1000._default, [flv, flv])

    allocate (sf_test_data_t :: data_strfun)
    select type (data_strfun)
    type is (sf_test_data_t)
       call data_strfun%init (model, pdg_in)
    end select
       
    allocate (sf_test_spectrum_data_t :: data_spectrum)
    select type (data_spectrum)
    type is (sf_test_spectrum_data_t)
       call data_spectrum%init (model, pdg_in, with_radiation=.true.)
    end select
       
    write (u, "(A)")  "* Set up chain with beams only"
    write (u, "(A)")
    
    call sf_chain%init (beam_data)

    call sf_chain_instance%init (sf_chain, n_channel = 1)

    call sf_chain_instance%link_interactions ()
    sf_chain_instance%status = SF_DONE_CONNECTIONS
    call sf_chain_instance%compute_kinematics (1, [real(default) ::])

    call write_separator (u, 2)
    call sf_chain%write (u)
    call write_separator (u, 2)
    call sf_chain_instance%write (u)
    call write_separator (u, 2)

    call sf_chain_instance%get_out_momenta (p)
    
    write (u, "(A)")
    write (u, "(A)")  "* Outgoing momenta:"
    
    do j = 1, 2
       write (u, "(A)")
       call vector4_write (p(j), u)
    end do
  
    call sf_chain_instance%final ()
    call sf_chain%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Set up chain with structure function"
    write (u, "(A)")
    
    allocate (sf_config (1))
    call sf_config(1)%init ([1], data_strfun)
    call sf_chain%init (beam_data, sf_config)

    call sf_chain_instance%init (sf_chain, n_channel = 1)
    
    call sf_channel(1)%init (1)
    call sf_channel(1)%activate_mapping ([1])
    call sf_chain_instance%set_channel (1, sf_channel(1))

    call sf_chain_instance%link_interactions ()
    sf_chain_instance%status = SF_DONE_CONNECTIONS
    call sf_chain_instance%compute_kinematics (1, [0.8_default])
    
    call write_separator (u, 2)
    call sf_chain%write (u)
    call write_separator (u, 2)
    call sf_chain_instance%write (u)
    call write_separator (u, 2)

    call sf_chain_instance%get_out_momenta (p)
    
    write (u, "(A)")
    write (u, "(A)")  "* Outgoing momenta:"
    
    do j = 1, 2
       write (u, "(A)")
       call vector4_write (p(j), u)
    end do
    
    call sf_chain_instance%final ()
    call sf_chain%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Set up chain with spectrum and structure function"
    write (u, "(A)")
    
    deallocate (sf_config)
    allocate (sf_config (2))
    call sf_config(1)%init ([1,2], data_spectrum)
    call sf_config(2)%init ([2], data_strfun)
    call sf_chain%init (beam_data, sf_config)

    call sf_chain_instance%init (sf_chain, n_channel = 1)
    
    call sf_channel(2)%init (2)
    call sf_channel(2)%activate_mapping ([2])
    call sf_chain_instance%set_channel (1, sf_channel(2))

    call sf_chain_instance%link_interactions ()
    sf_chain_instance%status = SF_DONE_CONNECTIONS
    call sf_chain_instance%compute_kinematics &
         (1, [0.5_default, 0.6_default, 0.8_default])
    
    call write_separator (u, 2)
    call sf_chain%write (u)
    call write_separator (u, 2)
    call sf_chain_instance%write (u)
    call write_separator (u, 2)

    call sf_chain_instance%get_out_momenta (p)
    
    write (u, "(A)")
    write (u, "(A)")  "* Outgoing momenta:"
    
    do j = 1, 2
       write (u, "(A)")
       call vector4_write (p(j), u)
    end do
    
    call sf_chain_instance%final ()
    call sf_chain%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_base_9"

  end subroutine sf_base_9

  subroutine sf_base_10 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(flavor_t) :: flv
    type(pdg_array_t) :: pdg_in
    type(beam_data_t), target :: beam_data
    class(sf_data_t), allocatable, target :: data_strfun
    type(sf_config_t), dimension(:), allocatable, target :: sf_config
    type(sf_chain_t), target :: sf_chain
    type(sf_chain_instance_t), target :: sf_chain_instance
    type(sf_channel_t), dimension(2) :: sf_channel
    real(default), dimension(2) :: x_saved
    
    write (u, "(A)")  "* Test output: sf_base_10"
    write (u, "(A)")  "*   Purpose: set up a structure-function chain"
    write (u, "(A)")  "*            and check mappings"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call model%init_test ()
    call flavor_init (flv, 25, model)
    pdg_in = 25

    call reset_interaction_counter ()
    
    call beam_data_init_sqrts (beam_data, &
         1000._default, [flv, flv])

    allocate (sf_test_data_t :: data_strfun)
    select type (data_strfun)
    type is (sf_test_data_t)
       call data_strfun%init (model, pdg_in)
    end select
       
    write (u, "(A)")  "* Set up chain with structure function pair &
         &and standard mapping"
    write (u, "(A)")
    
    allocate (sf_config (2))
    call sf_config(1)%init ([1], data_strfun)
    call sf_config(2)%init ([2], data_strfun)
    call sf_chain%init (beam_data, sf_config)

    call sf_chain_instance%init (sf_chain, n_channel = 1)

    call sf_channel(1)%init (2)
    call sf_channel(1)%set_s_mapping ([1,2])
    call sf_chain_instance%set_channel (1, sf_channel(1))

    call sf_chain_instance%link_interactions ()
    sf_chain_instance%status = SF_DONE_CONNECTIONS
    call sf_chain_instance%compute_kinematics (1, [0.8_default, 0.6_default])
    
    call write_separator (u, 2)
    call sf_chain_instance%write (u)
    call write_separator (u, 2)

    write (u, "(A)")
    write (u, "(A)")  "* Invert the kinematics calculation"
    write (u, "(A)")

    x_saved = sf_chain_instance%x

    call sf_chain_instance%init (sf_chain, n_channel = 1)

    call sf_channel(2)%init (2)
    call sf_channel(2)%set_s_mapping ([1, 2])
    call sf_chain_instance%set_channel (1, sf_channel(2))

    call sf_chain_instance%link_interactions ()
    sf_chain_instance%status = SF_DONE_CONNECTIONS
    call sf_chain_instance%inverse_kinematics (x_saved)
    
    call write_separator (u, 2)
    call sf_chain_instance%write (u)
    call write_separator (u, 2)

    
    call sf_chain_instance%final ()
    call sf_chain%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_base_10"

  end subroutine sf_base_10

  subroutine sf_base_11 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(flavor_t) :: flv
    type(pdg_array_t) :: pdg_in
    type(beam_data_t), target :: beam_data
    class(sf_data_t), allocatable, target :: data_strfun
    class(sf_data_t), allocatable, target :: data_spectrum
    type(sf_config_t), dimension(:), allocatable, target :: sf_config
    type(sf_chain_t), target :: sf_chain
    type(sf_chain_instance_t), target :: sf_chain_instance
    type(sf_channel_t), dimension(2) :: sf_channel
    type(particle_set_t) :: pset
    type(interaction_t), pointer :: int
    logical :: ok
    
    write (u, "(A)")  "* Test output: sf_base_11"
    write (u, "(A)")  "*   Purpose: set up a structure-function chain"
    write (u, "(A)")  "*            create an instance and evaluate"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call model%init_test ()
    call flavor_init (flv, 25, model)
    pdg_in = 25

    call reset_interaction_counter ()
    
    call beam_data_init_sqrts (beam_data, &
         1000._default, [flv, flv])

    allocate (sf_test_data_t :: data_strfun)
    select type (data_strfun)
    type is (sf_test_data_t)
       call data_strfun%init (model, pdg_in)
    end select
       
    allocate (sf_test_spectrum_data_t :: data_spectrum)
    select type (data_spectrum)
    type is (sf_test_spectrum_data_t)
       call data_spectrum%init (model, pdg_in, with_radiation=.true.)
    end select
       
    write (u, "(A)")  "* Set up chain with beams only"
    write (u, "(A)")
    
    call sf_chain%init (beam_data)

    call sf_chain_instance%init (sf_chain, n_channel = 1)
    call sf_chain_instance%link_interactions ()
    call sf_chain_instance%exchange_mask ()
    call sf_chain_instance%init_evaluators ()
    
    call sf_chain_instance%compute_kinematics (1, [real(default) ::])
    call sf_chain_instance%evaluate (scale=0._default)

    call write_separator (u, 2)
    call sf_chain_instance%write (u)
    call write_separator (u, 2)

    int => sf_chain_instance%get_out_int_ptr ()
    call particle_set_init (pset, ok, int, int, FM_IGNORE_HELICITY, &
         [0._default, 0._default], .false., .true.)
    call sf_chain_instance%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Particle content:"
    write (u, "(A)")

    call write_separator (u)
    call particle_set_write (pset, u)
    call write_separator (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover chain:"
    write (u, "(A)")
    
    call sf_chain_instance%init (sf_chain, n_channel = 1)
    call sf_chain_instance%link_interactions ()
    call sf_chain_instance%exchange_mask ()
    call sf_chain_instance%init_evaluators ()

    int => sf_chain_instance%get_out_int_ptr ()
    call particle_set_fill_interaction (pset, int, 2)

    call sf_chain_instance%recover_kinematics (1)
    call sf_chain_instance%evaluate (scale=0._default)

    call write_separator (u, 2)
    call sf_chain_instance%write (u)
    call write_separator (u, 2)

    call particle_set_final (pset)
    call sf_chain_instance%final ()
    call sf_chain%final ()

    write (u, "(A)")
    write (u, "(A)")
    write (u, "(A)")
    write (u, "(A)")  "* Set up chain with structure function"
    write (u, "(A)")
    
    allocate (sf_config (1))
    call sf_config(1)%init ([1], data_strfun)
    call sf_chain%init (beam_data, sf_config)

    call sf_chain_instance%init (sf_chain, n_channel = 1)
    call sf_channel(1)%init (1)
    call sf_channel(1)%activate_mapping ([1])
    call sf_chain_instance%set_channel (1, sf_channel(1))
    call sf_chain_instance%link_interactions ()
    call sf_chain_instance%exchange_mask ()
    call sf_chain_instance%init_evaluators ()

    call sf_chain_instance%compute_kinematics (1, [0.8_default])
    call sf_chain_instance%evaluate (scale=0._default)
    
    call write_separator (u, 2)
    call sf_chain_instance%write (u)
    call write_separator (u, 2)

    int => sf_chain_instance%get_out_int_ptr ()
    call particle_set_init (pset, ok, int, int, FM_IGNORE_HELICITY, &
         [0._default, 0._default], .false., .true.)
    call sf_chain_instance%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Particle content:"
    write (u, "(A)")

    call write_separator (u)
    call particle_set_write (pset, u)
    call write_separator (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover chain:"
    write (u, "(A)")
    
    call sf_chain_instance%init (sf_chain, n_channel = 1)
    call sf_channel(1)%init (1)
    call sf_channel(1)%activate_mapping ([1])
    call sf_chain_instance%set_channel (1, sf_channel(1))
    call sf_chain_instance%link_interactions ()
    call sf_chain_instance%exchange_mask ()
    call sf_chain_instance%init_evaluators ()

    int => sf_chain_instance%get_out_int_ptr ()
    call particle_set_fill_interaction (pset, int, 2)

    call sf_chain_instance%recover_kinematics (1)
    call sf_chain_instance%evaluate (scale=0._default)

    call write_separator (u, 2)
    call sf_chain_instance%write (u)
    call write_separator (u, 2)

    call particle_set_final (pset)
    call sf_chain_instance%final ()
    call sf_chain%final ()

    write (u, "(A)")
    write (u, "(A)")
    write (u, "(A)")
    write (u, "(A)")  "* Set up chain with spectrum and structure function"
    write (u, "(A)")
    
    deallocate (sf_config)
    allocate (sf_config (2))
    call sf_config(1)%init ([1,2], data_spectrum)
    call sf_config(2)%init ([2], data_strfun)
    call sf_chain%init (beam_data, sf_config)
    
    call sf_chain_instance%init (sf_chain, n_channel = 1)
    call sf_channel(2)%init (2)
    call sf_channel(2)%activate_mapping ([2])
    call sf_chain_instance%set_channel (1, sf_channel(2))
    call sf_chain_instance%link_interactions ()
    call sf_chain_instance%exchange_mask ()
    call sf_chain_instance%init_evaluators ()

    call sf_chain_instance%compute_kinematics &
         (1, [0.5_default, 0.6_default, 0.8_default])
    call sf_chain_instance%evaluate (scale=0._default)
    
    call write_separator (u, 2)
    call sf_chain_instance%write (u)
    call write_separator (u, 2)

    int => sf_chain_instance%get_out_int_ptr ()
    call particle_set_init (pset, ok, int, int, FM_IGNORE_HELICITY, &
         [0._default, 0._default], .false., .true.)
    call sf_chain_instance%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Particle content:"
    write (u, "(A)")

    call write_separator (u)
    call particle_set_write (pset, u)
    call write_separator (u)

    write (u, "(A)")
    write (u, "(A)")  "* Recover chain:"
    write (u, "(A)")
    
    call sf_chain_instance%init (sf_chain, n_channel = 1)
    call sf_channel(2)%init (2)
    call sf_channel(2)%activate_mapping ([2])
    call sf_chain_instance%set_channel (1, sf_channel(2))
    call sf_chain_instance%link_interactions ()
    call sf_chain_instance%exchange_mask ()
    call sf_chain_instance%init_evaluators ()

    int => sf_chain_instance%get_out_int_ptr ()
    call particle_set_fill_interaction (pset, int, 2)

    call sf_chain_instance%recover_kinematics (1)
    call sf_chain_instance%evaluate (scale=0._default)

    call write_separator (u, 2)
    call sf_chain_instance%write (u)
    call write_separator (u, 2)

    call particle_set_final (pset)
    call sf_chain_instance%final ()
    call sf_chain%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_base_11"

  end subroutine sf_base_11

  subroutine sf_base_12 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(flavor_t) :: flv
    type(pdg_array_t) :: pdg_in
    type(beam_data_t), target :: beam_data
    class(sf_data_t), allocatable, target :: data
    type(sf_config_t), dimension(:), allocatable, target :: sf_config
    type(sf_chain_t), target :: sf_chain
    type(sf_chain_instance_t), target :: sf_chain_instance
    real(default), dimension(2) :: x_saved
    real(default), dimension(2,3) :: p_saved
    type(sf_channel_t), dimension(:), allocatable :: sf_channel
    
    write (u, "(A)")  "* Test output: sf_base_12"
    write (u, "(A)")  "*   Purpose: set up and evaluate a multi-channel &
         &structure-function chain"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call model%init_test ()
    call flavor_init (flv, 25, model)
    pdg_in = 25

    call reset_interaction_counter ()
    
    call beam_data_init_sqrts (beam_data, &
         1000._default, [flv, flv])

    allocate (sf_test_data_t :: data)
    select type (data)
    type is (sf_test_data_t)
       call data%init (model, pdg_in)
    end select
       
    write (u, "(A)")  "* Set up chain with structure function pair &
         &and three different mappings"
    write (u, "(A)")
    
    allocate (sf_config (2))
    call sf_config(1)%init ([1], data)
    call sf_config(2)%init ([2], data) 
    call sf_chain%init (beam_data, sf_config)

    call sf_chain_instance%init (sf_chain, n_channel = 3)

    call allocate_sf_channels (sf_channel, n_channel = 3, n_strfun = 2)

    ! channel 1: no mapping
    call sf_chain_instance%set_channel (1, sf_channel(1))

    ! channel 2: single-particle mappings
    call sf_channel(2)%activate_mapping ([1,2])
    ! call sf_chain_instance%activate_mapping (2, [1,2])
    call sf_chain_instance%set_channel (2, sf_channel(2))
   
    ! channel 3: two-particle mapping
    call sf_channel(3)%set_s_mapping ([1,2])
    ! call sf_chain_instance%set_s_mapping (3, [1, 2])
    call sf_chain_instance%set_channel (3, sf_channel(3))

    call sf_chain_instance%link_interactions ()
    call sf_chain_instance%exchange_mask ()
    call sf_chain_instance%init_evaluators ()

    write (u, "(A)")  "* Compute kinematics in channel 1 and evaluate"
    write (u, "(A)")

    call sf_chain_instance%compute_kinematics (1, [0.8_default, 0.6_default])
    call sf_chain_instance%evaluate (scale=0._default)
    
    call write_separator (u, 2)
    call sf_chain_instance%write (u)
    call write_separator (u, 2)

    write (u, "(A)")
    write (u, "(A)")  "* Invert the kinematics calculation"
    write (u, "(A)")

    x_saved = sf_chain_instance%x

    call sf_chain_instance%inverse_kinematics (x_saved)
    call sf_chain_instance%evaluate (scale=0._default)
    
    call write_separator (u, 2)
    call sf_chain_instance%write (u)
    call write_separator (u, 2)

    write (u, "(A)")
    write (u, "(A)")  "* Compute kinematics in channel 2 and evaluate"
    write (u, "(A)")

    p_saved = sf_chain_instance%p

    call sf_chain_instance%compute_kinematics (2, p_saved(:,2))
    call sf_chain_instance%evaluate (scale=0._default)
    
    call write_separator (u, 2)
    call sf_chain_instance%write (u)
    call write_separator (u, 2)

    write (u, "(A)")
    write (u, "(A)")  "* Compute kinematics in channel 3 and evaluate"
    write (u, "(A)")

    call sf_chain_instance%compute_kinematics (3, p_saved(:,3))
    call sf_chain_instance%evaluate (scale=0._default)
    
    call write_separator (u, 2)
    call sf_chain_instance%write (u)
    call write_separator (u, 2)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call sf_chain_instance%final ()
    call sf_chain%final ()

    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_base_12"

  end subroutine sf_base_12

  subroutine sf_base_13 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(flavor_t) :: flv
    type(pdg_array_t) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    class(sf_int_t), allocatable :: sf_int
    type(vector4_t), dimension(2) :: k
    type(vector4_t), dimension(2) :: q
    real(default) :: E
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: f, x_free
    
    write (u, "(A)")  "* Test output: sf_base_13"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &a pair generator object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call model%init_test ()
    call flavor_init (flv, 25, model)
    pdg_in = 25

    call reset_interaction_counter ()
    
    allocate (sf_test_generator_data_t :: data)
    select type (data)
    type is (sf_test_generator_data_t)
       call data%init (model, pdg_in)
    end select
       
    write (u, "(A)")  "* Initialize generator object"
    write (u, "(A)")
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)

    allocate (r (data%get_n_par ()))
    allocate (rb(size (r)))
    allocate (x (size (r)))
    
    write (u, "(A)")  "* Generate free r values"
    write (u, "(A)")

    x_free = 1
    call sf_int%generate_free (r, rb, x_free)

    write (u, "(A)")  "* Initialize incoming momenta with sqrts=1000"

    E = 500
    k(1) = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    k(2) = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv)**2), 3)
    call sf_int%seed_kinematics (k)

    write (u, "(A)")
    write (u, "(A)")  "* Complete kinematics"
    write (u, "(A)")

    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "f =", f
    write (u, "(A,9(1x,F10.7))")  "xf=", x_free

    write (u, "(A)")
    write (u, "(A)")  "* Recover x from momenta"
    write (u, "(A)")

    q = interaction_get_momenta (sf_int%interaction_t, outgoing=.true.)
    call sf_int%final ()
    deallocate (sf_int)

    call reset_interaction_counter ()
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)

    call sf_int%seed_kinematics (k)
    call interaction_set_momenta (sf_int%interaction_t, q, outgoing=.true.)
    x_free = 1
    call sf_int%recover_x (x, x_free)
    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "xf=", x_free

    write (u, "(A)")
    write (u, "(A)")  "* Compute inverse kinematics &
         &and evaluate"
    write (u, "(A)")

    call sf_int%inverse_kinematics (x, f, r, rb, map=.false.)
    call sf_int%apply (scale=0._default)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A,9(1x,F10.7))")  "r =", r
    write (u, "(A,9(1x,F10.7))")  "f =", f

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call sf_int%final ()
    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_base_13"

  end subroutine sf_base_13

  subroutine sf_base_14 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(flavor_t) :: flv
    type(pdg_array_t) :: pdg_in
    type(beam_data_t), target :: beam_data
    class(sf_data_t), allocatable, target :: data_strfun
    class(sf_data_t), allocatable, target :: data_generator
    type(sf_config_t), dimension(:), allocatable, target :: sf_config
    real(default), dimension(:), allocatable :: p_in
    type(sf_chain_t), target :: sf_chain
    type(sf_chain_instance_t), target :: sf_chain_instance
    
    write (u, "(A)")  "* Test output: sf_base_14"
    write (u, "(A)")  "*   Purpose: set up a structure-function chain"
    write (u, "(A)")  "*            create an instance and evaluate"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call model%init_test ()
    call flavor_init (flv, 25, model)
    pdg_in = 25

    call reset_interaction_counter ()
    
    call beam_data_init_sqrts (beam_data, &
         1000._default, [flv, flv])

    allocate (sf_test_data_t :: data_strfun)
    select type (data_strfun)
    type is (sf_test_data_t)
       call data_strfun%init (model, pdg_in)
    end select
       
    allocate (sf_test_generator_data_t :: data_generator)
    select type (data_generator)
    type is (sf_test_generator_data_t)
       call data_generator%init (model, pdg_in)
    end select
       
    write (u, "(A)")  "* Set up chain with generator and structure function"
    write (u, "(A)")
    
    allocate (sf_config (2))
    call sf_config(1)%init ([1,2], data_generator)
    call sf_config(2)%init ([2], data_strfun)
    call sf_chain%init (beam_data, sf_config)
    
    call sf_chain_instance%init (sf_chain, n_channel = 1)
    call sf_chain_instance%link_interactions ()
    call sf_chain_instance%exchange_mask ()
    call sf_chain_instance%init_evaluators ()

    write (u, "(A)")  "* Inject integration parameter"
    write (u, "(A)")

    allocate (p_in (sf_chain%get_n_bound ()), source = 0.9_default)
    write (u, "(A,9(1x,F10.7))")  "p_in =", p_in
    
    write (u, "(A)")
    write (u, "(A)")  "* Evaluate"
    write (u, "(A)")

    call sf_chain_instance%compute_kinematics (1, p_in)
    call sf_chain_instance%evaluate (scale=0._default)
    
    call sf_chain_instance%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Extract integration parameter"
    write (u, "(A)")

    call sf_chain_instance%get_mcpar (1, p_in)
    write (u, "(A,9(1x,F10.7))")  "p_in =", p_in
    
    call sf_chain_instance%final ()
    call sf_chain%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_base_14"

  end subroutine sf_base_14


end module sf_base
