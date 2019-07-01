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

module sf_circe2

  use kinds, only: default !NODEP!
  use kinds, only: double !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use limits, only: FMT_19 !NODEP!
  use diagnostics !NODEP!
  use os_interface
  use unit_tests
  use lorentz !NODEP!
  use rng_base
  use selectors
  use pdg_arrays
  use models
  use flavors
  use helicities
  use quantum_numbers
  use state_matrices
  use polarizations
  use interactions
  use sf_aux
  use sf_base
  use circe2, circe2_rng_t => rng_type !NODEP!

  implicit none
  private

  public :: circe2_data_t
  public :: sf_circe2_test

  type, extends (sf_data_t) :: circe2_data_t
     private
     type(model_t), pointer :: model => null () 
     type(flavor_t), dimension(2) :: flv_in
     integer, dimension(2) :: pdg_in
     real(default) :: sqrts = 0
     logical :: polarized = .false.
     class(rng_factory_t), allocatable :: rng_factory
     type(string_t) :: filename
     type(string_t) :: file 
     type(string_t) :: design 
     real(default) :: lumi = 0
     real(default), dimension(4) :: lumi_hel_frac = 0
     integer, dimension(0:4) :: h1 = [0, -1, -1, 1, 1]
     integer, dimension(0:4) :: h2 = [0, -1,  1,-1, 1]
     integer :: error = 1
   contains
       procedure :: init => circe2_data_init
       procedure :: set_generator_mode => circe2_data_set_generator_mode
       procedure :: check_file => circe2_check_file
       procedure :: check => circe2_data_check 
       procedure :: write => circe2_data_write
       procedure :: is_generator => circe2_data_is_generator
       procedure :: get_n_par => circe2_data_get_n_par
       procedure :: get_pdg_out => circe2_data_get_pdg_out
       procedure :: allocate_sf_int => circe2_data_allocate_sf_int
  end type circe2_data_t

  type(circe2_state) :: circe2_global_state
  
  type, extends (circe2_rng_t) :: rng_obj_t
     class(rng_t), allocatable :: rng
   contains
     procedure :: generate => rng_obj_generate
  end type rng_obj_t
  
  type, extends (sf_int_t) :: circe2_t
     type(circe2_data_t), pointer :: data => null ()
     type(rng_obj_t) :: rng_obj
     type(selector_t) :: selector
     integer :: h_sel = 0
   contains
     procedure :: type_string => circe2_type_string
     procedure :: write => circe2_write
     procedure :: init => circe2_init
     procedure :: is_generator => circe2_is_generator
     procedure :: generate_free => circe2_generate_whizard_free
     procedure :: complete_kinematics => circe2_complete_kinematics
     procedure :: inverse_kinematics => circe2_inverse_kinematics
     procedure :: apply => circe2_apply
  end type circe2_t 
  

contains

  subroutine circe2_data_init &
       (data, os_data, model, pdg_in, sqrts, polarized, file, design)
    class(circe2_data_t), intent(out) :: data
    type(os_data_t), intent(in) :: os_data
    type(model_t), intent(in), target :: model
    type(pdg_array_t), dimension(2), intent(in) :: pdg_in
    real(default), intent(in) :: sqrts
    logical, intent(in) :: polarized
    type(string_t), intent(in) :: file, design
    integer :: h
    data%model => model
    if (any (pdg_array_get_length (pdg_in) /= 1)) then
       call msg_fatal ("CIRCE2: incoming beam particles must be unique")
    end if
    call flavor_init (data%flv_in(1), pdg_array_get (pdg_in(1), 1), model)
    call flavor_init (data%flv_in(2), pdg_array_get (pdg_in(2), 1), model)
    data%pdg_in = flavor_get_pdg (data%flv_in)
    data%sqrts = sqrts
    data%polarized = polarized
    data%filename = file
    data%design = design
    call data%check_file (os_data)
    call circe2_load (circe2_global_state, trim (char(data%file)), &
            trim (char(data%design)), data%sqrts, data%error)
    data%lumi = circe2_luminosity (circe2_global_state, data%pdg_in, [0, 0])
    call data%check ()
    if (data%polarized) then
       do h = 1, 4
          data%lumi_hel_frac(h) = &
               circe2_luminosity (circe2_global_state, data%pdg_in, &
                                  [data%h1(h), data%h2(h)]) &
               / data%lumi
       end do
    end if
  end subroutine circe2_data_init

  subroutine circe2_data_set_generator_mode (data, rng_factory)
    class(circe2_data_t), intent(inout) :: data
    class(rng_factory_t), intent(inout), allocatable :: rng_factory
    call move_alloc (from = rng_factory, to = data%rng_factory)
  end subroutine circe2_data_set_generator_mode
  
  subroutine circe2_check_file (data, os_data)
    class(circe2_data_t), intent(inout) :: data
    type(os_data_t), intent(in) :: os_data
    logical :: exist
    type(string_t) :: file
    file = data%filename
    inquire (file = char (file), exist = exist)
    if (exist) then
       data%file = file
    else
       file = os_data%whizard_circe2path // "/" // data%filename
       inquire (file = char (file), exist = exist)
       if (exist) then
          data%file = file
       else
          call msg_fatal ("CIRCE2: data file '" // char (data%filename) &
               // "' not found")
       end if
    end if
  end subroutine circe2_check_file
    
  subroutine circe2_data_check (data) 
    class(circe2_data_t), intent(in) :: data 
    type(flavor_t) :: flv_photon, flv_electron
    call flavor_init (flv_photon, PHOTON, data%model)
    if (flavor_get_pdg (flv_photon) == UNDEFINED) then
       call msg_fatal ("CIRCE2: model must contain photon")
    end if
    call flavor_init (flv_electron, ELECTRON, data%model)
    if (flavor_get_pdg (flv_electron) == UNDEFINED) then
       call msg_fatal ("CIRCE2: model must contain electron")
    end if
    if (any (abs (data%pdg_in) /= PHOTON .and. abs (data%pdg_in) /= ELECTRON)) &
         then
       call msg_fatal ("CIRCE2: applicable only for e+e- or photon collisions")
    end if
    select case (data%error)
    case (-1)
       call msg_fatal ("CIRCE2: data file not found.")
    case (-2)
       call msg_fatal ("CIRCE2: beam parameters do not match data file.")
    case (-3)
       call msg_fatal ("CIRCE2: invalid format of data file.")
    case (-4)
       call msg_fatal ("CIRCE2: data file too large.")
    end select
    if (data%lumi == 0) then
       call msg_fatal ("CIRCE2: luminosity vanishes for specified beams.")
    end if
  end subroutine circe2_data_check
  
  subroutine circe2_data_write (data, unit, verbose) 
    class(circe2_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose
    integer :: u, h
    u = output_unit (unit)
    write (u, "(1x,A)") "CIRCE2 data:"
    write (u, "(3x,A,A)")       "file   = ", char(data%filename)
    write (u, "(3x,A,A)")       "design = ", char(data%design)
    write (u, "(3x,A," // FMT_19 // ")") "sqrts  = ", data%sqrts
    write (u, "(3x,A,A,A,A)")   "prt_in = ", &
         char (flavor_get_name (data%flv_in(1))), &
         ", ", char (flavor_get_name (data%flv_in(2)))    
    write (u, "(3x,A,L1)")      "polarized  = ", data%polarized
    write (u, "(3x,A," // FMT_19 // ")") "luminosity = ", data%lumi
    if (data%polarized) then
       do h = 1, 4
          write (u, "(6x,'(',I2,1x,I2,')',1x,'=',1x)", advance="no") &
               data%h1(h), data%h2(h)
          write (u, "(6x, " // FMT_19 // ")")  data%lumi_hel_frac(h)
       end do
    end if
    call data%rng_factory%write (u)
  end subroutine circe2_data_write
  
  function circe2_data_is_generator (data) result (flag)
    class(circe2_data_t), intent(in) :: data
    logical :: flag
    flag = .true.
  end function circe2_data_is_generator
  
  function circe2_data_get_n_par (data) result (n)
    class(circe2_data_t), intent(in) :: data
    integer :: n
    n = 2
  end function circe2_data_get_n_par
  
  subroutine circe2_data_get_pdg_out (data, pdg_out)
    class(circe2_data_t), intent(in) :: data
    type(pdg_array_t), dimension(:), intent(inout) :: pdg_out
    integer :: i, n
    n = 2
    do i = 1, n
       pdg_out(i) = data%pdg_in(i)
    end do
  end subroutine circe2_data_get_pdg_out
  
  subroutine circe2_data_allocate_sf_int (data, sf_int)
    class(circe2_data_t), intent(in) :: data
    class(sf_int_t), intent(inout), allocatable :: sf_int
    allocate (circe2_t :: sf_int)
  end subroutine circe2_data_allocate_sf_int
  
  subroutine rng_obj_generate (rng_obj, u)
    class(rng_obj_t), intent(inout) :: rng_obj
    real(default), intent(out) :: u
    real(default) :: x
    call rng_obj%rng%generate (x)
    u = x
  end subroutine rng_obj_generate

  function circe2_type_string (object) result (string)
    class(circe2_t), intent(in) :: object
    type(string_t) :: string
    if (associated (object%data)) then
       string = "CIRCE2: " // object%data%design
    else
       string = "CIRCE2: [undefined]"
    end if
  end function circe2_type_string
  
  subroutine circe2_write (object, unit, testflag)
    class(circe2_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = output_unit (unit)
    if (associated (object%data)) then
       call object%data%write (u)
       call object%base_write (u, testflag)
    else
       write (u, "(1x,A)")  "CIRCE2 data: [undefined]"
    end if
  end subroutine circe2_write
    
  subroutine circe2_init (sf_int, data)
    class(circe2_t), intent(out) :: sf_int
    class(sf_data_t), intent(in), target :: data
    logical, dimension(4) :: mask_h
    real(default), dimension(0) :: null_array
    type(quantum_numbers_mask_t), dimension(4) :: mask
    type(quantum_numbers_t), dimension(4) :: qn
    type(helicity_t) :: hel
    integer :: h
    select type (data)
    type is (circe2_data_t)
       mask_h(1:2) = .true.
       mask_h(3:4) = .not. data%polarized       
       mask = new_quantum_numbers_mask (.false., .false., mask_h)
       call sf_int%base_init (mask, [0._default, 0._default], &
            null_array, [0._default, 0._default])    
       sf_int%data => data              
       if (data%polarized) then
          call sf_int%selector%init (data%lumi_hel_frac)
       end if
       call quantum_numbers_init (qn(1), flv = data%flv_in(1))
       call quantum_numbers_init (qn(2), flv = data%flv_in(2))
       if (data%polarized) then
          do h = 1, 4
             call helicity_init (hel, data%h1(h))
             call quantum_numbers_init (qn(3), flv = data%flv_in(1), hel = hel)
             call helicity_init (hel, data%h2(h))
             call quantum_numbers_init (qn(4), flv = data%flv_in(2), hel = hel)
             call interaction_add_state (sf_int%interaction_t, qn)
          end do
       else
          call quantum_numbers_init (qn(3), flv = data%flv_in(1))
          call quantum_numbers_init (qn(4), flv = data%flv_in(2))
          call interaction_add_state (sf_int%interaction_t, qn)
       end if
       call interaction_freeze (sf_int%interaction_t)
       call sf_int%set_incoming ([1,2])
       call sf_int%set_outgoing ([3,4])
       call sf_int%data%rng_factory%make (sf_int%rng_obj%rng)
       sf_int%status = SF_INITIAL
    end select
  end subroutine circe2_init

  function circe2_is_generator (sf_int) result (flag)
    class(circe2_t), intent(in) :: sf_int
    logical :: flag
    flag = sf_int%data%is_generator ()
  end function circe2_is_generator
  
  subroutine circe2_generate_whizard_free (sf_int, r, rb, x_free)
    class(circe2_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: r, rb
    real(default), intent(inout) :: x_free
    integer :: h_sel
    if (sf_int%data%polarized) then
       call sf_int%selector%generate (sf_int%rng_obj%rng, h_sel)
    else
       h_sel = 0
    end if
    sf_int%h_sel = h_sel
    call circe2_generate_whizard (r, sf_int%data%pdg_in, &
         [sf_int%data%h1(h_sel), sf_int%data%h2(h_sel)], &
         sf_int%rng_obj)
    rb = 1 - r
    x_free = x_free * product (r)
  end subroutine circe2_generate_whizard_free
    
  subroutine circe2_generate_whizard (x, pdg, hel, rng_obj)
    real(default), dimension(2), intent(out) :: x
    integer, dimension(2), intent(in) :: pdg
    integer, dimension(2), intent(in) :: hel
    class(rng_obj_t), intent(inout) :: rng_obj
    call circe2_generate (circe2_global_state, rng_obj, x, pdg, hel)
  end subroutine circe2_generate_whizard

  subroutine circe2_complete_kinematics (sf_int, x, f, r, rb, map)
    class(circe2_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: r
    real(default), dimension(:), intent(in) :: rb
    logical, intent(in) :: map
    if (map) then
       call msg_fatal ("CIRCE2: map flag not supported")
    else
       x = r
       f = 1
    end if
    call sf_int%reduce_momenta (x)
  end subroutine circe2_complete_kinematics

  subroutine circe2_inverse_kinematics (sf_int, x, f, r, rb, map, set_momenta)
    class(circe2_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(in) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: r
    real(default), dimension(:), intent(out) :: rb
    logical, intent(in) :: map
    logical, intent(in), optional :: set_momenta
    logical :: set_mom
    set_mom = .false.;  if (present (set_momenta))  set_mom = set_momenta
    if (map) then
       call msg_fatal ("CIRCE2: map flag not supported")
    else
       r = x
       rb= 1 - r
       f = 1
    end if
    if (set_mom) then
       call sf_int%reduce_momenta (x)
    end if
  end subroutine circe2_inverse_kinematics

  subroutine circe2_apply (sf_int, scale)
    class(circe2_t), intent(inout) :: sf_int
    real(default), intent(in) :: scale    
    complex(default) :: f
    integer :: h
    associate (data => sf_int%data)
      f = 1
      if (data%polarized) then
         h = sf_int%h_sel
      else
         h = 1
      end if
      call interaction_set_matrix_element (sf_int%interaction_t, h, f)
    end associate
    sf_int%status = SF_EVALUATED
  end subroutine circe2_apply


  subroutine sf_circe2_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (sf_circe2_1, "sf_circe2_1", &
         "structure function configuration", &
         u, results)
    call test (sf_circe2_2, "sf_circe2_2", &
         "generator, unpolarized", &
         u, results)
    call test (sf_circe2_3, "sf_circe2_3", &
         "generator, polarized", &
         u, results)
  end subroutine sf_circe2_test
  
  subroutine sf_circe2_1 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    type(pdg_array_t), dimension(2) :: pdg_in
    type(pdg_array_t), dimension(2) :: pdg_out
    integer, dimension(:), allocatable :: pdg1, pdg2
    class(sf_data_t), allocatable :: data
    class(rng_factory_t), allocatable :: rng_factory
    
    write (u, "(A)")  "* Test output: sf_circe2_1"
    write (u, "(A)")  "*   Purpose: initialize and display &
         &CIRCE structure function data"
    write (u, "(A)")
    
    write (u, "(A)")  "* Create empty data object"
    write (u, "(A)")

    call os_data_init (os_data)
    call syntax_model_file_init ()
    call model_list%read_model (var_str ("QED"), &
         var_str ("QED.mdl"), os_data, model)
    pdg_in(1) = PHOTON
    pdg_in(2) = PHOTON

    allocate (circe2_data_t :: data)
    allocate (rng_test_factory_t :: rng_factory)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize (unpolarized)"
    write (u, "(A)")

    select type (data)
    type is (circe2_data_t)
       call data%init (os_data, model, pdg_in, &
            sqrts = 500._default, &
            polarized = .false., &
            file = var_str ("teslagg_500_polavg.circe"), &
            design = var_str ("TESLA/GG"))
       call data%set_generator_mode (rng_factory)
    end select

    call data%write (u)

    write (u, "(A)")

    write (u, "(1x,A)")  "Outgoing particle codes:"
    call data%get_pdg_out (pdg_out)
    pdg1 = pdg_out(1)
    pdg2 = pdg_out(2)
    write (u, "(2x,99(1x,I0))")  pdg1, pdg2

    write (u, "(A)")
    write (u, "(A)")  "* Initialize (polarized)"
    write (u, "(A)")

    allocate (rng_test_factory_t :: rng_factory)

    select type (data)
    type is (circe2_data_t)
       call data%init (os_data, model, pdg_in, &
            sqrts = 500._default, &
            polarized = .true., &
            file = var_str ("teslagg_500.circe"), &
            design = var_str ("TESLA/GG"))
       call data%set_generator_mode (rng_factory)
    end select

    call data%write (u)

    call model_list%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_circe2_1"

  end subroutine sf_circe2_1

  subroutine sf_circe2_2 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    type(flavor_t), dimension(2) :: flv
    type(pdg_array_t), dimension(2) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    class(rng_factory_t), allocatable :: rng_factory
    class(sf_int_t), allocatable :: sf_int
    type(vector4_t) :: k1, k2
    real(default) :: E
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: f, x_free
    
    write (u, "(A)")  "* Test output: sf_circe2_2"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &circe2 structure function object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call os_data_init (os_data)
    call syntax_model_file_init ()
    call model_list%read_model (var_str ("QED"), &
         var_str ("QED.mdl"), os_data, model)
    call flavor_init (flv(1), PHOTON, model)
    call flavor_init (flv(2), PHOTON, model)
    pdg_in(1) = PHOTON
    pdg_in(2) = PHOTON

    call reset_interaction_counter ()
    
    allocate (circe2_data_t :: data)
    allocate (rng_test_factory_t :: rng_factory)
    select type (data)
    type is (circe2_data_t)
       call data%init (os_data, model, pdg_in, &
            sqrts = 500._default, &
            polarized = .false., &
            file = var_str ("teslagg_500_polavg.circe"), &
            design = var_str ("TESLA/GG"))
       call data%set_generator_mode (rng_factory)
    end select
       
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1,2])
    select type (sf_int)
    type is (circe2_t)
       call sf_int%rng_obj%rng%init (3)
    end select

    write (u, "(A)")  "* Initialize incoming momentum with E=500"
    write (u, "(A)")
    E = 250
    k1 = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv(1))**2), 3)
    k2 = vector4_moving (E,-sqrt (E**2 - flavor_get_mass (flv(2))**2), 3)
    call vector4_write (k1, u)
    call vector4_write (k2, u)
    call sf_int%seed_kinematics ([k1, k2])

    write (u, "(A)")
    write (u, "(A)")  "* Generate x"
    write (u, "(A)")

    allocate (r (data%get_n_par ()))
    allocate (rb(size (r)))
    allocate (x (size (r)))

    r  = 0
    rb = 0
    x_free = 1
    call sf_int%generate_free (r, rb, x_free)
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)

    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "f =", f
    write (u, "(A,9(1x,F10.7))")  "xf=", x_free

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate"
    write (u, "(A)")

    call sf_int%apply (scale = 0._default)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call sf_int%final ()
    call model_list%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_circe2_2"

  end subroutine sf_circe2_2

  subroutine sf_circe2_3 (u)
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model
    type(flavor_t), dimension(2) :: flv
    type(pdg_array_t), dimension(2) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    class(rng_factory_t), allocatable :: rng_factory
    class(sf_int_t), allocatable :: sf_int
    type(vector4_t) :: k1, k2
    real(default) :: E
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: f, x_free
    
    write (u, "(A)")  "* Test output: sf_circe2_3"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &circe2 structure function object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call os_data_init (os_data)
    call syntax_model_file_init ()
    call model_list%read_model (var_str ("QED"), &
         var_str ("QED.mdl"), os_data, model)
    call flavor_init (flv(1), PHOTON, model)
    call flavor_init (flv(2), PHOTON, model)
    pdg_in(1) = PHOTON
    pdg_in(2) = PHOTON

    call reset_interaction_counter ()
    
    allocate (circe2_data_t :: data)
    allocate (rng_test_factory_t :: rng_factory)
    select type (data)
    type is (circe2_data_t)
       call data%init (os_data, model, pdg_in, &
            sqrts = 500._default, &
            polarized = .true., &
            file = var_str ("teslagg_500.circe"), &
            design = var_str ("TESLA/GG"))
       call data%set_generator_mode (rng_factory)
    end select
       
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1,2])
    select type (sf_int)
    type is (circe2_t)
       call sf_int%rng_obj%rng%init (3)
    end select

    write (u, "(A)")  "* Initialize incoming momentum with E=500"
    write (u, "(A)")
    E = 250
    k1 = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv(1))**2), 3)
    k2 = vector4_moving (E,-sqrt (E**2 - flavor_get_mass (flv(2))**2), 3)
    call vector4_write (k1, u)
    call vector4_write (k2, u)
    call sf_int%seed_kinematics ([k1, k2])

    write (u, "(A)")
    write (u, "(A)")  "* Generate x"
    write (u, "(A)")

    allocate (r (data%get_n_par ()))
    allocate (rb(size (r)))
    allocate (x (size (r)))

    r  = 0
    rb = 0
    x_free = 1
    call sf_int%generate_free (r, rb, x_free)
    call sf_int%complete_kinematics (x, f, r, rb, map=.false.)

    write (u, "(A,9(1x,F10.7))")  "x =", x
    write (u, "(A,9(1x,F10.7))")  "f =", f
    write (u, "(A,9(1x,F10.7))")  "xf=", x_free

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate"
    write (u, "(A)")

    call sf_int%apply (scale = 0._default)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call sf_int%final ()
    call model_list%final ()
    call syntax_model_file_final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_circe2_3"

  end subroutine sf_circe2_3


end module sf_circe2
