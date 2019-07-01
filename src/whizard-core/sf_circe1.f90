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

module sf_circe1

  use kinds, only: default
  use kinds, only: double
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_defs, only: FMT_17, FMT_19
  use unit_tests
  use diagnostics
  use physics_defs, only: UNDEFINED, ELECTRON, PHOTON
  use lorentz
  use rng_base
  use pdg_arrays
  use model_data
  use flavors
  use colors
  use quantum_numbers
  use state_matrices
  use polarizations
  use interactions
  use sf_mappings
  use sf_aux
  use sf_base
  use circe1, circe1_rng_t => rng_type !NODEP!

  implicit none
  private

  public :: circe1_data_t 
  public :: sf_circe1_test

  type, extends (sf_data_t) :: circe1_data_t 
     private 
     class(model_data_t), pointer :: model => null () 
     type(flavor_t), dimension(2) :: flv_in 
     integer, dimension(2) :: pdg_in
     real(default), dimension(2) :: m_in = 0
     logical, dimension(2) :: photon = .false.
     logical :: generate = .false.
     class(rng_factory_t), allocatable :: rng_factory
     real(default) :: sqrts = 0
     real(default) :: eps = 0
     integer :: ver = 0 
     integer :: rev = 0      
     character(6) :: acc = "?"
     integer :: chat = 0    
   contains  
       procedure :: init => circe1_data_init 
       procedure :: set_generator_mode => circe1_data_set_generator_mode
       procedure :: check => circe1_data_check 
       procedure :: write => circe1_data_write 
       procedure :: is_generator => circe1_data_is_generator
       procedure :: get_n_par => circe1_data_get_n_par
       procedure :: get_pdg_out => circe1_data_get_pdg_out
       procedure :: get_pdg_int => circe1_data_get_pdg_int
       procedure :: allocate_sf_int => circe1_data_allocate_sf_int     
  end type circe1_data_t 
 
  type, extends (circe1_rng_t) :: rng_obj_t
     class(rng_t), allocatable :: rng
   contains
     procedure :: generate => rng_obj_generate
  end type rng_obj_t
  
  type, extends (sf_int_t) :: circe1_t
     type(circe1_data_t), pointer :: data => null ()
     real(default), dimension(2) :: x = 0
     real(default) :: f = 0
     logical, dimension(2) :: continuum = .true.
     logical, dimension(2) :: peak = .true.
     type(rng_obj_t) :: rng_obj
   contains
     procedure :: type_string => circe1_type_string
     procedure :: write => circe1_write
     procedure :: init => circe1_init
     procedure :: is_generator => circe1_is_generator
     procedure :: generate_free => circe1_generate_free
     procedure :: complete_kinematics => circe1_complete_kinematics
     procedure :: inverse_kinematics => circe1_inverse_kinematics
     procedure :: apply => circe1_apply
  end type circe1_t 
  

contains

  subroutine circe1_data_init &
       (data, model, pdg_in, sqrts, eps, out_photon, &
        ver, rev, acc, chat)
    class(circe1_data_t), intent(out) :: data 
    class(model_data_t), intent(in), target :: model
    type(pdg_array_t), dimension(2), intent(in) :: pdg_in
    real(default), intent(in) :: sqrts 
    real(default), intent(in) :: eps
    logical, dimension(2), intent(in) :: out_photon
    character(*), intent(in) :: acc
    integer, intent(in) :: ver, rev, chat 
    data%model => model 
    if (any (pdg_array_get_length (pdg_in) /= 1)) then
       call msg_fatal ("CIRCE1: incoming beam particles must be unique")
    end if
    call flavor_init (data%flv_in(1), pdg_array_get (pdg_in(1), 1), model)
    call flavor_init (data%flv_in(2), pdg_array_get (pdg_in(2), 1), model)
    data%pdg_in = flavor_get_pdg (data%flv_in)
    data%m_in = flavor_get_mass (data%flv_in)
    data%sqrts = sqrts
    data%eps = eps
    data%photon = out_photon
    data%ver = ver
    data%rev = rev
    data%acc = acc 
    data%chat = chat 
    call data%check ()
    call circex (0.d0, 0.d0, dble (data%sqrts), &
         data%acc, data%ver, data%rev, data%chat)
  end subroutine circe1_data_init 
 
  subroutine circe1_data_set_generator_mode (data, rng_factory)
    class(circe1_data_t), intent(inout) :: data
    class(rng_factory_t), intent(inout), allocatable :: rng_factory
    data%generate = .true.
    call move_alloc (from = rng_factory, to = data%rng_factory)
  end subroutine circe1_data_set_generator_mode
  
  subroutine circe1_data_check (data) 
    class(circe1_data_t), intent(in) :: data 
    type(flavor_t) :: flv_electron, flv_photon
    call flavor_init (flv_electron, ELECTRON, data%model)
    call flavor_init (flv_photon, PHOTON, data%model)
    if (flavor_get_pdg (flv_electron) == UNDEFINED &
         .or. flavor_get_pdg (flv_photon) == UNDEFINED) then
       call msg_fatal ("CIRCE1: model must contain photon and electron")
    end if
    if (any (abs (data%pdg_in) /= ELECTRON) &
         .or. (data%pdg_in(1) /= - data%pdg_in(2))) then
       call msg_fatal ("CIRCE1: applicable only for e+e- or e-e+ collisions")
    end if
    if (data%eps <= 0) then
       call msg_error ("CIRCE1: circe1_eps = 0: integration will &
            &miss x=1 peak")
    end if
  end subroutine circe1_data_check 
 
  subroutine circe1_data_write (data, unit, verbose)
    class(circe1_data_t), intent(in) :: data 
    integer, intent(in), optional :: unit 
    logical, intent(in), optional :: verbose
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return 
    write (u, "(1x,A)") "CIRCE1 data:" 
    write (u, "(3x,A,2(1x,A))") "prt_in   =", &
         char (flavor_get_name (data%flv_in(1))), &
         char (flavor_get_name (data%flv_in(2)))
    write (u, "(3x,A,2(1x,L1))")  "photon   =", data%photon
    write (u, "(3x,A,L1)")        "generate = ", data%generate
    write (u, "(3x,A,2(1x," // FMT_19 // "))") "m_in     =", data%m_in
    write (u, "(3x,A," // FMT_19 // ")") "sqrts    = ", data%sqrts 
    write (u, "(3x,A," // FMT_19 // ")") "eps      = ", data%eps
    write (u, "(3x,A,I0)") "ver      = ", data%ver 
    write (u, "(3x,A,I0)") "rev      = ", data%rev 
    write (u, "(3x,A,A)")  "acc      = ", data%acc 
    write (u, "(3x,A,I0)") "chat     = ", data%chat 
    if (data%generate)  call data%rng_factory%write (u)
  end subroutine circe1_data_write 
 
  function circe1_data_is_generator (data) result (flag)
    class(circe1_data_t), intent(in) :: data
    logical :: flag
    flag = data%generate
  end function circe1_data_is_generator
  
  function circe1_data_get_n_par (data) result (n)
    class(circe1_data_t), intent(in) :: data
    integer :: n
    n = 2
  end function circe1_data_get_n_par
  
  subroutine circe1_data_get_pdg_out (data, pdg_out)
    class(circe1_data_t), intent(in) :: data
    type(pdg_array_t), dimension(:), intent(inout) :: pdg_out
    integer :: i, n
    n = 2
    do i = 1, n
       if (data%photon(i)) then
          pdg_out(i) = PHOTON
       else
          pdg_out(i) = data%pdg_in(i)
       end if
    end do
  end subroutine circe1_data_get_pdg_out
  
  function circe1_data_get_pdg_int (data) result (pdg)
    class(circe1_data_t), intent(in) :: data
    integer, dimension(2) :: pdg
    integer :: i
    do i = 1, 2
       if (data%photon(i)) then
          pdg(i) = PHOTON
       else
          pdg(i) = data%pdg_in(i)
       end if
    end do
  end function circe1_data_get_pdg_int
  
  subroutine circe1_data_allocate_sf_int (data, sf_int)
    class(circe1_data_t), intent(in) :: data
    class(sf_int_t), intent(inout), allocatable :: sf_int
    allocate (circe1_t :: sf_int)
  end subroutine circe1_data_allocate_sf_int
  
  subroutine rng_obj_generate (rng_obj, u)
    class(rng_obj_t), intent(inout) :: rng_obj
    real(double), intent(out) :: u
    real(default) :: x
    call rng_obj%rng%generate (x)
    u = x
  end subroutine rng_obj_generate

  function circe1_type_string (object) result (string)
    class(circe1_t), intent(in) :: object
    type(string_t) :: string
    if (associated (object%data)) then
       string = "CIRCE1: beamstrahlung" 
    else
       string = "CIRCE1: [undefined]"
    end if
  end function circe1_type_string
  
  subroutine circe1_write (object, unit, testflag)
    class(circe1_t), intent(in) :: object
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u
    u = given_output_unit (unit)
    if (associated (object%data)) then
       call object%data%write (u)
       if (object%data%generate)  call object%rng_obj%rng%write (u)
       if (object%status >= SF_DONE_KINEMATICS) then
          write (u, "(3x,A,2(1x," // FMT_17 // "))")  "x =", object%x
          if (object%status >= SF_FAILED_EVALUATION) then
             write (u, "(3x,A,1x," // FMT_17 // ")")  "f =", object%f
          end if
       end if
       call object%base_write (u, testflag)
    else
       write (u, "(1x,A)")  "CIRCE1 data: [undefined]"
    end if
  end subroutine circe1_write
    
  subroutine circe1_init (sf_int, data) 
    class(circe1_t), intent(out) :: sf_int 
    class(sf_data_t), intent(in), target :: data
    logical, dimension(6) :: mask_h
    type(quantum_numbers_mask_t), dimension(6) :: mask 
    integer, dimension(6) :: hel_lock 
    type(polarization_t) :: pol1, pol2
    type(quantum_numbers_t), dimension(1) :: qn_fc1, qn_hel1, qn_fc2, qn_hel2
    type(flavor_t) :: flv_photon
    real(default), dimension(2) :: mi2, mr2, mo2
    type(quantum_numbers_t) :: qn_photon, qn1, qn2
    type(quantum_numbers_t), dimension(6) :: qn
    type(state_iterator_t) :: it_hel1, it_hel2
    hel_lock = 0
    mask_h = .false.
    select type (data)
    type is (circe1_data_t)
       mi2 = data%m_in**2
       if (data%photon(1)) then
          hel_lock(1) = 3;  hel_lock(3) = 1;  mask_h(5) = .true.
          mr2(1) = mi2(1)
          mo2(1) = 0._default
       else
          hel_lock(1) = 5;  hel_lock(5) = 1;  mask_h(3) = .true.
          mr2(1) = 0._default
          mo2(1) = mi2(1)
       end if
       if (data%photon(2)) then
          hel_lock(2) = 4;  hel_lock(4) = 2;  mask_h(6) = .true.
          mr2(2) = mi2(2)
          mo2(2) = 0._default
       else
          hel_lock(2) = 6;  hel_lock(6) = 2;  mask_h(4) = .true.
          mr2(2) = 0._default
          mo2(2) = mi2(2)
       end if
       mask = new_quantum_numbers_mask (.false., .false., mask_h)
       call sf_int%base_init (mask, mi2, mr2, mo2, &
            hel_lock = hel_lock)
       sf_int%data => data
       call flavor_init (flv_photon, PHOTON, data%model)
       call quantum_numbers_init (qn_photon, flv_photon)
       call polarization_init_generic (pol1, data%flv_in(1))
       call quantum_numbers_init (qn_fc1(1), flv = data%flv_in(1))
       call polarization_init_generic (pol2, data%flv_in(2))
       call quantum_numbers_init (qn_fc2(1), flv = data%flv_in(2))
       call state_iterator_init (it_hel1, pol1%state)
       
       do while (state_iterator_is_valid (it_hel1)) 
          qn_hel1 = state_iterator_get_quantum_numbers (it_hel1)
          qn1 = qn_hel1(1) .merge. qn_fc1(1) 
          qn(1) = qn1
          if (data%photon(1)) then
             qn(3) = qn1;  qn(5) = qn_photon
          else
             qn(3) = qn_photon;  qn(5) = qn1
          end if
          call state_iterator_init (it_hel2, pol2%state) 
          do while (state_iterator_is_valid (it_hel2)) 
             qn_hel2 = state_iterator_get_quantum_numbers (it_hel2) 
             qn2 = qn_hel2(1) .merge. qn_fc2(1) 
             qn(2) = qn2
             if (data%photon(2)) then
                qn(4) = qn2;  qn(6) = qn_photon
             else
                qn(4) = qn_photon;  qn(6) = qn2
             end if
             call interaction_add_state (sf_int%interaction_t, qn)
             call state_iterator_advance (it_hel2) 
          end do
          call state_iterator_advance (it_hel1)
       end do
       call polarization_final (pol1)
       call polarization_final (pol2)
       call interaction_freeze (sf_int%interaction_t) 
       call sf_int%set_incoming ([1,2])
       call sf_int%set_radiated ([3,4])
       call sf_int%set_outgoing ([5,6])
       sf_int%status = SF_INITIAL
    end select
    if (sf_int%data%generate) then
       call sf_int%data%rng_factory%make (sf_int%rng_obj%rng)
    end if
  end subroutine circe1_init

  function circe1_is_generator (sf_int) result (flag)
    class(circe1_t), intent(in) :: sf_int
    logical :: flag
    flag = sf_int%data%is_generator ()
  end function circe1_is_generator
  
  subroutine circe1_generate_free (sf_int, r, rb,  x_free)
    class(circe1_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: r, rb
    real(default), intent(inout) :: x_free

    if (sf_int%data%generate) then
       call circe_generate (r, sf_int%data%get_pdg_int (), sf_int%rng_obj)
       rb = 1 - r
       x_free = x_free * product (r)
    else
       r = 0
       rb= 1
    end if
  end subroutine circe1_generate_free
    
  subroutine circe_generate (x, pdg, rng_obj)
    real(default), dimension(2), intent(out) :: x
    integer, dimension(2), intent(in) :: pdg
    class(rng_obj_t), intent(inout) :: rng_obj
    real(double) :: xc1, xc2
    select case (abs (pdg(1)))
    case (ELECTRON)
       select case (abs (pdg(2)))
       case (ELECTRON)
          call gircee (xc1, xc2, rng_obj = rng_obj)
       case (PHOTON)
          call girceg (xc1, xc2, rng_obj = rng_obj)
       end select
    case (PHOTON)
       select case (abs (pdg(2)))
       case (ELECTRON)
          call girceg (xc2, xc1, rng_obj = rng_obj)
       case (PHOTON)
          call gircgg (xc1, xc2, rng_obj = rng_obj)
       end select
    end select
    x = [xc1, xc2]
  end subroutine circe_generate

  subroutine circe1_complete_kinematics (sf_int, x, f, r, rb, map)
    class(circe1_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(out) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(in) :: r
    real(default), dimension(:), intent(in) :: rb
    logical, intent(in) :: map
    real(default), dimension(2) :: xb1
    x = r
    sf_int%x = x
    f = 1
    xb1 = 1 - x
    call sf_int%split_momenta (x, xb1)
    select case (sf_int%status)
    case (SF_FAILED_KINEMATICS);  f = 0
    end select
  end subroutine circe1_complete_kinematics

  subroutine circe1_inverse_kinematics (sf_int, x, f, r, rb, map, set_momenta)
    class(circe1_t), intent(inout) :: sf_int
    real(default), dimension(:), intent(in) :: x
    real(default), intent(out) :: f
    real(default), dimension(:), intent(out) :: r
    real(default), dimension(:), intent(out) :: rb
    logical, intent(in) :: map
    logical, intent(in), optional :: set_momenta
    real(default), dimension(2) :: xb1
    logical :: set_mom
    set_mom = .false.;  if (present (set_momenta))  set_mom = set_momenta
    r = x
    rb = 1 - x
    sf_int%x = x
    f = 1
    if (set_mom) then
       xb1 = 1 - x
       call sf_int%split_momenta (x, xb1)
       select case (sf_int%status)
       case (SF_FAILED_KINEMATICS);  f = 0
       end select
    end if
  end subroutine circe1_inverse_kinematics

  subroutine circe1_apply (sf_int, scale)
    class(circe1_t), intent(inout) :: sf_int
    real(default), intent(in) :: scale
    real(default), dimension(2) :: xb
    real(double), dimension(2) :: xc
    real(double), parameter :: one = 1
    associate (data => sf_int%data)
      xc = sf_int%x
      xb = 1 - sf_int%x
      if (data%generate) then
         sf_int%f = 1
      else
         sf_int%f = 0
         if (all (sf_int%continuum)) then
            sf_int%f = circe (xc(1), xc(2), data%pdg_in(1), data%pdg_in(2))
         end if
         if (sf_int%continuum(2) .and. sf_int%peak(1)) then
            sf_int%f = sf_int%f &
                 + circe (one, xc(2), data%pdg_in(1), data%pdg_in(2)) &
                 * peak (xb(1), data%eps)
         end if
         if (sf_int%continuum(1) .and. sf_int%peak(2)) then
            sf_int%f = sf_int%f &
                 + circe (xc(1), one, data%pdg_in(1), data%pdg_in(2)) &
                 * peak (xb(2), data%eps)
         end if
         if (all (sf_int%peak)) then
            sf_int%f = sf_int%f &
                 + circe (one, one, data%pdg_in(1), data%pdg_in(2)) &
                 * peak (xb(1), data%eps) * peak (xb(2), data%eps)
         end if
      end if
    end associate
    call interaction_set_matrix_element &
        (sf_int%interaction_t, cmplx (sf_int%f, kind=default)) 
    sf_int%status = SF_EVALUATED
  end subroutine circe1_apply
 
  function peak (x, eps) result (f)
    real(default), intent(in) :: x, eps
    real(default) :: f
    f = exp (-x / eps) / eps
  end function peak
  

  subroutine sf_circe1_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (sf_circe1_1, "sf_circe1_1", &
         "structure function configuration", &
         u, results)
    call test (sf_circe1_2, "sf_circe1_2", &
         "structure function instance", &
         u, results)
    call test (sf_circe1_3, "sf_circe1_3", &
         "generator mode", &
         u, results)
  end subroutine sf_circe1_test
  
  subroutine sf_circe1_1 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(pdg_array_t), dimension(2) :: pdg_in
    type(pdg_array_t), dimension(2) :: pdg_out
    integer, dimension(:), allocatable :: pdg1, pdg2
    class(sf_data_t), allocatable :: data
    
    write (u, "(A)")  "* Test output: sf_circe1_1"
    write (u, "(A)")  "*   Purpose: initialize and display &
         &CIRCE structure function data"
    write (u, "(A)")
    
    write (u, "(A)")  "* Create empty data object"
    write (u, "(A)")

    call model%init_qed_test ()
    pdg_in(1) = ELECTRON
    pdg_in(2) = -ELECTRON

    allocate (circe1_data_t :: data)
    call data%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize"
    write (u, "(A)")

    select type (data)
    type is (circe1_data_t)
       call data%init (model, pdg_in, &
            sqrts = 500._default, &
            eps = 1e-6_default, &
            out_photon = [.false., .false.], &
            ver = 0, &
            rev = 0, &
            acc = "SBAND", &
            chat = 0)
    end select

    call data%write (u)

    write (u, "(A)")

    write (u, "(1x,A)")  "Outgoing particle codes:"
    call data%get_pdg_out (pdg_out)
    pdg1 = pdg_out(1)
    pdg2 = pdg_out(2)
    write (u, "(2x,99(1x,I0))")  pdg1, pdg2

    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_circe1_1"

  end subroutine sf_circe1_1

  subroutine sf_circe1_2 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(flavor_t), dimension(2) :: flv
    type(pdg_array_t), dimension(2) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    class(sf_int_t), allocatable :: sf_int
    type(vector4_t) :: k1, k2
    type(vector4_t), dimension(4) :: q
    real(default) :: E
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: f
    
    write (u, "(A)")  "* Test output: sf_circe1_2"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &circe1 structure function object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call model%init_qed_test ()
    call flavor_init (flv(1), ELECTRON, model)
    call flavor_init (flv(2), -ELECTRON, model)
    pdg_in(1) = ELECTRON
    pdg_in(2) = -ELECTRON

    call reset_interaction_counter ()
    
    allocate (circe1_data_t :: data)
    select type (data)
    type is (circe1_data_t)
       call data%init (model, pdg_in, &
            sqrts = 500._default, &
            eps = 1e-6_default, &
            out_photon = [.false., .false.], &
            ver = 0, &
            rev = 0, &
            acc = "SBAND", &
            chat = 0)
    end select
       
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1,2])
    
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Initialize incoming momentum with E=500"
    write (u, "(A)")
    E = 250
    k1 = vector4_moving (E, sqrt (E**2 - flavor_get_mass (flv(1))**2), 3)
    k2 = vector4_moving (E,-sqrt (E**2 - flavor_get_mass (flv(2))**2), 3)
    call vector4_write (k1, u)
    call vector4_write (k2, u)
    call sf_int%seed_kinematics ([k1, k2])

    write (u, "(A)")
    write (u, "(A)")  "* Set kinematics for x=0.95,0.85."
    write (u, "(A)")

    allocate (r (data%get_n_par ()))
    allocate (rb(size (r)))
    allocate (x (size (r)))

    r = [0.9_default, 0.8_default]
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

    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1, 2])

    call sf_int%seed_kinematics ([k1, k2])
    call interaction_set_momenta (sf_int%interaction_t, q, outgoing=.true.)
    call sf_int%recover_x (x)

    write (u, "(A,9(1x,F10.7))")  "x =", x

    write (u, "(A)")
    write (u, "(A)")  "* Evaluate"
    write (u, "(A)")

    call sf_int%complete_kinematics (x, f, r, rb, map=.false.) 
    call sf_int%apply (scale = 0._default)
    call sf_int%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call sf_int%final ()
    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_circe1_2"

  end subroutine sf_circe1_2

  subroutine sf_circe1_3 (u)
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(flavor_t), dimension(2) :: flv
    type(pdg_array_t), dimension(2) :: pdg_in
    class(sf_data_t), allocatable, target :: data
    class(rng_factory_t), allocatable :: rng_factory
    class(sf_int_t), allocatable :: sf_int
    type(vector4_t) :: k1, k2
    real(default) :: E
    real(default), dimension(:), allocatable :: r, rb, x
    real(default) :: f, x_free
    
    write (u, "(A)")  "* Test output: sf_circe1_3"
    write (u, "(A)")  "*   Purpose: initialize and fill &
         &circe1 structure function object"
    write (u, "(A)")
    
    write (u, "(A)")  "* Initialize configuration data"
    write (u, "(A)")

    call model%init_qed_test ()
    call flavor_init (flv(1), ELECTRON, model)
    call flavor_init (flv(2), -ELECTRON, model)
    pdg_in(1) = ELECTRON
    pdg_in(2) = -ELECTRON

    call reset_interaction_counter ()
    
    allocate (circe1_data_t :: data)
    allocate (rng_test_factory_t :: rng_factory)
    select type (data)
    type is (circe1_data_t)
       call data%init (model, pdg_in, &
            sqrts = 500._default, &
            eps = 1e-6_default, &
            out_photon = [.false., .false.], &
            ver = 0, &
            rev = 0, &
            acc = "SBAND", &
            chat = 0)
       call data%set_generator_mode (rng_factory)
    end select
       
    write (u, "(A)")  "* Initialize structure-function object"
    write (u, "(A)")
    
    call data%allocate_sf_int (sf_int)
    call sf_int%init (data)
    call sf_int%set_beam_index ([1,2])
    select type (sf_int)
    type is (circe1_t)
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
    call model%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sf_circe1_3"

  end subroutine sf_circe1_3


end module sf_circe1
