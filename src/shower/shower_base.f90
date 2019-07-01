! WHIZARD 2.6.2 Dec 13 2017
!
! Copyright (C) 1999-2017 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     cf. main AUTHORS file
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

module shower_base

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use constants
  use diagnostics
  use format_utils, only: write_separator
  use lorentz
  use particles
  use os_interface
  use rng_base
  use physics_defs
  use sm_physics, only: running_as_lam
  use particles
  use variables
  use model_data
  use pdf
  use tauola_interface

  implicit none
  private

  public :: PS_WHIZARD, PS_PYTHIA6, PS_PYTHIA8, PS_UNDEFINED
  public :: shower_method_of_string
  public :: shower_method_to_string
  public :: shower_settings_t
  public :: shower_base_t
  public :: D_alpha_s_isr
  public :: D_alpha_s_fsr
  public :: mass_type
  public :: mass_squared_type
  public :: number_of_flavors

  integer, parameter :: PS_UNDEFINED = 0
  integer, parameter :: PS_WHIZARD = 1
  integer, parameter :: PS_PYTHIA6 = 2
  integer, parameter :: PS_PYTHIA8 = 3
  real(default), public :: D_min_scale = 0.5_default
  logical, public :: treat_light_quarks_massless = .true.
  logical, public :: treat_duscb_quarks_massless = .false.
  real(default), public :: scalefactor1 = 0.02_default
  real(default), public :: scalefactor2 = 0.02_default

  type :: shower_settings_t
     logical :: active = .false.
     logical :: isr_active = .false.
     logical :: fsr_active = .false.
     logical :: muli_active = .false.
     logical :: hadronization_active = .false.
     logical :: tau_dec = .false.
     logical :: verbose = .false.
     integer :: method = PS_UNDEFINED
     logical :: hadron_collision = .false.
     logical :: mlm_matching = .false.
     logical :: ckkw_matching = .false.
     logical :: powheg_matching = .false.
     type(string_t) :: pythia6_pygive
     real(default) :: min_virtuality = 1._default   ! PARJ(82)^2
     real(default) :: fsr_lambda = 0.29_default     ! PARP(72)
     real(default) :: isr_lambda = 0.29_default     ! PARP(61)
     integer :: max_n_flavors = 5                   ! MSTJ(45)
     logical :: isr_alphas_running = .true.        ! MSTP(64)
     logical :: fsr_alphas_running = .true.        ! MSTJ(44)
     real(default) :: fixed_alpha_s = 0.2_default   ! PARU(111)
     logical :: alpha_s_fudged = .true.
     logical :: isr_pt_ordered = .false.
     logical :: isr_angular_ordered = .true.        ! MSTP(62)
     real(default) :: isr_primordial_kt_width = 1.5_default  ! PARP(91)
     real(default) :: isr_primordial_kt_cutoff = 5._default  ! PARP(93)
     real(default) :: isr_z_cutoff = 0.999_default  ! 1-PARP(66)
     real(default) :: isr_minenergy = 2._default    ! PARP(65)
     real(default) :: isr_tscalefactor = 1._default
     logical :: isr_only_onshell_emitted_partons = .true.   ! MSTP(63)
   contains
     procedure :: init => shower_settings_init
     procedure :: write => shower_settings_write
  end type shower_settings_t

  type, abstract :: shower_base_t
     class(rng_t), allocatable :: rng
     type(string_t) :: name
     type(pdf_data_t) :: pdf_data
     type(shower_settings_t) :: settings
     type(taudec_settings_t) :: taudec_settings
   contains
     procedure :: write_msg => shower_base_write_msg
     procedure :: import_rng => shower_base_import_rng
     procedure (shower_base_init), deferred :: init
     procedure (shower_base_prepare_new_event), deferred :: prepare_new_event
     procedure (shower_base_import_particle_set), deferred :: import_particle_set
     procedure (shower_base_generate_emissions), deferred :: generate_emissions
     procedure (shower_base_make_particle_set), deferred :: make_particle_set
     procedure (shower_base_get_final_colored_ME_momenta), deferred :: &
          get_final_colored_ME_momenta
  end type shower_base_t


  abstract interface
    subroutine shower_base_init (shower, settings, taudec_settings, pdf_data)
      import
      class(shower_base_t), intent(out) :: shower
      type(shower_settings_t), intent(in) :: settings
      type(taudec_settings_t), intent(in) :: taudec_settings
      type(pdf_data_t), intent(in) :: pdf_data
    end subroutine shower_base_init
   end interface

  abstract interface
     subroutine shower_base_prepare_new_event &
            (shower)
       import
       class(shower_base_t), intent(inout) :: shower
     end subroutine shower_base_prepare_new_event
  end interface

  abstract interface
     subroutine shower_base_import_particle_set &
            (shower, particle_set, os_data, scale)
       import
       class(shower_base_t), target, intent(inout) :: shower
       type(particle_set_t), intent(in) :: particle_set
       type(os_data_t), intent(in) :: os_data
       real(default), intent(in) :: scale
     end subroutine shower_base_import_particle_set
  end interface

  abstract interface
     subroutine shower_base_generate_emissions &
            (shower, valid, number_of_emissions)
      import
      class(shower_base_t), intent(inout), target :: shower
       logical, intent(out) :: valid
      integer, optional, intent(in) :: number_of_emissions
    end subroutine shower_base_generate_emissions
   end interface

  abstract interface
     subroutine shower_base_make_particle_set &
         (shower, particle_set, model, model_hadrons)
       import
       class(shower_base_t), intent(in) :: shower
       type(particle_set_t), intent(inout) :: particle_set
       class(model_data_t), intent(in), target :: model
       class(model_data_t), intent(in), target :: model_hadrons
     end subroutine shower_base_make_particle_set
  end interface

  abstract interface
     subroutine shower_base_get_final_colored_ME_momenta &
            (shower, momenta)
       import
       class(shower_base_t), intent(in) :: shower
       type(vector4_t), dimension(:), allocatable, intent(out) :: momenta
     end subroutine shower_base_get_final_colored_ME_momenta
  end interface


contains

  elemental function shower_method_of_string (string) result (i)
    integer :: i
    type(string_t), intent(in) :: string
    select case (char(string))
    case ("WHIZARD")
       i = PS_WHIZARD
    case ("PYTHIA6")
       i = PS_PYTHIA6
    case ("PYTHIA8")
       i = PS_PYTHIA8
    case default
       i = PS_UNDEFINED
    end select
  end function shower_method_of_string

  elemental function shower_method_to_string (i) result (string)
    type(string_t) :: string
    integer, intent(in) :: i
    select case (i)
    case (PS_WHIZARD)
       string = "WHIZARD"
    case (PS_PYTHIA6)
       string = "PYTHIA6"
    case (PS_PYTHIA8)
       string = "PYTHIA8"
    case default
       string = "UNDEFINED"
    end select
  end function shower_method_to_string

  subroutine shower_settings_init (settings, var_list)
    class(shower_settings_t), intent(out) :: settings
    type(var_list_t), intent(in) :: var_list

    settings%fsr_active = &
         var_list%get_lval (var_str ("?ps_fsr_active"))
    settings%isr_active = &
         var_list%get_lval (var_str ("?ps_isr_active"))
    settings%tau_dec = &
         var_list%get_lval (var_str ("?ps_taudec_active"))
    settings%muli_active = &
         var_list%get_lval (var_str ("?muli_active"))
    settings%hadronization_active = &
         var_list%get_lval (var_str ("?hadronization_active"))
    settings%mlm_matching = &
         var_list%get_lval (var_str ("?mlm_matching"))
    settings%ckkw_matching = &
         var_list%get_lval (var_str ("?ckkw_matching"))
    settings%powheg_matching = &
         var_list%get_lval (var_str ("?powheg_matching"))
    settings%method = shower_method_of_string ( &
         var_list%get_sval (var_str ("$shower_method")))
    settings%active = settings%isr_active .or. &
         settings%fsr_active .or. &
         settings%powheg_matching .or. &
         settings%muli_active .or. &
         settings%hadronization_active
    if (.not. settings%active)  return
    settings%verbose = &
         var_list%get_lval (var_str ("?shower_verbose"))
    settings%pythia6_pygive = &
         var_list%get_sval (var_str ("$ps_PYTHIA_PYGIVE"))
    settings%min_virtuality = &
         (var_list%get_rval (var_str ("ps_mass_cutoff"))**2)
    settings%fsr_lambda = &
         var_list%get_rval (var_str ("ps_fsr_lambda"))
    settings%isr_lambda = &
         var_list%get_rval (var_str ("ps_isr_lambda"))
    settings%max_n_flavors = &
         var_list%get_ival (var_str ("ps_max_n_flavors"))
    settings%isr_alphas_running = &
         var_list%get_lval (var_str ("?ps_isr_alphas_running"))
    settings%fsr_alphas_running = &
         var_list%get_lval (var_str ("?ps_fsr_alphas_running"))
    settings%fixed_alpha_s = &
         var_list%get_rval (var_str ("ps_fixed_alphas"))
    settings%isr_pt_ordered = &
         var_list%get_lval (var_str ("?ps_isr_pt_ordered"))
    settings%isr_angular_ordered = &
         var_list%get_lval (var_str ("?ps_isr_angular_ordered"))
    settings%isr_primordial_kt_width = &
         var_list%get_rval (var_str ("ps_isr_primordial_kt_width"))
    settings%isr_primordial_kt_cutoff = &
         var_list%get_rval (var_str ("ps_isr_primordial_kt_cutoff"))
    settings%isr_z_cutoff = &
         var_list%get_rval (var_str ("ps_isr_z_cutoff"))
    settings%isr_minenergy = &
         var_list%get_rval (var_str ("ps_isr_minenergy"))
    settings%isr_tscalefactor = &
         var_list%get_rval (var_str ("ps_isr_tscalefactor"))
    settings%isr_only_onshell_emitted_partons = &
         var_list%get_lval (&
         var_str ("?ps_isr_only_onshell_emitted_partons"))
  end subroutine shower_settings_init

  subroutine shower_settings_write (settings, unit)
    class(shower_settings_t), intent(in) :: settings
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A)")  "Shower settings:"
    call write_separator (u)
    write (u, "(1x,A)")  "Master switches:"
    write (u, "(3x,A,1x,L1)") &
         "ps_isr_active                = ", settings%isr_active
    write (u, "(3x,A,1x,L1)") &
         "ps_fsr_active                = ", settings%fsr_active
    write (u, "(3x,A,1x,L1)") &
         "ps_tau_dec                   = ", settings%tau_dec
    write (u, "(3x,A,1x,L1)") &
         "muli_active                  = ", settings%muli_active
    write (u, "(3x,A,1x,L1)") &
         "hadronization_active         = ", settings%hadronization_active
    write (u, "(1x,A)")  "General settings:"
    if (settings%isr_active .or. settings%fsr_active) then
       write (u, "(3x,A)") &
            "method                       =  " // &
            char (shower_method_to_string (settings%method))
       write (u, "(3x,A,1x,L1)") &
            "shower_verbose               = ", settings%verbose
       write (u, "(3x,A,ES19.12)") &
            "ps_mass_cutoff               = ", &
            sqrt (abs (settings%min_virtuality))
       write (u, "(3x,A,1x,I1)") &
            "ps_max_n_flavors             = ", settings%max_n_flavors
    else
       write (u, "(3x,A)") " [ISR and FSR off]"
    end if
    if (settings%isr_active) then
       write (u, "(1x,A)")  "ISR settings:"
       write (u, "(3x,A,1x,L1)") &
            "ps_isr_pt_ordered            = ", settings%isr_pt_ordered
       write (u, "(3x,A,ES19.12)") &
            "ps_isr_lambda                = ", settings%isr_lambda
       write (u, "(3x,A,1x,L1)") &
            "ps_isr_alphas_running        = ", settings%isr_alphas_running
       write (u, "(3x,A,ES19.12)") &
            "ps_isr_primordial_kt_width   = ", settings%isr_primordial_kt_width
       write (u, "(3x,A,ES19.12)") &
            "ps_isr_primordial_kt_cutoff  = ", &
            settings%isr_primordial_kt_cutoff
       write (u, "(3x,A,ES19.12)") &
            "ps_isr_z_cutoff              = ", settings%isr_z_cutoff
       write (u, "(3x,A,ES19.12)") &
            "ps_isr_minenergy             = ", settings%isr_minenergy
       write (u, "(3x,A,ES19.12)") &
            "ps_isr_tscalefactor          = ", settings%isr_tscalefactor
    else if (settings%fsr_active) then
       write (u, "(3x,A)") " [ISR off]"
    end if
    if (settings%fsr_active) then
       write (u, "(1x,A)")  "FSR settings:"
       write (u, "(3x,A,ES19.12)") &
            "ps_fsr_lambda                = ", settings%fsr_lambda
       write (u, "(3x,A,1x,L1)") &
            "ps_fsr_alphas_running        = ", settings%fsr_alphas_running
    else if (settings%isr_active) then
       write (u, "(3x,A)") " [FSR off]"
    end if
    write (u, "(1x,A)")  "Matching Settings:"
    write (u, "(3x,A,1x,L1)") &
         "mlm_matching                 = ", settings%mlm_matching
    write (u, "(3x,A,1x,L1)") &
         "ckkw_matching                = ", settings%ckkw_matching
    write (u, "(1x,A)")  "PYTHIA6 specific settings:"
    write (u, "(3x,A,A,A)") &
         "ps_PYTHIA_PYGIVE             = '", &
         char(settings%pythia6_pygive), "'"
  end subroutine shower_settings_write

  subroutine shower_base_write_msg (shower)
    class(shower_base_t), intent(inout) :: shower
    call msg_message ("Shower: Using " // char(shower%name) // " shower")
  end subroutine shower_base_write_msg

  pure subroutine shower_base_import_rng (shower, rng)
    class(shower_base_t), intent(inout) :: shower
    class(rng_t), intent(inout), allocatable :: rng
    call move_alloc (from = rng, to = shower%rng)
  end subroutine shower_base_import_rng

  function D_alpha_s_isr (tin, settings) result (alpha_s)
    real(default), intent(in) :: tin
    type(shower_settings_t), intent(in) :: settings
    real(default) :: min_virtuality, d_constalpha_s, d_lambda_isr
    integer :: d_nf
    real(default) :: t
    real(default) :: alpha_s
    min_virtuality = settings%min_virtuality
    d_lambda_isr = settings%isr_lambda
    d_constalpha_s = settings%fixed_alpha_s
    d_nf = settings%max_n_flavors
    if (settings%alpha_s_fudged) then
       t = max (max (0.1_default * min_virtuality, &
                     1.1_default * d_lambda_isr**2), abs(tin))
    else
       t = abs(tin)
    end if
    if (settings%isr_alphas_running) then
       alpha_s = running_as_lam (number_of_flavors(t, d_nf, min_virtuality), &
            sqrt(t), d_lambda_isr, 0)
    else
       alpha_s = d_constalpha_s
    end if
  end function D_alpha_s_isr

  function D_alpha_s_fsr (tin, settings) result (alpha_s)
    real(default), intent(in) :: tin
    type(shower_settings_t), intent(in) :: settings
    real(default) :: min_virtuality, d_lambda_fsr, d_constalpha_s
    integer :: d_nf
    real(default) :: t
    real(default) :: alpha_s
    min_virtuality = settings%min_virtuality
    d_lambda_fsr = settings%fsr_lambda
    d_constalpha_s = settings%fixed_alpha_s
    d_nf = settings%max_n_flavors
    if (settings%alpha_s_fudged) then
       t = max (max (0.1_default * min_virtuality, &
                     1.1_default * d_lambda_fsr**2), abs(tin))
    else
       t = abs(tin)
    end if
    if (settings%fsr_alphas_running) then
       alpha_s = running_as_lam (number_of_flavors (t, d_nf, min_virtuality), &
            sqrt(t), d_lambda_fsr, 0)
    else
       alpha_s = d_constalpha_s
    end if
  end function D_alpha_s_fsr

  elemental function mass_type (type, m2_default) result (mass)
    integer, intent(in) :: type
    real(default), intent(in) :: m2_default
    real(default) :: mass
    mass = sqrt (mass_squared_type (type, m2_default))
  end function mass_type

  elemental function mass_squared_type (type, m2_default) result (mass2)
    integer, intent(in) :: type
    real(default), intent(in) :: m2_default
    real(default) :: mass2
    select case (abs (type))
    !!!    case (1,2)
    !!!       if (treat_light_quarks_massless .or. &
    !!!            treat_duscb_quarks_massless) then
    !!!          mass2 = zero
    !!!       else
    !!!          mass2 = 0.330_default**2
    !!!       end if
    !!!    case (3)
    !!!       if (treat_duscb_quarks_massless) then
    !!!          mass2 = zero
    !!!       else
    !!!          mass2 = 0.500_default**2
    !!!       end if
    !!!    case (4)
    !!!       if (treat_duscb_quarks_massless) then
    !!!          mass2 = zero
    !!!       else
    !!!          mass2 = 1.500_default**2
    !!!       end if
    !!!    case (5)
    !!!       if (treat_duscb_quarks_massless) then
    !!!          mass2 = zero
    !!!       else
    !!!          mass2 = 4.800_default**2
    !!!       end if
    !!!    case (GLUON)
    !!!       mass2 = zero
    case (NEUTRON)
       mass2 = 0.939565_default**2
    case (PROTON)
       mass2 = 0.93827_default**2
    case (DPLUS)
       mass2 = 1.86960_default**2
    case (D0)
       mass2 = 1.86483_default**2
    case (B0)
       mass2 = 5.27950_default**2
    case (BPLUS)
       mass2 = 5.27917_default**2
    case (DELTAPLUSPLUS)
       mass2 = 1.232_default**2
    case (SIGMA0)
       mass2 = 1.192642_default**2
    case (SIGMAPLUS)
       mass2 = 1.18937_default**2
    case (SIGMACPLUS)
       mass2 = 2.4529_default**2
    case (SIGMACPLUSPLUS)
       mass2 = 2.45402_default**2
    case (SIGMAB0)
       mass2 = 5.8152_default**2
    case (SIGMABPLUS)
       mass2 = 5.8078_default**2
    case (BEAM_REMNANT)
       mass2 = zero !!! don't know how to handle the beamremnant
    case default
       mass2 = m2_default
    end select
  end function mass_squared_type

  elemental function number_of_flavors (t, d_nf, min_virtuality) result (nr)
    real(default), intent(in) :: t, min_virtuality
    integer, intent(in) :: d_nf
    real(default) :: nr
    integer :: i
    nr = 0
    if (t < min_virtuality) return   ! arbitrary cut off
    ! TODO: do i = 1, min (max (3, d_nf), 6)
    do i = 1, min (3, d_nf)
    !!! to do: take heavier quarks(-> cuts on allowed costheta in g->qq)
    !!!        into account
       if ((four * mass_squared_type (i, zero) + min_virtuality) < t ) then
          nr = i
       else
          exit
       end if
    end do
  end function number_of_flavors


end module shower_base
