! WHIZARD 2.2.6 May 02 2015
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

module shower_base

  use kinds, only: default, double
  use iso_varying_string, string_t => varying_string
  use io_units
  use constants
  use diagnostics
  use format_utils, only: write_separator
  use os_interface
  use rng_base
  use physics_defs
  use lhapdf !NODEP!
  use pdf_builtin !NODEP!
  use sm_physics, only: running_as_lam
  use particles
  use variables
  use model_data

  implicit none
  private

  integer, parameter :: PS_WHIZARD = 1
  integer, parameter :: PS_PYTHIA6 = 2
  integer, parameter :: PS_PYTHIA8 = 3
  integer, parameter :: PS_UNDEFINED = 17
  integer, parameter, public :: STRF_NONE = 0
  integer, parameter, public :: STRF_LHAPDF6 = 1
  integer, parameter, public :: STRF_LHAPDF5 = 2
  integer, parameter, public :: STRF_PDF_BUILTIN = 3

  logical, parameter, public :: DEBUG_WHIZ_SHOWER = .false.
  logical, parameter, public :: ENSURE = .false.
  logical, parameter, public :: DEBUG_SHOWER = .false.
  real(default), public :: D_min_scale = 0.5_default
  logical, public :: treat_light_quarks_massless = .true.
  logical, public :: treat_duscb_quarks_massless = .false.
  real(default), public :: scalefactor1 = 0.02_default
  real(default), public :: scalefactor2 = 0.02_default

  public :: PS_WHIZARD, PS_PYTHIA6, PS_PYTHIA8, PS_UNDEFINED
  public :: shower_method_of_string
  public :: shower_method_to_string
  public :: pdf_data_t
  public :: shower_settings_t
  public :: matching_settings_t
  public :: matching_data_t
  public :: shower_base_t
  public :: D_alpha_s_isr
  public :: D_alpha_s_fsr
  public :: mass_type
  public :: mass_squared_type
  public :: number_of_flavors

  type :: pdf_data_t
    type(lhapdf_pdf_t) :: pdf
    real(default) :: xmin, xmax, qmin, qmax
    integer :: type = STRF_NONE
    integer :: set = 0
   contains
     procedure :: init => pdf_data_init
     procedure :: write => pdf_data_write
     procedure :: evolve => pdf_data_evolve
  end type pdf_data_t

  type :: shower_settings_t
     logical :: active = .false.
     logical :: isr_active = .false.
     logical :: fsr_active = .false.
     logical :: muli_active = .false.
     logical :: verbose = .false.
     integer :: method = PS_UNDEFINED
     logical :: hadronization_active = .false.
     logical :: mlm_matching = .false.
     logical :: ckkw_matching = .false.
     logical :: powheg_matching = .false.
     type(string_t) :: pythia6_pygive
     !!! values present in PYTHIA and WHIZARDs PS,
     !!! comments denote corresponding PYTHIA values
     real(default) :: d_min_t = 1._default          ! PARJ(82)^2
     real(default) :: fsr_lambda = 0.29_default     ! PARP(72)
     real(default) :: isr_lambda = 0.29_default     ! PARP(61)
     integer :: max_n_flavors = 5                   ! MSTJ(45)
     logical :: isr_alpha_s_running = .true.        ! MSTP(64)
     logical :: fsr_alpha_s_running = .true.        ! MSTJ(44)
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

  type, abstract :: matching_settings_t
   contains
     procedure (matching_settings_init), deferred :: init
     procedure (matching_settings_write), deferred :: write
  end type matching_settings_t

  type, abstract :: matching_data_t
     logical :: is_hadron_collision = .false.
  end type matching_data_t

  type, abstract :: shower_base_t
     class(rng_t), allocatable :: rng
     type(string_t) :: name
     type(pdf_data_t) :: pdf_data
     type(shower_settings_t) :: settings
   contains
     procedure :: write_msg => shower_base_write_msg
     procedure :: import_rng => shower_base_import_rng
     procedure (shower_base_init), deferred :: init
     procedure (shower_base_generate_emissions), deferred :: generate_emissions
  end type shower_base_t


  abstract interface
     subroutine matching_settings_init (settings, var_list)
       import
       class(matching_settings_t), intent(out) :: settings
       type(var_list_t), intent(in) :: var_list
     end subroutine matching_settings_init
  end interface

  abstract interface
     subroutine matching_settings_write (settings, unit)
       import
       class(matching_settings_t), intent(in) :: settings
       integer, intent(in), optional :: unit
     end subroutine matching_settings_write
  end interface

  abstract interface
    subroutine shower_base_init (shower, settings, pdf_data)
      import
      class(shower_base_t), intent(out) :: shower
      type(shower_settings_t), intent(in) :: settings
      type(pdf_data_t), intent(in) :: pdf_data
    end subroutine shower_base_init
   end interface

  abstract interface
    subroutine shower_base_generate_emissions (shower, particle_set, &
       model, model_hadrons, os_data, matching_settings, data, &
       valid, vetoed, number_of_emissions)
      import
      class(shower_base_t), intent(inout), target :: shower
      type(particle_set_t), intent(inout) :: particle_set
      class(model_data_t), intent(in), target :: model
      class(model_data_t), intent(in), target :: model_hadrons
      class(matching_settings_t), intent(in), allocatable :: matching_settings
      class(matching_data_t), intent(inout), allocatable :: data
      type(os_data_t), intent(in) :: os_data
      logical, intent(inout) :: valid
      logical, intent(inout) :: vetoed
      integer, optional, intent(in) :: number_of_emissions
    end subroutine shower_base_generate_emissions
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

  subroutine pdf_data_init (pdf_data, pdf_data_in)
    class(pdf_data_t), intent(out) :: pdf_data
    type(pdf_data_t), target, intent(in) :: pdf_data_in
    pdf_data%xmin = pdf_data_in%xmin
    pdf_data%xmax = pdf_data_in%xmax
    pdf_data%qmin = pdf_data_in%qmin
    pdf_data%qmax = pdf_data_in%qmax
    pdf_data%set = pdf_data_in%set
    pdf_data%type = pdf_data_in%type
    if (pdf_data%type == STRF_LHAPDF6) then
       if (pdf_data_in%pdf%is_associated ()) then
          call lhapdf_copy_pointer (pdf_data_in%pdf, pdf_data%pdf)
       else
          call msg_bug ('pdf_data_init: pdf_data%pdf was not associated!')
       end if
    end if
  end subroutine pdf_data_init

  subroutine pdf_data_write (pdf_data, unit)
    class(pdf_data_t), intent(in) :: pdf_data
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit); if (u < 0) return
    write (u, "(3x,A,I0)") "PDF set  = ", pdf_data%set
    write (u, "(3x,A,I0)") "PDF type = ", pdf_data%type
  end subroutine pdf_data_write

  subroutine pdf_data_evolve (pdf_data, x, q_in, f)
    class(pdf_data_t), intent(inout) :: pdf_data
    real(double), intent(in) :: x, q_in
    real(double), dimension(-6:6), intent(out) :: f
    real(double) :: q
    select case (pdf_data%type)
    case (STRF_PDF_BUILTIN)
       call pdf_evolve_LHAPDF (pdf_data%set, x, q_in, f)
    case (STRF_LHAPDF6)
       q = min (pdf_data%qmax, q_in)
       q = max (pdf_data%qmin, q)
       call pdf_data%pdf%evolve_pdfm (x, q, f)
    case (STRF_LHAPDF5)
       q = min (pdf_data%qmax, q_in)
       q = max (pdf_data%qmin, q)
       call evolvePDFM (pdf_data%set, x, q, f)
    case default
       call msg_fatal ("PDF function: unknown PDF method.")
    end select
  end subroutine pdf_data_evolve
  subroutine shower_settings_init (shower_settings, var_list)
    class(shower_settings_t), intent(out) :: shower_settings
    type(var_list_t), intent(in) :: var_list

    shower_settings%fsr_active = &
         var_list%get_lval (var_str ("?ps_fsr_active"))
    shower_settings%isr_active = &
         var_list%get_lval (var_str ("?ps_isr_active"))
    shower_settings%muli_active = &
         var_list%get_lval (var_str ("?muli_active"))
    shower_settings%hadronization_active = &
         var_list%get_lval (var_str ("?hadronization_active"))
    shower_settings%mlm_matching = &
         var_list%get_lval (var_str ("?mlm_matching"))
    shower_settings%ckkw_matching = &
         var_list%get_lval (var_str ("?ckkw_matching"))
    shower_settings%powheg_matching = &
         var_list%get_lval (var_str ("?powheg_matching"))

    shower_settings%method = shower_method_of_string ( &
         var_list%get_sval (var_str ("$shower_method")))

    !!! We have to split off hadronization settings at some point.

    shower_settings%active = shower_settings%isr_active .or. &
         shower_settings%fsr_active .or. &
         shower_settings%powheg_matching .or. &
         shower_settings%muli_active .or. &
         shower_settings%hadronization_active
    if (.not. shower_settings%active)  return
    shower_settings%verbose = &
         var_list%get_lval (var_str ("?shower_verbose"))
    shower_settings%pythia6_pygive = &
         var_list%get_sval (var_str ("$ps_PYTHIA_PYGIVE"))
    shower_settings%d_min_t = &
         (var_list%get_rval (var_str ("ps_mass_cutoff"))**2)
    shower_settings%fsr_lambda = &
         var_list%get_rval (var_str ("ps_fsr_lambda"))
    shower_settings%isr_lambda = &
         var_list%get_rval (var_str ("ps_isr_lambda"))
    shower_settings%max_n_flavors = &
         var_list%get_ival (var_str ("ps_max_n_flavors"))
    shower_settings%isr_alpha_s_running = &
         var_list%get_lval (var_str ("?ps_isr_alpha_s_running"))
    shower_settings%fsr_alpha_s_running = &
         var_list%get_lval (var_str ("?ps_fsr_alpha_s_running"))
    shower_settings%fixed_alpha_s = &
         var_list%get_rval (var_str ("ps_fixed_alpha_s"))
    shower_settings%isr_pt_ordered = &
         var_list%get_lval (var_str ("?ps_isr_pt_ordered"))
    shower_settings%isr_angular_ordered = &
         var_list%get_lval (var_str ("?ps_isr_angular_ordered"))
    shower_settings%isr_primordial_kt_width = &
         var_list%get_rval (var_str ("ps_isr_primordial_kt_width"))
    shower_settings%isr_primordial_kt_cutoff = &
         var_list%get_rval (var_str ("ps_isr_primordial_kt_cutoff"))
    shower_settings%isr_z_cutoff = &
         var_list%get_rval (var_str ("ps_isr_z_cutoff"))
    shower_settings%isr_minenergy = &
         var_list%get_rval (var_str ("ps_isr_minenergy"))
    shower_settings%isr_tscalefactor = &
         var_list%get_rval (var_str ("ps_isr_tscalefactor"))
    shower_settings%isr_only_onshell_emitted_partons = &
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
    write (u, "(1x,A)")  "General settings:"
    if (settings%isr_active .or. settings%fsr_active) then
       write (u, "(3x,A)") &
            "shower_method                =  " // &
            char (shower_method_to_string (settings%method))
       write (u, "(3x,A,1x,L1)") &
            "shower_verbose               = ", settings%verbose
       write (u, "(3x,A,ES19.12)") &
            "ps_mass_cutoff               = ", sqrt (abs (settings%d_min_t))
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
            "ps_isr_alpha_s_running       = ", settings%isr_alpha_s_running
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
            "ps_fsr_alpha_s_running       = ", settings%fsr_alpha_s_running
    else if (settings%isr_active) then
       write (u, "(3x,A)") " [FSR off]"
    end if
    write (u, "(1x,A)")  "Hadronization settings:"
    write (u, "(3x,A,1x,L1)") &
         "hadronization_active         = ", settings%hadronization_active
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
    real(default) :: d_min_t, d_constalpha_s, d_lambda_isr
    integer :: d_nf
    real(default) :: t
    real(default) :: alpha_s
    d_min_t = settings%d_min_t
    d_lambda_isr = settings%isr_lambda
    d_constalpha_s = settings%fixed_alpha_s
    d_nf = settings%max_n_flavors
    if (settings%alpha_s_fudged) then
       t = max (max (0.1_default * d_min_t, &
                     1.1_default * d_lambda_isr**2), abs(tin))
    else
       t = abs(tin)
    end if
    if (settings%isr_alpha_s_running) then
       alpha_s = running_as_lam (number_of_flavors(t, d_nf, d_min_t), &
            sqrt(t), d_lambda_isr, 0)
    else
       alpha_s = d_constalpha_s
    end if
  end function D_alpha_s_isr

  function D_alpha_s_fsr (tin, settings) result (alpha_s)
    real(default), intent(in) :: tin
    type(shower_settings_t), intent(in) :: settings
    real(default) :: d_min_t, d_lambda_fsr, d_constalpha_s
    integer :: d_nf
    real(default) :: t
    real(default) :: alpha_s
    d_min_t = settings%d_min_t
    d_lambda_fsr = settings%fsr_lambda
    d_constalpha_s = settings%fixed_alpha_s
    d_nf = settings%max_n_flavors
    if (settings%alpha_s_fudged) then
       t = max (max (0.1_default * d_min_t, &
                     1.1_default * d_lambda_fsr**2), abs(tin))
    else
       t = abs(tin)
    end if
    if (settings%fsr_alpha_s_running) then
       alpha_s = running_as_lam (number_of_flavors (t, d_nf, d_min_t), &
            sqrt(t), d_lambda_fsr, 0)
    else
       alpha_s = d_constalpha_s
    end if
  end function D_alpha_s_fsr

  elemental function mass_type (type) result (mass)
    integer, intent(in) :: type
    real(default) :: mass
    mass = sqrt (mass_squared_type (type))
  end function mass_type

  elemental function mass_squared_type (type) result (mass2)
    integer, intent(in) :: type
    real(default) :: mass2

    select case (abs (type))
    case (1,2)
       if (treat_light_quarks_massless .or. &
            treat_duscb_quarks_massless) then
          mass2 = zero
       else
          mass2 = 0.330_default**2
       end if
    case (3)
       if (treat_duscb_quarks_massless) then
          mass2 = zero
       else
          mass2 = 0.500_default**2
       end if
    case (4)
       if (treat_duscb_quarks_massless) then
          mass2 = zero
       else
          mass2 = 1.500_default**2
       end if
    case (5)
       if (treat_duscb_quarks_massless) then
          mass2 = zero
       else
          mass2 = 4.800_default**2
       end if
    case (6)
       mass2 = 175.00_default**2
    case (GLUON)
       mass2 = zero
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
    case (UNDEFINED)
       mass2 = zero
    case (BEAM_REMNANT)
       mass2 = zero !!! don't know how to handle the beamremnant
    case default
       mass2 = zero
    end select
  end function mass_squared_type

  elemental function number_of_flavors (t, d_nf, d_min_t) result (nr)
    real(default), intent(in) :: t, d_min_t
    integer, intent(in) :: d_nf
    real(default) :: nr
    integer :: i
    nr = 0
    if (t < d_min_t) return   ! arbitrary cut off
    ! TODO: do i = 1, min (max (3, d_nf), 6)
    do i = 1, min (3, d_nf)
    !!! to do: take heavier quarks(-> cuts on allowed costheta in g->qq)
    !!!        into account
       if ((four * mass_squared_type (i) + d_min_t) < t ) then
          nr = i
       else
          exit
       end if
    end do
  end function number_of_flavors


end module shower_base
