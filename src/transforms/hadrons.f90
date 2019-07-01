! WHIZARD 2.4.0 Nov 28 2016
! 
! Copyright (C) 1999-2016 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     So Young Shim <soyoung.shim@desy.de>
!     Florian Staub <florian.staub@cern.ch>  
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, So-young Shim, Daniel Wiesler 
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

module hadrons

  use kinds, only: default, double
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_utils, only: write_separator
  use diagnostics
  use sm_qcd
  use lorentz
  use subevents, only: PRT_OUTGOING
  use particles
  use variables
  use model_data
  use models
  use rng_base
  use hep_common
  use event_transforms
  use shower_base
  use shower_pythia6
  use process
  
  implicit none
  private

  public :: HADRONS_UNDEFINED, HADRONS_WHIZARD, HADRONS_PYTHIA6, HADRONS_PYTHIA8
  public :: hadrons_method
  public :: hadron_settings_t
  public :: hadrons_hadrons_t
  public :: had_flav_t
  public :: lund_end
  public :: lund_pt_t
  public :: hadrons_pythia6_t
  public :: hadrons_pythia8_t
  public :: evt_hadrons_t

  integer, parameter :: HADRONS_UNDEFINED = 0  
  integer, parameter :: HADRONS_WHIZARD = 1
  integer, parameter :: HADRONS_PYTHIA6 = 2
  integer, parameter :: HADRONS_PYTHIA8 = 3

  type :: hadron_settings_t
     logical :: active = .false.
     integer :: method = HADRONS_UNDEFINED
     real(default) :: enhanced_fraction = 0
     real(default) :: enhanced_width = 0
   contains
     procedure :: init => hadron_settings_init
     procedure :: write => hadron_settings_write
  end type hadron_settings_t

  type, abstract :: hadrons_t
     class(rng_t), allocatable :: rng
     type(shower_settings_t) :: shower_settings
     type(hadron_settings_t) :: hadron_settings
     type(model_t), pointer :: model => null()
   contains
     procedure (hadrons_init), deferred :: init
     procedure (hadrons_hadronize), deferred :: hadronize
     procedure (hadrons_make_particle_set), deferred :: make_particle_set
     procedure :: import_rng => hadrons_import_rng
  end type hadrons_t

  type, extends (hadrons_t) :: hadrons_hadrons_t
     contains
         procedure :: init => hadrons_hadrons_init
         procedure :: hadronize => hadrons_hadrons_hadronize
         procedure :: make_particle_set => hadrons_hadrons_make_particle_set
    end type hadrons_hadrons_t

  type had_flav_t
  end type had_flav_t  

  type lund_end
     logical :: from_pos
     integer :: i_end
     integer :: i_max
     integer :: id_had
     integer :: i_pos_old
     integer :: i_neg_old
     integer :: i_pos_new
     integer :: i_neg_new
     real(default) :: px_old
     real(default) :: py_old
     real(default) :: px_new
     real(default) :: py_new
     real(default) :: px_had
     real(default) :: py_had
     real(default) :: m_had
     real(default) :: mT2_had
     real(default) :: z_had
     real(default) :: gamma_old
     real(default) :: gamma_new
     real(default) :: x_pos_old
     real(default) :: x_pos_new
     real(default) :: x_pos_had
     real(default) :: x_neg_old
     real(default) :: x_neg_new
     real(default) :: x_neg_had     
     type(had_flav_t) :: old_flav
     type(had_flav_t) :: new_flav
     type(vector4_t) :: p_had
     type(vector4_t) :: p_pre
  end type lund_end

  type lund_pt_t
     real(default) :: sigma_min
     real(default) :: sigma_q
     real(default) :: enhanced_frac
     real(default) :: enhanced_width
     real(default) :: sigma_to_had
     class(rng_t), allocatable :: rng
   contains
     procedure :: init => lund_pt_init  
  end type lund_pt_t

  type, extends (hadrons_t) :: hadrons_pythia6_t
   contains
     procedure :: init => hadrons_pythia6_init
     procedure :: hadronize => hadrons_pythia6_hadronize
     procedure :: make_particle_set => hadrons_pythia6_make_particle_set
  end type hadrons_pythia6_t

  type,extends (hadrons_t) :: hadrons_pythia8_t
  contains
     procedure :: init => hadrons_pythia8_init
     procedure :: hadronize => hadrons_pythia8_hadronize
     procedure :: make_particle_set => hadrons_pythia8_make_particle_set
  end type hadrons_pythia8_t

  type, extends (evt_t) :: evt_hadrons_t
     class(hadrons_t), allocatable :: hadrons
     type(model_t), pointer :: model_hadrons => null()
     type(qcd_t), pointer :: qcd_t => null()
     logical :: is_first_event
   contains
     procedure :: init => evt_hadrons_init
     procedure :: write_name => evt_hadrons_write_name
     procedure :: write => evt_hadrons_write
     procedure :: first_event => evt_hadrons_first_event
     procedure :: generate_weighted => evt_hadrons_generate_weighted
     procedure :: make_particle_set => evt_hadrons_make_particle_set
     procedure :: make_rng => evt_hadrons_make_rng
     procedure :: prepare_new_event => evt_hadrons_prepare_new_event
  end type evt_hadrons_t


  interface hadrons_method
     module procedure hadrons_method_of_string
     module procedure hadrons_method_to_string
  end interface
  abstract interface
    subroutine hadrons_init &
         (hadrons, shower_settings, hadron_settings, model_hadrons)
      import
      class(hadrons_t), intent(out) :: hadrons
      type(shower_settings_t), intent(in) :: shower_settings
      type(hadron_settings_t), intent(in) :: hadron_settings
      type(model_t), target, intent(in) :: model_hadrons
    end subroutine hadrons_init
   end interface

  abstract interface
     subroutine hadrons_hadronize (hadrons, particle_set, valid)
       import
       class(hadrons_t), intent(inout) :: hadrons
       type(particle_set_t), intent(in) :: particle_set
       logical, intent(out) :: valid
     end subroutine hadrons_hadronize
  end interface
  abstract interface
     subroutine hadrons_make_particle_set (hadrons, particle_set, &
          model, valid)
       import
       class(hadrons_t), intent(in) :: hadrons
       type(particle_set_t), intent(inout) :: particle_set
       class(model_data_t), intent(in), target :: model
       logical, intent(out) :: valid
     end subroutine hadrons_make_particle_set
  end interface


contains

  elemental function hadrons_method_of_string (string) result (i)
    integer :: i
    type(string_t), intent(in) :: string
    select case (char(string))
    case ("WHIZARD")
       i = HADRONS_WHIZARD
    case ("PYTHIA6")
       i = HADRONS_PYTHIA6
    case ("PYTHIA8")
       i = HADRONS_PYTHIA8
    case default
       i = HADRONS_UNDEFINED
    end select
  end function hadrons_method_of_string

  elemental function hadrons_method_to_string (i) result (string)
    type(string_t) :: string
    integer, intent(in) :: i
    select case (i)
    case (HADRONS_WHIZARD)
       string = "WHIZARD"
    case (HADRONS_PYTHIA6)
       string = "PYTHIA6"
    case (HADRONS_PYTHIA8)
       string = "PYTHIA8"
    case default
       string = "UNDEFINED"
    end select
  end function hadrons_method_to_string

  subroutine hadron_settings_init (hadron_settings, var_list)
    class(hadron_settings_t), intent(out) :: hadron_settings
    type(var_list_t), intent(in) :: var_list
    hadron_settings%active = &
         var_list%get_lval (var_str ("?hadronization_active"))
    hadron_settings%method = hadrons_method_of_string ( &
         var_list%get_sval (var_str ("$hadronization_method")))
    hadron_settings%enhanced_fraction = &
         var_list%get_rval (var_str ("hadron_enhanced_fraction"))
    hadron_settings%enhanced_width = &
         var_list%get_rval (var_str ("hadron_enhanced_width"))
  end subroutine hadron_settings_init

  subroutine hadron_settings_write (settings, unit)
    class(hadron_settings_t), intent(in) :: settings
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A)")  "Hadronization settings:"
    call write_separator (u)
    write (u, "(1x,A)")  "Master switches:"
    write (u, "(3x,A,1x,L1)") &
         "active                = ", settings%active
    write (u, "(1x,A)")  "General settings:"
    if (settings%active) then
       write (u, "(3x,A)") &
            "hadron_method         =  " // &
            char (hadrons_method_to_string (settings%method))
    else
       write (u, "(3x,A)") " [Hadronization off]"
    end if
    write (u, "(1x,A)")  "pT generation parameters"
    write (u, "(3x,A,1x,ES19.12)") &
         "enhanced_fraction     = ", settings%enhanced_fraction
    write (u, "(3x,A,1x,ES19.12)") &
         "enhanced_width        = ", settings%enhanced_width    
  end subroutine hadron_settings_write

  pure subroutine hadrons_import_rng (hadrons, rng)
    class(hadrons_t), intent(inout) :: hadrons
    class(rng_t), intent(inout), allocatable :: rng
    call move_alloc (from = rng, to = hadrons%rng)
  end subroutine hadrons_import_rng

  subroutine hadrons_hadrons_init &
       (hadrons, shower_settings, hadron_settings, model_hadrons)
    class(hadrons_hadrons_t), intent(out) :: hadrons
    type(shower_settings_t), intent(in) :: shower_settings
    type(hadron_settings_t), intent(in) :: hadron_settings
    type(model_t), intent(in), target :: model_hadrons
    hadrons%model => model_hadrons
    hadrons%shower_settings = shower_settings
    hadrons%hadron_settings = hadron_settings
    call msg_message &
         ("Hadronization: WHIZARD model for hadronization and decays")
  end subroutine hadrons_hadrons_init

  subroutine hadrons_hadrons_hadronize (hadrons, particle_set, valid)
    class(hadrons_hadrons_t), intent(inout) :: hadrons
    type(particle_set_t), intent(in) :: particle_set
    logical, intent(out) :: valid
    integer, dimension(:), allocatable :: cols, acols, octs
    integer :: n
    if (signal_is_pending ()) return
    call msg_debug (D_TRANSFORMS, "hadrons_hadrons_hadronize")
    call particle_set%write (6, compressed=.true.)
    n = particle_set%get_n_tot ()
    allocate (cols (n), acols (n), octs (n))
    call extract_color_systems (particle_set, cols, acols, octs)
    print *, "size(cols)  = ", size (cols)
    if (size(cols) > 0) then
       print *, "cols  = ", cols
    end if
    print *, "size(acols) = ", size(acols)    
    if (size(acols) > 0) then
       print *, "acols = ", acols
    end if
    print *, "size(octs)  = ", size(octs)    
    if (size (octs) > 0) then
       print *, "octs  = ", octs
    end if
    !!! if all arrays are empty, i.e. zero particles found, nothing to do
  end subroutine hadrons_hadrons_hadronize

  subroutine lund_pt_init (lund_pt, settings)
    class (lund_pt_t), intent(out) :: lund_pt
    type(hadron_settings_t), intent(in) :: settings
  end subroutine lund_pt_init

  subroutine hadrons_hadrons_make_particle_set &
         (hadrons, particle_set, model, valid)
    class(hadrons_hadrons_t), intent(in) :: hadrons
    type(particle_set_t), intent(inout) :: particle_set
    class(model_data_t), intent(in), target :: model
    logical, intent(out) :: valid
    if (signal_is_pending ()) return
    valid = .false.
    if (valid) then
    else
       call msg_fatal ("WHIZARD hadronization not yet implemented")
    end if
  end subroutine hadrons_hadrons_make_particle_set

  subroutine extract_color_systems (p_set, cols, acols, octs)
    type(particle_set_t), intent(in) :: p_set
    integer, dimension(:), allocatable, intent(out) :: cols, acols, octs
    logical, dimension(:), allocatable :: mask
    integer :: i, n, n_cols, n_acols, n_octs
    n = p_set%get_n_tot ()
    allocate (mask (n))
    do i = 1, n
       mask(i) = p_set%prt(i)%col%get_col () /= 0 .and. &
            p_set%prt(i)%col%get_acl () == 0 .and. &
            p_set%prt(i)%get_status () == PRT_OUTGOING
    end do
    n_cols = count (mask)
    allocate (cols (n_cols))
    cols = p_set%get_indices (mask)
    do i = 1, n
       mask(i) = p_set%prt(i)%col%get_col () == 0 .and. &
            p_set%prt(i)%col%get_acl () /= 0 .and. &
            p_set%prt(i)%get_status () == PRT_OUTGOING
    end do
    n_acols = count (mask)
    allocate (acols (n_acols))    
    acols = p_set%get_indices (mask)
    do i = 1, n
       mask(i) = p_set%prt(i)%col%get_col () /= 0 .and. &
            p_set%prt(i)%col%get_acl () /= 0 .and. &
            p_set%prt(i)%get_status () == PRT_OUTGOING
    end do
    n_octs = count (mask)
    allocate (octs (n_octs))
    octs = p_set%get_indices (mask)
  end subroutine extract_color_systems

  subroutine hadrons_pythia6_init &
       (hadrons, shower_settings, hadron_settings, model_hadrons)
    class(hadrons_pythia6_t), intent(out) :: hadrons
    type(shower_settings_t), intent(in) :: shower_settings
    type(hadron_settings_t), intent(in) :: hadron_settings
    type(model_t), intent(in), target :: model_hadrons
    logical :: pygive_not_set_by_shower
    hadrons%model => model_hadrons
    hadrons%shower_settings = shower_settings
    hadrons%hadron_settings = hadron_settings
    pygive_not_set_by_shower = .not. (shower_settings%method == PS_PYTHIA6 &
         .and. (shower_settings%isr_active .or. shower_settings%fsr_active))
    if (pygive_not_set_by_shower) then
       call pythia6_set_verbose (shower_settings%verbose)
       call pythia6_set_config (shower_settings%pythia6_pygive)
    end if
    call msg_message &
         ("Hadronization: Using PYTHIA6 interface for hadronization and decays")
  end subroutine hadrons_pythia6_init

  subroutine hadrons_pythia6_hadronize (hadrons, particle_set, valid)
    class(hadrons_pythia6_t), intent(inout) :: hadrons
    type(particle_set_t), intent(in) :: particle_set
    logical, intent(out) :: valid
    integer :: N, NPAD, K
    real(double) :: P, V
    common /PYJETS/ N, NPAD, K(4000,5), P(4000,5), V(4000,5)
    save /PYJETS/
    if (signal_is_pending ()) return
    call msg_debug (D_TRANSFORMS, "hadrons_pythia6_hadronize")
    call pygive ("MSTP(111)=1")    !!! Switch on hadronization and decays
    call pygive ("MSTJ(1)=1")      !!! String fragmentation
    call pygive ("MSTJ(21)=2")     !!! String fragmentation keeping resonance momentum
    call pygive ("MSTJ(28)=0")     !!! Switch off tau decays
    if (debug_active (D_TRANSFORMS)) then
       call msg_debug (D_TRANSFORMS, "N", N)
       call pylist(2)
       print *, ' line 7 : ', k(7,1:5), p(7,1:5)
    end if
    call pyedit (12)
    call pythia6_set_last_treated_line (N)
    call pyexec ()
    call pyedit (12)
    valid = .true.
  end subroutine hadrons_pythia6_hadronize

  subroutine hadrons_pythia6_make_particle_set &
         (hadrons, particle_set, model, valid)
    class(hadrons_pythia6_t), intent(in) :: hadrons
    type(particle_set_t), intent(inout) :: particle_set
    class(model_data_t), intent(in), target :: model
    logical, intent(out) :: valid
    if (signal_is_pending ()) return
    valid = pythia6_handle_errors ()
    if (valid) then
       call pythia6_combine_with_particle_set &
            (particle_set, model, hadrons%model, hadrons%shower_settings)
    end if
  end subroutine hadrons_pythia6_make_particle_set

  subroutine hadrons_pythia8_init &
       (hadrons, shower_settings, hadron_settings, model_hadrons)
    class(hadrons_pythia8_t), intent(out) :: hadrons
    type(shower_settings_t), intent(in) :: shower_settings
    type(hadron_settings_t), intent(in) :: hadron_settings
    type(model_t), intent(in), target :: model_hadrons
    logical :: options_not_set_by_shower
    hadrons%shower_settings = shower_settings
    hadrons%hadron_settings = hadron_settings
    options_not_set_by_shower = .not. (shower_settings%method == PS_PYTHIA8 &
         .and. (shower_settings%isr_active .or. shower_settings%fsr_active))
    if (options_not_set_by_shower) then
       !call pythia8_set_verbose (settings%verbose)
       !call pythia8_set_config (settings%pythia8_config)
       !call pythia8_set_config_file (settings%pythia8_config_file)
    end if
    call msg_message &
         ("Using Pythia8 interface for hadronization and decays")
  end subroutine hadrons_pythia8_init

  subroutine hadrons_pythia8_hadronize (hadrons, particle_set, valid)
    class(hadrons_pythia8_t), intent(inout) :: hadrons
    type(particle_set_t), intent(in) :: particle_set
    logical, intent(out) :: valid
    ! call pythia8_hadronize
    valid = .true.
  end subroutine hadrons_pythia8_hadronize

  subroutine hadrons_pythia8_make_particle_set &
         (hadrons, particle_set, model, valid)
    class(hadrons_pythia8_t), intent(in) :: hadrons
    type(particle_set_t), intent(inout) :: particle_set
    class(model_data_t), intent(in), target :: model
    logical, intent(out) :: valid
    ! call pythia8_combine_particle_set
    valid = .true.
  end subroutine hadrons_pythia8_make_particle_set

  subroutine evt_hadrons_init (evt, model_hadrons)
    class(evt_hadrons_t), intent(out) :: evt
    type(model_t), intent(in), target :: model_hadrons
    evt%model_hadrons => model_hadrons
    evt%is_first_event = .true.
  end subroutine evt_hadrons_init

  subroutine evt_hadrons_write_name (evt, unit)
    class(evt_hadrons_t), intent(in) :: evt
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "Event transform: hadronization"
  end subroutine evt_hadrons_write_name

  subroutine evt_hadrons_write (evt, unit, verbose, more_verbose, testflag)
    class(evt_hadrons_t), intent(in) :: evt
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, more_verbose, testflag
    integer :: u
    u = given_output_unit (unit)
    call write_separator (u, 2)
    call evt%write_name (u)
    call write_separator (u)
    call evt%base_write (u, testflag = testflag, show_set = .false.)
    if (evt%particle_set_exists)  &
         call evt%particle_set%write &
         (u, summary = .true., compressed = .true., testflag = testflag)
    call write_separator (u)
    call evt%hadrons%shower_settings%write (u)
    call write_separator (u)
    call evt%hadrons%hadron_settings%write (u)    
  end subroutine evt_hadrons_write

  subroutine evt_hadrons_first_event (evt)
    class(evt_hadrons_t), intent(inout) :: evt
    call msg_debug (D_TRANSFORMS, "evt_hadrons_first_event")
    associate (settings => evt%hadrons%shower_settings)
       settings%hadron_collision = .false.
       !!! !!! !!! Workaround for PGF90 16.1
       !!! if (all (evt%particle_set%prt(1:2)%flv%get_pdg_abs () <= 39)) then
       if (evt%particle_set%prt(1)%flv%get_pdg_abs () <= 39 .and. &
           evt%particle_set%prt(2)%flv%get_pdg_abs () <= 39) then
          settings%hadron_collision = .false.
       !!! else if (all (evt%particle_set%prt(1:2)%flv%get_pdg_abs () >= 100)) then
       else if (evt%particle_set%prt(1)%flv%get_pdg_abs () >= 100 .and. &
                evt%particle_set%prt(2)%flv%get_pdg_abs () >= 100) then
          settings%hadron_collision = .true.
       else
          call msg_fatal ("evt_hadrons didn't recognize beams setup")
       end if
       call msg_debug (D_TRANSFORMS, "hadron_collision", settings%hadron_collision)
       if (.not. (settings%isr_active .or. settings%fsr_active)) then
          call msg_fatal ("Hadronization without shower is not supported")
       end if
    end associate
    evt%is_first_event = .false.
  end subroutine evt_hadrons_first_event

  subroutine evt_hadrons_generate_weighted (evt, probability)
    class(evt_hadrons_t), intent(inout) :: evt
    real(default), intent(inout) :: probability
    logical :: valid
    if (signal_is_pending ())  return
    evt%particle_set = evt%previous%particle_set
    if (evt%is_first_event) then
       call evt%first_event ()
    end if
    call evt%hadrons%hadronize (evt%particle_set, valid)
    probability = 1
    evt%particle_set_exists = valid
  end subroutine evt_hadrons_generate_weighted

  subroutine evt_hadrons_make_particle_set &
       (evt, factorization_mode, keep_correlations, r)
    class(evt_hadrons_t), intent(inout) :: evt
    integer, intent(in) :: factorization_mode
    logical, intent(in) :: keep_correlations
    real(default), dimension(:), intent(in), optional :: r
    logical :: valid
    call evt%hadrons%make_particle_set (evt%particle_set, evt%model, valid)
    evt%particle_set_exists = evt%particle_set_exists .and. valid
  end subroutine evt_hadrons_make_particle_set

  subroutine evt_hadrons_make_rng (evt, process)
    class(evt_hadrons_t), intent(inout) :: evt
    type(process_t), intent(inout) :: process
    class(rng_t), allocatable :: rng
    call process%make_rng (rng)
    call evt%hadrons%import_rng (rng)
  end subroutine evt_hadrons_make_rng

  subroutine evt_hadrons_prepare_new_event (evt, i_mci, i_term)
    class(evt_hadrons_t), intent(inout) :: evt
    integer, intent(in) :: i_mci, i_term
    call evt%reset ()
  end subroutine evt_hadrons_prepare_new_event


end module hadrons
