! WHIZARD 2.2.7 Aug 11 2015
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

module hadrons

  use kinds, only: default, double
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_utils, only: write_separator
  use diagnostics
  use sm_qcd
  use particles
  use model_data
  use models
  use hep_common
  use event_transforms
  use shower_base
  use shower_pythia6

  implicit none
  private

  public :: HADRONS_PYTHIA6, HADRONS_PYTHIA8, HADRONS_UNDEFINED
  public :: hadrons_pythia8_t
  public :: evt_hadrons_t

  type, abstract :: hadrons_t
   type(shower_settings_t) :: settings
   type(model_t), pointer :: model => null()
   contains
     procedure (hadrons_init), deferred :: init
     procedure (hadrons_hadronize), deferred :: hadronize
     procedure (hadrons_make_particle_set), deferred :: make_particle_set
  end type hadrons_t

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
     procedure :: write => evt_hadrons_write
     procedure :: first_event => evt_hadrons_first_event
     procedure :: generate_weighted => evt_hadrons_generate_weighted
     procedure :: make_particle_set => evt_hadrons_make_particle_set
     procedure :: prepare_new_event => evt_hadrons_prepare_new_event
  end type evt_hadrons_t


  abstract interface
    subroutine hadrons_init (hadrons, settings, model_hadrons)
      import
      class(hadrons_t), intent(out) :: hadrons
      type(shower_settings_t), intent(in) :: settings
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


  integer, parameter :: HADRONS_PYTHIA6 = 1
  integer, parameter :: HADRONS_PYTHIA8 = 2
  integer, parameter :: HADRONS_UNDEFINED = 17

contains

  elemental function hadrons_method_of_string (string) result (i)
    integer :: i
    type(string_t), intent(in) :: string
    select case (char(string))
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
    case (HADRONS_PYTHIA6)
       string = "PYTHIA6"
    case (HADRONS_PYTHIA8)
       string = "PYTHIA8"
    case default
       string = "UNDEFINED"
    end select
  end function hadrons_method_to_string

  subroutine hadrons_pythia6_init (hadrons, settings, model_hadrons)
    class(hadrons_pythia6_t), intent(out) :: hadrons
    type(shower_settings_t), intent(in) :: settings
    type(model_t), intent(in), target :: model_hadrons
    logical :: pygive_not_set_by_shower
    hadrons%model => model_hadrons
    hadrons%settings = settings
    pygive_not_set_by_shower = .not. (settings%method == PS_PYTHIA6 &
         .and. (settings%isr_active .or. settings%fsr_active))
    if (pygive_not_set_by_shower) then
       call pythia6_set_verbose (settings%verbose)
       call pythia6_set_config (settings%pythia6_pygive)
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
            (particle_set, model, hadrons%model, hadrons%settings)
    end if
  end subroutine hadrons_pythia6_make_particle_set

  subroutine hadrons_pythia8_init (hadrons, settings, model_hadrons)
    class(hadrons_pythia8_t), intent(out) :: hadrons
    type(shower_settings_t), intent(in) :: settings
    type(model_t), intent(in), target :: model_hadrons
    logical :: options_not_set_by_shower
    hadrons%settings = settings
    options_not_set_by_shower = .not. (settings%method == PS_PYTHIA8 &
         .and. (settings%isr_active .or. settings%fsr_active))
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

  pure subroutine hadrons_pythia8_make_particle_set &
         (hadrons, particle_set, model, valid)
    class(hadrons_pythia8_t), intent(in) :: hadrons
    type(particle_set_t), intent(inout) :: particle_set
    class(model_data_t), intent(in), target :: model
    logical, intent(out) :: valid
    ! call pythia8_combine_particle_set
    valid = .true.
  end subroutine hadrons_pythia8_make_particle_set

  subroutine evt_hadrons_init (evt, settings, model_hadrons, method)
    class(evt_hadrons_t), intent(out) :: evt
    type(shower_settings_t), intent(in) :: settings
    type(model_t), intent(in), target :: model_hadrons
    type(string_t), intent(in) :: method
    evt%model_hadrons => model_hadrons
    !!! TODO: (bcn 2015-03-27) method should be part of hadronization settings
    select case (char (method))
    case ("PYTHIA6")
       allocate (hadrons_pythia6_t :: evt%hadrons)
    case ("PYTHIA8")
       allocate (hadrons_pythia8_t :: evt%hadrons)
    case default
       call msg_fatal ("Hadronization method " // char (method) // &
            " not implemented.")
    end select
    call evt%hadrons%init (settings, model_hadrons)
    evt%is_first_event = .true.
  end subroutine evt_hadrons_init

  subroutine evt_hadrons_write (evt, unit, verbose, more_verbose, testflag)
    class(evt_hadrons_t), intent(in) :: evt
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, more_verbose, testflag
    integer :: u
    u = given_output_unit (unit)
    call write_separator (u, 2)
    write (u, "(1x,A)")  "Event transform: hadronization"
    call write_separator (u)
    call evt%base_write (u, testflag = testflag, show_set = .false.)
    if (evt%particle_set_exists)  &
         call evt%particle_set%write &
         (u, summary = .true., compressed = .true., testflag = testflag)
    call write_separator (u)
    call evt%hadrons%settings%write (u)
  end subroutine evt_hadrons_write

  subroutine evt_hadrons_first_event (evt)
    class(evt_hadrons_t), intent(inout) :: evt
    call msg_debug (D_TRANSFORMS, "evt_hadrons_first_event")
    associate (settings => evt%hadrons%settings)
       settings%hadron_collision = .false.
       if (all (evt%particle_set%prt(1:2)%flv%get_pdg_abs () <= 18)) then
          settings%hadron_collision = .false.
       else if (all (evt%particle_set%prt(1:2)%flv%get_pdg_abs () >= 1000)) then
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

  subroutine evt_hadrons_prepare_new_event (evt, i_mci, i_term)
    class(evt_hadrons_t), intent(inout) :: evt
    integer, intent(in) :: i_mci, i_term
    call evt%reset ()
  end subroutine evt_hadrons_prepare_new_event


end module hadrons
