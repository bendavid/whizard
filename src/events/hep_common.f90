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

module hep_common
  
  use kinds
  use io_units
  use iso_varying_string, string_t => varying_string
  use diagnostics
  use physics_defs, only: HADRON_REMNANT
  use physics_defs, only: HADRON_REMNANT_SINGLET
  use physics_defs, only: HADRON_REMNANT_TRIPLET
  use physics_defs, only: HADRON_REMNANT_OCTET
  use lorentz
  use flavors
  use colors
  use polarizations
  use model_data
  use particles
  use subevents

  implicit none
  private

  public :: heprup_init
  public :: heprup_get_run_parameters
  public :: heprup_set_lhapdf_id
  public :: heprup_set_process_parameters
  public :: heprup_get_process_parameters
  public :: heprup_write_verbose
  public :: heprup_write_lhef
  public :: heprup_write_ascii
  public :: heprup_read_lhef
  public :: hepeup_init
  public :: hepeup_set_event_parameters
  public :: hepeup_get_event_parameters
  public :: hepeup_set_particle
  public :: hepeup_set_particle_lifetime
  public :: hepeup_set_particle_spin
  public :: hepeup_get_particle
  public :: hepevt_init
  public :: hepevt_set_event_parameters
  public :: hepevt_set_particle
  public :: hepevt_write_verbose
  public :: hepeup_write_verbose
  public :: hepeup_write_lhef
  public :: hepeup_write_lha
  public :: hepevt_write_hepevt
  public :: hepevt_write_ascii
  public :: hepevt_write_athena
  public :: hepevt_write_mokka  
  public :: hepeup_read_lhef
  public :: hepeup_from_particle_set
  public :: hepeup_to_particle_set
  public :: hepevt_from_particle_set

  interface hepeup_set_particle_spin
     module procedure hepeup_set_particle_spin_pol
  end interface

  integer, parameter, public :: MAXNUP = 500
  integer, parameter, public :: MAXPUP = 100

  integer, public :: NUP
  integer, public :: IDPRUP
  double precision, public :: XWGTUP
  double precision, public :: SCALUP
  double precision, public :: AQEDUP
  double precision, public :: AQCDUP
  integer, dimension(MAXNUP) :: IDUP
  integer, dimension(MAXNUP), public :: ISTUP
  integer, dimension(2,MAXNUP), public :: MOTHUP
  integer, dimension(2,MAXNUP), public :: ICOLUP
  double precision, dimension(5,MAXNUP), public :: PUP
  double precision, dimension(MAXNUP), public :: VTIMUP
  double precision, dimension(MAXNUP), public :: SPINUP
  integer, dimension(2), public :: IDBMUP
  double precision, dimension(2), public :: EBMUP
  integer, dimension(2), public :: PDFGUP
  integer, dimension(2), public :: PDFSUP
  integer, public :: IDWTUP
  integer, public :: NPRUP
  double precision, dimension(MAXPUP), public :: XSECUP
  double precision, dimension(MAXPUP), public :: XERRUP
  double precision, dimension(MAXPUP), public :: XMAXUP
  integer, dimension(MAXPUP), public :: LPRUP
  integer, parameter :: NMXHEP = 4000

  integer :: NEVHEP

  integer :: NHEP

  integer, dimension(NMXHEP) :: ISTHEP

  integer, dimension(NMXHEP) :: IDHEP

  integer, dimension(2, NMXHEP) :: JMOHEP

  integer, dimension(2, NMXHEP) :: JDAHEP

  double precision, dimension(5, NMXHEP) :: PHEP
  
  double precision, dimension(4, NMXHEP) :: VHEP
  
  integer, dimension(NMXHEP) :: hepevt_pol

  integer :: hepevt_n_out, hepevt_n_remnants

  double precision :: hepevt_weight, hepevt_function_value
  double precision :: hepevt_function_ratio
  

  common /HEPRUP/ &
       IDBMUP, EBMUP, PDFGUP, PDFSUP, IDWTUP, NPRUP, &
       XSECUP, XERRUP, XMAXUP, LPRUP
  save /HEPRUP/

  common /HEPEUP/ &
       NUP, IDPRUP, XWGTUP, SCALUP, AQEDUP, AQCDUP, &
       IDUP, ISTUP, MOTHUP, ICOLUP, PUP, VTIMUP, SPINUP
  save /HEPEUP/

  common /HEPEVT/ &
       NEVHEP, NHEP, ISTHEP, IDHEP, &
       JMOHEP, JDAHEP, PHEP, VHEP
  save /HEPEVT/
  

contains
  
  subroutine heprup_init &
       (beam_pdg, beam_energy, n_processes, unweighted, negative_weights)
    integer, dimension(2), intent(in) :: beam_pdg
    real(default), dimension(2), intent(in) :: beam_energy
    integer, intent(in) :: n_processes
    logical, intent(in) :: unweighted
    logical, intent(in) :: negative_weights
    IDBMUP = beam_pdg
    EBMUP = beam_energy
    PDFGUP = -1
    PDFSUP = -1
    if (unweighted) then
       IDWTUP = 3
    else
       IDWTUP = 4
    end if
    if (negative_weights)  IDWTUP = - IDWTUP
    NPRUP = n_processes
  end subroutine heprup_init

  subroutine heprup_get_run_parameters &
       (beam_pdg, beam_energy, n_processes, unweighted, negative_weights)
    integer, dimension(2), intent(out), optional :: beam_pdg
    real(default), dimension(2), intent(out), optional :: beam_energy
    integer, intent(out), optional :: n_processes
    logical, intent(out), optional :: unweighted
    logical, intent(out), optional :: negative_weights
    if (present (beam_pdg))  beam_pdg = IDBMUP
    if (present (beam_energy))  beam_energy = EBMUP
    if (present (n_processes))  n_processes = NPRUP
    if (present (unweighted)) then
       select case (abs (IDWTUP))
       case (3)
          unweighted = .true.
       case (4)
          unweighted = .false.
       case (1,2)  ! not supported by WHIZARD
          unweighted = .false.
       case default
          call msg_fatal ("HEPRUP: unsupported IDWTUP value")
       end select
    end if
    if (present (negative_weights)) then
       negative_weights = IDWTUP < 0
    end if
  end subroutine heprup_get_run_parameters

  subroutine heprup_set_lhapdf_id (i_beam, pdf_id)
    integer, intent(in) :: i_beam, pdf_id
    PDFGUP(i_beam) = 0
    PDFSUP(i_beam) = pdf_id
  end subroutine heprup_set_lhapdf_id

  subroutine heprup_set_process_parameters &
       (i, process_id, cross_section, error, max_weight)
    integer, intent(in) :: i, process_id
    real(default), intent(in), optional :: cross_section, error, max_weight
    real(default), parameter :: pb_per_fb = 1.e-3_default
    LPRUP(i) = process_id
    if (present (cross_section)) then
       XSECUP(i) = cross_section * pb_per_fb
    else
       XSECUP(i) = 0
    end if
    if (present (error)) then
       XERRUP(i) = error * pb_per_fb
    else
       XERRUP(i) = 0
    end if
    select case (IDWTUP)
    case (3);  XMAXUP(i) = 1
    case (4)
       if (present (max_weight)) then
          XMAXUP(i) = max_weight * pb_per_fb
       else
          XMAXUP(i) = 0
       end if
    end select
  end subroutine heprup_set_process_parameters

  subroutine heprup_get_process_parameters  &
       (i, process_id, cross_section, error, max_weight)
    integer, intent(in) :: i
    integer, intent(out), optional :: process_id
    real(default), intent(out), optional :: cross_section, error, max_weight
    real(default), parameter :: pb_per_fb = 1.e-3_default
    if (present (process_id))  process_id = LPRUP(i)
    if (present (cross_section)) then
       cross_section = XSECUP(i) / pb_per_fb
    end if
    if (present (error)) then
       error = XERRUP(i) / pb_per_fb
    end if
    if (present (max_weight)) then
       select case (IDWTUP)
       case (3)
          max_weight = 1
       case (4)
          max_weight = XMAXUP(i) / pb_per_fb
       case (1,2)   ! not supported by WHIZARD
          max_weight = 0
       case default
          call msg_fatal ("HEPRUP: unsupported IDWTUP value")
       end select
    end if
  end subroutine heprup_get_process_parameters

  subroutine heprup_write_verbose (unit)
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(A)")  "HEPRUP Common Block"
    write (u, "(3x,A6,' = ',I9,3x,1x,I9,3x,8x,A)")  "IDBMUP", IDBMUP, &
         "PDG code of beams"
    write (u, "(3x,A6,' = ',G12.5,1x,G12.5,8x,A)")  "EBMUP ", EBMUP, &
         "Energy of beams in GeV"
    write (u, "(3x,A6,' = ',I9,3x,1x,I9,3x,8x,A)")  "PDFGUP", PDFGUP, &
         "PDF author group [-1 = undefined]"
    write (u, "(3x,A6,' = ',I9,3x,1x,I9,3x,8x,A)")  "PDFSUP", PDFSUP, &
         "PDF set ID       [-1 = undefined]"
    write (u, "(3x,A6,' = ',I9,3x,1x,9x,3x,8x,A)")  "IDWTUP", IDWTUP, &
         "LHA code for event weight mode"
    write (u, "(3x,A6,' = ',I9,3x,1x,9x,3x,8x,A)")  "NPRUP ", NPRUP, &
         "Number of user subprocesses"
    do i = 1, NPRUP
       write (u, "(1x,A,I0)")  "Subprocess #", i
       write (u, "(3x,A6,' = ',ES12.5,1x,12x,8x,A)")  "XSECUP", XSECUP(i), &
            "Cross section in pb"
       write (u, "(3x,A6,' = ',ES12.5,1x,12x,8x,A)")  "XERRUP", XERRUP(i), &
            "Cross section error in pb"
       write (u, "(3x,A6,' = ',ES12.5,1x,12x,8x,A)")  "XMAXUP", XMAXUP(i), &
            "Maximum event weight (cf. IDWTUP)"
       write (u, "(3x,A6,' = ',I9,3x,1x,12x,8x,A)")  "LPRUP ", LPRUP(i), &
            "Subprocess ID"
    end do
  end subroutine heprup_write_verbose

  subroutine heprup_write_lhef (unit)
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(2(1x,I0),2(1x,ES17.10),6(1x,I0))") &
         IDBMUP, EBMUP, PDFGUP, PDFSUP, IDWTUP, NPRUP
    do i = 1, NPRUP
       write (u, "(3(1x,ES17.10),1x,I0)") &
            XSECUP(i), XERRUP(i), XMAXUP(i), LPRUP(i)
    end do
  end subroutine heprup_write_lhef
  
  subroutine heprup_write_ascii (unit)
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(2(1x,I0),2(1x,ES17.10),6(1x,I0))") &
         IDBMUP, EBMUP, PDFGUP, PDFSUP, IDWTUP, NPRUP
    do i = 1, NPRUP
       write (u, "(3(1x,ES17.10),1x,I0)") &
            XSECUP(i), XERRUP(i), XMAXUP(i), LPRUP(i)
    end do
  end subroutine heprup_write_ascii
  
  subroutine heprup_read_lhef (u)
    integer, intent(in) :: u
    integer :: i
    read (u, *) &
         IDBMUP, EBMUP, PDFGUP, PDFSUP, IDWTUP, NPRUP
    do i = 1, NPRUP
       read (u, *) &
            XSECUP(i), XERRUP(i), XMAXUP(i), LPRUP(i)
    end do
  end subroutine heprup_read_lhef
    
  subroutine hepeup_init (n_tot)
    integer, intent(in) :: n_tot
    NUP = n_tot
    IDPRUP = 0
    XWGTUP = 1
    SCALUP = -1
    AQEDUP = -1
    AQCDUP = -1
  end subroutine hepeup_init

  subroutine hepeup_set_event_parameters &
       (proc_id, weight, scale, alpha_qed, alpha_qcd)
    integer, intent(in), optional :: proc_id
    real(default), intent(in), optional :: weight, scale, alpha_qed, alpha_qcd
    if (present (proc_id))   IDPRUP = proc_id
    if (present (weight))    XWGTUP = weight
    if (present (scale))     SCALUP = scale
    if (present (alpha_qed)) AQEDUP = alpha_qed
    if (present (alpha_qcd)) AQCDUP = alpha_qcd
  end subroutine hepeup_set_event_parameters

  subroutine hepeup_get_event_parameters &
       (proc_id, weight, scale, alpha_qed, alpha_qcd)
    integer, intent(out), optional :: proc_id
    real(default), intent(out), optional :: weight, scale, alpha_qed, alpha_qcd
    if (present (proc_id))   proc_id   = IDPRUP
    if (present (weight))    weight    = XWGTUP
    if (present (scale))     scale     = SCALUP
    if (present (alpha_qed)) alpha_qed = AQEDUP
    if (present (alpha_qcd)) alpha_qcd = AQCDUP
  end subroutine hepeup_get_event_parameters

  subroutine hepeup_set_particle (i, pdg, status, parent, col, p, m2)
    integer, intent(in) :: i
    integer, intent(in) :: pdg, status
    integer, dimension(:), intent(in) :: parent
    type(vector4_t), intent(in) :: p
    integer, dimension(2), intent(in) :: col
    real(default), intent(in) :: m2
    if (i > MAXNUP) then
       call msg_error (arr=[ &
            var_str ("Too many particles in HEPEUP common block. " // &
                            "If this happened "), &
            var_str ("during event output, your events will be " // &
                            "invalid; please consider "), &
            var_str ("switching to a modern event format like HEPMC. " // &
                            "If you are not "), &
            var_str ("using an old, HEPEUP based format and " // &
                            "nevertheless get this error,"), &
            var_str ("please notify the WHIZARD developers,") ])
       return
    end if
    IDUP(i) = pdg
    select case (status)
    case (PRT_BEAM);         ISTUP(i) = -9
    case (PRT_INCOMING);     ISTUP(i) = -1
    case (PRT_BEAM_REMNANT); ISTUP(i) =  3
    case (PRT_OUTGOING);     ISTUP(i) =  1
    case (PRT_RESONANT);     ISTUP(i) =  2
    case (PRT_VIRTUAL);      ISTUP(i) =  3
    case default;            ISTUP(i) =  0
    end select
    select case (size (parent))
    case (0);      MOTHUP(:,i) = 0
    case (1);      MOTHUP(1,i) = parent(1); MOTHUP(2,i) = 0
    case default;  MOTHUP(:,i) = [ parent(1), parent(size (parent)) ]
    end select
    if (col(1) > 0) then
       ICOLUP(1,i) = 500 + col(1)
    else
       ICOLUP(1,i) = 0
    end if
    if (col(2) > 0) then
       ICOLUP(2,i) = 500 + col(2)
    else
       ICOLUP(2,i) = 0
    end if
    PUP(1:3,i) = vector3_get_components (space_part (p))
    PUP(4,i) = energy (p)
    PUP(5,i) = sign (sqrt (abs (m2)), m2)
    VTIMUP(i) = 0
    SPINUP(i) = 9
  end subroutine hepeup_set_particle

  subroutine hepeup_set_particle_lifetime (i, lifetime)
    integer, intent(in) :: i
    real(default), intent(in) :: lifetime
    VTIMUP(i) = lifetime
  end subroutine hepeup_set_particle_lifetime

  subroutine hepeup_set_particle_spin_pol (i, p, pol, p_mother)
    integer, intent(in) :: i
    type(vector4_t), intent(in) :: p
    type(polarization_t), intent(in) :: pol
    type(vector4_t), intent(in) :: p_mother
    type(vector3_t) :: s3, p3
    type(vector4_t) :: s4
    s3 = vector3_moving (polarization_get_axis (pol))
    p3 = space_part (p)
    s4 = rotation_to_2nd (3, p3) * vector4_moving (0._default, s3)
    SPINUP(i) = enclosed_angle_ct (s4, p_mother)
  end subroutine hepeup_set_particle_spin_pol
    
  subroutine hepeup_get_particle (i, pdg, status, parent, col, p, m2)
    integer, intent(in) :: i
    integer, intent(out), optional :: pdg, status
    integer, dimension(:), intent(out), optional :: parent
    type(vector4_t), intent(out), optional :: p
    integer, dimension(2), intent(out), optional :: col
    real(default), dimension(5,MAXNUP) :: pup_def
    real(default), intent(out), optional :: m2
    if (present (pdg))  pdg = IDUP(i)
    if (present (status)) then
       select case (ISTUP(i))
       case (-9);  status = PRT_BEAM
       case (-1);  status = PRT_INCOMING
       case (1);   status = PRT_OUTGOING
       case (2);   status = PRT_RESONANT
       case (3);
          select case (abs (IDUP(i)))
          case (HADRON_REMNANT, HADRON_REMNANT_SINGLET, &
               HADRON_REMNANT_TRIPLET, HADRON_REMNANT_OCTET)
             status = PRT_BEAM_REMNANT
          case default
             status = PRT_VIRTUAL
          end select
       case default
          status = PRT_UNDEFINED
       end select
    end if
    if (present (parent)) then
       select case (size (parent))
       case (0)
       case (1);    parent(1) = MOTHUP(1,i)
       case (2);    parent = MOTHUP(:,i)
       end select
    end if
    if (present (col)) then
       col = ICOLUP(:,i)
    end if
    if (present (p)) then
       pup_def = PUP
       p = vector4_moving (pup_def(4,i), vector3_moving (pup_def(1:3,i)))
    end if
    if (present (m2)) then
       m2 = sign (PUP(5,i) ** 2, PUP(5,i))
    end if
  end subroutine hepeup_get_particle

  subroutine hepevt_init (n_tot, n_out)
    integer, intent(in) :: n_tot, n_out
    NHEP              = n_tot
    NEVHEP            = 0
    hepevt_n_out      = n_out
    hepevt_n_remnants = 0
    hepevt_weight     = 1
    hepevt_function_value = 0
    hepevt_function_ratio = 1
  end subroutine hepevt_init
  
  subroutine hepevt_set_event_parameters &
       (weight, function_value, function_ratio, i_evt)
    integer, intent(in), optional :: i_evt
    real(default), intent(in), optional :: weight, function_value, &
       function_ratio
    if (present (i_evt)) NEVHEP = i_evt
    if (present (weight)) hepevt_weight = weight
    if (present (function_value)) hepevt_function_value = &
         function_value
    if (present (function_ratio)) hepevt_function_ratio = &
         function_ratio
  end subroutine hepevt_set_event_parameters

  subroutine hepevt_set_particle (i, pdg, status, parent, child, p, m2, hel)
    integer, intent(in) :: i
    integer, intent(in) :: pdg, status
    integer, dimension(:), intent(in) :: parent
    integer, dimension(:), intent(in) :: child
    type(vector4_t), intent(in) :: p
    real(default), intent(in) :: m2
    integer, intent(in) :: hel
    IDHEP(i) = pdg
    select case (status)
      case (PRT_BEAM);      ISTHEP(i) = 2
      case (PRT_INCOMING);  ISTHEP(i) = 2
      case (PRT_OUTGOING);  ISTHEP(i) = 1
      case (PRT_VIRTUAL);   ISTHEP(i) = 2
      case (PRT_RESONANT);  ISTHEP(i) = 2
      case default;         ISTHEP(i) = 0
    end select
    select case (size (parent))
    case (0);      JMOHEP(:,i) = 0
    case (1);      JMOHEP(1,i) = parent(1); JMOHEP(2,i) = 0
    case default;  JMOHEP(:,i) = [ parent(1), parent(size (parent)) ]
    end select
    select case (size (child))
    case (0);      JDAHEP(:,i) = 0
    case (1);      JDAHEP(:,i) = child(1)
    case default;  JDAHEP(:,i) = [ child(1), child(size (child)) ]
    end select
    PHEP(1:3,i) = vector3_get_components (space_part (p))
    PHEP(4,i) = energy (p)
    PHEP(5,i) = sign (sqrt (abs (m2)), m2)
    VHEP(1:4,i) = 0
    hepevt_pol(i) = hel
  end subroutine hepevt_set_particle

  subroutine hepevt_write_verbose (unit)
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(A)")  "HEPEVT Common Block"
    write (u, "(3x,A6,' = ',I9,3x,1x,20x,A)")  "NEVHEP", NEVHEP, &
         "Event number"
    write (u, "(3x,A6,' = ',I9,3x,1x,20x,A)")  "NHEP  ", NHEP, &
         "Number of particles in event"
    do i = 1, NHEP
       write (u, "(1x,A,I0)")  "Particle #", i
       write (u, "(3x,A6,' = ',I9,3x,1x,20x,A)", advance="no") &
            "ISTHEP", ISTHEP(i), "Status code: "
       select case (ISTHEP(i))
       case ( 0);  write (u, "(A)")  "null entry"
       case ( 1);  write (u, "(A)")  "outgoing"
       case ( 2);  write (u, "(A)")  "decayed"
       case ( 3);  write (u, "(A)")  "documentation"
       case (4:10);  write (u, "(A)")  "[unspecified]"
       case (11:200);  write (u, "(A)")  "[model-specific]"
       case (201:);  write (u, "(A)")  "[user-defined]"
       case default;  write (u, "(A)")  "[undefined]"
       end select
       write (u, "(3x,A6,' = ',I9,3x,1x,20x,A)")  "IDHEP ", IDHEP(i), &
            "PDG code of particle"
       write (u, "(3x,A6,' = ',I9,3x,1x,I9,3x,8x,A)")  "JMOHEP", JMOHEP(:,i), &
            "Index of first/second mother"
       write (u, "(3x,A6,' = ',I9,3x,1x,I9,3x,8x,A)")  "JDAHEP", JDAHEP(:,i), &
            "Index of first/last daughter"
       write (u, "(3x,A6,' = ',ES12.5,1x,ES12.5,8x,A)")  "PHEP12", &
            PHEP(1:2,i), "Transversal momentum (x/y) in GeV"
       write (u, "(3x,A6,' = ',ES12.5,1x,12x,8x,A)")  "PHEP3 ", PHEP(3,i), &
            "Longitudinal momentum (z) in GeV"
       write (u, "(3x,A6,' = ',ES12.5,1x,12x,8x,A)")  "PHEP4 ", PHEP(4,i), &
            "Energy in GeV"
       write (u, "(3x,A6,' = ',ES12.5,1x,12x,8x,A)")  "PHEP5 ", PHEP(5,i), &
            "Invariant mass in GeV"
       write (u, "(3x,A6,' = ',ES12.5,1x,ES12.5,8x,A)")  "VHEP12", VHEP(1:2,i), &
            "Transversal displacement (xy) in mm"
       write (u, "(3x,A6,' = ',ES12.5,1x,12x,8x,A)")  "VHEP3 ", VHEP(3,i), &
            "Longitudinal displacement (z) in mm"
       write (u, "(3x,A6,' = ',ES12.5,1x,12x,8x,A)")  "VHEP4 ", VHEP(4,i), &
            "Production time in mm"
    end do
  end subroutine hepevt_write_verbose

  subroutine hepeup_write_verbose (unit)
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(A)")  "HEPEUP Common Block"
    write (u, "(3x,A6,' = ',I9,3x,1x,20x,A)")  "NUP   ", NUP, &
         "Number of particles in event"
    write (u, "(3x,A6,' = ',I9,3x,1x,20x,A)")  "IDPRUP", IDPRUP, &
         "Subprocess ID"
    write (u, "(3x,A6,' = ',ES12.5,1x,20x,A)")  "XWGTUP", XWGTUP, &
         "Event weight"
    write (u, "(3x,A6,' = ',ES12.5,1x,20x,A)")  "SCALUP", SCALUP, &
         "Event energy scale in GeV"
    write (u, "(3x,A6,' = ',ES12.5,1x,20x,A)")  "AQEDUP", AQEDUP, &
         "QED coupling [-1 = undefined]"
    write (u, "(3x,A6,' = ',ES12.5,1x,20x,A)")  "AQCDUP", AQCDUP, &
         "QCD coupling [-1 = undefined]"
    do i = 1, NUP
       write (u, "(1x,A,I0)")  "Particle #", i
       write (u, "(3x,A6,' = ',I9,3x,1x,20x,A)")  "IDUP  ", IDUP(i), &
            "PDG code of particle"
       write (u, "(3x,A6,' = ',I9,3x,1x,20x,A)", advance="no") &
            "ISTUP ", ISTUP(i), "Status code: "
       select case (ISTUP(i))
       case (-1);  write (u, "(A)")  "incoming"
       case ( 1);  write (u, "(A)")  "outgoing"
       case (-2);  write (u, "(A)")  "spacelike"
       case ( 2);  write (u, "(A)")  "resonance"
       case ( 3);  write (u, "(A)")  "resonance (doc)"
       case (-9);  write (u, "(A)")  "beam"
       case default;  write (u, "(A)")  "[undefined]"
       end select
       write (u, "(3x,A6,' = ',I9,3x,1x,I9,3x,8x,A)")  "MOTHUP", MOTHUP(:,i), &
            "Index of first/last mother"
       write (u, "(3x,A6,' = ',I9,3x,1x,I9,3x,8x,A)")  "ICOLUP", ICOLUP(:,i), &
            "Color/anticolor flow index"
       write (u, "(3x,A6,' = ',ES12.5,1x,ES12.5,8x,A)")  "PUP1/2", PUP(1:2,i), &
            "Transversal momentum (x/y) in GeV"
       write (u, "(3x,A6,' = ',ES12.5,1x,12x,8x,A)")  "PUP3  ", PUP(3,i), &
            "Longitudinal momentum (z) in GeV"
       write (u, "(3x,A6,' = ',ES12.5,1x,12x,8x,A)")  "PUP4  ", PUP(4,i), &
            "Energy in GeV"
       write (u, "(3x,A6,' = ',ES12.5,1x,12x,8x,A)")  "PUP5  ", PUP(5,i), &
            "Invariant mass in GeV"
       write (u, "(3x,A6,' = ',ES12.5,1x,12x,8x,A)")  "VTIMUP", VTIMUP(i), &
            "Invariant lifetime in mm"
       write (u, "(3x,A6,' = ',ES12.5,1x,12x,8x,A)")  "SPINUP", SPINUP(i), &
            "cos(spin angle) [9 = undefined]"
    end do
  end subroutine hepeup_write_verbose

  subroutine hepeup_write_lhef (unit)
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(2(1x,I0),4(1x,ES17.10))") &
         NUP, IDPRUP, XWGTUP, SCALUP, AQEDUP, AQCDUP
    do i = 1, NUP
       write (u, "(6(1x,I0),7(1x,ES17.10))") &
            IDUP(i), ISTUP(i), MOTHUP(:,i), ICOLUP(:,i), &
            PUP(:,i), VTIMUP(i), SPINUP(i)
    end do
  end subroutine hepeup_write_lhef

  subroutine hepeup_write_lha (unit)
    integer, intent(in), optional :: unit
    integer :: u, i
    integer, dimension(MAXNUP) :: spin_up
    spin_up = SPINUP
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(2(1x,I5),1x,ES17.10,3(1x,ES13.6))") &
         NUP, IDPRUP, XWGTUP, SCALUP, AQEDUP, AQCDUP
    write (u, "(500(1x,I5))") IDUP(:NUP)
    write (u, "(500(1x,I5))") MOTHUP(1,:NUP)
    write (u, "(500(1x,I5))") MOTHUP(2,:NUP)
    write (u, "(500(1x,I5))") ICOLUP(1,:NUP)
    write (u, "(500(1x,I5))") ICOLUP(2,:NUP)
    write (u, "(500(1x,I5))") ISTUP(:NUP)
    write (u, "(500(1x,I5))") spin_up(:NUP)
    do i = 1, NUP
            write (u, "(1x,I5,4(1x,ES17.10))") i, PUP([ 4,1,2,3 ], i)
    end do

  end subroutine hepeup_write_lha  

  subroutine hepevt_write_hepevt (unit)
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(3(1x,I0),(1x,ES17.10))") &
         NHEP, hepevt_n_out, hepevt_n_remnants, hepevt_weight
    do i = 1, NHEP
       write (u, "(7(1x,I0))") &
            ISTHEP(i), IDHEP(i), JMOHEP(:,i), JDAHEP(:,i), hepevt_pol(i)
       write (u, "(5(1x,ES17.10))") PHEP(:,i)
       write (u, "(5(1x,ES17.10))") VHEP(:,i), 0.d0
    end do
  end subroutine hepevt_write_hepevt
  
  subroutine hepevt_write_ascii (unit, long)
    integer, intent(in), optional :: unit
    logical, intent(in) :: long   
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(3(1x,I0),(1x,ES17.10))") &
         NHEP, hepevt_n_out, hepevt_n_remnants, hepevt_weight
    do i = 1, NHEP
       if (ISTHEP(i) /= 1)  cycle
       write (u, "(2(1x,I0))") IDHEP(i), hepevt_pol(i)
       write (u, "(5(1x,ES17.10))") PHEP(:,i)
    end do
    if (long) then 
       write (u, "(2(1x,ES17.10))") &
            hepevt_function_value, hepevt_function_ratio
    end if
  end subroutine hepevt_write_ascii
  
  subroutine hepevt_write_athena (unit, i_evt)
    integer, intent(in), optional :: unit, i_evt
    integer :: u, i, num_event
    num_event = 0
    if (present (i_evt)) num_event = i_evt
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(2(1x,I0))") num_event, NHEP
    do i = 1, NHEP
       write (u, "(7(1x,I0))") &
            i, ISTHEP(i), IDHEP(i), JMOHEP(:,i), JDAHEP(:,i)
       write (u, "(5(1x,ES17.10))") PHEP(:,i)
       write (u, "(5(1x,ES17.10))") VHEP(1:4,i)
    end do
  end subroutine hepevt_write_athena
  
  subroutine hepevt_write_mokka (unit)
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(3(1x,I0),(1x,ES17.10))") &
         NHEP, hepevt_n_out, hepevt_n_remnants, hepevt_weight
    do i = 1, NHEP
       write (u, "(4(1x,I0),4(1x,ES17.10))") &
            ISTHEP(i), IDHEP(i), JDAHEP(1,i), JDAHEP(2,i), &
            PHEP(1:3,i), PHEP(5,i)
    end do
  end subroutine hepevt_write_mokka
  
  subroutine hepeup_read_lhef (u)
    integer, intent(in) :: u
    integer :: i
    read (u, *) &
         NUP, IDPRUP, XWGTUP, SCALUP, AQEDUP, AQCDUP
    do i = 1, NUP
       read (u, *) &
            IDUP(i), ISTUP(i), MOTHUP(:,i), ICOLUP(:,i), &
            PUP(:,i), VTIMUP(i), SPINUP(i)
    end do
  end subroutine hepeup_read_lhef

  subroutine hepeup_from_particle_set (particle_set, keep_beams)
    type(particle_set_t), intent(in), target :: particle_set
    logical, intent(in), optional :: keep_beams  
    type(particle_t), pointer :: prt
    type(particle_set_t), target :: pset_hepevt
    integer :: i, n_parents
    integer, dimension(1) :: i_mother
    call particle_set_to_hepevt_form (particle_set, pset_hepevt, keep_beams)
    call hepeup_init (pset_hepevt%n_tot)
    do i = 1, pset_hepevt%n_tot
       prt => pset_hepevt%prt(i)
       call hepeup_set_particle (i, &
            particle_get_pdg (prt), &
            particle_get_status (prt), &
            particle_get_parents (prt), &
            particle_get_color (prt), &
            particle_get_momentum (prt), &
            particle_get_p2 (prt))
       n_parents = particle_get_n_parents (prt)
       if (n_parents == 1) then
          i_mother = particle_get_parents (prt)
          select case (particle_get_polarization_status (prt))
          case (PRT_GENERIC_POLARIZATION)
             call hepeup_set_particle_spin (i, &
                  particle_get_momentum (prt), &
                  particle_get_polarization (prt), &
                  particle_get_momentum (pset_hepevt%prt(i_mother(1))))
          end select
       end if
    end do
    call particle_set_final (pset_hepevt)
  end subroutine hepeup_from_particle_set

  subroutine hepeup_to_particle_set &
       (particle_set, recover_beams, model, alt_model)
    type(particle_set_t), intent(inout), target :: particle_set
    logical, intent(in), optional :: recover_beams
    class(model_data_t), intent(in), target :: model, alt_model
    type(particle_t), dimension(:), allocatable :: prt
    integer, dimension(2) :: parent
    integer, dimension(:), allocatable :: child
    integer :: i, j, k, pdg, status
    type(flavor_t) :: flv
    type(color_t) :: col
    integer, dimension(2) :: c
    type(vector4_t) :: p
    real(default) :: p2
    logical :: reconstruct
    integer :: off
    if (present (recover_beams)) then
       reconstruct = recover_beams .and. .not. all (ISTUP(1:2) == PRT_BEAM)
    else
       reconstruct = .false.
    end if
    if (reconstruct) then
       off = 4
    else
       off = 0
    end if
    allocate (prt (NUP + off), child (NUP + off))
    do i = 1, NUP
       k = i + off
       call hepeup_get_particle (i, pdg, status, col = c, p = p, m2 = p2)
       call flavor_init (flv, pdg, model, alt_model)
       call particle_set_flavor (prt(k), flv)
       call particle_reset_status (prt(k), status)
       call color_init (col, c)
       call particle_set_color (prt(k), col)
       call particle_set_momentum (prt(k), p, p2)
       where (MOTHUP(:,i) /= 0)
          parent = MOTHUP(:,i) + off
       elsewhere
          parent = 0
       end where
       call particle_set_parents (prt(k), parent)
       child = [(j, j = 1 + off, NUP + off)]
       where (MOTHUP(1,:NUP) /= i .and. MOTHUP(2,:NUP) /= i)  child = 0
       call particle_set_children (prt(k), child)
    end do
    if (reconstruct) then
       do k = 1, 2
          call particle_reset_status (prt(k), PRT_BEAM)
          call particle_set_children (prt(k), [k+2,k+4])
       end do
       do k = 3, 4
          call particle_reset_status (prt(k), PRT_BEAM_REMNANT)
          call particle_set_parents (prt(k), [k-2])
       end do
       do k = 5, 6
          call particle_set_parents (prt(k), [k-4])
       end do
!     else
!        call handle_beams (prt)
    end if
    call particle_set_replace (particle_set, prt)
  end subroutine hepeup_to_particle_set
  
  subroutine hepevt_from_particle_set (particle_set, keep_beams)
    type(particle_set_t), intent(in), target :: particle_set
    type(particle_t), pointer :: prt
    type(particle_set_t), target :: pset_hepevt
    logical, intent(in), optional :: keep_beams
    integer :: i
    call particle_set_to_hepevt_form (particle_set, pset_hepevt, keep_beams)
    call hepevt_init (pset_hepevt%n_tot, pset_hepevt%n_out)    
    do i = 1, pset_hepevt%n_tot
       prt => pset_hepevt%prt(i)
       call hepevt_set_particle (i, &
            particle_get_pdg (prt), &
            particle_get_status (prt), &
            particle_get_parents (prt), &
            particle_get_children (prt), &
            particle_get_momentum (prt), &
            particle_get_p2 (prt), &
            particle_get_helicity (prt))
    end do
    call particle_set_final (pset_hepevt)
  end subroutine hepevt_from_particle_set


end module hep_common
