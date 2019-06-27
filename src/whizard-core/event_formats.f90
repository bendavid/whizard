! WHIZARD 2.0.3 Tue Aug 10 2010
! 
! (C) 1999-2010 by 
!     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
!     with contributions by Christian Speckner, Sebastian Schmidt, 
!     Daniel Wiesler, Felix Braam
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

module event_formats

  use kinds, only: default !NODEP!
  use kinds, only: i32, i64 !NODEP!
  use constants, only: pb_per_fb !NODEP!
  use file_utils !NODEP!
  use lorentz !NODEP!
  use prt_lists
  use flavors
  use colors
  use helicities
  use quantum_numbers
  use polarizations
  use stdhep_interface

  implicit none
  private

  public :: les_houches_events_write_header
  public :: les_houches_events_write_footer
  public :: lhef_write_matching_info
  public :: heprup_init
  public :: heprup_set_lhapdf_id
  public :: heprup_set_process_parameters
  public :: heprup_write_lhef
  public :: hepeup_init
  public :: hepeup_set_event_parameters
  public :: hepeup_set_particle
  public :: hepeup_set_particle_lifetime
  public :: hepeup_set_particle_spin
  public :: hepevt_init
  public :: hepevt_set_event_parameters
  public :: hepevt_set_particle
  public :: hepeup_write_lhef
  public :: hepeup_write_lha
  public :: hepevt_write_hepevt
  public :: hepevt_write_ascii
  public :: hepevt_write_athena

  integer, parameter :: MAXPUP = 100
  integer, parameter :: MAXNUP = 500

  integer, dimension(2) :: IDBMUP
  double precision, dimension(2) :: EBMUP
  integer, dimension(2) :: PDFGUP
  integer, dimension(2) :: PDFSUP
  integer :: IDWTUP
  integer :: NPRUP
  double precision, dimension(MAXPUP) :: XSECUP
  double precision, dimension(MAXPUP) :: XERRUP
  double precision, dimension(MAXPUP) :: XMAXUP
  integer, dimension(MAXPUP) :: LPRUP
  integer :: NUP
  integer :: IDPRUP
  double precision :: XWGTUP
  double precision :: SCALUP
  double precision :: AQEDUP
  double precision :: AQCDUP
  integer, dimension(MAXNUP) :: IDUP
  integer, dimension(MAXNUP) :: ISTUP
  integer, dimension(2,MAXNUP) :: MOTHUP
  integer, dimension(2,MAXNUP) :: ICOLUP
  double precision, dimension(5,MAXNUP) :: PUP
  double precision, dimension(MAXNUP) :: VTIMUP
  double precision, dimension(MAXNUP) :: SPINUP
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
  

  interface hepeup_set_particle_spin
     module procedure hepeup_set_particle_spin_pol
  end interface

contains

  subroutine les_houches_events_write_header (unit)
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) '<LesHouchesEvents version="1.0">'
    write (u, *) '<header>'
    write (u, *) '  <generator_name>WHIZARD</generator_name>'
    write (u, *) '  <generator_version>2.0.3</generator_version>'
    write (u, *) '</header>'
  end subroutine les_houches_events_write_header

  subroutine les_houches_events_write_footer (unit)
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) '</LesHouchesEvents>'
  end subroutine les_houches_events_write_footer

  subroutine lhef_write_matching_info (unit, ptmin, drmin, ktcut, ktmode, lhefout)
    integer, intent(in), optional :: unit, ktmode
    real(default), intent(in), optional :: ptmin, drmin, ktcut
    logical, intent(in), optional :: lhefout
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    if (present(ptmin).or.present(drmin).or.present(ktcut)) then
       write (u, *) '<!-- Matching information for PYTHIA'
       if (present(ptmin))    write (u, *) "# PTmin: ", ptmin
       if (present(drmin))    write (u, *) "# DRmin: ", drmin
       if (present(ktcut))    write (u, *) "# kTcut: ", ktcut
       if (present(ktmode))   write (u, *) "# kTmode: ", ktmode
       if (present(lhefout))  write (u, *) "# LHEFout: ", lhefout
       write (u, *) '-->'
    endif
  end subroutine lhef_write_matching_info

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

  subroutine heprup_set_lhapdf_id (i_beam, pdf_id)
    integer, intent(in) :: i_beam, pdf_id
    PDFGUP(i_beam) = 0
    PDFSUP(i_beam) = pdf_id
  end subroutine heprup_set_lhapdf_id

  subroutine heprup_set_process_parameters &
       (i, process_id, cross_section, error, max_weight)
    integer, intent(in) :: i, process_id
    real(default), intent(in), optional :: cross_section, error, max_weight
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

  subroutine heprup_write_lhef (unit)
    integer, intent(in), optional :: unit
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) "<init>"
    write (u, *) IDBMUP, EBMUP, PDFGUP, PDFSUP, IDWTUP, NPRUP
    do i = 1, NPRUP
       write (u, *) XSECUP(i), XERRUP(i), XMAXUP(i), LPRUP(i)
    end do
    write (u, *) "</init>"
  end subroutine heprup_write_lhef
  
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

  subroutine hepeup_set_particle (i, pdg, status, parent, col, p, m2)
    integer, intent(in) :: i
    integer, intent(in) :: pdg, status
    integer, dimension(:), intent(in) :: parent
    type(vector4_t), intent(in) :: p
    integer, dimension(2), intent(in) :: col
    real(default), intent(in) :: m2
    IDUP(i) = pdg
    select case (status)
    case (PRT_BEAM);      ISTUP(i) = -9
    case (PRT_INCOMING);  ISTUP(i) = -1
    case (PRT_OUTGOING);  ISTUP(i) =  1
    case (PRT_RESONANT);  ISTUP(i) =  2
    case default;         ISTUP(i) =  0
    end select
    select case (size (parent))
    case (1);    MOTHUP(:,i) = parent(1)
    case (2);    MOTHUP(:,i) = parent
    case default;  MOTHUP(:,i) = 0
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
    
  subroutine hepevt_init (n_tot, n_out)
    integer, intent(in) :: n_tot, n_out
    NHEP              = n_tot
    NEVHEP          = 0
    hepevt_n_out      = n_out
    hepevt_n_remnants = 0
    hepevt_weight     = 1
    hepevt_function_value = 0
    hepevt_function_ratio = 1
  end subroutine hepevt_init
  
  subroutine hepevt_set_event_parameters &
       (n_tot, n_out, n_remnants, weight, function_value, &
        function_ratio, i_evt)
    integer, intent(in), optional :: n_tot, n_out, n_remnants, i_evt
    real(default), intent(in), optional :: weight, function_value, &
       function_ratio
    integer(i32), parameter :: huge32 = huge (0_i32)
    if (present (n_tot)) NHEP = n_tot
    if (present (i_evt)) NEVHEP = i_evt
    if (present (n_out)) hepevt_n_out = n_out
    if (present (n_remnants)) hepevt_n_remnants = n_remnants
    if (present (weight)) hepevt_weight = weight
    if (present (function_value)) hepevt_function_value = &
         function_value
    if (present (function_ratio)) hepevt_function_ratio = &
         function_ratio
  end subroutine hepevt_set_event_parameters

  subroutine hepevt_set_particle (i, pdg, status, parent,  &
          children, p, m2, hel)
    integer, intent(in) :: i
    integer, intent(in) :: pdg, status
    integer, dimension(:), intent(in) :: parent
    integer, dimension(:), intent(in) :: children
    type(vector4_t), intent(in) :: p
    real(default), intent(in) :: m2
    integer, intent(in) :: hel
    IDHEP(i) = pdg
    select case (status)
      case (PRT_BEAM);      ISTHEP(i) = 2
      case (PRT_INCOMING);  ISTHEP(i) = 2
      case (PRT_OUTGOING);  ISTHEP(i) = 1
      case (PRT_RESONANT);  ISTHEP(i) = 2
      case default;         ISTHEP(i) = 0
    end select
    select case (size (parent))
    case (1);    JMOHEP(:,i) = parent(1)
    case (2);    JMOHEP(:,i) = parent
    case default;  JMOHEP(:,i) = 0
    end select
    select case (status)
      case (PRT_OUTGOING); JDAHEP(:,i) = 0
      case (PRT_BEAM,PRT_INCOMING,PRT_RESONANT)
         JDAHEP(1,i) = children(1);
         JDAHEP(2,i) = children(size (children));
      case default;    JDAHEP(:,i) = 0
    end select
    PHEP(1:3,i) = vector3_get_components (space_part (p))
    PHEP(4,i) = energy (p)
    PHEP(5,i) = sign (sqrt (abs (m2)), m2)
    VHEP(1:4,i) = 0
    hepevt_pol(i) = hel
  end subroutine hepevt_set_particle

  subroutine hepeup_write_lhef (unit)
    integer, intent(in), optional :: unit
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) "<event>"
    write (u, *) NUP, IDPRUP, XWGTUP, SCALUP, AQEDUP, AQCDUP
    do i = 1, NUP
       write (u, *) IDUP(i), ISTUP(i), MOTHUP(:,i), ICOLUP(:,i), &
            PUP(:,i), VTIMUP(i), SPINUP(i)
    end do
    write (u, *) "</event>"
  end subroutine hepeup_write_lhef

  subroutine hepeup_write_lha (unit)
    integer, intent(in), optional :: unit
    integer :: u, i
    integer, dimension(MAXNUP) :: spin_up
    spin_up = SPINUP
    u = output_unit (unit);  if (u < 0)  return
16 format(2(1x,I5),1x,F17.10,3(1x,F13.6))
17 format(500(1x,I5))
18 format(1x,I5,4(1x,F17.10))
    write (u, 16) NUP, IDPRUP, XWGTUP, SCALUP, AQEDUP, AQCDUP
    write (u, 17) IDUP(:NUP)
    write (u, 17) MOTHUP(1,:NUP)
    write (u, 17) MOTHUP(2,:NUP)
    write (u, 17) ICOLUP(1,:NUP)
    write (u, 17) ICOLUP(2,:NUP)
    write (u, 17) ISTUP(:NUP)
    write (u, 17) spin_up(:NUP)
    do i = 1, NUP
            write (u, 18) i, PUP((/ 4,1,2,3 /), i)
    end do

  end subroutine hepeup_write_lha
  
  subroutine hepevt_write_hepevt (unit)
    integer, intent(in), optional :: unit
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) NHEP, hepevt_n_out, hepevt_n_remnants, hepevt_weight
    do i = 1, NHEP
       write (u, *) ISTHEP(i), IDHEP(i), JMOHEP(:,i), JDAHEP(:,i), &
          hepevt_pol(i)
       write (u, *) PHEP(:,i)
       write (u, *) VHEP(:,i), 0.d0
    end do
  end subroutine hepevt_write_hepevt
  
  subroutine hepevt_write_ascii (unit, long)
    integer, intent(in), optional :: unit
    logical, intent(in) :: long   
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) NHEP, hepevt_n_out, hepevt_n_remnants, hepevt_weight
    do i = 1, NHEP
       write (u, *) IDHEP(i), hepevt_pol(i)
       write (u, *) PHEP(:,i)
    end do
    if (long) write (u, *) hepevt_function_value, hepevt_function_ratio
  end subroutine hepevt_write_ascii
  
  subroutine hepevt_write_athena (unit, i_evt)
    integer, intent(in), optional :: unit, i_evt
    integer :: u, i, num_event
    num_event = 0
    if (present (i_evt)) num_event = i_evt
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) num_event, NHEP
    do i = 1, NHEP
       write (u, *) i, ISTHEP(i), IDHEP(i), JMOHEP(:,i), JDAHEP(:,i)
       write (u, *) PHEP(:,i)
       write (u, *) VHEP(1:4,i)
    end do
  end subroutine hepevt_write_athena
  

end module event_formats
