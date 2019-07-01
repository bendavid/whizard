! WHIZARD 2.3.1 Aug 25 2016
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
!     Soyoung Shim <soyoung.shim@desy.de>
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

module phs_fks_uti

  use kinds, only: default
  use format_utils, only: write_separator, pac_fmt
  use format_defs, only: FMT_15, FMT_19
  use constants, only: tiny_07, zero, two
  use lorentz

  use ttv_formfactors, only: THR_POS_B, THR_POS_BBAR

  use cascades, only: resonance_contributors_t
  use phs_fks
  use nlo_data, only: phs_identifier_t

  implicit none
  private

  public :: phs_fks_generator_1
  public :: phs_fks_generator_2
  public :: phs_fks_generator_3
  public :: phs_fks_generator_4
  public :: phs_fks_generator_5
  public :: phs_fks_generator_6

contains

  subroutine phs_fks_generator_1 (u)
    integer, intent(in) :: u
    type(phs_fks_generator_t) :: generator
    type(vector4_t), dimension(:), allocatable :: p_born
    type(phs_point_t) :: p_real
    integer :: emitter, i_phs
    real(default) :: x1, x2, x3
    real(default), parameter :: sqrts = 250.0_default
    type(phs_identifier_t), dimension(2) :: phs_identifiers
    write (u, "(A)") "* Test output: phs_fks_generator_1"
    write (u, "(A)") "* Purpose: Create massless fsr phase space"
    write (u, "(A)")

    allocate (p_born (4))
    p_born(1)%p(0) = 125.0_default
    p_born(1)%p(1:2) = 0.0_default
    p_born(1)%p(3) = 125.0_default
    p_born(2)%p(0) = 125.0_default
    p_born(2)%p(1:2) = 0.0_default
    p_born(2)%p(3) = -125.0_default
    p_born(3)%p(0) = 125.0_default
    p_born(3)%p(1) = -39.5618_default
    p_born(3)%p(2) = -20.0791_default
    p_born(3)%p(3) = -114.6957_default
    p_born(4)%p(0) = 125.0_default
    p_born(4)%p(1:3) = -p_born(3)%p(1:3)

    allocate (generator%isr_kinematics)
    generator%n_in = 2

    call generator%set_sqrts_hat (sqrts)

    write (u, "(A)") "* Use four-particle phase space containing: "
    call vector4_write_set (p_born, u, testflag = .true., ultra = .true.)
    write (u, "(A)") "***********************"
    write (u, "(A)")

    x1 = 0.5_default; x2 = 0.25_default; x3 = 0.75_default
    write (u, "(A)" ) "* Use random numbers: "
    write (u, "(A,F3.2,1X,A,F3.2,1X,A,F3.2)") &
       "x1: ", x1, "x2: ", x2, "x3: ", x3

    allocate (generator%real_kinematics)
    call generator%real_kinematics%init (4, 2, 2, 1)

    allocate (generator%emitters (2))
    generator%emitters(1) = 3; generator%emitters(2) = 4
    allocate (generator%m2 (4))
    generator%m2 = zero
    allocate (generator%is_massive (4))
    generator%is_massive(1:2) = .false.
    generator%is_massive(3:4) = .true.
    phs_identifiers(1)%emitter = 3
    phs_identifiers(2)%emitter = 4
    call generator%compute_xi_ref_momenta (p_born)
    call generator%generate_radiation_variables ([x1,x2,x3], p_born, phs_identifiers)
    do i_phs = 1, 2
       emitter = phs_identifiers(i_phs)%emitter
       call generator%compute_xi_max (emitter, i_phs, p_born)
    end do
    write (u, "(A)")  &
         "* With these, the following radiation variables have been produced:"
    associate (rad_var => generator%real_kinematics)
      write (u, "(A,F3.2)") "xi_tilde: ", rad_var%xi_tilde
      write (u, "(A,F3.2)") "y: " , rad_var%y(1)
      write (u, "(A,F3.2)") "phi: ", rad_var%phi
    end associate
    call write_separator (u)
    write (u, "(A)") "Produce real momenta: "
    i_phs = 1; emitter = phs_identifiers(i_phs)%emitter
    write (u, "(A,I1)") "emitter: ", emitter
    p_real = 5
    call generator%generate_fsr (emitter, i_phs, p_born, p_real)
    call vector4_write_set (p_real%p, u, testflag = .true., ultra = .true.)
    call write_separator (u)
    write (u, "(A)")
    write (u, "(A)") "* Test output end: phs_fks_generator_1"

  end subroutine phs_fks_generator_1

  subroutine phs_fks_generator_2 (u)
    integer, intent(in) :: u
    type(phs_fks_generator_t) :: generator
    type(vector4_t), dimension(:), allocatable :: p_born
    type(phs_point_t) :: p_real
    integer :: emitter, i_phs
    real(default) :: x1, x2, x3
    real(default), parameter :: sqrts_hadronic = 250.0_default
    type(phs_identifier_t), dimension(2) :: phs_identifiers
    write (u, "(A)") "* Test output: phs_fks_generator_2"
    write (u, "(A)") "* Purpose: Create massless ISR phase space"
    write (u, "(A)")


    allocate (p_born (4))
    p_born(1)%p(0) = 114.661_default
    p_born(1)%p(1:2) = 0.0_default
    p_born(1)%p(3) = 114.661_default
    p_born(2)%p(0) = 121.784_default
    p_born(2)%p(1:2) = 0.0_default
    p_born(2)%p(3) = -121.784_default
    p_born(3)%p(0) = 115.148_default
    p_born(3)%p(1) = -46.250_default
    p_born(3)%p(2) = -37.711_default
    p_born(3)%p(3) = 98.478_default
    p_born(4)%p(0) = 121.296_default
    p_born(4)%p(1:2) = -p_born(3)%p(1:2)
    p_born(4)%p(3) = -105.601_default

    phs_identifiers(1)%emitter = 1
    phs_identifiers(2)%emitter = 2

    allocate (generator%emitters (2))
    allocate (generator%isr_kinematics)
    generator%n_in = 2
    generator%emitters(1) = 1; generator%emitters(2) = 2
    generator%sqrts = sqrts_hadronic
    generator%isr_kinematics%beam_energy = sqrts_hadronic / two
    call generator%set_sqrts_hat (sqrts_hadronic)
    call generator%set_isr_kinematics (p_born)

    write (u, "(A)") "* Use four-particle phase space containing: "
    call vector4_write_set (p_born, u, testflag = .true., ultra = .true.)
    write (u, "(A)") "***********************"
    write (u, "(A)")

    x1=0.5_default; x2=0.25_default; x3=0.65_default
    write (u, "(A)" ) "* Use random numbers: "
    write (u, "(A,F3.2,1X,A,F3.2,1X,A,F3.2)") &
       "x1: ", x1, "x2: ", x2, "x3: ", x3

    allocate (generator%real_kinematics)
    call generator%real_kinematics%init (4, 2, 2, 1)
    call generator%real_kinematics%p_born_lab%set_momenta (1, p_born)

    allocate (generator%m2 (2))
    generator%m2(1) = 0._default; generator%m2(2) = 0._default
    allocate (generator%is_massive (4))
    generator%is_massive = .false.
    call generator%generate_radiation_variables ([x1,x2,x3], p_born, phs_identifiers)
    call generator%compute_xi_ref_momenta (p_born)
    do i_phs = 1, 2
       emitter = phs_identifiers(i_phs)%emitter
       call generator%compute_xi_max (emitter, i_phs, p_born)
    end do
    write (u, "(A)")  &
         "* With these, the following radiation variables have been produced:"
    associate (rad_var => generator%real_kinematics)
      write (u, "(A,F3.2)") "xi_tilde: ", rad_var%xi_tilde
      write (u, "(A,F3.2)") "y: " , rad_var%y(1)
      write (u, "(A,F3.2)") "phi: ", rad_var%phi
    end associate
    write (u, "(A)") "Initial-state momentum fractions: "
    associate (xb => generator%isr_kinematics%x)
       write (u, "(A,F3.2)") "x_born_plus: ", xb(1)
       write (u, "(A,F3.2)") "x_born_minus: ", xb(2)
    end associate
    call write_separator (u)
    write (u, "(A)") "Produce real momenta: "
    i_phs = 1; emitter = phs_identifiers(i_phs)%emitter
    write (u, "(A,I1)") "emitter: ", emitter
    p_real = 5
    call generator%generate_isr (i_phs, p_born, p_real)
    call vector4_write_set (p_real%p, u, testflag = .true., ultra = .true.)
    call write_separator (u)
    write (u, "(A)")
    write (u, "(A)") "* Test output end: phs_fks_generator_2"

  end subroutine phs_fks_generator_2

  subroutine phs_fks_generator_3 (u)
    integer, intent(in) :: u
    type(phs_fks_generator_t) :: generator
    type(vector4_t), dimension(:), allocatable :: p_born
    type(phs_point_t) :: p_real
    real(default) :: x1, x2, x3
    real(default) :: mB, mW, mT
    integer :: i, emitter, i_phs
    type(phs_identifier_t), dimension(2) :: phs_identifiers

    write (u, "(A)") "* Test output: phs_fks_generator_3"
    write (u, "(A)") "* Puropse: Create real phase space for particle decays"
    write (u, "(A)")

    allocate (p_born(3))
    p_born(1)%p(0) = 172._default
    p_born(1)%p(1) = 0._default
    p_born(1)%p(2) = 0._default
    p_born(1)%p(3) = 0._default
    p_born(2)%p(0) = 104.72866679_default
    p_born(2)%p(1) = 45.028053213_default
    p_born(2)%p(2) = 29.450337581_default
    p_born(2)%p(3) = -5.910229156_default
    p_born(3)%p(0) = 67.271333209_default
    p_born(3)%p(1:3) = -p_born(2)%p(1:3)

    generator%n_in = 1

    mB = 4.2_default
    mW = 80.376_default
    mT = 172._default

    generator%sqrts = mT

    write (u, "(A)") "* Use three-particle phase space containing: "
    call vector4_write_set (p_born, u, testflag = .true., ultra = .true.)
    write (u, "(A)") "**********************"
    write (u, "(A)")

    x1 = 0.5_default; x2 = 0.25_default; x3 = 0.6_default
    write (u, "(A)") "* Use random numbers: "
    write (u, "(A,F3.2,1X,A,F3.2,A,1X,F3.2)") &
       "x1: ", x1, "x2: ", x2, "x3: ", x3

    allocate (generator%real_kinematics)
    call generator%real_kinematics%init (3, 2, 2, 1)
    call generator%real_kinematics%p_born_lab%set_momenta (1, p_born)

    allocate (generator%emitters(2))
    generator%emitters(1) = 1
    generator%emitters(2) = 3
    allocate (generator%m2 (3), generator%is_massive(3))
    generator%m2(1) = mT**2
    generator%m2(2) = mW**2
    generator%m2(3) = mB**2
    generator%is_massive = .true.
    phs_identifiers(1)%emitter = 1
    phs_identifiers(2)%emitter = 3

    call generator%generate_radiation_variables ([x1,x2,x3], p_born, phs_identifiers)
    call generator%compute_xi_ref_momenta (p_born)
    do i_phs = 1, 2
       emitter = phs_identifiers(i_phs)%emitter 
       call generator%compute_xi_max (emitter, i_phs, p_born)
    end do

    write (u, "(A)") &
       "* With these, the following radiation variables have been produced: "
    associate (rad_var => generator%real_kinematics)
      write (u, "(A,F4.2)") "xi_tilde: ", rad_var%xi_tilde
      do i = 1, 2
         write (u, "(A,I1,A,F5.2)") "i: ", i, "y: " , rad_var%y(i)
      end do
      write (u, "(A,F4.2)") "phi: ", rad_var%phi
    end associate

    call write_separator (u)
    write (u, "(A)") "Produce real momenta via initial-state emission: "
    i_phs = 1; emitter = phs_identifiers(i_phs)%emitter
    write (u, "(A,I1)") "emitter: ", emitter
    p_real = 4
    call generator%generate_isr_decay (i_phs, p_born, p_real)
    call pacify (p_real%p, 1E-6_default)
    call vector4_write_set (p_real%p, u, testflag = .true., ultra = .true.)
    call write_separator(u)
    write (u, "(A)") "Produce real momenta via final-state emisson: "
    i_phs = 2; emitter = phs_identifiers(i_phs)%emitter
    write (u, "(A,I1)") "emitter: ", emitter
    call generator%generate_fsr (emitter, i_phs, p_born, p_real)
    call pacify (p_real%p, 1E-6_default)
    call vector4_write_set (p_real%p, u, testflag = .true., ultra = .true.)
    write (u, "(A)")
    write (u, "(A)") "* Test output end: phs_fks_generator_3"

  end subroutine phs_fks_generator_3

  subroutine phs_fks_generator_4 (u)
    integer, intent(in) :: u
    type(phs_fks_generator_t) :: generator
    type(vector4_t), dimension(:), allocatable :: p_born
    type(phs_point_t) :: p_real
    integer, dimension(:), allocatable :: emitters
    integer, dimension(:,:), allocatable :: resonance_lists
    type(resonance_contributors_t), dimension(2) :: alr_contributors
    real(default) :: x1, x2, x3
    real(default), parameter :: sqrts = 250.0_default
    integer, parameter :: nlegborn = 6
    integer :: i_phs, i_con, emitter
    real(default) :: m_inv_born, m_inv_real
    character(len=7) :: fmt
    type(phs_identifier_t), dimension(2) :: phs_identifiers

    call pac_fmt (fmt, FMT_19, FMT_15, .true.)

    write (u, "(A)") "* Test output: phs_fks_generator_4"
    write (u, "(A)") "* Purpose: Create FSR phase space with fixed resonances"
    write (u, "(A)")

    allocate (p_born (nlegborn))
    p_born(1)%p(0) = 250._default
    p_born(1)%p(1) = 0._default
    p_born(1)%p(2) = 0._default
    p_born(1)%p(3) = 250._default
    p_born(2)%p(0) = 250._default
    p_born(2)%p(1) = 0._default
    p_born(2)%p(2) = 0._default
    p_born(2)%p(3) = -250._default
    p_born(3)%p(0) = 145.91184486_default
    p_born(3)%p(1) = 50.39727589_default
    p_born(3)%p(2) = 86.74156041_default
    p_born(3)%p(3) = -69.03608748_default
    p_born(4)%p(0) = 208.1064784_default
    p_born(4)%p(1) = -44.07610020_default
    p_born(4)%p(2) = -186.34264578_default
    p_born(4)%p(3) = 13.48038407_default
    p_born(5)%p(0) = 26.25614471_default
    p_born(5)%p(1) = -25.12258068_default
    p_born(5)%p(2) = -1.09540228_default
    p_born(5)%p(3) = -6.27703505_default
    p_born(6)%p(0) = 119.72553196_default
    p_born(6)%p(1) = 18.80140499_default
    p_born(6)%p(2) = 100.69648766_default
    p_born(6)%p(3) = 61.83273846_default

    allocate (generator%isr_kinematics)
    generator%n_in = 2

    call generator%set_sqrts_hat (sqrts)

    write (u, "(A)") "* Test process: e+ e- -> W+ W- b b~"
    write (u, "(A)") "* Resonance pairs: (3,5) and (4,6)"
    write (u, "(A)") "* Use four-particle phase space containing: "
    call vector4_write_set (p_born, u, testflag = .true., ultra = .true.)
    write (u, "(A)") "******************************"
    write (u, "(A)")

    x1 = 0.5_default; x2 = 0.25_default; x3 = 0.75_default
    write (u, "(A)") "* Use random numbers: "
    write (u, "(A,F3.2,1X,A,F3.2,1X,A,F3.2)") &
       "x1: ", x1, "x2: ", x2, "x3: ", x3

    allocate (generator%real_kinematics)
    call generator%real_kinematics%init (nlegborn, 2, 2, 2)

    allocate (generator%emitters (2))
    generator%emitters(1) = 5; generator%emitters(2) = 6
    allocate (generator%m2 (nlegborn))
    generator%m2 = p_born**2
    allocate (generator%is_massive (nlegborn))
    generator%is_massive (1:2) = .false.
    generator%is_massive (3:6) = .true.

    phs_identifiers(1)%emitter = 5
    phs_identifiers(2)%emitter = 6
    do i_phs = 1, 2
       allocate (phs_identifiers(i_phs)%contributors (2))
    end do
    allocate (resonance_lists (2, 2))
    resonance_lists (1,:) = [3,5]
    resonance_lists (2,:) = [4,6]
    !!! Here is obviously some redundance. Surely we can improve on this.
    do i_phs = 1, 2
       phs_identifiers(i_phs)%contributors = resonance_lists(i_phs,:)
    end do
    do i_con = 1, 2
       allocate (alr_contributors(i_con)%c (size (resonance_lists(i_con,:))))
       alr_contributors(i_con)%c = resonance_lists(i_con,:)
    end do
    call generator%generate_radiation_variables &
       ([x1, x2, x3], p_born, phs_identifiers)

    p_real = nlegborn + 1
    call generator%compute_xi_ref_momenta (p_born, alr_contributors)
    !!! Keep the distinction between i_phs and i_con because in general,
    !!! they are not the same.
    do i_phs = 1, 2
       i_con = i_phs
       emitter = phs_identifiers(i_phs)%emitter
       write (u, "(A,I1,1X,A,I1,A,I1,A)") &
          "* Generate FSR phase space for emitter ", emitter, &
          "and resonance pair (",  resonance_lists (i_con, 1), ",", &
          resonance_lists (i_con, 2), ")"
       call generator%compute_xi_max (emitter, i_phs, p_born, i_con)
       call generator%generate_fsr (emitter, i_phs, i_con, p_born, p_real)
       call vector4_write_set (p_real%p, u, testflag = .true., ultra = .true.)
       call write_separator(u)
       write (u, "(A)") "* Check if resonance masses are conserved: "
       m_inv_born = compute_resonance_mass (p_born, resonance_lists (i_con,:))
       m_inv_real = compute_resonance_mass (p_real%p, resonance_lists (i_con,:), 7)
       write (u, "(A,1X, " // fmt // ")") "m_inv_born = ", m_inv_born
       write (u, "(A,1X, " // fmt // ")") "m_inv_real = ", m_inv_real
       if (abs (m_inv_born - m_inv_real) < tiny_07) then
          write (u, "(A)") " Success! "
       else
          write (u, "(A)") " Failure! "
       end if
       call write_separator(u)
       call write_separator(u)
    end do
    call p_real%final ()
    write (u, "(A)")
    write (u, "(A)") "* Test output end: phs_fks_generator_4"
  end subroutine phs_fks_generator_4

  subroutine phs_fks_generator_5 (u)
    integer, intent(in) :: u
    type(phs_fks_generator_t) :: generator
    type(vector4_t), dimension(:), allocatable :: p_born
    type(phs_point_t) :: p_real
    integer, dimension(:), allocatable :: emitters
    real(default) :: x1, x2, x3
    integer, parameter :: nlegborn = 6
    integer, parameter :: n_phs = 4, n_alr = 4, n_contr = 1
    integer :: i_phs, emitter
    type(phs_identifier_t), dimension(n_phs) :: phs_identifiers
    write (u, "(A)") "* Test output: phs_fks_generator_5"
    write (u, "(A)") "* Purpose: Create FSR phase space with fixed resonances"
    write (u, "(A)")

    allocate (p_born (nlegborn))
    p_born(1)%p(0) = 250._default
    p_born(1)%p(1) = 0._default
    p_born(1)%p(2) = 0._default
    p_born(1)%p(3) = 250._default
    p_born(2)%p(0) = 250._default
    p_born(2)%p(1) = 0._default
    p_born(2)%p(2) = 0._default
    p_born(2)%p(3) = -250._default
    p_born(3)%p(0) = 145.91184486_default
    p_born(3)%p(1) = 50.39727589_default
    p_born(3)%p(2) = 86.74156041_default
    p_born(3)%p(3) = -69.03608748_default
    p_born(4)%p(0) = 208.1064784_default
    p_born(4)%p(1) = -44.07610020_default
    p_born(4)%p(2) = -186.34264578_default
    p_born(4)%p(3) = 13.48038407_default
    p_born(5)%p(0) = 26.25614471_default
    p_born(5)%p(1) = -25.12258068_default
    p_born(5)%p(2) = -1.09540228_default
    p_born(5)%p(3) = -6.27703505_default
    p_born(6)%p(0) = 119.72553196_default
    p_born(6)%p(1) = 18.80140499_default
    p_born(6)%p(2) = 100.69648766_default
    p_born(6)%p(3) = 61.83273846_default

    allocate (generator%isr_kinematics)
    generator%n_in = 2
    p_real = nlegborn + 1

    call generator%set_sqrts_hat (250._default)

    write (u, "(A)") "* Test process: e+ e- -> W+ W- b b~"
    write (u, "(A)") "* Use four-particle phase space containing: "
    call vector4_write_set (p_born, u, testflag = .true., ultra = .true.)
    write (u, "(A)") "******************************"
    write (u, "(A)")

    x1 = 0.5_default; x2 = 0.25_default; x3 = 0.75_default
    write (u, "(A)") "* Use random numbers: "
    write (u, "(A,F3.2,1X,A,F3.2,1X,A,F3.2)") &
       "x1: ", x1, "x2: ", x2, "x3: ", x3

    allocate (generator%real_kinematics)
    call generator%real_kinematics%init (nlegborn, n_phs, n_alr, n_contr)
    allocate (generator%emitters (n_phs))
    generator%emitters([1,3]) = THR_POS_B; generator%emitters([2,4]) = THR_POS_BBAR
    allocate (generator%m2 (nlegborn))
    generator%m2 = p_born**2
    allocate (generator%is_massive (nlegborn))
    generator%is_massive (1:2) = .false.
    generator%is_massive (3:6) = .true.
    phs_identifiers([1,3])%emitter = THR_POS_B
    phs_identifiers([2,4])%emitter = THR_POS_BBAR
    call generator%generate_radiation_variables &
       ([x1, x2, x3], p_born, phs_identifiers)
    call generator%generate_isr_factorized &
       (3, phs_identifiers(3)%emitter, p_born, p_real)
    call vector4_write_set (p_real%p, u, testflag = .true., ultra = .true.)
    call write_separator (u)
    call generator%generate_isr_factorized &
       (4, phs_identifiers(4)%emitter, p_born, p_real)
    call vector4_write_set (p_real%p, u, testflag = .true., ultra = .true.)
    call write_separator (u)
    
  end subroutine phs_fks_generator_5

  subroutine phs_fks_generator_6 (u)
    integer, intent(in) :: u
    type(phs_fks_generator_t) :: generator
    type(vector4_t), dimension(:), allocatable :: p_born
    type(phs_point_t) :: p_real
    real(default) :: x1, x2, x3
    real(default) :: mB, mW, mT
    integer :: i, emitter, i_phs
    type(phs_identifier_t), dimension(2) :: phs_identifiers

    write (u, "(A)") "* Test output: phs_fks_generator_6"
    write (u, "(A)") "* Puropse: Create real phase space for particle decays"
    write (u, "(A)")

    allocate (p_born(4))
    p_born(1)%p(0) = 173.1_default
    p_born(1)%p(1) = zero
    p_born(1)%p(2) = zero
    p_born(1)%p(3) = zero 
    p_born(2)%p(0) = 68.17074462929_default
    p_born(2)%p(1) = -37.32578717617_default
    p_born(2)%p(2) = 30.99675959336_default
    p_born(2)%p(3) = -47.70321718398_default
    p_born(3)%p(0) = 65.26639312326_default
    p_born(3)%p(1) = -1.362927648502_default
    p_born(3)%p(2) = -33.25327150840_default
    p_born(3)%p(3) = 56.14324922494_default
    p_born(4)%p(0) = 39.66286224745_default
    p_born(4)%p(1) = 38.68871482467_default
    p_born(4)%p(2) = 2.256511915049_default
    p_born(4)%p(3) = -8.440032040958_default

    generator%n_in = 1

    mB = 4.2_default
    mW = 80.376_default
    mT = 173.1_default

    generator%sqrts = mT

    write (u, "(A)") "* Use four-particle phase space containing: "
    call vector4_write_set (p_born, u, testflag = .true., ultra = .true.)
    write (u, "(A)") "**********************"
    write (u, "(A)")

    x1=0.5_default; x2=0.25_default; x3=0.6_default
    write (u, "(A)") "* Use random numbers: "
    write (u, "(A,F3.2,1X,A,F3.2,A,1X,F3.2)") &
       "x1: ", x1, "x2: ", x2, "x3: ", x3

    allocate (generator%real_kinematics)
    call generator%real_kinematics%init (3, 2, 2, 1)
    call generator%real_kinematics%p_born_lab%set_momenta (1, p_born)

    allocate (generator%emitters(2))
    generator%emitters(1) = 1
    generator%emitters(2) = 2
    allocate (generator%m2 (4), generator%is_massive(4))
    generator%m2(1) = mT**2
    generator%m2(1) = mB**2
    generator%m2(3) = zero
    generator%m2(4) = zero
    generator%is_massive(1:2) = .true.
    generator%is_massive(3:4) = .false.
    phs_identifiers(1)%emitter = 1
    phs_identifiers(2)%emitter = 2

    call generator%generate_radiation_variables ([x1,x2,x3], p_born, phs_identifiers)
    call generator%compute_xi_ref_momenta (p_born)
    do i_phs = 1, 2
       emitter = phs_identifiers(i_phs)%emitter 
       call generator%compute_xi_max (emitter, i_phs, p_born)
    end do

    write (u, "(A)") &
       "* With these, the following radiation variables have been produced: "
    associate (rad_var => generator%real_kinematics)
      write (u, "(A,F4.2)") "xi_tilde: ", rad_var%xi_tilde
      do i = 1, 2
         write (u, "(A,I1,A,F5.2)") "i: ", i, "y: " , rad_var%y(i)
      end do
      write (u, "(A,F4.2)") "phi: ", rad_var%phi
    end associate

    call write_separator (u)
    write (u, "(A)") "Produce real momenta via initial-state emission: "
    i_phs = 1; emitter = phs_identifiers(i_phs)%emitter
    write (u, "(A,I1)") "emitter: ", emitter
    p_real = 5
    call generator%generate_isr_decay (i_phs, p_born, p_real)
    call pacify (p_real%p, 1E-6_default)
    call vector4_write_set (p_real%p, u, testflag = .true., ultra = .true.)
    call write_separator(u)
    write (u, "(A)") "Produce real momenta via final-state emisson: "
    i_phs = 2; emitter = phs_identifiers(i_phs)%emitter
    write (u, "(A,I1)") "emitter: ", emitter
    call generator%generate_fsr (emitter, i_phs, p_born, p_real)
    call pacify (p_real%p, 1E-6_default)
    call vector4_write_set (p_real%p, u, testflag = .true., ultra = .true.)
    write (u, "(A)")
    write (u, "(A)") "* Test output end: phs_fks_generator_6"

  end subroutine phs_fks_generator_6


end module phs_fks_uti
