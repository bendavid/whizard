! WHIZARD 2.2.8 Nov 22 2015
! 
! Copyright (C) 1999-2015 by 
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
  use format_utils, only: write_separator
  use lorentz

  use phs_fks

  implicit none
  private

  public :: phs_fks_generator_1
  public :: phs_fks_generator_2
  public :: phs_fks_generator_3

contains

  subroutine phs_fks_generator_1 (u)
    integer, intent(in) :: u
    type(phs_fks_generator_t) :: generator
    type(vector4_t), dimension(:), allocatable :: p_born
    type(vector4_t), dimension(:), allocatable :: p_real
    integer :: emitter
    real(default) :: x1, x2, x3
    real(default), parameter :: sqrts = 250.0_default
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

    call generator%set_beam_energy (sqrts)

    write (u, "(A)") "* Use four-particle phase space containing: "
    call vector4_write_set (p_born, u, testflag = .true.)
    write (u, "(A)") "***********************"
    write (u, "(A)")

    x1=0.5_default; x2=0.25_default; x3=0.75_default
    write (u, "(A)" ) "* Use random numbers: "
    write (u, "(A,F3.2,1X,A,F3.2,1X,A,F3.2)") &
       "x1: ", x1, "x2: ", x2, "x3: ", x3
    
    allocate (generator%real_kinematics)
    call generator%real_kinematics%init (4)

    allocate (generator%emitters (2))
    generator%emitters(1) = 3; generator%emitters(2) = 4
    allocate (generator%m2 (4))
    generator%m2 = 0._default
    allocate (generator%is_massive (4))
    generator%is_massive(1:2) = .false.
    generator%is_massive(3:4) = .true.
    call generator%generate_radiation_variables ([x1,x2,x3], p_born)
    write (u, "(A)")  &
         "* With these, the following radiation variables have been produced:"
    associate (rad_var => generator%real_kinematics)
      write (u, "(A,F3.2)") "xi_tilde: ", rad_var%xi_tilde
      write (u, "(A,F3.2)") "y: " , rad_var%y(3)
      write (u, "(A,F3.2)") "phi: ", rad_var%phi
    end associate
    call write_separator (u)
    write (u, "(A)") "Produce real momenta: "
    emitter = 3
    write (u, "(A,I1)") "emitter: ", emitter
    call generator%generate_fsr (emitter, p_born, p_real)
    call vector4_write_set (p_real, u, testflag = .true., ultra = .true.)
    call write_separator (u)
    write (u, "(A)")  &
         "Test direct interface via phs_fks_generator_generate_from_x"
    p_real = generator%generate_fsr_from_x ([x1,x2,x3], emitter, p_born)
    call vector4_write_set (p_real, u, testflag = .true., ultra = .true.)
    write (u, "(A)")
    write (u, "(A)") "* Test output end: phs_fks_generator_1"

  end subroutine phs_fks_generator_1

  subroutine phs_fks_generator_2 (u)
    integer, intent(in) :: u
    type(phs_fks_generator_t) :: generator
    type(vector4_t), dimension(:), allocatable :: p_born
    type(vector4_t), dimension(:), allocatable :: p_real
    integer :: emitter
    real(default) :: x1, x2, x3
    real(default), parameter :: sqrts_hadronic = 250.0_default
    write (u, "(A)") "* Test output: phs_fks_generator_1"
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

    allocate (generator%emitters (2))
    allocate (generator%isr_kinematics)
    generator%n_in = 2
    generator%emitters(1) = 1; generator%emitters(2) = 2
    call generator%set_beam_energy (sqrts_hadronic)
    call generator%set_isr_kinematics (p_born)

    write (u, "(A)") "* Use four-particle phase space containing: "
    call vector4_write_set (p_born, u, testflag = .true.)
    write (u, "(A)") "***********************"
    write (u, "(A)")

    x1=0.5_default; x2=0.25_default; x3=0.65_default
    write (u, "(A)" ) "* Use random numbers: "
    write (u, "(A,F3.2,1X,A,F3.2,1X,A,F3.2)") &
       "x1: ", x1, "x2: ", x2, "x3: ", x3

    allocate (generator%real_kinematics)
    call generator%real_kinematics%init (4)
    generator%real_kinematics%p_born_lab = p_born

    allocate (generator%m2 (2))
    generator%m2(1) = 0._default; generator%m2(2) = 0._default
    allocate (generator%is_massive (4))
    generator%is_massive = .false.
    call generator%generate_radiation_variables ([x1,x2,x3], p_born)
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
    emitter = 1
    write (u, "(A,I1)") "emitter: ", emitter
    call generator%generate_isr (p_born, p_real)
    call vector4_write_set (p_real, u, testflag = .true., ultra = .true.)
    call write_separator (u)
    write (u, "(A)")
    write (u, "(A)") "* Test output end: phs_fks_generator_2"

  end subroutine phs_fks_generator_2

  subroutine phs_fks_generator_3 (u)
    integer, intent(in) :: u
    type(phs_fks_generator_t) :: generator
    type(vector4_t), dimension(:), allocatable :: p_born
    type(vector4_t), dimension(:), allocatable :: p_real
    real(default) :: x1, x2, x3
    real(default) :: mB, mW, mT
    integer :: i, emitter
    
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
    call vector4_write_set (p_born, u, testflag = .true., show_mass = .true.)
    write (u, "(A)") "**********************"
    write (u, "(A)") 

    x1=0.5_default; x2=0.25_default; x3=0.6_default
    write (u, "(A)") "* Use random numbers: "
    write (u, "(A,F3.2,1X,A,F3.2,A,1X,F3.2)") &
       "x1: ", x1, "x2: ", x2, "x3: ", x3

    allocate (generator%real_kinematics)
    call generator%real_kinematics%init (3)
    generator%real_kinematics%p_born_lab = p_born

    allocate (generator%emitters(2))
    generator%emitters(1) = 1
    generator%emitters(2) = 3
    allocate (generator%m2 (3), generator%is_massive(3))
    generator%m2(1) = mT**2
    generator%m2(2) = mW**2
    generator%m2(3) = mB**2
    generator%is_massive = .true.

    call generator%generate_radiation_variables ([x1,x2,x3], p_born)
    
    write (u, "(A)") &
       "* With these, the following radiation variables have been produced: "
    associate (rad_var => generator%real_kinematics)
      write (u, "(A,F4.2)") "xi_tilde: ", rad_var%xi_tilde
      do i = 1, 3
         write (u, "(A,I1,A,F5.2)") "i: ", i, "y: " , rad_var%y(i)
      end do
      write (u, "(A,F4.2)") "phi: ", rad_var%phi
    end associate

    call write_separator (u)
    write (u, "(A)") "Produce real momenta via initial-state emission: "
    emitter = 1
    write (u, "(A,I1)") "emitter: ", emitter
    call generator%generate_isr_decay (p_born, p_real)
    call pacify (p_real, 1E-6_default)
    call vector4_write_set (p_real, u, testflag = .true., & 
         show_mass = .true., ultra = .true.)
    call write_separator(u)
    write (u, "(A)") "Produce real momenta via final-state emisson: "
    emitter = 3
    write (u, "(A,I1)") "emitter: ", emitter    
    call generator%generate_fsr (emitter, p_born, p_real)
    call pacify (p_real, 1E-6_default)
    call vector4_write_set (p_real, u, testflag = .true., &
         show_mass = .true., ultra = .true.)
    write (u, "(A)")
    write (u, "(A)") "* Test output end: phs_fks_generator_3"

  end subroutine phs_fks_generator_3


end module phs_fks_uti
