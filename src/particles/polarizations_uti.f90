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

module polarizations_uti

  use kinds, only: default
  use format_defs, only: FMT_12
  use flavors
  use model_data

  use polarizations

  implicit none
  private

  public :: polarization_1
  public :: polarization_2

contains

  subroutine polarization_1 (u)
    use os_interface
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(polarization_t) :: pol
    type(flavor_t) :: flv
    real(default), dimension(3) :: alpha
    real(default) :: r, theta, phi

    write (u, "(A)")  "* Test output: polarization_1"
    write (u, "(A)")  "*   Purpose: test polarization setup"
    write (u, "(A)")      
    
    write (u, "(A)")  "* Reading model file"
    write (u, "(A)")      
    
    call model%init_sm_test ()

    write (u, "(A)") "* Unpolarized fermion"
    write (u, "(A)")
    
    call flv%init (1, model)
    call polarization_init_unpolarized (pol, flv)
    call polarization_write (pol, u)
    write (u, "(A,L1)")  "   diagonal =", polarization_is_diagonal (pol)
    call polarization_final (pol)
    
    write (u, "(A)") 
    write (u, "(A)")  "* Unpolarized fermion"
    write (u, "(A)") 
    
    call polarization_init_circular (pol, flv, 0._default)
    call polarization_write (pol, u)
    call polarization_final (pol)
    
    write (u, "(A)")
    write (u, "(A)")  "* Transversally polarized fermion, phi=0"
    write (u, "(A)")
    
    call polarization_init_transversal (pol, flv, 0._default, 1._default)
    call polarization_write (pol, u)
    write (u, "(A,L1)")  "   diagonal =", polarization_is_diagonal (pol)
    call polarization_final (pol)
    
    write (u, "(A)") 
    write (u, "(A)")  "* Transversally polarized fermion, phi=0.9, frac=0.8"
    write (u, "(A)")
    
    call polarization_init_transversal (pol, flv, 0.9_default, 0.8_default)
    call polarization_write (pol, u)
    write (u, "(A,L1)")  "   diagonal =", polarization_is_diagonal (pol)
    call polarization_final (pol)
    
    write (u, "(A)")
    write (u, "(A)") "* All polarization directions of a fermion"
    write (u, "(A)")
    
    call polarization_init_generic (pol, flv)
    call polarization_write (pol, u)
    call polarization_final (pol)
    call flv%init (21, model)
    
    write (u, "(A)") 
    write (u, "(A)")  "* Circularly polarized gluon, frac=0.3"
    write (u, "(A)") 
    
    call polarization_init_circular (pol, flv, 0.3_default)
    call polarization_write (pol, u)
    call polarization_final (pol)   
    call flv%init (23, model)
    
    write (u, "(A)") 
    write (u, "(A)") "* Circularly polarized massive vector, frac=-0.7"
    write (u, "(A)") 
    
    call polarization_init_circular (pol, flv,  -0.7_default)
    call polarization_write (pol, u)
    call polarization_final (pol)
    
    write (u, "(A)") 
    write (u, "(A)")  "* Circularly polarized massive vector"
    write (u, "(A)")
    
    call polarization_init_circular (pol, flv, 1._default)
    call polarization_write (pol, u)
    call polarization_final (pol)
    
    write (u, "(A)") 
    write (u, "(A)")  "* Longitudinally polarized massive vector, frac=0.4"
    write (u, "(A)")
    
    call polarization_init_longitudinal (pol, flv, 0.4_default)
    call polarization_write (pol, u)
    call polarization_final (pol)
    
    write (u, "(A)") 
    write (u, "(A)")  "* Longitudinally polarized massive vector"
    write (u, "(A)") 
    
    call polarization_init_longitudinal (pol, flv, 1._default)
    call polarization_write (pol, u)
    call polarization_final (pol)
    
    write (u, "(A)") 
    write (u, "(A)")  "* Diagonally polarized massive vector"
    write (u, "(A)")
    
    call polarization_init_diagonal &
         (pol, flv, [0._default, 1._default, 2._default])
    call polarization_write (pol, u)
    call polarization_final (pol)
    
    write (u, "(A)") 
    write (u, "(A)")  "* All polarization directions of a massive vector"
    write (u, "(A)") 

    call polarization_init_generic (pol, flv)
    call polarization_write (pol, u)
    call polarization_final (pol)
    call flv%init (21, model)
    
    write (u, "(A)") 
    write (u, "(A)")  "* Axis polarization (0.2, 0.4, 0.6)"
    write (u, "(A)") 
    
    alpha = [0.2_default, 0.4_default, 0.6_default]
    call polarization_init_axis (pol, flv, alpha)
    call polarization_write (pol, u)
    
    write (u, "(A)")  "   Recovered axis:"
    alpha = polarization_get_axis (pol)
    write (u, "(A)")  "   Angle polarization (0.5, 0.6, -1)"
    r = 0.5_default
    theta = 0.6_default
    phi = -1._default
    call polarization_init_angles (pol, flv, r, theta, phi)
    call polarization_write (pol, u)
    write (u, "(A)")  "   Recovered parameters (r, theta, phi):"
    call polarization_to_angles (pol, r, theta, phi)
    write (u, "(A,3(1x," // FMT_12 // "))")  "     ", r, theta, phi
    call polarization_final (pol)
    
    call model%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: polarization_1"    
      
  end subroutine polarization_1

  subroutine polarization_2 (u)
    use os_interface
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(flavor_t) :: flv
    type(polarization_t) :: pol
    real(default), dimension(3) :: alpha
    type(pmatrix_t) :: pmatrix
    real(default), parameter :: tolerance = 1e-8_default

    write (u, "(A)")  "* Test output: polarization_2"
    write (u, "(A)")  "*   Purpose: matrix polarization setup"
    write (u, "(A)")      
    
    write (u, "(A)")  "* Reading model file"
    write (u, "(A)") 
    
    call model%init_sm_test ()

    write (u, "(A)") "* Unpolarized fermion"
    write (u, "(A)")
    
    call flv%init (1, model)
    call pmatrix%init (2, 0)
    call pmatrix%normalize (flv, 0._default, tolerance)
    call pmatrix%write (u)
    write (u, *) 
    write (u, "(1x,A,L1)")  "polarized = ", pmatrix%is_polarized ()
    write (u, "(1x,A,L1)")  "diagonal = ", pmatrix%is_diagonal ()
    write (u, *) 

    call polarization_init_pmatrix (pol, pmatrix)
    call polarization_write (pol, u)
    call polarization_final (pol)
    
    write (u, "(A)")
    write (u, "(A)")  "* Transversally polarized fermion, phi=0"
    write (u, "(A)")
    
    call pmatrix%init (2, 3)
    call pmatrix%set_entry (1, [-1,-1], (1._default, 0._default))
    call pmatrix%set_entry (2, [+1,+1], (1._default, 0._default))
    call pmatrix%set_entry (3, [-1,+1], (1._default, 0._default))
    call pmatrix%normalize (flv, 1._default, tolerance)
    call pmatrix%write (u)
    write (u, *) 
    write (u, "(1x,A,L1)")  "polarized = ", pmatrix%is_polarized ()
    write (u, "(1x,A,L1)")  "diagonal = ", pmatrix%is_diagonal ()
    write (u, *) 

    call polarization_init_pmatrix (pol, pmatrix)
    call polarization_write (pol, u)
    call polarization_final (pol)
    
    write (u, "(A)") 
    write (u, "(A)")  "* Transversally polarized fermion, phi=0.9, frac=0.8"
    write (u, "(A)")
    
    call pmatrix%init (2, 3)
    call pmatrix%set_entry (1, [-1,-1], (1._default, 0._default))
    call pmatrix%set_entry (2, [+1,+1], (1._default, 0._default))
    call pmatrix%set_entry (3, [-1,+1], exp ((0._default, -0.9_default)))
    call pmatrix%normalize (flv, 0.8_default, tolerance)
    call pmatrix%write (u)
    write (u, *) 

    call polarization_init_pmatrix (pol, pmatrix)
    call polarization_write (pol, u)
    call polarization_final (pol)
    
    write (u, "(A)") 
    write (u, "(A)")  "* Left-handed massive fermion, frac=1"
    write (u, "(A)")
    
    call flv%init (11, model)
    call pmatrix%init (2, 1)
    call pmatrix%set_entry (1, [-1,-1], (1._default, 0._default))
    call pmatrix%normalize (flv, 1._default, tolerance)
    call pmatrix%write (u)
    write (u, *) 
    write (u, "(1x,A,L1)")  "polarized = ", pmatrix%is_polarized ()
    write (u, "(1x,A,L1)")  "diagonal = ", pmatrix%is_diagonal ()
    write (u, *) 

    call polarization_init_pmatrix (pol, pmatrix)
    call polarization_write (pol, u)
    call polarization_final (pol)
    
    write (u, "(A)") 
    write (u, "(A)")  "* Left-handed massive fermion, frac=0.8"
    write (u, "(A)")
    
    call flv%init (11, model)
    call pmatrix%init (2, 1)
    call pmatrix%set_entry (1, [-1,-1], (1._default, 0._default))
    call pmatrix%normalize (flv, 0.8_default, tolerance)
    call pmatrix%write (u)
    write (u, *) 

    call polarization_init_pmatrix (pol, pmatrix)
    call polarization_write (pol, u)
    call polarization_final (pol)
    
    write (u, "(A)") 
    write (u, "(A)")  "* Left-handed massless fermion"
    write (u, "(A)")
    
    call flv%init (12, model)
    call pmatrix%init (2, 0)
    call pmatrix%normalize (flv, 1._default, tolerance)
    call pmatrix%write (u)
    write (u, *) 

    call polarization_init_pmatrix (pol, pmatrix)
    call polarization_write (pol, u)
    call polarization_final (pol)
    
    write (u, "(A)") 
    write (u, "(A)")  "* Right-handed massless fermion, frac=0.5"
    write (u, "(A)")
    
    call flv%init (-12, model)
    call pmatrix%init (2, 1)
    call pmatrix%set_entry (1, [1,1], (1._default, 0._default))
    call pmatrix%normalize (flv, 0.5_default, tolerance)
    call pmatrix%write (u)
    write (u, *) 

    call polarization_init_pmatrix (pol, pmatrix)
    call polarization_write (pol, u)
    call polarization_final (pol)
    
    write (u, "(A)") 
    write (u, "(A)")  "* Circularly polarized gluon, frac=0.3"
    write (u, "(A)") 
    
    call flv%init (21, model)
    call pmatrix%init (2, 1)
    call pmatrix%set_entry (1, [1,1], (1._default, 0._default))
    call pmatrix%normalize (flv, 0.3_default, tolerance)
    call pmatrix%write (u)
    write (u, *) 

    call polarization_init_pmatrix (pol, pmatrix)
    call polarization_write (pol, u)
    call polarization_final (pol)   
    
    write (u, "(A)") 
    write (u, "(A)") "* Circularly polarized massive vector, frac=0.7"
    write (u, "(A)") 
    
    call flv%init (23, model)
    call pmatrix%init (2, 1)
    call pmatrix%set_entry (1, [1,1], (1._default, 0._default))
    call pmatrix%normalize (flv, 0.7_default, tolerance)
    call pmatrix%write (u)
    write (u, *) 

    call polarization_init_pmatrix (pol, pmatrix)
    call polarization_write (pol, u)
    call polarization_final (pol)
    
    write (u, "(A)") 
    write (u, "(A)")  "* Circularly polarized massive vector"
    write (u, "(A)")
    
    call flv%init (23, model)
    call pmatrix%init (2, 1)
    call pmatrix%set_entry (1, [1,1], (1._default, 0._default))
    call pmatrix%normalize (flv, 1._default, tolerance)
    call pmatrix%write (u)
    write (u, *) 

    call polarization_init_pmatrix (pol, pmatrix)
    call polarization_write (pol, u)
    call polarization_final (pol)
    
    write (u, "(A)") 
    write (u, "(A)")  "* Longitudinally polarized massive vector, frac=0.4"
    write (u, "(A)")
    
    call flv%init (23, model)
    call pmatrix%init (2, 1)
    call pmatrix%set_entry (1, [0,0], (1._default, 0._default))
    call pmatrix%normalize (flv, 0.4_default, tolerance)
    call pmatrix%write (u)
    write (u, *) 
    write (u, "(1x,A,L1)")  "polarized = ", pmatrix%is_polarized ()
    write (u, "(1x,A,L1)")  "diagonal = ", pmatrix%is_diagonal ()
    write (u, *) 

    call polarization_init_pmatrix (pol, pmatrix)
    call polarization_write (pol, u)
    call polarization_final (pol)
    
    write (u, "(A)") 
    write (u, "(A)")  "* Longitudinally polarized massive vector"
    write (u, "(A)") 
    
    call flv%init (23, model)
    call pmatrix%init (2, 1)
    call pmatrix%set_entry (1, [0,0], (1._default, 0._default))
    call pmatrix%normalize (flv, 1._default, tolerance)
    call pmatrix%write (u)
    write (u, *) 
    write (u, "(1x,A,L1)")  "polarized = ", pmatrix%is_polarized ()
    write (u, "(1x,A,L1)")  "diagonal = ", pmatrix%is_diagonal ()
    write (u, *) 

    call polarization_init_pmatrix (pol, pmatrix)
    call polarization_write (pol, u)
    call polarization_final (pol)
    
    write (u, "(A)") 
    write (u, "(A)")  "* Axis polarization (0.2, 0.4, 0.6)"
    write (u, "(A)") 
    
    call flv%init (11, model)
    alpha = [0.2_default, 0.4_default, 0.6_default]
    alpha = alpha / sqrt (sum (alpha**2))
    call pmatrix%init (2, 3)
    call pmatrix%set_entry (1, [-1,-1], cmplx (1 - alpha(3), kind=default))
    call pmatrix%set_entry (2, [1,-1], &
         cmplx (alpha(1), -alpha(2), kind=default))
    call pmatrix%set_entry (3, [1,1], cmplx (1 + alpha(3), kind=default))
    call pmatrix%normalize (flv, 1._default, tolerance)
    call pmatrix%write (u)
    write (u, *) 

    call polarization_init_pmatrix (pol, pmatrix)
    call polarization_write (pol, u)
    call polarization_final (pol)
    
    call model%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: polarization_2"    
      
  end subroutine polarization_2


end module polarizations_uti
