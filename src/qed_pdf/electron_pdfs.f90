! WHIZARD 2.7.0 Jan 21 2019
!
! Copyright (C) 1999-2019 by
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

module electron_pdfs

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use constants, only: pi
  use format_defs, only: FMT_19
  use numeric_utils
  use sm_physics, only: Li2
  
  implicit none
  private

  public :: qed_pdf_t

  type :: qed_pdf_t     
     private
     integer :: flv = 0
     real(default) :: mass = 0
     real(default) :: q_max = 0
     real(default) :: alpha = 0
     real(default) :: eps = 0
     integer :: order
   contains
     procedure :: init => qed_pdf_init
     procedure :: write => qed_pdf_write
     procedure :: set_order => qed_pdf_set_order
     procedure :: evolve_qed_pdf => qed_pdf_evolve_qed_pdf
  end type qed_pdf_t


contains

  subroutine qed_pdf_init &
       (qed_pdf, mass, alpha, charge, q_max, order)
    class(qed_pdf_t), intent(out) :: qed_pdf
    real(default), intent(in) :: mass, alpha, q_max, charge
    integer, intent(in) :: order
    qed_pdf%mass = mass
    qed_pdf%q_max = q_max
    qed_pdf%alpha = alpha
    qed_pdf%order = order    
    qed_pdf%eps = alpha/pi * charge**2 &
         * (2 * log (q_max / mass) - 1)         
  end subroutine qed_pdf_init

  subroutine qed_pdf_write (qed_pdf, unit)
    class(qed_pdf_t), intent(in) :: qed_pdf
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(3x,A)")  "QED structure function (PDF):"
    write (u, "(5x,A,I0)") "Flavor    = ", qed_pdf%flv
    write (u, "(5x,A," // FMT_19 // ")") "Mass      = ", qed_pdf%mass
    write (u, "(5x,A," // FMT_19 // ")") "q_max     = ", qed_pdf%q_max
    write (u, "(5x,A," // FMT_19 // ")") "alpha     = ", qed_pdf%alpha    
    write (u, "(5x,A,I0)") "Order     = ", qed_pdf%order 
    write (u, "(5x,A," // FMT_19 // ")") "epsilon   = ", qed_pdf%eps   
  end subroutine qed_pdf_write

  subroutine qed_pdf_set_order (qed_pdf, order)
    class(qed_pdf_t), intent(inout) :: qed_pdf
    integer, intent(in) :: order
    qed_pdf%order = order
  end subroutine qed_pdf_set_order

  subroutine qed_pdf_evolve_qed_pdf (qed_pdf, x, xb, rb, ff)
    class(qed_pdf_t), intent(inout) :: qed_pdf
    real(default), intent(in) :: x, xb, rb
    real(default), intent(inout) :: ff
    real(default), parameter :: &
         & xmin = 0.00714053329734592839549879772019_default   
    real(default), parameter :: &
         & zeta3 = 1.20205690315959428539973816151_default
    real(default), parameter :: &
         g1 = 3._default / 4._default, &
         g2 = (27 - 8 * pi**2) / 96._default, &
         g3 = (27 - 24 * pi**2 + 128 * zeta3) / 384._default
    real(default) :: x_2, log_x, log_xb
    if (ff > 0 .and. qed_pdf%order > 0) then
       ff = ff * (1 + g1 * qed_pdf%eps)
       x_2 = x * x
       if (rb > 0)  ff = ff * (1 - (1-x_2) / (2 * rb))
       if (qed_pdf%order > 1) then
          ff = ff * (1 + g2 * qed_pdf%eps**2)
          if (rb > 0 .and. xb > 0 .and. x > xmin) then
             log_x  = log_prec (x, xb)
             log_xb = log_prec (xb, x)
             ff = ff * (1 - ((1 + 3 * x_2) * log_x + xb * (4 * (1 + x) * &
                  log_xb + 5 + x)) / (8 * rb) * qed_pdf%eps)
          end if
          if (qed_pdf%order > 2) then
             ff = ff * (1 + g3 * qed_pdf%eps**3)
             if (rb > 0 .and. xb > 0 .and. x > xmin) then
                ff = ff * (1 - ((1 + x) * xb &
                     * (6 * Li2(x) + 12 * log_xb**2 - 3 * pi**2) &
                     + 1.5_default * (1 + 8 * x + 3 * x_2) * log_x &
                     + 6 * (x + 5) * xb * log_xb &
                     + 12 * (1 + x_2) * log_x * log_xb &
                     - (1 + 7 * x_2) * log_x**2 / 2 &
                     + (39 - 24 * x - 15 * x_2) / 4) &
                     / (48 * rb) * qed_pdf%eps**2)
             end if
          end if
       end if
    end if
  end subroutine qed_pdf_evolve_qed_pdf


end module electron_pdfs
