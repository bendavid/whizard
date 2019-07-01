! WHIZARD 2.1.0 June 15 2012
! 
! Copyright (C) 1999-2012 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     Christian Speckner <christian.speckner@physik.uni-freiburg.de>
!     with contributions by Sebastian Schmidt, Daniel Wiesler, Felix Braam
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

module beam_polarizations

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use flavors
  use polarizations

  implicit none
  private

  public :: BP_NONE, BP_CIRC, BP_TRANS, BP_LONG, BP_AXIS, BP_DIAG, BP_DENSITY
  public :: BP_TRIVIAL
  public :: beam_polarization_t
  public :: beam_polarization_init_none
  public :: beam_polarization_init_trivial
  public :: beam_polarization_init_circ
  public :: beam_polarization_init_trans
  public :: beam_polarization_init_long
  public :: beam_polarization_init_axis
  public :: beam_polarization_init_diag
  public :: beam_polarization_init_density
  public :: beam_polarization_final
  public :: beam_polarization2polarization
  public :: beam_polarization_write

  integer, parameter :: &
     BP_NONE = 0, BP_CIRC = 1, BP_TRANS = 2, BP_LONG = 3, BP_AXIS = 4, &
     BP_DIAG = 5, BP_DENSITY = 6, BP_TRIVIAL = 7

  type :: beam_polarization_t
    private
    integer :: type = BP_NONE
    real(default) :: fraction
    real(default) :: theta
    real(default) :: phi
    real(default) :: d
    complex(default) :: nd
    integer, dimension(:), allocatable :: hels
    real(default), dimension(:), allocatable :: fractions
  end type beam_polarization_t


contains

  subroutine beam_polarization_init_none (bp)
    type(beam_polarization_t), intent(inout) :: bp
    bp%type = BP_NONE
  end subroutine beam_polarization_init_none

  subroutine beam_polarization_init_trivial (bp)
    type(beam_polarization_t), intent(inout) :: bp
    bp%type = BP_TRIVIAL
  end subroutine beam_polarization_init_trivial

  subroutine beam_polarization_init_circ (bp, fraction)
    type(beam_polarization_t), intent(inout) :: bp
    real(default), intent(in) :: fraction
    bp%type = BP_CIRC
    bp%fraction = fraction
  end subroutine beam_polarization_init_circ

  subroutine beam_polarization_init_trans (bp, fraction, phi)
    type(beam_polarization_t), intent(inout) :: bp
    real(default), intent(in) :: fraction, phi
    bp%type = BP_TRANS
    bp%fraction = fraction
    bp%phi = phi
  end subroutine beam_polarization_init_trans

  subroutine beam_polarization_init_long (bp, fraction)
    type(beam_polarization_t), intent(inout) :: bp
    real(default), intent(in) :: fraction
    bp%type = BP_LONG
    bp%fraction = fraction
  end subroutine beam_polarization_init_long

  subroutine beam_polarization_init_axis (bp, fraction, theta, phi)
    type(beam_polarization_t), intent(inout) :: bp
    real(default), intent(in) :: fraction, theta, phi
    bp%type = BP_AXIS
    bp%fraction = fraction
    bp%theta = theta
    bp%phi = phi
  end subroutine beam_polarization_init_axis

  subroutine beam_polarization_init_diag (bp, hels, fracs)
    type(beam_polarization_t), intent(inout) :: bp
    integer, dimension(:), intent(in) :: hels
    real(default), dimension(:), intent(in) :: fracs
    bp%type = BP_DIAG
    allocate (bp%hels(size (hels)))
    allocate (bp%fractions(size (fracs)))
    bp%hels = hels
    bp%fractions = fracs
  end subroutine beam_polarization_init_diag

  subroutine beam_polarization_init_density (bp, d, nd)
    type(beam_polarization_t), intent(inout) :: bp
    real(default), intent(in) :: d
    complex(default), intent(in) :: nd
    bp%type = BP_DENSITY
    bp%d = d
    bp%nd = nd
  end subroutine beam_polarization_init_density

  subroutine beam_polarization_final (bp)
    type(beam_polarization_t), intent(inout) :: bp
    if (allocated (bp%hels)) deallocate (bp%hels)
    if (allocated (bp%fractions)) deallocate (bp%fractions)
  end subroutine beam_polarization_final

  function beam_polarization2polarization (bp, flv, decay) result (pol)
    type(beam_polarization_t), intent(in) :: bp
    type(flavor_t), intent(in) :: flv
    logical, optional, intent(in) :: decay
    type(polarization_t) :: pol
    logical :: fail
    real(default), dimension(:), allocatable :: frac_vector
    integer :: i, j, mult
    type(string_t) :: msg
    if (flavor_get_multiplicity (flv) == 1) then
       select case (bp%type)
          case (BP_NONE, BP_TRIVIAL)
          case default
             if (flavor_is_left_handed (flv)) then
                msg = "left-handed"
             elseif (flavor_is_right_handed (flv)) then
                msg = "right-handed"
             else
                msg = "scalar"
             end if
             call msg_error (char (msg) // " particle '" &
                // char (flavor_get_name (flv)) &
                // "' cannot be polarized - ignoring polarization")
             call emergency_unpolarized
             return
       end select
    end if
    select case (bp%type)
       case (BP_NONE)
          call polarization_init_unpolarized (pol, flv)
       case (BP_TRIVIAL)
          call polarization_init_trivial (pol, flv)
       case (BP_CIRC)
          if ((bp%fraction <= 1) .and. (bp%fraction >= -1)) then
             call polarization_init_circular (pol, flv, bp%fraction)
          else
             call msg_error ( &
                "circular polarization: 'fraction' must be within [-1; 1] - " &
                // "ignoring polarization")
             call emergency_unpolarized
          end if
       case (BP_TRANS)
          if ((bp%fraction <= 1) .and. (bp%fraction >= -1)) then
             call polarization_init_transversal (pol, flv, bp%phi, bp%fraction)
          else
             call msg_error ( &
                "transverse polarization: 'fraction' must be within [-1; 1] - " &
                // "ignoring polarization")
             call emergency_unpolarized
          end if
       case (BP_LONG)
          if ((bp%fraction > 1) .or. (bp%fraction < 0)) then
             call msg_error ( &
                "longitudinal polarization: 'fraction' must be within [0; 1]" &
                // " - ignoring polarization");
             call emergency_unpolarized
          elseif (mod (flavor_get_multiplicity (flv), 2) == 0) then
             call msg_error ( &
                "longitudinal polarization is only available for massive " &
                // " bosons - ignoring polarization")
             call emergency_unpolarized
          else
             call polarization_init_longitudinal (pol, flv, bp%fraction)
          end if
       case (BP_AXIS)
           if ((bp%fraction <= 1) .and. (bp%fraction >= -1)) then
             call polarization_init_angles (pol, flv, bp%fraction, bp%theta, &
                bp%phi)
          else
             call msg_error ( &
                "axial polarization: 'fraction' must be within [-1; 1] - " &
                // "ignoring polarization")
             call emergency_unpolarized
          end if
       case (BP_DENSITY)
          if ((bp%d <= 1) .and. (bp%d >= 0) .and. (abs (bp%nd) <= 0.5)) then
             call polarization_init_axis (pol, flv, &
                (/real (bp%nd, default), (-1.) * aimag (bp%nd), 2. * bp%d - 1./))
          else
             call msg_error ( &
                "density matrix polarization: 'a' must be within [0; 1], |b| " &
                // "within [0; 0.5] - ignoring polarization")
             call emergency_unpolarized
          end if
       case (BP_DIAG)
          fail = .false.
          mult = flavor_get_multiplicity (flv)
          allocate (frac_vector (mult))
          frac_vector = 0
          if (minval (bp%fractions) < 0) then
             call msg_error ( &
                "diagonal polarization: negative fractions are not allowed " &
                // "- ignoring polarization")
             fail = .true.
          else
          select case (mult) 
             case (1)
                call msg_bug (&
                   "beam_polarizeation2polarization: invalid multiplicity")
             case (2)
                if ((size (bp%hels) <= 2) .and. all (abs (bp%hels) == 1)) then
                   frac_vector = 0
                   do i = 1, size(bp%hels)
                      frac_vector((bp%hels(i) + 1) / 2 + 1) = bp%fractions(i)
                   end do
                else
                   call msg_error ( &
                      "diagonal polarization: the only admissible helicities " &
                      // "for particle '" // char (flavor_get_name (flv)) &
                      // "' are" // " -1 and 1 - ignoring polarization")
                   fail = .true.
                end if
             case default
                if (maxval (abs (bp%hels)) <= mult / 2) then
                   if (mod (mult, 2) == 0) then
                      if (minval (abs (bp%hels)) == 0) then
                         call msg_error ( &
                            "diagonal polarization: helicity 0 not allowed " &
                            // "for particle '" // char (flavor_get_name (flv)) &
                            // "' - ignoring polarization")
                         fail = .true.
                      else
                         do i = 1, size (bp%hels)
                            if (bp%hels(i) < 0) then
                               j = bp%hels(i) + mult / 2 + 1
                            else
                               j = bp%hels(i) + mult / 2
                            end if
                            frac_vector(j) = bp%fractions(i)
                         end do
                      end if
                   else
                      do i = 1, size (bp%hels)
                         j = bp%hels(i) + mult / 2 + 1
                         frac_vector(j) = bp%fractions(i)
                      end do
                   end if
                else
                   call msg_error ( &
                      "diagonal polarization: helicity exceeds admissible " &
                      // "range for particle '" // char (flavor_get_name (flv)) &
                      // "' - ignoring polarization")
                   fail = .true.
                end if
          end select
          end if
          if (fail) then
             call emergency_unpolarized
          else
             if (sum (frac_vector) /= 1) &
                call msg_warning ( &
                   "diagonal polarization: fractions will be normalized to 1")
             call polarization_init_diagonal (pol, flv, frac_vector)
          end if
          deallocate (frac_vector)
    end select

  contains

    subroutine emergency_unpolarized
      logical :: is_decay
      if (present (decay)) then
         is_decay = decay
      else
         is_decay = .false.
      end if
      if (is_decay) then
         call polarization_init_trivial (pol, flv)
      else
         call polarization_init_unpolarized (pol, flv)
      end if
    end subroutine emergency_unpolarized

  end function beam_polarization2polarization

  subroutine beam_polarization_write (bp, unit, indent)
    type(beam_polarization_t), intent(in) :: bp
    integer, intent(in), optional :: unit, indent
    integer :: u, i
    type(string_t), dimension(:), allocatable :: msgs
    type(string_t) :: header, is
    u = output_unit (unit)
    if (u < 0) return
    select case (bp%type)
       case (BP_NONE, BP_TRIVIAL)
          call printer ("none")
       case (BP_CIRC)
          call printer ("circular (fraction):")
          call printer ("   fraction: " // real2char (bp%fraction))
       case (BP_TRANS)
          call printer ("transverse (fraction, phi):")
          call printer ("   fraction: " // real2char (bp%fraction))
          call printer ("   phi     : " // real2char (bp%phi))
       case (BP_AXIS)
          call printer ("axis (fraction, theta, phi):")
          call printer ("   fraction: " // real2char (bp%fraction))
          call printer ("   theta   : " // real2char (bp%theta))
          call printer ("   phi     : " // real2char (bp%phi))
       case (BP_LONG)
          call printer ("longitudinal (fraction):")
          call printer ("   fraction: " // real2char (bp%fraction))
       case (BP_DENSITY)
          call printer ("density_matrix (a, b):")
          call printer ("   a: " // real2char (bp%d))
          call printer ("   b: " // char (cmplx2string (bp%nd)))
       case (BP_DIAG)
          allocate (msgs(size (bp%fractions)))
          header = "diagonal_density ("
          do i = 1, size (msgs)
             is = int2string (i)
             if (i > 1) header = header // ", "
             header = header // "h" // is // ":f" // is
             msgs (i) = "h" // is // ": " // int2string (bp%hels(i)) &
               // " , f" // is // ": " // real2string (bp%fractions(i))
          end do
          call printer (char (header) // ")")
          do i = 1, size (msgs)
             call printer ("   " // char (msgs(i)))
          end do
          deallocate (msgs)
       case default
          call msg_bug ("beam_polarization_write: illegal polarization type")
    end select
    flush (u)

  contains

    subroutine printer (s)
      character(*), intent(in) :: s
      if (present (indent)) write (u, '(A)', advance="no") &
         repeat (" ", indent)
      write (u, '(1x,A)') s
    end subroutine printer
  
  end subroutine beam_polarization_write


end module beam_polarizations
