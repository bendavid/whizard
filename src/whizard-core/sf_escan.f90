! WHIZARD 2.0.5 Tue May 10 2011
! 
! Copyright (C) 1999-2011 by 
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

module sf_escan

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use lorentz !NODEP!
  use flavors
  use helicities
  use colors
  use quantum_numbers
  use state_matrices
  use polarizations
  use interactions
  use sf_aux

  implicit none
  private

  public :: escan_data_t
  public :: escan_data_init
  public :: escan_data_write
  public :: interaction_init_escan
  public :: interaction_apply_escan

  type :: escan_data_t
     private
     logical, dimension(2) :: affects_beam
     type(flavor_t), dimension(2) :: flv
     real(default), dimension(2) :: mass = 0
     real(default) :: sqrts = 0
  end type escan_data_t


contains

  subroutine escan_data_init (data, affects_beam, flv_in, sqrts)
    type(escan_data_t), intent(out) :: data
    logical, dimension(2), intent(in) :: affects_beam
    type(flavor_t), dimension(2), intent(in) :: flv_in
    real(default), intent(in) :: sqrts
    data%affects_beam = affects_beam
    where (data%affects_beam)  data%flv = flv_in
    data%mass = flavor_get_mass (data%flv)
    data%sqrts = sqrts
  end subroutine escan_data_init

  subroutine escan_data_write (data, unit, md5)
    type(escan_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    integer :: u
    logical, intent(in), optional :: md5
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) "Energy-scan data:"
    if (all (data%affects_beam)) then
       write (u, *) "  [both beams]"
    else if (data%affects_beam(1)) then
       write (u, *) "  [first beam]"
    else if (data%affects_beam(2)) then
       write (u, *) "  [second beam]"
    end if
    write (u, *) "  prt_in  = ", char (flavor_get_name (data%flv(1))), &
         ", ", char (flavor_get_name (data%flv(2)))    
  end subroutine escan_data_write

  subroutine interaction_init_escan (int, data)
    type(interaction_t), intent(out) :: int
    type(escan_data_t), intent(in) :: data
    type(quantum_numbers_mask_t), dimension(4) :: mask
    integer, dimension(4) :: lock
    type(quantum_numbers_t), dimension(4) :: qn_fc, qn_hel, qn
    type(polarization_t) :: pol1, pol2
    type(state_iterator_t) :: it_hel1, it_hel2
    integer :: i
    if (all (data%affects_beam)) then
       lock = (/ 3, 4, 1, 2 /)
       call interaction_init &
            (int, 2, 0, 2, mask=mask, lock=lock, set_relations=.true.)
    else if (data%affects_beam(1)) then
       lock = (/ 2, 1, 0, 0 /)
       call interaction_init &
            (int, 1, 0, 1, mask=mask(1:2), lock=lock(1:2), set_relations=.true.)
    else if (data%affects_beam(2)) then
       lock = (/ 2, 1, 0, 0 /)
       call interaction_init &
            (int, 1, 0, 1, mask=mask(1:2), lock=lock(1:2), set_relations=.true.)
    end if
    do i = 1, 2
       call quantum_numbers_init (qn_fc(i), &
            flv = data%flv(i), col = color_from_flavor (data%flv(i)))
       call quantum_numbers_init (qn_fc(i+2), &
            flv = data%flv(i), col = color_from_flavor (data%flv(i)))
    end do
    call polarization_init_generic (pol1, data%flv(1))
    call state_iterator_init (it_hel1, pol1%state)
    do while (state_iterator_is_valid (it_hel1))
       qn_hel(1:1) = state_iterator_get_quantum_numbers (it_hel1)
       qn_hel(3:3) = state_iterator_get_quantum_numbers (it_hel1)
       call polarization_init_generic (pol2, data%flv(2))
       call state_iterator_init (it_hel2, pol2%state)
       do while (state_iterator_is_valid (it_hel2))
          qn_hel(2:2) = state_iterator_get_quantum_numbers (it_hel2)
          qn_hel(4:4) = state_iterator_get_quantum_numbers (it_hel2)
          qn = qn_hel .merge. qn_fc
          if (all (data%affects_beam)) then
             call interaction_add_state (int, qn)
          else if (data%affects_beam(1)) then
             call interaction_add_state (int, qn((/1,3/)))
          else if (data%affects_beam(2)) then
             call interaction_add_state (int, qn((/2,4/)))
          end if
          call state_iterator_advance (it_hel2)
       end do
       call polarization_final (pol2)
       call state_iterator_advance (it_hel1)
    end do
    call polarization_final (pol2)
    call interaction_freeze (int)
  end subroutine interaction_init_escan
    
  subroutine strfun (f, x, r, data)
    real(default), intent(out) :: f, x
    real(default), intent(in) :: r
    type(escan_data_t), intent(in) :: data
    x = r**2
    f = 1
  end subroutine strfun

  subroutine interaction_apply_escan (int, r, data)
    type(interaction_t), intent(inout), target :: int
    real(default), dimension(1), intent(in) :: r
    type(escan_data_t), intent(in) :: data
    type(vector4_t), dimension(2) :: k
    real(default) :: f, xx
    real(default), dimension(2) :: x
    type(splitting_data_t), dimension(2) :: sd
    type(vector4_t), dimension(4) :: q
    k(1) = interaction_get_momentum (int, 1)
    if (all (data%affects_beam)) then
       k(2) = interaction_get_momentum (int, 2)
    else
       k(2) = k(1)
    end if
    call strfun (f, xx, r(1), data)
    if (all (data%affects_beam)) then
       x = sqrt (xx)
    else
       where (data%affects_beam)
          x = xx
       elsewhere
          x = 1
       end where
    end if
    call interaction_set_matrix_element (int, cmplx (f, kind=default))
    sd = new_splitting_data (k, data%mass**2, 0._default, data%mass)
    call splitting_set_t_bounds (sd, x, 1-x)
    call splitting_set_collinear (sd)
    q((/1, 3/)) = split_momentum (k(1), sd(1))
    q((/2, 4/)) = split_momentum (k(2), sd(2))
    if (all (data%affects_beam)) then
       call interaction_set_momenta (int, q(3:4), outgoing=.true.)
    else if (data%affects_beam(1)) then
       call interaction_set_momenta (int, q(3:3), outgoing=.true.)
    else if (data%affects_beam(2)) then
       call interaction_set_momenta (int, q(4:4), outgoing=.true.)
    end if
  end subroutine interaction_apply_escan


end module sf_escan
