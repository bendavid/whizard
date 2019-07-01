!!! module: ckkw_pseudo_weights_module
!!! This code is part of my Ph.D studies.
!!! 
!!! Copyright (C) 2012 Sebastian Schmidt <sebastian.t.schmidt@desy.de>
!!! 
!!! This program is free software; you can redistribute it and/or modify it
!!! under the terms of the GNU General Public License as published by the Free 
!!! Software Foundation; either version 3 of the License, or (at your option) 
!!! any later version.
!!! 
!!! This program is distributed in the hope that it will be useful, but WITHOUT
!!! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
!!! FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
!!! more details.
!!! 
!!! You should have received a copy of the GNU General Public License along
!!! with this program; if not, see <http://www.gnu.org/licenses/>.
!!! 
!!! Latest Change: Thu Jul 19 18:31:34 2012 Time zone: 7200 seconds
!!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module ckkw_pseudo_weights_module

  use kinds, only: default, double !NODEP!

! The ckkw_pseudo_shower_weights_t gives the (relative) weights for different clusterings of the final particles, as given in (2.7) of hep-ph/0503281v1
! Each particle is given a power of 2 (first particle = 1, second particle = 2, third particle = 4, ...). Each recombination therefore corresponds to an
! integer, that is not a power of 2. Fur multiple subsequent recombinations, no different weights for different sequences of clustering are stored. It is
! assumed that the weight of a multiply recombined state is a combination of the states with one fewer recombination and that these states' contributions
! are proportional to their weights.
! For a (2->n) event, the weights array thus has the size 2^(2+n)-1.
! The weights_by_type array gives the weights depending on the type of the particle, the first index is the same as for weights, the second index gives the type of the new mother particle:
! 0: uncolored (gamma, Z, W, Higgs)
! 1: colored (quark)
! 2: gluon
! 3: squark
! 4: gluino
!
! alphaS gives the value for alpha_S used in the generation of the matrix element. This is needed for the reweighting using the values for a running alphaS at the scales of the clusterings.


  type :: ckkw_pseudo_shower_weights_t
     real(kind=default), dimension(:), allocatable :: weights
     real(kind=default), dimension(:,:), allocatable :: weights_by_type
  end type ckkw_pseudo_shower_weights_t

  public :: ckkw_pseudo_shower_weights_print
  public :: ckkw_pseudo_shower_weights_init

contains

  subroutine ckkw_pseudo_shower_weights_init(ckkw_pseudo_shower_weights)
    type(ckkw_pseudo_shower_weights_t), intent(out) :: ckkw_pseudo_shower_weights

    alphaS = 0.0
    if(allocated(ckkw_pseudo_shower_weights%weights)) then 
       deallocate(ckkw_pseudo_shower_weights%weights)
    end if
    if(allocated(ckkw_pseudo_shower_weights%weights_by_type)) then
       deallocate(ckkw_pseudo_shower_weights%weights_by_type)
    end if
  end subroutine ckkw_pseudo_shower_weights_init

  subroutine ckkw_pseudo_shower_weights_print(ckkw_pseudo_shower_weights)
    type(ckkw_pseudo_shower_weights_t), intent(in) :: ckkw_pseudo_shower_weights
    integer :: s, i

    s = size(ckkw_pseudo_shower_weights%weights)
    print *, " ckkw_pseudo_shower_weights: "
    do i=1, s
       print *, i, ckkw_pseudo_shower_weights%weights(i), ckkw_pseudo_shower_weights%weights_by_type(i,:)
    end do
  end subroutine ckkw_pseudo_shower_weights_print

end module ckkw_pseudo_weights_module
