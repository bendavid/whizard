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

module sf_circe2

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use tao_random_numbers !NODEP!
  use lorentz !NODEP!
  use models
  use flavors
  use colors
  use quantum_numbers
  use state_matrices
  use polarizations
  use interactions
  use sf_aux

  implicit none
  private

  public :: circe2_data_t
  public :: circe2_data_init
  public :: circe2_data_check
  public :: circe2_set_id
  public :: circe2_data_write
  public :: interaction_init_circe2
  public :: interaction_apply_circe2

  integer, parameter :: NONE = 0

  type :: circe2_data_t
     private
     type(model_t), pointer :: model => null ()
     type(flavor_t), dimension(2) :: flv_in
     logical, dimension(2) :: photon = .false.
     type(tao_random_state), pointer :: rng => null ()
     type(string_t) :: file 
     type(string_t) :: design 
     real(default) :: sqrts
     integer :: id = 0
     integer :: ver = 0
     integer :: error = NONE
  end type circe2_data_t


  interface
     subroutine cir2ld (file, design, roots, ierror)
       import 
       character*(*) file, design
       double precision roots
       integer ierror
     end subroutine cir2ld
  end interface
  interface
     double precision function cir2dn (p1, h1, p2, h2, yy1, yy2)
       import 
       integer p1, h1, p2, h2
       double precision yy1, yy2
     end function cir2dn
  end interface  
  interface
     double precision function cir2lm (p1, h1, p2, h2)
        import
        integer p1, h1, p2, h2
     end function cir2lm  
  end interface
  interface
     subroutine cir2ch (p1, h1, p2, h2, rng)
       import
       integer p1, h1, p2, h2
       !!! procedure(rn_sub_t) :: rn_sub
     end subroutine cir2ch  
  end interface
  interface
      subroutine cir2gn (p1, h1, p2, h2, x1, x2, rng)
        import
        integer p1, h1, p2, h2
        double precision x1, x2
        !!! procedure(rn_sub_t) :: rn_sub
      end subroutine cir2gn
  end interface
  

  type(tao_random_state), pointer :: rng_tmp => null ()

contains

  subroutine circe2_data_init (data, model, flv_in, out_photon, rng, sqrts, file, design)
    type(circe2_data_t), intent(inout) :: data
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2), intent(in) :: flv_in
    logical, dimension(2), intent(in) :: out_photon
    type(tao_random_state), intent(in), target :: rng
    type(string_t), intent(in) :: file, design
    real(default), intent(in) :: sqrts
    data%model => model
    data%flv_in = flv_in
    data%photon = out_photon
    data%rng => rng
    data%file = file
    data%design = design
    data%sqrts = sqrts
    data%ver = 0
    select case (char (model_get_name (data%model)))
    case ("QCD","Test")
       call msg_fatal ("CIRCE2 structure function not available for model " &
            // char (model_get_name (data%model)) // ".")
    end select     
    call cir2ld (trim (char(data%file)), trim (char(data%design)), &
            data%sqrts, data%error)
  end subroutine circe2_data_init

  subroutine circe2_data_check (data)
    type(circe2_data_t), intent(in) :: data
    select case (data%error)
    end select
  end subroutine circe2_data_check

  subroutine circe2_set_id (data, id)
    type (circe2_data_t), intent(inout) :: data
    integer, intent(in) :: id
    data%id = id
  end subroutine circe2_set_id 

  subroutine circe2_data_write (data, unit)
    type(circe2_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) "CIRCE2 data:"
    write (u, *) "  prt_in  = ", char (flavor_get_name (data%flv_in(1))), &
         ", ", char (flavor_get_name (data%flv_in(2)))    
    write (u, *) "  photon = ", data%photon
    write (u, *) "  sqrts = ", data%sqrts
    write (u, *) "  ver = ", data%ver
    write (u, *) "  file = ", char(data%file)
    write (u, *) "  design = ", char(data%design)
  end subroutine circe2_data_write

  subroutine interaction_init_circe2 (int, circe2_data)
    type(interaction_t), intent(out) :: int
    type(circe2_data_t), intent(in) :: circe2_data
    logical, dimension(6) :: mask_h
    type(quantum_numbers_mask_t), dimension(6) :: mask
    integer, dimension(6) :: lock
    type(polarization_t) :: pol1, pol2
    type(quantum_numbers_t), dimension(1) :: qn_fc1, qn_hel1, qn_fc2, qn_hel2
    type(flavor_t) :: flv_photon
    type(quantum_numbers_t) :: qn_photon, qn1, qn2
    type(quantum_numbers_t), dimension(6) :: qn
    type(state_iterator_t) :: it_hel1, it_hel2
    lock = 0
    mask_h = .false.
    if (circe2_data%photon(1)) then
       lock(1) = 3;  lock(3) = 1;  mask_h(5) = .true.
    else
       lock(1) = 5;  lock(5) = 1;  mask_h(3) = .true.
    end if
    if (circe2_data%photon(2)) then
       lock(2) = 4;  lock(4) = 2;  mask_h(6) = .true.
    else
       lock(2) = 6;  lock(6) = 2;  mask_h(4) = .true.
    end if
    mask = new_quantum_numbers_mask (.false., .false., mask_h)
    call interaction_init & 
         (int, 2, 0, 4, mask=mask, lock=lock, set_relations=.true.) 
    call flavor_init (flv_photon, PHOTON, circe2_data%model)
    call quantum_numbers_init (qn_photon, flv_photon)
    call polarization_init_generic (pol1, circe2_data%flv_in(1))
    call quantum_numbers_init (qn_fc1(1), flv = circe2_data%flv_in(1))
    call polarization_init_generic (pol2, circe2_data%flv_in(2))
    call quantum_numbers_init (qn_fc2(1), flv = circe2_data%flv_in(2))
    call state_iterator_init (it_hel1, pol1%state) 
    do while (state_iterator_is_valid (it_hel1)) 
       qn_hel1 = state_iterator_get_quantum_numbers (it_hel1)
       qn1 = qn_hel1(1) .merge. qn_fc1(1) 
       qn(1) = qn1
       if (circe2_data%photon(1)) then
          qn(3) = qn1;  qn(5) = qn_photon
       else
          qn(3) = qn_photon;  qn(5) = qn1
       end if
       call state_iterator_init (it_hel2, pol2%state) 
       do while (state_iterator_is_valid (it_hel2)) 
          qn_hel2 = state_iterator_get_quantum_numbers (it_hel2) 
          qn2 = qn_hel2(1) .merge. qn_fc2(1) 
          qn(2) = qn2
          if (circe2_data%photon(2)) then
             qn(4) = qn2;  qn(6) = qn_photon
          else
             qn(4) = qn_photon;  qn(6) = qn2
          end if
          call interaction_add_state (int, qn)
          call state_iterator_advance (it_hel2) 
       end do 
       call state_iterator_advance (it_hel1)
    end do 
    call polarization_final (pol1)
    call polarization_final (pol2)
    call interaction_freeze (int)     
  end subroutine interaction_init_circe2
    
  elemental subroutine strfun (f, x, xb, r, E, data)
    real(default), intent(out) :: f, x, xb
    real(default), intent(in) :: r, E
    type(circe2_data_t), intent(in) :: data
  end subroutine strfun
  
  subroutine rn_sub (r)
    double precision, intent(out) :: r
    real(double) :: x
    call tao_random_number (rng_tmp, x)
    r = x
  end subroutine rn_sub

  subroutine interaction_apply_circe2 (int, r, circe2_data)
    type(interaction_t), intent(inout) :: int
    real(default), dimension(:), intent(in) :: r
    type(circe2_data_t), dimension(:), intent(in) :: circe2_data
    type(vector4_t), dimension(2) :: k
    type(splitting_data_t), dimension(2) :: sd
    real(default), dimension(size(circe2_data)) :: f, x, xb
    type(vector4_t), dimension(4) :: q
    k(1) = interaction_get_momentum (int, 1)
    k(2) = interaction_get_momentum (int, 2)
    sd = new_splitting_data (k, k**2, 0._default, 0._default)
    call strfun (f, x, xb, r, energy (k), circe2_data)
    call interaction_set_matrix_element (int, cmplx (f, kind=default))
    call splitting_set_t_bounds (sd, x, xb)
    call splitting_set_collinear (sd)
    q((/1, 3/)) = split_momentum (k(1), sd(1))
    q((/2, 4/)) = split_momentum (k(2), sd(2))
    call interaction_set_momenta (int, q, outgoing=.true.)
  end subroutine interaction_apply_circe2


end module sf_circe2
