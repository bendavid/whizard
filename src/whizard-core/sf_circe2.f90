! WHIZARD 2.0.4 Tue Oct 26 2010
! 
! (C) 1999-2010 by 
!     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
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

module sf_circe2

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use tao_random_numbers !NODEP!
  use lorentz !NODEP!
  use models
  use flavors
  use helicities
  use quantum_numbers
  use state_matrices
  use polarizations
  use interactions
  use sf_aux

  implicit none
  private

  public :: circe2_data_t
  public :: circe2_data_init
  public :: circe2_data_write
  public :: interaction_init_circe2
  public :: interaction_apply_circe2

  type :: circe2_data_t
     private
     type(flavor_t), dimension(2) :: flv_in
     integer, dimension(2) :: pdg = 0
     real(default), dimension(2) :: mass = 0
     logical :: generate = .true.
     type(tao_random_state), pointer :: rng => null ()
     logical :: map = .true.
     integer, dimension(2) :: map_mode = -1
     real(default), dimension(2) :: map_power = 0
     type(string_t) :: file 
     type(string_t) :: design 
     real(default) :: sqrts = 0
     logical :: polarized = .false.
     real(default) :: lumi = 0
     real(default), dimension(-1:1,-1:1) :: lumi_hel_frac = 0
     real(default), dimension(0:4) :: lumi_hel_sum = 0
  end type circe2_data_t


  interface
     subroutine cir2ld (file, design, roots, ierror)
       import 
       character*(*), intent(in) :: file, design
       double precision, intent(in) :: roots
       integer, intent(inout) :: ierror
     end subroutine cir2ld
  end interface
  interface
     double precision function cir2lm (p1, h1, p2, h2)
        import
        integer, intent(in) :: p1, h1, p2, h2
     end function cir2lm  
  end interface
  interface
     double precision function cir2dn (p1, h1, p2, h2, x1, x2)
       import 
       integer, intent(in) :: p1, h1, p2, h2
       double precision, intent(in) :: x1, x2
     end function cir2dn
  end interface  
  abstract interface
     subroutine rng_call (x)
       double precision, intent(out) :: x
     end subroutine rng_call
  end interface
!   interface
!      subroutine cir2ch (p1, h1, p2, h2, rng)
!        import
!        integer, intent(out) :: p1, h1, p2, h2
!        procedure(rng_call) :: rng
!      end subroutine cir2ch  
!   end interface
  interface
     subroutine cir2gn (p1, h1, p2, h2, x1, x2, rng)
       import
       integer, intent(in) :: p1, h1, p2, h2
       double precision, intent(out) :: x1, x2
       procedure(rng_call) :: rng
     end subroutine cir2gn
  end interface
  

  type(tao_random_state), pointer :: rng_tmp => null ()

contains

  subroutine circe2_data_init &
       (data, flv_in, generate, rng, map, file, design, sqrts, polarized)
    type(circe2_data_t), intent(out) :: data
    type(flavor_t), dimension(2), intent(in) :: flv_in
    logical, intent(in) :: generate
    type(tao_random_state), intent(in), target :: rng
    logical, intent(in) :: map
    type(string_t), intent(in) :: file, design
    real(default), intent(in) :: sqrts
    logical, intent(in) :: polarized
    integer :: error, i, h1, h2, h
    data%flv_in = flv_in
    data%pdg = flavor_get_pdg (data%flv_in)
    data%mass = flavor_get_mass (data%flv_in)
    data%generate = generate
    data%rng => rng
    data%map = map
    if (data%map) then
       do i = 1, 2
          select case (abs (data%pdg(i)))
          case (PHOTON);    data%map_mode(i) = 0;  data%map_power(i) = 3
          case (ELECTRON);  data%map_mode(i) = 1;  data%map_power(i) = 12
          case default;  call msg_fatal &
               ("CIRCE2: defined only for photon and electron beams")
          end select
       end do
    else
       data%map_mode = -1
    end if
    data%file = file
    data%design = design
    data%sqrts = sqrts
    data%polarized = polarized
    error = 1
    call cir2ld (trim (char(data%file)), trim (char(data%design)), &
            dble (data%sqrts), error)
    select case (error)
    case (-1)
       call msg_fatal ("CIRCE2: data file not found.")
    case (-2)
       call msg_fatal ("CIRCE2: beam parameters do not match data file.")
    case (-3)
       call msg_fatal ("CIRCE2: invalid format of data file.")
    case (-4)
       call msg_fatal ("CIRCE2: data file too large.")
    end select
    data%lumi = cir2lm (data%pdg(1), 0, data%pdg(2), 0)
    if (data%lumi == 0) then
       call circe2_data_write (data)
       call msg_fatal ("CIRCE2: luminosity vanishes for specified beams.")
    end if
    if (data%polarized) then
       h = 0
       do h1 = -1, 1, 2
          do h2 = -1, 1, 2
             data%lumi_hel_frac(h1,h2) = &
                  cir2lm (data%pdg(1), h1, data%pdg(2), h2) / data%lumi
             h = h + 1
             data%lumi_hel_sum(h) = &
                  data%lumi_hel_sum(h-1) + data%lumi_hel_frac(h1,h2)
          end do
       end do
       data%lumi_hel_sum(4) = 1
    end if
  end subroutine circe2_data_init

  subroutine circe2_data_write (data, unit)
    type(circe2_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    integer :: h1, h2
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, *) "CIRCE2 data:"
    write (u, *) "  file = ", char(data%file)
    write (u, *) "  design = ", char(data%design)
    write (u, *) "  sqrts = ", data%sqrts
    write (u, *) "  prt_in  = ", char (flavor_get_name (data%flv_in(1))), &
         ", ", char (flavor_get_name (data%flv_in(2)))    
    write (u, *) "  mass  = ", data%mass
    write (u, *) "  polarized = ", data%polarized
    write (u, *) "  luminosity = ", data%lumi
    if (data%polarized) then
       do h1 = -1, 1, 2
          do h2 = -1, 1, 2
             write (u, "(6x,'(',I2,1x,I2,')',1x,'=',1x)", advance="no") h1, h2
             write (u, *)  data%lumi_hel_frac(h1,h2)
          end do
       end do
    end if
    write (u, *) "  generate = ", data%generate
    write (u, *) "  map = ", data%map
    if (data%map) then
       write (u, *) "  mode = ", data%map_mode
       write (u, *) "  power = ", data%map_power
    end if
  end subroutine circe2_data_write

  subroutine interaction_init_circe2 (int, circe2_data)
    type(interaction_t), intent(out) :: int
    type(circe2_data_t), intent(in) :: circe2_data
    type(polarization_t) :: pol3, pol4
    real(default), dimension(2), parameter :: pol_init = 0.5_default
    logical, dimension(4) :: mask_h
    type(quantum_numbers_mask_t), dimension(4) :: mask
    type(quantum_numbers_t), dimension(4) :: qn_fc, qn_hel, qn
    type(state_iterator_t) :: it_hel3, it_hel4
    mask_h(1:2) = .true.
    mask_h(3:4) = .not. circe2_data%polarized
    mask = new_quantum_numbers_mask (.false., .false., mask_h)
    call quantum_numbers_init (qn_fc(1), flv = circe2_data%flv_in(1))
    call quantum_numbers_init (qn_fc(2), flv = circe2_data%flv_in(2))
    call quantum_numbers_init (qn_fc(3), flv = circe2_data%flv_in(1))
    call quantum_numbers_init (qn_fc(4), flv = circe2_data%flv_in(2))
    if (circe2_data%polarized) then
       call polarization_init_diagonal (pol3, circe2_data%flv_in(1), pol_init)
       call polarization_init_diagonal (pol4, circe2_data%flv_in(1), pol_init)
    else
       call polarization_init_unpolarized (pol3, circe2_data%flv_in(1))
       call polarization_init_unpolarized (pol4, circe2_data%flv_in(1))
    end if
    call interaction_init & 
         (int, 2, 0, 2, mask=mask, set_relations=.true.) 
    call state_iterator_init (it_hel3, pol3%state) 
    qn(1) = qn_fc(1)
    qn(2) = qn_fc(2)
    do while (state_iterator_is_valid (it_hel3)) 
       qn_hel(3:3) = state_iterator_get_quantum_numbers (it_hel3)
       qn(3) = qn_hel(3) .merge. qn_fc(3) 
       call state_iterator_init (it_hel4, pol4%state) 
       do while (state_iterator_is_valid (it_hel4)) 
          qn_hel(4:4) = state_iterator_get_quantum_numbers (it_hel4) 
          qn(4) = qn_hel(4) .merge. qn_fc(4) 
          call interaction_add_state (int, qn)
          call state_iterator_advance (it_hel4) 
       end do 
       call state_iterator_advance (it_hel3)
    end do 
    call polarization_final (pol3)
    call polarization_final (pol4)
    call interaction_freeze (int)     
  end subroutine interaction_init_circe2

  subroutine strfun (f, x, r, hel, data, no_map)
    real(default), intent(out) :: f
    real(default), dimension(2), intent(out) :: x
    real(default), dimension(2), intent(in) :: r
    real(default), dimension(2) :: fi
    integer, dimension(2), intent(in) :: hel
    double precision, dimension(2) :: xx
    type(circe2_data_t), intent(in) :: data
    logical, intent(in) :: no_map
    if (data%generate .and. .not. no_map) then
       rng_tmp => data%rng
       call cir2gn &
            (data%pdg(1), hel(1), data%pdg(2), hel(2), xx(1), xx(2), rn_sub)
       x = xx
       if (data%polarized) then
          f = 4
       else
          f = 1
       end if
    else
       if (no_map) then
          x = r
          fi = 1
       else
          call circe2_map (fi, x, r, data%map_mode, data%map_power)
       end if
       f = product (fi) &
            * cir2dn (data%pdg(1), hel(1), data%pdg(2), hel(2), &
                      dble (x(1)), dble (x(2)))
       if (data%polarized) then
          f = 4 * f * data%lumi_hel_frac (hel(1), hel(2))
       end if
    end if
  end subroutine strfun
  
  subroutine rn_sub (r)
    double precision, intent(out) :: r
    real(double) :: x
    call tao_random_number (rng_tmp, x)
    r = x
  end subroutine rn_sub

  elemental subroutine circe2_map (f, x, r, mode, power)
    real(default), intent(out) :: f, x
    real(default), intent(in) :: r
    integer, intent(in) :: mode
    real(default), intent(in) :: power
    real(default) :: xbar, rbar
    select case (mode)
    case (0)
       if (r <= tiny(1._default)) then
          x = 0
          f = 0
       else
          x = r ** power
          f = power * x / r
       end if
    case (1)
       rbar = 1 - r
       if (rbar <= tiny(1._default)) then
          xbar = 0
          f = 0
       else
          xbar = rbar ** power
          f = power * xbar / rbar
       end if
       x = 1 - xbar
    case default
       x = r
       f = 1
    end select
  end subroutine circe2_map

  subroutine interaction_apply_circe2 (int, r, circe2_data, no_map)
    type(interaction_t), intent(inout), target :: int
    real(default), dimension(2), intent(in) :: r
    type(circe2_data_t), intent(in) :: circe2_data
    logical, intent(in) :: no_map
    complex(default), parameter :: CZERO = 0
    type(vector4_t), dimension(2) :: k
    integer, dimension(2) :: h, h_gen, h_tmp
    type(state_iterator_t) :: it
    real(default) :: f
    real(default), dimension(2) :: x
    type(splitting_data_t), dimension(2) :: sd
    type(vector4_t), dimension(4) :: q
    k(1) = interaction_get_momentum (int, 1)
    k(2) = interaction_get_momentum (int, 2)
    if (circe2_data%generate) then
       call interaction_set_matrix_element (int, CZERO)
       if (circe2_data%polarized) then
          call circe2_select_hel &
               (h_gen, circe2_data%rng, circe2_data%lumi_hel_sum)
       else
          h_gen = 0
       end if
    end if
    call state_iterator_init (it, interaction_get_state_matrix_ptr (int))
    LOOP_HEL: do while (state_iterator_is_valid (it))
       if (circe2_data%polarized) then
          h_tmp = helicity_get (state_iterator_get_helicity (it, 3))
          h(1) = h_tmp(1)
          h_tmp = helicity_get (state_iterator_get_helicity (it, 4))
          h(2) = h_tmp(1)
       else
          h = 0
       end if
       if (.not. circe2_data%generate .or. all (h == h_gen)) then
          call strfun (f, x, r, h, circe2_data, no_map)
          call state_iterator_set_matrix_element (it, cmplx (f, kind=default))
       end if
       call state_iterator_advance (it)
    end do LOOP_HEL
    sd = new_splitting_data &
         (k, circe2_data%mass**2, 0._default, circe2_data%mass)
    call splitting_set_t_bounds (sd, x, 1-x)
    call splitting_set_collinear (sd)
    q((/1, 3/)) = split_momentum (k(1), sd(1))
    q((/2, 4/)) = split_momentum (k(2), sd(2))
    call interaction_set_momenta (int, q(3:4), outgoing=.true.)
  end subroutine interaction_apply_circe2

  subroutine circe2_select_hel (hel, rng, threshold)
    integer, dimension(2), intent(out) :: hel
    type(tao_random_state), pointer :: rng
    real(default), dimension(0:4) :: threshold
    real(double) :: r
    real(default) :: x
    integer :: h1, h2
    integer :: h
    call tao_random_number (rng, r)
    x = r
    h = 0
    do h1 = -1, 1, 2
       do h2 = -1, 1, 2
          h = h + 1
          if (x <= threshold(h)) then
             hel(1) = h1;  hel(2) = h2;  return
          end if
       end do
    end do
    call msg_bug ("CIRCE2: helicity selection failed")
  end subroutine circe2_select_hel


end module sf_circe2
