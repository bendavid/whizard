! WHIZARD 2.2.4 Feb 06 2015
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

module particles

  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
  use format_utils, only: write_separator
  use format_utils, only: pac_fmt
  use format_defs, only: FMT_16, FMT_17, FMT_19
  use unit_tests
  use diagnostics
  use lorentz
  use model_data
  use flavors
  use colors
  use helicities
  use quantum_numbers
  use state_matrices
  use interactions
  use evaluators
  use hepmc_interface
  use lcio_interface
  use subevents
  use polarizations

  implicit none
  private

  public :: particle_t
  public :: particle_write
  public :: particle_reset_status
  public :: particle_set_color
  public :: particle_set_flavor
  public :: particle_set_model
  public :: particle_set_momentum
  public :: particle_set_children
  public :: particle_set_parents
  public :: particle_get_status
  public :: particle_get_polarization_status
  public :: particle_get_pdg
  public :: particle_get_color
  public :: particle_get_polarization
  public :: particle_get_helicity
  public :: particle_get_n_parents
  public :: particle_get_n_children
  public :: particle_get_parents
  public :: particle_get_children
  public :: particle_get_momentum
  public :: particle_get_p2
  public :: particle_set_t
  public :: particle_set_init
  public :: particle_set_set_model
  public :: particle_set_final
  public :: particle_set_write
  public :: particle_set_write_raw 
  public :: particle_set_read_raw 
  public :: hepmc_event_from_particle_set 
  public :: lcio_event_from_particle_set 
  public :: particle_set_get_n_beam
  public :: particle_set_get_n_in
  public :: particle_set_get_n_vir
  public :: particle_set_get_n_out
  public :: particle_set_get_n_tot
  public :: particle_set_get_particle
  public :: particle_set_reset_status
  public :: particle_set_reduce
  public :: particle_set_apply_keep_beams
  public :: particle_set_to_hepevt_form
  public :: particle_set_fill_interaction
  public :: particle_set_assign_vertices
  public :: particle_set_to_subevt
  public :: particle_set_replace
  public :: pacify
  public :: particles_test

  integer, parameter, public :: PRT_UNPOLARIZED = 0
  integer, parameter, public :: PRT_DEFINITE_HELICITY = 1
  integer, parameter, public :: PRT_GENERIC_POLARIZATION = 2

       
  type :: particle_t
     private
     integer :: status = PRT_UNDEFINED
     integer :: polarization = PRT_UNPOLARIZED
     type(flavor_t) :: flv
     type(color_t) :: col
     type(helicity_t) :: hel
     type(polarization_t) :: pol
     type(vector4_t) :: p = vector4_null
     real(default) :: p2 = 0
     integer, dimension(:), allocatable :: parent
     integer, dimension(:), allocatable :: child
  end type particle_t

  type :: particle_set_t
     ! private
     integer :: n_beam = 0
     integer :: n_in  = 0
     integer :: n_vir = 0
     integer :: n_out = 0
     integer :: n_tot = 0
     type(particle_t), dimension(:), allocatable :: prt
     type(state_matrix_t) :: correlated_state
  end type particle_set_t


  interface particle_init
     module procedure particle_init_particle
     module procedure particle_init_state
     module procedure particle_init_hepmc
     module procedure particle_init_lcio
  end interface

  interface particle_set_init
     module procedure particle_set_init_interaction
     module procedure particle_set_init_hepmc
     module procedure particle_set_init_lcio
  end interface

  interface pacify
     module procedure pacify_particle
     module procedure pacify_particle_set
  end interface pacify


contains

  subroutine particle_init_particle (prt_out, prt_in)
    type(particle_t), intent(out) :: prt_out
    type(particle_t), intent(in) :: prt_in
    prt_out%status = prt_in%status
    prt_out%polarization = prt_in%polarization
    prt_out%flv = prt_in%flv
    prt_out%col = prt_in%col
    prt_out%hel = prt_in%hel
    prt_out%pol = prt_in%pol
    prt_out%p = prt_in%p
    prt_out%p2 = prt_in%p2
  end subroutine particle_init_particle

  subroutine particle_init_state (prt, state, status, mode)
    type(particle_t), intent(out) :: prt
    type(state_matrix_t), intent(in) :: state
    integer, intent(in) :: status, mode
    type(state_iterator_t) :: it
    prt%status = status
    call state_iterator_init (it, state)
    prt%flv = state_iterator_get_flavor (it, 1)
    if (flavor_is_beam_remnant (prt%flv))  prt%status = PRT_BEAM_REMNANT
    prt%col = state_iterator_get_color (it, 1)
    select case (mode)
    case (FM_SELECT_HELICITY)
       prt%hel = state_iterator_get_helicity (it, 1)
       prt%polarization = PRT_DEFINITE_HELICITY
    case (FM_FACTOR_HELICITY)
       call polarization_init_state_matrix (prt%pol, state)
       prt%polarization = PRT_GENERIC_POLARIZATION
    end select
  end subroutine particle_init_state

  subroutine particle_init_hepmc (prt, hprt, model, polarization, barcode)
    type(particle_t), intent(out) :: prt
    type(hepmc_particle_t), intent(in) :: hprt
    type(model_data_t), intent(in), target :: model
    integer, intent(in) :: polarization
    integer, dimension(:), intent(in) :: barcode
    type(hepmc_polarization_t) :: hpol
    integer :: n_parents, n_children
    integer, dimension(:), allocatable :: parent_barcode, child_barcode
    integer :: i
    select case (hepmc_particle_get_status (hprt))
    case (1);  prt%status = PRT_OUTGOING
    case (2);  prt%status = PRT_RESONANT
    case (3);  prt%status = PRT_VIRTUAL
    end select
    if (hepmc_particle_is_beam (hprt)) prt%status = PRT_BEAM
    call flavor_init (prt%flv, hepmc_particle_get_pdg (hprt), model)
    if (flavor_is_beam_remnant (prt%flv))  prt%status = PRT_BEAM_REMNANT
    call color_init (prt%col, hepmc_particle_get_color (hprt))
    prt%polarization = polarization
    select case (polarization)
    case (PRT_DEFINITE_HELICITY)
       hpol = hepmc_particle_get_polarization (hprt)
       call hepmc_polarization_to_hel (hpol, prt%flv, prt%hel)
       call hepmc_polarization_final (hpol)
    case (PRT_GENERIC_POLARIZATION)
       hpol = hepmc_particle_get_polarization (hprt)
       call hepmc_polarization_to_pol (hpol, prt%flv, prt%pol)
       call hepmc_polarization_final (hpol)
    end select
    prt%p = hepmc_particle_get_momentum (hprt)
    prt%p2 = hepmc_particle_get_mass_squared (hprt)
    n_parents  = hepmc_particle_get_n_parents  (hprt)
    n_children = hepmc_particle_get_n_children (hprt)
    allocate (parent_barcode (n_parents),  prt%parent (n_parents))
    allocate (child_barcode  (n_children), prt%child  (n_children))
    parent_barcode = hepmc_particle_get_parent_barcodes (hprt)
    child_barcode  = hepmc_particle_get_child_barcodes  (hprt)
    do i = 1, size (barcode)
       where (parent_barcode == barcode(i))  prt%parent = i
       where (child_barcode  == barcode(i))  prt%child  = i
    end do
    if (prt%status == PRT_VIRTUAL .and. n_parents == 0) &
         prt%status = PRT_INCOMING
  end subroutine particle_init_hepmc

  subroutine particle_init_lcio (prt, lprt, model, daughters, parents, polarization)
    type(particle_t), intent(out) :: prt
    type(lcio_particle_t), intent(in) :: lprt
    type(model_data_t), intent(in), target :: model
    integer, intent(in) :: polarization
    integer :: n_parents, n_daughters
    integer, dimension(:) :: daughters, parents
    integer :: i
    select case (lcio_particle_get_status (lprt))
    case (1);  prt%status = PRT_OUTGOING
    case (2);  prt%status = PRT_RESONANT
    case (3);  prt%status = PRT_VIRTUAL
    end select
    !!! if (hepmc_particle_is_beam (hprt)) prt%status = PRT_BEAM
    call flavor_init (prt%flv, lcio_particle_get_pdg (lprt), model)
    if (flavor_is_beam_remnant (prt%flv))  prt%status = PRT_BEAM_REMNANT
    call color_init (prt%col, lcio_particle_get_flow (lprt))
    prt%polarization = polarization
    select case (polarization)
    case (PRT_DEFINITE_HELICITY)
       call lcio_particle_to_hel (lprt, prt%flv, prt%hel)
    case (PRT_GENERIC_POLARIZATION)
       call lcio_particle_to_pol (lprt, prt%flv, prt%pol)
    end select
    prt%p = lcio_particle_get_momentum (lprt)
    prt%p2 = lcio_particle_get_mass_squared (lprt)
    n_parents  = lcio_particle_get_n_parents (lprt)
    n_daughters  = lcio_particle_get_n_children (lprt)    
    allocate (prt%parent (n_parents))
    prt%parent = parents
    allocate (prt%child (n_daughters))
    prt%child = daughters
    if (prt%status == PRT_VIRTUAL .and. n_parents == 0) &
         prt%status = PRT_INCOMING
  end subroutine particle_init_lcio

  elemental subroutine particle_final (prt)
    type(particle_t), intent(inout) :: prt
    call polarization_final (prt%pol)
  end subroutine particle_final

  subroutine particle_write (prt, unit, testflag)
    type(particle_t), intent(in) :: prt
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    logical :: pacified
    integer :: u
    real(default) :: pp2
    character(len=7) :: fmt
    pacified = .false.
    if (present (testflag))  pacified = testflag
    call pac_fmt (fmt, FMT_19, FMT_16, testflag)
    u = given_output_unit (unit);  if (u < 0)  return
    select case (prt%status)
    case (PRT_UNDEFINED);    write (u, "(1x, A)", advance="no")  "[-]"
    case (PRT_BEAM);         write (u, "(1x, A)", advance="no")  "[b]"
    case (PRT_INCOMING);     write (u, "(1x, A)", advance="no")  "[i]"
    case (PRT_OUTGOING);     write (u, "(1x, A)", advance="no")  "[o]"
    case (PRT_VIRTUAL);      write (u, "(1x, A)", advance="no")  "[v]"
    case (PRT_RESONANT);     write (u, "(1x, A)", advance="no")  "[r]"
    case (PRT_BEAM_REMNANT); write (u, "(1x, A)", advance="no")  "[x]"
    end select
    write (u, "(1x)", advance="no")
    call flavor_write (prt%flv, unit)
    call color_write (prt%col, unit)
    select case (prt%polarization)
    case (PRT_DEFINITE_HELICITY)
       call helicity_write (prt%hel, unit)
       write (u, *)
    case (PRT_GENERIC_POLARIZATION)
       write (u, *)
       call polarization_write (prt%pol, unit)
    case default
       write (u, *)
    end select    
    call vector4_write (prt%p, unit, testflag = testflag)
    pp2 = prt%p2        
    if (pacified)  call pacify (pp2, tolerance = 1E-10_default) 
    write (u, "(1x,A,1x," // fmt // ")")  "T = ", pp2
    if (allocated (prt%parent)) then
       if (size (prt%parent) /= 0) then
          write (u, "(1x,A,40(1x,I0))")  "Parents: ", prt%parent
       end if
    end if
    if (allocated (prt%child)) then
       if (size (prt%child) /= 0) then
          write (u, "(1x,A,40(1x,I0))")  "Children:", prt%child
       end if
    end if
  end subroutine particle_write

  subroutine particle_write_raw (prt, u)
    type(particle_t), intent(in) :: prt
    integer, intent(in) :: u
    write (u) prt%status, prt%polarization
    call flavor_write_raw (prt%flv, u)
    call color_write_raw (prt%col, u)
    select case (prt%polarization)
    case (PRT_DEFINITE_HELICITY)
       call helicity_write_raw (prt%hel, u)
    case (PRT_GENERIC_POLARIZATION)
       call polarization_write_raw (prt%pol, u)
    end select
    call vector4_write_raw (prt%p, u)
    write (u) prt%p2
    write (u) allocated (prt%parent)
    if (allocated (prt%parent)) then
       write (u) size (prt%parent)
       write (u) prt%parent
    end if
    write (u) allocated (prt%child)
    if (allocated (prt%child)) then
       write (u) size (prt%child)
       write (u) prt%child
    end if
  end subroutine particle_write_raw

  subroutine particle_read_raw (prt, u, iostat)
    type(particle_t), intent(out) :: prt
    integer, intent(in) :: u
    integer, intent(out) :: iostat
    logical :: allocated_parent, allocated_child
    integer :: size_parent, size_child
    read (u, iostat=iostat) prt%status, prt%polarization
    call flavor_read_raw (prt%flv, u, iostat=iostat)
    call color_read_raw (prt%col, u, iostat=iostat)
    select case (prt%polarization)
    case (PRT_DEFINITE_HELICITY)
       call helicity_read_raw (prt%hel, u, iostat=iostat)
    case (PRT_GENERIC_POLARIZATION)
       call polarization_read_raw (prt%pol, u, iostat=iostat)
    end select
    call vector4_read_raw (prt%p, u, iostat=iostat)
    read (u, iostat=iostat) prt%p2
    read (u, iostat=iostat) allocated_parent
    if (allocated_parent) then
       read (u, iostat=iostat) size_parent
       allocate (prt%parent (size_parent))
       read (u, iostat=iostat) prt%parent
    end if
    read (u, iostat=iostat) allocated_child
    if (allocated_child) then
       read (u, iostat=iostat) size_child
       allocate (prt%child (size_child))
       read (u, iostat=iostat) prt%child
    end if
  end subroutine particle_read_raw

  elemental subroutine particle_reset_status (prt, status)
    type(particle_t), intent(inout) :: prt
    integer, intent(in) :: status
    prt%status = status
    select case (status)
    case (PRT_BEAM, PRT_INCOMING, PRT_OUTGOING)
       prt%p2 = flavor_get_mass (prt%flv) ** 2
    end select
  end subroutine particle_reset_status

  elemental subroutine particle_set_color (prt, col)
    type(particle_t), intent(inout) :: prt
    type(color_t), intent(in) :: col
    prt%col = col
  end subroutine particle_set_color

  subroutine particle_set_flavor (prt, flv)
    type(particle_t), intent(inout) :: prt
    type(flavor_t), intent(in) :: flv
    prt%flv = flv
  end subroutine particle_set_flavor

  subroutine particle_set_model (prt, model)
    type(particle_t), intent(inout) :: prt
    class(model_data_t), intent(in), target :: model
    call flavor_set_model (prt%flv, model)
  end subroutine particle_set_model
  
  elemental subroutine particle_set_momentum (prt, p, p2, on_shell)
    type(particle_t), intent(inout) :: prt
    type(vector4_t), intent(in) :: p
    real(default), intent(in), optional :: p2
    logical, intent(in), optional :: on_shell
    prt%p = p
    if (present (on_shell)) then
       if (on_shell) then
          if (flavor_is_associated (prt%flv)) then
             prt%p2 = flavor_get_mass (prt%flv) ** 2
             return
          end if
       end if
    end if
    if (present (p2)) then
       prt%p2 = p2
    else
       prt%p2 = p ** 2
    end if
  end subroutine particle_set_momentum

  elemental subroutine particle_set_resonance_flag (prt, resonant)
    type(particle_t), intent(inout) :: prt
    logical, intent(in) :: resonant
    select case (prt%status)
    case (PRT_VIRTUAL)
       if (resonant)  prt%status = PRT_RESONANT
    end select
  end subroutine particle_set_resonance_flag

  subroutine particle_set_children (prt, idx)
    type(particle_t), intent(inout) :: prt
    integer, dimension(:), intent(in) :: idx
    if (allocated (prt%child))  deallocate (prt%child)
    allocate (prt%child (count (idx /= 0)))
    prt%child = pack (idx, idx /= 0)
  end subroutine particle_set_children

  subroutine particle_set_parents (prt, idx)
    type(particle_t), intent(inout) :: prt
    integer, dimension(:), intent(in) :: idx
    if (allocated (prt%parent))  deallocate (prt%parent) 
    allocate (prt%parent (count (idx /= 0)))
    prt%parent = pack (idx, idx /= 0)
  end subroutine particle_set_parents

  elemental function particle_get_status (prt) result (status)
    integer :: status
    type(particle_t), intent(in) :: prt
    status = prt%status
  end function particle_get_status

  elemental function particle_is_real (prt, keep_beams) result (flag)
    logical :: flag, kb
    type(particle_t), intent(in) :: prt
    logical, intent(in), optional :: keep_beams
    kb = .false.
    if (present (keep_beams)) kb = keep_beams
    select case (prt%status)
    case (PRT_INCOMING, PRT_OUTGOING, PRT_RESONANT)
       flag = .true.
    case (PRT_BEAM)
       flag = kb 
    case default
       flag = .false.
    end select
  end function particle_is_real

  elemental function particle_get_polarization_status (prt) result (status)
    integer :: status
    type(particle_t), intent(in) :: prt
    status = prt%polarization
  end function particle_get_polarization_status

  elemental function particle_get_pdg (prt) result (pdg)
    integer :: pdg
    type(particle_t), intent(in) :: prt
    pdg = flavor_get_pdg (prt%flv)
  end function particle_get_pdg

  pure function particle_get_color (prt) result (col)
    integer, dimension(2) :: col
    type(particle_t), intent(in) :: prt
    col(1) = color_get_col (prt%col)
    col(2) = color_get_acl (prt%col)
  end function particle_get_color
  
  function particle_get_polarization (prt) result (pol)
    type(polarization_t) :: pol
    type(particle_t), intent(in) :: prt
    pol = prt%pol
  end function particle_get_polarization

  elemental function particle_get_helicity (prt) result (hel)
    integer :: hel
    integer, dimension(2) :: hel_arr
    type(particle_t), intent(in) :: prt    
    hel = 0
    if (helicity_is_defined (prt%hel) .and. &
        helicity_is_diagonal (prt%hel)) then
             hel_arr = helicity_get (prt%hel)
             hel = hel_arr (1)
    end if
  end function particle_get_helicity  
  
  elemental function particle_get_n_parents (prt) result (n)
    integer :: n
    type(particle_t), intent(in) :: prt
    if (allocated (prt%parent)) then
       n = size (prt%parent)
    else
       n = 0
    end if
  end function particle_get_n_parents
    
  elemental function particle_get_n_children (prt) result (n)
    integer :: n
    type(particle_t), intent(in) :: prt
    if (allocated (prt%child)) then
       n = size (prt%child)
    else
       n = 0
    end if
  end function particle_get_n_children
    
  function particle_get_parents (prt) result (parent)
    type(particle_t), intent(in) :: prt
    integer, dimension(:), allocatable :: parent
    if (allocated (prt%parent)) then
       allocate (parent (size (prt%parent)))
       parent = prt%parent
    else
       allocate (parent (0))
    end if
  end function particle_get_parents

  function particle_get_children (prt) result (child)
    type(particle_t), intent(in) :: prt
    integer, dimension(:), allocatable :: child
    if (allocated (prt%child)) then
       allocate (child (size (prt%child)))
       child = prt%child
    else
       allocate (child (0))
    end if
  end function particle_get_children

  elemental function particle_get_momentum (prt) result (p)
    type(vector4_t) :: p
    type(particle_t), intent(in) :: prt
    p = prt%p
  end function particle_get_momentum

  elemental function particle_get_p2 (prt) result (p2)
    real(default) :: p2
    type(particle_t), intent(in) :: prt
    p2 = prt%p2
  end function particle_get_p2

  subroutine particle_set_init_interaction &
       (particle_set, is_valid, int, int_flows, mode, x, &
        keep_correlations, keep_virtual, n_incoming)
    type(particle_set_t), intent(out) :: particle_set
    logical, intent(out) :: is_valid
    type(interaction_t), intent(in), target :: int, int_flows
    integer, intent(in) :: mode
    real(default), dimension(2), intent(in) :: x
    logical, intent(in) :: keep_correlations, keep_virtual
    integer, intent(in), optional :: n_incoming
    type(state_matrix_t), dimension(:), allocatable, target :: flavor_state
    type(state_matrix_t), dimension(:), allocatable, target :: single_state
    integer :: n_in, n_vir, n_out, n_tot
    type(quantum_numbers_t), dimension(:,:), allocatable :: qn
    logical :: ok
    integer :: i, j
    if (present (n_incoming)) then
       n_in  = n_incoming
       n_vir = interaction_get_n_vir (int) - n_incoming
    else
       n_in  = interaction_get_n_in  (int)
       n_vir = interaction_get_n_vir (int)
    end if
    n_out = interaction_get_n_out (int)
    n_tot = interaction_get_n_tot (int)
    particle_set%n_in  = n_in
    particle_set%n_out = n_out
    if (keep_virtual) then
       particle_set%n_vir = n_vir
       particle_set%n_tot = n_tot
    else
       particle_set%n_vir = 0
       particle_set%n_tot = n_in + n_out
    end if
    call interaction_factorize &
         (int, FM_IGNORE_HELICITY, x(1), is_valid, flavor_state)
    allocate (qn (n_tot,1))
    do i = 1, n_tot
       qn(i,:) = state_matrix_get_quantum_numbers (flavor_state(i), 1)
    end do
    if (keep_correlations .and. keep_virtual) then
       call interaction_factorize (int_flows, mode, x(2), ok, &
            single_state, particle_set%correlated_state, qn(:,1))
    else
       call interaction_factorize (int_flows, mode, x(2), ok, &
            single_state, qn_in=qn(:,1))
    end if
    is_valid = is_valid .and. ok
    allocate (particle_set%prt (particle_set%n_tot))
    j = 1
    do i = 1, n_tot
       if (i <= n_in) then
          call particle_init &
               (particle_set%prt(j), single_state(i), PRT_INCOMING, mode)
          call particle_set_momentum &
               (particle_set%prt(j), interaction_get_momentum (int, i))
       else if (i <= n_in + n_vir) then
          if (.not. keep_virtual)  cycle
          call particle_init &
               (particle_set%prt(j), single_state(i), PRT_VIRTUAL, mode) 
          call particle_set_momentum &
               (particle_set%prt(j), interaction_get_momentum (int, i))
       else
          call particle_init &
               (particle_set%prt(j), single_state(i), PRT_OUTGOING, mode)
          call particle_set_momentum &
               (particle_set%prt(j), interaction_get_momentum (int, i), &
               on_shell = .true.)
       end if
       if (keep_virtual) then
          call particle_set_children &
               (particle_set%prt(j), interaction_get_children (int, i))
          call particle_set_parents &
               (particle_set%prt(j), interaction_get_parents (int, i))
       end if
       j = j + 1
    end do
    if (keep_virtual) then
       call particle_set_resonance_flag &
            (particle_set%prt, interaction_get_resonance_flags (int))
    end if
    call state_matrix_final (flavor_state)
    call state_matrix_final (single_state)
  end subroutine particle_set_init_interaction

  subroutine particle_set_init_hepmc &
       (particle_set, evt, model, fallback_model, polarization)
    type(particle_set_t), intent(inout), target :: particle_set
    type(hepmc_event_t), intent(in) :: evt
    class(model_data_t), intent(in), target :: model, fallback_model
    integer, intent(in) :: polarization
    type(hepmc_event_particle_iterator_t) :: it
    type(hepmc_particle_t) :: prt
    integer, dimension(:), allocatable :: barcode
    integer :: n_tot, i, j
    n_tot = 0
    call hepmc_event_particle_iterator_init (it, evt)
    do while (hepmc_event_particle_iterator_is_valid (it))
       n_tot = n_tot + 1
       call hepmc_event_particle_iterator_advance (it)
    end do
    allocate (barcode (n_tot))
    call hepmc_event_particle_iterator_reset (it)
    do i = 1, n_tot
       barcode(i) = hepmc_particle_get_barcode &
            (hepmc_event_particle_iterator_get (it))
       call hepmc_event_particle_iterator_advance (it)
    end do
    allocate (particle_set%prt (n_tot))
    call hepmc_event_particle_iterator_reset (it)
    do i = 1, n_tot
       prt = hepmc_event_particle_iterator_get (it)
       call particle_init (particle_set%prt(i), &
            prt, model, polarization, barcode)
       call hepmc_event_particle_iterator_advance (it)
    end do
    do i = 1, n_tot
       if (particle_get_status (particle_set%prt(i)) == PRT_VIRTUAL) then
          CHECK_BEAM: do j = 1, size (particle_set%prt(i)%parent)
             if (particle_get_status (particle_set%prt(j)) == PRT_BEAM) &
                  particle_set%prt(i)%status = PRT_INCOMING
             exit CHECK_BEAM
          end do CHECK_BEAM
       end if
    end do
    call hepmc_event_particle_iterator_final (it)
    particle_set%n_tot = n_tot
    particle_set%n_beam  = &
         count (particle_get_status (particle_set%prt) == PRT_BEAM)
    particle_set%n_in  = &
         count (particle_get_status (particle_set%prt) == PRT_INCOMING)
    particle_set%n_out = &
         count (particle_get_status (particle_set%prt) == PRT_OUTGOING)
    particle_set%n_vir = &
         particle_set%n_tot - particle_set%n_in - particle_set%n_out
  end subroutine particle_set_init_hepmc

  subroutine particle_set_init_lcio &
       (particle_set, evt, model, fallback_model, polarization)
    type(particle_set_t), intent(inout), target :: particle_set
    type(lcio_event_t), intent(in) :: evt
    class(model_data_t), intent(in), target :: model, fallback_model
    integer, intent(in) :: polarization
    type(lcio_particle_t) :: prt
    integer, dimension(:), allocatable :: parents, daughters
    integer :: n_tot, i, j, n_parents, n_children
    n_tot = lcio_event_get_n_tot (evt)
    allocate (particle_set%prt (n_tot))
    do i = 1, n_tot
       prt = lcio_event_get_particle (evt, i-1)
       n_parents = lcio_particle_get_n_parents (prt)
       n_children = lcio_particle_get_n_children (prt) 
       allocate (daughters (n_children))
       allocate (parents (n_parents))
       if (n_children > 0) then
          do j = 1, n_children
             daughters(j) = lcio_get_n_children (evt,i,j)          
          end do
       end if
       if (n_parents > 0) then
          do j = 1, n_parents
             parents(j) = lcio_get_n_parents (evt,i,j)
          end do
       end if
       call particle_init (particle_set%prt(i), prt, model, &
            daughters, parents, polarization)
       deallocate (daughters, parents)
    end do
    do i = 1, n_tot
       if (particle_get_status (particle_set%prt(i)) == PRT_VIRTUAL) then
          CHECK_BEAM: do j = 1, size (particle_set%prt(i)%parent)
             if (particle_get_status (particle_set%prt(j)) == PRT_BEAM) &
                  particle_set%prt(i)%status = PRT_INCOMING
             exit CHECK_BEAM
          end do CHECK_BEAM
       end if
    end do
    particle_set%n_tot = n_tot
    particle_set%n_beam  = &
         count (particle_get_status (particle_set%prt) == PRT_BEAM)
    particle_set%n_in  = &
         count (particle_get_status (particle_set%prt) == PRT_INCOMING)
    particle_set%n_out = &
         count (particle_get_status (particle_set%prt) == PRT_OUTGOING)
    particle_set%n_vir = &
         particle_set%n_tot - particle_set%n_in - particle_set%n_out
  end subroutine particle_set_init_lcio

  subroutine particle_set_set_model (particle_set, model)
    type(particle_set_t), intent(inout) :: particle_set
    class(model_data_t), intent(in), target :: model
    integer :: i
    do i = 1, particle_set%n_tot
       call particle_set_model (particle_set%prt(i), model)
    end do
    call state_matrix_set_model (particle_set%correlated_state, model)
  end subroutine particle_set_set_model
    
  subroutine particle_set_final (particle_set)
    type(particle_set_t), intent(inout) :: particle_set
    if (allocated (particle_set%prt)) then
       call particle_final (particle_set%prt)
       deallocate(particle_set%prt)
    end if
    call state_matrix_final (particle_set%correlated_state)
  end subroutine particle_set_final

  subroutine particle_set_write (particle_set, unit, testflag)
    type(particle_set_t), intent(in) :: particle_set
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A)") "Particle set:"
    call write_separator (u)
    if (particle_set%n_tot /= 0) then
       do i = 1, particle_set%n_tot
          write (u, "(1x,A,1x,I0)", advance="no") "Particle", i
          call particle_write (particle_set%prt(i), u, testflag)
       end do
       if (state_matrix_is_defined (particle_set%correlated_state)) then
          call write_separator (u)
          write (u, *) "Correlated state density matrix:"
          call state_matrix_write (particle_set%correlated_state, u)
       end if
    else
       write (u, "(3x,A)") "[empty]"
    end if
  end subroutine particle_set_write

  subroutine particle_set_write_raw (particle_set, u)
    type(particle_set_t), intent(in) :: particle_set
    integer, intent(in) :: u
    integer :: i
    write (u) &
         particle_set%n_beam, particle_set%n_in, &
         particle_set%n_vir, particle_set%n_out
    write (u) particle_set%n_tot
    do i = 1, particle_set%n_tot
       call particle_write_raw (particle_set%prt(i), u)
    end do
    call state_matrix_write_raw (particle_set%correlated_state, u)
  end subroutine particle_set_write_raw

  subroutine particle_set_read_raw (particle_set, u, iostat)
    type(particle_set_t), intent(out) :: particle_set
    integer, intent(in) :: u
    integer, intent(out) :: iostat
    integer :: i
    read (u, iostat=iostat) &
         particle_set%n_beam, particle_set%n_in, &
         particle_set%n_vir, particle_set%n_out
    read (u, iostat=iostat) particle_set%n_tot
    allocate (particle_set%prt (particle_set%n_tot))
    do i = 1, size (particle_set%prt)
       call particle_read_raw (particle_set%prt(i), u, iostat=iostat)
    end do
    call state_matrix_read_raw (particle_set%correlated_state, u, iostat=iostat)
  end subroutine particle_set_read_raw

  subroutine particle_to_hepmc (prt, hprt)
    type(particle_t), intent(in) :: prt
    type(hepmc_particle_t), intent(out) :: hprt
    integer :: hepmc_status
    select case (particle_get_status (prt))
    case (PRT_UNDEFINED)
       hepmc_status = 0
    case (PRT_OUTGOING)
       hepmc_status = 1
    case (PRT_BEAM)
       hepmc_status = 4
    case (PRT_RESONANT)
       if(abs(particle_get_pdg(prt)) == 13 .or. &
            abs(particle_get_pdg(prt)) == 15) then
          hepmc_status = 2
       else
          hepmc_status = 11
       end if
    case default
       hepmc_status = 3
    end select
    call hepmc_particle_init (hprt, &
         particle_get_momentum (prt), &
         particle_get_pdg (prt), &
         hepmc_status)
    call hepmc_particle_set_color (hprt, &
         particle_get_color (prt))
    select case (particle_get_polarization_status (prt))
    case (PRT_DEFINITE_HELICITY)
       call hepmc_particle_set_polarization (hprt, &
            particle_get_helicity (prt))
    case (PRT_GENERIC_POLARIZATION)
       call hepmc_particle_set_polarization (hprt, &
            particle_get_polarization (prt))
    end select
  end subroutine particle_to_hepmc

  subroutine hepmc_event_from_particle_set (evt, particle_set)
    type(hepmc_event_t), intent(inout) :: evt
    type(particle_set_t), intent(in) :: particle_set
    type(hepmc_vertex_t), dimension(:), allocatable :: v
    type(hepmc_particle_t), dimension(:), allocatable :: hprt
    type(hepmc_particle_t), dimension(2) :: hbeam
    logical, dimension(:), allocatable :: is_beam
    integer, dimension(:), allocatable :: v_from, v_to
    integer :: n_vertices, n_tot, i
    n_tot = particle_set%n_tot
    allocate (v_from (n_tot), v_to (n_tot))
    call particle_set_assign_vertices (particle_set, v_from, v_to, n_vertices)
    allocate (v (n_vertices))
    do i = 1, n_vertices
       call hepmc_vertex_init (v(i))
       call hepmc_event_add_vertex (evt, v(i))
    end do
    allocate (hprt (n_tot))
    do i = 1, n_tot
       if (v_to(i) /= 0 .or. v_from(i) /= 0) then
          call particle_to_hepmc (particle_set%prt(i), hprt(i))
       end if
    end do
    allocate (is_beam (n_tot))
    is_beam = particle_get_status (particle_set%prt(1:n_tot)) == PRT_BEAM
    if (.not. any (is_beam)) then
       is_beam = particle_get_status (particle_set%prt(1:n_tot)) == PRT_INCOMING
    end if
    if (count (is_beam) == 2) then
       hbeam = pack (hprt, is_beam)
       call hepmc_event_set_beam_particles (evt, hbeam(1), hbeam(2))
    end if
    do i = 1, n_tot
       if (v_to(i) /= 0) then
          call hepmc_vertex_add_particle_in (v(v_to(i)), hprt(i))
       end if
    end do
    do i = 1, n_tot
       if (v_from(i) /= 0) then
          call hepmc_vertex_add_particle_out (v(v_from(i)), hprt(i))
       end if
    end do
  end subroutine hepmc_event_from_particle_set
  
  subroutine particle_to_lcio (prt, lprt)
    type(particle_t), intent(in) :: prt
    type(lcio_particle_t), intent(out) :: lprt
    integer :: lcio_status
    select case (particle_get_status (prt))
    case (PRT_UNDEFINED)
       lcio_status = 0
    case (PRT_OUTGOING)
       lcio_status = 1
    case (PRT_BEAM)
       lcio_status = 4
    case (PRT_RESONANT)
       if(abs(particle_get_pdg(prt)) == 13 .or. &
            abs(particle_get_pdg(prt)) == 15) then
          lcio_status = 2
       else
          lcio_status = 11
       end if
    case default
       lcio_status = 3
    end select
    call lcio_particle_init (lprt, &
         particle_get_momentum (prt), &
         particle_get_pdg (prt), &
         lcio_status)
    call lcio_particle_set_color (lprt, &
         particle_get_color (prt))
    select case (particle_get_polarization_status (prt))
    case (PRT_DEFINITE_HELICITY)
       call lcio_polarization_init (lprt, &
            particle_get_helicity (prt))
    case (PRT_GENERIC_POLARIZATION)
       call lcio_polarization_init (lprt, &
            particle_get_polarization (prt))
    end select
  end subroutine particle_to_lcio

  subroutine lcio_event_from_particle_set (evt, particle_set)
    type(lcio_event_t), intent(inout) :: evt
    type(particle_set_t), intent(in) :: particle_set
    type(lcio_particle_t), dimension(:), allocatable :: lprt
    type(lcio_particle_t), dimension(2) :: lbeam
    integer, dimension(:), allocatable :: parent, child    
    logical, dimension(:), allocatable :: is_beam
    integer :: n_tot, i, j, n_parents
    n_tot = particle_set%n_tot
    allocate (lprt (n_tot))
    do i = 1, n_tot
       call particle_to_lcio (particle_set%prt(i), lprt(i))
       n_parents = particle_get_n_parents (particle_set%prt(i))
       if (n_parents /= 0) then
          allocate (parent (n_parents))
          parent = particle_get_parents (particle_set%prt(i))
          do j = 1, n_parents
             call lcio_particle_set_parent (lprt(i), lprt(parent(j)))
          end do
          deallocate (parent)
       end if
       call lcio_particle_add_to_evt_coll (lprt(i), evt)
    end do
    call lcio_event_add_coll (evt)
  end subroutine lcio_event_from_particle_set
  
  function particle_set_get_real_parents (pset, i, keep_beams) result (parent)
    integer, dimension(:), allocatable :: parent
    type(particle_set_t), intent(in) :: pset
    integer, intent(in) :: i
    logical, intent(in), optional :: keep_beams
    logical, dimension(:), allocatable :: is_real
    logical, dimension(:), allocatable :: is_parent, is_real_parent
    logical :: kb
    integer :: j, k
    kb = .false.
    if (present (keep_beams)) kb = keep_beams
    allocate (is_real (pset%n_tot))
    is_real = particle_is_real (pset%prt, kb)
    allocate (is_parent (pset%n_tot), is_real_parent (pset%n_tot))
    is_real_parent = .false.
    is_parent = .false.
    is_parent(particle_get_parents(pset%prt(i))) = .true.
    do while (any (is_parent))
       where (is_real .and. is_parent)
          is_real_parent = .true.
          is_parent = .false.
       end where
       mark_next_parent: do j = size (is_parent), 1, -1
          if (is_parent(j)) then
             is_parent(particle_get_parents(pset%prt(j))) = .true.
             is_parent(j) = .false.
             exit mark_next_parent
          end if
       end do mark_next_parent
    end do
    allocate (parent (count (is_real_parent)))
    j = 0
    do k = 1, size (is_parent)
       if (is_real_parent(k)) then
          j = j + 1
          parent(j) = k
       end if
    end do
  end function particle_set_get_real_parents

  function particle_set_get_real_children (pset, i, keep_beams) result (child)
    integer, dimension(:), allocatable :: child
    type(particle_set_t), intent(in) :: pset
    integer, intent(in) :: i
    logical, dimension(:), allocatable :: is_real
    logical, dimension(:), allocatable :: is_child, is_real_child
    logical, intent(in), optional :: keep_beams
    integer :: j, k
    logical :: kb
    kb = .false.
    if (present (keep_beams)) kb = keep_beams
    allocate (is_real (pset%n_tot))
    is_real = particle_is_real (pset%prt, kb)
    allocate (is_child (pset%n_tot), is_real_child (pset%n_tot))
    is_real_child = .false.
    is_child = .false.
    is_child(particle_get_children(pset%prt(i))) = .true.
    do while (any (is_child))
       where (is_real .and. is_child)
          is_real_child = .true.
          is_child = .false.
       end where
       mark_next_child: do j = 1, size (is_child)
          if (is_child(j)) then
             is_child(particle_get_children(pset%prt(j))) = .true.
             is_child(j) = .false.
             exit mark_next_child
          end if
       end do mark_next_child
    end do
    allocate (child (count (is_real_child)))
    j = 0
    do k = 1, size (is_child)
       if (is_real_child(k)) then
          j = j + 1
          child(j) = k
       end if
    end do
  end function particle_set_get_real_children

  function particle_set_get_n_beam (pset) result (n_beam)
     type(particle_set_t), intent(in) :: pset
     integer :: n_beam
     n_beam = pset%n_beam
  end function particle_set_get_n_beam

  function particle_set_get_n_in (pset) result (n_in)
     type(particle_set_t), intent(in) :: pset
     integer :: n_in
     n_in = pset%n_in
  end function particle_set_get_n_in

  function particle_set_get_n_vir (pset) result (n_vir)
     type(particle_set_t), intent(in) :: pset
     integer :: n_vir
     n_vir = pset%n_in
   end function particle_set_get_n_vir

  function particle_set_get_n_out (pset) result (n_out)
     type(particle_set_t), intent(in) :: pset
     integer :: n_out
     n_out = pset%n_out
  end function particle_set_get_n_out
  
  function particle_set_get_n_tot (pset) result (n_tot)
     type(particle_set_t), intent(in) :: pset
     integer :: n_tot
     n_tot = pset%n_tot
  end function particle_set_get_n_tot

  function particle_set_get_particle(pset, index) result(particle)
    type(particle_set_t), intent(in) :: pset
    integer, intent(in) :: index
    type(particle_t) :: particle

    particle = pset%prt(index)
  end function particle_set_get_particle

  subroutine particle_set_reset_status (particle_set, index, status)
    type(particle_set_t), intent(inout) :: particle_set
    integer, dimension(:), intent(in) :: index
    integer, intent(in) :: status
    integer :: i
    if (allocated (particle_set%prt)) then
       do i = 1, size (index)
          call particle_reset_status (particle_set%prt(index(i)), status)
       end do
    end if
    particle_set%n_beam  = &
         count (particle_get_status (particle_set%prt) == PRT_BEAM)
    particle_set%n_in  = &
         count (particle_get_status (particle_set%prt) == PRT_INCOMING)
    particle_set%n_out = &
         count (particle_get_status (particle_set%prt) == PRT_OUTGOING)
    particle_set%n_vir = particle_set%n_tot &
         - particle_set%n_beam - particle_set%n_in - particle_set%n_out
  end subroutine particle_set_reset_status

  subroutine particle_set_reduce (pset_in, pset_out, keep_beams)
    type(particle_set_t), intent(in) :: pset_in
    type(particle_set_t), intent(out) :: pset_out
    logical, intent(in), optional :: keep_beams
    integer, dimension(:), allocatable :: status, map
    integer :: i, j
    logical :: kb
    kb = .false.;  if (present (keep_beams))  kb = keep_beams
    allocate (status (pset_in%n_tot))    
    status = particle_get_status (pset_in%prt)
    if (kb)  pset_out%n_beam  = count (status == PRT_BEAM)
    pset_out%n_in  = count (status == PRT_INCOMING)
    pset_out%n_vir = count (status == PRT_RESONANT)
    pset_out%n_out = count (status == PRT_OUTGOING)
    pset_out%n_tot = &
         pset_out%n_beam + pset_out%n_in + pset_out%n_vir + pset_out%n_out
    allocate (pset_out%prt (pset_out%n_tot))
    allocate (map (pset_in%n_tot))
    map = 0
    j = 0
    if (kb) call copy_particles (PRT_BEAM)
    call copy_particles (PRT_INCOMING)
    call copy_particles (PRT_RESONANT)
    call copy_particles (PRT_OUTGOING)
    do i = 1, pset_in%n_tot
       if (map(i) == 0)  cycle
       !!! !!! triggers nagfor bug!
       !!!  call particle_set_parents (pset_out%prt(map(i)), &
       !!!       map (particle_set_get_real_parents (pset_in, i)))
       !!!  call particle_set_children (pset_out%prt(map(i)), &
       !!!       map (particle_set_get_real_children (pset_in, i)))
       !!! !!! workaround:
       call particle_set_parents (pset_out%prt(map(i)), &
            particle_set_get_real_parents (pset_in, i, kb))
       call particle_set_parents (pset_out%prt(map(i)), &
            map (pset_out%prt(map(i))%parent))
       call particle_set_children (pset_out%prt(map(i)), &
            particle_set_get_real_children (pset_in, i, kb))
       call particle_set_children (pset_out%prt(map(i)), &
            map (pset_out%prt(map(i))%child))
    end do
  contains
    subroutine copy_particles (stat)
      integer, intent(in) :: stat
      integer :: i
      do i = 1, pset_in%n_tot
         if (status(i) == stat) then
            j = j + 1
            map(i) = j
            call particle_init (pset_out%prt(j), pset_in%prt(i))
         end if
      end do
    end subroutine copy_particles
  end subroutine particle_set_reduce

  subroutine particle_set_apply_keep_beams (pset_in, pset_out, keep_beams)
    type(particle_set_t), intent(in) :: pset_in
    type(particle_set_t), intent(out) :: pset_out
    logical, intent(in), optional :: keep_beams
    integer, dimension(:), allocatable :: status, map
    integer :: i, j
    logical :: kb
    kb = .false.;  if (present (keep_beams))  kb = keep_beams
    allocate (status (pset_in%n_tot))    
    status = particle_get_status (pset_in%prt)
    if (kb)  pset_out%n_beam  = count (status == PRT_BEAM)
    pset_out%n_in  = count (status == PRT_INCOMING)
    if (kb) then
       pset_out%n_vir = count (status == PRT_VIRTUAL) + count (status == PRT_RESONANT) &
            + count (status == PRT_BEAM_REMNANT)
    else 
       pset_out%n_vir = count (status == PRT_VIRTUAL) + count (status == PRT_RESONANT)
    end if
    pset_out%n_out = count (status == PRT_OUTGOING)
    pset_out%n_tot = &
         pset_out%n_beam + pset_out%n_in + pset_out%n_vir + pset_out%n_out
    allocate (pset_out%prt (pset_out%n_tot))
    allocate (map (pset_in%n_tot))
    map = 0
    j = 0
    if (kb) call copy_particles (PRT_BEAM)
    call copy_particles (PRT_INCOMING)
    if (kb) call copy_particles (PRT_BEAM_REMNANT)
    call copy_particles (PRT_RESONANT)
    call copy_particles (PRT_VIRTUAL)
    call copy_particles (PRT_OUTGOING)
    do i = 1, pset_in%n_tot
       if (map(i) == 0)  cycle
       !!! !!! triggers nagfor bug!
       !!!  call particle_set_parents (pset_out%prt(map(i)), &
       !!!       map (particle_set_get_real_parents (pset_in, i)))
       !!!  call particle_set_children (pset_out%prt(map(i)), &
       !!!       map (particle_set_get_real_children (pset_in, i)))
       !!! !!! workaround:
       call particle_set_parents (pset_out%prt(map(i)), &
            particle_set_get_real_parents (pset_in, i, kb))
       call particle_set_parents (pset_out%prt(map(i)), &
            map (pset_out%prt(map(i))%parent))
       call particle_set_children (pset_out%prt(map(i)), &
            particle_set_get_real_children (pset_in, i, kb))
       call particle_set_children (pset_out%prt(map(i)), &
            map (pset_out%prt(map(i))%child))
    end do
  contains
    subroutine copy_particles (stat)
      integer, intent(in) :: stat
      integer :: i
      do i = 1, pset_in%n_tot
         if (status(i) == stat) then
            j = j + 1
            map(i) = j
            call particle_init (pset_out%prt(j), pset_in%prt(i))
         end if
      end do
    end subroutine copy_particles
  end subroutine particle_set_apply_keep_beams

  subroutine particle_set_to_hepevt_form (pset_in, pset_out, keep_beams)
    type(particle_set_t), intent(in) :: pset_in
    type(particle_set_t), intent(out) :: pset_out
    logical, intent(in), optional :: keep_beams
    type(particle_set_t) :: pset
    type :: particle_entry_t
       integer :: src = 0
       integer :: status = 0
       integer :: orig = 0
       integer :: copy = 0
    end type particle_entry_t
    type(particle_entry_t), dimension(:), allocatable :: prt
    integer, dimension(:), allocatable :: map1, map2
    integer, dimension(:), allocatable :: parent, child
    integer :: n_tot, n_parents, n_children, i, j, c, n

    call particle_set_apply_keep_beams(pset_in, pset, keep_beams)
    n_tot = pset%n_tot
    allocate (prt (4 * n_tot))
    allocate (map1(4 * n_tot))
    allocate (map2(4 * n_tot))
    map1 = 0
    map2 = 0
    allocate (child (n_tot))
    allocate (parent (n_tot))
    n = 0
    do i = 1, n_tot
       if (particle_get_n_parents (pset%prt(i)) == 0) then
          call append (i)
       end if
    end do
    do i = 1, n_tot
       n_children = particle_get_n_children (pset%prt(i))
       if (n_children > 0) then
          child(1:n_children) = particle_get_children (pset%prt(i))
          c = child(1)
          if (map1(c) == 0) then
             n_parents = particle_get_n_parents (pset%prt(c))
             if (n_parents > 1) then
                parent(1:n_parents) = particle_get_parents (pset%prt(c))
                if (i == parent(1) .and. &
                    any( [(map1(i)+j-1, j=1,n_parents)] /= &
                           map1(parent(1:n_parents)))) &
                    then
                   do j = 1, n_parents
                      call append (parent(j))
                   end do
                end if
             else if (map1(i) == 0) then
                call append (i)
             end if
             do j = 1, n_children
                call append (child(j))
             end do
          end if
       else if (map1(i) == 0) then
          call append (i)
       end if
    end do
    do i = n, 1, -1
       if (prt(i)%status /= PRT_OUTGOING) then
          do j = 1, i-1
             if (prt(j)%status == PRT_OUTGOING) then
                call append(prt(j)%src)
             end if
          end do
          exit
       end if
    end do
    pset_out%n_beam = count (prt(1:n)%status == PRT_BEAM)
    pset_out%n_in   = count (prt(1:n)%status == PRT_INCOMING)
    pset_out%n_vir  = count (prt(1:n)%status == PRT_RESONANT)
    pset_out%n_out  = count (prt(1:n)%status == PRT_OUTGOING)
    pset_out%n_tot = n
    allocate (pset_out%prt (n))
    do i = 1, n
       call particle_init (pset_out%prt(i), pset%prt(prt(i)%src))
       call particle_reset_status (pset_out%prt(i), prt(i)%status)
       if (prt(i)%orig == 0) then
          !!! !!! This causes nagfor 5.2 (770) Panic
          !!!  call particle_set_parents &
          !!!       (pset_out%prt(i), &
          !!!        map2 (particle_get_parents (pset%prt(prt(i)%src))))
          !!! !!! Workaround
          n_parents = particle_get_n_parents (pset%prt(prt(i)%src))
          parent(1:n_parents) = particle_get_parents (pset%prt(prt(i)%src))
          call particle_set_parents (pset_out%prt(i), &
                                     map2(parent(1:n_parents)))
       else
          call particle_set_parents (pset_out%prt(i), [ prt(i)%orig ])
       end if
       if (prt(i)%copy == 0) then
          !!! !!! This causes nagfor 5.2 (770) Panic
          !!!  call particle_set_children &
          !!!       (pset_out%prt(i), &
          !!!        map1 (particle_get_children (pset%prt(prt(i)%src))))
          !!! !!! Workaround
          n_children = particle_get_n_children (pset%prt(prt(i)%src))
          child(1:n_children) = particle_get_children (pset%prt(prt(i)%src))
          call particle_set_children (pset_out%prt(i), &
                                      map1(child(1:n_children)))
       else
          call particle_set_children (pset_out%prt(i), [ prt(i)%copy ])
       end if
    end do
  contains
    subroutine append (i)
      integer, intent(in) :: i
      n = n + 1
      if (n > size (prt)) &
           call msg_bug ("Particle set transform to HEPEVT: insufficient space")
      prt(n)%src = i
      prt(n)%status = particle_get_status (pset%prt(i))
      if (map1(i) == 0) then
         map1(i) = n
      else
         prt(map2(i))%status = PRT_VIRTUAL
         prt(map2(i))%copy = n
         prt(n)%orig = map2(i)
      end if
      map2(i) = n
    end subroutine append
  end subroutine particle_set_to_hepevt_form

  subroutine particle_set_fill_interaction &
       (pset, int, n_in, recover_beams, check_match, state_flv)
    type(particle_set_t), intent(in) :: pset
    type(interaction_t), intent(inout) :: int
    integer, intent(in) :: n_in
    logical, intent(in), optional :: recover_beams, check_match
    type(state_flv_content_t), intent(in), optional :: state_flv
    integer, dimension(:), allocatable :: map, pdg
    integer, dimension(:), allocatable :: i_in, i_out, p_in, p_out
    logical, dimension(:), allocatable :: i_set
    integer :: n_out, i, p
    logical :: r_beams, check
    r_beams = .false.;  if (present (recover_beams))  r_beams = recover_beams
    check = .true.;  if (present (check_match))  check = check_match
    if (check) then
       call find_hard_process_in_int  (i_in, i_out)
       call find_hard_process_in_pset (p_in, p_out)
       n_out = size (i_out)
       if (size (i_in) /= n_in)  call err_int_n_in
       if (size (p_in) /= n_in)  call err_pset_n_in
       if (size (p_out) /= n_out)  call err_pset_n_out
       call extract_hard_process_from_pset (pdg)
       call determine_map_for_hard_process (map, state_flv)
       if (.not. r_beams) then
          select case (n_in)
          case (1)
             call recover_parents (p_in(1), map)
          case (2)
             do i = 1, 2
                call recover_parents (p_in(i), map)
             end do
             do p = 1, 2
                call recover_radiation (p, map)
             end do
          end select
       end if
    else
       allocate (map (interaction_get_n_tot (int)))
       map = [(i, i = 1, size (map))]
       r_beams = .false.
    end if
    allocate (i_set (interaction_get_n_tot (int)), source = .false.)
    do p = 1, size (map)
       if (map(p) /= 0) then
          i_set(map(p)) = .true.
          call interaction_set_momentum (int, &
               particle_get_momentum (pset%prt(p)), map(p))
       end if
    end do
    if (r_beams) then
       do i = 1, n_in
          call reconstruct_beam_and_radiation (i, i_set)
       end do
    end if
    if (any (.not. i_set))  call err_map
  contains
    subroutine find_hard_process_in_pset (p_in, p_out)
      integer, dimension(:), allocatable, intent(out) :: p_in, p_out
      integer, dimension(:), allocatable :: p_status, p_idx
      integer :: n_in_p, n_out_p
      integer :: i
      allocate (p_status (pset%n_tot), p_idx (pset%n_tot))
      p_status = particle_get_status (pset%prt)
      p_idx = [(i, i = 1, pset%n_tot)]
      n_in_p = count (p_status == PRT_INCOMING)
      allocate (p_in (n_in))
      p_in = pack (p_idx, p_status == PRT_INCOMING)
      if (size (p_in) == 0)  call err_pset_hard
      i = p_in(1)
      n_out_p = particle_get_n_children (pset%prt(i))
      allocate (p_out (n_out_p))
      p_out = particle_get_children (pset%prt(i))
    end subroutine find_hard_process_in_pset
    subroutine find_hard_process_in_int (i_in, i_out)
      integer, dimension(:), allocatable, intent(out) :: i_in, i_out
      integer, dimension(:), allocatable :: p_status, p_idx
      integer :: n_in_i, n_out_i
      integer :: i
      i = interaction_get_n_tot (int)
      n_in_i = interaction_get_n_parents (int, i)
      if (n_in_i /= n_in)  call err_int_n_in
      allocate (i_in (n_in))
      i_in = interaction_get_parents (int, i)
      i = i_in(1)
      n_out = interaction_get_n_children (int, i)
      allocate (i_out (n_out))
      i_out = interaction_get_children (int, i)
    end subroutine find_hard_process_in_int
    subroutine extract_hard_process_from_pset (pdg)
      integer, dimension(:), allocatable, intent(out) :: pdg
      integer, dimension(:), allocatable :: pdg_p
      logical, dimension(:), allocatable :: mask_p
      allocate (pdg_p (pset%n_tot))
      pdg_p = particle_get_pdg (pset%prt)
      allocate (mask_p (pset%n_tot), source = .false.)
      mask_p (p_in) = .true.
      mask_p (p_out) = .true.
      allocate (pdg (n_in + n_out))
      pdg = pack (pdg_p, mask_p)
    end subroutine extract_hard_process_from_pset
    subroutine determine_map_for_hard_process (map, state_flv)
      integer, dimension(:), allocatable, intent(out) :: map
      type(state_flv_content_t), intent(in), optional :: state_flv
      integer, dimension(:), allocatable :: pdg_i, map_i
      integer :: n_tot
      logical, dimension(:), allocatable :: mask_i, mask_p
      logical :: success
      n_tot = interaction_get_n_tot (int)
      allocate (map (n_tot), source = 0)
      if (present (state_flv)) then
         allocate (mask_i (n_tot), source = .false.)
         mask_i (i_in) = .true.
         mask_i (i_out) = .true.
         allocate (pdg_i (n_tot), map_i (n_tot))
         pdg_i = unpack (pdg, mask_i, 0)
         call state_flv%match (pdg_i, success, map_i)
         allocate (mask_p (pset%n_tot), source = .false.)
         mask_p (p_in) = .true.
         mask_p (p_out) = .true.
         map = unpack (pack (map_i, mask_i), mask_p, 0)
         if (.not. success)  call err_mismatch
      else
         map(p_in) = i_in
         map(p_out) = i_out
      end if
    end subroutine determine_map_for_hard_process
    recursive subroutine recover_parents (p, map)
      integer, intent(in) :: p
      integer, dimension(:), intent(inout) :: map
      integer :: i, n, n_p, q, k
      integer, dimension(:), allocatable :: i_parents, p_parents
      integer, dimension(1) :: pp
      i = map(p)
      n = interaction_get_n_parents (int, i)
      q = p
      n_p = particle_get_n_parents (pset%prt(q))
      do while (n_p == 1)
         pp = particle_get_parents (pset%prt(q))
         if (particle_get_n_children (pset%prt(pp(1))) > 1)  exit
         q = pp(1)
         n_p = particle_get_n_parents (pset%prt(q))
      end do
      if (n_p /= n)  call err_map
      allocate (i_parents (n), p_parents (n))
      i_parents = interaction_get_parents (int, i)
      p_parents = particle_get_parents (pset%prt(q))
      do k = 1, n
         q = p_parents(k)
         if (map(q) == 0) then
            map(q) = i_parents(k)
            call recover_parents (q, map)
         end if
      end do
    end subroutine recover_parents
    recursive subroutine recover_radiation (p, map)
      integer, intent(in) :: p
      integer, dimension(:), intent(inout) :: map
      integer :: i, n, n_p, q, k
      integer, dimension(:), allocatable :: i_children, p_children
      if (particle_get_status (pset%prt(p)) == PRT_INCOMING)  return
      i = map(p)
      n = interaction_get_n_children (int, i)
      n_p = particle_get_n_children (pset%prt(p))
      if (n_p /= n)  call err_map
      allocate (i_children (n), p_children (n))
      i_children = interaction_get_children (int, i)
      p_children = particle_get_children (pset%prt(p))
      do k = 1, n
         q = p_children(k)
         if (map(q) == 0) then
            i = i_children(k)
            if (interaction_get_n_children (int, i) == 0) then
               map(q) = i
            else
               select case (n)
               case (2)
                  select case (k)
                  case (1);  map(q) = i_children(2)
                  case (2);  map(q) = i_children(1)
                  end select
               case (4)
                  select case (k)
                  case (1);  map(q) = i_children(3)
                  case (2);  map(q) = i_children(4)
                  case (3);  map(q) = i_children(1)
                  case (4);  map(q) = i_children(2)
                  end select
               case default
                  call err_radiation
               end select
            end if
         else
            call recover_radiation (q, map)
         end if
      end do
    end subroutine recover_radiation
    subroutine reconstruct_beam_and_radiation (k, i_set)
      integer, intent(in) :: k
      logical, dimension(:), intent(inout) :: i_set
      integer :: k_src, k_in, k_rad
      type(interaction_t), pointer :: int_src
      integer, dimension(2) :: i_child
      call interaction_find_source (int, k, int_src, k_src)
      call interaction_set_momentum (int, &
           interaction_get_momentum (int_src, k_src), k)
      i_set(k) = .true.
      if (n_in == 2) then
         i_child = interaction_get_children (int, k)
         if (interaction_get_n_children (int, i_child(1)) > 0) then
            k_in = i_child(1);  k_rad = i_child(2)
         else
            k_in = i_child(2);  k_rad = i_child(1)
         end if
         if (.not. i_set(k_in))  call err_beams
         call interaction_set_momentum (int, &
              interaction_get_momentum (int, k) &
              - interaction_get_momentum (int, k_in), k_rad)
         i_set(k_rad) = .true.
      end if
    end subroutine reconstruct_beam_and_radiation
    subroutine err_pset_hard
      call msg_fatal ("Reading particle set: no particles marked as incoming")
    end subroutine err_pset_hard
    subroutine err_int_n_in
      integer :: n
      if (allocated (i_in)) then
         n = size (i_in)
      else
         n = 0
      end if
      write (msg_buffer, "(A,I0,A,I0)") &
           "Filling hard process from particle set: expect ", n_in, &
           " incoming particle(s), found ", n
      call msg_bug
    end subroutine err_int_n_in
    subroutine err_pset_n_in
      write (msg_buffer, "(A,I0,A,I0)") &
           "Reading hard-process particle set: should contain ", n_in, &
           " incoming particle(s), found ", size (p_in)
      call msg_fatal
    end subroutine err_pset_n_in
    subroutine err_pset_n_out
      write (msg_buffer, "(A,I0,A,I0)") &
           "Reading hard-process particle set: should contain ", n_out, &
           " outgoing particle(s), found ", size (p_out)
      call msg_fatal
    end subroutine err_pset_n_out
    subroutine err_mismatch
      call particle_set_write (pset)
      call state_flv%write ()
      call msg_fatal ("Reading particle set: Flavor combination " &
           // "does not match requested process")
    end subroutine err_mismatch
    subroutine err_map
      call particle_set_write (pset)
      call interaction_write (int)
      call msg_fatal ("Reading hard-process particle set: " &
           // "Incomplete mapping from particle set to interaction")
    end subroutine err_map
    subroutine err_beams
      call particle_set_write (pset)
      call interaction_write (int)
      call msg_fatal ("Reading particle set: Beam structure " &
           // "does not match requested process")
    end subroutine err_beams
    subroutine err_radiation
      call interaction_write (int)
      call msg_bug ("Reading particle set: Interaction " &
           // "contains inconsistent radiation pattern.")
    end subroutine err_radiation
  end subroutine particle_set_fill_interaction
    
  subroutine particle_set_assign_vertices &
       (particle_set, v_from, v_to, n_vertices)
    type(particle_set_t), intent(in) :: particle_set
    integer, dimension(:), intent(out) :: v_from, v_to
    integer, intent(out) :: n_vertices
    integer, dimension(:), allocatable :: parent, child
    integer :: n_parents, n_children, vf, vt
    integer :: i, j, v
    v_from = 0
    v_to = 0
    vf = 0
    vt = 0
    do i = 1, particle_set%n_tot
       n_parents = particle_get_n_parents (particle_set%prt(i))
       if (n_parents /= 0) then
          allocate (parent (n_parents))
          parent = particle_get_parents (particle_set%prt(i))
          SCAN_PARENTS: do j = 1, size (parent)
             v = v_to(parent(j))
             if (v /= 0) then
                v_from(i) = v;  exit SCAN_PARENTS
             end if
          end do SCAN_PARENTS
          if (v_from(i) == 0) then
             vf = vf + 1;  v_from(i) = vf
             v_to(parent) = vf
          end if
          deallocate (parent)
       end if
       n_children = particle_get_n_children (particle_set%prt(i))
       if (n_children /= 0) then
          allocate (child (n_children))
          child = particle_get_children (particle_set%prt(i))
          SCAN_CHILDREN: do j = 1, size (child)
             v = v_from(child(j))
             if (v /= 0) then
                v_to(i) = v;  exit SCAN_CHILDREN
             end if
          end do SCAN_CHILDREN
          if (v_to(i) == 0) then
             vt = vt + 1;  v_to(i) = vt
             v_from(child) = vt
          end if
          deallocate (child)
       end if
    end do
    n_vertices = max (vf, vt)
  end subroutine particle_set_assign_vertices

  subroutine particle_set_to_subevt (particle_set, subevt)
    type(particle_set_t), intent(in), target :: particle_set
    type(subevt_t), intent(out) :: subevt
    type(particle_t), pointer :: prt
    integer :: i, k
    integer, dimension(2) :: hel
    call subevt_init &
         (subevt, particle_set%n_beam + particle_set%n_in + particle_set%n_out)
    k = 0
    do i = 1, particle_set%n_tot
       prt => particle_set%prt(i)
       select case (particle_get_status (prt))
       case (PRT_BEAM)
          k = k + 1
          call subevt_set_beam (subevt, k, &
               particle_get_pdg (prt), &
               particle_get_momentum (prt), &
               particle_get_p2 (prt))
       case (PRT_INCOMING)
          k = k + 1
          call subevt_set_incoming (subevt, k, &
               particle_get_pdg (prt), &
               particle_get_momentum (prt), &
               particle_get_p2 (prt))
       case (PRT_OUTGOING)
          k = k + 1
          call subevt_set_outgoing (subevt, k, &
               particle_get_pdg (prt), &
               particle_get_momentum (prt), &
               particle_get_p2 (prt))
       end select
       select case (particle_get_status (prt))
       case (PRT_BEAM, PRT_INCOMING, PRT_OUTGOING)
          if (prt%polarization == PRT_DEFINITE_HELICITY) then
             if (helicity_is_diagonal (prt%hel)) then
                hel = helicity_get (prt%hel)
                call subevt_polarize (subevt, k, hel(1))
             end if
          end if
       end select
    end do
  end subroutine particle_set_to_subevt

  subroutine particle_set_replace (particle_set, newprt)
    type(particle_set_t), intent(inout) :: particle_set
    type(particle_t), intent(in), dimension(:), allocatable :: newprt
    if (allocated (particle_set%prt))  deallocate (particle_set%prt)
    allocate (particle_set%prt(size (newprt)))
    particle_set%prt = newprt
    particle_set%n_tot = size (newprt)
    particle_set%n_beam = count (particle_get_status (newprt) == PRT_BEAM)
    particle_set%n_in = count (particle_get_status (newprt) == PRT_INCOMING)
    particle_set%n_out = count (particle_get_status (newprt) == PRT_OUTGOING)
    particle_set%n_vir = particle_set%n_tot &
         - particle_set%n_beam - particle_set%n_in - particle_set%n_out
  end subroutine particle_set_replace

  subroutine pacify_particle (prt)
    class(particle_t), intent(inout) :: prt
    real(default) :: e
    e = epsilon (1._default) * energy (prt%p)
    call pacify (prt%p, 10 * e)
    call pacify (prt%p2, 1e4 * e)
  end subroutine pacify_particle
  
  subroutine pacify_particle_set (pset)
    class(particle_set_t), intent(inout) :: pset
    integer :: i
    do i = 1, pset%n_tot
       call pacify (pset%prt(i))
    end do
  end subroutine pacify_particle_set

  subroutine particles_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (particles_1, "particles_1", &
         "check particle_set routines", &
         u, results)
    if (hepmc_is_available ()) then
       call test (particles_2, "particles_2", &
            "check particle_set routines via HepMC", &
            u, results)
    end if
    call test (particles_3, "particles_3", &
         "reconstruct hard interaction", &
         u, results)
    call test (particles_4, "particles_4", &
         "reconstruct interaction with beam structure", &
         u, results)
    call test (particles_5, "particles_5", &
         "reconstruct interaction with missing beams", &
         u, results)
    call test (particles_6, "particles_6", &
         "reconstruct interaction with beams and duplicate entries", &
         u, results)
    call test (particles_7, "particles_7", &
         "reconstruct interaction with pair spectrum", &
         u, results)
    call test (particles_8, "particles_8", &
         "reconstruct decay interaction with reordering", &
         u, results)  
  end subroutine particles_test


  subroutine particles_1 (u)
    use os_interface
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(flavor_t), dimension(3) :: flv
    type(color_t), dimension(3) :: col
    type(helicity_t), dimension(3) :: hel
    type(quantum_numbers_t), dimension(3) :: qn
    type(vector4_t), dimension(3) :: p
    type(interaction_t), target :: int1, int2
    type(quantum_numbers_mask_t) :: qn_mask_conn, qn_rest
    type(evaluator_t), target :: eval
    type(interaction_t), pointer :: int
    type(particle_set_t) :: particle_set1, particle_set2
    type(particle_set_t) :: particle_set3, particle_set4
    type(subevt_t) :: subevt
    type(hepmc_event_t) :: hepmc_event
    type(hepmc_iostream_t) :: iostream    
    logical :: ok
    integer :: unit, iostat  

    write (u, "(A)")  "* Test output: Particles"
    write (u, "(A)")  "*   Purpose: test particle_set routines"
    write (u, "(A)")      
    
    write (u, "(A)")  "* Reading model file"

    call model%init_sm_test ()

    write (u, "(A)")
    write (u, "(A)")  "* Initializing production process"

    call interaction_init (int1, 2, 0, 1, set_relations=.true.)
    call flavor_init (flv, [1, -1, 23], model)
    call helicity_init (hel(3), 1, 1)
    call quantum_numbers_init (qn, flv, hel)
    call interaction_add_state (int1, qn, value=(0.25_default, 0._default))
    call helicity_init (hel(3), 1,-1)
    call quantum_numbers_init (qn, flv, hel)
    call interaction_add_state (int1, qn, value=(0._default, 0.25_default))
    call helicity_init (hel(3),-1, 1)
    call quantum_numbers_init (qn, flv, hel)
    call interaction_add_state (int1, qn, value=(0._default,-0.25_default))
    call helicity_init (hel(3),-1,-1)
    call quantum_numbers_init (qn, flv, hel)
    call interaction_add_state (int1, qn, value=(0.25_default, 0._default))
    call helicity_init (hel(3), 0, 0)
    call quantum_numbers_init (qn, flv, hel)
    call interaction_add_state (int1, qn, value=(0.5_default, 0._default))
    call interaction_freeze (int1)
    p(1) = vector4_moving (45._default, 45._default, 3)
    p(2) = vector4_moving (45._default,-45._default, 3)
    p(3) = p(1) + p(2)
    call interaction_set_momenta (int1, p)

    write (u, "(A)")
    write (u, "(A)")  "* Setup decay process"

    call interaction_init (int2, 1, 0, 2, set_relations=.true.)
    call flavor_init (flv, [23, 1, -1], model)
    call color_init_col_acl (col, [0, 501, 0], [0, 0, 501])
    call helicity_init (hel, [1, 1, 1], [1, 1, 1])
    call quantum_numbers_init (qn, flv, col, hel)
    call interaction_add_state (int2, qn, value=(1._default, 0._default))
    call helicity_init (hel, [1, 1, 1], [-1,-1,-1])
    call quantum_numbers_init (qn, flv, col, hel)
    call interaction_add_state (int2, qn, value=(0._default, 0.1_default))
    call helicity_init (hel, [-1,-1,-1], [1, 1, 1])
    call quantum_numbers_init (qn, flv, col, hel)
    call interaction_add_state (int2, qn, value=(0._default,-0.1_default))
    call helicity_init (hel, [-1,-1,-1], [-1,-1,-1])
    call quantum_numbers_init (qn, flv, col, hel)
    call interaction_add_state (int2, qn, value=(1._default, 0._default))
    call helicity_init (hel, [0, 1,-1], [0, 1,-1])
    call quantum_numbers_init (qn, flv, col, hel)
    call interaction_add_state (int2, qn, value=(4._default, 0._default))
    call helicity_init (hel, [0,-1, 1], [0, 1,-1])
    call quantum_numbers_init (qn, flv, col, hel)
    call interaction_add_state (int2, qn, value=(2._default, 0._default))
    call helicity_init (hel, [0, 1,-1], [0,-1, 1])
    call quantum_numbers_init (qn, flv, col, hel)
    call interaction_add_state (int2, qn, value=(2._default, 0._default))
    call helicity_init (hel, [0,-1, 1], [0,-1, 1])
    call quantum_numbers_init (qn, flv, col, hel)
    call interaction_add_state (int2, qn, value=(4._default, 0._default))
    call flavor_init (flv, [23, 2, -2], model)
    call helicity_init (hel, [0, 1,-1], [0, 1,-1])
    call quantum_numbers_init (qn, flv, col, hel)
    call interaction_add_state (int2, qn, value=(0.5_default, 0._default))
    call helicity_init (hel, [0,-1, 1], [0,-1, 1])
    call quantum_numbers_init (qn, flv, col, hel)
    call interaction_add_state (int2, qn, value=(0.5_default, 0._default))
    call interaction_freeze (int2)
    p(2) = vector4_moving (45._default, 45._default, 2)
    p(3) = vector4_moving (45._default,-45._default, 2)
    call interaction_set_momenta (int2, p)
    call interaction_set_source_link (int2, 1, int1, 3)
    call interaction_write (int1, u)
    call interaction_write (int2, u)

    write (u, "(A)")
    write (u, "(A)")  "* Concatenate production and decay"

    call evaluator_init_product (eval, int1, int2, qn_mask_conn, &
         connections_are_resonant=.true.)
    call evaluator_receive_momenta (eval)
    call eval%evaluate ()
    call eval%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Factorize as subevent (complete, polarized)"
    write (u, "(A)")
    
    int => evaluator_get_int_ptr (eval)
    call particle_set_init &
         (particle_set1, ok, int, int, FM_FACTOR_HELICITY, &
          [0.2_default, 0.2_default], .false., .true.)
    call particle_set_write (particle_set1, u)

    write (u, "(A)")
    write (u, "(A)")  "* Factorize as subevent (in/out only, selected helicity)"
    write (u, "(A)")
    
    int => evaluator_get_int_ptr (eval)
    call particle_set_init &
         (particle_set2, ok, int, int, FM_SELECT_HELICITY, &
          [0.9_default, 0.9_default], .false., .false.)
    call particle_set_write (particle_set2, u)
    call particle_set_final (particle_set2)

    write (u, "(A)")
    write (u, "(A)")  "* Factorize as subevent (complete, selected helicity)"
    write (u, "(A)") 
    
    int => evaluator_get_int_ptr (eval)
    call particle_set_init &
         (particle_set2, ok, int, int, FM_SELECT_HELICITY, &
          [0.7_default, 0.7_default], .false., .true.)
    call particle_set_write (particle_set2, u)  
        
    write (u, "(A)")
    write (u, "(A)")  &
         "* Factorize (complete, polarized, correlated); write and read again"
    write (u, "(A)")
    
    int => evaluator_get_int_ptr (eval)
    call particle_set_init &
         (particle_set3, ok, int, int, FM_FACTOR_HELICITY, &
          [0.7_default, 0.7_default], .true., .true.)
    call particle_set_write (particle_set3, u)

    unit = free_unit ()
    open (unit, action="readwrite", form="unformatted", status="scratch")
    call particle_set_write_raw (particle_set3, unit)
    rewind (unit)
    call particle_set_read_raw (particle_set4, unit, iostat=iostat)
    call particle_set_set_model (particle_set4, model)
    close (unit)
    
    write (u, "(A)")
    write (u, "(A)")  "* Result from reading"
    write (u, "(A)")

    call particle_set_write (particle_set4, u)

    !!! write (u, "(A)")
    !!! write (u, "(A)")  "* Transform to a subevt object"
    !!! write (u, "(A)")
    !!! 
    !!! call particle_set_to_subevt (particle_set4, subevt)
    !!! call subevt_write (subevt, u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call particle_set_final (particle_set1)
    call particle_set_final (particle_set2)
    call particle_set_final (particle_set3)
    call particle_set_final (particle_set4)
    call evaluator_final (eval)
    call interaction_final (int1)
    call interaction_final (int2)

    call model%final ()
       
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: particles_1"        
    
  end subroutine particles_1

  subroutine particles_2 (u)
    use os_interface
    integer, intent(in) :: u
    type(model_data_t), target :: model
    type(flavor_t), dimension(3) :: flv
    type(color_t), dimension(3) :: col
    type(helicity_t), dimension(3) :: hel
    type(quantum_numbers_t), dimension(3) :: qn
    type(vector4_t), dimension(3) :: p
    type(interaction_t), target :: int1, int2
    type(quantum_numbers_mask_t) :: qn_mask_conn, qn_rest
    type(evaluator_t), target :: eval
    type(interaction_t), pointer :: int
    type(particle_set_t) :: particle_set1, particle_set2
    type(particle_set_t) :: particle_set3, particle_set4
    type(subevt_t) :: subevt
    type(hepmc_event_t) :: hepmc_event
    type(hepmc_iostream_t) :: iostream    
    logical :: ok
    integer :: unit, iostat  

    write (u, "(A)")  "* Test output: Particles"
    write (u, "(A)")  "*   Purpose: test particle_set routines via HepMC"
    write (u, "(A)")      
       
    write (u, "(A)")  "* Reading model file"

    call model%init_sm_test ()

    write (u, "(A)")
    write (u, "(A)")  "* Initializing production process"

    call interaction_init (int1, 2, 0, 1, set_relations=.true.)
    call flavor_init (flv, [1, -1, 23], model)
    call helicity_init (hel(3), 1, 1)
    call quantum_numbers_init (qn, flv, hel)
    call interaction_add_state (int1, qn, value=(0.25_default, 0._default))
    call helicity_init (hel(3), 1,-1)
    call quantum_numbers_init (qn, flv, hel)
    call interaction_add_state (int1, qn, value=(0._default, 0.25_default))
    call helicity_init (hel(3),-1, 1)
    call quantum_numbers_init (qn, flv, hel)
    call interaction_add_state (int1, qn, value=(0._default,-0.25_default))
    call helicity_init (hel(3),-1,-1)
    call quantum_numbers_init (qn, flv, hel)
    call interaction_add_state (int1, qn, value=(0.25_default, 0._default))
    call helicity_init (hel(3), 0, 0)
    call quantum_numbers_init (qn, flv, hel)
    call interaction_add_state (int1, qn, value=(0.5_default, 0._default))
    call interaction_freeze (int1)
    p(1) = vector4_moving (45._default, 45._default, 3)
    p(2) = vector4_moving (45._default,-45._default, 3)
    p(3) = p(1) + p(2)
    call interaction_set_momenta (int1, p)

    write (u, "(A)")
    write (u, "(A)")  "* Setup decay process"

    call interaction_init (int2, 1, 0, 2, set_relations=.true.)
    call flavor_init (flv, [23, 1, -1], model)
    call color_init_col_acl (col, [0, 501, 0], [0, 0, 501])
    call helicity_init (hel, [1, 1, 1], [1, 1, 1])
    call quantum_numbers_init (qn, flv, col, hel)
    call interaction_add_state (int2, qn, value=(1._default, 0._default))
    call helicity_init (hel, [1, 1, 1], [-1,-1,-1])
    call quantum_numbers_init (qn, flv, col, hel)
    call interaction_add_state (int2, qn, value=(0._default, 0.1_default))
    call helicity_init (hel, [-1,-1,-1], [1, 1, 1])
    call quantum_numbers_init (qn, flv, col, hel)
    call interaction_add_state (int2, qn, value=(0._default,-0.1_default))
    call helicity_init (hel, [-1,-1,-1], [-1,-1,-1])
    call quantum_numbers_init (qn, flv, col, hel)
    call interaction_add_state (int2, qn, value=(1._default, 0._default))
    call helicity_init (hel, [0, 1,-1], [0, 1,-1])
    call quantum_numbers_init (qn, flv, col, hel)
    call interaction_add_state (int2, qn, value=(4._default, 0._default))
    call helicity_init (hel, [0,-1, 1], [0, 1,-1])
    call quantum_numbers_init (qn, flv, col, hel)
    call interaction_add_state (int2, qn, value=(2._default, 0._default))
    call helicity_init (hel, [0, 1,-1], [0,-1, 1])
    call quantum_numbers_init (qn, flv, col, hel)
    call interaction_add_state (int2, qn, value=(2._default, 0._default))
    call helicity_init (hel, [0,-1, 1], [0,-1, 1])
    call quantum_numbers_init (qn, flv, col, hel)
    call interaction_add_state (int2, qn, value=(4._default, 0._default))
    call flavor_init (flv, [23, 2, -2], model)
    call helicity_init (hel, [0, 1,-1], [0, 1,-1])
    call quantum_numbers_init (qn, flv, col, hel)
    call interaction_add_state (int2, qn, value=(0.5_default, 0._default))
    call helicity_init (hel, [0,-1, 1], [0,-1, 1])
    call quantum_numbers_init (qn, flv, col, hel)
    call interaction_add_state (int2, qn, value=(0.5_default, 0._default))
    call interaction_freeze (int2)
    p(2) = vector4_moving (45._default, 45._default, 2)
    p(3) = vector4_moving (45._default,-45._default, 2)
    call interaction_set_momenta (int2, p)
    call interaction_set_source_link (int2, 1, int1, 3)
    call interaction_write (int1, u)
    call interaction_write (int2, u)

    write (u, "(A)")
    write (u, "(A)")  "* Concatenate production and decay"

    call evaluator_init_product (eval, int1, int2, qn_mask_conn, &
         connections_are_resonant=.true.)
    call evaluator_receive_momenta (eval)
    call eval%evaluate ()
    call eval%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Factorize as subevent (complete, polarized)"
    write (u, "(A)")
    
    int => evaluator_get_int_ptr (eval)
    call particle_set_init &
         (particle_set1, ok, int, int, FM_FACTOR_HELICITY, &
          [0.2_default, 0.2_default], .false., .true.)
    call particle_set_write (particle_set1, u)

    write (u, "(A)")
    write (u, "(A)")  "* Factorize as subevent (in/out only, selected helicity)"
    write (u, "(A)")
    
    int => evaluator_get_int_ptr (eval)
    call particle_set_init &
         (particle_set2, ok, int, int, FM_SELECT_HELICITY, &
          [0.9_default, 0.9_default], .false., .false.)
    call particle_set_write (particle_set2, u)
    call particle_set_final (particle_set2)

    write (u, "(A)")
    write (u, "(A)")  "* Factorize as subevent (complete, selected helicity)"
    write (u, "(A)") 
    
    int => evaluator_get_int_ptr (eval)
    call particle_set_init &
         (particle_set2, ok, int, int, FM_SELECT_HELICITY, &
          [0.7_default, 0.7_default], .false., .true.)
    call particle_set_write (particle_set2, u)  

    write (u, "(A)")
    write (u, "(A)")  "* Transfer particle_set to HepMC, print, and output to"
    write (u, "(A)")  "        particles_test.hepmc.dat"
    write (u, "(A)")
    
    call hepmc_event_init (hepmc_event, 11, 127)
    call hepmc_event_from_particle_set (hepmc_event, particle_set2)
    call hepmc_event_print (hepmc_event)
    call hepmc_iostream_open_out &
         (iostream , var_str ("particles_test.hepmc.dat"))
    call hepmc_iostream_write_event (iostream, hepmc_event)
    call hepmc_iostream_close (iostream)

    write (u, "(A)")
    write (u, "(A)")  "* Recover from HepMC file"
    write (u, "(A)")
    
    call particle_set_final (particle_set2)
    call hepmc_event_final (hepmc_event)
    call hepmc_event_init (hepmc_event)
    call hepmc_iostream_open_in &
         (iostream , var_str ("particles_test.hepmc.dat"))
    call hepmc_iostream_read_event (iostream, hepmc_event, ok)
    call hepmc_iostream_close (iostream)
    call particle_set_init (particle_set2, &
         hepmc_event, model, model, PRT_DEFINITE_HELICITY)
    call particle_set_write (particle_set2, u)   

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call particle_set_final (particle_set1)
    call particle_set_final (particle_set2)
    call evaluator_final (eval)
    call interaction_final (int1)
    call interaction_final (int2)
    call hepmc_event_final (hepmc_event)            
    call model%final ()
       
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: particles_2"

  end subroutine particles_2
  
  subroutine particles_3 (u)

    integer, intent(in) :: u
    type(interaction_t) :: int
    type(state_flv_content_t) :: state_flv
    type(particle_set_t) :: pset
    type(flavor_t), dimension(:), allocatable :: flv
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    integer :: i, j
    
    write (u, "(A)")  "* Test output: Particles"
    write (u, "(A)")  "*   Purpose: reconstruct simple interaction"
    write (u, "(A)")      

    write (u, "(A)")  "* Set up a 2 -> 3 interaction"
    write (u, "(A)")  "    + incoming partons marked as virtual"
    write (u, "(A)")  "    + no quantum numbers"
    write (u, "(A)")      

    call reset_interaction_counter ()
    call interaction_init (int, 0, 2, 3)
    do i = 1, 2
       do j = 3, 5
          call interaction_relate (int, i, j)
       end do
    end do

    allocate (qn (5))
    call interaction_add_state (int, qn)
    call interaction_freeze (int)

    call interaction_write (int, u)

    write (u, "(A)")      
    write (u, "(A)")  "* Manually set up a flavor-content record"
    write (u, "(A)")      

    call state_flv%init (1, &
         mask = [.false., .false., .true., .true., .true.])
    call state_flv%set_entry (1, &
         pdg = [11, 12, 3, 4, 5], &
         map = [1, 2, 3, 4, 5])
    
    call state_flv%write (u)

    write (u, "(A)")      
    write (u, "(A)")  "* Manually create a matching particle set"
    write (u, "(A)")      

    pset%n_beam = 0
    pset%n_in   = 2
    pset%n_vir  = 0
    pset%n_out  = 3
    pset%n_tot  = 5
    allocate (pset%prt (pset%n_tot))
    do i = 1, 2
       call particle_reset_status (pset%prt(i), PRT_INCOMING)
       call particle_set_children (pset%prt(i), [3,4,5])
    end do
    do i = 3, 5
       call particle_reset_status (pset%prt(i), PRT_OUTGOING)
       call particle_set_parents (pset%prt(i), [1,2])
    end do
    call particle_set_momentum (pset%prt(1), vector4_at_rest (1._default))
    call particle_set_momentum (pset%prt(2), vector4_at_rest (2._default))
    call particle_set_momentum (pset%prt(3), vector4_at_rest (5._default))
    call particle_set_momentum (pset%prt(4), vector4_at_rest (4._default))
    call particle_set_momentum (pset%prt(5), vector4_at_rest (3._default))

    allocate (flv (5))
    call flavor_init (flv, [11,12,5,4,3])
    do i = 1, 5
       call particle_set_flavor (pset%prt(i), flv(i))
    end do

    call particle_set_write (pset, u)

    write (u, "(A)")      
    write (u, "(A)")  "*   Fill interaction from particle set"
    write (u, "(A)")

    call particle_set_fill_interaction (pset, int, 2, state_flv=state_flv)
    call interaction_write (int, u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call interaction_final (int)
    call particle_set_final (pset)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: particles_3"

  end subroutine particles_3
  
  subroutine particles_4 (u)

    integer, intent(in) :: u
    type(interaction_t) :: int
    type(state_flv_content_t) :: state_flv
    type(particle_set_t) :: pset
    type(flavor_t), dimension(:), allocatable :: flv
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    integer :: i, j
    
    write (u, "(A)")  "* Test output: Particles"
    write (u, "(A)")  "*   Purpose: reconstruct simple interaction"
    write (u, "(A)")      

    write (u, "(A)")  "* Set up a 2 -> 2 -> 3 interaction with radiation"
    write (u, "(A)")  "    + no quantum numbers"
    write (u, "(A)")      

    call reset_interaction_counter ()
    call interaction_init (int, 0, 6, 3)
    call interaction_relate (int, 1, 3)
    call interaction_relate (int, 1, 4)
    call interaction_relate (int, 2, 5)
    call interaction_relate (int, 2, 6)
    do i = 4, 6, 2
       do j = 7, 9
          call interaction_relate (int, i, j)
       end do
    end do

    allocate (qn (9))
    call interaction_add_state (int, qn)
    call interaction_freeze (int)

    call interaction_write (int, u)

    write (u, "(A)")      
    write (u, "(A)")  "* Manually set up a flavor-content record"
    write (u, "(A)")      

    call state_flv%init (1, &
         mask = [.false., .false., .false., .false., .false., .false., &
         .true., .true., .true.])
    call state_flv%set_entry (1, &
         pdg = [2011, 2012, 91, 11, 92, 12, 3, 4, 5], &
         map = [1, 2, 3, 4, 5, 6, 7, 8, 9])
    
    call state_flv%write (u)

    write (u, "(A)")      
    write (u, "(A)")  "* Manually create a matching particle set"
    write (u, "(A)")      

    pset%n_beam = 2
    pset%n_in   = 2
    pset%n_vir  = 2
    pset%n_out  = 3
    pset%n_tot  = 9

    allocate (pset%prt (pset%n_tot))
    call particle_reset_status (pset%prt(1), PRT_BEAM)
    call particle_reset_status (pset%prt(2), PRT_BEAM)
    call particle_reset_status (pset%prt(3), PRT_INCOMING)
    call particle_reset_status (pset%prt(4), PRT_INCOMING)
    call particle_reset_status (pset%prt(5), PRT_OUTGOING)
    call particle_reset_status (pset%prt(6), PRT_OUTGOING)
    call particle_reset_status (pset%prt(7), PRT_OUTGOING)
    call particle_reset_status (pset%prt(8), PRT_OUTGOING)
    call particle_reset_status (pset%prt(9), PRT_OUTGOING)

    call particle_set_children (pset%prt(1), [3,5])
    call particle_set_children (pset%prt(2), [4,6])
    call particle_set_children (pset%prt(3), [7,8,9])
    call particle_set_children (pset%prt(4), [7,8,9])

    call particle_set_parents (pset%prt(3), [1])
    call particle_set_parents (pset%prt(4), [2])
    call particle_set_parents (pset%prt(5), [1])
    call particle_set_parents (pset%prt(6), [2])
    call particle_set_parents (pset%prt(7), [5,6])
    call particle_set_parents (pset%prt(8), [5,6])
    call particle_set_parents (pset%prt(9), [5,6])

    call particle_set_momentum (pset%prt(1), vector4_at_rest (1._default))
    call particle_set_momentum (pset%prt(2), vector4_at_rest (2._default))
    call particle_set_momentum (pset%prt(3), vector4_at_rest (4._default))
    call particle_set_momentum (pset%prt(4), vector4_at_rest (6._default))
    call particle_set_momentum (pset%prt(5), vector4_at_rest (3._default))
    call particle_set_momentum (pset%prt(6), vector4_at_rest (5._default))
    call particle_set_momentum (pset%prt(7), vector4_at_rest (7._default))
    call particle_set_momentum (pset%prt(8), vector4_at_rest (8._default))
    call particle_set_momentum (pset%prt(9), vector4_at_rest (9._default))

    allocate (flv (9))
    call flavor_init (flv, [2011, 2012, 11, 12, 91, 92, 3, 4, 5])
    do i = 1, 9
       call particle_set_flavor (pset%prt(i), flv(i))
    end do

    call particle_set_write (pset, u)
 
    write (u, "(A)")      
    write (u, "(A)")  "*   Fill interaction from particle set"
    write (u, "(A)")

    call particle_set_fill_interaction (pset, int, 2, state_flv=state_flv)
    call interaction_write (int, u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call interaction_final (int)
    call particle_set_final (pset)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: particles_4"

  end subroutine particles_4
  
  subroutine particles_5 (u)

    integer, intent(in) :: u
    type(interaction_t) :: int
    type(interaction_t), target :: int_beams
    type(state_flv_content_t) :: state_flv
    type(particle_set_t) :: pset
    type(flavor_t), dimension(:), allocatable :: flv
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    integer :: i, j
    
    write (u, "(A)")  "* Test output: Particles"
    write (u, "(A)")  "*   Purpose: reconstruct beams"
    write (u, "(A)")      

    call reset_interaction_counter ()

    write (u, "(A)")  "* Set up an interaction that contains beams only"
    write (u, "(A)")      

    call interaction_init (int_beams, 0, 0, 2)
    call interaction_set_momentum (int_beams, &
         vector4_at_rest (1._default), 1)
    call interaction_set_momentum (int_beams, &
         vector4_at_rest (2._default), 2)
    allocate (qn (2))
    call interaction_add_state (int_beams, qn)
    call interaction_freeze (int_beams)
    
    call interaction_write (int_beams, u)

    write (u, "(A)")      
    write (u, "(A)")  "* Set up a 2 -> 2 -> 3 interaction with radiation"
    write (u, "(A)")  "    + no quantum numbers"
    write (u, "(A)")      

    call interaction_init (int, 0, 6, 3)
    call interaction_relate (int, 1, 3)
    call interaction_relate (int, 1, 4)
    call interaction_relate (int, 2, 5)
    call interaction_relate (int, 2, 6)
    do i = 4, 6, 2
       do j = 7, 9
          call interaction_relate (int, i, j)
       end do
    end do
    do i = 1, 2
       call interaction_set_source_link (int, i, int_beams, i)
    end do

    deallocate (qn)
    allocate (qn (9))
    call interaction_add_state (int, qn)
    call interaction_freeze (int)

    call interaction_write (int, u)

    write (u, "(A)")      
    write (u, "(A)")  "* Manually set up a flavor-content record"
    write (u, "(A)")      

    call state_flv%init (1, &
         mask = [.false., .false., .false., .false., .false., .false., &
         .true., .true., .true.])
    call state_flv%set_entry (1, &
         pdg = [2011, 2012, 91, 11, 92, 12, 3, 4, 5], &
         map = [1, 2, 3, 4, 5, 6, 7, 8, 9])
    
    call state_flv%write (u)

    write (u, "(A)")      
    write (u, "(A)")  "* Manually create a matching particle set"
    write (u, "(A)")      

    pset%n_beam = 0
    pset%n_in   = 2
    pset%n_vir  = 0
    pset%n_out  = 3
    pset%n_tot  = 5

    allocate (pset%prt (pset%n_tot))
    call particle_reset_status (pset%prt(1), PRT_INCOMING)
    call particle_reset_status (pset%prt(2), PRT_INCOMING)
    call particle_reset_status (pset%prt(3), PRT_OUTGOING)
    call particle_reset_status (pset%prt(4), PRT_OUTGOING)
    call particle_reset_status (pset%prt(5), PRT_OUTGOING)

    call particle_set_children (pset%prt(1), [3,4,5])
    call particle_set_children (pset%prt(2), [3,4,5])

    call particle_set_parents (pset%prt(3), [1,2])
    call particle_set_parents (pset%prt(4), [1,2])
    call particle_set_parents (pset%prt(5), [1,2])

    call particle_set_momentum (pset%prt(1), vector4_at_rest (6._default))
    call particle_set_momentum (pset%prt(2), vector4_at_rest (6._default))
    call particle_set_momentum (pset%prt(3), vector4_at_rest (3._default))
    call particle_set_momentum (pset%prt(4), vector4_at_rest (4._default))
    call particle_set_momentum (pset%prt(5), vector4_at_rest (5._default))

    allocate (flv (5))
    call flavor_init (flv, [11, 12, 3, 4, 5])
    do i = 1, 5
       call particle_set_flavor (pset%prt(i), flv(i))
    end do

    call particle_set_write (pset, u)
 
    write (u, "(A)")      
    write (u, "(A)")  "*   Fill interaction from particle set"
    write (u, "(A)")

    call particle_set_fill_interaction (pset, int, 2, state_flv=state_flv, &
         recover_beams = .true.)
    call interaction_write (int, u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call interaction_final (int)
    call particle_set_final (pset)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: particles_5"

  end subroutine particles_5
  
  subroutine particles_6 (u)

    integer, intent(in) :: u
    type(interaction_t) :: int
    type(state_flv_content_t) :: state_flv
    type(particle_set_t) :: pset
    type(flavor_t), dimension(:), allocatable :: flv
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    integer :: i, j
    
    write (u, "(A)")  "* Test output: Particles"
    write (u, "(A)")  "*   Purpose: reconstruct event with duplicate entries"
    write (u, "(A)")      

    write (u, "(A)")  "* Set up a 2 -> 2 -> 3 interaction with radiation"
    write (u, "(A)")  "    + no quantum numbers"
    write (u, "(A)")      

    call reset_interaction_counter ()
    call interaction_init (int, 0, 6, 3)
    call interaction_relate (int, 1, 3)
    call interaction_relate (int, 1, 4)
    call interaction_relate (int, 2, 5)
    call interaction_relate (int, 2, 6)
    do i = 4, 6, 2
       do j = 7, 9
          call interaction_relate (int, i, j)
       end do
    end do

    allocate (qn (9))
    call interaction_add_state (int, qn)
    call interaction_freeze (int)

    call interaction_write (int, u)

    write (u, "(A)")      
    write (u, "(A)")  "* Manually set up a flavor-content record"
    write (u, "(A)")      

    call state_flv%init (1, &
         mask = [.false., .false., .false., .false., .false., .false., &
         .true., .true., .true.])
    call state_flv%set_entry (1, &
         pdg = [2011, 2012, 91, 11, 92, 12, 3, 4, 5], &
         map = [1, 2, 3, 4, 5, 6, 7, 8, 9])
    
    call state_flv%write (u)

    write (u, "(A)")      
    write (u, "(A)")  "* Manually create a matching particle set"
    write (u, "(A)")      

    pset%n_beam = 2
    pset%n_in   = 2
    pset%n_vir  = 4
    pset%n_out  = 5
    pset%n_tot  = 13

    allocate (pset%prt (pset%n_tot))
    call particle_reset_status (pset%prt(1), PRT_BEAM)
    call particle_reset_status (pset%prt(2), PRT_BEAM)
    call particle_reset_status (pset%prt(3), PRT_VIRTUAL)
    call particle_reset_status (pset%prt(4), PRT_VIRTUAL)
    call particle_reset_status (pset%prt(5), PRT_VIRTUAL)
    call particle_reset_status (pset%prt(6), PRT_VIRTUAL)
    call particle_reset_status (pset%prt(7), PRT_INCOMING)
    call particle_reset_status (pset%prt(8), PRT_INCOMING)
    call particle_reset_status (pset%prt( 9), PRT_OUTGOING)
    call particle_reset_status (pset%prt(10), PRT_OUTGOING)
    call particle_reset_status (pset%prt(11), PRT_OUTGOING)
    call particle_reset_status (pset%prt(12), PRT_OUTGOING)
    call particle_reset_status (pset%prt(13), PRT_OUTGOING)

    call particle_set_children (pset%prt(1), [3,4])
    call particle_set_children (pset%prt(2), [5,6])
    call particle_set_children (pset%prt(3), [ 7])
    call particle_set_children (pset%prt(4), [ 9])
    call particle_set_children (pset%prt(5), [ 8])
    call particle_set_children (pset%prt(6), [10])
    call particle_set_children (pset%prt(7), [11,12,13])
    call particle_set_children (pset%prt(8), [11,12,13])

    call particle_set_parents (pset%prt(3), [1])
    call particle_set_parents (pset%prt(4), [1])
    call particle_set_parents (pset%prt(5), [2])
    call particle_set_parents (pset%prt(6), [2])
    call particle_set_parents (pset%prt( 7), [3])
    call particle_set_parents (pset%prt( 8), [5])
    call particle_set_parents (pset%prt( 9), [4])
    call particle_set_parents (pset%prt(10), [6])
    call particle_set_parents (pset%prt(11), [7,8])
    call particle_set_parents (pset%prt(12), [7,8])
    call particle_set_parents (pset%prt(13), [7,8])

    call particle_set_momentum (pset%prt(1), vector4_at_rest (1._default))
    call particle_set_momentum (pset%prt(2), vector4_at_rest (2._default))
    call particle_set_momentum (pset%prt(3), vector4_at_rest (4._default))
    call particle_set_momentum (pset%prt(4), vector4_at_rest (3._default))
    call particle_set_momentum (pset%prt(5), vector4_at_rest (6._default))
    call particle_set_momentum (pset%prt(6), vector4_at_rest (5._default))
    call particle_set_momentum (pset%prt(7), vector4_at_rest (4._default))
    call particle_set_momentum (pset%prt(8), vector4_at_rest (6._default))
    call particle_set_momentum (pset%prt( 9), vector4_at_rest (3._default))
    call particle_set_momentum (pset%prt(10), vector4_at_rest (5._default))
    call particle_set_momentum (pset%prt(11), vector4_at_rest (7._default))
    call particle_set_momentum (pset%prt(12), vector4_at_rest (8._default))
    call particle_set_momentum (pset%prt(13), vector4_at_rest (9._default))

    allocate (flv (13))
    call flavor_init (flv, &
         [2011, 2012, 11, 91, 12, 92, 11, 12, 91, 92, 3, 4, 5])
    do i = 1, 13
       call particle_set_flavor (pset%prt(i), flv(i))
    end do

    call particle_set_write (pset, u)
 
    write (u, "(A)")      
    write (u, "(A)")  "*   Fill interaction from particle set"
    write (u, "(A)")

    call particle_set_fill_interaction (pset, int, 2, state_flv=state_flv)
    call interaction_write (int, u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call interaction_final (int)
    call particle_set_final (pset)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: particles_6"

  end subroutine particles_6
  
  subroutine particles_7 (u)

    integer, intent(in) :: u
    type(interaction_t) :: int
    type(state_flv_content_t) :: state_flv
    type(particle_set_t) :: pset
    type(flavor_t), dimension(:), allocatable :: flv
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    integer :: i, j
    
    write (u, "(A)")  "* Test output: Particles"
    write (u, "(A)")  "*   Purpose: reconstruct interaction with pair spectrum"
    write (u, "(A)")      

    write (u, "(A)")  "* Set up a 2 -> 2 -> 3 interaction with radiation"
    write (u, "(A)")  "    + no quantum numbers"
    write (u, "(A)")      

    call reset_interaction_counter ()
    call interaction_init (int, 0, 6, 3)
    do i = 1, 2
       do j = 3, 6
          call interaction_relate (int, i, j)
       end do
    end do
    do i = 5, 6
       do j = 7, 9
          call interaction_relate (int, i, j)
       end do
    end do

    allocate (qn (9))
    call interaction_add_state (int, qn)
    call interaction_freeze (int)

    call interaction_write (int, u)

    write (u, "(A)")      
    write (u, "(A)")  "* Manually set up a flavor-content record"
    write (u, "(A)")      

    call state_flv%init (1, &
         mask = [.false., .false., .false., .false., .false., .false., &
         .true., .true., .true.])
    call state_flv%set_entry (1, &
         pdg = [1011, 1012, 21, 22, 11, 12, 3, 4, 5], &
         map = [1, 2, 3, 4, 5, 6, 7, 8, 9])
    
    call state_flv%write (u)

    write (u, "(A)")      
    write (u, "(A)")  "* Manually create a matching particle set"
    write (u, "(A)")      

    pset%n_beam = 2
    pset%n_in   = 2
    pset%n_vir  = 2
    pset%n_out  = 3
    pset%n_tot  = 9

    allocate (pset%prt (pset%n_tot))
    call particle_reset_status (pset%prt(1), PRT_BEAM)
    call particle_reset_status (pset%prt(2), PRT_BEAM)
    call particle_reset_status (pset%prt(3), PRT_INCOMING)
    call particle_reset_status (pset%prt(4), PRT_INCOMING)
    call particle_reset_status (pset%prt(5), PRT_OUTGOING)
    call particle_reset_status (pset%prt(6), PRT_OUTGOING)
    call particle_reset_status (pset%prt(7), PRT_OUTGOING)
    call particle_reset_status (pset%prt(8), PRT_OUTGOING)
    call particle_reset_status (pset%prt(9), PRT_OUTGOING)

    call particle_set_children (pset%prt(1), [3,4,5,6])
    call particle_set_children (pset%prt(2), [3,4,5,6])
    call particle_set_children (pset%prt(3), [7,8,9])
    call particle_set_children (pset%prt(4), [7,8,9])

    call particle_set_parents (pset%prt(3), [1,2])
    call particle_set_parents (pset%prt(4), [1,2])
    call particle_set_parents (pset%prt(5), [1,2])
    call particle_set_parents (pset%prt(6), [1,2])
    call particle_set_parents (pset%prt(7), [3,4])
    call particle_set_parents (pset%prt(8), [3,4])
    call particle_set_parents (pset%prt(9), [3,4])

    call particle_set_momentum (pset%prt(1), vector4_at_rest (1._default))
    call particle_set_momentum (pset%prt(2), vector4_at_rest (2._default))
    call particle_set_momentum (pset%prt(3), vector4_at_rest (5._default))
    call particle_set_momentum (pset%prt(4), vector4_at_rest (6._default))
    call particle_set_momentum (pset%prt(5), vector4_at_rest (3._default))
    call particle_set_momentum (pset%prt(6), vector4_at_rest (4._default))
    call particle_set_momentum (pset%prt(7), vector4_at_rest (7._default))
    call particle_set_momentum (pset%prt(8), vector4_at_rest (8._default))
    call particle_set_momentum (pset%prt(9), vector4_at_rest (9._default))

    allocate (flv (9))
    call flavor_init (flv, [1011, 1012, 11, 12, 21, 22, 3, 4, 5])
    do i = 1, 9
       call particle_set_flavor (pset%prt(i), flv(i))
    end do

    call particle_set_write (pset, u)
 
    write (u, "(A)")      
    write (u, "(A)")  "*   Fill interaction from particle set"
    write (u, "(A)")

    call particle_set_fill_interaction (pset, int, 2, state_flv=state_flv)
    call interaction_write (int, u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call interaction_final (int)
    call particle_set_final (pset)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: particles_7"

  end subroutine particles_7
  
  subroutine particles_8 (u)

    integer, intent(in) :: u
    type(interaction_t) :: int
    type(state_flv_content_t) :: state_flv
    type(particle_set_t) :: pset
    type(flavor_t), dimension(:), allocatable :: flv
    type(quantum_numbers_t), dimension(:), allocatable :: qn
    integer :: i, j
    
    write (u, "(A)")  "* Test output: Particles"
    write (u, "(A)")  "*   Purpose: reconstruct decay interaction with reordering"
    write (u, "(A)")      

    write (u, "(A)")  "* Set up a 1 -> 3 interaction"
    write (u, "(A)")  "    + no quantum numbers"
    write (u, "(A)")      

    call reset_interaction_counter ()
    call interaction_init (int, 0, 1, 3)
    do j = 2, 4
       call interaction_relate (int, 1, j)
    end do

    allocate (qn (4))
    call interaction_add_state (int, qn)
    call interaction_freeze (int)

    call interaction_write (int, u)

    write (u, "(A)")      
    write (u, "(A)")  "* Manually set up a flavor-content record"
    write (u, "(A)")  "*   assumed interaction: 6 12 5 -11"
    write (u, "(A)")      

    call state_flv%init (1, &
         mask = [.false., .true., .true., .true.])
    call state_flv%set_entry (1, &
         pdg = [6, 5, -11, 12], &
         map = [1, 4, 2, 3])
    
    call state_flv%write (u)

    write (u, "(A)")      
    write (u, "(A)")  "* Manually create a matching particle set"
    write (u, "(A)")      

    pset%n_beam = 0
    pset%n_in   = 1
    pset%n_vir  = 0
    pset%n_out  = 3
    pset%n_tot  = 4
    allocate (pset%prt (pset%n_tot))
    do i = 1, 1
       call particle_reset_status (pset%prt(i), PRT_INCOMING)
       call particle_set_children (pset%prt(i), [2,3,4])
    end do
    do i = 2, 4
       call particle_reset_status (pset%prt(i), PRT_OUTGOING)
       call particle_set_parents (pset%prt(i), [1])
    end do
    call particle_set_momentum (pset%prt(1), vector4_at_rest (1._default))
    call particle_set_momentum (pset%prt(2), vector4_at_rest (3._default))
    call particle_set_momentum (pset%prt(3), vector4_at_rest (2._default))
    call particle_set_momentum (pset%prt(4), vector4_at_rest (4._default))

    allocate (flv (5))
    call flavor_init (flv, [6,5,12,-11])
    do i = 1, 4
       call particle_set_flavor (pset%prt(i), flv(i))
    end do

    call particle_set_write (pset, u)

    write (u, "(A)")      
    write (u, "(A)")  "*   Fill interaction from particle set"
    write (u, "(A)")

    call particle_set_fill_interaction (pset, int, 1, state_flv=state_flv)
    call interaction_write (int, u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call interaction_final (int)
    call particle_set_final (pset)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: particles_8"

  end subroutine particles_8
  

end module particles
