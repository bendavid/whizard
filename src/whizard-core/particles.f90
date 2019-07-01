! WHIZARD 2.2.2 July 6 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Christian Speckner <cnspeckn@googlemail.com> 
!     and  Fabian Bach, Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use limits, only: FMT_16, FMT_17, FMT_19 !NODEP!
  use diagnostics !NODEP!
  use lorentz !NODEP!
  use unit_tests
  use subevents
  use expressions
  use models
  use flavors
  use colors
  use helicities
  use quantum_numbers
  use state_matrices
  use interactions
  use evaluators
  use polarizations
  use hepmc_interface

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
  public :: particle_set_extract_interaction
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
  end interface

  interface particle_set_init
     module procedure particle_set_init_interaction
     module procedure particle_set_init_hepmc
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
    type(model_t), intent(in), target :: model
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
    u = output_unit (unit);  if (u < 0)  return
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
    type(model_t), intent(in), target :: model
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
       (particle_set, evt, model, polarization)
    type(particle_set_t), intent(out) :: particle_set
    type(hepmc_event_t), intent(in) :: evt
    type(model_t), intent(in), target :: model
    integer, intent(in) :: polarization
    type(hepmc_event_particle_iterator_t) :: it
    type(hepmc_particle_t) :: prt
    integer, dimension(:), allocatable :: barcode
    integer :: n_tot, i
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

  subroutine particle_set_set_model (particle_set, model)
    type(particle_set_t), intent(inout) :: particle_set
    type(model_t), intent(in), target :: model
    integer :: i
    do i = 1, particle_set%n_tot
       call particle_set_model (particle_set%prt(i), model)
    end do
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
    u = output_unit (unit);  if (u < 0)  return
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
    if (state_matrix_is_defined (particle_set%correlated_state)) then
       call msg_bug ("Write particle set (raw): nontrivial state matrix not supported yet")
    else
       call state_matrix_write_raw (particle_set%correlated_state, u)
    end if
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
                    any( (/(map1(i)+j-1, j=1,n_parents)/) /= map1(parent(1:n_parents)))) &
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
          call particle_set_parents (pset_out%prt(i), (/ prt(i)%orig /))
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
          call particle_set_children (pset_out%prt(i), (/ prt(i)%copy /))
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

  subroutine particle_set_fill_interaction (pset, int, n_in, recover_beams)
    type(particle_set_t), intent(in) :: pset
    type(interaction_t), intent(inout) :: int
    integer, intent(in) :: n_in
    logical, intent(in), optional :: recover_beams
    logical, dimension(:), allocatable :: p_is_set
    logical :: recover
    integer, dimension(:), allocatable :: i_parent, j_parent
    integer :: n_tot, k
    n_tot = interaction_get_n_tot (int)
    recover = .false.;  if (present (recover_beams))  recover = recover_beams
    allocate (p_is_set (n_tot), source = .false.)
    allocate (i_parent (n_in), source = [(k, k = 1, n_in)])
    allocate (j_parent (n_in), source = i_parent)
    do k = 1, n_in
       call fill_subint (i_parent(k), j_parent(k))
    end do
    if (recover) then
       do k = 1, n_in
          call recover_beam_and_remnant (k)
       end do
    end if
    if (.not. all (p_is_set)) then
       call particle_set_write (pset)
       call interaction_write (int)
       call msg_fatal ("Mismatch between particle set and interaction")
    end if
  contains
    recursive subroutine fill_subint (i, j)
      integer, intent(in) :: i, j
      integer, dimension(:), allocatable :: i_child, j_child
      integer :: n_child, n_child_pset, k
      if (p_is_set(i))  return
      call interaction_set_momentum (int, &
           particle_get_momentum (pset%prt(j)), i)
      p_is_set(i) = .true.
      n_child = interaction_get_n_children (int, i)
      n_child_pset = particle_get_n_children (pset%prt(j))
      if (n_child /= 0 .and. n_child == n_child_pset) then
         allocate (i_child (n_child))
         allocate (j_child (n_child))
         i_child = interaction_get_children (int, i)
         j_child = particle_get_children (pset%prt(j))
         do k = 1, n_child
            call fill_subint (i_child(k), j_child(k))
         end do
      end if
    end subroutine fill_subint
    subroutine recover_beam_and_remnant (k)
      integer, intent(in) :: k
      integer :: k_src, k_in, k_rem
      type(interaction_t), pointer :: int_src
      integer, dimension(2) :: i_child
      call interaction_find_source (int, k, int_src, k_src)
      call interaction_set_momentum (int, &
           interaction_get_momentum (int_src, k_src), k)
      i_child = interaction_get_children (int, k)
      if (interaction_get_n_children (int, i_child(1)) > 0) then
         k_in = i_child(1);  k_rem = i_child(2)
      else
         k_in = i_child(2);  k_rem = i_child(1)
      end if
      call interaction_set_momentum (int, &
           interaction_get_momentum (int, k) &
           - interaction_get_momentum (int, k_in), k_rem)
    end subroutine recover_beam_and_remnant
  end subroutine particle_set_fill_interaction
    
  subroutine particle_set_extract_interaction (pset, int, flv_state)
    type(particle_set_t), intent(in) :: pset
    type(interaction_t), intent(inout) :: int
    integer, dimension(:,:), intent(in) :: flv_state
    integer :: n_in, n_out, n_tot
    integer, dimension(:), allocatable :: status, incoming, outgoing, index
    integer, dimension(:), allocatable :: pdg, perm
    integer :: i
    logical :: ok
    allocate (status (pset%n_tot))
    status = particle_get_status (pset%prt)
    n_in = count (status == PRT_INCOMING)
    allocate (incoming (n_in))
    incoming = pack ((/ (i, i = 1, pset%n_tot) /), status == PRT_INCOMING)
    i = incoming (1)
    n_out = particle_get_n_children (pset%prt(i))
    allocate (outgoing (n_out))
    outgoing = particle_get_children (pset%prt(i))
    n_tot = n_in + n_out
    if (n_in /= interaction_get_n_in (int) &
         .or. n_out /= interaction_get_n_out (int) &
         .or. n_tot /= interaction_get_n_tot (int)) then
       call msg_fatal &
            ("This event does not match the associated process (size)")
       return
    end if
    allocate (index (n_tot), pdg (n_tot), perm (n_tot))
    index(:n_in) = incoming
    index(n_in+1:) = outgoing
    pdg = particle_get_pdg (pset%prt(index))
    call find_flavor_ordering (flv_state, pdg, n_in, perm, ok)
    if (.not. ok) then
       call particle_set_write (pset)
       call msg_fatal &
            ("This event does not match the associated process (flavors)")
       return
    end if
    do i = 1, n_tot
       call interaction_set_momentum (int, &
            particle_get_momentum (pset%prt(i)), perm(i))
    end do
  end subroutine particle_set_extract_interaction

  subroutine find_flavor_ordering (flv_state, pdg, n_in, perm, ok)
    integer, dimension(:,:), intent(in) :: flv_state
    integer, dimension(:), intent(in) :: pdg
    integer, intent(in) :: n_in
    integer, dimension(:), intent(out) :: perm
    logical, intent(out) :: ok
    integer :: n_tot, f, i, j, k
    logical, dimension(:), allocatable :: found
    n_tot = size (pdg)
    if (size (flv_state, 1) /= n_tot) then
       ok = .false.
       return
    end if
    do i = 1, n_in
       perm(i) = i
    end do
    allocate (found (n_tot))
    ok = .false.
    do f = 1, size (flv_state, 2)
       call find_ordering_for_this_state (flv_state(:,f))
    end do
  contains
    subroutine find_ordering_for_this_state (pdg_state)    
      integer, dimension(:), intent(in) :: pdg_state
      found = .false.
      if (all (pdg_state(1:n_in) == pdg(1:n_in))) then
         SCAN_INPUT: do j = n_in + 1, n_tot
            SCAN_STATE: do k = n_in + 1, n_tot
              if (found(k))  cycle SCAN_STATE
              if (pdg_state(k) == pdg(j)) then
                 found(k) = .true.
                 perm(j) = k
                 cycle SCAN_INPUT
              end if
            end do SCAN_STATE
            return
         end do SCAN_INPUT
         ok = .true.
      end if
    end subroutine find_ordering_for_this_state
  end subroutine find_flavor_ordering

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
  end subroutine particles_test


  subroutine particles_1 (u)
    use os_interface
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model => null ()
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

    call syntax_model_file_init ()
    call os_data_init (os_data)    
    call model_list%read_model &
         (var_str("SM"), var_str("SM.mdl"), os_data, model)

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

!!! Raw I/O with state matrices currently disabled, cf. #627.
    !!! unit = free_unit ()
    !!! open (unit, action="readwrite", form="unformatted", status="scratch")
    !!! call particle_set_write_raw (particle_set3, unit)
    !!! rewind (unit)
    !!! call particle_set_read_raw (particle_set4, unit, iostat=iostat)
    !!! close (unit)
    !!! 
    !!! write (u, "(A)")
    !!! call particle_set_write (particle_set4, u)
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
    call model_list%final ()
    call syntax_model_file_final ()
       
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: particles_1"        
    
  end subroutine particles_1

  subroutine particles_2 (u)
    use os_interface
    integer, intent(in) :: u
    type(os_data_t) :: os_data
    type(model_list_t) :: model_list
    type(model_t), pointer :: model => null ()
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

    call syntax_model_file_init ()
    call os_data_init (os_data)    
    call model_list%read_model &
         (var_str("SM"), var_str("SM.mdl"), os_data, model)

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
         hepmc_event, model, PRT_DEFINITE_HELICITY)
    call particle_set_write (particle_set2, u)   

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call particle_set_final (particle_set1)
    call particle_set_final (particle_set2)
    call evaluator_final (eval)
    call interaction_final (int1)
    call interaction_final (int2)
    call hepmc_event_final (hepmc_event)            
    call model_list%final ()
    call syntax_model_file_final ()
       
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: particles_2"

  end subroutine particles_2
  

end module particles
