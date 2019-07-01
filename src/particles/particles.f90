! WHIZARD 2.2.5 Feb 27 2015
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
  use subevents
  use polarizations

  implicit none
  private

  public :: particle_t
  public :: particle_set_t
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
   contains
    generic :: init => init_state
    procedure :: init_state => particle_init_state
    procedure :: final => particle_final
    procedure :: write => particle_write
    procedure :: write_raw => particle_write_raw
    procedure :: read_raw => particle_read_raw  
    procedure :: reset_status => particle_reset_status
    procedure :: set_color => particle_set_color
    procedure :: set_flavor => particle_set_flavor
    procedure :: set_helicity => particle_set_helicity
    procedure :: set_pol => particle_set_pol
    procedure :: set_model => particle_set_model
    procedure :: set_momentum => particle_set_momentum
    procedure :: set_children => particle_set_children
    procedure :: set_parents => particle_set_parents
    procedure :: add_child => particle_add_child
    procedure :: set_status => particle_set_status
    procedure :: set_polarization => particle_set_polarization
    procedure :: get_status => particle_get_status
    procedure :: is_real => particle_is_real
    procedure :: get_polarization_status => particle_get_polarization_status
    procedure :: get_pdg => particle_get_pdg
    procedure :: get_color => particle_get_color
    procedure :: get_polarization => particle_get_polarization
    procedure :: get_flv => particle_get_flv
    procedure :: get_col => particle_get_col 
    procedure :: get_hel => particle_get_hel
    procedure :: get_helicity => particle_get_helicity
    procedure :: get_n_parents => particle_get_n_parents
    procedure :: get_n_children => particle_get_n_children
    procedure :: get_parents => particle_get_parents
    procedure :: get_children => particle_get_children
    procedure :: get_momentum => particle_get_momentum
    procedure :: get_p2 => particle_get_p2  
  end type particle_t

  type :: particle_set_t
     integer :: n_beam = 0
     integer :: n_in  = 0
     integer :: n_vir = 0
     integer :: n_out = 0
     integer :: n_tot = 0
     type(particle_t), dimension(:), allocatable :: prt
     type(state_matrix_t) :: correlated_state
   contains
     generic :: init => init_interaction
     procedure :: init_interaction => particle_set_init_interaction
     procedure :: set_model => particle_set_set_model
     procedure :: final => particle_set_final
     procedure :: write => particle_set_write
     procedure :: write_raw => particle_set_write_raw 
     procedure :: read_raw => particle_set_read_raw 
     procedure :: get_real_parents => particle_set_get_real_parents
     procedure :: get_real_children => particle_set_get_real_children
     procedure :: get_n_beam => particle_set_get_n_beam
     procedure :: get_n_in => particle_set_get_n_in
     procedure :: get_n_vir => particle_set_get_n_vir
     procedure :: get_n_out => particle_set_get_n_out
     procedure :: get_n_tot => particle_set_get_n_tot
     procedure :: get_particle => particle_set_get_particle
     procedure :: reset_status => particle_set_reset_status
     procedure :: reduce => particle_set_reduce
     procedure :: apply_keep_beams => particle_set_apply_keep_beams
     procedure :: to_hepevt_form => particle_set_to_hepevt_form
     procedure :: fill_interaction => particle_set_fill_interaction
     procedure :: assign_vertices => particle_set_assign_vertices
     procedure :: to_subevt => particle_set_to_subevt
     procedure :: replace => particle_set_replace
  end type particle_set_t


  interface pacify
     module procedure pacify_particle
     module procedure pacify_particle_set
  end interface pacify


contains

  subroutine particle_init_particle (prt_out, prt_in)
    type(particle_t), intent(in) :: prt_in
    class(particle_t), intent(out) :: prt_out
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
    class(particle_t), intent(out) :: prt
    type(state_matrix_t), intent(in) :: state
    integer, intent(in) :: status, mode
    type(state_iterator_t) :: it
    prt%status = status
    call it%init (state)
    prt%flv = it%get_flavor (1)
    if (prt%flv%is_radiated ())  prt%status = PRT_BEAM_REMNANT
    prt%col = it%get_color (1)
    select case (mode)
    case (FM_SELECT_HELICITY)
       prt%hel = it%get_helicity (1)
       if (prt%hel%is_defined ()) then
          prt%polarization = PRT_DEFINITE_HELICITY
       end if
    case (FM_FACTOR_HELICITY)
       call polarization_init_state_matrix (prt%pol, state)
       prt%polarization = PRT_GENERIC_POLARIZATION
    end select
  end subroutine particle_init_state

  subroutine particle_final (prt)
    class(particle_t), intent(inout) :: prt
    call polarization_final (prt%pol)
  end subroutine particle_final

  subroutine particle_write (prt, unit, testflag)
    class(particle_t), intent(in) :: prt
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
    call prt%flv%write (unit)
    if (prt%col%is_nonzero ()) then
       call color_write (prt%col, unit)
    end if
    select case (prt%polarization)
    case (PRT_DEFINITE_HELICITY)
       call prt%hel%write (unit)
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
    class(particle_t), intent(in) :: prt
    integer, intent(in) :: u
    write (u) prt%status, prt%polarization
    call prt%flv%write_raw (u)
    call prt%col%write_raw (u)
    select case (prt%polarization)
    case (PRT_DEFINITE_HELICITY)
       call prt%hel%write_raw (u)
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
    class(particle_t), intent(out) :: prt
    integer, intent(in) :: u
    integer, intent(out) :: iostat
    logical :: allocated_parent, allocated_child
    integer :: size_parent, size_child
    read (u, iostat=iostat) prt%status, prt%polarization
    call prt%flv%read_raw (u, iostat=iostat)
    call prt%col%read_raw (u, iostat=iostat)
    select case (prt%polarization)
    case (PRT_DEFINITE_HELICITY)
       call prt%hel%read_raw (u, iostat=iostat)
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
    class(particle_t), intent(inout) :: prt
    integer, intent(in) :: status
    prt%status = status
    select case (status)
    case (PRT_BEAM, PRT_INCOMING, PRT_OUTGOING)
       prt%p2 = prt%flv%get_mass () ** 2
    end select
  end subroutine particle_reset_status

  elemental subroutine particle_set_color (prt, col)
    class(particle_t), intent(inout) :: prt
    type(color_t), intent(in) :: col
    prt%col = col
  end subroutine particle_set_color

  subroutine particle_set_flavor (prt, flv)
    class(particle_t), intent(inout) :: prt
    type(flavor_t), intent(in) :: flv
    prt%flv = flv
  end subroutine particle_set_flavor

  subroutine particle_set_helicity (prt, hel)
    class(particle_t), intent(inout) :: prt
    type(helicity_t), intent(in) :: hel
    prt%hel = hel
  end subroutine particle_set_helicity

  subroutine particle_set_pol (prt, pol)
    class(particle_t), intent(inout) :: prt
    type(polarization_t), intent(in) :: pol
    prt%pol = pol
  end subroutine particle_set_pol

  subroutine particle_set_model (prt, model)
    class(particle_t), intent(inout) :: prt
    class(model_data_t), intent(in), target :: model
    call prt%flv%set_model (model)
  end subroutine particle_set_model
  
  elemental subroutine particle_set_momentum (prt, p, p2, on_shell)
    class(particle_t), intent(inout) :: prt
    type(vector4_t), intent(in) :: p
    real(default), intent(in), optional :: p2
    logical, intent(in), optional :: on_shell
    prt%p = p
    if (present (on_shell)) then
       if (on_shell) then
          if (prt%flv%is_associated ()) then
             prt%p2 = prt%flv%get_mass () ** 2
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
    class(particle_t), intent(inout) :: prt
    logical, intent(in) :: resonant
    select case (prt%status)
    case (PRT_VIRTUAL)
       if (resonant)  prt%status = PRT_RESONANT
    end select
  end subroutine particle_set_resonance_flag

  subroutine particle_set_children (prt, idx)
    class(particle_t), intent(inout) :: prt
    integer, dimension(:), intent(in) :: idx
    if (allocated (prt%child))  deallocate (prt%child)
    allocate (prt%child (count (idx /= 0)))
    prt%child = pack (idx, idx /= 0)
  end subroutine particle_set_children

  subroutine particle_set_parents (prt, idx)
    class(particle_t), intent(inout) :: prt
    integer, dimension(:), intent(in) :: idx
    if (allocated (prt%parent))  deallocate (prt%parent) 
    allocate (prt%parent (count (idx /= 0)))
    prt%parent = pack (idx, idx /= 0)
  end subroutine particle_set_parents

  subroutine particle_add_child (prt, new_child)
    class(particle_t), intent(inout) :: prt
    integer, intent(in) :: new_child
    integer, dimension(:), allocatable :: idx
    integer :: n
    n = prt%get_n_children()
    if (n == 0) then
       call prt%set_children ([new_child])
    else
       allocate (idx (1:n+1))
       idx(1:n) = prt%get_children ()
       idx(n+1) = new_child
       call prt%set_children (idx)
    end if
  end subroutine particle_add_child
  elemental subroutine particle_set_status (prt, status)
    class(particle_t), intent(inout) :: prt
    integer, intent(in) :: status
    prt%status = status
  end subroutine particle_set_status

  subroutine particle_set_polarization (prt, polarization)
    class(particle_t), intent(inout) :: prt
    integer, intent(in) :: polarization
    prt%polarization = polarization
  end subroutine particle_set_polarization

  elemental function particle_get_status (prt) result (status)
    integer :: status
    class(particle_t), intent(in) :: prt
    status = prt%status
  end function particle_get_status

  elemental function particle_is_real (prt, keep_beams) result (flag)
    logical :: flag, kb
    class(particle_t), intent(in) :: prt
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
    class(particle_t), intent(in) :: prt
    status = prt%polarization
  end function particle_get_polarization_status

  elemental function particle_get_pdg (prt) result (pdg)
    integer :: pdg
    class(particle_t), intent(in) :: prt
    pdg = prt%flv%get_pdg ()
  end function particle_get_pdg

  pure function particle_get_color (prt) result (col)
    integer, dimension(2) :: col
    class(particle_t), intent(in) :: prt
    col(1) = prt%col%get_col ()
    col(2) = prt%col%get_acl ()
  end function particle_get_color
  
  function particle_get_polarization (prt) result (pol)
    class(particle_t), intent(in) :: prt
    class(polarization_t), allocatable :: pol
    pol = prt%pol
  end function particle_get_polarization

  function particle_get_flv (prt) result (flv)
    class(particle_t), intent(in) :: prt
    type(flavor_t) :: flv
    flv = prt%flv
  end function particle_get_flv 
  
  function particle_get_col (prt) result (col)
    class(particle_t), intent(in) :: prt
    type(color_t) :: col
    col = prt%col
  end function particle_get_col
  
  function particle_get_hel (prt) result (hel)
    class(particle_t), intent(in) :: prt
    type(helicity_t) :: hel
    hel = prt%hel
  end function particle_get_hel
  
  elemental function particle_get_helicity (prt) result (hel)
    integer :: hel
    integer, dimension(2) :: hel_arr
    class(particle_t), intent(in) :: prt    
    hel = 0
    if (prt%hel%is_defined () .and. prt%hel%is_diagonal ()) then
       hel_arr = prt%hel%to_pair ()
       hel = hel_arr (1)
    end if
  end function particle_get_helicity  
  
  elemental function particle_get_n_parents (prt) result (n)
    integer :: n
    class(particle_t), intent(in) :: prt
    if (allocated (prt%parent)) then
       n = size (prt%parent)
    else
       n = 0
    end if
  end function particle_get_n_parents
    
  elemental function particle_get_n_children (prt) result (n)
    integer :: n
    class(particle_t), intent(in) :: prt
    if (allocated (prt%child)) then
       n = size (prt%child)
    else
       n = 0
    end if
  end function particle_get_n_children
    
  function particle_get_parents (prt) result (parent)
    class(particle_t), intent(in) :: prt
    integer, dimension(:), allocatable :: parent
    if (allocated (prt%parent)) then
       allocate (parent (size (prt%parent)))
       parent = prt%parent
    else
       allocate (parent (0))
    end if
  end function particle_get_parents

  function particle_get_children (prt) result (child)
    class(particle_t), intent(in) :: prt
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
    class(particle_t), intent(in) :: prt
    p = prt%p
  end function particle_get_momentum

  elemental function particle_get_p2 (prt) result (p2)
    real(default) :: p2
    class(particle_t), intent(in) :: prt
    p2 = prt%p2
  end function particle_get_p2

  subroutine particle_set_init_interaction &
       (particle_set, is_valid, int, int_flows, mode, x, &
        keep_correlations, keep_virtual, n_incoming)
    class(particle_set_t), intent(out) :: particle_set
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
       qn(i,:) = flavor_state(i)%get_quantum_numbers (1)
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
          call particle_set%prt(j)%init (single_state(i), PRT_INCOMING, mode)
          call particle_set%prt(j)%set_momentum &
               (interaction_get_momentum (int, i))
       else if (i <= n_in + n_vir) then
          if (.not. keep_virtual)  cycle
          call particle_set%prt(j)%init &
               (single_state(i), PRT_VIRTUAL, mode) 
          call particle_set%prt(j)%set_momentum &
               (interaction_get_momentum (int, i))
       else
          call particle_set%prt(j)%init (single_state(i), PRT_OUTGOING, mode)
          call particle_set%prt(j)%set_momentum &
               (interaction_get_momentum (int, i), on_shell = .true.)
       end if
       if (keep_virtual) then
          call particle_set%prt(j)%set_children &
               (interaction_get_children (int, i))
          call particle_set%prt(j)%set_parents &
               (interaction_get_parents (int, i))
       end if
       j = j + 1
    end do
    if (keep_virtual) then
       call particle_set_resonance_flag &
            (particle_set%prt, interaction_get_resonance_flags (int))
    end if
    do i = i, size(flavor_state)
       call flavor_state(i)%final ()
    end do
    do i = i, size(single_state)
       call single_state(i)%final ()
    end do
  end subroutine particle_set_init_interaction

  subroutine particle_set_set_model (particle_set, model)
    class(particle_set_t), intent(inout) :: particle_set
    class(model_data_t), intent(in), target :: model
    integer :: i
    do i = 1, particle_set%n_tot
       call particle_set%prt(i)%set_model (model)
    end do
    call particle_set%correlated_state%set_model (model)
  end subroutine particle_set_set_model
    
  subroutine particle_set_final (particle_set)
    class(particle_set_t), intent(inout) :: particle_set
    integer :: i
    if (allocated (particle_set%prt)) then
       do i = 1, size(particle_set%prt)
          call particle_set%prt(i)%final ()
       end do
       deallocate (particle_set%prt)
    end if
    call particle_set%correlated_state%final ()
  end subroutine particle_set_final

  subroutine particle_set_write (particle_set, unit, testflag)
    class(particle_set_t), intent(in) :: particle_set
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: testflag
    integer :: u, i
    u = given_output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A)") "Particle set:"
    call write_separator (u)
    if (particle_set%n_tot /= 0) then
       do i = 1, particle_set%n_tot
          write (u, "(1x,A,1x,I0)", advance="no") "Particle", i
          call particle_set%prt(i)%write (u, testflag)
       end do
       if (particle_set%correlated_state%is_defined ()) then
          call write_separator (u)
          write (u, *) "Correlated state density matrix:"
          call particle_set%correlated_state%write (u)
       end if
    else
       write (u, "(3x,A)") "[empty]"
    end if
  end subroutine particle_set_write

  subroutine particle_set_write_raw (particle_set, u)
    class(particle_set_t), intent(in) :: particle_set
    integer, intent(in) :: u
    integer :: i
    write (u) &
         particle_set%n_beam, particle_set%n_in, &
         particle_set%n_vir, particle_set%n_out
    write (u) particle_set%n_tot
    do i = 1, particle_set%n_tot
       call particle_set%prt(i)%write_raw (u)
    end do
    call particle_set%correlated_state%write_raw (u)
  end subroutine particle_set_write_raw

  subroutine particle_set_read_raw (particle_set, u, iostat)
    class(particle_set_t), intent(out) :: particle_set
    integer, intent(in) :: u
    integer, intent(out) :: iostat
    integer :: i
    read (u, iostat=iostat) &
         particle_set%n_beam, particle_set%n_in, &
         particle_set%n_vir, particle_set%n_out
    read (u, iostat=iostat) particle_set%n_tot
    allocate (particle_set%prt (particle_set%n_tot))
    do i = 1, size (particle_set%prt)
       call particle_set%prt(i)%read_raw (u, iostat=iostat)
    end do
    call particle_set%correlated_state%read_raw (u, iostat=iostat)
  end subroutine particle_set_read_raw

  function particle_set_get_real_parents (pset, i, keep_beams) result (parent)
    integer, dimension(:), allocatable :: parent
    class(particle_set_t), intent(in) :: pset
    integer, intent(in) :: i
    logical, intent(in), optional :: keep_beams
    logical, dimension(:), allocatable :: is_real
    logical, dimension(:), allocatable :: is_parent, is_real_parent
    logical :: kb
    integer :: j, k
    kb = .false.
    if (present (keep_beams)) kb = keep_beams
    allocate (is_real (pset%n_tot))
    is_real = pset%prt%is_real (kb)
    allocate (is_parent (pset%n_tot), is_real_parent (pset%n_tot))
    is_real_parent = .false.
    is_parent = .false.
    is_parent(pset%prt(i)%get_parents()) = .true.
    do while (any (is_parent))
       where (is_real .and. is_parent)
          is_real_parent = .true.
          is_parent = .false.
       end where
       mark_next_parent: do j = size (is_parent), 1, -1
          if (is_parent(j)) then
             is_parent(pset%prt(j)%get_parents()) = .true.
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
    class(particle_set_t), intent(in) :: pset
    integer, intent(in) :: i
    logical, dimension(:), allocatable :: is_real
    logical, dimension(:), allocatable :: is_child, is_real_child
    logical, intent(in), optional :: keep_beams
    integer :: j, k
    logical :: kb
    kb = .false.
    if (present (keep_beams)) kb = keep_beams
    allocate (is_real (pset%n_tot))
    is_real = pset%prt%is_real (kb)
    allocate (is_child (pset%n_tot), is_real_child (pset%n_tot))
    is_real_child = .false.
    is_child = .false.
    is_child(pset%prt(i)%get_children()) = .true.
    do while (any (is_child))
       where (is_real .and. is_child)
          is_real_child = .true.
          is_child = .false.
       end where
       mark_next_child: do j = 1, size (is_child)
          if (is_child(j)) then
             is_child(pset%prt(j)%get_children()) = .true.
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
     class(particle_set_t), intent(in) :: pset
     integer :: n_beam
     n_beam = pset%n_beam
  end function particle_set_get_n_beam

  function particle_set_get_n_in (pset) result (n_in)
     class(particle_set_t), intent(in) :: pset
     integer :: n_in
     n_in = pset%n_in
  end function particle_set_get_n_in

  function particle_set_get_n_vir (pset) result (n_vir)
     class(particle_set_t), intent(in) :: pset
     integer :: n_vir
     n_vir = pset%n_vir
   end function particle_set_get_n_vir

  function particle_set_get_n_out (pset) result (n_out)
     class(particle_set_t), intent(in) :: pset
     integer :: n_out
     n_out = pset%n_out
  end function particle_set_get_n_out
  
  function particle_set_get_n_tot (pset) result (n_tot)
     class(particle_set_t), intent(in) :: pset
     integer :: n_tot
     n_tot = pset%n_tot
  end function particle_set_get_n_tot

  function particle_set_get_n_rad (pset) result (n_rad)
    class(particle_set_t), intent(in) :: pset
    integer :: n_rad
    n_rad = count (pset%prt%get_status () == PRT_BEAM_REMNANT)
  end function particle_set_get_n_rad

  function particle_set_get_particle (pset, index) result (particle)
    class(particle_set_t), intent(in) :: pset
    integer, intent(in) :: index
    type(particle_t) :: particle
    particle = pset%prt(index)
  end function particle_set_get_particle

  subroutine particle_set_reset_status (particle_set, index, status)
    class(particle_set_t), intent(inout) :: particle_set
    integer, dimension(:), intent(in) :: index
    integer, intent(in) :: status
    integer :: i
    if (allocated (particle_set%prt)) then
       do i = 1, size (index)
          call particle_set%prt(index(i))%reset_status (status)
       end do
    end if
    particle_set%n_beam  = &
         count (particle_set%prt%get_status () == PRT_BEAM)
    particle_set%n_in  = &
         count (particle_set%prt%get_status () == PRT_INCOMING)
    particle_set%n_out = &
         count (particle_set%prt%get_status () == PRT_OUTGOING)
    particle_set%n_vir = particle_set%n_tot &
         - particle_set%n_beam - particle_set%n_in - particle_set%n_out
  end subroutine particle_set_reset_status

  subroutine particle_set_reduce (pset_in, pset_out, keep_beams)
    class(particle_set_t), intent(in) :: pset_in
    type(particle_set_t), intent(out) :: pset_out
    logical, intent(in), optional :: keep_beams
    integer, dimension(:), allocatable :: status, map
    integer :: i, j
    logical :: kb
    kb = .false.;  if (present (keep_beams))  kb = keep_beams
    allocate (status (pset_in%n_tot))    
    status = pset_in%prt%get_status ()
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
       call pset_out%prt(map(i))%set_parents &
            (pset_in%get_real_parents (i, kb))
       call pset_out%prt(map(i))%set_parents &
            (map (pset_out%prt(map(i))%parent))
       call pset_out%prt(map(i))%set_children &
            (pset_in%get_real_children (i, kb))
       call pset_out%prt(map(i))%set_children &
            (map (pset_out%prt(map(i))%child))
    end do
  contains
    subroutine copy_particles (stat)
      integer, intent(in) :: stat
      integer :: i
      do i = 1, pset_in%n_tot
         if (status(i) == stat) then
            j = j + 1
            map(i) = j
            call particle_init_particle (pset_out%prt(j), pset_in%prt(i))
         end if
      end do
    end subroutine copy_particles
  end subroutine particle_set_reduce

  subroutine particle_set_apply_keep_beams (pset_in, pset_out, keep_beams)
    class(particle_set_t), intent(in) :: pset_in
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
       pset_out%n_vir = count (status == PRT_VIRTUAL) + &
            count (status == PRT_RESONANT) + &
            count (status == PRT_BEAM_REMNANT)
    else 
       pset_out%n_vir = count (status == PRT_VIRTUAL) + &
            count (status == PRT_RESONANT)
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
       call pset_out%prt(map(i))%set_parents &
            (pset_in%get_real_parents (i, kb))
       call pset_out%prt(map(i))%set_parents &
            (map (pset_out%prt(map(i))%parent))
       call pset_out%prt(map(i))%set_children &
            (pset_in%get_real_children (i, kb))
       call pset_out%prt(map(i))%set_children &
            (map (pset_out%prt(map(i))%child))
    end do
  contains
    subroutine copy_particles (stat)
      integer, intent(in) :: stat
      integer :: i
      do i = 1, pset_in%n_tot
         if (status(i) == stat) then
            j = j + 1
            map(i) = j
            call particle_init_particle (pset_out%prt(j), pset_in%prt(i))
         end if
      end do
    end subroutine copy_particles
  end subroutine particle_set_apply_keep_beams

  subroutine particle_set_to_hepevt_form (pset_in, pset_out, keep_beams)
    class(particle_set_t), intent(in) :: pset_in
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

    call pset_in%apply_keep_beams (pset, keep_beams)
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
       if (pset%prt(i)%get_n_parents () == 0) then
          call append (i)
       end if
    end do
    do i = 1, n_tot
       n_children = pset%prt(i)%get_n_children ()
       if (n_children > 0) then
          child(1:n_children) = pset%prt(i)%get_children ()
          c = child(1)
          if (map1(c) == 0) then
             n_parents = pset%prt(c)%get_n_parents ()
             if (n_parents > 1) then
                parent(1:n_parents) = pset%prt(c)%get_parents ()
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
       call particle_init_particle (pset_out%prt(i), pset%prt(prt(i)%src))
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
      prt(n)%status = pset%prt(i)%get_status ()
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
    class(particle_set_t), intent(in) :: pset
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
               pset%prt(p)%get_momentum (), map(p))
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
         if (pset%prt(pp(1))%get_n_children () > 1)  exit
         q = pp(1)
         n_p = pset%prt(q)%get_n_parents ()
      end do
      if (n_p /= n)  call err_map
      allocate (i_parents (n), p_parents (n))
      i_parents = interaction_get_parents (int, i)
      p_parents = pset%prt(q)%get_parents ()
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
      n_p = pset%prt(p)%get_n_children ()
      if (n_p /= n)  call err_map
      allocate (i_children (n), p_children (n))
      i_children = interaction_get_children (int, i)
      p_children = pset%prt(p)%get_children ()
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
      call pset%write ()
      call state_flv%write ()
      call msg_fatal ("Reading particle set: Flavor combination " &
           // "does not match requested process")
    end subroutine err_mismatch
    subroutine err_map
      call pset%write ()
      call interaction_write (int)
      call msg_fatal ("Reading hard-process particle set: " &
           // "Incomplete mapping from particle set to interaction")
    end subroutine err_map
    subroutine err_beams
      call pset%write ()
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
    class(particle_set_t), intent(in) :: particle_set
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
       n_parents = particle_set%prt(i)%get_n_parents ()
       if (n_parents /= 0) then
          allocate (parent (n_parents))
          parent = particle_set%prt(i)%get_parents ()
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
       n_children = particle_set%prt(i)%get_n_children ()
       if (n_children /= 0) then
          allocate (child (n_children))
          child = particle_set%prt(i)%get_children ()
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
    class(particle_set_t), intent(in) :: particle_set
    type(subevt_t), intent(out) :: subevt
    integer :: n_tot, n_beam, n_in, n_out, n_rad
    integer :: i, k, n_active
    integer, dimension(2) :: hel
    logical :: keep
    n_tot  = particle_set_get_n_tot  (particle_set)
    n_beam = particle_set_get_n_beam (particle_set)
    n_in   = particle_set_get_n_in   (particle_set)
    n_out  = particle_set_get_n_out  (particle_set)
    n_rad  = particle_set_get_n_rad  (particle_set)
    call subevt_init (subevt, n_beam + n_rad + n_in + n_out)
    k = 0
    do i = 1, n_tot
       associate (prt => particle_set%prt(i))
         keep = .false.
         select case (particle_get_status (prt))
         case (PRT_BEAM)
            k = k + 1
            call subevt_set_beam (subevt, k, &
                 particle_get_pdg (prt), &
                 particle_get_momentum (prt), &
                 particle_get_p2 (prt))
            keep = .true.
         case (PRT_INCOMING)
            k = k + 1
            call subevt_set_incoming (subevt, k, &
                 particle_get_pdg (prt), &
                 particle_get_momentum (prt), &
                 particle_get_p2 (prt))
            keep = .true.
         case (PRT_OUTGOING)
            k = k + 1
            call subevt_set_outgoing (subevt, k, &
                 particle_get_pdg (prt), &
                 particle_get_momentum (prt), &
                 particle_get_p2 (prt))
            keep = .true.
         case (PRT_BEAM_REMNANT)
            if (particle_get_n_children (prt) == 0) then
               k = k + 1
               call subevt_set_outgoing (subevt, k, &
                    particle_get_pdg (prt), &
                    particle_get_momentum (prt), &
                    particle_get_p2 (prt))
               keep = .true.
            end if
         end select
         if (keep) then
            if (prt%polarization == PRT_DEFINITE_HELICITY) then
               if (prt%hel%is_diagonal ()) then
                  hel = prt%hel%to_pair ()
                  call subevt_polarize (subevt, k, hel(1))
               end if
            end if
         end if
       end associate
       n_active = k
    end do
    call subevt_reset (subevt, n_active)
  end subroutine particle_set_to_subevt

  subroutine particle_set_replace (particle_set, newprt)
    class(particle_set_t), intent(inout) :: particle_set
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
    call test (particles_2, "particles_2", &
         "reconstruct hard interaction", &
         u, results)
    call test (particles_3, "particles_3", &
         "reconstruct interaction with beam structure", &
         u, results)
    call test (particles_4, "particles_4", &
         "reconstruct interaction with missing beams", &
         u, results)
    call test (particles_5, "particles_5", &
         "reconstruct interaction with beams and duplicate entries", &
         u, results)
    call test (particles_6, "particles_6", &
         "reconstruct interaction with pair spectrum", &
         u, results)
    call test (particles_7, "particles_7", &
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
    call flv%init ([1, -1, 23], model)
    call col%init_col_acl ([0, 0, 0], [0, 0, 0])
    call hel(3)%init (1, 1)
    call qn%init (flv, col, hel)
    call interaction_add_state (int1, qn, value=(0.25_default, 0._default))
    call hel(3)%init (1,-1)
    call qn%init (flv, col, hel)
    call interaction_add_state (int1, qn, value=(0._default, 0.25_default))
    call hel(3)%init (-1, 1)
    call qn%init (flv, col, hel)
    call interaction_add_state (int1, qn, value=(0._default,-0.25_default))
    call hel(3)%init (-1,-1)
    call qn%init (flv, col, hel)
    call interaction_add_state (int1, qn, value=(0.25_default, 0._default))
    call hel(3)%init (0, 0)
    call qn%init (flv, col, hel)
    call interaction_add_state (int1, qn, value=(0.5_default, 0._default))
    call interaction_freeze (int1)
    p(1) = vector4_moving (45._default, 45._default, 3)
    p(2) = vector4_moving (45._default,-45._default, 3)
    p(3) = p(1) + p(2)
    call interaction_set_momenta (int1, p)

    write (u, "(A)")
    write (u, "(A)")  "* Setup decay process"

    call interaction_init (int2, 1, 0, 2, set_relations=.true.)
    call flv%init ([23, 1, -1], model)
    call col%init_col_acl ([0, 501, 0], [0, 0, 501])
    call hel%init ([1, 1, 1], [1, 1, 1])
    call qn%init (flv, col, hel)
    call interaction_add_state (int2, qn, value=(1._default, 0._default))
    call hel%init ([1, 1, 1], [-1,-1,-1])
    call qn%init (flv, col, hel)
    call interaction_add_state (int2, qn, value=(0._default, 0.1_default))
    call hel%init ([-1,-1,-1], [1, 1, 1])
    call qn%init (flv, col, hel)
    call interaction_add_state (int2, qn, value=(0._default,-0.1_default))
    call hel%init ([-1,-1,-1], [-1,-1,-1])
    call qn%init (flv, col, hel)
    call interaction_add_state (int2, qn, value=(1._default, 0._default))
    call hel%init ([0, 1,-1], [0, 1,-1])
    call qn%init (flv, col, hel)
    call interaction_add_state (int2, qn, value=(4._default, 0._default))
    call hel%init ([0,-1, 1], [0, 1,-1])
    call qn%init (flv, col, hel)
    call interaction_add_state (int2, qn, value=(2._default, 0._default))
    call hel%init ([0, 1,-1], [0,-1, 1])
    call qn%init (flv, col, hel)
    call interaction_add_state (int2, qn, value=(2._default, 0._default))
    call hel%init ([0,-1, 1], [0,-1, 1])
    call qn%init (flv, col, hel)
    call interaction_add_state (int2, qn, value=(4._default, 0._default))
    call flv%init ([23, 2, -2], model)
    call hel%init ([0, 1,-1], [0, 1,-1])
    call qn%init (flv, col, hel)
    call interaction_add_state (int2, qn, value=(0.5_default, 0._default))
    call hel%init ([0,-1, 1], [0,-1, 1])
    call qn%init (flv, col, hel)
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
    call particle_set1%init &
         (ok, int, int, FM_FACTOR_HELICITY, &
          [0.2_default, 0.2_default], .false., .true.)
    call particle_set_write (particle_set1, u)

    write (u, "(A)")
    write (u, "(A)")  "* Factorize as subevent (in/out only, selected helicity)"
    write (u, "(A)")
    
    int => evaluator_get_int_ptr (eval)
    call particle_set2%init &
         (ok, int, int, FM_SELECT_HELICITY, &
          [0.9_default, 0.9_default], .false., .false.)
    call particle_set2%write (u)
    call particle_set2%final ()

    write (u, "(A)")
    write (u, "(A)")  "* Factorize as subevent (complete, selected helicity)"
    write (u, "(A)") 
    
    int => evaluator_get_int_ptr (eval)
    call particle_set2%init &
         (ok, int, int, FM_SELECT_HELICITY, &
          [0.7_default, 0.7_default], .false., .true.)
    call particle_set2%write (u)      
        
    write (u, "(A)")
    write (u, "(A)")  &
         "* Factorize (complete, polarized, correlated); write and read again"
    write (u, "(A)")
    
    int => evaluator_get_int_ptr (eval)
    call particle_set3%init &
         (ok, int, int, FM_FACTOR_HELICITY, &
          [0.7_default, 0.7_default], .true., .true.)
    call particle_set3%write (u)

    unit = free_unit ()
    open (unit, action="readwrite", form="unformatted", status="scratch")
    call particle_set3%write_raw (unit)
    rewind (unit)
    call particle_set4%read_raw (unit, iostat=iostat)
    call particle_set4%set_model (model)
    close (unit)
    
    write (u, "(A)")
    write (u, "(A)")  "* Result from reading"
    write (u, "(A)")

    call particle_set4%write (u)

    !!! write (u, "(A)")
    !!! write (u, "(A)")  "* Transform to a subevt object"
    !!! write (u, "(A)")
    !!! 
    !!! call particle_set4%to_subevt (subevt)
    !!! call subevt_write (subevt, u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call particle_set1%final ()
    call particle_set2%final ()
    call particle_set3%final ()
    call particle_set4%final ()
    call evaluator_final (eval)
    call interaction_final (int1)
    call interaction_final (int2)

    call model%final ()
       
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: particles_1"        
    
  end subroutine particles_1

  subroutine particles_2 (u)

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
       call pset%prt(i)%reset_status (PRT_INCOMING)
       call pset%prt(i)%set_children ([3,4,5])
    end do
    do i = 3, 5
       call particle_reset_status (pset%prt(i), PRT_OUTGOING)
       call particle_set_parents (pset%prt(i), [1,2])
    end do
    call pset%prt(1)%set_momentum (vector4_at_rest (1._default))
    call pset%prt(2)%set_momentum (vector4_at_rest (2._default))
    call pset%prt(3)%set_momentum (vector4_at_rest (5._default))
    call pset%prt(4)%set_momentum (vector4_at_rest (4._default))
    call pset%prt(5)%set_momentum (vector4_at_rest (3._default))

    allocate (flv (5))
    call flv%init ([11,12,5,4,3])
    do i = 1, 5
       call pset%prt(i)%set_flavor (flv(i))
    end do

    call pset%write (u)

    write (u, "(A)")      
    write (u, "(A)")  "*   Fill interaction from particle set"
    write (u, "(A)")

    call pset%fill_interaction (int, 2, state_flv=state_flv)
    call interaction_write (int, u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call interaction_final (int)
    call pset%final ()
    
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
    call pset%prt(1)%reset_status (PRT_BEAM)
    call pset%prt(2)%reset_status (PRT_BEAM)
    call pset%prt(3)%reset_status (PRT_INCOMING)
    call pset%prt(4)%reset_status (PRT_INCOMING)
    call pset%prt(5)%reset_status (PRT_OUTGOING)
    call pset%prt(6)%reset_status (PRT_OUTGOING)
    call pset%prt(7)%reset_status (PRT_OUTGOING)
    call pset%prt(8)%reset_status (PRT_OUTGOING)
    call pset%prt(9)%reset_status (PRT_OUTGOING)

    call pset%prt(1)%set_children ([3,5])
    call pset%prt(2)%set_children ([4,6])
    call pset%prt(3)%set_children ([7,8,9])
    call pset%prt(4)%set_children ([7,8,9])

    call pset%prt(3)%set_parents ([1])
    call pset%prt(4)%set_parents ([2])
    call pset%prt(5)%set_parents ([1])
    call pset%prt(6)%set_parents ([2])
    call pset%prt(7)%set_parents ([5,6])
    call pset%prt(8)%set_parents ([5,6])
    call pset%prt(9)%set_parents ([5,6])

    call pset%prt(1)%set_momentum (vector4_at_rest (1._default))
    call pset%prt(2)%set_momentum (vector4_at_rest (2._default))
    call pset%prt(3)%set_momentum (vector4_at_rest (4._default))
    call pset%prt(4)%set_momentum (vector4_at_rest (6._default))
    call pset%prt(5)%set_momentum (vector4_at_rest (3._default))
    call pset%prt(6)%set_momentum (vector4_at_rest (5._default))
    call pset%prt(7)%set_momentum (vector4_at_rest (7._default))
    call pset%prt(8)%set_momentum (vector4_at_rest (8._default))
    call pset%prt(9)%set_momentum (vector4_at_rest (9._default))

    allocate (flv (9))
    call flv%init ([2011, 2012, 11, 12, 91, 92, 3, 4, 5])
    do i = 1, 9
       call pset%prt(i)%set_flavor (flv(i))
    end do

    call pset%write (u)
 
    write (u, "(A)")      
    write (u, "(A)")  "*   Fill interaction from particle set"
    write (u, "(A)")

    call particle_set_fill_interaction (pset, int, 2, state_flv=state_flv)
    call interaction_write (int, u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call interaction_final (int)
    call pset%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: particles_3"

  end subroutine particles_3
  
  subroutine particles_4 (u)

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
    call pset%prt(1)%reset_status (PRT_INCOMING)
    call pset%prt(2)%reset_status (PRT_INCOMING)
    call pset%prt(3)%reset_status (PRT_OUTGOING)
    call pset%prt(4)%reset_status (PRT_OUTGOING)
    call pset%prt(5)%reset_status (PRT_OUTGOING)

    call pset%prt(1)%set_children ([3,4,5])
    call pset%prt(2)%set_children ([3,4,5])

    call pset%prt(3)%set_parents ([1,2])
    call pset%prt(4)%set_parents ([1,2])
    call pset%prt(5)%set_parents ([1,2])

    call pset%prt(1)%set_momentum (vector4_at_rest (6._default))
    call pset%prt(2)%set_momentum (vector4_at_rest (6._default))
    call pset%prt(3)%set_momentum (vector4_at_rest (3._default))
    call pset%prt(4)%set_momentum (vector4_at_rest (4._default))
    call pset%prt(5)%set_momentum (vector4_at_rest (5._default))

    allocate (flv (5))
    call flv%init ([11, 12, 3, 4, 5])
    do i = 1, 5
       call pset%prt(i)%set_flavor (flv(i))
    end do

    call pset%write (u)
 
    write (u, "(A)")      
    write (u, "(A)")  "*   Fill interaction from particle set"
    write (u, "(A)")

    call pset%fill_interaction (int, 2, state_flv=state_flv, &
         recover_beams = .true.)
    call interaction_write (int, u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call interaction_final (int)
    call pset%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: particles_4"

  end subroutine particles_4
  
  subroutine particles_5 (u)

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
    call pset%prt(1)%reset_status (PRT_BEAM)
    call pset%prt(2)%reset_status (PRT_BEAM)
    call pset%prt(3)%reset_status (PRT_VIRTUAL)
    call pset%prt(4)%reset_status (PRT_VIRTUAL)
    call pset%prt(5)%reset_status (PRT_VIRTUAL)
    call pset%prt(6)%reset_status (PRT_VIRTUAL)
    call pset%prt(7)%reset_status (PRT_INCOMING)
    call pset%prt(8)%reset_status (PRT_INCOMING)
    call pset%prt( 9)%reset_status (PRT_OUTGOING)
    call pset%prt(10)%reset_status (PRT_OUTGOING)
    call pset%prt(11)%reset_status (PRT_OUTGOING)
    call pset%prt(12)%reset_status (PRT_OUTGOING)
    call pset%prt(13)%reset_status (PRT_OUTGOING)

    call pset%prt(1)%set_children ([3,4])
    call pset%prt(2)%set_children ([5,6])
    call pset%prt(3)%set_children ([ 7])
    call pset%prt(4)%set_children ([ 9])
    call pset%prt(5)%set_children ([ 8])
    call pset%prt(6)%set_children ([10])
    call pset%prt(7)%set_children ([11,12,13])
    call pset%prt(8)%set_children ([11,12,13])

    call pset%prt(3)%set_parents ([1])
    call pset%prt(4)%set_parents ([1])
    call pset%prt(5)%set_parents ([2])
    call pset%prt(6)%set_parents ([2])
    call pset%prt( 7)%set_parents ([3])
    call pset%prt( 8)%set_parents ([5])
    call pset%prt( 9)%set_parents ([4])
    call pset%prt(10)%set_parents ([6])
    call pset%prt(11)%set_parents ([7,8])
    call pset%prt(12)%set_parents ([7,8])
    call pset%prt(13)%set_parents ([7,8])

    call pset%prt(1)%set_momentum (vector4_at_rest (1._default))
    call pset%prt(2)%set_momentum (vector4_at_rest (2._default))
    call pset%prt(3)%set_momentum (vector4_at_rest (4._default))
    call pset%prt(4)%set_momentum (vector4_at_rest (3._default))
    call pset%prt(5)%set_momentum (vector4_at_rest (6._default))
    call pset%prt(6)%set_momentum (vector4_at_rest (5._default))
    call pset%prt(7)%set_momentum (vector4_at_rest (4._default))
    call pset%prt(8)%set_momentum (vector4_at_rest (6._default))
    call pset%prt( 9)%set_momentum (vector4_at_rest (3._default))
    call pset%prt(10)%set_momentum (vector4_at_rest (5._default))
    call pset%prt(11)%set_momentum (vector4_at_rest (7._default))
    call pset%prt(12)%set_momentum (vector4_at_rest (8._default))
    call pset%prt(13)%set_momentum (vector4_at_rest (9._default))

    allocate (flv (13))
    call flv%init ([2011, 2012, 11, 91, 12, 92, 11, 12, 91, 92, 3, 4, 5])
    do i = 1, 13
       call pset%prt(i)%set_flavor (flv(i))
    end do

    call pset%write (u)
 
    write (u, "(A)")      
    write (u, "(A)")  "*   Fill interaction from particle set"
    write (u, "(A)")

    call pset%fill_interaction (int, 2, state_flv=state_flv)
    call interaction_write (int, u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call interaction_final (int)
    call pset%final ()
    
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
    call pset%prt(1)%reset_status (PRT_BEAM)
    call pset%prt(2)%reset_status (PRT_BEAM)
    call pset%prt(3)%reset_status (PRT_INCOMING)
    call pset%prt(4)%reset_status (PRT_INCOMING)
    call pset%prt(5)%reset_status (PRT_OUTGOING)
    call pset%prt(6)%reset_status (PRT_OUTGOING)
    call pset%prt(7)%reset_status (PRT_OUTGOING)
    call pset%prt(8)%reset_status (PRT_OUTGOING)
    call pset%prt(9)%reset_status (PRT_OUTGOING)

    call pset%prt(1)%set_children ([3,4,5,6])
    call pset%prt(2)%set_children ([3,4,5,6])
    call pset%prt(3)%set_children ([7,8,9])
    call pset%prt(4)%set_children ([7,8,9])

    call pset%prt(3)%set_parents ([1,2])
    call pset%prt(4)%set_parents ([1,2])
    call pset%prt(5)%set_parents ([1,2])
    call pset%prt(6)%set_parents ([1,2])
    call pset%prt(7)%set_parents ([3,4])
    call pset%prt(8)%set_parents ([3,4])
    call pset%prt(9)%set_parents ([3,4])

    call pset%prt(1)%set_momentum (vector4_at_rest (1._default))
    call pset%prt(2)%set_momentum (vector4_at_rest (2._default))
    call pset%prt(3)%set_momentum (vector4_at_rest (5._default))
    call pset%prt(4)%set_momentum (vector4_at_rest (6._default))
    call pset%prt(5)%set_momentum (vector4_at_rest (3._default))
    call pset%prt(6)%set_momentum (vector4_at_rest (4._default))
    call pset%prt(7)%set_momentum (vector4_at_rest (7._default))
    call pset%prt(8)%set_momentum (vector4_at_rest (8._default))
    call pset%prt(9)%set_momentum (vector4_at_rest (9._default))

    allocate (flv (9))
    call flv%init ([1011, 1012, 11, 12, 21, 22, 3, 4, 5])
    do i = 1, 9
       call pset%prt(i)%set_flavor (flv(i))
    end do

    call pset%write (u)
 
    write (u, "(A)")      
    write (u, "(A)")  "*   Fill interaction from particle set"
    write (u, "(A)")

    call particle_set_fill_interaction (pset, int, 2, state_flv=state_flv)
    call interaction_write (int, u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call interaction_final (int)
    call pset%final ()
    
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
       call pset%prt(i)%reset_status (PRT_INCOMING)
       call pset%prt(i)%set_children ([2,3,4])
    end do
    do i = 2, 4
       call pset%prt(i)%reset_status (PRT_OUTGOING)
       call pset%prt(i)%set_parents ([1])
    end do
    call pset%prt(1)%set_momentum (vector4_at_rest (1._default))
    call pset%prt(2)%set_momentum (vector4_at_rest (3._default))
    call pset%prt(3)%set_momentum (vector4_at_rest (2._default))
    call pset%prt(4)%set_momentum (vector4_at_rest (4._default))

    allocate (flv (5))
    call flv%init ([6,5,12,-11])
    do i = 1, 4
       call pset%prt(i)%set_flavor (flv(i))
    end do

    call pset%write (u)

    write (u, "(A)")      
    write (u, "(A)")  "*   Fill interaction from particle set"
    write (u, "(A)")

    call particle_set_fill_interaction (pset, int, 1, state_flv=state_flv)
    call interaction_write (int, u)

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"

    call interaction_final (int)
    call pset%final ()
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: particles_7"

  end subroutine particles_7
  

end module particles
