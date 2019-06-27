! WHIZARD 2.0.2 Tue May 18 2010
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

module strfun

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use lorentz !NODEP!
  use models
  use quantum_numbers
  use interactions
  use evaluators
  use beams
  use sf_isr
  use sf_epa
  use sf_ewa
  use sf_lhapdf

  implicit none
  private

!  public :: strfun_t
!  public :: strfun_init
!  public :: strfun_final
!  public :: strfun_write
!  public :: strfun_get_name
!  public :: strfun_set_kinematics
!  public :: strfun_apply
  public :: strfun_chain_t
  public :: strfun_chain_init
  public :: strfun_chain_set_beam_momenta
  public :: strfun_chain_final
  public :: strfun_chain_write
  public :: assignment(=)
  public :: strfun_chain_get_n_strfun
  public :: strfun_chain_get_n_parameters_tot
  public :: strfun_chain_get_n_vir
  public :: strfun_chain_get_mapping_factor
  public :: strfun_chain_dimension_is_rigid
  public :: strfun_chain_get_colliding_particles
  public :: strfun_chain_get_colliding_particles_mask
  public :: strfun_chain_get_beam_int_ptr
  public :: strfun_chain_get_last_evaluator_ptr
  public :: strfun_chain_set_strfun
  public :: strfun_chain_set_mapping
  public :: strfun_chain_make_evaluators
  public :: strfun_chain_set_kinematics
  public :: strfun_chain_evaluate
  public :: strfun_test

  integer, parameter, public :: STRF_NONE = 0
  integer, parameter, public :: STRF_LHAPDF = 1, STRF_ISR = 2, &
       STRF_EPA = 3, STRF_EWA = 4
  
  integer, parameter, public :: SFM_NONE = 0
  integer, parameter, public :: SFM_PDFPAIR = 1
  integer, parameter, public :: SFM_ISRPAIR = 2
  integer, parameter, public :: SFM_EPAPAIR = 3
  integer, parameter, public :: SFM_EWAPAIR = 4  

  type :: strfun_t
     private
     integer :: type = STRF_NONE
     type(string_t) :: name
     type(interaction_t) :: int
     type(lhapdf_data_t), dimension(:), allocatable :: lhapdf_data
     type(isr_data_t), dimension(:), allocatable :: isr_data
     type(epa_data_t), dimension(:), allocatable :: epa_data
     type(ewa_data_t), dimension(:), allocatable :: ewa_data     
     real(default) :: x = 0, f = 1, s = 0
     real(default) :: scale = 0
  end type strfun_t

  type :: strfun_mapping_t
     private
     integer, dimension(:), allocatable :: index
     integer :: type = SFM_NONE
     real(default), dimension(:), allocatable :: par
  end type strfun_mapping_t

  type :: strfun_chain_t
     private
     type(beam_t) :: beam
     integer :: n_strfun = 0
     integer :: n_mapping = 0
     type(strfun_t), dimension(:), allocatable :: strfun
     type(strfun_mapping_t), dimension(:), allocatable :: sf_mapping
     real(default) :: mapping_factor = 1
     integer :: n_parameters_tot = 0
     integer, dimension(:), allocatable :: n_parameters
     type(evaluator_t), dimension(:), allocatable :: eval
     integer, dimension(:), allocatable :: last_strfun
     integer, dimension(:), allocatable :: out_index
     integer, dimension(:), allocatable :: coll_index
  end type strfun_chain_t


  interface strfun_init
     module procedure strfun_init_lhapdf
     module procedure strfun_init_isr
     module procedure strfun_init_epa
     module procedure strfun_init_ewa
  end interface

  interface assignment(=)
     module procedure strfun_chain_assign
  end interface

  interface strfun_chain_set_strfun
     module procedure strfun_chain_set_lhapdf
     module procedure strfun_chain_set_isr
     module procedure strfun_chain_set_epa
     module procedure strfun_chain_set_ewa     
  end interface

contains

  subroutine strfun_init_lhapdf (strfun, lhapdf_data)
    type(strfun_t), intent(out) :: strfun
    type(lhapdf_data_t), intent(in) :: lhapdf_data
    strfun%type = STRF_LHAPDF
    strfun%name = "LHAPDF"
    allocate (strfun%lhapdf_data (1))
    strfun%lhapdf_data = lhapdf_data
    call interaction_init_lhapdf (strfun%int, lhapdf_data)
  end subroutine strfun_init_lhapdf

  subroutine strfun_init_isr (strfun, isr_data)
    type(strfun_t), intent(out) :: strfun
    type(isr_data_t), intent(in) :: isr_data
    strfun%type = STRF_ISR
    strfun%name = "ISR"
    allocate (strfun%isr_data (1))
    strfun%isr_data = isr_data
    call interaction_init_isr (strfun%int, isr_data)
  end subroutine strfun_init_isr

  subroutine strfun_init_epa (strfun, epa_data)
    type(strfun_t), intent(out) :: strfun
    type(epa_data_t), intent(in) :: epa_data
    strfun%type = STRF_EPA
    strfun%name = "EPA"
    allocate (strfun%epa_data (1))
    strfun%epa_data = epa_data
    call interaction_init_epa (strfun%int, epa_data)
  end subroutine strfun_init_epa
  
  subroutine strfun_init_ewa (strfun, ewa_data, id)
    type(strfun_t), intent(out) :: strfun
    type(ewa_data_t), intent(inout) :: ewa_data
    integer, intent(in) :: id
    strfun%type = STRF_EWA
    strfun%name = "EWA"
    allocate (strfun%ewa_data (1))
    call ewa_set_id (ewa_data, id)
    strfun%ewa_data = ewa_data
    call interaction_init_ewa (strfun%int, ewa_data)
  end subroutine strfun_init_ewa

  elemental subroutine strfun_final (strfun)
    type(strfun_t), intent(inout) :: strfun
    select case (strfun%type)
    case (STRF_ISR)
       deallocate (strfun%isr_data)
    case (STRF_EPA)
       deallocate (strfun%epa_data)
    case (STRF_EWA)
       deallocate (strfun%ewa_data)
    case (STRF_LHAPDF)
       deallocate (strfun%lhapdf_data)
    end select
    call interaction_final (strfun%int)
    strfun%type = STRF_NONE
  end subroutine strfun_final

  subroutine strfun_write (strfun, unit, verbose, show_momentum_sum, show_mass)
    type(strfun_t), intent(in) :: strfun
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, show_momentum_sum, show_mass
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    if (strfun%type /= STRF_NONE) then
       write (u, *) char (strfun_get_name (strfun)) // " setup:"
       select case (strfun%type)
       case (STRF_LHAPDF)
          call lhapdf_data_write (strfun%lhapdf_data(1), u)
          write (u, *) "LHAPDF event data:"
          write (u, *) "  x     =", strfun%x
          write (u, *) "  f     =", strfun%f
          write (u, *) "  scale =", strfun%scale
          write (u, *) "  p2    =", strfun%s
       case (STRF_ISR)
          call isr_data_write (strfun%isr_data(1), u)
       case (STRF_EPA)
          call epa_data_write (strfun%epa_data(1), u)
       case (STRF_EWA)
          call ewa_data_write (strfun%ewa_data(1), u)
       end select
       call interaction_write &
            (strfun%int, unit, verbose, show_momentum_sum, show_mass)
    else
       write (u, *) "Structure function setup: [empty]"
    end if
  end subroutine strfun_write

  function strfun_get_name (strfun) result (name)
    type(string_t) :: name
    type(strfun_t), intent(in) :: strfun
    name = strfun%name
  end function strfun_get_name

  subroutine strfun_set_kinematics (strfun, r)
    type(strfun_t), intent(inout) :: strfun
    real(default), dimension(:), intent(in) :: r
    select case (strfun%type)
    case (STRF_LHAPDF)
       call interaction_set_kinematics_lhapdf (strfun%int, &
            strfun%x, strfun%f, strfun%s, r(1), strfun%lhapdf_data(1))
    case (STRF_ISR)
       call interaction_apply_isr (strfun%int, r, strfun%isr_data(1))
    case (STRF_EPA)
       call interaction_apply_epa (strfun%int, r, strfun%epa_data)
    case (STRF_EWA)
       call interaction_apply_ewa (strfun%int, r, strfun%ewa_data)
    end select
  end subroutine strfun_set_kinematics

  subroutine strfun_apply (strfun, scale)
    type(strfun_t), intent(inout) :: strfun
    real(default), intent(in) :: scale
    strfun%scale = scale
    select case (strfun%type)
    case (STRF_LHAPDF)
       call interaction_apply_lhapdf (strfun%int, scale, &
            strfun%x, strfun%f, strfun%s, strfun%lhapdf_data(1))
    end select
  end subroutine strfun_apply
    
  subroutine strfun_mapping_init (sf_mapping, index, type, par)
    type(strfun_mapping_t), intent(out) :: sf_mapping
    integer, dimension(:), intent(in) :: index
    integer, intent(in) :: type
    real(default), dimension(:), intent(in) :: par
    allocate (sf_mapping%index (size (index)))
    sf_mapping%index = index
    sf_mapping%type = type
    allocate (sf_mapping%par (size (par)))
    sf_mapping%par = par
  end subroutine strfun_mapping_init

  subroutine strfun_mapping_write (sf_mapping, unit)
    type(strfun_mapping_t), intent(in) :: sf_mapping
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A)", advance="no") "Strfun mapping for indices: "
    write (u, "(10(1x,I0))")  sf_mapping%index
    write (u, "(1x,A,1x,I0)")  "mapping type =", sf_mapping%type
    write (u, "(1x,A)", advance="no")  "mapping pars ="
    write (u, *)  sf_mapping%par
  end subroutine strfun_mapping_write

  subroutine strfun_mapping_apply (sf_mapping, x, factor)
    type(strfun_mapping_t), intent(in) :: sf_mapping
    real(default), dimension(:), intent(inout) :: x
    real(default), intent(inout) :: factor
    real(default), dimension(2) :: x2
    select case (sf_mapping%type)
    case (SFM_PDFPAIR)
       x2 = x(sf_mapping%index)
       call map_unit_square (x2, factor, sf_mapping%par(1))
       x(sf_mapping%index) = x2
    end select
  end subroutine strfun_mapping_apply

  subroutine map_unit_square (x, factor, power)
    real(kind=default), dimension(2), intent(inout) :: x
    real(kind=default), intent(inout) :: factor
    real(kind=default), intent(in), optional :: power
    real(kind=default) :: xx, yy
    xx = x(1)
    yy = x(2)
    if (present(power)) then
       if (x(1) > 0 .and. power > 1) then
          xx = x(1)**power
          factor = factor * power * xx / x(1)
       end if
    end if
    if (xx /= 0) then
       x(1) = xx ** yy
       x(2) = xx / x(1)
       factor = factor * abs (log (xx))
    else
       x = 0
    end if
  end subroutine map_unit_square

  subroutine strfun_chain_init (sfchain, beam_data, n_strfun, n_mapping)
    type(strfun_chain_t), intent(out) :: sfchain
    type(beam_data_t), intent(in), target :: beam_data
    integer, intent(in) :: n_strfun, n_mapping
    integer :: i
    sfchain%n_strfun = n_strfun
    allocate (sfchain%strfun (n_strfun))
    allocate (sfchain%sf_mapping (n_mapping))
    allocate (sfchain%n_parameters (n_strfun))
    sfchain%n_parameters = 0
    allocate (sfchain%eval (n_strfun))
    call beam_init (sfchain%beam, beam_data)
    allocate (sfchain%last_strfun (beam_data%n))
    allocate (sfchain%out_index (beam_data%n))
    allocate (sfchain%coll_index (beam_data%n))
    sfchain%last_strfun = 0
    do i = 1, size (sfchain%out_index)
       sfchain%out_index(i) = i
       sfchain%coll_index(i) = i
    end do
  end subroutine strfun_chain_init

  subroutine strfun_chain_set_beam_momenta (sfchain, p)
    type(strfun_chain_t), intent(inout) :: sfchain
    type(vector4_t), dimension(:), intent(in) :: p
    call beam_set_momenta (sfchain%beam, p)
  end subroutine strfun_chain_set_beam_momenta

  subroutine strfun_chain_final (sfchain)
    type(strfun_chain_t), intent(inout) :: sfchain
    call beam_final (sfchain%beam)
    if (allocated (sfchain%strfun))  call strfun_final (sfchain%strfun)
    if (allocated (sfchain%eval))  call evaluator_final (sfchain%eval)
  end subroutine strfun_chain_final

  subroutine strfun_chain_write &
       (sfchain, unit, verbose, show_momentum_sum, show_mass)
    type(strfun_chain_t), intent(in) :: sfchain
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: verbose, show_momentum_sum, show_mass
    integer :: u, i
    logical :: verb
    verb = .false.;  if (present (verbose))  verb = verbose
    u = output_unit (unit);  if (u < 0)  return
    write (u, *)  "Structure function chain:"
    write (u, *)
    call beam_write (sfchain%beam, unit, verbose, show_momentum_sum, show_mass)
    if (allocated (sfchain%strfun)) then
       do i = 1, size (sfchain%strfun)
          write (u, *)
          call strfun_write &
               (sfchain%strfun(i), unit, verbose, show_momentum_sum, show_mass)
          write (u, *)  "number of parameters = ", sfchain%n_parameters(i)
       end do
    end if
    if (allocated (sfchain%sf_mapping)) then
       do i = 1, size (sfchain%sf_mapping)
          write (u, *)
          call strfun_mapping_write (sfchain%sf_mapping(i), unit)
       end do
    end if
    if (allocated (sfchain%eval)) then
       write (u, *)
       write (u, *) "Evaluators:"
       do i = 1, size (sfchain%eval)
          call evaluator_write &
               (sfchain%eval(i), unit, verbose, show_momentum_sum, show_mass)
       end do
    end if
    write (u, *)
    write (u, *)  "Total number of parameters      = ", &
         sfchain%n_parameters_tot
    write (u, "(1x,A)", advance="no")  "Last structure function (index) = "
    if (allocated (sfchain%last_strfun)) then
       write (u, *) sfchain%last_strfun
    else
       write (u, *) "[not allocated]"
    end if
    write (u, "(1x,A)", advance="no")  "Outgoing particles (index)      = "
    if (allocated (sfchain%out_index)) then
       write (u, *) sfchain%out_index
    else
       write (u, *) "[not allocated]"
    end if
    write (u, "(1x,A)", advance="no")  "Colliding particles (index)     = "
    if (allocated (sfchain%coll_index)) then
       write (u, *)  sfchain%coll_index
    else
       write (u, *) "[not allocated]"
    end if
  end subroutine strfun_chain_write

  subroutine strfun_chain_assign (sfchain_out, sfchain_in)
    type(strfun_chain_t), intent(out) :: sfchain_out
    type(strfun_chain_t), intent(in) :: sfchain_in
    sfchain_out%beam = sfchain_in%beam
    sfchain_out%n_strfun = sfchain_in%n_strfun
    sfchain_out%n_mapping = sfchain_in%n_mapping
    if (allocated (sfchain_in%strfun)) then
       allocate (sfchain_out%strfun (size (sfchain_in%strfun)))
       sfchain_out%strfun = sfchain_in%strfun
    end if
    if (allocated (sfchain_in%sf_mapping)) then
       allocate (sfchain_out%sf_mapping (size (sfchain_in%sf_mapping)))
       sfchain_out%sf_mapping = sfchain_in%sf_mapping
    end if
    sfchain_out%mapping_factor = sfchain_in%mapping_factor
    sfchain_out%n_parameters_tot = sfchain_in%n_parameters_tot
    if (allocated (sfchain_in%n_parameters)) then
       allocate (sfchain_out%n_parameters (size (sfchain_in%n_parameters)))
       sfchain_out%n_parameters = sfchain_in%n_parameters
    end if
    if (allocated (sfchain_in%eval)) then
       allocate (sfchain_out%eval (size (sfchain_in%eval)))
       sfchain_out%eval = sfchain_in%eval
    end if
    if (allocated (sfchain_in%last_strfun)) then
       allocate (sfchain_out%last_strfun (size (sfchain_in%last_strfun)))
       sfchain_out%last_strfun = sfchain_in%last_strfun
    end if
    if (allocated (sfchain_in%out_index)) then
       allocate (sfchain_out%out_index (size (sfchain_in%out_index)))
       sfchain_out%out_index = sfchain_in%out_index
    end if
    if (allocated (sfchain_in%coll_index)) then
       allocate (sfchain_out%coll_index (size (sfchain_in%coll_index)))
       sfchain_out%coll_index = sfchain_in%coll_index
    end if
  end subroutine strfun_chain_assign

  function strfun_chain_get_n_strfun (sfchain) result (n)
    integer :: n
    type(strfun_chain_t), intent(in) :: sfchain
    n = sfchain%n_strfun
  end function strfun_chain_get_n_strfun

  function strfun_chain_get_n_parameters_tot (sfchain) result (n)
    integer :: n
    type(strfun_chain_t), intent(in) :: sfchain
    n = sfchain%n_parameters_tot
  end function strfun_chain_get_n_parameters_tot

  function strfun_chain_get_n_vir (sfchain) result (n)
    integer :: n
    type(strfun_chain_t), intent(in) :: sfchain
    if (sfchain%n_strfun /= 0) then
       n = evaluator_get_n_vir (sfchain%eval(sfchain%n_strfun))
    else
       n = 0
    end if
  end function strfun_chain_get_n_vir

  function strfun_chain_get_mapping_factor (sfchain) result (f)
    real(default) :: f
    type(strfun_chain_t), intent(in) :: sfchain
    f = sfchain%mapping_factor
  end function strfun_chain_get_mapping_factor

  function strfun_chain_dimension_is_rigid (sfchain) result (rigid)
    logical, dimension(:), allocatable :: rigid
    type(strfun_chain_t), intent(in) :: sfchain
    integer :: i, j, k
    allocate (rigid (sfchain%n_parameters_tot))
    k = 0
    do i = 1, size (sfchain%n_parameters)
       do j = 1, sfchain%n_parameters(i)
          k = k + 1
          select case (sfchain%strfun(i)%type)
          case default
             rigid(k) = .false.
          end select
       end do
    end do
  end function strfun_chain_dimension_is_rigid

  function strfun_chain_get_colliding_particles (sfchain) result (index)
    integer, dimension(:), allocatable :: index
    type(strfun_chain_t), intent(in) :: sfchain
    allocate (index (size (sfchain%coll_index)))
    index = sfchain%coll_index
  end function strfun_chain_get_colliding_particles

  function strfun_chain_get_colliding_particles_mask (sfchain) result (mask)
    type(quantum_numbers_mask_t), dimension(:), allocatable :: mask
    type(strfun_chain_t), intent(in), target :: sfchain
    integer :: n_strfun
    type(quantum_numbers_mask_t), dimension(:), allocatable :: mask_eval
    allocate (mask (size (sfchain%coll_index)))
    n_strfun = sfchain%n_strfun
    if (n_strfun /= 0) then
       allocate (mask_eval (evaluator_get_n_tot (sfchain%eval(n_strfun))))
       mask_eval = evaluator_get_mask (sfchain%eval(n_strfun))
       mask = mask_eval(sfchain%coll_index)
    else
       mask = interaction_get_mask (beam_get_int_ptr (sfchain%beam))
    end if
  end function strfun_chain_get_colliding_particles_mask

  function strfun_chain_get_beam_int_ptr (sfchain) result (int)
    type(interaction_t), pointer :: int
    type(strfun_chain_t), intent(in), target :: sfchain
    int => beam_get_int_ptr (sfchain%beam)
  end function strfun_chain_get_beam_int_ptr
  
  function strfun_chain_get_last_evaluator_ptr (sfchain) result (eval)
    type(evaluator_t), pointer :: eval
    type(strfun_chain_t), intent(in), target :: sfchain
    if (sfchain%n_strfun /= 0) then
       eval => sfchain%eval(sfchain%n_strfun)
    else
       eval => null ()
    end if
  end function strfun_chain_get_last_evaluator_ptr

  subroutine strfun_chain_set_lhapdf &
       (sfchain, i, line, lhapdf_data, n_parameters)
    type(strfun_chain_t), intent(inout), target :: sfchain
    integer, intent(in) :: i, line, n_parameters
    type(lhapdf_data_t), intent(in) :: lhapdf_data
    call strfun_init (sfchain%strfun(i), lhapdf_data)
    sfchain%n_parameters(i) = n_parameters
    call strfun_chain_link (sfchain, i, line, (/1/), (/3/))
  end subroutine strfun_chain_set_lhapdf
  
  subroutine strfun_chain_set_isr &
       (sfchain, i, line, isr_data, n_parameters)
    type(strfun_chain_t), intent(inout), target :: sfchain
    integer, intent(in) :: i, line, n_parameters
    type(isr_data_t), intent(in) :: isr_data
    call strfun_init (sfchain%strfun(i), isr_data)
    sfchain%n_parameters(i) = n_parameters
    call strfun_chain_link (sfchain, i, line, (/1/), (/3/))
  end subroutine strfun_chain_set_isr

  subroutine strfun_chain_set_epa &
       (sfchain, i, line, epa_data, n_parameters)
    type(strfun_chain_t), intent(inout), target :: sfchain
    integer, intent(in) :: i, line, n_parameters
    type(epa_data_t), intent(in) :: epa_data
    call strfun_init (sfchain%strfun(i), epa_data)
    sfchain%n_parameters(i) = n_parameters
    call strfun_chain_link (sfchain, i, line, (/1/), (/3/))
  end subroutine strfun_chain_set_epa

  subroutine strfun_chain_set_ewa &
       (sfchain, i, line, ewa_data, n_parameters, id)
    type(strfun_chain_t), intent(inout), target :: sfchain
    integer, intent(in) :: i, line, n_parameters, id
    type(ewa_data_t), intent(inout) :: ewa_data
    call strfun_init (sfchain%strfun(i), ewa_data, id)
    sfchain%n_parameters(i) = n_parameters
    call strfun_chain_link (sfchain, i, line, (/1/), (/3/))
  end subroutine strfun_chain_set_ewa

  subroutine strfun_chain_link (sfchain, i, line, in_index, out_index)
    type(strfun_chain_t), intent(inout), target :: sfchain
    integer, intent(in) :: i, line
    integer, dimension(:), intent(in) :: in_index, out_index
    select case (line)
    case (0)
       call link_single (1, in_index(1))
       call link_single (2, in_index(2))
       sfchain%last_strfun = i
       sfchain%out_index = out_index
    case default
       call link_single (line, in_index(1))
       sfchain%last_strfun(line) = i
       sfchain%out_index(line) = out_index(1)
    end select
  contains
    subroutine link_single (line, in_index)
      integer, intent(in) :: line, in_index
      integer :: j
      j = sfchain%last_strfun(line)
      select case (j)
      case (0)
         call interaction_set_source_link &
              (sfchain%strfun(i)%int, in_index, &
               sfchain%beam, sfchain%out_index(line))
      case default
         call interaction_set_source_link &
              (sfchain%strfun(i)%int, in_index, &
               sfchain%strfun(j)%int, sfchain%out_index(j))
      end select
    end subroutine link_single
  end subroutine strfun_chain_link

  subroutine strfun_chain_set_mapping (sfchain, i, index, type, par)
    type(strfun_chain_t), intent(inout) :: sfchain
    integer, intent(in) :: i
    integer, dimension(:), intent(in) :: index
    integer, intent(in) :: type
    real(default), dimension(:), intent(in) :: par
    call strfun_mapping_init (sfchain%sf_mapping(i), index, type, par)
  end subroutine strfun_chain_set_mapping

  subroutine strfun_chain_make_evaluators (sfchain, ok)
    type(strfun_chain_t), intent(inout), target :: sfchain
    logical, intent(out), optional :: ok
    type(interaction_t), pointer :: beam_int, eval_int
    type(quantum_numbers_mask_t) :: qn_mask_conn
    type(quantum_numbers_mask_t), dimension(:), allocatable :: qn_mask_beam
    integer :: i, j
    sfchain%n_parameters_tot = sum (sfchain%n_parameters)
    beam_int => beam_get_int_ptr (sfchain%beam)
    if (.not. associated (beam_int))  call msg_bug &
         ("strfun_chain_make_evaluators: null beam pointer")
    allocate (qn_mask_beam (interaction_get_n_out (beam_int)))
    qn_mask_beam = interaction_get_mask (beam_int)
    call interaction_exchange_mask (beam_int)
    do i = 1, size (sfchain%strfun) - 1
       call interaction_exchange_mask (sfchain%strfun(i)%int)
    end do
    do i = size (sfchain%strfun), 1, -1
       call interaction_exchange_mask (sfchain%strfun(i)%int)
    end do
    if (any (qn_mask_beam .neqv. interaction_get_mask (beam_int))) then
       call beam_write (sfchain%beam)
       call msg_fatal (" Beam polarization/color/flavor incompatible with structure functions")
    end if
    eval_int => beam_int
    do i = 1, size (sfchain%strfun)
       qn_mask_conn = new_quantum_numbers_mask (.false., .false., .true.)
       call evaluator_init_product (sfchain%eval(i), eval_int, &
            sfchain%strfun(i)%int, qn_mask_conn)
       if (evaluator_is_empty (sfchain%eval(i))) then
          call msg_fatal ("Mismatch in beam and structure-function chain")
          if (present (ok))  ok = .false.
          return
       end if
       eval_int => evaluator_get_int_ptr (sfchain%eval(i))
       do j = 1, size (sfchain%coll_index)
          sfchain%coll_index(i) = interaction_find_link (eval_int, &
               sfchain%strfun(sfchain%last_strfun(i))%int, &
               sfchain%out_index(i))
       end do
       if (any (sfchain%coll_index == 0)) &
            call msg_bug ("Structure functions: " &
            // "colliding particles can't be determined")
    end do
    if (present (ok))  ok = .true.
  end subroutine strfun_chain_make_evaluators

  subroutine strfun_chain_set_kinematics (sfchain, r)
    type(strfun_chain_t), intent(inout) :: sfchain
    real(default), dimension(:), intent(in) :: r
    real(default), dimension(size(r)) :: x
    integer :: i, n, n1
    if (size (r) == sfchain%n_parameters_tot) then
       x = r
       sfchain%mapping_factor = 1
       do i = 1, size (sfchain%sf_mapping)
          call strfun_mapping_apply &
               (sfchain%sf_mapping(i), x, sfchain%mapping_factor)
       end do
       n = 0
       do i = 1, size (sfchain%strfun)
          call interaction_receive_momenta (sfchain%strfun(i)%int)
          n1 = sfchain%n_parameters(i)
          call strfun_set_kinematics (sfchain%strfun(i), x(n+1:n+n1))
          n = n + n1
       end do
       do i = 1, size (sfchain%strfun)
          call evaluator_receive_momenta (sfchain%eval(i))
       end do
    else
       call msg_bug ("Structure functions: mismatch in number of parameters")
    end if
  end subroutine strfun_chain_set_kinematics

  subroutine strfun_chain_evaluate (sfchain, scale)
    type(strfun_chain_t), intent(inout) :: sfchain
    real(default), intent(in) :: scale
    integer :: i
    do i = size (sfchain%strfun), 1, -1
       call strfun_apply (sfchain%strfun(i), scale)
    end do
    do i = 1, size (sfchain%eval)
       call evaluator_evaluate (sfchain%eval(i))
    end do
  end subroutine strfun_chain_evaluate

  subroutine strfun_test ()
    use os_interface, only: os_data_t
    type(os_data_t) :: os_data
    type(model_t), pointer :: model
    print *, "*** Read model file"
    call syntax_model_file_init ()
    call model_list_read_model &
         (var_str("QCD"), var_str("test.mdl"), os_data, model)
    call syntax_model_file_final ()
    print *, "***********************************************************"
    call isr_test (model)
    print *, "***********************************************************"
    call epa_test (model)
    print *, "***********************************************************"
    call lhapdf_test (model)
  end subroutine strfun_test

  subroutine isr_test (model)
    use flavors
    use polarizations
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2) :: flv
    type(polarization_t), dimension(2) :: pol
    type(beam_data_t), target :: beam_data
    type(isr_data_t), dimension(2) :: isr_data
    type(strfun_chain_t), target :: sfchain
    integer :: i
    print *, "*** ISR test"
    call flavor_init (flv, (/11, -11/), model)
    call polarization_init_unpolarized (pol(1), flv(1))
    call polarization_init_unpolarized (pol(2), flv(2))
    call beam_data_init_sqrts (beam_data, 500._default, flv, pol)
    do i = 1, 2
       call isr_data_init (isr_data(i), &
            model, flv(i), 0.06_default, 500._default, 0.511e-3_default)
    end do
    call strfun_chain_init (sfchain, beam_data, 2, 0)
    call strfun_chain_set_strfun (sfchain, 1, 1, isr_data(1), 1)
    call strfun_chain_set_strfun (sfchain, 2, 2, isr_data(2), 3)
    call strfun_chain_make_evaluators (sfchain)
    call strfun_chain_set_kinematics &
         (sfchain, (/0.8_default, 0.4_default, 0.5_default, 0.2_default/))
    call strfun_chain_evaluate (sfchain, 0._default)
    call strfun_chain_write (sfchain)
    call strfun_chain_final (sfchain)
  end subroutine isr_test

  subroutine epa_test (model)
    use flavors
    use polarizations
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2) :: flv
    type(polarization_t), dimension(2) :: pol
    type(beam_data_t) :: beam_data
    type(epa_data_t) :: epa_data1
    type(epa_data_t) :: epa_data2
    type(strfun_chain_t), target :: sfchain
    print *, "*** EPA test"
    call flavor_init (flv, (/2, 1/), model)
    ! Prepare beams
    call polarization_init_circular (pol(1), flv(1), 0.3_default)
    call polarization_init_unpolarized (pol(2), flv(2))
    call beam_data_init_sqrts (beam_data, 1000._default, flv, pol)
    call strfun_chain_init (sfchain, beam_data, 2, 0)
    ! Initialize EPA for both
    call epa_data_init (epa_data1, model, &
         flv(1), 0.06_default, 1.e-6_default, 0._default, 500._default, &
         511.e-6_default)
    call epa_data_init (epa_data2, model, &
         flv(2), 0.06_default, 1.e-6_default, 1._default, 500._default)
    call strfun_chain_set_strfun (sfchain, 1, 1, epa_data1, 1)
    call strfun_chain_set_strfun (sfchain, 2, 2, epa_data2, 3)
!    call strfun_chain_write (sfchain); stop
    call strfun_chain_make_evaluators (sfchain)
    call strfun_chain_set_kinematics &
         (sfchain, (/0.8_default, 0.4_default, 0.5_default, 0.2_default/))
    call strfun_chain_evaluate (sfchain, 0._default)
    call strfun_chain_write (sfchain)
    ! Clean up
    call beam_data_final (beam_data)
    call polarization_final (pol)
    call strfun_chain_final (sfchain)
  end subroutine epa_test

  subroutine lhapdf_test (model)
    use flavors
    use polarizations
    type(model_t), intent(in), target :: model
    type(beam_data_t) :: beam_data
    type(flavor_t), dimension(2) :: flv
    type(polarization_t), dimension(2) :: pol
    type(lhapdf_data_t), dimension(2) :: data
    type(lhapdf_status_t) :: lhapdf_status
    type(strfun_chain_t), target :: sfchain
    real(default) :: scale
    print *, "*** LHAPDF test"
    call flavor_init (flv, (/ -PROTON, PHOTON /), model)
    call polarization_init_unpolarized (pol(1), flv(1))
    call polarization_init_unpolarized (pol(2), flv(2))
    call beam_data_init_sqrts (beam_data, 2000._default, flv, pol)
    call strfun_chain_init (sfchain, beam_data, 2, 0)
    call lhapdf_data_init (data(1), lhapdf_status, model, flv(1), member=1)
    call lhapdf_data_init (data(2), lhapdf_status, model, flv(2), &
         file=var_str("SASG.LHgrid"), photon_scheme=1)
    call lhapdf_data_set_mask (data(2), &
         (/.false.,.false.,.false., .true., .true., .true., &
           .false., &
           .true., .true., .true., .false., .false., .false. /))
!    call strfun_chain_write (sfchain); stop
    call strfun_chain_set_strfun (sfchain, 1, 1, data(1), 1) 
    call strfun_chain_set_strfun (sfchain, 2, 2, data(2), 1)
!    call strfun_chain_write (sfchain); stop
    call strfun_chain_make_evaluators (sfchain)
    call strfun_chain_set_kinematics (sfchain, (/0.9_default, 0.4_default/))
    scale = 1.e3_default
    call strfun_chain_evaluate (sfchain, scale)
    call strfun_chain_write (sfchain)
    call strfun_chain_final (sfchain)
  end subroutine lhapdf_test


end module strfun
