! WHIZARD 2.1.1 September 18 2012
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

module strfun_config

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use tao_random_numbers !NODEP!
  use pdf_builtin !NODEP!
  use md5
  use models
  use flavors
  use sf_isr
  use sf_epa
  use sf_ewa
  use sf_circe1
  use sf_circe2
  use sf_escan
  use sf_beam_events
  use sf_lhapdf
  use sf_pdf_builtin
  use sf_user
  use strfun
  use processes

  implicit none
  private

  public :: STRF_NONE
  public :: STRF_LHAPDF
  public :: STRF_ISR
  public :: STRF_EPA
  public :: STRF_EWA
  public :: STRF_CIRCE1
  public :: STRF_CIRCE2
  public :: STRF_ESCAN
  public :: STRF_BEVT
  public :: STRF_PDF_BUILTIN
  public :: STRF_USER
  public :: sf_data_t
  public :: sf_data_affects_beam
  public :: sf_data_get_n_parameters
  public :: sf_data_setup_mapping
  public :: sf_data_init_lhapdf
  public :: sf_data_init_pdf_builtin
  public :: sf_data_init_isr
  public :: sf_data_init_epa
  public :: sf_data_init_ewa
  public :: sf_data_init_circe1
  public :: sf_data_init_circe2
  public :: sf_data_init_escan
  public :: sf_data_init_beam_events
  public :: sf_data_init_user
  public :: sf_list_t
  public :: sf_list_write
  public :: sf_list_append
  public :: sf_list_freeze
  public :: sf_list_final
  public :: sf_list_get_n_strfun
  public :: sf_list_get_md5sum
  public :: sf_list_compute_md5sum
  public :: sf_list_get_lhapdf_data_ptr
  public :: sf_list_get_pdf_builtin_data_ptr
  public :: sf_list_transfer_to_process
  public :: sf_list_setup_mappings
  public :: lhapdf_status_t
  public :: lhapdf_status_reset

  type :: sf_mapping_t
     private
     integer, dimension(:), allocatable :: index
     integer :: type = SFM_NONE
     real(default), dimension(:), allocatable :: par
  end type sf_mapping_t

  type :: sf_data_t
     private
     integer :: type = STRF_NONE
     logical, dimension(2) :: affects_beam = .false.
     integer :: n_parameters = 0
     type(lhapdf_data_t) :: lhapdf
     type(pdf_builtin_data_t) :: pdf_builtin
     type(isr_data_t) :: isr
     type(epa_data_t) :: epa
     type(ewa_data_t) :: ewa     
     type(circe1_data_t) :: circe1         
     type(circe2_data_t) :: circe2
     type(escan_data_t) :: escan
     type(beam_events_data_t) :: beam_events
     type(sf_user_data_t) :: user
     logical :: has_mapping = .false.
     type(sf_mapping_t) :: mapping
     type(sf_data_t), pointer :: next => null ()
  end type sf_data_t

  type :: sf_list_t
     private
     integer :: n_strfun = 0
     logical :: multichannel = .false.
     integer :: n_mapping = 0
     integer, dimension(2) :: global_mapping_index = 0
     integer :: global_mapping_type = SFM_NONE
     real(default) :: global_mapping_par = 1
     type(sf_data_t), pointer :: first => null ()
     type(sf_data_t), pointer :: last => null ()
     character(32) :: md5sum = ""
  end type sf_list_t


contains

  subroutine sf_mapping_write (sf_mapping, unit)
    type(sf_mapping_t), intent(in) :: sf_mapping
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(1x,A,I0,10(', #',I0))")  "Mapping for parameters #", &
         sf_mapping%index
    select case (sf_mapping%type)
    case (SFM_NONE);  write (u, "(3x,A)")  "[none]"
    case (SFM_PAIR);  write (u, "(3x,A)")  "Pair mapping"
    end select
    if (allocated (sf_mapping%par)) then
       write (u, "(3x,A)", advance="no")  "Parameters = "
       write (u, *) sf_mapping%par
    end if
  end subroutine sf_mapping_write
     
  subroutine sf_data_write (sf_data, unit, md5, beam_fmt)
    type(sf_data_t), intent(in) :: sf_data
    integer, intent(in), optional :: unit
    integer :: u
    logical, intent(in), optional :: md5
    logical, intent(in), optional :: beam_fmt
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(A)", advance="no")  "Structure function"
    if (all (sf_data%affects_beam)) then
       write (u, "(1x,A)") "(both beams)"
    else if (sf_data%affects_beam(1)) then
       write (u, "(1x,A)") "(beam 1)"
    else if (sf_data%affects_beam(2)) then
       write (u, "(1x,A)") "(beam 2)"
    else
       write (u, "(1x,A)") "(no beams)"
    end if
    select case (sf_data%type)
    case (STRF_NONE)
       write (u, "(1x,A)") "[none]"
    case (STRF_LHAPDF)
       call lhapdf_data_write (sf_data%lhapdf, unit, md5, beam_fmt)
    case (STRF_PDF_BUILTIN)
       call pdf_builtin_data_write (sf_data%pdf_builtin, unit, md5, beam_fmt)
    case (STRF_ISR)
       call isr_data_write (sf_data%isr, unit, md5)
    case (STRF_EPA)
       call epa_data_write (sf_data%epa, unit, md5)
    case (STRF_EWA)
       call ewa_data_write (sf_data%ewa, unit, md5)
    case (STRF_CIRCE1)
       call circe1_data_write (sf_data%circe1, unit, md5)
    case (STRF_CIRCE2)
       call circe2_data_write (sf_data%circe2, unit, md5)
    case (STRF_ESCAN)
       call escan_data_write (sf_data%escan, unit, md5)
    case (STRF_BEVT)
       call beam_events_data_write (sf_data%beam_events, unit, md5)
    case (STRF_USER)
       call sf_user_data_write (sf_data%user, unit, md5)
    end select
    write (u, *)  "n_parameters  = ", sf_data%n_parameters
    if (sf_data%has_mapping) then
       call sf_mapping_write (sf_data%mapping, unit)
    end if
  end subroutine sf_data_write

  function sf_data_affects_beam (sf_data) result (affects_beam)
    logical, dimension(2) :: affects_beam
    type(sf_data_t), intent(in) :: sf_data
    affects_beam = sf_data%affects_beam
  end function sf_data_affects_beam

  function sf_data_get_n_parameters (sf_data) result (n_parameters)
    integer :: n_parameters
    type(sf_data_t), intent(in) :: sf_data
    n_parameters = sf_data%n_parameters
  end function sf_data_get_n_parameters

  function sf_data_get_lhapdf_data_ptr (sf_data) result (lhapdf_data)
    type(sf_data_t), intent(in), target :: sf_data
    type(lhapdf_data_t), pointer :: lhapdf_data
    select case (sf_data%type)
    case (STRF_LHAPDF)
       lhapdf_data => sf_data%lhapdf
    case default
       lhapdf_data => null ()
    end select
  end function sf_data_get_lhapdf_data_ptr

  function sf_data_get_pdf_builtin_data_ptr (sf_data) result (pdf_builtin_data)
    type(sf_data_t), intent(in), target :: sf_data
    type(pdf_builtin_data_t), pointer :: pdf_builtin_data
    select case (sf_data%type)
    case (STRF_PDF_BUILTIN)
       pdf_builtin_data => sf_data%pdf_builtin
    case default
       pdf_builtin_data => null ()
    end select
  end function sf_data_get_pdf_builtin_data_ptr

  subroutine sf_data_setup_mapping (sf_data, type, index, par)
    type(sf_data_t), intent(inout) :: sf_data
    integer, intent(in) :: type
    integer, dimension(:), intent(in) :: index
    real(default), intent(in) :: par
    sf_data%mapping%type = type
    allocate (sf_data%mapping%index (size (index)))
    sf_data%mapping%index = index
    allocate (sf_data%mapping%par (1))
    sf_data%mapping%par = par
    sf_data%has_mapping = .true.
  end subroutine sf_data_setup_mapping

  subroutine sf_data_init_lhapdf (sf_data, i, &
       lhapdf_status, model, flv, prefix, file, member, photon_scheme)
    type(sf_data_t), intent(out) :: sf_data
    integer, intent(in) :: i
    type(lhapdf_status_t), intent(inout) :: lhapdf_status
    type(model_t), intent(in), target :: model
    type(flavor_t), intent(in) :: flv
    type(string_t), intent(in), optional :: prefix, file
    integer, intent(in), optional :: member
    integer, intent(in), optional :: photon_scheme
    sf_data%type = STRF_LHAPDF
    call lhapdf_data_init (sf_data%lhapdf, lhapdf_status, &
               model, flv, prefix, file, member, photon_scheme)
    sf_data%affects_beam(i) = .true.
    sf_data%n_parameters = 1
  end subroutine sf_data_init_lhapdf

  subroutine sf_data_init_pdf_builtin (sf_data, i, &
       pdf_builtin_status, model, flv, name, path)
    type(sf_data_t), intent(out) :: sf_data
    integer, intent(in) :: i
    type(pdf_builtin_status_t), intent(inout) :: pdf_builtin_status
    type(model_t), intent(in), target :: model
    type(flavor_t), intent(in) :: flv
    type(string_t), intent(in), optional :: name, path
    sf_data%type = STRF_PDF_BUILTIN
    call pdf_builtin_init (sf_data%pdf_builtin, pdf_builtin_status, &
         model, flv, name, path)
    sf_data%affects_beam(i) = .true.
    sf_data%n_parameters = 1
  end subroutine sf_data_init_pdf_builtin

  subroutine sf_data_init_isr &
       (sf_data, i, model, flv, recoil, alpha, q_max, mass, order)
    type(sf_data_t), intent(out) :: sf_data
    integer, intent(in) :: i
    type(model_t), intent(in), target :: model
    type(flavor_t), intent(in) :: flv
    logical, intent(in) :: recoil
    real(default), intent(in) :: alpha, q_max
    real(default), intent(in), optional :: mass
    integer, intent(in), optional :: order
    sf_data%type = STRF_ISR
    call isr_data_init (sf_data%isr, model, flv, alpha, q_max, mass)
    if (present (order))  call isr_data_set_order (sf_data%isr, order)
    call isr_data_check (sf_data%isr)
    sf_data%affects_beam(i) = .true.
    if (recoil) then
       sf_data%n_parameters = 3
    else
       sf_data%n_parameters = 1
    end if
  end subroutine sf_data_init_isr

  subroutine sf_data_init_epa &
       (sf_data, i, model, flv, recoil, alpha, x_min, q_min, E_max, mass)
    type(sf_data_t), intent(out) :: sf_data
    integer, intent(in) :: i
    type(model_t), intent(in), target :: model
    type(flavor_t), intent(in) :: flv
    logical, intent(in) :: recoil
    real(default), intent(in) :: alpha, x_min, q_min, E_max
    real(default), intent(in), optional :: mass
    sf_data%type = STRF_EPA
    call epa_data_init (sf_data%epa, &
         model, flv, alpha, x_min, q_min, E_max, mass)
    call epa_data_check (sf_data%epa)
    sf_data%affects_beam(i) = .true.
    if (recoil) then
       sf_data%n_parameters = 3
    else
       sf_data%n_parameters = 1
    end if
  end subroutine sf_data_init_epa

  subroutine sf_data_init_ewa &
       (sf_data, i, model, flv, x_min, q_min, pt_max, sqrts, &
        keep_momentum, keep_energy, mass)
    type(sf_data_t), intent(out) :: sf_data
    integer, intent(in) :: i
    type(model_t), intent(in), target :: model
    type(flavor_t), intent(in) :: flv
    real(default), intent(in) ::  x_min, q_min, pt_max, sqrts
    logical, intent(in) :: keep_momentum, keep_energy
    real(default), intent(in), optional :: mass
    sf_data%type = STRF_EWA
    call ewa_data_init (sf_data%ewa, &
            model, flv, x_min, q_min, pt_max, sqrts, &
            keep_momentum, keep_energy, mass)
    call ewa_data_check (sf_data%ewa)
    sf_data%affects_beam(i) = .true.
    if (keep_momentum .or. keep_energy) then
       sf_data%n_parameters = 3
    else
       sf_data%n_parameters = 1
    end if 
  end subroutine sf_data_init_ewa

  subroutine sf_data_init_circe1 (sf_data, &
       model, flv, sqrts, photon, generate, rng, map, ver, rev, acc, chat)
    type(sf_data_t), intent(out) :: sf_data
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2), intent(in) :: flv
    real(default), intent(in) :: sqrts
    logical, dimension(2), intent(in) :: photon
    logical, intent(in) :: generate, map
    type(tao_random_state), intent(in), target :: rng
    integer, intent(in) :: ver, rev, acc, chat 
    sf_data%type = STRF_CIRCE1
    call circe1_data_init (sf_data%circe1, &
         model, flv, sqrts, photon, generate, rng, map, ver, rev, acc, chat)
    call circe1_data_check (sf_data%circe1)
    sf_data%affects_beam = .true.
    sf_data%n_parameters = 2
  end subroutine sf_data_init_circe1

  subroutine sf_data_init_circe2 (sf_data, &
        flv, generate, rng, map, file, design, sqrts, polarized)
    type(sf_data_t), intent(out) :: sf_data
    type(flavor_t), dimension(2), intent(in) :: flv
    logical, intent(in) :: generate
    type(tao_random_state), intent(in), target :: rng
    logical, intent(in) :: map
    type(string_t), intent(in) :: file, design
    real(default), intent(in) :: sqrts
    logical, intent(in) :: polarized
    sf_data%type = STRF_CIRCE2
    call circe2_data_init (sf_data%circe2, &
         flv, generate, rng, map, file, design, sqrts, polarized)
    sf_data%affects_beam = .true.
    sf_data%n_parameters = 2
  end subroutine sf_data_init_circe2

  subroutine sf_data_init_escan (sf_data, affects_beam, flv, sqrts)
    type(sf_data_t), intent(out) :: sf_data
    logical, dimension(2), intent(in) :: affects_beam
    type(flavor_t), dimension(2), intent(in) :: flv
    real(default), intent(in) :: sqrts
    sf_data%type = STRF_ESCAN
    call escan_data_init (sf_data%escan, affects_beam, flv, sqrts)
    sf_data%affects_beam = affects_beam
    sf_data%n_parameters = count (affects_beam)
  end subroutine sf_data_init_escan

  subroutine sf_data_init_beam_events &
       (sf_data, affects_beam, flv, file, warn_eof)
    type(sf_data_t), intent(out) :: sf_data
    logical, dimension(2), intent(in) :: affects_beam
    type(flavor_t), dimension(2), intent(in) :: flv
    type(string_t), intent(in) :: file
    logical, intent(in) :: warn_eof
    sf_data%type = STRF_BEVT
    call beam_events_data_init (sf_data%beam_events, &
         affects_beam, flv, file, warn_eof)
    call beam_events_data_open (sf_data%beam_events)
    sf_data%affects_beam = affects_beam
    sf_data%n_parameters = 0
  end subroutine sf_data_init_beam_events

  subroutine sf_data_init_user (sf_data, i, flv, name, model)
    type(sf_data_t), intent(out) :: sf_data
    integer, intent(in) :: i
    type(flavor_t), dimension(2), intent(in) :: flv
    type(string_t), intent(in) :: name
    type(model_t), intent(in), target :: model
    sf_data%type = STRF_USER
    call sf_user_data_init (sf_data%user, name, flv, model)
    select case (sf_user_data_get_n_in (sf_data%user))
    case (1)
       sf_data%affects_beam(i) = .true.
    case (2)
       sf_data%affects_beam = .true.
    end select
    sf_data%n_parameters = sf_user_data_get_n_dim (sf_data%user)
  end subroutine sf_data_init_user

  subroutine sf_list_write (sf_list, unit, md5, beam_fmt)
    type(sf_list_t), intent(in) :: sf_list
    integer, intent(in), optional :: unit
    integer :: u
    logical, intent(in), optional :: md5
    logical, intent(in), optional :: beam_fmt
    type(sf_data_t), pointer :: current
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(A)")  "Structure function list"
    if (sf_list%multichannel .and. sf_list%n_mapping /= 0) then
       write (u, "(2x,A,L1)")  "Global structure function mapping:"
       write (u, "(4x,A,2I0)")  "Index     = ", sf_list%global_mapping_index
       write (u, "(4x,A,I0)")   "Type      = ", sf_list%global_mapping_type
       write (u, "(4x,A)", advance="no")   "Parameter = "
       write (u, *)  sf_list%global_mapping_par
    end if
    if (associated (sf_list%first)) then
       current => sf_list%first
       do while (associated (current))
          call sf_data_write (current, unit, md5, beam_fmt)
          current => current%next
       end do
    else
       write (u, "(1x,A)") "[empty]"
    end if
  end subroutine sf_list_write

  subroutine sf_list_append (sf_list, sf_data)
    type(sf_list_t), intent(inout) :: sf_list
    type(sf_data_t), intent(in), target :: sf_data
    if (associated (sf_list%last)) then
       sf_list%last%next => sf_data
    else
       sf_list%first => sf_data
    end if
    sf_list%last => sf_data
    sf_list%n_strfun = sf_list%n_strfun + 1
  end subroutine sf_list_append
       
  subroutine sf_list_freeze (sf_list, multichannel)
    type(sf_list_t), intent(inout) :: sf_list
    logical, intent(in) :: multichannel
    type(sf_data_t), pointer :: sf_data
    integer :: i_par
    sf_list%multichannel = multichannel
    if (multichannel) then
       sf_list%n_mapping = 0
       i_par = 0
       sf_data => sf_list%first
       do while (associated (sf_data))
          if (sf_data%has_mapping) then
             sf_list%n_mapping = 1
             sf_list%global_mapping_index = i_par + sf_data%mapping%index
             sf_list%global_mapping_type = sf_data%mapping%type
             select case (sf_data%mapping%type)
             case (SFM_PAIR)
                if (allocated (sf_data%mapping%par)) then
                   sf_list%global_mapping_par = &
                        max (sf_list%global_mapping_par, sf_data%mapping%par(1))
                end if
             end select
          end if
          i_par = i_par + sf_data%n_parameters
          sf_data => sf_data%next
       end do
    else
       sf_list%n_mapping = 0
       sf_data => sf_list%first
       do while (associated (sf_data))
          if (sf_data%has_mapping) then
             sf_list%n_mapping = sf_list%n_mapping + 1
          end if
          sf_data => sf_data%next
       end do
    end if
  end subroutine sf_list_freeze

  subroutine sf_list_final (sf_list)
    type(sf_list_t), intent(inout) :: sf_list
    type(sf_data_t), pointer :: sf_data
    do while (associated (sf_list%first))
       sf_data => sf_list%first
       sf_list%first => sf_list%first%next
       deallocate (sf_data)
    end do
    sf_list%last => null ()
    sf_list%n_strfun = 0
  end subroutine sf_list_final

  function sf_list_get_n_strfun (sf_list) result (n)
    integer :: n
    type(sf_list_t), intent(in) :: sf_list
    n = sf_list%n_strfun
  end function sf_list_get_n_strfun

  function sf_list_get_md5sum (sf_list) result (sf_md5sum)
    character(32) :: sf_md5sum
    type(sf_list_t), intent(in) :: sf_list
    sf_md5sum = sf_list%md5sum
  end function sf_list_get_md5sum

  subroutine sf_list_compute_md5sum (sf_list)
    type(sf_list_t), intent(inout) :: sf_list
    integer :: unit
    unit = free_unit ()
    open (unit = unit, status = "scratch", action = "readwrite")
    call sf_list_write (sf_list, unit, md5=.true.)
    rewind (unit)
    sf_list%md5sum = md5sum (unit)
    close (unit)
  end subroutine sf_list_compute_md5sum

  function sf_list_get_lhapdf_data_ptr (sf_list) result (lhapdf_data)
    type(sf_list_t), intent(in) :: sf_list
    type(lhapdf_data_t), pointer :: lhapdf_data
    type(sf_data_t), pointer :: sf_data
    lhapdf_data => null ()
    sf_data => sf_list%first
    FIND_LHAPDF: do while (associated (sf_data))
       lhapdf_data => sf_data_get_lhapdf_data_ptr (sf_data)
       if (associated (lhapdf_data))  exit FIND_LHAPDF
       sf_data => sf_data%next
    end do FIND_LHAPDF
  end function sf_list_get_lhapdf_data_ptr

  function sf_list_get_pdf_builtin_data_ptr (sf_list) result (pdf_builtin_data)
    type(sf_list_t), intent(in) :: sf_list
    type(pdf_builtin_data_t), pointer :: pdf_builtin_data
    type(sf_data_t), pointer :: sf_data
    pdf_builtin_data => null ()
    sf_data => sf_list%first
    FIND_PDF_BUILTIN: do while (associated (sf_data))
       pdf_builtin_data => sf_data_get_pdf_builtin_data_ptr (sf_data)
       if (associated (pdf_builtin_data))  exit FIND_PDF_BUILTIN
       sf_data => sf_data%next
    end do FIND_PDF_BUILTIN
  end function sf_list_get_pdf_builtin_data_ptr

  subroutine sf_list_transfer_to_process (sf_list, process)
    type(sf_list_t), intent(in) :: sf_list
    type(process_t), intent(inout), target :: process
    type(sf_data_t), pointer :: sf_data
    integer :: i_sf, line
    i_sf = 0
    sf_data => sf_list%first
    do while (associated (sf_data))
       if (all (sf_data%affects_beam)) then
          line = 0
       else if (sf_data%affects_beam(1)) then
          line = 1
       else if (sf_data%affects_beam(2)) then
          line = 2
       end if
       i_sf = i_sf + 1
       select case (sf_data%type)
       case (STRF_LHAPDF)
          call process_set_strfun &
               (process, i_sf, line, sf_data%lhapdf, sf_data%n_parameters)
       case (STRF_PDF_BUILTIN)
          call process_set_strfun &
               (process, i_sf, line, sf_data%pdf_builtin, sf_data%n_parameters)
       case (STRF_ISR)
          call process_set_strfun &
               (process, i_sf, line, sf_data%isr, sf_data%n_parameters)
       case (STRF_EPA)
          call process_set_strfun &
               (process, i_sf, line, sf_data%epa, sf_data%n_parameters)
       case (STRF_EWA)
          call process_set_strfun &
               (process, i_sf, line, sf_data%ewa, sf_data%n_parameters)
       case (STRF_CIRCE1)
          call process_set_strfun &
               (process, i_sf, line, sf_data%circe1, sf_data%n_parameters)
       case (STRF_CIRCE2)
          call process_set_strfun &
               (process, i_sf, line, sf_data%circe2, sf_data%n_parameters)
       case (STRF_ESCAN)
          call process_set_strfun &
               (process, i_sf, line, sf_data%escan, sf_data%n_parameters)
       case (STRF_BEVT)
          call process_set_strfun &
               (process, i_sf, line, sf_data%beam_events, sf_data%n_parameters)
       case (STRF_USER)
          call process_set_strfun &
               (process, i_sf, line, sf_data%user, sf_data%n_parameters)
       end select
       sf_data => sf_data%next
    end do
  end subroutine sf_list_transfer_to_process
       
  subroutine sf_list_setup_mappings (sf_list, process)
    type(sf_list_t), intent(in) :: sf_list
    type(process_t), intent(inout), target :: process
    type(sf_data_t), pointer :: sf_data
    integer :: i_map, i_par, channel
    logical :: has_s_mapping
    real(default) :: sqrts, mass, width
    i_map = 0
    i_par = 0
    if (sf_list%multichannel) then
       call process_allocate_strfun_mappings &
            (process, sf_list%multichannel)
       if (sf_list%n_mapping /= 0) then
          select case (sf_list%global_mapping_type)
          case (SFM_PAIR)
             sqrts = process_get_sqrts (process)
             do channel = 1, process_get_n_channels (process)
                call process_get_s_mapping &
                     (process, channel, has_s_mapping, mass, width)
                if (has_s_mapping) then
                   call process_set_strfun_mapping &
                        (process, 1, channel, sf_list%global_mapping_index, &
                        SFM_PAIR_RESONANCE, &
                        (/sqrts, mass, width/))
                else
                   call process_set_strfun_mapping &
                        (process, 1, channel, sf_list%global_mapping_index, &
                        SFM_PAIR, &
                        (/sf_list%global_mapping_par/))
                end if
             end do
          case default
             call msg_bug ("Strfun mappings: inconsistent mapping type")
          end select
       end if
    else
       call process_allocate_strfun_mappings (process, &
            sf_list%multichannel, &
            sf_list%n_mapping)
       sf_data => sf_list%first
       do while (associated (sf_data))
          if (sf_data%has_mapping) then
             i_map = i_map + 1
             call process_set_strfun_mapping &
                  (process, i_map, 1, i_par + sf_data%mapping%index, &
                  sf_data%mapping%type, sf_data%mapping%par)
          end if
          i_par = i_par + sf_data%n_parameters
          sf_data => sf_data%next
       end do
    end if
  end subroutine sf_list_setup_mappings


end module strfun_config
