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

module strfun_config

  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use file_utils !NODEP!
  use diagnostics !NODEP!
  use tao_random_numbers !NODEP!
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
  public :: sf_data_t
  public :: sf_data_init_lhapdf
  public :: sf_data_init_isr
  public :: sf_data_init_epa
  public :: sf_data_init_ewa
  public :: sf_data_init_circe1
  public :: sf_data_init_circe2
  public :: sf_data_init_escan
  public :: sf_data_init_beam_events
  public :: sf_list_t
  public :: sf_list_append
  public :: sf_list_freeze
  public :: sf_list_final
  public :: sf_list_get_n_strfun
  public :: sf_list_get_n_mapping
  public :: sf_list_get_md5sum
  public :: sf_list_compute_md5sum
  public :: sf_list_transfer_to_process
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
     type(lhapdf_data_t), dimension(2) :: lhapdf
     type(isr_data_t), dimension(2) :: isr
     type(epa_data_t), dimension(2) :: epa
     type(ewa_data_t), dimension(2) :: ewa     
     type(circe1_data_t) :: circe1         
     type(circe2_data_t) :: circe2
     type(escan_data_t) :: escan
     type(beam_events_data_t) :: beam_events
     logical :: has_mapping = .false.
     type(sf_mapping_t) :: mapping
     type(sf_data_t), pointer :: next => null ()
  end type sf_data_t

  type :: sf_list_t
     private
     integer :: n_strfun = 0
     integer :: n_mapping = 0
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
    case (SFM_NONE);     write (u, "(3x,A)")  "[none]"
    case (SFM_PDFPAIR);  write (u, "(3x,A)")  "PDF pair mapping"
    case (SFM_ISRPAIR);  write (u, "(3x,A)")  "ISR pair mapping"
    case (SFM_EPAPAIR);  write (u, "(3x,A)")  "EPA pair mapping"
    case (SFM_EWAPAIR);  write (u, "(3x,A)")  "EWA pair mapping"
    case (SFM_CIRCE1PAIR);  write (u, "(3x,A)")  "CIRCE1 pair mapping"            
    case (SFM_CIRCE2PAIR);  write (u, "(3x,A)")  "CIRCE2 pair mapping"            
    end select
    if (allocated (sf_mapping%par)) then
       write (u, "(3x,A)", advance="no")  "Parameters = "
       write (u, *) sf_mapping%par
    end if
  end subroutine sf_mapping_write
     
  subroutine sf_data_write (sf_data, unit)
    type(sf_data_t), intent(in) :: sf_data
    integer, intent(in), optional :: unit
    integer :: u, i
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(A)")  "Structure function"
    do i = 1, 2
       if (sf_data%affects_beam(i)) then
          select case (sf_data%type)
          case (STRF_NONE)
             write (u, "(1x,A)") "[none]"
          case (STRF_LHAPDF)
             call lhapdf_data_write (sf_data%lhapdf(i), unit)             
          case (STRF_ISR)
             call isr_data_write (sf_data%isr(i), unit)
          case (STRF_EPA)
             call epa_data_write (sf_data%epa(i), unit)
          case (STRF_EWA)
             call ewa_data_write (sf_data%ewa(i), unit)
          end select
       end if
    end do
    select case (sf_data%type)
    case (STRF_CIRCE1)
       call circe1_data_write (sf_data%circe1, unit)
    case (STRF_CIRCE2)
       call circe2_data_write (sf_data%circe2, unit)
    case (STRF_ESCAN)
       call escan_data_write (sf_data%escan, unit)
    case (STRF_BEVT)
       call beam_events_data_write (sf_data%beam_events, unit)
    end select
    write (u, *)  "affects beams = ", sf_data%affects_beam
    write (u, *)  "n_parameters  = ", sf_data%n_parameters
    if (sf_data%has_mapping) then
       call sf_mapping_write (sf_data%mapping, unit)
    end if
  end subroutine sf_data_write

  subroutine sf_data_init_lhapdf &
       (sf_data, lhapdf_status, model, flv, file, member, photon_scheme)
    type(sf_data_t), intent(inout) :: sf_data
    type(lhapdf_status_t), intent(inout) :: lhapdf_status
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2), intent(in) :: flv
    type(string_t), intent(in), optional :: file
    integer, intent(in), optional :: member
    integer, intent(in), optional :: photon_scheme
    integer :: i
    do i = 1, 2
       if (sf_data%affects_beam(i)) then
          call lhapdf_data_init (sf_data%lhapdf(i), lhapdf_status, &
               model, flv(i), file, member, photon_scheme)
       end if
    end do
    if (all (sf_data%affects_beam)) then
       allocate (sf_data%mapping%index (2))
       sf_data%mapping%index = (/1, sf_data%n_parameters+1/)
       sf_data%mapping%type = SFM_PDFPAIR
       allocate (sf_data%mapping%par (1))
       sf_data%mapping%par = 2._default
       sf_data%has_mapping = .true.
    end if
  end subroutine sf_data_init_lhapdf

  subroutine sf_data_init_isr &
       (sf_data, model, flv, alpha, q_max, mass, order)
    type(sf_data_t), intent(inout) :: sf_data
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2), intent(in) :: flv
    real(default), intent(in) :: alpha, q_max
    real(default), intent(in), optional :: mass
    integer, intent(in), optional :: order
    integer :: i
    do i = 1, 2
       if (sf_data%affects_beam(i)) then
          call isr_data_init (sf_data%isr(i), &
               model, flv(i), alpha, q_max, mass)
          if (present (order)) &
               call isr_data_set_order (sf_data%isr(i), order)
          call isr_data_check (sf_data%isr(i))
       end if
    end do
!     if (all (sf_data%affects_beam)) then
!        allocate (sf_data%mapping%index (2))
!        sf_data%mapping%index = (/1, sf_data%n_parameters+1/)
!        sf_data%mapping%type = SFM_ISRPAIR
!        allocate (sf_data%mapping%par (1))
!        sf_data%mapping%par = 2._default
!        sf_data%has_mapping = .true.
!     end if
  end subroutine sf_data_init_isr

  subroutine sf_data_init_epa &
       (sf_data, model, flv, alpha, x_min, q_min, E_max, mass)
    type(sf_data_t), intent(inout) :: sf_data
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2), intent(in) :: flv
    real(default), intent(in) :: alpha, x_min, q_min, E_max
    real(default), intent(in), optional :: mass
    integer :: i
    do i = 1, 2
       if (sf_data%affects_beam(i)) then
          call epa_data_init (sf_data%epa(i), &
               model, flv(i), alpha, x_min, q_min, E_max, mass)
          call epa_data_check (sf_data%epa(i))
       end if
    end do
    if (all (sf_data%affects_beam)) then
       allocate (sf_data%mapping%index (2))
       sf_data%mapping%index = (/1, sf_data%n_parameters+1/)
       sf_data%mapping%type = SFM_EPAPAIR
       allocate (sf_data%mapping%par (1))
       sf_data%mapping%par = 1._default
       sf_data%has_mapping = .true.
    end if
  end subroutine sf_data_init_epa

  subroutine sf_data_init_ewa &
       (sf_data, model, flv, x_min, q_min, pt_max, sqrts, &
        keep_momentum, keep_energy, mass)
    type(sf_data_t), intent(inout) :: sf_data
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2), intent(in) :: flv
    real(default), intent(in) ::  x_min, q_min, pt_max, sqrts
    logical, intent(in) :: keep_momentum, keep_energy
    real(default), intent(in), optional :: mass
    integer :: i
    do i = 1, 2
       if (sf_data%affects_beam(i)) then
          call ewa_data_init (sf_data%ewa(i), &
               model, flv(i), x_min, q_min, pt_max, sqrts, &
               keep_momentum, keep_energy, mass)
          call ewa_data_check (sf_data%ewa(i))
       end if
    end do
    if (all (sf_data%affects_beam)) then
       allocate (sf_data%mapping%index (2))
       sf_data%mapping%index = (/1, sf_data%n_parameters+1/)
       sf_data%mapping%type = SFM_EWAPAIR
       allocate (sf_data%mapping%par (1))
       sf_data%mapping%par = 1._default
       sf_data%has_mapping = .true.
    end if
    if (keep_momentum .or. keep_energy) then
       sf_data%n_parameters = 3
    else
       sf_data%n_parameters = 1
    end if 
  end subroutine sf_data_init_ewa

  subroutine sf_data_init_circe1 (sf_data, &
       model, flv, sqrts, photon, generate, rng, map, ver, rev, acc, chat)
    type(sf_data_t), intent(inout) :: sf_data
    type(model_t), intent(in), target :: model
    type(flavor_t), dimension(2), intent(in) :: flv
    real(default), intent(in) :: sqrts
    logical, dimension(2), intent(in) :: photon
    logical, intent(in) :: generate, map
    type(tao_random_state), intent(in), target :: rng
    integer, intent(in) :: ver, rev, acc, chat 
    if (all (sf_data%affects_beam)) then
       call circe1_data_init (sf_data%circe1, &
            model, flv, sqrts, photon, generate, rng, map, ver, rev, acc, chat)
!        allocate (sf_data%mapping%index (2))
!        sf_data%mapping%index = (/1, sf_data%n_parameters+1/)
!        sf_data%mapping%type = SFM_CIRCE1PAIR
!        allocate (sf_data%mapping%par (1))
!        sf_data%mapping%par = 1._default
!        sf_data%has_mapping = .true.
       call circe1_data_check (sf_data%circe1)
    else
       call msg_fatal ("CIRCE1 beamstrahlung " &
            // "must be turned on/off for both beams simultaneously")
    end if
  end subroutine sf_data_init_circe1

  subroutine sf_data_init_circe2 (sf_data, &
        flv, generate, rng, map, file, design, sqrts, polarized)
    type(sf_data_t), intent(inout) :: sf_data
    type(flavor_t), dimension(2), intent(in) :: flv
    logical, intent(in) :: generate
    type(tao_random_state), intent(in), target :: rng
    logical, intent(in) :: map
    type(string_t), intent(in) :: file, design
    real(default), intent(in) :: sqrts
    logical, intent(in) :: polarized
    if (all (sf_data%affects_beam)) then
       call circe2_data_init (sf_data%circe2, &
            flv, generate, rng, map, file, design, sqrts, polarized)
    end if
  end subroutine sf_data_init_circe2

  subroutine sf_data_init_escan (sf_data, flv, sqrts)
    type(sf_data_t), intent(inout) :: sf_data
    type(flavor_t), dimension(2), intent(in) :: flv
    real(default), intent(in) :: sqrts
    call escan_data_init (sf_data%escan, &
         sf_data%affects_beam, flv, sqrts)
  end subroutine sf_data_init_escan

  subroutine sf_data_init_beam_events (sf_data, flv, file, warn_eof)
    type(sf_data_t), intent(inout) :: sf_data
    type(flavor_t), dimension(2), intent(in) :: flv
    type(string_t), intent(in) :: file
    logical, intent(in) :: warn_eof
    call beam_events_data_init (sf_data%beam_events, &
         sf_data%affects_beam, flv, file, warn_eof)
    call beam_events_data_open (sf_data%beam_events)
  end subroutine sf_data_init_beam_events

  subroutine sf_list_write (sf_list, unit)
    type(sf_list_t), intent(in) :: sf_list
    integer, intent(in), optional :: unit
    integer :: u
    type(sf_data_t), pointer :: current
    u = output_unit (unit);  if (u < 0)  return
    write (u, "(A)")  "Structure function list"
    if (associated (sf_list%first)) then
       current => sf_list%first
       do while (associated (current))
          call sf_data_write (current, unit)
          current => current%next
       end do
    else
       write (u, "(1x,A)") "[empty]"
    end if
  end subroutine sf_list_write

  subroutine sf_list_append (sf_list, type, affects_beam, n_parameters, current)
    type(sf_list_t), intent(inout) :: sf_list
    integer, intent(in) :: type
    logical, dimension(2), intent(in) :: affects_beam
    integer, intent(in) :: n_parameters
    type(sf_data_t), pointer :: current
    allocate (current)
    current%type = type
    current%affects_beam = affects_beam
    current%n_parameters = n_parameters
    if (associated (sf_list%last)) then
       sf_list%last%next => current
    else
       sf_list%first => current
    end if
    sf_list%last => current
    select case (current%type)
    case (STRF_CIRCE1, STRF_CIRCE2, STRF_ESCAN, STRF_BEVT)
       sf_list%n_strfun = sf_list%n_strfun + 1
    case default   
       sf_list%n_strfun = sf_list%n_strfun + count (affects_beam)
    end select
  end subroutine sf_list_append
       
  subroutine sf_list_freeze (sf_list)
    type(sf_list_t), intent(inout) :: sf_list
    type(sf_data_t), pointer :: current
    sf_list%n_mapping = 0
    current => sf_list%first
    do while (associated (current))
       if (current%has_mapping) then
          sf_list%n_mapping = sf_list%n_mapping + 1
       end if
       current => current%next
    end do
  end subroutine sf_list_freeze

  subroutine sf_list_final (sf_list)
    type(sf_list_t), intent(inout) :: sf_list
    type(sf_data_t), pointer :: current
    do while (associated (sf_list%first))
       current => sf_list%first
       sf_list%first => sf_list%first%next
       deallocate (current)
    end do
    sf_list%last => null ()
    sf_list%n_strfun = 0
  end subroutine sf_list_final

  function sf_list_get_n_strfun (sf_list) result (n)
    integer :: n
    type(sf_list_t), intent(in) :: sf_list
    n = sf_list%n_strfun
  end function sf_list_get_n_strfun

  function sf_list_get_n_mapping (sf_list) result (n)
    integer :: n
    type(sf_list_t), intent(in) :: sf_list
    n = sf_list%n_mapping
  end function sf_list_get_n_mapping

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
    call sf_list_write (sf_list, unit)
    rewind (unit)
    sf_list%md5sum = md5sum (unit)
    close (unit)
  end subroutine sf_list_compute_md5sum

  subroutine sf_list_transfer_to_process (sf_list, process)
    type(sf_list_t), intent(in) :: sf_list
    type(process_t), intent(inout), target :: process
    type(sf_data_t), pointer :: current
    integer :: i_sf, j, i_map, i_par
    i_sf = 0
    i_map = 0
    i_par = 0
    current => sf_list%first
    do while (associated (current))
       if (current%has_mapping) then
          i_map = i_map + 1
          call process_set_strfun_mapping &
               (process, i_map, i_par + current%mapping%index, &
                current%mapping%type, current%mapping%par)
       end if
       select case (current%type)
       case (STRF_CIRCE1)
          i_sf = i_sf + 1
          call process_set_strfun &
               (process, i_sf, 0, current%circe1, current%n_parameters)         
          i_par = i_par + current%n_parameters
       case (STRF_CIRCE2)
          i_sf = i_sf + 1
          call process_set_strfun &
               (process, i_sf, 0, current%circe2, current%n_parameters)
          i_par = i_par + current%n_parameters
       case (STRF_ESCAN)
          i_sf = i_sf + 1
          if (all (current%affects_beam)) then
             call process_set_strfun &
                  (process, i_sf, 0, current%escan, current%n_parameters)
          else if (current%affects_beam(1)) then
             call process_set_strfun &
                  (process, i_sf, 1, current%escan, current%n_parameters)
          else if (current%affects_beam(2)) then
             call process_set_strfun &
                  (process, i_sf, 2, current%escan, current%n_parameters)
          end if
          i_par = i_par + current%n_parameters
       case (STRF_BEVT)
          i_sf = i_sf + 1
          if (all (current%affects_beam)) then
             call process_set_strfun &
                  (process, i_sf, 0, current%beam_events, current%n_parameters)
          else if (current%affects_beam(1)) then
             call process_set_strfun &
                  (process, i_sf, 1, current%beam_events, current%n_parameters)
          else if (current%affects_beam(2)) then
             call process_set_strfun &
                  (process, i_sf, 2, current%beam_events, current%n_parameters)
          end if
          i_par = i_par + current%n_parameters
       case default
          do j = 1, 2
          if (current%affects_beam(j)) then
             i_sf = i_sf + 1
             select case (current%type)
             case (STRF_LHAPDF)
                call process_set_strfun &
                     (process, i_sf, j, current%lhapdf(j), current%n_parameters)
             case (STRF_ISR)
                call process_set_strfun &
                     (process, i_sf, j, current%isr(j), current%n_parameters)
             case (STRF_EPA)
                call process_set_strfun &
                     (process, i_sf, j, current%epa(j), current%n_parameters)
             case (STRF_EWA)
                call process_set_strfun &
                     (process, i_sf, j, current%ewa(j), current%n_parameters)
             end select
             i_par = i_par + current%n_parameters
          end if
          end do
       end select
       current => current%next
    end do
  end subroutine sf_list_transfer_to_process
       

end module strfun_config
