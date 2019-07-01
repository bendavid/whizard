! WHIZARD 2.2.3 Nov 30 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

module eio_data
  
  use kinds
  use io_units
  use iso_varying_string, string_t => varying_string
  use unit_tests
  use diagnostics

  use events

  public :: event_sample_data_t
  public :: eio_data_test

  type :: event_sample_data_t
     character(32) :: md5sum_prc = ""
     character(32) :: md5sum_cfg = ""
     logical :: unweighted = .true.
     logical :: negative_weights = .false.
     integer :: norm_mode = NORM_UNDEFINED
     integer :: n_beam = 0
     integer, dimension(2) :: pdg_beam = 0
     real(default), dimension(2) :: energy_beam = 0
     integer :: n_proc = 0
     integer :: n_evt = 0
     integer :: split_n_evt = 0
     integer :: split_index = 0
     real(default) :: total_cross_section = 0
     integer, dimension(:), allocatable :: proc_num_id
     integer :: n_alt = 0
     character(32), dimension(:), allocatable :: md5sum_alt
     real(default), dimension(:), allocatable :: cross_section
     real(default), dimension(:), allocatable :: error
   contains
     procedure :: init => event_sample_data_init
     procedure :: write => event_sample_data_write
  end type event_sample_data_t
  

contains
  
  subroutine event_sample_data_init (data, n_proc, n_alt)
    class(event_sample_data_t), intent(out) :: data
    integer, intent(in) :: n_proc
    integer, intent(in), optional :: n_alt
    data%n_proc = n_proc
    allocate (data%proc_num_id (n_proc), source = 0)
    allocate (data%cross_section (n_proc), source = 0._default)
    allocate (data%error (n_proc), source = 0._default)
    if (present (n_alt)) then
       data%n_alt = n_alt
       allocate (data%md5sum_alt (n_alt))
       data%md5sum_alt = ""
    end if
  end subroutine event_sample_data_init
  
  subroutine event_sample_data_write (data, unit)
    class(event_sample_data_t), intent(in) :: data
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit)
    write (u, "(1x,A)")  "Event sample properties:"
    write (u, "(3x,A,A,A)")  "MD5 sum (proc)   = '", data%md5sum_prc, "'"
    write (u, "(3x,A,A,A)")  "MD5 sum (config) = '", data%md5sum_cfg, "'"
    write (u, "(3x,A,L1)")  "unweighted       = ", data%unweighted
    write (u, "(3x,A,L1)")  "negative weights = ", data%negative_weights
    write (u, "(3x,A,A)")   "normalization    = ", &
         char (event_normalization_string (data%norm_mode))
    write (u, "(3x,A,I0)")  "number of beams  = ", data%n_beam
    write (u, "(5x,A,2(1x,I19))")  "PDG    = ", &
         data%pdg_beam(:data%n_beam)
    write (u, "(5x,A,2(1x,ES19.12))")  "Energy = ", &
         data%energy_beam(:data%n_beam)
    if (data%n_evt > 0) then
       write (u, "(3x,A,I0)")  "number of events = ", data%n_evt
    end if
    if (data%total_cross_section /= 0) then
       write (u, "(3x,A,ES19.12)")  "total cross sec. = ", &
            data%total_cross_section
    end if
    write (u, "(3x,A,I0)")  "num of processes = ", data%n_proc
    do i = 1, data%n_proc
       write (u, "(3x,A,I0)")  "Process #", data%proc_num_id (i)
       select case (data%n_beam)
       case (1)
          write (u, "(5x,A,ES19.12)")  "Width = ", data%cross_section(i)
       case (2)
          write (u, "(5x,A,ES19.12)")  "CSec  = ", data%cross_section(i)
       end select
       write (u, "(5x,A,ES19.12)")  "Error = ", data%error(i)
    end do
    if (data%n_alt > 0) then
       write (u, "(3x,A,I0)")  "num of alt wgt   = ", data%n_alt
       do i = 1, data%n_alt
          write (u, "(5x,A,A,A,1x,I0)")  "MD5 sum (cfg)  = '", &
               data%md5sum_alt(i), "'", i
       end do
    end if
  end subroutine event_sample_data_write
    

  subroutine eio_data_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (eio_data_1, "eio_data_1", &
         "event sample data", &
         u, results)
    call test (eio_data_2, "eio_data_2", &
         "event normalization", &
         u, results)
  end subroutine eio_data_test
  
  subroutine eio_data_1 (u)
    integer, intent(in) :: u
    type(event_sample_data_t) :: data

    write (u, "(A)")  "* Test output: eio_data_1"
    write (u, "(A)")  "*   Purpose:  display event sample data"
    write (u, "(A)")

    write (u, "(A)")  "* Decay process, one component"
    write (u, "(A)")
 
    call data%init (1, 1)
    data%n_beam = 1
    data%pdg_beam(1) = 25
    data%energy_beam(1) = 125

    data%norm_mode = NORM_UNIT
    
    data%proc_num_id = [42]
    data%cross_section = [1.23e-4_default]
    data%error = 5e-6_default
    
    data%md5sum_prc = "abcdefghijklmnopabcdefghijklmnop"
    data%md5sum_cfg = "12345678901234561234567890123456"
    data%md5sum_alt(1) = "uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuu"
    
    call data%write (u)

    write (u, "(A)")
    write (u, "(A)")  "* Scattering process, two components"
    write (u, "(A)")
    
    call data%init (2)
    data%n_beam = 2
    data%pdg_beam = [2212, -2212]
    data%energy_beam = [8._default, 10._default]
    
    data%norm_mode = NORM_SIGMA
    
    data%proc_num_id = [12, 34]
    data%cross_section = [100._default, 88._default]
    data%error = [1._default, 0.1_default]
    
    call data%write (u)
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: eio_data_1"
    
  end subroutine eio_data_1
  
  subroutine eio_data_2 (u)
    integer, intent(in) :: u
    type(string_t) :: s
    logical :: unweighted
    real(default) :: w, sigma
    integer :: n

    write (u, "(A)")  "* Test output: eio_data_2"
    write (u, "(A)")  "*   Purpose:  handle event normalization"
    write (u, "(A)")

    write (u, "(A)")  "* Normalization strings"
    write (u, "(A)")

    s = "auto"
    unweighted = .true.
    write (u, "(1x,A,1x,L1,1x,A)")  char (s), unweighted, &
         char (event_normalization_string &
         (event_normalization_mode (s, unweighted)))
    s = "AUTO"
    unweighted = .false.
    write (u, "(1x,A,1x,L1,1x,A)")  char (s), unweighted, &
         char (event_normalization_string &
         (event_normalization_mode (s, unweighted)))

    unweighted = .true.
    
    s = "1"
    write (u, "(2(1x,A))") char (s), char (event_normalization_string &
         (event_normalization_mode (s, unweighted)))
    s = "1/n"
    write (u, "(2(1x,A))") char (s), char (event_normalization_string &
         (event_normalization_mode (s, unweighted)))
    s = "Sigma"
    write (u, "(2(1x,A))") char (s), char (event_normalization_string &
         (event_normalization_mode (s, unweighted)))
    s = "sigma/N"
    write (u, "(2(1x,A))") char (s), char (event_normalization_string &
         (event_normalization_mode (s, unweighted)))

    write (u, "(A)")
    write (u, "(A)")  "* Normalization update"
    write (u, "(A)")
    
    sigma = 5
    n = 2

    w0 = 1

    w = w0
    call event_normalization_update (w, sigma, n, NORM_UNIT, NORM_UNIT)
    write (u, "(2(F6.3))")  w0, w
    w = w0
    call event_normalization_update (w, sigma, n, NORM_N_EVT, NORM_UNIT)
    write (u, "(2(F6.3))")  w0, w
    w = w0
    call event_normalization_update (w, sigma, n, NORM_SIGMA, NORM_UNIT)
    write (u, "(2(F6.3))")  w0, w
    w = w0
    call event_normalization_update (w, sigma, n, NORM_S_N, NORM_UNIT)
    write (u, "(2(F6.3))")  w0, w

    write (u, *)
    
    w0 = 0.5

    w = w0
    call event_normalization_update (w, sigma, n, NORM_UNIT, NORM_N_EVT)
    write (u, "(2(F6.3))")  w0, w
    w = w0
    call event_normalization_update (w, sigma, n, NORM_N_EVT, NORM_N_EVT)
    write (u, "(2(F6.3))")  w0, w
    w = w0
    call event_normalization_update (w, sigma, n, NORM_SIGMA, NORM_N_EVT)
    write (u, "(2(F6.3))")  w0, w
    w = w0
    call event_normalization_update (w, sigma, n, NORM_S_N, NORM_N_EVT)
    write (u, "(2(F6.3))")  w0, w
    
    write (u, *)
    
    w0 = 5.0

    w = w0
    call event_normalization_update (w, sigma, n, NORM_UNIT, NORM_SIGMA)
    write (u, "(2(F6.3))")  w0, w
    w = w0
    call event_normalization_update (w, sigma, n, NORM_N_EVT, NORM_SIGMA)
    write (u, "(2(F6.3))")  w0, w
    w = w0
    call event_normalization_update (w, sigma, n, NORM_SIGMA, NORM_SIGMA)
    write (u, "(2(F6.3))")  w0, w
    w = w0
    call event_normalization_update (w, sigma, n, NORM_S_N, NORM_SIGMA)
    write (u, "(2(F6.3))")  w0, w
    
    write (u, *)
    
    w0 = 2.5

    w = w0
    call event_normalization_update (w, sigma, n, NORM_UNIT, NORM_S_N)
    write (u, "(2(F6.3))")  w0, w
    w = w0
    call event_normalization_update (w, sigma, n, NORM_N_EVT, NORM_S_N)
    write (u, "(2(F6.3))")  w0, w
    w = w0
    call event_normalization_update (w, sigma, n, NORM_SIGMA, NORM_S_N)
    write (u, "(2(F6.3))")  w0, w
    w = w0
    call event_normalization_update (w, sigma, n, NORM_S_N, NORM_S_N)
    write (u, "(2(F6.3))")  w0, w
    
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: eio_data_2"
    
  end subroutine eio_data_2
  

end module eio_data
