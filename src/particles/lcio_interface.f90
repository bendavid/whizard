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

module lcio_interface

  use, intrinsic :: iso_c_binding !NODEP!
  
  use kinds, only: default
  use iso_varying_string, string_t => varying_string
  use io_units
!!  use constants
  use lorentz
!!  use unit_tests
!!  use model_data
!!  use flavors
  use colors
!!  use helicities
!!  use quantum_numbers
!!  use polarizations

  implicit none
  private

  public :: lcio_is_available
  public :: lcio_run_header_t
  public :: lcio_run_header_init
  public :: lcio_event_t
  public :: lcio_event_init
  public :: lcio_event_final
  public :: lcio_particle_t
  public :: lcio_particle_init
  public :: lcio_particle_set_color
  public :: lcio_particle_get_color
  public :: lcio_writer_t
  public :: lcio_writer_open_out
  public :: lcio_writer_close
  public :: lcio_event_write

  type :: lcio_run_header_t
     private
     type(c_ptr) :: obj
  end type lcio_run_header_t

  type :: lcio_event_t
     private
     type(c_ptr) :: obj
  end type lcio_event_t

  type :: lcio_particle_t
     private
     type(c_ptr) :: obj
  end type lcio_particle_t

  type :: lcio_writer_t
     private
     type(c_ptr) :: obj
  end type lcio_writer_t


  interface
     logical(c_bool) function lcio_available () bind(C)
       import
     end function lcio_available
  end interface
  interface
     type(c_ptr) function new_lcio_run_header (proc_id) bind(C)
       import
       integer(c_int), value :: proc_id
     end function new_lcio_run_header
  end interface
  interface
     type(c_ptr) function new_lcio_event (proc_id, event_id) bind(C)
       import
       integer(c_int), value :: proc_id, event_id
     end function new_lcio_event
  end interface
  interface
     subroutine lcio_event_delete (evt_obj) bind(C)
       import
       type(c_ptr), value :: evt_obj
     end subroutine lcio_event_delete
  end interface
  interface
     type(c_ptr) function new_lcio_particle (mom, pdg_id, mass, status) bind(C)
       import
       integer(c_int), value :: pdg_id, status
       real(c_double), value :: mass
       real(c_double), dimension(3) :: mom
     end function new_lcio_particle
  end interface
  interface
     subroutine lcio_set_color_flow (prt_obj, col) bind(C)
       import
       type(c_ptr), intent(inout) :: prt_obj
       integer(c_int), dimension(2), intent(in) :: col
     end subroutine lcio_set_color_flow
  end interface
  interface lcio_particle_set_color
     module procedure lcio_particle_set_color_col
     module procedure lcio_particle_set_color_int
  end interface lcio_particle_set_color
  interface
     subroutine lcio_particle_get_flow (prt_obj, flow) bind(C)
       import
       type(c_ptr), intent(in), value :: prt_obj
       integer(c_int), dimension(2), intent(out) :: flow
     end subroutine lcio_particle_get_flow
  end interface
  interface
     type(c_ptr) function open_lcio_writer_new (filename) bind(C)
       import
       character(c_char), dimension(*), intent(in) :: filename
     end function open_lcio_writer_new
  end interface
  interface
     subroutine lcio_writer_delete (io_obj) bind(C)
       import
       type(c_ptr), value :: io_obj
     end subroutine lcio_writer_delete
  end interface
  interface
     subroutine lcio_write_event (io_obj, evt_obj) bind(C)
       import
       type(c_ptr), value :: io_obj, evt_obj
     end subroutine lcio_write_event
  end interface

contains

  function lcio_is_available () result (flag)
    logical :: flag
    flag = lcio_available ()
  end function lcio_is_available

  subroutine lcio_run_header_init (runhdr, proc_id)
    type(lcio_run_header_t), intent(out) :: runhdr
    integer, intent(in), optional :: proc_id
    integer(c_int) :: pid
    pid = 0;  if (present (proc_id))  pid = proc_id
    runhdr%obj = new_lcio_run_header (pid)
  end subroutine lcio_run_header_init

  subroutine lcio_event_init (evt, proc_id, event_id)
    type(lcio_event_t), intent(out) :: evt
    integer, intent(in), optional :: proc_id, event_id
    integer(c_int) :: pid, eid
    pid = 0;  if (present (proc_id))  pid = proc_id
    eid = 0;  if (present (event_id)) eid = event_id
    evt%obj = new_lcio_event (pid, eid)
  end subroutine lcio_event_init

  subroutine lcio_event_final (evt)
    type(lcio_event_t), intent(inout) :: evt
    call lcio_event_delete (evt%obj)
  end subroutine lcio_event_final

  subroutine lcio_particle_init (prt, p, pdg, status, mass)
    type(lcio_particle_t), intent(out) :: prt
    type(vector4_t), intent(in) :: p
    real(default), intent(in) :: mass
    real(default) :: px, py, pz
    integer, intent(in) :: pdg, status
    px = vector4_get_component (p, 1)
    py = vector4_get_component (p, 2)
    pz = vector4_get_component (p, 3)
    prt%obj = new_lcio_particle ([real (px, c_double), real (py, c_double), &
         real (pz, c_double)], int (pdg, c_int), &
         real (mass, c_double), int (status, c_int))
  end subroutine lcio_particle_init

  subroutine lcio_particle_set_color_col (prt, col)
    type(lcio_particle_t), intent(inout) :: prt
    type(color_t), intent(in) :: col
    integer(c_int), dimension(2) :: c
    c(1) = color_get_col (col)
    c(2) = color_get_acl (col)    
    if (c(1) /= 0 .and. c(2) /= 0)  then
       call lcio_set_color_flow (prt%obj, c)
    end  if
  end subroutine lcio_particle_set_color_col

  subroutine lcio_particle_set_color_int (prt, col)
    type(lcio_particle_t), intent(inout) :: prt
    integer, dimension(2), intent(in) :: col
    integer(c_int), dimension(2) :: c
    c = col
    if (c(1) /= 0 .and. c(2) /= 0) then
       call lcio_set_color_flow (prt%obj, c)
    end if
  end subroutine lcio_particle_set_color_int

  function lcio_particle_get_color (prt) result (col)
    integer, dimension(2) :: col
    type(lcio_particle_t), intent(in) :: prt
    call lcio_particle_get_flow (prt%obj, col)
  end function lcio_particle_get_color

  subroutine lcio_writer_open_out (lcio_writer, filename)
    type(lcio_writer_t), intent(out) :: lcio_writer
    type(string_t), intent(in) :: filename
    lcio_writer%obj = open_lcio_writer_new (char (filename) // c_null_char)
  end subroutine lcio_writer_open_out

  subroutine lcio_writer_close (lciowriter)
    type(lcio_writer_t), intent(inout) :: lciowriter
    call lcio_writer_delete (lciowriter%obj)
  end subroutine lcio_writer_close

  subroutine lcio_event_write (wrt, evt)
    type(lcio_writer_t), intent(inout) :: wrt
    type(lcio_event_t), intent(in) :: evt
    call lcio_write_event (wrt%obj, evt%obj)
  end subroutine lcio_event_write




end module lcio_interface
