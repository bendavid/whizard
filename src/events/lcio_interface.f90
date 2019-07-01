! WHIZARD 2.3.0 July 21 2016
! 
! Copyright (C) 1999-2016 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Soyoung Shim <soyoung.shim@desy.de>
!     Florian Staub <florian.staub@cern.ch>  
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, So-young Shim, Daniel Wiesler 
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
  use constants, only: PI
  use diagnostics
  use lorentz
  use flavors
  use colors
  use helicities
  use polarizations

  implicit none
  private

  public :: lcio_is_available
  public :: lcio_run_header_t
  public :: lcio_run_header_init
  public :: lcio_run_header_write
  public :: lccollection_t
  public :: lcio_event_t
  public :: lcio_event_init
  public :: show_lcio_event
  public :: write_lcio_event
  public :: lcio_event_final
  public :: lcio_event_set_alpha_qcd
  public :: lcio_event_set_scale
  public :: lcio_event_add_coll
  public :: lcio_particle_t
  public :: lcio_particle_add_to_evt_coll
  public :: lcio_particle_init
  public :: lcio_particle_set_color
  public :: lcio_particle_get_flow
  public :: lcio_particle_get_momentum
  public :: lcio_particle_get_mass_squared
  public :: lcio_particle_get_vertex
  public :: lcio_particle_get_time
  public :: lcio_polarization_init
  public :: lcio_particle_to_pol
  public :: lcio_particle_to_hel
  public :: lcio_particle_set_vtx
  public :: lcio_particle_set_t 
  public :: lcio_particle_set_parent
  public :: lcio_particle_get_status  
  public :: lcio_particle_get_pdg  
  public :: lcio_particle_get_n_parents 
  public :: lcio_particle_get_n_children
  public :: lcio_get_n_parents
  public :: lcio_get_n_children
  public :: lcio_writer_t
  public :: lcio_writer_open_out
  public :: lcio_writer_close
  public :: lcio_event_write
  public :: lcio_reader_t
  public :: lcio_open_file
  public :: lcio_reader_close
  public :: lcio_read_event
  public :: lcio_event_get_process_id
  public :: lcio_event_get_n_tot
  public :: lcio_event_get_alphas
  public :: lcio_event_get_scaleval  
  public :: lcio_event_get_particle

  type :: lcio_run_header_t
     private
     type(c_ptr) :: obj
  end type lcio_run_header_t

  type :: lccollection_t
     private
     type(c_ptr) :: obj
  end type lccollection_t

  type :: lcio_event_t
     private
     type(c_ptr) :: obj
     type(lccollection_t) :: lccoll
  end type lcio_event_t

  type :: lcio_particle_t
     private
     type(c_ptr) :: obj
  end type lcio_particle_t

  type :: lcio_writer_t
     private
     type(c_ptr) :: obj
  end type lcio_writer_t

  type :: lcio_reader_t
     private
     type(c_ptr) :: obj
  end type lcio_reader_t


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
     subroutine run_header_set_simstring &
          (runhdr_obj, simstring) bind(C)
       import
       type(c_ptr), value :: runhdr_obj
       character(c_char), dimension(*), intent(in) :: simstring
     end subroutine run_header_set_simstring
  end interface
  interface
     subroutine write_run_header (lcwrt_obj, runhdr_obj) bind(C)
       import
       type(c_ptr), value :: lcwrt_obj
       type(c_ptr), value :: runhdr_obj
     end subroutine write_run_header
  end interface
  interface
     type(c_ptr) function new_lccollection () bind(C)
       import
     end function new_lccollection
  end interface
  interface
     type(c_ptr) function new_lcio_event (proc_id, event_id, run_id) bind(C)
       import
       integer(c_int), value :: proc_id, event_id, run_id
     end function new_lcio_event
  end interface
  interface
     subroutine lcio_event_delete (evt_obj) bind(C)
       import
       type(c_ptr), value :: evt_obj
     end subroutine lcio_event_delete
  end interface
  interface
     subroutine dump_lcio_event (evt_obj) bind(C)
       import
       type(c_ptr), value :: evt_obj
     end subroutine dump_lcio_event
  end interface
  interface
     subroutine lcio_event_to_file (evt_obj, filename) bind(C)
       import
       type(c_ptr), value :: evt_obj
       character(c_char), dimension(*), intent(in) :: filename       
     end subroutine lcio_event_to_file
  end interface
  interface
     subroutine lcio_set_alpha_qcd (evt_obj, alphas) bind(C)
       import
       type(c_ptr), value :: evt_obj
       real(c_double), value :: alphas
     end subroutine lcio_set_alpha_qcd
  end interface
  interface 
     subroutine lcio_set_scale (evt_obj, scale) bind(C)
       import
       type(c_ptr), value :: evt_obj
       real(c_double), value :: scale
     end subroutine lcio_set_scale
  end interface  
  interface
     subroutine lcio_event_add_collection &
          (evt_obj, lccoll_obj) bind(C)
       import
       type(c_ptr), value :: evt_obj, lccoll_obj
     end subroutine lcio_event_add_collection
  end interface
  interface
     type(c_ptr) function new_lcio_particle &
          (px, py, pz, pdg_id, mass, status) bind(C)
       import
       integer(c_int), value :: pdg_id, status
       real(c_double), value :: px, py, pz, mass
     end function new_lcio_particle
  end interface
  interface
     subroutine add_particle_to_collection &
          (prt_obj, lccoll_obj) bind(C)
       import
       type(c_ptr), value :: prt_obj, lccoll_obj
     end subroutine add_particle_to_collection
  end interface
  interface
     subroutine lcio_set_color_flow (prt_obj, col1, col2) bind(C)
       import
       type(c_ptr), value :: prt_obj
       integer(c_int), value :: col1, col2
     end subroutine lcio_set_color_flow
  end interface
  interface lcio_particle_set_color
     module procedure lcio_particle_set_color_col
     module procedure lcio_particle_set_color_int
  end interface lcio_particle_set_color
  interface
     integer(c_int) function lcio_particle_flow (prt_obj, col_index) bind(C)
       use iso_c_binding !NODEP!
       type(c_ptr), value :: prt_obj
       integer(c_int), value :: col_index
     end function lcio_particle_flow
  end interface
  interface
     real(c_double) function lcio_three_momentum (prt_obj, p_index) bind(C)
       use iso_c_binding !NODEP!
       type(c_ptr), value :: prt_obj
       integer(c_int), value :: p_index
     end function lcio_three_momentum
  end interface
  interface
     real(c_double) function lcio_energy (prt_obj) bind(C)
       import
       type(c_ptr), intent(in), value :: prt_obj
     end function lcio_energy
  end interface
  interface
     function lcio_mass (prt_obj) result (mass) bind(C)
       import
       real(c_double) :: mass
       type(c_ptr), value :: prt_obj
     end function lcio_mass
  end interface
  interface 
     real(c_double) function lcio_vtx_x (prt) bind(C)
       import
       type(c_ptr), value :: prt       
     end function lcio_vtx_x
  end interface
  interface 
     real(c_double) function lcio_vtx_y (prt) bind(C)
       import
       type(c_ptr), value :: prt       
     end function lcio_vtx_y
  end interface
  interface 
     real(c_double) function lcio_vtx_z (prt) bind(C)
       import
       type(c_ptr), value :: prt       
     end function lcio_vtx_z
  end interface  
  interface 
     real(c_double) function lcio_prt_time (prt) bind(C)
       import
       type(c_ptr), value :: prt    
     end function lcio_prt_time
  end interface    
  interface
     subroutine lcio_particle_set_spin (prt_obj, s1, s2, s3) bind(C)
       import
       type(c_ptr), value :: prt_obj
       real(c_double), value :: s1, s2, s3
     end subroutine lcio_particle_set_spin
  end interface
  interface lcio_polarization_init
     module procedure lcio_polarization_init_pol
     module procedure lcio_polarization_init_hel
     module procedure lcio_polarization_init_int
  end interface
  interface
     function lcio_polarization_degree (prt_obj) result (degree) bind(C)
       import
       real(c_double) :: degree
       type(c_ptr), value :: prt_obj
     end function lcio_polarization_degree
  end interface  
  interface
     function lcio_polarization_theta (prt_obj) result (theta) bind(C)
       import
       real(c_double) :: theta
       type(c_ptr), value :: prt_obj
     end function lcio_polarization_theta
  end interface
  interface
     function lcio_polarization_phi (prt_obj) result (phi) bind(C)
       import
       real(c_double) :: phi
       type(c_ptr), value :: prt_obj
     end function lcio_polarization_phi
  end interface
  interface
     subroutine lcio_particle_set_vertex (prt_obj, vx, vy, vz) bind(C)
       import
       type(c_ptr), value :: prt_obj
       real(c_double), value :: vx, vy, vz
     end subroutine lcio_particle_set_vertex
  end interface
  interface
     subroutine lcio_particle_set_time (prt_obj, t) bind(C)
       import
       type(c_ptr), value :: prt_obj
       real(c_double), value :: t
     end subroutine lcio_particle_set_time
  end interface  
  
  interface
     subroutine lcio_particle_add_parent (prt_obj1, prt_obj2) bind(C)
       import
       type(c_ptr), value :: prt_obj1, prt_obj2
     end subroutine lcio_particle_add_parent
  end interface  
  interface
     integer(c_int) function lcio_particle_get_generator_status &
          (prt_obj) bind(C)
       import
       type(c_ptr), value :: prt_obj
     end function lcio_particle_get_generator_status
  end interface
  interface
     integer(c_int) function lcio_particle_get_pdg_code (prt_obj) bind(C)
       import
       type(c_ptr), value :: prt_obj
     end function lcio_particle_get_pdg_code
  end interface
  interface
     integer(c_int) function lcio_n_parents (prt_obj) bind(C)
       import
       type(c_ptr), value :: prt_obj
     end function lcio_n_parents
  end interface
  interface
     integer(c_int) function lcio_n_daughters (prt_obj) bind(C)
       import
       type(c_ptr), value :: prt_obj
     end function lcio_n_daughters
  end interface
  interface
     integer(c_int) function lcio_event_parent_k &
          (evt_obj, num_part, k_parent) bind (C)
       use iso_c_binding !NODEP!
       type(c_ptr), value :: evt_obj
       integer(c_int), value :: num_part, k_parent
     end function lcio_event_parent_k
  end interface
  interface
     integer(c_int) function lcio_event_daughter_k &
          (evt_obj, num_part, k_daughter) bind (C)
       use iso_c_binding !NODEP!
       type(c_ptr), value :: evt_obj
       integer(c_int), value :: num_part, k_daughter
     end function lcio_event_daughter_k
  end interface
  interface
     type(c_ptr) function open_lcio_writer_new (filename, complevel) bind(C)
       import
       character(c_char), dimension(*), intent(in) :: filename
       integer(c_int), intent(in) :: complevel
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
  interface
     type(c_ptr) function open_lcio_reader (filename) bind(C)
       import
       character(c_char), dimension(*), intent(in) :: filename
     end function open_lcio_reader
  end interface
  interface
     subroutine lcio_reader_delete (io_obj) bind(C)
       import
       type(c_ptr), value :: io_obj
     end subroutine lcio_reader_delete
  end interface
  interface
     type(c_ptr) function read_lcio_event (io_obj) bind(C)
       import
       type(c_ptr), value :: io_obj
     end function read_lcio_event
  end interface
  interface
     integer(c_int) function lcio_event_signal_process_id (evt_obj) bind(C)
       import
       type(c_ptr), value :: evt_obj
     end function lcio_event_signal_process_id
  end interface
  interface
     integer(c_int) function lcio_event_get_n_particles (evt_obj) bind(C)
       import
       type(c_ptr), value :: evt_obj
     end function lcio_event_get_n_particles
  end interface
  interface
     function lcio_event_get_alpha_qcd (evt_obj) result (as) bind(C)
       import
       real(c_double) :: as
       type(c_ptr), value :: evt_obj
     end function lcio_event_get_alpha_qcd
  end interface
  interface
     function lcio_event_get_scale (evt_obj) result (scale) bind(C)
       import 
       real(c_double) :: scale
       type(c_ptr), value :: evt_obj
     end function lcio_event_get_scale
  end interface
  interface
     type(c_ptr) function lcio_event_particle_k (evt_obj, k) bind(C)
       import
       type(c_ptr), value :: evt_obj
       integer(c_int), value :: k
     end function lcio_event_particle_k
  end interface

contains

  function lcio_is_available () result (flag)
    logical :: flag
    flag = lcio_available ()
  end function lcio_is_available

  subroutine lcio_run_header_init (runhdr, proc_id, run_id)
    type(lcio_run_header_t), intent(out) :: runhdr
    integer, intent(in), optional :: proc_id, run_id
    integer(c_int) :: rid
    rid = 0; if (present (run_id))  rid = run_id
    runhdr%obj = new_lcio_run_header (rid)
    call run_header_set_simstring (runhdr%obj, &
         "WHIZARD version:" // "2.3.0")
  end subroutine lcio_run_header_init

  subroutine lcio_run_header_write (wrt, hdr)
    type(lcio_writer_t), intent(inout) :: wrt
    type(lcio_run_header_t), intent(inout) :: hdr
    call write_run_header (wrt%obj, hdr%obj)
  end subroutine lcio_run_header_write

  subroutine lcio_event_init (evt, proc_id, event_id, run_id)
    type(lcio_event_t), intent(out) :: evt
    integer, intent(in), optional :: proc_id, event_id, run_id
    integer(c_int) :: pid, eid, rid
    pid = 0;  if (present (proc_id))  pid = proc_id
    eid = 0;  if (present (event_id)) eid = event_id
    rid = 0;  if (present (run_id))   rid = run_id
    evt%obj = new_lcio_event (pid, eid, rid)
    evt%lccoll%obj = new_lccollection ()
  end subroutine lcio_event_init

  subroutine show_lcio_event (evt)
    type(lcio_event_t), intent(in) :: evt
    if (c_associated (evt%obj)) then
       call dump_lcio_event (evt%obj)
    else
       call msg_error ("LCIO event is not allocated.")
    end if
  end subroutine show_lcio_event

  subroutine write_lcio_event (evt, filename)
    type(lcio_event_t), intent(in) :: evt
    type(string_t), intent(in) :: filename
    call lcio_event_to_file (evt%obj, char (filename) // c_null_char)
  end subroutine write_lcio_event

  subroutine lcio_event_final (evt)
    type(lcio_event_t), intent(inout) :: evt
    call lcio_event_delete (evt%obj)
  end subroutine lcio_event_final

  subroutine lcio_event_set_alpha_qcd (evt, alphas)
    type(lcio_event_t), intent(inout) :: evt
    real(default), intent(in) :: alphas
    call lcio_set_alpha_qcd (evt%obj, real (alphas, c_double))
  end subroutine lcio_event_set_alpha_qcd

  subroutine lcio_event_set_scale (evt, scale) 
    type(lcio_event_t), intent(inout) :: evt
    real(default), intent(in) :: scale
    call lcio_set_scale (evt%obj, real (scale, c_double))
  end subroutine lcio_event_set_scale

  subroutine lcio_event_add_coll (evt) 
    type(lcio_event_t), intent(inout) :: evt
    call lcio_event_add_collection (evt%obj, &
         evt%lccoll%obj)
  end subroutine lcio_event_add_coll

  subroutine lcio_particle_add_to_evt_coll &
       (lprt, evt)
    type(lcio_particle_t), intent(in) :: lprt
    type(lcio_event_t), intent(inout) :: evt
    call add_particle_to_collection (lprt%obj, evt%lccoll%obj)
  end subroutine lcio_particle_add_to_evt_coll

  subroutine lcio_particle_init (prt, p, pdg, status)
    type(lcio_particle_t), intent(out) :: prt
    type(vector4_t), intent(in) :: p
    real(default) :: mass
    real(default) :: px, py, pz
    integer, intent(in) :: pdg, status
    px = vector4_get_component (p, 1)
    py = vector4_get_component (p, 2)
    pz = vector4_get_component (p, 3)
    mass = p**1
    prt%obj = new_lcio_particle (real (px, c_double), real (py, c_double), &
         real (pz, c_double), int (pdg, c_int), &
         real (mass, c_double), int (status, c_int))
  end subroutine lcio_particle_init

  subroutine lcio_particle_set_color_col (prt, col)
    type(lcio_particle_t), intent(inout) :: prt
    type(color_t), intent(in) :: col
    integer(c_int), dimension(2) :: c
    c(1) = col%get_col ()
    c(2) = col%get_acl ()    
    if (c(1) /= 0 .or. c(2) /= 0)  then
       call lcio_set_color_flow (prt%obj, c(1), c(2))
    end  if
  end subroutine lcio_particle_set_color_col

  subroutine lcio_particle_set_color_int (prt, col)
    type(lcio_particle_t), intent(inout) :: prt
    integer, dimension(2), intent(in) :: col
    integer(c_int), dimension(2) :: c
    c = col
    if (c(1) /= 0 .or. c(2) /= 0) then
       call lcio_set_color_flow (prt%obj, c(1), c(2))
    end if
  end subroutine lcio_particle_set_color_int

  function lcio_particle_get_flow (prt) result (col)
    integer, dimension(2) :: col
    type(lcio_particle_t), intent(in) :: prt
    col(1) = lcio_particle_flow (prt%obj, 0_c_int)
    col(2) = - lcio_particle_flow (prt%obj, 1_c_int)    
  end function lcio_particle_get_flow

  function lcio_particle_get_momentum (prt) result (p)
    type(vector4_t) :: p
    type(lcio_particle_t), intent(in) :: prt
    real(default) :: E, px, py, pz
    E = lcio_energy (prt%obj)
    px = lcio_three_momentum (prt%obj, 0_c_int)
    py = lcio_three_momentum (prt%obj, 1_c_int)
    pz = lcio_three_momentum (prt%obj, 2_c_int)
    p = vector4_moving ( E, vector3_moving ([ px, py, pz ]))
  end function lcio_particle_get_momentum

  function lcio_particle_get_mass_squared (prt) result (m2)
    real(default) :: m2
    type(lcio_particle_t), intent(in) :: prt
    real(default) :: m
    m = lcio_mass (prt%obj)
    m2 = sign (m**2, m)
  end function lcio_particle_get_mass_squared

  function lcio_particle_get_vertex (prt) result (vtx)
    type(vector3_t) :: vtx
    type(lcio_particle_t), intent(in) :: prt
    real(default) :: vx, vy, vz
    vx = lcio_vtx_x (prt%obj)
    vy = lcio_vtx_y (prt%obj)
    vz = lcio_vtx_z (prt%obj)    
    vtx = vector3_moving ([vx, vy, vz])
  end function lcio_particle_get_vertex

  function lcio_particle_get_time (prt) result (time)
    real(default) :: time
    type(lcio_particle_t), intent(in) :: prt
    time = lcio_prt_time (prt%obj)
  end function lcio_particle_get_time
  
  subroutine lcio_polarization_init_pol (prt, pol)
    type(lcio_particle_t), intent(inout) :: prt
    type(polarization_t), intent(in) :: pol
    real(default) :: r, theta, phi
    if (pol%is_polarized ()) then
       call pol%to_angles (r, theta, phi)
       call lcio_particle_set_spin (prt%obj, &
            real(r, c_double), real (theta, c_double), real (phi, c_double))
    end if
  end subroutine lcio_polarization_init_pol

  subroutine lcio_polarization_init_hel (prt, hel)
    type(lcio_particle_t), intent(inout) :: prt
    type(helicity_t), intent(in) :: hel
    integer, dimension(2) :: h
    if (hel%is_defined ()) then
       h = hel%to_pair ()
       select case (h(1))
       case (1:)
          call lcio_particle_set_spin (prt%obj, 1._c_double, &
               0._c_double, 0._c_double)
       case (:-1)
          call lcio_particle_set_spin (prt%obj, 1._c_double, &
               real (pi, c_double), 0._c_double)
       case (0)
          call lcio_particle_set_spin (prt%obj, 1._c_double, &
               real (pi/2, c_double), 0._c_double)
       end select
    end if
  end subroutine lcio_polarization_init_hel

  subroutine lcio_polarization_init_int (prt, hel)
    type(lcio_particle_t), intent(inout) :: prt
    integer, intent(in) :: hel
    select case (hel)
    case (1:)
       call lcio_particle_set_spin (prt%obj, 1._c_double, &
            0._c_double, 0._c_double)
    case (:-1)
       call lcio_particle_set_spin (prt%obj, 1._c_double, &
            real (pi, c_double), 0._c_double)
    case (0)
       call lcio_particle_set_spin (prt%obj, 1._c_double, &
            real (pi/2, c_double), 0._c_double)
    end select
  end subroutine lcio_polarization_init_int

  subroutine lcio_particle_to_pol (prt, flv, pol)
    type(lcio_particle_t), intent(in) :: prt
    type(flavor_t), intent(in) :: flv
    type(polarization_t), intent(out) :: pol
    real(default) :: degree, theta, phi
    degree = lcio_polarization_degree (prt%obj)
    theta = lcio_polarization_theta (prt%obj)
    phi = lcio_polarization_phi (prt%obj)
    call pol%init_angles (flv, degree, theta, phi)
  end subroutine lcio_particle_to_pol

  subroutine lcio_particle_to_hel (prt, flv, hel)
    type(lcio_particle_t), intent(in) :: prt
    type(flavor_t), intent(in) :: flv
    type(helicity_t), intent(out) :: hel
    real(default) :: theta
    integer :: hmax
    theta = lcio_polarization_theta (prt%obj)
    hmax = flv%get_spin_type () / 2
    call hel%init (sign (hmax, nint (cos (theta))))
  end subroutine lcio_particle_to_hel

  subroutine lcio_particle_set_vtx (prt, vtx) 
    type(lcio_particle_t), intent(inout) :: prt
    type(vector3_t), intent(in) :: vtx
    call lcio_particle_set_vertex (prt%obj, real(vtx%p(1), c_double), &
         real(vtx%p(2), c_double), real(vtx%p(3), c_double))
  end subroutine lcio_particle_set_vtx

  subroutine lcio_particle_set_t (prt, t)
    type(lcio_particle_t), intent(inout) :: prt
    real(default), intent(in) :: t
    call lcio_particle_set_time (prt%obj, real(t, c_double))
  end subroutine lcio_particle_set_t

  subroutine lcio_particle_set_parent (daughter, parent) 
    type(lcio_particle_t), intent(inout) :: daughter, parent
    call lcio_particle_add_parent (daughter%obj, parent%obj)
  end subroutine lcio_particle_set_parent

  function lcio_particle_get_status (lptr) result (status)
    integer :: status
    type(lcio_particle_t), intent(in) :: lptr
    status = lcio_particle_get_generator_status (lptr%obj)
  end function lcio_particle_get_status
  
  function lcio_particle_get_pdg (lptr) result (pdg)
    integer :: pdg
    type(lcio_particle_t), intent(in) :: lptr
    pdg = lcio_particle_get_pdg_code (lptr%obj)
  end function lcio_particle_get_pdg
  
  function lcio_particle_get_n_parents (lptr) result (n_parents)
    integer :: n_parents
    type(lcio_particle_t), intent(in) :: lptr
    n_parents = lcio_n_parents (lptr%obj)
  end function lcio_particle_get_n_parents
  
  function lcio_particle_get_n_children (lptr) result (n_children)
    integer :: n_children
    type(lcio_particle_t), intent(in) :: lptr
    n_children = lcio_n_daughters (lptr%obj)
  end function lcio_particle_get_n_children
  
  function lcio_get_n_parents (evt, num_part, k_parent) result (index_parent)
    type(lcio_event_t), intent(in) :: evt
    integer, intent(in) :: num_part, k_parent
    integer :: index_parent
    index_parent = lcio_event_parent_k (evt%obj, int (num_part, c_int), &
         int (k_parent, c_int))
  end function lcio_get_n_parents

  function lcio_get_n_children (evt, num_part, k_daughter) result (index_daughter)
    type(lcio_event_t), intent(in) :: evt
    integer, intent(in) :: num_part, k_daughter
    integer :: index_daughter
    index_daughter = lcio_event_daughter_k (evt%obj, int (num_part, c_int), &
         int (k_daughter, c_int))
  end function lcio_get_n_children

  subroutine lcio_writer_open_out (lcio_writer, filename)
    type(lcio_writer_t), intent(out) :: lcio_writer
    type(string_t), intent(in) :: filename
    lcio_writer%obj = open_lcio_writer_new (char (filename) // &
         c_null_char, 9_c_int)
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

  subroutine lcio_open_file (lcio_reader, filename)
    type(lcio_reader_t), intent(out) :: lcio_reader
    type(string_t), intent(in) :: filename
    lcio_reader%obj = open_lcio_reader (char (filename) // c_null_char)
  end subroutine lcio_open_file

  subroutine lcio_reader_close (lcioreader)
    type(lcio_reader_t), intent(inout) :: lcioreader
    call lcio_reader_delete (lcioreader%obj)
  end subroutine lcio_reader_close

  subroutine lcio_read_event (lcrdr, evt, ok)
    type(lcio_reader_t), intent(inout) :: lcrdr
    type(lcio_event_t), intent(out) :: evt
    logical, intent(out) :: ok
    evt%obj = read_lcio_event (lcrdr%obj)
    ok = c_associated (evt%obj)
  end subroutine lcio_read_event

  function lcio_event_get_process_id (evt) result (i_proc)
    integer :: i_proc
    type(lcio_event_t), intent(in) :: evt
    i_proc = lcio_event_signal_process_id (evt%obj)
  end function lcio_event_get_process_id

  function lcio_event_get_n_tot (evt) result (n_tot)
    integer :: n_tot
    type(lcio_event_t), intent(in) :: evt
    n_tot = lcio_event_get_n_particles (evt%obj)
  end function lcio_event_get_n_tot

  function lcio_event_get_alphas (evt) result (as)
    type(lcio_event_t), intent(in) :: evt
    real(default) :: as
    as = lcio_event_get_alpha_qcd (evt%obj)
  end function lcio_event_get_alphas

  function lcio_event_get_scaleval (evt) result (scale)
    type(lcio_event_t), intent(in) :: evt
    real(default) :: scale
    scale = lcio_event_get_scale (evt%obj)
  end function lcio_event_get_scaleval

  function lcio_event_get_particle (evt, n) result (prt)
    type(lcio_event_t), intent(in) :: evt
    integer, intent(in) :: n
    type(lcio_particle_t) :: prt
    prt%obj = lcio_event_particle_k (evt%obj, int (n, c_int))
  end function lcio_event_get_particle

end module lcio_interface
