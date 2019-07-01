! WHIZARD 2.0.7 Mar 19 2012
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

module hepmc_interface

  use iso_c_binding !NODEP!
  use kinds, only: default !NODEP!
  use iso_varying_string, string_t => varying_string !NODEP!
  use constants !NODEP!
  use lorentz !NODEP!
  use models
  use flavors
  use colors
  use helicities
  use quantum_numbers
  use polarizations

  implicit none
  private

  public :: hepmc_is_available
  public :: hepmc_four_vector_t
  public :: hepmc_four_vector_init
  public :: hepmc_four_vector_final
  public :: hepmc_four_vector_to_vector4
  public :: hepmc_polarization_t
  public :: hepmc_polarization_init
  public :: hepmc_polarization_final
  public :: hepmc_polarization_to_pol
  public :: hepmc_polarization_to_hel
  public :: hepmc_particle_t
  public :: hepmc_particle_init
  public :: hepmc_particle_set_color
  public :: hepmc_particle_set_polarization
  public :: hepmc_particle_get_barcode
  public :: hepmc_particle_get_momentum
  public :: hepmc_particle_get_mass_squared
  public :: hepmc_particle_get_pdg
  public :: hepmc_particle_get_status
  public :: hepmc_particle_get_production_vertex
  public :: hepmc_particle_get_decay_vertex
  public :: hepmc_particle_get_n_parents
  public :: hepmc_particle_get_n_children
  public :: hepmc_particle_get_parent_barcodes
  public :: hepmc_particle_get_child_barcodes
  public :: hepmc_particle_get_polarization
  public :: hepmc_particle_get_color
  public :: hepmc_vertex_t
  public :: hepmc_vertex_init
  public :: hepmc_vertex_is_valid
  public :: hepmc_vertex_add_particle_in
  public :: hepmc_vertex_add_particle_out
  public :: hepmc_vertex_get_n_in
  public :: hepmc_vertex_get_n_out
  public :: hepmc_vertex_particle_in_iterator_t
  public :: hepmc_vertex_particle_in_iterator_init
  public :: hepmc_vertex_particle_in_iterator_final
  public :: hepmc_vertex_particle_in_iterator_advance
  public :: hepmc_vertex_particle_in_iterator_reset
  public :: hepmc_vertex_particle_in_iterator_is_valid
  public :: hepmc_vertex_particle_in_iterator_get
  public :: hepmc_vertex_particle_out_iterator_t
  public :: hepmc_vertex_particle_out_iterator_init
  public :: hepmc_vertex_particle_out_iterator_final
  public :: hepmc_vertex_particle_out_iterator_advance
  public :: hepmc_vertex_particle_out_iterator_reset
  public :: hepmc_vertex_particle_out_iterator_is_valid
  public :: hepmc_vertex_particle_out_iterator_get
  public :: hepmc_event_t
  public :: hepmc_event_init
  public :: hepmc_event_final
  public :: hepmc_event_print
  public :: hepmc_event_get_event_index
  public :: hepmc_event_set_process_id
  public :: hepmc_event_get_process_id
  public :: hepmc_event_set_scale
  public :: hepmc_event_get_scale
  public :: hepmc_event_set_alpha_qcd
  public :: hepmc_event_get_alpha_qcd
  public :: hepmc_event_set_alpha_qed
  public :: hepmc_event_get_alpha_qed
  public :: hepmc_event_clear_weights
  public :: hepmc_event_add_weight
  public :: hepmc_event_get_weights_size
  public :: hepmc_event_get_weight
  public :: hepmc_event_add_vertex
  public :: hepmc_event_set_signal_process_vertex
  public :: hepmc_event_set_beam_particles
  public :: hepmc_event_set_cross_section
  public :: hepmc_event_particle_iterator_t
  public :: hepmc_event_particle_iterator_init
  public :: hepmc_event_particle_iterator_final
  public :: hepmc_event_particle_iterator_advance
  public :: hepmc_event_particle_iterator_reset
  public :: hepmc_event_particle_iterator_is_valid
  public :: hepmc_event_particle_iterator_get
  public :: hepmc_iostream_t
  public :: hepmc_iostream_open_out
  public :: hepmc_iostream_open_in
  public :: hepmc_iostream_close
  public :: hepmc_iostream_write_event
  public :: hepmc_iostream_read_event
  public :: hepmc_test

  type :: hepmc_four_vector_t
     private
     type(c_ptr) :: obj
  end type hepmc_four_vector_t

  type :: hepmc_polarization_t
     private
     logical :: polarized = .false.
     type(c_ptr) :: obj
  end type hepmc_polarization_t

  type :: hepmc_particle_t
     private
     type(c_ptr) :: obj
  end type hepmc_particle_t

  type :: hepmc_vertex_t
     private
     type(c_ptr) :: obj
  end type hepmc_vertex_t

  type :: hepmc_vertex_particle_in_iterator_t
     private
     type(c_ptr) :: obj
     type(c_ptr) :: v_obj
  end type hepmc_vertex_particle_in_iterator_t

  type :: hepmc_vertex_particle_out_iterator_t
     private
     type(c_ptr) :: obj
     type(c_ptr) :: v_obj
  end type hepmc_vertex_particle_out_iterator_t

  type :: hepmc_event_t
     private
     type(c_ptr) :: obj
  end type hepmc_event_t

  type :: hepmc_event_particle_iterator_t
     private
     type(c_ptr) :: obj
     type(c_ptr) :: evt_obj
  end type hepmc_event_particle_iterator_t

  type :: hepmc_iostream_t
     private
     type(c_ptr) :: obj
  end type hepmc_iostream_t


  interface
     logical(c_bool) function hepmc_available () bind(C)
       import
     end function hepmc_available
  end interface
  interface
     type(c_ptr) function new_four_vector_xyz (x, y, z) bind(C)
       import
       real(c_double), value :: x, y, z
     end function new_four_vector_xyz
  end interface
  interface
     type(c_ptr) function new_four_vector_xyzt (x, y, z, t) bind(C)
       import
       real(c_double), value :: x, y, z, t
     end function new_four_vector_xyzt
  end interface
  interface hepmc_four_vector_init
     module procedure hepmc_four_vector_init_v4
     module procedure hepmc_four_vector_init_v3
     module procedure hepmc_four_vector_init_hepmc_prt
  end interface
  interface
     subroutine four_vector_delete (p_obj) bind(C)
       import
       type(c_ptr), value :: p_obj
     end subroutine four_vector_delete
  end interface
  interface
     function four_vector_px (p_obj) result (px) bind(C)
       import
       real(c_double) :: px
       type(c_ptr), value :: p_obj
     end function four_vector_px
  end interface
  interface
     function four_vector_py (p_obj) result (py) bind(C)
       import
       real(c_double) :: py
       type(c_ptr), value :: p_obj
     end function four_vector_py
  end interface
  interface
     function four_vector_pz (p_obj) result (pz) bind(C)
       import
       real(c_double) :: pz
       type(c_ptr), value :: p_obj
     end function four_vector_pz
  end interface
  interface
     function four_vector_e (p_obj) result (e) bind(C)
       import
       real(c_double) :: e
       type(c_ptr), value :: p_obj
     end function four_vector_e
  end interface
  interface
     type(c_ptr) function new_polarization (theta, phi) bind(C)
       import
       real(c_double), value :: theta, phi
     end function new_polarization
  end interface
  interface hepmc_polarization_init
     module procedure hepmc_polarization_init_pol
     module procedure hepmc_polarization_init_hel
  end interface
  interface
     subroutine polarization_delete (pol_obj) bind(C)
       import
       type(c_ptr), value :: pol_obj
     end subroutine polarization_delete
  end interface
  interface
     function polarization_theta (pol_obj) result (theta) bind(C)
       import
       real(c_double) :: theta
       type(c_ptr), value :: pol_obj
     end function polarization_theta
  end interface
  interface
     function polarization_phi (pol_obj) result (phi) bind(C)
       import
       real(c_double) :: phi
       type(c_ptr), value :: pol_obj
     end function polarization_phi
  end interface
  interface
     type(c_ptr) function new_gen_particle (prt_obj, pdg_id, status) bind(C)
       import
       type(c_ptr), value :: prt_obj
       integer(c_int), value :: pdg_id, status
     end function new_gen_particle
  end interface
  interface
     subroutine gen_particle_set_flow (prt_obj, code_index, code) bind(C)
       import
       type(c_ptr), value :: prt_obj
       integer(c_int), value :: code_index, code
     end subroutine gen_particle_set_flow
  end interface
  interface
     subroutine gen_particle_set_polarization (prt_obj, pol_obj) bind(C)
       import
       type(c_ptr), value :: prt_obj, pol_obj
     end subroutine gen_particle_set_polarization
  end interface
  interface hepmc_particle_set_polarization
     module procedure hepmc_particle_set_polarization_pol
     module procedure hepmc_particle_set_polarization_hel
  end interface
  interface
     function gen_particle_barcode (prt_obj) result (barcode) bind(C)
       import
       integer(c_int) :: barcode
       type(c_ptr), value :: prt_obj
     end function gen_particle_barcode
  end interface
  interface
     type(c_ptr) function gen_particle_momentum (prt_obj) bind(C)
       import
       type(c_ptr), value :: prt_obj
     end function gen_particle_momentum
  end interface
  interface
     function gen_particle_generated_mass (prt_obj) result (mass) bind(C)
       import
       real(c_double) :: mass
       type(c_ptr), value :: prt_obj
     end function gen_particle_generated_mass
  end interface
  interface
     function gen_particle_pdg_id (prt_obj) result (pdg_id) bind(C)
       import
       integer(c_int) :: pdg_id
       type(c_ptr), value :: prt_obj
     end function gen_particle_pdg_id
  end interface
  interface
     function gen_particle_status (prt_obj) result (status) bind(C)
       import
       integer(c_int) :: status
       type(c_ptr), value :: prt_obj
     end function gen_particle_status
  end interface
  interface
     type(c_ptr) function gen_particle_production_vertex (prt_obj) bind(C)
       import
       type(c_ptr), value :: prt_obj
     end function gen_particle_production_vertex
  end interface
  interface
     type(c_ptr) function gen_particle_end_vertex (prt_obj) bind(C)
       import
       type(c_ptr), value :: prt_obj
     end function gen_particle_end_vertex
  end interface
  interface
     type(c_ptr) function gen_particle_polarization (prt_obj) bind(C)
       import
       type(c_ptr), value :: prt_obj
     end function gen_particle_polarization
  end interface
  interface
     function gen_particle_flow (prt_obj, code_index) result (code) bind(C)
       import
       integer(c_int) :: code
       type(c_ptr), value :: prt_obj
       integer(c_int), value :: code_index
     end function gen_particle_flow
  end interface
  interface
     type(c_ptr) function new_gen_vertex () bind(C)
       import
     end function new_gen_vertex
  end interface
  interface
     type(c_ptr) function new_gen_vertex_pos (prt_obj) bind(C)
       import
       type(c_ptr), value :: prt_obj
     end function new_gen_vertex_pos
  end interface
  interface
     function gen_vertex_is_valid (v_obj) result (flag) bind(C)
       import
       logical(c_bool) :: flag
       type(c_ptr), value :: v_obj
     end function gen_vertex_is_valid
  end interface
  interface
     subroutine gen_vertex_add_particle_in (v_obj, prt_obj) bind(C)
       import
       type(c_ptr), value :: v_obj, prt_obj
     end subroutine gen_vertex_add_particle_in
  end interface
  interface
     subroutine gen_vertex_add_particle_out (v_obj, prt_obj) bind(C)
       import
       type(c_ptr), value :: v_obj, prt_obj
     end subroutine gen_vertex_add_particle_out
  end interface
  interface
     function gen_vertex_particles_in_size (v_obj) result (size) bind(C)
       import
       integer(c_int) :: size
       type(c_ptr), value :: v_obj
     end function gen_vertex_particles_in_size
  end interface
  interface
     function gen_vertex_particles_out_size (v_obj) result (size) bind(C)
       import
       integer(c_int) :: size
       type(c_ptr), value :: v_obj
     end function gen_vertex_particles_out_size
  end interface
  interface
     type(c_ptr) function &
          new_vertex_particles_in_const_iterator (v_obj) bind(C)
       import
       type(c_ptr), value :: v_obj
     end function new_vertex_particles_in_const_iterator
  end interface
  interface
     subroutine vertex_particles_in_const_iterator_delete (it_obj) bind(C)
       import
       type(c_ptr), value :: it_obj
     end subroutine vertex_particles_in_const_iterator_delete
  end interface
  interface
     subroutine vertex_particles_in_const_iterator_advance (it_obj) bind(C)
       import
       type(c_ptr), value :: it_obj
     end subroutine vertex_particles_in_const_iterator_advance
  end interface
  interface
     subroutine vertex_particles_in_const_iterator_reset &
          (it_obj, v_obj) bind(C)
       import
       type(c_ptr), value :: it_obj, v_obj
     end subroutine vertex_particles_in_const_iterator_reset
  end interface
  interface
     function vertex_particles_in_const_iterator_is_valid &
          (it_obj, v_obj) result (flag) bind(C)
       import
       logical(c_bool) :: flag
       type(c_ptr), value :: it_obj, v_obj
     end function vertex_particles_in_const_iterator_is_valid
  end interface
  interface
     type(c_ptr) function &
          vertex_particles_in_const_iterator_get (it_obj) bind(C)
       import
       type(c_ptr), value :: it_obj
     end function vertex_particles_in_const_iterator_get
  end interface
  interface
     type(c_ptr) function &
          new_vertex_particles_out_const_iterator (v_obj) bind(C)
       import
       type(c_ptr), value :: v_obj
     end function new_vertex_particles_out_const_iterator
  end interface
  interface
     subroutine vertex_particles_out_const_iterator_delete (it_obj) bind(C)
       import
       type(c_ptr), value :: it_obj
     end subroutine vertex_particles_out_const_iterator_delete
  end interface
  interface
     subroutine vertex_particles_out_const_iterator_advance (it_obj) bind(C)
       import
       type(c_ptr), value :: it_obj
     end subroutine vertex_particles_out_const_iterator_advance
  end interface
  interface
     subroutine vertex_particles_out_const_iterator_reset &
          (it_obj, v_obj) bind(C)
       import
       type(c_ptr), value :: it_obj, v_obj
     end subroutine vertex_particles_out_const_iterator_reset
  end interface
  interface
     function vertex_particles_out_const_iterator_is_valid &
          (it_obj, v_obj) result (flag) bind(C)
       import
       logical(c_bool) :: flag
       type(c_ptr), value :: it_obj, v_obj
     end function vertex_particles_out_const_iterator_is_valid
  end interface
  interface
     type(c_ptr) function &
          vertex_particles_out_const_iterator_get (it_obj) bind(C)
       import
       type(c_ptr), value :: it_obj
     end function vertex_particles_out_const_iterator_get
  end interface
  interface
     type(c_ptr) function new_gen_event (proc_id, event_id) bind(C)
       import
       integer(c_int), value :: proc_id, event_id
     end function new_gen_event
  end interface
  interface
     subroutine gen_event_delete (evt_obj) bind(C)
       import
       type(c_ptr), value :: evt_obj
     end subroutine gen_event_delete
  end interface
  interface
     subroutine gen_event_print (evt_obj) bind(C)
       import
       type(c_ptr), value :: evt_obj
     end subroutine gen_event_print
  end interface
  interface
     integer(c_int) function gen_event_event_number (evt_obj) bind(C)
       use iso_c_binding !NODEP!
       type(c_ptr), value :: evt_obj
     end function gen_event_event_number
  end interface
  interface
     subroutine gen_event_set_signal_process_id (evt_obj, proc_id) bind(C)
       import
       type(c_ptr), value :: evt_obj
       integer(c_int), value :: proc_id
     end subroutine gen_event_set_signal_process_id
  end interface
  interface
     integer(c_int) function gen_event_signal_process_id (evt_obj) bind(C)
       import
       type(c_ptr), value :: evt_obj
     end function gen_event_signal_process_id
  end interface
  interface
     subroutine gen_event_set_event_scale (evt_obj, scale) bind(C)
       import
       type(c_ptr), value :: evt_obj
       real(c_double), value :: scale
     end subroutine gen_event_set_event_scale
  end interface
  interface
     real(c_double) function gen_event_event_scale (evt_obj) bind(C)
       import
       type(c_ptr), value :: evt_obj
     end function gen_event_event_scale
  end interface
  interface
     subroutine gen_event_set_alpha_qcd (evt_obj, a) bind(C)
       import
       type(c_ptr), value :: evt_obj
       real(c_double), value :: a
     end subroutine gen_event_set_alpha_qcd
  end interface
  interface
     real(c_double) function gen_event_alpha_qcd (evt_obj) bind(C)
       import
       type(c_ptr), value :: evt_obj
     end function gen_event_alpha_qcd
  end interface
  interface
     subroutine gen_event_set_alpha_qed (evt_obj, a) bind(C)
       import
       type(c_ptr), value :: evt_obj
       real(c_double), value :: a
     end subroutine gen_event_set_alpha_qed
  end interface
  interface
     real(c_double) function gen_event_alpha_qed (evt_obj) bind(C)
       import
       type(c_ptr), value :: evt_obj
     end function gen_event_alpha_qed
  end interface
  interface
     subroutine gen_event_clear_weights (evt_obj) bind(C)
       use iso_c_binding !NODEP!
       type(c_ptr), value :: evt_obj
     end subroutine gen_event_clear_weights
  end interface
  interface
     subroutine gen_event_add_weight (evt_obj, w) bind(C)
       use iso_c_binding !NODEP!
       type(c_ptr), value :: evt_obj
       real(c_double), value :: w
     end subroutine gen_event_add_weight
  end interface
  interface
     integer(c_int) function gen_event_weights_size (evt_obj) bind(C)
       use iso_c_binding !NODEP!
       type(c_ptr), value :: evt_obj
     end function gen_event_weights_size
  end interface
  interface
     real(c_double) function gen_event_weight (evt_obj, i) bind(C)
       use iso_c_binding !NODEP!
       type(c_ptr), value :: evt_obj
       integer(c_int), value :: i
     end function gen_event_weight
  end interface
  interface
     subroutine gen_event_add_vertex (evt_obj, v_obj) bind(C)
       import
       type(c_ptr), value :: evt_obj
       type(c_ptr), value :: v_obj
     end subroutine gen_event_add_vertex
  end interface
  interface
     subroutine gen_event_set_signal_process_vertex (evt_obj, v_obj) bind(C)
       import
       type(c_ptr), value :: evt_obj
       type(c_ptr), value :: v_obj
     end subroutine gen_event_set_signal_process_vertex
  end interface
  interface
     logical(c_bool) function gen_event_set_beam_particles &
          (evt_obj, prt1_obj, prt2_obj) bind(C)
       import
       type(c_ptr), value :: evt_obj, prt1_obj, prt2_obj
     end function gen_event_set_beam_particles
  end interface

  interface
     subroutine gen_event_set_cross_section (evt_obj, xs, xs_err) bind(C)
       import
       type(c_ptr), value :: evt_obj
       real(c_double), value :: xs, xs_err
     end subroutine gen_event_set_cross_section
  end interface

  interface
     type(c_ptr) function new_event_particle_const_iterator (evt_obj) bind(C)
       import
       type(c_ptr), value :: evt_obj
     end function new_event_particle_const_iterator
  end interface
  interface
     subroutine event_particle_const_iterator_delete (it_obj) bind(C)
       import
       type(c_ptr), value :: it_obj
     end subroutine event_particle_const_iterator_delete
  end interface
  interface
     subroutine event_particle_const_iterator_advance (it_obj) bind(C)
       import
       type(c_ptr), value :: it_obj
     end subroutine event_particle_const_iterator_advance
  end interface
  interface
     subroutine event_particle_const_iterator_reset (it_obj, evt_obj) bind(C)
       import
       type(c_ptr), value :: it_obj, evt_obj
     end subroutine event_particle_const_iterator_reset
  end interface
  interface
     function event_particle_const_iterator_is_valid &
          (it_obj, evt_obj) result (flag) bind(C)
       import
       logical(c_bool) :: flag
       type(c_ptr), value :: it_obj, evt_obj
     end function event_particle_const_iterator_is_valid
  end interface
  interface
     type(c_ptr) function event_particle_const_iterator_get (it_obj) bind(C)
       import
       type(c_ptr), value :: it_obj
     end function event_particle_const_iterator_get
  end interface
  interface
     type(c_ptr) function new_io_gen_event_out (filename) bind(C)
       import
       character(c_char), dimension(*), intent(in) :: filename
     end function new_io_gen_event_out
  end interface
  interface
     type(c_ptr) function new_io_gen_event_in (filename) bind(C)
       import
       character(c_char), dimension(*), intent(in) :: filename
     end function new_io_gen_event_in
  end interface
  interface
     subroutine io_gen_event_delete (io_obj) bind(C)
       import
       type(c_ptr), value :: io_obj
     end subroutine io_gen_event_delete
  end interface
  interface
     subroutine io_gen_event_write_event (io_obj, evt_obj) bind(C)
       import
       type(c_ptr), value :: io_obj, evt_obj
     end subroutine io_gen_event_write_event
  end interface
  interface
     logical(c_bool) function io_gen_event_read_event (io_obj, evt_obj) bind(C)
       import
       type(c_ptr), value :: io_obj, evt_obj
     end function io_gen_event_read_event
  end interface

contains

  function hepmc_is_available () result (flag)
    logical :: flag
    flag = hepmc_available ()
  end function hepmc_is_available

  subroutine hepmc_four_vector_init_v4 (pp, p)
    type(hepmc_four_vector_t), intent(out) :: pp
    type(vector4_t), intent(in) :: p
    real(default), dimension(0:3) :: pa
    pa = vector4_get_components (p)
    pp%obj = new_four_vector_xyzt &
         (real (pa(1), c_double), &
          real (pa(2), c_double), &
          real (pa(3), c_double), &
          real (pa(0), c_double))
  end subroutine hepmc_four_vector_init_v4
  
  subroutine hepmc_four_vector_init_v3 (pp, p)
    type(hepmc_four_vector_t), intent(out) :: pp
    type(vector3_t), intent(in) :: p
    real(default), dimension(3) :: pa
    pa = vector3_get_components (p)
    pp%obj = new_four_vector_xyz &
         (real (pa(1), c_double), &
          real (pa(2), c_double), &
          real (pa(3), c_double))
  end subroutine hepmc_four_vector_init_v3
  
  subroutine hepmc_four_vector_init_hepmc_prt (pp, prt)
    type(hepmc_four_vector_t), intent(out) :: pp
    type(hepmc_particle_t), intent(in) :: prt
    pp%obj = gen_particle_momentum (prt%obj)
  end subroutine hepmc_four_vector_init_hepmc_prt
  
  subroutine hepmc_four_vector_final (p)
    type(hepmc_four_vector_t), intent(inout) :: p
    call four_vector_delete (p%obj)
  end subroutine hepmc_four_vector_final

  subroutine hepmc_four_vector_to_vector4 (pp, p)
    type(hepmc_four_vector_t), intent(in) :: pp
    type(vector4_t), intent(out) :: p
    real(default) :: E
    real(default), dimension(3) :: p3
    E = four_vector_e (pp%obj)
    p3(1) = four_vector_px (pp%obj)
    p3(2) = four_vector_py (pp%obj)
    p3(3) = four_vector_pz (pp%obj)
    p = vector4_moving (E, vector3_moving (p3))
  end subroutine hepmc_four_vector_to_vector4

  subroutine hepmc_polarization_init_pol (hpol, pol)
    type(hepmc_polarization_t), intent(out) :: hpol
    type(polarization_t), intent(in) :: pol
    real(default) :: r, theta, phi
    if (polarization_is_polarized (pol)) then
       call polarization_to_angles (pol, r, theta, phi)
       if (r >= 0.5) then
          hpol%polarized = .true.
          hpol%obj = new_polarization &
               (real (theta, c_double), real (phi, c_double))
       end if
    end if
  end subroutine hepmc_polarization_init_pol

  subroutine hepmc_polarization_init_hel (hpol, hel)
    type(hepmc_polarization_t), intent(out) :: hpol
    type(helicity_t), intent(in) :: hel
    integer, dimension(2) :: h
    if (helicity_is_defined (hel)) then
       h = helicity_get (hel)
       select case (h(1))
       case (1:)
          hpol%polarized = .true.
          hpol%obj = new_polarization (0._c_double, 0._c_double)
       case (:-1)
          hpol%polarized = .true.
          hpol%obj = new_polarization (real (pi, c_double), 0._c_double)
       end select
    end if
  end subroutine hepmc_polarization_init_hel

  subroutine hepmc_polarization_final (hpol)
    type(hepmc_polarization_t), intent(inout) :: hpol
    if (hpol%polarized)  call polarization_delete (hpol%obj)
  end subroutine hepmc_polarization_final

  subroutine hepmc_polarization_to_pol (hpol, flv, pol)
    type(hepmc_polarization_t), intent(in) :: hpol
    type(flavor_t), intent(in) :: flv
    type(polarization_t), intent(out) :: pol
    real(default) :: theta, phi
    theta = polarization_theta (hpol%obj)
    phi = polarization_phi (hpol%obj)
    call polarization_init_angles (pol, flv, 1._default, theta, phi)
  end subroutine hepmc_polarization_to_pol

  subroutine hepmc_polarization_to_hel (hpol, flv, hel)
    type(hepmc_polarization_t), intent(in) :: hpol
    type(flavor_t), intent(in) :: flv
    type(helicity_t), intent(out) :: hel
    real(default) :: theta
    integer :: hmax
    theta = polarization_theta (hpol%obj)
    hmax = flavor_get_spin_type (flv) / 2
    call helicity_init (hel, sign (hmax, nint (cos (theta))))
  end subroutine hepmc_polarization_to_hel

  subroutine hepmc_particle_init (prt, p, pdg, status)
    type(hepmc_particle_t), intent(out) :: prt
    type(vector4_t), intent(in) :: p
    integer, intent(in) :: pdg, status
    type(hepmc_four_vector_t) :: pp
    call hepmc_four_vector_init (pp, p)
    prt%obj = new_gen_particle (pp%obj, int (pdg, c_int), int (status, c_int))
    call hepmc_four_vector_final (pp)
  end subroutine hepmc_particle_init

  subroutine hepmc_particle_set_color (prt, col)
    type(hepmc_particle_t), intent(inout) :: prt
    type(color_t), intent(in) :: col
    integer(c_int) :: c
    c = color_get_col (col)
    if (c /= 0)  call gen_particle_set_flow (prt%obj, 1_c_int, c)
    c = color_get_acl (col)
    if (c /= 0)  call gen_particle_set_flow (prt%obj, 2_c_int, c)
  end subroutine hepmc_particle_set_color

  subroutine hepmc_particle_set_polarization_pol (prt, pol)
    type(hepmc_particle_t), intent(inout) :: prt
    type(polarization_t), intent(in) :: pol
    type(hepmc_polarization_t) :: hpol
    call hepmc_polarization_init (hpol, pol)
    if (hpol%polarized)  call gen_particle_set_polarization (prt%obj, hpol%obj)
    call hepmc_polarization_final (hpol)
  end subroutine hepmc_particle_set_polarization_pol

  subroutine hepmc_particle_set_polarization_hel (prt, hel)
    type(hepmc_particle_t), intent(inout) :: prt
    type(helicity_t), intent(in) :: hel
    type(hepmc_polarization_t) :: hpol
    call hepmc_polarization_init (hpol, hel)
    if (hpol%polarized)  call gen_particle_set_polarization (prt%obj, hpol%obj)
    call hepmc_polarization_final (hpol)
  end subroutine hepmc_particle_set_polarization_hel

  function hepmc_particle_get_barcode (prt) result (barcode)
    integer :: barcode
    type(hepmc_particle_t), intent(in) :: prt
    barcode = gen_particle_barcode (prt%obj)
  end function hepmc_particle_get_barcode

  function hepmc_particle_get_momentum (prt) result (p)
    type(vector4_t) :: p
    type(hepmc_particle_t), intent(in) :: prt
    type(hepmc_four_vector_t) :: pp
    call hepmc_four_vector_init (pp, prt)
    call hepmc_four_vector_to_vector4 (pp, p)
    call hepmc_four_vector_final (pp)
  end function hepmc_particle_get_momentum

  function hepmc_particle_get_mass_squared (prt) result (m2)
    real(default) :: m2
    type(hepmc_particle_t), intent(in) :: prt
    real(default) :: m
    m = gen_particle_generated_mass (prt%obj)
    m2 = sign (m**2, m)
  end function hepmc_particle_get_mass_squared

  function hepmc_particle_get_pdg (prt) result (pdg)
    integer :: pdg
    type(hepmc_particle_t), intent(in) :: prt
    pdg = gen_particle_pdg_id (prt%obj)
  end function hepmc_particle_get_pdg

  function hepmc_particle_get_status (prt) result (status)
    integer :: status
    type(hepmc_particle_t), intent(in) :: prt
    status = gen_particle_status (prt%obj)
  end function hepmc_particle_get_status

  function hepmc_particle_get_production_vertex (prt) result (v)
    type(hepmc_vertex_t) :: v
    type(hepmc_particle_t), intent(in) :: prt
    v%obj = gen_particle_production_vertex (prt%obj)
  end function hepmc_particle_get_production_vertex

  function hepmc_particle_get_decay_vertex (prt) result (v)
    type(hepmc_vertex_t) :: v
    type(hepmc_particle_t), intent(in) :: prt
    v%obj = gen_particle_end_vertex (prt%obj)
  end function hepmc_particle_get_decay_vertex

  function hepmc_particle_get_n_parents (prt) result (n_parents)
    integer :: n_parents
    type(hepmc_particle_t), intent(in) :: prt
    type(hepmc_vertex_t) :: v
    v = hepmc_particle_get_production_vertex (prt)
    if (hepmc_vertex_is_valid (v)) then
       n_parents = hepmc_vertex_get_n_in (v)
    else
       n_parents = 0
    end if
  end function hepmc_particle_get_n_parents

  function hepmc_particle_get_n_children (prt) result (n_children)
    integer :: n_children
    type(hepmc_particle_t), intent(in) :: prt
    type(hepmc_vertex_t) :: v
    v = hepmc_particle_get_decay_vertex (prt)
    if (hepmc_vertex_is_valid (v)) then
       n_children = hepmc_vertex_get_n_out (v)
    else
       n_children = 0
    end if
  end function hepmc_particle_get_n_children

  function hepmc_particle_get_parent_barcodes (prt) result (parent_barcode)
    type(hepmc_particle_t), intent(in) :: prt
    integer, dimension(:), allocatable :: parent_barcode
    type(hepmc_vertex_t) :: v
    type(hepmc_vertex_particle_in_iterator_t) :: it
    integer :: i
    v = hepmc_particle_get_production_vertex (prt)
    if (hepmc_vertex_is_valid (v)) then
       allocate (parent_barcode (hepmc_vertex_get_n_in (v)))
       if (size (parent_barcode) /= 0) then
          call hepmc_vertex_particle_in_iterator_init (it, v)
          do i = 1, size (parent_barcode)
             parent_barcode(i) = hepmc_particle_get_barcode &
                  (hepmc_vertex_particle_in_iterator_get (it))
             call hepmc_vertex_particle_in_iterator_advance (it)
          end do
          call hepmc_vertex_particle_in_iterator_final (it)
       end if
    else
       allocate (parent_barcode (0))
    end if
  end function hepmc_particle_get_parent_barcodes

  function hepmc_particle_get_child_barcodes (prt) result (child_barcode)
    type(hepmc_particle_t), intent(in) :: prt
    integer, dimension(:), allocatable :: child_barcode
    type(hepmc_vertex_t) :: v
    type(hepmc_vertex_particle_out_iterator_t) :: it
    integer :: i
    v = hepmc_particle_get_decay_vertex (prt)
    if (hepmc_vertex_is_valid (v)) then
       allocate (child_barcode (hepmc_vertex_get_n_out (v)))
       call hepmc_vertex_particle_out_iterator_init (it, v)
       if (size (child_barcode) /= 0) then
          do i = 1, size (child_barcode)
             child_barcode(i) = hepmc_particle_get_barcode &
                  (hepmc_vertex_particle_out_iterator_get (it))
             call hepmc_vertex_particle_out_iterator_advance (it)
          end do
          call hepmc_vertex_particle_out_iterator_final (it)
       end if
    else
       allocate (child_barcode (0))
    end if
  end function hepmc_particle_get_child_barcodes

  function hepmc_particle_get_polarization (prt) result (pol)
    type(hepmc_polarization_t) :: pol
    type(hepmc_particle_t), intent(in) :: prt
    pol%obj = gen_particle_polarization (prt%obj)
  end function hepmc_particle_get_polarization

  function hepmc_particle_get_color (prt) result (col)
    integer, dimension(2) :: col
    type(hepmc_particle_t), intent(in) :: prt
    col(1) = gen_particle_flow (prt%obj, 1)
    col(2) = - gen_particle_flow (prt%obj, 2)
  end function hepmc_particle_get_color

  subroutine hepmc_vertex_init (v, x)
    type(hepmc_vertex_t), intent(out) :: v
    type(vector4_t), intent(in), optional :: x
    type(hepmc_four_vector_t) :: pos
    if (present (x)) then
       call hepmc_four_vector_init (pos, x)
       v%obj = new_gen_vertex_pos (pos%obj)
       call hepmc_four_vector_final (pos)
    else
       v%obj = new_gen_vertex ()
    end if
  end subroutine hepmc_vertex_init

  function hepmc_vertex_is_valid (v) result (flag)
    logical :: flag
    type(hepmc_vertex_t), intent(in) :: v
    flag = gen_vertex_is_valid (v%obj)
  end function hepmc_vertex_is_valid

  subroutine hepmc_vertex_add_particle_in (v, prt)
    type(hepmc_vertex_t), intent(inout) :: v
    type(hepmc_particle_t), intent(in) :: prt
    call gen_vertex_add_particle_in (v%obj, prt%obj)
  end subroutine hepmc_vertex_add_particle_in

  subroutine hepmc_vertex_add_particle_out (v, prt)
    type(hepmc_vertex_t), intent(inout) :: v
    type(hepmc_particle_t), intent(in) :: prt
    call gen_vertex_add_particle_out (v%obj, prt%obj)
  end subroutine hepmc_vertex_add_particle_out

  function hepmc_vertex_get_n_in (v) result (n_in)
    integer :: n_in
    type(hepmc_vertex_t), intent(in) :: v
    n_in = gen_vertex_particles_in_size (v%obj)
  end function hepmc_vertex_get_n_in

  function hepmc_vertex_get_n_out (v) result (n_out)
    integer :: n_out
    type(hepmc_vertex_t), intent(in) :: v
    n_out = gen_vertex_particles_out_size (v%obj)
  end function hepmc_vertex_get_n_out

  subroutine hepmc_vertex_particle_in_iterator_init (it, v)
    type(hepmc_vertex_particle_in_iterator_t), intent(out) :: it
    type(hepmc_vertex_t), intent(in) :: v
    it%obj = new_vertex_particles_in_const_iterator (v%obj)
    it%v_obj = v%obj
  end subroutine hepmc_vertex_particle_in_iterator_init

  subroutine hepmc_vertex_particle_in_iterator_final (it)
    type(hepmc_vertex_particle_in_iterator_t), intent(inout) :: it
    call vertex_particles_in_const_iterator_delete (it%obj)
  end subroutine hepmc_vertex_particle_in_iterator_final

  subroutine hepmc_vertex_particle_in_iterator_advance (it)
    type(hepmc_vertex_particle_in_iterator_t), intent(inout) :: it
    call vertex_particles_in_const_iterator_advance (it%obj)
  end subroutine hepmc_vertex_particle_in_iterator_advance

  subroutine hepmc_vertex_particle_in_iterator_reset (it)
    type(hepmc_vertex_particle_in_iterator_t), intent(inout) :: it
    call vertex_particles_in_const_iterator_reset (it%obj, it%v_obj)
  end subroutine hepmc_vertex_particle_in_iterator_reset

  function hepmc_vertex_particle_in_iterator_is_valid (it) result (flag)
    logical :: flag
    type(hepmc_vertex_particle_in_iterator_t), intent(in) :: it
    flag = vertex_particles_in_const_iterator_is_valid (it%obj, it%v_obj)
  end function hepmc_vertex_particle_in_iterator_is_valid

  function hepmc_vertex_particle_in_iterator_get (it) result (prt)
    type(hepmc_particle_t) :: prt
    type(hepmc_vertex_particle_in_iterator_t), intent(in) :: it
    prt%obj = vertex_particles_in_const_iterator_get (it%obj)
  end function hepmc_vertex_particle_in_iterator_get

  subroutine hepmc_vertex_particle_out_iterator_init (it, v)
    type(hepmc_vertex_particle_out_iterator_t), intent(out) :: it
    type(hepmc_vertex_t), intent(in) :: v
    it%obj = new_vertex_particles_out_const_iterator (v%obj)
    it%v_obj = v%obj
  end subroutine hepmc_vertex_particle_out_iterator_init

  subroutine hepmc_vertex_particle_out_iterator_final (it)
    type(hepmc_vertex_particle_out_iterator_t), intent(inout) :: it
    call vertex_particles_out_const_iterator_delete (it%obj)
  end subroutine hepmc_vertex_particle_out_iterator_final

  subroutine hepmc_vertex_particle_out_iterator_advance (it)
    type(hepmc_vertex_particle_out_iterator_t), intent(inout) :: it
    call vertex_particles_out_const_iterator_advance (it%obj)
  end subroutine hepmc_vertex_particle_out_iterator_advance

  subroutine hepmc_vertex_particle_out_iterator_reset (it)
    type(hepmc_vertex_particle_out_iterator_t), intent(inout) :: it
    call vertex_particles_out_const_iterator_reset (it%obj, it%v_obj)
  end subroutine hepmc_vertex_particle_out_iterator_reset

  function hepmc_vertex_particle_out_iterator_is_valid (it) result (flag)
    logical :: flag
    type(hepmc_vertex_particle_out_iterator_t), intent(in) :: it
    flag = vertex_particles_out_const_iterator_is_valid (it%obj, it%v_obj)
  end function hepmc_vertex_particle_out_iterator_is_valid

  function hepmc_vertex_particle_out_iterator_get (it) result (prt)
    type(hepmc_particle_t) :: prt
    type(hepmc_vertex_particle_out_iterator_t), intent(in) :: it
    prt%obj = vertex_particles_out_const_iterator_get (it%obj)
  end function hepmc_vertex_particle_out_iterator_get

  subroutine hepmc_event_init (evt, proc_id, event_id)
    type(hepmc_event_t), intent(out) :: evt
    integer, intent(in), optional :: proc_id, event_id
    integer(c_int) :: pid, eid
    pid = 0;  if (present (proc_id))  pid = proc_id
    eid = 0;  if (present (event_id)) eid = event_id
    evt%obj = new_gen_event (pid, eid)
  end subroutine hepmc_event_init

  subroutine hepmc_event_final (evt)
    type(hepmc_event_t), intent(inout) :: evt
    call gen_event_delete (evt%obj)
  end subroutine hepmc_event_final

  subroutine hepmc_event_print (evt)
    type(hepmc_event_t), intent(in) :: evt
    call gen_event_print (evt%obj)
  end subroutine hepmc_event_print
    
  function hepmc_event_get_event_index (evt) result (i_proc)
    integer :: i_proc
    type(hepmc_event_t), intent(in) :: evt
    i_proc = gen_event_event_number (evt%obj)
  end function hepmc_event_get_event_index

  subroutine hepmc_event_set_process_id (evt, proc)
    type(hepmc_event_t), intent(in) :: evt
    integer, intent(in) :: proc
    integer(c_int) :: i_proc
    i_proc = proc
    call gen_event_set_signal_process_id (evt%obj, i_proc)
  end subroutine hepmc_event_set_process_id

  function hepmc_event_get_process_id (evt) result (i_proc)
    integer :: i_proc
    type(hepmc_event_t), intent(in) :: evt
    i_proc = gen_event_signal_process_id (evt%obj)
  end function hepmc_event_get_process_id

  subroutine hepmc_event_set_scale (evt, scale)
    type(hepmc_event_t), intent(in) :: evt
    real(default), intent(in) :: scale
    real(c_double) :: cscale
    cscale = scale
    call gen_event_set_event_scale (evt%obj, cscale)
  end subroutine hepmc_event_set_scale

  function hepmc_event_get_scale (evt) result (scale)
    real(default) :: scale
    type(hepmc_event_t), intent(in) :: evt
    scale = gen_event_event_scale (evt%obj)
  end function hepmc_event_get_scale

  subroutine hepmc_event_set_alpha_qcd (evt, alpha)
    type(hepmc_event_t), intent(in) :: evt
    real(default), intent(in) :: alpha
    real(c_double) :: a
    a = alpha
    call gen_event_set_alpha_qcd (evt%obj, a)
  end subroutine hepmc_event_set_alpha_qcd

  function hepmc_event_get_alpha_qcd (evt) result (alpha)
    real(default) :: alpha
    type(hepmc_event_t), intent(in) :: evt
    alpha = gen_event_alpha_qcd (evt%obj)
  end function hepmc_event_get_alpha_qcd

  subroutine hepmc_event_set_alpha_qed (evt, alpha)
    type(hepmc_event_t), intent(in) :: evt
    real(default), intent(in) :: alpha
    real(c_double) :: a
    a = alpha
    call gen_event_set_alpha_qed (evt%obj, a)
  end subroutine hepmc_event_set_alpha_qed

  function hepmc_event_get_alpha_qed (evt) result (alpha)
    real(default) :: alpha
    type(hepmc_event_t), intent(in) :: evt
    alpha = gen_event_alpha_qed (evt%obj)
  end function hepmc_event_get_alpha_qed

  subroutine hepmc_event_clear_weights (evt)
    type(hepmc_event_t), intent(in) :: evt
    call gen_event_clear_weights (evt%obj)
  end subroutine hepmc_event_clear_weights

  subroutine hepmc_event_add_weight (evt, weight)
    type(hepmc_event_t), intent(in) :: evt
    real(default), intent(in) :: weight
    real(c_double) :: w
    w = weight
    call gen_event_add_weight (evt%obj, w)
  end subroutine hepmc_event_add_weight

  function hepmc_event_get_weights_size (evt) result (n)
    integer :: n
    type(hepmc_event_t), intent(in) :: evt
    n = gen_event_weights_size (evt%obj)
  end function hepmc_event_get_weights_size

  function hepmc_event_get_weight (evt, index) result (weight)
    real(default) :: weight
    type(hepmc_event_t), intent(in) :: evt
    integer, intent(in) :: index
    integer(c_int) :: i
    i = index - 1
    weight = gen_event_weight (evt%obj, i)
  end function hepmc_event_get_weight

  subroutine hepmc_event_add_vertex (evt, v)
    type(hepmc_event_t), intent(inout) :: evt
    type(hepmc_vertex_t), intent(in) :: v
    call gen_event_add_vertex (evt%obj, v%obj)
  end subroutine hepmc_event_add_vertex

  subroutine hepmc_event_set_signal_process_vertex (evt, v)
    type(hepmc_event_t), intent(inout) :: evt
    type(hepmc_vertex_t), intent(in) :: v
    call gen_event_set_signal_process_vertex (evt%obj, v%obj)
  end subroutine hepmc_event_set_signal_process_vertex

  subroutine hepmc_event_set_beam_particles (evt, prt1, prt2)
    type(hepmc_event_t), intent(inout) :: evt
    type(hepmc_particle_t), intent(in) :: prt1, prt2
    logical(c_bool) :: flag
    flag = gen_event_set_beam_particles (evt%obj, prt1%obj, prt2%obj)
  end subroutine hepmc_event_set_beam_particles

  subroutine hepmc_event_set_cross_section (evt, xsec, xsec_err)
    type(hepmc_event_t), intent(inout) :: evt
    real(default), intent(in) :: xsec, xsec_err
    call gen_event_set_cross_section &
         (evt%obj, &
         real (xsec * 1e-3_default, c_double), &
         real (xsec_err * 1e-3_default, c_double))
  end subroutine hepmc_event_set_cross_section

  subroutine hepmc_event_particle_iterator_init (it, evt)
    type(hepmc_event_particle_iterator_t), intent(out) :: it
    type(hepmc_event_t), intent(in) :: evt
    it%obj = new_event_particle_const_iterator (evt%obj)
    it%evt_obj = evt%obj
  end subroutine hepmc_event_particle_iterator_init

  subroutine hepmc_event_particle_iterator_final (it)
    type(hepmc_event_particle_iterator_t), intent(inout) :: it
    call event_particle_const_iterator_delete (it%obj)
  end subroutine hepmc_event_particle_iterator_final

  subroutine hepmc_event_particle_iterator_advance (it)
    type(hepmc_event_particle_iterator_t), intent(inout) :: it
    call event_particle_const_iterator_advance (it%obj)
  end subroutine hepmc_event_particle_iterator_advance

  subroutine hepmc_event_particle_iterator_reset (it)
    type(hepmc_event_particle_iterator_t), intent(inout) :: it
    call event_particle_const_iterator_reset (it%obj, it%evt_obj)
  end subroutine hepmc_event_particle_iterator_reset

  function hepmc_event_particle_iterator_is_valid (it) result (flag)
    logical :: flag
    type(hepmc_event_particle_iterator_t), intent(in) :: it
    flag = event_particle_const_iterator_is_valid (it%obj, it%evt_obj)
  end function hepmc_event_particle_iterator_is_valid

  function hepmc_event_particle_iterator_get (it) result (prt)
    type(hepmc_particle_t) :: prt
    type(hepmc_event_particle_iterator_t), intent(in) :: it
    prt%obj = event_particle_const_iterator_get (it%obj)
  end function hepmc_event_particle_iterator_get

  subroutine hepmc_iostream_open_out (iostream, filename)
    type(hepmc_iostream_t), intent(out) :: iostream
    type(string_t), intent(in) :: filename
    iostream%obj = new_io_gen_event_out (char (filename) // c_null_char)
  end subroutine hepmc_iostream_open_out

  subroutine hepmc_iostream_open_in (iostream, filename)
    type(hepmc_iostream_t), intent(out) :: iostream
    type(string_t), intent(in) :: filename
    iostream%obj = new_io_gen_event_in (char (filename) // c_null_char)
  end subroutine hepmc_iostream_open_in

  subroutine hepmc_iostream_close (iostream)
    type(hepmc_iostream_t), intent(inout) :: iostream
    call io_gen_event_delete (iostream%obj)
  end subroutine hepmc_iostream_close

  subroutine hepmc_iostream_write_event (iostream, evt)
    type(hepmc_iostream_t), intent(inout) :: iostream
    type(hepmc_event_t), intent(in) :: evt
    call io_gen_event_write_event (iostream%obj, evt%obj)
  end subroutine hepmc_iostream_write_event

  subroutine hepmc_iostream_read_event (iostream, evt, ok)
    type(hepmc_iostream_t), intent(inout) :: iostream
    type(hepmc_event_t), intent(in) :: evt
    logical, intent(out) :: ok
    ok = io_gen_event_read_event (iostream%obj, evt%obj)
  end subroutine hepmc_iostream_read_event

  subroutine hepmc_test
    type(hepmc_event_t) :: evt
    type(hepmc_vertex_t) :: v1, v2, v3, v4
    type(hepmc_particle_t) :: prt1, prt2, prt3, prt4, prt5, prt6, prt7, prt8
    type(hepmc_iostream_t) :: iostream
    type(flavor_t) :: flv
    type(color_t) :: col
    type(polarization_t) :: pol
    type(particle_data_t), target :: photon_data

    ! Initialize a photon flavor object and some polarization
    call particle_data_init (photon_data, var_str ("PHOTON"), 22)
    call particle_data_set (photon_data, spin_type=VECTOR)
    call particle_data_freeze (photon_data)
    call flavor_init (flv, photon_data)
    call polarization_init_angles &
         (pol, flv, 0.6_default, 1._default, 0.5_default)

    ! Event initialization
    call hepmc_event_init (evt, 20, 1)

    ! $p\to q$ splittings
    call hepmc_vertex_init (v1)
    call hepmc_event_add_vertex (evt, v1)
    call hepmc_vertex_init (v2)
    call hepmc_event_add_vertex (evt, v2)
    call particle_init (prt1, &
         0._default, 0._default, 7000._default, 7000._default, &
         2212, 3)
    call hepmc_vertex_add_particle_in (v1, prt1)
    call particle_init (prt2, &
         0._default, 0._default,-7000._default, 7000._default, &
         2212, 3)
    call hepmc_vertex_add_particle_in (v2, prt2)
    call particle_init (prt3, &
         .750_default, -1.569_default, 32.191_default, 32.238_default, &
         1, 3)
    call color_init_from_array (col, (/501/))
    call hepmc_particle_set_color (prt3, col)
    call hepmc_vertex_add_particle_out (v1, prt3)
    call particle_init (prt4, &
         -3.047_default, -19._default, -54.629_default, 57.920_default, &
         -2, 3)
    call color_init_from_array (col, (/-501/))
    call hepmc_particle_set_color (prt4, col)
    call hepmc_vertex_add_particle_out (v2, prt4)
    
    ! Hard interaction
    call hepmc_vertex_init (v3)
    call hepmc_event_add_vertex (evt, v3)
    call hepmc_vertex_add_particle_in (v3, prt3)
    call hepmc_vertex_add_particle_in (v3, prt4)
    call particle_init (prt6, &
         -3.813_default, 0.113_default, -1.833_default, 4.233_default, &
         22, 1)
    call hepmc_particle_set_polarization (prt6, pol)
    call hepmc_vertex_add_particle_out (v3, prt6)
    call particle_init (prt5, &
         1.517_default, -20.68_default, -20.605_default, 85.925_default, &
         -24, 3)
    call hepmc_vertex_add_particle_out (v3, prt5)
    call hepmc_event_set_signal_process_vertex (evt, v3)
    
    ! $W^-$ decay
    call vertex_init_pos (v4, &
         0.12_default, -0.3_default, 0.05_default, 0.004_default)
    call hepmc_event_add_vertex (evt, v4)
    call hepmc_vertex_add_particle_in (v4, prt5)
    call particle_init (prt7, &
         -2.445_default, 28.816_default, 6.082_default, 29.552_default, &
         1, 1)
    call hepmc_vertex_add_particle_out (v4, prt7)
    call particle_init (prt8, &
         3.962_default, -49.498_default, -26.687_default, 56.373_default, &
         -2, 1)
    call hepmc_vertex_add_particle_out (v4, prt8)
    
    ! Event output
    call hepmc_event_print (evt)
    print *, "Writing to file 'hepmc_test.hepmc.dat'"
    call hepmc_iostream_open_out (iostream , var_str ("hepmc_test.hepmc.dat"))
    call hepmc_iostream_write_event (iostream, evt)
    call hepmc_iostream_close (iostream)
    print *, "Write completed"

    ! Wrapup
    call polarization_final (pol)
    call hepmc_event_final (evt)

  contains

    subroutine vertex_init_pos (v, x, y, z, t)
      type(hepmc_vertex_t), intent(out) :: v
      real(default), intent(in) :: x, y, z, t
      type(vector4_t) :: xx
      xx = vector4_moving (t, vector3_moving ((/x, y, z/)))
      call hepmc_vertex_init (v, xx)
    end subroutine vertex_init_pos

    subroutine particle_init (prt, px, py, pz, E, pdg, status)
      type(hepmc_particle_t), intent(out) :: prt
      real(default), intent(in) :: px, py, pz, E
      integer, intent(in) :: pdg, status
      type(vector4_t) :: p
      p = vector4_moving (E, vector3_moving ((/px, py, pz/)))
      call hepmc_particle_init (prt, p, pdg, status)
    end subroutine particle_init

  end subroutine hepmc_test


end module hepmc_interface
