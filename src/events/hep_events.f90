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

module hep_events
  
  use kinds
  use io_units
  use iso_varying_string, string_t => varying_string  
  use diagnostics
  use lorentz
  use unit_tests
  
  use flavors
  use colors
  use helicities
  use quantum_numbers
  use interactions
  use state_matrices
  use evaluators
  use polarizations
  use model_data
  use subevents
  use particles
  use hep_common
  use hepmc_interface
  use lcio_interface
  use event_base

  implicit none
  private

  public :: hepeup_from_event
  public :: hepeup_to_event
  public :: hepevt_from_event
  public :: hepmc_event_from_particle_set 
  public :: hepmc_to_event
  public :: particle_to_lcio
  public :: particle_from_lcio_particle
  public :: lcio_event_from_particle_set 
  public :: lcio_event_to_particle_set
  public :: lcio_to_event
  public :: hep_events_test

contains
  
  subroutine hepeup_from_event &
       (event, keep_beams, keep_remnants, process_index)
    class(generic_event_t), intent(in), target :: event
    logical, intent(in), optional :: keep_beams
    logical, intent(in), optional :: keep_remnants
    integer, intent(in), optional :: process_index
    type(particle_set_t), pointer :: particle_set
    real(default) :: scale, alpha_qcd
    if (event%has_valid_particle_set ()) then
       particle_set => event%get_particle_set_ptr ()
       call hepeup_from_particle_set (particle_set, keep_beams, keep_remnants)
       if (present (process_index)) &
            call hepeup_set_event_parameters (proc_id = process_index)
       scale = event%get_fac_scale ()
       if (scale /= 0) &
            call hepeup_set_event_parameters (scale = scale)
       alpha_qcd = event%get_alpha_s ()       
       if (alpha_qcd /= 0) &
            call hepeup_set_event_parameters (alpha_qcd = alpha_qcd)
       if (event%weight_prc_is_known ()) then
          call hepeup_set_event_parameters (weight = event%get_weight_prc ())
       else
          call msg_bug ("HEPEUP: process weight is unknown")
       end if
    else
       call msg_bug ("HEPEUP: event incomplete")
    end if
  end subroutine hepeup_from_event

  subroutine hepeup_to_event &
       (event, fallback_model, process_index, recover_beams, &
       use_alpha_s, use_scale)
    class(generic_event_t), intent(inout), target :: event
    class(model_data_t), intent(in), target :: fallback_model
    integer, intent(out), optional :: process_index
    logical, intent(in), optional :: recover_beams
    logical, intent(in), optional :: use_alpha_s
    logical, intent(in), optional :: use_scale
    class(model_data_t), pointer :: model
    real(default) :: weight, scale, alpha_qcd
    type(particle_set_t) :: particle_set
    model => event%get_model_ptr ()
    call hepeup_to_particle_set &
         (particle_set, recover_beams, model, fallback_model)
    call event%set_hard_particle_set (particle_set)
    call particle_set%final ()
    if (present (process_index)) then
       call hepeup_get_event_parameters (proc_id = process_index)
    end if
    call hepeup_get_event_parameters (weight = weight, &
         scale = scale, alpha_qcd = alpha_qcd)
    call event%set_weight_ref (weight)
    if (present (use_alpha_s)) then
       if (use_alpha_s .and. alpha_qcd > 0) &
            call event%set_alpha_qcd_forced (alpha_qcd)
    end if
    if (present (use_scale)) then
       if (use_scale .and. scale > 0) &
            call event%set_scale_forced (scale)
    end if
  end subroutine hepeup_to_event

  subroutine hepevt_from_event (event, i_evt, keep_beams, keep_remnants)
    class(generic_event_t), intent(in), target :: event
    integer, intent(in), optional :: i_evt
    logical, intent(in), optional :: keep_beams  
    logical, intent(in), optional :: keep_remnants
    type(particle_set_t), pointer :: particle_set
    if (event%has_valid_particle_set ()) then
       particle_set => event%get_particle_set_ptr ()
       call hepevt_from_particle_set (particle_set, keep_beams, keep_remnants)
       if (event%weight_prc_is_known () .and. event%sqme_prc_is_known ()) then
          call hepevt_set_event_parameters ( &
               weight = event%get_weight_prc (), &
               function_value = event%get_sqme_prc ())
       else
          call msg_bug ("HEPEVT: event weight and/or sqme unknown")
       end if
       if (present (i_evt)) &
            call hepevt_set_event_parameters (i_evt = i_evt)
    else
       call msg_bug ("HEPEVT: event incomplete")
    end if
  end subroutine hepevt_from_event

  subroutine particle_to_hepmc (prt, hprt)
    type(particle_t), intent(in) :: prt
    type(hepmc_particle_t), intent(out) :: hprt
    integer :: hepmc_status
    select case (prt%get_status ())
    case (PRT_UNDEFINED)
       hepmc_status = 0
    case (PRT_OUTGOING)
       hepmc_status = 1
    case (PRT_BEAM)
       hepmc_status = 4
    case (PRT_RESONANT)
       if (abs(prt%get_pdg()) == 13 .or. &
            abs(prt%get_pdg()) == 15) then
          hepmc_status = 2
       else
          hepmc_status = 11
       end if
    case default
       hepmc_status = 3
    end select
    call hepmc_particle_init (hprt, &
         prt%get_momentum (), prt%get_pdg (), &
         hepmc_status)
    call hepmc_particle_set_color (hprt, prt%get_color ())
    select case (prt%get_polarization_status ())
    case (PRT_DEFINITE_HELICITY)
       call hepmc_particle_set_polarization (hprt, &
            prt%get_helicity ())
    case (PRT_GENERIC_POLARIZATION)
       call hepmc_particle_set_polarization (hprt, &
            prt%get_polarization ())
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
    n_tot = particle_set%get_n_tot ()
    allocate (v_from (n_tot), v_to (n_tot))
    call particle_set%assign_vertices (v_from, v_to, n_vertices)
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
    is_beam = particle_set%prt(1:n_tot)%get_status () == PRT_BEAM
    if (.not. any (is_beam)) then
       is_beam = particle_set%prt(1:n_tot)%get_status () == PRT_INCOMING
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
    FIND_SIGNAL_PROCESS: do i = 1, n_tot
       if (particle_set%prt(i)%get_status () == PRT_INCOMING) then
          call hepmc_event_set_signal_process_vertex (evt, v(v_to(i)))
          exit FIND_SIGNAL_PROCESS
       end if
    end do FIND_SIGNAL_PROCESS
  end subroutine hepmc_event_from_particle_set
  
  subroutine particle_from_hepmc_particle &
       (prt, hprt, model, polarization, barcode)
    type(particle_t), intent(out) :: prt
    type(hepmc_particle_t), intent(in) :: hprt
    type(model_data_t), intent(in), target :: model
    integer, intent(in) :: polarization
    integer, dimension(:), intent(in) :: barcode
    type(hepmc_polarization_t) :: hpol
    type(flavor_t) :: flv
    type(color_t) :: col
    type(helicity_t) :: hel
    type(polarization_t) :: pol
    integer :: n_parents, n_children
    integer, dimension(:), allocatable :: &
         parent_barcode, child_barcode, parent, child
    integer :: i
    select case (hepmc_particle_get_status (hprt))
    case (1);  call prt%set_status (PRT_OUTGOING)
    case (2);  call prt%set_status (PRT_RESONANT)
    case (3);  call prt%set_status (PRT_VIRTUAL)
    end select
    if (hepmc_particle_is_beam (hprt)) call prt%set_status (PRT_BEAM)
    call flv%init (hepmc_particle_get_pdg (hprt), model)
    call col%init (hepmc_particle_get_color (hprt))
    call prt%set_flavor (flv)   
    call prt%set_color (col)
    call prt%set_polarization (polarization)
    select case (polarization)
    case (PRT_DEFINITE_HELICITY)
       hpol = hepmc_particle_get_polarization (hprt)
       call hepmc_polarization_to_hel (hpol, prt%get_flv (), hel)
       call prt%set_helicity (hel)
       call hepmc_polarization_final (hpol)
    case (PRT_GENERIC_POLARIZATION)
       hpol = hepmc_particle_get_polarization (hprt)
       call hepmc_polarization_to_pol (hpol, prt%get_flv (), pol)
       call prt%set_pol (pol)
       call hepmc_polarization_final (hpol)
    end select
    call prt%set_momentum (hepmc_particle_get_momentum (hprt), &
         hepmc_particle_get_mass_squared (hprt))
    n_parents  = hepmc_particle_get_n_parents  (hprt)
    n_children = hepmc_particle_get_n_children (hprt)
    allocate (parent_barcode (n_parents),  parent (n_parents))
    allocate (child_barcode  (n_children), child  (n_children))
    parent_barcode = hepmc_particle_get_parent_barcodes (hprt)
    child_barcode  = hepmc_particle_get_child_barcodes  (hprt)
    do i = 1, size (barcode)
       where (parent_barcode == barcode(i))  parent = i
       where (child_barcode  == barcode(i))  child  = i
    end do
    call prt%set_parents (parent)
    call prt%set_children (child)
    if (prt%get_status () == PRT_VIRTUAL .and. n_parents == 0) &
         call prt%set_status (PRT_INCOMING)
  end subroutine particle_from_hepmc_particle

  subroutine hepmc_event_to_particle_set &
       (particle_set, evt, model, fallback_model, polarization)
    type(particle_set_t), intent(inout), target :: particle_set
    type(hepmc_event_t), intent(in) :: evt
    class(model_data_t), intent(in), target :: model, fallback_model
    integer, intent(in) :: polarization
    type(hepmc_event_particle_iterator_t) :: it
    type(hepmc_vertex_t) :: v
    type(hepmc_vertex_particle_in_iterator_t) :: v_it
    type(hepmc_particle_t) :: prt
    integer, dimension(:), allocatable :: barcode
    integer :: n_tot, i, j, bc
    logical :: has_parents, has_children
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
       call particle_from_hepmc_particle (particle_set%prt(i), &
            prt, model, polarization, barcode)
       call hepmc_event_particle_iterator_advance (it)
    end do
    call hepmc_event_particle_iterator_final (it)
    v = hepmc_event_get_signal_process_vertex (evt)
    if (hepmc_vertex_is_valid (v)) then
       call hepmc_vertex_particle_in_iterator_init (v_it, v)
       do while (hepmc_vertex_particle_in_iterator_is_valid (v_it))
          prt = hepmc_vertex_particle_in_iterator_get (v_it)
          bc = hepmc_particle_get_barcode &
               (hepmc_vertex_particle_in_iterator_get (v_it))
          do i = 1, size(barcode)
             if (bc == barcode(i))  &
               call particle_set%prt(i)%set_status (PRT_INCOMING)
          end do
          call hepmc_vertex_particle_in_iterator_advance (v_it)
       end do
       call hepmc_vertex_particle_in_iterator_final (v_it)
    end if
    do i = 1, n_tot
       if (particle_set%prt(i)%get_status () == PRT_VIRTUAL &
            .and. particle_set%prt(i)%get_n_children () == 0) &
            call particle_set%prt(i)%set_status (PRT_OUTGOING)
    end do
    particle_set%n_tot = n_tot
    particle_set%n_beam  = &
         count (particle_set%prt%get_status () == PRT_BEAM)
    particle_set%n_in  = &
         count (particle_set%prt%get_status () == PRT_INCOMING)
    particle_set%n_out = &
         count (particle_set%prt%get_status () == PRT_OUTGOING)
    particle_set%n_vir = &
         particle_set%n_tot - particle_set%n_in - particle_set%n_out
  end subroutine hepmc_event_to_particle_set

  subroutine hepmc_to_event &
       (event, hepmc_event, fallback_model, process_index, recover_beams, &
       use_alpha_s, use_scale)
    class(generic_event_t), intent(inout), target :: event
    type(hepmc_event_t), intent(inout) :: hepmc_event
    class(model_data_t), intent(in), target :: fallback_model
    integer, intent(out), optional :: process_index
    logical, intent(in), optional :: recover_beams
    logical, intent(in), optional :: use_alpha_s
    logical, intent(in), optional :: use_scale
    class(model_data_t), pointer :: model
    real(default) :: weight, scale, alpha_qcd
    type(particle_set_t) :: particle_set
    model => event%get_model_ptr ()
    call hepmc_event_to_particle_set (particle_set, &
         hepmc_event, model, fallback_model, PRT_DEFINITE_HELICITY)
    call event%set_hard_particle_set (particle_set)
    call particle_set%final ()
    call event%set_weight_ref (1._default)
    alpha_qcd = hepmc_event_get_alpha_qcd (hepmc_event)
    scale = hepmc_event_get_scale (hepmc_event)
    if (present (use_alpha_s)) then
       if (use_alpha_s .and. alpha_qcd > 0) &
            call event%set_alpha_qcd_forced (alpha_qcd)
    end if
    if (present (use_scale)) then
       if (use_scale .and. scale > 0) &
            call event%set_scale_forced (scale)
    end if
  end subroutine hepmc_to_event
  
  subroutine particle_to_lcio (prt, lprt)
    type(particle_t), intent(in) :: prt
    type(lcio_particle_t), intent(out) :: lprt
    integer :: lcio_status
    select case (prt%get_status ())
    case (PRT_UNDEFINED)
       lcio_status = 0
    case (PRT_OUTGOING)
       lcio_status = 1
    case (PRT_BEAM_REMNANT)
       if (prt%get_n_children () == 0) then
          lcio_status = 1
       else
          lcio_status = 3
       end if
    case (PRT_BEAM)
       lcio_status = 4
    case (PRT_RESONANT)
       if (abs (prt%get_pdg ()) == 13 .or. &
            abs (prt%get_pdg ()) == 15) then
          lcio_status = 2
       else
          lcio_status = 11
       end if
    case default
       lcio_status = 3
    end select
    call lcio_particle_init (lprt, &
         prt%get_momentum (), &
         prt%get_pdg (), &
         lcio_status)
    call lcio_particle_set_color (lprt, prt%get_color ())
    select case (prt%get_polarization_status ())
    case (PRT_DEFINITE_HELICITY)
       call lcio_polarization_init (lprt, prt%get_helicity ())
    case (PRT_GENERIC_POLARIZATION)
       call lcio_polarization_init (lprt, prt%get_polarization ())
    end select
  end subroutine particle_to_lcio

  subroutine particle_from_lcio_particle &
     (prt, lprt, model, daughters, parents, polarization)
    type(particle_t), intent(out) :: prt
    type(lcio_particle_t), intent(in) :: lprt
    type(model_data_t), intent(in), target :: model
    integer, dimension(:), intent(in) :: daughters, parents    
    type(flavor_t) :: flv
    type(color_t) :: col
    type(helicity_t) :: hel
    type(polarization_t) :: pol
    integer, intent(in) :: polarization
    integer :: i
    select case (lcio_particle_get_status (lprt))
    case (1);  call prt%set_status (PRT_OUTGOING)
    case (2);  call prt%set_status (PRT_RESONANT)
    case (3);  call prt%set_status (PRT_VIRTUAL)
    end select
    call flv%init (lcio_particle_get_pdg (lprt), model)
    call col%init (lcio_particle_get_flow (lprt))    
    if (flv%is_beam_remnant ())  call prt%set_status (PRT_BEAM_REMNANT)
    call prt%set_flavor (flv)
    call prt%set_color (col)
    call prt%set_polarization (polarization)
    select case (polarization)
    case (PRT_DEFINITE_HELICITY)
       call lcio_particle_to_hel (lprt, prt%get_flv (), hel)
       call prt%set_helicity (hel)
    case (PRT_GENERIC_POLARIZATION)
       call lcio_particle_to_pol (lprt, prt%get_flv (), pol)
       call prt%set_pol (pol)
    end select
    call prt%set_momentum (lcio_particle_get_momentum (lprt), &
         lcio_particle_get_mass_squared (lprt))
    call prt%set_parents (parents)
    call prt%set_children (daughters)
    if (prt%get_status () == PRT_VIRTUAL .and. size(parents) == 0) &
         call prt%set_status (PRT_INCOMING)
  end subroutine particle_from_lcio_particle

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
       n_parents = particle_set%prt(i)%get_n_parents ()
       if (n_parents /= 0) then
          allocate (parent (n_parents))
          parent = particle_set%prt(i)%get_parents ()
          do j = 1, n_parents
             call lcio_particle_set_parent (lprt(i), lprt(parent(j)))
          end do
          deallocate (parent)
       end if
       call lcio_particle_add_to_evt_coll (lprt(i), evt)
    end do
    call lcio_event_add_coll (evt)
  end subroutine lcio_event_from_particle_set
  
  subroutine lcio_event_to_particle_set &
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
       call particle_from_lcio_particle (particle_set%prt(i), prt, model, &
            daughters, parents, polarization)
       deallocate (daughters, parents)
    end do
    do i = 1, n_tot
       if (particle_set%prt(i)%get_status () == PRT_VIRTUAL) then
          CHECK_BEAM: do j = 1, particle_set%prt(i)%get_n_parents ()
             if (particle_set%prt(j)%get_status () == PRT_BEAM) &
                  call particle_set%prt(i)%set_status (PRT_INCOMING)
             exit CHECK_BEAM
          end do CHECK_BEAM
       end if
    end do
    particle_set%n_tot = n_tot
    particle_set%n_beam  = &
         count (particle_set%prt%get_status () == PRT_BEAM)
    particle_set%n_in  = &
         count (particle_set%prt%get_status () == PRT_INCOMING)
    particle_set%n_out = &
         count (particle_set%prt%get_status () == PRT_OUTGOING)
    particle_set%n_vir = &
         particle_set%n_tot - particle_set%n_in - particle_set%n_out
  end subroutine lcio_event_to_particle_set

  subroutine lcio_to_event &
       (event, lcio_event, fallback_model, process_index, recover_beams, &
       use_alpha_s, use_scale)
    class(generic_event_t), intent(inout), target :: event
    type(lcio_event_t), intent(inout) :: lcio_event
    class(model_data_t), intent(in), target :: fallback_model
    integer, intent(out), optional :: process_index
    logical, intent(in), optional :: recover_beams
    logical, intent(in), optional :: use_alpha_s
    logical, intent(in), optional :: use_scale
    class(model_data_t), pointer :: model
    real(default) :: weight, scale, alpha_qcd
    type(particle_set_t) :: particle_set
    model => event%get_model_ptr ()
    call lcio_event_to_particle_set (particle_set, &
         lcio_event, model, fallback_model, PRT_DEFINITE_HELICITY)
    call event%set_hard_particle_set (particle_set)
    call particle_set%final ()
    alpha_qcd = lcio_event_get_alphas (lcio_event)
    scale = lcio_event_get_scaleval (lcio_event)
    if (present (use_alpha_s)) then
       if (use_alpha_s .and. alpha_qcd > 0) &
            call event%set_alpha_qcd_forced (alpha_qcd)
    end if
    if (present (use_scale)) then
       if (use_scale .and. scale > 0) &
            call event%set_scale_forced (scale)
    end if
  end subroutine lcio_to_event
  
  subroutine hep_events_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    if (hepmc_is_available ()) then
       call test (hep_events_1, "hep_events_1", &
            "check HepMC event routines", &
            u, results)
    end if  
  end subroutine hep_events_test


  subroutine hep_events_1 (u)
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
    type(hepmc_event_t) :: hepmc_event
    type(hepmc_iostream_t) :: iostream    

    write (u, "(A)")  "* Test output: HEP events"
    write (u, "(A)")  "*   Purpose: test HepMC event routines"
    write (u, "(A)")      

    write (u, "(A)")  "* Reading model file"

    call model%init_sm_test ()

    write (u, "(A)")
    write (u, "(A)")  "* Initializing production process"

    call interaction_init (int1, 2, 0, 1, set_relations=.true.)
    call flv%init ([1, -1, 23], model)
    call col%init_col_acl ([0, 0, 0], [0, 0, 0])
    call hel(3)%init ( 1, 1)
    call qn%init (flv, col, hel)
    call interaction_add_state (int1, qn, value=(0.25_default, 0._default))
    call hel(3)%init ( 1,-1)
    call qn%init (flv, col, hel)
    call interaction_add_state (int1, qn, value=(0._default, 0.25_default))
    call hel(3)%init (-1, 1)
    call qn%init (flv, col, hel)
    call interaction_add_state (int1, qn, value=(0._default,-0.25_default))
    call hel(3)%init (-1,-1)
    call qn%init (flv, col, hel)
    call interaction_add_state (int1, qn, value=(0.25_default, 0._default))
    call hel(3)%init ( 0, 0)
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
    call particle_set1%write (u)

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
    write (u, "(A)")  "* Transfer particle_set to HepMC, print, and output to"
    write (u, "(A)")  "        hep_events.hepmc.dat"
    write (u, "(A)")
    
    call hepmc_event_init (hepmc_event, 11, 127)
    call hepmc_event_from_particle_set (hepmc_event, particle_set2)
    call hepmc_event_print (hepmc_event)
    call hepmc_iostream_open_out &
         (iostream , var_str ("hep_events.hepmc.dat"))
    call hepmc_iostream_write_event (iostream, hepmc_event)
    call hepmc_iostream_close (iostream)

    write (u, "(A)")
    write (u, "(A)")  "* Recover from HepMC file"
    write (u, "(A)")
    
    call particle_set2%final ()
    call hepmc_event_final (hepmc_event)
    call hepmc_event_init (hepmc_event)
    call hepmc_iostream_open_in &
         (iostream , var_str ("hep_events.hepmc.dat"))
    call hepmc_iostream_read_event (iostream, hepmc_event, ok)
    call hepmc_iostream_close (iostream)
    call hepmc_event_to_particle_set (particle_set2, &
         hepmc_event, model, model, PRT_DEFINITE_HELICITY)
    call particle_set2%write (u)   

    write (u, "(A)")
    write (u, "(A)")  "* Cleanup"
    
    call particle_set1%final ()
    call particle_set2%final ()
    call evaluator_final (eval)
    call interaction_final (int1)
    call interaction_final (int2)
    call hepmc_event_final (hepmc_event)            
    call model%final ()
       
    write (u, "(A)")
    write (u, "(A)")  "* Test output end: particles_2"

  end subroutine hep_events_1
  

end module hep_events
