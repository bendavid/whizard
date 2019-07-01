! WHIZARD 2.7.0 Jan 21 2019
!
! Copyright (C) 1999-2019 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     cf. main AUTHORS file
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

module kinematics

  use kinds, only: default
  use format_utils, only: write_separator
  use diagnostics
  use io_units
  use lorentz
  use physics_defs
  use sf_base
  use phs_base
  use interactions
  use mci_base
  use phs_fks
  use fks_regions
  use process_config
  use process_mci
  use pcm, only: pcm_instance_nlo_t
  use ttv_formfactors, only: m1s_to_mpole

  implicit none
  private

  public :: kinematics_t

  type :: kinematics_t
     integer :: n_in = 0
     integer :: n_channel = 0
     integer :: selected_channel = 0
     type(sf_chain_instance_t), pointer :: sf_chain => null ()
     class(phs_t), pointer :: phs => null ()
     real(default), dimension(:), pointer :: f => null ()
     real(default) :: phs_factor
     logical :: sf_chain_allocated = .false.
     logical :: phs_allocated = .false.
     logical :: f_allocated = .false.
     integer :: emitter = -1
     integer :: i_phs = 0
     integer :: i_con = 0
     logical :: only_cm_frame = .false.
     logical :: new_seed = .true.
     logical :: threshold = .false.
   contains
     procedure :: write => kinematics_write
     procedure :: final => kinematics_final
     procedure :: set_nlo_info => kinematics_set_nlo_info
     procedure :: init_sf_chain => kinematics_init_sf_chain
     procedure :: init_phs => kinematics_init_phs
     procedure :: evaluate_radiation_kinematics => kinematics_evaluate_radiation_kinematics
     procedure :: compute_xi_ref_momenta => kinematics_compute_xi_ref_momenta
     procedure :: compute_selected_channel => kinematics_compute_selected_channel
     procedure :: compute_other_channels => kinematics_compute_other_channels
     procedure :: get_incoming_momenta => kinematics_get_incoming_momenta
     procedure :: recover_mcpar => kinematics_recover_mcpar
     procedure :: recover_sfchain => kinematics_recover_sfchain
     procedure :: get_mcpar => kinematics_get_mcpar
     procedure :: evaluate_sf_chain => kinematics_evaluate_sf_chain
     procedure :: return_beam_momenta => kinematics_return_beam_momenta
     procedure :: lab_is_cm_frame => kinematics_lab_is_cm_frame
     procedure :: boost_to_cm_frame => kinematics_boost_to_cm_frame
     procedure :: modify_momenta_for_subtraction => kinematics_modify_momenta_for_subtraction
     procedure :: threshold_projection => kinematics_threshold_projection
     procedure :: evaluate_radiation => kinematics_evaluate_radiation
  end type kinematics_t


contains

  subroutine kinematics_write (object, unit)
    class(kinematics_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u, c
    u = given_output_unit (unit)
    if (object%f_allocated) then
       write (u, "(1x,A)")  "Flux * PHS volume:"
       write (u, "(2x,ES19.12)")  object%phs_factor
       write (u, "(1x,A)")  "Jacobian factors per channel:"
       do c = 1, size (object%f)
          write (u, "(3x,I0,':',1x,ES14.7)", advance="no")  c, object%f(c)
          if (c == object%selected_channel) then
             write (u, "(1x,A)")  "[selected]"
          else
             write (u, *)
          end if
       end do
    end if
    if (object%sf_chain_allocated) then
       call write_separator (u)
       call object%sf_chain%write (u)
    end if
    if (object%phs_allocated) then
       call write_separator (u)
       call object%phs%write (u)
    end if
  end subroutine kinematics_write

  subroutine kinematics_final (object)
    class(kinematics_t), intent(inout) :: object
    if (object%sf_chain_allocated) then
       call object%sf_chain%final ()
       deallocate (object%sf_chain)
       object%sf_chain_allocated = .false.
    end if
    if (object%phs_allocated) then
       call object%phs%final ()
       deallocate (object%phs)
       object%phs_allocated = .false.
    end if
    if (object%f_allocated) then
       deallocate (object%f)
       object%f_allocated = .false.
    end if
  end subroutine kinematics_final

  subroutine kinematics_set_nlo_info (k, nlo_type)
    class(kinematics_t), intent(inout) :: k
    integer, intent(in) :: nlo_type
    if (nlo_type == NLO_VIRTUAL)  k%only_cm_frame = .true.
  end subroutine kinematics_set_nlo_info

  subroutine kinematics_init_sf_chain (k, sf_chain, config, extended_sf)
    class(kinematics_t), intent(inout) :: k
    type(sf_chain_t), intent(in), target :: sf_chain
    type(process_beam_config_t), intent(in) :: config
    logical, intent(in), optional :: extended_sf
    integer :: n_strfun, n_channel
    integer :: c
    k%n_in = config%data%get_n_in ()
    n_strfun = config%n_strfun
    n_channel = config%n_channel
    allocate (k%sf_chain)
    k%sf_chain_allocated = .true.
    call k%sf_chain%init (sf_chain, n_channel)
    if (n_strfun /= 0) then
       do c = 1, n_channel
          call k%sf_chain%set_channel (c, config%sf_channel(c))
       end do
    end if
    call k%sf_chain%link_interactions ()
    call k%sf_chain%exchange_mask ()
    call k%sf_chain%init_evaluators (extended_sf = extended_sf)
  end subroutine kinematics_init_sf_chain

  subroutine kinematics_init_phs (k, config)
    class(kinematics_t), intent(inout) :: k
    class(phs_config_t), intent(in), target :: config
    k%n_channel = config%get_n_channel ()
    call config%allocate_instance (k%phs)
    call k%phs%init (config)
    k%phs_allocated = .true.
    allocate (k%f (k%n_channel))
    k%f = 0
    k%f_allocated = .true.
  end subroutine kinematics_init_phs

  subroutine kinematics_evaluate_radiation_kinematics (k, r_in)
    class(kinematics_t), intent(inout) :: k
    real(default), intent(in), dimension(:) :: r_in
    select type (phs => k%phs)
    type is (phs_fks_t)
       call phs%generate_radiation_variables &
            (r_in(phs%n_r_born + 1 : phs%n_r_born + 3), k%threshold)
       call phs%compute_cms_energy ()
    end select
  end subroutine kinematics_evaluate_radiation_kinematics

  subroutine kinematics_compute_xi_ref_momenta (k, reg_data, nlo_type)
    class(kinematics_t), intent(inout) :: k
    type(region_data_t), intent(in) :: reg_data
    integer, intent(in) :: nlo_type
    logical :: use_contributors
    use_contributors = allocated (reg_data%alr_contributors)
    select type (phs => k%phs)
    type is (phs_fks_t)
       if (use_contributors) then
          call phs%compute_xi_ref_momenta (contributors = reg_data%alr_contributors)
       else if (k%threshold) then
          if (.not. is_subtraction_component (k%emitter, nlo_type)) &
               call phs%compute_xi_ref_momenta_threshold ()
       else
          call phs%compute_xi_ref_momenta ()
       end if
    end select
  end subroutine kinematics_compute_xi_ref_momenta

  subroutine kinematics_compute_selected_channel &
       (k, mci_work, phs_channel, p, success)
    class(kinematics_t), intent(inout) :: k
    type(mci_work_t), intent(in) :: mci_work
    integer, intent(in) :: phs_channel
    type(vector4_t), dimension(:), intent(out) :: p
    logical, intent(out) :: success
    integer :: sf_channel
    k%selected_channel = phs_channel
    sf_channel = k%phs%config%get_sf_channel (phs_channel)
    call k%sf_chain%compute_kinematics (sf_channel, mci_work%get_x_strfun ())
    call k%sf_chain%get_out_momenta (p(1:k%n_in))
    call k%phs%set_incoming_momenta (p(1:k%n_in))
    call k%phs%compute_flux ()
    call k%phs%select_channel (phs_channel)
    call k%phs%evaluate_selected_channel (phs_channel, &
         mci_work%get_x_process ())

    select type (phs => k%phs)
    type is (phs_fks_t)
      if (phs%q_defined) then
         call phs%get_born_momenta (p)
         k%phs_factor = phs%get_overall_factor ()
         success = .true.
      else
         k%phs_factor = 0
         success = .false.
      end if
    class default
      if (phs%q_defined) then
         call k%phs%get_outgoing_momenta (p(k%n_in + 1 :))
         k%phs_factor = k%phs%get_overall_factor ()
         success = .true.
         if (k%only_cm_frame) then
            if (.not. k%lab_is_cm_frame()) &
               call k%boost_to_cm_frame (p)
         end if
      else
         k%phs_factor = 0
         success = .false.
      end if
    end select
  end subroutine kinematics_compute_selected_channel

  subroutine kinematics_compute_other_channels (k, mci_work, phs_channel)
    class(kinematics_t), intent(inout) :: k
    type(mci_work_t), intent(in) :: mci_work
    integer, intent(in) :: phs_channel
    integer :: c, c_sf
    call k%phs%evaluate_other_channels (phs_channel)
    do c = 1, k%n_channel
       c_sf = k%phs%config%get_sf_channel (c)
       k%f(c) = k%sf_chain%get_f (c_sf) * k%phs%get_f (c)
    end do
  end subroutine kinematics_compute_other_channels

  subroutine kinematics_get_incoming_momenta (k, p)
    class(kinematics_t), intent(in) :: k
    type(vector4_t), dimension(:), intent(out) :: p
    type(interaction_t), pointer :: int
    integer :: i
    int => k%sf_chain%get_out_int_ptr ()
    do i = 1, k%n_in
       p(i) = int%get_momentum (k%sf_chain%get_out_i (i))
    end do
  end subroutine kinematics_get_incoming_momenta

  subroutine kinematics_recover_mcpar (k, mci_work, phs_channel, p)
    class(kinematics_t), intent(inout) :: k
    type(mci_work_t), intent(inout) :: mci_work
    integer, intent(in) :: phs_channel
    type(vector4_t), dimension(:), intent(in) :: p
    integer :: c, c_sf
    real(default), dimension(:), allocatable :: x_sf, x_phs
    c = phs_channel
    c_sf = k%phs%config%get_sf_channel (c)
    k%selected_channel = c
    call k%sf_chain%recover_kinematics (c_sf)
    call k%phs%set_incoming_momenta (p(1:k%n_in))
    call k%phs%compute_flux ()
    call k%phs%set_outgoing_momenta (p(k%n_in+1:))
    call k%phs%inverse ()
    do c = 1, k%n_channel
       c_sf = k%phs%config%get_sf_channel (c)
       k%f(c) = k%sf_chain%get_f (c_sf) * k%phs%get_f (c)
    end do
    k%phs_factor = k%phs%get_overall_factor ()
    c = phs_channel
    c_sf = k%phs%config%get_sf_channel (c)
    allocate (x_sf (k%sf_chain%config%get_n_bound ()))
    allocate (x_phs (k%phs%config%get_n_par ()))
    call k%phs%select_channel (c)
    call k%sf_chain%get_mcpar (c_sf, x_sf)
    call k%phs%get_mcpar (c, x_phs)
    call mci_work%set_x_strfun (x_sf)
    call mci_work%set_x_process (x_phs)
  end subroutine kinematics_recover_mcpar

  subroutine kinematics_recover_sfchain (k, channel, p)
    class(kinematics_t), intent(inout) :: k
    integer, intent(in) :: channel
    type(vector4_t), dimension(:), intent(in) :: p
    k%selected_channel = channel
    call k%sf_chain%recover_kinematics (channel)
  end subroutine kinematics_recover_sfchain

  subroutine kinematics_get_mcpar (k, phs_channel, r)
    class(kinematics_t), intent(in) :: k
    integer, intent(in) :: phs_channel
    real(default), dimension(:), intent(out) :: r
    integer :: sf_channel, n_par_sf, n_par_phs
    sf_channel = k%phs%config%get_sf_channel (phs_channel)
    n_par_phs = k%phs%config%get_n_par ()
    n_par_sf = k%sf_chain%config%get_n_bound ()
    if (n_par_sf > 0) then
       call k%sf_chain%get_mcpar (sf_channel, r(1:n_par_sf))
    end if
    if (n_par_phs > 0) then
       call k%phs%get_mcpar (phs_channel, r(n_par_sf+1:))
    end if
  end subroutine kinematics_get_mcpar

  subroutine kinematics_evaluate_sf_chain (k, fac_scale, sf_rescale)
    class(kinematics_t), intent(inout) :: k
    real(default), intent(in) :: fac_scale
    class(sf_rescale_t), intent(inout), optional :: sf_rescale
    select case (k%sf_chain%get_status ())
    case (SF_DONE_KINEMATICS)
       call k%sf_chain%evaluate (fac_scale, sf_rescale)
    end select
  end subroutine kinematics_evaluate_sf_chain

  subroutine kinematics_return_beam_momenta (k)
    class(kinematics_t), intent(in) :: k
    call k%sf_chain%return_beam_momenta ()
  end subroutine kinematics_return_beam_momenta

  function kinematics_lab_is_cm_frame (k) result (cm_frame)
     logical :: cm_frame
     class(kinematics_t), intent(in) :: k
     cm_frame = k%phs%config%cm_frame
  end function kinematics_lab_is_cm_frame

  subroutine kinematics_boost_to_cm_frame (k, p)
     class(kinematics_t), intent(in) :: k
     type(vector4_t), intent(inout), dimension(:) :: p
     p = inverse (k%phs%lt_cm_to_lab) * p
  end subroutine kinematics_boost_to_cm_frame

  subroutine kinematics_modify_momenta_for_subtraction (k, p_in, p_out)
    class(kinematics_t), intent(inout) :: k
    type(vector4_t), intent(in), dimension(:) :: p_in
    type(vector4_t), intent(out), dimension(:), allocatable :: p_out
    allocate (p_out (size (p_in)))
    if (k%threshold) then
       select type (phs => k%phs)
       type is (phs_fks_t)
          p_out = phs%get_onshell_projected_momenta ()
       end select
    else
       p_out = p_in
    end if
  end subroutine kinematics_modify_momenta_for_subtraction

  subroutine kinematics_threshold_projection (k, pcm_instance, nlo_type)
    class(kinematics_t), intent(inout) :: k
    type(pcm_instance_nlo_t), intent(inout) :: pcm_instance
    integer, intent(in) :: nlo_type
    real(default) :: sqrts, mtop
    type(lorentz_transformation_t) :: L_to_cms
    type(vector4_t), dimension(:), allocatable :: p_tot
    integer :: n_tot
    n_tot = k%phs%get_n_tot ()
    allocate (p_tot (size (pcm_instance%real_kinematics%p_born_cms%phs_point(1)%p)))
    select type (phs => k%phs)
    type is (phs_fks_t)
       p_tot = pcm_instance%real_kinematics%p_born_cms%phs_point(1)%p
    class default
       p_tot(1 : k%n_in) = phs%p
       p_tot(k%n_in + 1 : n_tot) = phs%q
    end select
    sqrts = sum (p_tot (1:k%n_in))**1
    mtop = m1s_to_mpole (sqrts)
    L_to_cms = get_boost_for_threshold_projection (p_tot, sqrts, mtop)
    call pcm_instance%real_kinematics%p_born_cms%set_momenta (1, p_tot)
    associate (p_onshell => pcm_instance%real_kinematics%p_born_onshell%phs_point(1)%p)
       call threshold_projection_born (mtop, L_to_cms, p_tot, p_onshell)
       if (debug2_active (D_THRESHOLD)) then
          print *, 'On-shell projected Born: '
          call vector4_write_set (p_onshell)
       end if
    end associate
  end subroutine kinematics_threshold_projection

  subroutine kinematics_evaluate_radiation (k, p_in, p_out, success)
    class(kinematics_t), intent(inout) :: k
    type(vector4_t), intent(in), dimension(:) :: p_in
    type(vector4_t), intent(out), dimension(:), allocatable :: p_out
    logical, intent(out) :: success
    type(vector4_t), dimension(:), allocatable :: p_real
    type(vector4_t), dimension(:), allocatable :: p_born
    real(default) :: xi_max_offshell, xi_offshell, y_offshell, jac_rand_dummy, phi
    select type (phs => k%phs)
    type is (phs_fks_t)
       allocate (p_born (size (p_in)))
       if (k%threshold) then
          p_born = phs%get_onshell_projected_momenta ()
       else
          p_born = p_in
       end if
       if (.not. k%phs%is_cm_frame () .and. .not. k%threshold) then
            p_born = inverse (k%phs%lt_cm_to_lab) * p_born
       end if
       call phs%compute_xi_max (p_born, k%threshold)
       if (k%emitter >= 0) then
          allocate (p_real (size (p_born) + 1))
          allocate (p_out (size (p_born) + 1))
          if (k%emitter <= k%n_in) then
             call phs%generate_isr (k%i_phs, p_real)
          else
             if (k%threshold) then
                jac_rand_dummy = 1._default
                call compute_y_from_emitter (phs%generator%real_kinematics%x_rad (I_Y), &
                     phs%generator%real_kinematics%p_born_cms%get_momenta(1), &
                     k%n_in, k%emitter, .false., phs%generator%y_max, jac_rand_dummy, &
                     y_offshell)
                call phs%compute_xi_max (k%emitter, k%i_phs, y_offshell, &
                     phs%generator%real_kinematics%p_born_cms%get_momenta(1), &
                     xi_max_offshell)
                xi_offshell = xi_max_offshell * phs%generator%real_kinematics%xi_tilde
                phi = phs%generator%real_kinematics%phi
                call phs%generate_fsr (k%emitter, k%i_phs, p_real, &
                     xi_y_phi = [xi_offshell, y_offshell, phi], no_jacobians = .true.)
                call phs%generator%real_kinematics%p_real_cms%set_momenta (k%i_phs, p_real)
                call phs%generate_fsr_threshold (k%emitter, k%i_phs, p_real)
                if (debug2_active (D_SUBTRACTION)) &
                     call generate_fsr_threshold_for_other_emitters (k%emitter, k%i_phs)
             else if (k%i_con > 0) then
                call phs%generate_fsr (k%emitter, k%i_phs, p_real, k%i_con)
             else
                call phs%generate_fsr (k%emitter, k%i_phs, p_real)
             end if
          end if
          success = check_scalar_products (p_real)
          if (debug2_active (D_SUBTRACTION)) then
             call msg_debug2 (D_SUBTRACTION, "Real phase-space: ")
             call vector4_write_set (p_real)
          end if
          p_out = p_real
       else
          allocate (p_out (size (p_in))); p_out = p_in
          success = .true.
       end if
    end select
  contains
    subroutine generate_fsr_threshold_for_other_emitters (emitter, i_phs)
      integer, intent(in) :: emitter, i_phs
      integer :: ii_phs, this_emitter
      select type (phs => k%phs)
      type is (phs_fks_t)
         do ii_phs = 1, size (phs%phs_identifiers)
            this_emitter = phs%phs_identifiers(ii_phs)%emitter
            if (ii_phs /= i_phs .and. this_emitter /= emitter) &
                 call phs%generate_fsr_threshold (this_emitter, i_phs)
         end do
      end select
    end subroutine
  end subroutine kinematics_evaluate_radiation


end module kinematics
