! WHIZARD 2.6.3 Feb 10 2018
!
! Copyright (C) 1999-2018 by
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

module dglap_remnant

  use kinds, only: default, double
  use iso_varying_string, string_t => varying_string
  use numeric_utils
  use diagnostics
  use constants
  use physics_defs
  use pdg_arrays
  use sf_base, only: rescaling_function_t
  use phs_fks, only: isr_kinematics_t

  use nlo_data

  implicit none
  private

  public :: rescale_dglap_t
  public :: dglap_remnant_t

  type, extends(rescaling_function_t) :: rescale_dglap_t
     real(default), dimension(:), allocatable :: z
   contains
     procedure :: init_indices => rescale_dglap_init_indices
     procedure :: apply => rescale_dglap_apply
     procedure :: set => rescale_dglap_set
  end type rescale_dglap_t

  type :: dglap_remnant_t
    type(isr_kinematics_t), pointer :: isr_kinematics => null ()
    integer, dimension(:), allocatable :: light_quark_flv
    integer, dimension(:,:), allocatable :: flv_in
    real(default), dimension(:), allocatable :: sqme_born
    real(default), dimension(:,:,:), allocatable :: sqme_coll_isr
    integer :: n_flv
  contains
    procedure :: init => dglap_remnant_init
    procedure :: get_summed_quark_sqmes => dglap_remnant_get_summed_quark_sqmes
    procedure :: evaluate => dglap_remnant_evaluate
    procedure :: final => dglap_remnant_final
  end type dglap_remnant_t


contains

  subroutine rescale_dglap_init_indices (func)
    class(rescale_dglap_t), intent(inout) :: func
    integer :: i
    call msg_debug2 (D_BEAMS, "recale_dglap_init_indices")
    allocate (func%sf_indices(3, 13))
    allocate (func%sf_indices_gluon(2, 13))
    func%sf_indices(1,:) = [(i, i = 1, 13)]
    func%sf_indices(2,:) = [(13 + 4 * i - 3, i = 1, 13)]
    func%sf_indices(3,:) = [(13 + 4 * i - 2, i = 1, 13)]
    func%sf_indices_gluon(1,:) = [(13 + 4 * i - 1, i = 1 ,13)]
    func%sf_indices_gluon(2,:) = [(13 + 4 * i, i = 1 ,13)]
    if (debug2_active (D_BEAMS)) then
       print *, "Beam 1:"
       print *, " quark indices: ", func%sf_indices(2, :)
       print *, " gluon indices: ", func%sf_indices_gluon(1, :)
       print *, "Beam 2:"
       print *, " quark indices: ", func%sf_indices(3, :)
       print *, " gluon indices: ", func%sf_indices_gluon(2, :)
    end if
  end subroutine rescale_dglap_init_indices

  subroutine rescale_dglap_apply (func, x)
    class(rescale_dglap_t), intent(in) :: func
    real(default), intent(inout) :: x
    if (debug2_active (D_BEAMS))  then
       print *, "Rescaling function - DGLAP:"
       print *, "Input: ", x
       print *, "Beam index: ", func%i_beam
       print *, "z: ", func%z
    end if
    x = x / func%z(func%i_beam)
    if (debug2_active (D_BEAMS)) print *, "scaled x: ", x
  end subroutine rescale_dglap_apply

  subroutine rescale_dglap_set (func, z)
    class(rescale_dglap_t), intent(inout) :: func
    real(default), dimension(:), intent(in) :: z
    ! allocate-on-assginment
    func%z = z
  end subroutine rescale_dglap_set

  subroutine dglap_remnant_init (dglap, n_flv_born, isr_kinematics, flv, n_alr)
    class(dglap_remnant_t), intent(inout) :: dglap
    integer, intent(in) :: n_flv_born
    type(isr_kinematics_t), intent(in), target :: isr_kinematics
    integer, dimension(:,:), intent(in) :: flv
    integer, intent(in) :: n_alr
    integer :: i, j, n_quarks
    logical, dimension(-6:6) :: quark_checked = .false.

    allocate (dglap%sqme_born(n_flv_born))
    dglap%sqme_born = zero
    allocate (dglap%sqme_coll_isr(2, 2, n_flv_born))
    dglap%sqme_coll_isr = zero

    dglap%isr_kinematics => isr_kinematics
    dglap%n_flv = size (flv, dim=2)
    allocate (dglap%flv_in (2, dglap%n_flv))
    dglap%flv_in = flv
    n_quarks = 0
    do i = 1, size (flv, dim = 1)
       if (is_quark(flv(i,1))) then
          n_quarks = n_quarks + 1
          quark_checked(flv(i, 1)) = .true.
       end if
    end do
    allocate (dglap%light_quark_flv (n_quarks))
    j = 1
    do i = -6, 6
       if (quark_checked(i)) then
          dglap%light_quark_flv(j) = i
          j = j + 1
       end if
    end do
  end subroutine dglap_remnant_init

  function dglap_remnant_get_summed_quark_sqmes (dglap, emitter) result (sum_sqme)
    real(default) :: sum_sqme
    class(dglap_remnant_t), intent(in) :: dglap
    integer, intent(in) :: emitter
    integer :: i_flv
    sum_sqme = zero
    do i_flv = 1, size (dglap%sqme_coll_isr, dim=3)
       if (any (dglap%flv_in(emitter, i_flv) == dglap%light_quark_flv)) &
            sum_sqme = sum_sqme + dglap%sqme_coll_isr (emitter, 1, i_flv)
    end do
  end function dglap_remnant_get_summed_quark_sqmes

  subroutine dglap_remnant_evaluate (dglap, alpha_s, separate_alrs, sqme_dglap)
    class(dglap_remnant_t), intent(inout) :: dglap
    real(default), intent(in) :: alpha_s
    logical, intent(in) :: separate_alrs
    real(default), intent(inout), dimension(:) :: sqme_dglap
    real(default) :: factor, factor_soft, plus_dist_remnant
    integer :: i_flv, ii_flv, emitter
    real(default), dimension(2) :: tmp
    real(default) :: sb, xb, onemz
    real(default) :: fac_scale2, jac
    real(default) :: sqme_scaled

    sb = dglap%isr_kinematics%sqrts_born**2
    fac_scale2 = dglap%isr_kinematics%fac_scale**2

    do i_flv = 1, dglap%n_flv
       if (separate_alrs) then
          ii_flv = i_flv
       else
          ii_flv = 1
       end if

       tmp = zero
       do emitter = 1, 2
          associate (z => dglap%isr_kinematics%z(emitter))
             jac = dglap%isr_kinematics%jacobian(emitter)
             onemz = one - z
             factor = log(sb / z / fac_scale2) / onemz + two * log(onemz) / onemz
             factor_soft = log(sb / fac_scale2) / onemz + two * log(onemz) / onemz

             xb = dglap%isr_kinematics%x(emitter)
             plus_dist_remnant = log(one - xb) * log(sb / fac_scale2) + log(one - xb)**2

             if (is_gluon(dglap%flv_in(emitter, i_flv))) then
                sqme_scaled = dglap%sqme_coll_isr(emitter, 2, i_flv)
                tmp(emitter) = p_hat_gg(z) * factor / z * sqme_scaled * jac &
                     - p_hat_gg(one) * factor_soft * dglap%sqme_born(i_flv) * jac &
                     + p_hat_gg(one) * plus_dist_remnant * dglap%sqme_born(i_flv)

                tmp(emitter) = tmp(emitter) + &
                     (p_hat_qg(z) * factor - p_derived_qg(z)) / z * jac * &
                     dglap%get_summed_quark_sqmes (emitter)
             else if (is_quark(dglap%flv_in(emitter, i_flv))) then
                sqme_scaled = dglap%sqme_coll_isr(emitter, 1, i_flv)
                tmp(emitter) = p_hat_qq(z) * factor / z * sqme_scaled * jac &
                     - p_derived_qq(z) / z * sqme_scaled * jac &
                     - p_hat_qq(one) * factor_soft * dglap%sqme_born(i_flv) * jac &
                     + p_hat_qq(one) * plus_dist_remnant * dglap%sqme_born(i_flv)

                sqme_scaled = dglap%sqme_coll_isr(emitter, 2, i_flv)
                tmp(emitter) = tmp(emitter) + &
                     (p_hat_gq(z) * factor - p_derived_gq(z)) / z * sqme_scaled * jac

             end if
          end associate
       end do
       sqme_dglap(ii_flv) = sqme_dglap(ii_flv) + alpha_s / twopi * (tmp(1) + tmp(2))
    end do
  end subroutine dglap_remnant_evaluate

  function p_hat_gg (z)
    real(default) :: p_hat_gg
    real(default), intent(in) :: z
    real(default) :: onemz
    onemz = one - z

    p_hat_gg = two * CA * (z + onemz**2 / z + z * onemz**2)
  end function p_hat_gg

  function p_hat_qg (z)
    real(default) :: p_hat_qg
    real(default), intent(in) :: z
    real(default) :: onemz
    onemz = one - z

    p_hat_qg = CF * onemz / z * (one + onemz**2)
  end function p_hat_qg

  function p_hat_gq (z)
    real(default) :: p_hat_gq
    real(default), intent(in) :: z
    real(default) :: onemz
    onemz = one - z

    p_hat_gq = TR * (onemz - two * z * onemz**2)
  end function p_hat_gq

  function p_hat_qq (z)
    real(default) :: p_hat_qq
    real(default), intent(in) :: z
    p_hat_qq = CF * (one + z**2)
  end function p_hat_qq

  function p_derived_gg (z)
    real(default) :: p_derived_gg
    real(default), intent(in) :: z
    p_derived_gg = zero
  end function p_derived_gg

  function p_derived_qg (z)
    real(default) :: p_derived_qg
    real(default), intent(in) :: z
    p_derived_qg = -CF * z
  end function p_derived_qg

  function p_derived_gq (z)
    real(default) :: p_derived_gq
    real(default), intent(in) :: z
    real(default) :: onemz
    onemz = one - z

    p_derived_gq = -two * TR * z * onemz
  end function p_derived_gq

  function p_derived_qq (z)
    real(default) :: p_derived_qq
    real(default), intent(in) :: z
    real(default) :: onemz
    onemz = one - z

    p_derived_qq = -CF * onemz
  end function p_derived_qq

  subroutine dglap_remnant_final (dglap)
    class(dglap_remnant_t), intent(inout) :: dglap
    if (associated (dglap%isr_kinematics)) nullify (dglap%isr_kinematics)
    if (allocated (dglap%light_quark_flv)) deallocate (dglap%light_quark_flv)
    if (allocated (dglap%sqme_born)) deallocate (dglap%sqme_born)
    if (allocated (dglap%sqme_coll_isr)) deallocate (dglap%sqme_coll_isr)
  end subroutine dglap_remnant_final


end module dglap_remnant

