! WHIZARD 2.6.0 Sep 08 2017
!
! Copyright (C) 1999-2017 by
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
  use system_dependencies, only: LHAPDF6_AVAILABLE
  use diagnostics
  use constants
  use physics_defs
  use pdg_arrays
  use sf_lhapdf
  use pdf
  use phs_fks, only: isr_kinematics_t

  use nlo_data

  implicit none
  private

  public :: dglap_remnant_t

  type :: dglap_remnant_t
    type(pdf_data_t) :: pdf_data
    type(isr_kinematics_t), pointer :: isr_kinematics => null ()
    integer, dimension(:), allocatable :: i_light_quarks
    integer, dimension(:,:), allocatable :: flv_in
    integer :: n_flv
    type(pdf_container_t), dimension(2) :: pdf_scaled
    type(pdf_container_t), dimension(2) :: pdf_born
    real(default), dimension(:), pointer :: sqme_born => null ()
  contains
    procedure :: init => dglap_remnant_init
    procedure :: init_pdfs => dglap_remnant_init_pdfs
    procedure :: compute_pdfs => dglap_remnant_compute_pdfs
    procedure :: get_gluon_pdf => dglap_remnant_get_gluon_pdf
    procedure :: get_quark_pdf => dglap_remnant_get_quark_pdf
    procedure :: get_summed_quark_pdf => dglap_remnant_get_summed_quark_pdf
    procedure :: evaluate => dglap_remnant_evaluate
    procedure :: final => dglap_remnant_final
  end type dglap_remnant_t


contains

  subroutine dglap_remnant_init (dglap, isr_kinematics, flv, n_alr)
    class(dglap_remnant_t), intent(inout) :: dglap
    type(isr_kinematics_t), intent(in), target :: isr_kinematics
    integer, dimension(:,:), intent(in) :: flv
    integer, intent(in) :: n_alr
    integer :: i, j, n_quarks
    logical, dimension(-6:6) :: quark_checked = .false.

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
    allocate (dglap%i_light_quarks (n_quarks))
    j = 1
    do i = -6, 6
       if (quark_checked(i)) then
          dglap%i_light_quarks(j) = i
          j = j + 1
       end if
    end do


    call dglap%init_pdfs ()
  end subroutine dglap_remnant_init

  subroutine dglap_remnant_init_pdfs (dglap)
    class(dglap_remnant_t), intent(inout) :: dglap
    type(string_t) :: lhapdf_dir, lhapdf_file
    integer :: lhapdf_member
    lhapdf_dir = ""
    lhapdf_file = ""
    lhapdf_member = 0
    if (LHAPDF6_AVAILABLE) then
       call lhapdf_initialize &
            (1, lhapdf_dir, lhapdf_file, lhapdf_member, dglap%pdf_data%pdf)
       associate (pdf_data => dglap%pdf_data)
          pdf_data%type = STRF_LHAPDF6
          pdf_data%xmin = pdf_data%pdf%getxmin ()
          pdf_data%xmax = pdf_data%pdf%getxmax ()
          pdf_data%qmin = sqrt(pdf_data%pdf%getq2min ())
          pdf_data%qmax = sqrt(pdf_data%pdf%getq2max ())
       end associate
    else
       call msg_fatal ("PDF subtraction: PDFs could not be initialized")
    end if
  end subroutine dglap_remnant_init_pdfs

  subroutine dglap_remnant_compute_pdfs (dglap)
    class(dglap_remnant_t), intent(inout) :: dglap
    integer :: i
    real(default) :: z, x, Q
    real(double), dimension(-6:6) :: f_dble = 0._double
    Q = dglap%isr_kinematics%fac_scale
    do i = 1, 2
       x = dglap%isr_kinematics%x(i)
       z = dglap%isr_kinematics%z(i)
       call dglap%pdf_data%evolve (dble(x), dble(Q), f_dble)
       dglap%pdf_born(i)%f = f_dble / dble(x)
       call dglap%pdf_data%evolve (dble(x / z), dble(Q), f_dble)
       dglap%pdf_scaled(i)%f = f_dble / dble(x / z)
    end do
  end subroutine dglap_remnant_compute_pdfs

  function dglap_remnant_get_gluon_pdf (dglap, em, scaled) result (pdf)
    class(dglap_remnant_t), intent(in) :: dglap
    integer, intent(in) :: em
    logical, intent(in) :: scaled
    real(default) :: pdf
    if (scaled) then
       pdf = dglap%pdf_scaled(em)%f(0)
    else
       pdf = dglap%pdf_born(em)%f(0)
    end if
  end function dglap_remnant_get_gluon_pdf

  function dglap_remnant_get_quark_pdf (dglap, em, i, scaled) result (pdf)
    class(dglap_remnant_t), intent(in) :: dglap
    integer, intent(in) :: em, i
    logical, intent(in) :: scaled
    real(default) :: pdf
    if (scaled) then
       pdf = dglap%pdf_scaled(em)%f(i)
    else
       pdf = dglap%pdf_born(em)%f(i)
    end if
  end function dglap_remnant_get_quark_pdf

  function dglap_remnant_get_summed_quark_pdf (dglap, em) result (pdf)
    class(dglap_remnant_t), intent(in) :: dglap
    integer, intent(in) :: em
    real(default) :: pdf
    integer :: i_quark
    pdf = 0._default
    do i_quark = -6, 6
       if (any(i_quark == dglap%i_light_quarks)) &
          pdf = pdf + dglap%get_quark_pdf(em, i_quark, scaled = .true.)
    end do
  end function dglap_remnant_get_summed_quark_pdf

  subroutine dglap_remnant_evaluate (dglap, alpha_s, sqme_born, &
         separate_alrs, sqme_dglap)
    class(dglap_remnant_t), intent(inout) :: dglap
    real(default), intent(in) :: alpha_s
    real(default), intent(in), dimension(:) :: sqme_born
    logical, intent(in) :: separate_alrs
    real(default), intent(inout), dimension(:) :: sqme_dglap
    real(default) :: factor, factor_soft, plus_dist_remnant
    real(default) :: pdfs, pdfb
    integer :: i_flv, ii_flv, emitter
    real(default), dimension(2) :: tmp
    real(default) :: sb, xb, onemz
    real(default) :: fac_scale2, jac

    sb = dglap%isr_kinematics%sqrts_born**2
    fac_scale2 = dglap%isr_kinematics%fac_scale**2

    call dglap%compute_pdfs ()

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
                pdfs = dglap%get_gluon_pdf (emitter, scaled = .true.)
                pdfb = dglap%get_gluon_pdf (emitter, scaled = .false.)
                tmp(emitter) = p_hat_gg(z) * factor / z * pdfs / pdfb * jac &
                     - p_hat_gg(one) * factor_soft * jac &
                     + p_hat_gg(one) * plus_dist_remnant
                pdfs = dglap%get_summed_quark_pdf (emitter)
                tmp(emitter) = tmp(emitter) + &
                     (p_hat_qg(z) * factor - p_derived_qg(z)) / z * pdfs / pdfb * jac
             else if (is_quark(dglap%flv_in(emitter, i_flv))) then
                pdfs = dglap%get_quark_pdf (emitter, dglap%flv_in(emitter, i_flv), scaled = .true.)
                pdfb = dglap%get_quark_pdf (emitter, dglap%flv_in(emitter, i_flv), scaled = .false.)
                if (vanishes (pdfb)) then
                   sqme_dglap = zero
                   return
                end if
                tmp(emitter) = p_hat_qq(z) * factor / z * pdfs / pdfb * jac &
                     - p_derived_qq(z) / z * pdfs / pdfb * jac &
                     - p_hat_qq(one) * factor_soft * jac &
                     + p_hat_qq(one) * plus_dist_remnant
                pdfs = dglap%get_gluon_pdf (emitter, scaled = .true.)
                tmp(emitter) = tmp(emitter) + &
                     (p_hat_gq(z) * factor - p_derived_gq(z)) / z * pdfs / pdfb * jac
             end if
          end associate
       end do
       sqme_dglap(ii_flv) = sqme_dglap(ii_flv) + alpha_s / twopi * (tmp(1) + tmp(2)) * sqme_born(i_flv)
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
    if (allocated (dglap%i_light_quarks)) deallocate (dglap%i_light_quarks)
    if (associated (dglap%sqme_born)) deallocate (dglap%sqme_born)
  end subroutine dglap_remnant_final


end module dglap_remnant

