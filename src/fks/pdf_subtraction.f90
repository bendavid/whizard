! WHIZARD 2.2.8 Nov 22 2015
! 
! Copyright (C) 1999-2015 by 
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

module pdf_subtraction

  use kinds, only: default, double
  use iso_varying_string, string_t => varying_string
  use system_dependencies, only: LHAPDF6_AVAILABLE
  use diagnostics
  use constants
  use physics_defs
  use pdg_arrays
  use sf_lhapdf
  use pdf
  use nlo_data

  implicit none
  private

  public :: pdf_subtraction_t

  type :: pdf_subtraction_t
    type(pdf_data_t) :: pdf_data
    logical :: required = .false.
    type(isr_kinematics_t), pointer :: isr_kinematics => null ()
    integer, dimension(:), allocatable :: i_light_quarks
    integer, dimension(2) :: flv_in
    type(pdf_container_t), dimension(2) :: pdf_scaled
    type(pdf_container_t), dimension(2) :: pdf_born
    real(default), dimension(:), pointer :: sqme_born => null ()
    real(default), dimension(:), allocatable :: value
  contains
    procedure :: init => pdf_subtraction_init
    procedure :: set_incoming_flavor => pdf_subtraction_set_incoming_flavor
    procedure :: init_pdfs => pdf_subtraction_init_pdfs
    procedure :: compute_pdfs => pdf_subtraction_compute_pdfs
    procedure :: get_gluon_pdf => pdf_subtraction_get_gluon_pdf
    procedure :: get_quark_pdf => pdf_subtraction_get_quark_pdf
    procedure :: get_summed_quark_pdf => pdf_subtraction_get_summed_quark_pdf
    procedure :: evaluate => pdf_subtraction_evaluate
  end type pdf_subtraction_t


contains

  subroutine pdf_subtraction_init (pdf_sub, isr_kinematics, flv, n_alr, sqme_collector)
    class(pdf_subtraction_t), intent(inout) :: pdf_sub
    type(isr_kinematics_t), intent(in), target :: isr_kinematics
    integer, dimension(:,:), intent(in) :: flv
    integer, intent(in) :: n_alr
    type(sqme_collector_t), intent(in), target :: sqme_collector
    integer :: i, j, n_quarks
    logical, dimension(-6:6) :: quark_checked = .false.
    pdf_sub%required = any ([is_quark(flv(1,1)), &
        is_quark(flv(2,1)), is_gluon(flv(1,1)), is_gluon(flv(2,1))])
    if (.not. pdf_sub%required) return

    pdf_sub%sqme_born => sqme_collector%sqme_born_list
    pdf_sub%isr_kinematics => isr_kinematics
    allocate (pdf_sub%value (n_alr))
    call pdf_sub%set_incoming_flavor (flv(1,1), flv(2,1))
    n_quarks = 0
    do i = 1, size (flv, dim=1 )
       if (is_quark(flv(i,1))) then
          n_quarks = n_quarks+1
          quark_checked(flv(i,1)) = .true.
       end if
    end do
    allocate (pdf_sub%i_light_quarks (n_quarks))
    j = 1
    do i = -6, 6
       if (quark_checked(i)) then
          pdf_sub%i_light_quarks(j) = i
          j = j+1
       end if
    end do

    call pdf_sub%init_pdfs ()
  end subroutine pdf_subtraction_init

  subroutine pdf_subtraction_set_incoming_flavor (pdf_sub, flv1, flv2)
    class(pdf_subtraction_t), intent(inout) :: pdf_sub
    integer, intent(in) :: flv1, flv2
    pdf_sub%flv_in(1) = flv1; pdf_sub%flv_in(2) = flv2
  end subroutine pdf_subtraction_set_incoming_flavor

  subroutine pdf_subtraction_init_pdfs (pdf_sub)
    class(pdf_subtraction_t), intent(inout) :: pdf_sub
    type(string_t) :: lhapdf_dir, lhapdf_file
    integer :: lhapdf_member
    lhapdf_dir = ""
    lhapdf_file = ""
    lhapdf_member = 0
    if (LHAPDF6_AVAILABLE) then
       call lhapdf_initialize &
            (1, lhapdf_dir, lhapdf_file, lhapdf_member, pdf_sub%pdf_data%pdf)
       associate (pdf_data => pdf_sub%pdf_data)
          pdf_data%type = STRF_LHAPDF6
          pdf_data%xmin = pdf_data%pdf%getxmin ()
          pdf_data%xmax = pdf_data%pdf%getxmax ()
          pdf_data%qmin = sqrt(pdf_data%pdf%getq2min ())
          pdf_data%qmax = sqrt(pdf_data%pdf%getq2max ())
       end associate
    else
       call msg_fatal ("PDF subtraction: PDFs could not be initialized")
    end if
  end subroutine pdf_subtraction_init_pdfs

  subroutine pdf_subtraction_compute_pdfs (pdf_sub)
    class(pdf_subtraction_t), intent(inout) :: pdf_sub
    integer :: i
    real(default) :: z, x, Q
    real(double), dimension(-6:6) :: f_dble = 0._double
    do i = 1, 2
       x = pdf_sub%isr_kinematics%x(i)
       z = pdf_sub%isr_kinematics%z(i)
       Q = pdf_sub%isr_kinematics%fac_scale
       call pdf_sub%pdf_data%evolve (dble(x), dble(Q), f_dble)
       pdf_sub%pdf_born(i)%f = f_dble
       call pdf_sub%pdf_data%evolve (dble(x/z), dble(Q), f_dble)
       pdf_sub%pdf_scaled(i)%f = f_dble
    end do
  end subroutine pdf_subtraction_compute_pdfs

  function pdf_subtraction_get_gluon_pdf (pdf_sub, em, scaled) result (pdf)
    class(pdf_subtraction_t), intent(in) :: pdf_sub
    integer, intent(in) :: em
    logical, intent(in) :: scaled
    real(default) :: pdf
    if (scaled) then
       pdf = pdf_sub%pdf_scaled(em)%f(0)
    else
       pdf = pdf_sub%pdf_born(em)%f(0)
    end if
  end function pdf_subtraction_get_gluon_pdf

  function pdf_subtraction_get_quark_pdf (pdf_sub, em, i, scaled) result (pdf)
    class(pdf_subtraction_t), intent(in) :: pdf_sub
    integer, intent(in) :: em, i
    logical, intent(in) :: scaled
    real(default) :: pdf
    if (scaled) then
       pdf = pdf_sub%pdf_scaled(em)%f(i)
    else
       pdf = pdf_sub%pdf_born(em)%f(i)
    end if
  end function pdf_subtraction_get_quark_pdf

  function pdf_subtraction_get_summed_quark_pdf (pdf_sub, em) result (pdf)
    class(pdf_subtraction_t), intent(in) :: pdf_sub
    integer, intent(in) :: em
    real(default) :: pdf
    integer :: i_quark
    pdf = 0._default
    do i_quark = -6, 6
       if (any(i_quark == pdf_sub%i_light_quarks)) &
          pdf = pdf + pdf_sub%get_quark_pdf(em, i_quark, scaled = .true.)
    end do
  end function pdf_subtraction_get_summed_quark_pdf

  subroutine pdf_subtraction_evaluate (pdf_sub, alpha_s, sqme_born, alr)
    class(pdf_subtraction_t), intent(inout) :: pdf_sub
    real(default), intent(in) :: alpha_s
    real(default), intent(inout) :: sqme_born
    integer, intent(in) :: alr
    real(default) :: factor, factor_soft, remnant
    real(default) :: pdfs, pdfb
    integer :: emitter
    real(default), dimension(2) :: tmp
    real(default) :: sb, xb, onemz
    real(default) :: fac_scale2, jac

    pdf_sub%value = 0._default
    sb = pdf_sub%isr_kinematics%sqrts_born**2
    tmp = 0._default
    fac_scale2 = pdf_sub%isr_kinematics%fac_scale**2

    call pdf_sub%compute_pdfs ()

    do emitter = 1, 2
       associate (z => pdf_sub%isr_kinematics%z(emitter))
          jac = pdf_sub%isr_kinematics%jacobian(emitter)
          onemz = one - z
          factor = log(sb/z/fac_scale2)/onemz + 2*log(onemz)/onemz
          factor_soft = log(sb/fac_scale2)/onemz + 2*log(onemz)/onemz

          xb = pdf_sub%isr_kinematics%x(emitter)
          remnant = log(1-xb)*log(sb/fac_scale2) + log(1-xb)**2

          if (is_gluon(pdf_sub%flv_in(emitter))) then
             pdfs = pdf_sub%get_gluon_pdf (emitter, scaled = .true.)
             pdfb = pdf_sub%get_gluon_pdf (emitter, scaled = .false.)
             tmp(emitter) = p_hat_gg(z) * factor/z * pdfs/pdfb * jac &
                  - p_hat_gg(one) * factor_soft * jac &
                  + p_hat_gg(one) * remnant
             pdfs = pdf_sub%get_summed_quark_pdf (emitter)
             tmp(emitter) = tmp(emitter) + (p_hat_qg(z)*factor - p_derived_qg(z))/z * pdfs/pdfb * jac
          else if (is_quark(abs(pdf_sub%flv_in(emitter)))) then
             pdfs = pdf_sub%get_quark_pdf (emitter, pdf_sub%flv_in(emitter), scaled = .true.)
             pdfb = pdf_sub%get_quark_pdf (emitter, pdf_sub%flv_in(emitter), scaled = .false.)
             if (pdfb == 0._default) then
                sqme_born = 0._default
                return
             end if
             tmp(emitter) = p_hat_qq(z) * factor/z * pdfs/pdfb * jac &
                  - p_derived_qq(z)/z * pdfs/pdfb * jac &
                  - p_hat_qq(one) * factor_soft * jac &
                  + p_hat_qq(one) * remnant
             pdfs = pdf_sub%get_gluon_pdf (emitter, scaled = .true.)
             tmp(emitter) = tmp(emitter) + (p_hat_gq(z)*factor - p_derived_gq(z))/z * pdfs/pdfb * jac
          end if
       end associate
    end do
    sqme_born = alpha_s/twopi * (tmp(1)+tmp(2)) * sqme_born
  end subroutine pdf_subtraction_evaluate

  function p_hat_gg (z)
    real(default) :: p_hat_gg
    real(default), intent(in) :: z
    real(default) :: onemz
    onemz = one - z
    p_hat_gg = 2*CA*(z + onemz**2/z + z*onemz**2)
  end function p_hat_gg

  function p_hat_qg (z)
    real(default) :: p_hat_qg
    real(default), intent(in) :: z
    real(default) :: onemz
    onemz = one - z
    p_hat_qg = CF * onemz/z * (one+onemz**2)
  end function p_hat_qg

  function p_hat_gq (z)
    real(default) :: p_hat_gq
    real(default), intent(in) :: z
    real(default) :: onemz
    onemz = one - z
    p_hat_gq = TR*(onemz - 2*z*onemz**2)
  end function p_hat_gq

  function p_hat_qq (z)
    real(default) :: p_hat_qq
    real(default), intent(in) :: z
    real(default) :: onemz
    onemz = one - z
    p_hat_qq = CF*(one+z**2)
  end function p_hat_qq

  function p_derived_gg (z)
    real(default) :: p_derived_gg
    real(default), intent(in) :: z
    real(default) :: onemz
    onemz = one - z
    p_derived_gg = 0._default
  end function p_derived_gg

  function p_derived_qg (z)
    real(default) :: p_derived_qg
    real(default), intent(in) :: z
    real(default) :: onemz
    onemz = one - z
    p_derived_qg = -CF*z
  end function p_derived_qg

  function p_derived_gq (z)
    real(default) :: p_derived_gq
    real(default), intent(in) :: z
    real(default) :: onemz
    onemz = one - z
    p_derived_gq = -2*TR*z*onemz
  end function p_derived_gq

  function p_derived_qq (z)
    real(default) :: p_derived_qq
    real(default), intent(in) :: z
    real(default) :: onemz
    onemz = one - z
    p_derived_qq = -CF*onemz
  end function p_derived_qq


end module pdf_subtraction

