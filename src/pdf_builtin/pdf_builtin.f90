!$Id: pdf_builtin.f90 2965 2011-01-17 16:31:16Z cnspeckn $

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Copyright (C) 1999-2011 by 
!     Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
!     Christian Speckner <christian.speckner@physik.uni-freiburg.de>
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

! Wrap a common interface around the different PDF sets.

module pdf_builtin
use kinds, only: default, double
use diagnostics
use iso_varying_string, string_t => varying_string
use file_utils
use mrst2004qed
use cteq6pdf
use mstw2008
use ct10pdf
implicit none
save
private

! The available sets
integer, parameter :: nsets = 10
integer, parameter, public :: &
   CTEQ6M = 1, CTEQ6D = 2, CTEQ6L = 3, CTEQ6L1 = 4, &
   MRST2004QEDp = 5, MRST2004QEDn = 6, MSTW2008LO = 7, MSTW2008NLO = 8, &
   MSTW2008NNLO = 9, CT10 = 10

! Limits
real(kind=default), parameter :: &
   cteq6_q_min = 1.3, cteq6_q_max = 10.E3, &
   cteq6_x_min = 1.E-6, cteq6_x_max = 1., &
   mrst2004qed_q_min = sqrt (1.26_default), mrst2004qed_q_max = sqrt (0.99E7_default), &
   mrst2004qed_x_min = 1.01E-5, mrst2004qed_x_max = 1., &
   mstw2008_q_min = 1.01, mstw2008_q_max = sqrt (0.99E9_default), &
   mstw2008_x_min = 1.01E-6, mstw2008_x_max = 1., &
   ct10_q_min = 1.3, ct10_q_max = 1.E5, &
   ct10_x_min = 1.E-8, ct10_x_max = 1.

! Init flags
integer :: cteq6_initialized = -1
integer :: mstw2008_initialized = -1
logical :: ct10_initialized = .false.
logical :: &
   mrst2004qedp_initialized =  .false., &
   mrst2004qedn_initialized =  .false.
type(string_t) :: mrst2004qedp_prefix, mrst2004qedn_prefix, &
   mstw2008_prefix

! Public stuff
public :: pdf_init, pdf_get_name, pdf_evolve, &
   pdf_provides_photon, pdf_get_id

contains

! Get PDF name
function pdf_get_name (pdftype) result (name)
integer, intent(in) :: pdftype
type(string_t) :: name
select case (pdftype)
   case (CTEQ6M)
      name = var_str ("CTEQ6M")
   case (CTEQ6D)
      name = var_str ("CTEQ6D")
   case (CTEQ6L)
      name = var_str ("CTEQ6L")
   case (CTEQ6L1)
      name = var_str ("CTEQ6L1")
   case (MRST2004QEDp)
      name = var_str ("MRST2004QEDp")
   case (MRST2004QEDn)
      name = var_str ("MRST2004QEDn")
   case (MSTW2008LO)
      name = var_str ("MSTW2008LO")
   case (MSTW2008NLO)
      name = var_str ("MSTW2008NLO")
   case (MSTW2008NNLO)
      name = var_str ("MSTW2008NNLO")
   case (CT10)
      name = var_str ("CT10")
   case default
      call msg_fatal ("pdf_builtin: internal: invalid PDF set!")
end select
end function pdf_get_name

! Get the ID of a PDF set
function pdf_get_id (name) result (id)
type(string_t), intent(in) :: name
integer :: id
do id = 1, nsets
   if (upper_case (pdf_get_name (id)) == upper_case (name)) return
end do
id = -1
end function pdf_get_id

! Query whether a PDF supplies a photon distribution
function pdf_provides_photon (pdftype) result (flag)
integer, intent(in) :: pdftype
logical :: flag
select case (pdftype)
   case (CTEQ6M, CTEQ6D, CTEQ6L, CTEQ6L1)
      flag = .false.
   case (MRST2004QEDp, MRST2004QEDn)
      flag = .true.
   case (MSTW2008LO, MSTW2008NLO, MSTW2008NNLO)
      flag = .false.
   case (CT10)
      flag = .false.
   case default
      call msg_fatal ("pdf_builtin: internal: invalid PDF set!")
end select
end function pdf_provides_photon

! Initialize a PDF
subroutine pdf_init (pdftype, prefix, verbose)
integer, intent(in) :: pdftype
type(string_t), intent(in), optional :: prefix
type(string_t) :: mprefix
logical, intent(in), optional :: verbose
logical :: mverbose
if (present (prefix)) then
   mprefix = prefix
else
   mprefix = ""
end if
if (present (verbose)) then
   mverbose = verbose
else
   mverbose = .true.
end if
select case (pdftype)
   case (CTEQ6M, CTEQ6D, CTEQ6L, CTEQ6L1)
      if (cteq6_initialized == pdftype) return
      call setctq6 (pdftype, char (mprefix))
      cteq6_initialized = pdftype
   case (MRST2004QEDp)
      if (mrst2004qedp_initialized) return
      mrst2004qedp_initialized = .true.
      mrst2004qedp_prefix = mprefix
   case (MRST2004QEDn)
      if (mrst2004qedn_initialized) return
      mrst2004qedn_initialized = .true.
      mrst2004qedn_prefix = mprefix
   case (MSTW2008LO, MSTW2008NLO, MSTW2008NNLO)
      if (mstw2008_initialized == pdftype) return
      mstw2008_initialized = pdftype
      mstw2008_prefix = mprefix
   case (CT10)
      if (ct10_initialized) return
      call setct10 (char (mprefix), 100)
      ct10_initialized = .true.
   case default
      call msg_fatal ("pdf_builtin: internal: invalid PDF set!")
end select
if (mverbose) call msg_message ("Initialized builtin PDF " // &
      char (pdf_get_name (pdftype)))
end subroutine pdf_init

! Evolve PDF
subroutine pdf_evolve (pdftype, x, q, f, fphoton)
integer, intent(in) :: pdftype
real(kind=default), intent(in) :: x, q
real(kind=default), intent(out), optional :: f(-6:6), fphoton
real(kind=default) :: mx, mq
real(kind=default) :: upv, dnv, ups, dns, str, chm, bot, glu, phot, &
   sbar, bbar, cbar
type(string_t) :: setname, prefix
select case (pdftype)
   case (CTEQ6M, CTEQ6D, CTEQ6L, CTEQ6L1)
      if (cteq6_initialized < 0) &
         call msg_fatal ("pdf_builtin: internal: PDF set " // &
            char (pdf_get_name (pdftype)) // " requested without initialization!")
      if (cteq6_initialized /= pdftype) &
         call msg_fatal ( &
            "PDF sets " // char (pdf_get_name (pdftype)) // " and " // &
               char (pdf_get_name (cteq6_initialized)) // &
               " cannot be used simultaneously")
      mx = max (min (x, cteq6_x_max), cteq6_x_min)
      mq = max (min (q, cteq6_q_max), cteq6_q_min)
      if (present (f)) f = (/ 0._default, &
                ctq6pdf (-5, mx, mq), ctq6pdf (-4, mx, mq), &
                ctq6pdf (-3, mx, mq), ctq6pdf (-1, mx, mq), &
                ctq6pdf (-2, mx, mq), ctq6pdf ( 0, mx, mq), &
                ctq6pdf ( 2, mx, mq), ctq6pdf ( 1, mx, mq), &
                ctq6pdf ( 3, mx, mq), ctq6pdf ( 4, mx, mq), &
                ctq6pdf ( 5, mx, mq), 0._default /)
      if (present (fphoton)) call msg_fatal ("photon pdf requested for " // &
         char (pdf_get_name (pdftype)) // " which does not provide it!")
   case (MRST2004QEDp)
      if (.not. mrst2004qedp_initialized) call msg_fatal ( &
         "pdf_builtin: internal: PDF set MRST2004QEDp requested without " // &
         "initialization!")
      mx = max (min (x, mrst2004qed_x_max), mrst2004qed_x_min)
      mq = max (min (q, mrst2004qed_q_max), mrst2004qed_q_min)
      call mrstqed (mx, mq, 1, upv, dnv, ups, dns, str, chm, bot, glu, phot, &
         char (mrst2004qedp_prefix))
      if (present (f)) f = &
         (/ 0._default, bot, chm, str, ups, dns, glu, dns + dnv, ups + upv, &
            str, chm, bot, 0._default /) / mx
      if (present (fphoton)) fphoton = phot / mx
   case (MRST2004QEDn)
      if (.not. mrst2004qedn_initialized) call msg_fatal ( &
         "pdf_builtin: internal: PDF set MRST2004QEDn requested without " // &
         "initialization!")
      mx = max (min (x, mrst2004qed_x_max), mrst2004qed_x_min)
      mq = max (min (q, mrst2004qed_q_max), mrst2004qed_q_min)
      call mrstqed (mx, mq, 2, upv, dnv, ups, dns, str, chm, bot, glu, phot, &
         char (mrst2004qedn_prefix))
      if (present (f)) f = &
         (/ 0._default, bot, chm, str, ups, dns, glu, dns + dnv, ups + upv, &
            str, chm, bot, 0._default /) / mx
      if (present (fphoton)) fphoton = phot / mx
   case (MSTW2008LO, MSTW2008NLO, MSTW2008NNLO)
      if (mstw2008_initialized < 0) &
         call msg_fatal ("pdf_builtin: internal: PDF set " // &
            char (pdf_get_name (pdftype)) // " requested without initialization!")
      if (mstw2008_initialized /= pdftype) &
         call msg_fatal ( &
            "PDF sets " // char (pdf_get_name (pdftype)) // " and " // &
               char (pdf_get_name (mstw2008_initialized)) // &
               " cannot be used simultaneously")
      select case (pdftype)
         case (MSTW2008LO)
            setname = var_str ("mstw2008lo")
         case (MSTW2008NLO)
            setname = var_str ("mstw2008nlo")
         case (MSTW2008NNLO)
            setname = var_str ("mstw2008nnlo")
      end select
      mx = max (min (x, mstw2008_x_max), mstw2008_x_min)
      mq = max (min (q, mstw2008_q_max), mstw2008_q_min)
      call getmstw2008 (char (mstw2008_prefix), char (setname), 0, mx, mq, &
         upv, dnv, ups, dns, str, sbar, chm, cbar, bot, bbar, glu, phot)
      if (present (f)) f = &
         (/ 0._default, bbar, cbar, sbar, ups, dns, glu, dns + dnv, ups + upv, &
            str, chm, bot, 0._default /) / mx
      if (present (fphoton)) call msg_fatal ("photon pdf requested for " // &
         char (pdf_get_name (pdftype)) // " which does not provide it!")
   case (CT10)
      if (.not. ct10_initialized) call msg_fatal ( &
         "pdf_builtin: internal: PDF set CT10 requested without " // &
         "initialization!")
      mx = max (min (x, ct10_x_max), ct10_x_min)
      mq = max (min (q, ct10_q_max), ct10_q_min)
      if (present (f)) f = (/ 0._default, &
                getct10pdf (-5, mx, mq), getct10pdf (-4, mx, mq), &
                getct10pdf (-3, mx, mq), getct10pdf (-1, mx, mq), &
                getct10pdf (-2, mx, mq), getct10pdf ( 0, mx, mq), &
                getct10pdf ( 2, mx, mq), getct10pdf ( 1, mx, mq), &
                getct10pdf ( 3, mx, mq), getct10pdf ( 4, mx, mq), &
                getct10pdf ( 5, mx, mq), 0._default /)
      if (present (fphoton)) call msg_fatal ("photon pdf requested for " // &
         "CT10 which does not provide it!")
   case default
      call msg_fatal ("pdf_builtin: internal: invalid PDF set!")
end select
end subroutine pdf_evolve

end module pdf_builtin
