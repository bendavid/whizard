! WHIZARD 2.2.2 July 6 2014
! 
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     
!     with contributions from
!     Christian Speckner <cnspeckn@googlemail.com> 
!     and  Fabian Bach, Felix Braam, Sebastian Schmidt, Daniel Wiesler 
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

module sm_qcd

  use kinds, only: default !NODEP!
  use file_utils !NODEP!
  use limits, only: FMT_12, MZ_REF, ALPHA_QCD_MZ_REF, LAMBDA_QCD_REF !NODEP!
  use diagnostics !NODEP!
  use md5
  use unit_tests
  
  use sm_physics !NODEP!

  implicit none
  private

  public :: alpha_qcd_t
  public :: alpha_qcd_fixed_t
  public :: alpha_qcd_from_scale_t
  public :: alpha_qcd_from_lambda_t
  public :: qcd_t
  public :: sm_qcd_test

  type, abstract :: alpha_qcd_t
   contains
     procedure (alpha_qcd_write), deferred :: write
     procedure (alpha_qcd_get), deferred :: get
  end type alpha_qcd_t
  
  type, extends (alpha_qcd_t) :: alpha_qcd_fixed_t
     real(default) :: val = ALPHA_QCD_MZ_REF
   contains
     procedure :: write => alpha_qcd_fixed_write
     procedure :: get => alpha_qcd_fixed_get
  end type alpha_qcd_fixed_t
     
  type, extends (alpha_qcd_t) :: alpha_qcd_from_scale_t
     real(default) :: mu_ref = MZ_REF
     real(default) :: ref = ALPHA_QCD_MZ_REF
     integer :: order = 0
     integer :: nf = 5
   contains
     procedure :: write => alpha_qcd_from_scale_write
     procedure :: get => alpha_qcd_from_scale_get
  end type alpha_qcd_from_scale_t
     
  type, extends (alpha_qcd_t) :: alpha_qcd_from_lambda_t
     real(default) :: lambda = LAMBDA_QCD_REF
     integer :: order = 0
     integer :: nf = 5
   contains
     procedure :: write => alpha_qcd_from_lambda_write
     procedure :: get => alpha_qcd_from_lambda_get
  end type alpha_qcd_from_lambda_t
     
  type :: qcd_t
     class(alpha_qcd_t), allocatable :: alpha
     character(32) :: md5sum = ""
   contains
     procedure :: write => qcd_write
     procedure :: compute_alphas_md5sum => qcd_compute_alphas_md5sum
     procedure :: get_md5sum => qcd_get_md5sum
  end type qcd_t


  abstract interface
     subroutine alpha_qcd_write (object, unit)
       import
       class(alpha_qcd_t), intent(in) :: object
       integer, intent(in), optional :: unit
     end subroutine alpha_qcd_write
  end interface

  abstract interface
     function alpha_qcd_get (alpha_qcd, scale) result (alpha)
       import
       class(alpha_qcd_t), intent(in) :: alpha_qcd
       real(default), intent(in) :: scale
       real(default) :: alpha
     end function alpha_qcd_get
  end interface
  

contains

  subroutine alpha_qcd_fixed_write (object, unit)
    class(alpha_qcd_fixed_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(3x,A)")  "QCD parameters (fixed coupling):"
    write (u, "(5x,A," // FMT_12 // ")")  "alpha = ", object%val
  end subroutine alpha_qcd_fixed_write
  
  function alpha_qcd_fixed_get (alpha_qcd, scale) result (alpha)
    class(alpha_qcd_fixed_t), intent(in) :: alpha_qcd
    real(default), intent(in) :: scale
    real(default) :: alpha
    alpha = alpha_qcd%val
  end function alpha_qcd_fixed_get
  
  subroutine alpha_qcd_from_scale_write (object, unit)
    class(alpha_qcd_from_scale_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(3x,A)")  "QCD parameters (running coupling):"
    write (u, "(5x,A," // FMT_12 // ")")  "Scale mu  = ", object%mu_ref
    write (u, "(5x,A," // FMT_12 // ")")  "alpha(mu) = ", object%ref
    write (u, "(5x,A,I0)")      "LL order  = ", object%order
    write (u, "(5x,A,I0)")      "N(flv)    = ", object%nf
  end subroutine alpha_qcd_from_scale_write
  
  function alpha_qcd_from_scale_get (alpha_qcd, scale) result (alpha)
    class(alpha_qcd_from_scale_t), intent(in) :: alpha_qcd
    real(default), intent(in) :: scale
    real(default) :: alpha
    alpha = running_as (scale, &
         alpha_qcd%ref, alpha_qcd%mu_ref, alpha_qcd%order, &
         real (alpha_qcd%nf, kind=default))
  end function alpha_qcd_from_scale_get
  
  subroutine alpha_qcd_from_lambda_write (object, unit)
    class(alpha_qcd_from_lambda_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = output_unit (unit)
    write (u, "(3x,A)")  "QCD parameters (Lambda_QCD as input):"
    write (u, "(5x,A," // FMT_12 // ")")  "Lambda_QCD = ", object%lambda
    write (u, "(5x,A,I0)")      "LL order   = ", object%order
    write (u, "(5x,A,I0)")      "N(flv)     = ", object%nf
  end subroutine alpha_qcd_from_lambda_write
  
  function alpha_qcd_from_lambda_get (alpha_qcd, scale) result (alpha)
    class(alpha_qcd_from_lambda_t), intent(in) :: alpha_qcd
    real(default), intent(in) :: scale
    real(default) :: alpha
    alpha = running_as_lam (real (alpha_qcd%nf, kind=default), scale, &
         alpha_qcd%lambda, alpha_qcd%order)
  end function alpha_qcd_from_lambda_get
  
  subroutine qcd_write (qcd, unit, show_md5sum)
    class(qcd_t), intent(in) :: qcd
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: show_md5sum
    logical :: show_md5
    integer :: u
    u = output_unit (unit)
    show_md5 = .true.;  if (present (show_md5sum))  show_md5 = show_md5sum
    if (allocated (qcd%alpha)) then
       call qcd%alpha%write (u)
    else
       write (u, "(3x,A)")  "QCD parameters (coupling undefined)"
    end if
    if (show_md5 .and. qcd%md5sum /= "") &
         write (u, "(5x,A,A,A)") "md5sum = '", qcd%md5sum, "'"    
  end subroutine qcd_write
  
  subroutine qcd_compute_alphas_md5sum (qcd) 
    class(qcd_t), intent(inout) :: qcd
    integer :: unit
    if (allocated (qcd%alpha)) then    
       unit = free_unit ()
       open (unit, status="scratch", action="readwrite")
       call qcd%alpha%write (unit)
       rewind (unit)       
       qcd%md5sum = md5sum (unit)       
       close (unit)       
    end if
  end subroutine qcd_compute_alphas_md5sum

  function qcd_get_md5sum (qcd) result (md5sum)
    character(32) :: md5sum
    class(qcd_t), intent(inout) :: qcd
    md5sum = qcd%md5sum
  end function qcd_get_md5sum


  subroutine sm_qcd_test (u, results)
    integer, intent(in) :: u
    type(test_results_t), intent(inout) :: results
    call test (sm_qcd_1, "sm_qcd_1", &
         "running alpha_s", &
         u, results)
  end subroutine sm_qcd_test
  
  subroutine sm_qcd_1 (u)
    integer, intent(in) :: u
    type(qcd_t) :: qcd
    
    write (u, "(A)")  "* Test output: sm_qcd_1"
    write (u, "(A)")  "*   Purpose: compute running alpha_s"
    write (u, "(A)")
    
    write (u, "(A)")  "* Fixed:"
    write (u, "(A)")

    allocate (alpha_qcd_fixed_t :: qcd%alpha)
    call qcd%compute_alphas_md5sum ()
    
    call qcd%write (u)
    write (u, *)
    write (u, "(1x,A,F10.7)")  "alpha_s (mz)    =", &
         qcd%alpha%get (MZ_REF)
    write (u, "(1x,A,F10.7)")  "alpha_s (1 TeV) =", &
         qcd%alpha%get (1000._default)
    write (u, *)
    deallocate (qcd%alpha)
     
    write (u, "(A)")  "* Running from MZ (LO):"
    write (u, "(A)")

    allocate (alpha_qcd_from_scale_t :: qcd%alpha)
    call qcd%compute_alphas_md5sum ()
    
    call qcd%write (u)
    write (u, *)
    write (u, "(1x,A,F10.7)")  "alpha_s (mz)    =", &
         qcd%alpha%get (MZ_REF)
    write (u, "(1x,A,F10.7)")  "alpha_s (1 TeV) =", &
         qcd%alpha%get (1000._default)
    write (u, *)
     
    write (u, "(A)")  "* Running from MZ (NLO):"
    write (u, "(A)")

    select type (alpha => qcd%alpha)
    type is (alpha_qcd_from_scale_t)
       alpha%order = 1
    end select
    call qcd%compute_alphas_md5sum ()
    
    call qcd%write (u)
    write (u, *)
    write (u, "(1x,A,F10.7)")  "alpha_s (mz)    =", &
         qcd%alpha%get (MZ_REF)
    write (u, "(1x,A,F10.7)")  "alpha_s (1 TeV) =", &
         qcd%alpha%get (1000._default)
    write (u, *)
     
    write (u, "(A)")  "* Running from MZ (NNLO):"
    write (u, "(A)")

    select type (alpha => qcd%alpha)
    type is (alpha_qcd_from_scale_t)
       alpha%order = 2
    end select
    call qcd%compute_alphas_md5sum ()
    
    call qcd%write (u)
    write (u, *)
    write (u, "(1x,A,F10.7)")  "alpha_s (mz)    =", &
         qcd%alpha%get (MZ_REF)
    write (u, "(1x,A,F10.7)")  "alpha_s (1 TeV) =", &
         qcd%alpha%get (1000._default)
    write (u, *)

    deallocate (qcd%alpha)
    write (u, "(A)")  "* Running from Lambda_QCD (LO):"
    write (u, "(A)")

    allocate (alpha_qcd_from_lambda_t :: qcd%alpha)
    call qcd%compute_alphas_md5sum ()
    
    call qcd%write (u)
    write (u, *)
    write (u, "(1x,A,F10.7)")  "alpha_s (mz)    =", &
         qcd%alpha%get (MZ_REF)
    write (u, "(1x,A,F10.7)")  "alpha_s (1 TeV) =", &
         qcd%alpha%get (1000._default)
    write (u, *)
     
    write (u, "(A)")  "* Running from Lambda_QCD (NLO):"
    write (u, "(A)")

    select type (alpha => qcd%alpha)
    type is (alpha_qcd_from_lambda_t)
       alpha%order = 1
    end select
    call qcd%compute_alphas_md5sum ()
    
    call qcd%write (u)
    write (u, *)
    write (u, "(1x,A,F10.7)")  "alpha_s (mz)    =", &
         qcd%alpha%get (MZ_REF)
    write (u, "(1x,A,F10.7)")  "alpha_s (1 TeV) =", &
         qcd%alpha%get (1000._default)
    write (u, *)
     
    write (u, "(A)")  "* Running from Lambda_QCD (NNLO):"
    write (u, "(A)")

    select type (alpha => qcd%alpha)
    type is (alpha_qcd_from_lambda_t)
       alpha%order = 2
    end select
    call qcd%compute_alphas_md5sum ()
    
    call qcd%write (u)
    write (u, *)
    write (u, "(1x,A,F10.7)")  "alpha_s (mz)    =", &
         qcd%alpha%get (MZ_REF)
    write (u, "(1x,A,F10.7)")  "alpha_s (1 TeV) =", &
         qcd%alpha%get (1000._default)

    write (u, "(A)")
    write (u, "(A)")  "* Test output end: sm_qcd_1"

  end subroutine sm_qcd_1
  

end module sm_qcd
