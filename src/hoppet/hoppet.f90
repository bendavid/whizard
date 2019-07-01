! HOPPET wrapper


!----------------------------------------------------------------------
! The module containing the HOPPET pdf
module whizard_hoppet_extras
  use streamlined_interface
  use dglap_holders
  implicit none

  type(pdf_table), save :: whizard_table
end module whizard_hoppet_extras


!----------------------------------------------------------------------
! Get a copy of the LHAPDF structure function, and subtract the LO gluon
! splitting contribution from the b quark pdf
subroutine InitForWhizard ()
  use whizard_hoppet_extras
  integer :: iQ, lcl_nf, nloop, order, factscheme
  double precision :: mb, Q, dy, ymax, Qmin, Qmax, dlnlnQ
  double precision :: alphasPDF
  external :: evolvePDF

  ! set hoppet start parameters
  dy    = 0.1d0      ! the internal grid spacing (smaller->higher accuarcy)
                     ! 0.1 should provide at least 10^{-3} accuracy 
  nloop = 2          ! the number of loops to initialise (max=3!)
  !--------------------------------------
  ! set extended start parameters
  ymax = 15.0d0      ! highest value of ln1/x user wants to access (default: 12.0d0)
  Qmin = 1.0d0       ! (default)
  Qmax = 28000d0     ! twice LHC c.o.m. (default)
  dlnlnQ = dy/4.0_dp ! min(0.5_dp*dy,0.07_dp) (default)
  order = -6         ! order of numerical interpolation (default)
  factscheme = 1     ! 1=unpol-MSbar, 2=unpol-DIS, 3=Pol-MSbar (default)

  ! start the hoppet evolution/convolution package 
!  call hoppetStart(dy, nloop)
  call hoppetStartExtended(ymax, dy, Qmin, Qmax, dlnlnQ, nloop, order, factscheme)

  ! initialise our PDF using the LHAPDF subroutine for PDF-access
  ! (any other subroutine with same interface can be used in its place)
  call hoppetAssign(evolvePDF)

  ! allocate a PDF table from from the streamlined interface (table)
  call AllocPdfTable(whizard_table, tables(0))
  whizard_table%tab = tables(0)%tab

  lcl_nf=5
  call SetNfDglapHolder(dh, lcl_nf)

  ! use lhapdf's b mass
  call GetQmass(5,mb)

  do iQ = lbound(whizard_table%tab,dim=3), ubound(whizard_table%tab,dim=3)

    Q = whizard_table%Q_vals(iQ)
    if ( Q > mb ) then

      ! NB 1/(2*lcl_nf) factor is because Pqg includes a 2nf factor
      whizard_table%tab(:,5,iQ) = whizard_table%tab(:,5,iQ) &
           &  - (dh%P_LO%qg * whizard_table%tab(:,0,iQ))  &
           &    * ( log(Q**2/mb**2) * alphasPDF(Q)/ (2*pi*2*lcl_nf) )
      whizard_table%tab(:,-5,iQ) = whizard_table%tab(:,-5,iQ) &
           &  - (dh%P_LO%qg * whizard_table%tab(:,0,iQ))  &
           &    * ( log(Q**2/mb**2) * alphasPDF(Q)/ (2*pi*2*lcl_nf) )
    end if
  end do
  
end subroutine InitForWhizard


!----------------------------------------------------------------------
! Return in f(-6:6) the value of the internally stored pdf at the
! given x,Q, with the usual LHApdf meanings for the indices -6:6.
subroutine EvalForWhizard (x,Q,f)
  use whizard_hoppet_extras
  implicit none
  double precision, intent(in)  :: x, Q
  double precision, intent(out) :: f(-6:6)
  
  call EvalPdfTable_xQ(whizard_table,x,Q,f)
end subroutine EvalForWhizard
