  subroutine random (r)
    use kinds
    real(kind=double) :: r
    integer, parameter :: M = 259200, A = 7141, C = 54773
    integer, save :: n = 0
    n = mod (n*A + C, M)
    r = real (n, kind=double) / real (M, kind=double)
  end subroutine random
  program demo4
    use kinds
    use circe2
    implicit none
    real(kind=double) :: x1, x2, lumipp, lumimm
    integer :: n, nevent, npp, nmm, ierror
    external random
    nevent = 20
    ierror = 1
    call cir2ld ('default.circe', '*', 500D0, ierror)
    if (ierror .lt. 0) stop
    lumipp = cir2lm (22,  1, 22,  1)
    lumimm = cir2lm (22, -1, 22, -1)
    npp = nevent * lumipp / (lumipp + lumimm)
    nmm = nevent - npp
    write (*, '(A7,2(1X,A10))') '#', 'x1', 'x2'
    do n = 1, npp
      call cir2gn (22,  1, 22,  1, x1, x2, random)
      write (*, '(I7,2(1X,F10.8))') n, x1, x2
    end do
    do n = 1, nmm
      call cir2gn (22, -1, 22, -1, x1, x2, random)
      write (*, '(I7,2(1X,F10.8))') n, x1, x2
    end do
  end program demo4
