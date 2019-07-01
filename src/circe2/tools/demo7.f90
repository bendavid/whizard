  subroutine random (r)
    use kinds
    real(kind=double) :: r
    integer, parameter :: M = 259200, A = 7141, C = 54773
    integer, save :: n = 0
    n = mod (n*A + C, M)
    r = real (n, kind=double) / real (M, kind=double)
  end subroutine random
  program demo7
    use kinds
    use circe2
    implicit none
    integer :: n, nevent, ierror
    real(kind=double) :: x1, x2, w
    nevent = 20
    ierror = 1
    call cir2ld ('default.circe', '*', 500D0, ierror)
    if (ierror .lt. 0) stop
    write (*, '(A7,3(1X,A10))') '#', 'x1', 'x2', 'weight'
    do n = 1, nevent
      call random (x1)
      call random (x2)
      w = cir2dn (22, 1, 22, 1, x1, x2)
      write (*, '(I7,2(1X,F10.8),1X,E10.4)') n, x1, x2, w
    end do
  end program demo7
