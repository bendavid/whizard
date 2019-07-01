  program demo1
    use kinds
    use circe2
    implicit none
    integer :: p1, h1, p2, h2, n, nevent, ierror
    real(kind=double) :: x1, x2
    external random
    nevent = 20
    ierror = 1
    call cir2ld ('default.circe', '*', 500D0, ierror)
    if (ierror .lt. 0) stop
    write (*, '(A7,4(1X,A4),2(1X,A10))') &
         '#', 'pdg1', 'hel1', 'pdg2', 'hel2', 'x1', 'x2'
    do n = 1, nevent
      call cir2ch (p1, h1, p2, h2, random)
      call cir2gn (p1, h1, p2, h2, x1, x2, random)
      write (*, '(I7,4(1X,I4),2(1X,F10.8))') n, p1, h1, p2, h2, x1, x2
    end do
  end program demo1
  subroutine random (r)
    use kinds
    real(kind=double) :: r
    integer, parameter :: M = 259200, A = 7141, C = 54773
    integer, save :: n = 0
    n = mod (n*A + C, M)
    r = real (n, kind=double) / real (M, kind=double)
  end subroutine random
