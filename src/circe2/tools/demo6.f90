  subroutine random (r)
    use kinds
    real(kind=double) :: r
    integer, parameter :: M = 259200, A = 7141, C = 54773
    integer, save :: n = 0
    n = mod (n*A + C, M)
    r = real (n, kind=double) / real (M, kind=double)
  end subroutine random
  program demo6
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
    write (*, '(A7,2(1X,A10))') '#', 'x1', 'x2'
    n = 0
    do
      call cir2ch (p1, h1, p2, h2, random)
      call cir2gn (p1, h1, p2, h2, x1, x2, random)
      if ((p1 .eq. 22) .and. (p2 .eq. 22) .and.  &
         (((h1 .eq.  1) .and. (h2 .eq.  1)) .or. &
         ((h1 .eq. -1) .and. (h2 .eq. -1)))) then
        n = n + 1
        write (*, '(I7,2(1X,F10.8))') n, x1, x2
      end if
      if (n .ge. nevent) exit
    end do
  end program demo6
