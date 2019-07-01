  subroutine random (r)
    use kinds
    real(kind=double) :: r
    integer, parameter :: M = 259200, A = 7141, C = 54773
    integer, save :: n = 0
    n = mod (n*A + C, M)
    r = real (n, kind=double) / real (M, kind=double)
  end subroutine random
  program demo5
    use kinds
    use circe2
    implicit none    
    real(kind=double) :: x1, x2, u, lumipp, lumimm
    integer :: n, nevent, ierror
    external random
    nevent = 20
    ierror = 1
    call cir2ld ('default.circe', '*', 500D0, ierror)
    if (ierror .lt. 0) stop
    lumipp = cir2lm (22,  1, 22,  1)
    lumimm = cir2lm (22, -1, 22, -1)
    write (*, '(A7,2(1X,A10))') '#', 'x1', 'x2'
    do n = 1, nevent
      call random (u)
      if (u * (lumipp + lumimm) .lt. lumipp) then
        call cir2gn (22,  1, 22,  1, x1, x2, random)
      else
        call cir2gn (22, -1, 22, -1, x1, x2, random)
      endif
      write (*, '(I7,2(1X,F10.8))') n, x1, x2
    end do
  end program demo5
