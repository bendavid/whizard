  subroutine random (r)
    use kinds
    real(kind=double) :: r
    integer, parameter :: M = 259200, A = 7141, C = 54773
    integer, save :: n = 0
    n = mod (n*A + C, M)
    r = real (n, kind=double) / real (M, kind=double)
  end subroutine random
  program demo2
    use kinds
    use circe2
    implicit none
    integer :: i1, i2, h1, h2, i, n, nevent, nev, ierror
    integer, dimension(3), save :: pdg = (/ 22, 11, -11 /)
    real(kind=double) :: x1, x2, lumi
    external random
    nevent = 20
    ierror = 1
    call cir2ld ('default.circe', '*', 500D0, ierror)
    if (ierror .lt. 0) stop
    lumi = cir2lm (0, 0, 0, 0)
    write (*, '(A7,4(1X,A4),2(1X,A10))') &
        '#', 'pdg1', 'hel1', 'pdg2', 'hel2', 'x1', 'x2'
    i = 0
    do i1 = 1, 3
      do i2 = 1, 3
        do h1 = -1, 1, 2
          do h2 = -1, 1, 2
            nev = nevent * cir2lm (pdg(i1), h1, pdg(i2), h2) / lumi
            do n = 1, nev
              call cir2gn (pdg(i1), h1, pdg(i2), h2, x1, x2, random)
              i = i + 1
              write (*, '(I7,4(1X,I4),2(1X,F10.8))') &
                   i, pdg(i1), h1, pdg(i2), h2, x1, x2
            end do
          end do  
        end do  
      end do
    end do
  end program demo2
