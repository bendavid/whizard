  program circe2_taotst
    use kinds
    use tao_rng

    implicit none
    integer :: i, N, S
    real(kind=double) :: r
    real(kind=double) :: sum30
    call taornt ()
    S = 0
    N = 10000000
    sum30 = 0
    call taorns (S)
    do i = 1, N
       call taornu (r)
       sum30 = sum30 + r
    end do
    print *, 'sum30 = ', sum30
  end program circe2_taotst

