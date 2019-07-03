! $Id:$
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module kinds
  implicit none
  integer, parameter, public :: &
       double = selected_real_kind (precision (1.0) + 1, range (1.0) + 1)
end module kinds

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module histograms
  use kinds
  implicit none

  integer, parameter :: DEFAULT_UNIT = 42

  public :: create_histogram
  public :: fill_histogram
  public :: delete_histogram
  public :: write_histogram
  private :: create_histogram1, create_histogram2
  private :: fill_histogram1, fill_histogram2s, fill_histogram2v
  private :: delete_histogram1, delete_histogram2
  private :: write_histogram1, write_histogram2
  private :: midpoint
  private :: midpoint1, midpoint2
  interface create_histogram
     module procedure create_histogram1, create_histogram2
  end interface
  interface fill_histogram
     module procedure fill_histogram1, fill_histogram2s, fill_histogram2v
  end interface
  interface delete_histogram
     module procedure delete_histogram1, delete_histogram2
  end interface
  interface write_histogram
     module procedure write_histogram1, write_histogram2
  end interface
  interface midpoint
     module procedure midpoint1, midpoint2
  end interface
  integer, parameter, private :: N_BINS_DEFAULT = 10
  type, public :: histogram
     integer :: n_bins
     real(kind=double) :: x_min, x_max
     real(kind=double), dimension(:), pointer :: bins, bins2
  end type histogram
  type, public :: histogram2

     integer, dimension(2) :: n_bins
     real(kind=double), dimension(2) :: x_min, x_max
     real(kind=double), dimension(:,:), pointer :: bins, bins2
  end type histogram2
  character(len=*), public, parameter :: HISTOGRAMS_RCS_ID = &
       "$Id: triangle90.f90,v 1.8 1998/09/09 11:10:14 ohl Exp $"
contains
  elemental subroutine create_histogram1 (h, x_min, x_max, nb)
    type(histogram), intent(out) :: h
    real(kind=double), intent(in) :: x_min, x_max
    integer, intent(in), optional :: nb
    if (present (nb)) then
       h%n_bins = nb
    else
       h%n_bins = N_BINS_DEFAULT
    end if
    h%x_min = x_min
    h%x_max = x_max
    allocate (h%bins(0:h%n_bins+1), h%bins2(0:h%n_bins+1))
    h%bins = 0
    h%bins2 = 0
  end subroutine create_histogram1
  pure subroutine create_histogram2 (h, x_min, x_max, nb)
    type(histogram2), intent(out) :: h
    real(kind=double), dimension(:), intent(in) :: x_min, x_max
    integer, intent(in), dimension(:), optional :: nb
    if (present (nb)) then
       h%n_bins = nb
    else
       h%n_bins = N_BINS_DEFAULT
    end if
    h%x_min = x_min
    h%x_max = x_max
    allocate (h%bins(0:h%n_bins(1)+1,0:h%n_bins(1)+1), &
         h%bins2(0:h%n_bins(2)+1,0:h%n_bins(2)+1))
    h%bins = 0
    h%bins2 = 0
  end subroutine create_histogram2
  elemental subroutine fill_histogram1 (h, x, weight)
    type(histogram), intent(inout) :: h
    real(kind=double), intent(in) :: x
    real(kind=double), intent(in), optional :: weight
    integer :: i
    i = 1 + h%n_bins * (x - h%x_min) / (h%x_max - h%x_min)
    i = min (max (i, 0), h%n_bins + 1)
    if (present (weight)) then
       h%bins(i) = h%bins(i) + weight
       h%bins2(i) = h%bins2(i) + weight*weight
    else
       h%bins(i) = h%bins(i) + 1
       h%bins2(i) = h%bins2(i) + 1
    end if
  end subroutine fill_histogram1
  elemental subroutine fill_histogram2s (h, x1, x2, weight)
    type(histogram2), intent(inout) :: h
    real(kind=double), intent(in) :: x1, x2
    real(kind=double), intent(in), optional :: weight
    call fill_histogram2v (h, (/ x1, x2 /), weight)
  end subroutine fill_histogram2s
  pure subroutine fill_histogram2v (h, x, weight)
    type(histogram2), intent(inout) :: h
    real(kind=double), dimension(:), intent(in) :: x
    real(kind=double), intent(in), optional :: weight
    integer, dimension(2) :: i
    i = 1 + h%n_bins * (x - h%x_min) / (h%x_max - h%x_min)
    i = min (max (i, 0), h%n_bins + 1)
    if (present (weight)) then
       h%bins(i(1),i(2)) = h%bins(i(1),i(2)) + weight
       h%bins2(i(1),i(2)) = h%bins2(i(1),i(2)) + weight*weight
    else
       h%bins(i(1),i(2)) = h%bins(i(1),i(2)) + 1
       h%bins2(i(1),i(2)) = h%bins2(i(1),i(2)) + 1
    end if
  end subroutine fill_histogram2v
  elemental subroutine delete_histogram1 (h)
    type(histogram), intent(inout) :: h
    deallocate (h%bins, h%bins2)
  end subroutine delete_histogram1
  elemental subroutine delete_histogram2 (h)
    type(histogram2), intent(inout) :: h
    deallocate (h%bins, h%bins2)
  end subroutine delete_histogram2
  subroutine write_histogram1 (h, name, over)
    type(histogram), intent(in) :: h
    character(len=*), intent(in), optional :: name
    logical, intent(in), optional :: over
    integer :: i
    if (present (name)) then
       if (DEFAULT_UNIT > 0) then
          open (unit = DEFAULT_UNIT, action = "write", status = "replace", &
               file = name)
          if (present (over)) then
             if (over) then
                write (unit = DEFAULT_UNIT, fmt = *) &
                     "underflow", h%bins(0), sqrt (h%bins2(0))
             end if
          end if
          do i = 1, h%n_bins
             write (unit = DEFAULT_UNIT, fmt = *) &
                  midpoint (h, i), h%bins(i), sqrt (h%bins2(i))
          end do
          if (present (over)) then
             if (over) then
                write (unit = DEFAULT_UNIT, fmt = *) &
                     "overflow", h%bins(h%n_bins+1), &
                     sqrt (h%bins2(h%n_bins+1))
             end if
          end if
          close (unit = DEFAULT_UNIT)
       else
          print *, "write_histogram: Can't find a free unit!"
       end if
    else
       if (present (over)) then
          if (over) then
             print *, "underflow", h%bins(0), sqrt (h%bins2(0))
          end if
       end if
       do i = 1, h%n_bins
          print *, midpoint (h, i), h%bins(i), sqrt (h%bins2(i))
       end do
       if (present (over)) then
          if (over) then
             print *, "overflow", h%bins(h%n_bins+1), &
                  sqrt (h%bins2(h%n_bins+1))
          end if
       end if
    end if
  end subroutine write_histogram1
  elemental function midpoint1 (h, bin) result (x)
    type(histogram), intent(in) :: h
    integer, intent(in) :: bin
    real(kind=double) :: x
    x = h%x_min + (h%x_max - h%x_min) * (bin - 0.5) / h%n_bins
  end function midpoint1
  elemental function midpoint2 (h, bin, d) result (x)
    type(histogram2), intent(in) :: h
    integer, intent(in) :: bin, d
    real(kind=double) :: x
    x = h%x_min(d) + (h%x_max(d) - h%x_min(d)) * (bin - 0.5) / h%n_bins(d)
  end function midpoint2
  subroutine write_histogram2 (h, name, over)
    type(histogram2), intent(in) :: h
    character(len=*), intent(in), optional :: name
    logical, intent(in), optional :: over
    integer :: i1, i2
    if (present (name)) then
       if (DEFAULT_UNIT > 0) then
          open (unit = DEFAULT_UNIT, action = "write", status = "replace", &
               file = name)
          if (present (over)) then
             if (over) then
                write (unit = DEFAULT_UNIT, fmt = *) &
                     "double underflow", h%bins(0,0), sqrt (h%bins2(0,0))
                do i2 = 1, h%n_bins(2)
                   write (unit = DEFAULT_UNIT, fmt = *) &
                        "x1 underflow", midpoint (h, i2, 2), &
                        h%bins(0,i2), sqrt (h%bins2(0,i2))
                end do
                do i1 = 1, h%n_bins(1)
                   write (unit = DEFAULT_UNIT, fmt = *) &
                        "x2 underflow", midpoint (h, i1, 1), &
                        h%bins(i1,0), sqrt (h%bins2(i1,0))
                end do
             end if
          end if
          do i1 = 1, h%n_bins(1)
             do i2 = 1, h%n_bins(2)
                write (unit = DEFAULT_UNIT, fmt = *) &
                     midpoint (h, i1, 1), midpoint (h, i2, 2), &
                     h%bins(i1,i2), sqrt (h%bins2(i1,i2))
             end do
          end do
          if (present (over)) then
             if (over) then
                do i2 = 1, h%n_bins(2)
                   write (unit = DEFAULT_UNIT, fmt = *) &
                        "x1 overflow", midpoint (h, i2, 2), &
                        h%bins(h%n_bins(1)+1,i2), &
                        sqrt (h%bins2(h%n_bins(1)+1,i2))
                end do
                do i1 = 1, h%n_bins(1)
                   write (unit = DEFAULT_UNIT, fmt = *) &
                        "x2 overflow", midpoint (h, i1, 1), &
                        h%bins(i1,h%n_bins(2)+1), &
                        sqrt (h%bins2(i1,h%n_bins(2)+1))
                end do
                write (unit = DEFAULT_UNIT, fmt = *) "double overflow", &
                     h%bins(h%n_bins(1)+1,h%n_bins(2)+1), &
                     sqrt (h%bins2(h%n_bins(1)+1,h%n_bins(2)+1))
             end if
          end if
          close (unit = DEFAULT_UNIT)
       else
          print *, "write_histogram: Can't find a free unit!"
       end if
    else
       if (present (over)) then
          if (over) then
             print *, "double underflow", h%bins(0,0), sqrt (h%bins2(0,0))
             do i2 = 1, h%n_bins(2)
                print *, "x1 underflow", midpoint (h, i2, 2), &
                     h%bins(0,i2), sqrt (h%bins2(0,i2))
             end do
             do i1 = 1, h%n_bins(1)
                print *, "x2 underflow", midpoint (h, i1, 1), &
                     h%bins(i1,0), sqrt (h%bins2(i1,0))
             end do
          end if
       end if
       do i1 = 1, h%n_bins(1)
          do i2 = 1, h%n_bins(2)
             print *, midpoint (h, i1, 1), midpoint (h, i2, 2), &
                  h%bins(i1,i2), sqrt (h%bins2(i1,i2))
          end do
       end do
       if (present (over)) then
          if (over) then
             do i2 = 1, h%n_bins(2)
                print *, "x1 overflow", midpoint (h, i2, 2), &
                     h%bins(h%n_bins(1)+1,i2), &
                     sqrt (h%bins2(h%n_bins(1)+1,i2))
             end do
             do i1 = 1, h%n_bins(1)
                print *, "x2 overflow", midpoint (h, i1, 1), &
                     h%bins(i1,h%n_bins(2)+1), &
                     sqrt (h%bins2(i1,h%n_bins(2)+1))
             end do
             print *, "double overflow", &
                  h%bins(h%n_bins(1)+1,h%n_bins(2)+1), &
                  sqrt (h%bins2(h%n_bins(1)+1,h%n_bins(2)+1))
          end if
       end if
    end if
  end subroutine write_histogram2
end module histograms

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!! module specfun77
!!!   use kinds
!!!   implicit none
!!! contains
!!!   function besi0(x)
!!!     implicit double precision (a-h,o-z)
!!!     implicit integer (i-n)
!!!     logical lex
!!!     dimension ci(0:24,0:1),ck(0:16,0:1)
!!!     parameter (eps=1d-15)
!!!     parameter (z1 = 1, hf = z1/2)
!!!     parameter (pi = 3.14159265358979324d0)
!!!     parameter (ce = 0.57721566490153286d0)
!!!     parameter (pih = pi/2, rpih = 2/pi, rpi2 = 1/(2*pi))
!!! 
!!!     data ci( 0,0) /+1.008279205458740032d0/
!!!     data ci( 1,0) /+0.008445122624920943d0/
!!!     data ci( 2,0) /+0.000172700630777567d0/
!!!     data ci( 3,0) /+0.000007247591099959d0/
!!!     data ci( 4,0) /+0.000000513587726878d0/
!!!     data ci( 5,0) /+0.000000056816965808d0/
!!!     data ci( 6,0) /+0.000000008513091223d0/
!!!     data ci( 7,0) /+0.000000001238425364d0/
!!!     data ci( 8,0) /+0.000000000029801672d0/
!!!     data ci( 9,0) /-0.000000000078956698d0/
!!!     data ci(10,0) /-0.000000000033127128d0/
!!!     data ci(11,0) /-0.000000000004497339d0/
!!!     data ci(12,0) /+0.000000000001799790d0/
!!!     data ci(13,0) /+0.000000000000965748d0/
!!!     data ci(14,0) /+0.000000000000038604d0/
!!!     data ci(15,0) /-0.000000000000104039d0/
!!!     data ci(16,0) /-0.000000000000023950d0/
!!!     data ci(17,0) /+0.000000000000009554d0/
!!!     data ci(18,0) /+0.000000000000004443d0/
!!!     data ci(19,0) /-0.000000000000000859d0/
!!!     data ci(20,0) /-0.000000000000000709d0/
!!!     data ci(21,0) /+0.000000000000000087d0/
!!!     data ci(22,0) /+0.000000000000000112d0/
!!!     data ci(23,0) /-0.000000000000000012d0/
!!!     data ci(24,0) /-0.000000000000000018d0/
!!!                                      
!!!     data ci( 0,1) /+0.975800602326285926d0/
!!!     data ci( 1,1) /-0.024467442963276385d0/
!!!     data ci( 2,1) /-0.000277205360763829d0/
!!!     data ci( 3,1) /-0.000009732146728020d0/
!!!     data ci( 4,1) /-0.000000629724238640d0/
!!!     data ci( 5,1) /-0.000000065961142154d0/
!!!     data ci( 6,1) /-0.000000009613872919d0/
!!!     data ci( 7,1) /-0.000000001401140901d0/
!!!     data ci( 8,1) /-0.000000000047563167d0/
!!!     data ci( 9,1) /+0.000000000081530681d0/
!!!     data ci(10,1) /+0.000000000035408148d0/
!!!     data ci(11,1) /+0.000000000005102564d0/
!!!     data ci(12,1) /-0.000000000001804409d0/
!!!     data ci(13,1) /-0.000000000001023594d0/
!!!     data ci(14,1) /-0.000000000000052678d0/
!!!     data ci(15,1) /+0.000000000000107094d0/
!!!     data ci(16,1) /+0.000000000000026120d0/
!!!     data ci(17,1) /-0.000000000000009561d0/
!!!     data ci(18,1) /-0.000000000000004713d0/
!!!     data ci(19,1) /+0.000000000000000829d0/
!!!     data ci(20,1) /+0.000000000000000743d0/
!!!     data ci(21,1) /-0.000000000000000080d0/
!!!     data ci(22,1) /-0.000000000000000117d0/
!!!     data ci(23,1) /+0.000000000000000011d0/
!!!     data ci(24,1) /+0.000000000000000019d0/
!!!                                      
!!!     data ck( 0,0) /+0.988408174230825800d0/
!!!     data ck( 1,0) /-0.011310504646928281d0/
!!!     data ck( 2,0) /+0.000269532612762724d0/
!!!     data ck( 3,0) /-0.000011106685196665d0/
!!!     data ck( 4,0) /+0.000000632575108500d0/
!!!     data ck( 5,0) /-0.000000045047337641d0/
!!!     data ck( 6,0) /+0.000000003792996456d0/
!!!     data ck( 7,0) /-0.000000000364547179d0/
!!!     data ck( 8,0) /+0.000000000039043756d0/
!!!     data ck( 9,0) /-0.000000000004579936d0/
!!!     data ck(10,0) /+0.000000000000580811d0/
!!!     data ck(11,0) /-0.000000000000078832d0/
!!!     data ck(12,0) /+0.000000000000011360d0/
!!!     data ck(13,0) /-0.000000000000001727d0/
!!!     data ck(14,0) /+0.000000000000000275d0/
!!!     data ck(15,0) /-0.000000000000000046d0/
!!!     data ck(16,0) /+0.000000000000000008d0/
!!!                                      
!!!     data ck( 0,1) /+1.035950858772358331d0/
!!!     data ck( 1,1) /+0.035465291243331114d0/
!!!     data ck( 2,1) /-0.000468475028166889d0/
!!!     data ck( 3,1) /+0.000016185063810053d0/
!!!     data ck( 4,1) /-0.000000845172048124d0/
!!!     data ck( 5,1) /+0.000000057132218103d0/
!!!     data ck( 6,1) /-0.000000004645554607d0/
!!!     data ck( 7,1) /+0.000000000435417339d0/
!!!     data ck( 8,1) /-0.000000000045757297d0/
!!!     data ck( 9,1) /+0.000000000005288133d0/
!!!     data ck(10,1) /-0.000000000000662613d0/
!!!     data ck(11,1) /+0.000000000000089048d0/
!!!     data ck(12,1) /-0.000000000000012726d0/
!!!     data ck(13,1) /+0.000000000000001921d0/
!!!     data ck(14,1) /-0.000000000000000305d0/
!!!     data ck(15,1) /+0.000000000000000050d0/
!!!     data ck(16,1) /-0.000000000000000009d0/
!!! 
!!!     nu=0
!!!     lex=.false.
!!!     go to 6
!!! 
!!!     entry  ebesi0(x)
!!!     nu=0
!!!     lex=.true.
!!!     go to 6
!!!     
!!!     entry  besi1(x)
!!!     nu=1
!!!     lex=.false.
!!!     go to 6
!!!     
!!!     entry  ebesi1(x)
!!!     nu=1
!!!     lex=.true.
!!! 
!!! 6   continue
!!!     v=abs(x)
!!!     if(v .lt. 8) then
!!!        y=(hf*v)**2
!!!        xl=nu+2
!!!        a0=1
!!!        a1=1+2*y/((xl+1)*(xl-1))
!!!        a2=1+y*(4+3*y/((xl+2)*xl))/((xl+3)*(xl-1))
!!!        b0=1
!!!        b1=1-y/(xl+1)
!!!        b2=1-y*(1-y/(2*(xl+2)))/(xl+3)
!!!        w1=3+xl
!!!        v1=3-xl
!!!        v3=xl-1
!!!        v2=v3+v3
!!!        c=0
!!!        do n = 3,30
!!!           c0=c
!!!           fn=n
!!!           w1=w1+2
!!!           w2=w1-1
!!!           w3=w2-1
!!!           w4=w3-1
!!!           w5=w4-1
!!!           w6=w5-1
!!!           v1=v1+1
!!!           v2=v2+1
!!!           v3=v3+1
!!!           u1=fn*w4
!!!           e=v3/(u1*w3)
!!!           u2=e*y
!!!           f1=1+y*v1/(u1*w1)
!!!           f2=(1+y*v2/(v3*w2*w5))*u2
!!!           f3=-y*y*u2/(w4*w5*w5*w6)
!!!           a=f1*a2+f2*a1+f3*a0
!!!           b=f1*b2+f2*b1+f3*b0
!!!           c=a/b
!!!           if(abs(c0-c) .lt. eps*abs(c)) go to 4
!!!           a0=a1
!!!           a1=a2
!!!           a2=a
!!!           b0=b1
!!!           b1=b2
!!!           b2=b
!!!        end do
!!! 4      continue
!!!        h=c
!!!        if(nu .eq. 1) h=hf*x*h
!!!        if(lex) h=exp(-v)*h
!!!     else
!!!        r=1/v
!!!        h=16*r-1
!!!        alfa=h+h
!!!        b1=0
!!!        b2=0
!!!        do i = 24,0,-1
!!!           b0=ci(i,nu)+alfa*b1-b2
!!!           b2=b1
!!!           b1=b0
!!!        end do
!!!        h=sqrt(rpi2*r)*(b0-h*b2)
!!!        if(nu*x .lt. 0) h=-h
!!!        if(.not.lex) h=exp(v)*h
!!!     end if
!!!     go to 9
!!! 
!!!     entry  besk0(x)
!!!     nu=0
!!!     lex=.false.
!!!     go to 8
!!!     
!!!     entry  ebesk0(x)
!!!     nu=0
!!!     lex=.true.
!!!     go to 8
!!!     
!!!     entry  besk1(x)
!!!     nu=1
!!!     lex=.false.
!!!     go to 8
!!!     
!!!     entry  ebesk1(x)
!!!     nu=1
!!!     lex=.true.
!!!     
!!! 8   continue
!!!     if(x .le. 0) then
!!!        h = - huge (h)
!!!        return
!!!     elseif(x .lt. 1) then
!!!        b=hf*x
!!!        bk=-(log(b)+ce)
!!!        f=bk
!!!        p=hf
!!!        q=hf
!!!        c=1
!!!        d=b**2
!!!        bk1=p
!!!        do n = 1,15
!!!           fn=n
!!!           rfn=1/fn
!!!           p=p*rfn
!!!           q=q*rfn
!!!           f=(f+p+q)*rfn
!!!           c=c*d*rfn
!!!           g=c*(p-fn*f)
!!!           h=c*f
!!!           bk=bk+h
!!!           bk1=bk1+g
!!!           if(bk1*h+abs(g)*bk .le. eps*bk*bk1) go to 12
!!!        end do
!!! 12     continue
!!!        h=bk
!!!        if(nu .eq. 1) h=bk1/b
!!!        if(lex) h=exp(x)*h
!!!     elseif(x .le. 5) then
!!!        xn=4*nu**2
!!!        a=9-xn
!!!        b=25-xn
!!!        c=768*x**2
!!!        c0=48*x
!!!        a0=1
!!!        a1=(16*x+7+xn)/a
!!!        a2=(c+c0*(xn+23)+xn*(xn+62)+129)/(a*b)
!!!        b0=1
!!!        b1=(16*x+9-xn)/a
!!!        b2=(c+c0*b)/(a*b)+1
!!!        c=0
!!!        do n = 3,30
!!!           c0=c
!!!           fn=n
!!!           fn2=fn+fn
!!!           fn1=fn2-1
!!!           fn3=fn1/(fn2-3)
!!!           fn4=12*fn**2-(1-xn)
!!!           fn5=16*fn1*x
!!!           ran=1/((fn2+1)**2-xn)
!!!           f1=fn3*(fn4-20*fn)+fn5
!!!           f2=28*fn-fn4-8+fn5
!!!           f3=fn3*((fn2-5)**2-xn)
!!!           a=(f1*a2+f2*a1+f3*a0)*ran
!!!           b=(f1*b2+f2*b1+f3*b0)*ran
!!!           c=a/b
!!!           if(abs(c0-c) .lt. eps*abs(c)) go to 25
!!!           a0=a1
!!!           a1=a2
!!!           a2=a
!!!           b0=b1
!!!           b1=b2
!!!           b2=b
!!!        end do
!!! 25     continue
!!!        h=c/sqrt(rpih*x)
!!!        if(.not.lex) h=exp(-x)*h
!!!     else
!!!        r=1/x
!!!        h=10*r-1
!!!        alfa=h+h
!!!        b1=0
!!!        b2=0
!!!        do i = 16,0,-1
!!!           b0=ck(i,nu)+alfa*b1-b2
!!!           b2=b1
!!!           b1=b0
!!!        end do
!!!        h=sqrt(pih*r)*(b0-h*b2)
!!!        if(.not.lex) h=exp(-x)*h
!!!     end if
!!! 9   continue
!!!     besi0=h
!!!     return
!!!   end function besi0
!!! end module specfun77

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module specfun
  use kinds
  implicit none
  real(kind=double), private, parameter :: PI = 3.14159265358979324d0
  real(kind=double), private, parameter :: CE = 0.57721566490153286d0
  real(kind=double), private, parameter :: PIH = PI/2
  real(kind=double), private, parameter :: RPIH = 2/PI
  real(kind=double), private, parameter :: RPI2 = 1/(2*PI)

contains

  elemental function bessel_i0 (x) result (i0)
    real(kind=double), intent(in) :: x
    real(kind=double) :: i0
    i0 = bessel_i (x, 0, .false.)
  end function bessel_i0

  elemental function bessel_i1 (x) result (i1)
    real(kind=double), intent(in) :: x
    real(kind=double) :: i1
    i1 = bessel_i (x, 1, .false.)
  end function bessel_i1

  elemental function bessel_i0_exp (x) result (i0)
    real(kind=double), intent(in) :: x
    real(kind=double) :: i0
    i0 = bessel_i (x, 0, .true.)
  end function bessel_i0_exp

  elemental function bessel_i1_exp (x) result (i1)
    real(kind=double), intent(in) :: x
    real(kind=double) :: i1
    i1 = bessel_i (x, 1, .true.)
  end function bessel_i1_exp

  elemental function bessel_k0 (x) result (i0)
    real(kind=double), intent(in) :: x
    real(kind=double) :: i0
    i0 = bessel_k (x, 0, .false.)
  end function bessel_k0

  elemental function bessel_k1 (x) result (i1)
    real(kind=double), intent(in) :: x
    real(kind=double) :: i1
    i1 = bessel_k (x, 1, .false.)
  end function bessel_k1

  elemental function bessel_k0_exp (x) result (i0)
    real(kind=double), intent(in) :: x
    real(kind=double) :: i0
    i0 = bessel_k (x, 0, .true.)
  end function bessel_k0_exp

  elemental function bessel_k1_exp (x) result (i1)
    real(kind=double), intent(in) :: x
    real(kind=double) :: i1
    i1 = bessel_k (x, 1, .true.)
  end function bessel_k1_exp

  elemental function bessel_i (x, nu, lex)
    ! implicit double precision (a-h,o-z)
    real(kind=double), intent(in) :: x
    integer, intent(in) :: nu
    logical, intent(in) :: lex
    real(kind=double) :: bessel_i
    real(kind=double), parameter :: EPS = 1.0e-15_double
    integer :: i, n
    real(kind=double) :: alfa, xl, y, e, h, r
    real(kind=double) :: w1, w2, w3, w4, w5, w6
    real(kind=double) :: fn, f1, f2, f3, u1, u2, v, v1, v2, v3
    real(kind=double) :: a, a0, a1, a2, b, b0, b1, b2, c, c0

    real(kind=double), parameter :: CI_00_0 = +1.008279205458740032_double
    real(kind=double), parameter :: CI_01_0 = +0.008445122624920943_double
    real(kind=double), parameter :: CI_02_0 = +0.000172700630777567_double
    real(kind=double), parameter :: CI_03_0 = +0.000007247591099959_double
    real(kind=double), parameter :: CI_04_0 = +0.000000513587726878_double
    real(kind=double), parameter :: CI_05_0 = +0.000000056816965808_double
    real(kind=double), parameter :: CI_06_0 = +0.000000008513091223_double
    real(kind=double), parameter :: CI_07_0 = +0.000000001238425364_double
    real(kind=double), parameter :: CI_08_0 = +0.000000000029801672_double
    real(kind=double), parameter :: CI_09_0 = -0.000000000078956698_double
    real(kind=double), parameter :: CI_10_0 = -0.000000000033127128_double
    real(kind=double), parameter :: CI_11_0 = -0.000000000004497339_double
    real(kind=double), parameter :: CI_12_0 = +0.000000000001799790_double
    real(kind=double), parameter :: CI_13_0 = +0.000000000000965748_double
    real(kind=double), parameter :: CI_14_0 = +0.000000000000038604_double
    real(kind=double), parameter :: CI_15_0 = -0.000000000000104039_double
    real(kind=double), parameter :: CI_16_0 = -0.000000000000023950_double
    real(kind=double), parameter :: CI_17_0 = +0.000000000000009554_double
    real(kind=double), parameter :: CI_18_0 = +0.000000000000004443_double
    real(kind=double), parameter :: CI_19_0 = -0.000000000000000859_double
    real(kind=double), parameter :: CI_20_0 = -0.000000000000000709_double
    real(kind=double), parameter :: CI_21_0 = +0.000000000000000087_double
    real(kind=double), parameter :: CI_22_0 = +0.000000000000000112_double
    real(kind=double), parameter :: CI_23_0 = -0.000000000000000012_double
    real(kind=double), parameter :: CI_24_0 = -0.000000000000000018_double
    real(kind=double), parameter :: CI_00_1 = +0.975800602326285926_double
    real(kind=double), parameter :: CI_01_1 = -0.024467442963276385_double
    real(kind=double), parameter :: CI_02_1 = -0.000277205360763829_double
    real(kind=double), parameter :: CI_03_1 = -0.000009732146728020_double
    real(kind=double), parameter :: CI_04_1 = -0.000000629724238640_double
    real(kind=double), parameter :: CI_05_1 = -0.000000065961142154_double
    real(kind=double), parameter :: CI_06_1 = -0.000000009613872919_double
    real(kind=double), parameter :: CI_07_1 = -0.000000001401140901_double
    real(kind=double), parameter :: CI_08_1 = -0.000000000047563167_double
    real(kind=double), parameter :: CI_09_1 = +0.000000000081530681_double
    real(kind=double), parameter :: CI_10_1 = +0.000000000035408148_double
    real(kind=double), parameter :: CI_11_1 = +0.000000000005102564_double
    real(kind=double), parameter :: CI_12_1 = -0.000000000001804409_double
    real(kind=double), parameter :: CI_13_1 = -0.000000000001023594_double
    real(kind=double), parameter :: CI_14_1 = -0.000000000000052678_double
    real(kind=double), parameter :: CI_15_1 = +0.000000000000107094_double
    real(kind=double), parameter :: CI_16_1 = +0.000000000000026120_double
    real(kind=double), parameter :: CI_17_1 = -0.000000000000009561_double
    real(kind=double), parameter :: CI_18_1 = -0.000000000000004713_double
    real(kind=double), parameter :: CI_19_1 = +0.000000000000000829_double
    real(kind=double), parameter :: CI_20_1 = +0.000000000000000743_double
    real(kind=double), parameter :: CI_21_1 = -0.000000000000000080_double
    real(kind=double), parameter :: CI_22_1 = -0.000000000000000117_double
    real(kind=double), parameter :: CI_23_1 = +0.000000000000000011_double
    real(kind=double), parameter :: CI_24_1 = +0.000000000000000019_double

    real(kind=double), dimension(0:24,0:1), parameter :: &
         CI = reshape ( (/ &
         CI_00_0, CI_01_0, CI_02_0, CI_03_0, CI_04_0, CI_05_0, &
         CI_06_0, CI_07_0, CI_08_0, CI_09_0, CI_10_0, CI_11_0, &
         CI_12_0, CI_13_0, CI_14_0, CI_15_0, CI_16_0, CI_17_0, &
         CI_18_0, CI_19_0, CI_20_0, CI_21_0, CI_22_0, CI_23_0, CI_24_0, &
         CI_00_1, CI_01_1, CI_02_1, CI_03_1, CI_04_1, CI_05_1, &
         CI_06_1, CI_07_1, CI_08_1, CI_09_1, CI_10_1, CI_11_1, &
         CI_12_1, CI_13_1, CI_14_1, CI_15_1, CI_16_1, CI_17_1, &
         CI_18_1, CI_19_1, CI_20_1, CI_21_1, CI_22_1, CI_23_1, CI_24_1 /), &
         (/ 25, 2 /) )

    v = abs(x)
    if (v < 8) then
       y = (0.5_double*v)**2
       xl = nu+2
       a0 = 1
       a1 = 1+2*y/((xl+1)*(xl-1))
       a2 = 1+y*(4+3*y/((xl+2)*xl))/((xl+3)*(xl-1))
       b0 = 1
       b1 = 1-y/(xl+1)
       b2 = 1-y*(1-y/(2*(xl+2)))/(xl+3)
       w1 = 3+xl
       v1 = 3-xl
       v3 = xl-1
       v2 = v3+v3
       c = 0
       cheby: do n = 3,30
          c0 = c
          fn = n
          w1 = w1+2
          w2 = w1-1
          w3 = w2-1
          w4 = w3-1
          w5 = w4-1
          w6 = w5-1
          v1 = v1+1
          v2 = v2+1
          v3 = v3+1
          u1 = fn*w4
          e = v3/(u1*w3)
          u2 = e*y
          f1 = 1+y*v1/(u1*w1)
          f2 = (1+y*v2/(v3*w2*w5))*u2
          f3 = -y*y*u2/(w4*w5*w5*w6)
          a = f1*a2+f2*a1+f3*a0
          b = f1*b2+f2*b1+f3*b0
          c = a/b
          if (abs(c0-c) < eps*abs(c)) then
             exit cheby
          end if
          a0 = a1
          a1 = a2
          a2 = a
          b0 = b1
          b1 = b2
          b2 = b
       end do cheby
       h = c
       if (nu == 1) then
          h = 0.5_double*x*h
       end if
       if (lex) then
          h = exp(-v)*h
       end if
    else
       r = 1/v
       h = 16*r-1
       alfa = h+h
       b1 = 0
       b2 = 0
       do i = 24, 0, -1
          b0 = ci(i,nu)+alfa*b1-b2
          b2 = b1
          b1 = b0
       end do
       h = sqrt(rpi2*r)*(b0-h*b2)
       if (nu*x < 0) then
          h = -h
       end if
       if (.not.lex) then
          h = exp(v)*h
       end if
    end if
    bessel_i = h
  end function bessel_i

  elemental function bessel_k (x, nu, lex)
    real(kind=double), intent(in) :: x
    integer, intent(in) :: nu
    logical, intent(in) :: lex
    real(kind=double) :: bessel_k
    real(kind=double), parameter :: EPS = 1.0e-15_double
    integer :: i, n
    real(kind=double) :: alfa, xn, h, r, p, q, ran
    real(kind=double) :: f, f1, f2, f3, rfn, fn, fn1, fn2, fn3, fn4, fn5, g
    real(kind=double) :: a, a0, a1, a2, b, b0, b1, b2, bk, bk1, c, c0, d

    real(kind=double), parameter :: CK_00_0 = +0.988408174230825800_double
    real(kind=double), parameter :: CK_01_0 = -0.011310504646928281_double
    real(kind=double), parameter :: CK_02_0 = +0.000269532612762724_double
    real(kind=double), parameter :: CK_03_0 = -0.000011106685196665_double
    real(kind=double), parameter :: CK_04_0 = +0.000000632575108500_double
    real(kind=double), parameter :: CK_05_0 = -0.000000045047337641_double
    real(kind=double), parameter :: CK_06_0 = +0.000000003792996456_double
    real(kind=double), parameter :: CK_07_0 = -0.000000000364547179_double
    real(kind=double), parameter :: CK_08_0 = +0.000000000039043756_double
    real(kind=double), parameter :: CK_09_0 = -0.000000000004579936_double
    real(kind=double), parameter :: CK_10_0 = +0.000000000000580811_double
    real(kind=double), parameter :: CK_11_0 = -0.000000000000078832_double
    real(kind=double), parameter :: CK_12_0 = +0.000000000000011360_double
    real(kind=double), parameter :: CK_13_0 = -0.000000000000001727_double
    real(kind=double), parameter :: CK_14_0 = +0.000000000000000275_double
    real(kind=double), parameter :: CK_15_0 = -0.000000000000000046_double
    real(kind=double), parameter :: CK_16_0 = +0.000000000000000008_double
    real(kind=double), parameter :: CK_00_1 = +1.035950858772358331_double
    real(kind=double), parameter :: CK_01_1 = +0.035465291243331114_double
    real(kind=double), parameter :: CK_02_1 = -0.000468475028166889_double
    real(kind=double), parameter :: CK_03_1 = +0.000016185063810053_double
    real(kind=double), parameter :: CK_04_1 = -0.000000845172048124_double
    real(kind=double), parameter :: CK_05_1 = +0.000000057132218103_double
    real(kind=double), parameter :: CK_06_1 = -0.000000004645554607_double
    real(kind=double), parameter :: CK_07_1 = +0.000000000435417339_double
    real(kind=double), parameter :: CK_08_1 = -0.000000000045757297_double
    real(kind=double), parameter :: CK_09_1 = +0.000000000005288133_double
    real(kind=double), parameter :: CK_10_1 = -0.000000000000662613_double
    real(kind=double), parameter :: CK_11_1 = +0.000000000000089048_double
    real(kind=double), parameter :: CK_12_1 = -0.000000000000012726_double
    real(kind=double), parameter :: CK_13_1 = +0.000000000000001921_double
    real(kind=double), parameter :: CK_14_1 = -0.000000000000000305_double
    real(kind=double), parameter :: CK_15_1 = +0.000000000000000050_double
    real(kind=double), parameter :: CK_16_1 = -0.000000000000000009_double

    real(kind=double), dimension(0:16,0:1),  parameter :: &
         CK = reshape ( (/ &
         CK_00_0, CK_01_0, CK_02_0, CK_03_0, CK_04_0, CK_05_0, &
         CK_06_0, CK_07_0, CK_08_0, CK_09_0, CK_10_0, CK_11_0, &
         CK_12_0, CK_13_0, CK_14_0, CK_15_0, CK_16_0, &
         CK_00_1, CK_01_1, CK_02_1, CK_03_1, CK_04_1, CK_05_1, &
         CK_06_1, CK_07_1, CK_08_1, CK_09_1, CK_10_1, CK_11_1, &
         CK_12_1, CK_13_1, CK_14_1, CK_15_1, CK_16_1 /), &
         (/ 17, 2 /) )

    if (x <= 0) then
       h = - huge (h)
       return
       elseif (x < 1) then
       b = 0.5_double*x
       bk = -(log(b)+ce)
       f = bk
       p = 0.5_double
       q = 0.5_double
       c = 1
       d = b**2
       bk1 = p
       cheby1: do n = 1, 15
          fn = n
          rfn = 1/fn
          p = p*rfn
          q = q*rfn
          f = (f+p+q)*rfn
          c = c*d*rfn
          g = c*(p-fn*f)
          h = c*f
          bk = bk+h
          bk1 = bk1+g
          if(bk1*h+abs(g)*bk <= eps*bk*bk1) then
             exit cheby1
          end if
       end do cheby1
       h = bk
       if(nu == 1) h = bk1/b
       if(lex) h = exp(x)*h
       elseif(x <= 5) then
       xn = 4*nu**2
       a = 9-xn
       b = 25-xn
       c = 768*x**2
       c0 = 48*x
       a0 = 1
       a1 = (16*x+7+xn)/a
       a2 = (c+c0*(xn+23)+xn*(xn+62)+129)/(a*b)
       b0 = 1
       b1 = (16*x+9-xn)/a
       b2 = (c+c0*b)/(a*b)+1
       c = 0
       cheby2: do n = 3, 30
          c0 = c
          fn = n
          fn2 = fn+fn
          fn1 = fn2-1
          fn3 = fn1/(fn2-3)
          fn4 = 12*fn**2-(1-xn)
          fn5 = 16*fn1*x
          ran = 1/((fn2+1)**2-xn)
          f1 = fn3*(fn4-20*fn)+fn5
          f2 = 28*fn-fn4-8+fn5
          f3 = fn3*((fn2-5)**2-xn)
          a = (f1*a2+f2*a1+f3*a0)*ran
          b = (f1*b2+f2*b1+f3*b0)*ran
          c = a/b
          if (abs(c0-c) < eps*abs(c)) then
             exit cheby2
          end if
          a0 = a1
          a1 = a2
          a2 = a
          b0 = b1
          b1 = b2
          b2 = b
       end do cheby2
       h = c/sqrt(rpih*x)
       if (.not.lex) then
          h = exp(-x)*h
       end if
    else
       r = 1/x
       h = 10*r-1
       alfa = h+h
       b1 = 0
       b2 = 0
       do i = 16, 0, -1
          b0 = ck(i,nu)+alfa*b1-b2
          b2 = b1
          b1 = b0
       end do
       h = sqrt(pih*r)*(b0-h*b2)
       if (.not.lex) then
          h = exp(-x)*h
       end if
    end if
    bessel_k = h
  end function bessel_k
end module specfun

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module util

  use kinds
  implicit none

  real(kind=double), private, parameter :: PI = 3.14159265358979324_double

contains

  function gauss (f, a, b, eps) result (integral)
    real(kind=double), intent(in) :: a, b, eps
    real(kind=double) :: integral
    interface
       elemental function f (x) result (f_val)
         use kinds
         real(kind=double), intent(in) :: x
         real(kind=double) :: f_val
       end function f
    end interface
    real(kind=double) :: aa, bb, sum8, sum16
    real(kind=double) :: midpoint, half_width, min_half_width
    real(kind=double), dimension(4) :: u8
    real(kind=double), dimension(4), parameter :: X8 = &
         (/ 9.6028985649753623e-1_double, &
         7.9666647741362674e-1_double, &
         5.2553240991632899e-1_double, &
         1.8343464249564980e-1_double /)
    real(kind=double), dimension(4), parameter :: W8 = &
         (/ 1.0122853629037626e-1_double, &
         2.2238103445337447e-1_double, &
         3.1370664587788729e-1_double, &
         3.6268378337836198e-1_double /)
    real(kind=double), dimension(8) :: u16
    real(kind=double), dimension(8), parameter :: X16 = &
         (/ 9.8940093499164993e-1_double, &
         9.4457502307323258e-1_double, &
         8.6563120238783174e-1_double, &
         7.5540440835500303e-1_double, &
         6.1787624440264375e-1_double, &
         4.5801677765722739e-1_double, &
         2.8160355077925891e-1_double, &
         9.5012509837637440e-2_double /)
    real(kind=double), dimension(8), parameter :: W16 = &
         (/ 2.7152459411754095e-2_double, &
         6.2253523938647893e-2_double, &
         9.5158511682492785e-2_double, &
         1.2462897125553387e-1_double, &
         1.4959598881657673e-1_double, &
         1.6915651939500254e-1_double, &
         1.8260341504492359e-1_double, &
         1.8945061045506850e-1_double /)
    integral = 0
    if (b == a) then
       return
    end if
    min_half_width = (1 + 200 * abs (b - a)) * epsilon (min_half_width)
    aa = a
    bb = b
    do
       midpoint = (bb + aa) / 2
       half_width = (bb - aa) / 2
       u8 =  half_width * X8
       sum8 = half_width * sum (W8 * (f (midpoint + u8) + f (midpoint - u8)))
       u16 =  half_width * X16
       sum16 = half_width * sum (W16 * (f (midpoint + u16) + f (midpoint - u16)))
       if (abs (sum16 - sum8) > eps * (1 + abs (sum16))) then
          ! not accurate enough, try to subdivide
          if (abs (half_width) < min_half_width) then
             ! too high accuracy required, bail out
             integral = - huge (integral)
             return
          end if
          bb = midpoint
       else
          ! accuracy reached, accept the partial integral
          integral = integral + sum16
          if (bb == b) then
             ! endpoint reached, accept the integral
             return
          end if
          aa = bb
          bb = b
       end if
    end do
  end function gauss

  pure subroutine solve_quadratic (z, a)
    complex(kind=double), dimension(2), intent(out) :: z
    real(kind=double), dimension(0:2), intent(in) :: a
    real(kind=double) :: d
    complex(kind=double) :: q
    d = a(1)**2 - 4*a(0)*a(2)
    if (d >= 0) then
       q = - (a(1) + sign (sqrt (d), a(1))) / 2
    else
       q = - cmplx (a(1), sign (sqrt (-d), a(1))) / 2
    end if
    z(1) = q / a(2)
    z(2) = a(0) / q
  end subroutine solve_quadratic

  pure subroutine solve_cubic (z, a0)
    complex(kind=double), dimension(3), intent(out) :: z
    real(kind=double), dimension(0:3), intent(in) :: a0
    real(kind=double), dimension(0:3) :: a
    real(kind=double) :: q, sqrt_q, r, r2q3, theta, s1, s2
    a = a0 / a0(3)
    q = (a(2)**2 - 3*a(1)) / 9
    r = (2*a(2)**3 - 9*a(1)*a(2) + 27*a(0)) / 54
    r2q3 = r**2 - q**3
    if (r2q3 < 0) then
       sqrt_q = sqrt (q)
       theta = acos (r / sqrt_q**3)
       z(1) = - 2 * sqrt_q * cos (theta / 3) - a(2) / 3
       z(2) = - 2 * sqrt_q * cos ((theta + 2*PI) / 3) - a(2) / 3
       z(3) = - 2 * sqrt_q * cos ((theta - 2*PI) / 3) - a(2) / 3
    else
       s1 = - sign ((abs (r) + sqrt (r2q3)) ** (1.0_double / 3), r)
       if (s1 == 0) then
          s2 = 0
       else
          s2 = q / s1
       endif
       z(1) = s1 + s2 - a(2) / 3
       z(2) = - (s1 + s2) / 2 - a(2) / 3 &
            + (s1 - s2) * cmplx (0, sqrt (3.0_double) / 2)
       z(3) = - (s1 + s2) / 2 - a(2) / 3 &
            - (s1 - s2) * cmplx (0, sqrt (3.0_double) / 2)
    end if
  end subroutine solve_cubic

end module util

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module gg

  use kinds
  use util
  use specfun
  implicit none

  real(kind=double), parameter ::  m_e = 511.0e-6_double
  real(kind=double), private, parameter :: PI = 3.14159265358979324_double

  type, private :: lumi_closure
     real(kind=double) :: x, z, y_max, rho, pol1, pol2
  end type lumi_closure
  type(lumi_closure), private :: cl

contains

  elemental function x_of_kinematics (e_gamma, e_e, alpha) result (x)
    real(kind=double), intent(in) :: e_gamma, e_e, alpha
    real(kind=double) :: x
    x = 4 * e_gamma * e_e * cos(alpha/2)**2 / m_e**2
  end function x_of_kinematics

  elemental function theta0_of_kinematics (e_gamma, e_e, alpha) result (theta0)
    real(kind=double), intent(in) :: e_gamma, e_e, alpha
    real(kind=double) :: theta0
    real(kind=double) :: x
    x = x_of_kinematics (e_gamma, e_e, alpha)
    theta0 = m_e / e_e * sqrt (1 + x)
  end function theta0_of_kinematics

  elemental function y_of_theta (theta, theta0, y_max) result (y)
    real(kind=double), intent(in) :: theta, theta0, y_max
    real(kind=double) :: y
    y = y_max / (1 + (theta/theta0)**2)
  end function y_of_theta

  elemental function sigma (x, pol) result (s)
    real(kind=double), intent(in) :: x, pol
    real(kind=double) :: s
    real(kind=double) :: s0, s1
    s0 = (2 / x) * ((1 - 4/x - 8/x**2) * log (x + 1) &
         + 0.5_double + 8/x - 0.5_double / (x + 1)**2)
    s1 = (2 / x) * ((1 + 2/x) * log (x + 1) &
         - 2.5_double + 1/(x+1) - 0.5_double / (x + 1)**2)
    s = s0 + pol*s1
  end function sigma

  elemental function dsigma_dy (y, x, pol) result (s)
    real(kind=double), intent(in) :: y, x, pol
    real(kind=double) :: s
    real(kind=double) :: y_max, r
    y_max = x / (1 + x)
    if ((y > y_max) .or. (y < 0)) then
       s = 0
    else
       r = y / (x * (1 - y))
       s = (2 / x) * (1 / (1-y) + (1-y) - 4*r*(1-r) + pol*r*x*(1-2*r)*(2-y))
       !!! s = s + (2 / x) * y**(-0.9_double)
    end if
  end function dsigma_dy

  elemental function polarization (y, x, pol_gamma, pol_e) result (p)
    real(kind=double), intent(in) :: y, x, pol_gamma, pol_e
    real(kind=double) :: p
    real(kind=double) :: y_max, r
    y_max = x / (1 + x)
    if ((y > y_max) .or. (y < 0)) then
       p = 0
    else
       r = y / (x * (1 - y))
       p = (pol_e * x * r * (1 + (1-y) * (1-2*r)**2) &
            + pol_gamma * (1-2*r) * (1/(1-y) + (1-y))) &
              / (1 / (1-y) + (1-y) - 4*r*(1-r) &
                   + pol_gamma*pol_e*r*x*(1-2*r)*(2-y))
    end if
  end function polarization

  elemental function dsigma_dy_pol (y, x, pol, pol_gamma, pol_e) result (s)
    real(kind=double), intent(in) :: y, x
    integer, intent(in) :: pol
    real(kind=double), intent(in) :: pol_gamma, pol_e
    real(kind=double) :: s
    real(kind=double) :: y_max, s_unpol
    y_max = x / (1 + x)
    if ((y > y_max) .or. (y < 0)) then
       s = 0
    else
       s_unpol = dsigma_dy (y, x, pol_gamma*pol_e)
       select case (pol)
       case (1)
          s = (1 + polarization (y, x, pol_gamma, pol_e)) / 2 * s_unpol
       case (0)
          s = s_unpol
       case (-1)
          s = (1 - polarization (y, x, pol_gamma, pol_e)) / 2 * s_unpol
       case default
          s = 0
       end select
    end if
  end function dsigma_dy_pol

  subroutine generate_dsigma_dy (y, x, pol)
    real(kind=double), intent(out) :: y
    real(kind=double), intent(in), optional :: pol, x
    real(kind=double), save :: x_saved = -1, pol_saved = -1, &
         y_max = -1, p_plus = -1, w_plus = -1, &
         w0 = -1, w2 = -1, w2_int = -1
    real(kind=double) :: r0, w, s0, s1, c0, c1, int_plus, int_minus
    real(kind=double), dimension(2) :: r
    real(kind=double), dimension(0:2) :: a2
    complex(kind=double), dimension(2) :: z2
    real(kind=double), dimension(0:3) :: a3
    complex(kind=double), dimension(3) :: z3
    if (present (x) .and. present (pol)) then
       x_saved = x
       pol_saved = pol
       y_max = x / (x + 1)
       !!! find maximum for pol = +1
       a3(0) = 4 * x * (2 - x)        ! a3(0) = 4 * x * (2 - x * pol)
       a3(1) = 4 * ((x + 2) * x - 4)  ! a3(1) = 4 * ((2 * pol - 1) * (x + 2) * x - 4)
       a3(2) = - 12 * x               ! a3(2) = - 6 * x * ((x + 2) * pol - x)
       a3(3) = 4 * x                  ! a3(3) = 2 * x * ((x + 2) * pol - x)
       call solve_cubic (z3, a3)
       w_plus = dsigma_dy (real (z3(1), kind=double), x, 1.0_double)
       !!! find simple envelope for pol = -1
       s0 = dsigma_dy (0.0_double, x, -1.0_double)
       s1 = dsigma_dy (y_max - 2 * epsilon (y_max), x, -1.0_double)
       c1 = (s1 - s0) / (1 / (1 - y_max)**2 - 1)
       c0 = s0 - c1
       w0 = c0
       w2 = c1 / w0
       w2_int = y_max * (1 + w2 / (1 - y_max))
       !!! find fraction of pol = +1 events
       int_plus = sigma (x, 1.0_double)
       int_minus = sigma (x, -1.0_double)
       p_plus = (1 + pol) * int_plus &
                  / ((1 + pol) * int_plus + (1 - pol) * int_minus)
    end if
    call random_number (r0)
    if (r0 <= p_plus) then
       rejection_plus: do
          call random_number (r)
          y = r(1)
          w = dsigma_dy (y, x_saved, 1.0_double) / w_plus
          if (r(2) < w) then
             return
          end if
       end do rejection_plus
    else
       rejection_minus: do
          call random_number (r)
          a2(0) = w2_int * r(1)
          a2(1) = - (1 + w2 + w2_int * r(1))
          a2(2) = 1
          call solve_quadratic (z2, a2)
          y = real (z2(2))
          if ((y > y_max) .or. (y < 0)) then
             y = - huge (y)
             return
          end if
          w = dsigma_dy (y, x_saved, -1.0_double) / w0 / (1 + w2 / (1 - y)**2)
          if (w > 1) then
             print *, y, w
          end if
          if (r(2) < w) then
             return
          end if
       end do rejection_minus
    end if
  end subroutine generate_dsigma_dy

  subroutine generate_d2lumi_dy1dy2 (y1, y2, rho, x, pol1, pol2)
    real(kind=double), intent(out) :: y1, y2
    real(kind=double), intent(in) :: rho
    real(kind=double), intent(in), optional :: x, pol1, pol2
    real(kind=double) :: r, w, y_max
    real(kind=double), save :: x_saved = -1, pol1_saved, pol2_saved
    real(kind=double), save :: w_max = -1, w_sum = 0
    integer, save :: w_count = 0
    if (present (x) .and. present (pol1) .and. present (pol2)) then
       x_saved = x
       y_max = x / (x + 1)
       pol1_saved = pol1
       pol2_saved = pol2
       w_max = d2bessel_dy1dy2 (y_max, y_max, x, rho)
       call generate_dsigma_dy (y1, x, pol1)
       call generate_dsigma_dy (y2, x, pol2)
       if (w_count > 0) then
          print *, "eff = ", w_sum / w_count, w_count
       end if
    end if
    rejection: do
       call generate_dsigma_dy (y1)
       call generate_dsigma_dy (y2)
       w = d2bessel_dy1dy2 (y1, y2, x_saved, rho) / w_max
       if (w > 1) then
          print *, "w>1: ", real(y1), real(y2), real(w)
          return
       end if
       if (w /= w) then
          print *, "NAN: ", real(y1), real(y2), real(w)
          return
       end if
       w_count = w_count + 1
       w_sum = w_sum + w
       ! print *, real(y1), real(y2), real(w), real(w_sum), w_count
       call random_number (r)
       if (r < w) then
          return
       end if
    end do rejection
  end subroutine generate_d2lumi_dy1dy2

  elemental function dsigma_dy_a (y, x, pol) result (s)
    real(kind=double), intent(in) :: y, x, pol
    real(kind=double) :: s
    real(kind=double) :: y_max, s0, s1, c0, c1
    y_max = x / (1 + x)
    if ((y > y_max) .or. (y < 0)) then
       s = 0
    else
       s0 = dsigma_dy (0.0_double, x, pol)
       s1 = dsigma_dy (y_max - 2 * epsilon (y_max), x, pol)
       c1 = (s1 - s0) / (1 / (1 - y_max)**2 - 1)
       c0 = s0 - c1
       s = c0 + c1 / (1 - y)**2
    end if
  end function dsigma_dy_a

  elemental function dsigma_dy_b (y, x, pol) result (s)
    real(kind=double), intent(in) :: y, x, pol
    real(kind=double) :: s
    real(kind=double) :: y_max, y_extreme
    complex(kind=double), dimension(3) :: z
    real(kind=double), dimension(0:3) :: a
    y_max = x / (1 + x)
    if ((y > y_max) .or. (y < 0)) then
       s = 0
    else
       a(0) = 4 * x * (2 - x * pol)
       a(1) = 4 * ((2 * pol - 1) * (x + 2) * x - 4)
       a(2) = - 6 * x * ((x + 2) * pol - x)
       a(3) = 2 * x * ((x + 2) * pol - x)
       call solve_cubic (z, a)
       y_extreme = real (z(1), kind=double)
       s = dsigma_dy (y_extreme, x, pol)
    end if
  end function dsigma_dy_b

  elemental function dsigma_dtheta (theta, theta0, x, pol) result (s)
    real(kind=double), intent(in) :: theta, theta0, x, pol
    real(kind=double) :: s
    real(kind=double) :: y, y_max
    y_max = x / (1 + x)
    y = y_of_theta (theta, theta0, y_max)
    if ((y > y_max) .or. (y < 0)) then
       s = 0
    else
       s = y_max * dsigma_dy (y, x, pol) &
            / (PI * theta0**2 * (1 + (theta/theta0)**2)**2)
    end if
  end function dsigma_dtheta

!!! gfortran does not allow this function to be elemental (?)
  function d2lumi_dzdy (y) result (l)
!!!  elemental function d2lumi_dzdy (y) result (l)
    real(kind=double), intent(in) :: y
    real(kind=double) :: l
    real(kind=double) :: rho2, xi1, xi2
    rho2 = cl%rho**2 * (cl%x + 1)
    xi1 = cl%y_max / y - 1
    xi2 = cl%y_max * y / cl%z**2 - 1
    l = dsigma_dy (y, cl%x, cl%pol1) * dsigma_dy (cl%z**2/y, cl%x, cl%pol2) &
         * bessel_i0 (rho2 * sqrt (xi1 * xi2)) &
         * exp (- rho2/2 * (xi1 + xi2)) / y
  end function d2lumi_dzdy

  elemental function d2lumi_dy1dy2 (y1, y2, rho, x, pol1, pol2) result (l)
    real(kind=double), intent(in) :: y1, y2, rho, x, pol1, pol2
    real(kind=double) :: l
    real(kind=double) :: y_max, rho2, xi1, xi2
    y_max = x / (1 + x)
    rho2 = rho**2 * (x + 1)
    xi1 = y_max / y1 - 1
    xi2 = y_max / y2 - 1
    l = dsigma_dy (y1, x, pol1) * dsigma_dy (y2, x, pol2) &
         * d2bessel_dy1dy2 (y1, y2, x, rho)
  end function d2lumi_dy1dy2

  elemental function d2bessel_dy1dy2 (y1, y2, x, rho) result (l)
    real(kind=double), intent(in) :: y1, y2, x, rho
    real(kind=double) :: l
    real(kind=double) :: y_max, rho2, xi1, xi2
    y_max = x / (1 + x)
    if ((y1 > y_max) .or. (y2 > y_max)) then
       l = 0
    else
       rho2 = rho**2 * (x + 1)
       xi1 = y_max / y1 - 1
       xi2 = y_max / y2 - 1
       l = bessel_i0_exp (rho2 * sqrt (xi1 * xi2)) &
            * exp (rho2 * (sqrt (xi1 * xi2) - (xi1 + xi2) / 2))
    end if
  end function d2bessel_dy1dy2

  elemental function d2bessel_dy1dy2_broken (y1, y2, x, rho) result (l)
    real(kind=double), intent(in) :: y1, y2, x, rho
    real(kind=double) :: l
    real(kind=double) :: y_max, rho2, xi1, xi2
    y_max = x / (1 + x)
    if ((y1 > y_max) .or. (y2 > y_max)) then
       l = 0
    else
       rho2 = rho**2 * (x + 1)
       xi1 = y_max / y1 - 1
       xi2 = y_max / y2 - 1
       l = bessel_i0 (rho2 * sqrt (xi1 * xi2)) * exp (- rho2/2 * (xi1 + xi2))
    end if
  end function d2bessel_dy1dy2_broken

  function dlumi_dz (z, x, rho, pol) result (l)
    real(kind=double), intent(in) :: z, x, rho, pol
    real(kind=double) :: l
    cl%z = z
    cl%x = x
    cl%rho = rho
    cl%pol1 = pol
    cl%pol2 = pol
    cl%y_max = x / (1 + x)
    l = z * gauss (d2lumi_dzdy, z**2 / cl%y_max, cl%y_max, 1e-8_double)
  end function dlumi_dz

end module gg

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program test
  use kinds
  use gg
  use histograms
  implicit none
  real(kind=double) :: z, y, y1, y2, x, x1, x2
  integer :: i, i1, i2, irho, ipol, ipol1, ipol2
  real(kind=double) :: e_gamma, e_e, rho, pol, pol1, pol2
  character(len=*), dimension(-1:1), parameter :: &
       tag_pol = (/ "minus", "unpol", "plus " /)
  character(len=1) :: tag_rho
  character(len=2) :: cmd
  type(histogram) :: h_unweighted(-1:1), h_reweighted(-1:1)
  type(histogram2) :: h_unweighted2, h_reweighted2
  e_gamma = 1.25e-9_double
  e_e =  250.0_double
  ! e_e =  1000.0_double
  ! e_e =  100.0_double
  x = x_of_kinematics (e_gamma, e_e, 0.0_double)
  print *, x
  read (*, fmt="(A)") cmd
  select case (cmd)
  case ("po")
     do ipol1 = -1, 1
        do ipol2 = -1, 1
           open (10, file = trim (tag_pol(ipol1)) // "_" // trim (tag_pol(ipol2)))
           do i = 0, 1000
              y = real (i, kind=double) / 1000
              write (10, *) y, polarization (y, x, &
                   real (ipol1, kind=double), &
                   real (ipol2, kind=double))
           end do
           close (10)
        end do
     end do
  case ("co")
     do ipol = -1, 1
        open (10, file = trim (tag_pol(ipol)))
        do i = 0, 1000
           y = real (i, kind=double) / 1000
           write (10, *) y, dsigma_dy (y, x, real (ipol, kind=double))
        end do
        close (10)
     end do
  case ("lu")
     do irho = 0, 9
        rho = irho * 0.2_double
        write (tag_rho, "(i1)") irho
        do ipol = -1, 1
           open (10, file = trim (tag_pol(ipol)) // tag_rho)
           do i = 0, 1000
              z = real (i, kind=double) / 10000
              write (10, *) z, dlumi_dz (z, x, rho, real (ipol, kind=double))
           end do
           do i = 100, 1000
              z = real (i, kind=double) / 1000
              write (10, *) z, dlumi_dz (z, x, rho, real (ipol, kind=double))
           end do
           close (10)
        end do
     end do
  case ("2d")
     do irho = 0, 9
        rho = irho
        write (tag_rho, "(i1)") irho
        do ipol = -1, 1
           open (10, file = trim (tag_pol(ipol)) // tag_rho)
           do i1 = 0, 100
              y1 = real (i1, kind=double) / 100
              do i2 = 0, 100
                 y2 = real (i2, kind=double) / 100
                 write (10, *) y1, y2, &
                      d2lumi_dy1dy2 (y1, y2, rho, x, &
                                     real (ipol, kind=double), &
                                     real (ipol, kind=double))
              end do
           end do
           close (10)
        end do
     end do
  case ("2b")
     do irho = 0, 9
        rho = irho
        write (tag_rho, "(i1)") irho
        open (10, file = "bessel" // tag_rho)
        do i1 = 0, 100
           y1 = real (i1, kind=double) / 100
           do i2 = 0, 100
              y2 = real (i2, kind=double) / 100
              write (10, *) y1, y2, d2bessel_dy1dy2 (y1, y2, x, rho)
           end do
        end do
        close (10)
     end do
  case ("xx")
     do ipol = -1, 1
        open (10, file = trim (tag_pol(ipol)))
        open (11, file = trim (tag_pol(ipol)) // "a")
        open (12, file = trim (tag_pol(ipol)) // "b")
        ! open (13, file = trim (tag_pol(ipol)) // "c")
        do i = 0, 1000
           y = real (i, kind=double) / 1000
           write (10, *) y, dsigma_dy (y, x, real (ipol, kind=double))
           write (11, *) y, dsigma_dy_a (y, x, real (ipol, kind=double))
           z = dsigma_dy_b (y, x, real (ipol, kind=double))
           write (12, *) y, z
           ! write (13, *) y, dsigma_dy_c (y, x, real (ipol, kind=double))
        end do
        close (10)
        close (11)
        close (12)
        ! close (13)
     end do
  case ("mc")
     call create_histogram (h_unweighted, 0.0_double, 1.0_double, 100)
     call create_histogram (h_reweighted, 0.0_double, 1.0_double, 100)
     call generate_dsigma_dy (y, x, pol)
     do i = 0, 10000000
        call generate_dsigma_dy (y)
        call fill_histogram (h_unweighted(int(pol)), y)
        call fill_histogram (h_reweighted(int(pol)), y, 1 / dsigma_dy (y, x, pol))
        call fill_histogram (h_unweighted(0), y)
        call fill_histogram (h_reweighted(0), y, 1 / dsigma_dy (y, x, 0.0_double))
     end do
     do ipol = -1, 1
        call write_histogram (h_unweighted(ipol), trim (tag_pol(ipol)))
        call write_histogram (h_reweighted(ipol), trim (tag_pol(ipol)) // "x")
     end do
  case ("m2")
     rho = 1.5
     read *, rho
     call create_histogram (h_unweighted(0), 0.0_double, 1.0_double, 50)
     call create_histogram (h_reweighted(0), 0.0_double, 1.0_double, 50)
     call create_histogram (h_unweighted2, &
          (/ 0.0_double, 0.0_double /), (/ 1.0_double, 1.0_double /), &
          (/ 20, 20 /) )
     call create_histogram (h_reweighted2, &
          (/ 0.0_double, 0.0_double /), (/ 1.0_double, 1.0_double /), &
          (/ 20, 20 /) )
     call generate_d2lumi_dy1dy2 (y1, y2, rho, x, pol1, pol2)
     do i = 0, 100000! 00
        call generate_d2lumi_dy1dy2 (y1, y2, rho)
        call fill_histogram (h_unweighted(0), sqrt (y1*y2))
        call fill_histogram (h_reweighted(0), sqrt (y1*y2), &
             1 / dlumi_dz (sqrt (y1*y2), x, rho, 0.0_double))
        call fill_histogram (h_unweighted2, y1, y2)
        call fill_histogram (h_reweighted2, y1, y2, &
             1 / d2lumi_dy1dy2 (y1, y2, rho, x, 0.0_double, 0.0_double))
     end do
     call write_histogram (h_unweighted(0), "lumi")
     call write_histogram (h_reweighted(0), "lumix")
     call write_histogram (h_unweighted2, "lumi2")
     call write_histogram (h_reweighted2, "lumi2x")
     call generate_d2lumi_dy1dy2 (y1, y2, rho, x, pol1, pol2)
  end select
end program test

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
