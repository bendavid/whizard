      program taotst
      implicit none
      integer i, N, S
      double precision r
      double precision sum30
      call taornt ()
      S = 0
      N = 10000000
      sum30 = 0
      call taorns (S)
      do 10 i = 1, N
         call taornu (r)
         sum30 = sum30 + r
 10   continue
      print *, 'sum30 = ', sum30
      end
