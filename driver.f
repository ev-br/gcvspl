      implicit none

      integer :: N=10    ! number of data points
      integer :: K=1     ! number of datasets
      integer :: NY      ! number of Y values (=N)

      real*8, allocatable :: x(:), y(:, :), wx(:), wy(:)

      integer :: M, MD
      real*8  :: val              ! real*8 = double precision = C double type

      real*8, allocatable  :: c(:, :)
      integer :: NC

      real*8, allocatable :: wk(:)
      integer :: iwk
      integer :: ier

      ! work variables
      integer :: i, j

      allocate( x(1:N), y(1:N, 1:K), wx(1:N), wy(1:K))

      NY = N
      NC = N
      allocate( c(1:NC, 1:K) )

      M = 2   ! cubic spline

      iwk = 6 * (N*M + 1) + N + 10
      allocate(wk(1:iwk))
    
      !!!!!! input data, trivial

      do i=1, N
          x(i) = i
          wx(i) = 1.d0    ! d0 = 'double precision'
          do j=1, K
             y(i, j) = x(i)
             wy(j) = 1.d0
          enddo
      enddo

      !!!!! call gcvspl
      MD = 1
      val = 2
      call gcvspl(x, y, NY, wx, wy, M, N, K, MD, val, c, NC, wk, ier)

      print*, 'val = ', val, ier
      print*
      print*, c

      ! print the array, nicely
      do j=1, NC
         print*, j, c(j, 1)
      enddo


      end
