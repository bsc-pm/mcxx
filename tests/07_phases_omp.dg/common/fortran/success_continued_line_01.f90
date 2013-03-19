! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
program p
integer :: j
integer :: a(10), b(10)
a = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /)
b = a + 1
!$omp parallel do &
!$omp& private(j, b)
      DO j=1,10
        a(j) = a(j) + 1
        b(j) = 44
      END DO
!$omp end parallel do

if (any(a /= b)) stop 1

end program p
