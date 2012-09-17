! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM bsc_cg

  IMPLICIT NONE

     real    :: c, r(10), z(10)
     integer :: ii

     
     c       = 0.0
     do ii = 1, 10
        z(ii) = ii
        r(ii) = 10-ii
     enddo

!$OMP PARALLEL DO                                          &
!$OMP SCHEDULE  (STATIC)                                   &
!$OMP DEFAULT   (NONE)                                     &
!$OMP PRIVATE   (ii)                                       &
!$OMP SHARED    (r, z)                                     &
!$OMP REDUCTION (+: c)
     do ii = 1, 10
        c      = c + (z(ii) * r(ii))
     enddo

     IF (ABS(C - 165.00) >= 1e-5) THEN
         PRINT *, C, " /= " , 165.00
         STOP 1
     END IF

END PROGRAM

