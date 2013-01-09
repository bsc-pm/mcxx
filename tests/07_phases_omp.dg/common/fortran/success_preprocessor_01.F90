! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM bsc_cg

  IMPLICIT NONE

     integer :: ii, cc = 0

#ifdef _OPENMP
         INTEGER     :: jj = 2
#endif

!$OMP PARALLEL DO                                                         &
!$OMP SCHEDULE (STATIC)                                                   &
!$OMP DEFAULT  (NONE)                                                     &
!$OMP PRIVATE  (ii)                                                       &
!$OMP SHARED   (jj, cc)
      DO ii = 1, 10
#ifdef _OPENMP
!$OMP CRITICAL
         cc = cc + jj
!$OMP END CRITICAL
#endif
     ENDDO

     write (*,*) cc

     if (CC /= 20) THEN
         STOP 1
     END IF

#ifndef _OPENMP
    STOP 1
#endif

END PROGRAM

