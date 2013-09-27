! <testinfo>
! test_generator=config/mercurium-omp
! test_compile_fail=yes
! test_compile_faulty=yes
! </testinfo>
SUBROUTINE FOO
   IMPLICIT NONE
   INTEGER :: i, loc

   !$OMP PARALLEL DO SHARED(loc)
   do i = 1, 10
      loc = 1
   end do
  !$OMP END PARALLEL DO
END SUBROUTINE FOO

PROGRAM MAIN
    CALL FOO
END PROGRAM MAIN
