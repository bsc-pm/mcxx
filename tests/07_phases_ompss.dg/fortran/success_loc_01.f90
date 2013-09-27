! <testinfo>
! test_generator=config/mercurium-ompss
! test_compile_fail=yes
! test_compile_faulty=yes
! </testinfo>
SUBROUTINE FOO
   IMPLICIT NONE
   INTEGER :: i, loc

   !$OMP DO SHARED(loc)
   do i = 1, 10
      loc = 1
   end do
  !$OMP END DO
END SUBROUTINE FOO

PROGRAM MAIN
    CALL FOO
END PROGRAM MAIN
