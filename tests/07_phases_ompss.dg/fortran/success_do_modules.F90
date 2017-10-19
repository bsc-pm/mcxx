! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! test_compile_fail_nanos6_mercurium_mod0=yes
! test_compile_fail_nanos6_mercurium_mod1=yes
! test_compile_fail_nanos6_imfc_mod0=yes
! test_compile_fail_nanos6_imfc_mod1=yes
! compile_versions="mod0 mod1"
! test_FFLAGS_mod0="-DMOD0"
! test_FFLAGS_mod1="-DMOD1"
! </testinfo>

#ifdef MOD0
MODULE M1
CONTAINS

   SUBROUTINE FOO

      IMPLICIT NONE
      INTEGER :: i

      !$OMP DO PRIVATE(i)
      do i = 1, 10
      end do
      !$OMP END DO

   END SUBROUTINE FOO
END MODULE M1
#endif

#ifdef MOD1
MODULE M2
CONTAINS

   SUBROUTINE BAR

      USE M1
      IMPLICIT NONE
      INTEGER :: i

      !$OMP DO PRIVATE(i)
      do i = 1, 10
      end do
      !$OMP END DO

   END SUBROUTINE BAR

END MODULE M2
#endif

PROGRAM MAIN
    ! Dummy
END PROGRAM MAIN
