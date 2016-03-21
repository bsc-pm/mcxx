! <testinfo>
! test_generator="config/mercurium-ompss"
! compile_versions="nanox_mercurium_mod0 nanox_mercurium_mod1"
! test_FFLAGS_nanox_mercurium_mod0="-DMOD0"
! test_FFLAGS_nanox_mercurium_mod1="-DMOD1"
! test_compile_fail_nanox_mercurium_mod0_nanos6_mercurium=yes
! test_compile_fail_nanox_mercurium_mod1_nanos6_mercurium=yes
! test_compile_fail_nanox_mercurium_mod0_nanos6_imfc=yes
! test_compile_fail_nanox_mercurium_mod1_nanos6_imfc=yes
! test_compile_fail_nanox_mercurium_mod1_nanox_imfc=yes
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
