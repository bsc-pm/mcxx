! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
subroutine convolut_kinetic_per_c(n3)
  implicit none
  integer :: n3

  call conv_kin_z()

  contains

  subroutine conv_kin_z()
    implicit none

   !$omp parallel
   if (n3 /= 12) STOP 1
   !$omp end parallel

  END SUBROUTINE conv_kin_z
END SUBROUTINE convolut_kinetic_per_c


PROGRAM MAIN
    interface
    subroutine convolut_kinetic_per_c(n3)
      implicit none
      integer :: n3
    end subroutine convolut_kinetic_per_c
    end interface

    call convolut_kinetic_per_c(12)
END PROGRAM MAIN
