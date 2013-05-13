! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
subroutine apply_potential(nspinor, psir)
  implicit none
  integer, intent(in) :: nspinor
  integer(4), dimension(nspinor), intent(inout) :: psir

  psir = 1

!$omp parallel default(none)&
!$omp shared(psir, nspinor)
    psir(:) = nspinor
!$OMP End parallel

    IF ( ANY(psir /= nspinor) ) STOP 1

end subroutine apply_potential

PROGRAM MAIN
  implicit none

    INTERFACE
subroutine apply_potential(nspinor, psir)
  implicit none
  integer, intent(in) :: nspinor
  integer(4), dimension(nspinor), intent(inout) :: psir
  end subroutine apply_potential
  END INTERFACE

  INTEGER :: X
  integer :: PSIR(100)

  X = 100

  CALL apply_potential(X, PSIR)

    IF ( ANY(psir /= X) ) STOP 1
END PROGRAM MAIN
