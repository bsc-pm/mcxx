! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM P
    IMPLICIT NONE

    TYPE  T1
        INTEGER, POINTER :: PX(:)
    END TYPE T1

    INTEGER, PARAMETER :: V(1:2) = (/ 1, 1 /)

    TYPE(T1) :: VAR

    ALLOCATE(VAR % PX(2))
    VAR % PX = V
    DEALLOCATE(VAR % PX)
END PROGRAM P
