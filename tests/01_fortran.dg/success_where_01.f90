! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    REAL(8) :: ustarw(100)
    REAL(8) :: cmw(100)
    REAL(8) :: cfncw(100)
    REAL(8) :: cfmw(100)
    REAL(8) :: cdnw(100)
    REAL(8) :: wind(100)

    REAL(8) :: u(100), v(100)

    wind(:)=SQRT(u(:)*u(:)+v(:)*v(:))

    WHERE (ABS(cfncw(:))> TINY(cfncw(1)))
        cmw(:)=cdnw(:)*cfmw(:)/cfncw(:)
    ELSEWHERE
        cmw(:)=0.
    ENDWHERE

    ustarw(:)=SQRT(cmw(:))*wind(:)
END PROGRAM MAIN
