! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
MODULE M
    CONTAINS

        SUBROUTINE S(X, Y, P)
            INTEGER, INTENT(IN):: X
            LOGICAL, INTENT(IN) :: P
            INTEGER, DIMENSION(:), INTENT(OUT), OPTIONAL :: Y

            !$OMP DO SHARED(X, Y, P) PRIVATE(I)
            DO I = 1, 1
                IF (PRESENT(y) /= P) THEN
                    STOP 1
                END IF
           ENDDO
           !$OMP END DO
        END SUBROUTINE S

END MODULE M

PROGRAM MAIN
    USE M

    INTEGER, DIMENSION(:), ALLOCATABLE :: Phi
    INTEGER :: X

     allocate( Phi(2:10) )
     Phi = 0

    CALL S(X, P=.FALSE.)
    CALL S(X,Phi, P=.TRUE.)
END PROGRAM MAIN
