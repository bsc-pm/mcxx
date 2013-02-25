! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM MAIN
        TYPE T
                INTEGER, DIMENSION(20) :: A(10)
                INTEGER, POINTER, DIMENSION(:) :: B(:, :)
        END TYPE T

        TYPE(T) :: S

        ALLOCATE(S % B(10, 20))

        IF (SIZE(S%A, DIM=1) /= 10) STOP 1

        DEALLOCATE(S % B)
END PROGRAM MAIN
