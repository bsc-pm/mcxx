! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    TYPE :: T
        INTEGER :: A(10)
    END TYPE 

    TYPE(T) :: S

    PRINT *, S % A(1)
END PROGRAM P
