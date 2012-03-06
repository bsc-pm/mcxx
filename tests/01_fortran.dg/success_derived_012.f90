! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    TYPE T
        INTEGER :: X
        INTEGER, POINTER :: PX
        INTEGER :: A(10)
        INTEGER, POINTER :: PA(:)
    END TYPE T

    INTEGER, TARGET :: TX
    TYPE(T) :: S
    TYPE(T), TARGET :: TS
    TYPE(T), POINTER  :: PS

    INTEGER, TARGET :: TA(10)

    S % X = 1

    S % PX => TX
    S % PX = 1

    S % A(1) = 1
    S % A(1:2) = 1

    S % A = 1
    S % A(1) = 1
    S % A(1:1) = 1

    S % PA => TA
    S % PA = 1
    S % PA(1) = 1
    S % PA(1:1) = 1

    PS => TS

    PS % X = 1

    PS % PX => TX
    PS % PX = 1

    PS % A(1) = 1
    PS % A(1:2) = 1

    PS % A = 1
    PS % A(1) = 1
    PS % A(1:1) = 1

    PS % PA => TA
    PS % PA = 1
    PS % PA(1) = 1
    PS % PA(1:1) = 1

END PROGRAM MAIN

