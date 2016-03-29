! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
MODULE MOO
    USE ISO_C_BINDING
    INTEGER(KIND=C_INT), BIND(C, NAME="hola") :: HOLA
END MODULE MOO

PROGRAM MAIN
    USE MOO, ONLY : HOLA
    HOLA = 1
END PROGRAM MAIN
