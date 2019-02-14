! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    CHARACTER(*), PARAMETER :: MONJA =  "MONJA!!"
    CHARACTER(*), PARAMETER :: JA    = MONJA(4:5)
    CHARACTER(*), PARAMETER :: JAEXCL= MONJA(4:)
    CHARACTER(*), PARAMETER :: MONJAEXCL = MONJA(:7)
    CHARACTER(*), PARAMETER :: MON   = MONJA(1:3)
    CHARACTER(*), PARAMETER :: EXCL  = MONJA(6:7)
    CHARACTER(*), PARAMETER :: JAMON = JA // MON // EXCL
    CHARACTER(*), PARAMETER :: EMPTY  = MONJA(3:1)

    CHARACTER(7) :: VAR

    VAR = JAMON

    IF (MONJA /= "MONJA!!") STOP 1

    IF (JA /= "JA") STOP 2

    IF (JAEXCL /= "JA!!") STOP 3

    IF (MONJAEXCL /= "MONJA!!") STOP 4

    IF (MON /= "MON") STOP 5

    IF (EXCL /= "!!") STOP 6

    IF (JAMON /= "JAMON!!") STOP 7

    IF (VAR /= "JAMON!!") STOP 8

    IF (EMPTY /= "") STOP 9

END PROGRAM P
