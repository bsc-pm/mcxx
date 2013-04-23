! --------------------------------------------------------------------
!   (C) Copyright 2006-2013 Barcelona Supercomputing Center
!                           Centro Nacional de Supercomputacion
!   
!   This file is part of Mercurium C/C++ source-to-source compiler.
!   
!   See AUTHORS file in the top level directory for information
!   regarding developers and contributors.
!   
!   This library is free software; you can redistribute it and/or
!   modify it under the terms of the GNU Lesser General Public
!   License as published by the Free Software Foundation; either
!   version 3 of the License, or (at your option) any later version.
!   
!   Mercurium C/C++ source-to-source compiler is distributed in the hope
!   that it will be useful, but WITHOUT ANY WARRANTY; without even the
!   implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
!   PURPOSE.  See the GNU Lesser General Public License for more
!   details.
!   
!   You should have received a copy of the GNU Lesser General Public
!   License along with Mercurium C/C++ source-to-source compiler; if
!   not, write to the Free Software Foundation, Inc., 675 Mass Ave,
!   Cambridge, MA 02139, USA.
! --------------------------------------------------------------------


! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE MOD1

    TYPE T
        INTEGER :: X
    END TYPE T

    INTERFACE OPERATOR(/=)
        MODULE PROCEDURE FOO
    END INTERFACE

    INTERFACE OPERATOR(//)
        MODULE PROCEDURE BAR
    END INTERFACE

    CONTAINS

        LOGICAL FUNCTION FOO(A, B)
            TYPE(T), INTENT(IN) :: A, B
            FOO = (A % X /= B % X)
        END FUNCTION FOO

        TYPE(T) FUNCTION BAR(A, B)
            TYPE(T), INTENT(IN) :: A, B
            BAR = T(A % X + B % X)
        END FUNCTION BAR
END MODULE MOD1

PROGRAM MAIN
    USE MOD1
    IMPLICIT NONE

    TYPE(T) :: A, B, C
    A = T(1)
    B = T(2)

    IF (.NOT. A /= B) STOP 1

    C = A // B

END PROGRAM MAIN
