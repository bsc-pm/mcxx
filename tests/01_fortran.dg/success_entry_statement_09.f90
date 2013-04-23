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
! test_generator="config/mercurium-fortran run"
! </testinfo>
FUNCTION S()
        INTEGER :: S2, S3_RESULT

        S = 1.0
        RETURN

        ENTRY S2()
        S2 = 2
        RETURN

        ENTRY S3() RESULT(S3_RESULT)
        S3_RESULT = 3
        RETURN

END FUNCTION S

PROGRAM MAIN
    IMPLICIT NONE
    REAL, EXTERNAL :: S
    INTEGER, EXTERNAL :: S2, S3

    IF (S() /= 1.0) STOP 1
    IF (S2() /= 2) STOP 2
    IF (S3() /= 3) STOP 3
END PROGRAM MAIN
