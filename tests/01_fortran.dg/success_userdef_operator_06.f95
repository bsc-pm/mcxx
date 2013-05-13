! --------------------------------------------------------------------
!   (C) Copyright 2006-2012 Barcelona Supercomputing Center
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
MODULE M
    IMPLICIT NONE

    TYPE T
        INTEGER :: X
    END TYPE T

    INTERFACE ASSIGNMENT(=)
        MODULE PROCEDURE JARL
    END INTERFACE ASSIGNMENT(=)

    CONTAINS
        SUBROUTINE FOO(X)
            IMPLICIT NONE
            TYPE(T) :: A(10)
            INTEGER :: X

            ! This looks like as a statement function statement
            ! but we should not attempt to check the expression
            ! since JARL has not been fully registered yet
            A(X) = 3
        END SUBROUTINE FOO

        SUBROUTINE JARL(DEST, SRC)
            IMPLICIT NONE
            TYPE(T), INTENT(OUT) :: DEST
            INTEGER, INTENT(IN) :: SRC

            DEST % X = SRC
        END SUBROUTINE JARL
END MODULE M
