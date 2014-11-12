! --------------------------------------------------------------------
!   (C) Copyright 2006-2011 Barcelona Supercomputing Center
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
! test_generator=config/mercurium-opencl
! compile_versions=ompss
! </testinfo>
MODULE M
    INTERFACE
        !$OMP TARGET DEVICE(OPENCL) NDRANGE(1,N,32) FILE(t6.cl) COPY_DEPS
        !$OMP TASK  INOUT(A, B)
        SUBROUTINE FOO(A, B, N)
            INTEGER(4), VALUE :: N
           INTEGER(4) :: A(N), B(N)
        END SUBROUTINE FOO
    END INTERFACE
    CONTAINS
END MODULE

PROGRAM TEST
    IMPLICIT NONE
    CALL K_TEST
END PROGRAM

SUBROUTINE K_TEST
    USE M
    IMPLICIT NONE
    INTEGER :: A(10), B(10)

    A = -1
    B = 1
    CALL FOO(A, B, 10)
    !$OMP TASKWAIT
    IF (ANY(A /= 1) .or. ANY(B /= -1)) STOP 1

    PRINT *, "OK!"
END SUBROUTINE
