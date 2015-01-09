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
PROGRAM P
    IMPLICIT NONE
    TYPE T
        REAL(8), ALLOCATABLE :: V(:)
    END TYPE T
    TYPE(T) :: MYVAR
    INTEGER :: N

    INTERFACE
        !$OMP TARGET DEVICE(OPENCL) NDRANGE(1, N, 128) FILE(t1.cl) COPY_DEPS
        !$OMP TASK INOUT(V)
        SUBROUTINE OCL_TASK(V, N)
            IMPLICIT NONE
            INTEGER, VALUE :: N
            REAL(8) :: V(N)
        END SUBROUTINE OCL_TASK
    END INTERFACE

    N = 10
    ALLOCATE(MYVAR % V(N))
    MYVAR % V = -1

    CALL OCL_TASK(MYVAR % V(2), N - 1)
    !$OMP TASKWAIT

    ! V(1:2) == -1  && V(3:10) == 2

    CONTAINS
        !$OMP TARGET DEVICE(SMP) COPY_DEPS IMPLEMENTS(OCL_TASK)
        !$OMP TASK INOUT(V)
        SUBROUTINE SMP_TASK(V, N)
            IMPLICIT NONE
            INTEGER, VALUE :: N
            REAL(8) :: V(N)
            V = 2
        END SUBROUTINE SMP_TASK
END PROGRAM P
