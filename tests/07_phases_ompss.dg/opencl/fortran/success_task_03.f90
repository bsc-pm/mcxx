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
    IMPLICIT NONE

    TYPE T
        INTEGER, ALLOCATABLE :: V_SMP(:)
        INTEGER, ALLOCATABLE :: V_GPU(:)
    END TYPE T

    TYPE(T) :: VAR
END MODULE M

PROGRAM P
USE M
IMPLICIT NONE
INTEGER :: I
INTEGER, PARAMETER :: N = 100
INTEGER, PARAMETER :: BS = 20

INTERFACE
!$OMP TARGET DEVICE(SMP)
!$OMP TASK INOUT(V(1:BS))
    SUBROUTINE INITIALIZE_SMP(BS, V)
        IMPLICIT NONE
        INTEGER, VALUE :: BS
        INTEGER :: V(BS)
    END SUBROUTINE INITIALIZE_SMP

!$OMP TARGET DEVICE(OPENCL) COPY_DEPS NDRANGE(1, BS, 3) file(t2.cl) IMPLEMENTS(INITIALIZE_SMP)
!$OMP TASK INOUT(V(1:BS))
    SUBROUTINE INITIALIZE_GPU(BS, V)
        IMPLICIT NONE
        INTEGER, VALUE :: BS
        INTEGER :: V(BS)
    END SUBROUTINE INITIALIZE_GPU

END INTERFACE


ALLOCATE(VAR % V_SMP(N))
ALLOCATE(VAR % V_GPU(N))

VAR % V_SMP = -1
VAR % V_GPU = -1

DO I=1, N, BS
    CALL INITIALIZE_SMP(BS, VAR % V_SMP(I))
END DO

DO I=1, N, BS
    CALL INITIALIZE_GPU(BS, VAR % V_GPU(I))
END DO
!$OMP TASKWAIT

IF (ANY(VAR % V_GPU /= VAR % V_SMP)) CALL ABORT()
END PROGRAM P

SUBROUTINE INITIALIZE_SMP(BS, V)
    IMPLICIT NONE
    INTEGER, VALUE :: BS
    INTEGER :: V(BS)
    INTEGER :: I
    DO I=1, BS
        V(I) = I - 1
    END DO
END SUBROUTINE INITIALIZE_SMP
