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
MODULE m_type
    TYPE :: x
      INTEGER(4) :: n
    END TYPE x
END MODULE m_type
MODULE m
    USE m_type, only : x
    IMPLICIT NONE
    INTERFACE OPERATOR(.EQ.)
        MODULE PROCEDURE FOO1
    END INTERFACE OPERATOR(.EQ.)
  CONTAINS
      ELEMENTAL FUNCTION foo1(a, b) RESULT(c)
        USE m_type, only : x
        IMPLICIT NONE
        TYPE(x), INTENT(IN) :: a
        TYPE(x), INTENT(IN) :: b
        LOGICAL :: c
      END FUNCTION foo1
END MODULE m

MODULE M2
    USE M
END MODULE M2


PROGRAM p
    USE m2
    IMPLICIT NONE
    TYPE(x) :: u(1:10)
    TYPE(x) :: v(1:10)
    LOGICAL :: L(1:10)

    L = (U .EQ. V)
END PROGRAM p

