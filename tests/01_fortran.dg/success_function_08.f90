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
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE M1

   IMPLICIT NONE
   PUBLIC :: my_erf

   TYPE T
       INTEGER :: X
   END TYPE T

   INTERFACE my_erf
      MODULE PROCEDURE my_erf_1
   END INTERFACE

CONTAINS

   FUNCTION my_erf_1(x) RESULT(res)
       REAL, INTENT(in) :: x
       REAL :: res1
       TYPE(T) :: res
       ! INTRINSIC erf

       res1 = erf(x)
       res = T(1)
   END FUNCTION my_erf_1

END MODULE M1


SUBROUTINE foo

    USE M1, ONLY: erf => my_erf, T

    IMPLICIT NONE
    TYPE(T) :: r

    r = erf(0.0)

END SUBROUTINE foo
