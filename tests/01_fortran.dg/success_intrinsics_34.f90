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
MODULE FOO
   IMPLICIT NONE
   TYPE T
           INTEGER :: A, B
   END TYPE T

   INTERFACE mod
      MODULE PROCEDURE foo_mod
   END INTERFACE mod

CONTAINS
        FUNCTION foo_mod(X, Y) RESULT(Z)
           IMPLICIT NONE
           TYPE(T) :: X, Y, Z
        END FUNCTION foo_mod
END MODULE FOO

PROGRAM MAIN
   USE FOO, ONLY :T, MOD
   IMPLICIT NONE

   TYPE(T) :: A, B, C
   INTEGER :: I, J, K

   C = MOD(A, B)
   I = MOD(J, K)

END PROGRAM MAIN

