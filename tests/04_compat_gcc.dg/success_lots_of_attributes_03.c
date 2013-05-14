 /*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/

/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

// Do not worry about the warnings
__attribute__ ((deprecated))
 struct U1
 {
 } x1, y1;
 
 struct __attribute__ ((deprecated)) U
 {
 } x2, y2;
 
 struct U3
 {
 } __attribute__ ((deprecated)) x3, y3;
 
 struct U4
 {
 } x4 __attribute__ ((deprecated)), y4;

 typedef __attribute__ ((deprecated))
     struct V1
 {
 } a1, b1;
 
 struct V1 c1;
 a1 d1;
 b1 e1;

typedef struct __attribute__ ((deprecated)) V2
{
} a2, b2;

struct V2 c2;
a2 d2;
b2 e2;

 typedef struct V3
 {
 } __attribute__ ((deprecated)) a3, b3;
 
 struct V3 c3;
 a3 d3;
 b3 e3;
 
 
 typedef struct V4
 {
 } a4 __attribute__ ((deprecated)), b4;
 
 struct V4 c4;
 a4 d4;
 b4 e4;
