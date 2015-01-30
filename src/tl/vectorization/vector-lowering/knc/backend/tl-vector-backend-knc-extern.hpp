/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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

#ifndef KNC_VECTOR_MACROS_HPP
#define KNC_VECTOR_MACROS_HPP

namespace KNCComparison
{
    const int EQ_OQ     = 0x00;
    const int LT_OS     = 0x01;
    const int LE_OS     = 0x02;
    const int UNORD_Q   = 0x03;
    const int NEQ_UQ    = 0x04;
    const int NLT_US    = 0x05;
    const int NLE_US    = 0x06;
    const int ORD_Q     = 0x07;
    const int EQ_UQ     = 0x08;
    const int NGE_US    = 0x09;
    const int NGT_US    = 0x0A;
    const int FALSE_OQ  = 0x0B;
    const int NEQ_OQ    = 0x0C;
    const int GE_OS     = 0x0D;
    const int GT_OS     = 0x0E;
    const int TRUE_UQ   = 0x0F;
    const int EQ_OS     = 0x10;
    const int LT_OQ     = 0x11;
    const int LE_OQ     = 0x12;
    const int UNORD_S   = 0x13;
    const int NEQ_US    = 0x14;
    const int NLT_UQ    = 0x15;
    const int NLE_UQ    = 0x16;
    const int ORD_S     = 0x17;
    const int EQ_US     = 0x18;
    const int NGE_UQ    = 0x19;
    const int NGT_UQ    = 0x1A;
    const int FALSE_OS  = 0x1B;
    const int NEQ_OS    = 0x1C;
    const int GE_OQ     = 0x1D;
    const int GT_OQ     = 0x1E;
    const int TRUE_US   = 0x1F;
}

/*
typedef enum {
    _MM_CMPINT_EQ = 0,      // Equal 
    _MM_CMPINT_LT = 1,      // Less than 
    _MM_CMPINT_LE = 2,      // Less than or Equal 
    _MM_CMPINT_UNUSED = 3,
    _MM_CMPINT_NE = 4,      // Not Equal 
    _MM_CMPINT_NLT = 5,     // Not Less than 
#define _MM_CMPINT_GE   _MM_CMPINT_NLT  // Greater than or Equal 
    _MM_CMPINT_NLE = 6      // Not Less than or Equal 
#define _MM_CMPINT_GT   _MM_CMPINT_NLE  // Greater than 
} _MM_CMPINT_ENUM;


const char* cmpint_enum_to_string[] = {"_MM_CMPINT_EQ", "_MM_CMPINT_LT", "_MM_CMPINT_LE", "_MM_CMPINT_UNUSED",
                              "_MM_CMPINT_NE", "_MM_CMPINT_NLT", "_MM_CMPINT_NLE"};
*/

#endif // KNC_VECTOR_MACROS_HPP
