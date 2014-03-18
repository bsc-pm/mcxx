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

#ifndef KNC_VECTOR_MACROS_HPP
#define KNC_VECTOR_MACROS_HPP

#define _CMP_EQ_OQ     0x00
#define _CMP_LT_OS     0x01
#define _CMP_LE_OS     0x02
#define _CMP_UNORD_Q   0x03
#define _CMP_NEQ_UQ    0x04
#define _CMP_NLT_US    0x05
#define _CMP_NLE_US    0x06
#define _CMP_ORD_Q     0x07
#define _CMP_EQ_UQ     0x08
#define _CMP_NGE_US    0x09
#define _CMP_NGT_US    0x0A
#define _CMP_FALSE_OQ  0x0B
#define _CMP_NEQ_OQ    0x0C
#define _CMP_GE_OS     0x0D
#define _CMP_GT_OS     0x0E
#define _CMP_TRUE_UQ   0x0F
#define _CMP_EQ_OS     0x10
#define _CMP_LT_OQ     0x11
#define _CMP_LE_OQ     0x12
#define _CMP_UNORD_S   0x13
#define _CMP_NEQ_US    0x14
#define _CMP_NLT_UQ    0x15
#define _CMP_NLE_UQ    0x16
#define _CMP_ORD_S     0x17
#define _CMP_EQ_US     0x18
#define _CMP_NGE_UQ    0x19
#define _CMP_NGT_UQ    0x1A
#define _CMP_FALSE_OS  0x1B
#define _CMP_NEQ_OS    0x1C
#define _CMP_GE_OQ     0x1D
#define _CMP_GT_OQ     0x1E
#define _CMP_TRUE_US   0x1F

typedef enum {
    _MM_CMPINT_EQ,      /* Equal */
    _MM_CMPINT_LT,      /* Less than */
    _MM_CMPINT_LE,      /* Less than or Equal */
    _MM_CMPINT_UNUSED,
    _MM_CMPINT_NE,      /* Not Equal */
    _MM_CMPINT_NLT,     /* Not Less than */
#define _MM_CMPINT_GE   _MM_CMPINT_NLT  /* Greater than or Equal */
    _MM_CMPINT_NLE      /* Not Less than or Equal */
#define _MM_CMPINT_GT   _MM_CMPINT_NLE  /* Greater than */
} _MM_CMPINT_ENUM;

#endif // KNC_VECTOR_MACROS_HPP
