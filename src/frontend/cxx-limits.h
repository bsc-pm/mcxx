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

#ifndef CXX_LIMITS_H
#define CXX_LIMITS_H

enum 
{
    // AST limits
    MCXX_MAX_AST_CHILDREN = 4,

    // Function limits
    MCXX_MAX_FUNCTION_PARAMETERS = 1024,
    MCXX_MAX_FUNCTIONS_PER_CLASS = 2048,
    MCXX_MAX_FUNCTION_CALL_ARGUMENTS = 1024,

    // Class limits
    MCXX_MAX_CLASS_BASES = 256,

    // C++ overload
    MCXX_MAX_BUILTINS_IN_OVERLOAD = 256,
    MCXX_MAX_SURROGATE_FUNCTIONS = 64,
    MCXX_MAX_USER_DEFINED_CONVERSIONS = 64,

    // Template limits
    MCXX_MAX_TEMPLATE_PARAMETERS = 256,
    MCXX_MAX_TEMPLATE_ARGUMENTS = 256,
    MCXX_MAX_FEASIBLE_SPECIALIZATIONS = 256,
    MCXX_MAX_ARGUMENTS_FOR_DEDUCTION = 256,
    MCXX_MAX_TEMPLATE_NESTING_LEVELS = 32,

    // Scope limits
    MCXX_MAX_SCOPES_NESTING = 128,

    // Max linkage nesting
    MCXX_MAX_LINKAGE_NESTING = 128,

    // C99 Designator
    MCXX_MAX_DESIGNATORS = 64,

    // GCC attributes
    MCXX_MAX_GCC_ATTRIBUTES_PER_SYMBOL = 256,

    // C++ associated namespaces during lookup
    MCXX_MAX_ASSOCIATED_NAMESPACES = 256,

    // Environmental limits
    MCXX_MAX_BYTES_INTEGER = 16,

    // Type limits
    MCXX_MAX_QUALIFIER_CONVERSION = 256,

    // Fortran modules limits
    MCXX_MAX_RENAMED_SYMBOLS = 256,
    MCXX_MAX_ARRAY_SPECIFIER = 16,

    // Generic types in c
    MCXX_MAX_GENERIC_TYPES = 4,

    // Number of C (not C++) "overloads"
    MCXX_MAX_C_OVERLOADS = 256,

    // Number of extra declarations in an expression
    MCXX_MAX_EXTRA_DECLARATIONS = 256,

    // Max file name length
    MCXX_MAX_FILENAME = 4096,

    // Max Hollerith length
    MCXX_MAX_LENGTH_HOLLERITH_CONSTANT = 256,

    // Max unparenthesized aggregates in initializers
    MCXX_MAX_UNBRACED_AGGREGATES = 64,
};

#endif // CXX_LIMITS_H
