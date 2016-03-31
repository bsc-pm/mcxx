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




#ifndef CXX_UTILS_H
#define CXX_UTILS_H

#include <stdlib.h>
#include <stdio.h>
#include <signal.h>

#include "cxx-driver.h"
#include "cxx-macros.h"
#include "uniquestr.h"
#include "string_utils.h"

MCXX_BEGIN_DECLS

#define DEBUG_CODE() if (debug_options.enable_debug_code)
#define NOT_DEBUG_CODE() if (!debug_options.enable_debug_code)

#define IS_CXX_LANGUAGE (CURRENT_CONFIGURATION->source_language == SOURCE_LANGUAGE_CXX)
#define CXX_LANGUAGE() if (IS_CXX_LANGUAGE)

#define IS_CXX03_LANGUAGE (IS_CXX_LANGUAGE && !CURRENT_CONFIGURATION->enable_cxx11)
#define CXX03_LANGUAGE() if (IS_CXX03_LANGUAGE)

#define IS_CXX11_LANGUAGE (IS_CXX_LANGUAGE && CURRENT_CONFIGURATION->enable_cxx11)
#define CXX11_LANGUAGE() if (IS_CXX11_LANGUAGE)

#define IS_CXX14_LANGUAGE (IS_CXX_LANGUAGE && CURRENT_CONFIGURATION->enable_cxx14)
#define CXX14_LANGUAGE() if (IS_CXX14_LANGUAGE)

#define IS_C_LANGUAGE (CURRENT_CONFIGURATION->source_language == SOURCE_LANGUAGE_C)
#define C_LANGUAGE() if (IS_C_LANGUAGE)

#define IS_C11_LANGUAGE (IS_C_LANGUAGE && CURRENT_CONFIGURATION->enable_c11)
#define C11_LANGUAGE() if (IS_C11_LANGUAGE)

#define IS_FORTRAN_LANGUAGE (CURRENT_CONFIGURATION->source_language == SOURCE_LANGUAGE_FORTRAN)
#define FORTRAN_LANGUAGE() if (IS_FORTRAN_LANGUAGE)

#define STATIC_ARRAY_LENGTH(_v) (sizeof(_v)/sizeof(_v[0]))

MCXX_END_DECLS

#endif // CXX_UTILS_H
