/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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




#ifndef CXX_TYPEENVIRON_H
#define CXX_TYPEENVIRON_H

#ifdef HAVE_CONFIG_H
  // Required for DEFAULT_TYPE_ENVIRONMENT
  #include "config.h"
#endif

#include "libmcxx-common.h"
#include "cxx-typeenviron-decls.h"

MCXX_BEGIN_DECLS

#define DEBUG_SIZEOF_CODE() if (debug_options.debug_sizeof)

// Environments are to be defined in cxx-typeenviron.c
// and declared here

// A NULL ended list of those above
LIBMCXX_EXTERN type_environment_t* type_environment_list[];
LIBMCXX_EXTERN type_environment_t* default_environment;
LIBMCXX_EXTERN void init_type_environments(void);

// Fallback for faulty configuration
#ifndef DEFAULT_TYPE_ENVIRONMENT
  #error Missing default type environment
#endif

MCXX_END_DECLS

#endif // CXX_TYPEENVIRON_H
