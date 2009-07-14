/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#ifndef CXX_TYPEENVIRON_H
#define CXX_TYPEENVIRON_H

#ifdef HAVE_CONFIG_H
  // Required for DEFAULT_TYPE_ENVIRONMENT
  #include "config.h"
#endif

#include "libmcxx-common.h"
#include "cxx-typeenviron-decls.h"

MCXX_BEGIN_DECLS

#define DEBUG_SIZEOF_CODE() if (CURRENT_CONFIGURATION->debug_options.debug_sizeof)

// Environments are to be defined in cxx-typeenviron.c
// and declared here

// A NULL ended list of those above
LIBMCXX_EXTERN type_environment_t* type_environment_list[];
LIBMCXX_EXTERN type_environment_t* default_environment;

// Fallback for faulty configuration
#ifndef DEFAULT_TYPE_ENVIRONMENT
  #error Missing default type environment
#endif

MCXX_END_DECLS

#endif // CXX_TYPEENVIRON_H
