/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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




#ifndef CALCULATOR_PARSER_TYPES_H
#define CALCULATOR_PARSER_TYPES_H

#include <stdint.h>


#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
	int_type,
	long_type,
	long_long_type,
	float_type,
	double_type,
	long_double_type
} calculator_type_class_t;

typedef enum {
	unsigned_type,
	signed_type
} calculator_type_signedness_t;

typedef struct {
	calculator_type_class_t type_class;
	calculator_type_signedness_t type_signedness;
	
	union {
		uint64_t integer_value;
		long double floating_value;
	} value;
} calculator_value_t;


#ifdef __cplusplus
}
#endif


#endif // CALCULATOR_PARSER_TYPES_H
