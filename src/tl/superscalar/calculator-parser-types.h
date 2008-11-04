/*
    SMP superscalar Compiler
    Copyright (C) 2008 Barcelona Supercomputing Center

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; version 2.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

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
