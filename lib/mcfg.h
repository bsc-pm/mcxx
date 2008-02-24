/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef INI_H
#define INI_H

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** function prototypes
*/
typedef
enum param_process_result_tag
{
    PPR_SUCCESS = 0,
    PPR_OPEN_FILE_ERROR = -1,
    PPR_MALLOC_ERROR = -2,
    PPR_PARSE_ERROR = -3
} param_process_t;

int param_process(const char *filename,
        int (*sfunc)(const char *),
        int (*pfunc)(const char * option, const char * value, int num_flags, const char** flags));

#ifdef __cplusplus
}
#endif

#endif  /* INI_H */
