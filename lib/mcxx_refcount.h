/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef MCXX_REFCOUNT_H
#define MCXX_REFCOUNT_H

#include <stdlib.h>

typedef
enum _mcxx_ref_colour_tag
{
    _MCXX_BLACK, // In use or free
    _MCXX_GRAY, // Possible member of cycle
    _MCXX_WHITE, // Member of garbage cycle
    _MCXX_PURPLE, // Possible root of cycle
    // _MCXX_GREEN, // Acyclic (unused)
} _mcxx_ref_colour_t;

#define MCXX_REFCOUNT_OBJECT \
    int _mcxx_refcount; \
    int _mcxx_buffered; \
    _mcxx_ref_colour_t _mcxx_colour; \
    void (*_mcxx_children)(void*, void (*_mcxx_do)(void*))

typedef
struct _mcxx_base_refcount_tag
{
    MCXX_REFCOUNT_OBJECT;
} *_p_mcxx_base_refcount_t, _mcxx_base_refcount_t;

void *_mcxx_calloc(size_t nmemb, size_t size);

void _mcxx_increment(void *p);
void _mcxx_decrement(void *p);

void _mcxx_collectcycles(void);

#define MCXX_NEW(_type) (_type*)(_mcxx_calloc(1, sizeof(_type)))

#define MCXX_CHILDREN(x) ((x)->_mcxx_children)

#define MCXX_INCREF(x) _mcxx_increment(x)

#define MCXX_DECREF(x) _mcxx_decrement(x)


#endif // MCXX_REFCOUNT_H
