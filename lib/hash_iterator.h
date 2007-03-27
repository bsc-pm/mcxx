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
/***************************************************************************
 
  PhantasienMud Development Team 2001 (c)

  hash_iterator.h - Hash iteration functions (No Locking support yet)

  ChangeLog:

  Sat Jan 20 2001 - Created by Ingwe

***************************************************************************/

#ifndef HASH_ITERATOR_H
#define HASH_ITERATOR_H

#include "hash.h"
#include "iterator.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct
{
  Iterator iterator;
  HashNode *act;
  Hash *hash;
  int table;
}
HashIterator;

void hash_iterator_init (HashIterator * i, Hash * h);
HashIterator *hash_iterator_create (Hash * h);

#ifdef __cplusplus
}
#endif

#endif
