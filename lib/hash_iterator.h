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

#endif
