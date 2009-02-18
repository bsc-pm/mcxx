#ifndef CXX_TYPE_TRIE_H
#define CXX_TYPE_TRIE_H

#include "cxx-type-decls.h"

typedef struct type_trie_tag type_trie_t;

type_trie_t* allocate_type_trie(void);

const type_t* lookup_type_trie(type_trie_t* trie, const type_t** type_seq, int num_types);
void insert_type_trie(type_trie_t* trie, const type_t** type_seq, int num_types, const type_t* funct_type);

#endif // CXX_TYPE_TRIE_H
