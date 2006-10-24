#ifndef HASH_H
#define HASH_H

#include "mem_ctl.h"
#include "s_types.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _hashnode HashNode;
typedef struct _hash Hash;
typedef int HASH_FUNC (void *key, int size);
typedef int KEYCMP_FUNC (void *key1, void *key2);

enum
{ HASH_FREE_KEYS, HASH_FREE_ITEMS, HASH_KEYS_DESTRUCTOR,
  HASH_ITEMS_DESTRUCTOR
}
HashOps;

#define HASHFUNC(f) ((HASH_FUNC *)f)
#define KEYCMPFUNC(f) ((KEYCMP_FUNC *)f)

struct _hashnode
{
  void *key;
  void *item;

  HashNode *next;
  HashNode *prev;
};

struct _hash
{
  int size;
  int no_items;
  HASH_FUNC *hash_func;
  KEYCMP_FUNC *keycmp_func;
  HashNode **table;

  bool_type free_keys;
  delete_func *df_keys;
  bool_type free_items;
  delete_func *df_items;
};

void hash_init (Hash * h, int size, HASH_FUNC * hash_func,
        KEYCMP_FUNC * keycmp_func);
void hash_set (Hash * h, int op, void *value);
#define hash_set_bool(h,op,val) hash_set(h,op,(void *)val)
Hash *hash_create (int size, HASH_FUNC * hash_func,
           KEYCMP_FUNC * keycmp_func);
void hash_dump (Hash * h);
void hash_destroy (Hash * h);
void hash_put (Hash * h, void *key, void *item);
HashNode *hash_getnode (Hash * h, int i, void *key);
void *hash_get (Hash * h, void *key);
void *hash_delete (Hash * h, void *key);

/* useful hash functions */

int hash_string (char *name, int size);
int hash_caseless_string (char *name, int size);

#ifdef __cplusplus
}
#endif

#endif
