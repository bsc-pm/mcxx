#ifndef DHASH_PTR_H
#define DHASH_PTR_H

// Hash for strings

#ifdef __cplusplus
extern "C" {
#endif

typedef void* dhash_ptr_info_t;

typedef struct dhash_ptr_tag dhash_ptr_t;

dhash_ptr_t* dhash_ptr_new(int initial_size);
void dhash_ptr_destroy(dhash_ptr_t*);
void* dhash_ptr_query(dhash_ptr_t*, const void* key);
void dhash_ptr_insert(dhash_ptr_t*, const void* key, dhash_ptr_info_t info);
void dhash_ptr_remove(dhash_ptr_t*, const void* key);

typedef void dhash_ptr_walk_fn(const void* key, void* info, void *walk_info);

void dhash_ptr_walk(dhash_ptr_t*, dhash_ptr_walk_fn walk_fn, void* walk_info);

#ifdef __cplusplus
}
#endif

#endif // DHASH_PTR_H
