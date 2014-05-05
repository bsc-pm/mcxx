#ifndef DHASH_STR_H
#define DHASH_STR_H

// Hash for strings

#ifdef __cplusplus
extern "C" {
#endif

typedef void* dhash_str_info_t;

typedef struct dhash_str_tag dhash_str_t;

dhash_str_t* dhash_str_new(int initial_size);
void dhash_str_destroy(dhash_str_t*);
void* dhash_str_query(dhash_str_t*, const char* key);
void dhash_str_insert(dhash_str_t*, const char* key, dhash_str_info_t info);
void dhash_str_remove(dhash_str_t*, const char* key);

typedef void dhash_str_walk_fn(const char* key, void* info, void *walk_info);

void dhash_str_walk(dhash_str_t*, dhash_str_walk_fn walk_fn, void* walk_info);

#ifdef __cplusplus
}
#endif

#endif // DHASH_STR_H
