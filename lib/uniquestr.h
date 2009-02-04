#ifndef UNIQUESTR_H
#define UNIQUESTR_H

#include "libutils-common.h"

#define uniqstr uniquestr
LIBUTILS_EXTERN const char *uniquestr(const char*);

LIBUTILS_EXTERN unsigned long long int char_trie_used_memory(void);

#endif // UNIQUESTR_H
