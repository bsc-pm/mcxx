#ifndef CXX_LOCUS_H
#define CXX_LOCUS_H

#include "cxx-macros.h"

MCXX_BEGIN_DECLS

typedef struct locus_tag locus_t;

const locus_t* make_locus(const char* filename, unsigned int line, unsigned int col);
const char* locus_to_str(const locus_t*);

const char* locus_get_filename(const locus_t*);
unsigned int locus_get_line(const locus_t*);
unsigned int locus_get_col(const locus_t*);

MCXX_END_DECLS

#endif // CXX_LOCUS_H
