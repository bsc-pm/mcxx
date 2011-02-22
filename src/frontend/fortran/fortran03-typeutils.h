#ifndef FORTRAN03_TYPEUTILS_H
#define FORTRAN03_TYPEUTILS_H

#include "libmf03-common.h"
#include "cxx-typeutils.h"

LIBMF03_EXTERN const char* fortran_print_type_str(type_t*);

LIBMF03_EXTERN char is_pointer_to_array_type(type_t*);
LIBMF03_EXTERN int get_rank_of_type(type_t* t);
LIBMF03_EXTERN type_t* get_rank0_type(type_t* t);
LIBMF03_EXTERN type_t* get_n_ranked_type(type_t* scalar_type, int rank, decl_context_t decl_context);

LIBMF03_EXTERN char equivalent_tkr_types(type_t* t1, type_t* t2);

LIBMF03_EXTERN char is_fortran_character_type(type_t*);
LIBMF03_EXTERN char is_pointer_to_fortran_character_type(type_t* t);

LIBMF03_EXTERN char is_fortran_array_type(type_t* t);
LIBMF03_EXTERN char is_pointer_to_fortran_array_type(type_t* t);

LIBMF03_EXTERN type_t* replace_return_type_of_function_type(type_t* function_type, 
        type_t* new_return_type);

LIBMF03_EXTERN type_t* update_basic_type_with_type(type_t* type_info, type_t* basic_type);

LIBMF03_EXTERN char basic_type_is_void(type_t* t);

LIBMF03_EXTERN type_t* rebuild_array_type(type_t* rank0_type, type_t* array_type);


#endif // FORTRAN03_TYPEUTILS_H
