#include "fortran03-codegen.h"

void fortran_codegen_translation_unit(FILE* f UNUSED_PARAMETER, AST a UNUSED_PARAMETER, scope_link_t* sl UNUSED_PARAMETER)
{
    fprintf(stderr, "%s does nothing", __FUNCTION__);
}

