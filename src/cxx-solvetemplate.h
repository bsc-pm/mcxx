#ifndef CXX_SOLVETEMPLATE_H
#define CXX_SOLVETEMPLATE_H

#include "cxx-symtab.h"

symtab_entry_t* solve_template(symtab_entry_list_t* candidate_templates, template_argument_list_t* arguments, symtab_t* st);

#endif // CXX_SOLVETEMPLATE_H
