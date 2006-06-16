#ifndef CXX_SOLVETEMPLATE_H
#define CXX_SOLVETEMPLATE_H

#include "cxx-scope.h"

scope_entry_t* solve_template(scope_entry_list_t* candidate_templates, template_argument_list_t* arguments, scope_t* st,
		char give_exact_match);

#endif // CXX_SOLVETEMPLATE_H
