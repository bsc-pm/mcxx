#ifndef CXX_INSTANTIATION_H
#define CXX_INSTANTIATION_H

#include "cxx-scope.h"

scope_entry_t* instantiate_primary_template(scope_entry_t* matched_template,
		template_argument_list_t* arguments, scope_t* st);
scope_entry_t* instantiate_specialized_template(scope_entry_t* matched_template, template_argument_list_t* arguments, 
		unification_set_t* unification_set, scope_t* st);

scope_entry_t* instantiate_template(scope_entry_t* matched_template, template_argument_list_t* arguments, 
		unification_set_t* unification_set, scope_t* st);

#endif // CXX_INSTANTIATION_H
