#ifndef CXX_INSTANTIATION_H
#define CXX_INSTANTIATION_H

#include "cxx-scope.h"

void instantiate_primary_template(scope_entry_t* matched_template,
		template_argument_list_t* arguments, scope_t* st);
void instantiate_specialized_template(scope_entry_t* matched_template, template_argument_list_t* arguments, 
		unification_set_t* unification_set, scope_t* st);

void instantiate_template(matching_pair_t* match_pair, template_argument_list_t* arguments, scope_t* st);

#endif // CXX_INSTANTIATION_H
