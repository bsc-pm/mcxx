#ifndef CXX_INSTANTIATION_H
#define CXX_INSTANTIATION_H

#include "cxx-scope.h"

void instantiate_template(matching_pair_t* match_pair, template_argument_list_t* arguments, scope_t* st, int instantiate_line);
void instantiate_template_in_symbol(scope_entry_t* instance_symbol, matching_pair_t* match_pair, 
		template_argument_list_t* arguments, scope_t* st);

#endif // CXX_INSTANTIATION_H
