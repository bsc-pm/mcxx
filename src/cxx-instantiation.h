#ifndef CXX_INSTANTIATION_H
#define CXX_INSTANTIATION_H

#include "cxx-scope.h"

void instantiate_template(matching_pair_t* match_pair, template_argument_list_t* arguments, scope_t* st, 
		int instantiate_line, decl_context_t decl_context);
void instantiate_template_in_symbol(scope_entry_t* instance_symbol, matching_pair_t* match_pair, 
        template_argument_list_t* arguments, scope_t* st, decl_context_t decl_context);
scope_entry_t* create_holding_symbol_for_template(scope_entry_t* matched_template, template_argument_list_t*
        arguments, scope_t* st, int instantiation_line);

#endif // CXX_INSTANTIATION_H
