#ifndef CXX_GCCSUPPORT_H
#define CXX_GCCSUPPORT_H

void gather_gcc_attribute(AST attribute, 
        gather_decl_spec_t* gather_info, 
        decl_context_t decl_context);

void gather_gcc_attribute_list(AST attribute_list, 
        gather_decl_spec_t *gather_info, 
        decl_context_t decl_context);

#endif // CXX_GCCSUPPORT_H
