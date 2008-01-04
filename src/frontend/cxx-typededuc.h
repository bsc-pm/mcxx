#ifndef CXX_TYPEDEDUC_H
#define CXX_TYPEDEDUC_H

#include "cxx-typeunif.h"

char deduce_template_arguments_common(
        template_parameter_list_t* template_parameters,
        struct type_tag** arguments, int num_arguments,
        struct type_tag** parameters,
        decl_context_t decl_context,
        deduction_set_t **deduced_arguments,
        const char *filename, int line,
        template_argument_list_t* explicit_template_arguments);

char deduce_arguments_from_call_to_specific_template_function(struct type_tag** call_argument_types,
        int num_arguments, struct type_tag* specialized_named_type, 
        template_parameter_list_t* template_parameters, 
        decl_context_t decl_context,
        deduction_set_t **deduction_result, 
        const char* filename, int line,
        template_argument_list_t* explicit_template_arguments);

char deduce_arguments_of_conversion(
        struct type_tag* destination_type,
        struct type_tag* specialized_named_type,
        template_parameter_list_t* template_parameters,
        decl_context_t decl_context_t,
        deduction_set_t **deduction_result,
        const char *filename, int line);

template_argument_list_t* build_template_argument_list_from_deduction_set(
        deduction_set_t* deduction_set);

#endif // CXX_TYPEDEDUC_H
