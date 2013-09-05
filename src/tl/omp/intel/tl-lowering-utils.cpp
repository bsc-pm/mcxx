
#include "tl-lowering-utils.hpp"
#include "tl-type.hpp"
#include "tl-source.hpp"
#include "tl-counters.hpp"
#include "tl-nodecl.hpp"
#include "tl-nodecl-utils.hpp"
#include "cxx-cexpr.h"

#include <sstream>

namespace TL { namespace Intel {

namespace
{
    TL::Type ident_t_type(NULL);
    TL::Type kmp_int32_type(NULL);
}

} // Intel

TL::Symbol Intel::new_global_ident_symbol(Nodecl::NodeclBase location)
{
    std::string filename = location.get_filename();
    int start_line = location.get_line();
    int end_line = location.get_line();

    if (!ident_t_type.is_valid())
    {
        ident_t_type = Source("ident_t").parse_c_type_id(TL::Scope(CURRENT_COMPILED_FILE->global_decl_context));
    }
    if (!kmp_int32_type.is_valid())
    {
        kmp_int32_type = Source("kmp_int32").parse_c_type_id(TL::Scope(CURRENT_COMPILED_FILE->global_decl_context));
    }

    ERROR_CONDITION(!ident_t_type.is_valid(), "Type ident_t not found", 0);

    TL::Counter &loc_num = TL::CounterManager::get_counter("intel-omp-ident");

    std::stringstream new_name;
    new_name << "_loc_" << (int)loc_num;
    loc_num++;

    // Due to location limitations of Mercurium itself end_line will always be start_line
    scope_entry_t* new_ident_sym = ::new_symbol(
            CURRENT_COMPILED_FILE->global_decl_context,
            CURRENT_COMPILED_FILE->global_decl_context.current_scope,
            new_name.str().c_str());

    new_ident_sym->kind = SK_VARIABLE;
    new_ident_sym->type_information = ident_t_type.get_internal_type();
    new_ident_sym->defined = new_ident_sym->entity_specs.is_user_declared = 1;
    new_ident_sym->entity_specs.is_static = 1;
    new_ident_sym->locus = make_locus(filename.c_str(), start_line, /* col */ 0);

    Source string_literal;
    string_literal << "\"" << filename << ";" << start_line << ";" << end_line << "\"";
    Nodecl::NodeclBase string_literal_tree =
        string_literal.parse_expression(TL::Scope(CURRENT_COMPILED_FILE->global_decl_context));

    Nodecl::StructuredValue value = Nodecl::StructuredValue::make(
            Nodecl::List::make(
                /* reserved_1 */ Nodecl::IntegerLiteral::make(
                    kmp_int32_type,
                    const_value_get_zero(/* bytes */ 4, /* sign */1)),
                /* flags */ Nodecl::IntegerLiteral::make(
                    kmp_int32_type,
                    const_value_get_zero(/* bytes */ 4, /* sign */1)),
                /* reserved_2 */ Nodecl::IntegerLiteral::make(
                    kmp_int32_type,
                    const_value_get_zero(/* bytes */ 4, /* sign */1)),
                /* reserved_3 */ Nodecl::IntegerLiteral::make(
                    kmp_int32_type,
                    const_value_get_zero(/* bytes */ 4, /* sign */1)),
                /* psource */
                string_literal_tree),
            ident_t_type,
            new_ident_sym->locus);

    new_ident_sym->value = value.get_internal_nodecl();

    Nodecl::NodeclBase object_init = Nodecl::ObjectInit::make(new_ident_sym);
    Nodecl::Utils::prepend_to_enclosing_top_level_location(location,
            object_init);

    return new_ident_sym;
}

TL::Symbol Intel::new_private_symbol(TL::Symbol original_symbol, TL::Scope private_scope)
{
    TL::Counter &private_num = TL::CounterManager::get_counter("intel-omp-privates");

    std::stringstream new_name;
    new_name << "p_" << original_symbol.get_name() << (int)private_num;
    private_num++;

    scope_entry_t* new_private_sym = ::new_symbol(
            private_scope.get_decl_context(),
            private_scope.get_decl_context().current_scope,
            new_name.str().c_str());

    new_private_sym->kind = original_symbol.get_internal_symbol()->kind;
    new_private_sym->type_information = original_symbol.get_internal_symbol()->type_information;
    new_private_sym->defined = new_private_sym->entity_specs.is_user_declared = 1;

    return new_private_sym;
}

} // TL
