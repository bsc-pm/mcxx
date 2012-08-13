#include "tl-nanox-ptr.hpp"

#include "tl-scope.hpp"
#include "cxx-utils.h"
#include "cxx-scope.h"

#include "fortran03-scope.h"
#include "fortran03-typeutils.h"
#include "fortran03-buildscope.h"

#include "tl-compilerpipeline.hpp"

#include "tl-nodecl.hpp"
#include "tl-source.hpp"
#include "tl-nodecl-utils.hpp"

namespace TL { 

    // FIXME - Move this to a SymbolKit
    static TL::Symbol new_function_symbol(Scope sc,
            const std::string& name,
            const std::string& return_symbol_name,
            TL::Type return_type,
            ObjectList<std::string> parameter_names,
            ObjectList<TL::Type> parameter_types)
    {
        // FIXME - Wrap
        decl_context_t decl_context = sc.get_decl_context();

        scope_entry_t* entry = new_symbol(decl_context, decl_context.current_scope, name.c_str());
        entry->entity_specs.is_user_declared = 1;

        entry->kind = SK_FUNCTION;
        entry->file = "";
        entry->line = 0;

        ERROR_CONDITION(parameter_names.size() != parameter_types.size(), "Mismatch between names and types", 0);

        decl_context_t function_context ;
        if (IS_FORTRAN_LANGUAGE)
        {
            function_context = new_program_unit_context(decl_context);
        }
        else
        {
            function_context = new_function_context(decl_context);
            function_context = new_block_context(function_context);
        }
        function_context.function_scope->related_entry = entry;
        function_context.block_scope->related_entry = entry;

        entry->related_decl_context = function_context;

        parameter_info_t* p_types = new parameter_info_t[parameter_types.size()];

        parameter_info_t* it_ptypes = &(p_types[0]);
        ObjectList<TL::Type>::iterator type_it = parameter_types.begin();
        for (ObjectList<std::string>::iterator it = parameter_names.begin();
                it != parameter_names.end();
                it++, it_ptypes++, type_it++)
        {
            scope_entry_t* param = new_symbol(function_context, function_context.current_scope, it->c_str());
            param->entity_specs.is_user_declared = 1;
            param->kind = SK_VARIABLE;
            param->file = "";
            param->line = 0;

            param->defined = 1;

            symbol_set_as_parameter_of_function(param, entry, entry->entity_specs.num_related_symbols);

            param->type_information = get_unqualified_type(type_it->get_internal_type());

            P_LIST_ADD(entry->entity_specs.related_symbols,
                    entry->entity_specs.num_related_symbols,
                    param);

            it_ptypes->is_ellipsis = 0;
            it_ptypes->nonadjusted_type_info = NULL;
            it_ptypes->type_info = get_indirect_type(param);
        }

        // Return symbol
        scope_entry_t* return_sym = new_symbol(function_context, function_context.current_scope, return_symbol_name.c_str());
        return_sym->entity_specs.is_user_declared = 1;
        return_sym->kind = SK_VARIABLE;
        return_sym->file = "";
        return_sym->line = 0;

        return_sym->defined = 1;

        return_sym->entity_specs.is_result = 1;

        return_sym->type_information = get_unqualified_type(return_type.get_internal_type());

        P_LIST_ADD(entry->entity_specs.related_symbols,
                entry->entity_specs.num_related_symbols,
                return_sym);

        // Type of the function
        type_t *function_type = get_new_function_type(
                return_type.get_internal_type(),
                p_types,
                parameter_types.size());

        entry->type_information = function_type;

        delete[] p_types;

        return entry;
    }

    static void build_empty_body_for_function(
            TL::Symbol function_symbol,
            Nodecl::NodeclBase &function_code,
            Nodecl::NodeclBase &empty_stmt
            )
    {
        empty_stmt = Nodecl::EmptyStatement::make("", 0);
        TL::ObjectList<Nodecl::NodeclBase> stmt_list_;
        stmt_list_.append(empty_stmt);

        Nodecl::List stmt_list = Nodecl::List::make(stmt_list_);

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            Nodecl::CompoundStatement compound_statement =
                Nodecl::CompoundStatement::make(stmt_list,
                        /* destructors */ Nodecl::NodeclBase::null(),
                        "", 0);
            stmt_list = Nodecl::List::make(compound_statement);
        }

        Nodecl::NodeclBase context = Nodecl::Context::make(
                stmt_list,
                function_symbol.get_related_scope(), "", 0);

        function_symbol.get_internal_symbol()->defined = 1;

        function_code = Nodecl::FunctionCode::make(context,
                Nodecl::NodeclBase::null(),
                Nodecl::NodeclBase::null(),
                function_symbol,
                "", 0);
    }

    namespace 
    {
        TL::Type get_assumed_shape_array(TL::Type t)
        {
            if (t.is_array())
            {
                Nodecl::NodeclBase lower, upper;

                t.array_get_bounds(lower, upper);

                TL::Type element_type = get_assumed_shape_array(t.array_element());

                if (lower.is_null()
                        || lower.is_constant())
                {
                    return element_type.get_array_to_with_descriptor(
                            lower.shallow_copy(),
                            Nodecl::NodeclBase::null(),
                            CURRENT_COMPILED_FILE->global_decl_context);
                }
                else
                {
                    internal_error("Non constant lower bound for shaped arrays not implemented yet", 0);
                }
            }
            else
            {
                return t;
            }
        }

    }

    // This is for Fortran only
    TL::Symbol Nanox::get_function_ptr_of(TL::Type t, TL::Scope original_scope)
    {
        static int num = 0;

        // FIXME - Avoid creating functions twice for a same t
        std::stringstream ss;
        ss << "nanox_ptr_of_" 
            << std::hex 
            << simple_hash_str(TL::CompilationProcess::get_current_file().get_filename(/* fullpath */ true).c_str())
            << std::dec
            << "_" 
            << num;

        num++;

        if (t.is_any_reference())
            t = t.references_to();

        TL::Type return_type = t;
        if (t.is_array())
        {
            int rank = ::fortran_get_rank_of_type(t.get_internal_type());
            return_type = TL::Type(
                    ::fortran_get_n_ranked_type_with_descriptor(
                        ::fortran_get_rank0_type(return_type.get_internal_type()), rank, CURRENT_COMPILED_FILE->global_decl_context)
                    );
        }
        return_type = return_type.get_pointer_to();

        ObjectList<std::string> parameter_names;
        parameter_names.append("nanox_target_phony");

        TL::Type argument_type = get_assumed_shape_array(t);

        ObjectList<TL::Type> parameter_types;
        parameter_types.append(argument_type.get_lvalue_reference_to());

        TL::Symbol result = new_function_symbol(
                CURRENT_COMPILED_FILE->global_decl_context,
                ss.str(),
                /* return_name */ "nanox_pointer_phony",
                return_type,
                parameter_names,
                parameter_types);

        Nodecl::NodeclBase function_code, empty_stmt;
        build_empty_body_for_function(
                result,
                function_code,
                empty_stmt
                );

        // Copy USEd information
        {
            TL::ReferenceScope ref_scope(empty_stmt);
            decl_context_t decl_context = ref_scope.get_scope().get_decl_context();
            TL::Symbol related_function = original_scope.get_related_symbol();
            scope_entry_t* original_used_modules_info
                = related_function.get_used_modules().get_internal_symbol();

            if (original_used_modules_info != NULL)
            {
                scope_entry_t* new_used_modules_info
                    = get_or_create_used_modules_symbol_info(decl_context);
                int i;
                for (i = 0 ; i< original_used_modules_info->entity_specs.num_related_symbols; i++)
                {
                    P_LIST_ADD(new_used_modules_info->entity_specs.related_symbols,
                            new_used_modules_info->entity_specs.num_related_symbols,
                            original_used_modules_info->entity_specs.related_symbols[i]);
                }
            }

            if (related_function.is_in_module())
            {
                TL::Symbol module = related_function.in_module();
                original_used_modules_info = module.get_used_modules().get_internal_symbol();

                if (original_used_modules_info != NULL)
                {
                    scope_entry_t* new_used_modules_info
                        = get_or_create_used_modules_symbol_info(decl_context);
                    int i;
                    for (i = 0 ; i< original_used_modules_info->entity_specs.num_related_symbols; i++)
                    {
                        P_LIST_ADD(new_used_modules_info->entity_specs.related_symbols,
                                new_used_modules_info->entity_specs.num_related_symbols,
                                original_used_modules_info->entity_specs.related_symbols[i]);
                    }
                }
            }
        }

        TL::Source body_src, extra_ref;
        body_src
            << "IMPLICIT NONE\n"
            << "TARGET :: nanox_target_phony\n" // This is despicable, I know :)
            << "nanox_pointer_phony => nanox_target_phony" << extra_ref << "\n"
            ;

        Nodecl::NodeclBase new_body = body_src.parse_statement(empty_stmt);
        empty_stmt.replace(new_body);

        Nodecl::Utils::append_to_top_level_nodecl(function_code);

        return result;
    }

}

