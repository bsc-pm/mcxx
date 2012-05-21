/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information 
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/

#include "tl-lowering-visitor.hpp"
#include "tl-source.hpp"
#include "tl-counters.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-outline-info.hpp"
#include "tl-replace.hpp"

#include "codegen-phase.hpp"
#include "codegen-fortran.hpp"

#include "fortran03-scope.h"

using TL::Source;

namespace TL { namespace Nanox {

    struct FortranExtraDeclsVisitor : Nodecl::ExhaustiveVisitor<void>
    {
        public:

            TL::ObjectList<TL::Symbol> extra_decl_sym;

            virtual void visit(const Nodecl::FunctionCall &function_call)
            {
                Nodecl::NodeclBase function_name = function_call.get_called();
                Nodecl::NodeclBase alternate_name = function_call.get_alternate_name();
                Nodecl::NodeclBase argument_seq = function_call.get_arguments();

                if (alternate_name.is_null())
                {
                    walk(function_name);
                }
                else
                {
                    walk(alternate_name);
                }

                walk(argument_seq);
            }

            virtual void visit(const Nodecl::Symbol &node_sym)
            {
                TL::Symbol sym = node_sym.get_symbol();
                if (sym.is_function())
                {
                    extra_decl_sym.insert(sym);
                }
            }

            virtual void visit(const Nodecl::StructuredValue &node)
            {
                TL::Type t = node.get_type();
                walk(node.get_items());

                if (t.is_named_class())
                {
                    extra_decl_sym.insert(t.get_symbol());
                }
            }
    };

    static TL::Symbol new_function_symbol(Scope sc,
            const std::string& name,
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

            param->entity_specs.is_parameter = 1;

            param->type_information = get_unqualified_type(type_it->get_internal_type());

            P_LIST_ADD(entry->entity_specs.related_symbols,
                    entry->entity_specs.num_related_symbols,
                    param);

            it_ptypes->is_ellipsis = 0;
            it_ptypes->nonadjusted_type_info = NULL;
            it_ptypes->type_info = get_indirect_type(param);
        }

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

        Nodecl::NodeclBase context = Nodecl::Context::make(stmt_list, function_symbol.get_related_scope(), "", 0);

        function_code = Nodecl::FunctionCode::make(context, 
                Nodecl::NodeclBase::null(),
                Nodecl::NodeclBase::null(),
                function_symbol,
                "", 0);
    }

    void LoweringVisitor::emit_outline(OutlineInfo& outline_info,
            Nodecl::NodeclBase original_statements,
            Source body_source,
            const std::string& outline_name,
            TL::Symbol structure_symbol)
    {
        TL::Symbol current_function = original_statements.retrieve_context().get_decl_context().current_scope->related_entry;

        if (current_function.is_nested_function())
        {
            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                running_error("%s: error: nested functions are not supported\n", 
                        original_statements.get_locus().c_str());
            if (IS_FORTRAN_LANGUAGE)
                running_error("%s: error: internal subprograms are not supported\n", 
                        original_statements.get_locus().c_str());
        }

        TL::ObjectList<std::string> parameter_names;
        TL::ObjectList<TL::Type> parameter_types;

        Source unpack_code, unpacked_arguments, cleanup_code, private_entities, extra_declarations;

        TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();
        for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            switch ((*it)->get_sharing())
            {
                case OutlineDataItem::SHARING_PRIVATE:
                    {
                        TL::Symbol sym = (*it)->get_symbol();

                        std::string name;
                        TL::Type t;
                        if (sym.is_valid())
                        {
                            name = sym.get_name();
                            t = sym.get_type();
                        }
                        else
                        {
                            name = (*it)->get_field_name();
                            t = (*it)->get_in_outline_type();
                        }

                        if (IS_C_LANGUAGE
                                || IS_CXX_LANGUAGE)
                        {
                            private_entities
                                << as_type(t) << " " << name << ";"
                                ;
                        }
                        else if (IS_FORTRAN_LANGUAGE)
                        {
                            // @IS_VARIABLE@ means that this symbol must already be assumed a variable
                            //
                            // Fortran FE is very lax and this symbol would be left as a SK_UNDEFINED
                            // which is a kind of symbol that the C/C++ FE does not know anything about
                            private_entities
                                << as_type(t) << ", @IS_VARIABLE@ :: " << name << "\n"
                                ;
                        }
                        else
                        {
                            internal_error("Code unreachable", 0);
                        }

                        break;
                    }
                case OutlineDataItem::SHARING_SHARED:
                case OutlineDataItem::SHARING_CAPTURE:
                case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
                    {
                        TL::Type param_type = (*it)->get_in_outline_type();

                        switch ((*it)->get_item_kind())
                        {
                            case OutlineDataItem::ITEM_KIND_NORMAL:
                            case OutlineDataItem::ITEM_KIND_DATA_DIMENSION:
                                {
                                    parameter_names.append((*it)->get_field_name());
                                    break;
                                }
                            case OutlineDataItem::ITEM_KIND_DATA_ADDRESS:
                                {
                                    param_type = TL::Type::get_void_type().get_pointer_to();
                                    parameter_names.append("ptr_" + (*it)->get_field_name());

                                    private_entities
                                        << as_type((*it)->get_in_outline_type()) 
                                        << " " << (*it)->get_field_name() 
                                        << " = " << "(" << as_type((*it)->get_in_outline_type()) << ") ptr_" << (*it)->get_field_name() 
                                        << ";"
                                        ;

                                    break;
                                }
                            default:
                                {
                                    internal_error("Code unreachable", 0);
                                }
                        }

                        parameter_types.append(param_type);

                        Source argument;
                        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                        {
                            // Normal shared items are passed by reference from a pointer,
                            // derreference here
                            if ((*it)->get_sharing() == OutlineDataItem::SHARING_SHARED
                                    && (*it)->get_item_kind() == OutlineDataItem::ITEM_KIND_NORMAL)
                            {
                                argument << "*(args." << (*it)->get_field_name() << ")";
                            }
                            // Any other thing is passed by value
                            else
                            {
                                argument << "args." << (*it)->get_field_name();
                            }

                            if (IS_CXX_LANGUAGE
                                    && (*it)->get_allocation_policy() == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DESTROY)
                            {
                                internal_error("Not yet implemented: call the destructor", 0);
                            }
                        }
                        else if (IS_FORTRAN_LANGUAGE)
                        {
                            argument << "args % " << (*it)->get_field_name();

                            if (((*it)->get_allocation_policy()
                                        & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE ==
                                        OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE)
                                    || ((*it)->get_allocation_policy()
                                        & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_POINTER ==
                                        OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_POINTER))
                            {
                                cleanup_code
                                    << "DEALLOCATE(args % " << (*it)->get_field_name() << ")\n"
                                    ;
                            }
                        }
                        else
                        {
                            internal_error("running error", 0);
                        }
                        unpacked_arguments.append_with_separator(argument, ", ");
                        break;
                    }
                case OutlineDataItem::SHARING_REDUCTION:
                    {
                        // This is a mixture of private and shared
                        // A private is emitted for the partial reduction
                        // Such partial reduction must be initialized with the entity
                        TL::Symbol sym = (*it)->get_symbol();

                        std::string name;
                        TL::Type t;
                        if (sym.is_valid())
                        {
                            name = sym.get_name();
                            t = sym.get_type();
                        }
                        else
                        {
                            name = (*it)->get_field_name();
                            t = (*it)->get_in_outline_type();
                        }

                        if (IS_C_LANGUAGE
                                || IS_CXX_LANGUAGE)
                        {
                            private_entities
                                << as_type(t) << " " << name << " = " << as_expression((*it)->get_reduction_info()->get_identity().shallow_copy()) << ";"
                                ;
                        }
                        else if (IS_FORTRAN_LANGUAGE)
                        {
                            // @IS_VARIABLE@ means that this symbol must already be assumed a variable
                            //
                            // Fortran FE is very lax and this symbol would be left as a SK_UNDEFINED
                            // which is a kind of symbol that the C/C++ FE does not know anything about
                            private_entities
                                << as_type(t) << ", @IS_VARIABLE@ :: " << name << "\n"
                                << name << " = " << as_expression((*it)->get_reduction_info()->get_identity().shallow_copy()) << "\n"
                                ;
                        }
                        else
                        {
                            internal_error("Code unreachable", 0);
                        }

                        // Note here that we use the same type as the field for convenience
                        TL::Type param_type = (*it)->get_field_type();
                        if (IS_FORTRAN_LANGUAGE)
                        {
                            param_type = param_type.get_lvalue_reference_to();
                        }

                        parameter_names.append("rdp_" + (*it)->get_field_name());
                        parameter_types.append(param_type);

                        Source argument;
                        // Now the shared part
                        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                        {
                            // Normal shared items are passed by reference from a pointer,
                            // derreference here
                            argument << "args." << (*it)->get_field_name();
                        }
                        else if (IS_FORTRAN_LANGUAGE)
                        {
                            argument << "args % " << (*it)->get_field_name();
                        }
                        unpacked_arguments.append_with_separator(argument, ", ");

                        break;
                    }
                default:
                    {
                        internal_error("Unexpected data sharing kind", 0);
                    }
            }
        }

        TL::Symbol unpacked_function = new_function_symbol(
                // We want a sibling of the current function
                current_function.get_scope(),
                outline_name + "_unpacked",
                TL::Type::get_void_type(),
                parameter_names,
                parameter_types);

        outline_info.set_unpacked_function_symbol(unpacked_function);

        // FIXME - C++ static for members and such
        ObjectList<std::string> structure_name;
        structure_name.append("args");
        ObjectList<TL::Type> structure_type;
        structure_type.append(
                TL::Type(get_user_defined_type( structure_symbol.get_internal_symbol())).get_lvalue_reference_to() 
                );

        TL::Symbol outline_function = new_function_symbol(
                current_function.get_scope(),
                outline_name,
                TL::Type::get_void_type(),
                structure_name,
                structure_type);

        if (IS_FORTRAN_LANGUAGE
                && current_function.is_in_module())
        {
            scope_entry_t* module_sym = current_function.in_module().get_internal_symbol();

            unpacked_function.get_internal_symbol()->entity_specs.in_module = module_sym;
            P_LIST_ADD(
                    module_sym->entity_specs.related_symbols,
                    module_sym->entity_specs.num_related_symbols,
                    unpacked_function.get_internal_symbol());

            unpacked_function.get_internal_symbol()->entity_specs.is_module_procedure = 1;

            outline_function.get_internal_symbol()->entity_specs.in_module = module_sym;
            P_LIST_ADD(
                    module_sym->entity_specs.related_symbols,
                    module_sym->entity_specs.num_related_symbols,
                    outline_function.get_internal_symbol());
            outline_function.get_internal_symbol()->entity_specs.is_module_procedure = 1;
        }

        Nodecl::NodeclBase unpacked_function_code, unpacked_function_body;
        build_empty_body_for_function(unpacked_function, 
                unpacked_function_code,
                unpacked_function_body);
        Nodecl::Utils::append_to_top_level_nodecl(unpacked_function_code);

        Nodecl::NodeclBase body_placeholder;
        Source unpacked_source;
        if (!IS_FORTRAN_LANGUAGE)
        {
            unpacked_source
                << "{";
        }
        unpacked_source
            << extra_declarations
            << private_entities
            << statement_placeholder(body_placeholder)
            ;
        if (!IS_FORTRAN_LANGUAGE)
        {
            unpacked_source
                << "}";
        }

        // Fortran may require more symbols
        if (IS_FORTRAN_LANGUAGE)
        {
            FortranExtraDeclsVisitor fun_visitor;
            fun_visitor.walk(original_statements);

            extra_declarations
                << "IMPLICIT NONE\n";

            // Insert extra symbols
            TL::ReferenceScope ref_scope(unpacked_function_body);
            decl_context_t decl_context = ref_scope.get_scope().get_decl_context();

            for (ObjectList<Symbol>::iterator it = fun_visitor.extra_decl_sym.begin();
                    it != fun_visitor.extra_decl_sym.end();
                    it++)
            {
                // Insert the name in the context...
                TL::Scope sc = ref_scope.get_scope();
                ::insert_entry(decl_context.current_scope, it->get_internal_symbol());
            }
        }

        Nodecl::NodeclBase new_unpacked_body = unpacked_source.parse_statement(unpacked_function_body);
        unpacked_function_body.integrate(new_unpacked_body);

        FORTRAN_LANGUAGE()
        {
            // Parse in C
            Source::source_language = SourceLanguage::C;
        }
        Nodecl::NodeclBase outline_body_code = body_source.parse_statement(body_placeholder);
        FORTRAN_LANGUAGE()
        {
            Source::source_language = SourceLanguage::Current;
        }
        body_placeholder.integrate(outline_body_code);

        Nodecl::NodeclBase outline_function_code, outline_function_body;
        build_empty_body_for_function(outline_function, 
                outline_function_code,
                outline_function_body);
        Nodecl::Utils::append_to_top_level_nodecl(outline_function_code);

        Source outline_src;
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            outline_src
                << "{"
                <<      unpack_code
                <<      outline_name << "_unpacked(" << unpacked_arguments << ");"
                <<      cleanup_code
                << "}"
                ;
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            outline_src
                << unpack_code << "\n"
                << "CALL " << outline_name << "_unpacked(" << unpacked_arguments << ")\n"
                << cleanup_code
                ;
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

        Nodecl::NodeclBase new_outline_body = outline_src.parse_statement(outline_function_body);
        outline_function_body.integrate(new_outline_body);
    }


    std::string LoweringVisitor::get_outline_name(TL::Symbol function_symbol)
    {
        std::string outline_name;

        Counter& task_counter = CounterManager::get_counter("nanos++-outline");
        std::stringstream ss;
        ss << "ol_" << function_symbol.get_name() << "_" << (int)task_counter;
        outline_name = ss.str();

        task_counter++;

        return outline_name;
    }

} }
