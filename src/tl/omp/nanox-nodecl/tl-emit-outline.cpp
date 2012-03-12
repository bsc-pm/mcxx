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
#include "tl-nodecl-alg.hpp"
#include "tl-outline-info.hpp"
#include "tl-replace.hpp"

#include "codegen-phase.hpp"
#include "codegen-fortran.hpp"

using TL::Source;

namespace TL { namespace Nanox {

    static TL::Symbol new_function_symbol(Scope sc,
            const std::string& name,
            TL::Type return_type,
            ObjectList<std::string> parameter_names,
            ObjectList<TL::Type> parameter_types)
    {
        // FIXME - Wrap
        decl_context_t decl_context = sc.get_decl_context();

        scope_entry_t* entry = new_symbol(decl_context, decl_context.current_scope, name.c_str());

        entry->kind = SK_FUNCTION;
        entry->file = "";
        entry->line = 0;

        ERROR_CONDITION(parameter_names.size() != parameter_types.size(), "Mismatch between names and types", 0);

        decl_context_t function_context = new_function_context(decl_context);
        function_context = new_block_context(function_context);
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
            param->kind = SK_VARIABLE;
            param->file = "";
            param->line = 0;

            param->defined = 1;

            param->entity_specs.is_parameter = 1;

            param->type_information = type_it->get_internal_type();

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
            Nodecl::NodeclBase body,
            const std::string& outline_name,
            TL::Symbol structure_symbol)
    {
        TL::Symbol current_function = body.retrieve_context().get_decl_context().current_scope->related_entry;

        if (current_function.is_nested_function())
        {
            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                running_error("%s: error: nested functions are not supported\n", 
                        body.get_locus().c_str());
            if (IS_FORTRAN_LANGUAGE)
                running_error("%s: error: internal subprograms are not supported\n", 
                        body.get_locus().c_str());
        }

        TL::ObjectList<std::string> parameter_names;
        TL::ObjectList<TL::Type> parameter_types;

        Source unpack_code, unpacked_arguments, cleanup_code;

        TL::ObjectList<OutlineDataItem> data_items = outline_info.get_data_items();
        for (TL::ObjectList<OutlineDataItem>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            if (!it->get_symbol().is_valid())
                continue;

            switch (it->get_sharing())
            {
                case OutlineDataItem::SHARING_PRIVATE:
                    {
                        break;
                    }
                case OutlineDataItem::SHARING_SHARED:
                case OutlineDataItem::SHARING_CAPTURE:
                case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
                    {
                        switch (it->get_item_kind())
                        {
                            case OutlineDataItem::ITEM_KIND_NORMAL:
                            case OutlineDataItem::ITEM_KIND_DATA_DIMENSION:
                                {
                                    parameter_names.append(it->get_field_name());
                                    break;
                                }
                            case OutlineDataItem::ITEM_KIND_DATA_ADDRESS:
                                {
                                    parameter_names.append("ptr_" + it->get_field_name());

                                    break;
                                }
                            default:
                                {
                                    internal_error("Code unreachable", 0);
                                }
                        }

                        TL::Type param_type = it->get_in_outline_type();
                        parameter_types.append(param_type);

                        Source argument;
                        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                        {
                            // Normal shared items are passed by reference from a pointer,
                            // derreference here
                            if (it->get_sharing() == OutlineDataItem::SHARING_SHARED
                                    && it->get_item_kind() == OutlineDataItem::ITEM_KIND_NORMAL)
                            {
                                argument << "*(args." << it->get_field_name() << ")";
                            }
                            // Any other thing is passed by value
                            else
                            {
                                argument << "args." << it->get_field_name();
                            }

                            if (IS_CXX_LANGUAGE
                                    && it->get_allocation_policy() != OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DESTROY)
                            {
                                internal_error("Not yet implemented: call the destructor", 0);
                            }
                        }
                        else if (IS_FORTRAN_LANGUAGE)
                        {
                            argument << "args % " << it->get_field_name();

                            if (it->get_allocation_policy() == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE)
                            {
                                cleanup_code
                                    << "DEALLOCATE(args % " << it->get_field_name() << ")\n"
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

        TL::ReplaceSymbols replace_symbols;

        Source replaced_body_src;
        replaced_body_src << replace_symbols.replace(body);

        Nodecl::NodeclBase new_unpacked_body = replaced_body_src.parse_statement(unpacked_function_body);
        unpacked_function_body.replace(new_unpacked_body);

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
        outline_function_body.replace(new_outline_body);
    }

#if 0
    void LoweringVisitor::emit_outline(OutlineInfo& outline_info,
            Nodecl::NodeclBase body,
            const std::string& outline_name,
            const std::string& structure_name)
    {
        Source outline, 
            unpacked_arguments, 
            unpacked_parameters, 
            unpacked_parameter_declarations, // Fortran only
            unpack_code, 
            private_entities, 
            cleanup_code;

        // Fortran extras
        Source declaration_part,
               // Not filled at the moment
               internal_subprograms;

        Nodecl::NodeclBase placeholder_body;

        // FIXME - Factorize this as a common action of "create a function"
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            outline
                << "void " << outline_name << "_unpacked(" << unpacked_parameters << ")"
                << "{"
                <<      private_entities
                <<      statement_placeholder(placeholder_body)
                << "}"
                << "void " << outline_name << "(" << structure_name << " @ref@ args)"
                << "{"
                <<      unpack_code
                <<      outline_name << "_unpacked(" << unpacked_arguments << ");"
                <<      cleanup_code
                << "}"
                ;
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            outline
                << "SUBROUTINE " << outline_name << "_unpacked(" << unpacked_parameters << ")\n"
                <<      declaration_part
                <<      unpacked_parameter_declarations << "\n"
                <<      private_entities << "\n"
                <<      statement_placeholder(placeholder_body) << "\n"
                <<      internal_subprograms
                << "END SUBROUTINE " << outline_name << "_unpacked\n"

                << "SUBROUTINE " << outline_name << "(args)\n"
                <<      "IMPLICIT NONE\n"
                <<      "TYPE(" << structure_name << ") :: args\n"
                <<      "INTERFACE\n"
                <<           "SUBROUTINE " << outline_name << "_unpacked(" << unpacked_parameters << ")\n"
                <<                "IMPLICIT NONE\n"
                <<                unpacked_parameter_declarations << "\n"
                <<           "END SUBROUTINE\n"
                <<      "END INTERFACE\n"
                <<      unpack_code << "\n"
                <<      "CALL " << outline_name << "_unpacked(" << unpacked_arguments << ")\n"
                <<      cleanup_code
                << "END SUBROUTINE " << outline_name << "\n"
                ;
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

        TL::ReplaceSymbols replace_symbols;
        TL::ObjectList<Symbol> do_not_declare;

        TL::ObjectList<OutlineDataItem> data_items = outline_info.get_data_items();
        for (TL::ObjectList<OutlineDataItem>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            if (!it->get_symbol().is_valid())
                continue;

            // We are manually declaring this symbol, do not declare it at all
            do_not_declare.insert(it->get_symbol());

            switch (it->get_sharing())
            {
                case OutlineDataItem::SHARING_PRIVATE:
                    {
                        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                        {
                            private_entities 
                                << it->get_field_type().get_declaration(it->get_symbol().get_scope(), it->get_field_name())
                                << ";"
                                ;
                        }
                        else if (IS_FORTRAN_LANGUAGE)
                        {
                            private_entities 
                                << it->get_field_type().get_fortran_declaration(
                                        it->get_symbol().get_scope(), 
                                        it->get_field_name()) 
                                << "\n"
                                ;
                        }
                        break;
                    }
                case OutlineDataItem::SHARING_SHARED:
                case OutlineDataItem::SHARING_CAPTURE:
                case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
                    {
                        Source parameter;
                        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                        {
                            switch (it->get_item_kind())
                            {
                                case OutlineDataItem::ITEM_KIND_NORMAL:
                                case OutlineDataItem::ITEM_KIND_DATA_DIMENSION:
                                    {
                                        parameter << it->get_field_type().get_declaration(it->get_symbol().get_scope(), it->get_field_name());
                                        break;
                                    }
                                case OutlineDataItem::ITEM_KIND_DATA_ADDRESS:
                                    {
                                        parameter 
                                            << it->get_field_type().get_declaration(it->get_symbol().get_scope(), "ptr_" + it->get_field_name());

                                        // Note the type being emitted here is using as names those of the fields 
                                        // FIXME: This will not work in C++ (where members will appear as A::b)
                                        // We need to update the type again... with the real members but this requires parsing the function first
                                        private_entities
                                            << it->get_in_outline_type().get_declaration(it->get_symbol().get_scope(), it->get_field_name())
                                            << " = "
                                            << "(" << it->get_in_outline_type().get_declaration(it->get_symbol().get_scope(), "") << ")"
                                            << "ptr_" << it->get_field_name()
                                            << ";"
                                            ;
                                        break;
                                    }
                                default:
                                    {
                                        internal_error("Code unreachable", 0);
                                    }
                            }
                        }
                        else if (IS_FORTRAN_LANGUAGE)
                        {
                            parameter << it->get_symbol().get_name();

                            unpacked_parameter_declarations 
                                << it->get_in_outline_type().get_fortran_declaration(
                                        it->get_symbol().get_scope(), 
                                        it->get_field_name(), 
                                        Type::PARAMETER_DECLARATION) 
                                << "\n"
                                ;
                        }
                        else
                        {
                            internal_error("Code unreachable", 0);
                        }
                        unpacked_parameters.append_with_separator(parameter, ", ");

                        Source argument;
                        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                        {
                            // Normal shared items are passed by reference from a pointer,
                            // derreference here
                            if (it->get_sharing() == OutlineDataItem::SHARING_SHARED
                                    && it->get_item_kind() == OutlineDataItem::ITEM_KIND_NORMAL)
                            {
                                argument << "*(args." << it->get_field_name() << ")";
                            }
                            // Any other thing is passed by value
                            else
                            {
                                argument << "args." << it->get_field_name();
                            }

                            if (IS_CXX_LANGUAGE
                                    && it->get_allocation_policy() != OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DESTROY)
                            {
                                internal_error("Not yet implemented: call the destructor", 0);
                            }
                        }
                        else if (IS_FORTRAN_LANGUAGE)
                        {
                            argument << "args % " << it->get_field_name();

                            if (it->get_allocation_policy() == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE)
                            {
                                cleanup_code
                                    << "DEALLOCATE(args % " << it->get_field_name() << ")\n"
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
                default:
                    {
                        internal_error("Unexpected data sharing kind", 0);
                    }
            }
        }

        if (IS_FORTRAN_LANGUAGE)
        {
            // Complete all what fortran needs
            Codegen::FortranBase& codegen = static_cast<Codegen::FortranBase&>(Codegen::get_current());

            declaration_part << codegen.emit_declaration_part(body, do_not_declare);
        }

        Nodecl::NodeclBase node = outline.parse_global(body);
        Nodecl::Utils::append_to_top_level_nodecl(node);

        // Now replace the body
        Source replaced_body_src;
        replaced_body_src << replace_symbols.replace(body);

        Nodecl::NodeclBase new_body = replaced_body_src.parse_statement(placeholder_body);
        placeholder_body.replace(new_body);
    }
#endif

} }
