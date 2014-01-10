/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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

#include "tl-devices.hpp"
#include "nanox-smp.hpp"

#include "tl-source.hpp"
#include "tl-counters.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-outline-info.hpp"
#include "tl-replace.hpp"
#include "tl-compilerpipeline.hpp"

#include "tl-nodecl-utils-c.hpp"
#include "tl-nodecl-utils-fortran.hpp"
#include "tl-symbol-utils.hpp"

#include "codegen-phase.hpp"
#include "codegen-fortran.hpp"

#include "cxx-cexpr.h"
#include "cxx-profile.h"
#include "cxx-driver-utils.h"
#include "cxx-symbol-deep-copy.h"

#include <errno.h>
#include <string.h>

using TL::Source;

namespace TL { namespace Nanox {

    static std::string smp_outline_name(const std::string &task_name)
    {
        return "smp_" + task_name;
    }

    void DeviceSMP::create_outline(CreateOutlineInfo& info,
            Nodecl::NodeclBase& outline_placeholder,
            Nodecl::NodeclBase& output_statements,
            Nodecl::Utils::SimpleSymbolMap* &symbol_map)
    {
        // Unpack DTO
        const std::string& outline_name = smp_outline_name(info._outline_name);
        const Nodecl::NodeclBase& task_statements = info._task_statements;
        const Nodecl::NodeclBase& original_statements = info._original_statements;
        bool is_function_task = info._called_task.is_valid();
        TL::ObjectList<OutlineDataItem*> data_items = info._data_items;

        output_statements = task_statements;

        TL::Symbol current_function =
            original_statements.retrieve_context().get_decl_context().current_scope->related_entry;
        if (current_function.is_nested_function())
        {
            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                running_error("%s: error: nested functions are not supported\n",
                        original_statements.get_locus_str().c_str());
            // if (IS_FORTRAN_LANGUAGE)
            //     running_error("%s: error: internal subprograms are not supported\n",
            //             original_statements.get_locus().c_str());
        }

        symbol_map = new Nodecl::Utils::SimpleSymbolMap();

        Source extra_declarations;
        Source final_statements, initial_statements;

        // *** Unpacked (and forward in Fortran) function ***
        TL::Symbol unpacked_function, forward_function;
        if (IS_FORTRAN_LANGUAGE)
        {
            forward_function = new_function_symbol_forward(
                    current_function,
                    outline_name + "_forward",
                    info);
            unpacked_function = new_function_symbol_unpacked(
                    current_function,
                    outline_name + "_unpack",
                    info,
                    // out
                    symbol_map,
                    initial_statements,
                    final_statements);
        }
        else
        {
            unpacked_function = new_function_symbol_unpacked(
                    current_function,
                    outline_name + "_unpacked",
                    info,
                    // out
                    symbol_map,
                    initial_statements,
                    final_statements);
        }

        Nodecl::NodeclBase unpacked_function_code, unpacked_function_body;
        SymbolUtils::build_empty_body_for_function(unpacked_function,
                unpacked_function_code,
                unpacked_function_body);

        Nodecl::Utils::append_to_top_level_nodecl(unpacked_function_code);

        Source unpacked_source;
        if (!IS_FORTRAN_LANGUAGE)
        {
            unpacked_source
                << "{";
        }
        unpacked_source
            << extra_declarations
            << initial_statements
            << statement_placeholder(outline_placeholder)
            << final_statements
            ;
        if (!IS_FORTRAN_LANGUAGE)
        {
            unpacked_source
                << "}";
        }

        // Aftere this we can use outline_placeholder
        Nodecl::NodeclBase new_unpacked_body = unpacked_source.parse_statement(unpacked_function_body);

        if (IS_FORTRAN_LANGUAGE)
        {
            TL::Scope unpacked_function_scope = unpacked_function.get_related_scope();

            Nodecl::Utils::Fortran::ExtraDeclsVisitor fun_visitor(symbol_map,
                    unpacked_function_scope,
                    current_function);
            if (is_function_task)
            {
                fun_visitor.insert_extra_symbol(info._called_task);
            }
            fun_visitor.insert_extra_symbols(task_statements);

            if (current_function.is_in_module())
            {
                TL::Symbol in_module = current_function.in_module();
                Nodecl::Utils::Fortran::append_used_modules(
                        original_statements.retrieve_context(),
                        in_module.get_related_scope());
            }

            Nodecl::Utils::Fortran::append_used_modules(
                    original_statements.retrieve_context(),
                    unpacked_function_scope);

            if (is_function_task)
            {
                Nodecl::Utils::Fortran::append_used_modules(
                        info._called_task.get_related_scope(),
                        unpacked_function_scope);
            }

            // Add also used types
            add_used_types(data_items, unpacked_function.get_related_scope());

            // Now get all the needed internal functions and duplicate them in the outline
            Nodecl::Utils::Fortran::InternalFunctions internal_functions;
            internal_functions.walk(info._original_statements);

            duplicate_internal_subprograms(internal_functions.function_codes,
                    unpacked_function_scope,
                    symbol_map,
                    output_statements);

            extra_declarations
                << "IMPLICIT NONE\n";
        }
        else if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            TL::Scope scope_in_outline = outline_placeholder.retrieve_context();

            Nodecl::Utils::C::ExtraDeclsVisitor fun_visitor(symbol_map,
                    scope_in_outline,
                    current_function);

            if (is_function_task
                    && info._called_task.get_scope().is_block_scope()
                    && !info._called_task.is_nested_function())
            {
                fun_visitor.insert_extra_symbol(info._called_task);
            }
            fun_visitor.insert_extra_symbols(task_statements);

            if (IS_CXX_LANGUAGE)
            {
                if (!unpacked_function.is_member())
                {
                    Nodecl::NodeclBase nodecl_decl = Nodecl::CxxDecl::make(
                            /* optative context */ nodecl_null(),
                            unpacked_function,
                            original_statements.get_locus());
                    Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, nodecl_decl);
                }
            }

            if (IS_C_LANGUAGE)
            {
                // Now get all the needed nested functions and duplicate them in the outline
                Nodecl::Utils::C::NestedFunctions nested_functions;
                nested_functions.walk(info._original_statements);

                duplicate_nested_functions(nested_functions.function_codes,
                        scope_in_outline,
                        symbol_map,
                        output_statements);
            }
        }

        unpacked_function_body.replace(new_unpacked_body);

        // **** Outline function *****
        ObjectList<std::string> structure_name;
        structure_name.append("args");
        ObjectList<TL::Type> structure_type;
        structure_type.append(
                TL::Type(get_user_defined_type(info._arguments_struct.get_internal_symbol())).get_lvalue_reference_to()
                );

        TL::Symbol outline_function = SymbolUtils::new_function_symbol(
                current_function,
                outline_name,
                TL::Type::get_void_type(),
                structure_name,
                structure_type);

        Nodecl::NodeclBase outline_function_code, outline_function_body;
        SymbolUtils::build_empty_body_for_function(outline_function,
                outline_function_code,
                outline_function_body);
        Nodecl::Utils::append_to_top_level_nodecl(outline_function_code);

        // Prepare arguments for the call to the unpack (or forward in Fortran)
        TL::Scope outline_function_scope(outline_function_body.retrieve_context());
        TL::Symbol structure_symbol = outline_function_scope.get_symbol_from_name("args");
        ERROR_CONDITION(!structure_symbol.is_valid(), "Argument of outline function not found", 0);

        Source unpacked_arguments, cleanup_code;
        for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            if (!is_function_task
                    && (*it)->get_is_cxx_this())
                continue;

            switch ((*it)->get_sharing())
            {
                case OutlineDataItem::SHARING_PRIVATE:
                case OutlineDataItem::SHARING_ALLOCA:
                    {
                        // Do nothing
                        break;
                    }
                case OutlineDataItem::SHARING_SHARED:
                case OutlineDataItem::SHARING_SHARED_WITH_CAPTURE:
                case OutlineDataItem::SHARING_SHARED_ALLOCA:
                case OutlineDataItem::SHARING_CAPTURE:
                case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
                    {
                        TL::Type param_type = (*it)->get_in_outline_type();

                        Source argument;
                        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                        {
                            // Normal shared items are passed by reference from a pointer,
                            // derreference here
                            if (
                                 ((*it)->get_sharing() == OutlineDataItem::SHARING_SHARED)
                                    && !(IS_CXX_LANGUAGE && (*it)->get_symbol().get_name() == "this"))
                            {
                                if (!param_type.no_ref().depends_on_nonconstant_values())
                                {
                                    argument << "*(args." << (*it)->get_field_name() << ")";
                                }
                                else
                                {
                                    TL::Type ptr_type = (*it)->get_in_outline_type().references_to().get_pointer_to();
                                    TL::Type cast_type = rewrite_type_of_vla_in_outline(ptr_type, data_items, structure_symbol);

                                    argument << "*((" << as_type(cast_type) << ")args." << (*it)->get_field_name() << ")";
                                }
                            }
                            // Any other parameter is bound to the storage of the struct
                            else
                            {
                                if (!param_type.no_ref().depends_on_nonconstant_values())
                                {
                                    argument << "args." << (*it)->get_field_name();
                                }
                                else
                                {
                                    TL::Type cast_type = rewrite_type_of_vla_in_outline(param_type, data_items, structure_symbol);
                                    argument << "(" << as_type(cast_type) << ")args." << (*it)->get_field_name();
                                }
                            }
                        }
                        else if (IS_FORTRAN_LANGUAGE)
                        {
                            argument << "args % " << (*it)->get_field_name();

                            bool is_allocatable = (*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE;
                            bool is_pointer = (*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_POINTER;

                            if (is_allocatable
                                    || is_pointer)
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
                        // // Pass the original reduced variable as if it were a shared
                        Source argument;
                        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                        {
                            argument << "*(args." << (*it)->get_field_name() << ")";
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

        Source outline_src,
               instrument_before,
               instrument_after;

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
           Source unpacked_function_call;
            if (IS_CXX_LANGUAGE
                    && !is_function_task
                    && current_function.is_member()
                    && !current_function.is_static())
            {
                unpacked_function_call << "args.this_->";
            }

           unpacked_function_call
               << unpacked_function.get_qualified_name() << "(" << unpacked_arguments << ");";

            outline_src
                << "{"
                <<      instrument_before
                <<      unpacked_function_call
                <<      instrument_after
                <<      cleanup_code
                << "}"
                ;

            if (IS_CXX_LANGUAGE)
            {
                if (!outline_function.is_member())
                {
                    Nodecl::NodeclBase nodecl_decl = Nodecl::CxxDecl::make(
                            /* optative context */ nodecl_null(),
                            outline_function,
                            original_statements.get_locus());
                    Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, nodecl_decl);
                }
            }
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            Source outline_function_addr;

            outline_src
                << instrument_before << "\n"
                << "CALL " << outline_name << "_forward(" << outline_function_addr << unpacked_arguments << ")\n"
                << instrument_after << "\n"
                << cleanup_code
                ;

            outline_function_addr << "LOC(" << unpacked_function.get_name() << ")";
            if (!unpacked_arguments.empty())
            {
                outline_function_addr << ", ";
            }

            // Copy USEd information to the outline and forward functions
            TL::Symbol *functions[] = { &outline_function, &forward_function, NULL };

            for (int i = 0; functions[i] != NULL; i++)
            {
                TL::Symbol &function(*functions[i]);

                Nodecl::Utils::Fortran::append_used_modules(original_statements.retrieve_context(),
                        function.get_related_scope());

                add_used_types(data_items, function.get_related_scope());
            }

            // Generate ancillary code in C
            add_forward_function_code_to_extra_c_code(outline_name, data_items, outline_placeholder);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

        if (instrumentation_enabled())
        {
            get_instrumentation_code(
                    info._called_task,
                    outline_function,
                    outline_function_body,
                    info._task_label,
                    original_statements.get_locus(),
                    instrument_before,
                    instrument_after);
        }

        Nodecl::NodeclBase new_outline_body = outline_src.parse_statement(outline_function_body);
        outline_function_body.replace(new_outline_body);
    }

    DeviceSMP::DeviceSMP()
        : DeviceProvider(/* device_name */ std::string("smp"))
    {
        set_phase_name("Nanox SMP support");
        set_phase_description("This phase is used by Nanox phases to implement SMP device support");
    }

    void DeviceSMP::pre_run(DTO& dto)
    {
    }

    void DeviceSMP::run(DTO& dto)
    {
        DeviceProvider::run(dto);
    }

    void DeviceSMP::get_device_descriptor(DeviceDescriptorInfo& info,
            Source &ancillary_device_description,
            Source &device_descriptor,
            Source &fortran_dynamic_init)
    {
        const std::string& outline_name = smp_outline_name(info._outline_name);
        const std::string& arguments_struct = info._arguments_struct;
        TL::Symbol current_function = info._current_function;

        //FIXME: This is confusing. In a future, we should get the template
        //arguments of the outline function and print them

        //Save the original name of the current function
        std::string original_name = current_function.get_name();

        current_function.set_name(outline_name);
        Nodecl::NodeclBase code = current_function.get_function_code();

        Nodecl::Context context = (code.is<Nodecl::TemplateFunctionCode>())
            ? code.as<Nodecl::TemplateFunctionCode>().get_statements().as<Nodecl::Context>()
            : code.as<Nodecl::FunctionCode>().get_statements().as<Nodecl::Context>();

        TL::Scope function_scope = context.retrieve_context();
        std::string qualified_name = current_function.get_qualified_name(function_scope);

        // Restore the original name of the current function
        current_function.set_name(original_name);

        if (!IS_FORTRAN_LANGUAGE)
        {
            // Extra cast for solving some issues of GCC 4.6.* and lowers (this
            // issues seem to be fixed in GCC 4.7 =D)
            std::string ref = IS_CXX_LANGUAGE ? "&" : "*";
            std::string extra_cast = "(void(*)(" + arguments_struct + ref + "))";

            ancillary_device_description
                << "static nanos_smp_args_t " << outline_name << "_args = {"
                << ".outline = ((void(*)(void*)) " << "(" << extra_cast << " &" << qualified_name << "))"
                << "};"
                ;
            device_descriptor
                << "{"
                << /* factory */ "&nanos_smp_factory, &" << outline_name << "_args"
                << "}"
                ;
        }
        else
        {
            ancillary_device_description
                << "static nanos_smp_args_t " << outline_name << "_args;"
                ;

            device_descriptor
                << "{"
                // factory, arg
                << "0, 0"
                << "}"
                ;

            fortran_dynamic_init
                << outline_name << "_args.outline = (void(*)(void*))&" << outline_name << ";"
                << "nanos_wd_const_data.devices[" << info._fortran_device_index << "].factory = &nanos_smp_factory;"
                << "nanos_wd_const_data.devices[" << info._fortran_device_index << "].arg = &" << outline_name << "_args;"
                ;
        }
    }

    bool DeviceSMP::remove_function_task_from_original_source() const
    {
        return false;
    }

    void DeviceSMP::copy_stuff_to_device_file(
            const TL::ObjectList<Nodecl::NodeclBase>& stuff_to_be_copied)
    {
        // This function is expressly empty
    }

    void DeviceSMP::phase_cleanup(DTO& data_flow)
    {
        if (_extra_c_code.is_null())
            return;

        std::string original_filename = TL::CompilationProcess::get_current_file().get_filename();
        std::string new_filename = "smp_aux_nanox_outline_file_" + original_filename  + ".c";

        FILE* ancillary_file = fopen(new_filename.c_str(), "w");
        if (ancillary_file == NULL)
        {
            running_error("%s: error: cannot open file '%s'. %s\n",
                    original_filename.c_str(),
                    new_filename.c_str(),
                    strerror(errno));
        }

        CURRENT_CONFIGURATION->source_language = SOURCE_LANGUAGE_C;

        compilation_configuration_t* configuration = ::get_compilation_configuration("auxcc");
        ERROR_CONDITION (configuration == NULL, "auxcc profile is mandatory when using Fortran", 0);

        // Make sure phases are loaded (this is needed for codegen)
        load_compiler_phases(configuration);

        TL::CompilationProcess::add_file(new_filename, "auxcc");

        ::mark_file_for_cleanup(new_filename.c_str());

        Codegen::CodegenPhase* phase = reinterpret_cast<Codegen::CodegenPhase*>(configuration->codegen_phase);
        phase->codegen_top_level(_extra_c_code, ancillary_file);

        CURRENT_CONFIGURATION->source_language = SOURCE_LANGUAGE_FORTRAN;

        fclose(ancillary_file);
        // Do not forget the clear the code for next files
        _extra_c_code.get_internal_nodecl() = nodecl_null();
    }

} }

EXPORT_PHASE(TL::Nanox::DeviceSMP);
