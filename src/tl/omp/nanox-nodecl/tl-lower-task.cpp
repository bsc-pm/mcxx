/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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
#include "tl-nanos.hpp"
#include "tl-source.hpp"
#include "tl-counters.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-datareference.hpp"
#include "tl-devices.hpp"
#include "tl-symbol-utils.hpp"
#include "fortran03-typeutils.h"
#include "cxx-diagnostic.h"
#include "cxx-cexpr.h"
#include "tl-compilerpipeline.hpp"

#include "tl-lower-task-common.hpp"
#include "tl-nanox-ptr.hpp"

using TL::Source;

namespace TL { namespace Nanox {

TL::Symbol LoweringVisitor::declare_const_wd_type(int num_implementations, Nodecl::NodeclBase construct)
{
    //FIXME: The 'construct' parameter is only used to obtain the line and the filename
    std::map<int, Symbol>::iterator it = _declared_const_wd_type_map.find(num_implementations);
    if (it == _declared_const_wd_type_map.end())
    {
        std::stringstream ss;
        if (IS_C_LANGUAGE)
        {
            ss << "struct ";
        }
        ss << "nanos_const_wd_definition_" << num_implementations;

        TL::Scope sc(CURRENT_COMPILED_FILE->global_decl_context);
        TL::Symbol new_class_symbol = sc.new_symbol(ss.str());
        new_class_symbol.get_internal_symbol()->kind = SK_CLASS;
        symbol_entity_specs_set_is_user_declared(new_class_symbol.get_internal_symbol(), 1);

        type_t* new_class_type = get_new_class_type(sc.get_decl_context(), TT_STRUCT);
        const decl_context_t* class_context = new_class_context(sc.get_decl_context(), new_class_symbol.get_internal_symbol());
        TL::Scope class_scope(class_context);

        class_type_set_inner_context(new_class_type, class_context);

        new_class_symbol.get_internal_symbol()->type_information = new_class_type;

        _declared_const_wd_type_map[num_implementations] = new_class_symbol;

        TL::Symbol base_class = sc.get_symbol_from_name("nanos_const_wd_definition_t");
        ERROR_CONDITION(!base_class.is_valid(), "Invalid symbol", 0);
        {
            // Base field

            TL::Symbol field = class_scope.new_symbol("base");
            field.get_internal_symbol()->kind = SK_VARIABLE;
            symbol_entity_specs_set_is_user_declared(field.get_internal_symbol(), 1);

            symbol_entity_specs_set_is_member(field.get_internal_symbol(), 1);
            symbol_entity_specs_set_class_type(field.get_internal_symbol(), ::get_user_defined_type(new_class_symbol.get_internal_symbol()));
            symbol_entity_specs_set_access(field.get_internal_symbol(), AS_PUBLIC);

            field.get_internal_symbol()->locus = make_locus("", 0, 0);

            field.get_internal_symbol()->type_information = ::get_user_defined_type(base_class.get_internal_symbol());
            class_type_add_member(new_class_type, field.get_internal_symbol(),
                   field.get_internal_symbol()->decl_context, /* is_definition */ 1);
        }

        {
            // Devices field
            TL::Symbol devices_class = sc.get_symbol_from_name("nanos_device_t");
            ERROR_CONDITION(!devices_class.is_valid(), "Invalid symbol", 0);

            TL::Symbol field = class_scope.new_symbol("devices");
            field.get_internal_symbol()->kind = SK_VARIABLE;
            symbol_entity_specs_set_is_user_declared(field.get_internal_symbol(), 1);

            symbol_entity_specs_set_is_member(field.get_internal_symbol(), 1);
            symbol_entity_specs_set_class_type(field.get_internal_symbol(), ::get_user_defined_type(new_class_symbol.get_internal_symbol()));


            symbol_entity_specs_set_access(field.get_internal_symbol(), AS_PUBLIC);

            field.get_internal_symbol()->locus = make_locus("", 0, 0);

            field.get_internal_symbol()->type_information = 
                ::get_array_type(
                        ::get_user_defined_type(devices_class.get_internal_symbol()),
                        const_value_to_nodecl( const_value_get_signed_int(num_implementations)),
                        class_scope.get_decl_context());

            class_type_add_member(new_class_type,
                    field.get_internal_symbol(),
                    field.get_internal_symbol()->decl_context,
                    /* is_definition */ 1);
        }

        nodecl_t nodecl_output = nodecl_null();
        finish_class_type(new_class_type, 
                ::get_user_defined_type(new_class_symbol.get_internal_symbol()),
                sc.get_decl_context(), 
                make_locus("", 0, 0),
                // construct.get_filename().c_str(),
                // construct.get_line(),
                &nodecl_output);
        set_is_complete_type(new_class_type, /* is_complete */ 1);
        set_is_complete_type(get_actual_class_type(new_class_type), /* is_complete */ 1);

        if (!nodecl_is_null(nodecl_output))
        {
            std::cerr << "FIXME: finished class issues nonempty nodecl" << std::endl; 
        }

        if (IS_CXX_LANGUAGE)
        {
            Nodecl::NodeclBase nodecl_decl = Nodecl::CxxDef::make(
                    /* optative context */ nodecl_null(),
                    new_class_symbol,
                    construct.get_locus());

            TL::ObjectList<Nodecl::NodeclBase> defs =
                Nodecl::Utils::get_declarations_or_definitions_of_entity_at_top_level(base_class);
            ERROR_CONDITION(defs.empty(), "No declaration of %s not found!\n", base_class.get_name().c_str());

            // Append to the last declaration
            defs.back().append_sibling(nodecl_decl);
        }

        return new_class_symbol;
    }
    else
    {
        return it->second;
    }
}

Source LoweringVisitor::fill_const_wd_info(
        Source &struct_arg_type_name,
        bool is_untied,
        bool mandatory_creation,
        bool is_function_task,
        const std::string& wd_description,
        OutlineInfo& outline_info,
        Nodecl::NodeclBase construct)
{
    // Static stuff
    //
    // typedef struct {
    //     nanos_wd_props_t props;
    //     size_t data_alignment;
    //     size_t num_copies;
    //     size_t num_devices;
    //     size_t num_dimensions; // copies_api >= 1000
    //     const char *description; // master >= 5022
    // } nanos_const_wd_definition_t;
    // MultiMap with every implementation of the current function task

    DeviceHandler device_handler = DeviceHandler::get_device_handler();
    Source num_copies;

    int num_static_copies, num_dynamic_copies;
    count_copies(outline_info, num_static_copies, num_dynamic_copies);

    if (IS_FORTRAN_LANGUAGE)
    {
        num_copies << "0";
    }
    else
    {
        if (num_dynamic_copies != 0)
        {
            if (num_static_copies != 0)
            {
                num_copies << num_static_copies << " + ";
            }

            num_copies << as_expression(count_dynamic_copies(outline_info));
        }
        else
        {
            num_copies << num_static_copies;
        }
    }

    Nodecl::NodeclBase num_copies_dimensions = count_copies_dimensions(outline_info);
    OutlineInfo::implementation_table_t implementation_table = outline_info.get_implementation_table();

    int num_implementations = 0;
    {
        for (OutlineInfo::implementation_table_t::iterator it = implementation_table.begin();
                it != implementation_table.end();
                ++it)
        {
            TargetInformation target_info = it->second;
            num_implementations += target_info.get_device_names().size();
        }
    }
    TL::Symbol const_wd_type = declare_const_wd_type(num_implementations, construct);

    Source alignment, props_init;

    Source ancillary_device_descriptions,
           device_descriptions,
           wd_dynamic_init;

    // In Fortran, the copies are filled in the function 'setCopies',
    // after the creation of the WorkDescriptor. For this reason,
    // we initialize the 'num_copies' and the 'num_dimensions' to zero
    Source result;
    result
        << ancillary_device_descriptions
        << "static " << const_wd_type.get_name() << " nanos_wd_const_data = {"
        << "{"
        << /* ".props = " << */ props_init << ", \n"
        << /* ".data_alignment = " << */ alignment << ", \n"
        ;

    if (num_dynamic_copies == 0)
    {
        // Note that this will be zero in Fortran
        result
            << /* ".num_copies = " << */ num_copies << ",\n"
            ;
    }
    else
    {
        result
            << /* ".num_copies = " << */ 0 << ",\n"
            ;
         wd_dynamic_init << "nanos_wd_const_data.base.num_copies = " << num_copies << ";";
    }
    result
        << /* ".num_devices = " << */ num_implementations << ",\n"
        ;

    if (Nanos::Version::interface_is_at_least("copies_api", 1000))
    {
        if (num_dynamic_copies == 0)
        {
            if (IS_FORTRAN_LANGUAGE)
            {
                result
                    << /* ".num_dimensions = " */ 0 << ",\n"
                    ;
            }
            else
            {
                result
                    << /* ".num_dimensions = " */ as_expression(num_copies_dimensions) << ",\n"
                    ;
            }
        }
        else
        {
            result
                << /* ".num_dimensions = " */ 0 << ",\n"
                ;
            wd_dynamic_init << "nanos_wd_const_data.base.num_dimensions = " << as_expression(num_copies_dimensions) << ";";
        }
    }

    if (Nanos::Version::interface_is_at_least("master", 5022)
        && (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        && (_lowering->nanos_debug_enabled()
            || _lowering->instrumentation_enabled()))
    {
        result
            << /* ".description = " */ "\"" << wd_description << "\",\n"
            ;
    }
    else
    {
        result
            << /* ".description = " */ "0,\n"
            ;
    }

    result
        << "}, "
        << /* ".devices = " << */ "{" << device_descriptions << "}"
        << "};"
        << wd_dynamic_init
        ;

    alignment << "__alignof__(" << struct_arg_type_name << ")";

    Source tiedness,
           priority;

    // The chunk is only cleared in Fortran
    int clear_chunk = 0;
    if (IS_FORTRAN_LANGUAGE)
        clear_chunk = 1;

    // We expand all the struct due to a limitation in the FE. See ticket
    // #963
    props_init
        << "{ "
        << /* ".mandatory_creation = " << */ (int)mandatory_creation << ",\n"
        << /* ".tied = " << */ tiedness << ",\n"
        << /* ".clear_chunk =" << */ clear_chunk <<",\n"
        << /* ".reserved0 =" << */ "0,\n"
        << /* ".reserved1 =" << */ "0,\n"
        << /* ".reserved2 =" << */ "0,\n"
        << /* ".reserved3 =" << */ "0,\n"
        << /* ".reserved4 =" << */ "0,\n"
        << "}"
        ;

    tiedness << (int)!is_untied;

    Symbol current_function = Nodecl::Utils::get_enclosing_function(construct);

    // For every existant implementation (including the one which defines the task),
    // we should get its device descriptor information.
    // Note that in this case we use the implementor outline name as outline name
    int fortran_device_index = 0;
    for (OutlineInfo::implementation_table_t::iterator it = implementation_table.begin();
            it != implementation_table.end();
            ++it)
    {
        TL::Symbol implementor_symbol = it->first;
        TargetInformation target_info = it->second;
        std::string implementor_outline_name = target_info.get_outline_name();

        // The symbol 'real_called_task' will be invalid if the current task is
        // a inline task. Otherwise, It will be the implementor symbol
        TL::Symbol real_called_task =
            (is_function_task) ?
            implementor_symbol : TL::Symbol::invalid();

        ObjectList<std::string> devices = target_info.get_device_names();
        for (ObjectList<std::string>::iterator it2 = devices.begin();
                it2 != devices.end();
                ++it2, ++fortran_device_index)
        {
            Source ancillary_device_description, device_description, aux_fortran_init;

            if (it != implementation_table.begin()
                    || it2 != devices.begin())
            {
                device_descriptions <<  ", ";
            }

            std::string device_name = *it2;
            DeviceProvider* device = device_handler.get_device(device_name);
            ERROR_CONDITION(device == NULL, " Device '%s' has not been loaded.", device_name.c_str());

            std::string arguments_structure = struct_arg_type_name.get_source();

            DeviceDescriptorInfo info_implementor(
                    implementor_outline_name,
                    arguments_structure,
                    current_function,
                    target_info,
                    fortran_device_index,
                    outline_info.get_data_items(),
                    real_called_task);

            device->get_device_descriptor(
                    info_implementor,
                    ancillary_device_description,
                    device_description,
                    aux_fortran_init);

            device_descriptions << device_description;
            ancillary_device_descriptions << ancillary_device_description;
            wd_dynamic_init << aux_fortran_init;
        }
    }

    if (Nanos::Version::interface_is_at_least("master", 5022)
            && IS_FORTRAN_LANGUAGE
            && (_lowering->nanos_debug_enabled()
                || _lowering->instrumentation_enabled()))
    {
        result
            << "static char nanos_wd_const_data_description[] = \"" << wd_description << "\";\n"
            << "nanos_wd_const_data.base.description = &nanos_wd_const_data_description;\n"
            ;
    }

    return result;
}

void LoweringVisitor::allocate_immediate_structure(
        OutlineInfo& outline_info,
        Source &struct_arg_type_name,
        Source &struct_size,

        // out
        Source &immediate_decl,
        Source &dynamic_size)
{
    // Account for the extra size due to overallocated items
    bool there_are_overallocated = false;
    bool immediate_is_alloca = false;

    // We overallocate with an alignment of 8
    const int overallocation_alignment = 8;

    TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();
    if (IS_C_LANGUAGE
            || IS_CXX_LANGUAGE)
    {
        for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            if (((*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_OVERALLOCATED) 
                    == OutlineDataItem::ALLOCATION_POLICY_OVERALLOCATED)
            {
                dynamic_size << "+ " << overallocation_alignment << " + sizeof(" << as_symbol((*it)->get_symbol()) << ")";
                there_are_overallocated = true;
            }
        }
    }

    if (there_are_overallocated)
    {
        immediate_is_alloca = true;
    }

    // Fill argument structure
    if (!immediate_is_alloca)
    {
        immediate_decl
            << struct_arg_type_name << " imm_args;"
            ;
    }
    else
    {
        immediate_decl
            // Create a rebindable reference
            << struct_arg_type_name << "@reb-ref@ imm_args;"
            // Note that in rebindable referencex "&x" is a "T* lvalue" (not a "T* rvalue" as usual)
            << "&imm_args = (" << struct_arg_type_name << " *) __builtin_alloca(" << struct_size << ");"
            ;
    }
}

void LoweringVisitor::emit_async_common(
        Nodecl::NodeclBase construct,
        TL::Symbol current_function,
        TL::Symbol called_task,
        Nodecl::NodeclBase statements,
        Nodecl::NodeclBase priority_expr,
        Nodecl::NodeclBase if_condition,
        Nodecl::NodeclBase final_condition,
        Nodecl::NodeclBase task_label,
        bool is_untied,

        OutlineInfo& outline_info,

        /* this is non-NULL only for function tasks */
        OutlineInfo* parameter_outline_info,
        /* this is non-NULL only for task expressions */
        Nodecl::NodeclBase* placeholder_task_expr_transformation)
{
    Source spawn_code;

    Source struct_arg_type_name,
           struct_size,
           copy_ol_decl,
           copy_ol_arg,
           copy_ol_setup,
           immediate_decl,
           copy_imm_arg,
           copy_imm_setup,
           translation_function,
           const_wd_info,
           dynamic_wd_info,
           dependences_info,
           register_reductions_opt;

    TL::Symbol xlate_function_symbol;

    Nodecl::NodeclBase fill_outline_arguments_tree;
    Source fill_outline_arguments;

    Nodecl::NodeclBase fill_immediate_arguments_tree;
    Source fill_immediate_arguments;

    bool is_function_task = called_task.is_valid();

    Nodecl::NodeclBase code = current_function.get_function_code();

    Nodecl::Context context = (code.is<Nodecl::TemplateFunctionCode>())
        ? code.as<Nodecl::TemplateFunctionCode>().get_statements().as<Nodecl::Context>()
        : code.as<Nodecl::FunctionCode>().get_statements().as<Nodecl::Context>();

    // Declare argument structure
    TL::Scope function_scope = context.retrieve_context();
    TL::Symbol structure_symbol = declare_argument_structure(outline_info, construct);
    struct_arg_type_name
         << ((structure_symbol.get_type().is_template_specialized_type()
                     &&  structure_symbol.get_type().is_dependent()) ? "typename " : "")
         << structure_symbol.get_qualified_name(function_scope);

     // Map with every implementation of the current function task
    OutlineInfo::implementation_table_t implementation_table = outline_info.get_implementation_table();


    // Disallow GPU tasks to be executed at the time they are created
    bool mandatory_creation = false;
    DeviceHandler device_handler = DeviceHandler::get_device_handler();
    {
        std::set<std::string> used_devices;
        for (OutlineInfo::implementation_table_t::iterator it = implementation_table.begin();
                it != implementation_table.end() && !mandatory_creation;
                ++it)
        {
            TargetInformation target_info = it->second;
            ObjectList<std::string> devices = target_info.get_device_names();
            for (ObjectList<std::string>::iterator it2 = devices.begin();
                    it2 != devices.end() && !mandatory_creation;
                    ++it2)
            {
                std::string device_name = *it2;
                if (used_devices.find(device_name) == used_devices.end())
                {
                    DeviceProvider* device = device_handler.get_device(device_name);
                    ERROR_CONDITION(device == NULL, " Device '%s' has not been loaded.", device_name.c_str());
                    mandatory_creation = device->allow_mandatory_creation();

                    // Finally, update the set of used devices
                    used_devices.insert(device_name);
                }
            }
        }
    }

    std::string wd_description;
    if (!task_label.is_null())
    {
        wd_description = task_label.get_text();
    }
    else if (is_function_task)
    {
        wd_description = called_task.get_name();
    }
    else
    {
        wd_description = current_function.get_name();
    }

    const_wd_info << fill_const_wd_info(
            struct_arg_type_name,
            is_untied,
            mandatory_creation,
            is_function_task,
            wd_description,
            outline_info,
            construct);

    if (priority_expr.is_null())
    {
        priority_expr = const_value_to_nodecl(const_value_get_signed_int(0));
    }
    else
    {
        _lowering->seen_task_with_priorities = true;
    }

    if (final_condition.is_null())
    {
        final_condition = const_value_to_nodecl(const_value_get_signed_int(0));
    }

    dynamic_wd_info
        << "nanos_wd_dyn_props_t nanos_wd_dyn_props;"
        << "nanos_wd_dyn_props.tie_to = 0;"
        << "nanos_wd_dyn_props.priority = " << as_expression(priority_expr) << ";"
        ;

    if (!_lowering->final_clause_transformation_disabled()
            && Nanos::Version::interface_is_at_least("master", 5024))
    {
        if (IS_FORTRAN_LANGUAGE
                && !final_condition.is_constant())
        {
            dynamic_wd_info
                << "if (" << as_expression(final_condition) << ")"
                << "{"
                <<      "nanos_wd_dyn_props.flags.is_final = 1;"
                << "}"
                << "else"
                << "{"
                <<      "nanos_wd_dyn_props.flags.is_final = 0;"
                << "}"
                ;
        }
        else
        {
            dynamic_wd_info
                << "nanos_wd_dyn_props.flags.is_final = " << as_expression(final_condition) << ";"
                ;
        }
    }

    // Only tasks created in a parallel construct are marked as implicit
    if (Nanos::Version::interface_is_at_least("master", 5029))
    {
        dynamic_wd_info
            << "nanos_wd_dyn_props.flags.is_implicit = 0;"
            ;
    }

    Source dynamic_size;

    struct_size << "sizeof(imm_args)" << dynamic_size;

    allocate_immediate_structure(
            outline_info,
            struct_arg_type_name,
            struct_size,
            // out
            immediate_decl,
            dynamic_size);

    // For every existant implementation (including the one which defines the task),
    // we should create its outline function.
    for (OutlineInfo::implementation_table_t::iterator it = implementation_table.begin();
            it != implementation_table.end();
            ++it)
    {
        TL::Symbol implementor_symbol = it->first;
        TargetInformation target_info = it->second;
        std::string implementor_outline_name = target_info.get_outline_name();

        // The symbol 'real_called_task' will be invalid if the current task is
        // a inline task. Otherwise, It will be the implementor symbol
        TL::Symbol real_called_task =
            (is_function_task) ?
            implementor_symbol : TL::Symbol::invalid();

        Nodecl::NodeclBase task_statements = statements;
        if (is_function_task
                && called_task != implementor_symbol)
        {
            // We cannot use the original statements because they contain a
            // function call to the original function task instead of a call to
            // the function specified in the 'implements' clause.
            Nodecl::Utils::SimpleSymbolMap symbol_map_copy_statements;
            symbol_map_copy_statements.add_map(called_task, implementor_symbol);

            task_statements = Nodecl::Utils::deep_copy(
                    statements,
                    implementor_symbol.get_related_scope(),
                    symbol_map_copy_statements);
        }

        ObjectList<std::string> devices = target_info.get_device_names();
        for (ObjectList<std::string>::iterator it2 = devices.begin();
                it2 != devices.end();
                ++it2)
        {
            std::string device_name = *it2;

            DeviceProvider* device = device_handler.get_device(device_name);
            ERROR_CONDITION(device == NULL, " Device '%s' has not been loaded.", device_name.c_str());

            CreateOutlineInfo info_implementor(
                    _lowering,
                    implementor_outline_name,
                    outline_info.get_data_items(),
                    target_info,
                    statements,
                    task_statements,
                    task_label,
                    structure_symbol,
                    real_called_task);

            Nodecl::NodeclBase outline_placeholder, output_statements;
            Nodecl::Utils::SimpleSymbolMap* symbol_map = NULL;
            device->create_outline(info_implementor, outline_placeholder, output_statements, symbol_map);

            Nodecl::Utils::LabelSymbolMap label_symbol_map(symbol_map, output_statements, outline_placeholder);

            Nodecl::NodeclBase outline_statements_code =
                    Nodecl::Utils::deep_copy(output_statements, outline_placeholder, label_symbol_map);

            outline_placeholder.replace(outline_statements_code);

            delete symbol_map;
        }
    }

    // In Fortran we don't need to remove the function task from the original source because:
    //  - The function task is a smp task, or
    //  - The function task is not a smp task and we only have it's declaration
    if (!IS_FORTRAN_LANGUAGE
            && is_function_task)
    {
        remove_fun_tasks_from_source_as_possible(implementation_table);
    }

    Source err_name;
    err_name << "nanos_err";

    Source placeholder_task_expression_opt, update_alloca_decls_opt;
    if (placeholder_task_expr_transformation != NULL)
    {
        placeholder_task_expression_opt
            << statement_placeholder(*placeholder_task_expr_transformation);


        TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();
        for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            if ((*it)->get_sharing() == OutlineDataItem::SHARING_ALLOCA)
            {
                TL::Symbol sym = (*it)->get_symbol();
                update_alloca_decls_opt
                    << sym.get_name() << " = &(ol_args->" << sym.get_name() << ");"
                    ;
            }
            else if((*it)->get_sharing() == OutlineDataItem::SHARING_SHARED_ALLOCA)
            {
                TL::Symbol sym = (*it)->get_symbol();
                update_alloca_decls_opt
                    << sym.get_name() << " = &(ol_args->" << sym.get_name() << "_storage);"
                    ;
            }
        }
    }

    Source if_condition_begin_opt, if_condition_end_opt;
    if (!if_condition.is_null())
    {
        if_condition_begin_opt << "if (" << as_expression(if_condition) << ") {";
        if_condition_end_opt << "}";
    }

    Source num_dependences, num_dependences_if_dynamic;
    // Spawn code
    spawn_code
        << "{"
        // Devices related to this task
        <<     const_wd_info
        <<     dynamic_wd_info
        <<     struct_arg_type_name << "* ol_args;"
        <<     "ol_args = (" << struct_arg_type_name << "*) 0;"
        <<     immediate_decl
        <<     "nanos_wd_t nanos_wd_ = (nanos_wd_t)0;"
        <<     copy_ol_decl
        <<     "nanos_err_t " << err_name <<";"
        <<     register_reductions_opt
        <<     if_condition_begin_opt
        <<     err_name << " = nanos_create_wd_compact(&nanos_wd_, &(nanos_wd_const_data.base), &nanos_wd_dyn_props, "
        <<                 struct_size << ", (void**)&ol_args, nanos_current_wd(),"
        <<                 copy_ol_arg << ");"
        <<     "if (" << err_name << " != NANOS_OK) nanos_handle_error (" << err_name << ");"
        <<     if_condition_end_opt
        <<     update_alloca_decls_opt
        <<     placeholder_task_expression_opt
        <<     num_dependences_if_dynamic
        <<     dependences_info
        <<     "if (nanos_wd_ != (nanos_wd_t)0)"
        <<     "{"
                  // This is a placeholder because arguments are filled using the base language (possibly Fortran)
        <<        statement_placeholder(fill_outline_arguments_tree)
        <<        copy_ol_setup
        <<        err_name << " = nanos_submit(nanos_wd_, " << num_dependences << ", &dependences[0], (nanos_team_t)0);"
        <<        "if (" << err_name << " != NANOS_OK) nanos_handle_error (" << err_name << ");"
        <<     "}"
        <<     "else"
        <<     "{"
                    // This is a placeholder because arguments are filled using the base language (possibly Fortran)
        <<          statement_placeholder(fill_immediate_arguments_tree)
        <<          copy_imm_setup
        <<          err_name << " = nanos_create_wd_and_run_compact(&(nanos_wd_const_data.base), &nanos_wd_dyn_props, "
        <<                  struct_size << ", "
        <<                  "&imm_args,"
        <<                  num_dependences << ", &dependences[0], "
        <<                  copy_imm_arg << ", "
        <<                  translation_function << ");"
        <<          "if (" << err_name << " != NANOS_OK) nanos_handle_error (" << err_name << ");"
        <<     "}"
        << "}"
        ;

    // Fill arguments
    fill_arguments(construct, outline_info, fill_outline_arguments, fill_immediate_arguments);

    // Fill dependences for outline
    int num_static_dependences, num_dynamic_dependences;
    count_dependences(outline_info, num_static_dependences, num_dynamic_dependences);
    if (num_dynamic_dependences == 0)
    {
        num_dependences << num_static_dependences;
    }
    else
    {
        Source num_deps_init;
        num_dependences_if_dynamic
            << "int num_dyn_dependences = " << num_deps_init << ";"
            ;

        if (num_static_dependences == 0)
        {
            num_deps_init
                << as_expression(
                        count_dynamic_dependences(outline_info));
        }
        else
        {
            num_deps_init
                << num_static_dependences << "+ ("
                << as_expression(
                        count_dynamic_dependences(outline_info))
                << ")";
        }

        num_dependences << "num_dyn_dependences";
    }

    int num_static_copies, num_dynamic_copies;
    count_copies(outline_info, num_static_copies, num_dynamic_copies);

    Source num_copies;
    fill_copies(construct,
            outline_info,
            parameter_outline_info,
            structure_symbol,

            num_copies,
            copy_ol_decl,
            copy_ol_arg,
            copy_ol_setup,
            copy_imm_arg,
            copy_imm_setup,
            xlate_function_symbol);

    if (num_static_copies == 0
            && num_dynamic_copies == 0)
    {
        translation_function << "(nanos_translate_args_t)0";
    }
    else
    {
        Source reference_to_xlate;
        if (xlate_function_symbol.get_type().is_template_specialized_type())
        {
            // Extra cast needed for g++ 4.6 or lower (otherwise the
            // compilation may fail or the template function not be emitted)
            reference_to_xlate << "(" << as_type(xlate_function_symbol.get_type().get_pointer_to()) << ")";
        }
        reference_to_xlate << xlate_function_symbol.get_qualified_name();

        translation_function << "(nanos_translate_args_t)" << reference_to_xlate;

        copy_ol_setup
            << err_name << " = nanos_set_translate_function(nanos_wd_, (nanos_translate_args_t)"
            << reference_to_xlate << ");"
            << "if (" << err_name << " != NANOS_OK) nanos_handle_error(" << err_name << ");"
            ;
    }

    fill_dependences(construct,
            outline_info,
            num_static_dependences,
            num_dynamic_dependences,
            num_dependences,
            dependences_info);

    register_reductions(construct, outline_info, register_reductions_opt);

    FORTRAN_LANGUAGE()
    {
        // Parse in C
        Source::source_language = SourceLanguage::C;
    }

    Nodecl::NodeclBase spawn_code_tree = spawn_code.parse_statement(construct);

    FORTRAN_LANGUAGE()
    {
        Source::source_language = SourceLanguage::Current;
    }

    if (!fill_outline_arguments.empty())
    {
        Nodecl::NodeclBase new_tree = fill_outline_arguments.parse_statement(fill_outline_arguments_tree);
        fill_outline_arguments_tree.replace(new_tree);
    }

    if (!fill_immediate_arguments.empty())
    {
        Nodecl::NodeclBase new_tree = fill_immediate_arguments.parse_statement(fill_immediate_arguments_tree);
        fill_immediate_arguments_tree.replace(new_tree);
    }

    construct.replace(spawn_code_tree);
}

void LoweringVisitor::visit(const Nodecl::OpenMP::Task& construct)
{
    visit_task(construct, /* inside_task_expression */ false, /* placeholder_task_expr_transformation */ NULL);
}

// This function is called by the visitors of a OpenMP::Task and OpenMP::TaskExpression
void LoweringVisitor::visit_task(
        const Nodecl::OpenMP::Task& construct,
        bool inside_task_expression,
        Nodecl::NodeclBase* placeholder_task_expr_transformation)
{
    Nodecl::NodeclBase environment = construct.get_environment();
    Nodecl::NodeclBase statements = construct.get_statements();

    walk(statements);

    TaskEnvironmentVisitor task_environment;
    task_environment.walk(environment);

    Scope  enclosing_scope = construct.retrieve_context();
    Symbol function_symbol = Nodecl::Utils::get_enclosing_function(construct);

    OutlineInfo outline_info(*_lowering, environment, function_symbol);

    bool generate_final_stmts = Nanos::Version::interface_is_at_least("master", 5024)
            && !_lowering->final_clause_transformation_disabled()
            && outline_info.only_has_smp_or_mpi_implementations()
            && !inside_task_expression;

    // In the case of task reductions, we cannot use the final stmts generated
    // by the FinalStmtsGenerator because we need to introduce some extra function
    // calls to obtain the thread private storage
    //
    // The task_reduction_final_statements will be filled if the current task has
    // a reduction clause
    bool has_task_reduction = false;
    Nodecl::NodeclBase task_reduction_final_statements;
    has_task_reduction = handle_reductions_on_task(
            construct,
            outline_info,
            statements,
            generate_final_stmts,
            task_reduction_final_statements);

    // Handle the special object 'this'
    if (IS_CXX_LANGUAGE
            && !function_symbol.is_static()
            && function_symbol.is_member())
    {
        TL::Symbol this_symbol = enclosing_scope.get_symbol_this();
        ERROR_CONDITION(!this_symbol.is_valid(), "Invalid symbol", 0);

        Nodecl::NodeclBase sym_ref = Nodecl::Symbol::make(this_symbol);
        sym_ref.set_type(this_symbol.get_type());

        // The object 'this' may already have an associated OutlineDataItem
        OutlineDataItem& argument_outline_data_item =
            outline_info.get_entity_for_symbol(this_symbol);

        argument_outline_data_item.set_is_cxx_this(true);

        // This is a special kind of shared
        if (argument_outline_data_item.get_sharing() == OutlineDataItem::SHARING_UNDEFINED)
            argument_outline_data_item.set_sharing(OutlineDataItem::SHARING_CAPTURE_ADDRESS);
        argument_outline_data_item.set_base_address_expression(sym_ref);
    }

    Nodecl::NodeclBase new_construct;
    if (generate_final_stmts)
    {
        // We create a new Node OpenMP::Task with the same childs as the
        // original construct. Another solution is shallow copy all the
        // construct (less efficient)
        new_construct = Nodecl::OpenMP::Task::make(environment, statements);
        TL::Source code;

        Nodecl::NodeclBase copied_statements_placeholder;
        code
            << "{"
            <<      as_type(TL::Type::get_bool_type()) << "mcc_is_in_final;"
            <<      "nanos_err_t mcc_err_in_final = nanos_in_final(&mcc_is_in_final);"
            <<      "if (mcc_err_in_final != NANOS_OK) nanos_handle_error(mcc_err_in_final);"
            <<      "if (mcc_is_in_final)"
            <<      "{"
            <<          statement_placeholder(copied_statements_placeholder)
            <<      "}"
            <<      "else"
            <<      "{"
            <<          as_statement(new_construct)
            <<      "}"
            << "}"
            ;

        if (IS_FORTRAN_LANGUAGE)
            Source::source_language = SourceLanguage::C;

        Nodecl::NodeclBase if_else_tree = code.parse_statement(construct);

        if (IS_FORTRAN_LANGUAGE)
            Source::source_language = SourceLanguage::Current;

        construct.replace(if_else_tree);

        // We obtain the list node which contains the placeholder used to store
        // the final stmts. This must be done before the replace because at
        // this point the parent of the copied_statements_placeholder is the
        // first (and the unique) list node
        Nodecl::NodeclBase final_stmt_list = copied_statements_placeholder.get_parent();

        std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>::iterator it = _final_stmts_map.find(construct);

        ERROR_CONDITION(it == _final_stmts_map.end(), "Unreachable code", 0);

        // We need to replace the placeholder before transforming the OpenMP/OmpSs pragmas
        if (!task_reduction_final_statements.is_null())
            copied_statements_placeholder.replace(task_reduction_final_statements);
        else
            copied_statements_placeholder.replace(it->second);

        ERROR_CONDITION(!copied_statements_placeholder.is_in_list(), "Unreachable code\n", 0);

        // Walk over the tree transforming OpenMP/OmpSs non-task pragmas
        walk(final_stmt_list);
    }
    else
    {
        new_construct = construct;
    }

    // Our implementation of reduction tasks forces them to be tied
    bool is_untied = task_environment.is_untied && !has_task_reduction;

    Symbol called_task_dummy = Symbol::invalid();
    emit_async_common(
            new_construct,
            function_symbol,
            called_task_dummy,
            statements,
            task_environment.priority,
            task_environment.if_condition,
            task_environment.final_condition,
            task_environment.task_label,
            is_untied,

            outline_info,
            /* parameter_outline_info */ NULL,
            placeholder_task_expr_transformation);
}

Source LoweringVisitor::compute_num_refs_in_multiref(DataReference& data_ref)
{
    ERROR_CONDITION(!data_ref.is_multireference(), "Invalid data reference", 0);
    Source src;

    ObjectList<DataReference::MultiRefIterator> m = data_ref.multireferences();

    for (ObjectList<DataReference::MultiRefIterator>::iterator current_multidep = m.begin();
            current_multidep != m.end();
            current_multidep++)
    {
        Nodecl::Range range = current_multidep->second.as<Nodecl::Range>();
        ERROR_CONDITION(!range.is<Nodecl::Range>(), "Invalid node %s", ast_print_node_type(range.get_kind()));

        Nodecl::NodeclBase lower = range.get_lower().shallow_copy();
        Nodecl::NodeclBase upper = range.get_upper().shallow_copy();
        Nodecl::NodeclBase stride = range.get_stride().shallow_copy();

        if (stride.is_constant()
                && const_value_is_one(stride.get_constant()))
        {
            // Common case
            src << "(" <<  as_expression(upper.shallow_copy()) << " - " << as_expression(lower.shallow_copy()) << " + 1)";
        }
        else
        {
            src << "(((" <<  as_expression(upper.shallow_copy()) << " - " << as_expression(lower.shallow_copy()) << " + 1)"
                " + (" << as_expression(stride.shallow_copy()) << " - 1)) / " << as_expression(stride.shallow_copy()) << ")";
        }
    }

    return src;
}

void LoweringVisitor::initialize_multicopies_index(
        Nodecl::NodeclBase ctr,
        OutlineInfo& outline_info,
        // out
        Source& fill_outline_arguments,
        Source& fill_immediate_arguments
        )
{
    if (!outline_info.get_multicopies_index_symbol().is_valid())
        return;

    if (IS_CXX_LANGUAGE)
    {
        Nodecl::NodeclBase def = Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
                outline_info.get_multicopies_index_symbol(),
                ctr.get_locus());
        ctr.prepend_sibling(def);
    }

    Source src;

    int multicopy_index = 0;

    int num_static_copies = 0;

    TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        TL::ObjectList<OutlineDataItem::CopyItem> copies = (*it)->get_copies();

        if (copies.empty())
            continue;
        for (TL::ObjectList<OutlineDataItem::CopyItem>::iterator copy_it = copies.begin();
                copy_it != copies.end();
                copy_it++)
        {
            TL::DataReference copy_expr(copy_it->expression);
            if (!copy_expr.is_multireference())
                num_static_copies++;
        }
    }

    Source previous_num_copies;
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        TL::ObjectList<OutlineDataItem::CopyItem> copies = (*it)->get_copies();

        if (copies.empty())
            continue;

        Source current_num_copies;

        bool has_multicopies = false;
        int num_dynamic_copies_of_item = 0;
        for (TL::ObjectList<OutlineDataItem::CopyItem>::iterator copy_it = copies.begin();
                copy_it != copies.end();
                copy_it++, num_dynamic_copies_of_item++)
        {
            TL::DataReference copy_expr(copy_it->expression);
            if (copy_expr.is_multireference())
            {
                has_multicopies = true;

                Source num_copies_in_multiref;
                num_copies_in_multiref
                    << compute_num_refs_in_multiref(copy_expr);
                if (num_dynamic_copies_of_item > 0)
                {
                    current_num_copies << " + ";
                }
                current_num_copies << num_copies_in_multiref;
            }
        }

        if (has_multicopies)
        {
            if (multicopy_index == 0)
            {
                // Skip the first one
            }
            else
            {
                src << as_symbol(outline_info.get_multicopies_index_symbol()) << "[" << (multicopy_index-1) << "] = "
                    ;

                if (multicopy_index == 1)
                    src << num_static_copies;
                else if (multicopy_index > 1)
                    src << as_symbol(outline_info.get_multicopies_index_symbol()) << "[" << (multicopy_index-2) << "]";

                src << "+" << previous_num_copies << ";"
                    ;
            }
            multicopy_index++;
        }

        previous_num_copies = current_num_copies;
    }

    fill_outline_arguments << src;
    fill_immediate_arguments << src;
}

void LoweringVisitor::fill_arguments(
        Nodecl::NodeclBase ctr,
        OutlineInfo& outline_info,
        // out
        Source& fill_outline_arguments,
        Source& fill_immediate_arguments
        )
{
    // Multicopies may require extra information
    initialize_multicopies_index(ctr, outline_info, fill_outline_arguments, fill_immediate_arguments);

    // We overallocate with an alignment of 8
    const int overallocation_alignment = 8;
    const int overallocation_mask = overallocation_alignment - 1;
    Source intptr_type;
    intptr_type << Type(::get_size_t_type()).get_declaration(ctr.retrieve_context(), "")
        ;

    TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();
    // Now fill the arguments information (this part is language dependent)
    if (IS_C_LANGUAGE
            || IS_CXX_LANGUAGE)
    {
        Source overallocation_base_offset; 
        // We round up to the alignment
        overallocation_base_offset << "(void*)(((" 
            << intptr_type << ")(char*)(ol_args + 1) + " 
            << overallocation_mask << ") & (~" << overallocation_mask << "))";

        Source imm_overallocation_base_offset;
        imm_overallocation_base_offset << "(void*)(((" 
            << intptr_type << ")(char*)(&imm_args + 1) + " 
            << overallocation_mask << ") & (~" << overallocation_mask << "))";

        for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            if (!(*it)->get_symbol().is_valid())
                continue;

            switch ((*it)->get_sharing())
            {
                case OutlineDataItem::SHARING_CAPTURE:
                    {
                        if (((*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_OVERALLOCATED)
                                == OutlineDataItem::ALLOCATION_POLICY_OVERALLOCATED)
                        {
                            TL::Type sym_type = (*it)->get_symbol().get_type();
                            if (sym_type.is_any_reference())
                                sym_type = sym_type.references_to();

                            ERROR_CONDITION(!sym_type.is_array(), "Only arrays can be overallocated", 0);

                            // Overallocated
                            fill_outline_arguments << 
                                "ol_args->" << (*it)->get_field_name() << " = " << Source(overallocation_base_offset) << ";"
                                ;

                            // Overwrite source
                            overallocation_base_offset = Source() << "(void*)((" 
                                << intptr_type << ")((char*)(ol_args->" << 
                                (*it)->get_field_name() << ") + sizeof(" << as_symbol((*it)->get_symbol()) << ") + " 
                                << overallocation_mask << ") & (~" << overallocation_mask << "))"
                                ;
                            fill_immediate_arguments << 
                                "imm_args." << (*it)->get_field_name() << " = " << Source(imm_overallocation_base_offset) << ";";
                            ;
                            // Overwrite source
                            imm_overallocation_base_offset = Source() << "(void*)((" 
                                << intptr_type << ")((char*)(imm_args." << 
                                (*it)->get_field_name() << ") + sizeof(" << as_symbol((*it)->get_symbol()) << ") + "
                                << overallocation_mask << ") & (~" << overallocation_mask << "))"
                                ;

                            if (IS_CXX_LANGUAGE
                                    && (sym_type.is_dependent()
                                        || !sym_type.is_pod()))
                            {
                                TL::Type base_type = sym_type;
                                while (base_type.is_array())
                                    base_type = base_type.array_element();

                                base_type = base_type.get_unqualified_type().get_pointer_to();

                                // Non pod vla-arrays
                                Source array_copy;
                                array_copy
                                    << "while (__orig < ((" << as_type(base_type) << ")(&(" << as_symbol((*it)->get_symbol()) << ") + 1)))"
                                    << "{"
                                    << " new (__dest) " << as_type(base_type.points_to()) << ";"
                                    << " *__dest = *__orig; __dest++; __orig++; "
                                    << "}"
                                    ;

                                fill_outline_arguments
                                    << "{"
                                    << as_type(base_type) << " __dest = (" << as_type(base_type) << ") ol_args->" << (*it)->get_field_name() << ";"
                                    << as_type(base_type) << " __orig = (" << as_type(base_type) << ") " <<  as_symbol((*it)->get_symbol()) << ";"
                                    << array_copy
                                    << "}"
                                    ;
                                fill_immediate_arguments
                                    << "{"
                                    << as_type(base_type) << " __dest = (" << as_type(base_type) << ") imm_args." << (*it)->get_field_name() << ";"
                                    << as_type(base_type) << " __orig = (" << as_type(base_type) << ") " <<  as_symbol((*it)->get_symbol()) << ";"
                                    << array_copy
                                    << "}"
                                    ;
                            }
                            else
                            {
                                fill_outline_arguments
                                    << "__builtin_memcpy(ol_args->" << (*it)->get_field_name() 
                                    << ", &" << as_symbol((*it)->get_symbol()) 
                                    << ", sizeof(" << as_symbol((*it)->get_symbol()) << "));"
                                    ;
                                fill_immediate_arguments
                                    << "__builtin_memcpy(imm_args." << (*it)->get_field_name() 
                                    << ", &" << as_symbol((*it)->get_symbol()) 
                                    << ", sizeof(" << as_symbol((*it)->get_symbol()) << "));"
                                    ;
                            }
                        }
                        else
                        {
                            // Not overallocated
                            TL::Type sym_type = (*it)->get_symbol().get_type();
                            if (sym_type.is_any_reference())
                                sym_type = sym_type.references_to();

                            if (sym_type.is_array())
                            {
                                if (IS_CXX_LANGUAGE
                                        && (sym_type.is_dependent()
                                            || !sym_type.is_pod()))
                                {
                                    TL::Type base_type = sym_type;
                                    while (base_type.is_array())
                                        base_type = base_type.array_element();

                                    base_type = base_type.get_unqualified_type().get_pointer_to();

                                    // Non pod fixed-length arrays
                                    Source array_copy;
                                    array_copy
                                        << "while (__orig < ((" << as_type(base_type) << ")(&(" << as_symbol((*it)->get_symbol()) << ") + 1)))"
                                        << "{"
                                        << " new (__dest) " << as_type(base_type.points_to()) << ";"
                                        << " *__dest = *__orig; __dest++; __orig++; "
                                        << "}"
                                        ;

                                    fill_outline_arguments
                                        << "{"
                                        << as_type(base_type) << " __dest = (" << as_type(base_type) << ") ol_args->" << (*it)->get_field_name() << ";"
                                        << as_type(base_type) << " __orig = (" << as_type(base_type) << ") " <<  as_symbol((*it)->get_symbol()) << ";"
                                        << array_copy
                                        << "}"
                                        ;
                                    fill_immediate_arguments
                                        << "{"
                                        << as_type(base_type) << " __dest = (" << as_type(base_type) << ") imm_args." << (*it)->get_field_name() << ";"
                                        << as_type(base_type) << " __orig = (" << as_type(base_type) << ") " <<  as_symbol((*it)->get_symbol()) << ";"
                                        << array_copy
                                        << "}"
                                        ;
                                }
                                else
                                {
                                    fill_outline_arguments
                                        << "__builtin_memcpy(&ol_args->" << (*it)->get_field_name() 
                                        << ", &" << as_symbol((*it)->get_symbol())
                                        << ", sizeof(" << as_symbol((*it)->get_symbol()) << "));"
                                        ;
                                    fill_immediate_arguments
                                        << "__builtin_memcpy(&imm_args." << (*it)->get_field_name() 
                                        << ", &" << as_symbol((*it)->get_symbol())
                                        << ", sizeof(" << as_symbol((*it)->get_symbol()) << "));"
                                        ;
                                }
                            }
                            else
                            {
                                sym_type = sym_type.no_ref().get_unqualified_type();
                                if ((*it)->get_captured_value().is_null())
                                {
                                    if (IS_CXX_LANGUAGE
                                            && (sym_type.is_dependent()
                                                || (sym_type.is_class()
                                                    && !sym_type.is_pod())))
                                    {
                                        fill_outline_arguments <<
                                            "new (& ol_args->" << (*it)->get_field_name() << " )"
                                            << as_type(sym_type)
                                            << "( " << as_symbol((*it)->get_symbol()) << ");"
                                            ;
                                        fill_immediate_arguments <<
                                            "new (& imm_args." << (*it)->get_field_name() << " )"
                                            << as_type(sym_type)
                                            << "( " << as_symbol((*it)->get_symbol()) << ");"
                                            ;
                                    }
                                    else
                                    {
                                        // Plain assignment is enough
                                        fill_outline_arguments <<
                                            "ol_args->" << (*it)->get_field_name() << " = " << as_symbol((*it)->get_symbol()) << ";"
                                            ;
                                        fill_immediate_arguments <<
                                            "imm_args." << (*it)->get_field_name() << " = " << as_symbol((*it)->get_symbol()) << ";"
                                            ;
                                    }
                                }
                                else
                                {
                                    Nodecl::NodeclBase captured = (*it)->get_captured_value();
                                    Nodecl::NodeclBase condition = (*it)->get_conditional_capture_value();
                                    if (!condition.is_null())
                                    {
                                        fill_outline_arguments << "if (" << as_expression(condition.shallow_copy()) << ") {";
                                        fill_immediate_arguments << "if (" << as_expression(condition.shallow_copy()) << ") {";
                                    }

                                    if (IS_CXX_LANGUAGE
                                            && (sym_type.is_dependent()
                                                || (sym_type.is_class()
                                                    && !sym_type.is_pod())))
                                    {
                                        fill_outline_arguments <<
                                            "new (&ol_args->" << (*it)->get_field_name() << ")"
                                            << as_type(sym_type)
                                            << "(" << as_expression(captured.shallow_copy()) << ");"
                                            ;
                                        fill_immediate_arguments <<
                                            "new (&imm_args." << (*it)->get_field_name() << ")"
                                            << as_type(sym_type)
                                            << "(" << as_expression(captured.shallow_copy()) << ");"
                                            ;
                                    }
                                    else
                                    {
                                        fill_outline_arguments <<
                                            "ol_args->" << (*it)->get_field_name()
                                            << " = " << as_expression(captured.shallow_copy()) << ";"
                                            ;
                                        fill_immediate_arguments <<
                                            "imm_args." << (*it)->get_field_name()
                                            << " = " << as_expression(captured.shallow_copy()) << ";"
                                            ;
                                    }

                                    if (!condition.is_null())
                                    {
                                        fill_outline_arguments << "}";
                                        fill_immediate_arguments << "}";
                                    }
                                }
                            }
                        }
                        break;
                    }
                case OutlineDataItem::SHARING_SHARED:
                case OutlineDataItem::SHARING_REDUCTION:
                    {
                        // 'this' is special in C++
                        if (IS_CXX_LANGUAGE
                                && (*it)->get_symbol().get_name() == "this")
                        {
                            fill_outline_arguments <<
                                "ol_args->" << (*it)->get_field_name() << " = " << as_symbol((*it)->get_symbol()) << ";"
                                ;
                            fill_immediate_arguments <<
                                "imm_args." << (*it)->get_field_name() << " = " << as_symbol((*it)->get_symbol()) << ";"
                                ;
                        }
                        else
                        {
                            fill_outline_arguments <<
                                "ol_args->" << (*it)->get_field_name() << " = &" << as_symbol((*it)->get_symbol()) << ";"
                                ;
                            fill_immediate_arguments <<
                                "imm_args." << (*it)->get_field_name() << " = &" << as_symbol((*it)->get_symbol()) << ";"
                                ;
                        }
                        break;
                    }
                case OutlineDataItem::SHARING_SHARED_ALLOCA:
                    {
                        fill_outline_arguments
                            << "ol_args->" << (*it)->get_field_name() << "= &(ol_args->" << (*it)->get_field_name() << "_storage);"
                            ;

                        fill_immediate_arguments
                            << "imm_args." << (*it)->get_field_name() << " = &(imm_args." << (*it)->get_field_name() << "_storage);"
                            ;
                        break;
                    }
                case  OutlineDataItem::SHARING_CAPTURE_ADDRESS:
                    {
                        Nodecl::NodeclBase base_expr;

                        if (IS_CXX_LANGUAGE
                                && (*it)->get_symbol().get_type().is_lvalue_reference())
                        {
                            TL::Type t = base_expr.get_type();
                            if (t.is_any_reference())
                                t = t.references_to();

                            t = t.get_pointer_to();

                            base_expr = Nodecl::Reference::make(
                                    (*it)->get_base_address_expression().shallow_copy(),
                                    t,
                                    base_expr.get_locus());
                        }
                        else
                        {
                            base_expr = (*it)->get_base_address_expression().shallow_copy();
                        }

                        fill_outline_arguments
                            << "ol_args->" << (*it)->get_field_name() << " = " << as_expression( base_expr ) << ";"
                            ;
                        fill_immediate_arguments
                            << "imm_args." << (*it)->get_field_name() << " = " << as_expression( base_expr.shallow_copy() ) << ";"
                            ;
                        break;
                    }
                case OutlineDataItem::SHARING_PRIVATE:
                case OutlineDataItem::SHARING_ALLOCA:
                    {
                        // Do nothing
                        break;
                    }
                default:
                    {
                        internal_error("Unexpected sharing kind", 0);
                    }
            }
        }
    }
    else if (IS_FORTRAN_LANGUAGE)
    {
        for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            TL::Symbol sym = (*it)->get_symbol();
            if (!sym.is_valid())
                continue;

            switch ((*it)->get_sharing())
            {
                case OutlineDataItem::SHARING_CAPTURE:
                    {
                        TL::Type t = sym.get_type();
                        if (t.is_any_reference())
                            t = t.references_to();

                        if (!(*it)->get_prepare_capture_code().is_null())
                        {
                            Nodecl::NodeclBase capture_code = (*it)->get_prepare_capture_code();
                            fill_outline_arguments << as_statement(capture_code.shallow_copy());
                            fill_immediate_arguments << as_statement(capture_code.shallow_copy());
                        }

                        if ((*it)->get_captured_value().is_null())
                        {
                            if (t.is_pointer() || t.is_function())
                            {
                                fill_outline_arguments <<
                                    "ol_args % " << (*it)->get_field_name() << " => " << (*it)->get_symbol().get_name() << "\n"
                                    ;
                                fill_immediate_arguments <<
                                    "imm_args % " << (*it)->get_field_name() << " => " << (*it)->get_symbol().get_name() << "\n"
                                    ;
                            }
                            else
                            {
                                fill_outline_arguments <<
                                    "ol_args % " << (*it)->get_field_name() << " = " << (*it)->get_symbol().get_name() << "\n"
                                    ;
                                fill_immediate_arguments <<
                                    "imm_args % " << (*it)->get_field_name() << " = " << (*it)->get_symbol().get_name() << "\n"
                                    ;
                            }
                        }
                        else
                        {
                            Nodecl::NodeclBase captured = (*it)->get_captured_value();
                            Nodecl::NodeclBase condition = (*it)->get_conditional_capture_value();
                            if (!condition.is_null())
                            {
                                fill_outline_arguments << "IF (" << as_expression(condition.shallow_copy()) << ") THEN\n";
                                fill_immediate_arguments << "IF (" << as_expression(condition.shallow_copy()) << ") THEN\n";
                            }
                            if (t.is_pointer() || t.is_function())
                            {
                                fill_outline_arguments <<
                                    "ol_args % " << (*it)->get_field_name() << " => " << as_expression(captured.shallow_copy()) << "\n"
                                    ;
                                fill_immediate_arguments <<
                                    "imm_args % " << (*it)->get_field_name() << " => " << as_expression(captured.shallow_copy()) << "\n"
                                    ;
                            }
                            else
                            {
                                fill_outline_arguments <<
                                    "ol_args % " << (*it)->get_field_name() << " = " <<  as_expression(captured.shallow_copy()) << "\n"
                                    ;
                                fill_immediate_arguments <<
                                    "imm_args % " << (*it)->get_field_name() << " = " <<  as_expression(captured.shallow_copy()) << "\n"
                                    ;
                            }
                            if (!condition.is_null())
                            {
                                fill_outline_arguments << "END IF\n";
                                fill_immediate_arguments << "END IF\n";
                            }
                        }
                        break;
                    }
                case OutlineDataItem::SHARING_SHARED:
                case OutlineDataItem::SHARING_REDUCTION:
                    {
                        TL::Type t = sym.get_type();
                        if (t.is_any_reference())
                            t = t.references_to();

                        if (sym.is_optional())
                        {
                            fill_outline_arguments << "IF (PRESENT(" << sym.get_name() << ")) THEN\n";
                            fill_immediate_arguments << "IF (PRESENT(" << sym.get_name() << ")) THEN\n";
                        }

                        if (t.is_pointer()
                                || sym.is_allocatable())
                        {
                            TL::Symbol ptr_of_sym = get_function_ptr_of((*it)->get_symbol(),
                                    ctr.retrieve_context());

                            fill_outline_arguments << 
                                "ol_args %" << (*it)->get_field_name() << " => " 
                                << ptr_of_sym.get_name() << "( " << (*it)->get_symbol().get_name() << ") \n"
                                ;
                            fill_immediate_arguments << 
                                "imm_args % " << (*it)->get_field_name() << " => " 
                                << ptr_of_sym.get_name() << "( " << (*it)->get_symbol().get_name() << ") \n"
                                ;
                        }
                        else if (t.is_array() && t.array_requires_descriptor())
                        {
                            // This must be an assumed shape, so it will have a descriptor
                            OutlineDataItem* copy_of_array_descriptor = (*it)->get_copy_of_array_descriptor();
                            ERROR_CONDITION(copy_of_array_descriptor == NULL, "Missing array descriptor copy entity", 0);

                            fill_outline_arguments <<
                                "ol_args %" << (*it)->get_field_name() << " => "
                                << "MERCURIUM_LOC( ol_args %" << copy_of_array_descriptor->get_field_name() << ")\n"
                                ;
                            fill_immediate_arguments <<
                                "imm_args % " << (*it)->get_field_name() << " => "
                                << "MERCURIUM_LOC( imm_args %" << copy_of_array_descriptor->get_field_name() << ")\n"
                                ;
                        }
                        else
                        {
                            Source lbound_specifier;
                            if (t.is_fortran_array())
                            {
                                lbound_specifier << "(";

                                int i, N = t.fortran_rank();
                                for (i = 1; i <= N; i++)
                                {
                                    if (i > 1)
                                    {
                                        lbound_specifier << ", ";
                                    }
                                    lbound_specifier << "LBOUND(" << (*it)->get_symbol().get_name() << ", DIM = " << i << ")";
                                }

                                lbound_specifier << ")";
                            }

                            fill_outline_arguments <<
                                "ol_args %" << (*it)->get_field_name() << " => MERCURIUM_LOC("
                                << (*it)->get_symbol().get_name() << lbound_specifier << ") \n"
                                ;
                            fill_immediate_arguments <<
                                "imm_args % " << (*it)->get_field_name() << " => MERCURIUM_LOC("
                                << (*it)->get_symbol().get_name() << lbound_specifier << ") \n"
                                ;
                        }

                        if (sym.is_optional())
                        {
                            fill_outline_arguments
                                << "ELSE\n"
                                <<    "ol_args %" << (*it)->get_field_name() << " => MERCURIUM_NULL()\n"
                                << "END IF\n";

                            fill_immediate_arguments
                                << "ELSE\n"
                                <<    "imm_args %" << (*it)->get_field_name() << " => MERCURIUM_NULL()\n"
                                << "END IF\n";
                        }

                        break;
                    }
                case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
                    {
                        // Best effort, this may fail sometimes
                        DataReference data_ref((*it)->get_base_address_expression());
                        if (!data_ref.is_valid())
                        {
                            warn_printf_at(
                                (*it)->get_base_address_expression().get_locus(),
                                "an argument is not a valid data-reference, compilation is likely to fail\n");
                        }

                        // This is a pointer reference
                        if ((*it)->get_base_address_expression().is<Nodecl::Dereference>())
                        {
                            TL::Symbol ptr_of_sym = get_function_ptr_of(
                                    (*it)->get_base_address_expression().get_type(),
                                    ctr.retrieve_context());

                            fill_outline_arguments
                                << "ol_args %" << (*it)->get_field_name() << " => "
                                << ptr_of_sym.get_name() << "(" << as_expression( (*it)->get_base_address_expression().shallow_copy()) << ")\n"
                                ;
                            fill_immediate_arguments
                                << "imm_args % " << (*it)->get_field_name() << " => "
                                << ptr_of_sym.get_name() << "(" << as_expression( (*it)->get_base_address_expression().shallow_copy()) << ")\n"
                                ;
                        }
                        else
                        {
                            fill_outline_arguments
                                << "ol_args %" << (*it)->get_field_name() << " => "
                                << "MERCURIUM_LOC(" << as_expression( (*it)->get_base_address_expression().shallow_copy()) << ")\n"
                                ;
                            fill_immediate_arguments
                                << "imm_args % " << (*it)->get_field_name() << " => "
                                << "MERCURIUM_LOC(" << as_expression( (*it)->get_base_address_expression().shallow_copy()) << ")\n"
                                ;
                        }

                        break;
                    }
                case OutlineDataItem::SHARING_PRIVATE:
                case OutlineDataItem::SHARING_ALLOCA:
                    {
                        // Do nothing
                        break;
                    }
                default:
                    {
                        internal_error("Unexpected sharing kind", 0);
                    }
            }
        }
    }
}

void LoweringVisitor::count_dependences(OutlineInfo& outline_info,
        int &num_static_dependences,
        int &num_dynamic_dependences)
{
    count_items<OutlineDataItem::DependencyItem>(outline_info,
            &OutlineDataItem::get_dependences,
            num_static_dependences,
            num_dynamic_dependences);
}

Nodecl::NodeclBase LoweringVisitor::count_multidependences_extent(
        const TL::ObjectList<DataReference::MultiRefIterator>& multideps)
{
    ERROR_CONDITION(multideps.empty(), "There must be multidependences", 0);
    Nodecl::NodeclBase total_size;
    for (TL::ObjectList<DataReference::MultiRefIterator>::const_iterator
            mit = multideps.begin();
            mit != multideps.end();
            mit++)
    {
        // TL::Symbol iterator_sym = mit->first;
        Nodecl::Range range = mit->second.as<Nodecl::Range>();

        Nodecl::NodeclBase m;
        if (range.get_upper().is_constant()
                && range.get_lower().is_constant())
        {
            m = const_value_to_nodecl(
                    const_value_sub(
                        range.get_upper().get_constant(),
                        range.get_lower().get_constant()));
        }
        else
        {
            m = Nodecl::Minus::make(
                    range.get_upper().shallow_copy(),
                    range.get_lower().shallow_copy(),
                    TL::Type::get_int_type(),
                    range.get_locus());
        }

        Nodecl::NodeclBase a;
        if (m.is_constant())
        {
            a = const_value_to_nodecl(
                    const_value_add(
                        m.get_constant(),
                        const_value_get_signed_int(1)));
        }
        else
        {
            a = Nodecl::Add::make(
                    m,
                    const_value_to_nodecl(const_value_get_signed_int(1)),
                    TL::Type::get_int_type(),
                    range.get_locus());
        }

        Nodecl::NodeclBase current_size = a;
        if (total_size.is_null())
        {
            total_size = current_size;
        }
        else
        {
            if (total_size.is_constant()
                    && current_size.is_constant())
            {
                total_size = const_value_to_nodecl(
                        const_value_mul(
                            total_size.get_constant(),
                            current_size.get_constant()));
            }
            else
            {
                if (total_size.is_constant()
                        && const_value_is_one(total_size.get_constant()))
                {
                    total_size = current_size;
                }
                else if (current_size.is_constant()
                        && const_value_is_one(current_size.get_constant()))
                {
                    // total_size = total_size;
                }
                else
                {
                    total_size = Nodecl::Mul::make(
                            total_size,
                            current_size,
                            TL::Type::get_int_type(),
                            total_size.get_locus());

                }
            }
        }
    }

    return total_size;
}

template <typename Items>
Nodecl::NodeclBase LoweringVisitor::count_dynamic_items(OutlineInfo& outline_info,
        const TL::ObjectList<Items>& (OutlineDataItem::*getter)() const)
{
    Nodecl::NodeclBase result;

    TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        const TL::ObjectList<Items> &items = ((*it)->*getter)();

        for (typename TL::ObjectList<Items>::const_iterator it_items = items.begin();
                it_items != items.end();
                it_items++)
        {
            DataReference data_ref(it_items->expression);
            if (!data_ref.is_multireference())
                continue;

            TL::ObjectList<DataReference::MultiRefIterator> multideps = data_ref.multireferences();
            Nodecl::NodeclBase total_size = count_multidependences_extent(multideps);

            if (result.is_null())
            {
                result = total_size;
            }
            else
            {
                result = Nodecl::Add::make(
                        result,
                        total_size,
                        TL::Type::get_int_type(),
                        result.get_locus());
            }
        }
    }

    return result;
}

Nodecl::NodeclBase LoweringVisitor::count_dynamic_dependences(OutlineInfo& outline_info)
{
    return count_dynamic_items<OutlineDataItem::DependencyItem>(outline_info, &OutlineDataItem::get_dependences);
}

Nodecl::NodeclBase LoweringVisitor::count_dynamic_copies(OutlineInfo& outline_info)
{
    return count_dynamic_items<OutlineDataItem::CopyItem>(outline_info, &OutlineDataItem::get_copies);
}

template <typename Items>
void LoweringVisitor::count_items(OutlineInfo& outline_info,
        const TL::ObjectList<Items>& (OutlineDataItem::*getter)() const,
        int &num_static_items,
        int &num_dynamic_items)
{
    num_static_items = 0;
    num_dynamic_items = 0;

    TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        const TL::ObjectList<Items> &items = ((*it)->*getter)();
        for (typename TL::ObjectList<Items>::const_iterator it_items = items.begin();
                it_items != items.end();
                it_items++)
        {
            DataReference data_ref(it_items->expression);
            if (!data_ref.is_multireference())
            {
                num_static_items++;
            }
            else
            {
                num_dynamic_items++;
            }
        }
    }
}

void LoweringVisitor::count_copies(OutlineInfo& outline_info,
        int &num_static_copies,
        int &num_dynamic_copies)
{
    count_items<OutlineDataItem::CopyItem>(outline_info,
            &OutlineDataItem::get_copies,
            num_static_copies, num_dynamic_copies);
}

Nodecl::NodeclBase LoweringVisitor::count_copies_dimensions(OutlineInfo& outline_info)
{
    Nodecl::NodeclBase result;

    TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        TL::ObjectList<OutlineDataItem::CopyItem> copies = (*it)->get_copies();
        for (TL::ObjectList<OutlineDataItem::CopyItem>::iterator copy_it = copies.begin();
                copy_it != copies.end();
                copy_it++)
        {
            DataReference data_ref(copy_it->expression);
            int v = std::max(1, data_ref.get_data_type().get_num_dimensions());

            Nodecl::NodeclBase current_value = const_value_to_nodecl(const_value_get_signed_int(v));

            if (data_ref.is_multireference())
            {
                Nodecl::NodeclBase total_base =
                    count_multidependences_extent(data_ref.multireferences());

                if (total_base.is_constant()
                        && current_value.is_constant())
                {
                    current_value = const_value_to_nodecl(
                            const_value_mul(
                                total_base.get_constant(),
                                current_value.get_constant()));
                }
                else
                {
                    if (current_value.is_constant()
                            && const_value_is_one(current_value.get_constant()))
                    {
                        current_value = total_base;
                    }
                    else if (total_base.is_constant()
                            && const_value_is_one(total_base.get_constant()))
                    {
                        // current_value = current_value;
                    }
                    else
                    {
                        current_value = Nodecl::Mul::make(
                                total_base,
                                current_value,
                                TL::Type::get_int_type());
                    }
                }
            }

            if (result.is_null())
            {
                result = current_value;
            }
            else
            {
                if (result.is_constant()
                        && current_value.is_constant())
                {
                    result = const_value_to_nodecl(
                            const_value_add(
                                result.get_constant(),
                                current_value.get_constant()));
                }
                else
                {
                    result = Nodecl::Add::make(
                            result,
                            current_value,
                            TL::Type::get_int_type());
                }
            }
        }
    }

    if (result.is_null())
    {
        result = const_value_to_nodecl(const_value_get_signed_int(0));
    }

    return result;
}

void LoweringVisitor::handle_copy_item(
        TL::DataReference& data_ref,
        OutlineDataItem::CopyDirectionality dir,
        Nodecl::NodeclBase ctr,
        Source current_copy_index,
        Source current_dimension_descriptor_index,
        // out
        Source &copy_ol_setup,
        Source &copy_imm_setup,
        int &num_dimensions_of_copy)
{
    Nodecl::NodeclBase address_of_object = data_ref.get_address_of_symbol();

    int input = (dir & OutlineDataItem::COPY_IN) == OutlineDataItem::COPY_IN;
    int output = (dir & OutlineDataItem::COPY_OUT) == OutlineDataItem::COPY_OUT;

    Source num_dimensions, dimension_descriptor_name, ol_dimension_descriptors, imm_dimension_descriptors, copy_offset;

    copy_ol_setup
        << ol_dimension_descriptors
        << "ol_copy_data[" << current_copy_index << "].sharing = NANOS_SHARED;"
        << "ol_copy_data[" << current_copy_index << "].address = (void*)" << as_expression(address_of_object) << ";"
        << "ol_copy_data[" << current_copy_index << "].flags.input = " << input << ";"
        << "ol_copy_data[" << current_copy_index << "].flags.output = " << output << ";"
        << "ol_copy_data[" << current_copy_index << "].dimension_count = (short)" << num_dimensions << ";"
        << "ol_copy_data[" << current_copy_index << "].dimensions = &(ol_copy_dimensions[" << current_dimension_descriptor_index << "]);"
        << "ol_copy_data[" << current_copy_index << "].offset = " << copy_offset << ";"
        ;

    copy_imm_setup
        << imm_dimension_descriptors
        << "imm_copy_data[" << current_copy_index << "].sharing = NANOS_SHARED;"
        << "imm_copy_data[" << current_copy_index << "].address = (void*)" << as_expression(address_of_object) << ";"
        << "imm_copy_data[" << current_copy_index << "].flags.input = " << input << ";"
        << "imm_copy_data[" << current_copy_index << "].flags.output = " << output << ";"
        << "imm_copy_data[" << current_copy_index << "].dimension_count = (short)" << num_dimensions << ";"
        << "imm_copy_data[" << current_copy_index << "].dimensions = &(imm_copy_dimensions[" << current_dimension_descriptor_index << "]);"
        << "imm_copy_data[" << current_copy_index << "].offset = " << copy_offset << ";"
        ;

    copy_offset << as_expression(data_ref.get_offsetof_copy(data_ref, ctr.retrieve_context()));

    TL::Type copy_type = data_ref.get_data_type();
    num_dimensions_of_copy = copy_type.get_num_dimensions();

    TL::Type base_type;
    ObjectList<Nodecl::NodeclBase> lower_bounds, upper_bounds, dims_sizes;
    if (num_dimensions_of_copy == 0)
    {
       base_type = copy_type;
       lower_bounds.append(const_value_to_nodecl(const_value_get_signed_int(0)));
       upper_bounds.append(const_value_to_nodecl(const_value_get_signed_int(0)));
       dims_sizes.append(const_value_to_nodecl(const_value_get_signed_int(1)));
       num_dimensions_of_copy++;
    }
    else
    {
       compute_array_info(ctr, data_ref, copy_type, base_type, lower_bounds, upper_bounds, dims_sizes);

       // Sanity check
        ERROR_CONDITION(num_dimensions_of_copy != (signed)lower_bounds.size()
                || num_dimensions_of_copy != (signed)upper_bounds.size()
                || num_dimensions_of_copy != (signed)dims_sizes.size(),
                "Mismatch between dimensions", 0);
    }

    num_dimensions
        << num_dimensions_of_copy;

    int k = 0;
    for (int dim = num_dimensions_of_copy - 1; dim >= 0; dim--, k++)
    {
        if (dim == num_dimensions_of_copy - 1)
        {
            // In bytes
            ol_dimension_descriptors
                << "ol_copy_dimensions[" << current_dimension_descriptor_index << "+" << k << "].size = "
                << "(" << as_expression(dims_sizes[dim].shallow_copy()) << ") * sizeof(" << as_type(base_type) << ");"
                <<  "ol_copy_dimensions[" << current_dimension_descriptor_index << "+" << k  << "].lower_bound = "
                << "(" << as_expression(lower_bounds[dim].shallow_copy()) << ") * sizeof(" << as_type(base_type) << ");"
                <<  "ol_copy_dimensions[" << current_dimension_descriptor_index << "+" << k  << "].accessed_length = "
                << "((" << as_expression(upper_bounds[dim].shallow_copy()) << ") - ("
                << as_expression(lower_bounds[dim].shallow_copy()) << ") + 1) * sizeof(" << as_type(base_type) << ");"
                ;
            imm_dimension_descriptors
                << "imm_copy_dimensions[" << current_dimension_descriptor_index << "+" << k  << "].size = "
                << "(" << as_expression(dims_sizes[dim].shallow_copy()) << ") * sizeof(" << as_type(base_type) << ");"
                <<  "imm_copy_dimensions[" << current_dimension_descriptor_index << "+" << k  << "].lower_bound = "
                << "(" << as_expression(lower_bounds[dim].shallow_copy()) << ") * sizeof(" << as_type(base_type) << ");"
                <<  "imm_copy_dimensions[" << current_dimension_descriptor_index << "+" << k  << "].accessed_length = "
                << "((" << as_expression(upper_bounds[dim].shallow_copy()) << ") - ("
                << as_expression(lower_bounds[dim].shallow_copy()) << ") + 1) * sizeof(" << as_type(base_type) << ");"
                ;
        }
        else
        {
            // In elements
            ol_dimension_descriptors
                << "ol_copy_dimensions[" << current_dimension_descriptor_index << "+" << k  << "].size = "
                << as_expression(dims_sizes[dim].shallow_copy()) << ";"
                << "ol_copy_dimensions[" << current_dimension_descriptor_index << "+" << k  << "].lower_bound = "
                << as_expression(lower_bounds[dim].shallow_copy()) << ";"
                << "ol_copy_dimensions[" << current_dimension_descriptor_index << "+" << k  << "].accessed_length = "
                << "(" << as_expression(upper_bounds[dim].shallow_copy()) << ") - ("
                << as_expression(lower_bounds[dim].shallow_copy()) << ") + 1;"
                ;
            imm_dimension_descriptors
                << "imm_copy_dimensions[" << current_dimension_descriptor_index << "+" << k  << "].size = "
                << as_expression(dims_sizes[dim].shallow_copy()) << ";"
                << "imm_copy_dimensions[" << current_dimension_descriptor_index << "+" << k  << "].lower_bound = "
                << as_expression(lower_bounds[dim].shallow_copy()) << ";"
                << "imm_copy_dimensions[" << current_dimension_descriptor_index << "+" << k  << "].accessed_length = "
                << "(" << as_expression(upper_bounds[dim].shallow_copy()) << ") - ("
                << as_expression(lower_bounds[dim].shallow_copy()) << ") + 1;"
                ;
        }
    }
}

void LoweringVisitor::fill_copies_region(
        Nodecl::NodeclBase ctr,
        OutlineInfo& outline_info,
        int num_static_copies,
        // int num_dynamic_copies,
        Source num_copies,
        Nodecl::NodeclBase num_copies_dimensions,
        // out
        Source& copy_ol_decl,
        Source& copy_ol_arg,
        Source& copy_ol_setup,
        Source& copy_imm_arg,
        Source& copy_imm_setup)
{
    // typedef struct {
    //     void *address;
    //     nanos_sharing_t sharing;
    //     struct {
    //         bool input: 1;
    //         bool output: 1;
    //     } flags;
    //     short dimension_count;
    //     nanos_region_dimension_internal_t const *dimensions;
    //     ptrdiff_t offset;
    // } nanos_copy_data_internal_t;

    if (IS_C_LANGUAGE
            || IS_CXX_LANGUAGE)
    {
        copy_ol_decl
            << "nanos_copy_data_t *ol_copy_data = (nanos_copy_data_t*)0;"
            << "nanos_region_dimension_internal_t * ol_copy_dimensions = (nanos_region_dimension_internal_t*)0;"
            ;
        copy_ol_arg << "&ol_copy_data, &ol_copy_dimensions";
        copy_imm_arg << "imm_copy_data, imm_copy_dimensions";
        copy_imm_setup
            << "nanos_copy_data_t imm_copy_data[" << num_copies << "];"
            << "nanos_region_dimension_internal_t imm_copy_dimensions[" << as_expression(num_copies_dimensions) << "];"
            ;
    }
    else if (IS_FORTRAN_LANGUAGE)
    {
        copy_ol_decl
            << "nanos_copy_data_t ol_copy_data[" << num_copies << "];"
            << "nanos_region_dimension_internal_t ol_copy_dimensions[" << as_expression(num_copies_dimensions) << "];"
            ;
        copy_ol_arg << "(nanos_copy_data_t**)0, (nanos_region_dimension_internal_t**)0";
        copy_imm_arg << "imm_copy_data, imm_copy_dimensions";
        copy_imm_setup
            << "nanos_copy_data_t imm_copy_data[" << num_copies << "];"
            << "nanos_region_dimension_internal_t imm_copy_dimensions[" << as_expression(num_copies_dimensions) << "];"
            ;
    }

    TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();

    TL::Counter &dep_dim_num = TL::CounterManager::get_counter("nanos++-copy-deps-dimensions");
    int current_copy_idx = 0;
    int current_dimension_descriptor_idx = 0;
    bool there_are_dynamic_copies = false;

    // Static copies first
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        TL::ObjectList<OutlineDataItem::CopyItem> copies = (*it)->get_copies();

        if (copies.empty())
            continue;

        for (TL::ObjectList<OutlineDataItem::CopyItem>::iterator copy_it = copies.begin();
                copy_it != copies.end();
                copy_it++)
        {
            TL::DataReference copy_expr(copy_it->expression);
            if (copy_expr.is_multireference())
            {
                there_are_dynamic_copies = true;
                // We handle them below
                continue;
            }

            Source current_copy_index;
            current_copy_index << current_copy_idx;

            Source current_dimension_descriptor_index;
            current_dimension_descriptor_index << current_dimension_descriptor_idx;

            int num_dimensions_of_copy = 0;
            handle_copy_item(copy_expr,
                    copy_it->directionality,
                    ctr,
                    current_copy_index,
                    current_dimension_descriptor_index,
                    // out
                    copy_ol_setup,
                    copy_imm_setup,
                    num_dimensions_of_copy);

            current_copy_idx++;
            current_dimension_descriptor_idx += num_dimensions_of_copy;
        }
    }

    if (there_are_dynamic_copies)
    {
        TL::Scope sc = ctr.retrieve_context();
        // Index for the dimension array
        std::stringstream ss;
        ss << "nanos_dyn_copy_idx_" << (int)dep_dim_num;
        dep_dim_num++;

        TL::Symbol dyn_copy_idx = sc.new_symbol(ss.str());
        dyn_copy_idx.get_internal_symbol()->kind = SK_VARIABLE;
        dyn_copy_idx.get_internal_symbol()->type_information = get_signed_int_type();
        symbol_entity_specs_set_is_user_declared(dyn_copy_idx.get_internal_symbol(), 1);

        if (IS_CXX_LANGUAGE)
        {
            Nodecl::NodeclBase def = Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
                    dyn_copy_idx,
                    ctr.get_locus());
            ctr.prepend_sibling(def);
        }

        ss.str(""); ss << "nanos_dyn_copy_dim_idx_" << (int)dep_dim_num;
        dep_dim_num++;

        TL::Symbol dyn_dim_idx = sc.new_symbol(ss.str());
        dyn_dim_idx.get_internal_symbol()->kind = SK_VARIABLE;
        dyn_dim_idx.get_internal_symbol()->type_information = get_signed_int_type();
        symbol_entity_specs_set_is_user_declared(dyn_dim_idx.get_internal_symbol(), 1);

        if (IS_CXX_LANGUAGE)
        {
            Nodecl::NodeclBase def = Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
                    dyn_dim_idx,
                    ctr.get_locus());
            ctr.prepend_sibling(def);
        }

        copy_ol_setup << as_symbol(dyn_copy_idx) << " = " << current_copy_idx << ";"
                      << as_symbol(dyn_dim_idx) << " = " << num_static_copies << ";"
            ;
        copy_imm_setup << as_symbol(dyn_copy_idx) << " = " << current_copy_idx << ";"
                       << as_symbol(dyn_dim_idx) << " = " << num_static_copies << ";"
            ;

        // Dynamic copies second
        for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            TL::ObjectList<OutlineDataItem::CopyItem> copies = (*it)->get_copies();

            if (copies.empty())
                continue;

            for (TL::ObjectList<OutlineDataItem::CopyItem>::iterator copy_it = copies.begin();
                    copy_it != copies.end();
                    copy_it++)
            {
                TL::DataReference copy_expr(copy_it->expression);
                if (!copy_expr.is_multireference())
                {
                    // We handled them above
                    continue;
                }

                Source copies_loop;
                ObjectList<DataReference::MultiRefIterator> m = copy_expr.multireferences();

                Source dimension_array;
                dimension_array << "nanos_dyn_copy_dims_" << (int)dep_dim_num;
                dep_dim_num++;

                Nodecl::Utils::SimpleSymbolMap symbol_map;

                for (ObjectList<DataReference::MultiRefIterator>::iterator current_multidep = m.begin();
                        current_multidep != m.end();
                        current_multidep++)
                {
                    // Create the current induction variable
                    ss.str(""); ss << "nanos_dyn_copy_" << (int)dep_dim_num;
                    TL::Symbol new_sym = sc.new_symbol(ss.str() + "_" + current_multidep->first.get_name());
                    new_sym.get_internal_symbol()->kind = SK_VARIABLE;
                    new_sym.get_internal_symbol()->type_information = get_signed_int_type();
                    symbol_entity_specs_set_is_user_declared(new_sym.get_internal_symbol(), 1);

                    if (IS_CXX_LANGUAGE)
                    {
                        Nodecl::NodeclBase def = Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
                                new_sym,
                                ctr.get_locus());
                        ctr.prepend_sibling(def);
                    }

                    symbol_map.add_map(current_multidep->first, new_sym);

                    Nodecl::Range range = current_multidep->second.as<Nodecl::Range>();
                    ERROR_CONDITION(!range.is<Nodecl::Range>(), "Invalid node %s", ast_print_node_type(range.get_kind()));

                    Nodecl::NodeclBase lower = range.get_lower().shallow_copy();
                    Nodecl::NodeclBase upper = range.get_upper().shallow_copy();
                    Nodecl::NodeclBase stride = range.get_stride().shallow_copy();

                    copy_ol_setup
                        << "for ("
                        <<       as_symbol(new_sym) << "=" << as_expression(lower) << ";"
                        <<       as_symbol(new_sym) << "<=" << as_expression(upper) << ";"
                        <<       as_symbol(new_sym) << "+=" << as_expression(stride) << ")"
                        << "{"
                        ;
                    copy_imm_setup
                        << "for ("
                        <<       as_symbol(new_sym) << "=" << as_expression(lower) << ";"
                        <<       as_symbol(new_sym) << "<=" << as_expression(upper) << ";"
                        <<       as_symbol(new_sym) << "+=" << as_expression(stride) << ")"
                        << "{"
                        ;


                    if (current_multidep + 1 == m.end())
                    {
                        // If this is the last iterator, map the copy and
                        // generate the loop body
                        Nodecl::NodeclBase orig_copy = current_multidep->second;

                        // Now ignore the multidependence as such...
                        Nodecl::NodeclBase current_copy = copy_expr;
                        while (current_copy.is<Nodecl::MultiExpression>())
                        {
                            current_copy =
                                current_copy.as<Nodecl::MultiExpression>().get_base();
                        }

                        // and update it
                        Nodecl::NodeclBase updated_copy = Nodecl::Utils::deep_copy(
                                current_copy, sc, symbol_map);
                        TL::DataReference updated_copy_ref(updated_copy);

                        Source current_copy_index;
                        current_copy_index << as_symbol(dyn_copy_idx);

                        Source current_dimension_descriptor_index;
                        current_dimension_descriptor_index << as_symbol(dyn_dim_idx);

                        int num_dimensions_of_copy = 0;
                        handle_copy_item(updated_copy_ref,
                                copy_it->directionality,
                                ctr,
                                current_copy_index,
                                current_dimension_descriptor_index,
                                // out
                                copy_ol_setup,
                                copy_imm_setup,
                                num_dimensions_of_copy);

                        copy_ol_setup << as_symbol(dyn_copy_idx) << "++;";
                        copy_imm_setup << as_symbol(dyn_copy_idx) << "++;";

                        copy_ol_setup << as_symbol(dyn_dim_idx) << "+= " << num_dimensions_of_copy << ";";
                        copy_imm_setup << as_symbol(dyn_dim_idx) << "+= " << num_dimensions_of_copy << ";";

                        for (ObjectList<DataReference::MultiRefIterator>::reverse_iterator
                                rev_current_multidep = m.rbegin();
                                rev_current_multidep != m.rend();
                                rev_current_multidep++)
                        {
                            copy_ol_setup
                                << "}";
                            copy_imm_setup
                                << "}";
                        }
                    }
                }
            }
        }
    }

    if (IS_FORTRAN_LANGUAGE)
    {
        copy_ol_setup
            << "{"
            << "nanos_err_t nanos_err;"
            << "nanos_err = nanos_set_copies(nanos_wd_, " << num_copies << ", ol_copy_data);"
            << "if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);"
            << "}"
            ;
    }
}

void LoweringVisitor::fill_copies(
        Nodecl::NodeclBase ctr,
        OutlineInfo& outline_info,
        OutlineInfo* parameter_outline_info,
        TL::Symbol structure_symbol,
        // Source arguments_accessor,
        // out
        Source &num_copies,
        Source& copy_ol_decl,
        Source& copy_ol_arg,
        Source& copy_ol_setup,
        Source& copy_imm_arg,
        Source& copy_imm_setup,
        TL::Symbol& xlate_function_symbol
        )
{
    int num_static_copies, num_dynamic_copies;
    count_copies(outline_info, num_static_copies, num_dynamic_copies);

    if (num_dynamic_copies == 0)
    {
        num_copies << num_static_copies;
    }
    else
    {
        if (num_static_copies != 0)
        {
            num_copies << num_static_copies << "+";
        }

        num_copies << as_expression(count_dynamic_copies(outline_info));
    }

    if (Nanos::Version::interface_is_at_least("copies_api", 1000))
    {
        Nodecl::NodeclBase num_copies_dimensions = count_copies_dimensions(outline_info);

        if (num_static_copies == 0
                && num_dynamic_copies == 0)
        {
            copy_ol_arg << "(nanos_copy_data_t**)0, (nanos_region_dimension_internal_t**)0";
            copy_imm_arg << "(nanos_copy_data_t*)0, (nanos_region_dimension_internal_t*)0";
        }
        else
        {
            fill_copies_region(ctr,
                    outline_info,
                    num_static_copies,
                    // num_dynamic_dependences
                    num_copies,
                    num_copies_dimensions,
                    copy_ol_decl,
                    copy_ol_arg,
                    copy_ol_setup,
                    copy_imm_arg,
                    copy_imm_setup);

            if (/* bool allow_multiple_copies = */Nanos::Version::interface_is_at_least("copies_api", 1002))
            {
                emit_translation_function_region(ctr,
                        outline_info,
                        parameter_outline_info,
                        structure_symbol,
                        xlate_function_symbol);
            }
            else
            {
                internal_error("Code unreachable", 0);
            }
        }
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
}

// This visitor makes an in-place modification!
struct RewriteAddressExpression : public Nodecl::ExhaustiveVisitor<void>
{
    typedef std::map<TL::Symbol, TL::Symbol> sym_to_field_t;
    sym_to_field_t sym_to_field;
    TL::Symbol structure;

    void visit_post(const Nodecl::Symbol &n)
    {
        sym_to_field_t::iterator it = sym_to_field.find(n.get_symbol());
        if (it != sym_to_field.end())
        {
            Nodecl::NodeclBase struct_node = Nodecl::Symbol::make(structure);
            struct_node.set_type(structure.get_type());

            Nodecl::NodeclBase field_node = Nodecl::Symbol::make(it->second);
            field_node.set_type(it->second.get_type());

            n.replace(
                    Nodecl::ClassMemberAccess::make(
                        struct_node,
                        field_node,
                        /* member-form */ Nodecl::NodeclBase::null(),
                        field_node.get_type().get_lvalue_reference_to())
                    );
        }
    }

};

bool is_not_alnum(int charact) {
    return !std::isalnum(charact);
}

void LoweringVisitor::translate_single_item(
        Source &translations,
        Nodecl::NodeclBase ctr,
        OutlineDataItem* item,
        Nodecl::NodeclBase copy_num)
{
    translations
        << "{"
        << "void *device_base_address;"
        << "nanos_err_t nanos_err;"

        << "device_base_address = 0;"
        << "nanos_err = nanos_get_addr(" << as_expression(copy_num) << ", &device_base_address, wd);"
        << "if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);"
        ;

    if ((item->get_symbol().get_type().no_ref().is_fortran_array()
                && item->get_symbol().get_type().no_ref().array_requires_descriptor())
            || (item->get_symbol().get_type().no_ref().is_pointer()
                && item->get_symbol().get_type().no_ref().points_to().is_fortran_array()
                && item->get_symbol().get_type().no_ref().points_to().array_requires_descriptor()))
    {
        TL::Symbol new_function = get_function_modify_array_descriptor(
                item->get_field_name(),
                item->get_field_type(),
                ctr.retrieve_context());

        ERROR_CONDITION(item->get_copy_of_array_descriptor() == NULL, "This needs a copy of the array descriptor", 0);

        translations
            //<<  new_function.get_name() << "(arg." << item->get_field_name() << ", device_base_address);"
            <<  new_function.get_name() << "(arg." <<
            item->get_copy_of_array_descriptor()->get_field_name() << ", device_base_address);"
            << "}"
            ;
    }
    else
    {
        // Currently we do not support copies on non-shared stuff, so this should be always a pointer
        ERROR_CONDITION(!item->get_field_type().is_pointer(), "Invalid type, expecting a pointer", 0);

        translations
            << "arg." << item->get_field_name() << " = (" << as_type(item->get_field_type()) << ")device_base_address;"
            << "}"
            ;
    }
}

void LoweringVisitor::emit_translation_function_region(
        Nodecl::NodeclBase ctr,
        OutlineInfo& outline_info,
        OutlineInfo* parameter_outline_info,
        TL::Symbol structure_symbol,

        // Out
        TL::Symbol& translation_function_symbol
        )
{
    TL::Counter &fun_num = TL::CounterManager::get_counter("nanos++-translation-functions");
    std::string filename = TL::CompilationProcess::get_current_file().get_filename();
    //Remove non-alphanumeric characters from the string
    filename.erase(std::remove_if(filename.begin(), filename.end(), (bool(*)(int))is_not_alnum), filename.end());
    Source fun_name;
    fun_name << "nanos_xlate_fun_" << filename << "_" << fun_num;
    fun_num++;

    TL::Type argument_type = ::get_user_defined_type(structure_symbol.get_internal_symbol());
    argument_type = argument_type.get_lvalue_reference_to();

    ObjectList<std::string> parameter_names;
    ObjectList<TL::Type> parameter_types;

    parameter_names.append("arg");
    parameter_types.append(argument_type);

    TL::Symbol sym_nanos_wd_t = ReferenceScope(ctr).get_scope().get_symbol_from_name("nanos_wd_t");
    ERROR_CONDITION(!sym_nanos_wd_t.is_valid(), "Typename nanos_wd_t not found", 0);
    parameter_names.append("wd");
    parameter_types.append(sym_nanos_wd_t.get_user_defined_type());

    translation_function_symbol = SymbolUtils::new_function_symbol(
            Nodecl::Utils::get_enclosing_function(ctr),
            fun_name.get_source(),
            TL::Type::get_void_type(),
            parameter_names,
            parameter_types);

    Nodecl::NodeclBase function_code, empty_statement;
    SymbolUtils::build_empty_body_for_function(
            translation_function_symbol,
            function_code,
            empty_statement);

    TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();

    Source translations;

    // First the static ones
    TL::ObjectList<OutlineDataItem*> already_processed;
    int current_copy_num = 0;
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        TL::ObjectList<OutlineDataItem::CopyItem> copies = (*it)->get_copies();

        if (copies.empty())
            continue;

        //ERROR_CONDITION((*it)->get_sharing() != OutlineDataItem::SHARING_SHARED, "Unexpected sharing\n", 0);

        int num_static_copies = 0;

        for (TL::ObjectList<OutlineDataItem::CopyItem>::iterator
                copy_it = copies.begin();
                copy_it != copies.end();
                copy_it++)
        {
            TL::DataReference copy_expr(copy_it->expression);
            if (!copy_expr.is_multireference())
                num_static_copies++;
        }

        if (num_static_copies == 0)
            continue;

        already_processed.append(*it);

        translate_single_item(translations,
                ctr,
                *it,
                // copy_num
                const_value_to_nodecl(const_value_get_signed_int(current_copy_num)));

        current_copy_num += num_static_copies;
    }

    // Second chance for multidependences
    int num_dynamic_copies = 0;
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        if (already_processed.contains(*it))
            continue;

        TL::ObjectList<OutlineDataItem::CopyItem> copies = (*it)->get_copies();
        if (copies.empty())
            continue;

        Nodecl::NodeclBase copy_num;
        if (num_dynamic_copies == 0)
        {
            copy_num = const_value_to_nodecl(const_value_get_signed_int(current_copy_num));
        }
        else
        {
            Source src;
            OutlineDataItem& multicopies_index_symbol = outline_info.get_entity_for_symbol(
                    outline_info.get_multicopies_index_symbol()
                    );
            src << "arg." << multicopies_index_symbol.get_field_name() << "[" << (num_dynamic_copies - 1) << "]";

            copy_num = src.parse_expression(empty_statement);
        }

        translate_single_item(translations,
                ctr,
                *it,
                copy_num);

        num_dynamic_copies++;
    }

    if (IS_FORTRAN_LANGUAGE)
    {
        Source::source_language = SourceLanguage::C;
    }
    Nodecl::NodeclBase translations_tree = translations.parse_statement(empty_statement);
    if (IS_FORTRAN_LANGUAGE)
    {
        Source::source_language = SourceLanguage::Current;
    }

    empty_statement.replace(translations_tree);

    Nodecl::Utils::prepend_to_enclosing_top_level_location(ctr, function_code);
}

void LoweringVisitor::fill_dependences(
        Nodecl::NodeclBase ctr,
        OutlineInfo& outline_info,
        int num_static_dependences,
        int num_dynamic_dependences,
        Source runtime_num_dependences,
        // out
        Source& result_src)
{
    fill_dependences_internal(ctr, outline_info, /* on_wait */ false,
            num_static_dependences,
            num_dynamic_dependences,
            runtime_num_dependences,
            result_src);
}

void LoweringVisitor::fortran_dependence_extra_check(
        const TL::DataReference& dep_expr,
        // out
        bool &is_fortran_allocatable_dependence,
        bool &is_fortran_pointer_dependence)
{
    is_fortran_allocatable_dependence = false;
    is_fortran_pointer_dependence = false;

    if (!IS_FORTRAN_LANGUAGE)
        return;

    TL::Symbol relevant_symbol;
    for (Nodecl::NodeclBase n = dep_expr;;)
    {
        if (n.is<Nodecl::Symbol>())
        {
            relevant_symbol = n.get_symbol();
            break;
        }
        else if (n.is<Nodecl::ClassMemberAccess>())
        {
            n = n.as<Nodecl::ClassMemberAccess>().get_member();
        }
        else if (n.is<Nodecl::ArraySubscript>())
        {
            n = n.as<Nodecl::ArraySubscript>().get_subscripted();
        }
        else if (n.is<Nodecl::Dereference>())
        {
            n = n.as<Nodecl::Dereference>().get_rhs();
        }
        else if (n.is<Nodecl::Conversion>())
        {
            n = n.as<Nodecl::Conversion>().get_nest();
        }
        else
        {
            break;
        }
    }

    if (relevant_symbol.is_valid())
    {
        is_fortran_allocatable_dependence = relevant_symbol.is_allocatable();
        is_fortran_pointer_dependence = relevant_symbol.get_type().no_ref().is_pointer();

        ERROR_CONDITION(is_fortran_allocatable_dependence
                && is_fortran_pointer_dependence, "Not possible", 0);
    }
}


void LoweringVisitor::handle_dependency_item(
        Nodecl::NodeclBase ctr,
        TL::DataReference dep_expr,
        OutlineDataItem::DependencyDirectionality dir,
        Source dimension_array,
        Source& current_dep_num,
        Source& result_src)
{
    if (!dep_expr.is_valid())
    {
        dep_expr.commit_diagnostic();
        internal_error(
            "%s: Invalid dependency detected '%s'",
            dep_expr.get_locus_str().c_str(),
            dep_expr.prettyprint().c_str());
    }

    Source dependency_offset,
           dependency_flags,
           dependency_flags_in,
           dependency_flags_out,
           dependency_flags_concurrent,
           dependency_flags_commutative;

    Nodecl::NodeclBase base_address, dep_source_expr = dep_expr;

    // if (!(*it)->get_base_address_expression().is_null())
    // {
    //     // This is only for function task dependences
    //     // Outline tasks do not need any of this
    //     dep_source_expr = base_address = (*it)->get_base_address_expression();

    //     if ((IS_CXX_LANGUAGE
    //                 || IS_FORTRAN_LANGUAGE)
    //             && (*it)->get_symbol().get_type().is_lvalue_reference())
    //     {
    //         // If the parameter type
    //         TL::Type t = base_address.get_type();
    //         if (t.is_any_reference())
    //             t = t.references_to();
    //         t = t.get_pointer_to();
    //         // Create a reference here
    //         base_address = Nodecl::Reference::make(
    //                 base_address.shallow_copy(),
    //                 t,
    //                 base_address.get_filename(),
    //                 base_address.get_line());
    //     }
    // }
    // else
    {
        base_address = dep_expr.get_base_address().shallow_copy();
    }

    // std::cerr << "BASE ADDRESS -> |" << base_address.prettyprint() << "|" << std::endl;

    dependency_flags
        << "{"
        << dependency_flags_in << ","
        << dependency_flags_out << ", "
        << /* renaming has not yet been implemented */ "0, "
        << dependency_flags_concurrent << ","
        << dependency_flags_commutative
        << "}"
        ;

    bool input         = ((dir & OutlineDataItem::DEP_IN) == OutlineDataItem::DEP_IN);
    bool input_alloca  = ((dir & OutlineDataItem::DEP_IN_ALLOCA) == OutlineDataItem::DEP_IN_ALLOCA);
    bool input_private = ((dir & OutlineDataItem::DEP_IN_PRIVATE) == OutlineDataItem::DEP_IN_PRIVATE);
    bool output        = ((dir & OutlineDataItem::DEP_OUT) == OutlineDataItem::DEP_OUT);
    bool concurrent    = ((dir & OutlineDataItem::DEP_CONCURRENT) == OutlineDataItem::DEP_CONCURRENT);
    bool commutative   = ((dir & OutlineDataItem::DEP_COMMUTATIVE) == OutlineDataItem::DEP_COMMUTATIVE);

    dependency_flags_in  << (input || input_alloca || input_private || concurrent || commutative);
    dependency_flags_out << (output || concurrent || commutative);
    dependency_flags_concurrent << concurrent;
    dependency_flags_commutative << commutative;

    // Compute the base type of the dependency and the array containing the size of each dimension
    Nodecl::NodeclBase dep_expr_offset = dep_expr.get_offsetof_dependence();
    ERROR_CONDITION(dep_expr_offset.is_null(), "Failed to synthesize an expression denoting offset", 0);

    dependency_offset << as_expression(dep_expr_offset);


    bool is_fortran_allocatable_dependence = false;
    bool is_fortran_pointer_dependence = false;

    fortran_dependence_extra_check(
            dep_expr,
            // out
            is_fortran_allocatable_dependence,
            is_fortran_pointer_dependence);

    if (is_fortran_allocatable_dependence)
    {
        Nodecl::NodeclBase n = dep_expr;
        if (n.is<Nodecl::ArraySubscript>())
            n = n.as<Nodecl::ArraySubscript>().get_subscripted();
        n = n.shallow_copy();

        Source check_for_allocated_src;
        check_for_allocated_src
            << "ALLOCATED(" << as_expression(n) << ")";

        Nodecl::NodeclBase check_for_allocated =
            check_for_allocated_src.parse_expression(Scope(CURRENT_COMPILED_FILE->global_decl_context));

        result_src << "if (" << as_expression(check_for_allocated) << ") {"
            ;
    }
    else if (is_fortran_pointer_dependence)
    {
        Nodecl::NodeclBase n = dep_expr;
        if (n.is<Nodecl::ArraySubscript>())
            n = n.as<Nodecl::ArraySubscript>().get_subscripted();

        n = n.no_conv();
        ERROR_CONDITION(!n.is<Nodecl::Dereference>(), "Invalid node", 0);
        n = n.as<Nodecl::Dereference>().get_rhs();

        n = n.shallow_copy();

        Source check_for_allocated_src;
        check_for_allocated_src
            << "ASSOCIATED(" << as_expression(n) << ")";

        Nodecl::NodeclBase check_for_associated =
            check_for_allocated_src.parse_expression(Scope(CURRENT_COMPILED_FILE->global_decl_context));

        result_src << "if (" << as_expression(check_for_associated) << ") {"
            ;
    }

    Type dependency_type = dep_expr.get_data_type();
    int num_dimensions_of_dep = dependency_type.get_num_dimensions();

    TL::Type base_type;
    ObjectList<Nodecl::NodeclBase> lower_bounds, upper_bounds, dims_sizes;
    if (num_dimensions_of_dep == 0)
    {
       base_type = dependency_type;
       lower_bounds.append(const_value_to_nodecl(const_value_get_signed_int(0)));
       upper_bounds.append(const_value_to_nodecl(const_value_get_signed_int(0)));
       dims_sizes.append(const_value_to_nodecl(const_value_get_signed_int(1)));
       num_dimensions_of_dep++;
    }
    else
    {
       compute_array_info(ctr, dep_expr, dependency_type, base_type, lower_bounds, upper_bounds, dims_sizes);

        // Sanity check
        ERROR_CONDITION(num_dimensions_of_dep != (signed)lower_bounds.size()
                || num_dimensions_of_dep != (signed)upper_bounds.size()
                || num_dimensions_of_dep != (signed)dims_sizes.size(),
                "Mismatch between dimensions", 0);
    }

    int idx = 0;
    for (int dim = num_dimensions_of_dep - 1; dim >= 0; dim--, idx++)
    {
        if (dim == num_dimensions_of_dep - 1)
        {
            // In bytes
            result_src
                << dimension_array << "[" << idx << "].size = "
                << "(" << as_expression(dims_sizes[dim].shallow_copy()) << ") * sizeof(" << as_type(base_type) << ");"
                << dimension_array << "[" << idx  << "].lower_bound = "
                << "(" << as_expression(lower_bounds[dim].shallow_copy()) << ") * sizeof(" << as_type(base_type) << ");"
                <<  dimension_array << "[" << idx  << "].accessed_length = "
                << "((" << as_expression(upper_bounds[dim].shallow_copy()) << ") - ("
                << as_expression(lower_bounds[dim].shallow_copy()) << ") + 1) * sizeof(" << as_type(base_type) << ");"
                ;
        }
        else
        {
            // In elements
            result_src
                << dimension_array << "[" << idx  << "].size = "
                << as_expression(dims_sizes[dim].shallow_copy()) << ";"
                << dimension_array << "[" << idx  << "].lower_bound = "
                << as_expression(lower_bounds[dim].shallow_copy()) << ";"
                << dimension_array << "[" << idx  << "].accessed_length = "
                << "(" << as_expression(upper_bounds[dim].shallow_copy()) << ") - ("
                << as_expression(lower_bounds[dim].shallow_copy()) << ") + 1;"
                ;
        }
    }



    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
    {
        result_src
            << "dependences[" << current_dep_num << "].address = (void*)"
            << as_expression(base_address) << ";"
	    << "dependences[" << current_dep_num << "].offset = " << dependency_offset << ";"
            << "dependences[" << current_dep_num << "].dimensions = " << dimension_array << ";"
            ;
    }
    else if (IS_FORTRAN_LANGUAGE)
    {
        result_src
            << "dependences[" << current_dep_num << "].address ="
            << as_expression(base_address) << ";"
	    << "dependences[" << current_dep_num << "].offset = " << dependency_offset << ";"
            << "dependences[" << current_dep_num << "].dimensions = &(" << dimension_array << "[0]);"
            ;

        if (is_fortran_allocatable_dependence
                || is_fortran_pointer_dependence)
        {
            result_src
                << "} else {"
                <<    "dependences[" << current_dep_num << "].address = 0;"
                <<    "dependences[" << current_dep_num << "].offset = 0;"
                << "}"
                ;
        }
    }
    else
    {
        internal_error("Code unreachable", 0);
    }

    result_src
        << "dependences[" << current_dep_num << "].flags.input = " << dependency_flags_in << ";"
        << "dependences[" << current_dep_num << "].flags.output = " << dependency_flags_out << ";"
        << "dependences[" << current_dep_num << "].flags.can_rename = 0;"
        << "dependences[" << current_dep_num << "].flags.concurrent = " << dependency_flags_concurrent << ";"
        << "dependences[" << current_dep_num << "].flags.commutative = " << dependency_flags_commutative << ";"
        << "dependences[" << current_dep_num << "].dimension_count = " << num_dimensions_of_dep << ";"
        ;
}

void LoweringVisitor::fill_dependences_internal(
        Nodecl::NodeclBase ctr,
        OutlineInfo& outline_info,
        bool on_wait,
        int num_static_dependences,
        int num_dynamic_dependences,
        Source& runtime_num_dependences,
        // out
        Source& result_src)
{
    TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();

    int total_dependences = num_static_dependences + num_dynamic_dependences;
    if (total_dependences == 0)
    {
        if (Nanos::Version::interface_is_at_least("deps_api", 1001))
        {
            result_src << "nanos_data_access_t dependences[1];"
                ;
        }
        else
        {
            result_src << "nanos_dependence_t dependences[1];"
                ;
        }

        return;
    }

    if (!Nanos::Version::interface_is_at_least("deps_api", 1001))
    {
        fatal_printf_at(ctr.get_locus(),
                "please update your runtime version. deps_api < 1001 not supported\n");
    }

    Source dependency_regions;

    result_src
        << dependency_regions
        << "nanos_data_access_t dependences[" << runtime_num_dependences << "];"
        ;


    int current_static_dep_idx = 0;
    bool there_are_dynamic_dependences = false;

    // Static dependences
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        TL::ObjectList<OutlineDataItem::DependencyItem> deps = (*it)->get_dependences();
        for (ObjectList<OutlineDataItem::DependencyItem>::iterator dep_it = deps.begin();
                dep_it != deps.end();
                dep_it++)
        {
            OutlineDataItem::DependencyDirectionality dir = dep_it->directionality;
            TL::DataReference dep_expr(dep_it->expression);

            if (dep_expr.is_multireference())
            {
                there_are_dynamic_dependences = true;
                // We will handle them later
                continue;
            }

            Source current_dep_num;
            current_dep_num << current_static_dep_idx;

            Type dependency_type = dep_expr.get_data_type();
            int num_dimensions = dependency_type.get_num_dimensions();

            TL::Counter &dep_dim_num = TL::CounterManager::get_counter("nanos++-copy-deps-dimensions");
            Source dimension_name;
            dimension_name << "dimensions_" << (int)dep_dim_num;
            dep_dim_num++;
            dependency_regions << "nanos_region_dimension_t " << dimension_name << "[" << std::max(num_dimensions, 1) << "];"
                ;

            handle_dependency_item(ctr, dep_expr, dir,
                    dimension_name, current_dep_num, result_src);

            current_static_dep_idx++;
        }
    }

    // Dynamic dependences
    if (there_are_dynamic_dependences)
    {
        TL::Scope sc = ctr.retrieve_context();
        TL::Symbol dyn_dep_idx;

        TL::Counter &dep_dim_num = TL::CounterManager::get_counter("nanos++-dynamic-deps");
        std::stringstream ss; ss << "nanos_dyn_dep_idx_" << (int)dep_dim_num;
        dep_dim_num++;

        // Create the global dynamic index
        dyn_dep_idx = sc.new_symbol(ss.str());
        dyn_dep_idx.get_internal_symbol()->kind = SK_VARIABLE;
        dyn_dep_idx.get_internal_symbol()->type_information = get_signed_int_type();
        dyn_dep_idx.get_internal_symbol()->value =
            const_value_to_nodecl(const_value_get_signed_int(0));
        symbol_entity_specs_set_is_user_declared(dyn_dep_idx.get_internal_symbol(), 1);

        result_src << as_symbol(dyn_dep_idx) << " = " << num_static_dependences << ";";

        if (IS_CXX_LANGUAGE)
        {
            Nodecl::NodeclBase def = Nodecl::CxxDef::make(
                    Nodecl::NodeclBase::null(),
                    dyn_dep_idx,
                    ctr.get_locus());
            // FIXME - Check this
            ctr.prepend_sibling(def);
        }

        for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            TL::ObjectList<OutlineDataItem::DependencyItem> deps = (*it)->get_dependences();

            for (ObjectList<OutlineDataItem::DependencyItem>::iterator dep_it = deps.begin();
                    dep_it != deps.end();
                    dep_it++)
            {
                OutlineDataItem::DependencyDirectionality dir = dep_it->directionality;
                TL::DataReference dep_expr(dep_it->expression);

                if (!dep_expr.is_multireference())
                {
                    // Static dependences were handled above
                    continue;
                }

                Source dependency_loop;
                ObjectList<DataReference::MultiRefIterator> m = dep_expr.multireferences();

                // Create the dimensionality array
                Nodecl::NodeclBase total_base = count_multidependences_extent(m);

                Source dimension_array;
                dimension_array << "nanos_dyn_dims_" << (int)dep_dim_num;
                dep_dim_num++;

                Type dependency_type = dep_expr.get_data_type();
                int num_dimensions = dependency_type.get_num_dimensions();

                dependency_regions
                    << "nanos_region_dimension_t " << dimension_array
                        << "[" << as_expression(total_base) << "][" << std::max(1, num_dimensions) << "];"
                    ;

                // Index for the dimension array
                ss.str(""); ss << "nanos_dyn_dim_idx_" << (int)dep_dim_num;
                dep_dim_num++;

                TL::Symbol dyn_dim_idx = sc.new_symbol(ss.str());
                dyn_dim_idx.get_internal_symbol()->kind = SK_VARIABLE;
                dyn_dim_idx.get_internal_symbol()->type_information = get_signed_int_type();
                symbol_entity_specs_set_is_user_declared(dyn_dim_idx.get_internal_symbol(), 1);

                if (IS_CXX_LANGUAGE)
                {
                    Nodecl::NodeclBase def = Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
                            dyn_dim_idx,
                            ctr.get_locus());
                    ctr.prepend_sibling(def);
                }

                result_src
                    << as_symbol(dyn_dim_idx) << "= 0;"
                    ;

                Nodecl::Utils::SimpleSymbolMap symbol_map;

                for (ObjectList<DataReference::MultiRefIterator>::iterator current_multidep = m.begin();
                        current_multidep != m.end();
                        current_multidep++)
                {
                    // Create the current induction variable
                    ss.str(""); ss << "nanos_dyn_dep_" << (int)dep_dim_num;
                    TL::Symbol new_sym = sc.new_symbol(ss.str() + "_" + current_multidep->first.get_name());
                    new_sym.get_internal_symbol()->kind = SK_VARIABLE;
                    new_sym.get_internal_symbol()->type_information = get_signed_int_type();
                    symbol_entity_specs_set_is_user_declared(new_sym.get_internal_symbol(), 1);

                    if (IS_CXX_LANGUAGE)
                    {
                        Nodecl::NodeclBase def = Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
                                new_sym,
                                ctr.get_locus());
                        // FIXME - Check this
                        ctr.prepend_sibling(def);
                    }

                    symbol_map.add_map(current_multidep->first, new_sym);

                    Nodecl::Range range = current_multidep->second.as<Nodecl::Range>();
                    ERROR_CONDITION(!range.is<Nodecl::Range>(), "Invalid node %s", ast_print_node_type(range.get_kind()));

                    Nodecl::NodeclBase lower = range.get_lower().shallow_copy();
                    Nodecl::NodeclBase upper = range.get_upper().shallow_copy();
                    Nodecl::NodeclBase stride = range.get_stride().shallow_copy();

                    result_src
                        << "for ("
                        <<       as_symbol(new_sym) << "=" << as_expression(lower) << ";"
                        <<       as_symbol(new_sym) << "<=" << as_expression(upper) << ";"
                        <<       as_symbol(new_sym) << "+=" << as_expression(stride) << ")"
                        << "{"
                        ;


                    if (current_multidep + 1 == m.end())
                    {
                        // If this is the last iterator, map the dependence and
                        // generate the loop body
                        Nodecl::NodeclBase orig_dep = current_multidep->second;

                        // Now ignore the multidependence as such...
                        Nodecl::NodeclBase current_dep = dep_expr;
                        while (current_dep.is<Nodecl::MultiExpression>())
                        {
                            current_dep =
                                current_dep.as<Nodecl::MultiExpression>().get_base();
                        }

                        // and update it
                        Nodecl::NodeclBase updated_dep = Nodecl::Utils::deep_copy(
                                current_dep, sc, symbol_map);

                        Source current_dep_num;
                        current_dep_num << as_symbol(dyn_dep_idx);

                        Source current_dimension_array;
                        current_dimension_array << dimension_array << "[" << as_symbol(dyn_dim_idx) << "]";

                        Source current_dep_src;
                        handle_dependency_item(ctr, updated_dep, dir,
                                current_dimension_array,
                                current_dep_num, current_dep_src);

                        result_src << current_dep_src;
                        result_src << as_symbol(dyn_dep_idx) << "++;";
                        result_src << as_symbol(dyn_dim_idx) << "++;";

                        for (ObjectList<DataReference::MultiRefIterator>::reverse_iterator
                                rev_current_multidep = m.rbegin();
                                rev_current_multidep != m.rend();
                                rev_current_multidep++)
                        {
                            result_src
                                << "}";
                        }
                    }
                }
            }
        }
    }
}

void LoweringVisitor::compute_array_info(
        Nodecl::NodeclBase ctr,
        TL::DataReference array_expr,
        TL::Type array_type,
        // Out
        TL::Type& base_type,
        TL::ObjectList<Nodecl::NodeclBase>& lower_bounds,
        TL::ObjectList<Nodecl::NodeclBase>& upper_bounds,
        TL::ObjectList<Nodecl::NodeclBase>& dims_sizes)
{
    ERROR_CONDITION(!array_type.is_array(), "Unexpected type", 0);

    TL::Type t = array_type;
    int fortran_rank = array_type.fortran_rank();

    while (t.is_array())
    {
        Nodecl::NodeclBase array_lb, array_ub;
        Nodecl::NodeclBase region_lb, region_ub;
        Nodecl::NodeclBase dim_size;

        dim_size = t.array_get_size();
        t.array_get_bounds(array_lb, array_ub);
        if (t.array_is_region())
        {
            t.array_get_region_bounds(region_lb, region_ub);
        }

        if (IS_FORTRAN_LANGUAGE
                && t.is_fortran_array())
        {
            if (array_lb.is_null())
            {
                array_lb = get_lower_bound(array_expr, fortran_rank);
            }
            if (array_ub.is_null())
            {
                array_ub = get_upper_bound(array_expr, fortran_rank);
            }
            if (dim_size.is_null())
            {
                dim_size = get_size_for_dimension(t, fortran_rank, array_expr);
            }
        }

        // The region is the whole array
        if (region_lb.is_null())
            region_lb = array_lb;
        if (region_ub.is_null())
            region_ub = array_ub;

        // Adjust bounds to be 0-based
        Nodecl::NodeclBase adjusted_region_lb =
            (Source() << "(" << as_expression(region_lb) << ") - (" << as_expression(array_lb) << ")").
            parse_expression(ctr);
        Nodecl::NodeclBase adjusted_region_ub =
            (Source() << "(" << as_expression(region_ub) << ") - (" << as_expression(array_lb) << ")").
            parse_expression(ctr);

        lower_bounds.append(adjusted_region_lb);
        upper_bounds.append(adjusted_region_ub);
        dims_sizes.append(dim_size);

        t = t.array_element();

        fortran_rank--;
    }
    base_type = t;
}

Nodecl::NodeclBase LoweringVisitor::get_size_for_dimension(
        TL::Type array_type,
        int fortran_dimension,
        DataReference data_reference)
{
    Nodecl::NodeclBase n = array_type.array_get_size();

    // Let's try to get the size using SIZE
    if (n.is_null()
            && IS_FORTRAN_LANGUAGE)
    {
        // Craft a SIZE if possible
        TL::Symbol sym = data_reference.get_base_symbol();

        if (sym.is_parameter()
                && sym.get_type().no_ref().is_array()
                && !sym.get_type().no_ref().array_requires_descriptor()
                && fortran_dimension == fortran_get_rank_of_type(array_type.no_ref().get_internal_type()))
        {
            Nodecl::NodeclBase expr = data_reference;
            if (expr.is<Nodecl::ArraySubscript>())
            {
                expr = expr.as<Nodecl::ArraySubscript>().get_subscripts();

                expr = expr.as<Nodecl::List>()[0];

                if (expr.is<Nodecl::Range>())
                {
                    // Use the subscript
                    Source src;
                    Nodecl::NodeclBase lower =  expr.as<Nodecl::Range>().get_lower().shallow_copy();
                    if (lower.is_null())
                    {
                        lower = const_value_to_nodecl(const_value_get_signed_int(1));
                    }
                    src << "(" << as_expression(expr.as<Nodecl::Range>().get_upper().shallow_copy()) << ")"
                        << " - "
                        << "(" << as_expression(lower) << ")"
                        << " + 1";
                    n = src.parse_expression(Scope(CURRENT_COMPILED_FILE->global_decl_context));
                }
                else if (fortran_dimension != 1)
                {
                    n = const_value_to_nodecl(const_value_get_signed_int(1));
                }
            }
            else
            {
                internal_error("Assumed size array is not fully specified", 0);
            }
        }
        else
        {
            Source src;

            Nodecl::NodeclBase expr = data_reference;
            if (expr.is<Nodecl::ArraySubscript>())
            {
                expr = expr.as<Nodecl::ArraySubscript>().get_subscripted();
            }

            src << "SIZE(" << as_expression(expr.shallow_copy()) << ", " << fortran_dimension << ")";

            n = src.parse_expression(Scope(CURRENT_COMPILED_FILE->global_decl_context));
        }
    }

    return n;
}

Nodecl::NodeclBase LoweringVisitor::get_lower_bound(Nodecl::NodeclBase dep_expr, int dimension_num)
{
    Source src;
    Nodecl::NodeclBase expr = dep_expr;
    if (dep_expr.is<Nodecl::ArraySubscript>())
    {
        dep_expr = dep_expr.as<Nodecl::ArraySubscript>().get_subscripted();
    }

    src << "LBOUND(" << as_expression(dep_expr) << ", " << dimension_num << ")";

    return src.parse_expression(Scope(CURRENT_COMPILED_FILE->global_decl_context));
}

Nodecl::NodeclBase LoweringVisitor::get_upper_bound(Nodecl::NodeclBase dep_expr, int dimension_num)
{
    Source src;
    Nodecl::NodeclBase expr = dep_expr;
    if (dep_expr.is<Nodecl::ArraySubscript>())
    {
        dep_expr = dep_expr.as<Nodecl::ArraySubscript>().get_subscripted();
    }

    src << "UBOUND(" << as_expression(dep_expr) << ", " << dimension_num << ")";

    return src.parse_expression(Scope(CURRENT_COMPILED_FILE->global_decl_context));
}

void LoweringVisitor::remove_fun_tasks_from_source_as_possible(const OutlineInfo::implementation_table_t& implementation_table)
{
    DeviceHandler device_handler = DeviceHandler::get_device_handler();
    for (OutlineInfo::implementation_table_t::const_iterator it = implementation_table.begin();
            it != implementation_table.end();
            ++it)
    {
        bool remove_function_code = true;
        TL::Symbol implementor = it->first;
        ObjectList<std::string> devices = it->second.get_device_names();
        for (ObjectList<std::string>::iterator it2 = devices.begin();
                it2 != devices.end() && remove_function_code;
                ++it2)
        {
            DeviceProvider* device = device_handler.get_device(*it2);
            remove_function_code = device->remove_function_task_from_original_source();
        }

        if (remove_function_code
                && !implementor.get_function_code().is_null())
        {
            Nodecl::Utils::remove_from_enclosing_list(implementor.get_function_code());
        }
    }
}

} }
