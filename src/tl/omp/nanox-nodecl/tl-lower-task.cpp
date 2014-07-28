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
        new_class_symbol.get_internal_symbol()->entity_specs.is_user_declared = 1;

        type_t* new_class_type = get_new_class_type(sc.get_decl_context(), TT_STRUCT);
        decl_context_t class_context = new_class_context(sc.get_decl_context(), new_class_symbol.get_internal_symbol());
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
            field.get_internal_symbol()->entity_specs.is_user_declared = 1;

            field.get_internal_symbol()->entity_specs.is_member = 1;
            field.get_internal_symbol()->entity_specs.class_type = ::get_user_defined_type(new_class_symbol.get_internal_symbol());
            field.get_internal_symbol()->entity_specs.access = AS_PUBLIC;

            field.get_internal_symbol()->locus = make_locus("", 0, 0);

            field.get_internal_symbol()->type_information = ::get_user_defined_type(base_class.get_internal_symbol());
            class_type_add_member(new_class_type, field.get_internal_symbol(), /* is_definition */ 1);
        }

        {
            // Devices field
            TL::Symbol devices_class = sc.get_symbol_from_name("nanos_device_t");
            ERROR_CONDITION(!devices_class.is_valid(), "Invalid symbol", 0);

            TL::Symbol field = class_scope.new_symbol("devices");
            field.get_internal_symbol()->kind = SK_VARIABLE;
            field.get_internal_symbol()->entity_specs.is_user_declared = 1;

            field.get_internal_symbol()->entity_specs.is_member = 1;
            field.get_internal_symbol()->entity_specs.class_type = ::get_user_defined_type(new_class_symbol.get_internal_symbol());


            field.get_internal_symbol()->entity_specs.access = AS_PUBLIC;

            field.get_internal_symbol()->locus = make_locus("", 0, 0);

            field.get_internal_symbol()->type_information = 
                ::get_array_type(
                        ::get_user_defined_type(devices_class.get_internal_symbol()),
                        const_value_to_nodecl( const_value_get_signed_int(num_implementations)),
                        class_scope.get_decl_context());

            class_type_add_member(new_class_type, field.get_internal_symbol(), /* is_definition */ 1);
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
    int num_copies = count_copies(outline_info);
    int num_copies_dimensions = count_copies_dimensions(outline_info);
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
           opt_fortran_dynamic_init;

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
        // We do not register copies at creation in Fortran
        << /* ".num_copies = " << */ (!IS_FORTRAN_LANGUAGE ? num_copies : 0) << ",\n"
        << /* ".num_devices = " << */ num_implementations << ",\n"
        ;

    if (Nanos::Version::interface_is_at_least("copies_api", 1000))
    {
        result
            << /* ".num_dimensions = " */ (!IS_FORTRAN_LANGUAGE ? num_copies_dimensions : 0) << ",\n"
            ;
    }

    if (Nanos::Version::interface_is_at_least("master", 5022))
    {
        if (_lowering->instrumentation_enabled()
                && (IS_C_LANGUAGE || IS_CXX_LANGUAGE))
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
    }

    result
        << "}, "
        << /* ".devices = " << */ "{" << device_descriptions << "}"
        << "};"
        << opt_fortran_dynamic_init
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
            opt_fortran_dynamic_init << aux_fortran_init;
        }
    }

    if (IS_FORTRAN_LANGUAGE &&
            Nanos::Version::interface_is_at_least("master", 5022))
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
           dependences_info;

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
    err_name << "err";

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

    Source num_dependences;
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
        <<     if_condition_begin_opt
        <<     err_name << " = nanos_create_wd_compact(&nanos_wd_, &(nanos_wd_const_data.base), &nanos_wd_dyn_props, "
        <<                 struct_size << ", (void**)&ol_args, nanos_current_wd(),"
        <<                 copy_ol_arg << ");"
        <<     "if (" << err_name << " != NANOS_OK) nanos_handle_error (" << err_name << ");"
        <<     if_condition_end_opt
        <<     update_alloca_decls_opt
        <<     placeholder_task_expression_opt
        <<     dependences_info
        <<     "if (nanos_wd_ != (nanos_wd_t)0)"
        <<     "{"
                  // This is a placeholder because arguments are filled using the base language (possibly Fortran)
        <<        statement_placeholder(fill_outline_arguments_tree)
        <<        copy_ol_setup
        <<        err_name << " = nanos_submit(nanos_wd_, " << num_dependences << ", dependences, (nanos_team_t)0);"
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
        <<                  num_dependences << ", dependences, "
        <<                  copy_imm_arg << ", "
        <<                  translation_function << ");"
        <<          "if (" << err_name << " != NANOS_OK) nanos_handle_error (" << err_name << ");"
        <<     "}"
        << "}"
        ;

    // Fill arguments
    fill_arguments(construct, outline_info, fill_outline_arguments, fill_immediate_arguments);

    // Fill dependences for outline
    num_dependences << count_dependences(outline_info);

    int num_copies = 0;
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

    if (num_copies == 0)
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

    fill_dependences(construct, outline_info, dependences_info);

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

    // This copied_statements will be used when we are generating the code for the 'final' clause
    Nodecl::NodeclBase copied_statements = Nodecl::Utils::deep_copy(statements, construct);

    walk(statements);

    TaskEnvironmentVisitor task_environment;
    task_environment.walk(environment);

    Scope  enclosing_scope = construct.retrieve_context();
    Symbol function_symbol = Nodecl::Utils::get_enclosing_function(construct);

    OutlineInfo outline_info(environment,function_symbol);

    // Handle the special object 'this'
    if (IS_CXX_LANGUAGE
            && !function_symbol.is_static()
            && function_symbol.is_member())
    {
        TL::Symbol this_symbol = enclosing_scope.get_symbol_from_name("this");
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
    if (!_lowering->final_clause_transformation_disabled()
            && Nanos::Version::interface_is_at_least("master", 5024)
            && outline_info.only_has_smp_or_mpi_implementations()
            && !inside_task_expression)
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

        // We need to replace the placeholder before transforming the OpenMP/OmpSs pragmas
        copied_statements_placeholder.replace(copied_statements);

        ERROR_CONDITION(!copied_statements_placeholder.is_in_list(), "Unreachable code\n", 0);

        // Remove the OmpSs/OpenMP task stuff from the tree
        RemoveOpenMPTaskStuff visitor;
        visitor.walk(final_stmt_list);

        // Walk over the tree transforming OpenMP/OmpSs pragmas
        walk(final_stmt_list);
    }
    else
    {
        new_construct = construct;
    }

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
            task_environment.is_untied,

            outline_info,
            /* parameter_outline_info */ NULL,
            placeholder_task_expr_transformation);
}

void LoweringVisitor::fill_arguments(
        Nodecl::NodeclBase ctr,
        OutlineInfo& outline_info,
        // out
        Source& fill_outline_arguments,
        Source& fill_immediate_arguments
        )
{
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

                 case OutlineDataItem::SHARING_SHARED_WITH_CAPTURE:
                    {
                        fill_outline_arguments
                            << "ol_args->" << (*it)->get_field_name() << " = " << as_symbol((*it)->get_symbol()) << ";"
                            ;

                        fill_immediate_arguments
                            << "imm_args." << (*it)->get_field_name() << " = " << as_symbol((*it)->get_symbol()) << ";"
                            ;
                        break;
                    }
                case OutlineDataItem::SHARING_SHARED:
                case OutlineDataItem::SHARING_REDUCTION: // Reductions are passed as if they were shared
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
                            if (t.is_pointer())
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
                            if (t.is_pointer())
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
                case OutlineDataItem::SHARING_REDUCTION: // Reductions are passed as if they were shared variables
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
                            std::cerr
                                << (*it)->get_base_address_expression().get_locus()
                                << ": warning: an argument is not a valid data-reference, compilation is likely to fail"
                                << std::endl;
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

int LoweringVisitor::count_dependences(OutlineInfo& outline_info)
{
    int num_deps = 0;

    TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        num_deps += (*it)->get_dependences().size();
    }

    return num_deps;
}

int LoweringVisitor::count_copies(OutlineInfo& outline_info)
{
    int num_copies = 0;

    TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        num_copies += (*it)->get_copies().size();
    }

    return num_copies;
}

int LoweringVisitor::count_copies_dimensions(OutlineInfo& outline_info)
{
    int num_copies_dimensions = 0;

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
            TL::DataReference data_ref(copy_it->expression);

            num_copies_dimensions += std::max(1, data_ref.get_data_type().get_num_dimensions());
        }
    }

    return num_copies_dimensions;
}

void LoweringVisitor::fill_copies_nonregion(
        Nodecl::NodeclBase ctr,
        OutlineInfo& outline_info,
        int num_copies,
        // Source arguments_accessor,
        // out
        Source& copy_ol_decl,
        Source& copy_ol_arg,
        Source& copy_ol_setup,
        Source& copy_imm_arg,
        Source& copy_imm_setup)
{
    if (IS_C_LANGUAGE
            || IS_CXX_LANGUAGE)
    {
        copy_ol_arg << "&ol_copy_data";
        copy_imm_arg << "imm_copy_data";

        copy_ol_decl
            << "nanos_copy_data_t *ol_copy_data = (nanos_copy_data_t*)0;"
            ;
        copy_imm_setup 
            << "nanos_copy_data_t imm_copy_data[" << num_copies << "];";
    }
    else if (IS_FORTRAN_LANGUAGE)
    {
        copy_ol_arg << "(nanos_copy_data_t**)0";
        copy_imm_arg << "imm_copy_data";

        copy_ol_decl
            << "nanos_copy_data_t ol_copy_data[" << num_copies << "];";
            ;
        copy_imm_setup
            << "nanos_copy_data_t imm_copy_data[" << num_copies << "];";
    }

    // typedef struct {
    //    uint64_t address;
    //    nanos_sharing_t sharing;
    //    struct {
    //       bool input: 1;
    //       bool output: 1;
    //    } flags;
    //    size_t size;
    // } nanos_copy_data_internal_t;

    TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();

    int current_copy_num = 0;
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
            TL::DataReference data_ref((*copy_it).expression);
            OutlineDataItem::CopyDirectionality dir = (*copy_it).directionality;


            int input = (dir & OutlineDataItem::COPY_IN) == OutlineDataItem::COPY_IN;
            int output = (dir & OutlineDataItem::COPY_OUT) == OutlineDataItem::COPY_OUT;

            copy_ol_setup
                << "ol_copy_data[" << current_copy_num << "].sharing = NANOS_SHARED;"
                << "ol_copy_data[" << current_copy_num << "].address = (uint64_t)" << as_expression(data_ref.get_base_address()) << ";"
                << "ol_copy_data[" << current_copy_num << "].size = " << as_expression(data_ref.get_sizeof()) << ";"
                << "ol_copy_data[" << current_copy_num << "].flags.input = " << input << ";"
                << "ol_copy_data[" << current_copy_num << "].flags.output = " << output << ";"
                ;
            copy_imm_setup
                << "imm_copy_data[" << current_copy_num << "].sharing = NANOS_SHARED;"
                << "imm_copy_data[" << current_copy_num << "].address = (uint64_t)" << as_expression(data_ref.get_base_address()) << ";"
                << "imm_copy_data[" << current_copy_num << "].size = " << as_expression(data_ref.get_sizeof()) << ";"
                << "imm_copy_data[" << current_copy_num << "].flags.input = " << input << ";"
                << "imm_copy_data[" << current_copy_num << "].flags.output = " << output << ";"
                ;
            current_copy_num++;
        }
    }

    if (IS_FORTRAN_LANGUAGE)
    {
        copy_ol_setup
            << "{"
            << "nanos_err_t err;"
            << "err = nanos_set_copies(nanos_wd_, " << num_copies << ", ol_copy_data);"
            << "if (err != NANOS_OK) nanos_handle_error(err);"
            << "}"
            ;
    }
}

void LoweringVisitor::fill_copies_region(
        Nodecl::NodeclBase ctr,
        OutlineInfo& outline_info,
        int num_copies,
        int num_copies_dimensions,
        // Source arguments_accessor,
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
            << "nanos_region_dimension_internal_t imm_copy_dimensions[" << num_copies_dimensions << "];"
            ;
    }
    else if (IS_FORTRAN_LANGUAGE)
    {
        copy_ol_decl
            << "nanos_copy_data_t ol_copy_data[" << num_copies << "];"
            << "nanos_region_dimension_internal_t ol_copy_dimensions[" << num_copies_dimensions << "];"
            ;
        copy_ol_arg << "(nanos_copy_data_t**)0, (nanos_region_dimension_internal_t**)0";
        copy_imm_arg << "imm_copy_data, imm_copy_dimensions";
        copy_imm_setup
            << "nanos_copy_data_t imm_copy_data[" << num_copies << "];"
            << "nanos_region_dimension_internal_t imm_copy_dimensions[" << num_copies_dimensions << "];"
            ;
    }

    TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();

    int current_dimension_descriptor = 0;
    int i = 0;
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        TL::ObjectList<OutlineDataItem::CopyItem> copies = (*it)->get_copies();

        if (copies.empty())
            continue;

        for (TL::ObjectList<OutlineDataItem::CopyItem>::iterator copy_it = copies.begin();
                copy_it != copies.end();
                copy_it++, i++)
        {
            TL::DataReference data_ref(copy_it->expression);
            OutlineDataItem::CopyDirectionality dir = copy_it->directionality;

            Nodecl::NodeclBase address_of_object = data_ref.get_address_of_symbol();

            int input = (dir & OutlineDataItem::COPY_IN) == OutlineDataItem::COPY_IN;
            int output = (dir & OutlineDataItem::COPY_OUT) == OutlineDataItem::COPY_OUT;

            Source num_dimensions, dimension_descriptor_name, ol_dimension_descriptors, imm_dimension_descriptors, copy_offset;

            copy_ol_setup
                << ol_dimension_descriptors
                << "ol_copy_data[" << i << "].sharing = NANOS_SHARED;"
                << "ol_copy_data[" << i << "].address = (void*)" << as_expression(address_of_object) << ";"
                << "ol_copy_data[" << i << "].flags.input = " << input << ";"
                << "ol_copy_data[" << i << "].flags.output = " << output << ";"
                << "ol_copy_data[" << i << "].dimension_count = " << num_dimensions << ";"
                << "ol_copy_data[" << i << "].dimensions = &(ol_copy_dimensions[" << current_dimension_descriptor << "]);"
                << "ol_copy_data[" << i << "].offset = " << copy_offset << ";"
                ;

            copy_imm_setup
                << imm_dimension_descriptors
                << "imm_copy_data[" << i << "].sharing = NANOS_SHARED;"
                << "imm_copy_data[" << i << "].address = (void*)" << as_expression(address_of_object) << ";"
                << "imm_copy_data[" << i << "].flags.input = " << input << ";"
                << "imm_copy_data[" << i << "].flags.output = " << output << ";"
                << "imm_copy_data[" << i << "].dimension_count = " << num_dimensions << ";"
                << "imm_copy_data[" << i << "].dimensions = &(imm_copy_dimensions[" << current_dimension_descriptor << "]);"
                << "imm_copy_data[" << i << "].offset = " << copy_offset << ";"
                ;

            copy_offset << as_expression(data_ref.get_offsetof(data_ref, ctr.retrieve_context()));

            TL::Type copy_type = data_ref.get_data_type();
            TL::Type base_type = copy_type;

            ObjectList<Nodecl::NodeclBase> lower_bounds, upper_bounds, total_sizes;

            int num_dimensions_count = copy_type.get_num_dimensions();
            if (num_dimensions_count == 0)
            {
                lower_bounds.append(const_value_to_nodecl(const_value_get_signed_int(0)));
                upper_bounds.append(const_value_to_nodecl(const_value_get_signed_int(0)));
                total_sizes.append(const_value_to_nodecl(const_value_get_signed_int(1)));
                num_dimensions_count++;
            }
            else
            {
                TL::Type t = copy_type;
                int rank = copy_type.fortran_rank();

                while (t.is_array())
                {
                    Nodecl::NodeclBase array_lb, array_ub;
                    Nodecl::NodeclBase region_lb, region_ub;
                    Nodecl::NodeclBase region_size;
                    if (t.array_is_region())
                    {
                        t.array_get_bounds(array_lb, array_ub);
                        t.array_get_region_bounds(region_lb, region_ub);
                        region_size = t.array_get_size();
                    }
                    else
                    {
                        t.array_get_bounds(array_lb, array_ub);
                        region_size = t.array_get_size();
                    }

                    if (IS_FORTRAN_LANGUAGE
                            && t.is_fortran_array())
                    {
                        if (array_lb.is_null())
                        {
                            array_lb = get_lower_bound(data_ref, rank);
                        }
                        if (array_ub.is_null())
                        {
                            array_ub = get_upper_bound(data_ref, rank);
                        }
                        if (region_size.is_null())
                        {
                            region_size = get_size_for_dimension(t, rank, data_ref);
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
                    total_sizes.append(region_size);

                    t = t.array_element();

                    rank--;
                }

                base_type = t;

                // Sanity check
                ERROR_CONDITION(num_dimensions_count != (signed)lower_bounds.size()
                        || num_dimensions_count != (signed)upper_bounds.size()
                        || num_dimensions_count != (signed)total_sizes.size(),
                        "Mismatch between dimensions", 0);

            }

            num_dimensions
                << num_dimensions_count;

            for (int dim = num_dimensions_count - 1; dim >= 0; dim--, current_dimension_descriptor++)
            {
                // Sanity check
                ERROR_CONDITION(current_dimension_descriptor >= num_copies_dimensions, "Wrong number of dimensions %d >= %d",
                        current_dimension_descriptor, num_copies_dimensions);

                if (dim == num_dimensions_count - 1)
                {
                    // In bytes
                    ol_dimension_descriptors
                        << "ol_copy_dimensions[" << current_dimension_descriptor  << "].size = "
                        << "(" << as_expression(total_sizes[dim].shallow_copy()) << ") * sizeof(" << as_type(base_type) << ");"
                        <<  "ol_copy_dimensions[" << current_dimension_descriptor  << "].lower_bound = "
                        << "(" << as_expression(lower_bounds[dim].shallow_copy()) << ") * sizeof(" << as_type(base_type) << ");"
                        <<  "ol_copy_dimensions[" << current_dimension_descriptor  << "].accessed_length = "
                        << "((" << as_expression(upper_bounds[dim].shallow_copy()) << ") - ("
                        << as_expression(lower_bounds[dim].shallow_copy()) << ") + 1) * sizeof(" << as_type(base_type) << ");"
                        ;
                    imm_dimension_descriptors
                        << "imm_copy_dimensions[" << current_dimension_descriptor  << "].size = "
                        << "(" << as_expression(total_sizes[dim].shallow_copy()) << ") * sizeof(" << as_type(base_type) << ");"
                        <<  "imm_copy_dimensions[" << current_dimension_descriptor  << "].lower_bound = "
                        << "(" << as_expression(lower_bounds[dim].shallow_copy()) << ") * sizeof(" << as_type(base_type) << ");"
                        <<  "imm_copy_dimensions[" << current_dimension_descriptor  << "].accessed_length = "
                        << "((" << as_expression(upper_bounds[dim].shallow_copy()) << ") - ("
                        << as_expression(lower_bounds[dim].shallow_copy()) << ") + 1) * sizeof(" << as_type(base_type) << ");"
                        ;
                }
                else
                {
                    // In elements
                    ol_dimension_descriptors
                        << "ol_copy_dimensions[" << current_dimension_descriptor  << "].size = "
                        << as_expression(total_sizes[dim].shallow_copy()) << ";"
                        << "ol_copy_dimensions[" << current_dimension_descriptor  << "].lower_bound = "
                        << as_expression(lower_bounds[dim].shallow_copy()) << ";"
                        << "ol_copy_dimensions[" << current_dimension_descriptor  << "].accessed_length = "
                        << "(" << as_expression(upper_bounds[dim].shallow_copy()) << ") - ("
                        << as_expression(lower_bounds[dim].shallow_copy()) << ") + 1;"
                        ;
                    imm_dimension_descriptors
                        << "imm_copy_dimensions[" << current_dimension_descriptor  << "].size = "
                        << as_expression(total_sizes[dim].shallow_copy()) << ";"
                        << "imm_copy_dimensions[" << current_dimension_descriptor  << "].lower_bound = "
                        << as_expression(lower_bounds[dim].shallow_copy()) << ";"
                        << "imm_copy_dimensions[" << current_dimension_descriptor  << "].accessed_length = "
                        << "(" << as_expression(upper_bounds[dim].shallow_copy()) << ") - ("
                        << as_expression(lower_bounds[dim].shallow_copy()) << ") + 1;"
                        ;
                }
            }

//            if (Nanos::Version::interface_is_at_least("copies_api", 1003))
//            {
//                bool has_serializer=false;
//                TL::Type ser_type = copy_type;
//                //If the object is a class, generate everything so nanox can it
//                if (IS_CXX_LANGUAGE && (ser_type.is_class() || ser_type.is_pointer_to_class())) {  
//                        TL::Symbol sym_serializer = ser_type.get_symbol();
//                        if (sym_serializer.get_type().is_pointer_to_class()){
//                            ser_type= sym_serializer.get_type().get_pointer_to();
//                            sym_serializer= sym_serializer.get_type().get_pointer_to().get_symbol();
//                        }
//                        ObjectList<TL::Symbol> ser_members = ser_type.get_all_members();
//                        ObjectList<TL::Symbol>::iterator ser_it;
//                        for (ser_it=ser_members.begin(); ser_it!=ser_members.end() && !has_serializer; ++ser_it){
//                            if (ser_it->get_name()=="serialize") has_serializer=true;
//                        }    
//                }
//                //If its serializable and input, create serialization adapters and fill copy info
//                //if it's not input, the device should warn the programmer in case it's needed
//                if (has_serializer && input) {
//                    TL::Symbol sym_serializer = ser_type.get_symbol();
//                    std::string serialize_prefix_name= outline_info.get_implementation_table().begin()->second.get_outline_name() + sym_serializer.get_name() + data_ref.get_base_symbol().get_name();
//
//                    Source serialize_adapters;
//
//                    serialize_adapters << "static size_t " << serialize_prefix_name << "_ser_size_adapter(void* this_)"
//                            << "{  "
//                            << "   return (("<< sym_serializer.get_qualified_name() << "*)this_)->serialize_size(); "
//                            << "} ;";
//                    serialize_adapters << "static void " << serialize_prefix_name << "_ser_adapter(void * this_, void* buff)"
//                            << "{ "
//                            << "   nanos::omemstream* buff_ptr=(nanos::omemstream*) buff; "
//                            << "   (("<< sym_serializer.get_qualified_name() << "*)this_)->serialize(*buff_ptr); "
//                            << "} ;";
//                    
//                    serialize_adapters << "static void " << serialize_prefix_name << "_ser_assign_adapter(void* this_, void* buff)"
//                            << "{"
//                            << "  nanos::imemstream* buff_ptr=(nanos::imemstream*) buff; "
//                            << "  (*("<< sym_serializer.get_qualified_name() << "*)this_)=(*buff_ptr);"
//                            << "} ;";
//
//                    copy_ol_setup
//                        << "ol_copy_data[" << i << "].serialize_size_adapter = (typeSerSizeAdapter)" << serialize_prefix_name << "_ser_size_adapter;"
//                        << "ol_copy_data[" << i << "].serialize_adapter = (typeSerAdapter)" << serialize_prefix_name << "_ser_adapter;"
//                        << "ol_copy_data[" << i << "].serialize_assign_adapter = (typeSerAssignAdapter)" << serialize_prefix_name << "_ser_assign_adapter;"
//                        ;
//
//                    copy_imm_setup
//                        << "imm_copy_data[" << i << "].serialize_size_adapter = (typeSerSizeAdapter)" << serialize_prefix_name << "_ser_size_adapter;"
//                        << "imm_copy_data[" << i << "].serialize_adapter = (typeSerAdapter)" << serialize_prefix_name << "_ser_adapter;"
//                        << "imm_copy_data[" << i << "].serialize_assign_adapter = (typeSerAssignAdapter)" << serialize_prefix_name << "_ser_assign_adapter;"
//                        ;
//
//                    Nodecl::NodeclBase serialize_tree = serialize_adapters.parse_global(ctr);
//                    Nodecl::Utils::prepend_to_enclosing_top_level_location(ctr,serialize_tree);
//                } else {
//                    copy_ol_setup
//                        << "ol_copy_data[" << i << "].serialize_size_adapter = 0;"
//                        << "ol_copy_data[" << i << "].serialize_adapter = 0;"
//                        << "ol_copy_data[" << i << "].serialize_assign_adapter = 0;"
//                        ;
//
//                    copy_imm_setup
//                        << "imm_copy_data[" << i << "].serialize_size_adapter = 0;"
//                        << "imm_copy_data[" << i << "].serialize_adapter = 0;"
//                        << "imm_copy_data[" << i << "].serialize_assign_adapter = 0;"
//                        ;
//                }
//            }
        }
    }
    
    if (IS_FORTRAN_LANGUAGE)
    {
        copy_ol_setup
            << "{"
            << "nanos_err_t err;"
            << "err = nanos_set_copies(nanos_wd_, " << num_copies << ", ol_copy_data);"
            << "if (err != NANOS_OK) nanos_handle_error(err);"
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
        int &num_copies,
        Source& copy_ol_decl,
        Source& copy_ol_arg,
        Source& copy_ol_setup,
        Source& copy_imm_arg,
        Source& copy_imm_setup,
        TL::Symbol& xlate_function_symbol
        )
{
    num_copies = count_copies(outline_info);


    if (Nanos::Version::interface_is_at_least("copies_api", 1000))
    {
        int num_copies_dimensions = count_copies_dimensions(outline_info);
        if (num_copies == 0)
        {
            copy_ol_arg << "(nanos_copy_data_t**)0, (nanos_region_dimension_internal_t**)0";
            copy_imm_arg << "(nanos_copy_data_t*)0, (nanos_region_dimension_internal_t*)0";
        }
        else
        {
            fill_copies_region(ctr,
                    outline_info,
                    num_copies,
                    num_copies_dimensions,
                    copy_ol_decl,
                    copy_ol_arg,
                    copy_ol_setup,
                    copy_imm_arg,
                    copy_imm_setup);

            if (bool allow_multiple_copies = Nanos::Version::interface_is_at_least("copies_api", 1002))
            {
                emit_translation_function_region(ctr,
                        outline_info,
                        parameter_outline_info,
                        structure_symbol,
                        xlate_function_symbol);
            }
            else
            {
                emit_translation_function_nonregion(ctr,
                        outline_info,
                        parameter_outline_info,
                        structure_symbol,
                        allow_multiple_copies,
                        xlate_function_symbol);
            }
        }
    }
    else
    {
        if (num_copies == 0)
        {
            copy_ol_arg << "(nanos_copy_data_t**)0";
            copy_imm_arg << "(nanos_copy_data_t*)0";
        }
        else
        {
            fill_copies_nonregion(ctr,
                    outline_info,
                    num_copies,
                    copy_ol_decl,
                    copy_ol_arg,
                    copy_ol_setup,
                    copy_imm_arg,
                    copy_imm_setup);

            emit_translation_function_nonregion(ctr,
                    outline_info,
                    parameter_outline_info,
                    structure_symbol,
                    /* allow_multiple_copies */ false,
                    xlate_function_symbol);
        }
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

void LoweringVisitor::emit_translation_function_nonregion(
        Nodecl::NodeclBase ctr,
        OutlineInfo& outline_info,
        OutlineInfo* parameter_outline_info,
        TL::Symbol structure_symbol,
        bool allow_multiple_copies,
        // Out
        TL::Symbol& translation_function_symbol
        )
{
    TL::Counter &fun_num = TL::CounterManager::get_counter("nanos++-translation-functions");
    Source fun_name;
    std::string filename = TL::CompilationProcess::get_current_file().get_filename();
    //Remove non-alphanumeric characters from the string
    filename.erase(std::remove_if(filename.begin(), filename.end(), (bool(*)(int))is_not_alnum), filename.end());
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

    TL::ObjectList<OutlineDataItem*> data_items;
    data_items = outline_info.get_fields();

    Source translations;

    Nodecl::Utils::SimpleSymbolMap symbol_map;

    // Initialize the rewrite visitor
    RewriteAddressExpression rewrite_base_address;
    TL::Symbol argument_structure_symbol = ReferenceScope(empty_statement).get_scope().get_symbol_from_name("arg");
    ERROR_CONDITION(!argument_structure_symbol.is_valid(), "Invalid symbol 'arg' just created!", 0);
    rewrite_base_address.structure = argument_structure_symbol;

    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end(); it++)
    {
        // Create a mapping "var" to "args->var"
        ERROR_CONDITION(!(*it)->get_field_symbol().is_valid(), "Invalid field symbol", 0);
        rewrite_base_address.sym_to_field[(*it)->get_symbol()] = (*it)->get_field_symbol();
    }

    int copy_num = 0;
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        TL::ObjectList<OutlineDataItem::CopyItem> copies = (*it)->get_copies();

        if (copies.empty())
            continue;

        if (!allow_multiple_copies
                && copies.size() > 1)
        {
            info_printf("%s: info: more than one copy specified for '%s' but the runtime does not support it. "
                    "Only the first copy (%s) will be translated\n",
                    ctr.get_locus_str().c_str(),
                    (*it)->get_symbol().get_name().c_str(),
                    copies[0].expression.prettyprint().c_str());
        }

        TL::DataReference data_ref(copies[0].expression);

        // if (IS_FORTRAN_LANGUAGE)
        // {
        //     base_address = data_ref.get_base_address_as_integer();
        // }
        // else
        // {
        //     base_address = data_ref.get_base_address().shallow_copy();
        // }

        // // rewrite
        // rewrite_base_address.walk(base_address);

        Nodecl::NodeclBase offset = data_ref.get_offsetof();
        rewrite_base_address.walk(offset);

        translations
            << "{"
            << "intptr_t device_base_address;"
            << "signed long offset;"
            << "nanos_err_t err;"
            << "intptr_t host_base_address;"

            << "host_base_address = (intptr_t)arg." << (*it)->get_field_name() << ";"
            << "offset = " << as_expression(offset) << ";"
            << "device_base_address = 0;"
            << "err = nanos_get_addr(" << copy_num << ", (void**)&device_base_address, wd);"
            << "device_base_address -= offset;"
            << "if (err != NANOS_OK) nanos_handle_error(err);"
            << "arg." << (*it)->get_field_name() << " = (" << as_type((*it)->get_field_type()) << ")device_base_address;"
            << "}"
            ;
        copy_num += copies.size();
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

    int copy_num = 0;
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        TL::ObjectList<OutlineDataItem::CopyItem> copies = (*it)->get_copies();

        if (copies.empty())
            continue;

        //ERROR_CONDITION((*it)->get_sharing() != OutlineDataItem::SHARING_SHARED, "Unexpected sharing\n", 0);

        translations
            << "{"
            << "void *device_base_address;"
            << "nanos_err_t err;"

            << "device_base_address = 0;"
            << "err = nanos_get_addr(" << copy_num << ", &device_base_address, wd);"
            << "if (err != NANOS_OK) nanos_handle_error(err);"
            ;

        if ((*it)->get_symbol().is_allocatable()
                || ((*it)->get_symbol().get_type().is_pointer()
                    && (*it)->get_symbol().get_type().points_to().is_array()
                    && (*it)->get_symbol().get_type().points_to().array_requires_descriptor()))
        {
            TL::Symbol new_function = get_function_modify_array_descriptor(
                    (*it)->get_field_name(),
                    (*it)->get_field_type(),
                    ctr.retrieve_context());

            ERROR_CONDITION((*it)->get_copy_of_array_descriptor() == NULL, "This needs a copy of the array descriptor", 0);

            translations
                //<<  new_function.get_name() << "(arg." << (*it)->get_field_name() << ", device_base_address);"
                <<  new_function.get_name() << "(arg." <<
                        (*it)->get_copy_of_array_descriptor()->get_field_name() << ", device_base_address);"
                << "}"
                ;
        }
        else
        {
            // Currently we do not support copies on non-shared stuff, so this should be always a pointer
            ERROR_CONDITION(!(*it)->get_field_type().is_pointer(), "Invalid type, expecting a pointer", 0);

            translations
                << "arg." << (*it)->get_field_name() << " = (" << as_type((*it)->get_field_type()) << ")device_base_address;"
                << "}"
                ;
        }

        copy_num += copies.size();
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
        // out
        Source& result_src)
{
    fill_dependences_internal(ctr, outline_info, /* on_wait */ false, result_src);
}

void LoweringVisitor::handle_dependency_item(
        Nodecl::NodeclBase ctr,
        TL::DataReference dep_expr,
        OutlineDataItem::DependencyDirectionality dir,
        int current_dep_num,
        Source& dependency_regions,
        Source& dependency_init,
        Source& result_src)

{
    ERROR_CONDITION(!dep_expr.is_valid(),
            "%s: Invalid dependency detected '%s'. Reason: %s\n",
            dep_expr.get_locus_str().c_str(),
            dep_expr.prettyprint().c_str(),
            dep_expr.get_error_log().c_str());

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

    dependency_flags
        << "{"
        << dependency_flags_in << ","
        << dependency_flags_out << ", "
        << /* renaming has not yet been implemented */ "0, "
        << dependency_flags_concurrent << ","
        << dependency_flags_commutative
        << "}"
        ;

    Type dependency_type = dep_expr.get_data_type();

    int num_dimensions = dependency_type.get_num_dimensions();

    bool input        = ((dir & OutlineDataItem::DEP_IN) == OutlineDataItem::DEP_IN);
    bool input_value  = ((dir & OutlineDataItem::DEP_IN_VALUE) == OutlineDataItem::DEP_IN_VALUE);
    bool input_alloca = ((dir & OutlineDataItem::DEP_IN_ALLOCA) == OutlineDataItem::DEP_IN_ALLOCA);
    bool input_private = ((dir & OutlineDataItem::DEP_IN_PRIVATE) == OutlineDataItem::DEP_IN_PRIVATE);
    bool concurrent   = ((dir & OutlineDataItem::DEP_CONCURRENT) == OutlineDataItem::DEP_CONCURRENT);
    bool commutative  = ((dir & OutlineDataItem::DEP_COMMUTATIVE) == OutlineDataItem::DEP_COMMUTATIVE);

    dependency_flags_in << ( input || input_value || input_alloca || input_private || concurrent || commutative);

    dependency_flags_out << (((dir & OutlineDataItem::DEP_OUT) == OutlineDataItem::DEP_OUT)
            || concurrent || commutative);
    dependency_flags_concurrent << concurrent;
    dependency_flags_commutative << commutative;
    //
    // Compute the base type of the dependency and the array containing the size of each dimension
    Type dependency_base_type = dependency_type;

    Nodecl::NodeclBase dimension_sizes[num_dimensions];
    for (int dim = 0; dim < num_dimensions; dim++)
    {
        dimension_sizes[dim] = get_size_for_dimension(dependency_base_type, num_dimensions - dim, dep_source_expr);

        dependency_base_type = dependency_base_type.array_element();
    }

    std::string base_type_name = dependency_base_type.get_declaration(dep_expr.retrieve_context(), "");

    dependency_regions << "nanos_region_dimension_t dimensions_" << current_dep_num << "[" << std::max(num_dimensions, 1) << "]"
        ;

    Source dims_description;

    if (IS_C_LANGUAGE
            || IS_CXX_LANGUAGE)
    {
        dependency_regions << "=  { " << dims_description << "}";
    }

    dependency_regions << ";"
        ;

    Nodecl::NodeclBase dep_expr_offset = dep_expr.get_offsetof();

    if (dep_expr_offset.is_null())
    {
        dep_expr_offset = dep_expr.get_offsetof(/* base symbol */ dep_source_expr, ctr.retrieve_context());
    }
    ERROR_CONDITION(dep_expr_offset.is_null(), "Failed to synthesize an expression denoting offset", 0);

    dependency_offset << as_expression(dep_expr_offset);

    if (num_dimensions == 0)
    {
        // This is a scalar
        Source dimension_size, dimension_lower_bound, dimension_accessed_length;

        dimension_size << "sizeof(" << base_type_name << ")";
        dimension_lower_bound << "0";
        dimension_accessed_length << dimension_size;

        if (IS_C_LANGUAGE
                || IS_CXX_LANGUAGE)
        {
            dims_description
                << "{"
                << dimension_size << ","
                << dimension_lower_bound << ","
                << dimension_accessed_length
                << "}"
                ;
        }
        else
        {
            dependency_regions
                << "dimensions_" << current_dep_num << "[0].size = " << dimension_size << ";"
                << "dimensions_" << current_dep_num << "[0].lower_bound = " << dimension_lower_bound << ";"
                << "dimensions_" << current_dep_num << "[0].accessed_length = " << dimension_accessed_length << ";"
                ;
        }

    }
    else
    {
        // This an array
        Source dimension_size, dimension_lower_bound, dimension_accessed_length;

        // Compute the contiguous array type
        Type contiguous_array_type = dependency_type;
        while (contiguous_array_type.array_element().is_array())
        {
            contiguous_array_type = contiguous_array_type.array_element();
        }

        Nodecl::NodeclBase lb, ub, size;
        if (contiguous_array_type.array_is_region())
        {
            // This should be the lower bound of the array region minus lower bound of the array
            Nodecl::NodeclBase array_lb, array_ub;
            Nodecl::NodeclBase region_lb, region_ub;

            contiguous_array_type.array_get_bounds(array_lb, array_ub);
            contiguous_array_type.array_get_region_bounds(region_lb, region_ub);

            if (array_lb.is_null()
                    && IS_FORTRAN_LANGUAGE)
            {
                // Compute a LBOUND on it. The contiguous dimension is always 1 in Fortran
                array_lb = get_lower_bound(dep_source_expr, /* dimension */ 1);
            }

            // Meaning that in this context A(:) is OK
            if (region_lb.is_null())
                region_lb = array_lb;

            // Adjust bounds to be 0-based
            lb = (Source() <<  "(" << as_expression(region_lb) << ") - (" << as_expression(array_lb) << ")")
                .parse_expression(ctr);

            size = contiguous_array_type.array_get_region_size();

            if (size.is_null())
                size = get_size_for_dimension(contiguous_array_type, 1, dep_source_expr);
        }
        else
        {
            // Lower bound here should be zero since we want all the array
            lb = const_value_to_nodecl(const_value_get_signed_int(0));

            size = get_size_for_dimension(contiguous_array_type, 1, dep_source_expr);
        }

        dimension_size << "sizeof(" << base_type_name << ") * " << as_expression(dimension_sizes[num_dimensions - 1]);
        dimension_lower_bound << "sizeof(" << base_type_name << ") * " << as_expression(lb);
        dimension_accessed_length << "sizeof(" << base_type_name << ") * " << as_expression(size);

        if (IS_C_LANGUAGE
                || IS_CXX_LANGUAGE)
        {
            dims_description
                << "{"
                << dimension_size << ","
                << dimension_lower_bound << ","
                << dimension_accessed_length
                << "}"
                ;
        }
        else
        {
            dependency_regions
                << "dimensions_" << current_dep_num << "[0].size = " << dimension_size << ";"
                << "dimensions_" << current_dep_num << "[0].lower_bound = " << dimension_lower_bound << ";"
                << "dimensions_" << current_dep_num << "[0].accessed_length = " << dimension_accessed_length << ";"
                ;
        }

        if (num_dimensions > 1)
        {
            // All the remaining dimensions (but 0) are filled here
            fill_dimensions(
                    num_dimensions,
                    /* current_dim */ num_dimensions,
                    current_dep_num,
                    dep_source_expr,
                    dimension_sizes,
                    dependency_type,
                    dims_description,
                    dependency_regions,
                    dep_source_expr.retrieve_context());
        }
    }

    int num_dimension_items = num_dimensions;
    if (num_dimension_items == 0)
        num_dimension_items = 1;

    if (IS_C_LANGUAGE
            || IS_CXX_LANGUAGE)
    {
        if (current_dep_num > 0)
        {
            dependency_init << ", ";
        }

        dependency_init
            << "{"
            << "(void *) " << as_expression(base_address) << ", "
            << dependency_flags << ", "
            << num_dimension_items << ", "
            << "dimensions_" << current_dep_num << ","
            << dependency_offset
            << "}";
    }
    else if (IS_FORTRAN_LANGUAGE)
    {
        result_src
            << "dependences[" << current_dep_num << "].address = "
            << as_expression(base_address) << ";"
            << "dependences[" << current_dep_num << "].offset = " << dependency_offset << ";"
            << "dependences[" << current_dep_num << "].flags.input = " << dependency_flags_in << ";"
            << "dependences[" << current_dep_num << "].flags.output = " << dependency_flags_out << ";"
            << "dependences[" << current_dep_num << "].flags.can_rename = 0;"
            << "dependences[" << current_dep_num << "].flags.concurrent = " << dependency_flags_concurrent << ";"
            << "dependences[" << current_dep_num << "].flags.commutative = " << dependency_flags_commutative << ";"
            << "dependences[" << current_dep_num << "].dimension_count = " << num_dimension_items << ";"
            << "dependences[" << current_dep_num << "].dimensions = &dimensions_" << current_dep_num << ";"
            ;
    }
}

void LoweringVisitor::fill_dependences_internal(
        Nodecl::NodeclBase ctr,
        OutlineInfo& outline_info,
        bool on_wait,
        // out
        Source& result_src)
{
    Source dependency_init;

    int num_deps = count_dependences(outline_info);

    TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();

    if (num_deps == 0)
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

    if (Nanos::Version::interface_is_at_least("deps_api", 1001))
    {
        Source dependency_regions;

        result_src
            << dependency_regions
            << "nanos_data_access_t dependences[" << num_deps << "]"
            ;

        if (IS_C_LANGUAGE
                || IS_CXX_LANGUAGE)
        {
            result_src << " = {"
                << dependency_init
                << "};"
                ;
        }
        result_src << ";"
            ;

        int current_dep_num = 0;
        for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            TL::ObjectList<OutlineDataItem::DependencyItem> deps = (*it)->get_dependences();

            if (deps.empty())
                continue;

            for (ObjectList<OutlineDataItem::DependencyItem>::iterator dep_it = deps.begin();
                    dep_it != deps.end();
                    dep_it++, current_dep_num++)
            {
                OutlineDataItem::DependencyDirectionality dir = dep_it->directionality;
                TL::DataReference dep_expr(dep_it->expression);

                handle_dependency_item(ctr, dep_expr, dir,
                        current_dep_num, dependency_regions, dependency_init, result_src);
            }
        }
    }
    else
    {
        running_error("%s: error: please update your runtime version. deps_api < 1001 not supported\n", ctr.get_locus_str().c_str());
    }
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

void LoweringVisitor::fill_dimensions(
        int n_dims,
        int current_dim,
        int current_dep_num,
        Nodecl::NodeclBase dep_expr,
        Nodecl::NodeclBase * dim_sizes,
        Type dep_type,
        Source& dims_description,
        Source& dependency_regions_code,
        Scope sc)
{
    // We do not handle the contiguous dimension here
    if (current_dim > 1)
    {
        fill_dimensions(n_dims, current_dim - 1, current_dep_num,
                dep_expr, dim_sizes,
                dep_type.array_element(), dims_description, dependency_regions_code, sc);

        Source dimension_size, dimension_lower_bound, dimension_accessed_length;
        Nodecl::NodeclBase array_lb, array_ub, size;
        Nodecl::NodeclBase region_lb, region_ub;

        if (dep_type.array_is_region())
        {
            dep_type.array_get_bounds(array_lb, array_ub);
            dep_type.array_get_region_bounds(region_lb, region_ub);
            size = dep_type.array_get_region_size();
        }
        else
        {
            dep_type.array_get_bounds(array_lb, array_ub);
            size = get_size_for_dimension(dep_type, current_dim, dep_expr);
        }

        if (array_lb.is_null() && IS_FORTRAN_LANGUAGE)
        {
            array_lb = get_lower_bound(dep_expr, current_dim);
        }

        // The region is the whole array
        if (region_lb.is_null())
            region_lb = array_lb;
        if (region_ub.is_null())
            region_ub = array_ub;

        // Adjust bounds to be 0-based
        Nodecl::NodeclBase adjusted_lb = 
            (Source() <<  "(" << as_expression(region_lb) << ") - (" << as_expression(array_lb) << ")")
            .parse_expression(sc);

        dimension_size << as_expression(dim_sizes[n_dims - current_dim]);
        dimension_lower_bound << as_expression(adjusted_lb);
        dimension_accessed_length << as_expression(size);

        if (IS_C_LANGUAGE
                || IS_CXX_LANGUAGE)
        {
            dims_description << ", {" 
                << dimension_size << ", " 
                << dimension_lower_bound << ", "
                << dimension_accessed_length 
                << "}"
                ;
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            dependency_regions_code
                << "dimensions_" << current_dep_num << "[" << current_dim - 1 << "].size = " << dimension_size << ";"
                << "dimensions_" << current_dep_num << "[" << current_dim - 1 << "].lower_bound = " << dimension_lower_bound << ";"
                << "dimensions_" << current_dep_num << "[" << current_dim - 1 << "].accessed_length = " << dimension_accessed_length << ";"
                ;
        }
    }
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
