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
#include "tl-nanos.hpp"
#include "tl-source.hpp"
#include "tl-counters.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-datareference.hpp"
#include "tl-devices.hpp"
#include "fortran03-typeutils.h"
#include "cxx-diagnostic.h"
#include "cxx-cexpr.h"

#include "tl-lower-task-common.hpp"

using TL::Source;

namespace TL { namespace Nanox {

struct TaskEnvironmentVisitor : public Nodecl::ExhaustiveVisitor<void>
{
    public:
        bool is_untied;
        Nodecl::NodeclBase priority;

        TaskEnvironmentVisitor()
            : is_untied(false),
            priority()
        {
        }

        void visit(const Nodecl::OpenMP::Priority& priority)
        {
            this->priority = priority.get_priority();
        }

        void visit(const Nodecl::OpenMP::Untied& untied)
        {
            this->is_untied = true;
        }
};

TL::Symbol LoweringVisitor::declare_const_wd_type(int num_devices, Nodecl::NodeclBase construct)
{
    //FIXME: The 'construct' parameter is only used to obtain the line and the filename
    std::map<int, Symbol>::iterator it = _declared_const_wd_type_map.find(num_devices);
    if (it == _declared_const_wd_type_map.end())
    {
        std::stringstream ss;
        if (IS_C_LANGUAGE)
        {
            ss << "struct ";
        }
        ss << "nanos_const_wd_definition_" << num_devices;

        TL::Scope sc(CURRENT_COMPILED_FILE->global_decl_context);
        TL::Symbol new_class_symbol = sc.new_symbol(ss.str());
        new_class_symbol.get_internal_symbol()->kind = SK_CLASS;
        new_class_symbol.get_internal_symbol()->entity_specs.is_user_declared = 1;

        type_t* new_class_type = get_new_class_type(sc.get_decl_context(), TT_STRUCT);
        decl_context_t class_context = new_class_context(sc.get_decl_context(), new_class_symbol.get_internal_symbol());
        TL::Scope class_scope(class_context);

        class_type_set_inner_context(new_class_type, class_context);

        new_class_symbol.get_internal_symbol()->type_information = new_class_type;

        _declared_const_wd_type_map[num_devices] = new_class_symbol;

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

            field.get_internal_symbol()->file = "";
            field.get_internal_symbol()->line = 0;

            field.get_internal_symbol()->type_information = ::get_user_defined_type(base_class.get_internal_symbol());
            class_type_add_member(new_class_type, field.get_internal_symbol());
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

            field.get_internal_symbol()->file = "";
            field.get_internal_symbol()->line = 0;

            field.get_internal_symbol()->type_information = 
                ::get_array_type(
                        ::get_user_defined_type(devices_class.get_internal_symbol()),
                        const_value_to_nodecl( const_value_get_signed_int(num_devices)),
                        class_scope.get_decl_context());

            class_type_add_member(new_class_type, field.get_internal_symbol());
        }

        nodecl_t nodecl_output = nodecl_null();
        finish_class_type(new_class_type, 
                ::get_user_defined_type(new_class_symbol.get_internal_symbol()),
                sc.get_decl_context(), 
                "", 0,
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
                    construct.get_filename(),
                    construct.get_line());

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
        OutlineInfo& outline_info,
        std::multimap<std::string, std::string>& devices_and_implementors,
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
    // } nanos_const_wd_definition_t;
    // MultiMap with every implementation of the current function task
    
    DeviceHandler device_handler = DeviceHandler::get_device_handler();
    int num_copies=/* num_copies */ count_copies(outline_info);
    int num_copies_dimensions=/* num_copies_dimensions */ count_copies_dimensions(outline_info);
    OutlineInfo::implementation_table_t implementation_table = outline_info.get_implementation_table();

    int num_devices = devices_and_implementors.size();
    TL::Symbol const_wd_type = declare_const_wd_type(num_devices, construct);

    Source alignment, props_init;

    Source ancillary_device_descriptions,
           device_descriptions,
           opt_fortran_dynamic_init;

    Source result;
    result
        << ancillary_device_descriptions
        << "static " << const_wd_type.get_name() << " nanos_wd_const_data = {"
        << "{"
        << /* ".props = " << */ props_init << ", \n"
        << /* ".data_alignment = " << */ alignment << ", \n"
        // We do not register copies at creation in Fortran
        << /* ".num_copies = " << */ (!IS_FORTRAN_LANGUAGE ? num_copies : 0) << ",\n"
        << /* ".num_devices = " << */ num_devices << ",\n"
        ;
    if (Nanos::Version::interface_is_at_least("copies_api", 1000))
    {
        result
            << /* ".num_dimensions = " */ num_copies_dimensions << ",\n"
            ;
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

    // We expand all the struct due to a limitation in the FE. See ticket
    // #963
    props_init
        << "{ "
        << /* ".mandatory_creation = " << */ (int)mandatory_creation << ",\n"
        << /* ".tied = " << */ tiedness << ",\n"
        << /* ".reserved0 =" << */ "0,\n"
        << /* ".reserved1 =" << */ "0,\n"
        << /* ".reserved2 =" << */ "0,\n"
        << /* ".reserved3 =" << */ "0,\n"
        << /* ".reserved4 =" << */ "0,\n"
        << /* ".reserved5 =" << */ "0,\n"
        << "}"
        ;

    tiedness << (int)!is_untied;

//    // For every device name specified in the 'device' clause, we should get
//    // its device descriptor
//    DeviceDescriptorInfo info(outline_name);
//    for (ObjectList<std::string>::const_iterator it = device_names.begin();
//            it != device_names.end();
//            ++it)
//    {
//        Source ancillary_device_description, device_description, aux_fortran_init;
//
//        if (it != device_names.begin())
//            device_descriptions <<  ", ";
//
//        std::string device_name = *it;
//        DeviceProvider* device = device_handler.get_device(device_name);
//        ERROR_CONDITION(device == NULL, " Device '%s' has not been loaded.", device_name.c_str());
//
//        device->get_device_descriptor(
//                info,
//                ancillary_device_description,
//                device_description,
//                aux_fortran_init);
//
//        device_descriptions << device_description;
//        ancillary_device_descriptions << ancillary_device_description;
//        opt_fortran_dynamic_init << aux_fortran_init;
//    }

    // For every existant implementation (including the one which defines the task),
    // we should get its device descriptor information.
    // Note that in this case we use the implementor outline name as outline name    
    OutlineInfo::implementation_table_t::iterator implementation_table_it = implementation_table.begin();
    std::multimap<std::string, std::string>::iterator devices_and_implementors_it = devices_and_implementors.begin();
    int n_devices=implementation_table_it->second.get_device_names().size();
    while (devices_and_implementors_it != devices_and_implementors.end())
    {
        if (n_devices<1){
                implementation_table_it++;
                n_devices=implementation_table_it->second.get_device_names().size();
        }
        Source ancillary_device_description, device_description, aux_fortran_init;

        if (devices_and_implementors_it!=devices_and_implementors.begin()) device_descriptions <<  ", ";

        std::string device_name = devices_and_implementors_it->first;
        std::string implementor_outline_name = devices_and_implementors_it->second;

        DeviceProvider* device = device_handler.get_device(device_name);
        ERROR_CONDITION(device == NULL, " Device '%s' has not been loaded.", device_name.c_str());

        DeviceDescriptorInfo info_implementor(implementor_outline_name,implementation_table_it->second);
        device->get_device_descriptor(
                info_implementor,
                ancillary_device_description,
                device_description,
                aux_fortran_init);

        device_descriptions << device_description;
        ancillary_device_descriptions << ancillary_device_description;
        opt_fortran_dynamic_init << aux_fortran_init;
        
        devices_and_implementors_it++;
        n_devices--;
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
        bool is_untied,

        OutlineInfo& outline_info,

        /* this is non-NULL only for function tasks */
        OutlineInfo* parameter_outline_info)
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
           xlate_function_name;

    Nodecl::NodeclBase fill_outline_arguments_tree,
        fill_dependences_outline_tree;
    Source fill_outline_arguments,
           fill_dependences_outline;

    Nodecl::NodeclBase fill_immediate_arguments_tree,
        fill_dependences_immediate_tree;
    Source fill_immediate_arguments,
           fill_dependences_immediate;

    std::string outline_name = get_outline_name(current_function);

    // Declare argument structure
    TL::Symbol structure_symbol = declare_argument_structure(outline_info, construct);
    struct_arg_type_name << structure_symbol.get_name();

    // List of device names

    // MultiMap with every implementation of the current function task
    OutlineInfo::implementation_table_t implementation_table = outline_info.get_implementation_table();

    
    std::multimap<std::string, std::string> devices_and_implementors;
    for (OutlineInfo::implementation_table_t::iterator it = implementation_table.begin();
            it != implementation_table.end();
            ++it)
    {
        ObjectList<std::string> devices=it->second.get_device_names();
        for (ObjectList<std::string>::iterator it2 = devices.begin();
                    it2 != devices.end();
                    ++it2)
        {
            devices_and_implementors.insert(
                    make_pair(
                        *it2, /* device name */
                        get_outline_name(it->first))); /*implementor outline name */
        }
    }


    // Disallow GPU tasks to be executed at the time they are created
    bool mandatory_creation = false;
    DeviceHandler device_handler = DeviceHandler::get_device_handler();
    for ( std::multimap<std::string, std::string>::iterator it = devices_and_implementors.begin();
            it != devices_and_implementors.end() && !mandatory_creation;
            it++)
    {
        std::string device_name = it->first;
        DeviceProvider* device = device_handler.get_device(device_name);
        ERROR_CONDITION(device == NULL, " Device '%s' has not been loaded.", device_name.c_str());
        mandatory_creation = device->allow_mandatory_creation();
    }

    const_wd_info << fill_const_wd_info(
            struct_arg_type_name,
            is_untied,
            mandatory_creation,
            outline_info,
            devices_and_implementors,
            construct);

    if (priority_expr.is_null())
    {
        priority_expr = const_value_to_nodecl(const_value_get_signed_int(0));
    }

    dynamic_wd_info
        << "nanos_wd_dyn_props_t nanos_wd_dyn_props;"
        << "nanos_wd_dyn_props.tie_to = 0;"
        << "nanos_wd_dyn_props.priority = " << as_expression(priority_expr) << ";"
        ;

    Source dynamic_size;
    struct_size << "sizeof(imm_args)" << dynamic_size;

    allocate_immediate_structure(
            outline_info,
            struct_arg_type_name,
            struct_size,
            // out
            immediate_decl,
            dynamic_size);

    // For every device name specified in the 'device' clause, we create its outline function
//    CreateOutlineInfo info(outline_name, outline_info.get_data_items(),outline_info.get_implementation_table()[function_symbol], statements, structure_symbol, called_task);
//    for (TL::ObjectList<std::string>::const_iterator it = device_names.begin();
//            it != device_names.end();
//            it++)
//    {
//        std::string device_name = *it;
//        DeviceProvider* device = device_handler.get_device(device_name);
//
//        ERROR_CONDITION(device == NULL, " Device '%s' has not been loaded.", device_name.c_str());
//
//        Nodecl::NodeclBase outline_placeholder, output_statements;
//        Nodecl::Utils::SymbolMap *symbol_map = NULL;
//        device->create_outline(info, outline_placeholder, output_statements, symbol_map);
//
//        Nodecl::NodeclBase outline_statements_code =
//            Nodecl::Utils::deep_copy(output_statements, outline_placeholder, *symbol_map);
//        delete symbol_map;
//
//        outline_placeholder.replace(outline_statements_code);
//    }

    
    // For every existant implementation (including the one which defines the task),
    // we should create its outline function.
    OutlineInfo::implementation_table_t::iterator implementation_table_it = implementation_table.begin();
    std::multimap<std::string, std::string>::iterator devices_and_implementors_it = devices_and_implementors.begin();
    int n_devices=implementation_table_it->second.get_device_names().size();    
    while (devices_and_implementors_it != devices_and_implementors.end())
    {
        if (n_devices<1){
                implementation_table_it++;
                n_devices=implementation_table_it->second.get_device_names().size();
        }
        std::string device_name = devices_and_implementors_it->first;
        std::string implementor_outline_name = devices_and_implementors_it->second;
        TL::Symbol implementor_symbol = implementation_table_it->first;

        DeviceProvider* device = device_handler.get_device(device_name);
        ERROR_CONDITION(device == NULL, " Device '%s' has not been loaded.", device_name.c_str());

        CreateOutlineInfo info_implementor(
                implementor_outline_name,
                outline_info.get_data_items(),
                implementation_table_it->second,
                statements,
                structure_symbol,
		implementor_symbol);

        Nodecl::NodeclBase outline_placeholder, output_statements;
        Nodecl::Utils::SymbolMap *symbol_map = NULL;
        device->create_outline(info_implementor, outline_placeholder, output_statements, symbol_map);

        // We cannot use the original statements because It contains a function
        // call to the original function task and we really want to call to the
        // function specified in the 'implements' clause. For this reason, we
        // copy the tree and we replace the function task symbol with the
        // implementor symbol
        Nodecl::NodeclBase outline_statements_code;
        if (current_function.is_valid() && current_function!=implementor_symbol){
            Nodecl::Utils::SimpleSymbolMap symbol_map_copy_statements;
            symbol_map_copy_statements.add_map(current_function, implementor_symbol);
            
            Nodecl::NodeclBase copy_statements = Nodecl::Utils::deep_copy(
                output_statements,
                implementor_symbol.get_related_scope(),
                symbol_map_copy_statements);
            outline_statements_code =
                Nodecl::Utils::deep_copy(copy_statements, outline_placeholder, *symbol_map);
        } else {            
            outline_statements_code =
                Nodecl::Utils::deep_copy(output_statements, outline_placeholder, *symbol_map);
        }

        delete symbol_map;

        outline_placeholder.replace(outline_statements_code);

        devices_and_implementors_it++;        
        n_devices--;
    }


    Source err_name;
    err_name << "err";

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
        <<     err_name << " = nanos_create_wd_compact(&nanos_wd_, &(nanos_wd_const_data.base), &nanos_wd_dyn_props, " 
        <<                 struct_size << ", (void**)&ol_args, nanos_current_wd(),"
        <<                 copy_ol_arg << ");"
        <<     "if (" << err_name << " != NANOS_OK) nanos_handle_error (" << err_name << ");"
        <<     "if (nanos_wd_ != (nanos_wd_t)0)"
        <<     "{"
                  // This is a placeholder because arguments are filled using the base language (possibly Fortran)
        <<        statement_placeholder(fill_outline_arguments_tree)
        <<        fill_dependences_outline
        <<        copy_ol_setup
        <<        err_name << " = nanos_submit(nanos_wd_, " << num_dependences << ", dependences, (nanos_team_t)0);"
        <<        "if (" << err_name << " != NANOS_OK) nanos_handle_error (" << err_name << ");"
        <<     "}"
        <<     "else"
        <<     "{"
                    // This is a placeholder because arguments are filled using the base language (possibly Fortran)
        <<          statement_placeholder(fill_immediate_arguments_tree)
        <<          fill_dependences_immediate
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
            xlate_function_name);

    if (num_copies == 0)
    {
        translation_function << "(nanos_translate_args_t)0";
    }
    else
    {
        translation_function << "(nanos_translate_args_t)" << xlate_function_name;

        copy_ol_setup
            << err_name << " = nanos_set_translate_function(nanos_wd_, (nanos_translate_args_t)" << xlate_function_name << ");"
            << "if (" << err_name << " != NANOS_OK) nanos_handle_error(" << err_name << ");"
            ;
    }

    fill_dependences(construct, 
            outline_info, 
            /* accessor */ Source("ol_args->"),
            fill_dependences_outline);
    fill_dependences(construct, 
            outline_info, 
            /* accessor */ Source("imm_args."),
            fill_dependences_immediate);

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
    Nodecl::NodeclBase environment = construct.get_environment();
    Nodecl::NodeclBase statements = construct.get_statements();

    walk(statements);

    // Get the new statements
    statements = construct.get_statements();

    TaskEnvironmentVisitor task_environment;
    task_environment.walk(environment);
    
    Symbol function_symbol = Nodecl::Utils::get_enclosing_function(construct);

    OutlineInfo outline_info(environment,function_symbol);

    Symbol called_task_dummy = Symbol::invalid();

    emit_async_common(
            construct,
            function_symbol,
            called_task_dummy,
            statements,
            task_environment.priority,
            task_environment.is_untied,

            outline_info,
            /* parameter_outline_info */ NULL);
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
                        else
                        {
                            // Not overallocated
                            TL::Type sym_type = (*it)->get_symbol().get_type();
                            if (sym_type.is_any_reference())
                                sym_type = sym_type.references_to();

                            if (sym_type.is_array())
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
                            else
                            {
                                if ((*it)->get_captured_value().is_null())
                                {
                                    // Plain assignment is enough
                                    fill_outline_arguments << 
                                        "ol_args->" << (*it)->get_field_name() << " = " << as_symbol((*it)->get_symbol()) << ";"
                                        ;
                                    fill_immediate_arguments << 
                                        "imm_args." << (*it)->get_field_name() << " = " << as_symbol((*it)->get_symbol()) << ";"
                                        ;
                                }
                                else
                                {
                                    Nodecl::NodeclBase captured = (*it)->get_captured_value();

                                    fill_outline_arguments << 
                                        "ol_args->" << (*it)->get_field_name() << " = " << as_expression(captured.shallow_copy()) << ";"
                                        ;
                                    fill_immediate_arguments << 
                                        "imm_args." << (*it)->get_field_name() << " = " << as_expression(captured.shallow_copy()) << ";"
                                        ;
                                }
                            }
                        }
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
                                    base_expr.get_filename(),
                                    base_expr.get_line());
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
                        }
                        break;
                    }
                case OutlineDataItem::SHARING_SHARED:
                case OutlineDataItem::SHARING_REDUCTION: // Reductions are passed as if they were shared variables
                    {
                        TL::Type t = sym.get_type();
                        if (t.is_any_reference())
                            t = t.references_to();

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
                            if (t.is_array())
                            {
                                lbound_specifier << "(";

                                int i, N = t.get_num_dimensions();
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

void LoweringVisitor::fill_allocatable_dimensions(
        TL::Symbol symbol,
        TL::Type current_type,
        int current_rank,
        int rank_size,
        Source &fill_outline_arguments, 
        Source &fill_immediate_arguments, 
        int &lower_bound_index, 
        int &upper_bound_index)
{
    if (current_type.is_array())
    {
        fill_allocatable_dimensions(
                symbol,
                current_type.array_element(),
                current_rank - 1,
                rank_size,
                fill_outline_arguments, 
                fill_immediate_arguments,
                lower_bound_index,
                upper_bound_index);

        Nodecl::NodeclBase lower, upper;
        current_type.array_get_bounds(lower, upper);

        if (lower.is_null())
        {
            fill_outline_arguments 
                << "ol_args % mcc_lower_bound_" << lower_bound_index 
                << " = LBOUND(" << symbol.get_name() <<", " << (current_rank+1) <<")\n"
                ;
            fill_immediate_arguments 
                << "imm_args % mcc_lower_bound_" << lower_bound_index 
                << " = LBOUND(" << symbol.get_name() <<", " << (current_rank+1) <<")\n"
                ;

            lower_bound_index++;
        }

        if (upper.is_null())
        {
            fill_outline_arguments 
                << "ol_args % mcc_upper_bound_" << upper_bound_index 
                << " = UBOUND(" << symbol.get_name() <<", " << (current_rank+1) <<")\n"
                ;
            fill_immediate_arguments 
                << "imm_args % mcc_upper_bound_" << upper_bound_index 
                << " = UBOUND(" << symbol.get_name() <<", " << (current_rank+1) <<")\n"
                ;

            upper_bound_index++;
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

            copy_offset << as_expression(data_ref.get_offsetof());

            TL::Type copy_type = data_ref.get_data_type();
            TL::Type base_type = copy_type;

            ObjectList<Nodecl::NodeclBase> lower_bounds, upper_bounds, region_sizes;

            int num_dimensions_count = copy_type.get_num_dimensions();
            if (num_dimensions_count == 0)
            {
                lower_bounds.append(const_value_to_nodecl(const_value_get_signed_int(0)));
                upper_bounds.append(const_value_to_nodecl(const_value_get_signed_int(0)));
                region_sizes.append(const_value_to_nodecl(const_value_get_signed_int(1)));
                num_dimensions_count++;
            }
            else
            {
                TL::Type t = copy_type;
                while (t.is_array())
                {
                    Nodecl::NodeclBase lower, upper, region_size;
                    if (t.array_is_region())
                    {
                        t.array_get_region_bounds(lower, upper);
                        region_size = t.array_get_region_size();
                    }
                    else
                    {
                        t.array_get_bounds(lower, upper);
                        region_size = t.array_get_size();
                    }


                    lower_bounds.append(lower);
                    upper_bounds.append(upper);
                    region_sizes.append(region_size);

                    t = t.array_element();
                }

                base_type = t;

                // Sanity check
                ERROR_CONDITION(num_dimensions_count != (signed)lower_bounds.size()
                        || num_dimensions_count != (signed)upper_bounds.size()
                        || num_dimensions_count != (signed)region_sizes.size(),
                        "Mismatch between dimensions", 0);

            }

            num_dimensions
                << num_dimensions_count;


            for (int dim = 0; dim < num_dimensions_count; dim++)
            {
                // Sanity check
                ERROR_CONDITION(current_dimension_descriptor >= num_copies_dimensions, "Wrong number of dimensions %d >= %d",
                        current_dimension_descriptor, num_copies_dimensions);

                if (dim == 0)
                {
                    // In bytes
                    ol_dimension_descriptors
                        << "ol_copy_dimensions[" << current_dimension_descriptor  << "].size = "
                        << "(" << as_expression(region_sizes[dim].shallow_copy()) << ") * sizeof(" << as_type(base_type) << ");"
                        <<  "ol_copy_dimensions[" << current_dimension_descriptor  << "].lower_bound = "
                        << "(" << as_expression(lower_bounds[dim].shallow_copy()) << ") * sizeof(" << as_type(base_type) << ");"
                        <<  "ol_copy_dimensions[" << current_dimension_descriptor  << "].accessed_length = "
                        << "((" << as_expression(upper_bounds[dim].shallow_copy()) << ") - ("
                        << as_expression(lower_bounds[dim].shallow_copy()) << ") + 1) * sizeof(" << as_type(base_type) << ");"
                        ;
                    imm_dimension_descriptors
                        << "imm_copy_dimensions[" << current_dimension_descriptor  << "].size = "
                        << "(" << as_expression(region_sizes[dim].shallow_copy()) << ") * sizeof(" << as_type(base_type) << ");"
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
                        << as_expression(region_sizes[dim].shallow_copy()) << ";"
                        << "ol_copy_dimensions[" << current_dimension_descriptor  << "].lower_bound = "
                        << as_expression(lower_bounds[dim].shallow_copy()) << ";"
                        << "ol_copy_dimensions[" << current_dimension_descriptor  << "].accessed_length = "
                        << "(" << as_expression(upper_bounds[dim].shallow_copy()) << ") - ("
                        << as_expression(lower_bounds[dim].shallow_copy()) << ") + 1;"
                        ;
                    imm_dimension_descriptors
                        << "imm_copy_dimensions[" << current_dimension_descriptor  << "].size = "
                        << as_expression(region_sizes[dim].shallow_copy()) << ";"
                        << "imm_copy_dimensions[" << current_dimension_descriptor  << "].lower_bound = "
                        << as_expression(lower_bounds[dim].shallow_copy()) << ";"
                        << "imm_copy_dimensions[" << current_dimension_descriptor  << "].accessed_length = "
                        << "(" << as_expression(upper_bounds[dim].shallow_copy()) << ") - ("
                        << as_expression(lower_bounds[dim].shallow_copy()) << ") + 1;"
                        ;
                }
                current_dimension_descriptor++;
            }
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
        Source& xlate_function_name
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

        fill_copies_region(ctr,
                outline_info,
                num_copies,
                num_copies_dimensions,
                copy_ol_decl,
                copy_ol_arg,
                copy_ol_setup,
                copy_imm_arg,
                copy_imm_setup);
        emit_translation_function_region(ctr,
                outline_info,
                parameter_outline_info,
                structure_symbol,
                xlate_function_name);
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
                    xlate_function_name);
        }
    }
}

void LoweringVisitor::emit_translation_function_nonregion(
        Nodecl::NodeclBase ctr,
        OutlineInfo& outline_info,
        OutlineInfo* parameter_outline_info,
        TL::Symbol structure_symbol,

        // Out
        TL::Source& xlate_function_name
        )
{
    TL::Counter &fun_num = TL::CounterManager::get_counter("nanos++-translation-functions");
    Source fun_name;
    fun_name << "nanos_xlate_fun_" << fun_num;
    fun_num++;
    xlate_function_name = fun_name;

    Source function_def;

    Nodecl::NodeclBase function_body;

    TL::Type argument_type = ::get_user_defined_type(structure_symbol.get_internal_symbol());
    argument_type = argument_type.get_lvalue_reference_to();

    function_def
        << "static void " << fun_name << "(" << as_type(argument_type) << " arg, nanos_wd_t wd)"
        << "{"
        <<      statement_placeholder(function_body)
        << "}"
        ;

    if (IS_FORTRAN_LANGUAGE)
    {
        Source::source_language = SourceLanguage::C;
    }
    Nodecl::NodeclBase function_def_tree = function_def.parse_global(ctr);
    if (IS_FORTRAN_LANGUAGE)
    {
        Source::source_language = SourceLanguage::Current;
    }

    TL::ObjectList<OutlineDataItem*> data_items;
    data_items = outline_info.get_fields();

    Source translations;

    Nodecl::Utils::SimpleSymbolMap symbol_map;

    // First gather all the data, so the translations are easier later
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end(); it++)
    {
        TL::ObjectList<OutlineDataItem::CopyItem> copies = (*it)->get_copies();

        Source declaration;
        declaration
            << as_type((*it)->get_field_type()) << " " << (*it)->get_symbol().get_name() << ";";

        if (IS_FORTRAN_LANGUAGE)
        {
            Source::source_language = SourceLanguage::C;
        }
        declaration.parse_statement(function_body);
        if (IS_FORTRAN_LANGUAGE)
        {
            Source::source_language = SourceLanguage::Current;
        }

        TL::Symbol new_sym = ReferenceScope(function_body).get_scope().get_symbol_from_name((*it)->get_symbol().get_name());
        ERROR_CONDITION(!new_sym.is_valid(), "Invalid symbol just created", 0);

        symbol_map.add_map((*it)->get_symbol(), new_sym);

        if ((*it)->get_base_symbol_of_argument().is_valid())
        {
            symbol_map.add_map((*it)->get_base_symbol_of_argument(), new_sym);
        }

        if (IS_CXX_LANGUAGE)
        {
            translations << as_statement(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), new_sym));
        }

        translations
            << (*it)->get_symbol().get_name()
            << " = arg." << (*it)->get_field_name() << ";"
            ;
    }

    int copy_num = 0;
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        TL::ObjectList<OutlineDataItem::CopyItem> copies = (*it)->get_copies();

        if (copies.empty())
            continue;

        ERROR_CONDITION(copies.empty(), "Invalid copy set", 0);
        if (copies.size() > 1)
        {
            info_printf("%s: info: more than one copy specified for '%s' but the runtime does not support it. "
                    "Only the first copy (%s) will be translated\n",
                    ctr.get_locus().c_str(),
                    (*it)->get_symbol().get_name().c_str(),
                    copies[0].expression.prettyprint().c_str());
        }

        TL::DataReference data_ref(copies[0].expression);

        Nodecl::NodeclBase base_address;

        if (IS_FORTRAN_LANGUAGE)
        {
            base_address = data_ref.get_base_address_as_integer();
        }
        else
        {
            base_address = data_ref.get_base_address();
        }

        translations
            << "{"
            << "intptr_t device_base_address;"
            << "signed long offset;"
            << "nanos_err_t err;"
            << "intptr_t host_base_address;"

            << "host_base_address = (intptr_t)arg." << (*it)->get_field_name() << ";"
            << "offset = (intptr_t)(" << as_expression(
                        Nodecl::Utils::deep_copy(base_address, function_body,
                            symbol_map)) << ") - (intptr_t)host_base_address;"
            << "device_base_address = 0;"
            << "err = nanos_get_addr(" << copy_num << ", (void**)&device_base_address, wd);"
            << "device_base_address -= offset;"
            << "if (err != NANOS_OK) nanos_handle_error(err);"
            << "arg." << (*it)->get_field_name() << " = (" << as_type((*it)->get_field_type()) << ")device_base_address;"
            << "}"
            ;
        copy_num++;
    }

    if (IS_FORTRAN_LANGUAGE)
    {
        Source::source_language = SourceLanguage::C;
    }
    Nodecl::NodeclBase translations_tree = translations.parse_statement(function_body);
    if (IS_FORTRAN_LANGUAGE)
    {
        Source::source_language = SourceLanguage::Current;
    }

    function_body.replace(translations_tree);

    Nodecl::Utils::prepend_to_enclosing_top_level_location(ctr, function_def_tree);
}

void LoweringVisitor::emit_translation_function_region(
        Nodecl::NodeclBase ctr,
        OutlineInfo& outline_info,
        OutlineInfo* parameter_outline_info,
        TL::Symbol structure_symbol,

        // Out
        TL::Source& xlate_function_name
        )
{
    TL::Counter &fun_num = TL::CounterManager::get_counter("nanos++-translation-functions");
    Source fun_name;
    fun_name << "nanos_xlate_fun_" << fun_num;
    fun_num++;
    xlate_function_name = fun_name;

    Source function_def;

    Nodecl::NodeclBase function_body;

    TL::Type argument_type = ::get_user_defined_type(structure_symbol.get_internal_symbol());
    argument_type = argument_type.get_lvalue_reference_to();

    function_def
        << "static void " << fun_name << "(" << as_type(argument_type) << " arg, nanos_wd_t wd)"
        << "{"
        <<      statement_placeholder(function_body)
        << "}"
        ;

    if (IS_FORTRAN_LANGUAGE)
    {
        Source::source_language = SourceLanguage::C;
    }
    Nodecl::NodeclBase function_def_tree = function_def.parse_global(ctr);
    if (IS_FORTRAN_LANGUAGE)
    {
        Source::source_language = SourceLanguage::Current;
    }

    TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();

    Source translations;

    // First gather all the data, so the translations are easier later
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end(); it++)
    {
        TL::ObjectList<OutlineDataItem::CopyItem> copies = (*it)->get_copies();

        if (copies.empty())
            continue;

        translations
            << as_type((*it)->get_field_type()) << " " << (*it)->get_symbol().get_name() 
            << " = arg." << (*it)->get_field_name() << ";"
            ;
    }

    int copy_num = 0;
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        TL::ObjectList<OutlineDataItem::CopyItem> copies = (*it)->get_copies();

        if (copies.empty())
            continue;

        translations
            << "{"
            << "void *device_base_address;"
            << "nanos_err_t err;"

            << "device_base_address = 0;"
            << "err = nanos_get_addr(" << copy_num << ", &device_base_address, wd);"
            << "if (err != NANOS_OK) nanos_handle_error(err);"
            << "arg." << (*it)->get_field_name() << " = (" << as_type((*it)->get_field_type()) << ")device_base_address;"
            << "}"
            ;

        copy_num++;
    }

    if (IS_FORTRAN_LANGUAGE)
    {
        Source::source_language = SourceLanguage::C;
    }
    Nodecl::NodeclBase translations_tree = translations.parse_statement(function_body);
    if (IS_FORTRAN_LANGUAGE)
    {
        Source::source_language = SourceLanguage::Current;
    }

    function_body.replace(translations_tree);

    Nodecl::Utils::prepend_to_enclosing_top_level_location(ctr, function_def_tree);
}

void LoweringVisitor::fill_dependences(
        Nodecl::NodeclBase ctr,
        OutlineInfo& outline_info,
        Source arguments_accessor,
        // out
        Source& result_src
        )
{
    fill_dependences_internal(ctr, outline_info, arguments_accessor, /* on_wait */ false, result_src);
}

void LoweringVisitor::fill_dependences_internal(
        Nodecl::NodeclBase ctr,
        OutlineInfo& outline_info,
        Source arguments_accessor,
        bool on_wait,
        // out
        Source& result_src
        )
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
        int num_handled_dependences = 0;
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

                ERROR_CONDITION(!dep_expr.is_valid(),
                        "%s: Invalid dependency detected '%s'. Reason: %s\n",
                        dep_expr.get_locus().c_str(),
                        dep_expr.prettyprint().c_str(),
                        dep_expr.get_error_log().c_str());

                Source dependency_offset,
                       dependency_flags,
                       dependency_flags_in,
                       dependency_flags_out,
                       dependency_flags_concurrent,
                       dependency_flags_commutative;

                Nodecl::NodeclBase base_address, dep_source_expr = dep_expr;

                if (!(*it)->get_base_address_expression().is_null())
                {
                    // This is only for function task dependences
                    // Outline tasks do not need any of this
                    dep_source_expr = base_address = (*it)->get_base_address_expression();

                    if ((IS_CXX_LANGUAGE
                                || IS_FORTRAN_LANGUAGE)
                            && (*it)->get_symbol().get_type().is_lvalue_reference())
                    {
                        // If the parameter type
                        TL::Type t = base_address.get_type();
                        if (t.is_any_reference())
                            t = t.references_to();
                        t = t.get_pointer_to();
                        // Create a reference here
                        base_address = Nodecl::Reference::make(
                                base_address.shallow_copy(),
                                t,
                                base_address.get_filename(),
                                base_address.get_line());
                    }
                }
                else
                {
                    base_address = dep_expr.get_base_address().shallow_copy();
                }

                dependency_flags 
                    << "{" 
                    << dependency_flags_in << "," 
                    << dependency_flags_out << ", "
                    << /* renaming has not yet been implemented */ "0, " 
                    << dependency_flags_concurrent
                    << "}"
                    ;

                Type dependency_type = dep_expr.get_data_type();

                int num_dimensions = dependency_type.get_num_dimensions();

                int concurrent = ((dir & OutlineDataItem::DEP_CONCURRENT) == OutlineDataItem::DEP_CONCURRENT);
                int commutative = ((dir & OutlineDataItem::DEP_COMMUTATIVE) == OutlineDataItem::DEP_COMMUTATIVE);

                dependency_flags_in << (((dir & OutlineDataItem::DEP_IN) == OutlineDataItem::DEP_IN) 
                        || concurrent || commutative);
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
                        Source dims_description;
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

                        Source diff;
                        diff
                            << "(" << as_expression(region_lb) << ") - (" << as_expression(array_lb) << ")";

                        lb = diff.parse_expression(ctr);

                        size = contiguous_array_type.array_get_region_size();
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
                    if (num_handled_dependences > 0)
                    {
                        dependency_init << ", ";
                    }

                    Source dep_address;
                    // if (on_wait)
                    {
                       dep_address << as_expression(base_address);
                    }
                    // else
                    // {                        
                    //     dep_address << "(void*)" << arguments_accessor << (*it)->get_field_name()
                    //         ;
                    // }

                    dependency_init
                        << "{"
                        << dep_address << ", "
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
                num_handled_dependences++;
            }
        }
    }
    else
    {
        running_error("%s: error: please update your runtime version. deps_api < 1001 not supported\n", ctr.get_locus().c_str());
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
        // Craft a SIZE
        Source src;

        Nodecl::NodeclBase expr = data_reference;
        if (expr.is<Nodecl::ArraySubscript>())
        {
            expr = expr.as<Nodecl::ArraySubscript>().get_subscripted();
        }

        src << "SIZE(" << as_expression(expr.shallow_copy()) << ", " << fortran_dimension << ")";

        n = src.parse_expression(Scope(CURRENT_COMPILED_FILE->global_decl_context));
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

    // The contiguous dimension in Fortran is always the number 1
    src << "LBOUND(" << as_expression(dep_expr) << ", " << dimension_num << ")";

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
        Nodecl::NodeclBase lb, ub, size;

        if (dep_type.array_is_region())
        {
            dep_type.array_get_region_bounds(lb, ub);
            size = dep_type.array_get_region_size();
        }
        else
        {
            dep_type.array_get_bounds(lb, ub);

            if (lb.is_null() && IS_FORTRAN_LANGUAGE)
            {
                lb = get_lower_bound(dep_expr, current_dim);
            }

            size = get_size_for_dimension(dep_type, current_dim, dep_expr);
        }

        dimension_size << as_expression(dim_sizes[n_dims - current_dim]);
        dimension_lower_bound << as_expression(lb);
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


} }
