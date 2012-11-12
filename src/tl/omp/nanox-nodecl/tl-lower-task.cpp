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
        const std::string& outline_name,
        bool is_untied,
        bool mandatory_creation,
        int num_copies,
        const ObjectList<std::string>& device_names,
        const std::multimap<std::string, std::string>& devices_and_implementors,
        Nodecl::NodeclBase construct)
{
    // Static stuff
    //
    // typedef struct {
    //     nanos_wd_props_t props;
    //     size_t data_alignment;
    //     size_t num_copies;
    //     size_t num_devices;
    // } nanos_const_wd_definition_t;

    int num_devices = device_names.size();
    TL::Symbol const_wd_type = declare_const_wd_type(num_devices + devices_and_implementors.size(), construct);

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
        << /* ".num_copies = " << */ num_copies << ",\n"
        << /* ".num_devices = " << */ num_devices << ",\n"
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

    // For every device name specified in the 'device' clause, we should get
    // its device descriptor
    DeviceDescriptorInfo info(outline_name);
    DeviceHandler device_handler = DeviceHandler::get_device_handler();
    for (ObjectList<std::string>::const_iterator it = device_names.begin();
            it != device_names.end();
            ++it)
    {
        Source ancillary_device_description, device_description, aux_fortran_init;

        if (it != device_names.begin())
            device_descriptions <<  ", ";

        std::string device_name = *it;
        DeviceProvider* device = device_handler.get_device(device_name);
        ERROR_CONDITION(device == NULL, " Device '%s' has not been loaded.", device_name.c_str());

        device->get_device_descriptor(
                info,
                ancillary_device_description,
                device_description,
                aux_fortran_init);

        device_descriptions << device_description;
        ancillary_device_descriptions << ancillary_device_description;
        opt_fortran_dynamic_init << aux_fortran_init;
    }

    // The current function task may have additional implementations. For every
    // existant implementation, we should get its device descriptor information.
    // Note that in this case we use the implementor outline name as outline name
    for (std::multimap<std::string, std::string>::const_iterator it =
            devices_and_implementors.begin();
            it != devices_and_implementors.end();
            ++it)
    {
        Source ancillary_device_description, device_description, aux_fortran_init;

        device_descriptions <<  ", ";

        std::string device_name = it->first;
        std::string implementor_outline_name = it->second;

        DeviceProvider* device = device_handler.get_device(device_name);
        ERROR_CONDITION(device == NULL, " Device '%s' has not been loaded.", device_name.c_str());

        DeviceDescriptorInfo info_implementor(implementor_outline_name);
        device->get_device_descriptor(
                info_implementor,
                ancillary_device_description,
                device_description,
                aux_fortran_init);

        device_descriptions << device_description;
        ancillary_device_descriptions << ancillary_device_description;
        opt_fortran_dynamic_init << aux_fortran_init;
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
        TL::Symbol function_symbol,
        TL::Symbol called_task,
        Nodecl::NodeclBase statements,
        Nodecl::NodeclBase priority_expr,
        bool is_untied,

        OutlineInfo& outline_info)
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
           translation_fun_arg_name,
           const_wd_info,
           dynamic_wd_info;

    Nodecl::NodeclBase fill_outline_arguments_tree,
        fill_dependences_outline_tree;
    Source fill_outline_arguments,
           fill_dependences_outline;

    Nodecl::NodeclBase fill_immediate_arguments_tree,
        fill_dependences_immediate_tree;
    Source fill_immediate_arguments,
           fill_dependences_immediate;

    std::string outline_name = get_outline_name(function_symbol);

    // Declare argument structure
    TL::Symbol structure_symbol = declare_argument_structure(outline_info, construct);
    struct_arg_type_name << structure_symbol.get_name();

    // List of device names
    TL::ObjectList<std::string> device_names = outline_info.get_device_names();

    // MultiMap with every implementation of the current function task
    OutlineInfo::implementation_table_t implementation_table = outline_info.get_implementation_table();

    std::multimap<std::string, std::string> devices_and_implementors;
    for (OutlineInfo::implementation_table_t::iterator it = implementation_table.begin();
            it != implementation_table.end();
            ++it)
    {
        devices_and_implementors.insert(
                make_pair(
                    it->first, /* device name */
                    get_outline_name(it->second))); /*implementor outline name */
    }


    const_wd_info << fill_const_wd_info(
            struct_arg_type_name,
            outline_name,
            is_untied,
            /* mandatory_creation */ 0,
            /* num_copies */ count_copies(outline_info),
            device_names,
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

    translation_fun_arg_name << "(void (*)(void*, void*))0";

    // For every device name specified in the 'device' clause, we create its outline function
    CreateOutlineInfo info(outline_name, outline_info, statements, structure_symbol, called_task);
    DeviceHandler device_handler = DeviceHandler::get_device_handler();
    for (TL::ObjectList<std::string>::const_iterator it = device_names.begin();
            it != device_names.end();
            it++)
    {
        std::string device_name = *it;
        DeviceProvider* device = device_handler.get_device(device_name);

        ERROR_CONDITION(device == NULL, " Device '%s' has not been loaded.", device_name.c_str());

        Nodecl::NodeclBase outline_placeholder;
        Nodecl::Utils::SymbolMap *symbol_map = NULL;
        device->create_outline(info, outline_placeholder, symbol_map);

        Nodecl::NodeclBase outline_statements_code =
            Nodecl::Utils::deep_copy(statements, outline_placeholder, *symbol_map);
        delete symbol_map;

        outline_placeholder.replace(outline_statements_code);
    }

    // Nodecl::NodeclBase function_call_nodecl = statements.as<Nodecl::List>().begin()->as<Nodecl::ExpressionStatement>().get_nest();
    // Nodecl::NodeclBase called_symbol_nodecl = function_call_nodecl.as<Nodecl::FunctionCall>().get_called();
    // TL::Symbol called_task = called_symbol_nodecl.as<Nodecl::Symbol>().get_symbol();

    // The current function task may have additional implementations. For every
    // existant implementation, we should create its outline function.
    OutlineInfo::implementation_table_t::iterator implementation_table_it = implementation_table.begin();
    std::multimap<std::string, std::string>::iterator devices_and_implementors_it = devices_and_implementors.begin();
    while (devices_and_implementors_it != devices_and_implementors.end())
    {
        std::string device_name = devices_and_implementors_it->first;
        std::string implementor_outline_name = devices_and_implementors_it->second;
        TL::Symbol implementor_symbol = implementation_table_it->second;

        DeviceProvider* device = device_handler.get_device(device_name);
        ERROR_CONDITION(device == NULL, " Device '%s' has not been loaded.", device_name.c_str());

        CreateOutlineInfo info_implementor(
                implementor_outline_name,
                outline_info,
                statements,
                structure_symbol,
				implementor_symbol);

        Nodecl::NodeclBase outline_placeholder;
        Nodecl::Utils::SymbolMap *symbol_map = NULL;
        device->create_outline(info_implementor, outline_placeholder, symbol_map);

        // We cannot use the original statements because It contains a function
        // call to the original function task and we really want to call to the
        // function specified in the 'implements' clause. For this reason, we
        // copy the tree and we replace the function task symbol with the
        // implementor symbol
        Nodecl::Utils::SimpleSymbolMap symbol_map_copy_statements;
        symbol_map_copy_statements.add_map(called_task, implementor_symbol);

        Nodecl::NodeclBase copy_statements = Nodecl::Utils::deep_copy(
                statements,
                implementor_symbol.get_related_scope(),
                symbol_map_copy_statements);

        Nodecl::NodeclBase outline_statements_code =
            Nodecl::Utils::deep_copy(copy_statements, outline_placeholder, *symbol_map);
        delete symbol_map;

        outline_placeholder.replace(outline_statements_code);

        devices_and_implementors_it++;
        implementation_table_it++;
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
        <<                  translation_fun_arg_name << ");"
        <<          "if (" << err_name << " != NANOS_OK) nanos_handle_error (" << err_name << ");"
        <<     "}"
        << "}"
        ;

    // Fill arguments
    fill_arguments(construct, outline_info, fill_outline_arguments, fill_immediate_arguments);
    
    // Fill dependences for outline
    num_dependences << count_dependences(outline_info);

    fill_copies(construct,
        outline_info,
        copy_ol_decl,
        copy_ol_arg,
        copy_ol_setup,
        copy_imm_arg,
        copy_imm_setup);

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

    OutlineInfo outline_info(environment);

    Symbol called_task_dummy = Symbol::invalid();
    Symbol function_symbol = Nodecl::Utils::get_enclosing_function(construct);

    emit_async_common(
            construct,
            function_symbol,
            called_task_dummy,
            statements,
            task_environment.priority,
            task_environment.is_untied,

            outline_info);
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
                            TL::Symbol ptr_of_sym = get_function_ptr_of((*it)->get_symbol(),
                                    ctr.retrieve_context());

                            fill_outline_arguments <<
                                "ol_args %" << (*it)->get_field_name() << " => MERCURIUM_LOC("
                                << (*it)->get_symbol().get_name() << ") \n"
                                ;
                            fill_immediate_arguments <<
                                "imm_args % " << (*it)->get_field_name() << " => MERCURIUM_LOC("
                                << (*it)->get_symbol().get_name() << ") \n"
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
        if ((*it)->get_directionality() == OutlineDataItem::DIRECTIONALITY_NONE)
            continue;

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
        if ((*it)->get_copy_directionality() == OutlineDataItem::COPY_NONE)
            continue;

        num_copies += (*it)->get_copies().size();
    }

    return num_copies;
}

void LoweringVisitor::fill_copies(
        Nodecl::NodeclBase ctr,
        OutlineInfo& outline_info, 
        // Source arguments_accessor,
        // out
        Source& copy_ol_decl,
        Source& copy_ol_arg,
        Source& copy_ol_setup,
        Source& copy_imm_arg,
        Source& copy_imm_setup)
{
    int num_copies = count_copies(outline_info);

    if (num_copies == 0)
    {
        copy_ol_arg << "(nanos_copy_data_t**)0";
        copy_imm_arg << "(nanos_copy_data_t*)0";
        return;
    }
    
    copy_ol_arg << "&ol_copy_data";
    copy_imm_arg << "imm_copy_data";

    // FIXME - This must be versioned
    if (Nanos::Version::interface_is_at_least("copies_dep", 1000))
    {
        internal_error("New copies dependency not implemented", 0);
        return;
    }

    copy_ol_decl
        << "nanos_copy_data_t *ol_copy_data = (nanos_copy_data_t*)0;"
        ;
    copy_imm_setup 
        << "nanos_copy_data_t imm_copy_data[" << num_copies << "];";

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
        OutlineDataItem::CopyDirectionality dir = (*it)->get_copy_directionality();
        if (dir == OutlineDataItem::COPY_NONE)
            continue;

        TL::ObjectList<Nodecl::NodeclBase> copies = (*it)->get_copies();
        for (ObjectList<Nodecl::NodeclBase>::iterator copy_it = copies.begin();
                copy_it != copies.end();
                copy_it++, current_copy_num++)
        {
            TL::DataReference data_ref(*copy_it);

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
        }
    }
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
            OutlineDataItem::Directionality dir = (*it)->get_directionality();
            if (dir == OutlineDataItem::DIRECTIONALITY_NONE)
                continue;

            TL::ObjectList<Nodecl::NodeclBase> deps = (*it)->get_dependences();
            for (ObjectList<Nodecl::NodeclBase>::iterator dep_it = deps.begin();
                    dep_it != deps.end();
                    dep_it++, current_dep_num++)
            {
                TL::DataReference dep_expr(*dep_it);

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

                int concurrent = ((dir & OutlineDataItem::DIRECTIONALITY_CONCURRENT) == OutlineDataItem::DIRECTIONALITY_CONCURRENT);
                int commutative = ((dir & OutlineDataItem::DIRECTIONALITY_COMMUTATIVE) == OutlineDataItem::DIRECTIONALITY_COMMUTATIVE);

                dependency_flags_in << (((dir & OutlineDataItem::DIRECTIONALITY_IN) == OutlineDataItem::DIRECTIONALITY_IN) 
                        || concurrent || commutative);
                dependency_flags_out << (((dir & OutlineDataItem::DIRECTIONALITY_OUT) == OutlineDataItem::DIRECTIONALITY_OUT) 
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

typedef std::map<TL::Symbol, Nodecl::NodeclBase> sym_to_argument_expr_t;
typedef std::map<TL::Symbol, TL::Symbol> param_sym_to_arg_sym_t;

// When a dependency expression type has an expression (e.g. in an array) this function
// rewrites the expresion using the proper argument of the function task
static Nodecl::NodeclBase rewrite_expression_in_dependency(Nodecl::NodeclBase node, const sym_to_argument_expr_t& map)
{
    if (node.is_null())
        return node;

    TL::Symbol sym = node.get_symbol();
    if (sym.is_valid())
    {
        if (sym.is_saved_expression())
        {
            return rewrite_expression_in_dependency(sym.get_value(), map);
        }

        sym_to_argument_expr_t::const_iterator it = map.find(sym);
        if (it != map.end())
        {
            Nodecl::NodeclBase arg = it->second.shallow_copy();
            return Nodecl::ParenthesizedExpression::make(
                    arg,
                    arg.get_type(),
                    arg.get_filename(),
                    arg.get_line());
        }
    }

    TL::ObjectList<Nodecl::NodeclBase> children = node.children();
    for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
            it != children.end();
            it++)
    {
        *it = rewrite_expression_in_dependency(*it, map);
    }

    node.rechild(children);

    return node;
}

// Update the types of a dependency expression
static TL::Type rewrite_dependency_type(TL::Type t, const sym_to_argument_expr_t& map)
{
    if (!t.is_valid())
        return t;

    if (t.is_lvalue_reference())
    {
        return rewrite_dependency_type(t.references_to(), map).get_lvalue_reference_to();
    }
    else if (t.is_pointer())
    {
        return (rewrite_dependency_type(t.points_to(), map)).get_pointer_to();
    }
    else if (t.is_array())
    {
        TL::Type element_type = rewrite_dependency_type(t.array_element(), map);

        Nodecl::NodeclBase lower_bound, upper_bound;
        t.array_get_bounds(lower_bound, upper_bound);

        lower_bound = rewrite_expression_in_dependency(lower_bound.shallow_copy(), map);
        upper_bound = rewrite_expression_in_dependency(upper_bound.shallow_copy(), map);

        if (!t.array_is_region())
        {
            return element_type.get_array_to(lower_bound, upper_bound,
                    CURRENT_COMPILED_FILE->global_decl_context);
        }
        else
        {
            Nodecl::NodeclBase region_lower_bound, region_upper_bound;
            t.array_get_region_bounds(region_lower_bound, region_upper_bound);

            region_lower_bound = rewrite_expression_in_dependency(region_lower_bound.shallow_copy(), map);
            region_upper_bound = rewrite_expression_in_dependency(region_upper_bound.shallow_copy(), map);

            return element_type.get_array_to_with_region(
                    lower_bound, upper_bound,
                    region_lower_bound, region_upper_bound,
                    CURRENT_COMPILED_FILE->global_decl_context);
        }
    }
    else
    {
        // Best effort
        return t;
    }
}

// This function only updates the types of a dependency expression. Everything else is left as is
static Nodecl::NodeclBase rewrite_single_dependency(Nodecl::NodeclBase node, const sym_to_argument_expr_t& map)
{
    if (node.is_null())
        return node;

    node.set_type(rewrite_dependency_type(node.get_type(), map));

    TL::ObjectList<Nodecl::NodeclBase> children = node.children();
    for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
            it != children.end();
            it++)
    {
        *it = rewrite_single_dependency(*it, map);
    }

    node.rechild(children);

    // Update indexes where is due
    if (node.is<Nodecl::ArraySubscript>())
    {
        Nodecl::ArraySubscript arr_subscr = node.as<Nodecl::ArraySubscript>();
        arr_subscr.set_subscripts(
                rewrite_expression_in_dependency(arr_subscr.get_subscripts(), map));
    }
    else if (node.is<Nodecl::Shaping>())
    {
        Nodecl::Shaping shaping = node.as<Nodecl::Shaping>();
        shaping.set_shape(
                rewrite_expression_in_dependency(shaping.get_shape(), map));
    }

    return node;
}

// Rewrite every dependence in terms of the arguments of the function task call
static TL::ObjectList<Nodecl::NodeclBase> rewrite_dependences(
        const TL::ObjectList<Nodecl::NodeclBase>& deps, 
        const sym_to_argument_expr_t& param_to_arg_expr)
{
    TL::ObjectList<Nodecl::NodeclBase> result;
    for (TL::ObjectList<Nodecl::NodeclBase>::const_iterator it = deps.begin();
            it != deps.end();
            it++)
    {
        Nodecl::NodeclBase copy = it->shallow_copy();
        result.append( rewrite_single_dependency(copy, param_to_arg_expr) );
    }

    return result;
}

static TL::ObjectList<Nodecl::NodeclBase> rewrite_copies(
        const TL::ObjectList<Nodecl::NodeclBase>& deps, 
        const sym_to_argument_expr_t& param_to_arg_expr)
{
    TL::ObjectList<Nodecl::NodeclBase> result;
    for (TL::ObjectList<Nodecl::NodeclBase>::const_iterator it = deps.begin();
            it != deps.end();
            it++)
    {
        Nodecl::NodeclBase copy = it->shallow_copy();
        result.append( rewrite_expression_in_dependency(copy, param_to_arg_expr) );
    }

    return result;
}

static void copy_outline_data_item(
        OutlineDataItem& dest_info, 
        const OutlineDataItem& source_info,
        const sym_to_argument_expr_t& param_to_arg_expr)
{
    // Copy dependence directionality
    dest_info.set_directionality(source_info.get_directionality());
    dest_info.get_dependences() = rewrite_dependences(source_info.get_dependences(), param_to_arg_expr);

    // Copy copy directionality
    dest_info.set_copy_directionality(source_info.get_copy_directionality());
    dest_info.get_copies() = rewrite_copies(source_info.get_copies(), param_to_arg_expr);
}

static void fill_map_parameters_to_arguments(
        TL::Symbol function,
        Nodecl::List arguments,
        sym_to_argument_expr_t& param_to_arg_expr)
{
    int i = 0;
    Nodecl::List::iterator it = arguments.begin();

    // If the current function is a non-static function and It is member of a
    // class, the first argument of the arguments list represents the object of
    // this class. Skip it!
    if (IS_CXX_LANGUAGE
            && !function.is_static()
            && function.is_member())
    {
        it++;
    }

    for (; it != arguments.end(); it++, i++)
    {
        Nodecl::NodeclBase expression;
        TL::Symbol parameter_sym;
        if (it->is<Nodecl::FortranNamedPairSpec>())
        {
            // If this is a Fortran style argument use the symbol
            Nodecl::FortranNamedPairSpec named_pair(it->as<Nodecl::FortranNamedPairSpec>());

            param_to_arg_expr[named_pair.get_name().get_symbol()] = named_pair.get_argument();
        }
        else
        {
            // Get the i-th parameter of the function
            ERROR_CONDITION(((signed int)function.get_related_symbols().size() <= i), "Too many parameters", 0);
            TL::Symbol parameter = function.get_related_symbols()[i];
            param_to_arg_expr[parameter] = *it;
        }
    }
}

static int outline_data_item_get_parameter_position(const OutlineDataItem& outline_data_item)
{
    TL::Symbol sym = outline_data_item.get_symbol();
    return (sym.is_parameter() ? sym.get_parameter_position(): -1);
}

// When a type has an expression update it using the parameter we will use in the outline function
static Nodecl::NodeclBase rewrite_expression_in_outline(Nodecl::NodeclBase node, const param_sym_to_arg_sym_t& map)
{
    if (node.is_null())
        return node;

    TL::Symbol sym = node.get_symbol();
    if (sym.is_valid())
    {
        if (sym.is_saved_expression())
        {
            return rewrite_expression_in_outline(sym.get_value(), map);
        }

        param_sym_to_arg_sym_t::const_iterator it = map.find(sym);
        if (it != map.end())
        {
            TL::Symbol sym = it->second;
            Nodecl::NodeclBase result = Nodecl::Symbol::make(
                    sym,
                    sym.get_filename(),
                    sym.get_line());

            result.set_type(sym.get_type());

            return result;
        }
    }

    TL::ObjectList<Nodecl::NodeclBase> children = node.children();
    for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
            it != children.end();
            it++)
    {
        *it = rewrite_expression_in_outline(*it, map);
    }

    node.rechild(children);

    return node;
}

// This function updates the type of the parameter using the types of the
// encapsulating function
static TL::Type rewrite_type_in_outline(TL::Type t, const param_sym_to_arg_sym_t& map)
{
    if (!t.is_valid())
        return t;

    if (t.is_lvalue_reference())
    {
        return rewrite_type_in_outline(t.references_to(), map).get_lvalue_reference_to();
    }
    else if (t.is_pointer())
    {
        return (rewrite_type_in_outline(t.points_to(), map)).get_pointer_to();
    }
    else if (t.is_array())
    {
        TL::Type element_type = rewrite_type_in_outline(t.array_element(), map);

        Nodecl::NodeclBase lower_bound, upper_bound;
        t.array_get_bounds(lower_bound, upper_bound);

        lower_bound = rewrite_expression_in_outline(lower_bound.shallow_copy(), map);
        upper_bound = rewrite_expression_in_outline(upper_bound.shallow_copy(), map);

        if (!t.array_is_region())
        {
            return element_type.get_array_to(lower_bound, upper_bound,
                    CURRENT_COMPILED_FILE->global_decl_context);
        }
        else
        {
            Nodecl::NodeclBase region_lower_bound, region_upper_bound;
            t.array_get_region_bounds(region_lower_bound, region_upper_bound);

            region_lower_bound = rewrite_expression_in_outline(region_lower_bound.shallow_copy(), map);
            region_upper_bound = rewrite_expression_in_outline(region_upper_bound.shallow_copy(), map);

            return element_type.get_array_to_with_region(
                    lower_bound, upper_bound,
                    region_lower_bound, region_upper_bound,
                    CURRENT_COMPILED_FILE->global_decl_context);
        }
    }
    else
    {
        // Best effort
        return t;
    }
}

static Nodecl::NodeclBase array_section_to_array_element(Nodecl::NodeclBase expr)
{
    Nodecl::NodeclBase base_expr = expr;

    Nodecl::NodeclBase result;
    if (base_expr.is<Nodecl::Symbol>())
    {
        TL::Symbol sym = base_expr.get_symbol();

        TL::Type t = sym.get_type();
        if (t.is_any_reference())
            t = t.references_to();

        if (!::fortran_is_array_type(t.get_internal_type()))
            return expr;

        int ndims = ::fortran_get_rank_of_type(t.get_internal_type());

        Source src;

        src << base_expr.prettyprint() << "(";

        for (int i = 1; i <= ndims; i++)
        {
            if (i > 1)
                src << ", ";

            src << "LBOUND(" << as_expression(base_expr.shallow_copy()) << ", DIM = " << i << ")";
        }

        src << ")";

        return src.parse_expression(base_expr.retrieve_context());
    }
    else if (base_expr.is<Nodecl::ArraySubscript>())
    {
        Nodecl::ArraySubscript arr_subscript = base_expr.as<Nodecl::ArraySubscript>();

        Nodecl::List subscripts = arr_subscript.get_subscripts().as<Nodecl::List>();

        Nodecl::NodeclBase result = arr_subscript.get_subscripted().shallow_copy();
        TL::ObjectList<Nodecl::NodeclBase> fixed_subscripts;

        int num_dimensions = subscripts.size();
        for (Nodecl::List::iterator it = subscripts.begin();
                it != subscripts.end();
                it++, num_dimensions--)
        {
            Source src;
            src << "LBOUND(" << as_expression(result.shallow_copy()) << ", DIM = " << num_dimensions << ")";

            fixed_subscripts.append(src.parse_expression(base_expr.retrieve_context()));
        }

        return Nodecl::ArraySubscript::make(result,
                Nodecl::List::make(fixed_subscripts),
                ::get_lvalue_reference_type(::fortran_get_rank0_type(base_expr.get_type().get_internal_type())),
                base_expr.get_filename(),
                base_expr.get_line());
    }
    else
    {
        return expr;
    }
}

static void give_up_task_call(const Nodecl::OpenMP::TaskCall& construct)
{
    Nodecl::FunctionCall function_call = construct.get_call().as<Nodecl::FunctionCall>();
    TL::Symbol called_sym = function_call.get_called().get_symbol();

    std::cerr << construct.get_locus() << ": note: call to task function '"
        << called_sym.get_qualified_name() << "' has been skipped due to errors" << std::endl;

    construct.replace(construct.get_call());
}

typedef std::map<TL::Symbol, TL::Symbol> extra_map_replacements_t;

static void add_extra_dimensions_for_arguments(const Nodecl::NodeclBase data_ref, 
        OutlineInfoRegisterEntities& outline_register_entities,
        extra_map_replacements_t& extra_map_replacements,
        Scope sc,
        bool do_capture = false)
{
    if (data_ref.is_null())
        return;

    if (data_ref.is<Nodecl::ArraySubscript>())
    {
        Nodecl::ArraySubscript arr_subscript = data_ref.as<Nodecl::ArraySubscript>();
        Nodecl::List subscripts = arr_subscript.get_subscripts().as<Nodecl::List>();

        for (Nodecl::List::iterator it = subscripts.begin();
                it != subscripts.end();
                it++)
        {
            add_extra_dimensions_for_arguments(*it, outline_register_entities, extra_map_replacements, sc, /* do_capture = */ true);
        }

        add_extra_dimensions_for_arguments(arr_subscript.get_subscripted(), outline_register_entities, 
                extra_map_replacements, sc, do_capture);
    }
    else if (data_ref.is<Nodecl::Symbol>()
            && do_capture)
    {
        TL::Symbol current_sym = data_ref.get_symbol();

        if (extra_map_replacements.find(current_sym) == extra_map_replacements.end())
        {
            Counter& arg_counter = CounterManager::get_counter("nanos++-outline-arguments");

            std::stringstream ss;
            ss << "mcc_arg_" << (int)arg_counter;
            TL::Symbol new_symbol = sc.new_symbol(ss.str());
            arg_counter++;

            new_symbol.get_internal_symbol()->kind = current_sym.get_internal_symbol()->kind;
            new_symbol.get_internal_symbol()->type_information = current_sym.get_internal_symbol()->type_information;

            extra_map_replacements[current_sym] = new_symbol;

            outline_register_entities.add_capture_with_value(new_symbol, data_ref);
        }
    }
    else
    {
        TL::ObjectList<Nodecl::NodeclBase> children = data_ref.children();

        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
                it != children.end();
                it++)
        {
            add_extra_dimensions_for_arguments(*it,
                    outline_register_entities,
                    extra_map_replacements,
                    sc,
                    do_capture);
        }
    }
}

static Nodecl::NodeclBase replace_arguments_with_extra(Nodecl::NodeclBase n, extra_map_replacements_t& extra_map)
{
    TL::Symbol sym;
    extra_map_replacements_t::iterator it;
    if (n.is_null())
    {
        return n;
    }
    else if ((sym = n.get_symbol()).is_valid()
            && (it = extra_map.find(sym)) != extra_map.end())
    {
        Nodecl::NodeclBase result = Nodecl::Symbol::make(
                it->second,
                n.get_filename(),
                n.get_line());

        result.set_type(n.get_type());

        return result;
    }
    else
    {
        TL::ObjectList<Nodecl::NodeclBase> children = n.children();
        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
                it != children.end();
                it++)
        {
            *it = replace_arguments_with_extra(*it, extra_map);
        }

        n.rechild(children);

        return n;
    }
}

void LoweringVisitor::visit(const Nodecl::OpenMP::TaskCall& construct)
{
    Nodecl::FunctionCall function_call = construct.get_call().as<Nodecl::FunctionCall>();
    ERROR_CONDITION(!function_call.get_called().is<Nodecl::Symbol>(), "Invalid ASYNC CALL!", 0);

    TL::Symbol called_sym = function_call.get_called().get_symbol();

    std::cerr << construct.get_locus() << ": note: call to task function '" << called_sym.get_qualified_name() << "'" << std::endl;

    // Get parameters outline info
    Nodecl::NodeclBase parameters_environment = construct.get_environment();
    OutlineInfo parameters_outline_info(parameters_environment, /* function_task */ true);

    TaskEnvironmentVisitor task_environment;
    task_environment.walk(parameters_environment);

    // Fill arguments outline info using parameters
    OutlineInfo arguments_outline_info;

    // Copy device information from parameters_outline_info to arguments_outline_info
    TL::ObjectList<std::string> _device_names = parameters_outline_info.get_device_names();
    for (TL::ObjectList<std::string>::const_iterator it = _device_names.begin();
            it != _device_names.end();
            it++)
    {
        arguments_outline_info.add_device_name(*it);
    }

    // This map associates every parameter symbol with its argument expression
    sym_to_argument_expr_t param_to_arg_expr;
    param_sym_to_arg_sym_t param_sym_to_arg_sym;
    Nodecl::List arguments = function_call.get_arguments().as<Nodecl::List>();
    fill_map_parameters_to_arguments(called_sym, arguments, param_to_arg_expr);

    Scope sc = construct.retrieve_context();
    TL::ObjectList<TL::Symbol> new_arguments;

    // If the current function is a non-static function and It is member of a
    // class, the first argument of the arguments list represents the object of
    // this class
    if (IS_CXX_LANGUAGE
            && !called_sym.is_static()
            && called_sym.is_member())
    {
        Nodecl::NodeclBase class_object = *(arguments.begin());
        TL::Symbol this_symbol = called_sym.get_scope().get_symbol_from_name("this");
        ERROR_CONDITION(!this_symbol.is_valid(), "Invalid symbol", 0);

        Counter& arg_counter = CounterManager::get_counter("nanos++-outline-arguments");
        std::stringstream ss;
        ss << "mcc_arg_" << (int)arg_counter;
        TL::Symbol new_symbol = sc.new_symbol(ss.str());
        arg_counter++;

        new_symbol.get_internal_symbol()->kind = SK_VARIABLE;
        new_symbol.get_internal_symbol()->type_information = this_symbol.get_type().get_internal_type();

        new_arguments.append(new_symbol);

        OutlineDataItem& argument_outline_data_item = arguments_outline_info.get_entity_for_symbol(new_symbol);
        // This is a special kind of shared
        argument_outline_data_item.set_sharing(OutlineDataItem::SHARING_CAPTURE_ADDRESS);

        argument_outline_data_item.set_base_address_expression(
                Nodecl::Reference::make(
                    class_object,
                    new_symbol.get_type(),
                    function_call.get_filename(),
                    function_call.get_line()));
    }

    OutlineInfoRegisterEntities outline_register_entities(arguments_outline_info, sc);

    extra_map_replacements_t extra_map_replacements;

    TL::ObjectList<OutlineDataItem*> data_items = parameters_outline_info.get_data_items();
    for (sym_to_argument_expr_t::iterator it = param_to_arg_expr.begin();
            it != param_to_arg_expr.end();
            it++)
    {
        // We search by parameter position here
        ObjectList<OutlineDataItem*> found = data_items.find(
                lift_pointer(functor(outline_data_item_get_parameter_position)),
                it->first.get_parameter_position_in(called_sym));

        if (found.empty())
        {
            internal_error("%s: error: cannot find parameter '%s' in OutlineInfo",
                    arguments.get_locus().c_str(),
                    it->first.get_name().c_str());
        }

        Counter& arg_counter = CounterManager::get_counter("nanos++-outline-arguments");
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            // Create a new variable holding the value of the argument
            std::stringstream ss;
            ss << "mcc_arg_" << (int)arg_counter;
            TL::Symbol new_symbol = sc.new_symbol(ss.str());
            arg_counter++;

            // FIXME - Wrap this sort of things
            new_symbol.get_internal_symbol()->kind = SK_VARIABLE;
            new_symbol.get_internal_symbol()->type_information = it->first.get_type().get_internal_type();

            param_sym_to_arg_sym[it->first] = new_symbol;

            new_arguments.append(new_symbol);

            OutlineDataItem& parameter_outline_data_item = parameters_outline_info.get_entity_for_symbol(it->first);

            outline_register_entities.add_capture_with_value(new_symbol, it->second);

            OutlineDataItem& argument_outline_data_item = arguments_outline_info.get_entity_for_symbol(new_symbol);
            copy_outline_data_item(argument_outline_data_item, parameter_outline_data_item, param_to_arg_expr);

            if (parameter_outline_data_item.get_directionality() != OutlineDataItem::DIRECTIONALITY_NONE)
            {
                argument_outline_data_item.set_base_address_expression(it->second);
            }
        }
        else
        {
            // Create a new variable holding the address of the dependency
            std::stringstream ss;
            ss << "mcc_arg_" << (int)arg_counter;
            TL::Symbol new_symbol = sc.new_symbol(ss.str());
            arg_counter++;

            // FIXME - Wrap this sort of things
            new_symbol.get_internal_symbol()->kind = SK_VARIABLE;
            new_symbol.get_internal_symbol()->type_information = no_ref(it->first.get_type().get_internal_type());

            new_arguments.append(new_symbol);

            OutlineDataItem& parameter_outline_data_item = parameters_outline_info.get_entity_for_symbol(it->first);

            if (parameter_outline_data_item.get_directionality() == OutlineDataItem::DIRECTIONALITY_NONE)
            {
                outline_register_entities.add_capture_with_value(new_symbol, it->second);
                param_sym_to_arg_sym[it->first] = new_symbol;
            }
            else
            {
                // Create a new variable holding the base symbol of the data-reference of the argument
                DataReference data_ref(it->second);
                if (!data_ref.is_valid())
                {
                    error_printf("%s: error: actual argument '%s' must be a data-reference "
                            "because it is associated to dependence dummy argument '%s'\n",
                            construct.get_locus().c_str(),
                            it->second.prettyprint().c_str(),
                            it->first.get_name().c_str());
                    give_up_task_call(construct);
                    return;
                }

                new_symbol.get_internal_symbol()->type_information
                    = data_ref.get_base_symbol().get_type().get_internal_type();

                OutlineDataItem& argument_outline_data_item = arguments_outline_info.get_entity_for_symbol(new_symbol);
                // This is a special kind of shared
                argument_outline_data_item.set_sharing(OutlineDataItem::SHARING_CAPTURE_ADDRESS);
                argument_outline_data_item.set_field_type(TL::Type::get_void_type().get_pointer_to());

                argument_outline_data_item.set_base_address_expression(
                        // The argument may not be suitable for a base address
                        array_section_to_array_element(it->second));

                param_sym_to_arg_sym[data_ref.get_base_symbol()] = new_symbol;
                extra_map_replacements[data_ref.get_base_symbol()] = new_symbol;

                TL::Type in_outline_type = outline_register_entities.add_extra_dimensions(
                        data_ref.get_base_symbol(),
                        data_ref.get_base_symbol().get_type());

                add_extra_dimensions_for_arguments(it->second, outline_register_entities,
                        extra_map_replacements, construct.retrieve_context());

                if (!in_outline_type.is_any_reference())
                    in_outline_type = in_outline_type.get_lvalue_reference_to();

                argument_outline_data_item.set_in_outline_type(in_outline_type);

                // Copy what must be copied from the parameter info
                copy_outline_data_item(argument_outline_data_item, parameter_outline_data_item, param_to_arg_expr);

            }
        }
    }

    // Now fix again arguments of the outline
    data_items = arguments_outline_info.get_data_items();
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        TL::Symbol sym = (*it)->get_symbol();
        if (!sym.is_valid())
            continue;

        TL::Type updated_type = rewrite_type_in_outline((*it)->get_in_outline_type(), param_sym_to_arg_sym);

        sym.get_internal_symbol()->type_information = updated_type.get_internal_type();
        (*it)->set_in_outline_type(updated_type);
    }

    TL::Symbol alternate_name;
    if (!function_call.get_alternate_name().is_null())
    {
        alternate_name = function_call.get_alternate_name().get_symbol();
    }

    // Craft a new function call with the new mcc_arg_X symbols
    TL::ObjectList<TL::Symbol>::iterator args_it = new_arguments.begin();
    TL::ObjectList<Nodecl::NodeclBase> arg_list;

    // If the current function is a non-static function and It is member of a
    // class, the first argument of the arguments list represents the object of
    // this class
    if (IS_CXX_LANGUAGE
            && !called_sym.is_static()
            && called_sym.is_member())
    {
        // The symbol which represents the object 'this' must be dereferenced!
        Nodecl::NodeclBase nodecl_arg = Nodecl::Dereference::make(
                Nodecl::Symbol::make(*args_it,
                    function_call.get_filename(),
                    function_call.get_line()),
                args_it->get_type().points_to(),
                function_call.get_filename(),
                function_call.get_line());

        arg_list.append(nodecl_arg);
        args_it++;
    }

    for (sym_to_argument_expr_t::iterator params_it = param_to_arg_expr.begin();
            params_it != param_to_arg_expr.end();
            params_it++, args_it++)
    {
        Nodecl::NodeclBase nodecl_arg;

        if (!IS_FORTRAN_LANGUAGE)
        {
            nodecl_arg = Nodecl::Symbol::make(*args_it,
                    function_call.get_filename(),
                    function_call.get_line());
            nodecl_arg.set_type(args_it->get_type());
        }
        else
        {
            nodecl_arg = replace_arguments_with_extra(
                    params_it->second.shallow_copy(),
                    extra_map_replacements);
        }

        // We must respect symbols in Fortran because of optional stuff
        if (IS_FORTRAN_LANGUAGE
                // If the alternate name lacks prototype, do not add keywords
                // here
                && !(alternate_name.is_valid()
                    && alternate_name.get_type().lacks_prototype()))
        {
            Nodecl::Symbol nodecl_param = Nodecl::Symbol::make(
                    params_it->first,
                    function_call.get_filename(),
                    function_call.get_line());

            nodecl_arg = Nodecl::FortranNamedPairSpec::make(
                    nodecl_param,
                    nodecl_arg,
                    function_call.get_filename(),
                    function_call.get_line());
        }

        arg_list.append(nodecl_arg);
    }

    Nodecl::List nodecl_arg_list = Nodecl::List::make(arg_list);

    Nodecl::NodeclBase called = function_call.get_called().shallow_copy();
    Nodecl::NodeclBase function_form = nodecl_null();
    Symbol called_symbol = called.get_symbol();
    if (!called_symbol.is_valid()
            && called_symbol.get_type().is_template_specialized_type())
    {
        function_form =
            Nodecl::CxxFunctionFormTemplateId::make(
                    function_call.get_filename(),
                    function_call.get_line());

        TemplateParameters template_args =
            called.get_template_parameters();
        function_form.set_template_parameters(template_args);
    }

    Nodecl::NodeclBase expr_statement =
        Nodecl::ExpressionStatement::make(
                Nodecl::FunctionCall::make(
                    called,
                    nodecl_arg_list,
                    function_call.get_alternate_name().shallow_copy(),
                    function_form,
                    Type::get_void_type(),
                    function_call.get_filename(),
                    function_call.get_line()),
                function_call.get_filename(),
                function_call.get_line());

    TL::ObjectList<Nodecl::NodeclBase> list_stmt;
    list_stmt.append(expr_statement);

    Nodecl::NodeclBase statements = Nodecl::List::make(list_stmt);

    construct.as<Nodecl::OpenMP::Task>().set_statements(statements);

    Symbol function_symbol = Nodecl::Utils::get_enclosing_function(construct);

    //Copy implementation table from parameter_outline_info to arguments_outline_info
    OutlineInfo::implementation_table_t implementation_table = parameters_outline_info.get_implementation_table();
    for (OutlineInfo::implementation_table_t::iterator it = implementation_table.begin();
            it != implementation_table.end();
            ++it)
    {
        arguments_outline_info.add_implementation(it->first, it->second);
    }

    emit_async_common(
            construct,
            function_symbol,
            called_symbol,
            statements,
            /* priority */ Nodecl::NodeclBase::null(),
            /* is_untied */ false,
            arguments_outline_info);
}

} }
