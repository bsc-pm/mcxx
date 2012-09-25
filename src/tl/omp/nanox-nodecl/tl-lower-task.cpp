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

static TL::Symbol declare_const_wd_type(int num_devices, Nodecl::NodeclBase construct)
{
    // FIXME: Move this to the class! 
    static std::map<int, Symbol> _map;
    std::map<int, Symbol>::iterator it = _map.find(num_devices);

    if (it == _map.end())
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

        _map[num_devices] = new_class_symbol;

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
        const ObjectList<std::string>& device_names,
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
    TL::Symbol const_wd_type = declare_const_wd_type(num_devices, construct);

    Source alignment, props_init, num_copies;

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
    num_copies << "0";

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

    // Fill device information
    DeviceHandler device_handler = DeviceHandler::get_device_handler();
    for (ObjectList<std::string>::const_iterator it = device_names.begin();
            it != device_names.end();
            ++it)
    {
        Source ancillary_device_description, device_description, aux_fortran_init;

        if (it != device_names.begin())
            ancillary_device_descriptions <<  ", ";

        std::string device_name = *it;
        DeviceProvider* device = device_handler.get_device(device_name);

        ERROR_CONDITION(device == NULL, " Device '%s' has not been loaded.", device_name.c_str());

        // FIXME: Can it be done only once?
        DeviceDescriptorInfo info(outline_name);
        device->get_device_descriptor(info, ancillary_device_description, device_description, aux_fortran_init);

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
    const int overallocation_mask = overallocation_alignment - 1;

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

    const_wd_info << fill_const_wd_info(
            struct_arg_type_name,
            outline_name,
            is_untied,
            /* mandatory_creation */ 0,
            device_names,
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

    // Outline
    DeviceHandler device_handler = DeviceHandler::get_device_handler();
    for (TL::ObjectList<std::string>::const_iterator it = device_names.begin();
            it != device_names.end();
            it++)
    {
        std::string device_name = *it;
        DeviceProvider* device = device_handler.get_device(device_name);

        ERROR_CONDITION(device == NULL, " Device '%s' has not been loaded.", device_name.c_str());

        // FIXME: Can it be done only once?
        CreateOutlineInfo info(outline_name, outline_info, statements, structure_symbol);
        Nodecl::NodeclBase outline_placeholder;
        Nodecl::Utils::SymbolMap *symbol_map = NULL;

        device->create_outline(info, outline_placeholder, symbol_map);

        Nodecl::NodeclBase outline_statements_code = Nodecl::Utils::deep_copy(statements, outline_placeholder, *symbol_map);
        delete symbol_map;

        outline_placeholder.replace(outline_statements_code);
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
        <<     "nanos_wd_t wd = (nanos_wd_t)0;"
        <<     copy_ol_decl
        <<     "nanos_err_t " << err_name <<";"
        <<     err_name << " = nanos_create_wd_compact(&wd, &(nanos_wd_const_data.base), &nanos_wd_dyn_props, " 
        <<                 struct_size << ", (void**)&ol_args, nanos_current_wd(),"
        <<                 copy_ol_arg << ");"
        <<     "if (" << err_name << " != NANOS_OK) nanos_handle_error (" << err_name << ");"
        <<     "if (wd != (nanos_wd_t)0)"
        <<     "{"
                  // This is a placeholder because arguments are filled using the base language (possibly Fortran)
        <<        statement_placeholder(fill_outline_arguments_tree)
        <<        fill_dependences_outline
        <<        copy_ol_setup
        <<        err_name << " = nanos_submit(wd, " << num_dependences << ", dependences, (nanos_team_t)0);"
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

    Symbol function_symbol = Nodecl::Utils::get_enclosing_function(construct);

    emit_async_common(
            construct,
            function_symbol, 
            statements, 
            task_environment.priority, 
            task_environment.is_untied, 

            outline_info);
}


namespace {
    void fill_extra_ref_assumed_size(Source &extra_ref, TL::Type t)
    {
        if (t.is_array())
        {
            fill_extra_ref_assumed_size(extra_ref, t.array_element());

            Source current_dims;
            Nodecl::NodeclBase lower_bound, upper_bound;
            t.array_get_bounds(lower_bound, upper_bound);

            if (lower_bound.is_null())
            {
                current_dims << "1:1";
            }
            else if (upper_bound.is_null())
            {
                current_dims << as_expression(lower_bound) << ":" << as_expression(lower_bound);
            }
            else
            {
                current_dims << as_expression(lower_bound) << ":" << as_expression(upper_bound);
            }

            extra_ref.append_with_separator(current_dims, ",");
        }
    }
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
                case OutlineDataItem::SHARING_SHARED_CAPTURED_PRIVATE:
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
                                // Plain assignment is enough
                                fill_outline_arguments << 
                                    "ol_args->" << (*it)->get_field_name() << " = " << as_symbol((*it)->get_symbol()) << ";"
                                    ;
                                fill_immediate_arguments << 
                                    "imm_args." << (*it)->get_field_name() << " = " << as_symbol((*it)->get_symbol()) << ";"
                                    ;
                            }
                        }
                        break;
                    }
                case OutlineDataItem::SHARING_SHARED:
                case OutlineDataItem::SHARING_SHARED_PRIVATE:
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
                        fill_outline_arguments 
                            << "ol_args->" << (*it)->get_field_name() << " = " << as_expression( (*it)->get_shared_expression()) << ";"
                            ;
                        fill_immediate_arguments 
                            << "imm_args." << (*it)->get_field_name() << " = " << as_expression( (*it)->get_shared_expression()) << ";"
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
        int lower_bound_index = 0;
        int upper_bound_index = 0;
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
                case OutlineDataItem::SHARING_SHARED_CAPTURED_PRIVATE:
                    {
                        TL::Type t = sym.get_type();
                        if (t.is_any_reference())
                            t = t.references_to();

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
                        break;
                    }
                case OutlineDataItem::SHARING_SHARED:
                case OutlineDataItem::SHARING_SHARED_PRIVATE:
                case OutlineDataItem::SHARING_REDUCTION: // Reductions are passed as if they were shared variables
                    {
                        Source extra_ref;
                        TL::Type t = sym.get_type();
                        if (sym.is_parameter()
                                && t.is_any_reference())
                        {
                            t = t.references_to();
                            if (t.is_array()
                                    && !t.array_requires_descriptor()
                                    && t.array_get_size().is_null())
                            {
                                // This is an assumed-size
                                Source array_section;
                                fill_extra_ref_assumed_size(array_section, t);
                                extra_ref << "(" << array_section << ")";
                            }
                        }

                        TL::Symbol ptr_of_sym = get_function_ptr_of((*it)->get_symbol(),
                                ctr.retrieve_context());

                        fill_outline_arguments << 
                            "ol_args %" << (*it)->get_field_name() << " => " 
                            << ptr_of_sym.get_name() << "( " << (*it)->get_symbol().get_name() << extra_ref << ") \n"
                            ;
                        fill_immediate_arguments << 
                            "imm_args % " << (*it)->get_field_name() << " => " 
                            << ptr_of_sym.get_name() << "( " << (*it)->get_symbol().get_name() << extra_ref << ") \n"
                            ;

                        break;
                    }
                case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
                    {
                        TL::Symbol ptr_of_sym = get_function_ptr_of(
                                (*it)->get_shared_expression().get_type(),
                                ctr.retrieve_context());

                        fill_outline_arguments
                            << "ol_args %" << (*it)->get_field_name() << " => "
                            << ptr_of_sym.get_name() << "(" << as_expression( (*it)->get_shared_expression()) << ")\n"
                            ;
                        fill_immediate_arguments
                            << "imm_args % " << (*it)->get_field_name() << " => "
                            << ptr_of_sym.get_name() << "(" << as_expression( (*it)->get_shared_expression()) << ")\n"
                            ;

                        // Best effort, this may fail sometimes
                        DataReference data_ref((*it)->get_shared_expression());
                        if (!data_ref.is_valid())
                        {
                            std::cerr
                                << (*it)->get_shared_expression().get_locus()
                                << ": warning: an argument is not a valid data-reference, compilation is likely to fail"
                                << std::endl;
                        }
                        break;
                    }
                case OutlineDataItem::SHARING_PRIVATE:
                    {
                        // Do nothing
                        if (sym.is_allocatable())
                        {
                            TL::Type t = sym.get_type();
                            if (t.is_any_reference())
                                t = t.references_to();

                            fill_allocatable_dimensions(
                                    sym, sym.get_type(),
                                    0, t.get_num_dimensions(),
                                    fill_outline_arguments, 
                                    fill_immediate_arguments, 
                                    lower_bound_index, 
                                    upper_bound_index);
                        }
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

static void fill_dimensions(
        int n_dims, 
        int actual_dim, 
        int current_dep_num,
        Nodecl::NodeclBase * dim_sizes, 
        Type dep_type, 
        Source& dims_description, 
        Source& dependency_regions_code, 
        Scope sc);

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
            int output = (dir & OutlineDataItem::COPY_IN) == OutlineDataItem::COPY_OUT;

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
            OutlineDataItem::Directionality dir = (*it)->get_directionality();
            if (dir == OutlineDataItem::DIRECTIONALITY_NONE)
                continue;

            TL::ObjectList<Nodecl::NodeclBase> deps = (*it)->get_dependences();
            for (ObjectList<Nodecl::NodeclBase>::iterator dep_it = deps.begin();
                    dep_it != deps.end();
                    dep_it++, current_dep_num++)
            {
                TL::DataReference dep_expr(*dep_it);

                Source dependency_offset,
                       dependency_flags,
                       dependency_flags_in,
                       dependency_flags_out,
                       dependency_flags_concurrent,
                       dependency_flags_commutative,
                       dependency_size;

                Source dep_expr_addr;
                dep_expr_addr << as_expression(dep_expr.get_base_address());
                dependency_size << as_expression(dep_expr.get_sizeof());

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
                int commutative = ((dir & OutlineDataItem::DIRECTIONALITY_CONCURRENT) == OutlineDataItem::DIRECTIONALITY_COMMUTATIVE);

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
                    dimension_sizes[dim] = dependency_base_type.array_get_size();
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

                    dependency_offset << 0;
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
                        size = contiguous_array_type.array_get_size();
                    }

                    dependency_offset
                        << "(char*)(" << dep_expr_addr << ") - (char*)&(" << as_symbol(dep_expr.get_base_symbol()) << ")";
                        ;

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
                                dimension_sizes,
                                dependency_type,
                                dims_description,
                                dependency_regions,
                                dep_expr.retrieve_context());
                    }
                }

                int num_dimension_items = num_dimensions;
                if (num_dimension_items == 0) 
                    num_dimension_items = 1;

                if (IS_C_LANGUAGE
                        || IS_CXX_LANGUAGE)
                {
                    dependency_init
                        << "{"
                        << "(void*)" << arguments_accessor << (*it)->get_field_name() << ","
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
                                   << as_expression(dep_expr.get_base_address()) << ";"
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
        }
    }
    else
    {
        result_src
            << "nanos_dependence_t dependences[" << num_deps << "]";

        // We only initialize in C/C++, in Fortran we will make a set of assignments
        if (IS_C_LANGUAGE
                || IS_CXX_LANGUAGE)
        {
            result_src << "= {"
            << dependency_init
            << "}";
        }

        result_src << ";"
            ;

        int current_dep_num = 0;
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

                Source current_dependency_init,
                       dependency_offset,
                       dependency_flags,
                       dependency_flags_in,
                       dependency_flags_out,
                       dependency_flags_concurrent,
                       dependency_flags_commutative,
                       dependency_size;

                if (IS_C_LANGUAGE
                        || IS_CXX_LANGUAGE)
                {
                    current_dependency_init
                        << "{"
                        << "(void**)&(" << arguments_accessor << (*it)->get_field_name() << "),"
                        << dependency_offset << ","
                        << dependency_flags << ","
                        << dependency_size
                        << "}"
                        ;
                }
                else if (IS_FORTRAN_LANGUAGE)
                {
                    // We use plain assignments in Fortran
                    result_src
                        << "dependences[" << current_dep_num << "].address = " << as_expression(dep_expr.get_base_address()) << ";"
                        << "dependences[" << current_dep_num << "].offset = " << dependency_offset << ";"
                        << "dependences[" << current_dep_num << "].size = " << dependency_size << ";"
                        << "dependences[" << current_dep_num << "].flags.input = " << dependency_flags_in << ";"
                        << "dependences[" << current_dep_num << "].flags.output = " << dependency_flags_out << ";"
                        << "dependences[" << current_dep_num << "].flags.can_rename = 0;"
                        << "dependences[" << current_dep_num << "].flags.concurrent = " << dependency_flags_concurrent << ";"
                        << "dependences[" << current_dep_num << "].flags.commutative = " << dependency_flags_commutative << ";"
                        ;
                }

                Source dep_expr_addr;
                dep_expr_addr << as_expression(dep_expr.get_base_address());
                dependency_size << as_expression(dep_expr.get_sizeof());

                dependency_offset
                    << "((char*)(" << dep_expr_addr << ") - " << "(char*)" << arguments_accessor << (*it)->get_field_name() << ")"
                    ;

                if (Nanos::Version::interface_is_at_least("master", 5001))
                {
                    dependency_flags 
                        << "{" 
                        << dependency_flags_in << "," 
                        << dependency_flags_out << ", "
                        << /* renaming has not yet been implemented */ "0, " 
                        << dependency_flags_concurrent
                        << "}"
                        ;
                }
                else
                {
                    dependency_flags 
                        << "{" 
                        << dependency_flags_in << "," 
                        << dependency_flags_out << ", "
                        << /* renaming has not yet been implemented */ "0, " 
                        << "}"
                        ;
                }

                int concurrent = ((dir & OutlineDataItem::DIRECTIONALITY_CONCURRENT) == OutlineDataItem::DIRECTIONALITY_CONCURRENT);
                int commutative = ((dir & OutlineDataItem::DIRECTIONALITY_CONCURRENT) == OutlineDataItem::DIRECTIONALITY_COMMUTATIVE);

                dependency_flags_in << (((dir & OutlineDataItem::DIRECTIONALITY_IN) == OutlineDataItem::DIRECTIONALITY_IN) || concurrent);
                dependency_flags_out << (((dir & OutlineDataItem::DIRECTIONALITY_OUT) == OutlineDataItem::DIRECTIONALITY_OUT) || concurrent);
                dependency_flags_concurrent << concurrent;
                dependency_flags_commutative << commutative;

                dependency_init.append_with_separator(current_dependency_init, ",");
            }
        }
    }
}

void LoweringVisitor::fill_dimensions(
        int n_dims, 
        int current_dim, 
        int current_dep_num,
        Nodecl::NodeclBase * dim_sizes, 
        Type dep_type, 
        Source& dims_description, 
        Source& dependency_regions_code, 
        Scope sc)
{
    // We do not handle the contiguous dimension here
    if (current_dim > 1)
    {
        fill_dimensions(n_dims, current_dim - 1, current_dep_num, dim_sizes, dep_type.array_element(), dims_description, dependency_regions_code, sc);

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
            size = dep_type.array_get_size();
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

static Nodecl::NodeclBase rewrite_single_dependency(Nodecl::NodeclBase node, const sym_to_argument_expr_t& map)
{
    if (node.is_null())
        return node;

    if (node.is<Nodecl::Symbol>())
    {
        sym_to_argument_expr_t::const_iterator it = map.find(node.get_symbol());

        if (it != map.end())
        {
            return (it->second.shallow_copy());
        }
    }

    TL::ObjectList<Nodecl::NodeclBase> children = node.children();
    for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
            it != children.end();
            it++)
    { 
        *it = rewrite_single_dependency(*it, map);
    }
    node.rechild(children);

    return node;
}

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

static void copy_outline_data_item(
        OutlineDataItem& dest_info, 
        const OutlineDataItem& source_info,
        const sym_to_argument_expr_t& param_to_arg_expr)
{
    dest_info.set_sharing(source_info.get_sharing());
    dest_info.set_allocation_policy(source_info.get_allocation_policy());
    dest_info.set_directionality(source_info.get_directionality());

    dest_info.set_field_type(source_info.get_field_type());

    FORTRAN_LANGUAGE()
    {
        // We need an additional pointer due to pass by reference in Fortran
        dest_info.set_field_type(dest_info.get_field_type().get_pointer_to());
    }

    // Update dependences to reflect arguments as well
    dest_info.get_dependences() = rewrite_dependences(source_info.get_dependences(), param_to_arg_expr);
}

static void fill_map_parameters_to_arguments(
        TL::Symbol function,
        Nodecl::List arguments, 
        sym_to_argument_expr_t& param_to_arg_expr)
{
    int i = 0;
    for (Nodecl::List::iterator it = arguments.begin();
            it != arguments.end();
            it++, i++)
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
            ERROR_CONDITION((function.get_related_symbols().size() <= i), "Too many parameters", 0);
            TL::Symbol parameter = function.get_related_symbols()[i];
            param_to_arg_expr[parameter] = *it;
        }
    }
}

static int outline_data_item_get_parameter_position(const OutlineDataItem& outline_data_item)
{
    TL::Symbol sym = outline_data_item.get_symbol();

    ERROR_CONDITION(!sym.is_parameter(), "This symbol must be a parameter", 0);

    return sym.get_parameter_position();
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
    Nodecl::List arguments = function_call.get_arguments().as<Nodecl::List>();
    fill_map_parameters_to_arguments(called_sym, arguments, param_to_arg_expr);

    TL::ObjectList<TL::Symbol> new_arguments;
    Scope sc = construct.retrieve_context();
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
        std::stringstream ss;
        ss << "mcc_arg_" << (int)arg_counter;
        TL::Symbol new_symbol = sc.new_symbol(ss.str());
        arg_counter++;

        // FIXME - Wrap this sort of things
        new_symbol.get_internal_symbol()->kind = SK_VARIABLE;
        new_symbol.get_internal_symbol()->type_information = no_ref(it->first.get_internal_symbol()->type_information);

        new_arguments.append(new_symbol);

        OutlineDataItem& parameter_outline_data_item = parameters_outline_info.get_entity_for_symbol(it->first);

        OutlineDataItem& argument_outline_data_item = arguments_outline_info.get_entity_for_symbol(new_symbol);

        // Copy what must be copied from the parameter info
        copy_outline_data_item(argument_outline_data_item, parameter_outline_data_item, param_to_arg_expr);

        // This is a special kind of shared
        argument_outline_data_item.set_sharing(OutlineDataItem::SHARING_CAPTURE_ADDRESS);

        if (IS_FORTRAN_LANGUAGE)
        {
            argument_outline_data_item.set_field_type(TL::Type::get_void_type().get_pointer_to());
        }

        argument_outline_data_item.set_shared_expression(it->second);
    }

    TL::Symbol alternate_name;
    if (!function_call.get_alternate_name().is_null())
    {
        alternate_name = function_call.get_alternate_name().get_symbol();
    }

    // Craft a new function call with the new mcc_arg_X symbols
    TL::ObjectList<TL::Symbol>::iterator args_it = new_arguments.begin();
    TL::ObjectList<Nodecl::NodeclBase> arg_list;
    for (sym_to_argument_expr_t::iterator params_it = param_to_arg_expr.begin();
            params_it != param_to_arg_expr.end();
            params_it++, args_it++)
    {
        Nodecl::NodeclBase nodecl_arg = Nodecl::Symbol::make(*args_it, 
                function_call.get_filename(), 
                function_call.get_line());
        nodecl_arg.set_type( args_it->get_type() );

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

    Nodecl::NodeclBase statements =
        Nodecl::List::make(
                Nodecl::Context::make(
                    Nodecl::List::make(list_stmt),
                    function_call.retrieve_context(),
                    function_call.get_filename(),
                    function_call.get_line()
                    ));

    construct.as<Nodecl::OpenMP::Task>().set_statements(statements);

    Symbol function_symbol = Nodecl::Utils::get_enclosing_function(construct);

    emit_async_common(
            construct,
            function_symbol,
            statements,
            /* priority */ Nodecl::NodeclBase::null(),
            /* is_untied */ false,
            arguments_outline_info);
}

} }
