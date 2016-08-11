/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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

#include <errno.h>

#include <fstream>

#include "cxx-diagnostic.h"
#include "tl-devices.hpp"
#include "tl-compilerpipeline.hpp"
#include "tl-multifile.hpp"
#include "tl-source.hpp"
#include "codegen-phase.hpp"
#include "codegen-cxx.hpp"
#include "cxx-cexpr.h"
#include "cxx-driver-utils.h"
#include "cxx-process.h"
#include "cxx-cexpr.h"

#include "nanox-fpga.hpp"

#include "cxx-nodecl.h"
#include "cxx-graphviz.h"

#include "tl-nanos.hpp"
#include "tl-symbol-utils.hpp"

using namespace TL;
using namespace TL::Nanox;

const std::string DeviceFPGA::HLS_VPREF = "_hls_var_";
const std::string DeviceFPGA::HLS_I = HLS_VPREF + "i";
const std::string DeviceFPGA::hls_in = HLS_VPREF + "in";
const std::string DeviceFPGA::hls_out = HLS_VPREF + "out";


static std::string fpga_outline_name(const std::string &name)
{
    return "fpga_" + name;
}

UNUSED_PARAMETER static void print_ast_dot(const Nodecl::NodeclBase &node)
{
    std::cerr << std::endl << std::endl;
    ast_dump_graphviz(nodecl_get_ast(node.get_internal_nodecl()), stderr);
    std::cerr << std::endl << std::endl;
}

void DeviceFPGA::create_outline(CreateOutlineInfo &info,
        Nodecl::NodeclBase &outline_placeholder,
        Nodecl::NodeclBase &output_statements,
        Nodecl::Utils::SimpleSymbolMap* &symbol_map)
{
    if (IS_FORTRAN_LANGUAGE)
        fatal_error("Fortran for FPGA devices is not supported yet\n");

    // Unpack DTO
    const std::string& device_outline_name = fpga_outline_name(info._outline_name);
    const Nodecl::NodeclBase& original_statements = info._original_statements;
    const TL::Symbol& arguments_struct = info._arguments_struct;
    const TL::Symbol& called_task = info._called_task;

    symbol_map = new Nodecl::Utils::SimpleSymbolMap(&_copied_fpga_functions);

    TL::Symbol current_function = original_statements.retrieve_context().get_related_symbol();
    if (current_function.is_nested_function())
    {
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            fatal_printf_at(original_statements.get_locus(), "nested functions are not supported\n");
    }

    // Add the user function to the intermediate file -> to HLS
    if (called_task.is_valid())//is a function task
    {
        if ( (IS_C_LANGUAGE || IS_CXX_LANGUAGE) && !called_task.get_function_code().is_null())
        {


            if (_copied_fpga_functions.map(called_task) == called_task)
            {
                //new task-> add it to the list
                TL::Symbol new_function = SymbolUtils::new_function_symbol_for_deep_copy(
                        called_task,
                        called_task.get_name() + "_hls");

                _copied_fpga_functions.add_map(called_task, new_function);
                _fpga_file_code.append (Nodecl::Utils::deep_copy(
                            called_task.get_function_code(),
                            called_task.get_scope(),
                            *symbol_map)
                        );
            }
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            fatal_error("There is no fortran support for FPGA devices\n");
        }
        else
        {
            fatal_error("Inline tasks not supported yet\n");
        }

    }
    Source unpacked_arguments, private_entities;

    TL::ObjectList<OutlineDataItem*> data_items = info._data_items;
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        switch ((*it)->get_sharing())
        {
            case OutlineDataItem::SHARING_PRIVATE:
                {
                    break;
                }
            case OutlineDataItem::SHARING_SHARED:
            case OutlineDataItem::SHARING_CAPTURE:
            case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
                {
                    TL::Type param_type = (*it)->get_in_outline_type();

                    Source argument;
                    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                    {
                        // Normal shared items are passed by reference from a pointer,
                        // derreference here
                        if ((*it)->get_sharing() == OutlineDataItem::SHARING_SHARED
                                && !(IS_CXX_LANGUAGE && (*it)->get_symbol().get_name() == "this"))
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
                    else
                    {
                        internal_error("running error", 0);
                    }

                    unpacked_arguments.append_with_separator(argument, ", ");
                    break;
                }
            case OutlineDataItem::SHARING_REDUCTION:
                {
                    WARNING_MESSAGE("Reductions are not tested for FPGA", "");
                    // Pass the original reduced variable as if it were a shared
                    Source argument;
                    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                    {
                        argument << "*(args." << (*it)->get_field_name() << ")";
                    }
                    else
                    {
                        internal_error("running error", 0);
                    }
                    unpacked_arguments.append_with_separator(argument, ", ");

                    //Seems that this is not needed anymore
//                    std::string name = (*it)->get_symbol().get_name();
//
//                    private_entities
//                        << "rdp_" << name << " = " << as_expression( (*it)->get_reduction_info()->get_identity()) << ";"
//                        ;
                    break;
                }
            default:
                {
                    std::cerr << "Warning: Cannot copy function code to the device file" << std::endl;
                }
        }
    }



    // Create the new unpacked function
    TL::Source dummy_init_statements, dummy_final_statements;
    TL::Symbol unpacked_function = new_function_symbol_unpacked(
            current_function,
            device_outline_name + "_unpacked",
            info,
            symbol_map,
            dummy_init_statements,
            dummy_final_statements);

    // The unpacked function must not be static and must have external linkage because
    // this function is called from the original source 
    symbol_entity_specs_set_is_static(unpacked_function.get_internal_symbol(), 0);
    if (IS_C_LANGUAGE)
    {
        symbol_entity_specs_set_linkage_spec(unpacked_function.get_internal_symbol(), "\"C\"");
    }

    Nodecl::NodeclBase unpacked_function_code, unpacked_function_body;
    SymbolUtils::build_empty_body_for_function(unpacked_function,
            unpacked_function_code,
            unpacked_function_body);

    Source fpga_params;
    //Only generate scalar parameter passing when it's necessary
    //FIXME: We are not generating any code to pass parameters right now
//    if (task_has_scalars(data_items))
//    {
//        fpga_params = fpga_param_code(info._data_items, symbol_map, called_task.get_scope());
//    }

    Source unpacked_source;
    unpacked_source
        << dummy_init_statements
        << private_entities
        << fpga_params
        << statement_placeholder(outline_placeholder)
        << dummy_final_statements
        ;

    Nodecl::NodeclBase new_unpacked_body =
        unpacked_source.parse_statement(unpacked_function_body);
    unpacked_function_body.replace(new_unpacked_body);

    Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, unpacked_function_code);

    // Add a declaration of the unpacked function symbol in the original source
    if (IS_CXX_LANGUAGE)
    {
        Nodecl::NodeclBase nodecl_decl = Nodecl::CxxDecl::make(
                /* optative context */ nodecl_null(),
                unpacked_function,
                original_statements.get_locus());
        Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, nodecl_decl);
    }

    // Create the outline function
    //The outline function has always only one parameter which name is 'args'
    ObjectList<std::string> structure_name;
    structure_name.append("args");

    //The type of this parameter is an struct (i. e. user defined type)
    ObjectList<TL::Type> structure_type;
    structure_type.append(TL::Type(
                get_user_defined_type(
                    arguments_struct.get_internal_symbol())).get_lvalue_reference_to());

    TL::Symbol outline_function = SymbolUtils::new_function_symbol(
            current_function,
            device_outline_name,
            TL::Type::get_void_type(),
            structure_name,
            structure_type);

    Nodecl::NodeclBase outline_function_code, outline_function_body;
    SymbolUtils::build_empty_body_for_function(outline_function,
            outline_function_code,
            outline_function_body);

    Source outline_src;
    Source instrument_before,
           instrument_after;

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

    outline_src
        << "{"
        <<      instrument_before
        <<      unpacked_function.get_qualified_name_for_expression(
                   /* in_dependent_context */
                   (current_function.get_type().is_template_specialized_type()
                    && current_function.get_type().is_dependent())
                ) << "(" << unpacked_arguments << ");"
        <<      instrument_after
        << "}"
        ;

    Nodecl::NodeclBase new_outline_body = outline_src.parse_statement(outline_function_body);
    outline_function_body.replace(new_outline_body);
    Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, outline_function_code);

    output_statements = Nodecl::EmptyStatement::make(
                original_statements.get_locus());
}

DeviceFPGA::DeviceFPGA()
    : DeviceProvider(std::string("fpga"))
{
    set_phase_name("Nanox FPGA support");
    set_phase_description("This phase is used by Nanox phases to implement FPGA device support");
}

void DeviceFPGA::pre_run(DTO& dto)
{
}

void DeviceFPGA::run(DTO& dto)
{
    DeviceProvider::run(dto);
}

bool DeviceFPGA::task_has_scalars(TL::ObjectList<OutlineDataItem*> & dataitems)
{
    for (ObjectList<OutlineDataItem*>::iterator it = dataitems.begin();
            it != dataitems.end();
            it++)
    {
        /*
         * We happily assume that everything that does not need a copy is a scalar
         * Which is true as long everything that is not a scalar is going to be copied
         * We could also use DataReference to check this
         * FIXME structs should be treated
         */
        if((*it)->get_copies().empty())
        {
            return true;
        }
    }
    return false;
}

void DeviceFPGA::get_device_descriptor(DeviceDescriptorInfo& info,
        Source &ancillary_device_description,
        Source &device_descriptor,
        Source &fortran_dynamic_init)
{

    const std::string& outline_name = fpga_outline_name(info._outline_name);
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

    bool without_template_args =
        !current_function.get_type().is_template_specialized_type()
        || current_function.get_scope().get_template_parameters()->is_explicit_specialization;

    TL::Scope function_scope = context.retrieve_context();
    std::string qualified_name = current_function.get_qualified_name(function_scope, without_template_args);

    // Restore the original name of the current function
    current_function.set_name(original_name);

    //get onto information
    ObjectList<Nodecl::NodeclBase> onto_clause = info._target_info.get_onto();
    Nodecl::Utils::SimpleSymbolMap param_to_args_map = info._target_info.get_param_arg_map();

    std::string acc_num = "-1";
    if (onto_clause.size() >= 1)
    {
        //TODO
        //Process list of values in onto clause. Multiple values mean that the task
        //can be run in several accelerators

        Nodecl::NodeclBase onto_acc = onto_clause[0];
        if (onto_clause.size() > 1)
        {
            warn_printf_at(onto_acc.get_locus(), "More than one argument in onto clause. Using only first one\n");
        }

        if (onto_clause[0].is_constant())
        {
            const_value_t *ct_val = onto_acc.get_constant();
            if (!const_value_is_integer(ct_val))
            {
                error_printf_at(onto_acc.get_locus(), "Constant is not integer type in onto clause\n");
            }
            else
            {
                int acc = const_value_cast_to_signed_int(ct_val);
                std::stringstream tmp_str;
                tmp_str << acc;
                acc_num = tmp_str.str();
            }
        }
        else
        {
            if (onto_acc.get_symbol().is_valid() ) {
                acc_num = as_symbol(onto_acc.get_symbol());
                //as_symbol(param_to_args_map.map(onto_acc.get_symbol()));
            }
        }
    }
    else
    {
        //warning??
    }



    if (!IS_FORTRAN_LANGUAGE)
    {
        // Extra cast for solving some issues of GCC 4.6.* and lowers (this
        // issues seem to be fixed in GCC 4.7 =D)
        std::string ref = IS_CXX_LANGUAGE ? "&" : "*";
        std::string extra_cast = "(void(*)(" + arguments_struct + ref + "))";

        Source args_name;
        args_name << outline_name << "_args";

        ancillary_device_description
            << comment("device argument type")
            << "static nanos_fpga_args_t " << args_name << ";"
            << args_name << ".outline = (void(*)(void*)) " << extra_cast << " &" << qualified_name << ";"
            << args_name << ".acc_num = " << acc_num << ";"
            ;
        device_descriptor
            << "{"
            << /* factory */ "&nanos_fpga_factory, &" << outline_name << "_args"
            << "}"
            ;
    }
    else
    {
        internal_error("Fortran is not supperted in fpga devices", 0);
    }

}

bool DeviceFPGA::remove_function_task_from_original_source() const
{
    return true;
}

//write/close intermediate files, free temporal nodes, etc.
void DeviceFPGA::phase_cleanup(DTO& data_flow)
{
    //XXX: We may need to create one file per function 
    //  if we want to work with multiple accelerators
    if (!_fpga_file_code.is_null())
    {
        std::string original_filename = TL::CompilationProcess::get_current_file().get_filename();
        std::string new_filename = "hls_" + original_filename;

        std::ofstream hls_file;

        hls_file.open(new_filename.c_str()); //open as output

        if (! hls_file.is_open())
        {
            fatal_error("%s: error: cannot open file '%s'. %s\n",
                    original_filename.c_str(),
                    new_filename.c_str(),
                    strerror(errno));
        }


        ObjectList<IncludeLine> includes = CurrentFile::get_top_level_included_files();

        for (ObjectList<IncludeLine>::iterator it = includes.begin(); it != includes.end(); it++)
        {
            hls_file << it->get_preprocessor_line() << std::endl;
        }
        hls_file << _fpga_file_code.prettyprint();
        hls_file.close();

#if 0
        FILE* ancillary_file = fopen(new_filename.c_str(), "w");
        if (ancillary_file == NULL)
        {
            fatal_error("%s: error: cannot open file '%s'. %s\n",
                    original_filename.c_str(),
                    new_filename.c_str(),
                    strerror(errno));
        }


        compilation_configuration_t* configuration = CURRENT_CONFIGURATION;
        ERROR_CONDITION (configuration == NULL, "The compilation configuration cannot be NULL", 0);

        // Make sure phases are loaded (this is needed for codegen)
        load_compiler_phases(configuration);

        TL::CompilationProcess::add_file(new_filename, "fpga");

        //Remove the intermediate source file
        ::mark_file_for_cleanup(new_filename.c_str());

        Codegen::CxxBase* phase = reinterpret_cast<Codegen::CxxBase*>(configuration->codegen_phase);

        phase->codegen_top_level(_fpga_file_code, ancillary_file);
        fclose(ancillary_file);
#endif
        // Do not forget the clear the code for next files
        _fpga_file_code = Nodecl::List();
    }
}

/*
 * We may need to set scalar arguments here, but not transfers
 */
Source DeviceFPGA::fpga_param_code(
        TL::ObjectList<OutlineDataItem*> &data_items,
        Nodecl::Utils::SymbolMap *symbol_map,//we may not need it
        Scope sc
        )
{

    //Nodecl::Utils::SimpleSymbolMap *ssmap = (Nodecl::Utils::SimpleSymbolMap*)symbol_map;
    //TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();
    Source args_src;

    /*
     * Get the fpga handle to write the data that we need.
     *
     * TODO: Make sure mmap + set arg does not break when we don't have scalar arguments
     */
    args_src
        << "int fd = open(\"/dev/mem\", NANOS_O_RDWR);"    //2=O_RDWR
//        << "unsigned int acc_addr = NANOS_AXI_BASE_ADDRESS;"
//        << "printf(\"address: %x\\n\", acc_addr);"
        << "unsigned int *acc_handle = "
        << "    (unsigned int *) mmap(0, NANOS_MMAP_SIZE,"     //0=NULL
        << "    NANOS_PROT_READ|NANOS_PROT_WRITE, NANOS_MAP_SHARED,"           //"        PROT_READ | PROT_WRITE, MAP_SHARED"
        << "    fd, NANOS_AXI_BASE_ADDRESS);"
    ;

    //set scalar arguments
    /* FIXME
     * We assume that the base address to set scalar parameters
     * (which appears to be true)
     * In fact all of this is defined in the 
     *   impl/pcores/foo_top_v1_00_a/include/xfoo_AXIlite.h
     * where foo is the name of the function and top_v1_00_a is the version name
     * This path may change so we are assuming base addres does not
     */

    /*
     * Parameter have an offset of 8 bytes with the preceding one (except for 64bit ones)
     * If any parameter is smaller than 32bit (4byte), padding is added in between
     * If a paramater is 64bit(aka long long int) another 32bit of padding are added 
     */

    int argIndex = 0x14/4;  //base address/(sizeof(int)=4)
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
//        print_dataItem_info(*it, sc);
        Symbol outline_symbol = symbol_map->map((*it)->get_symbol());
        const TL::ObjectList<OutlineDataItem::CopyItem> &copies = (*it)->get_copies();

        //if copies are empty, we need to set the scalar value
        if (copies.empty())
        {
            const Type & type = (*it)->get_field_type();

            args_src
                << "acc_handle[" << argIndex << "] = "
                << outline_symbol.get_name() << ";"
            ;

            //+1 for field +1 for padding
            //we are adding an index to int[] => addresses are 4x
            argIndex+=2;
            if (type.get_size() >= 8)
            {
                argIndex ++;
            }
        }

    }

    /*
     * There should be a control bus mapped at 0x40440000
     * This is true if function has non-scalar parameters
     * To start the device we must set the first bt to 1
     */
    args_src << "acc_handle[0] = 1;"
        << "munmap(acc_handle, NANOS_MMAP_SIZE);"
        << "close(fd);"
        ;


    return args_src;
}

void DeviceFPGA::add_hls_pragmas(
        Nodecl::NodeclBase &task,
        TL::ObjectList<OutlineDataItem*> &data_items
        )
{
    /*
     * Insert hls pragmas in order to denerate input/output connections
     * Every parameter needs a directive:
     * scalar: create plain wire connections:
     *      #pragma HLS INTERFACE ap_none port=VAR
     *      #pragma AP resource core=AXI_SLAVE variable=VAR metadata="-bus_bundle AXIlite"
     *
     * Array; create fifo port to be handled by axi stream
     *      #pragma HLS stream variable=VAR <-- NOT NEEDED
     *      #pragma HLS resource core=AXI4Stream variable=VAR
     *      #pragma HLS interface ap_fifo port=VAR
     *
     * For every task there is a control bus defined to kick the accelerator off:
     *
     * #pragma AP resource core=AXI_SLAVE variable=return metadata="-bus_bundle AXIlite" \
     *      port_map={{ap_start START} {ap_done DONE} {ap_idle IDLE} {ap_return RETURN}}
     *
     * All of this stuff must be inside the function body i.e.
     *
     * void foo(...)
     * {
     *     pragma stuff
     *     function body
     * }
     *
     */

    //see what kind of ast it really is

    std::cerr << ast_node_type_name(task.get_kind()) 
        << " in_list: " << task.is_in_list()
        << " locus: " << task.get_locus()
        << std::endl;

    //Dig into the tree and find where the function statements are
    Nodecl::NodeclBase::Children tchildren = task.children();
    Nodecl::NodeclBase& context = tchildren.front();
    Nodecl::NodeclBase::Children cchildren = context.children();
    Nodecl::List list(cchildren.front().get_internal_nodecl());
    Nodecl::List stlist(list.begin()->children().front().get_internal_nodecl());

    Nodecl::UnknownPragma ctrl_bus = Nodecl::UnknownPragma::make(
        "AP resource core=AXI_SLAVE variable=return metadata=\"-bus_bundle AXIlite\" port_map={{ap_start START} {ap_done DONE} {ap_idle IDLE} {ap_return RETURN}}");
    stlist.prepend(ctrl_bus);


    //since we are using prepend, everything is going to appar in reverse order
    //but this may not be a real issue
//    TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        std::string field_name = (*it)->get_field_name();
        Nodecl::UnknownPragma pragma_node;

        if ((*it)->get_copies().empty())
        {
            //set scalar argumenit pragmas
            pragma_node = Nodecl::UnknownPragma::make("HLS INTERFACE ap_none port=" + field_name);
            stlist.prepend(pragma_node);

            pragma_node = Nodecl::UnknownPragma::make("AP resource core=AXI_SLAVE variable="
                    + field_name
                    + " metadata=\"-bus_bundle AXIlite\"");
            stlist.prepend(pragma_node);
        }
        else
        {
            //set array/stream pragmas
            pragma_node = Nodecl::UnknownPragma::make(
                    "HLS resource core=AXI4Stream variable=" + field_name);
            stlist.prepend(pragma_node);
            pragma_node = Nodecl::UnknownPragma::make(
                    "HLS interface ap_fifo port=" + field_name);
            stlist.prepend(pragma_node);
            
        }
    }
}

static void get_inout_decl(ObjectList<OutlineDataItem*>& data_items, std::string &in_type, std::string &out_type)
{
    in_type = "";
    out_type = "";
    for (ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        const ObjectList<OutlineDataItem::CopyItem> &copies = (*it)->get_copies();
        if (!copies.empty())
        {
            Scope scope = (*it)->get_symbol().get_scope();
            if (copies.front().directionality == OutlineDataItem::COPY_IN
                    && in_type == "")
            {
                in_type = (*it)->get_field_type().get_simple_declaration(scope, "");
            }
            else if  (copies.front().directionality == OutlineDataItem::COPY_OUT
                    && out_type == "")
            {
                out_type = (*it)->get_field_type().get_simple_declaration(scope, "");
            } else if (copies.front().directionality == OutlineDataItem::COPY_INOUT)
            {
                //If we find an inout, set both input and output types and return
                out_type = (*it)->get_field_type().get_simple_declaration(scope, "");
                in_type = out_type;
                return;
            }
        }
    }
}

static int get_copy_elements(Nodecl::NodeclBase expr)
{
    int elems;
    DataReference datareference(expr);
    if (!datareference.is_valid())
    {
        internal_error("invalid data reference (%s)", datareference.get_locus_str().c_str());
    }
    Type type = datareference.get_data_type();

    if (type.array_is_region()) //it's a region
    {
        Nodecl::NodeclBase cp_size = type.array_get_region_size();
//        if (!cp_size.is_constant())
//        {
//            internal_error("Copy expressions must be known at compile time when working in 'block mode' (%s; %s)",
//                    datareference.get_locus_str().c_str(), cp_size.prettyprint().c_str());
//        }
        elems = const_value_cast_to_4(cp_size.get_constant());
    }
    else if (type.is_array()) //it's a shape
    {
        Nodecl::NodeclBase lower, upper;
        type.array_get_bounds(lower, upper);
//        if (!lower.is_constant() || !upper.is_constant())
//        {
//            internal_error("Copy expressions must be known at compile time when working in 'block mode' (%s)",
//                    datareference.get_locus_str().c_str());
//        }
        elems = const_value_cast_to_4(upper.get_constant()) - const_value_cast_to_4(lower.get_constant()) + 1;
    }
    else //it's a trap!
    {
        internal_error("Data copies must be an array region expression (%d)", datareference.get_locus_str().c_str());
    }
    return elems;
}


/*
 * Create wrapper function for HLS to unpack streamed arguments
 * 
 */
Nodecl::NodeclBase DeviceFPGA::gen_hls_wrapper(const Symbol &func_symbol, ObjectList<OutlineDataItem*>& data_items)
{
    //Check that we are calling a function task (this checking may be performed earlyer in the code)
    if (!func_symbol.is_function())
    {
        fatal_error("Only function-tasks are supperted at this moment");
    }
    Scope fun_scope = func_symbol.get_scope();
//    const ObjectList<Symbol> &param_list = func_symbol.get_function_parameters();
    /*
     * FIXME We suppose that all the input or the output arrays
     * are of the same type
     * Otherwise we must convert (~cast, raw type conversion) for each type
     */

    /*
     * The wrapper function must have:
     *      An input and an output parameters
     *      with respective pragmas needed for streaming
     *      For each scalar parameter, another scalar patameter
     *      IN THE SAME ORDER AS THE ORIGINAL FUNCTION as long as we are generating
     *      scalar parameter passing based on original function task parameters
     */
    //Source wrapper_params;
    std::string in_dec, out_dec;
    get_inout_decl(data_items, in_dec, out_dec);
    Source pragmas_src;
    //call to task_has_scalars is not the optimal, but it is much more simple and readable
    //than checking inside another loop
    if (task_has_scalars(data_items))
    {
        pragmas_src
            << "#pragma HLS resource core=AXI_SLAVE variable=return metadata=\"-bus_bundle AXIlite\" "
            << "port_map={{ap_start START} {ap_done DONE} {ap_idle IDLE} {ap_return RETURN}}\n";
        ;
    }
    Source args;
    if (in_dec != "")
    {
        args << in_dec << hls_in;
        //add stream parameter pragma
        pragmas_src
            << "#pragma HLS resource core=AXI4Stream variable=" << hls_in << "\n"
            << "#pragma HLS interface ap_fifo port=" << hls_in << "\n"
        ;
    }
    if (out_dec != "")
    {
        args.append_with_separator(out_dec + hls_out, ",");
        pragmas_src
            << "#pragma HLS resource core=AXI4Stream variable=" << hls_out << "\n"
            << "#pragma HLS interface ap_fifo port=" << hls_out << "\n"
        ;
    }

    /*
     * Generate wrapper code
     * We are going to keep original parameter name for the original function
     *
     * input/outlut parameters are received concatenated one after another.
     * The wrapper must create local variables for each input/output and unpack
     * streamed input/output data into that local variables.
     *
     * Scalar parameters are going to be copied as long as no unpacking is needed
     */
    Source copies_src;
    Source in_copies, out_copies;
    Source fun_params;
    Source local_decls;
    int in_offset  = 0;
    int out_offset = 0;

    for (ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {


        fun_params.append_with_separator((*it)->get_field_name(), ",");
        const std::string &field_name = (*it)->get_field_name();
        const Scope &scope = (*it)->get_symbol().get_scope();
        const ObjectList<OutlineDataItem::CopyItem> &copies = (*it)->get_copies();
        if (!copies.empty())
        {
            Nodecl::NodeclBase expr = copies.front().expression;
            if (copies.size() > 1)
            {
                internal_error("Only one copy per object (in/out/inout) is allowed (%s)",
                        expr.get_locus_str().c_str());
            }

            /*
             * emit copy code
             * - Create local variable (known size in compile time)
             * - Create create copy loop + update param offset
             */

            //get copy size (must be known at compile time)
            int n_elements = get_copy_elements(expr);

            const Type &field_type = (*it)->get_field_type();
            Type elem_type;
            if (field_type.is_pointer())
            {
                elem_type = field_type.points_to();
            }
            else if (field_type.is_array())
            {
                elem_type = field_type.array_element();
            }
            else
            {
                internal_error("invalid type for input/output, only pointer and array is allowed (%d)",
                        expr.get_locus_str().c_str());
            }

            std::string par_simple_decl = elem_type.get_simple_declaration(scope, field_name);
            local_decls
                << par_simple_decl << "[" << n_elements << "];\n";

            if (copies.front().directionality == OutlineDataItem::COPY_IN
                    or copies.front().directionality == OutlineDataItem::COPY_INOUT)
            {
                in_copies
                    << "for (" << HLS_I << "=0;" << HLS_I << "<" << n_elements << "; " << HLS_I << "++)"
                    << "{"
                    << "  " << field_name << "[" << HLS_I << "] = " << hls_in << "[" << HLS_I << "+" << in_offset << "];"
                    << "}"
                ;
                in_offset += n_elements;
            }
            if (copies.front().directionality == OutlineDataItem::COPY_OUT
                    or copies.front().directionality == OutlineDataItem::COPY_INOUT)
            {
                out_copies
                    << "for (" << HLS_I << "=0;" << HLS_I << "<" << n_elements << "; " << HLS_I << "++)"
                    //<< "for (i=0; i<" << n_elements << "; i++)"
                    << "{"
                    << "  "  << hls_out << "[" << HLS_I << "+" << out_offset << "] = " << field_name << "[" << HLS_I << "];"
                    << "}"
                ;
                out_offset += n_elements;
            }
        }
        else
        {
            //generate scalar parameter code
            Source par_src;

            par_src
                << (*it)->get_field_type().get_simple_declaration(scope, field_name)
            ;
            args.append_with_separator(par_src, ",");
            pragmas_src
                << "#pragma HLS INTERFACE ap_none port=" <<  field_name << "\n"
                << "#pragma AP resource core=AXI_SLAVE variable=" << field_name << " metadata=\"-bus_bundle AXIlite\"\n"
            ;
        }
    }
    Nodecl::NodeclBase fun_code =  func_symbol.get_function_code();
    Source wrapper_src;
    wrapper_src
        << "void core_hw_accelerator(" << args<< "){"
    ;
    local_decls << "unsigned int " << HLS_I << ";";

    wrapper_src
        << pragmas_src
        << local_decls
        << in_copies
        << func_symbol.get_name() << "(" << fun_params << ");"
        << out_copies
        << "}"
    ;
    //parse source
    ReferenceScope refscope(func_symbol.get_scope());
    Nodecl::NodeclBase wrapper_node = wrapper_src.parse_global(refscope);

    return wrapper_node;

}

void DeviceFPGA::copy_stuff_to_device_file(
        const TL::ObjectList<Nodecl::NodeclBase>& stuff_to_be_copied)
{
    for (TL::ObjectList<Nodecl::NodeclBase>::const_iterator it = stuff_to_be_copied.begin();
            it != stuff_to_be_copied.end();
            ++it)
    {
        if (it->is<Nodecl::FunctionCode>()
                || it->is<Nodecl::TemplateFunctionCode>())
        {
            TL::Symbol function = it->get_symbol();
            TL::Symbol new_function = SymbolUtils::new_function_symbol(function, function.get_name() + "_hls");

            _copied_fpga_functions.add_map(function, new_function);
            _fpga_file_code.append(Nodecl::Utils::deep_copy(*it, *it, _copied_fpga_functions));
        }
        else
        {
            _fpga_file_code.append(Nodecl::Utils::deep_copy(*it, *it));
        }
    }
}

EXPORT_PHASE(TL::Nanox::DeviceFPGA);

