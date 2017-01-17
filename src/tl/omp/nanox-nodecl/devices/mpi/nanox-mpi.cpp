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


#include "tl-devices.hpp"
#include "nanox-mpi.hpp"
#include "tl-nanos.hpp"
#include "tl-multifile.hpp"
#include "tl-compilerpipeline.hpp"
#include "tl-nanox-ptr.hpp"

#include "cxx-profile.h"
#include "codegen-phase.hpp"
#include "codegen-cxx.hpp"
#include "cxx-cexpr.h"
#include "filename.h"
#include "tl-nodecl-utils-fortran.hpp"
#include "tl-symbol-utils.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-nodecl-utils-c.hpp"
#include <errno.h>
#include "cxx-driver-utils.h"

using namespace TL;
using namespace TL::Nanox;

static std::string get_outline_name(const std::string & name) {
    return "mpi_" + name;
}


//This function tries to return if we are using icc/gcc, looks for a "g" in the preprocessor (which should mean gnu...)
static bool compilingWithIcc()
{
    //FIXME: no existing reliable/clear way to detect if we are using icc or gcc
    std::string preprocessorName(CURRENT_CONFIGURATION->preprocessor_name);
    return preprocessorName.find("g")==std::string::npos;
}


void DeviceMPI::generate_additional_mpi_code(
        const TL::ObjectList<OutlineDataItem*>& data_items,
        const TL::Symbol& struct_args,
        const std::string& outline_name,
        TL::Source& code_host,
        TL::Source& code_device_pre,        
        TL::Source& code_device_post,
        const TL::Symbol& curr_function_host,
        const TL::Symbol& curr_function_dev ) 
{    
    std::string ompss_get_mpi_type="ompss_get_mpi_type";

    TL::Type argument_type = ::get_user_defined_type(struct_args.get_internal_symbol());
    TL::ObjectList<TL::Symbol> parameters_called = argument_type.get_fields();
    TL::ObjectList<std::string> param_called_names;
    
    int num_params = parameters_called.size();    
    for (int i = 0; i < num_params; ++i) {
        param_called_names.append(parameters_called.at(i).get_name());
    }
    
    //We fill it manually with "Null" values
    //Nanox will search the right communicator and rank at runtime (based on the binding)
    TL::ObjectList<std::string> new_dev_info;
    new_dev_info.append("0");
    new_dev_info.append(UNKOWN_RANKSRCDST);


    
    Source type_name_host;
    type_name_host << "(void*)"
        << "(" << as_type(curr_function_host.get_type().get_pointer_to()) << " )"
        << curr_function_host.get_qualified_name();
    code_host << "int offload_err; ";
    code_host << "int id_func_ompss=ompss_mpi_get_function_index_host(" << type_name_host << ")" << ";";
    
    
    
    Source type_name_dev;
    type_name_dev << "(void*)"
        << "(" << as_type(curr_function_dev.get_type().get_pointer_to()) << " )"
        << curr_function_dev.get_qualified_name();
    Source struct_arg_type_name;
    struct_arg_type_name
         << ((struct_args.get_type().is_template_specialized_type()
                     &&  struct_args.get_type().is_dependent()) ? "typename " : "")
         << struct_args.get_qualified_name();      
    code_device_pre << struct_arg_type_name.get_source() << " args;"
            << "int offload_err; "            
            << "MPI_Comm ompss_parent_comp; "            
            << "offload_err= nanos_mpi_get_parent(&ompss_parent_comp);"
            << "int id_func_ompss=ompss_mpi_get_function_index_dev(" << type_name_dev << ")" << ";";

    Source typelist_src, blocklen_src, displ_src;
    //Source parameter_call;
    

    //If there are parameters, add/build the structures 
    if (num_params>0){
        int count_params=num_params;
        Source struct_mpi_create, host_call, device_call;
        for (int i = 0; i < num_params; ++i) { 
            std::string ompss_mpi_type = get_ompss_mpi_type(parameters_called[i].get_type());
            
            displ_src.append_with_separator("(( (char *)&(args." + parameters_called[i].get_name() + ") - (char *)&args ))", ",");
            if (parameters_called[i].get_type().is_pointer()) {
                typelist_src.append_with_separator(ompss_get_mpi_type  + "(mpitype_ompss_unsigned_long_long)", ",");

                blocklen_src.append_with_separator("1", ",");
            } else {
                typelist_src.append_with_separator(ompss_mpi_type, ",");

                if (parameters_called[i].get_type().array_has_size()) {
                    blocklen_src.append_with_separator(
                            as_expression(parameters_called[i].get_type().array_get_size().shallow_copy())
                            , ",");
                } else {
                    blocklen_src.append_with_separator("1", ",");
                }
            }

        }
        
	if(Nanos::Version::interface_is_at_least("offload", 1001))
	{
		struct_mpi_create <<"MPI_Datatype ompss___datatype;"
			"offload_err=nanos_mpi_type_get_struct( id_func_ompss, &ompss___datatype );"
			"if ( ompss___datatype == ompss_get_mpi_type(mpitype_ompss_null) )  ";
	} else {
		struct_mpi_create <<"MPI_Datatype* ompss___datatype;"
			"offload_err=nanos_mpi_type_get_struct( id_func_ompss, &ompss___datatype );"
			"if ( ompss___datatype == 0 )  ";
	}

	struct_mpi_create <<"{ "
		"   MPI_Datatype ompss___typelist[" << count_params << "]= {" << typelist_src << "};"
		"   int ompss___blocklen[" << count_params << "] = {" << blocklen_src << "};"
		"   MPI_Aint ompss___displ[" << count_params << "] = {" << displ_src << "};"
		"   offload_err= nanos_mpi_type_create_struct( " << count_params << ", ompss___blocklen, ompss___displ, "
		"   ompss___typelist, &ompss___datatype, id_func_ompss); "
		"}";

	host_call << " offload_err=nanos_mpi_send_taskinit(&id_func_ompss, 1," + new_dev_info[1] + " , " + new_dev_info[0] + ");";
	host_call << " offload_err=nanos_mpi_send_datastruct( (void *) &args, 1,  ompss___datatype," + new_dev_info[1] + "," + new_dev_info[0] + ");";

	//Recv datastruct from parent (rank will be ignored by nanox)
	device_call << " offload_err=nanos_mpi_recv_datastruct(&args, 1, ompss___datatype, 0, ompss_parent_comp); ";

	code_host << struct_mpi_create
		<< host_call;
	code_device_pre << struct_mpi_create
		<< device_call;

    } else {
	//If there are no parameters, just send the order to start the task
        code_host << " offload_err=nanos_mpi_send_taskinit(&id_func_ompss, 1," + new_dev_info[1] + " , " + new_dev_info[0] + ");";
    }
    
    
    //Insert implicit #taskwait noflush after task
    code_device_post << "nanos_err_t tskwait_err;"
                     << " nanos_wd_t nanos_wd_ = nanos_current_wd(); "
                     << " tskwait_err = nanos_wg_wait_completion(nanos_wd_, 1); "
                     << " if (tskwait_err != NANOS_OK) "
                     << "  { "
                     << "    nanos_handle_error(tskwait_err); "
                     << "  } ";
    //Send taskEnd to parent (rank will be ignored by nanox)
    code_device_post << "offload_err= nanos_mpi_send_taskend(&id_func_ompss, 1, 0, ompss_parent_comp);";


}

/**
 * In MPI we generate three functions
 * _host function, function which it's called on the host (by nanox)
 * _device function, function which it's called on the device (by the daemon mercurium generates)
 * _unpacked function, function which it's called inside the _device function, and calls the original user-code function
 * @param info
 * @param outline_placeholder
 * @param output_statements
 * @param symbol_map
 */
void DeviceMPI::create_outline(CreateOutlineInfo &info,
        Nodecl::NodeclBase &outline_placeholder,
        Nodecl::NodeclBase &output_statements,
        Nodecl::Utils::SimpleSymbolMap* &symbol_map) {
    
    TL::ObjectList<OutlineDataItem*> data_items = info._data_items;
    
    // Unpack DTO 
    const std::string& device_outline_name = get_outline_name(info._outline_name);
    const Nodecl::NodeclBase& original_statements = info._original_statements;
    const TL::Symbol& called_task = info._called_task;
    bool is_function_task = called_task.is_valid();

    output_statements = original_statements;

    symbol_map = new Nodecl::Utils::SimpleSymbolMap();

    //OutlineInfo& outline_info = info._outline_info;
    
    //At first time we process a task, declare a function
    if (!_mpi_task_processed){
        _mpi_task_processed = true;
        Source search_function;
        search_function << "typedef float(*ptrToFunc)(float, float);";
        search_function << "extern int ompss_mpi_get_function_index_host(void* func);";
        if (IS_FORTRAN_LANGUAGE)
            Source::source_language = SourceLanguage::C;
        Nodecl::NodeclBase search_function_tree = search_function.parse_global(_root);
        Source::source_language = SourceLanguage::Current;
        Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, search_function_tree);
    }

    ERROR_CONDITION(called_task.is_valid() && !called_task.is_function(),
            "The '%s' symbol is not a function", called_task.get_name().c_str());

    TL::Symbol current_function = original_statements.retrieve_context().get_related_symbol();
    if (current_function.is_nested_function()) {
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            fatal_printf_at(original_statements.get_locus(), "nested functions are not supported\n");
    }

    Source unpacked_arguments, fortran_allocatable_translation, cleanup_code;
    
    
    ObjectList<std::string> structure_name;
    ObjectList<TL::Type> structure_type;
    // Create the new unpacked function
    TL::Symbol device_function = SymbolUtils::new_function_symbol(
            current_function,
            device_outline_name + "_device",
            TL::Type::get_void_type(),
            structure_name,
            structure_type);
    
    Nodecl::NodeclBase device_function_code, device_function_body;
    SymbolUtils::build_empty_body_for_function(device_function,
            device_function_code,
            device_function_body);
    

    // Create the outline function
    //The outline function has always only one parameter which name is 'args'
    structure_name.append("args");

    //The type of this parameter is an struct (i. e. user defined type)
    structure_type.append(TL::Type(
            get_user_defined_type(
            info._arguments_struct.get_internal_symbol())).get_lvalue_reference_to());

    TL::Symbol host_function = SymbolUtils::new_function_symbol(
            current_function,
            device_outline_name + "_host",
            TL::Type::get_void_type(),
            structure_name,
            structure_type);
    
    Nodecl::NodeclBase host_function_code, host_function_body;
    SymbolUtils::build_empty_body_for_function(host_function,
            host_function_code,
            host_function_body);
    
    // Create the new unpacked function
    Source initial_statements, final_statements;
    TL::Symbol unpacked_function, forward_function;
    
    if (IS_FORTRAN_LANGUAGE)
    {
        forward_function = new_function_symbol_forward(
                current_function,
                device_outline_name + "_forward",
                info);
        unpacked_function = new_function_symbol_unpacked(
                current_function,
                device_outline_name + "_unpack",
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
                device_outline_name + "_unpacked",
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
    
    // Add the unpacked function to the file
    Nodecl::Utils::prepend_to_enclosing_top_level_location(info._original_statements,unpacked_function_code);
    
    TL::Scope host_function_scope(host_function_body.retrieve_context());    
    TL::Symbol structure_symbol = host_function_scope.get_symbol_from_name("args");
    ERROR_CONDITION(!structure_symbol.is_valid(), "Argument of outline function not found", 0);

    std::map< TL::Symbol,TL::ObjectList<TL::Symbol> > modules_with_params;
    Source data_input_global;
    Source data_output_global;

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
            case OutlineDataItem::SHARING_CAPTURE:   
                //If it's firstprivate (sharing capture), copy input and change address to the global/private var
                if ((*it)->get_symbol().is_fortran_common() || (*it)->get_symbol().is_in_module() || (*it)->get_symbol().is_from_module() || (*it)->get_symbol().get_scope().is_namespace_scope()){
                   std::string symbol_name=(*it)->get_symbol().get_name();
                   if ((*it)->get_sharing() == OutlineDataItem::SHARING_CAPTURE && !(*it)->get_symbol().get_type().is_const()){
                        //Copy value of the captured to the global (aka initialize the global)
                        data_input_global << "offload_err = nanos_memcpy(&" << symbol_name <<",&args." << symbol_name <<",sizeof(" << symbol_name << "));";  
                   }
                }
            case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
                {
                    //If is from module(fort)/common (fort)/global (C) and is sharing ca or sharing shared (no sharing capture)
                    //copy address and data
                    if ((*it)->get_symbol().is_allocatable() && (*it)->get_copy_of_array_descriptor()!=NULL){  
                        if ( !(*it)->get_copies().empty() ) {
                            std::string symbol_name=(*it)->get_symbol().get_name();
                            std::string descriptor_name=(*it)->get_copy_of_array_descriptor()->get_symbol().get_name();

                            data_input_global << "args." << symbol_name <<"= &(args." << descriptor_name << ");"; 
                            //data_input_global << "void* " << descriptor_name << "_ptr = &(args." << descriptor_name << ");";
                            //data_input_global << "offload_err = nanos_memcpy(&args." << symbol_name <<" ,&"<<descriptor_name<<"_ptr,sizeof("<<descriptor_name<<"_ptr));";
                            if ((*it)->get_sharing() != OutlineDataItem::SHARING_CAPTURE &&
                                ((*it)->get_symbol().is_fortran_common() || (*it)->get_symbol().is_in_module() || (*it)->get_symbol().is_from_module() || (*it)->get_symbol().get_scope().is_namespace_scope())){
                                TL::Symbol ptr_of_sym = get_function_ptr_of((*it)->get_symbol(),
                                        info._original_statements.retrieve_context());

                                data_input_global << "offload_err = nanos_memcpy(" << ptr_of_sym.get_name() << "(" << symbol_name <<"),&(args."<< descriptor_name << "),sizeof(args." << descriptor_name << "));"; 
                            }
                        } else {

                            if ((*it)->get_sharing() != OutlineDataItem::SHARING_CAPTURE &&
                                ((*it)->get_symbol().is_fortran_common() || (*it)->get_symbol().is_in_module() ||(*it)->get_symbol().is_from_module() || (*it)->get_symbol().get_scope().is_namespace_scope())){
                                std::string symbol_name=(*it)->get_symbol().get_name();
                                TL::Symbol ptr_of_sym = get_function_ptr_of((*it)->get_symbol(),
                                            info._original_statements.retrieve_context());
                                data_input_global << "args." << symbol_name <<"= " << ptr_of_sym.get_name() << "(" << symbol_name <<");";  
                            } else {                                
                                std::string symbol_name=(*it)->get_symbol().get_name();
                                std::string descriptor_name=(*it)->get_copy_of_array_descriptor()->get_symbol().get_name();
                                data_input_global << "args." << symbol_name <<"= &(args." << descriptor_name << ");"; 
                            }
                        }
                    }

                    //Copy and swap addresses
                    if (!(*it)->get_symbol().get_type().is_const() && !(*it)->get_symbol().is_allocatable() && (*it)->get_sharing() != OutlineDataItem::SHARING_CAPTURE &&
                            ( (*it)->get_symbol().is_fortran_common() || (*it)->get_symbol().is_in_module() || (*it)->get_symbol().is_from_module() || (*it)->get_symbol().get_scope().is_namespace_scope() )){  
                        std::string symbol_name=(*it)->get_symbol().get_name();

                        if (!(*it)->get_copies().empty())
                        {
                            data_input_global << "void* " << symbol_name << "_BACKUP =  args." << symbol_name <<";";   
                            data_input_global << "offload_err = nanos_memcpy(&" << symbol_name <<","<< symbol_name << "_BACKUP,sizeof(" << symbol_name << "));";
                        }

                        data_input_global << "args." << symbol_name <<"= &" << symbol_name << ";"; 

                        if (!(*it)->get_copies().empty())
                        data_output_global << "offload_err = nanos_memcpy("<< symbol_name << "_BACKUP,&" << symbol_name <<",sizeof(" << symbol_name << "));";    
                    }
                    
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
                            if (!((*it)->get_symbol().get_type().depends_on_nonconstant_values()))
                            {
                                argument << "*(args." << (*it)->get_field_name() << ")";
                            }
                            else
                            {
                                C_LANGUAGE()
                                {
                                    TL::Type ptr_type = (*it)->get_in_outline_type().references_to().get_pointer_to();
                                    TL::Type cast_type = rewrite_type_of_vla_in_outline(ptr_type, data_items, structure_symbol);
                                    argument << "*(args." << (*it)->get_field_name() << ")";
                                }
                                CXX_LANGUAGE()
                                {
                                    // No VLAs in C++ means that we have to pass a void*
                                    // It will have to be reshaped again in the outline
                                    argument << "args." << (*it)->get_field_name();
                                }
                            }
                        }
                        // Any other parameter is bound to the storage of the struct
                        else
                        {
                            if (!((*it)->get_symbol().get_type().depends_on_nonconstant_values()))
                            {
                                argument << "args." << (*it)->get_field_name();
                            }
                            else
                            {
                                C_LANGUAGE()
                                {
                                    if (((*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_OVERALLOCATED)                                                     == OutlineDataItem::ALLOCATION_POLICY_OVERALLOCATED)
                                    {
                                        TL::Type ptr_type = (*it)->get_in_outline_type().references_to().get_pointer_to();
                                        TL::Type cast_type = rewrite_type_of_vla_in_outline(ptr_type, data_items, structure_symbol);
                                        argument << "*(args." << (*it)->get_field_name() << ")";
                                    }
                                    else
                                    {
                                        TL::Type cast_type = rewrite_type_of_vla_in_outline(param_type, data_items, structure_symbol);
                                        argument << "args." << (*it)->get_field_name();
                                    }
                                }

                                CXX_LANGUAGE()
                                {
                                    // No VLAs in C++ means that we have to pass a void*
                                    // It will have to be reshaped again in the outline
                                    argument << "args." << (*it)->get_field_name();
                                }
                            }
                        }
                    }
                    else if (IS_FORTRAN_LANGUAGE)
                    {
                        //Build list with modules and vars when they are from module
                        if ((*it)->get_symbol().is_from_module()){
                            TL::Symbol mod_sym=(*it)->get_symbol().from_module();
                            std::map< TL::Symbol,TL::ObjectList<TL::Symbol> >::iterator mod_list= modules_with_params.find(mod_sym);
                            if (mod_list==modules_with_params.end()){
                                TL::ObjectList<TL::Symbol> list;
                                list.append((*it)->get_symbol());
                                modules_with_params.insert(std::pair<TL::Symbol,TL::ObjectList<TL::Symbol> >(mod_sym,list));
                            } else {
                                mod_list->second.append((*it)->get_symbol());                    
                            }                            
                        }
                        argument << "args % " << (*it)->get_field_name();

                        bool is_allocatable = (*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE;
                        bool is_pointer = (*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_POINTER;

                        if ((((*it)->get_symbol().is_in_module() || (*it)->get_symbol().is_from_module()) && is_allocatable)
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

    Source code_host;
    Source code_device_pre;
    Source code_device_post;
    
    generate_additional_mpi_code(
            data_items,
            info._arguments_struct,
            info._outline_name,
            code_host,
            code_device_pre,
            code_device_post,
            host_function,
            device_function);
    
    Source unpacked_source;
    Source extra_declarations;
    
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
    
    
    //if (IS_FORTRAN_LANGUAGE)
    //   Source::source_language = SourceLanguage::C;
    Nodecl::NodeclBase new_unpacked_body =
            unpacked_source.parse_statement(unpacked_function_body);    
    //Source::source_language = SourceLanguage::Current;
    //unpacked_function_body.replace(new_unpacked_body);


    // Add a declaration of the unpacked function symbol in the original source
     // Fortran may require more symbols
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
        fun_visitor.insert_extra_symbols(info._task_statements);

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
        fun_visitor.insert_extra_symbols(info._task_statements);

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

    Source host_src,
           instrument_before_host,
           instrument_after_host;

    
    
    if (instrumentation_enabled())
    {
        get_instrumentation_code(
                info._called_task,
                host_function,
                host_function_body,
                info._task_label,
                original_statements.get_locus(),
                instrument_before_host,
                instrument_after_host); 
    } 
    
    host_src
            << "{"
            << instrument_before_host
            << code_host
            << instrument_after_host;    
    
    if (!cleanup_code.empty()){
           Nodecl::NodeclBase cleanup_code_tree = cleanup_code.parse_statement(host_function_body);
           host_src << as_statement(cleanup_code_tree);
    }
    
    host_src << "}";

    if (IS_FORTRAN_LANGUAGE)
       Source::source_language = SourceLanguage::C;
    Nodecl::NodeclBase new_host_body = host_src.parse_statement(host_function_body);
    Source::source_language = SourceLanguage::Current;

    host_function_body.replace(new_host_body);
    
    Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, host_function_code);


    
    Source unpacked_function_call;
    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
    {
       if (IS_CXX_LANGUAGE
                && !is_function_task
                && current_function.is_member()
                && !current_function.is_static())
        {
            unpacked_function_call << "args.this_->";
        }

       unpacked_function_call << unpacked_function.get_qualified_name_for_expression(
               /* in_dependent_context */
               (current_function.get_type().is_template_specialized_type()
                && current_function.get_type().is_dependent())
               ) << "(" << unpacked_arguments << ");";

        //TODO: Test this and check what it does (fsainz)
        if (IS_CXX_LANGUAGE)
        {
            if (!host_function.is_member())
            {
                Nodecl::NodeclBase nodecl_decl = Nodecl::CxxDecl::make(
                        /* optative context */ nodecl_null(),
                        host_function,
                        original_statements.get_locus());
                Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, nodecl_decl);
            }
        }
    }
    else if (IS_FORTRAN_LANGUAGE)
    {
        Source unpacked_function_addr;
        unpacked_function_call
            << "CALL " << device_outline_name << "_forward(" << unpacked_function_addr << unpacked_arguments << ")\n"
            ;

        unpacked_function_addr << "LOC(" << unpacked_function.get_name() << ")";
        if (!unpacked_arguments.empty())
        {
            unpacked_function_addr << ", ";
        }

       // Copy USEd information to the outline and forward functions
        TL::Symbol *functions[] = { &device_function ,&host_function, &forward_function, NULL };

        for (int i = 0; functions[i] != NULL; i++)
        {
            TL::Symbol &function(*functions[i]);

            Nodecl::Utils::Fortran::append_used_modules(original_statements.retrieve_context(),
                    function.get_related_scope());

            add_used_types(data_items, function.get_related_scope());
        }

        // Generate ancillary code in C
        add_forward_function_code_to_extra_c_code(device_outline_name, data_items, outline_placeholder);
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
    
    Source device_src, 
            instrument_before_dev,
            instrument_after_dev;
    
    
    if (instrumentation_enabled())
    {
        get_instrumentation_code(
                info._called_task,
                device_function,
                device_function_body,
                info._task_label,
                original_statements.get_locus(),
                instrument_before_dev,
                instrument_after_dev); 
    } 
    
    Nodecl::NodeclBase new_device_body;
    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
    {
        device_src
                << "{"
                << code_device_pre
                << data_input_global
                << instrument_before_dev
                << unpacked_function_call
                << data_output_global
                << code_device_post
                << instrument_after_dev
                << "}"
                ;
        
       new_device_body = device_src.parse_statement(device_function_body);
       // std::cout << new_device_body.get_text() << "\n";
    }
    else if (IS_FORTRAN_LANGUAGE)
    {
        //Generate the USE (module), ONLY: params
        Source mod_list_with_params;
        
        std::map< TL::Symbol,TL::ObjectList<TL::Symbol> >::iterator it_mods;
        for (it_mods= modules_with_params.begin(); it_mods!=modules_with_params.end(); it_mods++){
            mod_list_with_params << "USE " << it_mods->first.get_name() << ", ONLY: ";
            TL::ObjectList<TL::Symbol> lst_params=it_mods->second;
            TL::ObjectList<TL::Symbol>::iterator it_lst;
            Source par_list;
            for (it_lst= lst_params.begin(); it_lst!=lst_params.end(); it_lst++){
                par_list.append_with_separator(it_lst->get_name(),",");
            }
            mod_list_with_params << par_list << "\n";
        }
        Nodecl::NodeclBase mod_params_tree = mod_list_with_params.parse_statement(device_function_body);
        Source::source_language = SourceLanguage::C;
        Nodecl::NodeclBase code_pre = code_device_pre.parse_statement(device_function_body);
        device_src
                << as_statement(code_pre);
        
        Nodecl::NodeclBase data_input_tree;
        Nodecl::NodeclBase data_output_tree;
        if (!data_input_global.empty()){
            data_input_tree = data_input_global.parse_statement(device_function_body);
            device_src
                    << as_statement(data_input_tree);
        }
        Nodecl::NodeclBase code_post = code_device_post.parse_statement(device_function_body);
        device_src
                << fortran_allocatable_translation
                << unpacked_function_call;
        if (!data_output_global.empty()){        
            data_output_tree = data_output_global.parse_statement(device_function_body);
            device_src
                    << as_statement(data_output_tree);
        }
        device_src
                << as_statement(code_post);
        Source::source_language = SourceLanguage::Current;      
        
        new_device_body = device_src.parse_statement(device_function_body);        
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
    
    device_function_body.replace(new_device_body);
    Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, device_function_code);
        
    
    
    std::string append;
    if (IS_FORTRAN_LANGUAGE){
        append="_";
        
        std::string prepend_module="";

        bool isIcc=compilingWithIcc();
        //If we are not gcc
        if ( isIcc )
        {
            if ( host_function.in_module()!=NULL && host_function.in_module().is_valid() )
                prepend_module=host_function.in_module().get_name()+"_mp_";
        } else {
            if ( host_function.in_module()!=NULL && host_function.in_module().is_valid() )
                prepend_module="__" + host_function.in_module().get_name()+"_MOD_";            
        }

        _extraFortranDecls <<
               "extern void " << prepend_module << device_outline_name << "_host" << append  << "(struct " << info._arguments_struct.get_name() << " *const args);"
               "extern void " << prepend_module << device_outline_name << "_device"  << append << "(void);";
        
       _sectionCodeHost.append_with_separator("(void*)" + prepend_module + host_function.get_qualified_name() + append,",");
       _sectionCodeDevice.append_with_separator("(void(*)())" + prepend_module + device_function.get_qualified_name() + append,",");
       _currTaskId++; 
    } else {
        if( current_function.get_type().is_template_specialized_type()
         && !current_function.get_scope().get_template_parameters()->is_explicit_specialization ){
            type_t* type=template_specialized_type_get_related_template_type(current_function.get_type().get_internal_type());
            int n = template_type_get_num_specializations(type);
            
            //Emmit pointers to all the specialization for the original function, but in our device/host functions
            for (int i = 0; i < n; i++)
            {
                TL::Type t=template_type_get_specialization_num(type, i);
                if ( template_type_get_primary_type(type) != t.get_internal_type() ) {              
                    template_parameter_list_t* arguments_list= template_specialized_type_get_template_arguments(t.get_symbol().get_type().get_internal_type());
                    std::string templateName(template_arguments_to_str(arguments_list,0,1,current_function.get_scope().get_decl_context()));
                    _sectionCodeHost.append_with_separator("(void*)" + host_function.get_qualified_name(true) + templateName,",");
                    _sectionCodeDevice.append_with_separator("(void(*)())" + device_function.get_qualified_name(true) + templateName,",");    
                    _currTaskId++; 
                }
            }
        } else {
           _sectionCodeHost.append_with_separator("(void*)" + host_function.get_qualified_name() + append,",");
           _sectionCodeDevice.append_with_separator("(void(*)())" + device_function.get_qualified_name() + append,",");
           _currTaskId++; 
        }
    }  
    
}

DeviceMPI::DeviceMPI()
: DeviceProvider(/* device_name */ std::string("mpi")) //, _cudaFilename(""), _cudaHeaderFilename("")
{
    set_phase_name("Nanox MPI support");
    set_phase_description("This phase is used by Nanox phases to implement MPI device support");
}

void DeviceMPI::get_device_descriptor(DeviceDescriptorInfo& info,
        Source &ancillary_device_description,
        Source &device_descriptor,
        Source &fortran_dynamic_init UNUSED_PARAMETER) {
    TargetInformation& target_information = info._target_info;
    const std::string& device_outline_name = get_outline_name(info._outline_name);
    const std::string& arguments_struct = info._arguments_struct;
    if (Nanos::Version::interface_is_at_least("master", 5012)) {         
        ObjectList<Nodecl::NodeclBase> onto_clause = target_information.get_onto();
        Nodecl::Utils::SimpleSymbolMap param_to_args_map = info._target_info.get_param_arg_map();
        
        //Set rank and comm, -95 and 0 means undefined so
        //runtime can pick any FREE spawned node
        //(user can specify any rank and any comm using onto clause)
        std::string assignedComm = "(MPI_Comm)0";
        std::string assignedRank = UNKOWN_RANKSRCDST;
        if (onto_clause.size() >= 1) {
            const TL::Symbol& called_task = info._called_task;
            bool is_function_task = called_task.is_valid();
            if (is_function_task) {
               Nodecl::NodeclBase base=Nodecl::Utils::deep_copy(onto_clause[0], onto_clause[0], param_to_args_map);
               assignedComm = as_expression(base);
            } else {
               assignedComm = as_expression(onto_clause.at(0));
            }
        }
        if (onto_clause.size() >= 2) {
            const TL::Symbol& called_task = info._called_task;
            bool is_function_task = called_task.is_valid();
            if (is_function_task) {
               Nodecl::NodeclBase base=Nodecl::Utils::deep_copy(onto_clause[1], onto_clause[1], param_to_args_map);
               assignedRank = as_expression(base);
            } else {
               assignedRank = as_expression(onto_clause.at(1));
            }
        }
        
        if (!IS_FORTRAN_LANGUAGE)
        {                  
            // Extra cast for solving some issues of GCC 4.6.* and lowers (this
            // issues seem to be fixed in GCC 4.7 =D)
            std::string ref = IS_CXX_LANGUAGE ? "&" : "*";
            std::string extra_cast = "(void(*)(" + arguments_struct + ref + "))";
            
            TL::Symbol current_function = info._current_function;

            //FIXME (together with SMP): This is confusing. In a future, we should get the template
            //arguments of the outline function and print them
            std::string original_name = current_function.get_name();
            current_function.set_name(device_outline_name+"_host");
            Nodecl::NodeclBase code = current_function.get_function_code();
            Nodecl::Context context = (code.is<Nodecl::TemplateFunctionCode>())
                ? code.as<Nodecl::TemplateFunctionCode>().get_statements().as<Nodecl::Context>()
                : code.as<Nodecl::FunctionCode>().get_statements().as<Nodecl::Context>();    
            bool without_template_args =
                !current_function.get_type().is_template_specialized_type()
                || current_function.get_scope().get_template_parameters()->is_explicit_specialization;
            TL::Scope function_scope = context.retrieve_context();
            std::string qualified_name = current_function.get_qualified_name(function_scope,without_template_args);            
            // Restore the original name of the current function
            current_function.set_name(original_name);


            //WARNING: maybe this nanos_mpi_args_t variable should not be static (non thread-safe)
            //Initialize static with 0's and then change its value each time we spawn a task
            //This struct will be copied by nanox at task creation time, so values will be correct in the runtime
            ancillary_device_description
                << "static nanos_mpi_args_t "
                << device_outline_name << "_mpi_args = "
                << "{ ((void(*)(void*))" << "(" << extra_cast << " &" << qualified_name << ")),0,0 };"
                << device_outline_name << "_mpi_args.assignedComm = " << assignedComm << ";"
                << device_outline_name << "_mpi_args.assignedRank = " << assignedRank << ";";

            device_descriptor << "{ &nanos_mpi_factory, &" << device_outline_name << "_mpi_args }";
        }
        else
        {
            ancillary_device_description
                << "static nanos_mpi_args_t " << device_outline_name << "_args;"
                ;

            device_descriptor
                << "{"
                // factory, arg
                << "0, 0"
                << "}"
                ;

            fortran_dynamic_init
                << device_outline_name << "_args.outline = (void(*)(void*))&" << device_outline_name << "_host;"
                << device_outline_name << "_args.assignedComm = " << assignedComm << ";"
                << device_outline_name << "_args.assignedRank = " << assignedRank << ";"
                << "nanos_wd_const_data.devices[0].factory = &nanos_mpi_fortran_factory;"
                << "nanos_wd_const_data.devices[0].arg = &" << device_outline_name << "_args;" 
                ;
        }
    } else {
        internal_error("Unsupported Nanos version.", 0);
    }   
}

bool DeviceMPI::remove_function_task_from_original_source() const
{
    return false;
}

bool DeviceMPI::allow_mandatory_creation() {
    return true;
}


void DeviceMPI::copy_stuff_to_device_file(const TL::ObjectList<Nodecl::NodeclBase>& stuff_to_be_copied) {
}

static std::ifstream::pos_type get_filesize(const char* filename)
{
    std::ifstream in(filename, std::ifstream::in | std::ifstream::binary);
    in.seekg(0, std::ifstream::end);
    std::ifstream::pos_type pos=in.tellg();
    if (pos==-1) {
        pos=1;
    }
    return pos; 
}

static unsigned hash_str(const char* s)
{
   unsigned h = 31 /* also prime */;
   while (*s) {
     h = (h * 54059) ^ (s[0] * 76963);
     s++;
   }
   //Make sure this number is bigger than MASK_TASK_NUMBER (not sure if needed)
   return h+MASK_TASK_NUMBER+5;
}


void DeviceMPI::phase_cleanup(DTO& data_flow) {
    
    std::string original_filename = TL::CompilationProcess::get_current_file().get_filename();        
    original_filename =original_filename.substr(0, original_filename.find("."));

    Symbol main;
    if (IS_FORTRAN_LANGUAGE){
        Nodecl::List top_level_list = _root.as<Nodecl::TopLevel>().get_top_level().as<Nodecl::List>();
        bool found=false;
        for (Nodecl::List::iterator it = top_level_list.begin();
            it != top_level_list.end() && !found; 
            it++)
        {
           Nodecl::NodeclBase current_item = *it;
           if (current_item.is<Nodecl::FunctionCode>())
           {
               Nodecl::FunctionCode function_code = current_item.as<Nodecl::FunctionCode>();
               TL::Symbol function_sym = function_code.get_symbol();
               if (function_sym.get_internal_symbol()->kind==SK_PROGRAM){               
                   main=function_sym;
                   found=true;
               }
           }
        }
    } else {        
        main = _root.retrieve_context().get_symbol_from_name("main");
    }
    
    //Create MPI sections
    //This section will be synchronized in "nanos_sync_dev_pointers" nanox call
    //so we have function pointers in the same order in both processes
    if (_mpi_task_processed || main.is_valid()) {
        
//        if (main.is_valid()) {
//        std::string new_filename = "/home/Computational/fsainz/mpiall.o";
//        std::cout << "adding " << new_filename << "to files\n";
//        TL::CompilationProcess::add_file(new_filename, "plaincxx");
//        }
//        
        Source functions_section;   
        
        //Extern declaration of fortran tasks in C file (needed, until codegen can do them, but it's this is unlikely to happen
        //because codegen doesn't know that we have two "scopes/languages")
        functions_section << _extraFortranDecls; 
        //Section with MASKS (this is just an array containing 989, so we can count how many files we compiled, when this finishes, next section
        //will not have that value)
        functions_section << "int (ompss_mpi_masks[]) __attribute__((weak)) __attribute__ ((section (\"ompss_file_mask\"))) = { "
                << MASK_TASK_NUMBER
                << "}; ";
        
        //Filename hash (so we can know in which order files linked)
        std::stringstream filenameHash;
        filenameHash << hash_str(TL::CompilationProcess::get_current_file().get_filename(true).c_str());
        functions_section << "unsigned int(ompss_mpi_filenames[]) __attribute__((weak)) __attribute__ ((section (\"ompss_file_names\"))) = { "
                << filenameHash.str()
                << "}; ";
        
        //File size (so we "ensure" that both files compiled exactly the same code)
        std::stringstream fileSize;
        fileSize << get_filesize(TL::CompilationProcess::get_current_file().get_filename(true).c_str()) << _currTaskId;
        functions_section << "unsigned int (ompss_mpi_file_sizes[]) __attribute__((weak)) __attribute__ ((section (\"ompss_file_sizes\"))) = { "
                << fileSize.str()
                << "}; ";
        //Number of tasks in file  (used to ensure that both files had the same number, and also for ordering)
        functions_section << "unsigned int (ompss_mpi_file_ntasks[]) __attribute__((weak)) __attribute__ ((section (\"ompss_mpi_file_n_tasks\"))) = { "
                << _currTaskId
                << "}; ";
        //Pointers to the host functions
        functions_section << "void (*ompss_mpi_func_pointers_host[]) __attribute__((weak)) __attribute__ ((section (\"ompss_func_pointers_host\"))) = { "
                << _sectionCodeHost
                << "}; ";
        //Pointers to the device functions
        functions_section << "void (*ompss_mpi_func_pointers_dev[])() __attribute__((weak)) __attribute__ ((section (\"ompss_func_pointers_dev\"))) = { "
                << _sectionCodeDevice
                << "}; ";

        if (IS_FORTRAN_LANGUAGE)
           Source::source_language = SourceLanguage::C;

        Nodecl::NodeclBase functions_section_tree = functions_section.parse_global(_root);

        Source::source_language = SourceLanguage::Current;

        if (IS_FORTRAN_LANGUAGE)
        {
           _extra_c_code.prepend(functions_section_tree);
        } else
        {
           Nodecl::Utils::append_to_top_level_nodecl(functions_section_tree);
        }
    }

    if (!_extra_c_code.is_null()){

        original_filename = TL::CompilationProcess::get_current_file().get_filename();
        std::string new_filename = "mpi_aux_nanox_outline_file_" + original_filename  + ".c";

        FILE* ancillary_file = fopen(new_filename.c_str(), "w");
        if (ancillary_file == NULL)
        {
            fatal_error("%s: error: cannot open file '%s'. %s\n",
                    original_filename.c_str(),
                    new_filename.c_str(),
                    strerror(errno));
        }

        compilation_configuration_t* prev_config = CURRENT_CONFIGURATION;

        compilation_configuration_t* auxcc_configuration = ::get_compilation_configuration("auxcc");
        ERROR_CONDITION (auxcc_configuration == NULL, "auxcc profile is mandatory when using Fortran", 0);
        SET_CURRENT_CONFIGURATION(auxcc_configuration);

        // Make sure phases are loaded (this is needed for codegen)
        load_compiler_phases(auxcc_configuration);

        TL::CompilationProcess::add_file(new_filename, "auxcc");

        ::mark_file_for_cleanup(new_filename.c_str());

        Codegen::CodegenPhase* phase = reinterpret_cast<Codegen::CodegenPhase*>(auxcc_configuration->codegen_phase);
        phase->codegen_top_level(_extra_c_code, ancillary_file, new_filename);

        SET_CURRENT_CONFIGURATION(prev_config);

        fclose(ancillary_file);
        // Do not forget the clear the code for next files
        _extra_c_code = Nodecl::List();
    }
    
    //Clear sources
    Source empty_src;
    _sectionCodeDevice=empty_src;
    _extraFortranDecls=empty_src;
    _sectionCodeHost=empty_src;
    _mpi_task_processed=false;
    _currTaskId=0;
}

void DeviceMPI::pre_run(DTO& dto) {
    _root = *std::static_pointer_cast<Nodecl::NodeclBase>(dto["nodecl"]);
    _mpi_task_processed = false;
}

void DeviceMPI::run(DTO& dto) {
}

std::string DeviceMPI::get_ompss_mpi_type(Type type) {
    std::string result = "ompss_get_mpi_type(mpitype_ompss_";
    type=type.basic_type();
    if (type.is_char()) {
        result += "char";
    } else if (type.is_signed_short_int()) {
        result += "signed_short";
    } else if (type.is_signed_int()) {
        result += "signed_int";
    } else if (type.is_signed_long_int()) {
        result += "signed_long";
    } else if (type.is_signed_char()) {
        result += "signed_char";
    } else if (type.is_unsigned_char()) {
        result += "unsigned_char";
    } else if (type.is_unsigned_short_int()) {
        result += "unsigned_short";
    } else if (type.is_unsigned_int()) {
        result += "unsigned_int";
    } else if (type.is_unsigned_long_int()) {
        result += "unsigned_long";
    } else if (type.is_float()) {
        result += "float";
    } else if (type.is_double()) {
        result += "double";
    } else if (type.is_long_double()) {
        result += "long_double";
    } else if (type.is_bool()) {
        result += "bool";
    } else if (type.is_wchar_t()) {
        result += "wchar_t";
    } else {
        result += "byte";
    }
    result += ")";
    return result;
}

EXPORT_PHASE(TL::Nanox::DeviceMPI);
