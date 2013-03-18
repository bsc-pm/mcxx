/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center
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
// #include "fortran03-scope.h"

//#include "cuda-aux.hpp"
//#include "tl-declarationclosure.hpp"

//#include "tl-cuda.hpp"
//#include "tl-omp-nanox.hpp"

#include "codegen-phase.hpp"
#include "codegen-cxx.hpp"
#include "cxx-cexpr.h"
#include "filename.h"
//#include "codegen-fortran.hpp"

//#include <iostream>
//#include <fstream>

#include <errno.h>
#include "cxx-driver-utils.h"

using namespace TL;
using namespace TL::Nanox;

static std::string get_outline_name(const std::string & name) {
    return "mpi_" + name;
}

void DeviceMPI::generate_additional_mpi_code(
        const TL::ObjectList<Nodecl::NodeclBase>& onto_clause,
        const TL::Symbol& struct_args,
        const std::string& outline_name,
        TL::Source& code_host,
        TL::Source& code_device_pre,        
        TL::Source& code_device_post) {

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
    new_dev_info.append("-2");

    Source struct_mpi_create;
    Source hostCall;
    Source deviceCall;

    code_host << "MPI_Status ompss___status; "
            << struct_mpi_create
            << hostCall;
    
    code_device_pre << struct_args.get_name() << " args;"
            << "MPI_Comm ompss_parent_comp; "            
            << "MPI_Comm_get_parent(&ompss_parent_comp);"
            << "MPI_Status ompss___status; "
            << struct_mpi_create
            << deviceCall;

    Source typelist_src, blocklen_src, displ_src;
    //Source parameter_call;
    
    struct_mpi_create << "MPI_Datatype ompss___datatype;"
            "MPI_Datatype ompss___typelist[" << num_params << "]= {" << typelist_src << "};"
            "int ompss___blocklen[" << num_params << "] = {" << blocklen_src << "};"
            "MPI_Aint ompss___displ[" << num_params << "] = {" << displ_src << "};";
    
    const std::string& device_outline_name = get_outline_name(outline_name);
    hostCall << " int id_func_ompss=" << "ompss_mpi_get_function_index_host((void *)" << device_outline_name << "_host)" << ";";
    hostCall << " nanos_MPI_Send_taskinit(&id_func_ompss, 1,  ompss_get_mpi_type(\"__mpitype_ompss_signed_int\")," + new_dev_info[1] + " , " + new_dev_info[0] + ");";
    hostCall << " nanos_MPI_Send_datastruct( (void *) &args, 1,  ompss___datatype," + new_dev_info[1] + "," + new_dev_info[0] + ");";
    hostCall << " nanos_MPI_Recv_taskend(&id_func_ompss, 1,  ompss_get_mpi_type(\"__mpitype_ompss_signed_int\")," + new_dev_info[1] + " , " + new_dev_info[0] + ",&ompss___status);";

    deviceCall << " nanos_MPI_Recv_datastruct(&args, 1, ompss___datatype, 0, ompss_parent_comp, &ompss___status); ";
    //deviceCall << called_task.get_name() << "(" << parameter_call << ");";

    
    code_device_post << " int ompss_id_func=" << _currTaskId << ";";
    code_device_post << " nanos_MPI_Send_taskend(&ompss_id_func, 1, ompss_get_mpi_type(\"__mpitype_ompss_signed_int\"), 0, ompss_parent_comp);";

    for (int i = 0; i < num_params; ++i) {
        //parameter_call.append_with_separator("args." + parameters_called[i].get_name(),",");
        std::string ompss_mpi_type = get_ompss_mpi_type(parameters_called[i].get_type());
        displ_src.append_with_separator("((size_t) ( (char *)&((" + struct_args.get_name() + " *)0)->" + parameters_called[i].get_name() + " - (char *)0 ))", ",");
        if (parameters_called[i].get_type().is_pointer()) {
            typelist_src.append_with_separator("ompss_get_mpi_type(\"__mpitype_ompss_unsigned_long_long\")", ",");

            blocklen_src.append_with_separator("1", ",");
        } else {
            typelist_src.append_with_separator(ompss_mpi_type, ",");

            if (parameters_called[i].get_type().array_has_size()) {
                blocklen_src.append_with_separator(parameters_called[i].get_type().array_get_size().prettyprint(), ",");
            } else {
                blocklen_src.append_with_separator("1", ",");
            }
        }
        
    }

    struct_mpi_create << "MPI_Type_create_struct( " << num_params << ", ompss___blocklen, ompss___displ, ompss___typelist, &ompss___datatype); ";
    struct_mpi_create << "MPI_Type_commit(&ompss___datatype);";


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
        Nodecl::Utils::SymbolMap* &symbol_map) {
    
    symbol_map = new Nodecl::Utils::SimpleSymbolMap();
    
    if (IS_FORTRAN_LANGUAGE)
        running_error("Fortran for MPI devices is not supported yet\n", 0);
    

    // Unpack DTO 
    const std::string& device_outline_name = get_outline_name(info._outline_name);
    const Nodecl::NodeclBase& task_statements = info._task_statements;
    const Nodecl::NodeclBase& original_statements = info._original_statements;
    const TL::Symbol& called_task = info._called_task;
    //OutlineInfo& outline_info = info._outline_info;
    
    //At first time we process a task, declare a function
    if (!_mpi_task_processed){
        _mpi_task_processed = true;
        Source search_function;
        search_function << "typedef float(*ptrToFunc)(float, float);";
        search_function << "extern int ompss_mpi_get_function_index_host(void* func);";
        Nodecl::NodeclBase search_function_tree = search_function.parse_global(_root);
        Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, search_function_tree);
    }

    ERROR_CONDITION(called_task.is_valid() && !called_task.is_function(),
            "The '%s' symbol is not a function", called_task.get_name().c_str());

    TL::Symbol current_function =
            original_statements.retrieve_context().get_decl_context().current_scope->related_entry;

    if (current_function.is_nested_function()) {
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            running_error("%s: error: nested functions are not supported\n",
                original_statements.get_locus().c_str());
    }

    Source unpacked_arguments, private_entities;

    TL::ObjectList<OutlineDataItem*> data_items = info._data_items;
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++) {
        switch ((*it)->get_sharing()) {
            case OutlineDataItem::SHARING_PRIVATE:
            {
                if (IS_CXX_LANGUAGE)
                {
                    // We need the declarations of the private symbols!
                    private_entities << as_statement(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), (*it)->get_symbol()));
                }
                break;
            }
            case OutlineDataItem::SHARING_SHARED:
            case OutlineDataItem::SHARING_CAPTURE:
            case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
            {
                TL::Type param_type = (*it)->get_in_outline_type();

                Source argument;
                if (IS_C_LANGUAGE || IS_CXX_LANGUAGE) {
                    // Normal shared items are passed by reference from a pointer,
                    // derreference here
                    if ((*it)->get_sharing() == OutlineDataItem::SHARING_SHARED
                            && !(IS_CXX_LANGUAGE && (*it)->get_symbol().get_name() == "this")) {
                        argument << "*(args." << (*it)->get_field_name() << ")";
                    }// Any other thing is passed by value
                    else {
                        argument << "args." << (*it)->get_field_name();
                    }

                    if (IS_CXX_LANGUAGE
                            && (*it)->get_allocation_policy() == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DESTROY) {
                        internal_error("Not yet implemented: call the destructor", 0);
                    }
                } else {
                    internal_error("running error", 0);
                }

                unpacked_arguments.append_with_separator(argument, ", ");
                break;
            }
            case OutlineDataItem::SHARING_REDUCTION:
            {
                // // Pass the original reduced variable as if it were a shared
                Source argument;
                if (IS_C_LANGUAGE || IS_CXX_LANGUAGE) {
                    argument << "*(args." << (*it)->get_field_name() << ")";
                } else {
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

    // Add the user function to the intermediate file
    //    if (called_task.is_valid()
    //            && !called_task.get_function_code().is_null())
    //    {
    //        _cuda_file_code.append(Nodecl::Utils::deep_copy(
    //                    called_task.get_function_code(),
    //                    called_task.get_scope()));
    //
    //        // Remove the user function definition from the original source because
    //        // It is used only in the intermediate file
    //        Nodecl::Utils::remove_from_enclosing_list(called_task.get_function_code());
    //    }



    ObjectList<std::string> structure_name;
    ObjectList<TL::Type> structure_type;
    // Create the new unpacked function
    TL::Symbol device_function = new_function_symbol(
            current_function,
            device_outline_name + "_device",
            TL::Type::get_void_type(),
            structure_name,
            structure_type);

    // Create the outline function
    //The outline function has always only one parameter which name is 'args'
    structure_name.append("args");

    //The type of this parameter is an struct (i. e. user defined type)
    structure_type.append(TL::Type(
            get_user_defined_type(
            info._arguments_struct.get_internal_symbol())).get_lvalue_reference_to());

    TL::Symbol host_function = new_function_symbol(
            current_function,
            device_outline_name + "_host",
            TL::Type::get_void_type(),
            structure_name,
            structure_type);

    Source code_host;
    Source code_device_pre;
    Source code_device_post;
    //if (called_task.is_valid()) {
        generate_additional_mpi_code(
                info._target_info.get_onto(),
                info._arguments_struct,
                info._outline_name,
                code_host,
                code_device_pre,
                code_device_post);
    //}

    // Create the new unpacked function
    Source dummy_initial_statements, dummy_final_statements;
    TL::Symbol unpacked_function = new_function_symbol_unpacked(
            current_function,
            device_outline_name + "_unpacked",
            info,
            symbol_map,
            dummy_initial_statements,
            dummy_final_statements);

    Nodecl::NodeclBase unpacked_function_code, unpacked_function_body;
    build_empty_body_for_function(unpacked_function,
            unpacked_function_code,
            unpacked_function_body);

    Source unpacked_source;
    unpacked_source
            << "{"
            << private_entities
            //<< code_host
            << statement_placeholder(outline_placeholder)
            << "}"
            ;

    Nodecl::NodeclBase new_unpacked_body =
            unpacked_source.parse_statement(unpacked_function_body);
    unpacked_function_body.replace(new_unpacked_body);


    // Add the unpacked function to the file
    Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, unpacked_function_code);

    // Add a declaration of the unpacked function symbol in the original source
    if (IS_CXX_LANGUAGE) {
        Nodecl::NodeclBase nodecl_decl = Nodecl::CxxDecl::make(
                /* optative context */ nodecl_null(),
                host_function,
                original_statements.get_filename(),
                original_statements.get_line());
        Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, nodecl_decl);
    }


    Nodecl::NodeclBase host_function_code, host_function_body;
    build_empty_body_for_function(host_function,
            host_function_code,
            host_function_body);

    Source host_src,
            instrument_before,
            instrument_after;

    host_src
            << "{"
            << instrument_before
            << code_host
            << instrument_after
            //<< statement_placeholder(outline_placeholder)
            << "}"
            ;

    Nodecl::NodeclBase new_host_body = host_src.parse_statement(host_function_body);
    host_function_body.replace(new_host_body);
    Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, host_function_code);

    Nodecl::NodeclBase device_function_code, device_function_body;
    build_empty_body_for_function(device_function,
            device_function_code,
            device_function_body);

    Source device_src;

    device_src
            << "{"
            << code_device_pre
            << device_outline_name << "_unpacked(" << unpacked_arguments << ");"
            << code_device_post
            << "}"
            ;
    Nodecl::NodeclBase new_device_body = device_src.parse_statement(device_function_body);
    device_function_body.replace(new_device_body);
    Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, device_function_code);
    
    output_statements = task_statements;
}

//

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
    if (Nanos::Version::interface_is_at_least("master", 5012)) {
        ObjectList<Nodecl::NodeclBase> onto_clause = target_information.get_onto();
        Nodecl::Utils::SimpleSymbolMap param_to_args_map = info._target_info.get_param_arg_map();
        std::string assignedComm = "0";
        std::string assignedRank = "-2";
        if (onto_clause.size() >= 1) {
            assignedComm = as_symbol(param_to_args_map.map(onto_clause.at(0).get_symbol()));
        }
        if (onto_clause.size() >= 2) {
            assignedRank = as_symbol(param_to_args_map.map(onto_clause.at(1).get_symbol()));
        }
        ancillary_device_description
                << comment("MPI device descriptor")
                << "static nanos_mpi_args_t "
                << device_outline_name << "_mpi_args;"
                << device_outline_name << "_mpi_args.outline = (void(*)(void*))" << device_outline_name << "_host;"
                << device_outline_name << "_mpi_args._assignedComm = " << assignedComm << ";"
                << device_outline_name << "_mpi_args._assignedRank = " << assignedRank << ";";
        
        _sectionCodeHost.append_with_separator("(void*)" + device_outline_name + "_host",",");
        
        _sectionCodeDevice.append_with_separator("(void(*)())" + device_outline_name + "_device",",");

    } else {
        internal_error("Unsupported Nanos version.", 0);
    }

    device_descriptor << "{ &nanos_mpi_factory, &" << device_outline_name << "_mpi_args }";
    
    _currTaskId++;    
}

bool DeviceMPI::remove_function_task_from_original_source() const
{
    return false;
}

bool DeviceMPI::allow_mandatory_creation() {
    return true;
}


void DeviceMPI::copy_stuff_to_device_file(const TL::ObjectList<Nodecl::NodeclBase>& stuff_to_be_copied) {
    //    for (Nodecl::List::iterator it = symbols.begin();
    //            it != symbols.end();
    //            ++it)
    //    {
    //        Symbol sym = (*it).as<Nodecl::Symbol>().get_symbol();
    //        if (sym.is_function()
    //                && !sym.get_function_code().is_null())
    //        {
    //            _cuda_file_code.append(Nodecl::Utils::deep_copy(
    //                        sym.get_function_code(),
    //                        sym.get_scope()));
    //        }
    //    }
}

static std::ifstream::pos_type get_filesize(const char* filename)
{
    std::ifstream in(filename, std::ifstream::in | std::ifstream::binary);
    in.seekg(0, std::ifstream::end);
    return in.tellg(); 
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
    Source _mpiDaemonMain;
    _mpiDaemonMain << "int ompss___mpi_daemon_main(int argc, char* argv[]) { "
            << " nanos_MPI_Init(&argc, &argv);	"
            << "nanos_sync_dev_pointers(ompss_mpi_masks, "<< MASK_TASK_NUMBER << ", ompss_mpi_filenames, ompss_mpi_file_sizes,"
            << "ompss_mpi_file_ntasks,ompss_mpi_func_pointers_dev);"
            << " int ompss_id_func; "
            << " MPI_Status ompss___status; "
            << " MPI_Comm ompss_parent_comp; "
            << " MPI_Comm_get_parent(&ompss_parent_comp);"
            << " while(1){ "
            << " nanos_MPI_Recv_taskinit(&ompss_id_func, 1, ompss_get_mpi_type(\"__mpitype_ompss_signed_int\"), 0, ompss_parent_comp, &ompss___status); "
            << " if (ompss_id_func==-1){ "
            << " nanos_MPI_Finalize(); "
            << " return 0; "
            << " } else { "
            << " void (* function_pointer)()=(void (*)()) ompss_mpi_func_pointers_dev[ompss_id_func];"
            << " function_pointer();"
            << "  }}}"; //END main

    // Check if the file has already been created (and written)
    //std::ofstream mpiFile;
    //std::string _mpiFilename;
    //bool new_file = false;
    
    //if (_mpi_task_processed) {
        //In the host function void* are OK, they'll identify functions
    Symbol main = _root.retrieve_context().get_symbol_from_name("main");
    if (_mpi_task_processed || main.is_valid()) {
        Source functions_section;                 
        functions_section << "int (ompss_mpi_masks[]) __attribute__((weak)) __attribute__ ((section (\"ompss_file_mask\"))) = { "
                << MASK_TASK_NUMBER
                << "}; ";
        functions_section << "unsigned int(ompss_mpi_filenames[]) __attribute__((weak)) __attribute__ ((section (\"ompss_file_names\"))) = { "
                << hash_str(TL::CompilationProcess::get_current_file().get_filename().c_str())
                << "}; ";
        functions_section << "unsigned int (ompss_mpi_file_sizes[]) __attribute__((weak)) __attribute__ ((section (\"ompss_file_sizes\"))) = { "
                << get_filesize(TL::CompilationProcess::get_current_file().get_filename().c_str()) << _currTaskId
                << "}; ";
        functions_section << "unsigned int (ompss_mpi_file_ntasks[]) __attribute__((weak)) __attribute__ ((section (\"ompss_mpi_file_n_tasks\"))) = { "
                << _currTaskId
                << "}; ";
        functions_section << "void (*ompss_mpi_func_pointers_host[]) __attribute__((weak)) __attribute__ ((section (\"ompss_func_pointers_host\"))) = { "
                << _sectionCodeHost
                << "}; ";
                //<<"extern void(*__datahost_start[]);";
        //In device functions, store a real function pointer so we can call it correctly regardless of the architecture
        functions_section << "void (*ompss_mpi_func_pointers_dev[])() __attribute__((weak)) __attribute__ ((section (\"ompss_func_pointers_dev\"))) = { "
                << _sectionCodeDevice
                << "}; ";
                //<<"extern void(*__datadev_start[]);";
        Nodecl::NodeclBase functions_section_tree = functions_section.parse_global(_root);
        Nodecl::Utils::append_to_top_level_nodecl(functions_section_tree);
    }
        
        //Source included_files;
//        if (0 && CompilationProcess::get_current_file().get_filename(false).find("ompss___mpiWorker_") == std::string::npos) {
//                // Set the file name 
//                _mpiFilename = "ompss___mpiWorker_";
//                _mpiFilename += CompilationProcess::get_current_file().get_filename(false);
//                new_file = true;
//                mpiFile.open(_mpiFilename.c_str());
//                std::ifstream streamFile(CompilationProcess::get_current_file().get_filename(true).c_str());
//                mpiFile << streamFile.rdbuf();
//                // Remove the intermediate source file
//                mark_file_for_cleanup(_mpiFilename.c_str());
//                //Add a copy of current file for MIC
//                const std::string configuration_name = "mic";
//                //CompilationProcess::add_file(_mpiFilename, configuration_name, new_file);
//                mpiFile.close();
//        }

        if (main.is_valid()) {
            Source real_main;
            real_main << "int ompss_tmp_main(int argc, char* argv[]) {"
                    << "nanos_set_MPI_control_pointers(ompss_mpi_masks, "<< MASK_TASK_NUMBER << ", ompss_mpi_filenames, ompss_mpi_file_sizes);"
                    << "if (argc > 1 && !strcmp(argv[argc-1],\"" << TAG_MAIN_OMPSS << "\")){"
                    << "ompss___mpi_daemon_main(argc,argv);"
                    << "return 0;"
                    << "} else {"
                    << "return main(argc,argv);"
                    << "}}"
                    ;

            Nodecl::NodeclBase newompss_main = _mpiDaemonMain.parse_global(_root);
            Nodecl::NodeclBase new_main = real_main.parse_global(main.get_function_code());
            Nodecl::Utils::append_to_top_level_nodecl(newompss_main);
            Nodecl::Utils::append_to_top_level_nodecl(new_main); 
            main.set_name("ompss___user_main");
            //lRoot.at(lRoot.size()-1).append_sibling(new_main);
            _root.retrieve_context().get_symbol_from_name("ompss_tmp_main").set_name("main");


            //This function search for it's index in the pointer arrays
            //so we can pass it to the device array, we only add it on main
            Source search_function;
            //There can't be errors here, sooner or later we'll find the pointer
            search_function << "int ompss_mpi_get_function_index_host(void* func_pointer){"
                            "int i=0;"
                            "for (i=0;ompss_mpi_func_pointers_host[i]!=func_pointer;i++);"
                            "return i;"
                            "}";       

            Nodecl::NodeclBase search_function_tree = search_function.parse_global(_root);
            Nodecl::Utils::append_to_top_level_nodecl(search_function_tree); 
            //ast_dump_graphviz(nodecl_get_ast(search_function_tree.get_internal_nodecl()),stderr);
        }
    //}
}

void DeviceMPI::pre_run(DTO& dto) {
    _root = dto["nodecl"];
    _mpi_task_processed = false;
}

void DeviceMPI::run(DTO& dto) {
}

std::string DeviceMPI::get_ompss_mpi_type(Type type) {
    std::string result = "ompss_get_mpi_type(\"__mpitype_ompss_";
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
    result += "\")";
    return result;
}

EXPORT_PHASE(TL::Nanox::DeviceMPI);
