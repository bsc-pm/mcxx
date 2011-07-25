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
#include "nanox-gpu.hpp"
#include "tl-declarationclosure.hpp"
#include "tl-multifile.hpp"
#include "cxx-driver-utils.h"
#include "tl-simd.hpp"
#include "nanox-find_common.hpp"

#include <iostream>
#include <fstream>


using namespace TL;
using namespace TL::Nanox;

const unsigned int _vector_width = 1;

static std::string gpu_outline_name(const std::string &task_name)
{
    return "_gpu_" + task_name;
}

static Type compute_replacement_type_for_vla(Type type, 
        ObjectList<Source>::iterator dim_names_begin,
        ObjectList<Source>::iterator dim_names_end)
{
    Type new_type(NULL);
    if (type.is_array())
    {
        new_type = compute_replacement_type_for_vla(type.array_element(), dim_names_begin + 1, dim_names_end);

        if (dim_names_begin == dim_names_end)
        {
            internal_error("Invalid dimension list", 0);
        }

        new_type = new_type.get_array_to(*dim_names_begin);
    }
    else if (type.is_pointer())
    {
        new_type = compute_replacement_type_for_vla(type.points_to(), dim_names_begin, dim_names_end);
        new_type = new_type.get_pointer_to();
    }
    else
    {
        new_type = type;
    }

    return new_type;
}

static bool is_nonstatic_member_symbol(Symbol s)
{
    return s.is_member()
        && !s.is_static();
}

const char* ReplaceSrcGPU::recursive_prettyprint(AST_t a, void* data)
{
    return prettyprint_in_buffer_callback(a.get_internal_ast(), 
            &ReplaceSrcGPU::prettyprint_callback, data);
}


const char* ReplaceSrcGPU::prettyprint_callback (AST a, void* data)
{
    ObjectList<Expression> arg_list;
    std::stringstream result;
    bool constant_evaluation;

    //Standar prettyprint_callback
    const char *c = ReplaceSrcIdExpression::prettyprint_callback(a, data);

    if(c == NULL)
    {
        ReplaceSrcGPU *_this = reinterpret_cast<ReplaceSrcGPU*>(data);

        AST_t ast(a);

        if (FindAttribute(_this->_sl, ATTR_GEN_VEC_NAME).do_(ast))
        {
            return "";
        }
        /*
        if(FindFunction(_this->_sl, BUILTIN_VL_NAME).do_(ast))
        {
            Expression expr(ast, _this->_sl);
            arg_list = expr.get_argument_list();

            if (arg_list.size() != 3){
                internal_error("Wrong number of arguments in __builtin_vector_loop", 0);
            }

            result
                << arg_list[0].get_id_expression()
                << " += "
                << (arg_list[1].evaluate_constant_int_expression(constant_evaluation)
                        * (_vector_width))
               // / arg_list[2].evaluate_constant_int_expression(constant_evaluation)))
                ;
            return result.str().c_str();
        }
        */
        if(FindFunction(_this->_sl, BUILTIN_VR_NAME).do_(ast))
        {
            Expression expr(ast, _this->_sl);
            arg_list = expr.get_argument_list();

            if (arg_list.size() != 1){
                internal_error("Wrong number of arguments in %s", BUILTIN_VR_NAME);
            }

            result << recursive_prettyprint(arg_list[0].get_ast(), data);

            return result.str().c_str();
        }
        if(FindFunction(_this->_sl, BUILTIN_IV_NAME).do_(ast))
        {
            Expression expr(ast, _this->_sl);
            arg_list = expr.get_argument_list();

            if (arg_list.size() != 1){
                internal_error("Wrong number of arguments in %s", BUILTIN_IV_NAME);
            }

            result 
                << "(blockIdx.x * blockDim.x + threadIdx.x) + "
                << prettyprint_in_buffer_callback(arg_list[0].get_ast().get_internal_ast(),
                    &ReplaceSrcGPU::prettyprint_callback, data);

            return result.str().c_str();
        }

        return NULL;
    }

    return c;
}

Source ReplaceSrcGPU::replace(AST_t a) const
{
    Source result;

    const char *c = prettyprint_in_buffer_callback(a.get_internal_ast(),
            &ReplaceSrcGPU::prettyprint_callback, (void*)this);

    // Not sure whether this could happen or not
    if (c != NULL)
    {
        result << std::string(c);
    }

    // The returned pointer came from C code, so 'free' it
    free((void*)c);

    return result;
}


static void do_gpu_outline_replacements(
        AST_t body,
        ScopeLink scope_link,
        const DataEnvironInfo& data_env_info,
        Source &initial_code,
        Source &replaced_outline)
{  
    int i, counter;
    bool constant_evaluation;
    AST_t ast;

    Source copy_setup;
    Scope sc = scope_link.get_scope(body);

    initial_code
        << copy_setup
        ;

    ReplaceSrcGPU replace_src(scope_link);

    bool err_declared = false;

    ObjectList<DataEnvironItem> data_env_items = data_env_info.get_items();

    // Set up all replacements and needed castings
    for (ObjectList<DataEnvironItem>::iterator it = data_env_items.begin();
            it != data_env_items.end();
            it++)
    {
        DataEnvironItem& data_env_item(*it);

        Symbol sym = data_env_item.get_symbol();
        Type type = sym.get_type();
        const std::string field_name = data_env_item.get_field_name();

        if (data_env_item.is_private())
            continue;

        //I think this is not necessary
/*        
        if (data_env_item.is_copy())
        {
            replace_src.add_replacement(sym, "_args->" + field_name);
        }
*/

        if (data_env_item.is_vla_type())
        {
            // These do not require replacement because we define a
            // local variable for them

            ObjectList<Source> vla_dims = data_env_item.get_vla_dimensions();

            ObjectList<Source> arg_vla_dims;
            for (ObjectList<Source>::iterator it = vla_dims.begin();
                    it != vla_dims.end();
                    it++)
            {
                Source new_dim;
                //                new_dim << "_args->" << *it;
                new_dim << *it;

                arg_vla_dims.append(new_dim);
            }

            // Now compute a replacement type which we will use to declare the proper type
            Type repl_type = 
                compute_replacement_type_for_vla(data_env_item.get_symbol().get_type(),
                        arg_vla_dims.begin(), arg_vla_dims.end());

            // Adjust the type if it is an array

            if (repl_type.is_array())
            {
                repl_type = repl_type.array_element().get_pointer_to();
            }

            initial_code
                << repl_type.get_declaration(sym.get_scope(), sym.get_name())
                << "="
                << "(" << repl_type.get_declaration(sym.get_scope(), "") << ")"
                << "("
                //                << "_args->" << field_name
                << field_name
                << ");"
                ;
        }
        else
        {
            // If this is not a copy this corresponds to a SHARED entity
            if (!data_env_item.is_firstprivate())
            {
                if (type.is_array())
                {
                    // Just replace a[i] by (_args->a), no need to derreferentiate
                    Type array_elem_type = type.array_element();
                    // Set up a casting pointer
                    initial_code
                        << array_elem_type.get_pointer_to().get_declaration(sym.get_scope(), field_name) 
                        << "="
                        << "("
                        << array_elem_type.get_pointer_to().get_declaration(sym.get_scope(), "")
                        //                        << ") (_args->" << field_name << ");"
                        << ") (" << field_name << ");"
                        ;
                    replace_src.add_replacement(sym, field_name);
                }
                else
                {
                    // Set up a casting pointer
                    initial_code
                        << type.get_pointer_to().get_declaration(sym.get_scope(), field_name) 
                        << "="
                        << "("
                        << type.get_pointer_to().get_declaration(sym.get_scope(), "")
                        //                        << ") (_args->" << field_name << ");"
                        << ") (" << field_name << ");"
                        ;
                    replace_src.add_replacement(sym, "(*" + field_name + ")");
                }
            }
            // This is a copy, so it corresponds to a FIRSTPRIVATE entity (or something to be copied)
            else
            {
                if (data_env_item.is_raw_buffer())
                {
                    C_LANGUAGE()
                    {
                        replace_src.add_replacement(sym, "(*" + field_name + ")");
                    }
                    CXX_LANGUAGE()
                    {
                        // Set up a reference to the raw buffer properly casted to the data type

                        Type ref_type = type;
                        Type ptr_type = type;

                        if (!type.is_reference())
                        {
                            ref_type = type.get_reference_to();
                            ptr_type = type.get_pointer_to();

                            initial_code
                                << ref_type.get_declaration(sym.get_scope(), field_name)
                                << "(" 
                                << "*(" << ptr_type.get_declaration(sym.get_scope(), "") << ")"
                                //                                << "_args->" << field_name
                                << field_name
                                << ");"
                                ;
                        }
                        else
                        {
                            ptr_type = ref_type.references_to().get_pointer_to();

                            initial_code
                                << ref_type.get_declaration(sym.get_scope(), field_name)
                                << "(" 
                                << "*(" << ptr_type.get_declaration(sym.get_scope(), "") << ")"
                                //                                << "_args->" << field_name
                                << field_name
                                << ");"
                                ;
                        }

                        // This is the neatest aspect of references
                        replace_src.add_replacement(sym, field_name);
                    }
                }
                else
                {
                    //                    replace_src.add_replacement(sym, "(_args->" + field_name + ")");
                    replace_src.add_replacement(sym, "(" + field_name + ")");
                }
            }
        }
    }

    ObjectList<OpenMP::CopyItem> copies = data_env_info.get_copy_items();
    unsigned int j = 0;
    for (ObjectList<OpenMP::CopyItem>::iterator it = copies.begin();
            it != copies.end();
            it++, j++)
    {
        DataReference data_ref = it->get_copy_expression();
        Symbol sym = data_ref.get_base_symbol();
        Type type = sym.get_type();

        if (type.is_array())
        {
            type = type.array_element().get_pointer_to();
        }

        // There are some problems with the typesystem currently
        // that require these workarounds
        if (data_ref.is_shaping_expression())
        {
            // Shaping expressions ([e] a)  have a type of array but we do not
            // want the array but the related pointer
            type = data_ref.get_data_type();
        }
        else if (data_ref.is_array_section_range())
        {
            // Array sections have a scalar type, but the data type will be array
            // See ticket #290
            type = data_ref.get_data_type().array_element().get_pointer_to();
        }

        DataEnvironItem data_env_item = data_env_info.get_data_of_symbol(sym);

        std::string copy_name = data_env_item.get_field_name();

        if (!err_declared)
        {
            copy_setup
                << "\n\tnanos_err_t cp_err;\n"
                ;
            err_declared = true;
        }

        ERROR_CONDITION(!data_env_item.get_symbol().is_valid(),
                "Invalid data for copy symbol", 0);

//        std::string field_addr = "_args->" + data_env_item.get_field_name();

        copy_setup
            << "\t" << type.get_declaration(sc, copy_name) << ";\n"  
            << "\tcp_err = nanos_get_addr(" << j << ", (void**)&" << copy_name << ");\n"
            << "\tif (cp_err != NANOS_OK) nanos_handle_error(cp_err);\n"
            ;

        //replace_src.add_replacement(sym, copy_name);
    }

    // Nonstatic members have a special replacement (this may override some symbols!)
    ObjectList<Symbol> nonstatic_members; 
    nonstatic_members.insert(Statement(body, scope_link)
            .non_local_symbol_occurrences().map(functor(&IdExpression::get_symbol))
            .filter(predicate(is_nonstatic_member_symbol)));
    for (ObjectList<Symbol>::iterator it = nonstatic_members.begin();
            it != nonstatic_members.end();
            it++)
    {
        //        replace_src.add_replacement(*it, "(_args->_this->" + it->get_name() + ")");
        replace_src.add_replacement(*it, "(" + it->get_name() + ")");
    }

    replaced_outline << replace_src.replace(body);
}

DeviceGPU::DeviceGPU()
    : DeviceProvider("gpu")
{
    set_phase_name("Nanox GPU support");
    set_phase_description("This phase is used by Nanox phases to implement GPU device support");
}

void DeviceGPU::pre_run(DTO& dto)
{
}

void DeviceGPU::create_outline(
        const std::string& task_name,
        const std::string& struct_typename,
        DataEnvironInfo &data_environ,
        const OutlineFlags& outline_flags,
        AST_t reference_tree,
        ScopeLink sl,
        Source initial_setup,
        Source outline_body)
{
    /***************** Write the GPU file *****************/

    // Check if the file has already been created (and written)
    bool new_file = false;

    if (_gpuFilename == "") {
        // Set the file name
        _gpuFilename = "gpucc_";
        _gpuFilename += CompilationProcess::get_current_file().get_filename(false);
        size_t file_extension = _gpuFilename.find_last_of(".");
        _gpuFilename.erase(file_extension, _gpuFilename.length());
        _gpuFilename += ".cu";
        new_file = true;

        // Remove the intermediate source file
        mark_file_for_cleanup( _gpuFilename.c_str() );
    }

    const std::string configuration_name = "gpu";
    CompilationProcess::add_file(_gpuFilename, configuration_name, new_file);

    // Get all the needed symbols and GPU included files
    Source included_files, forward_declaration;
    AST_t function_tree;

    // Get *.cu included files
    ObjectList<IncludeLine> lines = CurrentFile::get_top_level_included_files();
    std::string gpu_line (".cu\"");
    std::size_t gpu_size = gpu_line.size();

    for (ObjectList<IncludeLine>::iterator it = lines.begin(); it != lines.end(); it++)
    {
        std::string line = (*it).get_preprocessor_line();
        if (line.size() > gpu_size)
        {
            std::string matching = line.substr(line.size()-gpu_size,gpu_size);
            if (matching == gpu_line)
            {
                included_files << line << "\n";
            }
        }
    }

    // Check if the task is a function, or it is inlined
    if (outline_flags.task_symbol != NULL)
    {
        // Get the definition of non local symbols
        function_tree = outline_flags.task_symbol.get_point_of_declaration();
        LangConstruct construct (function_tree, sl);
        ObjectList<IdExpression> extern_occurrences = construct.non_local_symbol_occurrences();
        DeclarationClosure decl_closure (sl);
        std::set<Symbol> extern_symbols;

        for (ObjectList<IdExpression>::iterator it = extern_occurrences.begin();
                it != extern_occurrences.end();
                it++)
        {
            Symbol s = (*it).get_symbol();
            decl_closure.add(s);

            // TODO: check the symbol is not a global variable
            extern_symbols.insert(s);
        }

        forward_declaration << decl_closure.closure() << "\n";

        for (std::set<Symbol>::iterator it = extern_symbols.begin();
                it != extern_symbols.end(); it++)
        {
            forward_declaration << (*it).get_point_of_declaration().prettyprint_external() << "\n";
        }

        // Check if the task symbol is actually a function definition or a declaration
        if (FunctionDefinition::predicate(function_tree))
        {
            // Check if we have already printed the function definition in the GPU file
            if (_taskSymbols.count(outline_flags.task_symbol.get_name()) == 0) {
                forward_declaration << function_tree.get_enclosing_function_definition().prettyprint_external();

                // Keep record of which tasks have been printed to the GPU file
                // in order to avoid repeating them
                _taskSymbols.insert(outline_flags.task_symbol.get_name());
            }

            // Remove the function definition from the original source code
            function_tree.remove_in_list();
        }
        else
        {
            // Not a function definition
            // Create a filter to search for the definition
            struct FilterFunctionDef : Predicate<AST_t>
            {
                private:
                    Symbol _sym;
                    ScopeLink _sl;
                public:
                    FilterFunctionDef(Symbol sym, ScopeLink sl)
                        : _sym(sym), _sl(sl) { }

                    virtual bool do_(const AST_t& a) const
                    {
                        if (!FunctionDefinition::predicate(a))
                            return false;

                        FunctionDefinition funct_def(a, _sl);

                        Symbol sym = funct_def.get_function_symbol();
                        return _sym == sym;
                    }
            };

            // Search for the function definition
            ObjectList<AST_t> funct_def_list =
                _root.depth_subtrees(FilterFunctionDef(outline_flags.task_symbol, sl));

            if (funct_def_list.size() == 1)
            {
                // Check if we have already printed the function definition in the GPU file
                if (_taskSymbols.count(outline_flags.task_symbol.get_name()) == 0)
                {
                    forward_declaration << funct_def_list[0].get_enclosing_function_definition().prettyprint_external();

                    // Keep record of which tasks have been printed to the GPU file
                    // in order to avoid repeating them
                    _taskSymbols.insert(outline_flags.task_symbol.get_name());
                }

                // Remove the function definition from the original source code
                funct_def_list[0].remove_in_list();
            }
            else if (funct_def_list.size() == 0
                    && _taskSymbols.count(outline_flags.task_symbol.get_name()) > 0)
            {
                // We have already removed it and printed it in the GPU file, do nothing
            }
            else
            {
                std::stringstream msg;
                msg << "Could not find the task function definition of '"
                    << outline_flags.task_symbol.get_name()
                    << "'";
                internal_error(msg.str().c_str(), 0);
            }
        }
    }

    AST_t function_def_tree = reference_tree.get_enclosing_function_definition();
    FunctionDefinition enclosing_function(function_def_tree, sl);

    Source result, arguments_struct_definition, outline_name, 
           parameter_list, argument_list, body, cuda_kernel, body_kernel;
    Source instrument_before, instrument_after;

    result
        << arguments_struct_definition
        << cuda_kernel
        << "\nvoid " << outline_name << "(" << struct_typename << "* const __restrict__ _args)"
        << "{"
        << initial_setup
        << instrument_before
        << body
        << instrument_after
        << "}"
        ;

    // Add the tracing instrumentation if needed
    if (instrumentation_enabled())
    {
        Source uf_name_id, uf_name_descr;
        Source uf_location_id, uf_location_descr;
        Symbol function_symbol = enclosing_function.get_function_symbol();

        instrument_before
            << "static int nanos_funct_id_init = 0;"
            << "static nanos_event_key_t nanos_instr_uf_name_key = 0;"
            << "static nanos_event_value_t nanos_instr_uf_name_value = 0;"
            << "static nanos_event_key_t nanos_instr_uf_location_key = 0;"
            << "static nanos_event_value_t nanos_instr_uf_location_value = 0;"
            << "if (nanos_funct_id_init == 0)"
            << "{"
            <<    "nanos_err_t err = nanos_instrument_get_key(\"user-funct-name\", &nanos_instr_uf_name_key);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
            <<    "err = nanos_instrument_register_value ( &nanos_instr_uf_name_value, \"user-funct-name\","
            <<               uf_name_id << "," << uf_name_descr << ", 0);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"

            <<    "err = nanos_instrument_get_key(\"user-funct-location\", &nanos_instr_uf_location_key);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
            <<    "err = nanos_instrument_register_value ( &nanos_instr_uf_location_value, \"user-funct-location\","
            <<               uf_location_id << "," << uf_location_descr << ", 0);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
            <<    "nanos_funct_id_init = 1;"
            << "}"
            << "nanos_event_t events_before[2];"
            << "events_before[0].type = NANOS_BURST_START;"
            << "events_before[0].info.burst.key = nanos_instr_uf_name_key;"
            << "events_before[0].info.burst.value = nanos_instr_uf_name_value;"
            << "events_before[1].type = NANOS_BURST_START;"
            << "events_before[1].info.burst.key = nanos_instr_uf_location_key;"
            << "events_before[1].info.burst.value = nanos_instr_uf_location_value;"
            << "nanos_instrument_events(2, events_before);"
            // << "nanos_instrument_point_event(1, &nanos_instr_uf_location_key, &nanos_instr_uf_location_value);"
            // << "nanos_instrument_enter_burst(nanos_instr_uf_name_key, nanos_instr_uf_name_value);"
            ;

        instrument_after
            << "nanos_instrument_close_user_fun_event();"
            ;


        if (outline_flags.task_symbol != NULL)
        {
            uf_name_id
                << "\"" << outline_flags.task_symbol.get_name() << "\""
                ;
            uf_location_id
                << "\"" << outline_name << ":" << reference_tree.get_locus() << "\""
                ;

            uf_name_descr
                << "\"Task '" << outline_flags.task_symbol.get_name() << "'\""
                ;
            uf_location_descr
                << "\"It was invoked from function '" << function_symbol.get_qualified_name() << "'"
                << " in construct at '" << reference_tree.get_locus() << "'\""
                ;
        }
        else
        {
            uf_name_id
                << uf_location_id
                ;
            uf_location_id
                << "\"" << outline_name << ":" << reference_tree.get_locus() << "\""
                ;

            uf_name_descr
                << uf_location_descr
                ;
            uf_location_descr
                << "\"Outline created after construct at '"
                << reference_tree.get_locus()
                << "' found in function '" << function_symbol.get_qualified_name() << "'\""
                ;
        }
    }

    // arguments_struct_definition
    Scope sc = sl.get_scope(reference_tree);
    Symbol struct_typename_sym = sc.get_symbol_from_name(struct_typename);

    if (!struct_typename_sym.is_valid())
    {
        running_error("Invalid typename for struct args", 0);
    }

    arguments_struct_definition
        << struct_typename_sym.get_point_of_declaration().prettyprint();
/*
    Source argument_struct_def_fields;
    arguments_struct_definition << "struct " << struct_typename_sym.get_name() << " {"
        << argument_struct_def_fields
        << "\n};\n"
        ;

    Type struct_typename_type = struct_typename_sym.get_type();
    ObjectList<Symbol> data_members_list(struct_typename_type.get_nonstatic_data_members());

    for (ObjectList<Symbol>::iterator it = data_members_list.begin();
            it != data_members_list.end();
            it++)
    {
        argument_struct_def_fields << "\n";

        Symbol sym = (Symbol)(*it);

        argument_struct_def_fields << sym.get_type()
            .get_simple_declaration(sc, sym.get_name())
            << ";"
            ;
    }
*/

    // cuda_kernel
    cuda_kernel
        << "\n\n__global__ void " << outline_name << "_kernel(" << parameter_list << ")"
        << body_kernel
        ;

    // parameter_list & argument_list
    Type struct_typename_type = struct_typename_sym.get_type();
    ObjectList<Symbol> data_member_list(struct_typename_type.get_nonstatic_data_members());
    Symbol sym;
    int i;

    for (i=0;
         i < (data_member_list.size()-1);
         i++)
    {
        sym = (Symbol)data_member_list[i];
        parameter_list 
            << sym.get_type().get_simple_declaration(sc, sym.get_name()) << ", "
            ;

        if (sym.get_type().is_pointer())
        {
            argument_list
                << sym.get_name() << ", "
                ;
        }
        else
        {
            argument_list
                << "_args->" << sym.get_name() << ", "
                ;
        }
    }

    sym = (Symbol)data_member_list[i];
    parameter_list
        << sym.get_type().get_simple_declaration(sc, sym.get_name())
        ;

    if (sym.get_type().is_pointer())
    {
        argument_list
            << sym.get_name()
            ;
    }
    else
    {
        argument_list
            << "_args->" << sym.get_name()
            ;
    }

    // outline_name
    outline_name
        << gpu_outline_name(task_name)
        ;

    // body_kernel
    Source private_vars, final_code;

    body_kernel
        << private_vars
        // << initial_setup
        << outline_body
        << final_code
        ;

    // private_vars
    ObjectList<DataEnvironItem> data_env_items = data_environ.get_items();

    for (ObjectList<DataEnvironItem>::iterator it = data_env_items.begin();
            it != data_env_items.end();
            it++)
    {
        if (!it->is_private())
            continue;

        Symbol sym = it->get_symbol();
        Type type = sym.get_type();

        private_vars
            << type.get_declaration(sym.get_scope(), sym.get_name()) << ";"
            ;
    }

    // final_code
    if (outline_flags.barrier_at_end)
    {
        final_code
            << "nanos_team_barrier();"
            ;
    }

    if (outline_flags.leave_team)
    {
        final_code
            << "nanos_leave_team();"
            ;
    }

    // body
    body
       << "\n" << outline_name 
       << "_kernel<<<1,1>>>("
       << argument_list
       << ");\n"
       ;

    // Parse it in a sibling function context
    // AST_t outline_code_tree =
    //                 result.parse_declaration(enclosing_function.get_ast(), sl);

    std::ofstream gpuFile;
    if (new_file)
    {
        gpuFile.open (_gpuFilename.c_str());
        gpuFile << included_files.get_source(false) << "\n";
    }
    else
    {
        gpuFile.open (_gpuFilename.c_str(), std::ios_base::app);
    }

    gpuFile << "extern \"C\" {\n";
    gpuFile << forward_declaration.get_source(false) << "\n";
    //gpuFile << outline_code_tree.prettyprint_external() << "\n";
    gpuFile << result.get_source();
    // gpuFile << outline_code_tree.prettyprint() << "\n";
    gpuFile << "}\n";
    gpuFile.close();

    /******************* Write the C file ******************/

    // Check if the task is a function, or it is inlined
    if (outline_flags.task_symbol != NULL)
    {
        // We have already removed the function definition
        // Now replace it for the outline declaration
        Source function_decl_src;

        CXX_LANGUAGE()
        {
            function_decl_src
                << "extern \"C\" { "
                ;
        }

        function_decl_src
            << "void " << outline_name << "(" << struct_typename << "*);"
            ;

        CXX_LANGUAGE()
        {
            function_decl_src
                << "}"
                ;
        }

        AST_t function_decl_tree = function_decl_src.parse_declaration(reference_tree, sl);
        reference_tree.prepend_sibling_function(function_decl_tree);
    }
    else
    {
        // Forward declaration of the task outline
        Source outline_declaration_src;

        CXX_LANGUAGE()
        {
            outline_declaration_src
                << "extern \"C\" { "
                ;
        }

        outline_declaration_src
            << "void " << outline_name << "(" << parameter_list << ");"
            ;
        CXX_LANGUAGE()
        {
            outline_declaration_src
                << "}"
                ;
        }
        AST_t outline_declaration_tree = outline_declaration_src.parse_declaration(reference_tree, sl);
        reference_tree.prepend_sibling_function(outline_declaration_tree);
    }
}

void DeviceGPU::get_device_descriptor(const std::string& task_name,
        DataEnvironInfo &data_environ,
        const OutlineFlags& outline_flags,
        AST_t reference_tree,
        ScopeLink sl,
        Source &ancillary_device_description,
        Source &device_descriptor)
{
	Source outline_name;
	if (!outline_flags.implemented_outline)
	{
		outline_name
			<< gpu_outline_name(task_name)
		;
	}
	else
	{
		outline_name << task_name;
	}

	ancillary_device_description
		<< comment("GPU device descriptor")
		<< "static nanos_smp_args_t " 
        << task_name << "_gpu_args = { (void(*)(void*))" << outline_name << "};"
		;

	device_descriptor
		<< "{ nanos_gpu_factory, nanos_gpu_dd_size, &" << task_name << "_gpu_args },"
		;
}

void DeviceGPU::do_replacements(DataEnvironInfo& data_environ,
        AST_t body,
        ScopeLink scope_link,
        Source &initial_setup,
        Source &replaced_src)
{
    do_gpu_outline_replacements(body,
            scope_link,
            data_environ,
            initial_setup,
            replaced_src);

}

EXPORT_PHASE(TL::Nanox::DeviceGPU);
