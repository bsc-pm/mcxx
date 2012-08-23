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
#include "tl-nanos.hpp"
#include "nanox-cuda.hpp"
#include "cuda-aux.hpp"
#include "tl-declarationclosure.hpp"
#include "tl-multifile.hpp"
#include "tl-cuda.hpp"
#include "tl-omp-nanox.hpp"

#include <iostream>
#include <fstream>

#include "cxx-driver-utils.h"

using namespace TL;
using namespace TL::Nanox;

static std::string gpu_outline_name(const std::string &task_name)
{
	return "_gpu_" + task_name;
}

std::string DeviceCUDA::get_header_macro()
{
	std::string macro = "__"  + _cudaHeaderFilename + "__";
	std::string replacement("_");
	char_replace_all_occurrences(macro, std::string("-"), replacement);
	char_replace_all_occurrences(macro, std::string("."), replacement);

	return macro;
}

void DeviceCUDA::char_replace_all_occurrences(std::string &str, // String to process
		std::string original, // Matching pattern
		std::string replaced) // Substitution
{
	size_t pos = str.find(original, original.size());
	while (pos != std::string::npos)
	{
		str.replace(pos, original.size(), replaced.c_str());
		pos = str.find(original, original.size());
	}
}

void DeviceCUDA::replace_kernel_config(AST_t &kernel_call, ScopeLink sl)
{
	CUDA::KernelCall kcall(kernel_call, sl);

	Source new_kernel_call;
	Source new_config, new_param_list, nanos_stream_call;

	new_kernel_call << kcall.get_called_expression() << "<<<" << new_config << ">>>(" << new_param_list << ")";

	ObjectList<Expression> argument_list = kcall.get_argument_list();

	for (ObjectList<Expression>::iterator it = argument_list.begin();
			it != argument_list.end();
			it++)
	{
		new_param_list.append_with_separator(it->prettyprint(), ",");
	}

	nanos_stream_call << "nanos_get_kernel_execution_stream()";
	ObjectList<Expression> kernel_config = kcall.get_kernel_configuration();

	if (kernel_config.size() == 2)
	{
		new_config << kernel_config[0] << ","
				<< kernel_config[1] << ","
				<< "0, "
				<< nanos_stream_call;
	}
	else if (kernel_config.size() == 3)
	{
		new_config << kernel_config[0] << ","
				<< kernel_config[1] << ","
				<< kernel_config[2] << ","
				<< nanos_stream_call;
	}
	else if (kernel_config.size() == 4)
	{
		// Do nothing by now
		new_config << kernel_config[0] << ","
				<< kernel_config[1] << ","
				<< kernel_config[2] << ","
				<< kernel_config[3];
	}
	else
	{
		internal_error("Code unreachable: a kernel call configuration must have between 2 and 4 parameters", 0);
	}

	AST_t expr = new_kernel_call.parse_expression(kernel_call, sl);
	kernel_call.replace(expr);
}

void DeviceCUDA::do_cuda_inline_get_addresses(
		const Scope& sc,
		const DataEnvironInfo& data_env_info,
		Source &copy_setup,
		ReplaceSrcIdExpression& replace_src,
		bool &err_declared)
{
	Source current_wd_param;
	if (Nanos::Version::interface_is_at_least("master", 5005))
	{
		copy_setup
			<< "nanos_wd_t current_wd = nanos_current_wd();"
			;
		current_wd_param
			<< ", current_wd"
			;
	}

	ObjectList<OpenMP::CopyItem> copies = data_env_info.get_copy_items();
	unsigned int j = 0;
	for (ObjectList<OpenMP::CopyItem>::iterator it = copies.begin();
			it != copies.end();
			it++, j++)
	{
		DataReference data_ref = it->get_copy_expression();
		Symbol sym = data_ref.get_base_symbol();

		OpenMP::DataSharingEnvironment &data_sharing = data_env_info.get_data_sharing();
		OpenMP::DataSharingAttribute data_sharing_attr = data_sharing.get_data_sharing(sym);

		DataEnvironItem data_env_item = data_env_info.get_data_of_symbol(sym);

		ERROR_CONDITION(!data_env_item.get_symbol().is_valid(),
				"Invalid data for copy symbol", 0);

		bool is_shared = data_env_item.is_shared();

		Type type = sym.get_type();

		if (type.is_array())
		{
			type = type.array_element().get_pointer_to();
			is_shared = false;
		}
		else if (is_shared)
		{
			type = type.get_pointer_to();
		}

		std::string copy_name = "_cp_" + sym.get_name();

		if (!err_declared)
		{
			copy_setup
				<< "nanos_err_t cp_err;"
				;
			err_declared = true;
		}

		copy_setup
			<< type.get_declaration(sc, copy_name) << ";"
			<< "cp_err = nanos_get_addr(" << j << ", (void**)&" << copy_name << current_wd_param << ");"
			<< "if (cp_err != NANOS_OK) nanos_handle_error(cp_err);"
			;

		if (!is_shared)
		{
			replace_src.add_replacement(sym, copy_name);
		}
		else
		{
			replace_src.add_replacement(sym, "(*" + copy_name + ")");
		}
	}
}

void DeviceCUDA::do_cuda_outline_replacements(
		AST_t body,
		ScopeLink scope_link,
		const DataEnvironInfo& data_env_info,
		Source &initial_code,
		Source &replaced_outline)
{

	Source copy_setup;
	Scope sc = scope_link.get_scope(body);

	initial_code
		<< copy_setup
		;

	ReplaceSrcIdExpression replace_src(scope_link);

	bool err_declared = false;

	ObjectList<DataEnvironItem> data_env_items = data_env_info.get_items();
	for (ObjectList<DataEnvironItem>::iterator it = data_env_items.begin();
			it != data_env_items.end();
			it++)
	{
		DataEnvironItem& data_env_item(*it);

		Symbol sym = data_env_item.get_symbol();
		const std::string field_name = data_env_item.get_field_name();

		if (data_env_item.is_private())
		{
			// Do nothing as they are private, we create a variable with the
			// same original name
		}
		else if (data_env_item.is_firstprivate()
				|| data_env_item.is_shared())
		{
			replace_src.add_replacement(sym, "_args->" + field_name);
		}
		// else if (data_env_item.is_shared())
		// {
		// 	replace_src.add_replacement(sym, "(*_args->" + field_name + ")");
		// }
		else
		{
			internal_error("Code unreachable", 0);
		}
	}

	if (create_translation_function())
	{
		// We already created a function that performs the translation in the runtime
		copy_setup
			<< comment("Translation is done by the runtime")
			;
	}
	else
	{
		do_cuda_inline_get_addresses(
				sc,
				data_env_info,
				copy_setup,
				replace_src,
				err_declared);
	}

	replaced_outline << replace_src.replace(body);
}

DeviceCUDA::DeviceCUDA()
: DeviceProvider("cuda"), _cudaFilename(""), _cudaHeaderFilename("")
{
	set_phase_name("Nanox CUDA support");
	set_phase_description("This phase is used by Nanox phases to implement CUDA device support");
}

void DeviceCUDA::get_output_file(std::ofstream& cudaFile)
{
	std::ofstream header;
	get_output_file(cudaFile, header);
	header.close();
}

void DeviceCUDA::get_output_file(std::ofstream& cudaFile, std::ofstream& cudaHeaderFile)
{
	// Check if the file has already been created (and written)
	bool new_file = false;

	Source included_files;
	if (_cudaFilename == "")
	{
		// Set the file name
		_cudaFilename = "cudacc_";
		_cudaFilename += CompilationProcess::get_current_file().get_filename(false);
		size_t file_extension = _cudaFilename.find_last_of(".");
		_cudaFilename.erase(file_extension, _cudaFilename.length());

		// Set the header file name here since it is easier
		_cudaHeaderFilename = _cudaFilename + ".cuh";

		// Set the cuda file's extension
		_cudaFilename += ".cu";

		new_file = true;

		// Remove the intermediate source files
		mark_file_for_cleanup( _cudaFilename.c_str() );
		mark_file_for_cleanup( _cudaHeaderFilename.c_str() );

		// Get *.cu included files
		ObjectList<IncludeLine> lines = CurrentFile::get_top_level_included_files();
		std::string cuda_file_ext(".cu\"");
		std::string cuda_header_ext(".cuh\"");

		for (ObjectList<IncludeLine>::iterator it = lines.begin(); it != lines.end(); it++)
		{
			std::string line = (*it).get_preprocessor_line();
			std::string extension = line.substr(line.find_last_of("."));

			if (extension == cuda_file_ext || extension == cuda_header_ext)
				included_files << line << "\n";
		}
	}

	const std::string configuration_name = "cuda";
	CompilationProcess::add_file(_cudaFilename, configuration_name, new_file);

	if (new_file)
	{
		cudaFile.open (_cudaFilename.c_str());
		cudaHeaderFile.open(_cudaHeaderFilename.c_str());

		cudaFile << "#include \"" << _cudaHeaderFilename.c_str() << "\"\n";

		// Protect the header with ifndef/define/endif
		std::string def = get_header_macro();
		cudaHeaderFile << "#ifndef " << def.c_str() << "\n";
		cudaHeaderFile << "#define " << def.c_str() << "\n";

		// Add .cu / .cuh includes
		cudaHeaderFile << included_files.get_source(false) << "\n";
	}
	else
	{
		cudaFile.open (_cudaFilename.c_str(), std::ios_base::app);
		cudaHeaderFile.open(_cudaHeaderFilename.c_str(), std::ios_base::app);
	}
}

void DeviceCUDA::insert_function_definition(PragmaCustomConstruct ctr, bool is_copy)
{
	std::ofstream cudaFile;
	get_output_file(cudaFile);

	bool needs_device = false;
	bool is_global_defined = false;
	bool needs_global = false;
	bool needs_extern_c = false;
	AST_t decl = ctr.get_declaration();

	if (FunctionDefinition::predicate(decl))
	{
		// unless we find a kernel configuration call
		needs_device = true;

		FunctionDefinition funct_def(decl, ctr.get_scope_link());
		Statement stmt = funct_def.get_function_body();

		if (!stmt.get_ast().depth_subtrees(PredicateType(AST_CUDA_KERNEL_CALL)).empty())
		{
			needs_device = false;
		}

		if (_fwdSymbols.count(funct_def.get_function_symbol()) != 0)
		{
			// Nothing to do here, already defined
			return;
		}

		_fwdSymbols.insert(funct_def.get_function_symbol());
	}
	else if (Declaration::predicate(decl))
	{
		Declaration declaration(decl, ctr.get_scope_link());

		DeclarationSpec decl_specifier_seq = declaration.get_declaration_specifiers();
		if (decl_specifier_seq.get_ast().depth_subtrees(PredicateType(AST_TYPEDEF_SPEC)).empty())
		{
			needs_device = true;
		}

		ObjectList<DeclaredEntity> declared_entities = declaration.get_declared_entities();

		ObjectList<Symbol> sym_list;
		for (ObjectList<DeclaredEntity>::iterator it = declared_entities.begin();
				it != declared_entities.end();
				it++)
		{
			sym_list.insert(it->get_declared_symbol());
		}

		for (ObjectList<Symbol>::iterator it = sym_list.begin();
				it != sym_list.end();
				it++)
		{
			if (_function_task_set->is_function_task_or_implements(*it))
			{
				needs_device = false;
			}
		}
	}

	if (needs_device)
	{
		// Check the kernel is not labeled as __global__
		std::string global("global");
		ObjectList<AST_t> ast_list = decl.depth_subtrees(GCCAttributeSpecifier::predicate);
		for (ObjectList<AST_t>::iterator it = ast_list.begin(); it != ast_list.end(); it++)
		{
			AST_t &ast = *it;
			if (GCCAttributeSpecifier::predicate(ast))
			{
				GCCAttributeSpecifier gcc_attr_spec(ast, ctr.get_scope_link());
				ObjectList<GCCAttribute> gcc_attributes = gcc_attr_spec.get_gcc_attribute_list();

				if (gcc_attributes.contains(functor(&GCCAttribute::get_name), global) && gcc_attributes.size() == 1)
				{
					// Remove the attribute from the list to avoid __attribute__((global)) appear in the CUDA file
					ast.remove_in_list();
					is_global_defined = true;
					needs_global = true;
					break;
				}
			}
		}
	}

	if (!needs_device
			&& IS_C_LANGUAGE)
	{
		needs_extern_c = true;
	}

	if (needs_extern_c)
	{
		cudaFile << "extern \"C\" {\n";
	}

	if (needs_device)
	{
		if (needs_global)
		{
			cudaFile << "__global__ ";
		}
		else if (is_global_defined) {
			// Already defined, no need to add neither __global__ nor __device__
		}
		else
		{
			cudaFile << "__device__ ";
		}
	}

	cudaFile << decl.prettyprint_external() << "\n";

	if (needs_extern_c)
	{
		cudaFile << "}\n";
	}

	cudaFile.close();

	if (!is_copy)
	{
		ctr.get_ast().remove_in_list();
	}
}

void DeviceCUDA::insert_declaration(PragmaCustomConstruct ctr, bool is_copy)
{
	insert_function_definition(ctr, is_copy);
}

void DeviceCUDA::insert_instrumentation_code(
		Symbol &function_symbol,
		Source outline_name,
		const OutlineFlags& outline_flags,
		AST_t reference_tree,
		Source &instrument_before,
		Source &instrument_after)
{
	Source uf_name_id, uf_name_descr;
	Source uf_location_id, uf_location_descr;
	//Symbol function_symbol = enclosing_function.get_function_symbol();

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
			<< "\"'" << function_symbol.get_qualified_name() << "'"
			<< " invoked at '" << reference_tree.get_locus() << "'\""
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
			<< "\"Outline from '"
			<< reference_tree.get_locus()
			<< "' in '" << function_symbol.get_qualified_name() << "'\""
			;
	}
}

void DeviceCUDA::create_outline(
		const std::string& task_name,
		const std::string& struct_typename,
		DataEnvironInfo &data_environ,
		const OutlineFlags& outline_flags,
		AST_t reference_tree,
		ScopeLink sl,
		Source initial_setup,
		Source outline_body)
{
	/***************** Write the CUDA file *****************/

	// Check if the task is a function, or it is inlined
	// Outline tasks need more work to do
	bool is_outline_task = (outline_flags.task_symbol != NULL);

	ObjectList<IdExpression> extern_occurrences;
	DeclarationClosure decl_closure (sl);
	std::set<Symbol> extern_symbols;

	// Get all the needed symbols and CUDA included files
	Source forward_declaration;
	AST_t function_tree = (is_outline_task ?
			outline_flags.task_symbol.get_point_of_declaration() :
			reference_tree);

	// Get the definition of non local symbols
	LangConstruct construct (function_tree, sl);
	extern_occurrences = construct.non_local_symbol_occurrences(LangConstruct::ALL_SYMBOLS);

	for (ObjectList<IdExpression>::iterator it = extern_occurrences.begin();
			it != extern_occurrences.end();
			it++)
	{
		Symbol s = it->get_symbol();

		// TODO: check the symbol is not a global variable
		// Ignore non-constant variables
		if (s.is_variable()
				&& !s.get_type().is_const())
			continue;

		if (s.get_internal_symbol()->kind == SK_ENUMERATOR)
		{
			s = s.get_type().get_symbol();
		}
		while (s.is_member())
		{
			s = s.get_class_type().get_symbol();
		}

		// Check we have not already added the symbol
		if (_fwdSymbols.count(s) == 0)
		{
			_fwdSymbols.insert(s);
			//decl_closure.add(s);

			extern_symbols.insert(s);
		}
	}

	// Check we have the definition of all symbol local occurrences, like typedef's
	ObjectList<IdExpression> local_occurrences;
	local_occurrences = construct.all_symbol_occurrences(LangConstruct::ALL_SYMBOLS);

	for (ObjectList<IdExpression>::iterator it = local_occurrences.begin();
			it != local_occurrences.end();
			it++)
	{
		Symbol s = it->get_symbol();

		// If this symbol comes from the guts of CUDA, ignore it
		if (CheckIfInCudacompiler::check(s.get_filename()))
			continue;

		// Let's check its type as well
		TL::Type t = s.get_type();
		if (CheckIfInCudacompiler::check_type(t))
			continue;

		// Check we have not already added the symbol
		if (_localDecls.find(s.get_internal_symbol()->type_information) == _localDecls.end())
		{
			_localDecls.insert(s.get_internal_symbol()->type_information);

			decl_closure.add(s);
		}
	}

	// User-defined structs must be included in GPU kernel's file
	// 'closure()' method is not working for extern symbols...
	forward_declaration << decl_closure.closure() << "\n";

	for (std::set<Symbol>::iterator it = extern_symbols.begin();
			it != extern_symbols.end(); it++)
	{
		// Check the symbol is not a function definition before adding it to forward declaration (see #529)
		// Check the symbol does not come from CUDA (see #753 and #959)
		AST_t a = it->get_point_of_declaration();
		if (!FunctionDefinition::predicate(a) && !CheckIfInCudacompiler::check(it->get_filename()))
		{
			forward_declaration << a.prettyprint_external() << "\n";
		}
	}

	// If it is an outlined task, do some more work
	if (is_outline_task)
	{
		process_outline_task(outline_flags, function_tree, sl, forward_declaration);
	}

	AST_t function_def_tree = reference_tree.get_enclosing_function_definition();
	FunctionDefinition enclosing_function(function_def_tree, sl);

	Source result, arguments_struct_definition, outline_name, parameter_list, body;
	Source instrument_before, instrument_after;

	result
		<< arguments_struct_definition
		<< "void " << outline_name << "(" << parameter_list << ")"
		<< "{"
		<< instrument_before
		<< body
		<< instrument_after
		<< "}"
		;

	// Add the tracing instrumentation if needed
	if (instrumentation_enabled())
	{
		insert_instrumentation_code(
				enclosing_function.get_function_symbol(),
				outline_name,
				outline_flags,
				reference_tree,
				instrument_before,
				instrument_after);
	}

	// arguments_struct_definition
	Scope sc = sl.get_scope(reference_tree);
	Symbol struct_typename_sym = sc.get_symbol_from_name(struct_typename);

	if (!struct_typename_sym.is_valid())
	{
		running_error("Invalid typename for struct args", 0);
	}

	// Check if we have already printed the argument's struct definition in the CUDA file
	if (_taskSymbols.count(struct_typename_sym) == 0)
	{
		arguments_struct_definition
			<< struct_typename_sym.get_point_of_declaration().prettyprint();

		// Keep record of which argument's struct definitions have been printed to the CUDA file
		// in order to avoid repeating them
		_taskSymbols.insert(struct_typename_sym);
	}

	// outline_name
	outline_name
		<< gpu_outline_name(task_name)
		;

	// parameter_list
	parameter_list
		<< struct_typename << "* const _args"
		;

	// body
	Source private_vars, final_code;

	body
		<< private_vars
		<< initial_setup
		<< outline_body
		<< final_code
		;

	// private_vars
	ObjectList<DataEnvironItem> data_env_items = data_environ.get_items();

	for (ObjectList<DataEnvironItem>::iterator it = data_env_items.begin();
			it != data_env_items.end();
			it++)
	{
		if (it->is_private())
		{
			Symbol sym = it->get_symbol();
			Type type = sym.get_type();

			private_vars
				<< type.get_declaration(sym.get_scope(), sym.get_name()) << ";"
				;
		}
		else if (it->is_raw_buffer())
		{
			Symbol sym = it->get_symbol();
			Type type = sym.get_type();
			std::string field_name = it->get_field_name();

			if (type.is_reference())
			{
				type = type.references_to();
			}

			if (!type.is_named_class())
			{
				internal_error("invalid class type in field of raw buffer", 0);
			}

			final_code
				<< field_name << ".~" << type.get_symbol().get_name() << "();"
				;
		}
	}

	if (outline_flags.parallel)
	{
		running_error("%s: error: parallel not supported in CUDA devices", reference_tree.get_locus().c_str());
	}

	// final_code
    if (outline_flags.parallel || outline_flags.barrier_at_end)
	{
        final_code
            << OMPTransform::get_barrier_code(reference_tree)
            ;
	}

	// Parse it in a sibling function context
	AST_t outline_code_tree =
			result.parse_declaration(enclosing_function.get_ast(), sl);

	// This registers the output file in the compilation pipeline if needed
	std::ofstream cudaFile, cudaHeaderFile;
	get_output_file(cudaFile, cudaHeaderFile);

	// Look for kernel calls to add the Nanos++ kernel execution stream whenever possible
	ObjectList<AST_t> kernel_call_list = outline_code_tree.depth_subtrees(CUDA::KernelCall::predicate);
	for (ObjectList<AST_t>::iterator it = kernel_call_list.begin();
			it != kernel_call_list.end();
			it++)
	{
		replace_kernel_config(*it, sl);
	}

	// Print declarations in header file in the following way:
	// 1 - Protect the header with ifndef/define/endif
	//     |--> Done at get_output_file() and run()
	// 2 - Emit extern C when we're compiling a C code
	//     |--> WARNING: C code included from CXX may need extern C, too, but this is not checked (not trivial)
	// 3 - Write forward declarations

 	if (IS_C_LANGUAGE) {
		cudaHeaderFile << "extern \"C\" {\n";
	}
	cudaHeaderFile << forward_declaration.get_source(false) << "\n";
	if (IS_C_LANGUAGE) {
		cudaHeaderFile << "}\n";
	}
	cudaHeaderFile.close();

	// Print definitions in source file
	cudaFile << "extern \"C\" {\n";
	//cudaFile << forward_declaration.get_source(false) << "\n";
	cudaFile << outline_code_tree.prettyprint_external() << "\n";
	cudaFile << "}\n";
	cudaFile.close();

	/******************* Generate the host side code (C/C++ file) ******************/
	insert_host_side_code(outline_name, outline_flags, struct_typename, parameter_list, reference_tree, sl);
}

void DeviceCUDA::process_outline_task(
		const OutlineFlags& outline_flags,
		AST_t& function_tree,
		ScopeLink& sl,
		Source& forward_declaration)
{
	// Check if the task symbol is actually a function definition or a declaration
	if (FunctionDefinition::predicate(function_tree))
	{
		// Check if we have already printed the function definition in the CUDA file
		if (_taskSymbols.count(outline_flags.task_symbol) == 0)
		{
			// Look for kernel calls to add the Nanos++ kernel execution stream whenever possible
			ObjectList<AST_t> kernel_call_list = function_tree.get_enclosing_function_definition().depth_subtrees(CUDA::KernelCall::predicate);
			for (ObjectList<AST_t>::iterator it = kernel_call_list.begin();
					it != kernel_call_list.end();
					it++)
			{
				replace_kernel_config(*it, sl);
			}

			forward_declaration << function_tree.get_enclosing_function_definition().prettyprint_external();

			// Keep record of which tasks have been printed to the CUDA file
			// in order to avoid repeating them
			_taskSymbols.insert(outline_flags.task_symbol);

			// Remove the function definition from the original source code
			function_tree.remove_in_list();
		}
	}
	else
	{
		// Not a function definition
		// Search for the function definition
		ObjectList<AST_t> funct_def_list =
				_root.depth_subtrees(FilterFunctionDef(outline_flags.task_symbol, sl));

		if (funct_def_list.size() == 1)
		{
			// Check if we have already printed the function definition in the CUDA file
			if (_taskSymbols.count(outline_flags.task_symbol) == 0)
			{
				// Look for kernel calls to add the Nanos++ kernel execution stream whenever possible
				ObjectList<AST_t> kernel_call_list = funct_def_list[0].get_enclosing_function_definition().depth_subtrees(CUDA::KernelCall::predicate);
				for (ObjectList<AST_t>::iterator it = kernel_call_list.begin();
						it != kernel_call_list.end();
						it++)
				{
					replace_kernel_config(*it, sl);
				}

				forward_declaration << funct_def_list[0].get_enclosing_function_definition().prettyprint_external();

				// Keep record of which tasks have been printed to the CUDA file
				// in order to avoid repeating them
				_taskSymbols.insert(outline_flags.task_symbol);
			}

			// Remove the function definition from the original source code
			funct_def_list[0].remove_in_list();
		}
		else if (funct_def_list.size() == 0
				&& _taskSymbols.count(outline_flags.task_symbol) > 0)
		{
			// We have already removed it and printed it in the CUDA file, do nothing
		}
	}
}

void DeviceCUDA::insert_host_side_code(
		Source &outline_name,
		const OutlineFlags& outline_flags,
		const std::string& struct_typename,
		Source &parameter_list,
		AST_t &reference_tree,
		ScopeLink &sl)
{
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

void DeviceCUDA::get_device_descriptor(const std::string& task_name,
		DataEnvironInfo &data_environ,
		const OutlineFlags& outline_flags,
		AST_t reference_tree,
		ScopeLink sl,
		Source &ancillary_device_description,
		Source &device_descriptor)
{
    FunctionDefinition enclosing_function_def(reference_tree.get_enclosing_function_definition(), sl);
    Symbol function_symbol = enclosing_function_def.get_function_symbol();

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

    Source nanos_dd_size_opt;
    if (Nanos::Version::interface_is_at_least("master", 5012))
    {
        ancillary_device_description
            << comment("CUDA device descriptor")
            << "static nanos_smp_args_t "
            << task_name << "_gpu_args = { (void(*)(void*))" << outline_name << "};"
            ;
    }
    else
    {
        ancillary_device_description
            << comment("CUDA device descriptor")
            << "nanos_smp_args_t "
            << task_name << "_gpu_args = { (void(*)(void*))" << outline_name << "};"
            ;
        nanos_dd_size_opt << "nanos_gpu_dd_size, ";
    }

    device_descriptor
        << "{ nanos_gpu_factory, " << nanos_dd_size_opt << "&" << task_name << "_gpu_args },"
        ;
}

void DeviceCUDA::do_replacements(DataEnvironInfo& data_environ,
		AST_t body,
		ScopeLink scope_link,
		Source &initial_setup,
		Source &replaced_src)
{
	do_cuda_outline_replacements(body,
			scope_link,
			data_environ,
			initial_setup,
			replaced_src);
}

void DeviceCUDA::phase_cleanup(DTO& data_flow)
{
	_cudaFilename = "";
	_cudaHeaderFilename = "";
	_root = AST_t(0);
}

void DeviceCUDA::pre_run(DTO& dto)
{
	_root = dto["translation_unit"];
	if (dto.get_keys().contains("openmp_task_info"))
	{
		_function_task_set = RefPtr<OpenMP::FunctionTaskSet>::cast_static(dto["openmp_task_info"]);
	}
}

void DeviceCUDA::run(DTO& dto)
{
	if (_cudaHeaderFilename != "") {
		// Protect the header with ifndef/define/endif
		// Here we emit the last endif
		std::string def = get_header_macro();
		std::ofstream cudaHeaderFile;
		cudaHeaderFile.open(_cudaHeaderFilename.c_str(), std::ios_base::app);
		cudaHeaderFile << "#endif // " << def.c_str() << "\n";
		cudaHeaderFile.close();
	}
}

EXPORT_PHASE(TL::Nanox::DeviceCUDA);
