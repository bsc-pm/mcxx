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

//
//std::string DeviceCUDA::get_outline_name_for_instrumentation(const std::string & name,
//        bool is_template_specialized UNUSED_PARAMETER, const FunctionDefinition& enclosing_function UNUSED_PARAMETER) const
//{
//	return cuda_outline_name(name);
//}
//
//bool DeviceCUDA::is_wrapper_needed(PragmaCustomConstruct ctr)
//{
//	return ctr.get_clause("ndrange").is_defined();
//}
//
//bool DeviceCUDA::is_wrapper_needed(const Symbol& symbol)
//{
//	Symbol sym = symbol;
//	return is_wrapper_needed(sym);
//}
//
//bool DeviceCUDA::is_wrapper_needed(Symbol& symbol)
//{
//	std::string ndrange;
//
//	if (_function_task_set->is_function_task_or_implements(symbol))
//	{
//		ndrange = _function_task_set->get_function_task(symbol).get_target_info().get_ndrange();
//	}
//
//	return !ndrange.empty();
//}
//
//bool DeviceCUDA::is_wrapper_needed(PragmaCustomConstruct ctr, Symbol& symbol)
//{
//	return is_wrapper_needed(ctr) || is_wrapper_needed(symbol);
//}
//
//std::string DeviceCUDA::get_header_macro()
//{
//	std::string macro = "__"  + _cudaHeaderFilename + "__";
//	std::string replacement("_");
//	char_replace_all_occurrences(macro, std::string("-"), replacement);
//	char_replace_all_occurrences(macro, std::string("."), replacement);
//
//	return macro;
//}
//
//void DeviceCUDA::char_replace_all_occurrences(
//		std::string &str,       // String to process
//		std::string original,   // Matching pattern
//		std::string replaced)   // Substitution
//{
//	size_t pos = str.find(original, original.size());
//	while (pos != std::string::npos)
//	{
//		str.replace(pos, original.size(), replaced.c_str());
//		pos = str.find(original, original.size());
//	}
//}
//
//void DeviceCUDA::replace_all_kernel_configs(AST_t& function_code_tree, ScopeLink sl)
//{
//	// Look for kernel calls to add the Nanos++ kernel execution stream whenever possible
//	ObjectList<AST_t> kernel_call_list = function_code_tree.depth_subtrees(CUDA::KernelCall::predicate);
//	for (ObjectList<AST_t>::iterator it = kernel_call_list.begin();
//			it != kernel_call_list.end();
//			it++)
//	{
//		replace_kernel_config(*it, sl);
//	}
//}
//
//void DeviceCUDA::replace_kernel_config(AST_t &kernel_call, ScopeLink sl)
//{
//	CUDA::KernelCall kcall(kernel_call, sl);
//
//	Source new_kernel_call;
//	Source new_config, new_param_list, nanos_stream_call;
//
//	new_kernel_call << kcall.get_called_expression() << "<<<" << new_config << ">>>(" << new_param_list << ")";
//
//	ObjectList<Expression> argument_list = kcall.get_argument_list();
//
//	for (ObjectList<Expression>::iterator it = argument_list.begin();
//			it != argument_list.end();
//			it++)
//	{
//		new_param_list.append_with_separator(it->prettyprint(), ",");
//	}
//
//	nanos_stream_call << "nanos_get_kernel_execution_stream()";
//	ObjectList<Expression> kernel_config = kcall.get_kernel_configuration();
//
//	if (kernel_config.size() == 2)
//	{
//		new_config << kernel_config[0] << ","
//				<< kernel_config[1] << ","
//				<< "0, "
//				<< nanos_stream_call;
//	}
//	else if (kernel_config.size() == 3)
//	{
//		new_config << kernel_config[0] << ","
//				<< kernel_config[1] << ","
//				<< kernel_config[2] << ","
//				<< nanos_stream_call;
//	}
//	else if (kernel_config.size() == 4)
//	{
//		// Do nothing by now
//		new_config << kernel_config[0] << ","
//				<< kernel_config[1] << ","
//				<< kernel_config[2] << ","
//				<< kernel_config[3];
//	}
//	else
//	{
//		internal_error("Code unreachable: a kernel call configuration must have between 2 and 4 parameters", 0);
//	}
//
//	AST_t expr = new_kernel_call.parse_expression(kernel_call, sl);
//	kernel_call.replace(expr);
//}
//
//void DeviceCUDA::generateNDrangeCode(
//		Declaration &kernel_decl,
//		Source &code_ndrange,
//		std::string &ndrange,
//		std::map<std::string, int> &param_positions,
//		ObjectList<std::string> &param_names)
//{
//	ObjectList<std::string> ndrange_v;
//	ObjectList<std::string> ndrange_v_aux;
//
//	// If ndrange contains a parameter, we add it as a position, otherwise we use its value
//	ndrange_v_aux = ExpressionTokenizerTrim().tokenize(ndrange);
//	ndrange_v.insert(ndrange_v_aux.at(0));
//
//	int ix;
//	for (ix = 1; ix < ndrange_v_aux.size(); ix++)
//	{
//		// Add () around name, not sure if always needed
//		std::string name2 = "(" + ndrange_v_aux.at(ix) + ")";
//
//		// Replace variables with their future name (same translation Mercurium task does itself)
//		int ex;
//		for (ex = 0 ; ex < param_names.size(); ex++)
//		{
//			std::string name = param_names.at(ex);
//			replaceAllStringVar(name2, name, "%" + param_position_name(param_positions.at(name)) + "%");
//		}
//
//		ndrange_v.append(name2);
//	}
//
//	bool checkDim = !(ndrange_v_aux.at(ndrange_v.size() - 1) == "noCheckDim");
//	int n_dimensions = atoi(ndrange_v.at(0).c_str());
//	if (n_dimensions < 1 || n_dimensions > 3)
//	{
//		std::cerr << kernel_decl << ": warning, ndrange directive must have dimensions 1, 2 or 3" << std::endl;
//	}
//	else if (n_dimensions * 2 + 1 + !checkDim != ndrange_v.size())
//	{
//		std::cerr << kernel_decl << ": warning, incorrect number of arguments (" << ndrange_v.size() - 1
//				<< ") for ndrange directive, it should be 2*n_dimensions" << std::endl;
//	}
//	else
//	{
//		code_ndrange << "dim3 dimGrid;";
//		code_ndrange << "dim3 dimBlock;";
//		if (checkDim && n_dimensions == 1)
//		{
//			code_ndrange
//					<< "dimBlock.x = ("
//					<< ndrange_v.at(1)
//					<< " < " << ndrange_v.at(n_dimensions + 1)
//					<< " ? " << ndrange_v.at(1)
//					<< " : " << ndrange_v.at(n_dimensions + 1) << ");";
//
//			code_ndrange
//					<< "dimGrid.x = ("
//					<< ndrange_v.at(1)
//					<< " < " << ndrange_v.at(n_dimensions + 1)
//					<< " ?  1 : "
//					<< ndrange_v.at(1) << "/" << ndrange_v.at(n_dimensions + 1)
//					<< " + (" << ndrange_v.at(1) << " %  " << ndrange_v.at(n_dimensions + 1)
//					<< " == 0 ? 0 : 1));";
//		}
//		else
//		{
//			code_ndrange << "dimBlock.x = " << ndrange_v.at(n_dimensions + 1) << ";";
//
//			code_ndrange << "dimGrid.x = " << ndrange_v.at(1) << "/" << ndrange_v.at(n_dimensions + 1) << ";";
//
//			if (checkDim)
//			{
//				code_ndrange
//					<< "if (" <<ndrange_v.at(1) << "%" << ndrange_v.at(n_dimensions + 1) << " != 0) "
//					<< "printf(\"WARNING: Size x of cuda kernel is not divisible by local size\\n\");";
//			}
//		}
//
//		if (n_dimensions >= 2)
//		{
//			if (checkDim && n_dimensions == 2)
//			{
//				code_ndrange
//						<< "dimBlock.y = ("
//						<< ndrange_v.at(2)
//						<< " < " << ndrange_v.at(n_dimensions + 2)
//						<< " ? " << ndrange_v.at(2)
//						<< " : " << ndrange_v.at(n_dimensions + 2) << ");";
//
//				code_ndrange
//						<< "dimGrid.y = ("
//						<< ndrange_v.at(2)
//						<< " < " << ndrange_v.at(n_dimensions + 2)
//						<< " ?  1 : "
//						<< ndrange_v.at(2) << "/" << ndrange_v.at(n_dimensions + 2)
//						<< " + (" << ndrange_v.at(2) << " %  " << ndrange_v.at(n_dimensions + 2)
//						<< " == 0 ? 0 : 1));";
//			}
//			else
//			{
//				code_ndrange << "dimBlock.y = " << ndrange_v.at(n_dimensions + 2) << ";";
//
//				code_ndrange << "dimGrid.y = " << ndrange_v.at(2) << "/" << ndrange_v.at(n_dimensions + 2) << ";";
//
//				if (checkDim)
//				{
//					code_ndrange
//							<< "if (" <<ndrange_v.at(2) << "%" << ndrange_v.at(n_dimensions + 2) << " != 0 ) "
//							<< "fatal(\"WARNING:  Size y of cuda kernel is not divisible by local size\\n\");";
//				}
//			}
//		}
//		else
//		{
//			code_ndrange << "dimBlock.y = 1;";
//			code_ndrange << "dimGrid.y = 1;";
//		}
//
//		if (n_dimensions == 3)
//		{
//			if (checkDim)
//			{
//				code_ndrange
//						<< "dimBlock.z = ("
//						<< ndrange_v.at(3)
//						<< " < " << ndrange_v.at(n_dimensions + 3)
//						<< " ? " << ndrange_v.at(3)
//						<< " : " << ndrange_v.at(n_dimensions + 3) << ");";
//
//				code_ndrange
//						<< "dimGrid.z = ("
//						<< ndrange_v.at(3)
//						<< " < " << ndrange_v.at(n_dimensions + 3)
//						<< " ?  1 : "
//						<< ndrange_v.at(3) << "/" << ndrange_v.at(n_dimensions + 3)
//						<< " + (" << ndrange_v.at(3) << " %  " << ndrange_v.at(n_dimensions + 3)
//						<< " == 0 ? 0 : 1));";
//			}
//			else
//			{
//				code_ndrange << "dimBlock.z = " << ndrange_v.at(n_dimensions + 3) << ";";
//
//				code_ndrange << "dimGrid.z = " << ndrange_v.at(3) << "/" << ndrange_v.at(n_dimensions + 3) << ";";
//
//				if (checkDim)
//				{
//					code_ndrange << "if (" <<ndrange_v.at(3) << "%" << ndrange_v.at(n_dimensions + 3) << " != 0 ) "
//							<< "printf(\"WARNING: Size y of cuda kernel is not divisible by local size\\n\");";
//				}
//			}
//		}
//		else
//		{
//			code_ndrange << "dimBlock.z = 1;";
//			code_ndrange << "dimGrid.z = 1;";
//		}
//	}
//}
//
//void DeviceCUDA::generateParametersCall(
//		Source &cuda_call,
//		Declaration& kernel_decl,
//		std::string calls,
//		std::map<std::string, int> &param_positions,
//		ObjectList<std::string> &param_names,
//		ObjectList<ParameterDeclaration> &parameters_impl,
//		Declaration &implements_decl)
//{
//	// Here we get kernel arguments, and check they exist on implemented function
//	Source cuda_parameters;
//	ObjectList<DeclaredEntity> declared_entities = kernel_decl.get_declared_entities();
//	DeclaredEntity method_decl = declared_entities.at(0);
//	ObjectList<ParameterDeclaration> parameters = method_decl.get_parameter_declarations();
//
//	ObjectList<std::string> calls_aux;
//	if (!calls.empty())
//	{
//		calls_aux = ExpressionTokenizerTrim().tokenize(calls);
//	}
//	else
//	{
//		for (ObjectList<ParameterDeclaration>::iterator it = parameters.begin();
//				it != parameters.end();
//				it++)
//		{
//			bool found = false;
//
//			for (ObjectList<ParameterDeclaration>::iterator it2 = parameters_impl.begin();
//					it2 != parameters_impl.end();
//					it2++)
//			{
//				if (it->get_type().basic_type() == it2->get_type().basic_type() && it->get_name().prettyprint() == it2->get_name().prettyprint())
//				{
//					found = true;
//				}
//			}
//
//			if (!found)
//			{
//				std::cerr << kernel_decl.get_ast().get_locus()
//						<< ": warning cuda kernel should include same parameter name and type than implemented function or use calls(..) clause, argument '"
//						<< it->get_name().prettyprint()
//						<< "' not found in " << implements_decl.get_ast().get_locus()
//						<< "(" << implements_decl.get_declared_entities().at(0).prettyprint() << ")" << std::endl;
//			}
//
//			calls_aux.append(it->get_name());
//		}
//	}
//
//	// Same thing we did with ndrange, now with calls:
//	// replace variable names by a position-identified-name
//	for (int ix = 0; ix < calls_aux.size(); ix++)
//	{
//		std::string name2 = calls_aux.at(ix);
//		// Replace variables with their future name (same translation Mercurium task
//		// does itself)
//		int ex;
//		for (ex =0 ; ex < param_names.size(); ex++)
//		{
//			std::string name = param_names.at(ex);
//			replaceAllStringVar(name2, name, "%" + param_position_name(param_positions.at(name)) + "%");
//		}
//
//		cuda_parameters.append_with_separator(name2, ",");
//	}
//
//	cuda_call << method_decl.get_declared_symbol().get_name() << "<<<dimGrid,dimBlock>>>(" << cuda_parameters << ");";
//}
//
///**
// * Generates code needed to call cuda kernel using implements_decl arguments
// * if both implements_decl and kernel_decl are the same, it means the kernel is a task by itself
// * @param implements_decl
// * @param kernel_decl
// * @param ndrange
// * @param calls
// */
//void DeviceCUDA::generate_wrapper_code(
//		Declaration implements_decl,
//		Declaration kernel_decl,
//		std::string ndrange,
//		std::string calls)
//{
//	// Get function declaration (implemented one, not kernel)
//	ObjectList<DeclaredEntity> declared_entities_impl = implements_decl.get_declared_entities();
//
//	// First declared entity is the function
//	DeclaredEntity method_decl_impl = declared_entities_impl.at(0);
//	ObjectList<ParameterDeclaration> parameters_impl = method_decl_impl.get_parameter_declarations();
//
//	// Add implemented function parameters to a map where we save their position
//	std::map<std::string, int> param_positions;
//	ObjectList<std::string> param_names;
//	int counter = 0;
//
//	for (ObjectList<ParameterDeclaration>::iterator it = parameters_impl.begin();
//			it != parameters_impl.end();
//			it++)
//	{
//		param_positions.insert(std::pair<std::string, int>(it->get_name().prettyprint(), counter));
//		param_names.insert(it->get_name().prettyprint());
//		counter++;
//	}
//
//	// Generate ndrange
//	Source code_ndrange;
//	code_ndrange << comment("This code is generated from ndrange and calls clause from: " + kernel_decl.get_ast().get_locus());
//	generateNDrangeCode(kernel_decl, code_ndrange, ndrange, param_positions, param_names);
//
//	// Handle calls clause
//	Source cuda_call;
//
//	// Generate kernel call
//	generateParametersCall(cuda_call, kernel_decl, calls,param_positions, param_names, parameters_impl, implements_decl);
//
//	//Declaration kernel_decl(ctr.get_declaration(), ctr.get_scope_link());
//	ObjectList<DeclaredEntity> declared_entities = kernel_decl.get_declared_entities();
//	DeclaredEntity method_decl = declared_entities.at(0);
//	_implementCalls.insert(std::pair<std::string, std::pair<Source, Source> >(method_decl.get_declared_symbol().get_name() +
//			method_decl_impl.get_declared_symbol().get_qualified_name(), std::pair<Source, Source > (code_ndrange, cuda_call)));
//
//}
//
//void DeviceCUDA::do_cuda_inline_get_addresses(
//		const Scope& sc,
//		const DataEnvironInfo& data_env_info,
//		Source &copy_setup,
//		ReplaceSrcIdExpression& replace_src,
//		bool &err_declared)
//{
//	Source current_wd_param;
//	if (Nanos::Version::interface_is_at_least("master", 5005))
//	{
//		copy_setup
//			<< "nanos_wd_t current_wd = nanos_current_wd();"
//			;
//		current_wd_param
//			<< ", current_wd"
//			;
//	}
//
//	ObjectList<OpenMP::CopyItem> copies = data_env_info.get_copy_items();
//	unsigned int j = 0;
//	for (ObjectList<OpenMP::CopyItem>::iterator it = copies.begin();
//			it != copies.end();
//			it++, j++)
//	{
//		DataReference data_ref = it->get_copy_expression();
//		Symbol sym = data_ref.get_base_symbol();
//
//		OpenMP::DataSharingEnvironment &data_sharing = data_env_info.get_data_sharing();
//		OpenMP::DataSharingAttribute data_sharing_attr = data_sharing.get_data_sharing(sym);
//
//		DataEnvironItem data_env_item = data_env_info.get_data_of_symbol(sym);
//
//		ERROR_CONDITION(!data_env_item.get_symbol().is_valid(),
//				"Invalid data for copy symbol", 0);
//
//		bool is_shared = data_env_item.is_shared();
//
//		Type type = sym.get_type();
//
//		if (type.is_array())
//		{
//			type = type.array_element().get_pointer_to();
//			is_shared = false;
//		}
//		else if (is_shared)
//		{
//			type = type.get_pointer_to();
//		}
//
//		std::string copy_name = "_cp_" + sym.get_name();
//
//		if (!err_declared)
//		{
//			copy_setup
//				<< "nanos_err_t cp_err;"
//				;
//			err_declared = true;
//		}
//
//		copy_setup
//			<< type.get_declaration(sc, copy_name) << ";"
//			<< "cp_err = nanos_get_addr(" << j << ", (void**)&" << copy_name << current_wd_param << ");"
//			<< "if (cp_err != NANOS_OK) nanos_handle_error(cp_err);"
//			;
//
//		if (!is_shared)
//		{
//			replace_src.add_replacement(sym, copy_name);
//		}
//		else
//		{
//			replace_src.add_replacement(sym, "(*" + copy_name + ")");
//		}
//	}
//}
//
//void DeviceCUDA::do_cuda_outline_replacements(
//		AST_t body,
//		ScopeLink scope_link,
//		const DataEnvironInfo& data_env_info,
//		Source &initial_code,
//		Source &replaced_outline)
//{
//
//	Source copy_setup;
//	Scope sc = scope_link.get_scope(body);
//
//	initial_code
//		<< copy_setup
//		;
//
//	ReplaceSrcIdExpression replace_src(scope_link);
//
//	bool err_declared = false;
//
//	ObjectList<DataEnvironItem> data_env_items = data_env_info.get_items();
//	for (ObjectList<DataEnvironItem>::iterator it = data_env_items.begin();
//			it != data_env_items.end();
//			it++)
//	{
//		DataEnvironItem& data_env_item(*it);
//
//		Symbol sym = data_env_item.get_symbol();
//		const std::string field_name = data_env_item.get_field_name();
//
//		if (data_env_item.is_private())
//		{
//			// Do nothing as they are private, we create a variable with the
//			// same original name
//		}
//		else if (data_env_item.is_firstprivate()
//				|| data_env_item.is_shared())
//		{
//			replace_src.add_replacement(sym, "_args->" + field_name);
//		}
//		// else if (data_env_item.is_shared())
//		// {
//		// 	replace_src.add_replacement(sym, "(*_args->" + field_name + ")");
//		// }
//		else
//		{
//			internal_error("Code unreachable", 0);
//		}
//	}
//
//	if (create_translation_function())
//	{
//		// We already created a function that performs the translation in the runtime
//		copy_setup
//			<< comment("Translation is done by the runtime")
//			;
//	}
//	else
//	{
//		do_cuda_inline_get_addresses(
//				sc,
//				data_env_info,
//				copy_setup,
//				replace_src,
//				err_declared);
//	}
//
//	replaced_outline << replace_src.replace(body);
//}
//
//void DeviceCUDA::get_output_file(std::ofstream& cudaFile)
//{
//	std::ofstream header;
//	get_output_file(cudaFile, header);
//	header.close();
//}
//
//void DeviceCUDA::get_output_file(std::ofstream& cudaFile, std::ofstream& cudaHeaderFile)
//{
//	// Check if the file has already been created (and written)
//	bool new_file = false;
//
//	Source included_files;
//	if (_cudaFilename == "")
//	{
//		// Set the file name
//		_cudaFilename = "cudacc_";
//		_cudaFilename += CompilationProcess::get_current_file().get_filename(false);
//		size_t file_extension = _cudaFilename.find_last_of(".");
//		_cudaFilename.erase(file_extension, _cudaFilename.length());
//
//		// Set the header file name here since it is easier
//		_cudaHeaderFilename = _cudaFilename + ".cuh";
//
//		// Set the cuda file's extension
//		_cudaFilename += ".cu";
//
//		new_file = true;
//
//		// Remove the intermediate source files
//		mark_file_for_cleanup( _cudaFilename.c_str() );
//		mark_file_for_cleanup( _cudaHeaderFilename.c_str() );
//
//		// Get *.cu included files
//		ObjectList<IncludeLine> lines = CurrentFile::get_top_level_included_files();
//		std::string cuda_file_ext(".cu\"");
//		std::string cuda_header_ext(".cuh\"");
//
//		for (ObjectList<IncludeLine>::iterator it = lines.begin(); it != lines.end(); it++)
//		{
//			std::string line = (*it).get_preprocessor_line();
//			std::string extension = line.substr(line.find_last_of("."));
//
//			if (extension == cuda_file_ext || extension == cuda_header_ext)
//				included_files << line << "\n";
//		}
//	}
//
//	const std::string configuration_name = "cuda";
//	CompilationProcess::add_file(_cudaFilename, configuration_name, new_file);
//
//	if (new_file)
//	{
//		cudaFile.open (_cudaFilename.c_str());
//		cudaHeaderFile.open(_cudaHeaderFilename.c_str());
//
//		cudaFile << "#include \"" << _cudaHeaderFilename.c_str() << "\"\n";
//
//		// Protect the header with ifndef/define/endif
//		std::string def = get_header_macro();
//		cudaHeaderFile << "#ifndef " << def.c_str() << "\n";
//		cudaHeaderFile << "#define " << def.c_str() << "\n";
//
//		// Add .cu / .cuh includes
//		cudaHeaderFile << included_files.get_source(false) << "\n";
//	}
//	else
//	{
//		cudaFile.open (_cudaFilename.c_str(), std::ios_base::app);
//		cudaHeaderFile.open(_cudaHeaderFilename.c_str(), std::ios_base::app);
//	}
//}
//
//void DeviceCUDA::process_wrapper(
//		PragmaCustomConstruct ctr,
//		AST_t &decl,
//		bool& needs_device,
//		bool& needs_global,
//		bool& is_global_defined,
//		bool& needs_extern_c)
//{
//	// Prepare arguments to call the wrapper generator
//	if (ctr.get_clause("implements").is_defined())
//	{
//		ObjectList<Expression> implements_list = ctr.get_clause("implements").get_expression_list();
//		Expression implements_name = implements_list[0];
//		IdExpression id_expr = implements_name.get_id_expression();
//		Declaration implements_decl = id_expr.get_declaration();
//		Declaration kernel_decl(ctr.get_declaration(), ctr.get_scope_link());
//		PragmaCustomClause ndrange = ctr.get_clause("ndrange");
//		PragmaCustomClause calls = ctr.get_clause("calls");
//
//		if (!ctr.get_clause("ndrange").is_defined())
//		{
//			std::cerr << ctr.get_ast().get_locus() << ": warning: implements with device 'cuda' "
//					<< "must be used with ndrange on a cuda kernel"
//					<< std::endl;
//		}
//		else
//		{
//			needs_global = true;
//			if (!calls.is_defined())
//			{
//				generate_wrapper_code(implements_decl, kernel_decl, ndrange.get_arguments().at(0), "");
//			}
//			else
//			{
//				generate_wrapper_code(implements_decl, kernel_decl, ndrange.get_arguments().at(0), calls);
//			}
//		}
//	}
//
//	Source fwd_decl;
//	ScopeLink sl = ctr.get_scope_link();
//	process_local_symbols(decl, sl, fwd_decl);
//	process_extern_symbols(decl, sl, fwd_decl);
//
//	insert_device_side_code(fwd_decl);
//}
//
//void DeviceCUDA::check_needs_device(
//		AST_t& decl,
//		ScopeLink sl,
//		bool& needs_device)
//{
//
//	if (FunctionDefinition::predicate(decl))
//	{
//		// Unless we find a kernel configuration call
//		needs_device = true;
//
//		FunctionDefinition funct_def(decl, sl);
//		Statement stmt = funct_def.get_function_body();
//
//		if (!stmt.get_ast().depth_subtrees(PredicateType(AST_CUDA_KERNEL_CALL)).empty())
//		{
//			needs_device = false;
//		}
//
//		if (_fwdSymbols.count(funct_def.get_function_symbol()) != 0)
//		{
//			// Nothing to do here, already defined
//			return;
//		}
//
//		_fwdSymbols.insert(funct_def.get_function_symbol());
//	}
//	else if (Declaration::predicate(decl))
//	{
//		Declaration declaration(decl, sl);
//
//		DeclarationSpec decl_specifier_seq = declaration.get_declaration_specifiers();
//		if (decl_specifier_seq.get_ast().depth_subtrees(PredicateType(AST_TYPEDEF_SPEC)).empty())
//		{
//			needs_device = true;
//		}
//
//		ObjectList<DeclaredEntity> declared_entities = declaration.get_declared_entities();
//
//		ObjectList<Symbol> sym_list;
//		for (ObjectList<DeclaredEntity>::iterator it = declared_entities.begin();
//				it != declared_entities.end();
//				it++)
//		{
//			sym_list.insert(it->get_declared_symbol());
//		}
//
//		for (ObjectList<Symbol>::iterator it = sym_list.begin();
//				it != sym_list.end();
//				it++)
//		{
//			if (_function_task_set->is_function_task_or_implements(*it))
//			{
//				needs_device = false;
//			}
//		}
//	}
//
//}
//
//void DeviceCUDA::insert_function_definition(PragmaCustomConstruct ctr, bool is_copy)
//{
//	bool needs_device = false;
//	bool is_global_defined = false;
//	bool needs_global = false;
//	bool needs_extern_c = false;
//	AST_t decl = ctr.get_declaration();
//
//	Symbol func_sym = decl.get_attribute(LANG_FUNCTION_SYMBOL);
//	if (is_wrapper_needed(ctr, func_sym))
//	{
//		process_wrapper(ctr, decl, needs_device, needs_global, is_global_defined, needs_extern_c);
//	}
//
//	check_needs_device(decl, ctr.get_scope_link(), needs_device);
//
//	if (needs_device)
//	{
//		// Check whether the kernel is labeled as '__global__' or we need to add '__device__'
//		check_global_kernel(decl, ctr.get_scope_link(), needs_global, is_global_defined);
//	}
//
//	insert_declaration_device_side(decl, needs_device, needs_global, is_global_defined, needs_extern_c);
//
//	if (!is_copy)
//	{
//		ctr.get_ast().remove_in_list();
//	}
//}
//
//
//void DeviceCUDA::check_global_kernel(AST_t& decl, ScopeLink sl, bool& needs_global, bool& is_global_defined)
//{
//	// Check the kernel is not labeled as __global__
//	std::string global("global");
//	ObjectList<AST_t> ast_list = decl.depth_subtrees(GCCAttributeSpecifier::predicate);
//	for (ObjectList<AST_t>::iterator it = ast_list.begin(); it != ast_list.end(); it++)
//	{
//		AST_t &ast = *it;
//		if (GCCAttributeSpecifier::predicate(ast))
//		{
//			GCCAttributeSpecifier gcc_attr_spec(ast, sl);
//			ObjectList<GCCAttribute> gcc_attributes = gcc_attr_spec.get_gcc_attribute_list();
//
//			if (gcc_attributes.contains(functor(&GCCAttribute::get_name), global) && gcc_attributes.size() == 1)
//			{
//				// Remove the attribute from the list to avoid __attribute__((global)) appear in the CUDA file
//				ast.remove_in_list();
//				is_global_defined = true;
//				needs_global = true;
//				return;
//			}
//		}
//	}
//}
//
//void DeviceCUDA::insert_declaration_device_side(
//		AST_t& decl,
//		bool needs_device,
//		bool needs_global,
//		bool is_global_defined,
//		bool needs_extern_c)
//{
//
//	std::ofstream cudaFile;
//	get_output_file(cudaFile);
//
//	if (!needs_device
//			&& IS_C_LANGUAGE)
//	{
//		needs_extern_c = true;
//	}
//
//	if (needs_extern_c)
//	{
//		cudaFile << "extern \"C\" {\n";
//	}
//
//	if (needs_device)
//	{
//		if (needs_global)
//		{
//			cudaFile << "__global__ ";
//		}
//		else if (is_global_defined) {
//			// Already defined, no need to add neither __global__ nor __device__
//		}
//		else
//		{
//			cudaFile << "__device__ ";
//		}
//	}
//
//	cudaFile << decl.prettyprint_external() << "\n";
//
//	if (needs_extern_c)
//	{
//		cudaFile << "}\n";
//	}
//
//	cudaFile.close();
//}
//
//void DeviceCUDA::insert_declaration(PragmaCustomConstruct ctr, bool is_copy)
//{
//	insert_function_definition(ctr, is_copy);
//}
//
//void DeviceCUDA::insert_instrumentation_code(
//        Symbol function_symbol,
//        const FunctionDefinition& enclosing_function,
//        const std::string & task_name,
//        Source& outline_name,
//        const OutlineFlags& outline_flags,
//        AST_t& reference_tree,
//        Source& instrument_before,
//        Source& instrument_after)
//{
//    Source uf_name_id, uf_name_descr;
//    Source uf_location_id, uf_location_descr;
//
//    if (Nanos::Version::interface_is_at_least("master", 5017))
//    {
//        // The outline name used by instrumentantion may contain template arguments
//        std::string outline_name_inst =
//            get_outline_name_for_instrumentation(task_name,
//                    function_symbol.get_type().is_template_specialized_type(), enclosing_function);
//
//        instrument_before
//            << "static int nanos_funct_id_init = 0;"
//            << "static nanos_event_key_t nanos_instr_uf_name_key = 0;"
//            << "static nanos_event_key_t nanos_instr_uf_location_key = 0;"
//            << "if (nanos_funct_id_init == 0)"
//            << "{"
//            <<    "nanos_err_t err = nanos_instrument_get_key(\"user-funct-name\", &nanos_instr_uf_name_key);"
//            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
//            <<    "err = nanos_instrument_register_value_with_val ((nanos_event_value_t) " << outline_name_inst << ","
//            <<               " \"user-funct-name\", " << uf_name_id << "," << uf_name_descr << ", 0);"
//            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
//            <<    "err = nanos_instrument_get_key(\"user-funct-location\", &nanos_instr_uf_location_key);"
//            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
//            <<    "err = nanos_instrument_register_value_with_val((nanos_event_value_t) " << outline_name_inst << ", \"user-funct-location\","
//            <<               uf_location_id << "," << uf_location_descr << ", 0);"
//            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
//            <<    "nanos_funct_id_init = 1;"
//            << "}"
//            << "nanos_event_t events_before[2];"
//            << "events_before[0].type = NANOS_BURST_START;"
//            << "events_before[1].type = NANOS_BURST_START;"
//            << "events_before[0].key = nanos_instr_uf_name_key;"
//            << "events_before[1].key = nanos_instr_uf_location_key;"
//            << "nanos_instrument_events(2, events_before);"
//            ;
//    }
//    else
//    {
//        instrument_before
//            << "static int nanos_funct_id_init = 0;"
//            << "static nanos_event_key_t nanos_instr_uf_name_key = 0;"
//            << "static nanos_event_value_t nanos_instr_uf_name_value = 0;"
//            << "static nanos_event_key_t nanos_instr_uf_location_key = 0;"
//            << "static nanos_event_value_t nanos_instr_uf_location_value = 0;"
//            << "if (nanos_funct_id_init == 0)"
//            << "{"
//            <<    "nanos_err_t err = nanos_instrument_get_key(\"user-funct-name\", &nanos_instr_uf_name_key);"
//            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
//            <<    "err = nanos_instrument_register_value ( &nanos_instr_uf_name_value, \"user-funct-name\", "
//            <<               uf_name_id << "," << uf_name_descr << ", 0);"
//            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
//            <<    "err = nanos_instrument_get_key(\"user-funct-location\", &nanos_instr_uf_location_key);"
//            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
//            <<    "err = nanos_instrument_register_value ( &nanos_instr_uf_location_value, \"user-funct-location\","
//            <<               uf_location_id << "," << uf_location_descr << ", 0);"
//            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
//            <<    "nanos_funct_id_init = 1;"
//            << "}"
//            << "nanos_event_t events_before[2];"
//            << "events_before[0].type = NANOS_BURST_START;"
//            << "events_before[1].type = NANOS_BURST_START;"
//            << "events_before[0].info.burst.key = nanos_instr_uf_name_key;"
//            << "events_before[0].info.burst.value = nanos_instr_uf_name_value;"
//            << "events_before[1].info.burst.key = nanos_instr_uf_location_key;"
//            << "events_before[1].info.burst.value = nanos_instr_uf_location_value;"
//            << "nanos_instrument_events(2, events_before);"
//            ;
//    }
//
//    instrument_after
//        << "nanos_instrument_close_user_fun_event();"
//        ;
//
//	if (outline_flags.task_symbol != NULL)
//	{
//		uf_name_id
//			<< "\"" << outline_flags.task_symbol.get_name() << "\""
//			;
//		uf_location_id
//			<< "\"" << outline_name << ":" << reference_tree.get_locus() << "\""
//			;
//
//		uf_name_descr
//			<< "\"Task '" << outline_flags.task_symbol.get_name() << "'\""
//			;
//		uf_location_descr
//			<< "\"'" << function_symbol.get_qualified_name() << "'"
//			<< " invoked at '" << reference_tree.get_locus() << "'\""
//			;
//	}
//	else
//	{
//		uf_name_id
//			<< uf_location_id
//			;
//		uf_location_id
//			<< "\"" << outline_name << ":" << reference_tree.get_locus() << "\""
//			;
//
//		uf_name_descr
//			<< uf_location_descr
//			;
//		uf_location_descr
//			<< "\"Outline from '"
//			<< reference_tree.get_locus()
//			<< "' in '" << function_symbol.get_qualified_name() << "'\""
//			;
//	}
//}
//
//
//void DeviceCUDA::create_wrapper_code(
//		PragmaCustomConstruct pragma_construct,
//		const OutlineFlags &outline_flags,
//		ScopeLink sl,
//		std::string& implemented_name)
//{
//	// When we are implementing a task, we receive a pragma custom construct
//	// with __symbol clause which includes implemented function name
//	//PragmaCustomConstruct pragma_construct(reference_tree, sl);
//	implemented_name = "";
//
//	bool task_impl = false;
//	// We search for the wrapper body, either with kernel_name+implemented function name
//	// or kernel_name+kernel_name in case kernel is a task itself
//	PragmaCustomClause symbolClause = pragma_construct.get_clause("__symbol");
//	if (symbolClause.is_defined())
//	{
//		implemented_name = symbolClause.get_arguments().at(0);
//		task_impl = true;
//	}
//	else
//	{
//		implemented_name = outline_flags.task_symbol.get_name();
//	}
//
//	// If this is a task (could be a only-target function too), we generate wrapper code to call it
//	// (insert_function_definition method will not be called before, only when it is a target directly)
//	if (_function_task_set->is_function_task(outline_flags.task_symbol))
//	{
//		std::string ndrange = _function_task_set->get_function_task(outline_flags.task_symbol).get_target_info().get_ndrange();
//		std::string calls = _function_task_set->get_function_task(outline_flags.task_symbol).get_target_info().get_calls();
//		Declaration kernel_decl(outline_flags.task_symbol.get_point_of_declaration(), sl);
//
//		if (task_impl)
//		{
//			// If this is a task and it implements another task we generate wrapper code
//			// for calling this kernel using original task declaration
//			ObjectList<Expression> implements_list = symbolClause.get_expression_list();
//			Expression implements_name = implements_list[0];
//			IdExpression id_expr = implements_name.get_id_expression();
//			Declaration implements_decl = id_expr.get_declaration();
//			generate_wrapper_code(implements_decl, kernel_decl, ndrange, calls);
//		}
//		else
//		{
//			generate_wrapper_code(kernel_decl, kernel_decl, ndrange, calls);
//		}
//
//		if (_fwdSymbols.count(outline_flags.task_symbol) == 0) {
//			// Tasks are called from host, they need global
//			// if already defined, use it, otherwise use declaration
//			AST_t task_code;
//			if (outline_flags.task_symbol.is_defined())
//			{
//				task_code = outline_flags.task_symbol.get_point_of_definition();
//			}
//			else
//			{
//				task_code = outline_flags.task_symbol.get_point_of_declaration();
//			}
//
//			bool needs_device, needs_global, is_global_defined;
//			check_global_kernel(task_code, sl, needs_global, is_global_defined);
//			check_needs_device(task_code, sl, needs_device);
//
//			insert_declaration_device_side(task_code, needs_device, needs_global, is_global_defined, /*needs_extern_c*/ false);
//
//			// Already inserted in check_needs_device()
//			//_fwdSymbols.insert(outline_flags.task_symbol);
//			kernel_decl.get_ast().remove_in_list();
//		}
//	}
//}
//

static void build_empty_body_for_function(
        TL::Symbol function_symbol,
        Nodecl::NodeclBase &function_code,
        Nodecl::NodeclBase &empty_stmt
        ) {
    empty_stmt = Nodecl::EmptyStatement::make("", 0);
    Nodecl::List stmt_list = Nodecl::List::make(empty_stmt);

    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE) {
        Nodecl::CompoundStatement compound_statement =
                Nodecl::CompoundStatement::make(stmt_list,
                /* destructors */ Nodecl::NodeclBase::null(),
                "", 0);
        stmt_list = Nodecl::List::make(compound_statement);
    }

    Nodecl::NodeclBase context = Nodecl::Context::make(
            stmt_list,
            function_symbol.get_related_scope(), "", 0);

    function_symbol.get_internal_symbol()->defined = 1;

    function_code = Nodecl::FunctionCode::make(context,
            // Initializers
            Nodecl::NodeclBase::null(),
            // Internal functions
            Nodecl::NodeclBase::null(),
            function_symbol,
            "", 0);
}

static TL::Symbol new_function_symbol(
        TL::Symbol current_function,
        const std::string& name,
        TL::Type return_type,
        ObjectList<std::string> parameter_names,
        ObjectList<TL::Type> parameter_types) {
    Scope sc = current_function.get_scope();

    // FIXME - Wrap
    decl_context_t decl_context = sc.get_decl_context();

    scope_entry_t* entry = new_symbol(decl_context, decl_context.current_scope, name.c_str());
    entry->entity_specs.is_user_declared = 1;

    entry->kind = SK_FUNCTION;
    entry->file = "";
    entry->line = 0;

    // Make it static
    entry->entity_specs.is_static = 1;

    // Make it member if the enclosing function is member
    if (current_function.is_member()) {
        entry->entity_specs.is_member = 1;
        entry->entity_specs.class_type = current_function.get_class_type().get_internal_type();

        entry->entity_specs.access = AS_PUBLIC;

        ::class_type_add_member(entry->entity_specs.class_type, entry);
    }

    ERROR_CONDITION(parameter_names.size() != parameter_types.size(), "Mismatch between names and types", 0);

    decl_context_t function_context;
    function_context = new_function_context(decl_context);
    function_context = new_block_context(function_context);

    function_context.function_scope->related_entry = entry;
    function_context.block_scope->related_entry = entry;

    entry->related_decl_context = function_context;

    parameter_info_t* p_types = new parameter_info_t[parameter_types.size()];

    parameter_info_t* it_ptypes = &(p_types[0]);
    ObjectList<TL::Type>::iterator type_it = parameter_types.begin();
    for (ObjectList<std::string>::iterator it = parameter_names.begin();
            it != parameter_names.end();
            it++, it_ptypes++, type_it++) {
        scope_entry_t* param = new_symbol(function_context, function_context.current_scope, it->c_str());
        param->entity_specs.is_user_declared = 1;
        param->kind = SK_VARIABLE;
        param->file = "";
        param->line = 0;

        param->defined = 1;

        symbol_set_as_parameter_of_function(param, entry, entry->entity_specs.num_related_symbols);

        param->type_information = get_unqualified_type(type_it->get_internal_type());

        P_LIST_ADD(entry->entity_specs.related_symbols,
                entry->entity_specs.num_related_symbols,
                param);

        it_ptypes->is_ellipsis = 0;
        it_ptypes->nonadjusted_type_info = NULL;
        it_ptypes->type_info = get_indirect_type(param);
    }

    type_t *function_type = get_new_function_type(
            return_type.get_internal_type(),
            p_types,
            parameter_types.size());

    entry->type_information = function_type;

    delete[] p_types;

    return entry;
}

static TL::Symbol new_function_symbol_unpacked(
        TL::Symbol current_function,
        const std::string& function_name,
        CreateOutlineInfo& outline_info,
        Nodecl::Utils::SymbolMap*& out_symbol_map) {
    Scope sc = current_function.get_scope();

    decl_context_t decl_context = sc.get_decl_context();
    decl_context_t function_context;

    function_context = new_function_context(decl_context);
    function_context = new_block_context(function_context);

    // Create all the symbols and an appropiate mapping

    Nodecl::Utils::SimpleSymbolMap *symbol_map = new Nodecl::Utils::SimpleSymbolMap();

    TL::ObjectList<TL::Symbol> parameter_symbols, private_symbols;

    TL::ObjectList<OutlineDataItem*> data_items = outline_info._data_items;
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++) {
        TL::Symbol sym = (*it)->get_symbol();

        std::string name;
        if (sym.is_valid()) {
            name = sym.get_name();
            if (IS_CXX_LANGUAGE
                    && name == "this") {
                name = "this_";
            }
        } else {
            name = (*it)->get_field_name();
        }

        bool already_mapped = false;

        switch ((*it)->get_sharing()) {
            case OutlineDataItem::SHARING_PRIVATE:
            {
                scope_entry_t* private_sym = ::new_symbol(function_context, function_context.current_scope, name.c_str());
                private_sym->kind = SK_VARIABLE;
                private_sym->type_information = (*it)->get_in_outline_type().get_internal_type();
                private_sym->defined = private_sym->entity_specs.is_user_declared = 1;

                if (sym.is_valid()) {
                    symbol_map->add_map(sym, private_sym);

                    // Copy attributes that must be preserved
                    private_sym->entity_specs.is_allocatable = !sym.is_member() && sym.is_allocatable();
                }

                private_symbols.append(private_sym);
                break;
            }
            case OutlineDataItem::SHARING_SHARED:
            case OutlineDataItem::SHARING_CAPTURE:
            case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
            {
                scope_entry_t* private_sym = ::new_symbol(function_context, function_context.current_scope,
                        name.c_str());
                private_sym->kind = SK_VARIABLE;
                private_sym->type_information = (*it)->get_in_outline_type().get_internal_type();
                private_sym->defined = private_sym->entity_specs.is_user_declared = 1;


                if (sym.is_valid()) {
                    private_sym->entity_specs.is_optional = sym.is_optional();
                    private_sym->entity_specs.is_allocatable =
                            !sym.is_member() && sym.is_allocatable();
                    if (!already_mapped) {
                        symbol_map->add_map(sym, private_sym);
                    }
                }

                private_sym->entity_specs.is_allocatable =
                        sym.is_allocatable() ||
                        (((*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE)
                        == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE);

                parameter_symbols.append(private_sym);

                break;
            }
            case OutlineDataItem::SHARING_REDUCTION:
            {
                // Original reduced variable. Passed as we pass shared parameters
                TL::Type param_type = (*it)->get_in_outline_type();
                scope_entry_t* shared_reduction_sym = ::new_symbol(function_context, function_context.current_scope,
                        (*it)->get_field_name().c_str());
                shared_reduction_sym->kind = SK_VARIABLE;
                shared_reduction_sym->type_information = param_type.get_internal_type();
                shared_reduction_sym->defined = shared_reduction_sym->entity_specs.is_user_declared = 1;
                parameter_symbols.append(shared_reduction_sym);

                shared_reduction_sym->entity_specs.is_allocatable = sym.is_valid()
                        && !sym.is_member()
                        && sym.is_allocatable();

                // Private vector of partial reductions. This is a local pointer variable
                // rdv stands for reduction vector
                TL::Type private_reduction_vector_type = (*it)->get_private_type();
                if (IS_C_LANGUAGE
                        || IS_CXX_LANGUAGE) {
                    // T*
                    private_reduction_vector_type = private_reduction_vector_type.get_pointer_to();
                } else {
                    internal_error("Code unreachable", 0);
                }

                scope_entry_t* private_reduction_vector_sym = ::new_symbol(function_context, function_context.current_scope,
                        ("rdv_" + name).c_str());
                private_reduction_vector_sym->kind = SK_VARIABLE;
                private_reduction_vector_sym->type_information = private_reduction_vector_type.get_internal_type();
                private_reduction_vector_sym->defined = private_reduction_vector_sym->entity_specs.is_user_declared = 1;

                // Local variable (rdp stands for reduction private)
                // This variable must be initialized properly
                scope_entry_t* private_sym = ::new_symbol(function_context, function_context.current_scope,
                        ("rdp_" + name).c_str());
                private_sym->kind = SK_VARIABLE;
                private_sym->type_information = (*it)->get_private_type().get_internal_type();
                private_sym->defined = private_sym->entity_specs.is_user_declared = 1;

                if (sym.is_valid()) {
                    symbol_map->add_map(sym, private_sym);
                }

                break;
            }
            default:
            {
                internal_error("Unexpected data sharing kind", 0);
            }
        }
    }

    // Update types of parameters (this is needed by VLAs)
    for (TL::ObjectList<TL::Symbol>::iterator it = parameter_symbols.begin();
            it != parameter_symbols.end();
            it++) {
        it->get_internal_symbol()->type_information =
                type_deep_copy(it->get_internal_symbol()->type_information,
                function_context,
                symbol_map->get_symbol_map());
    }
    // Update types of privates (this is needed by VLAs)
    for (TL::ObjectList<TL::Symbol>::iterator it = private_symbols.begin();
            it != private_symbols.end();
            it++) {
        it->get_internal_symbol()->type_information =
                type_deep_copy(it->get_internal_symbol()->type_information,
                function_context,
                symbol_map->get_symbol_map());
    }

    // Now everything is set to register the function
    scope_entry_t* new_function_sym = new_symbol(decl_context, decl_context.current_scope, function_name.c_str());
    new_function_sym->entity_specs.is_user_declared = 1;

    new_function_sym->kind = SK_FUNCTION;
    new_function_sym->file = "";
    new_function_sym->line = 0;
    
    // Make it static
    new_function_sym->entity_specs.is_static = 1;

    // Make it member if the enclosing function is member
    if (current_function.is_member()) {
        new_function_sym->entity_specs.is_member = 1;
        new_function_sym->entity_specs.class_type = current_function.get_class_type().get_internal_type();

        new_function_sym->entity_specs.access = AS_PUBLIC;

        ::class_type_add_member(new_function_sym->entity_specs.class_type,
                new_function_sym);
    }

    function_context.function_scope->related_entry = new_function_sym;
    function_context.block_scope->related_entry = new_function_sym;

    new_function_sym->related_decl_context = function_context;

    parameter_info_t* p_types = new parameter_info_t[parameter_symbols.size()];

    parameter_info_t* it_ptypes = &(p_types[0]);
    for (ObjectList<TL::Symbol>::iterator it = parameter_symbols.begin();
            it != parameter_symbols.end();
            it++, it_ptypes++) {
        scope_entry_t* param = it->get_internal_symbol();

        symbol_set_as_parameter_of_function(param, new_function_sym, new_function_sym->entity_specs.num_related_symbols);

        P_LIST_ADD(new_function_sym->entity_specs.related_symbols,
                new_function_sym->entity_specs.num_related_symbols,
                param);

        it_ptypes->is_ellipsis = 0;
        it_ptypes->nonadjusted_type_info = NULL;

        // FIXME - We should do all the remaining lvalue adjustments
        type_t* param_type = get_unqualified_type(param->type_information);
        it_ptypes->type_info = param_type;
    }

    type_t *function_type = get_new_function_type(
            get_void_type(),
            p_types,
            parameter_symbols.size());

    new_function_sym->type_information = function_type;

    delete[] p_types;

    out_symbol_map = symbol_map;
    return new_function_sym;
}

void DeviceMPI::generate_additional_mpi_code(
        const TL::Symbol& called_task,
        const TL::Symbol& unpacked_function,
        const TL::ObjectList<Nodecl::NodeclBase>& onto_clause,
        const Nodecl::Utils::SimpleSymbolMap& param_to_args_map,
        const TL::Symbol& struct_args,
        TL::Source& code_host,
        TL::Source& code_device_pre,        
        TL::Source& code_device_post) {
    int num_args_devinfo = onto_clause.size();

    TL::Type argument_type = ::get_user_defined_type(struct_args.get_internal_symbol());
    TL::ObjectList<TL::Symbol> parameters_called = argument_type.get_fields();
    TL::ObjectList<std::string> param_called_names;
    
    int num_params = parameters_called.size();    
    for (int i = 0; i < num_params; ++i) {
        param_called_names.append(parameters_called.at(i).get_name());
    }
    
    TL::ObjectList<std::string> new_dev_info;
    for (int i = 0; i < num_args_devinfo; ++i) {
        if (param_called_names.contains(onto_clause[i].get_symbol().get_name())){
            new_dev_info.append("args."+onto_clause[i].get_symbol().get_name());
        } else {
            new_dev_info.append(onto_clause[i].get_symbol().get_name());            
        }
    }
    if (new_dev_info.size()==0){
        new_dev_info.append("0");
    }
    if (new_dev_info.size()==1){
        new_dev_info.append("-2");
    }

    Source struct_mpi_create;
    Source hostCall;
    Source deviceCall;

    code_host << "MPI_Status ompss___status; "
            << struct_mpi_create
            << hostCall;
    
    code_device_pre << struct_args.get_name() << " args;"
            << "MPI_Comm ompss_parent_comp; "
            << "MPI_Comm_get_parent( &ompss_parent_comp ); "
            << "MPI_Status ompss___status; "
            << struct_mpi_create
            << deviceCall;

    Source typelist_src, blocklen_src, displ_src;
    //Source parameter_call;
    
    struct_mpi_create << "MPI_Datatype ompss___datatype;"
            "MPI_Datatype ompss___typelist[" << num_params << "]= {" << typelist_src << "};"
            "int ompss___blocklen[" << num_params << "] = {" << blocklen_src << "};"
            "MPI_Aint ompss___displ[" << num_params << "] = {" << displ_src << "};";
        
    hostCall << " int id_func_ompss=" << _currTaskId << ";";
    hostCall << " nanos_MPI_Send_taskinit(&id_func_ompss, 1,  ompss_get_mpi_type(\"__mpitypeompss_signed_short\")," + new_dev_info[1] + " , " + new_dev_info[0] + ");";
    hostCall << " nanos_MPI_Send_datastruct( (void *) &args, 1,  ompss___datatype," + new_dev_info[1] + "," + new_dev_info[0] + ");";
    hostCall << " nanos_MPI_Recv_taskend(&id_func_ompss, 1,  ompss_get_mpi_type(\"__mpitypeompss_signed_short\")," + new_dev_info[1] + " , " + new_dev_info[0] + ",&ompss___status);";

    deviceCall << " nanos_MPI_Recv_datastruct(&args, 1, ompss___datatype, 0, ompss_parent_comp, &ompss___status); ";
    //deviceCall << called_task.get_name() << "(" << parameter_call << ");";

    
    code_device_post << " int ompss_id_func=" << _currTaskId << ";";
    code_device_post << " nanos_MPI_Send_taskend(&ompss_id_func, 1, ompss_get_mpi_type(\"__mpitypeompss_signed_short\"), 0, ompss_parent_comp);";

    for (int i = 0; i < num_params; ++i) {
        //parameter_call.append_with_separator("args." + parameters_called[i].get_name(),",");
        std::string ompss_mpi_type = get_ompss_mpi_type(parameters_called[i].get_type());
        displ_src.append_with_separator("((size_t) ( (char *)&((" + struct_args.get_name() + " *)0)->" + parameters_called[i].get_name() + " - (char *)0 ))", ",");
        if (parameters_called[i].get_type().is_pointer()) {
            typelist_src.append_with_separator("ompss_get_mpi_type(\"__mpitypeompss_unsigned_long_long\")", ",");

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

    _mpi_task_processed = true;
    _currTaskId++;
    // Unpack DTO
    const std::string& device_outline_name = get_outline_name(info._outline_name);
    const Nodecl::NodeclBase& original_statements = info._original_statements;
    const TL::Symbol& called_task = info._called_task;
    //OutlineInfo& outline_info = info._outline_info;

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
                // Pass the original reduced variable as if it were a shared
                Source argument;
                if (IS_C_LANGUAGE || IS_CXX_LANGUAGE) {
                    argument << "*(args." << (*it)->get_field_name() << ")";
                } else {
                    internal_error("running error", 0);
                }
                unpacked_arguments.append_with_separator(argument, ", ");

                std::string name = (*it)->get_symbol().get_name();

                private_entities
                        << "rdp_" << name << " = " << as_expression((*it)->get_reduction_info()->get_identity()) << ";"
                        ;

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
    if (called_task.is_valid()) {
        generate_additional_mpi_code(called_task,
                host_function,
                info._target_info.get_onto(),
                info._target_info.get_param_arg_map(),
                info._arguments_struct,
                code_host,
                code_device_pre,
                code_device_post);
    }

    // Create the new unpacked function
    TL::Symbol unpacked_function = new_function_symbol_unpacked(
            current_function,
            device_outline_name + "_unpacked",
            info,
            symbol_map);
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
    Nodecl::Utils::append_to_top_level_nodecl(unpacked_function_code);

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
    
    //std::cout << code_host.get_source(true) << "\n";
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
//    
    // device_outline_name << "_host(" << unpacked_arguments << ");"
    _mpiDaemonMain << " else if (ompss_id_func== " << _currTaskId << "){"
                << device_outline_name << "_device();"
                << "}";
    
    output_statements = original_statements;
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
        ObjectList<Nodecl::NodeclBase> onto_clause=target_information.get_onto();
        Nodecl::Utils::SimpleSymbolMap param_to_args_map = info._target_info.get_param_arg_map();
        std::string assignedComm="0";
        std::string assignedRank="-2";
        if (onto_clause.size()>=1){
            assignedComm=as_symbol( param_to_args_map.map(onto_clause.at(0).get_symbol()));
        }
        if (onto_clause.size()>=2){
            assignedRank=as_symbol( param_to_args_map.map(onto_clause.at(1).get_symbol()));            
        }
        ancillary_device_description
                << comment("MPI device descriptor")
                << "static nanos_mpi_args_t "
                << device_outline_name << "_mpi_args;"
                << device_outline_name << "_mpi_args.outline = (void(*)(void*))" << device_outline_name << "_host;"
                << device_outline_name << "_mpi_args._assignedComm = " << assignedComm << ";"
                << device_outline_name << "_mpi_args._assignedRank = " << assignedRank << ";";
                
//        ancillary_device_description
//                << comment("MPI device descriptor")
//                << "static nanos_mpi_args_t 
//                << device_outline_name << "_args;"
//                << device_outline_name << "_args.outline = { (void(*)(void*))" << device_outline_name << "_host};"
//                << device_outline_name << "_args._assignedComm = " << _assignedComm << ";"
//                << device_outline_name << "_args._assignedRank = " << _assignedRank << ";"
                ;
    } else {
        internal_error("Unsupported Nanos version.", 0);
    }

    device_descriptor << "{ &nanos_mpi_factory, &" << device_outline_name << "_mpi_args }";
}

//void DeviceCUDA::do_replacements(
//		DataEnvironInfo& data_environ,
//		AST_t body,
//		ScopeLink scope_link,
//		Source &initial_setup,
//		Source &replaced_src)
//{
//	do_cuda_outline_replacements(body,
//			scope_link,
//			data_environ,
//			initial_setup,
//			replaced_src);
//}
//

//void DeviceMPI::add_included_cuda_files(FILE* file) {
//    ObjectList<IncludeLine> lines = CurrentFile::get_top_level_included_files();
//    std::string cuda_file_ext(".cu\"");
//    std::string cuda_header_ext(".cuh\"");
//
//    for (ObjectList<IncludeLine>::iterator it = lines.begin(); it != lines.end(); it++) {
//        std::string line = (*it).get_preprocessor_line();
//        std::string extension = line.substr(line.find_last_of("."));
//
//        if (extension == cuda_file_ext || extension == cuda_header_ext) {
//            int output = fprintf(file, "%s\n", line.c_str());
//            if (output < 0)
//                internal_error("Error trying to write the intermediate cuda file\n", 0);
//        }
//    }
//}

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

void DeviceMPI::phase_cleanup(DTO& data_flow) {
    _mpiDaemonMain << "}"; //END WHILE (daemon)
    _mpiDaemonMain << "}"; //END main

    // Check if the file has already been created (and written)
    std::ofstream mpiFile;
    std::string _mpiFilename;
    bool new_file = false;

    //Source included_files;
    if (_mpi_task_processed && !CURRENT_CONFIGURATION->do_not_link) {
        if (CompilationProcess::get_current_file().get_filename(false).find("ompss___mpiWorker_") == std::string::npos) {
            // Set the file name 
            _mpiFilename = "ompss___mpiWorker_";
            _mpiFilename += CompilationProcess::get_current_file().get_filename(false);
            new_file = true;
            mpiFile.open(_mpiFilename.c_str());
            std::ifstream streamFile(CompilationProcess::get_current_file().get_filename(true).c_str());

            mpiFile << streamFile.rdbuf();
            // Remove the intermediate source file
            mark_file_for_cleanup(_mpiFilename.c_str());
            
            Symbol main  = _root.retrieve_context().get_symbol_from_name("main");            
            if (main.is_valid()){
                //Symbol sMain=listMain.at(0);
                //Get filename and pass value to nanox
                Source filenameSrc;                
                filenameSrc << "setMpiFilename(\"./";
//                if (CompilationConfiguration::get_current_configuration() != "mic"){
//                    filenameSrc << "mic";
//                }
                if (CURRENT_CONFIGURATION->linked_output_filename != NULL)
                 {
                    filenameSrc << give_basename(CURRENT_CONFIGURATION->linked_output_filename)  << "\");";
                } else {
                    filenameSrc << "a.out"  << "\");";
                }
                Source real_main;
                real_main << "int ompss_tmp_main(int argc, char* argv[]) {"
                        << filenameSrc
                        << "if (argc > 1 && !strcmp(argv[argc-1],\"" << TAG_MAIN_OMPSS << "\")){"
                        << "ompss___mpi_daemon_main(argc,argv);"
                        << "return 0;"
                        << "} else {"
                        << "return main(argc,argv);"
                        <<"}}"
                        ;
                
                //Nodecl::FunctionCode function_code = main.get_function_code().as<Nodecl::FunctionCode>();
                //std::cout << _root.prettyprint() << "\n"; 
                Nodecl::NodeclBase new_main = real_main.parse_global(main.get_function_code()); 
                Nodecl::NodeclBase newompss_main = _mpiDaemonMain.parse_global(_root);
                Nodecl::Utils::prepend_to_top_level_nodecl(newompss_main);
                Nodecl::Utils::append_to_top_level_nodecl(new_main);
                main.set_name("ompss___user_main");
                //lRoot.at(lRoot.size()-1).append_sibling(new_main);
                _root.retrieve_context().get_symbol_from_name("ompss_tmp_main").set_name("main");
            }
            

            // Get *.cu included files
            //        ObjectList<IncludeLine> lines = CurrentFile::get_top_level_included_files();
            //
            //        for (ObjectList<IncludeLine>::iterator it = lines.begin(); it != lines.end(); it++) {
            //            std::string line = (*it).get_preprocessor_line();
            //            if (line.find("nanos") == std::string::npos) {
            //                included_files << line << "\n";
            //            }
            //        }
            //Add a copy of current file for MIC
            const std::string configuration_name = "mic";
            //CompilationProcess::add_file(_mpiFilename, configuration_name, new_file);
            mpiFile.close();
        }
    }



    //_cudaFilename = "";
    //_cudaHeaderFilename = "";
    //_root = AST_t(0);
}

void DeviceMPI::pre_run(DTO& dto) {
    _root = dto["nodecl"];
    _mpi_task_processed = false;
    //_mpiDaemonMain <<"    MPI_Datatype dame_int(){	return MPI_INT;}MPI_Datatype ompss_get_mpi_type(__mpitypeompss_signed_short{	return MPI_SHORT;}MPI_Datatype dame_byte(){return MPI_BYTE;}";

    _mpiDaemonMain << "int ompss___mpi_daemon_main(int argc, char* argv[]) { "
            << " nanos_MPI_Init(&argc, &argv);	"
            << " int ompss_id_func; "
            << " MPI_Status ompss___status; "
            << " MPI_Comm ompss_parent_comp; "
            << " MPI_Comm_get_parent( &ompss_parent_comp ); "
            << " while(1){ "
            << " nanos_MPI_Recv_taskinit(&ompss_id_func, 1, ompss_get_mpi_type(\"__mpitypeompss_signed_short\"), 0, ompss_parent_comp, &ompss___status); "
            << " if (ompss_id_func==-1){ "
            // << " MPI_Finalize(); "
            << " return 0; "
            << " } ";
    //_root = dto["translation_unit"];
    //if (dto.get_keys().contains("openmp_task_info"))
    //{
    //	_function_task_set = RefPtr<OpenMP::FunctionTaskSet>::cast_static(dto["openmp_task_info"]);
    //}
}

void DeviceMPI::run(DTO& dto) {
}

std::string DeviceMPI::get_ompss_mpi_type(Type type) {
    std::string result = "ompss_get_mpi_type(\"__mpitypeompss_";
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
