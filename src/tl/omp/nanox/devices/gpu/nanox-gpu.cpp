#include "tl-devices.hpp"
#include "nanox-gpu.hpp"
#include "tl-declarationclosure.hpp"
#include "tl-multifile.hpp"

#include <iostream>
#include <fstream>

#include "cxx-driver-utils.h"

using namespace TL;
using namespace TL::Nanox;

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

static void do_gpu_outline_replacements(
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
            continue;

        if (data_env_item.is_copy())
        {
        	replace_src.add_replacement(sym, "_args->" + field_name);
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
        else if (data_ref.is_array_section())
        {
            // Array sections have a scalar type, but the data type will be array
            // See ticket #290
            type = data_ref.get_data_type().array_element().get_pointer_to();
        }

        std::string copy_name = "_cp_" + sym.get_name();

        if (!err_declared)
        {
            copy_setup
                << "nanos_err_t cp_err;"
                ;
            err_declared = true;
        }

        DataEnvironItem data_env_item = data_env_info.get_data_of_symbol(sym);

        ERROR_CONDITION(!data_env_item.get_symbol().is_valid(),
                "Invalid data for copy symbol", 0);

        std::string field_addr = "_args->" + data_env_item.get_field_name();

        copy_setup
            << type.get_declaration(sc, copy_name) << ";"
            << "cp_err = nanos_get_addr(" << j << ", (void**)&" << copy_name << ");"
            << "if (cp_err != NANOS_OK) nanos_handle_error(cp_err);"
            ;

	replace_src.add_replacement(sym, copy_name);
    }

    replaced_outline << replace_src.replace(body);
}

DeviceGPU::DeviceGPU()
	: DeviceProvider(/* needs_copies */ true), _cudaFilename("")
{
	DeviceHandler &device_handler(DeviceHandler::get_device_handler());

	device_handler.register_device("cuda", this);

	set_phase_name("Nanox GPU (CUDA) support");
	set_phase_description("This phase is used by Nanox phases to implement GPU (CUDA) device support");
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
	/***************** Write the CUDA file *****************/

	// Check if the file has already been created (and written)
	bool new_file = false;

	if (_cudaFilename == "") {
		// Set the file name
		_cudaFilename = "cudacc_";
		_cudaFilename += CompilationProcess::get_current_file().get_filename(false);
		size_t file_extension = _cudaFilename.find_last_of(".");
		_cudaFilename.erase(file_extension, _cudaFilename.length());
		_cudaFilename += ".cu";
		new_file = true;

		// Remove the intermediate source file
		mark_file_for_cleanup( _cudaFilename.c_str() );
	}

	const std::string configuration_name = "cuda";
	CompilationProcess::add_file(_cudaFilename, configuration_name, new_file);

	// Get all the needed symbols and CUDA included files
	Source included_files, forward_declaration;
	AST_t function_tree;

	// Get *.cu included files
	ObjectList<IncludeLine> lines = CurrentFile::get_top_level_included_files();
	std::string cuda_line (".cu\"");
	std::size_t cuda_size = cuda_line.size();

	for (ObjectList<IncludeLine>::iterator it = lines.begin(); it != lines.end(); it++)
	{
		std::string line = (*it).get_preprocessor_line();
		if (line.size() > cuda_size)
		{
			std::string matching = line.substr(line.size()-cuda_size,cuda_size);
			if (matching == cuda_line)
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
			// Check if we have already printed the function definition in the CUDA file
			if (_taskSymbols.count(outline_flags.task_symbol.get_name()) == 0) {
				forward_declaration << function_tree.get_enclosing_function_definition(false).prettyprint_external();

				// Keep record of which tasks have been printed to the CUDA file
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
				// Check if we have already printed the function definition in the CUDA file
				if (_taskSymbols.count(outline_flags.task_symbol.get_name()) == 0)
				{
					forward_declaration << funct_def_list[0].get_enclosing_function_definition(false).prettyprint_external();

					// Keep record of which tasks have been printed to the CUDA file
					// in order to avoid repeating them
					_taskSymbols.insert(outline_flags.task_symbol.get_name());
				}

				// Remove the function definition from the original source code
				funct_def_list[0].remove_in_list();
			}
			else if (funct_def_list.size() == 0
					&& _taskSymbols.count(outline_flags.task_symbol.get_name()) > 0)
			{
				// We have already removed it and printed it in the CUDA file, do nothing
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
		Source funct_id, funct_description;
		Symbol function_symbol = enclosing_function.get_function_symbol();

		instrument_before
			<< "static int nanos_funct_id_init = 0;"
			<< "static nanos_event_key_t nanos_instr_user_fun_key = 0;"
			<< "static nanos_event_value_t nanos_instr_user_fun_value = 0;"
			<< "if (nanos_funct_id_init == 0)"
			<< "{"
			<<    "nanos_err_t err = nanos_instrument_get_key(\"user-funct\", &nanos_instr_user_fun_key);"
			<<    "if (err != NANOS_OK) nanos_handle_error(err);"
			<<    "err = nanos_instrument_register_value ( &nanos_instr_user_fun_value, \"user-funct\","
			<<               funct_id << "," << funct_description << ", 0);"
			<<    "if (err != NANOS_OK) nanos_handle_error(err);"
			<<    "nanos_funct_id_init = 1;"
			<< "}"
			<< "nanos_instrument_enter_burst(nanos_instr_user_fun_key, nanos_instr_user_fun_value);"
			;

		instrument_after
			<< "nanos_instrument_leave_burst(nanos_instr_user_fun_key);"
			;

		funct_id
			<< "\"" << outline_name << ":" << reference_tree.get_locus() << "\""
			;

		funct_description
			<< "\"Outline created after construct at '"
			<< reference_tree.get_locus()
			<< "' found in function '" << function_symbol.get_qualified_name() << "'\""
			;
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

	// outline_name
	outline_name
		<< gpu_outline_name(task_name)
		;

	// parameter_list
	parameter_list
		<< struct_typename << "* _args"
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

	// Parse it in a sibling function context
	AST_t outline_code_tree =
			result.parse_declaration(enclosing_function.get_ast(), sl);

	std::ofstream cudaFile;
	if (new_file)
	{
		cudaFile.open (_cudaFilename.c_str());
		cudaFile << included_files.get_source(false) << "\n";
	}
	else
	{
		cudaFile.open (_cudaFilename.c_str(), std::ios_base::app);
	}

	cudaFile << "extern \"C\" {\n";
	cudaFile << forward_declaration.get_source(false) << "\n";
	cudaFile << outline_code_tree.prettyprint_external() << "\n";
	cudaFile << "}\n";
	cudaFile.close();

	/******************* Write the C file ******************/

	// Check if the task is a function, or it is inlined
	if (outline_flags.task_symbol != NULL)
	{
		// We have already removed the function definition
		// Now replace it for the outline declaration
		Source function_decl_src;
		function_decl_src << "void " << outline_name << "(" << struct_typename << "*);"
				;

		AST_t function_decl_tree = function_decl_src.parse_declaration(reference_tree, sl);
		reference_tree.prepend_sibling_function(function_decl_tree);
	}
	else
	{
		// Forward declaration of the task outline
		Source outline_declaration_src;
		outline_declaration_src << "void " << outline_name << "(" << parameter_list << ");";
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
		<< "nanos_smp_args_t " << task_name << "_gpu_args = { (void(*)(void*))" << outline_name << "};"
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

void DeviceGPU::phase_cleanup(DTO& data_flow)
{
	_cudaFilename = "";
	_root = AST_t(0);
}

void DeviceGPU::pre_run(DTO& dto)
{
	_root = dto["translation_unit"];
}

EXPORT_PHASE(TL::Nanox::DeviceGPU);
