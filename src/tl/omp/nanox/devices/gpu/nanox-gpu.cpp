#include "tl-devices.hpp"
#include "nanox-gpu.hpp"
#include "tl-declarationclosure.hpp"
#include "tl-multifile.hpp"

#include <iostream>
#include <fstream>

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

    ObjectList<OpenMP::CopyItem> copies = data_env_info.get_copy_items();

    unsigned int j = 0;
    for (ObjectList<OpenMP::CopyItem>::iterator it = copies.begin();
            it != copies.end();
            it++, j++)
    {
        DataReference data_ref = it->get_copy_expression();
        Symbol sym = data_ref.get_base_symbol();
        Type type = sym.get_type();

        bool points_an_array = false;

        if (type.is_array())
        {
            type = type.array_element().get_pointer_to();
            points_an_array = true;
        }
        else
        {
            type = type.get_pointer_to();
        }

        // There are some problems with the typesystem currently
        // that require these workarounds
        if (data_ref.get_type().is_array()
                && data_ref.get_data_type().is_pointer())
        {
            // Shaping expressions ([e] a)  have a type of array but we do not
            // want the array but the related pointer
            type = data_ref.get_data_type();
            points_an_array = true;
        }
        else if (data_ref.get_data_type().is_array())
        {
            // Array sections have a scalar type, but the data type will be array
            // See ticket #290
            type = data_ref.get_data_type().array_element().get_pointer_to();
            points_an_array = true;
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

        replace_src.add_replacement(sym, "(*" + copy_name + ")");

        // No replacement for arrays
        if (points_an_array)
        {
            replace_src.add_replacement(sym, copy_name);
        }
        else
        {
            replace_src.add_replacement(sym, "(*" + copy_name + ")");
        }
    }

    replaced_outline << replace_src.replace(body);
}

DeviceGPU::DeviceGPU()
    : DeviceProvider(/* needs_copies */ true)
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
    AST_t function_def_tree = reference_tree.get_enclosing_function_definition();
    FunctionDefinition enclosing_function(function_def_tree, sl);

    Source result, body, outline_name, parameter_list;

    Source forward_declaration;
    Symbol function_symbol = enclosing_function.get_function_symbol();

    result
        << "void " << outline_name << "(" << parameter_list << ")"
        << "{"
        << body
        << "}"
        ;

    parameter_list
		<< "void * _args"
        ;

    outline_name
        << gpu_outline_name(task_name)
        ;


    if (outline_flags.task_symbol != NULL)
    {
    	AST_t function_tree = outline_flags.task_symbol.get_point_of_declaration();

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
    				forward_declaration << line << "\n";
    			}
    		}
    	}

    	forward_declaration << "extern \"C\" {\n";

    	// Get the definition of non local symbols
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

    	// Check if the task symbol has a function definition or declaration
    	if (FunctionDefinition::predicate(function_tree))
    	{
    		forward_declaration << function_tree.get_enclosing_function_definition(false).prettyprint_external();
    	}
    	else
    	{
    		forward_declaration << function_tree.get_enclosing_global_tree().prettyprint_external();
    	}

    	// Remove the task body from the original source file and replace it for the outline declaration

    	Source outline_decl;
    	outline_decl << "void " << outline_name << "(" << parameter_list << ");";
    	AST_t outline_decl_tree
    	        = outline_decl.parse_declaration(outline_flags.task_symbol.get_point_of_declaration()/*function_tree.get_enclosing_function_definition(true)*/, sl);
    	function_tree.get_enclosing_function_definition(true).replace_in_list(outline_decl_tree);

    	// Close the extern \"C\" key
    	forward_declaration << "}\n";

    }

    Source private_vars, final_code;

    body
        << private_vars
        << initial_setup
        << outline_body
        << final_code
        ;

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
    AST_t outline_code_tree
        = result.parse_declaration(enclosing_function.get_ast(), sl);

    std::string file_name ("mcc____");
    file_name += CompilationProcess::get_current_file().get_filename(false);
    size_t file_extension = file_name.find_last_of(".");
    file_name.erase(file_extension, file_name.length());

    file_name += ".cu";

    const std::string configuration_name = "cuda";
    bool new_file = true;

    CompilationProcess::add_file(file_name, configuration_name, new_file);

    std::ofstream cudaFile;
    cudaFile.open (file_name.c_str());
    cudaFile << forward_declaration.get_source(false) << "\n";
    cudaFile << "extern \"C\" {\n";
    cudaFile << outline_code_tree.prettyprint_external() << "\n";
    cudaFile << "}";
    cudaFile.close();

    // reference_tree.prepend_sibling_function(outline_code_tree);
}

void DeviceGPU::get_device_descriptor(const std::string& task_name,
        DataEnvironInfo &data_environ,
        const OutlineFlags&,
        Source &ancillary_device_description,
        Source &device_descriptor)
{
    Source outline_name;
    outline_name
        << gpu_outline_name(task_name);
        ;

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

EXPORT_PHASE(TL::Nanox::DeviceGPU);
