#include "tl-omp-nanox.hpp"
#include "tl-data-env.hpp"
#include "tl-counters.hpp"
#include "tl-outline-nanox.hpp"
#include "tl-parallel-common.hpp"

using namespace TL;
using namespace TL::Nanox;

void OMPTransform::parallel_postorder(PragmaCustomConstruct ctr)
{
    OpenMP::DataSharing& data_sharing = openmp_info->get_data_sharing(ctr.get_ast());

    ObjectList<Symbol> shared_symbols;
    data_sharing.get_all_symbols(OpenMP::DA_SHARED, shared_symbols);

    ObjectList<Symbol> firstprivate_symbols;
    data_sharing.get_all_symbols(OpenMP::DA_FIRSTPRIVATE, firstprivate_symbols);

    Source private_decls;
    ObjectList<Symbol> private_symbols;
    data_sharing.get_all_symbols(OpenMP::DA_PRIVATE, private_symbols);
    for (ObjectList<Symbol>::iterator it = private_symbols.begin();
            it != private_symbols.end();
            it++)
    {
        Symbol& sym(*it);
        Type type = sym.get_type();

        // In C++ private vars types must be default constructible
        private_decls
            << type.get_declaration(sym.get_scope(), sym.get_name()) << ";"
            ;
    }

    // FIXME - Reductions!!
    DataEnvironInfo data_environ_info;

    Source struct_arg_type_decl_src;
    std::string struct_arg_type_name;
    fill_data_environment_structure(firstprivate_symbols,
            shared_symbols,
            ctr.get_scope_link(),
            struct_arg_type_name,
            struct_arg_type_decl_src,
            data_environ_info);

    FunctionDefinition funct_def = ctr.get_enclosing_function();
    Symbol function_symbol = funct_def.get_function_symbol();

    int outline_num = TL::CounterManager::get_counter(NANOX_OUTLINE_COUNTER);
    TL::CounterManager::get_counter(NANOX_OUTLINE_COUNTER)++;
    Source outline_name;
    outline_name
        << "_ol_" << function_symbol.get_name() << "_" << outline_num
        ;

    Source initial_replace_code, replaced_body;

    do_outline_replacements(ctr.get_statement(),
            data_environ_info,
            replaced_body,
            initial_replace_code);

    Source final_barrier;

    final_barrier
        << "nanos_team_barrier();"
        ;

    Source outline_body, outline_parameters, outline_code;

    outline_parameters << struct_arg_type_name << "* _args";
    outline_body
        << private_decls
        << initial_replace_code
        << replaced_body
        << final_barrier
        ;

    outline_code = create_outline(
            funct_def,
            outline_name,
            outline_parameters,
            outline_body);

    Source device_description, device_descriptor, num_devices;

    // Refactor!
    Source newly_generated_code;
    newly_generated_code
        << struct_arg_type_decl_src
        << outline_code
        // Devices related to this task
        << device_description
        ;
    
    // Device descriptor
    // FIXME - Currently only SMP is supported
    device_descriptor << outline_name << "_devices";
    device_description
        << "nanos_smp_args_t " << outline_name << "_smp_args = { (void(*)(void*))" << outline_name << "};"
        << "nanos_device_t " << device_descriptor << "[] ="
        << "{"
        // SMP
        << "{nanos_smp_factory, &" << outline_name << "_smp_args" << "},"
        << "};"
        ;

    // Currently only SMP is supported
    num_devices << 1;
    
    // Parse it in a sibling function context
    AST_t outline_code_tree
        = newly_generated_code.parse_declaration(funct_def.get_ast(), ctr.get_scope_link());
    ctr.get_ast().prepend_sibling_function(outline_code_tree);

    Source num_threads;

    PragmaCustomClause num_threads_clause = ctr.get_clause("num_threads");
    if (num_threads_clause.is_defined())
    {
        num_threads << num_threads_clause.get_expression_list()[0];
    }
    else
    {
        // Do not know how to request the default parallel team thread number
        num_threads << "0";
    }

    Source spawn_source = common_parallel_spawn_code(num_devices,
            device_descriptor,
            struct_arg_type_name,
            num_threads,
            data_environ_info);

    AST_t spawn_tree = spawn_source.parse_statement(ctr.get_ast(), ctr.get_scope_link());
    ctr.get_ast().replace(spawn_tree);
}

