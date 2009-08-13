#include "tl-omp-nanox.hpp"
#include "tl-data-env.hpp"
#include "tl-counters.hpp"

using namespace TL;
using namespace TL::Nanox;

void OMPTransform::task_postorder(PragmaCustomConstruct ctr)
{
    OpenMP::DataSharing& data_sharing = openmp_info->get_data_sharing(ctr.get_ast());

    ObjectList<Symbol> shared_symbols;
    data_sharing.get_all_symbols(OpenMP::DA_SHARED, shared_symbols);

    ObjectList<Symbol> firstprivate_symbols;
    data_sharing.get_all_symbols(OpenMP::DA_FIRSTPRIVATE, firstprivate_symbols);

    ObjectList<Symbol> private_symbols;
    data_sharing.get_all_symbols(OpenMP::DA_PRIVATE, private_symbols);

    DataEnvironInfo data_environ_info;

    Source struct_arg_type_decl_src;
    std::string struct_arg_type_name;
    fill_data_environment_structure(firstprivate_symbols,
            shared_symbols,
            ctr.get_scope_link(),
            struct_arg_type_name,
            struct_arg_type_decl_src,
            data_environ_info);

    Source forward_declaration;
    FunctionDefinition funct_def = ctr.get_enclosing_function();
    Symbol function_symbol = funct_def.get_function_symbol();

    if (!function_symbol.is_member())
    {
        IdExpression function_name = funct_def.get_function_name();
        Declaration point_of_decl = function_name.get_declaration();
        DeclarationSpec decl_specs = point_of_decl.get_declaration_specifiers();
        ObjectList<DeclaredEntity> declared_entities = point_of_decl.get_declared_entities();
        DeclaredEntity declared_entity = *(declared_entities.begin());

        forward_declaration 
            // << template_header
            << decl_specs.prettyprint()
            << " "
            << declared_entity.prettyprint()
            << ";";
    }

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

    Source device_description, device_descriptor;

    // FIXME - Refactor this since it is quite common
    Source newly_generated_code;
    newly_generated_code
        << struct_arg_type_decl_src
        << forward_declaration
        << "void " << outline_name << "(" << struct_arg_type_name << "* _args)"
        << "{"
        <<     initial_replace_code
        <<     replaced_body
        << "}"
        // Devices related to this task
        << device_description
        ;

    // Device descriptor
    // FIXME - Currently only SMP is supported
    device_descriptor << outline_name << "_devices";
    device_description
        << "nanos_device_t " << device_descriptor << "[] ="
        << "{"
        // SMP
        // FIXME: g++ will not allow C99 initialization syntax!!!
        // (technically: only C90 intialization is valid in C++03)
        // (sad note aside: even C++0x is not going to support C99 initializers)
        // So, args of a device should be a void* and we could statically make
        // it point a previously device-dependent structure 
        << "{nanos_smp_factory, { .smp = { (void(*)(void*))"  << outline_name << " }}},"
        // There must be some kind of sentinel, otherwise nobody will know the length of this array
        // and does not seem to be a size argument for the device
        << "{ (void* (*)(void*))0 }"
        << "};"
        ;

    // Parse it in a sibling function context
    AST_t outline_code 
        = newly_generated_code.parse_declaration(funct_def.get_ast(), ctr.get_scope_link());
    ctr.get_ast().prepend_sibling_function(outline_code);
    
// FIXME - No dependences (yet)
    Source spawn_code;
    Source fill_outline_arguments, fill_immediate_arguments;

    fill_data_args("ol_args->", data_environ_info, fill_outline_arguments);
    fill_data_args("imm_args.", data_environ_info, fill_immediate_arguments);

    // Honour if clause
    Source if_expr_cond_start, if_expr_cond_end;
    PragmaCustomClause if_clause = ctr.get_clause("if");
    if (if_clause.is_defined())
    {
        ObjectList<Expression> expr_list = if_clause.get_expression_list();

        if (expr_list.size() != 1)
        {
            running_error("%s: error: clause 'if' requires just one argument\n",
                    ctr.get_ast().get_locus().c_str());
        }

        Expression &expr = expr_list[0];

        if_expr_cond_start
            << "if (" << expr << ")"
            << "{"
            ;

        if_expr_cond_end << "}";
    }

    spawn_code
        << "{"
        <<     struct_arg_type_name << "* ol_args = (" << struct_arg_type_name << "*)0;"
        <<     "nanos_wd_t wd = (nanos_wd_t)0;"
        // <<     "nanos_wd_props_t props;"
        <<     "nanos_err_t err;"
        <<      if_expr_cond_start
        <<      "err = nanos_create_wd(&wd, " << device_descriptor << ", sizeof(" << struct_arg_type_name << "),"
        <<                 "(void**)&ol_args, nanos_current_wd(), (nanos_wd_props_t*)0);" // props are 0 here
        <<      if_expr_cond_end
        //     FIXME - Do something useful with err
        <<     "if (wd != (nanos_wd_t)0)"
        <<     "{"
        <<        fill_outline_arguments
        <<        "err = nanos_submit(wd, (nanos_dependence_t*)0, (nanos_team_t)0);"
        //     FIXME - Do something useful with err 
        <<     "}"
        <<     "else"
        <<     "{"
        <<        struct_arg_type_name << " imm_args;"
        <<        fill_immediate_arguments
        <<        "err = nanos_create_wd_and_run(" << device_descriptor << ", " 
        <<        "       &imm_args, (nanos_dependence_t*)0, (nanos_wd_props_t*)0);"
        //     FIXME - Do something useful with err 
        <<     "}"
        << "}"
        ;

    AST_t spawn_tree = spawn_code.parse_statement(ctr.get_ast(), ctr.get_scope_link());
    ctr.get_ast().replace(spawn_tree);
}
