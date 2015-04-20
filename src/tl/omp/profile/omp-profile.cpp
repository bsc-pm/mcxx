/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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



#include "omp-profile.hpp"
#include <sstream>
#include <string>
#include "tl-langconstruct.hpp"

namespace TL
{
    OpenMPProfile::OpenMPProfile()
        : _current_task_id(1),
        _task_ops_threshold(0)
    {
        set_phase_name("OpenMP task static profiler");
        set_phase_description("This phase adds to task constructs static profiling such "
                "as number of instructions and accesses to shared/firstprivate variables");

        register_parameter("ops_threshold",
                "Integer that specifies the threshold of operations for a task. Zero means no threshold at all",
                _task_ops_threshold_str,
                "0").connect(functor(&OpenMPProfile::set_task_threshold, *this));

        register_parameter("fun_ops",
                "Sequence of 'name:cost' interspersed with ':' to tell the compiler the cost in "
                "number of operations of a library function (example: 'sin:100:cos:100:log:200')",
                _fun_ops_str,
                "").connect(functor(&OpenMPProfile::set_fun_ops, *this));
                
    }

    void OpenMPProfile::set_task_threshold(const std::string& str)
    {
        std::stringstream ss;
        ss << str;
        ss >> _task_ops_threshold;

        if (ss.fail())
        {
            _task_ops_threshold = 0;
            std::cerr << "Invalid specification for parameter 'ops_threshold'. Ignoring" << std::endl;
        }
    }

    void OpenMPProfile::set_fun_ops(const std::string& _str)
    {
        std::string str(_str);

        // To simplify the code below
        str += ":";

        std::string::size_type pos;

        bool func_name = true;

        std::string current_function_name;

        pos = str.find_first_of(':');
        while (pos != std::string::npos)
        {
            std::string current_entity = str.substr(0, pos);
            str = str.substr(pos + 1);

            // std::cerr << "pos " << pos << std::endl;
            // std::cerr << "1 " << current_entity << std::endl;
            // std::cerr << "2 " << str << std::endl;

            if (func_name)
            {
                current_function_name = current_entity;
            }
            else
            {
                std::string current_cost_str = current_entity;

                // std::cerr << "User has specified that function '" 
                //     << current_function_name 
                //     << "' will have cost " 
                //     << current_cost << std::endl;

                std::stringstream ss(current_cost_str);

                int current_cost = 0;

                ss >> current_cost;

                if (ss.fail()
                        || !ss.eof())
                {
                    std::cerr << "Invalid specification of cost '" << current_cost_str << "'. Ignoring" << std::endl;
                }
                else
                {
                    _cost_map[current_function_name] = current_cost;
                }
            }
            func_name = !func_name;

            pos = str.find_first_of(':');
        }

        if (!func_name)
        {
            std::cerr << "Wrong syntax for fun_ops parameter, a cost for the function '" 
                << current_function_name << "' was expected"  << std::endl;
        }
    }

    void OpenMPProfile::run(DTO& dto)
    {
        AST_t translation_unit = dto["translation_unit"];
        ScopeLink scope_link = dto["scope_link"];

        // First gather all function defs
        _function_definition_list = translation_unit.depth_subtrees(FunctionDefinition::predicate);

        OpenMPPhase::run(dto);
        perform_delayed_closures();

        // Add a destructor showing the statistics
        if (_current_task_id > 1)
        {
            Source src;
            src << "__attribute__((destructor))"
                << "static void gather_statistics(void)"
                << "{"
                <<     "TaskProfileInfo task_summary;"
                <<     "__builtin_memset(&task_summary, 0, sizeof(task_summary));"
                <<     "fprintf(stderr, \"%s\", \"Task statistics for file '" 
                <<     CompilationProcess::get_current_file().get_filename()
                <<     "'\\n\");"
                ;

            for (int i = 1; i < _current_task_id; i++)
            {
                src <<
                    "fprintf(stderr, \"Task at '%s:%u'\\n\", task_info_" << i << ".file, task_info_" << i << ".line);"
                    "fprintf(stderr, \"\\tNumber of times run: %llu\\n\", task_info_" << i << ".num_invocations);"
                    "task_summary.num_invocations += task_info_" << i << ".num_invocations;"

                    "fprintf(stderr, \"\\tNumber of times task has exceeded the threshold: %llu\\n\", task_info_" << i << ".times_in_threshold);"
                    "task_summary.times_in_threshold += task_info_" << i << ".times_in_threshold;"

                    "fprintf(stderr, \"\\tNumber of taskwaits: %llu\\n\", task_info_" << i << ".num_taskwait);"
                    "task_summary.num_taskwait += task_info_" << i << ".num_taskwait;"

                    "fprintf(stderr, \"\\tNumber of reads of variables: %llu\\n\", task_info_" << i << ".num_read);"
                    "task_summary.num_read += task_info_" << i << ".num_read;"

                    "fprintf(stderr, \"\\tNumber of writes to variables: %llu\\n\", task_info_" << i << ".num_write);"
                    "task_summary.num_write += task_info_" << i << ".num_write;"

                    "fprintf(stderr, \"\\tNumber of shared variables: %llu\\n\", task_info_" << i << ".num_shared);"
                    "task_summary.num_shared += task_info_" << i << ".num_shared;"

                    "fprintf(stderr, \"\\tNumber of firstprivate variables: %llu\\n\", task_info_" << i << ".num_firstprivate);"
                    "task_summary.num_firstprivate += task_info_" << i << ".num_firstprivate;"

                    "fprintf(stderr, \"\\tNumber of reads of global shared vars: %llu\\n\", task_info_" << i << ".num_global_shared_read);"
                    "task_summary.num_global_shared_read += task_info_" << i << ".num_global_shared_read;"

                    "fprintf(stderr, \"\\tNumber of writes to global shared vars: %llu\\n\", task_info_" << i << ".num_global_shared_write);"
                    "task_summary.num_global_shared_write += task_info_" << i << ".num_global_shared_write;"

                    "fprintf(stderr, \"\\tNumber of reads of parent shared vars: %llu\\n\", task_info_" << i << ".num_parent_shared_read);"
                    "task_summary.num_parent_shared_read += task_info_" << i << ".num_parent_shared_read;"

                    "fprintf(stderr, \"\\tNumber of writes to parent shared vars: %llu\\n\", task_info_" << i << ".num_parent_shared_write);"
                    "task_summary.num_parent_shared_write += task_info_" << i << ".num_parent_shared_write;"

                    "fprintf(stderr, \"\\tNumber of reads of potentially shared vars: %llu\\n\", task_info_" << i << ".num_potential_shared_read);"
                    "task_summary.num_potential_shared_read += task_info_" << i << ".num_potential_shared_read;"

                    "fprintf(stderr, \"\\tNumber of writes to potentially shared vars: %llu\\n\", task_info_" << i << ".num_potential_shared_write);"
                    "task_summary.num_potential_shared_write += task_info_" << i << ".num_potential_shared_write;"

                    "fprintf(stderr, \"\\tNumber of sizeof of firstprivate vars: %llu\\n\", task_info_" << i << ".sizeof_fp);"
                    "task_summary.sizeof_fp += task_info_" << i << ".sizeof_fp;"

                    "fprintf(stderr, \"\\tNumber of reads of firsprivate vars: %llu\\n\", task_info_" << i << ".num_fp_read);"
                    "task_summary.num_fp_read += task_info_" << i << ".num_fp_read;"

                    "fprintf(stderr, \"\\tNumber of writes to firstprivate vars: %llu\\n\", task_info_" << i << ".num_fp_write);"
                    "task_summary.num_fp_write += task_info_" << i << ".num_fp_write;"

                    "fprintf(stderr, \"\\tNumber of arithmetic operations: %llu\\n\", task_info_" << i << ".num_ops);"
                    "task_summary.num_ops += task_info_" << i << ".num_ops;"

                    "fprintf(stderr, \"\\tNumber of childs: %llu\\n\", task_info_" << i << ".num_childs);"
                    "task_summary.num_childs += task_info_" << i << ".num_childs;"

                    "fprintf(stderr, \"\\n\");"
                    ;
            }

            src << "fprintf(stderr, \"%s\", \"Summary of task statistics for file '" 
                << CompilationProcess::get_current_file().get_filename()
                << "'\\n\");"
                "fprintf(stderr, \"\\tNumber of times run: %llu\\n\", task_summary.num_invocations);"

                "fprintf(stderr, \"\\tNumber of times tasks have exceeded the threshold: %llu\\n\", task_summary.times_in_threshold);"

                "fprintf(stderr, \"\\tNumber of reads of variables: %llu\\n\", task_summary.num_read);"

                "fprintf(stderr, \"\\tNumber of writes to variables: %llu\\n\", task_summary.num_write);"

                "fprintf(stderr, \"\\tNumber of taskwaits: %llu\\n\", task_summary.num_taskwait);"

                "fprintf(stderr, \"\\tNumber of shared variables: %llu\\n\", task_summary.num_shared);"

                "fprintf(stderr, \"\\tNumber of firstprivate variables: %llu\\n\", task_summary.num_firstprivate);"

                "fprintf(stderr, \"\\tNumber of reads of global shared vars: %llu\\n\", task_summary.num_global_shared_read);"

                "fprintf(stderr, \"\\tNumber of writes of global shared vars: %llu\\n\", task_summary.num_global_shared_write);"

                "fprintf(stderr, \"\\tNumber of reads of parent shared vars: %llu\\n\", task_summary.num_parent_shared_read);"

                "fprintf(stderr, \"\\tNumber of writes of parent shared vars: %llu\\n\", task_summary.num_parent_shared_write);"

                "fprintf(stderr, \"\\tNumber of reads of potentially shared vars: %llu\\n\", task_summary.num_potential_shared_read);"

                "fprintf(stderr, \"\\tNumber of writes of potentially shared vars: %llu\\n\", task_summary.num_potential_shared_write);"

                "fprintf(stderr, \"\\tNumber of sizeof of firstprivate vars: %llu\\n\", task_summary.sizeof_fp);"

                "fprintf(stderr, \"\\tNumber of reads of firsprivate vars: %llu\\n\", task_summary.num_fp_read);"

                "fprintf(stderr, \"\\tNumber of writes of firstprivate vars: %llu\\n\", task_summary.num_fp_write);"

                "fprintf(stderr, \"\\tNumber of arithmetic operations: %llu\\n\", task_summary.num_ops);"

                "fprintf(stderr, \"\\tNumber of childs: %llu\\n\", task_summary.num_childs);"

                "fprintf(stderr, \"\\n\");"
                "}"
                ;

            AST_t tree = src.parse_global(translation_unit, scope_link);
            translation_unit.append_to_translation_unit(tree);
        }
    }


    void OpenMPProfile::init(DTO& dto)
    {
        AST_t translation_unit = dto["translation_unit"];
        ScopeLink sl = dto["scope_link"];

        // Parse the type of TaskProfileInfo
        Source task_profile_type_src(TaskProfileInfo_str);
        // And declare two variables
        task_profile_type_src
            << "static TaskProfileInfo task_info_0 = {(void*)0, 0, 0};"
            << "static TaskProfileInfo* current_task_info = &task_info_0;"
            ;

        AST_t task_profile_type_ast = task_profile_type_src.parse_global(
                translation_unit, sl);
        translation_unit.prepend_to_translation_unit(task_profile_type_ast);


        // At the moment only connect task
        on_directive_pre["task"].connect(functor(&OpenMPProfile::task_preorder, *this));
        on_directive_post["task"].connect(functor(&OpenMPProfile::task_postorder, *this));

        // taskwait
        on_directive_post["taskwait"].connect(functor(&OpenMPProfile::taskwait_postorder, *this));
    }

    void OpenMPProfile::task_preorder(PragmaCustomConstruct task_construct)
    {
        int& task_id = task_construct.get_data<int>("task_id");
        task_id = _current_task_id++;

        TaskProfileInfo &task_profile_info 
            = task_construct.get_data<TaskProfileInfo>("task_profile");
        memset(&task_profile_info, 0, sizeof(task_profile_info));
    }

    void OpenMPProfile::task_postorder(PragmaCustomConstruct task_construct)
    {
        TaskProfileInfo &task_profile_info 
            = task_construct.get_data<TaskProfileInfo>("task_profile");

        int& task_id = task_construct.get_data<int>("task_id");

        Statement construct_body = task_construct.get_statement();

        profile_task(task_profile_info, task_construct, construct_body, task_id);

        {
            // Declare task info variable
            FunctionDefinition enclosing_function_def = task_construct.get_enclosing_function();
            Source task_info_decl_src;
            task_info_decl_src
                << "static TaskProfileInfo task_info_" << task_id << " = { " 
                << "\"" << task_construct.get_ast().get_file() << "\"" << "," 
                << task_construct.get_ast().get_line() << ", 0 };"
                ;
            AST_t task_info_decl_tree = task_info_decl_src.parse_declaration(
                    enclosing_function_def.get_ast(),
                    enclosing_function_def.get_scope_link());

            task_construct.get_ast().prepend_sibling_function(task_info_decl_tree);
        }

        Source task_counters;

        task_counters
            << "current_task_info->num_invocations++;"
            ;

        ADD_COUNTER_CODE(task_profile_info, num_shared, task_counters);
        ADD_COUNTER_CODE(task_profile_info, num_firstprivate, task_counters);
        ADD_COUNTER_CODE(task_profile_info, sizeof_fp, task_counters);

        if (!task_counters.empty())
        {
            Source replaced_task_body_src;

#define MERGE(_FIELD) \
       "task_info_" << task_id << "." #_FIELD "+=" "local_task_info." #_FIELD ";"
            
            replaced_task_body_src
                << "{"
                <<    "current_task_info->num_childs++;"
                <<    "TaskProfileInfo* previous_task = current_task_info;"
                <<    "TaskProfileInfo local_task_info;"
                <<    "__builtin_memset(&local_task_info, 0, sizeof(local_task_info));"
                <<    "current_task_info = &local_task_info;"
                <<    task_counters
                <<    construct_body.prettyprint()
                <<    "if (local_task_info.num_ops >= " << _task_ops_threshold << ")"
                <<         "local_task_info.times_in_threshold++;"
                
                // Merge everything, since all has been stored in local_task_info
                <<    MERGE(num_invocations)
                <<    MERGE(num_taskwait)
                <<    MERGE(num_shared)
                <<    MERGE(num_firstprivate)
                <<    MERGE(num_global_shared_read)
                <<    MERGE(num_global_shared_write)
                <<    MERGE(num_parent_shared_read)
                <<    MERGE(num_parent_shared_write)
                <<    MERGE(num_potential_shared_read)
                <<    MERGE(num_potential_shared_write)
                <<    MERGE(num_read)
                <<    MERGE(num_write)
                <<    MERGE(sizeof_fp)
                <<    MERGE(num_fp_read)
                <<    MERGE(num_fp_write)
                <<    MERGE(num_ops)
                <<    MERGE(num_childs)
                <<    MERGE(times_in_threshold)
                <<    "current_task_info = previous_task;"
                << "}"
                ;
#undef MERGE

            AST_t replaced_task_body_tree = replaced_task_body_src.parse_statement(
                    construct_body.get_ast(),
                    construct_body.get_scope_link());

            construct_body.get_ast().replace(replaced_task_body_tree);
        }
    }

    void OpenMPProfile::taskwait_postorder(PragmaCustomConstruct taskwait_directive)
    {
        Source taskwait_profiled_src;
        // We are using ({ to avoid recounting ourselves
        taskwait_profiled_src
            << "({"
            << "current_task_info->num_taskwait++;" << "\n"
            << "});"
            << taskwait_directive.prettyprint() << "\n"
            ;

        AST_t taskwait_profiled_tree = taskwait_profiled_src.parse_statement(
                taskwait_directive.get_ast(),
                taskwait_directive.get_scope_link());

        taskwait_directive.get_ast().replace(taskwait_profiled_tree);
    }

    void OpenMPProfile::profile_task(TaskProfileInfo &task_profile_info, 
            PragmaCustomConstruct task_construct,
            Statement construct_body,
            int task_id)
    {
        // This predicate does not walk nested tasks
        ExpressionNotNestedTaskPred expr_pred(task_construct.get_scope_link());
        ObjectList<AST_t> expression_list 
            = construct_body.get_ast().depth_subtrees(expr_pred);

        for(ObjectList<AST_t>::iterator it = expression_list.begin();
                it != expression_list.end();
                it++)
        {
            Expression expr(*it, task_construct.get_scope_link());

            TaskProfileInfo expr_profile_info;
            memset(&expr_profile_info, 0, sizeof(expr_profile_info));

            ObjectList<Symbol> read_set;
            analyze_expression(expr, expr_profile_info, task_construct, 
                    /* written */ 0,
                    read_set);

            Source expr_counters;

            ADD_COUNTER_CODE(expr_profile_info, num_global_shared_read, expr_counters);
            ADD_COUNTER_CODE(expr_profile_info, num_global_shared_write, expr_counters);
            ADD_COUNTER_CODE(expr_profile_info, num_parent_shared_read, expr_counters);
            ADD_COUNTER_CODE(expr_profile_info, num_parent_shared_write, expr_counters);
            // These two should be zero, but if they appear we know there's a bug
            ADD_COUNTER_CODE(expr_profile_info, num_potential_shared_read, expr_counters);
            ADD_COUNTER_CODE(expr_profile_info, num_potential_shared_write, expr_counters);
            ADD_COUNTER_CODE(expr_profile_info, num_fp_read, expr_counters);
            ADD_COUNTER_CODE(expr_profile_info, num_fp_write, expr_counters);
            ADD_COUNTER_CODE(expr_profile_info, num_ops, expr_counters);
            ADD_COUNTER_CODE(expr_profile_info, num_read, expr_counters);
            ADD_COUNTER_CODE(expr_profile_info, num_write, expr_counters);

            if (!expr_counters.empty())
            {
                Source profiled_expr_src;

                profiled_expr_src
                    << "({" << expr_counters 
                    << expr.prettyprint() << "; })"
                    ;

                AST_t profiled_expr = profiled_expr_src.parse_expression(expr.get_ast(), expr.get_scope_link());
                expr.get_ast().replace(profiled_expr);
            }
        }
    }

    void OpenMPProfile::analyze_expression(Expression expr,
            TaskProfileInfo &profile_info,
            PragmaCustomConstruct &task_construct,
            bool written,
            ObjectList<Symbol> &read_set)
    {
        OpenMP::DataSharingEnvironment& data_sharing = openmp_info->get_data_sharing(task_construct.get_ast());

        if (expr.is_id_expression())
        {
            IdExpression id_expr = expr.get_id_expression();
            Symbol sym = id_expr.get_symbol();

            if (sym.is_function())
                return;

            if (sym.is_variable()
                    && sym.get_type().is_array()
                    && !sym.is_parameter())
                return;

            OpenMP::DataSharingAttribute da = data_sharing.get_data_sharing(sym);

            if (!written)
            {
                // If this is a read and has been read before, do not count it again
                if(read_set.contains(sym))
                {
                    return;
                }
                else
                {
                    read_set.append(sym);
                }
            }

            if ((da & OpenMP::DS_SHARED) == OpenMP::DS_SHARED)
            {
                if (written)
                {
                    profile_info.num_parent_shared_write++;
                }
                else
                {
                    profile_info.num_parent_shared_read++;
                }
            }
            else if (!sym.has_local_scope())
            {
                // Since global scoped variables do not have shared attribute enabled
                // if we reach here they are always globals
                if (written)
                {
                    profile_info.num_global_shared_write++;
                }
                else
                {
                    profile_info.num_global_shared_read++;
                }
            }
            else if ((da & OpenMP::DS_FIRSTPRIVATE) == OpenMP::DS_FIRSTPRIVATE)
            {
                if (written)
                {
                    profile_info.num_fp_write++;
                }
                else
                {
                    profile_info.num_fp_read++;
                }
            }

            if (written)
            {
                profile_info.num_write++;
            }
            else
            {
                profile_info.num_read++;
            }
        }
        else if (expr.is_array_subscript())
        {
            analyze_expression(expr.get_subscript_expression(),
                    profile_info,
                    task_construct,
                    /* written */ false,
                    read_set);
            analyze_expression(expr.get_subscripted_expression(),
                    profile_info,
                    task_construct,
                    written,
                    read_set);
        }
        else if (expr.is_member_access())
        {
            analyze_expression(expr.get_accessed_entity(),
                    profile_info,
                    task_construct,
                    written,
                    read_set);
        }
        else if (expr.is_pointer_member_access())
        {
            analyze_expression(expr.get_accessed_entity(),
                    profile_info,
                    task_construct,
                    /* read */ false,
                    read_set);

            if (written)
            {
                profile_info.num_potential_shared_write++;
            }
            else
            {
                profile_info.num_potential_shared_read++;
            }
        }
        else if (expr.is_assignment())
        {
            analyze_expression(expr.get_first_operand(), 
                    profile_info, 
                    task_construct, /* written */ true,
                    read_set);
            analyze_expression(expr.get_second_operand(), 
                    profile_info, 
                    task_construct, written,
                    read_set);
        }
        else if (expr.is_operation_assignment())
        {
            // One to read
            analyze_expression(expr.get_first_operand(),
                    profile_info,
                    task_construct,
                    /* written */ false,
                    read_set);
            // One to write
            analyze_expression(expr.get_first_operand(),
                    profile_info,
                    task_construct,
                    /* written */ true,
                    read_set);
            // There is an op
            profile_info.num_ops++;

            // And we read the rhs
            analyze_expression(expr.get_second_operand(),
                    profile_info,
                    task_construct,
                    /* written */ false,
                    read_set);
        }
        else if (expr.is_binary_operation())
        {
            profile_info.num_ops++;
            analyze_expression(expr.get_first_operand(), 
                    profile_info, 
                    task_construct, written,
                    read_set);
            analyze_expression(expr.get_second_operand(), 
                    profile_info, 
                    task_construct, written,
                    read_set);
        }
        else if (expr.is_unary_operation())
        {
            Expression::OperationKind kind = expr.get_operation_kind();

            switch ((int)kind)
            {
                case Expression::PREINCREMENT:
                case Expression::PREDECREMENT:
                case Expression::POSTINCREMENT:
                case Expression::POSTDECREMENT:
                {
                    profile_info.num_ops++;
                    // We will count it twice, one to read and another to write
                    analyze_expression(expr.get_unary_operand(),
                            profile_info,
                            task_construct,
                            /* read count */ false,
                            read_set);
                    analyze_expression(expr.get_unary_operand(),
                            profile_info,
                            task_construct,
                            /* write count */ true,
                            read_set);
                    break;
                }
                case Expression::MINUS :
                case Expression::BITWISE_NOT :
                case Expression::LOGICAL_NOT :
                {
                    profile_info.num_ops++;
                    analyze_expression(expr.get_unary_operand(),
                            profile_info,
                            task_construct,
                            written,
                            read_set);
                    break;
                }
                case Expression::DERREFERENCE :
                {
                    // We need to read the derreferenced expression
                    analyze_expression(expr.get_unary_operand(),
                            profile_info,
                            task_construct,
                            /* read */ false,
                            read_set);

                    // And then we perform an access that will point to something shared
                    if (written)
                    {
                        profile_info.num_potential_shared_write++;
                    }
                    else
                    {
                        profile_info.num_potential_shared_read++;
                    }
                    break;
                }
                default:
                {
                    break;
                }
            }
        }
        else if (expr.is_conditional())
        {
            analyze_expression(expr.get_condition_expression(), 
                    profile_info, 
                    task_construct, /* read */ false,
                    read_set);
            analyze_expression(expr.get_true_expression(), 
                    profile_info, 
                    task_construct, /* read */ false,
                    read_set);
            analyze_expression(expr.get_false_expression(), 
                    profile_info, 
                    task_construct, /* read */ false,
                    read_set);
        }
        else if (expr.is_function_call())
        {
            ObjectList<Expression> arguments = expr.get_argument_list();

            for (ObjectList<Expression>::iterator it = arguments.begin();
                    it != arguments.end();
                    it++)
            {
                analyze_expression(*it,
                        profile_info,
                        task_construct,
                        /* read */ false,
                        read_set);
            }

            Expression called = expr.get_called_expression();

#if 0
            if (called.is_id_expression())
            {
                IdExpression called_id = called.get_id_expression();
                Symbol sym = called_id.get_symbol();
                
                if (sym.is_function()
                        && sym.is_defined())
                {
                    ClosureInfo closure_info = fill_closure_info(sym, arguments, task_construct);
                    std::string name_profiled_function = 
                        perform_closure(task_construct.get_scope_link(), sym, closure_info);

                    Source src;
                    src << name_profiled_function;

                    AST_t tree = src.parse_expression(called_id.get_ast(),
                            called_id.get_scope_link());

                    called_id.get_ast().replace(tree);
                }
                else
                {
                    // This is a bit crude, and it won't work in C++
                    std::string fun_name = called_id.prettyprint();

                    bool already_handled = false;
                    if (fun_name == "memcpy"
                             || fun_name == "__builtin_memcpy")
                    {
                        // Special case for memcpy, we check that a call
                        // memcpy(d, s, size)
                        // has 'd' and 's' of type pointer and 'd' and 's' of the form
                        // 'p' or '&p' and that 'p' is private or firstprivate for 'd' 
                        // and 'p' is shared or firstprivate for 's'
                        if (arguments.size() == 3)
                        {
                            Expression &dest = arguments[0];
                            Expression &orig = arguments[1];
                            Expression &size = arguments[2];

                            if ((dest.get_type().is_pointer() 
                                        || dest.get_type().is_array())
                                    // Dest is 'p' or '&a'
                                    && (dest.is_id_expression()
                                        || (dest.is_unary_operation() 
                                            && (dest.get_operation_kind() == Expression::REFERENCE)
                                            && dest.get_unary_operand().is_id_expression()))
                                    && (orig.get_type().is_pointer() 
                                        || orig.get_type().is_array())
                                    // orig is 'p' or '&a'
                                    && (orig.is_id_expression()
                                        || (orig.is_unary_operation() 
                                            && (orig.get_operation_kind() == Expression::REFERENCE)
                                            && orig.get_unary_operand().is_id_expression())))
                            {
                                Symbol orig_sym(NULL);
                                if (orig.is_id_expression())
                                {
                                    orig_sym = orig.get_id_expression().get_symbol();
                                }
                                else
                                {
                                    orig_sym = orig.get_unary_operand().get_id_expression().get_symbol();
                                }
                                Symbol dest_sym(NULL);
                                if (dest.is_id_expression())
                                {
                                    dest_sym = dest.get_id_expression().get_symbol();
                                }
                                else
                                {
                                    dest_sym = dest.get_unary_operand().get_id_expression().get_symbol();
                                }

                                if (orig_sym.is_valid()
                                        && dest_sym.is_valid())
                                {
                                    OpenMP::DataSharingAttribute da_orig = data_sharing.get_data_sharing(orig_sym);
                                    OpenMP::DataSharingAttribute da_dest = data_sharing.get_data_sharing(dest_sym);
                                    
                                    if (
                                            (((da_orig & OpenMP::DS_SHARED) == OpenMP::DS_SHARED)
                                             || ((da_orig & OpenMP::DS_FIRSTPRIVATE) == OpenMP::DS_FIRSTPRIVATE))
                                            && 
                                            (((da_dest & OpenMP::DS_PRIVATE) == OpenMP::DS_PRIVATE)
                                             // Local variables are given undefined data attribute (we should fix this one day)
                                             || ((da_dest & OpenMP::DS_UNDEFINED) == OpenMP::DS_UNDEFINED)
                                             || ((da_dest & OpenMP::DS_FIRSTPRIVATE) == OpenMP::DS_FIRSTPRIVATE))
                                       )
                                    {
                                        // task_profile_info.sizeof_fp += expr.p
                                        already_handled = true;

                                        std::cerr << expr.get_ast().get_locus_str() << ": note: invocation of memcpy has been accounted" << std::endl;

                                        // We have to fix the code since the value of the size will be known
                                        // only at runtime, this one will be replaced twice
                                        Source new_code;
                                        new_code 
                                            << "({"
                                            << "current_task_info->sizeof_fp += (" << size.prettyprint() << ");"
                                            << expr.prettyprint() << ";"
                                            << "})"
                                            ;

                                        AST_t new_code_tree = new_code.parse_expression(expr.get_ast(),
                                                expr.get_scope_link());

                                        expr.get_ast().replace(new_code_tree);
                                    }
                                }
                            }
                        }
                    }

                    if (!already_handled)
                    {
                        if (_cost_map.find(fun_name) != _cost_map.end())
                        {
                            profile_info.num_ops += _cost_map[fun_name];
                        }
                        else
                        {
                            std::cerr << expr.get_ast().get_locus_str() << ": warning: call to '" 
                                << called_id.prettyprint() << "' cannot be profiled since no definition is available" << std::endl;
                            info_funops_param(expr);
                        }
                    }
                }
            }
            else
            {
                std::cerr << expr.get_ast().get_locus_str() << ": warning: indirect call '" 
                    << called.prettyprint() << "' cannot be profiled" << std::endl;
            }
#endif
        }
        else if (expr.is_casting())
        {
            analyze_expression(expr.get_casted_expression(),
                    profile_info,
                    task_construct,
                    written,
                    read_set);
        }
    }

    void OpenMPProfile::info_funops_param(Expression expr)
    {
        static bool given_message = false;
        if (!given_message)
        {
            std::cerr << expr.get_ast().get_locus_str() 
                << ": note: parameter --variable=fun_ops can be used to give a cost for these functions"
                << std::endl;
            given_message = true;
        }
    }
}

EXPORT_PHASE(TL::OpenMPProfile);
