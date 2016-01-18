/*--------------------------------------------------------------------
  (C) Copyright 2015-2015 Barcelona Supercomputing Center
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


#include "tl-nanos6-lower.hpp"
#include "tl-nanos6-task-properties.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-symbol-utils.hpp"
#include "tl-counters.hpp"
#include "cxx-exprtype.h"
#include "cxx-diagnostic.h"
#include <map>

namespace TL { namespace Nanos6 {

    namespace {

    struct TaskCallRewriterExpr : Nodecl::ExhaustiveVisitor<void>
        {
            private:
                const TL::ObjectList<TL::Symbol>& _argument_captures_syms;

                TL::Symbol replace_symbol(TL::Symbol s)
                {
                    if (!s.is_valid())
                        return s;

                    if (s.is_parameter()
                            && s.get_parameter_position() < (int)_argument_captures_syms.size())
                    {
                        s = _argument_captures_syms[s.get_parameter_position()];
                    }

                    return s;
                }


            public:
              void visit(const Nodecl::OpenMP::Shared &n)
                {
                    Nodecl::List sym_list = n.as<Nodecl::OpenMP::Shared>()
                                                .get_symbols()
                                                .as<Nodecl::List>();
                    TL::ObjectList<Nodecl::NodeclBase> pruned_list;
                    TL::ObjectList<Nodecl::NodeclBase> captured_arguments;
                    for (Nodecl::List::iterator it = sym_list.begin();
                         it != sym_list.end();
                         it++)
                    {
                        TL::Symbol sym = it->get_symbol();
                        if (!sym.is_parameter()
                            || sym.get_parameter_position()
                                   >= (int)_argument_captures_syms.size())
                        {
                            pruned_list.append(sym.make_nodecl());
                        }
                        else
                        {
                            // In Nanos6 we have to capture the arguments of the
                            // task call
                            captured_arguments.append(sym.make_nodecl());
                        }
                    }
                    if (!captured_arguments.empty())
                    {
                        // Capture arguments
                        Nodecl::NodeclBase captured_arg_list
                            = Nodecl::OpenMP::Firstprivate::make(
                                Nodecl::List::make(captured_arguments),
                                n.get_locus());
                        walk(captured_arg_list);
                        n.prepend_sibling(captured_arg_list);
                    }
                    if (pruned_list.empty())
                    {
                        Nodecl::Utils::remove_from_enclosing_list(n);
                    }
                    else
                    {
                        // Note: this is unlikely
                        n.replace(Nodecl::OpenMP::Shared::make(
                            Nodecl::List::make(pruned_list), n.get_locus()));
                    }
                }

                void visit(const Nodecl::Symbol &n)
                {
                    TL::Symbol s = n.get_symbol();
                    if (!s.is_saved_expression())
                    {
                        TL::Symbol repl_s = replace_symbol(s);
                        if (repl_s != s)
                        {
                            n.replace(repl_s.make_nodecl(
                                /* set_ref_type */ true, n.get_locus()));
                        }
                    }
                    else
                    {
                        Nodecl::NodeclBase new_value = s.get_value().shallow_copy();
                        walk(new_value);
                        n.replace(new_value);
                    }
                }

                TaskCallRewriterExpr(
                    const TL::ObjectList<TL::Symbol> &argument_captures_syms)
                    : _argument_captures_syms(argument_captures_syms)
                {
                }
        };


        struct TaskCallRewriterType
        {
          private:
            TaskCallRewriterExpr &_r;

          public:
            TL::Type rewrite_type(TL::Type t)
            {
                if (!t.is_valid())
                    return t;

                if (t.is_pointer())
                {
                    return rewrite_type(t.points_to())
                        .get_pointer_to()
                        .get_as_qualified_as(t);
                    }
                    else if (t.is_array())
                    {
                        if (t.array_is_region())
                        {
                            Nodecl::NodeclBase lower_bound, upper_bound;
                            t.array_get_bounds(lower_bound, upper_bound);
                            lower_bound = lower_bound.shallow_copy();
                            upper_bound = upper_bound.shallow_copy();

                            _r.walk(lower_bound);
                            _r.walk(upper_bound);

                            Nodecl::NodeclBase lower_region_bound,
                                upper_region_bound;
                            t.array_get_region_bounds(lower_region_bound,
                                                      upper_region_bound);
                            lower_region_bound
                                = lower_region_bound.shallow_copy();
                            upper_region_bound
                                = upper_region_bound.shallow_copy();

                            _r.walk(lower_region_bound);
                            _r.walk(upper_region_bound);

                            return rewrite_type(t.array_element())
                                .get_array_to_with_region(
                                     lower_bound,
                                     upper_bound,
                                     lower_region_bound,
                                     upper_region_bound,
                                     TL::Scope::get_global_scope())
                                .get_as_qualified_as(t);
                        }
                        else
                        {
                            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                            {
                                Nodecl::NodeclBase s
                                    = t.array_get_size().shallow_copy();
                                _r.walk(s);

                                return rewrite_type(t.array_element())
                                    .get_array_to(s,
                                                  TL::Scope::get_global_scope())
                                    .get_as_qualified_as(t);
                            }
                            else if (IS_FORTRAN_LANGUAGE)
                            {
                                Nodecl::NodeclBase lower_bound, upper_bound;
                                t.array_get_bounds(lower_bound, upper_bound);
                                lower_bound = lower_bound.shallow_copy();
                                upper_bound = upper_bound.shallow_copy();

                                _r.walk(lower_bound);
                                _r.walk(upper_bound);

                                return rewrite_type(t.array_element())
                                    .get_array_to(lower_bound,
                                                  upper_bound,
                                                  TL::Scope::get_global_scope())
                                    .get_as_qualified_as(t);
                            }
                            else
                            {
                                internal_error("Code unreachable", 0);
                            }
                        }
                    }
                    else if (t.is_lvalue_reference())
                    {
                        return rewrite_type(t.no_ref())
                            .get_lvalue_reference_to();
                    }
                    else if (t.is_rvalue_reference())
                    {
                        return rewrite_type(t.no_ref())
                            .get_rvalue_reference_to();
                    }
                    else if (t.is_vector())
                    {
                        return rewrite_type(t.vector_element())
                            .get_vector_of_elements(t.vector_num_elements())
                            .get_as_qualified_as(t);
                    }

                    return t;
            }

            void rewrite_types(Nodecl::NodeclBase n)
            {
                if (n.is_null())
                    return;

                if (n.is<Nodecl::List>())
                {
                    Nodecl::List l = n.as<Nodecl::List>();
                    for (Nodecl::List::iterator it = l.begin(); it != l.end();
                         it++)
                        {
                            rewrite_types(*it);
                        }
                    }
                    else
                    {
                        n.set_type(rewrite_type(n.get_type()));

                        Nodecl::NodeclBase::Children c = n.children();
                        for (Nodecl::NodeclBase::Children::size_type i = 0;
                             i < c.size();
                             i++)
                        {
                            rewrite_types(c[i]);
                        }
                    }
                }

                TaskCallRewriterType(TaskCallRewriterExpr &r) : _r(r)
                {
                }
        };

        struct TaskCallRewriter
        {
          private:
            const TL::ObjectList<TL::Symbol> &_argument_captures_syms;

          public:
            TaskCallRewriter(
                const TL::ObjectList<TL::Symbol> &argument_captures_syms)
                : _argument_captures_syms(argument_captures_syms)
            {
            }

            void walk(Nodecl::NodeclBase n)
            {
                TaskCallRewriterExpr rewriter_expr(
                    _argument_captures_syms);
                rewriter_expr.walk(n);

                TaskCallRewriterType rewriter_types(rewriter_expr);
                rewriter_types.rewrite_types(n);
            }
        };

        Nodecl::NodeclBase rewrite_task_call_environment(
            Nodecl::NodeclBase parameters_environment,
            const TL::ObjectList<TL::Symbol> &argument_captures_syms)
        {
            Nodecl::NodeclBase result = parameters_environment.shallow_copy();

            TaskCallRewriter task_call_rewriter(
                argument_captures_syms);
            task_call_rewriter.walk(result);

            return result;
        }

        TL::Type rewrite_type(
            TL::Type t,
            const TL::ObjectList<TL::Symbol> &argument_captures_syms)
        {
            TaskCallRewriterExpr rewriter_expr(
                argument_captures_syms);

            TaskCallRewriterType rewriter_types(rewriter_expr);
            TL::Type res = rewriter_types.rewrite_type(t);

            return res;
        }
    }

    void Lower::visit_task_call_c(const Nodecl::OmpSs::TaskCall& construct)
    {
        Nodecl::FunctionCall function_call = construct.get_call().as<Nodecl::FunctionCall>();
        ERROR_CONDITION(!function_call.get_called().is<Nodecl::Symbol>(),
                        "Invalid TaskCall",
                        0);

        TL::Symbol called_sym = function_call.get_called().get_symbol();

        Nodecl::NodeclBase parameters_environment = construct.get_environment();

        Nodecl::OpenMP::FunctionTaskParsingContext function_parsing_context
            = parameters_environment.as<Nodecl::List>()
            .find_first<Nodecl::OpenMP::FunctionTaskParsingContext>();
        ERROR_CONDITION(function_parsing_context.is_null(), "Invalid node", 0);

        info_printf_at(construct.get_locus(),
                "call to task function '%s'\n",
                called_sym.get_qualified_name().c_str());
        info_printf_at(function_parsing_context.get_locus(),
                "task function declared here\n");

        Scope sc = construct.retrieve_context();
        Scope new_block_context_sc = new_block_context(sc.get_decl_context());

        TaskProperties parameter_task_properties = TaskProperties::gather_task_properties(_phase, construct);

        Nodecl::List arguments = function_call.get_arguments().as<Nodecl::List>();

        TL::ObjectList<TL::Symbol> captured_arguments;

        TL::ObjectList<TL::Symbol> parameters = called_sym.get_related_symbols();

        Nodecl::List new_args, argument_captures;

        TL::ObjectList<TL::Symbol> argument_captures_syms;

        TL::ObjectList<TL::Symbol>::iterator it_params = parameters.begin();
        Nodecl::List::iterator it_args = arguments.begin();
        for (;
                it_params != parameters.end() && it_args != arguments.end();
                it_params++, it_args++)
        {
            // Capture the value of the argument
            std::string symbol_name;
            {
                TL::Counter& arg_counter = CounterManager::get_counter("nanos6-task-call-args");
                // Create a new variable holding the value of the argument
                std::stringstream ss;
                ss << "nanos_task_call_arg_" << (int)arg_counter;
                arg_counter++;
                symbol_name = ss.str();
            }

            TL::Symbol new_symbol = new_block_context_sc.new_symbol(symbol_name);

            new_symbol.get_internal_symbol()->kind = SK_VARIABLE;
            new_symbol.get_internal_symbol()->type_information
                = rewrite_type(it_params->get_type(),
                               argument_captures_syms
                               ).get_internal_type();
            symbol_entity_specs_set_is_user_declared(new_symbol.get_internal_symbol(), 1);

            new_symbol.get_internal_symbol()->value = it_args->shallow_copy().get_internal_nodecl();

            Nodecl::NodeclBase symbol_ref = new_symbol.make_nodecl(
                    /* set_ref_type */ true,
                    it_args->get_locus());
            Nodecl::NodeclBase new_arg = ::cxx_nodecl_make_conversion(symbol_ref.get_internal_nodecl(),
                    it_params->get_type().get_internal_type(),
                    TL::Scope::get_global_scope().get_decl_context(),
                    symbol_ref.get_locus());
            new_args.append(new_arg);

            if (IS_CXX_LANGUAGE)
            {
                argument_captures.append(
                        Nodecl::CxxDef::make(
                            Nodecl::NodeclBase::null(),
                            new_symbol));
            }
            argument_captures.append(
                    Nodecl::ObjectInit::make(new_symbol)
                    );

            argument_captures_syms.append(new_symbol);
        }

        Nodecl::List new_task_statements;
        new_task_statements.append(
                Nodecl::ExpressionStatement::make(
                    Nodecl::FunctionCall::make(
                        called_sym.make_nodecl(/* set_ref_type */ true),
                        new_args,
                        /* alternate-symbol */ Nodecl::NodeclBase::null(),
                        /* function-form */ Nodecl::NodeclBase::null(),
                        called_sym.get_type().returns(),
                        construct.get_locus()
                        ),
                    construct.get_locus())
                );

        Scope new_task_block_context_sc =
            new_block_context(new_block_context_sc.get_decl_context());

        Nodecl::NodeclBase new_task_body =
            Nodecl::List::make(
                    Nodecl::Context::make(
                        Nodecl::List::make(
                            Nodecl::CompoundStatement::make(
                                new_task_statements,
                                Nodecl::NodeclBase::null(),
                                construct.get_locus())
                            ),
                        new_task_block_context_sc,
                        construct.get_locus())
                    );

        Nodecl::NodeclBase new_omp_exec_environment
            = rewrite_task_call_environment(
                parameters_environment, argument_captures_syms);

        Nodecl::NodeclBase new_task_construct =
            Nodecl::OpenMP::Task::make(
                    new_omp_exec_environment,
                    new_task_body,
                    construct.get_locus());

        Nodecl::List new_statements;
        new_statements.append(argument_captures);
        new_statements.append(new_task_construct);

        Nodecl::NodeclBase new_compound_stmt =
            Nodecl::Context::make(
                    Nodecl::List::make(
                        Nodecl::CompoundStatement::make(
                            new_statements,
                            /* finally */ Nodecl::NodeclBase::null(),
                            construct.get_locus()
                            )
                        ),
                    new_block_context_sc,
                    construct.get_locus()
                    );

        Nodecl::NodeclBase parent = construct.get_parent();
        ERROR_CONDITION(!parent.is<Nodecl::ExpressionStatement>(),
                "Invalid parent", 0);
        parent.replace(new_compound_stmt);

        // Now follow the usual path
        this->walk(parent);
    }

    void Lower::visit_task_call_fortran(const Nodecl::OmpSs::TaskCall& construct)
    {
        TL::Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);

        Nodecl::FunctionCall function_call = construct.get_call().as<Nodecl::FunctionCall>();
        Nodecl::NodeclBase parameters_environment = construct.get_environment();

        ERROR_CONDITION(!function_call.get_called().is<Nodecl::Symbol>(),
                        "Invalid TaskCall",
                        0);

        // For Fortran we create an adaptor function because otherwise it is
        // impossible to represent some arguments as temporaries. The adaptor
        // function has the same type as the function task. We create it as a sibling
        // of the current function. Note that we rely on Fortran codegen to add the
        // required USEs in this function

        TL::Symbol called_sym = function_call.get_called().get_symbol();
        Nodecl::NodeclBase orig_arguments = function_call.get_arguments();

        std::string adapter_name;
        {
            TL::Counter& counter = CounterManager::get_counter("nanos6-fortran-adapter");
            std::stringstream ss;
            ss << enclosing_function.get_name() << "_adapter_" << (int)counter;
            counter++;
            adapter_name = ss.str();
        }

        TL::ObjectList<TL::Symbol> orig_parameter_symbols = called_sym.get_related_symbols();
        TL::ObjectList<std::string> parameter_names =
            orig_parameter_symbols.map<std::string>(&TL::Symbol::get_name);
        TL::ObjectList<TL::Type> parameter_types = called_sym.get_type().parameters();

        TL::Symbol adapter_function = SymbolUtils::new_function_symbol(
                enclosing_function,
                adapter_name,
                TL::Type::get_void_type(),
                parameter_names,
                parameter_types);


        Nodecl::NodeclBase empty_stmt, adapter_function_code;
        SymbolUtils::build_empty_body_for_function(
                adapter_function,
                adapter_function_code,
                empty_stmt);
        Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, adapter_function_code);

        TL::ObjectList<TL::Symbol> new_parameter_symbols = adapter_function.get_related_symbols();

        Nodecl::Utils::SimpleSymbolMap parameter_symbol_map;
        // Map original parameters to new symbols
        {
            for (TL::ObjectList<TL::Symbol>::iterator
                    it_orig = orig_parameter_symbols.begin(),
                    it_new = new_parameter_symbols.begin();

                    it_orig != orig_parameter_symbols.end()
                    && it_new != new_parameter_symbols.end();

                    it_orig++, it_new++)
            {
                parameter_symbol_map.add_map(*it_orig, *it_new);
            }
        }

        // Build body
        struct SymbolToArgument
        {
            static Nodecl::NodeclBase run(TL::Symbol sym)
            {
                return sym.make_nodecl(/* set_ref */ true);
            }
        };

        TL::Scope scope_inside_new_function = empty_stmt.retrieve_context();
        Scope new_task_block_context_sc =
            new_block_context(scope_inside_new_function.get_decl_context());

        TL::ObjectList<Nodecl::NodeclBase> new_arguments
            = new_parameter_symbols.map<Nodecl::NodeclBase>(SymbolToArgument::run)
            ;

        Nodecl::List new_task_statements;
        new_task_statements.append(
                Nodecl::ExpressionStatement::make(
                    Nodecl::FunctionCall::make(
                        called_sym.make_nodecl(/* set_ref */ true),
                        Nodecl::List::make(new_arguments),
                        /* alternate */ Nodecl::NodeclBase::null(),
                        /* function-form */ Nodecl::NodeclBase::null(),
                        called_sym.get_type().returns(),
                        construct.get_locus()),
                    construct.get_locus()));

        Nodecl::NodeclBase new_task_body =
            Nodecl::List::make(
                    Nodecl::Context::make(
                        Nodecl::List::make(
                            Nodecl::CompoundStatement::make(
                                new_task_statements,
                                Nodecl::NodeclBase::null(),
                                construct.get_locus())
                            ),
                        new_task_block_context_sc,
                        construct.get_locus())
                    );

        Nodecl::NodeclBase new_omp_exec_environment;
        new_omp_exec_environment = Nodecl::Utils::deep_copy(
                parameters_environment,
                scope_inside_new_function,
                parameter_symbol_map);

        Nodecl::NodeclBase new_task_construct =
            Nodecl::OpenMP::Task::make(
                    new_omp_exec_environment,
                    new_task_body,
                    construct.get_locus());
        empty_stmt.replace(new_task_construct);

        // Now follow the usual path
        this->walk(empty_stmt);

        // Replace the call site
        construct.replace(
                    Nodecl::FunctionCall::make(
                        adapter_function.make_nodecl(/* set_ref */ true),
                        orig_arguments.shallow_copy(),
                        /* alternate */ Nodecl::NodeclBase::null(),
                        /* function-form */ Nodecl::NodeclBase::null(),
                        called_sym.get_type().returns(),
                        construct.get_locus()));
    }

    void Lower::visit_task_call(const Nodecl::OmpSs::TaskCall& construct)
    {
        if (IS_C_LANGUAGE
                || IS_CXX_LANGUAGE)
        {
            visit_task_call_c(construct);
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            visit_task_call_fortran(construct);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    void Lower::visit(const Nodecl::OmpSs::TaskCall& node)
    {
        visit_task_call(node);
    }

} }
