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
#include "tl-nanos6-support.hpp"
#include "tl-nanos6-fortran-support.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-nodecl-utils-fortran.hpp"
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
              TL::Symbol _called_sym;
                const TL::ObjectList<TL::Symbol>& _argument_captures_syms;

                TL::Symbol replace_symbol(TL::Symbol s)
                {
                    if (!s.is_valid())
                        return s;

                    if (IS_CXX_LANGUAGE && _called_sym.is_member()
                        && !_called_sym.is_static())
                    {
                        if (s.get_name() == "this")
                        {
                            s = _argument_captures_syms[0];
                        }
                        else if (s.is_parameter()
                                 && (s.get_parameter_position() + 1)
                                        < (int)_argument_captures_syms.size())
                        {
                            s = _argument_captures_syms
                                [s.get_parameter_position() + 1];
                        }
                    }
                    else
                    {
                        if (s.is_parameter()
                            && s.get_parameter_position()
                                   < (int)_argument_captures_syms.size())
                        {
                            s = _argument_captures_syms
                                [s.get_parameter_position()];
                        }
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
                        if (IS_CXX_LANGUAGE && sym.get_name() == "this")
                        {
                            // Skip 'this'
                        }
                        else if (!sym.is_parameter()
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

                void visit(const Nodecl::Dereference &n)
                {
                    walk(n.get_rhs());
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
                    TL::Symbol called_sym,
                    const TL::ObjectList<TL::Symbol> &argument_captures_syms)
                    : _called_sym(called_sym),
                      _argument_captures_syms(argument_captures_syms)
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
            TL::Symbol _called_sym;
            const TL::ObjectList<TL::Symbol> &_argument_captures_syms;

          public:
            TaskCallRewriter(
                TL::Symbol called_sym,
                const TL::ObjectList<TL::Symbol> &argument_captures_syms)
                : _called_sym(called_sym),
                  _argument_captures_syms(argument_captures_syms)
            {
            }

            void walk(Nodecl::NodeclBase n)
            {
                TaskCallRewriterExpr rewriter_expr(_called_sym,
                                                   _argument_captures_syms);
                rewriter_expr.walk(n);

                TaskCallRewriterType rewriter_types(rewriter_expr);
                rewriter_types.rewrite_types(n);
            }
        };

        Nodecl::NodeclBase rewrite_task_call_environment(
            TL::Symbol called_sym,
            Nodecl::NodeclBase parameters_environment,
            const TL::ObjectList<TL::Symbol> &argument_captures_syms)
        {
            Nodecl::NodeclBase result = parameters_environment.shallow_copy();

            TaskCallRewriter task_call_rewriter(called_sym,
                                                argument_captures_syms);
            task_call_rewriter.walk(result);

            if (called_sym.is_member() && !called_sym.is_static())
            {
                // Capture the address of this
                result.as<Nodecl::List>().append(
                    Nodecl::OpenMP::Firstprivate::make(Nodecl::List::make(
                        Nodecl::Symbol::make(argument_captures_syms[0]))));
            }

            return result;
        }

        TL::Type rewrite_type(
            TL::Symbol called_sym,
            TL::Type t,
            const TL::ObjectList<TL::Symbol> &argument_captures_syms)
        {
            TaskCallRewriterExpr rewriter_expr(called_sym,
                                               argument_captures_syms);

            TaskCallRewriterType rewriter_types(rewriter_expr);
            TL::Type res = rewriter_types.rewrite_type(t);

            return res;
        }
    }

    void Lower::capture_argument_for_task_call(
        TL::Symbol called_sym,
        TL::Scope new_block_context_sc,
        TL::Type parameter_type,
        Nodecl::NodeclBase argument,
        /* out */ TL::ObjectList<TL::Symbol> &argument_captures_syms,
        /* out */ Nodecl::List &new_args,
        /* out */ Nodecl::List &argument_captures)
    {
        // Capture the value of the argument
        std::string symbol_name;
        {
            TL::Counter &arg_counter
                = CounterManager::get_counter("nanos6-task-call-args");
            // Create a new variable holding the value of the argument
            std::stringstream ss;
            ss << "nanos_task_call_arg_" << (int)arg_counter;
            arg_counter++;
            symbol_name = ss.str();
        }

        TL::Symbol new_symbol = new_block_context_sc.new_symbol(symbol_name);

        new_symbol.get_internal_symbol()->kind = SK_VARIABLE;
        new_symbol.get_internal_symbol()->type_information
            = rewrite_type(called_sym, parameter_type, argument_captures_syms)
                  .get_internal_type();

        bool param_type_is_ref = new_symbol.get_type().is_any_reference();
        if (param_type_is_ref)
        {
            // If the parameter type, we capture the address of the argument
            // here. FIXME: this does not work for all arguments valid for const
            // T& parameters
            new_symbol.set_type(
                new_symbol.get_type().no_ref().get_pointer_to());
        }
        symbol_entity_specs_set_is_user_declared(
            new_symbol.get_internal_symbol(), 1);

        new_symbol.set_value(argument.shallow_copy().get_internal_nodecl());
        if (param_type_is_ref)
        {
            new_symbol.set_value(Nodecl::Reference::make(new_symbol.get_value(),
                                                         new_symbol.get_type(),
                                                         argument.get_locus()));
        }

        Nodecl::NodeclBase symbol_ref = new_symbol.make_nodecl(
            /* set_ref_type */ true, argument.get_locus());
        Nodecl::NodeclBase new_arg = symbol_ref;
        if (param_type_is_ref)
            new_arg = Nodecl::Dereference::make(
                symbol_ref, parameter_type, argument.get_locus());

        new_arg = ::cxx_nodecl_make_conversion(
            new_arg.get_internal_nodecl(),
            parameter_type.get_internal_type(),
            TL::Scope::get_global_scope().get_decl_context(),
            symbol_ref.get_locus());

        new_args.append(new_arg);

        if (IS_CXX_LANGUAGE)
        {
            argument_captures.append(
                Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), new_symbol));
        }
        argument_captures.append(Nodecl::ObjectInit::make(new_symbol));

        argument_captures_syms.append(new_symbol);
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

        if (called_sym.is_member() && !called_sym.is_static())
        {
            TL::Type implicit_parameter_type = called_sym.get_class_type();
            if (called_sym.get_type().is_const())
            {
                implicit_parameter_type
                    = implicit_parameter_type.get_const_type();
            }
            implicit_parameter_type
                = implicit_parameter_type.get_lvalue_reference_to();

            Nodecl::NodeclBase implicit_captured_arg = it_args->shallow_copy();

            capture_argument_for_task_call(called_sym,
                                           new_block_context_sc,
                                           implicit_parameter_type,
                                           implicit_captured_arg,
                                           /* out */ argument_captures_syms,
                                           /* out */ new_args,
                                           /* out */ argument_captures);
            it_args++;
        }

        for (; it_params != parameters.end() && it_args != arguments.end();
             it_params++, it_args++)
        {
            capture_argument_for_task_call(called_sym,
                                           new_block_context_sc,
                                           it_params->get_type(),
                                           *it_args,
                                           /* out */ argument_captures_syms,
                                           /* out */ new_args,
                                           /* out */ argument_captures);
        }

        Nodecl::List new_task_statements;
        new_task_statements.append(Nodecl::ExpressionStatement::make(
            Nodecl::FunctionCall::make(
                called_sym.make_nodecl(/* set_ref_type */ true),
                new_args,
                /* alternate-symbol */ Nodecl::NodeclBase::null(),
                function_call.get_function_form().shallow_copy(),
                called_sym.get_type().returns(),
                construct.get_locus()),
            construct.get_locus()));

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
                called_sym, parameters_environment, argument_captures_syms);

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

        if (called_sym.is_nested_function())
        {
            error_printf_at(construct.get_locus(),
                    "calls to function tasks of internal subprograms are not supported in Nanos 6\n");
            return;
        }

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
        TL::ObjectList<TL::Type> parameter_types
            = orig_parameter_symbols.map<TL::Type>(&TL::Symbol::get_type);

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

        TL::Symbol current_function = construct.retrieve_context().get_related_symbol();
        TL::Scope scope_inside_new_function = empty_stmt.retrieve_context();

        Nodecl::Utils::Fortran::ExtraDeclsVisitor fun_visitor(
                parameter_symbol_map,
                scope_inside_new_function,
                current_function);
        fun_visitor.insert_extra_symbols(function_call);
        // Map the called symbol (if needed)
        // We do it early because we do not want to map the parameters
        // of the called symbol to the arguments of the adapter
        called_sym = parameter_symbol_map.map(called_sym);

        // Map original parameters to new symbols
        {
            for (TL::ObjectList<TL::Symbol>::iterator it_orig
                 = orig_parameter_symbols.begin(),
                 it_new = new_parameter_symbols.begin();

                 it_orig != orig_parameter_symbols.end()
                     && it_new != new_parameter_symbols.end();

                 it_orig++, it_new++)
            {
                parameter_symbol_map.add_map(*it_orig, *it_new);

                // Propagate OPTIONAL attribute
                symbol_entity_specs_set_is_optional(
                    it_new->get_internal_symbol(), it_orig->is_optional());

                // Propagate ALLOCATABLE attribute
                symbol_entity_specs_set_is_allocatable(
                    it_new->get_internal_symbol(), it_orig->is_allocatable());
            }
        }

        // Add extra mappings for VLAs
        TL::ObjectList<TL::Symbol> new_vlas;
        for (TL::ObjectList<TL::Symbol>::iterator it_new
             = new_parameter_symbols.begin();
             it_new != new_parameter_symbols.end();
             it_new++)
        {
            add_extra_mappings_for_vla_types(it_new->get_type(),
                                             scope_inside_new_function,
                                             /* out */
                                             parameter_symbol_map,
                                             new_vlas);
        }

        // Now rewrite types
        for (TL::ObjectList<TL::Symbol>::iterator it_new
             = new_parameter_symbols.begin();
             it_new != new_parameter_symbols.end();
             it_new++)
        {
            it_new->set_type(
                ::type_deep_copy(it_new->get_type().get_internal_type(),
                                 scope_inside_new_function.get_decl_context(),
                                 parameter_symbol_map.get_symbol_map()));
        }

        // Build body
        struct SymbolToArgument
        {
            static Nodecl::NodeclBase run(TL::Symbol sym)
            {
                return sym.make_nodecl(/* set_ref */ true);
            }
        };

        Scope new_task_block_context_sc =
            new_block_context(scope_inside_new_function.get_decl_context());

        TL::ObjectList<Nodecl::NodeclBase> new_arguments
            = new_parameter_symbols.map<Nodecl::NodeclBase>(SymbolToArgument::run)
            ;

        TL::Nanos6::fortran_add_types(new_parameter_symbols,
                                      scope_inside_new_function);

        Nodecl::Utils::Fortran::append_used_modules(
            current_function.get_related_scope(),
            adapter_function.get_related_scope());

        Nodecl::Utils::Fortran::append_used_modules(
            called_sym.get_related_scope(),
            adapter_function.get_related_scope());

        // If the current function is in a module, make this new function a
        // sibling of it
        if (current_function.is_in_module()
            && current_function.is_module_procedure())
        {
            symbol_entity_specs_set_in_module(
                adapter_function.get_internal_symbol(),
                current_function.in_module().get_internal_symbol());
            symbol_entity_specs_set_access(
                adapter_function.get_internal_symbol(), AS_PRIVATE);
            symbol_entity_specs_set_is_module_procedure(
                adapter_function.get_internal_symbol(), 1);

            symbol_entity_specs_add_related_symbols(
                symbol_entity_specs_get_in_module(
                    adapter_function.get_internal_symbol()),
                adapter_function.get_internal_symbol());
        }

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

        for (TL::ObjectList<TL::Symbol>::iterator it = new_vlas.begin();
             it != new_vlas.end();
             it++)
        {
            empty_stmt.prepend_sibling(Nodecl::ObjectInit::make(*it));
        }

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
        Nodecl::FunctionCall function_call
            = construct.get_call().as<Nodecl::FunctionCall>();
        ERROR_CONDITION(!function_call.get_called().is<Nodecl::Symbol>(),
                        "Invalid TaskCall",
                        0);
        TL::Symbol called_sym = function_call.get_called().get_symbol();

        if (!called_sym.get_type().returns().is_void())
        {
            error_printf_at(
                function_call.get_locus(),
                "non-void task functions are not yet supported in Nanos 6\n");
            return;
        }

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
