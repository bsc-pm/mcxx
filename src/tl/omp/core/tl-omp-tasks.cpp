/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#include "tl-omp-core.hpp"
#include "tl-omp-tasks.hpp"

namespace TL
{
    namespace OpenMP
    {
        FunctionTaskDependency::FunctionTaskDependency(Expression expr,
                DependencyDirection direction)
            : _direction(direction), _expr(expr)
        {
        }

        Expression FunctionTaskDependency::get_expression() const
        {
            return _expr;
        }

        DependencyDirection FunctionTaskDependency::get_direction() const
        {
            return _direction;
        }

        FunctionTaskInfo::FunctionTaskInfo(Symbol sym,
                ObjectList<FunctionTaskDependency> parameter_info)
            : _sym(sym), _parameters(parameter_info)
        {
        }

        ObjectList<Symbol> FunctionTaskInfo::get_involved_parameters() const
        {
            ObjectList<Symbol> result;

            for (ObjectList<FunctionTaskDependency>::const_iterator it = _parameters.begin();
                    it != _parameters.end();
                    it++)
            {
                Expression expr(it->get_expression());

                ObjectList<Symbol> current_syms = expr.all_symbol_occurrences().map(functor(&IdExpression::get_symbol));
                result.insert(current_syms);
            }

            return result;
        }

        ObjectList<FunctionTaskDependency> FunctionTaskInfo::get_parameter_info() const
        {
            return _parameters;
        }

        void FunctionTaskInfo::add_device(const std::string& device_name)
        {
            _implementation_table[device_name] = Symbol(NULL);
        }

        void FunctionTaskInfo::add_device_with_implementation(
                const std::string& device_name,
                Symbol implementor_symbol)
        {
            _implementation_table[device_name] = implementor_symbol;
        }

        ObjectList<std::string> FunctionTaskInfo::get_all_devices()
        {
            ObjectList<std::string> result;
            for (implementation_table_t::iterator it = _implementation_table.begin();
                    it != _implementation_table.end();
                    it++)
            {
                result.append(it->first);
            }

            return result;
        }

        ObjectList<FunctionTaskInfo::implementation_pair_t> FunctionTaskInfo::get_devices_with_implementation()
        {
            ObjectList<implementation_pair_t> result;

            for (implementation_table_t::iterator it = _implementation_table.begin();
                    it != _implementation_table.end();
                    it++)
            {
                if (it->second.is_valid())
                {
                    implementation_pair_t pair(*it);
                    result.append(pair);
                }
            }

            return result;
        }

        FunctionTaskSet::FunctionTaskSet()
        {
        }

        bool FunctionTaskSet::is_function_task(Symbol sym) const
        {
            return (_map.find(sym) != _map.end());
        }

        FunctionTaskInfo& FunctionTaskSet::get_function_task(Symbol sym)
        {
            return _map.find(sym)->second;
        }

        const FunctionTaskInfo& FunctionTaskSet::get_function_task(Symbol sym) const
        {
            return _map.find(sym)->second;
        }

        bool FunctionTaskSet::add_function_task(Symbol sym, const FunctionTaskInfo& function_info)
        {
            std::pair<Symbol, FunctionTaskInfo> pair(sym, function_info);
            _map.insert(pair);
        }

        bool FunctionTaskSet::empty() const
        {
            return _map.empty();
        }

        struct FunctionTaskDependencyGenerator : public Functor<FunctionTaskDependency, std::string>
        {
            private:
                DependencyDirection _direction;
                AST_t _ref_tree;
                ScopeLink _sl;

            public:
                FunctionTaskDependencyGenerator(DependencyDirection direction,
                        AST_t ref_tree, ScopeLink sl)
                    : _direction(direction), _ref_tree(ref_tree), _sl(sl)
                {
                }

                FunctionTaskDependency do_(std::string& str) const
                {
                    Source src;

                    src
                        << "#line " << _ref_tree.get_line() << " \"" << _ref_tree.get_file() << "\"\n"
                        << str;

                    AST_t expr_tree = src.parse_expression(_ref_tree, _sl);
                    Expression expr(expr_tree, _sl);

                    return FunctionTaskDependency(expr, _direction);
                }
        };

        static void dependence_list_check(const ObjectList<FunctionTaskDependency>& function_task_param_list)
        {
            // More things could be checked
            for (ObjectList<FunctionTaskDependency>::const_iterator it = function_task_param_list.begin();
                    it != function_task_param_list.end();
                    it++)
            {
                DependencyDirection direction(it->get_direction());
                Expression expr(it->get_expression());

                if (expr.is_id_expression())
                {
                    Symbol sym = expr.get_id_expression().get_computed_symbol();
                    if (sym.is_parameter()
                            && !sym.get_type().is_reference())
                    {
                        // Copy semantics of values in C/C++ lead to this fact
                        running_error("%s: error: dependence expression '%s' "
                                "only names a parameter, which by itself never defines a dependence\n",
                                expr.get_ast().get_locus().c_str(),
                                expr.prettyprint().c_str());
                    }
                }
            }
        }

        void Core::task_function_handler_pre(PragmaCustomConstruct construct)
        {
            // Generic warning so nobody gets fooled by what the compiler will do
            ObjectList<std::string> clauses = construct.get_clause_names();

            // FIXME This message should be improved
            // for (ObjectList<std::string>::iterator it = clauses.begin();
            //         it != clauses.end();
            //         it++)
            // {
            //     if (*it != "input"
            //             && *it != "output"
            //             && *it != "inout"
            //             && *it != "untied")
            //     {
            //         std::cerr << construct.get_ast().get_locus() << ": warning: in a '#pragma omp task' applied to a function, clause '" 
            //             << (*it) << "' has no effect" << std::endl;
            //     }
            // }

            PragmaCustomClause input_clause = construct.get_clause("input");
            ObjectList<std::string> input_arguments;
            if (input_clause.is_defined())
            {
                input_arguments = input_clause.get_arguments(ExpressionTokenizer());
            }

            PragmaCustomClause output_clause = construct.get_clause("output");
            ObjectList<std::string> output_arguments;
            if (output_clause.is_defined())
            {
                output_arguments = output_clause.get_arguments(ExpressionTokenizer());
            }

            PragmaCustomClause inout_clause = construct.get_clause("inout");
            ObjectList<std::string> inout_arguments;
            if (inout_clause.is_defined())
            {
                inout_arguments = inout_clause.get_arguments(ExpressionTokenizer());
            }

            // Now discover whether this is a function definition or a declaration
            DeclaredEntity decl_entity(AST_t(), construct.get_scope_link());
            if (Declaration::predicate(construct.get_declaration()))
            {
                Declaration decl(construct.get_declaration(), construct.get_scope_link());
                ObjectList<DeclaredEntity> declared_entities = decl.get_declared_entities();

                if (declared_entities.size() != 1)
                {
                    std::cerr << construct.get_ast().get_locus() 
                        << ": warning: '#pragma omp task' construct applied to non suitable declaration, skipping" << std::endl;
                    return;
                }

                decl_entity = declared_entities[0];
            }
            else if (FunctionDefinition::predicate(construct.get_declaration()))
            {
                FunctionDefinition funct_def(construct.get_declaration(), construct.get_scope_link());
                decl_entity = funct_def.get_declared_entity();
            }
            else
            {
                std::cerr << construct.get_ast().get_locus() 
                        << ": warning: invalid use of '#pragma omp task', skipping" << std::endl;
                return;
            }

            if (!decl_entity.is_functional_declaration())
            {
                std::cerr << construct.get_ast().get_locus() 
                    << ": warning: '#pragma omp task' must precede a single function declaration or a function definition, skipping" << std::endl;
                return;
            }

            bool has_ellipsis = false;
            ObjectList<ParameterDeclaration> parameter_decl = decl_entity.get_parameter_declarations(has_ellipsis);
            Symbol function_sym = decl_entity.get_declared_symbol();

            if (has_ellipsis)
            {
                std::cerr << construct.get_ast().get_locus() 
                    << ": warning: '#pragma omp task' cannot be applied to functions declarations with ellipsis, skipping" << std::endl;
                return;
            }

            if (parameter_decl.empty()
                    || (parameter_decl.size() == 1 && parameter_decl[0].get_type().is_void()))
            {
                std::cerr << construct.get_ast().get_locus()
                    << ": warning: '#pragma omp task' cannot be applied to functions with no parameters, skipping" << std::endl;
                return;
            }

            Type function_type = function_sym.get_type();
            if (!function_type.returns().is_void())
            {
                std::cerr << construct.get_ast().get_locus()
                    << ": warning: '#pragma omp task' cannot be applied to functions returning non-void, skipping" << std::endl;
                return;
            }

            // Use the first parameter as a reference tree so we can parse the specifications
            AST_t param_ref_tree = parameter_decl[0].get_ast();

            ObjectList<FunctionTaskDependency> parameter_list;
            parameter_list.append(input_arguments.map(
                        FunctionTaskDependencyGenerator(DEP_DIR_INPUT,
                            param_ref_tree,
                            construct.get_scope_link())
                        )
                    );

            parameter_list.append(output_arguments.map(
                        FunctionTaskDependencyGenerator(DEP_DIR_OUTPUT,
                            param_ref_tree,
                            construct.get_scope_link())
                        )
                    );

            parameter_list.append(inout_arguments.map(
                        FunctionTaskDependencyGenerator(DEP_DIR_INOUT,
                            param_ref_tree,
                            construct.get_scope_link())
                        )
                    );

            dependence_list_check(parameter_list);

            FunctionTaskInfo task_info(function_sym, parameter_list);

            std::cerr << construct.get_ast().get_locus()
                << ": note: adding task function '" << function_sym.get_name() << "'" << std::endl;
            _function_task_set->add_function_task(function_sym, task_info);
        }
    }

}
