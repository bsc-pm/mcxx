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



#include "tl-omp-core.hpp"

namespace TL
{
    namespace OpenMP
    {
        FunctionTaskTargetInfo::FunctionTaskTargetInfo()
            : _copy_in(),
            _copy_out(),
            _copy_inout(),
            _device_list(),
            _copy_deps()
        {
        }

        bool FunctionTaskTargetInfo::can_be_ommitted()
        {
            return _copy_in.empty()
                && _copy_out.empty()
                && _copy_inout.empty()
                && _device_list.empty();
        }

        void FunctionTaskTargetInfo::set_copy_in(const ObjectList<CopyItem>& copy_items)
        {
            _copy_in = copy_items;
        }

        void FunctionTaskTargetInfo::set_copy_out(const ObjectList<CopyItem>& copy_items)
        {
            _copy_out = copy_items;
        }

        void FunctionTaskTargetInfo::set_copy_inout(const ObjectList<CopyItem>& copy_items)
        {
            _copy_inout = copy_items;
        }

        ObjectList<CopyItem> FunctionTaskTargetInfo::get_copy_in() const
        {
            return _copy_in;
        }

        ObjectList<CopyItem> FunctionTaskTargetInfo::get_copy_out() const
        {
            return _copy_out;
        }

        ObjectList<CopyItem> FunctionTaskTargetInfo::get_copy_inout() const
        {
            return _copy_inout;
        }

        void FunctionTaskTargetInfo::set_copy_deps(bool b)
        {
            _copy_deps = b;
        }

        bool FunctionTaskTargetInfo::has_copy_deps() const
        {
            return _copy_deps;
        }

        void FunctionTaskTargetInfo::set_device_list(const ObjectList<std::string>& device_list)
        {
            _device_list = device_list;
        }

        ObjectList<std::string> FunctionTaskTargetInfo::get_device_list() const
        {
            return _device_list;
        }

        FunctionTaskDependency::FunctionTaskDependency(DataReference expr,
                DependencyDirection direction)
            : _direction(direction), _expr(expr)
        {
        }

        DataReference FunctionTaskDependency::get_expression() const
        {
            return _expr;
        }

        DependencyDirection FunctionTaskDependency::get_direction() const
        {
            return _direction;
        }

        FunctionTaskInfo::FunctionTaskInfo(Symbol sym,
                ObjectList<FunctionTaskDependency> parameter_info,
                FunctionTaskTargetInfo target_info)
            : _sym(sym),
            _parameters(parameter_info),
            _implementation_table(),
            _target_info(target_info)
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

        ObjectList<FunctionTaskInfo::implementation_pair_t> FunctionTaskInfo::get_devices_with_implementation() const
        {
            ObjectList<implementation_pair_t> result;

            for (implementation_table_t::const_iterator it = _implementation_table.begin();
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

        void FunctionTaskInfo::set_real_time_info(const RealTimeInfo & rt_info)
        {
            _real_time_info = rt_info;
        }

        RealTimeInfo FunctionTaskInfo::get_real_time_info()
        {
            return _real_time_info;
        }


        FunctionTaskSet::FunctionTaskSet()
        {
        }

        bool FunctionTaskSet::is_function_task(Symbol sym) const
        {
            return (_map.find(sym) != _map.end());
        }

        bool FunctionTaskSet::is_function_task_or_implements(Symbol sym) const
        {
            if (is_function_task(sym))
                return true;

            typedef std::map<Symbol, FunctionTaskInfo>::const_iterator iterator;

            for (iterator it = _map.begin();
                    it != _map.end();
                    it++)
            {
                typedef ObjectList<FunctionTaskInfo::implementation_pair_t>::iterator dev_iterator;
                ObjectList<FunctionTaskInfo::implementation_pair_t> devices_and_implementations = it->second.get_devices_with_implementation();

                for (dev_iterator dev_it = devices_and_implementations.begin();
                        dev_it != devices_and_implementations.end();
                        dev_it++)
                {
                    if (dev_it->second == sym)
                        return true;
                }
            }

            return false;
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

        FunctionTaskTargetInfo FunctionTaskInfo::get_target_info() const
        {
            return _target_info;
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

                FunctionTaskDependency do_(FunctionTaskDependencyGenerator::ArgType str) const
                {
                    Source src;
                    src
                        << "#line " << _ref_tree.get_line() << " \"" << _ref_tree.get_file() << "\"\n"
                        << str;

                    AST_t expr_tree = src.parse_expression(_ref_tree, _sl);
                    DataReference expr(expr_tree, _sl);

                    return FunctionTaskDependency(expr, _direction);
                }
        };

        struct FunctionCopyItemGenerator : public Functor<CopyItem, std::string>
        {
            private:
                CopyDirection _copy_direction;
                AST_t _ref_tree;
                ScopeLink _sl;

            public:
                FunctionCopyItemGenerator(CopyDirection copy_direction,
                        AST_t ref_tree, ScopeLink sl)
                    : _copy_direction(copy_direction), _ref_tree(ref_tree), _sl(sl)
                {
                }

                CopyItem do_(FunctionCopyItemGenerator::ArgType str) const
                {
                    Source src;
                    src
                        << "#line " << _ref_tree.get_line() << " \"" << _ref_tree.get_file() << "\"\n"
                        << str
                        ;

                    AST_t expr_tree = src.parse_expression(_ref_tree, _sl);
                    DataReference data_ref(expr_tree, _sl);

                    return CopyItem(data_ref, _copy_direction);
                }
        };

        static bool is_useless_dependence(const FunctionTaskDependency& function_dep)
        {
            Expression expr(function_dep.get_expression());
            if (expr.is_id_expression())
            {
                Symbol sym = expr.get_id_expression().get_computed_symbol();
                if (sym.is_parameter()
                        && !sym.get_type().is_reference())
                {
                    return true;
                }
            }
            return false;
        }

        static void dependence_list_check(ObjectList<FunctionTaskDependency>& function_task_param_list)
        {
            ObjectList<FunctionTaskDependency>::iterator begin_remove = std::remove_if(function_task_param_list.begin(),
                    function_task_param_list.end(),
                    is_useless_dependence);

            for (ObjectList<FunctionTaskDependency>::iterator it = begin_remove;
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
                        // If the dependence is output (or inout) this should
                        // be regarded as an error
                        if ((direction & DEP_DIR_OUTPUT) == DEP_DIR_OUTPUT)
                        {
                            running_error("%s: error: output dependence '%s' "
                                    "only names a parameter. The value of a parameter is never copied out of a function "
                                    "so it cannot generate an output dependence",
                                    expr.get_ast().get_locus().c_str(),
                                    expr.prettyprint().c_str());
                        }
                        else
                        {
                            std::cerr << expr.get_ast().get_locus() << ": warning: "
                                "skipping useless dependence '"<< expr.prettyprint() << "'. The value of a parameter "
                                "is always copied and cannot define an input dependence"
                                << std::endl;
                        }
                    }
                }
            }

            // Remove useless expressions
            function_task_param_list.erase(begin_remove, function_task_param_list.end());
        }

        void Core::task_function_handler_pre(PragmaCustomConstruct construct)
        {
            RealTimeInfo rt_info = task_real_time_handler_pre(construct);

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

            PragmaCustomClause reduction_clause = construct.get_clause("__shared_reduction");
            ObjectList<std::string> reduction_arguments;
            if (reduction_clause.is_defined())
            {
                reduction_arguments = reduction_clause.get_arguments(ExpressionTokenizer());
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

            Type function_type = function_sym.get_type();
            if (!function_type.returns().is_void())
            {
                std::cerr << construct.get_ast().get_locus()
                    << ": warning: '#pragma omp task' cannot be applied to functions returning non-void, skipping" << std::endl;
                return;
            }

            ObjectList<FunctionTaskDependency> dependence_list;
            FunctionTaskTargetInfo target_info;

            AST_t param_ref_tree = function_sym.get_point_of_declaration();
            if (parameter_decl.empty()
                    || (parameter_decl.size() == 1 && parameter_decl[0].get_type().is_void()))
            {
                if(!function_sym.is_member() || function_sym.is_static())
                {
                    std::cerr << construct.get_ast().get_locus()
                              << ": warning: '#pragma omp task' "
                              << "applied to a function with no parameters"
                              << std::endl;
                }
            }
            else
            {
                // Use the first parameter as a reference tree so we can parse the specifications
                param_ref_tree = parameter_decl[0].get_ast();
            }
             dependence_list.append(input_arguments.map(FunctionTaskDependencyGenerator(DEP_DIR_INPUT,
                             param_ref_tree,construct.get_scope_link())));

             dependence_list.append(output_arguments.map(FunctionTaskDependencyGenerator(DEP_DIR_OUTPUT,
                             param_ref_tree,construct.get_scope_link())));

             dependence_list.append(inout_arguments.map(FunctionTaskDependencyGenerator(DEP_DIR_INOUT,
                             param_ref_tree,construct.get_scope_link())));

             dependence_list.append(reduction_arguments.map(FunctionTaskDependencyGenerator(DEP_REDUCTION,
                             param_ref_tree,construct.get_scope_link())));

             dependence_list_check(dependence_list);

             // Now gather task information
             if (!_target_context.empty())
             {
                 TargetContext& target_context = _target_context.top();

                 ObjectList<CopyItem> copy_in = target_context.copy_in.map(FunctionCopyItemGenerator(
                             COPY_DIR_IN, param_ref_tree, construct.get_scope_link()));
                 target_info.set_copy_in(copy_in);

                 ObjectList<CopyItem> copy_out = target_context.copy_out.map(FunctionCopyItemGenerator(
                             COPY_DIR_OUT, param_ref_tree, construct.get_scope_link()));
                 target_info.set_copy_out(copy_out);

                 ObjectList<CopyItem> copy_inout = target_context.copy_inout.map(FunctionCopyItemGenerator(
                             COPY_DIR_INOUT, param_ref_tree, construct.get_scope_link()));
                 target_info.set_copy_inout(copy_inout);

                 target_info.set_device_list(target_context.device_list);

                 target_info.set_copy_deps(target_context.copy_deps);
             }


            FunctionTaskInfo task_info(function_sym, dependence_list, target_info);

            //adding real time information to the task
            task_info.set_real_time_info(rt_info);

            std::cerr << construct.get_ast().get_locus()
                << ": note: adding task function '" << function_sym.get_name() << "'" << std::endl;
            _function_task_set->add_function_task(function_sym, task_info);
        }


        void Core::task_inline_handler_pre(PragmaCustomConstruct construct)
        {
            RealTimeInfo rt_info = task_real_time_handler_pre(construct);

            DataSharingEnvironment& data_sharing = _openmp_info->get_new_data_sharing(construct.get_ast());
            _openmp_info->push_current_data_sharing(data_sharing);

            //adding real time information to the task
            data_sharing.set_real_time_info(rt_info);

            get_data_explicit_attributes(construct, data_sharing);

            get_dependences_info(construct, data_sharing);

            DataSharingAttribute default_data_attr = get_default_data_sharing(construct, /* fallback */ DS_UNDEFINED);
            get_data_implicit_attributes_task(construct, data_sharing, default_data_attr);

            // Target info applies after
            get_target_info(construct, data_sharing);
        }


        RealTimeInfo Core::task_real_time_handler_pre(PragmaCustomConstruct construct)
        {
            RealTimeInfo rt_info;

            PragmaCustomClause deadline_clause = construct.get_clause("deadline");
            if (deadline_clause.is_defined())
            {
                ObjectList<std::string> deadline_args =
                    deadline_clause.get_arguments(ExpressionTokenizer());

                if(deadline_args.size() != 1)
                {
                    std::cerr << construct.get_ast().get_locus()
                              << ": warning: '#pragma omp task deadline' "
                              << "has a wrong number of arguments, skipping"
                              << std::endl;
                }
                else
                {
                    std::cerr << "warning: '#pragma omp task deadline' "
                              << "is not implemented yet (tl-omp-tasks.cpp)"
                              << std::endl;
                }
            }

            PragmaCustomClause onerror_clause = construct.get_clause("onerror");
            if (onerror_clause.is_defined())
            {
                ObjectList<std::string> onerror_args =
                    onerror_clause.get_arguments(ExpressionTokenizer());

                if(onerror_args.size() != 1)
                {
                    std::cerr << construct.get_ast().get_locus()
                              << ": warning: '#pragma omp task onerror' "
                              << "has a wrong number of arguments, skipping"
                              << std::endl;
                }
                else
                {
                    //tokens structure: 'identifier:identifier'
                    Lexer l = Lexer::get_current_lexer();

                    ObjectList<int> tokens = l.lex_string(onerror_args[0]);

                    if(tokens.size() != 3)
                    {
                        std::cerr << construct.get_ast().get_locus()
                                  << ": warning: '#pragma omp task onerror' "
                                  << "has a wrong number of tokens. "
                                  << "It is expecting 'identifier:identifier', skipping"
                                  << std::endl;

                    }
                    else if ((IS_C_LANGUAGE   && (tokens[0] != TokensC::IDENTIFIER)) ||
                             (IS_CXX_LANGUAGE && (tokens[0] != TokensCXX::IDENTIFIER)))
                    {
                        std::cerr << construct.get_ast().get_locus()
                                  << ": warning: '#pragma omp task onerror' "
                                  << "first token must be an identifier, skipping"
                                  << std::endl;
                    }
                    else if (tokens[1] != (int)':')
                    {
                        std::cerr << construct.get_ast().get_locus()
                                  << ": warning: '#pragma omp task onerror' "
                                  << "second token must be a colon, skipping"
                                  << std::endl;
                    }
                    else if ((IS_C_LANGUAGE   && (tokens[2] != TokensC::IDENTIFIER)) ||
                             (IS_CXX_LANGUAGE && (tokens[2] != TokensCXX::IDENTIFIER)))
                    {
                        std::cerr << construct.get_ast().get_locus()
                                  << ": warning: '#pragma omp task onerror' "
                                  << "third token must be an identifier, skipping"
                                  << std::endl;
                    }
                    else
                    {
                        std::cerr << "warning: '#pragma omp task onerror' "
                                  << "is not implemented yet (tl-omp-tasks.cpp)"
                                  << std::endl;
                    }
                }
            }

            return rt_info;
        }

    }
}
