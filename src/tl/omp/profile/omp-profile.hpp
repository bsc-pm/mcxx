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



#ifndef OMP_PROFILE_HPP
#define OMP_PROFILE_HPP

#include "tl-compilerphase.hpp"
#include "tl-omp.hpp"
#include "tl-symbol.hpp"

#include <map>

#define STRUCT(_NAME, _FIELDS) \
    typedef struct _NAME##_tag { _FIELDS } _NAME; \
    static const char* const _NAME##_str = "typedef struct "#_NAME"_tag" "{" #_FIELDS "}" #_NAME ";"

#define ADD_COUNTER_CODE(_var, _field, _src) \
            if (_var._field != 0) \
            { \
            _src << "current_task_info->"#_field " += "  << _var._field << ";"; \
            } \

namespace TL
{
    STRUCT(TaskProfileInfo,
            char* file;
            unsigned int line;
            unsigned long long int num_invocations;
            unsigned long long int num_taskwait;
            unsigned long long int num_shared;
            unsigned long long int num_firstprivate;
            unsigned long long int num_global_shared_read;
            unsigned long long int num_global_shared_write;
            unsigned long long int num_parent_shared_read;
            unsigned long long int num_parent_shared_write;
            unsigned long long int num_potential_shared_read;
            unsigned long long int num_potential_shared_write;
            unsigned long long int num_read;
            unsigned long long int num_write;
            unsigned long long int sizeof_fp;
            unsigned long long int num_fp_read;
            unsigned long long int num_fp_write;
            unsigned long long int num_ops;
            unsigned long long int num_childs;
            unsigned long long int times_in_threshold;
          );

    
    class SpecificFunctionDef : public Predicate<AST_t>
    {
        private:
            Symbol _sym;
            ScopeLink _sl;
        public:
            SpecificFunctionDef(Symbol sym, ScopeLink sl)
                : _sym(sym), _sl(sl)
            {
            }

            virtual bool do_(SpecificFunctionDef::ArgType a) const
            {
                if (FunctionDefinition::predicate(a))
                {
                    FunctionDefinition function_def(a, _sl);
                    Symbol function_symbol 
                        = function_def.get_ast().get_attribute(LANG_FUNCTION_SYMBOL);
                    if (function_symbol == _sym)
                    {
                        return true;
                    }
                    else
                    {
                        return false;
                    }
                }
                else return false;
            }
    };

    class ExpressionNotNestedTaskPred : public TraverseASTFunctor
    {
        private:
            ScopeLink _sl;
            PredicateAttr _pred_pragma;
        public:
            ExpressionNotNestedTaskPred(ScopeLink sl)
                  : _sl(sl), _pred_pragma(LANG_IS_PRAGMA_CUSTOM_LINE)
            {
            }

            ASTTraversalResult do_(ExpressionNotNestedTaskPred::ArgType a) const
            {
                if (Expression::predicate(a))
                {
                    Expression expr(a, _sl);
                    return ast_traversal_result_helper(/* match */ expr.is_top_level_expression(), 
                            /* recurse */ false);
                }
                else if (DeclaredEntity::predicate(a))
                {
                    return ast_traversal_result_helper(/* match */ false,
                            /* recurse */ false);
                }
                // Ignore pragmas, nothing can be expanded there
                else if (_pred_pragma(a))
                {
                    return ast_traversal_result_helper(/* match */ false,
                            /* recurse */ false);
                }
                else
                {
                    return ast_traversal_result_helper(/* match */ false, /* recurse */ true);
                }
            }
    };

    struct ClosureInfo
    {
        private:
            ObjectList<OpenMP::DataSharingAttribute> _data_attrib;
        public:
            ClosureInfo(int num_args)
            {
                for (int i = 0; i < num_args; i++)
                {
                    _data_attrib.append(OpenMP::DS_UNDEFINED);
                }
            }

            void set_arg_data_sharing(int arg, OpenMP::DataSharingAttribute da)
            {
                _data_attrib[arg] = da;
            }

            OpenMP::DataSharingAttribute get_arg_data_sharing(int arg)
            {
                return _data_attrib[arg];
            }
    };

    struct DelayedClosure
    {
        ScopeLink _sl;
        Symbol _sym;
        ClosureInfo _closure_info;

        DelayedClosure(ScopeLink sl, Symbol sym, ClosureInfo closure_info)
            : _sl(sl), _sym(sym), _closure_info(closure_info)
        {
        }
    };

    class OpenMPProfile : public OpenMP::OpenMPPhase
    {
        public:
            OpenMPProfile();

            virtual void run(DTO& dto);
            virtual void init(DTO& dto);

        private:
            int _current_task_id;
            int _task_ops_threshold;
            std::string _task_ops_threshold_str;
            std::string _fun_ops_str;
            ObjectList<Symbol> _closured_functions;
            ObjectList<DelayedClosure> _delayed_closures;
            ObjectList<AST_t> _function_definition_list;

            std::map<std::string, int> _cost_map;

            void set_task_threshold(const std::string& str);
            void set_fun_ops(const std::string& str);

            void info_funops_param(Expression expr);

            void parallel_preorder(PragmaCustomConstruct parallel_construct);
            void parallel_for_preorder(PragmaCustomConstruct parallel_construct);
            void parallel_sections_preorder(PragmaCustomConstruct parallel_construct);

            void task_preorder(PragmaCustomConstruct task_construct);
            void task_postorder(PragmaCustomConstruct task_construct);

            void taskwait_postorder(PragmaCustomConstruct taskwait_directive);

            void profile_task(TaskProfileInfo &task_profile_info, 
                    PragmaCustomConstruct task_construct,
                    Statement construct_body,
                    int task_id);

            void profile_taskwaits(PragmaCustomConstruct task_construct,
                    Statement construct_body,
                    int task_id);

            void analyze_expression(Expression expr, 
                    TaskProfileInfo &task_profile_info,
                    PragmaCustomConstruct &task_construct,
                    bool written,
                    ObjectList<Symbol> &read_set);

            void analyze_expression_closure(Expression expr, 
                    TaskProfileInfo &profile_info,
                    ClosureInfo closure_info,
                    bool written,
                    ObjectList<Symbol> &read_set);

            void common_parallel_data_sharing_code(PragmaCustomConstruct &parallel_construct);

            void task_compute_explicit_data_sharing(
                    PragmaCustomConstruct &directive,
                    ObjectList<Symbol> &captureaddress_references,
                    ObjectList<Symbol> &local_references,
                    ObjectList<Symbol> &captureprivate_references,
                    Scope& function_scope,
                    FunctionDefinition &function_definition,
                    PragmaCustomConstruct &task_construct);

            void task_compute_implicit_data_sharing(
                    PragmaCustomConstruct &directive,
                    ObjectList<Symbol> &captureaddress_references,
                    ObjectList<Symbol> &local_references,
                    ObjectList<Symbol> &captureprivate_references,
                    Scope& function_scope,
                    FunctionDefinition &function_definition,
                    Statement& construct_body,
                    PragmaCustomConstruct &task_construct);

            bool is_unqualified_member_symbol(Symbol current_symbol, FunctionDefinition function_definition);

            void get_data_attributes(
                    PragmaCustomConstruct &construct,
                    PragmaCustomConstruct directive,
                    Statement construct_body,
                    ObjectList<Symbol>& shared_references,
                    ObjectList<Symbol>& private_references,
                    ObjectList<Symbol>& firstprivate_references,
                    ObjectList<Symbol>& lastprivate_references,
                    ObjectList<OpenMP::ReductionSymbol>& reduction_references,
                    ObjectList<Symbol>& copyin_references,
                    ObjectList<Symbol>& copyprivate_references);

            void get_data_explicit_attributes(
                    PragmaCustomConstruct & construct,
                    PragmaCustomConstruct directive,
                    ObjectList<Symbol>& shared_references,
                    ObjectList<Symbol>& private_references,
                    ObjectList<Symbol>& firstprivate_references,
                    ObjectList<Symbol>& lastprivate_references,
                    ObjectList<OpenMP::ReductionSymbol>& reduction_references,
                    ObjectList<Symbol>& copyin_references,
                    ObjectList<Symbol>& copyprivate_references);

            std::string perform_closure(
                    ScopeLink sl, 
                    Symbol sym,
                    ClosureInfo closure_info);

            void perform_one_delayed_closure(
                    ScopeLink sl, 
                    Symbol sym,
                    ClosureInfo closure_info);

            void perform_delayed_closures();

            ClosureInfo fill_closure_info(
                    Symbol function_sym,
                    ObjectList<Expression> arguments, 
                    PragmaCustomConstruct& task_construct);

            ClosureInfo fill_closure_info_after_another(
                    Symbol function_sym,
                    ObjectList<Expression> arguments, 
                    ClosureInfo orig_closure_info);

            Symbol get_related_symbol_of_expr(Expression expr);

            void count_indirect_reference(Expression expr,
                    TaskProfileInfo &profile_info,
                    ClosureInfo closure_info,
                    bool written,
                    ObjectList<Symbol> &read_set);

            AST_t find_function_definition(Symbol sym);
    };
}

#endif // OMP_PROFILE_HPP
