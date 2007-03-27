/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#ifndef TL_OMP_HPP
#define TL_OMP_HPP

#include "tl-compilerphase.hpp"
#include "tl-ast.hpp"
#include "tl-scope.hpp"
#include "tl-scopelink.hpp"
#include "tl-langconstruct.hpp"
#include "tl-handler.hpp"
#include "tl-traverse.hpp"
#include "tl-dto.hpp"

#include <set>
#include <stack>
#include <utility>

namespace TL
{
    namespace OpenMP
    {
        class Directive;
        class Construct : public LangConstruct
        {
            public:
                Construct(AST_t ref, ScopeLink scope_link)
                    : LangConstruct(ref, scope_link)
                {
                }

                ~Construct() { }

                Statement body();
                Directive directive();
        };

        class Clause;
        class DefaultClause;
        class ReductionClause;
        class CustomClause;

        class Directive : public LangConstruct
        {
            private:
            public:
                Directive(AST_t ref, ScopeLink scope_link)
                    : LangConstruct(ref, scope_link)
                {
                }

                Clause nowait_clause();
                Clause num_threads_clause();
                Clause if_clause();

                Clause shared_clause();
                Clause private_clause();
                Clause firstprivate_clause();
                Clause lastprivate_clause();

                Clause copyin_clause();
                Clause copyprivate_clause();

                DefaultClause default_clause();

                ReductionClause reduction_clause();

                CustomClause custom_clause(const std::string& src);
                CustomClause custom_clause(ObjectList<std::string>& src);

                Clause parameter_clause();
                
                ObjectList<std::string> get_all_custom_clauses();
        };


        class Clause : public LangConstruct
        {
            private:
                const char* _clause_filter_name;
            public:
                Clause(AST_t ref, 
                        ScopeLink scope_link,
                        const char* clause_filter_name)
                    : LangConstruct(ref, scope_link), _clause_filter_name(clause_filter_name)
                { 
                }

                ObjectList<Expression> get_expression_list();
                ObjectList<IdExpression> id_expressions(IdExpressionCriteria criteria = VALID_SYMBOLS);
                bool is_defined();
        };

        class DefaultClause : public LangConstruct
        {
            public:
                DefaultClause(AST_t ref, ScopeLink scope_link)
                    : LangConstruct(ref, scope_link)
                {
                }

                bool is_none() const;
                bool is_shared() const;
        };

        class ReductionIdExpression
        {
            private:
                IdExpression _symbol;
                AST_t _op;
                AST_t _neuter;
                bool _is_user_defined;
            public:
                ReductionIdExpression(IdExpression s, AST_t op, AST_t neuter, bool is_user_defined = false)
                    : _symbol(s), _op(op), _neuter(neuter), _is_user_defined(is_user_defined)
                {
                }

                bool is_user_defined() const
                {
                    return _is_user_defined;
                }

                IdExpression get_id_expression() const
                {
                    return _symbol;
                }

                Symbol get_symbol() const
                {
                    return _symbol.get_symbol();
                }

                AST_t get_neuter() const
                {
                    return _neuter;
                }

                AST_t get_operation() const
                {
                    return _op;
                }

                IdExpression get_user_defined_reductor()
                {
                    return IdExpression(_op, _symbol.get_scope_link());
                }
        };

        class ReductionClause : public LangConstruct
        {
            public:
                ReductionClause(AST_t ref, ScopeLink scope_link)
                    : LangConstruct(ref, scope_link)
                {
                }

                ObjectList<ReductionIdExpression> id_expressions(IdExpressionCriteria criteria = VALID_SYMBOLS);
        };

        class CustomClause : public LangConstruct
        {
            private:
                ObjectList<std::string> _clause_names;

                ObjectList<AST_t> filter_custom_clause();
            public:
                CustomClause(const ObjectList<std::string>& names, AST_t ref, ScopeLink scope_link)
                    : LangConstruct(ref, scope_link), _clause_names(names)
                {
                }

                ObjectList<Expression> get_expression_list();
                ObjectList<IdExpression> id_expressions(IdExpressionCriteria criteria = VALID_SYMBOLS);

                bool is_defined();
        };

        class ParallelConstruct : public Construct
        {
            public:
                ParallelConstruct(AST_t ref, ScopeLink scope_link)
                    : Construct(ref, scope_link)
                {
                }
        };

        class ParallelForConstruct : public Construct
        {
            public:
                ParallelForConstruct(AST_t ref, ScopeLink scope_link)
                    : Construct(ref, scope_link)
                {
                }
        };

        class ForConstruct : public Construct
        {
            public:
                ForConstruct(AST_t ref, ScopeLink scope_link)
                    : Construct(ref, scope_link)
                {
                }
        };

        class BarrierDirective : public Construct
        {
            public:
                BarrierDirective(AST_t ref, ScopeLink scope_link)
                    : Construct(ref, scope_link)
                {
                }
        };

        class AtomicConstruct : public Construct
        {
            public:
                AtomicConstruct(AST_t ref, ScopeLink scope_link)
                    : Construct(ref, scope_link)
                {
                }
        };

        class MasterConstruct : public Construct
        {
            public:
                MasterConstruct(AST_t ref, ScopeLink scope_link)
                    : Construct(ref, scope_link)
                {
                }
        };

        class ParallelSingleConstruct : public Construct
        {
            public:
                ParallelSingleConstruct(AST_t ref, ScopeLink scope_link)
                    : Construct(ref, scope_link)
                {
                }
        };

        class SingleConstruct : public Construct
        {
            public:
                SingleConstruct(AST_t ref, ScopeLink scope_link)
                    : Construct(ref, scope_link)
                {
                }
        };

        class CriticalConstruct : public Construct
        {
            public:
                CriticalConstruct(AST_t ref, ScopeLink scope_link)
                    : Construct(ref, scope_link)
                {
                }
        };

        class FlushDirective : public Construct
        {
            public:
                FlushDirective(AST_t ref, ScopeLink scope_link)
                    : Construct(ref, scope_link)
                {
                }
        };

        class ParallelSectionsConstruct : public Construct
        {
            public:
                ParallelSectionsConstruct(AST_t ref, ScopeLink scope_link)
                    : Construct(ref, scope_link)
                {
                }
        };

        class SectionsConstruct : public Construct
        {
            public:
                SectionsConstruct(AST_t ref, ScopeLink scope_link)
                    : Construct(ref, scope_link)
                {
                }
        };

        class ThreadPrivateDirective : public Construct
        {
            public:
                ThreadPrivateDirective(AST_t ref, ScopeLink scope_link)
                    : Construct(ref, scope_link)
                {
                }
        };

        class SectionConstruct : public Construct
        {
            public:
                SectionConstruct(AST_t ref, ScopeLink scope_link)
                    : Construct(ref, scope_link)
                {
                }
        };

        class OrderedConstruct : public Construct
        {
            public:
                OrderedConstruct(AST_t ref, ScopeLink scope_link)
                    : Construct(ref, scope_link)
                {
                }
        };

        class CustomConstruct : public Construct
        {
            public:
                CustomConstruct(AST_t ref, ScopeLink scope_link)
                    : Construct(ref, scope_link)
                {
                }
        };

        // Stores information of the used clauses (this is ugly)
        typedef std::pair<std::string, std::string> construct_map_locus_t;
        typedef std::stack<ObjectList<construct_map_locus_t> > construct_map_t;
        extern construct_map_t construct_map;

        template<class T>
            class OpenMPConstructFunctor : public TraverseFunctor
        {
            private:
                Signal1<T>& _on_construct_pre;
                Signal1<T>& _on_construct_post;
            public:
                virtual void preorder(Context ctx, AST_t node) 
                {
                    T parallel_construct(node, ctx.scope_link);

                    // Empty set of strings
                    Directive directive = parallel_construct.directive();

                    ObjectList<construct_map_locus_t> current_custom_clauses;
                    
                    ObjectList<std::string> clauses_names = directive.get_all_custom_clauses();
                    for (ObjectList<std::string>::iterator it = clauses_names.begin();
                            it != clauses_names.end();
                            it++)
                    {
                        construct_map_locus_t p(*it, directive.get_ast().get_locus());
                        current_custom_clauses.push_back(p);
                    }

                    construct_map.push(current_custom_clauses);

                    _on_construct_pre.signal(parallel_construct);
                }

                virtual void postorder(Context ctx, AST_t node) 
                {
                    T parallel_construct(node, ctx.scope_link);

                    _on_construct_post.signal(parallel_construct);

                    // Emit a warning for every unused one
                    ObjectList<construct_map_locus_t> &unhandled_clauses = construct_map.top();
                    for (ObjectList<construct_map_locus_t>::iterator it = unhandled_clauses.begin();
                            it != unhandled_clauses.end();
                            it++)
                    {
                        std::cerr << "Warning: Clause '" << it->first << "' unused in OpenMP directive at " << it->second << std::endl;
                    }

                    construct_map.pop();
                }

                OpenMPConstructFunctor(Signal1<T>& on_construct_pre,
                        Signal1<T>& on_construct_post)
                    : _on_construct_pre(on_construct_pre),
                    _on_construct_post(on_construct_post)
                {
                }
        };

        typedef std::map<std::string, Signal1<CustomConstruct> > CustomFunctorMap;

        class CustomConstructFunctor : public TraverseFunctor
        {
            private:
                CustomFunctorMap& _custom_functor_pre;
                CustomFunctorMap& _custom_functor_post;

                void dispatch_custom_construct(CustomFunctorMap& search_map, Context ctx, AST_t node);
            public:
                virtual void preorder(Context ctx, AST_t node);
                virtual void postorder(Context ctx, AST_t node);

                CustomConstructFunctor(CustomFunctorMap& custom_functor_pre, 
                        CustomFunctorMap& custom_functor_post)
                    : _custom_functor_pre(custom_functor_pre),
                    _custom_functor_post(custom_functor_post)
            {
            }
        };

        class OpenMPPhase : public CompilerPhase
        {
            private:
                typedef OpenMPConstructFunctor<ParallelConstruct> ParallelFunctor;
                typedef OpenMPConstructFunctor<ParallelForConstruct> ParallelForFunctor;
                typedef OpenMPConstructFunctor<ForConstruct> ForFunctor;
                typedef OpenMPConstructFunctor<BarrierDirective> BarrierFunctor;
                typedef OpenMPConstructFunctor<CriticalConstruct> CriticalFunctor;
                typedef OpenMPConstructFunctor<AtomicConstruct> AtomicFunctor;
                typedef OpenMPConstructFunctor<ParallelSingleConstruct> ParallelSingleFunctor;
                typedef OpenMPConstructFunctor<SingleConstruct> SingleFunctor;
                typedef OpenMPConstructFunctor<FlushDirective> FlushFunctor;
                typedef OpenMPConstructFunctor<ParallelSectionsConstruct> ParallelSectionsFunctor;
                typedef OpenMPConstructFunctor<SectionsConstruct> SectionsFunctor;
                typedef OpenMPConstructFunctor<SectionConstruct> SectionFunctor;
                typedef OpenMPConstructFunctor<OrderedConstruct> OrderedFunctor;
                typedef OpenMPConstructFunctor<MasterConstruct> MasterFunctor;
                typedef OpenMPConstructFunctor<ThreadPrivateDirective> ThreadPrivateFunctor;
            protected:
                AST_t translation_unit;
                ScopeLink scope_link;
                Scope global_scope;
            public:
                Signal1<ParallelConstruct> on_parallel_pre;
                Signal1<ParallelConstruct> on_parallel_post;
                
                Signal1<ParallelForConstruct> on_parallel_for_pre;
                Signal1<ParallelForConstruct> on_parallel_for_post;

                Signal1<ForConstruct> on_for_pre;
                Signal1<ForConstruct> on_for_post;

                Signal1<BarrierDirective> on_barrier_pre;
                Signal1<BarrierDirective> on_barrier_post;

                Signal1<AtomicConstruct> on_atomic_pre;
                Signal1<AtomicConstruct> on_atomic_post;

                Signal1<SingleConstruct> on_single_pre;
                Signal1<SingleConstruct> on_single_post;

                Signal1<FlushDirective> on_flush_pre;
                Signal1<FlushDirective> on_flush_post;

                Signal1<CriticalConstruct> on_critical_pre;
                Signal1<CriticalConstruct> on_critical_post;

                Signal1<ParallelSectionsConstruct> on_parallel_sections_pre;
                Signal1<ParallelSectionsConstruct> on_parallel_sections_post;

                Signal1<ParallelSingleConstruct> on_parallel_single_pre;
                Signal1<ParallelSingleConstruct> on_parallel_single_post;

                Signal1<SectionsConstruct> on_sections_pre;
                Signal1<SectionsConstruct> on_sections_post;

                Signal1<SectionConstruct> on_section_pre;
                Signal1<SectionConstruct> on_section_post;

                Signal1<OrderedConstruct> on_ordered_pre;
                Signal1<OrderedConstruct> on_ordered_post;

                Signal1<ThreadPrivateDirective> on_threadprivate_pre;
                Signal1<ThreadPrivateDirective> on_threadprivate_post;

                Signal1<MasterConstruct> on_master_pre;
                Signal1<MasterConstruct> on_master_post;

                std::map<std::string, Signal1<CustomConstruct> > on_custom_construct_pre;
                std::map<std::string, Signal1<CustomConstruct> > on_custom_construct_post;

                virtual void run(DTO& data_flow);
                virtual void init();

                virtual ~OpenMPPhase() { }
        };
    }
}

extern "C"
{
    TL::CompilerPhase* give_compiler_phase_object(void);
}

#endif // TL_OMP_HPP
