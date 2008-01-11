/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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

#include "cxx-utils.h"

#include "tl-compilerphase.hpp"
#include "tl-ast.hpp"
#include "tl-scope.hpp"
#include "tl-scopelink.hpp"
#include "tl-langconstruct.hpp"
#include "tl-handler.hpp"
#include "tl-traverse.hpp"
#include "tl-dto.hpp"

#include <map>
#include <set>
#include <stack>
#include <utility>

namespace TL
{
    namespace OpenMP
    {
#define BITMAP(x) (1<<x)
        enum DataAttribute
        {
            DA_UNDEFINED = 0,
            DA_SHARED = BITMAP(1),
            DA_PRIVATE = BITMAP(2),
            DA_FIRSTPRIVATE = BITMAP(3) | DA_PRIVATE,
            DA_LASTPRIVATE = BITMAP(4) | DA_PRIVATE,
            DA_FIRSTLASTPRIVATE = DA_FIRSTPRIVATE | DA_LASTPRIVATE,
            DA_REDUCTION = BITMAP(5),
            DA_THREADPRIVATE = BITMAP(6),
            DA_COPYIN = BITMAP(7),
            DA_COPYPRIVATE = BITMAP(8)
        };
#undef BITMAP

        class DataSharing
        {
            private:
                int *_num_refs;
                std::map<Symbol, DataAttribute>  *_map;
                DataSharing *_enclosing;

                DataAttribute get_internal(Symbol sym);
            public:
                DataSharing(DataSharing *enclosing);
                ~DataSharing();

                DataSharing(const DataSharing& ds);
                void set(Symbol sym, DataAttribute data_attr);
                DataAttribute get(Symbol sym);
        };

        class Directive;
        class Construct : public LangConstruct, public LinkData
        {
            protected:
                Construct *_enclosing_construct;
                DataSharing *_data_sharing;
            public:
                Construct(AST_t ref, 
                        ScopeLink scope_link,
                        Construct* enclosing_construct,
                        DataSharing* enclosing_data_sharing)
                    : LangConstruct(ref, scope_link),
                    _enclosing_construct(enclosing_construct),
                    _data_sharing(enclosing_data_sharing)
                {
                }

                DataSharing& get_data_sharing() const
                {
                    return *_data_sharing;
                }

                Construct& get_enclosing_construct() const
                {
                    return *_enclosing_construct;
                }

                bool is_orphaned() const
                {
                    return _enclosing_construct == NULL;
                }

                bool no_data_sharing() const
                {
                    return _data_sharing == NULL;
                }

                void set_data_attribute(Symbol sym, DataAttribute data_attr);
                void add_data_attribute(Symbol sym, DataAttribute data_attr);
                DataAttribute get_data_attribute(Symbol sym) const;

                ~Construct() { }

                Statement body();
                Directive directive();
        };


        class Clause;
        class DefaultClause;
        class ReductionClause;
        class CustomClause;
        class ScheduleClause;

        class Directive : public LangConstruct
        {
            private:
            public:
                Directive(AST_t ref, ScopeLink scope_link)
                    : LangConstruct(ref, scope_link)
                {
                }

                ScheduleClause schedule_clause();

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
                bool is_private() const;
                
                bool is_custom(const std::string& str) const;

                bool is_custom(ObjectList<std::string>& names) const;

                bool is_defined() const;
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

        class ScheduleClause : public LangConstruct
        {
            public:
                int internal_code();
                bool is_dynamic();
                bool is_static();
                bool is_guided();
                bool is_runtime();
                bool is_default();
                bool is_defined();
                bool is_custom_schedule();
                std::string custom_schedule();
                AST_t get_chunk();
                ScheduleClause(AST_t ref, ScopeLink sl)
                    : LangConstruct(ref, sl) { }
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

        class DataEnvironmentConstruct : public Construct
        {
            public:
                DataEnvironmentConstruct(AST_t ref, 
                        ScopeLink scope_link, 
                        Construct *enclosing_construct,
                        DataSharing* enclosing_data_sharing)
                    : Construct(ref, scope_link, enclosing_construct, enclosing_data_sharing)
                {
                    // Do not inherit but create a new data sharing
                    _data_sharing = new DataSharing(enclosing_data_sharing);
                }
        };

#define DEFINE_CONSTRUCT_CLASS(_name, _derives_from) \
        class _name : public _derives_from  \
        { \
            public: \
                _name(AST_t ref,  \
                        ScopeLink scope_link,  \
                        Construct *enclosing_construct, \
                        DataSharing* enclosing_data_sharing) \
                    : _derives_from(ref, scope_link,  \
                            enclosing_construct,  \
                            enclosing_data_sharing) \
                { \
                } \
        }

        DEFINE_CONSTRUCT_CLASS(ParallelConstruct, DataEnvironmentConstruct);

        DEFINE_CONSTRUCT_CLASS(ParallelForConstruct, DataEnvironmentConstruct);
        DEFINE_CONSTRUCT_CLASS(ForConstruct, DataEnvironmentConstruct);

        DEFINE_CONSTRUCT_CLASS(BarrierDirective, Construct);
        DEFINE_CONSTRUCT_CLASS(AtomicConstruct, Construct);
        DEFINE_CONSTRUCT_CLASS(MasterConstruct, Construct);

        DEFINE_CONSTRUCT_CLASS(ParallelSingleConstruct, DataEnvironmentConstruct);
        DEFINE_CONSTRUCT_CLASS(SingleConstruct, DataEnvironmentConstruct);

        DEFINE_CONSTRUCT_CLASS(CriticalConstruct, DataEnvironmentConstruct);
        DEFINE_CONSTRUCT_CLASS(FlushDirective, Construct);

        DEFINE_CONSTRUCT_CLASS(ParallelSectionsConstruct, DataEnvironmentConstruct);
        DEFINE_CONSTRUCT_CLASS(SectionsConstruct, DataEnvironmentConstruct);

        DEFINE_CONSTRUCT_CLASS(ThreadPrivateDirective, Construct);

        DEFINE_CONSTRUCT_CLASS(SectionConstruct, Construct);

        DEFINE_CONSTRUCT_CLASS(OrderedConstruct, Construct);

        DEFINE_CONSTRUCT_CLASS(CustomConstruct, DataEnvironmentConstruct);

        // Stores information of the used clauses (this is ugly)
        typedef std::pair<std::string, std::string> clause_locus_t;

        class ConstructInfo
        {
            public:
                Construct *current_construct;
                ObjectList<clause_locus_t> clause_list;
        };

        typedef std::stack<ConstructInfo> construct_stack_t;
        extern construct_stack_t construct_stack;

        template<class T>
            class OpenMPConstructFunctor : public TraverseFunctor
        {
            private:
                Signal1<T>& _on_construct_pre;
                Signal1<T>& _on_construct_post;
                DataSharing &_global_data_sharing;
            public:
                virtual void preorder(Context ctx, AST_t node) 
                {
                    Construct* enclosing_construct = NULL;
                    DataSharing* enclosing_data_sharing = &_global_data_sharing;
                    if (!construct_stack.empty())
                    {
                        enclosing_construct = construct_stack.top().current_construct;
                        enclosing_data_sharing = &(enclosing_construct->get_data_sharing());
                    }

                    T *parallel_construct = new T(node, 
                            ctx.scope_link, 
                            enclosing_construct,
                            enclosing_data_sharing);

                    Directive directive = parallel_construct->directive();

                    // Fill the construct info
                    ConstructInfo current_construct_info;
                    current_construct_info.current_construct = parallel_construct;

                    ObjectList<std::string> clauses_names = directive.get_all_custom_clauses();
                    for (ObjectList<std::string>::iterator it = clauses_names.begin();
                            it != clauses_names.end();
                            it++)
                    {
                        clause_locus_t p(*it, directive.get_ast().get_locus());
                        current_construct_info.clause_list.push_back(p);
                    }

                    construct_stack.push(current_construct_info);

                    _on_construct_pre.signal(*parallel_construct);
                }

                // These two parameters are not used because we use a stack
                virtual void postorder(Context /*ctx*/, AST_t /*node*/) 
                {
                    ERROR_CONDITION( (construct_stack.empty()),
                            "Stack of OpenMP constructs is empty in the postorder", 0);
                    
                    ConstructInfo &current_construct_info = construct_stack.top();

                    T *parallel_construct = static_cast<T*>(current_construct_info.current_construct);
                    _on_construct_post.signal(*parallel_construct);

                    // Emit a warning for every unused one
                    for (ObjectList<clause_locus_t>::iterator it = current_construct_info.clause_list.begin();
                            it != current_construct_info.clause_list.end();
                            it++)
                    {
                        std::cerr << "Warning: Clause '" << it->first << "' unused in OpenMP directive at " << it->second << std::endl;
                    }

                    delete parallel_construct;
                    construct_stack.pop();
                }

                OpenMPConstructFunctor(Signal1<T>& on_construct_pre,
                        Signal1<T>& on_construct_post,
                        DataSharing& global_data_sharing
                        )
                    : _on_construct_pre(on_construct_pre),
                    _on_construct_post(on_construct_post),
                    _global_data_sharing(global_data_sharing)
                {
                }
        };

        typedef std::map<std::string, Signal1<CustomConstruct> > CustomFunctorMap;

        class CustomConstructPredicate : Predicate<AST_t>
        {
            private:
                std::string _construct_name;
            public:
                CustomConstructPredicate(const std::string construct_name)
                    : _construct_name(construct_name)
                {
                }

                bool operator()(AST_t& node) const
                {
                    TL::Bool is_directive = node.get_attribute(OMP_IS_CUSTOM_DIRECTIVE);
                    TL::Bool is_construct = node.get_attribute(OMP_IS_CUSTOM_CONSTRUCT);

                    if (!is_construct && !is_directive)
                    {
                        return false;
                    }

                    AST_t directive;
                    if (is_construct)
                    {
                        directive = node.get_attribute(OMP_CONSTRUCT_DIRECTIVE);
                    }
                    else
                    {
                        directive = node;
                    }

                    TL::String directive_name = directive.get_attribute(OMP_CUSTOM_DIRECTIVE_NAME);
                    return (directive_name == _construct_name);
                }
        };

        class CustomConstructFunctor : public TraverseFunctor
        {
            private:
                CustomFunctorMap& _custom_functor_pre;
                CustomFunctorMap& _custom_functor_post;
                DataSharing &_global_data_sharing;

                void dispatch_custom_construct(CustomFunctorMap& search_map, Context ctx, AST_t node);
            public:
                virtual void preorder(Context ctx, AST_t node);
                virtual void postorder(Context ctx, AST_t node);

                CustomConstructFunctor(CustomFunctorMap& custom_functor_pre, 
                        CustomFunctorMap& custom_functor_post,
                        DataSharing &global_data_sharing)
                    : _custom_functor_pre(custom_functor_pre),
                    _custom_functor_post(custom_functor_post),
                    _global_data_sharing(global_data_sharing)
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
                DataSharing global_data_sharing;
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

                void register_directive(const std::string& str);
                void register_construct(const std::string& str);

                virtual void run(DTO& data_flow);
                virtual void init();

                OpenMPPhase() 
                    : global_data_sharing(NULL) 
                { 
                }

                virtual ~OpenMPPhase() { }
        };
    }
}

extern "C"
{
    TL::CompilerPhase* give_compiler_phase_object(void);
}

#endif // TL_OMP_HPP
