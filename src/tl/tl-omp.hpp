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
    //! All OpenMP related classes are defined in this namespace
    namespace OpenMP
    {
        //! \addtogroup OpenMP OpenMP related classes
        // @{
#define BITMAP(x) (1<<x)
        //! Data attribute for data-sharing
        /*!
         * This is a bitmap and some values are already combined to express inclusion
         * of some data-sharing attributes within others
         */
        enum DataAttribute
        {
            DA_UNDEFINED = 0,
            //! Shared data sharing
            DA_SHARED = BITMAP(1),
            //! Private data sharing
            DA_PRIVATE = BITMAP(2),
            //! Firstprivate data sharing
            DA_FIRSTPRIVATE = BITMAP(3) | DA_PRIVATE,
            //! Lastprivate data sharing
            DA_LASTPRIVATE = BITMAP(4) | DA_PRIVATE,
            //! Both lastprivate and firstprivate
            DA_FIRSTLASTPRIVATE = DA_FIRSTPRIVATE | DA_LASTPRIVATE,
            //! Reduction data-sharing 
            DA_REDUCTION = BITMAP(5),
            //! Threadprivate data-sharing
            DA_THREADPRIVATE = BITMAP(6),
            //! Copy in data-sharing
            DA_COPYIN = BITMAP(7),
            //! Copy private data-sharing
            DA_COPYPRIVATE = BITMAP(8)
        };
#undef BITMAP

        //! This class represents data sharing environment in a OpenMP construct
        class DataSharing
        {
            private:
                int *_num_refs;
                std::map<Symbol, DataAttribute>  *_map;
                DataSharing *_enclosing;

                DataAttribute get_internal(Symbol sym);
            public:
                //! Constructor
                /*!
                 * \param enclosing Enclosing data sharing used when looking up
                 * the data sharing of a given symbol
                 */
                DataSharing(DataSharing *enclosing);
                ~DataSharing();

                //! Copy constructor
                DataSharing(const DataSharing& ds);

                //! Sets a data sharing attribute of a symbol
                /*!
                 * \param sym The symbol to be set the data sharing attribute
                 * \param data_attr The symbol to which the data sharing will be set
                 */
                void set(Symbol sym, DataAttribute data_attr);

                //! Gets the data sharing attribute of a symbol
                /*!
                 * \param sym The symbol requested its data sharing attribute
                 * \return The data sharing attribute or DA_UNDEFINED if no data sharing was set for it in this, and only this, DataSharing
                 */
                DataAttribute get(Symbol sym);
        };

        class Directive;

        //! Base class for all OpenMP constructs 
        class Construct : public LangConstruct, public LinkData
        {
            protected:
                //! The enclosing construct
                Construct *_enclosing_construct;
                //! The data sharing defined by this construct
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

                //! Gets the data sharing context of the current Construct
                DataSharing& get_data_sharing() const
                {
                    return *_data_sharing;
                }

                //! Gets the enclosed construct, if any
                /*!
                 * This function is only valid if is_orphaned returned false
                 */
                Construct& get_enclosing_construct() const
                {
                    return *_enclosing_construct;
                }

                //! States if the construct is orphaned
                bool is_orphaned() const
                {
                    return _enclosing_construct == NULL;
                }

                //! States if the construct does not have any data sharing
                bool no_data_sharing() const
                {
                    return _data_sharing == NULL;
                }

                //! Sets data attribute sharing for symbol sym
                /*!
                 * This function overwrites any previous data sharing attribute
                 */
                void set_data_attribute(Symbol sym, DataAttribute data_attr);
                //! Adds data attribute sharing for symbol sym
                /*!
                 * This function does not overwrite any previous data sharing
                 * attribute but adds the given one to the existing.
                 */
                void add_data_attribute(Symbol sym, DataAttribute data_attr);
                //! Gets the data attribute for given symbol
                /*!
                 * \return The data attribute or DA_UNDEFINED if none was set
                 * in the current DataSharing environment (the environment includes
                 * the enclosing DataSharing, if any)
                 */
                DataAttribute get_data_attribute(Symbol sym) const;

                ~Construct() { }

                //! Returns the statement related to this OpenMP construct
                Statement body();

                //! Returns the directive line related to this OpenMP construct
                Directive directive();
        };


        class Clause;
        class DefaultClause;
        class ReductionClause;
        class CustomClause;
        class ScheduleClause;

        //! Directive of any OpenMP construct
        /*!
         * This class wraps the pragma line of a given OpenMP construct
         */
        class Directive : public LangConstruct
        {
            private:
            public:
                Directive(AST_t ref, ScopeLink scope_link)
                    : LangConstruct(ref, scope_link)
                {
                }

                //! Get the 'schedule' clause
                ScheduleClause schedule_clause();

                //! Get the 'nowait' clause
                Clause nowait_clause();
                //! Get the 'num_threads' clause
                Clause num_threads_clause();
                //! Get the 'if' clause
                Clause if_clause();

                //! Get the 'shared' clause
                Clause shared_clause();
                //! Get the 'private' clause
                Clause private_clause();
                //! Get the 'firstprivate' clause
                Clause firstprivate_clause();
                //! Get the 'lastprivate' clause
                Clause lastprivate_clause();

                //! Get the 'copyin' clause
                Clause copyin_clause();
                //! Get the 'copyprivate' clause
                Clause copyprivate_clause();

                //! Get the 'default' clause
                DefaultClause default_clause();

                //! Get the 'reduction' clause
                ReductionClause reduction_clause();

                //! Get a custom clause
                /*!
                 * \param src The name of the custom clause
                 */
                CustomClause custom_clause(const std::string& src);
                //! Get a custom clause of a given set of names
                /*!
                 * \param src The valid alias for a custom clause
                 *
                 * Use this function if the OpenMP committee changes
                 * the name of clauses every meeting and you need
                 * to support code written during this process.
                 */
                CustomClause custom_clause(ObjectList<std::string>& src);

                //! Get the parameter clause of a construct
                /*!
                 * The parameter clause is sort of an unnamed clause that
                 * some OpenMP constructs have, like 'critical' construct
                 */
                Clause parameter_clause();
                
                //! Returns all custom clauses
                /*!
                 * This is used to give warnings of unhandled custom clauses
                 */
                ObjectList<std::string> get_all_custom_clauses();
        };


        //! This class wraps a clause in an OpenMP directive
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

                //! Gets the list of expressions specified in the clause
                ObjectList<Expression> get_expression_list();
                //! Convenience function to get a list of id-expressions in the clause
                ObjectList<IdExpression> id_expressions(IdExpressionCriteria criteria = VALID_SYMBOLS);
                //! States where the class was actually defined in the directive
                bool is_defined();
        };

        //! This class wraps 'default' class in an OpenMP directive
        class DefaultClause : public LangConstruct
        {
            public:
                DefaultClause(AST_t ref, ScopeLink scope_link)
                    : LangConstruct(ref, scope_link)
                {
                }

                //! States that clause is actually 'default(none)'
                bool is_none() const;
                //! States that clause is actually 'default(shared)'
                bool is_shared() const;
                //! States that clause is actually 'default(private)'
                bool is_private() const;
                
                //! States that clause is actually 'default(str)' with some given \a str
                /*!
                 * \param str The name of 'default(str)'
                 */
                bool is_custom(const std::string& str) const;

                //! States that clause is actually 'default(str)' whith \a str belonging to a list of names
                /*!
                 * \param names A list of names
                 */
                bool is_custom(ObjectList<std::string>& names) const;

                //! States whether the clause was actually defined in the directive
                bool is_defined() const;
        };

        //! Auxiliar class used in reduction clauses
        class ReductionSymbol
        {
            private:
                Symbol _symbol;
                AST_t _op;
                AST_t _neuter;
                bool _is_user_defined;
            public:
                ReductionSymbol(Symbol s, AST_t op, AST_t neuter, bool is_user_defined = false)
                    : _symbol(s), _op(op), _neuter(neuter), _is_user_defined(is_user_defined)
                {
                }

                //! States that the reduction is user defined
                /*!
                 * \bug Unsupported at the moment
                 */
                bool is_user_defined() const
                {
                    return _is_user_defined;
                }

                //! Returns the symbol related to this reduction
                Symbol get_symbol() const
                {
                    return _symbol;
                }

                //! Returns a tree with an expression of the neuter value of the reduction
                AST_t get_neuter() const
                {
                    return _neuter;
                }

                //! Gets the reduction operation
                AST_t get_operation() const
                {
                    return _op;
                }
        };

        //! This class wraps 'schedule' clause
        class ScheduleClause : public LangConstruct
        {
            public:
                int internal_code();

                //! States that clause is 'schedule(dynamic)'
                bool is_dynamic();
                //! States that clause is 'schedule(static)'
                bool is_static();
                //! States that clause is 'schedule(guided)'
                bool is_guided();
                //! States that clause is 'schedule(runtime)'
                bool is_runtime();
                //! States that clause is 'schedule(default)'
                bool is_default();
                //! States that a clause 'schedule' was actually defined in the directive
                bool is_defined();
                //! States that this is a custom schedule
                bool is_custom_schedule();
                //! Returns the name of the schedule
                std::string custom_schedule();

                // Returns the chunk expression related to some schedule kinds
                AST_t get_chunk();
                ScheduleClause(AST_t ref, ScopeLink sl)
                    : LangConstruct(ref, sl) { }
        };

        //! This class wraps 'reduction' clause
        class ReductionClause : public LangConstruct
        {
            public:
                ReductionClause(AST_t ref, ScopeLink scope_link)
                    : LangConstruct(ref, scope_link)
                {
                }

                //! A list of reduction symbols as defined in the clause
                ObjectList<ReductionSymbol> id_expressions(IdExpressionCriteria criteria = VALID_SYMBOLS);
        };

        //! Wraps a custom clause, for which no class might exist
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

                //! Gets a list of expressions in the custom clause
                ObjectList<Expression> get_expression_list();
                //! Convenience function that returns a list of id-expressions
                ObjectList<IdExpression> id_expressions(IdExpressionCriteria criteria = VALID_SYMBOLS);

                //! States whether this clause was actually defined in the directive
                bool is_defined();
        };

        //! Base class for constructs that inherit data sharing attribute
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

        //! Used to store information of used clauses
        typedef std::pair<std::string, std::string> clause_locus_t;

        //! Auxiliar class for construct information
        class ConstructInfo
        {
            public:
                Construct *current_construct;
                ObjectList<clause_locus_t> clause_list;
        };

        typedef std::stack<ConstructInfo> construct_stack_t;
        //! Stack of constructs built when traversing OpenMP constructs
        extern construct_stack_t construct_stack;

        //! Generic functor for OpenMP constructs
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

        //! Predicate for custom predicates
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

        //! Functor for custom constructs
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

        //! Base class for any implementation of OpenMP in Mercurium
        /*!
         * This class is currently used for the Nanos 4 runtime but it might be
         * used to target other runtimes
         */
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

                //! Custom map for custom OpenMP constructs found on preorder
                std::map<std::string, Signal1<CustomConstruct> > on_custom_construct_pre;
                //! Custom map for custom OpenMP constructs found on postorder
                std::map<std::string, Signal1<CustomConstruct> > on_custom_construct_post;

                //! Registers a custom directive
                /*!
                 * This is needed for proper parsing of directives and constructs.
                 * mcxx will complain if an unknown construct is found without 
                 * being registered. This function must be called in the constructor
                 * of the phase.
                 */
                void register_directive(const std::string& str);
                //! Registers a custom construct
                /*!
                 * This is needed for proper parsing of directives and constructs.
                 * mcxx will complain if an unknown construct is found without 
                 * being registered. This function must be called in the constructor
                 * of the phase.
                 */
                void register_construct(const std::string& str);

                //! Virtual function that registers all predicates when
                //traversing the tree looking for OpenMP constructs
                virtual void run(DTO& data_flow);
                //! User definable function called in run
                virtual void init(DTO& data_flow);

                OpenMPPhase() 
                    : global_data_sharing(NULL) 
                { 
                }

                virtual ~OpenMPPhase() { }
        };
        
    // @}
    }
    
}

extern "C"
{
    TL::CompilerPhase* give_compiler_phase_object(void);
}

#endif // TL_OMP_HPP
