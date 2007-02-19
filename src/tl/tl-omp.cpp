#include "tl-omp.hpp"
#include "tl-builtin.hpp"
#include "tl-ast.hpp"
#include "tl-source.hpp"
#include "tl-scopelink.hpp"
#include "tl-traverse.hpp"
#include "tl-predicateutils.hpp"
#include "cxx-attrnames.h"

namespace TL
{

    namespace OpenMP
    {
        Directive Construct::directive()
        {
            AST_t ast = _ref.get_attribute(OMP_CONSTRUCT_DIRECTIVE);

            Directive result(ast, _scope_link);
            return result;
        }

        Statement Construct::body()
        {
            AST_t ast = _ref.get_attribute(OMP_CONSTRUCT_BODY);
            Statement result(ast, _scope_link);

            return result;
        }

        void CustomConstructFunctor::dispatch_custom_construct(CustomFunctorMap& search_map, Context ctx, AST_t node)
        {
            TL::Bool is_directive = node.get_attribute(OMP_IS_CUSTOM_DIRECTIVE);
            TL::Bool is_construct = node.get_attribute(OMP_IS_CUSTOM_CONSTRUCT);

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

            // Find this directive in custom map
            if (search_map.find(directive_name) != search_map.end())
            {
                // Invoke its functor if found
                CustomConstruct custom_construct(node, ctx.scope_link);
                Signal1<CustomConstruct>& functor = search_map[directive_name];

                functor.signal(custom_construct);
            }
            else
            {
                if (&search_map == &_custom_functor_pre)
                {
                    if (_custom_functor_post.find(directive_name) == _custom_functor_post.end())
                    {
                        std::cerr << "Custom OpenMP construct '" << directive_name << "' in " 
                            << node.get_locus() << " is not currently handled" << std::endl;
                    }
                }
            }
        }

        void CustomConstructFunctor::preorder(Context ctx, AST_t node)
        {
            dispatch_custom_construct(_custom_functor_pre, ctx, node);
        }

        void CustomConstructFunctor::postorder(Context ctx, AST_t node)
        {
            dispatch_custom_construct(_custom_functor_post, ctx, node);
        }

        void OpenMPPhase::run(DTO& data_flow)
        {
            // get the translation_unit tree
            translation_unit = data_flow["translation_unit"];
            // get the scope_link
            scope_link = data_flow["scope_link"];
            // Get the global_scope
            global_scope = scope_link.get_scope(translation_unit);

            // Instantiate a DepthTraverse
            DepthTraverse depth_traverse;

            // Functor for #pragma omp parallel
            PredicateBool<OMP_IS_PARALLEL_CONSTRUCT> parallel_construct;
            ParallelFunctor parallel_functor(on_parallel_pre, on_parallel_post);
            // Register the #pragma omp parallel 
            // filter with its functor
            depth_traverse.add_predicate(parallel_construct, parallel_functor);

            // Functor for #pragma omp parallel for
            PredicateBool<OMP_IS_PARALLEL_FOR_CONSTRUCT> parallel_for_construct;
            ParallelForFunctor parallel_for_functor(on_parallel_for_pre, on_parallel_for_post);
            // Register the #pragma omp parallel for
            // filter with its functor 
            depth_traverse.add_predicate(parallel_for_construct, parallel_for_functor);

            // Functor for #pragma omp for
            PredicateBool<OMP_IS_FOR_CONSTRUCT> for_construct;
            ForFunctor for_functor(on_for_pre, on_for_post);
            // Register the #pragma omp parallel for
            // filter with its functor 
            depth_traverse.add_predicate(for_construct, for_functor);

            // #pragma omp parallel sections
            PredicateBool<OMP_IS_PARALLEL_SECTIONS_CONSTRUCT> parallel_sections_construct;
            ParallelSectionsFunctor parallel_sections_functor(on_parallel_sections_pre, 
                    on_parallel_sections_post);
            depth_traverse.add_predicate(parallel_sections_construct, parallel_sections_functor);

            // #pragma omp section
            PredicateBool<OMP_IS_SECTION_CONSTRUCT> section_construct;
            SectionFunctor section_functor(on_section_pre, on_section_post);
            depth_traverse.add_predicate(section_construct, section_functor);
            
            // #pragma omp barrier
            PredicateBool<OMP_IS_BARRIER_DIRECTIVE> barrier_directive;
            BarrierFunctor barrier_functor(on_barrier_pre, on_barrier_post);
            depth_traverse.add_predicate(barrier_directive, barrier_functor);

            // #pragma omp atomic
            PredicateBool<OMP_IS_ATOMIC_CONSTRUCT> atomic_construct;
            AtomicFunctor atomic_functor(on_atomic_pre, on_atomic_post);
            depth_traverse.add_predicate(atomic_construct, atomic_functor);
            
            // #pragma omp critical
            PredicateBool<OMP_IS_CRITICAL_CONSTRUCT> critical_construct;
            CriticalFunctor critical_functor(on_critical_pre, on_critical_post);
            depth_traverse.add_predicate(critical_construct, critical_functor);
            
            // #pragma omp parallel single
            PredicateBool<OMP_IS_PARALLEL_SINGLE_CONSTRUCT> parallel_single_construct;
            ParallelSingleFunctor parallel_single_functor(on_parallel_single_pre, on_parallel_single_post);
            depth_traverse.add_predicate(parallel_single_construct, parallel_single_functor);

            // #pragma omp single
            PredicateBool<OMP_IS_SINGLE_CONSTRUCT> single_construct;
            SingleFunctor single_functor(on_single_pre, on_single_post);
            depth_traverse.add_predicate(single_construct, single_functor);

            // #pragma omp flush
            PredicateBool<OMP_IS_FLUSH_DIRECTIVE> flush_directive;
            FlushFunctor flush_functor(on_flush_pre, on_flush_post);
            depth_traverse.add_predicate(flush_directive, flush_functor);

            // #pragma omp threadprivate
            PredicateBool<OMP_IS_THREADPRIVATE_DIRECTIVE> threadprivate_directive;
            ThreadPrivateFunctor threadprivate_functor(on_threadprivate_pre, on_threadprivate_post);
            depth_traverse.add_predicate(threadprivate_directive, threadprivate_functor);

            // #pragma omp ordered
            PredicateBool<OMP_IS_ORDERED_CONTRUCT> ordered_construct;
            OrderedFunctor ordered_functor(on_ordered_pre, on_ordered_post);
            depth_traverse.add_predicate(ordered_construct, ordered_functor);

            // #pragma omp master
            PredicateBool<OMP_IS_MASTER_CONSTRUCT> master_construct;
            MasterFunctor master_functor(on_master_pre, on_master_post);
            depth_traverse.add_predicate(master_construct, master_functor);

            // #pragma omp constructs|directives
            // (custom constructions)
            PredicateBool<OMP_IS_CUSTOM_CONSTRUCT> custom_construct;
            CustomConstructFunctor custom_construct_functor(on_custom_construct_pre, on_custom_construct_post);
            depth_traverse.add_predicate(custom_construct, custom_construct_functor);
            PredicateBool<OMP_IS_CUSTOM_DIRECTIVE> custom_directive;
            depth_traverse.add_predicate(custom_directive, custom_construct_functor);
            
            // Let the user register its slots
            this->init();

            // Traverse in a depth-first fashion the AST
            depth_traverse.traverse(translation_unit, scope_link);
        }

        void OpenMPPhase::init()
        {
        }

        Clause Directive::nowait_clause()
        {
            Clause result(_ref, _scope_link, OMP_IS_NOWAIT_CLAUSE);
            return result;
        }

        bool Clause::is_defined()
        {
            PredicateAttr predicate_clause(_clause_filter_name);
            ObjectList<AST_t> clauses = _ref.depth_subtrees().filter(predicate_clause);

            return (!clauses.empty());
        }

        DefaultClause Directive::default_clause()
        {
            class DefaultClausePredicate : public Predicate<AST_t>
            {
                public:
                    virtual bool operator()(AST_t& ast) const
                    {
                        TL::Bool attr1 = ast.get_attribute(OMP_IS_DEFAULT_NONE_CLAUSE);
                        TL::Bool attr2 = ast.get_attribute(OMP_IS_DEFAULT_SHARED_CLAUSE);

                        return attr1 || attr2;
                    }
                    virtual ~DefaultClausePredicate() { }
            };

            DefaultClausePredicate default_clause_predicate;

            ObjectList<AST_t> clauses = _ref.depth_subtrees().filter(default_clause_predicate);

            if (clauses.empty())
            {
                AST_t empty;
                DefaultClause result(empty, _scope_link);

                return result;
            }
            else
            {
                DefaultClause result(*(clauses.begin()), _scope_link);

                return result;
            }
        }

        Clause Directive::parameter_clause()
        {
            Clause result(_ref, _scope_link, OMP_IS_PARAMETER_CLAUSE);

            return result;
        }

        Clause Directive::shared_clause()
        {
            Clause result(_ref, _scope_link, OMP_IS_SHARED_CLAUSE);
            return result;
        }

        Clause Directive::private_clause()
        {
            Clause result(_ref, _scope_link, OMP_IS_PRIVATE_CLAUSE);
            return result;
        }

        Clause Directive::firstprivate_clause()
        {
            Clause result(_ref, _scope_link, OMP_IS_FIRSTPRIVATE_CLAUSE);
            return result;
        }

        Clause Directive::copyin_clause()
        {
            Clause result(_ref, _scope_link, OMP_IS_COPYIN_CLAUSE);
            return result;
        }

        Clause Directive::copyprivate_clause()
        {
            Clause result(_ref, _scope_link, OMP_IS_COPYPRIVATE_CLAUSE);
            return result;
        }

        Clause Directive::lastprivate_clause()
        {
            Clause result(_ref, _scope_link, OMP_IS_LASTPRIVATE_CLAUSE);
            return result;
        }

        Clause Directive::num_threads_clause()
        {
            Clause result(_ref, _scope_link, OMP_IS_NUM_THREADS_CLAUSE);
            return result;
        }

        CustomClause Directive::custom_clause(const std::string& src)
        {
            CustomClause result(src, _ref, _scope_link);
            return result;
        }

        ObjectList<AST_t> CustomClause::filter_custom_clause()
        {
            class PredicateCustomClause : public Predicate<AST_t>
            {
                private:
                    PredicateAttr _custom_clause;
                    const std::string& _clause_name;

                public:
                    PredicateCustomClause(const std::string& clause_name)
                         : _custom_clause(OMP_IS_CUSTOM_CLAUSE), _clause_name(clause_name)
                    {
                    }

                    virtual bool operator()(AST_t& t) const
                    {
                        if (_custom_clause(t))
                        {
                            TL::String clause_name_attr = t.get_attribute(OMP_CUSTOM_CLAUSE_NAME);

                            return (clause_name_attr.compare_case_insensitive_to(_clause_name));
                        }
                        else return false;
                    }
            };

            PredicateCustomClause predicate_custom_clause(_clause_name);

            ObjectList<AST_t> result = _ref.depth_subtrees(predicate_custom_clause);

            return result;
        }

        bool CustomClause::is_defined()
        {
            ObjectList<AST_t> clauses = filter_custom_clause();

            return (!clauses.empty());
        }

        // Time to start to think to fuse Clause and CustomClause in an inheritance tree
        ObjectList<Expression> Clause::get_expression_list()
        {
            PredicateBool<LANG_IS_EXPRESSION_NEST> expression_nest;
            ObjectList<Expression> result;

            PredicateAttr predicate_clause(_clause_filter_name);
            ObjectList<AST_t> clauses = _ref.depth_subtrees().filter(predicate_clause);

            for (ObjectList<AST_t>::iterator it = clauses.begin();
                    it != clauses.end();
                    it++)
            {
                ObjectList<AST_t> expression_nest_list = it->depth_subtrees(expression_nest, AST_t::NON_RECURSIVE);

                for (ObjectList<AST_t>::iterator it2 = expression_nest_list.begin();
                        it2 != expression_nest_list.end();
                        it2++)
                {
                    Expression expr(*it2, _scope_link);
                    result.append(expr);
                }
            }

            return result;
        }

        ObjectList<Expression> CustomClause::get_expression_list()
        {
            PredicateBool<LANG_IS_EXPRESSION_NEST> expression_nest;
            ObjectList<Expression> result;

            ObjectList<AST_t> custom_clauses = filter_custom_clause();
            for (ObjectList<AST_t>::iterator it = custom_clauses.begin();
                    it != custom_clauses.end();
                    it++)
            {
                ObjectList<AST_t> expression_nest_list = it->depth_subtrees(expression_nest, AST_t::NON_RECURSIVE);

                for (ObjectList<AST_t>::iterator it2 = expression_nest_list.begin();
                        it2 != expression_nest_list.end();
                        it2++)
                {
                    Expression expr(*it2, _scope_link);
                    result.append(expr);
                }
            }

            return result;
        }

        ObjectList<IdExpression> CustomClause::id_expressions(IdExpressionCriteria criteria)
        {
            PredicateBool<LANG_IS_ID_EXPRESSION> id_expr_pred;

            ObjectList<AST_t> clauses = filter_custom_clause();

            ObjectList<IdExpression> result;
            GetSymbolFromAST get_symbol_from_ast(this->_scope_link);

            for(ObjectList<AST_t>::iterator it = clauses.begin();
                    it != clauses.end();
                    it++)
            {
                ObjectList<AST_t> id_expressions = it->depth_subtrees().filter(id_expr_pred);

                for (ObjectList<AST_t>::iterator jt = id_expressions.begin();
                        jt != id_expressions.end();
                        jt++)
                {
                    Symbol sym = get_symbol_from_ast(*jt);

                    bool eligible = false;

                    switch (criteria)
                    {
                        case ALL_FOUND_SYMBOLS :
                            eligible = true;
                            break;
                        case VALID_SYMBOLS :
                            eligible = sym.is_valid();
                            break;
                        case INVALID_SYMBOLS :
                            eligible = !sym.is_valid();
                            break;
                    }

                    if (eligible)
                    {
                        IdExpression id_expr(*jt, this->_scope_link);
                        result.append(id_expr);
                    }
                }
            }

            return result;
        }

        ReductionClause Directive::reduction_clause()
        {
            ReductionClause result(_ref, _scope_link);
            return result;
        }

        bool DefaultClause::is_none() const
        {
            return _ref.is_valid() 
                && TL::Bool(_ref.get_attribute(OMP_IS_DEFAULT_NONE_CLAUSE));
        }

        bool DefaultClause::is_shared() const
        {
            return _ref.is_valid() 
                && TL::Bool(_ref.get_attribute(OMP_IS_DEFAULT_SHARED_CLAUSE));
        }

        ObjectList<IdExpression> Clause::id_expressions(IdExpressionCriteria criteria)
        {
            PredicateBool<LANG_IS_ID_EXPRESSION> id_expr_pred;

            PredicateAttr predicate_clause(_clause_filter_name);
            ObjectList<AST_t> clauses = _ref.depth_subtrees().filter(predicate_clause);

            ObjectList<IdExpression> result;
            GetSymbolFromAST get_symbol_from_ast(this->_scope_link);

            for(ObjectList<AST_t>::iterator it = clauses.begin();
                    it != clauses.end();
                    it++)
            {
                ObjectList<AST_t> id_expressions = it->depth_subtrees();

                id_expressions = id_expressions.filter(id_expr_pred);

                for (ObjectList<AST_t>::iterator jt = id_expressions.begin();
                        jt != id_expressions.end();
                        jt++)
                {
                    Symbol sym = get_symbol_from_ast(*jt);

                    bool eligible = false;

                    switch (criteria)
                    {
                        case ALL_FOUND_SYMBOLS :
                            eligible = true;
                            break;
                        case VALID_SYMBOLS :
                            eligible = sym.is_valid();
                            break;
                        case INVALID_SYMBOLS :
                            eligible = !sym.is_valid();
                            break;
                    }

                    if (eligible)
                    {
                        IdExpression id_expr(*jt, this->_scope_link);
                        result.append(id_expr);
                    }
                }
            }

            return result;
        }

        ObjectList<ReductionIdExpression> ReductionClause::id_expressions(IdExpressionCriteria criteria)
        {
            PredicateBool<LANG_IS_ID_EXPRESSION> id_expr_pred;

            ObjectList<ReductionIdExpression> result;
            GetSymbolFromAST get_symbol_from_ast(this->_scope_link);

            PredicateAttr reduction_clause_predicate(OMP_IS_REDUCTION_CLAUSE);

            ObjectList<AST_t> reduction_clauses = _ref.depth_subtrees().filter(reduction_clause_predicate);

            for (ObjectList<AST_t>::iterator it = reduction_clauses.begin();
                    it != reduction_clauses.end();
                    it++)
            {
                AST_t reduction_clause = *it;

                AST_t reduct_entity;

                TL::Bool is_user_defined = it->get_attribute(OMP_IS_USER_DEFINED_REDUCTION);
                
                if (is_user_defined)
                {
                    reduct_entity = it->get_attribute(OMP_REDUCTION_FUNCTION);
                }
                else
                {
                    reduct_entity = it->get_attribute(OMP_REDUCTION_OPERATOR);
                }


                AST_t reduct_neuter = it->get_attribute(OMP_REDUCTION_NEUTER);

                AST_t reduct_vars = it->get_attribute(OMP_REDUCTION_VARIABLES);

                ObjectList<AST_t> reduct_references = reduct_vars.depth_subtrees().filter(id_expr_pred);

                for (ObjectList<AST_t>::iterator jt = reduct_references.begin();
                        jt != reduct_references.end();
                        jt++)
                {
                    Symbol sym = get_symbol_from_ast(*jt);

                    bool eligible = false;

                    switch (criteria)
                    {
                        case ALL_FOUND_SYMBOLS :
                            eligible = true;
                            break;
                        case VALID_SYMBOLS :
                            eligible = sym.is_valid();
                            break;
                        case INVALID_SYMBOLS :
                            eligible = !sym.is_valid();
                            break;
                    }

                    if (eligible)
                    {
                        IdExpression id_expr(*jt, this->_scope_link);
                        ReductionIdExpression reduct_id_expr(id_expr, reduct_entity, reduct_neuter, is_user_defined);

                        result.append(reduct_id_expr);
                    }
                }
            }

            return result;
        }
    }
}
