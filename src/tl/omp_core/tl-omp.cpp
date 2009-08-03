/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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

        // Definition of predicates
#define OMP_CONSTRUCT(_class_name, _derives_from, _attr_name, _functor_name, _on_name) \
        const PredicateAttr _class_name::predicate(_attr_name);
#define OMP_CONSTRUCT_MAP(_class_name, _derives_from, _attr_name, _functor_name, _on_name)
#include "tl-omp-constructs.def"
#undef OMP_CONSTRUCT
#undef OMP_CONSTRUCT_MAP

        DataSharing::DataSharing(DataSharing *enclosing)
            : _num_refs(new int(1)), 
            _map(new std::map<Symbol, DataAttribute>),
            _map_reductions(new std::map<Symbol, std::string>),
            _enclosing(enclosing)
        {
            if (_enclosing != NULL)
            {
                (*_enclosing->_num_refs)++;
            }
        }

        DataSharing::~DataSharing()
        {
            (*_num_refs)--;
            if (*_num_refs == 0)
            {
                if (_enclosing != NULL)
                {
                    (*_enclosing->_num_refs)--;
                }

                delete _map;
                delete _map_reductions;
                delete _num_refs;
            }
        }

        DataSharing::DataSharing(const DataSharing& ds)
            : _num_refs(ds._num_refs),
            _map(ds._map),
            _map_reductions(ds._map_reductions),
            _enclosing(ds._enclosing)
        {
            (*_num_refs)++;
            if (_enclosing != NULL)
            {
                (*_enclosing->_num_refs)++;
            }
        }

        DataSharing* DataSharing::get_enclosing()
        {
            return _enclosing;
        }

        void DataSharing::set(Symbol sym, DataAttribute data_attr)
        {
            (_map->operator[](sym)) = data_attr;
        }

        void DataSharing::set_reduction(Symbol sym, const std::string& reductor_name)
        {
            (_map->operator[](sym)) = DA_REDUCTION;
            (_map_reductions->operator[](sym)) = reductor_name;
        }

        DataAttribute DataSharing::get_internal(Symbol sym)
        {
            std::map<Symbol, DataAttribute>::iterator it = _map->find(sym);
            if (it == _map->end())
            {
                return DA_UNDEFINED;
            }
            else
            {
                return it->second;
            }
        }

        DataAttribute DataSharing::get(Symbol sym)
        {
            DataAttribute result;
            result = get_internal(sym);
            return result;
        }

        std::string DataSharing::get_reductor_name(Symbol sym)
        {
            if (_map_reductions->find(sym) == _map_reductions->end())
                return "(unknown-reductor)";

            return (*_map_reductions)[sym];
        }

        Directive Construct::directive()
        {
            if (TL::Bool(_ref.get_attribute(OMP_IS_OMP_CONSTRUCT)))
            {
                AST_t ast = _ref.get_attribute(OMP_CONSTRUCT_DIRECTIVE);
                return Directive(ast, _scope_link);
            }
            else
            {
                return Directive(_ref, _scope_link);
            }
        }

        Statement Construct::body()
        {
            AST_t ast = _ref.get_attribute(OMP_CONSTRUCT_BODY);
            Statement result(ast, _scope_link);

            return result;
        }

        void Construct::set_data_attribute(Symbol sym, DataAttribute data_attr)
        {
            ERROR_CONDITION ((_data_sharing == NULL),
                    "Data sharing not defined for this construct", 0);

            ERROR_CONDITION(data_attr == DA_UNDEFINED,
                    "Invalid data sharing attribute", 0);

            if (_data_sharing->get(sym) != DA_UNDEFINED)
            {
                std::cerr << "Warning setting twice the data attribute for symbol '" 
                    << sym.get_name() << "'" << std::endl;
            }

            _data_sharing->set(sym, data_attr);
        }

        void Construct::add_data_attribute(Symbol sym, DataAttribute data_attr)
        {
            ERROR_CONDITION ((_data_sharing == NULL),
                    "Data sharing not defined for this construct", 0);

            ERROR_CONDITION(data_attr == DA_UNDEFINED,
                    "Invalid data sharing attribute", 0);

            DataAttribute prev_data_attr = _data_sharing->get(sym);

            _data_sharing->set(sym, (DataAttribute)(data_attr | prev_data_attr));
        }

        DataAttribute Construct::get_data_attribute(Symbol sym) const
        {
            DataAttribute result = DA_UNDEFINED;

            DataSharing *current_data_sharing = this->_data_sharing;

            while (result == DA_UNDEFINED
                    && current_data_sharing != NULL)
            {
                result = current_data_sharing->get(sym);

                current_data_sharing = current_data_sharing->get_enclosing();
            }

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
            CustomConstruct *custom_construct = NULL; 

            if (&search_map == &_custom_functor_pre)
            {
                Construct* enclosing_construct = NULL;

                if (!construct_stack.empty())
                {
                    enclosing_construct = construct_stack.top().current_construct;
                }

                // We are in preorder
                custom_construct = new CustomConstruct(node, ctx.scope_link, 
                        enclosing_construct, _openmp_info);

                ConstructInfo current_construct_info;
                current_construct_info.current_construct = custom_construct;

                Directive directive = custom_construct->directive();

                ObjectList<std::string> clauses_names = directive.get_all_custom_clauses();

                for (ObjectList<std::string>::iterator it = clauses_names.begin();
                        it != clauses_names.end();
                        it++)
                {
                    clause_locus_t p(*it, directive.get_ast().get_locus());
                    current_construct_info.clause_list.push_back(p);
                }

                construct_stack.push(current_construct_info);
            }
            else if (&search_map == &_custom_functor_post)
            {
                ERROR_CONDITION( (construct_stack.empty()),
                        "Stack of OpenMP constructs is empty in the postorder", 0);

                // We are in postorder
                custom_construct = static_cast<CustomConstruct*>(construct_stack.top().current_construct);
            }
            else
            {
                internal_error("Unreachable code", 0);
            }

            ERROR_CONDITION(custom_construct == NULL,
                    "Custom construct is NULL", 0);

            // Find this directive in custom map
            if (search_map.find(directive_name) != search_map.end())
            {
                // Invoke its functor if found
                Signal1<CustomConstruct>& functor = search_map[directive_name];

                functor.signal(*custom_construct);
            }
            else
            {
                // Kludge: If we are in preorder and not handled, and postorder
                // it is not handled either then warning
                if (&search_map == &_custom_functor_pre)
                {
                    if (_custom_functor_post.find(directive_name) == _custom_functor_post.end())
                    {
                        std::cerr << node.get_locus() 
                            << ": warning: custom OpenMP construct '" << directive_name << "' does not have any handler" << std::endl;
                    }
                }
            }

            // We are in postorder
            if (&search_map == &_custom_functor_post)
            {
                ConstructInfo &current_construct_info = construct_stack.top();
                // Emit a warning for every unused one
                if (!_disable_clause_warnings)
                {
                    for (ObjectList<clause_locus_t>::iterator it = current_construct_info.clause_list.begin();
                            it != current_construct_info.clause_list.end();
                            it++)
                    {
                        std::cerr << it->second 
                            << ": warning: clause '" << it->first << "' unused in custom OpenMP directive" << std::endl;
                    }
                }

                construct_stack.pop();
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
            // Use the DTO instead

            // get the translation_unit tree
            translation_unit = data_flow["translation_unit"];
            // get the scope_link
            scope_link = data_flow["scope_link"];
            // Get the global_scope
            global_scope = scope_link.get_scope(translation_unit);

            // Instantiate a DepthTraverse
            DepthTraverse depth_traverse;

            RefPtr<OpenMP::Info> openmp_info;

            if (data_flow.get_keys().contains("openmp_info"))
            {
                openmp_info = RefPtr<OpenMP::Info>::cast_static(data_flow["openmp_info"]);
            }
            else
            {
                std::cerr << "No OpenMP info was found" << std::endl;
                set_phase_status(PHASE_STATUS_ERROR);
            }

            // Add all functors, as needed
#define OMP_CONSTRUCT(_class_name, _derives_from, _attr_name, _functor_name, _on_name) \
            _functor_name _class_name##_functor(_on_name##_pre, _on_name##_post, *openmp_info, _disable_clause_warnings); \
            PredicateAttr _class_name##_predicate(_attr_name); \
            depth_traverse.add_predicate(_class_name##_predicate, _class_name##_functor);
#define OMP_CONSTRUCT_MAP(_class_name, _derives_from, _attr_name, _functor_name, _on_name)
#include "tl-omp-constructs.def"
#undef OMP_CONSTRUCT
#undef OMP_CONSTRUCT_MAP

            // #pragma omp constructs|directives
            // (custom constructions)
            PredicateAttr custom_construct(OMP_IS_CUSTOM_CONSTRUCT);
            CustomConstructFunctor custom_construct_functor(on_custom_construct_pre, on_custom_construct_post, 
                    *openmp_info, _disable_clause_warnings);
            depth_traverse.add_predicate(custom_construct, custom_construct_functor);
            PredicateAttr custom_directive(OMP_IS_CUSTOM_DIRECTIVE);
            depth_traverse.add_predicate(custom_directive, custom_construct_functor);

            // Let the user register its slots
            this->init(data_flow);

            // Traverse in a depth-first fashion the AST
            depth_traverse.traverse(translation_unit, scope_link);
        }

        void OpenMPPhase::init(DTO& dto)
        {
        }

        void OpenMPPhase::pre_run(DTO& dto)
        {
        }

        void OpenMPPhase::register_directive(const std::string& str)
        {
            register_new_directive("omp", str.c_str(), false);
        }

        void OpenMPPhase::register_construct(const std::string& str)
        {
            register_new_directive("omp", str.c_str(), true);
        }

        void OpenMPPhase::disable_clause_warnings(bool b)
        {
            _disable_clause_warnings = b;
        }

        Clause Directive::nowait_clause()
        {
            Clause result(_ref, _scope_link, OMP_IS_NOWAIT_CLAUSE);
            return result;
        }

        Clause Directive::if_clause()
        {
            Clause result(_ref, _scope_link, OMP_IS_IF_CLAUSE);
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
                    virtual bool do_(AST_t& ast) const
                    {
                        TL::Bool attr1 = ast.get_attribute(OMP_IS_DEFAULT_NONE_CLAUSE);
                        TL::Bool attr2 = ast.get_attribute(OMP_IS_DEFAULT_SHARED_CLAUSE);
                        TL::Bool attr3 = ast.get_attribute(OMP_IS_DEFAULT_CUSTOM_CLAUSE);

                        return attr1 || attr2 || attr3;
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

        CustomClause Directive::custom_clause(const std::string& name)
        {
            ObjectList<std::string> names;
            names.append(name);

            return custom_clause(names);
        }

        // Definition of construct_stack
        construct_stack_t construct_stack;

        CustomClause Directive::custom_clause(ObjectList<std::string>& names)
        {
            CustomClause result(names, _ref, _scope_link);

            // Copy
            ObjectList<clause_locus_t> new_referenced_clauses = construct_stack.top().clause_list;

            // And put again only those that were not mentioned
            construct_stack.top().clause_list.clear();

            for (ObjectList<clause_locus_t>::iterator it = new_referenced_clauses.begin();
                    it != new_referenced_clauses.end();
                    it++)
            {
                if (!names.contains(it->first))
                {
                    construct_stack.top().clause_list.push_back(*it);
                }
            }

            return result;
        }

        ObjectList<std::string> Directive::get_all_custom_clauses()
        {
            PredicateAttr predicate_custom_clause(OMP_IS_CUSTOM_CLAUSE);
            ObjectList<AST_t> clauses_list = _ref.depth_subtrees(predicate_custom_clause);
            ObjectList<std::string> result;

            for (ObjectList<AST_t>::iterator it = clauses_list.begin();
                    it != clauses_list.end();
                    it++)
            {
                TL::String clause_name_attr = it->get_attribute(OMP_CUSTOM_CLAUSE_NAME);
                result.insert(clause_name_attr);
            }

            return result;
        }

        ScheduleClause Directive::schedule_clause()
        {
            ScheduleClause result(_ref, _scope_link);
            return result;
        }

        AST_t Directive::get_clause_tree()
        {
            return _ref.get_attribute(OMP_CLAUSE_LIST);
        }

        bool ScheduleClause::is_defined()
        {
            PredicateAttr predicate_custom_clause(OMP_IS_SCHEDULE_CLAUSE);
            ObjectList<AST_t> clauses_list = _ref.depth_subtrees(predicate_custom_clause);

            return !(clauses_list.empty());
        }

        int ScheduleClause::internal_code()
        {
            PredicateAttr predicate_custom_clause(OMP_IS_SCHEDULE_CLAUSE);
            ObjectList<AST_t> clauses_list = _ref.depth_subtrees(predicate_custom_clause);

            AST_t first = *(clauses_list.begin());

            TL::Integer result = first.get_attribute(OMP_SCHEDULE_KIND);

            return result;
        }

        bool ScheduleClause::is_dynamic()
        {
            PredicateAttr predicate_custom_clause(OMP_IS_SCHEDULE_CLAUSE);
            ObjectList<AST_t> clauses_list = _ref.depth_subtrees(predicate_custom_clause);

            AST_t first = *(clauses_list.begin());

            TL::Integer result = first.get_attribute(OMP_SCHEDULE_KIND);

            return (result == 2);
        }

        bool ScheduleClause::is_static()
        {
            PredicateAttr predicate_custom_clause(OMP_IS_SCHEDULE_CLAUSE);
            ObjectList<AST_t> clauses_list = _ref.depth_subtrees(predicate_custom_clause);

            AST_t first = *(clauses_list.begin());

            TL::Integer result = first.get_attribute(OMP_SCHEDULE_KIND);

            return (result == 1);
        }

        bool ScheduleClause::is_guided()
        {
            PredicateAttr predicate_custom_clause(OMP_IS_SCHEDULE_CLAUSE);
            ObjectList<AST_t> clauses_list = _ref.depth_subtrees(predicate_custom_clause);

            AST_t first = *(clauses_list.begin());

            TL::Integer result = first.get_attribute(OMP_SCHEDULE_KIND);

            return (result == 4);
        }

        bool ScheduleClause::is_runtime()
        {
            PredicateAttr predicate_custom_clause(OMP_IS_SCHEDULE_CLAUSE);
            ObjectList<AST_t> clauses_list = _ref.depth_subtrees(predicate_custom_clause);

            AST_t first = *(clauses_list.begin());

            TL::Integer result = first.get_attribute(OMP_SCHEDULE_KIND);

            return (result == 8);
        }

        bool ScheduleClause::is_default()
        {
            PredicateAttr predicate_custom_clause(OMP_IS_SCHEDULE_CLAUSE);
            ObjectList<AST_t> clauses_list = _ref.depth_subtrees(predicate_custom_clause);

            AST_t first = *(clauses_list.begin());

            TL::Integer result = first.get_attribute(OMP_SCHEDULE_KIND);

            return (result == 0);
        }

        AST_t ScheduleClause::get_chunk()
        {
            PredicateAttr predicate_custom_clause(OMP_IS_SCHEDULE_CLAUSE);
            ObjectList<AST_t> clauses_list = _ref.depth_subtrees(predicate_custom_clause);

            AST_t first = *(clauses_list.begin());

            AST_t ast = first.get_attribute(OMP_SCHEDULE_CHUNK);
            return ast;
        }

        bool ScheduleClause::is_custom_schedule()
        {
            PredicateAttr predicate_custom_clause(OMP_IS_SCHEDULE_CLAUSE);
            ObjectList<AST_t> clauses_list = _ref.depth_subtrees(predicate_custom_clause);

            AST_t first = *(clauses_list.begin());

            TL::Integer result = first.get_attribute(OMP_SCHEDULE_KIND);

            return (result == 0);
        }

        std::string ScheduleClause::custom_schedule()
        {
            if (is_custom_schedule())
            {

                PredicateAttr predicate_custom_clause(OMP_IS_SCHEDULE_CLAUSE);
                ObjectList<AST_t> clauses_list = _ref.depth_subtrees(predicate_custom_clause);

                AST_t first = *(clauses_list.begin());

                TL::String str = first.get_attribute(OMP_SCHEDULE_CUSTOM_NAME);

                return str;
            }
            else 
                return "not a custom schedule";
        }

        ObjectList<AST_t> CustomClause::filter_custom_clause()
        {
            class PredicateCustomClause : public Predicate<AST_t>
            {
                private:
                    PredicateAttr _custom_clause;
                    // const std::string& _clause_name;
                    ObjectList<std::string>& _clause_names;

                public:
                    PredicateCustomClause(ObjectList<std::string>& clause_names)
                        : _custom_clause(OMP_IS_CUSTOM_CLAUSE), _clause_names(clause_names)
                    {
                    }

                    virtual bool do_(AST_t& t) const
                    {
                        if (_custom_clause(t))
                        {
                            TL::String clause_name_attr = t.get_attribute(OMP_CUSTOM_CLAUSE_NAME);

                            for (ObjectList<std::string>::iterator it = _clause_names.begin();
                                    it != _clause_names.end();
                                    it++)
                            {
                                if (clause_name_attr.compare_case_insensitive_to(*it))
                                {
                                    return true;
                                }
                            }

                            return false;
                        }
                        else return false;
                    }
            };

            PredicateCustomClause predicate_custom_clause(_clause_names);

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
            PredicateAttr expression_nest(LANG_IS_EXPRESSION_NEST);
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
            PredicateAttr expression_nest(LANG_IS_EXPRESSION_NEST);
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
            PredicateAttr id_expr_pred(LANG_IS_ID_EXPRESSION);

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

        bool DefaultClause::is_private() const
        {
            ObjectList<std::string> private_name;
            private_name.append("private");

            return this->is_custom(private_name);
        }

        bool DefaultClause::is_custom(const std::string& str) const
        {
            ObjectList<std::string> list;
            list.append(str);

            return is_custom(list);
        }

        bool DefaultClause::is_custom(ObjectList<std::string>& names) const
        {
            bool result = false;
            if (_ref.is_valid())
            {
                for (ObjectList<std::string>::iterator it = names.begin();
                        it != names.end() && !result;
                        it++)
                {
                    bool is_custom = TL::Bool(_ref.get_attribute(OMP_IS_DEFAULT_CUSTOM_CLAUSE));
                    std::string default_custom = "";
                    if (is_custom)
                    {
                        TL::String default_custom = _ref.get_attribute(OMP_DEFAULT_CUSTOM_CLAUSE);
                        result = (default_custom == *it);
                    }
                }
            }

            return result;
        }

        bool DefaultClause::is_defined() const
        {
            return _ref.is_valid();
        }

        ObjectList<IdExpression> Clause::id_expressions(IdExpressionCriteria criteria)
        {
            PredicateAttr id_expr_pred(LANG_IS_ID_EXPRESSION);

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

        ObjectList<ReductionSymbol> ReductionClause::id_expressions(IdExpressionCriteria criteria)
        {
            PredicateAttr id_expr_pred(LANG_IS_ID_EXPRESSION);

            ObjectList<ReductionSymbol> result;
            GetSymbolFromAST get_symbol_from_ast(this->_scope_link);

            PredicateAttr reduction_clause_predicate(OMP_IS_REDUCTION_CLAUSE);

            ObjectList<AST_t> reduction_clauses = _ref.depth_subtrees().filter(reduction_clause_predicate);

            for (ObjectList<AST_t>::iterator it = reduction_clauses.begin();
                    it != reduction_clauses.end();
                    it++)
            {
                AST_t reduction_clause = *it;

                AST_t reduct_operator = it->get_attribute(OMP_REDUCTION_OPERATOR);

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
                        ReductionSymbol reduct_id_expr(sym, reduct_operator.prettyprint());
                        result.append(reduct_id_expr);
                    }
                }
            }

            return result;
        }

        DataSharing& Info::get_new_data_sharing(AST_t a)
        {
            if (_map_data_sharing.find(a) != _map_data_sharing.end())
                delete _map_data_sharing[a];

            DataSharing* new_data_sharing = new DataSharing(_current_data_sharing);
            _map_data_sharing[a] = new_data_sharing;

            return *new_data_sharing;
        }

        DataSharing& Info::get_data_sharing(AST_t a)
        {
            if (_map_data_sharing.find(a) == _map_data_sharing.end())
                return *_root_data_sharing;
            else 
                return *(_map_data_sharing[a]);
        }

        DataSharing& Info::get_current_data_sharing()
        {
            return *_current_data_sharing;
        }

        DataSharing& Info::get_root_data_sharing()
        {
            return *_current_data_sharing;
        }

        void Info::push_current_data_sharing(DataSharing& data_sharing)
        {
            _stack_data_sharing.push(_current_data_sharing);
            _current_data_sharing = &data_sharing;
        }

        void Info::pop_current_data_sharing()
        {
            _current_data_sharing = _stack_data_sharing.top();
            _stack_data_sharing.pop();
        }
    }

}
