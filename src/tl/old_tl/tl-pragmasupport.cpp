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




#include <typeinfo>
#include <cctype>
#include "cxx-utils.h"
#include "tl-pragmasupport.hpp"

namespace TL
{
    static AST_t build_list_after_asts(ObjectList<AST_t> ast_list)
    {
        if (ast_list.empty())
        {
            // Invalid tree
            return AST_t(NULL);
        }
        else
        {
            ObjectList<AST_t>::iterator it = ast_list.begin();

            AST result = ::ast_list_leaf(it->get_internal_ast());
            it++;

            while (it != ast_list.end())
            {
                result = ::ast_list(result, it->get_internal_ast());
                it++;
            }

            return AST_t(result);
        }
    }

    static ObjectList<Expression> parse_as_expression_list(ObjectList<AST_t> clause_list, AST_t ref_tree, ScopeLink scope_link)
    {
        ObjectList<Expression> result;
        PredicateAttr clause_argument_pred(LANG_IS_PRAGMA_CUSTOM_CLAUSE_ARGUMENT);

        for (ObjectList<AST_t>::iterator it = clause_list.begin();
                it != clause_list.end();
                it++)
        {
            ObjectList<AST_t> argument_list = it->depth_subtrees(clause_argument_pred, AST_t::NON_RECURSIVE);

            // There should be only one of these
            for (ObjectList<AST_t>::iterator it2 = argument_list.begin();
                    it2 != argument_list.end();
                    it2++)
            {
                bool is_expr_list = false;

                if (it2->is_list())
                {
                    ASTIterator iter = it2->get_list_iterator();
                    iter.rewind();
                    // Do nothing if empty
                    if (iter.end())
                    {
                        is_expr_list = true;
                    }
                    else
                    {
                        is_expr_list = Expression::predicate(iter.item());
                    }
                }

                if (!is_expr_list)
                {
                    // Tokenize and parse properly
                    ObjectList<AST_t> current_tree_list;
                    ObjectList<std::string> expr_list_src = ExpressionTokenizer().tokenize(it2->prettyprint());

                    for (ObjectList<std::string>::iterator it_src = expr_list_src.begin();
                            it_src != expr_list_src.end();
                            it_src++)
                    {
                        Source src;

                        src 
                            << "#line " << ref_tree.get_line() << " \"" << ref_tree.get_file() << "\"\n"
                            << pad_to_column(ref_tree.get_column()) << (*it_src)
                            ;

                        AST_t parsed_expr = src.parse_expression(ref_tree, scope_link);

                        if (parsed_expr.is_valid())
                        {
                            current_tree_list.append(parsed_expr);

                            Expression expr(parsed_expr, scope_link);
                            result.append(expr);
                        }

                    }
                    AST_t list_of_exprs = build_list_after_asts(current_tree_list);

                    it2->replace(list_of_exprs);
                    it2->set_attribute(LANG_IS_PRAGMA_CUSTOM_CLAUSE_ARGUMENT, true);
                }
                else
                {
                    ASTIterator ast_list_iter = it2->get_list_iterator();

                    ast_list_iter.rewind();
                    while (!ast_list_iter.end())
                    {
                        Expression expr(ast_list_iter.item(), scope_link);
                        result.push_back(expr);
                        ast_list_iter.next();
                    }
                }
            }
        }

        return result;
    }

    PragmaCustomDispatcher::PragmaCustomDispatcher(const std::string& pragma_handled, 
            CustomFunctorMap& pre_map, CustomFunctorMap& post_map, bool warning_clauses)
        : _pragma_handled(pragma_handled), 
          _pre_map(pre_map), 
          _post_map(post_map),
		  _dto(NULL),
          _warning_clauses(warning_clauses)
    { 
    }

    void PragmaCustomDispatcher::preorder(Context ctx, AST_t node)
    {
        // Create it here
        PragmaCustomConstruct* pragma_custom_construct = new PragmaCustomConstruct(node, ctx.scope_link);
        pragma_custom_construct->set_dto(_dto);
        _construct_stack.push(pragma_custom_construct);

        dispatch_pragma_construct(_pre_map, *pragma_custom_construct);
    }

    void PragmaCustomDispatcher::postorder(Context ctx, AST_t node)
    {
        PragmaCustomConstruct* pragma_custom_construct = _construct_stack.top();
        _construct_stack.pop();

        dispatch_pragma_construct(_post_map, *pragma_custom_construct);

        RefPtr<ClausesInfo> clauses_info = RefPtr<ClausesInfo>::cast_dynamic((*_dto)["clauses"]);
        ObjectList<std::string> unreferenced_clauses = clauses_info->get_unreferenced_clauses(pragma_custom_construct->get_ast());
        if (_warning_clauses)
        {
		    for(ObjectList<std::string>::iterator it = unreferenced_clauses.begin(); it != unreferenced_clauses.end(); it++)
		    {
                warn_printf_at(clauses_info.get_locus_info(pragma_custom_construct.get_ast()),
                        "unused clause '%s' for '#pragma %s'\n",
                        it->c_str(),
                        clauses_info->get_pragma(pragma_custom_construct->get_ast()).c_str());
		    }
        }

        // Destroy it here
        delete pragma_custom_construct;
    }

    void PragmaCustomDispatcher::dispatch_pragma_construct(CustomFunctorMap& search_map, PragmaCustomConstruct& pragma_custom_construct)
    {
        // If this is a handled pragma in this class
        if (pragma_custom_construct.get_pragma() == _pragma_handled)
        {
            // Search its functor
            if (search_map.find(pragma_custom_construct.get_directive()) != search_map.end())
            {
                Signal1<PragmaCustomConstruct>& functor = search_map[pragma_custom_construct.get_directive()];
                functor.signal(pragma_custom_construct);
            }
        }
    }

    void PragmaCustomDispatcher::set_dto(DTO* dto)
    {
        _dto = dto;
    }

    void PragmaCustomDispatcher::set_warning_clauses(bool warning)
    {
        _warning_clauses = warning;
    }

    std::string PragmaCustomConstruct::get_pragma() const
    {
        TL::String result = this->get_ast().get_attribute(LANG_PRAGMA_CUSTOM);
        return result;
    }

    std::string PragmaCustomConstruct::get_directive() const
    {
        TL::AST_t pragma_line = this->get_ast().get_link_to_child(LANG_PRAGMA_CUSTOM_LINE);

        TL::String result = pragma_line.get_attribute(LANG_PRAGMA_CUSTOM_DIRECTIVE);

        return result;
    }

    bool PragmaCustomConstruct::is_directive() const
    {
        TL::Bool is_directive = this->get_ast().get_attribute(LANG_IS_PRAGMA_CUSTOM_DIRECTIVE);

        return is_directive;
    }

    AST_t PragmaCustomConstruct::get_declaration() const
    {
        // The user will put it into a FunctionDefinition or a Declaration
        // depending on his needs
        TL::AST_t declaration = this->get_ast().get_link_to_child(LANG_PRAGMA_CUSTOM_DECLARATION);

        if (!declaration.is_valid())
        {
            // Try to find a statement which is a declaration itself
            AST_t tree = this->get_ast().get_link_to_child(LANG_PRAGMA_CUSTOM_STATEMENT);
            if (tree.is_valid())
            {
                Statement result(tree, this->get_scope_link());
                if (result.is_declaration())
                {
                    return result.get_simple_declaration().get_ast();
                }
            }
        }

        return declaration;
    }

    AST_t PragmaCustomConstruct::get_pragma_line() const
    {
        TL::AST_t declaration = this->get_ast().get_link_to_child(LANG_PRAGMA_CUSTOM_LINE);
        return declaration;
    }

    bool PragmaCustomConstruct::is_construct() const
    {
        TL::Bool is_construct = this->get_ast().get_attribute(LANG_IS_PRAGMA_CUSTOM_CONSTRUCT);

        return is_construct;
    }

    Statement PragmaCustomConstruct::get_statement() const
    {
        AST_t tree = this->get_ast().get_link_to_child(LANG_PRAGMA_CUSTOM_STATEMENT);
        Statement result(tree, this->get_scope_link());

        return result;
    }

    ObjectList<std::string> PragmaCustomConstruct::get_clause_names() const
    {
        ObjectList<std::string> result;
        AST_t pragma_line = _ref.get_link_to_child(LANG_PRAGMA_CUSTOM_LINE);

        ObjectList<AST_t> clause_list = pragma_line.depth_subtrees(PredicateAttr(LANG_IS_PRAGMA_CUSTOM_CLAUSE));

        for (ObjectList<AST_t>::iterator it = clause_list.begin();
                it != clause_list.end();
                it++)
        {
            TL::String clause_name_attr = it->get_attribute(LANG_PRAGMA_CUSTOM_CLAUSE);
            result.insert(clause_name_attr);
        }

        return result;
    }
    
    void PragmaCustomConstruct::init_clause_info() const
    {
        RefPtr<ClausesInfo> clauses_info = RefPtr<ClausesInfo>::cast_dynamic((*_dto)["clauses"]);
        if (!clauses_info->directive_already_defined(this->get_ast()))
        {
            clauses_info->set_all_clauses(this->get_ast(), this->get_clause_names());
            clauses_info->set_locus_info(this->get_ast());
            clauses_info->set_pragma(*this);   
        }
    }

    PragmaCustomClause PragmaCustomConstruct::get_clause(const std::string& name) const
    {
        AST_t pragma_line = _ref.get_link_to_child(LANG_PRAGMA_CUSTOM_LINE);
        PragmaCustomClause result(name, pragma_line, this->get_scope_link());
        if (_dto!=NULL)
        {
		    RefPtr<ClausesInfo> clauses_info = RefPtr<ClausesInfo>::cast_dynamic((*_dto)["clauses"]);
		    if (!clauses_info->directive_already_defined(this->get_ast()))
		    {
                init_clause_info();
            }
		    clauses_info->add_referenced_clause(this->get_ast(), name);
        }

        return result;
    }
    PragmaCustomClause PragmaCustomConstruct::get_clause(const ObjectList<std::string>& names) const
    {
        AST_t pragma_line(_ref.get_attribute(LANG_PRAGMA_CUSTOM_LINE));

        PragmaCustomClause result(names, pragma_line, this->get_scope_link());
        if (_dto!=NULL)
        {
		    RefPtr<ClausesInfo> clauses_info = RefPtr<ClausesInfo>::cast_dynamic((*_dto)["clauses"]);
		    if (!clauses_info->directive_already_defined(this->get_ast()))
		    {
                init_clause_info();
            }
		    clauses_info->add_referenced_clause(this->get_ast(), names);
        }

        return result;
    }
    bool PragmaCustomConstruct::is_function_definition() const
    {
        AST_t declaration = get_declaration();
        if (declaration.is_valid())
        {
            TL::Bool b = declaration.get_attribute(LANG_IS_FUNCTION_DEFINITION);
            return b;
        }
        else return false;
    }

    bool PragmaCustomConstruct::is_parameterized() const
    {
        AST_t pragma_line = _ref.get_link_to_child(LANG_PRAGMA_CUSTOM_LINE);
        TL::Bool b = pragma_line.get_attribute(LANG_PRAGMA_CUSTOM_LINE_IS_PARAMETERIZED);

        return b;
    }

    ObjectList<Expression> PragmaCustomConstruct::get_parameter_expressions() const
    {
        ObjectList<Expression> result;
        AST_t pragma_line = _ref.get_link_to_child(LANG_PRAGMA_CUSTOM_LINE);
        AST_t parameter = pragma_line.get_link_to_child(LANG_PRAGMA_CUSTOM_LINE_PARAMETER);

        ObjectList<AST_t> parameter_list;
        parameter_list.append(parameter);

        result = parse_as_expression_list(parameter_list, this->_ref, this->_scope_link);

        return result;
    }

    ObjectList<IdExpression> PragmaCustomConstruct::get_parameter_id_expressions(IdExpressionCriteria criteria) const
    {
        ObjectList<IdExpression> result;
        ObjectList<Expression> expr_list = get_parameter_expressions();

        for (ObjectList<Expression>::iterator it = expr_list.begin();
                it != expr_list.end();
                it++)
        {
            if (it->is_id_expression())
            {
                IdExpression id_expr(it->get_id_expression());

                bool eligible = false;

                switch (criteria)
                {
                    case VALID_SYMBOLS:
                        {
                            eligible = (id_expr.get_symbol().is_valid());
                            break;
                        }
                    case ALL_FOUND_SYMBOLS:
                        {
                            eligible = true;
                            break;
                        }
                    case INVALID_SYMBOLS:
                        {
                            eligible = !(id_expr.get_symbol().is_valid());
                            break;
                        }
                    default: { }
                }

                if (eligible)
                {
                    result.append(id_expr);
                }
            }
        }

        return result;
    }

    ObjectList<std::string> PragmaCustomConstruct::get_parameter_arguments() const
    {
        return get_parameter_arguments(NullClauseTokenizer());
    }

    ObjectList<std::string> PragmaCustomConstruct::get_parameter_arguments(const ClauseTokenizer& tokenizer) const
    {
        ObjectList<std::string> result;
        AST_t pragma_line = _ref.get_link_to_child(LANG_PRAGMA_CUSTOM_LINE);
        AST_t parameter = pragma_line.get_link_to_child(LANG_PRAGMA_CUSTOM_LINE_PARAMETER);

        PredicateAttr clause_argument_pred(LANG_IS_PRAGMA_CUSTOM_CLAUSE_ARGUMENT);

        ObjectList<AST_t> argument_list = parameter.depth_subtrees(clause_argument_pred, AST_t::NON_RECURSIVE);

        for (ObjectList<AST_t>::iterator it = argument_list.begin();
                it != argument_list.end();
                it++)
        {
            result.append(tokenizer.tokenize(it->prettyprint()));
        }

        return result;
    }

    void PragmaCustomConstruct::set_dto(DTO* dto)
    {
        _dto = dto;
    }

	bool PragmaCustomConstruct::get_show_warnings()
	{
        if(_dto->get_keys().contains("show_warnings")) 
        {
            RefPtr<Bool> sw = RefPtr<Bool>::cast_static((*_dto)["show_warnings"]);
            return true;
        }
        return false;
	}

// Initialize here the warnings to the dispatcher
    PragmaCustomCompilerPhase::PragmaCustomCompilerPhase(const std::string& pragma_handled)
        : _pragma_handled(pragma_handled), 
         _pragma_dispatcher(pragma_handled, on_directive_pre, on_directive_post, false)
    {
    }

    void PragmaCustomCompilerPhase::pre_run(DTO& data_flow)
    {
        // Do nothing
    }

    void PragmaCustomCompilerPhase::run(DTO& data_flow)
    {
        // get the translation_unit tree
        AST_t translation_unit (data_flow["translation_unit"]);
        // get the scope_link
        ScopeLink scope_link = data_flow["scope_link"];
        // Get the global_scope
        Scope global_scope = scope_link.get_scope(translation_unit);

        // Instantiate a DepthTraverse
        DepthTraverse depth_traverse;

        PredicateAttr pragma_custom_directive_pred(LANG_IS_PRAGMA_CUSTOM_DIRECTIVE);
        PredicateAttr pragma_custom_construct_pred(LANG_IS_PRAGMA_CUSTOM_CONSTRUCT);

        depth_traverse.add_predicate(pragma_custom_directive_pred, _pragma_dispatcher);
        depth_traverse.add_predicate(pragma_custom_construct_pred, _pragma_dispatcher);

        if (!data_flow.get_keys().contains("clauses"))
        {
		    RefPtr<ClausesInfo> clauses_info_ptr(new ClausesInfo());
		    data_flow.set_object("clauses", clauses_info_ptr);
        }
		_pragma_dispatcher.set_dto(&data_flow);
        depth_traverse.traverse(translation_unit, scope_link);
		_pragma_dispatcher.set_dto(NULL);
    }

    void PragmaCustomCompilerPhase::register_directive(const std::string& str)
    {
        register_new_directive(_pragma_handled.c_str(), str.c_str(), 0, 0);
    }

    void PragmaCustomCompilerPhase::register_construct(const std::string& str, bool bound_to_statement)
    {
        if (IS_FORTRAN_LANGUAGE)
        {
            register_new_directive(_pragma_handled.c_str(), str.c_str(), 1, bound_to_statement);
        }
        else
        {
            register_new_directive(_pragma_handled.c_str(), str.c_str(), 1, 0);
        }
    }

    void PragmaCustomCompilerPhase::warning_pragma_unused_clauses(bool warning)
    {
        _pragma_dispatcher.set_warning_clauses(warning);
    }

    ObjectList<Expression> PragmaCustomClause::get_expression_list()
    {
        return parse_as_expression_list(filter_pragma_clause(), this->_ref, this->_scope_link);
    }

    ObjectList<AST_t> PragmaCustomClause::filter_pragma_clause()
    {
        class PredicateCustomClause : public Predicate<AST_t>
        {
            private:
                PredicateAttr _custom_clause;
                const ObjectList<std::string> & _clause_names;

                static std::string to_lower(std::string  s) 
                {
                    std::transform(s.begin(), s.end(), s.begin(), (int (*)(int))std::tolower);
                    return s;
                }

            public:
                PredicateCustomClause(const ObjectList<std::string> & clause_names)
                    : _custom_clause(LANG_IS_PRAGMA_CUSTOM_CLAUSE), _clause_names(clause_names)
                {
                }

                virtual bool do_(PredicateCustomClause::ArgType t) const
                {
                    if (_custom_clause(t))
                    {
                        TL::String clause_name_attr = t.get_attribute(LANG_PRAGMA_CUSTOM_CLAUSE);
                        
                        std::string str = to_lower(clause_name_attr);
                        return _clause_names.contains(str);
                    }
                    else return false;
                }
        };

        PredicateCustomClause predicate_custom_clause(_clause_names);

        ObjectList<AST_t> result = _ref.depth_subtrees(predicate_custom_clause);

        return result;
    }

    ObjectList<std::string> PragmaCustomClause::get_arguments()
    {
        return get_arguments(NullClauseTokenizer());
    }

    ObjectList<std::string> PragmaCustomClause::get_arguments(const ClauseTokenizer& tokenizer)
    {
        ObjectList<std::string> result;

        PredicateAttr clause_arg_pred(LANG_IS_PRAGMA_CUSTOM_CLAUSE_ARGUMENT);

        ObjectList<AST_t> clause_list = filter_pragma_clause();
        for (ObjectList<AST_t>::iterator it = clause_list.begin();
                it != clause_list.end();
                it++)
        {
            ObjectList<AST_t> arguments = it->depth_subtrees(clause_arg_pred, AST_t::NON_RECURSIVE);
            for (ObjectList<AST_t>::iterator jt = arguments.begin();
                    jt != arguments.end();
                    jt++)
            {
                result.append(tokenizer.tokenize(jt->prettyprint()));
            }
        }

        return result;
    }

    ObjectList<ObjectList<std::string> > PragmaCustomClause::get_arguments_unflattened()
    {
        ObjectList<ObjectList<std::string> > result;

        PredicateAttr clause_arg_pred(LANG_IS_PRAGMA_CUSTOM_CLAUSE_ARGUMENT);

        ObjectList<AST_t> clause_list = filter_pragma_clause();
        for (ObjectList<AST_t>::iterator it = clause_list.begin();
                it != clause_list.end();
                it++)
        {
            ObjectList<std::string> list;
            ObjectList<AST_t> arguments = it->depth_subtrees(clause_arg_pred, AST_t::NON_RECURSIVE);
            for (ObjectList<AST_t>::iterator jt = arguments.begin();
                    jt != arguments.end();
                    jt++)
            {
                list.append(jt->prettyprint());
            }

            result.append(list);
        }

        return result;
    }

    ObjectList<AST_t> PragmaCustomClause::get_arguments_tree()
    {
        ObjectList<AST_t> result;

        PredicateAttr clause_arg_pred(LANG_IS_PRAGMA_CUSTOM_CLAUSE_ARGUMENT);

        ObjectList<AST_t> clause_list = filter_pragma_clause();
        for (ObjectList<AST_t>::iterator it = clause_list.begin();
                it != clause_list.end();
                it++)
        {
            ObjectList<AST_t> arguments = it->depth_subtrees(clause_arg_pred, AST_t::NON_RECURSIVE);
            for (ObjectList<AST_t>::iterator jt = arguments.begin();
                    jt != arguments.end();
                    jt++)
            {
                AST_t tree= *jt;
                result.append(tree);
            }
        }

        return result;
    }

    ObjectList<IdExpression> PragmaCustomClause::id_expressions(IdExpressionCriteria criteria)
    {
        return get_id_expressions(criteria);
    }

    ObjectList<IdExpression> PragmaCustomClause::get_id_expressions(IdExpressionCriteria criteria)
    {
        ObjectList<IdExpression> result;
        ObjectList<Expression> expr_list = get_expression_list();

        for (ObjectList<Expression>::iterator it = expr_list.begin();
                it != expr_list.end();
                it++)
        {
            if (it->is_id_expression())
            {
                IdExpression id_expr(it->get_id_expression());

                bool eligible = false;

                switch (criteria)
                {
                    case VALID_SYMBOLS:
                        {
                            eligible = (id_expr.get_symbol().is_valid());
                            break;
                        }
                    case ALL_FOUND_SYMBOLS:
                        {
                            eligible = true;
                            break;
                        }
                    case INVALID_SYMBOLS:
                        {
                            eligible = !(id_expr.get_symbol().is_valid());
                            break;
                        }
                    default: { }
                }

                if (eligible)
                {
                    result.append(id_expr);
                }
            }
        }

        return result;
    }

    bool PragmaCustomClause::is_defined()
    {
        ObjectList<AST_t> clauses = filter_pragma_clause();

        return (!clauses.empty());
    }

    bool is_pragma_custom(const std::string& pragma_preffix,
            AST_t ast, ScopeLink scope_link)
    {
        PredicateAttr pred_ctr(LANG_IS_PRAGMA_CUSTOM_CONSTRUCT);
        PredicateAttr pred_dir(LANG_IS_PRAGMA_CUSTOM_DIRECTIVE);

        if (pred_ctr(ast) || pred_dir(ast))
        {
            PragmaCustomConstruct pragma_construct(ast, scope_link);

            if (pragma_construct.get_pragma() == pragma_preffix)
                return true;
        }
        return false;
    }

    bool is_pragma_custom_directive(const std::string& pragma_preffix, 
            const std::string& pragma_directive, 
            AST_t ast,
            ScopeLink scope_link)
    {
        PredicateAttr pred(LANG_IS_PRAGMA_CUSTOM_DIRECTIVE);

        if (pred(ast))
        {
            PragmaCustomConstruct pragma_construct(ast, scope_link);

            if (pragma_construct.get_pragma() == pragma_preffix
                    && pragma_construct.get_directive() == pragma_directive)
                return true;
        }

        return false;
    }


    bool is_pragma_custom_construct(const std::string& pragma_preffix, 
            const std::string& pragma_directive, 
            AST_t ast,
            ScopeLink scope_link)
    {
        PredicateAttr pred(LANG_IS_PRAGMA_CUSTOM_CONSTRUCT);

        if (pred(ast))
        {
            PragmaCustomConstruct pragma_construct(ast, scope_link);

            if (pragma_construct.get_pragma() == pragma_preffix
                    && pragma_construct.get_directive() == pragma_directive)
                return true;
        }

        return false;
    }
}
