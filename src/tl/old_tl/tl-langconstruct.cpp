/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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




#include "tl-langconstruct.hpp"
#include "tl-predicateutils.hpp"
#include "tl-source.hpp"
#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include "cxx-attrnames.h"
#include "cxx-exprtype.h"

namespace TL
{
    // Remove it once all LangConstruct sons have their own
    const AlwaysFalse<AST_t> LangConstruct::predicate;

    // Static predicates for LangConstructs
    const PredicateAttr IdExpression::predicate(LANG_IS_ID_EXPRESSION);
    const PredicateAttr FunctionDefinition::predicate(LANG_IS_FUNCTION_DEFINITION);
    const PredicateAttr Expression::predicate(LANG_IS_EXPRESSION_COMPONENT);
    const PredicateAttr ParameterDeclaration::predicate(LANG_IS_PARAMETER_DECLARATION);
    const PredicateAttr DeclaredEntity::predicate(LANG_IS_DECLARED_NAME);
    const PredicateAttr Declaration::predicate(LANG_IS_DECLARATION);
    const PredicateAttr GCCAttributeSpecifier::predicate(LANG_IS_GCC_ATTRIBUTE);
    const PredicateAttr TypeSpec::predicate(LANG_IS_TYPE_SPECIFIER);


    std::string LangConstruct::prettyprint() const
    {
        return _ref.prettyprint();
    }

    LangConstruct::operator std::string() const
    {
        return this->prettyprint();
    }
    
    std::ostream& operator<< (std::ostream& o, const LangConstruct& lang_construct)
    {
        return (o << lang_construct.prettyprint());
    }

    ObjectList<IdExpression> LangConstruct::all_symbol_occurrences(SymbolsWanted symbol_filter) const
    {
        PredicateAttr id_expr_pred(LANG_IS_ID_EXPRESSION);
        PredicateAttr member_access(LANG_IS_ACCESSED_MEMBER);
        ObjectList<AST_t> id_expressions = _ref.depth_subtrees()
            .filter(id_expr_pred)
            .filter(negate(member_access));

        ObjectList<IdExpression> result;

        for (ObjectList<AST_t>::iterator it = id_expressions.begin();
                it != id_expressions.end();
                it++)
        {
            AST_t& ref = *it;

            Scope ref_scope = _scope_link.get_scope(ref);
            Symbol symbol = ref_scope.get_symbol_from_id_expr(ref);

            if (symbol.is_valid() 
                    && !symbol.is_builtin())
            {
                IdExpression id_expression(*it, _scope_link);

                bool eligible = true;
                if (symbol_filter == ONLY_OBJECTS)
                {
                    eligible = symbol.is_variable();
                }
                else if (symbol_filter == ONLY_FUNCTIONS)
                {
                    eligible = symbol.is_function();
                }

                if (eligible)
                {
                    result.push_back(id_expression);
                }
            }
        }

        return result;
    }
    
    ObjectList<Symbol> LangConstruct::non_local_symbols(SymbolsWanted symbol_filter) const
    {
        ObjectList<IdExpression> id_expr_list = this->non_local_symbol_occurrences(symbol_filter);
        ObjectList<Symbol> result;

        result.insert(id_expr_list.map(functor(&IdExpression::get_computed_symbol)));

        return result;
    }

    ObjectList<IdExpression> LangConstruct::non_local_symbol_occurrences(SymbolsWanted symbol_filter) const
    {
        PredicateAttr id_expr_pred(LANG_IS_ID_EXPRESSION);
        PredicateAttr member_access(LANG_IS_ACCESSED_MEMBER);
        ObjectList<AST_t> id_expressions = _ref.depth_subtrees()
            .filter(id_expr_pred)
            .filter(negate(member_access));

        Scope statement_scope = _scope_link.get_scope(_ref);

        ObjectList<IdExpression> result;

        for (ObjectList<AST_t>::iterator it = id_expressions.begin();
                it != id_expressions.end();
                it++)
        {
            AST_t& ref = *it;

            Symbol symbol = statement_scope.get_symbol_from_id_expr(ref);

            if (symbol.is_valid() 
                    && !symbol.is_builtin())
            {
                Scope ref_scope = _scope_link.get_scope(ref);
                Symbol local_symbol = ref_scope.get_symbol_from_id_expr(ref);

                if (local_symbol == symbol)
                {
                    IdExpression id_expression(*it, _scope_link);

                    bool eligible = true;
                    if (symbol_filter == ONLY_OBJECTS)
                    {
                        eligible = symbol.is_variable();
                    }
                    else if (symbol_filter == ONLY_FUNCTIONS)
                    {
                        eligible = symbol.is_function();
                    }

                    if (eligible)
                    {
                        result.push_back(id_expression);
                    }
                }
            }
        }

        return result;
    }

    FunctionDefinition LangConstruct::get_enclosing_function() const
    {
        AST_t enclosing_function = _ref.get_enclosing_function_definition();
        FunctionDefinition result(enclosing_function, _scope_link);

        return result;
    }

    Statement LangConstruct::get_enclosing_statement() const
    {
        AST_t enclosing_statement = _ref.get_enclosing_statement();

        return Statement(enclosing_statement, _scope_link);
    }

    void FunctionDefinition::prepend_sibling(AST_t ast)
    {
        _ref.prepend_sibling_function(ast);
    }

    IdExpression FunctionDefinition::get_function_name() const
    {
        TL::AST_t ast = _ref.get_link_to_child(LANG_FUNCTION_NAME);

        return IdExpression(ast, _scope_link);
    }

    Symbol FunctionDefinition::get_function_symbol() const
    {
        return get_declared_entity().get_declared_symbol();
    }

    Statement FunctionDefinition::get_function_body() const
    {
        TL::AST_t ast = _ref.get_link_to_child(LANG_FUNCTION_BODY);

        return Statement(ast, _scope_link);
    }

    bool FunctionDefinition::is_templated() const
    {
        TL::Bool result = _ref.get_attribute(LANG_IS_TEMPLATED_FUNCTION_DEFINITION);
        return result;
    }

    ObjectList<TemplateHeader> FunctionDefinition::get_template_header() const
    {
        // FIXME - Improve this
        AST_t start_t = _ref.get_link_to_child(LANG_TEMPLATE_HEADER);
        AST a = start_t.get_internal_ast();

        ObjectList<TemplateHeader> result;

        while (ASTKind(a) == AST_TEMPLATE_DECLARATION)
        {
            result.append(TemplateHeader(ASTSon0(a), get_scope_link()));
            a = ASTSon1(a);
        }

        return result;
    }

    bool FunctionDefinition::has_linkage_specifier() const
    {
        TL::Bool result = _ref.get_attribute(LANG_HAS_LINKAGE_SPECIFIER);
        return result;
    }

    std::string LinkageSpecifier::prettyprint() const
    {
        return "extern " + _ref.prettyprint();
    }

    ObjectList<LinkageSpecifier> FunctionDefinition::get_linkage_specifier() const
    {
        TL::AST_t start = _ref.get_link_to_child(LANG_LINKAGE_SPECIFIER_HEADER);

        PredicateAttr template_header_pred(LANG_IS_LINKAGE_SPECIFIER);

        ObjectList<AST_t> trees = start.depth_subtrees(template_header_pred);
        ObjectList<LinkageSpecifier> result;

        for (ObjectList<AST_t>::iterator it = trees.begin();
                it != trees.end();
                it++)
        {
            result.append(LinkageSpecifier(*it, _scope_link));
        }

        return result;
    }

    AST_t FunctionDefinition::get_point_of_declaration() const
    {
        return _ref.get_enclosing_function_definition_declaration().get_parent();
    }

    DeclaredEntity FunctionDefinition::get_declared_entity() const
    {
        AST_t declarator = _ref.get_link_to_child(LANG_FUNCTION_DECLARATOR);
        return DeclaredEntity(declarator, _scope_link);
    }

    bool Declaration::is_templated() const
    {
        TL::Bool result = _ref.get_attribute(LANG_IS_TEMPLATED_DECLARATION);
        return result;
    }

    bool Declaration::has_linkage_specifier() const
    {
        TL::Bool result = _ref.get_attribute(LANG_HAS_LINKAGE_SPECIFIER);
        return result;
    }

    ObjectList<LinkageSpecifier> Declaration::get_linkage_specifier() const
    {
        TL::AST_t start = _ref.get_link_to_child(LANG_LINKAGE_SPECIFIER_HEADER);

        PredicateAttr template_header_pred(LANG_IS_LINKAGE_SPECIFIER);

        ObjectList<AST_t> trees = start.depth_subtrees(template_header_pred);
        ObjectList<LinkageSpecifier> result;

        for (ObjectList<AST_t>::iterator it = trees.begin();
                it != trees.end();
                it++)
        {
            result.append(LinkageSpecifier(*it, _scope_link));
        }

        return result;
    }

    ObjectList<TemplateHeader> Declaration::get_template_header() const
    {
        // FIXME - Improve this
        AST_t start_t = _ref.get_link_to_child(LANG_TEMPLATE_HEADER);
        AST a = start_t.get_internal_ast();

        ObjectList<TemplateHeader> result;

        while (ASTKind(a) == AST_TEMPLATE_DECLARATION)
        {
            result.append(TemplateHeader(ASTSon0(a), get_scope_link()));
            a = ASTSon1(a);
        }

        return result;
    }

    AST_t Declaration::get_point_of_declaration() const
    {
        if (is_templated())
        {
            // If it is templated we want to spring over the template headers
            TL::AST_t start = _ref.get_link_to_child(LANG_TEMPLATE_HEADER);
            AST inner_tree = start.get_internal_ast();
            return ASTParent(inner_tree);
        }
        else if (has_linkage_specifier())
        {
            AST_t tree = _ref;
            while (TL::Bool b = tree.get_attribute(LANG_HAS_LINKAGE_SPECIFIER))
            {
                tree = tree.get_parent();
            }
            return tree;
        }
        else
        {
            // Otherwise this is fine
            return _ref;
        }
    }

    bool Declaration::is_empty_declaration() const
    {
        TL::Bool result = _ref.get_attribute(LANG_IS_EMPTY_DECLARATION);
        return result;
    }

    // Returns a flattened version of this id-expression
    std::string IdExpression::mangle_id_expression() const
    {
        std::string id_expr_str = _ref.prettyprint();
        unsigned int length = id_expr_str.size();
        for (unsigned int i = 0; i < length; i++)
        {
            if (id_expr_str[i] == ':'
                    || id_expr_str[i] == '<'
                    || id_expr_str[i] == '>')
            {
                id_expr_str[i] = '_';
            }
        }

        return id_expr_str;
    }

    Declaration IdExpression::get_declaration() const
    {
        AST_t point_of_declaration = this->get_symbol().get_point_of_declaration();

        TL::Bool b = point_of_declaration.get_attribute(LANG_IS_DECLARATION);

        if (b)
        {
            return Declaration(point_of_declaration, this->_scope_link);
        }
        else
        {
            std::cerr << "Cannot retrieve a proper declaration of id-expression '" 
                << _ref.prettyprint() << "' from its point of declaration '" 
                <<  point_of_declaration.prettyprint() << "'"  << std::endl;
            return Declaration(AST_t(), this->_scope_link);
        }
    }

    std::string IdExpression::get_qualified_part() const
    {
        if (is_unqualified())
        {
            return "";
        }
        else
        {
            TL::AST_t nested_name_part = _ref.get_link_to_child(LANG_NESTED_NAME_SPECIFIER);
            return nested_name_part.prettyprint();
        }
    }

    bool IdExpression::is_template_id() const
    {
        TL::Bool ret = _ref.
            get_attribute(LANG_UNQUALIFIED_ID)
            ->get_attribute(LANG_IS_TEMPLATE_ID);
        return ret;
    }

    std::string IdExpression::get_template_arguments() const
    {
        TL::AST_t ret = _ref
            .get_link_to_child(LANG_UNQUALIFIED_ID)
            .get_link_to_child(LANG_TEMPLATE_ARGS);
        return "<" + ret.prettyprint(/*comma=*/true) + ">";
    }

    std::string IdExpression::get_template_name() const
    {
        TL::AST_t ret = _ref.get_link_to_child(LANG_TEMPLATE_NAME);
        return ret.prettyprint();
    }

    std::string IdExpression::get_unqualified_part(bool with_template_id) const
    {
        if (!this->is_qualified())
        {
            return _ref.prettyprint();
        }

        TL::AST_t unqualified_part = _ref.get_link_to_child(LANG_UNQUALIFIED_ID);

        if (!with_template_id)
        {
            TL::Bool is_template_id = unqualified_part.get_attribute(LANG_IS_TEMPLATE_ID);
            if (!is_template_id)
            {
                return unqualified_part.prettyprint();
            }
            else
            {
                TL::AST_t template_name = unqualified_part.get_link_to_child(LANG_TEMPLATE_NAME);

                return template_name.prettyprint();
            }
        }
        else
        {
            return unqualified_part.prettyprint();
        }
    }

    bool IdExpression::is_qualified() const
    {
        TL::Bool is_qualif = _ref.get_attribute(LANG_IS_QUALIFIED_ID);
        return (bool)is_qualif;
    }

    bool IdExpression::is_unqualified() const
    {
        TL::Bool is_qualif = _ref.get_attribute(LANG_IS_QUALIFIED_ID);
        return !((bool)is_qualif);
    }

    Symbol IdExpression::get_symbol() const
    {
        Scope id_expr_scope = _scope_link.get_scope(_ref);
        Symbol result = id_expr_scope.get_symbol_from_id_expr(_ref);
        return result;
    }

    Symbol IdExpression::get_computed_symbol() const
    {
        return Symbol(NULL);
    }

    void ReplaceIdExpression::add_this_replacement(const std::string& str)
    {
        _repl_this = str;
    }

    void ReplaceIdExpression::add_this_replacement(Source src)
    {
        add_this_replacement(src.get_source());
    }

    void ReplaceIdExpression::add_this_replacement(AST_t ast)
    {
        add_this_replacement(ast.prettyprint());
    }

    void ReplaceIdExpression::add_replacement(Symbol sym, const std::string& str)
    {
        _repl_map[sym] = str;
    }

    void ReplaceIdExpression::add_replacement(Symbol sym, Source src)
    {
        add_replacement(sym, src.get_source());
    }

    void ReplaceIdExpression::add_replacement(Symbol sym, AST_t ast)
    {
        add_replacement(sym, ast.prettyprint());
    }

    void ReplaceIdExpression::add_replacement(Symbol sym, const std::string& str, AST_t ref_tree, ScopeLink scope_link)
    {
        add_replacement(sym, str);
    }

    void ReplaceIdExpression::add_replacement(Symbol sym, Source src, AST_t ref_tree, ScopeLink scope_link)
    {
        add_replacement(sym, src.get_source());
    }

    bool ReplaceIdExpression::has_replacement(Symbol sym) const
    {
        return (_repl_map.find(sym) != _repl_map.end());
    }

    AST_t Expression::advance_over_nests(AST_t expr)
    {
        if (!expr.is_valid())
        {
            std::cerr << "Expression is not valid" << std::endl;
            return expr;
        }

        TL::Bool is_expression_nest = expr.get_attribute(LANG_IS_EXPRESSION_NEST);

        while (is_expression_nest)
        {
            expr = expr.get_link_to_child(LANG_EXPRESSION_NESTED);
            is_expression_nest = expr.get_attribute(LANG_IS_EXPRESSION_NEST);
        }

        return expr;
    }

    bool Expression::is_literal()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_LITERAL);

        return b;
    }

    bool Expression::is_id_expression()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_ID_EXPRESSION);

        return b;
    }

    bool Expression::is_accessed_member()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_ACCESSED_MEMBER);

        return b;
    }

    IdExpression Expression::get_id_expression()
    {
        return IdExpression(_ref, this->_scope_link);
    }

    bool Expression::is_binary_operation()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_BINARY_OPERATION);

        return b;
    }

    bool Expression::is_unary_operation()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_UNARY_OPERATION);

        return b;
    }

    bool Expression::is_assignment()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_ASSIGNMENT);

        return b;
    }
    
    bool Expression::is_operation_assignment()
    {
        TL::Bool b(false);
        b = TL::Bool(_ref.get_attribute(LANG_IS_MUL_ASSIGNMENT))
            || TL::Bool(_ref.get_attribute(LANG_IS_DIV_ASSIGNMENT))
            || TL::Bool(_ref.get_attribute(LANG_IS_ADD_ASSIGNMENT))
            || TL::Bool(_ref.get_attribute(LANG_IS_SUB_ASSIGNMENT))
            || TL::Bool(_ref.get_attribute(LANG_IS_SHL_ASSIGNMENT))
            || TL::Bool(_ref.get_attribute(LANG_IS_SHR_ASSIGNMENT))
            || TL::Bool(_ref.get_attribute(LANG_IS_AND_ASSIGNMENT))
            || TL::Bool(_ref.get_attribute(LANG_IS_OR_ASSIGNMENT))
            || TL::Bool(_ref.get_attribute(LANG_IS_XOR_ASSIGNMENT))
            || TL::Bool(_ref.get_attribute(LANG_IS_MOD_ASSIGNMENT));

        return b;
    }

    bool Expression::is_function_call()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_FUNCTION_CALL);
        return b;
    }

    Expression Expression::get_called_expression()
    {
        TL::AST_t result = _ref.get_link_to_child(LANG_CALLED_EXPRESSION);
        Expression expr(result, get_scope_link());

        return expr;
    }

    bool Expression::is_named_function_call()
    {
        if (is_function_call())
        {
            Expression expr = get_called_expression();
            Symbol sym = expr.get_ast().get_attribute(LANG_FUNCTION_SYMBOL);
            if (sym.is_valid())
            {
                return true;
            }
        }
        return false;
    }

    Symbol Expression::get_called_entity()
    {
        Expression expr = get_called_expression();
        Symbol result = expr.get_ast().get_attribute(LANG_FUNCTION_SYMBOL);

        return result;
    }

    ObjectList<Expression> Expression::get_argument_list()
    {
        AST_t expression_list = _ref.get_link_to_child(LANG_FUNCTION_ARGUMENTS);

        ObjectList<Expression> result;

        if (expression_list.is_list())
        {
            ASTIterator it = expression_list.get_list_iterator();
            it.rewind();
            while (!it.end())
            {
                Expression expr(it.item(), _scope_link);
                result.push_back(expr);
                it.next();
            }
        }

        return result;
    }

    bool Expression::is_array_subscript()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_ARRAY_SUBSCRIPT);

        return b;
    }

    Expression Expression::get_subscripted_expression()
    {
        TL::AST_t result = _ref.get_link_to_child(LANG_SUBSCRIPTED_EXPRESSION);

        return Expression(result, _scope_link);
    }

    Expression Expression::get_subscript_expression()
    {
        TL::AST_t result = _ref.get_link_to_child(LANG_SUBSCRIPT_EXPRESSION);

        return Expression(result, _scope_link);
    }

    bool Expression::is_casting()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_CAST);

        return b;
    }

    AST_t Expression::get_cast_type()
    {
        AST_t result = _ref.get_link_to_child(LANG_CAST_TYPE);

        return result;
    }

    Expression Expression::get_casted_expression()
    {
        Expression result(_ref.get_link_to_child(LANG_CASTED_EXPRESSION), this->_scope_link);

        return result;
    }

    Expression Expression::get_first_operand()
    {
        AST_t result = _ref.get_link_to_child(LANG_LHS_OPERAND);
        return Expression(result, this->_scope_link);
    }

    Expression Expression::get_second_operand()
    {
        AST_t result = _ref.get_link_to_child(LANG_RHS_OPERAND);
        return Expression(result, this->_scope_link);
    }

    Expression Expression::get_unary_operand()
    {
        AST_t result = _ref.get_link_to_child(LANG_UNARY_OPERAND);

        return Expression(result, this->_scope_link);
    }

    bool Expression::is_member_access()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_MEMBER_ACCESS);
        return b;
    }

    bool Expression::is_this_variable()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_THIS_VARIABLE);
        return b;
    }

    Symbol Expression::get_this_symbol()
    {
        return this->get_symbol();
    }
    
    bool Expression::is_this_access()
    {
        if(is_pointer_member_access())
        {
            Expression l_expr = get_accessed_entity();
            Expression r_expr = get_accessed_member().get_expression();
            return (l_expr.is_this_variable() && r_expr.is_accessed_member());
        }
        return false;
    }

    bool Expression::is_pointer_member_access()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_POINTER_MEMBER_ACCESS);
        return b;
    }

    IdExpression Expression::get_accessed_member()
    {
        TL::AST_t ast = _ref.get_link_to_child(LANG_ACCESSED_MEMBER);

        return IdExpression(ast, this->get_scope_link());
    }

    Expression Expression::get_accessed_entity()
    {
        TL::AST_t ast = _ref.get_link_to_child(LANG_ACCESSED_ENTITY);

        return Expression(ast, this->get_scope_link());
    }

    Expression::OperationKind Expression::get_operation_kind()
    {
        if (is_unary_operation())
        {
            if( TL::Bool(_ref.get_attribute(LANG_IS_PLUS_OP)))
                return PLUS;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_NEGATE_OP)))
                return MINUS;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_NOT_OP)))
                return LOGICAL_NOT;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_COMPLEMENT_OP)))
                return BITWISE_NOT;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_REFERENCE_OP)))
                return REFERENCE;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_DERREFERENCE_OP)))
                return DERREFERENCE;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_PREINCREMENT)))
                return PREINCREMENT;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_PREDECREMENT)))
                return PREDECREMENT;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_POSTINCREMENT)))
                return POSTINCREMENT;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_POSTDECREMENT)))
                return POSTDECREMENT;
        }
        else if (is_binary_operation())
        {
            if (TL::Bool(_ref.get_attribute(LANG_IS_MULT_OP)))
                return MULTIPLICATION;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_DIVISION_OP)))
                return DIVISION;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_MODULUS_OP)))
                return MODULUS;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_ADDITION_OP)))
                return ADDITION;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_SUBSTRACTION_OP)))
                return SUBSTRACTION;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_SHIFT_LEFT_OP)))
                return SHIFT_LEFT;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_SHIFT_RIGHT_OP)))
                return SHIFT_RIGHT;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_LOWER_THAN_OP)))
                return LOWER_THAN;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_GREATER_THAN_OP)))
                return GREATER_THAN;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_GREATER_OR_EQUAL_THAN_OP)))
                return GREATER_EQUAL_THAN;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_LOWER_OR_EQUAL_THAN_OP)))
                return LOWER_EQUAL_THAN;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_EQUAL_OP)))
                return COMPARISON;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_DIFFERENT_OP)))
                return DIFFERENT;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_BITWISE_AND_OP)))
                return BITWISE_AND;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_BITWISE_XOR_OP)))
                return BITWISE_XOR;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_BITWISE_OR_OP)))
                return BITWISE_OR;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_LOGICAL_AND_OP)))
                return LOGICAL_AND;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_LOGICAL_OR_OP)))
                return LOGICAL_OR;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_MUL_ASSIGNMENT)))
                return MULTIPLICATION;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_DIV_ASSIGNMENT)))
                return DIVISION;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_ADD_ASSIGNMENT)))
                return ADDITION;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_SUB_ASSIGNMENT)))
                return SUBSTRACTION;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_SHL_ASSIGNMENT)))
                return SHIFT_LEFT;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_SHR_ASSIGNMENT)))
                return SHIFT_RIGHT;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_AND_ASSIGNMENT)))
                return BITWISE_AND;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_OR_ASSIGNMENT)))
                return BITWISE_OR;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_XOR_ASSIGNMENT)))
                return BITWISE_XOR;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_MOD_ASSIGNMENT)))
                return MODULUS;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_ASSIGNMENT)))
                return ASSIGNMENT;
        }

        return UNKNOWN;
    }

    std::string Expression::get_operator_str()
    {
        switch ((int)this->get_operation_kind())
        {
            case ADDITION :
            case PLUS :
                return "+";
            case MINUS :
            case SUBSTRACTION :
                return "-";
            case LOGICAL_NOT :
                return "!";
            case BITWISE_NOT :
                return "~";
            case REFERENCE :
                return "&";
            case MULTIPLICATION :
            case DERREFERENCE :
                return "*";
            case POSTDECREMENT :
            case PREDECREMENT :
                return "--";
            case PREINCREMENT :
            case POSTINCREMENT :
                return "++";
            case DIVISION :
                return "/";
            case  MODULUS :
                return "%";
            case  SHIFT_LEFT :
                return "<<";
            case  SHIFT_RIGHT :
                return ">>";
            case  LOWER_THAN :
                return "<";
            case  GREATER_THAN :
                return ">";
            case  GREATER_EQUAL_THAN :
                return ">=";
            case  LOWER_EQUAL_THAN :
                return "<=";
            case  COMPARISON :
                return "==";
            case  DIFFERENT :
                return "!=";
            case  BITWISE_AND :
                return "&";
            case  BITWISE_XOR :
                return "^";
            case  BITWISE_OR :
                return "|";
            case  LOGICAL_AND :
                return "&&";
            case  LOGICAL_OR :
                return "||";
            case  ASSIGNMENT :
                return "=";
            default:
                return "??";
        }
    }

    bool Expression::is_conditional()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_CONDITIONAL_EXPRESSION);
        return b;
    }

    Expression Expression::get_condition_expression()
    {
        TL::AST_t condition_expr = _ref.get_link_to_child(LANG_CONDITIONAL_EXPRESSION);
        return Expression(condition_expr, this->get_scope_link());
    }

    Expression Expression::get_true_expression()
    {
        TL::AST_t condition_expr = _ref.get_link_to_child(LANG_CONDITIONAL_TRUE_EXPRESSION);
        return Expression(condition_expr, this->get_scope_link());
    }

    Expression Expression::get_false_expression()
    {
        TL::AST_t condition_expr = _ref.get_link_to_child(LANG_CONDITIONAL_FALSE_EXPRESSION);
        return Expression(condition_expr, this->get_scope_link());
    }

    Type Expression::get_type() const
    {
        bool _dummy;
        return get_type(_dummy);
    }

    Type Expression::get_type(bool &is_lvalue) const
    {
        return Type(NULL);
    }
    
    bool Expression::is_array_section()
    {
        return this->is_array_section_range();
    }

    bool Expression::is_array_section_range()
    {
        TL::Bool result = _ref.get_attribute(LANG_IS_ARRAY_SECTION_RANGE);
        return result;
    }

    bool Expression::is_array_section_size()
    {
        TL::Bool result = _ref.get_attribute(LANG_IS_ARRAY_SECTION_SIZE);
        return result;
    }

    Expression Expression::array_section_item()
    {
        AST_t array_section_item = _ref.get_link_to_child(LANG_ARRAY_SECTION_ITEM);
        return Expression(array_section_item, _scope_link);
    }

    Expression Expression::array_section_lower()
    {
        AST_t array_section_lower = _ref.get_link_to_child(LANG_ARRAY_SECTION_LOWER);
        return Expression(array_section_lower, _scope_link);
    }

    Expression Expression::array_section_upper()
    {
        AST_t array_section_upper = _ref.get_link_to_child(LANG_ARRAY_SECTION_UPPER);
        return Expression(array_section_upper, _scope_link);
    }

    bool Expression::is_shaping_expression()
    {
        TL::Bool result = _ref.get_attribute(LANG_IS_SHAPING_EXPRESSION);
        return result;
    }

    Expression Expression::shaped_expression()
    {
        AST_t shaped_expression = _ref.get_link_to_child(LANG_SHAPED_EXPRESSION);
        return Expression(shaped_expression, _scope_link);
    }

    ObjectList<Expression> Expression::shape_list()
    {
        ObjectList<Expression> result;
        AST_t shape_list = _ref.get_link_to_child(LANG_SHAPE_LIST);

        ASTIterator it = shape_list.get_list_iterator();

        it.rewind();
        while (!it.end())
        {
            Expression expr(it.item(), _scope_link);
            result.push_back(expr);
            it.next();
        }

        return result;
    }

    bool Expression::is_throw_expression()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_THROW_EXPRESSION);
        return b;
    }
    
    Expression Expression::get_throw_expression()
    {
        AST_t throw_ast = _ref.get_link_to_child(LANG_THROW_EXPRESSION);
        Expression throw_expression(throw_ast, _scope_link);
        return throw_expression;
    }

    Expression Expression::get_enclosing_expression()
    {
        AST_t parent = _orig.get_parent();

        TL::Bool is_expr(false);

        is_expr = parent.get_attribute(LANG_IS_EXPRESSION_COMPONENT);

        if (!is_expr)
        {
            return *this;
        }
        else
        {
            return Expression(parent, this->_scope_link);
        }
    }


    bool Expression::is_top_level_expression()
    {
        AST_t parent = _orig.get_parent();

        TL::Bool is_expr(false);

        is_expr = parent.get_attribute(LANG_IS_EXPRESSION_COMPONENT);

        if (!is_expr)
        {
            return true;
        }
        else
        {
            return false;
        }
    }

    Expression Expression::get_top_enclosing_expression()
    {
        Expression result (*this);
        while (!result.is_top_level_expression())
        {
            result = result.get_enclosing_expression();
        }

        return result;
    }

    bool Expression::is_constant()
    {
        return false;
    }

    int Expression::evaluate_constant_int_expression(bool &valid)
    {
        return 0;
    }

    //! States if the frontend tagged this expression with a related symbol
    bool Expression::has_symbol()
    {
        return false;
    }

    //! Returns the symbol with which the frontend tagged this expression
    Symbol Expression::get_symbol()
    {
        return Symbol(NULL);
    }

    bool Expression::is_sizeof()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_SIZEOF);
        return b;
    }

    bool Expression::is_sizeof_typeid()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_SIZEOF_TYPEID);
        return b;
    }

    // Do not use this one, instead use get_declared_symbol
    // since this one will not work for type-names
    IdExpression DeclaredEntity::get_declared_entity() const
    {
        AST_t declared_name = _ref.get_link_to_child(LANG_DECLARED_NAME);

        Source declared_name_str = declared_name.prettyprint();

        Scope sc = this->_scope_link.get_scope(declared_name);

        AST_t expression_ast = declared_name_str.parse_expression(this->_ref, this->_scope_link);

        Expression expression(expression_ast, this->_scope_link);

        return expression.get_id_expression();
    }

    AST_t DeclaredEntity::get_declared_tree() const
    {
        AST_t declared_name = _ref.get_link_to_child(LANG_DECLARED_NAME);
        return declared_name;
    }

    Symbol DeclaredEntity::get_declared_symbol() const
    {
        AST_t declared_name = _ref.get_link_to_child(LANG_DECLARED_NAME);
        TL::Symbol sym = declared_name.get_attribute(LANG_DECLARED_SYMBOL);

        if (!sym.is_valid())
        {
            std::cerr << "Warning: declared name '" << declared_name.prettyprint() 
                << " at " << declared_name.get_locus() 
                << " does not have a valid symbol. This is likely a bug." << std::endl;
        }

        return sym;
    }

    DeclarationSpec Declaration::get_declaration_specifiers() const
    {
        AST_t declaration_specifiers = _ref.get_link_to_child(LANG_DECLARATION_SPECIFIERS);

        DeclarationSpec result(declaration_specifiers, this->_scope_link);

        return result;
    }

    TypeSpec DeclarationSpec::get_type_spec() const
    {
        AST_t tree = _ref.get_link_to_child(LANG_TYPE_SPECIFIER);

        return TypeSpec(tree, this->_scope_link);
    }
   
    bool TypeSpec::is_class_specifier() const
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_CLASS_SPECIFIER);
        return b;
    }

    Symbol TypeSpec::get_class_symbol() const
    {
        TL::Symbol sym = _ref.get_attribute(LANG_CLASS_SPECIFIER_SYMBOL);
        return sym;
    }

    bool TypeSpec::is_enum_specifier() const
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_ENUM_SPECIFIER);
        return b;
    }

    Symbol TypeSpec::get_enum_symbol() const
    {
        TL::Symbol sym = _ref.get_attribute(LANG_ENUM_SPECIFIER_SYMBOL);
        return sym;
    }

    Type TypeSpec::get_type() const
    {
        TL::Type t = _ref.get_attribute(LANG_TYPE_SPECIFIER_TYPE);
        return t;
    }

    bool DeclaredEntity::has_initializer() const
    {
        AST_t initializer = _ref.get_link_to_child(LANG_INITIALIZER);
        return initializer.is_valid();
    }

    Expression DeclaredEntity::get_initializer() const
    {
        AST_t initializer = _ref.get_link_to_child(LANG_INITIALIZER);
        return Expression(initializer, this->_scope_link);
    }

    AST_t DeclaredEntity::get_declarator_tree() const
    {
        AST_t declarator = _ref.get_link_to_child(LANG_DECLARATOR);
        return declarator;
    }

    ObjectList<DeclaredEntity> Declaration::get_declared_entities() const
    {
        PredicateAttr lang_declared_name_pred(LANG_IS_DECLARED_NAME);

        AST_t declarators_tree = this->_ref.get_link_to_child(LANG_DECLARATION_DECLARATORS);

        ObjectList<AST_t> declared_symbols =
            declarators_tree.depth_subtrees(lang_declared_name_pred, AST_t::NON_RECURSIVE);

        ObjectList<DeclaredEntity> result;
        for (ObjectList<AST_t>::iterator it = declared_symbols.begin();
                it != declared_symbols.end();
                it++)
        {
            DeclaredEntity declared(*it, this->_scope_link);

            result.push_back(declared);
        }

        return result;
    }

    bool DeclaredEntity::is_functional_declaration() const
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_FUNCTIONAL_DECLARATOR);
        return b;
    }

    // A traverse functor only for the first level of parameter declarations
    // (We want to avoid finding inner parameters of pointer-to-function
    // parameters)
    class TraverseParameters : public Functor<ASTTraversalResult, AST_t>
    {
        private:
            PredicateAttr _pred;
        public:
            TraverseParameters()
                : _pred(LANG_IS_PARAMETER_DECLARATION)
            {
            }

            ASTTraversalResult do_(TraverseParameters::ArgType a) const
            {
                bool match = _pred(a);
                bool recurse = !match;

                return ast_traversal_result_helper(match, recurse);
            }
    };

    bool DeclaredEntity::functional_declaration_lacks_prototype() const
    {
        C_LANGUAGE()
        {
            TraverseParameters traverse_parameter_declarations;
            ObjectList<AST_t> parameter_declarations = _ref.depth_subtrees(traverse_parameter_declarations);

            return parameter_declarations.empty();
        }

        return false;
    }

    ObjectList<ParameterDeclaration> DeclaredEntity::get_parameter_declarations() const
    {
        bool _dummy;
        return get_parameter_declarations(_dummy);
    }

    ObjectList<ParameterDeclaration> DeclaredEntity::get_parameter_declarations(bool &has_ellipsis) const
    {
        ObjectList<ParameterDeclaration> result;

        Symbol symbol = get_declared_symbol();
        Type type = symbol.get_type();

        if (!type.is_function())
        {
            std::cerr 
                << "Error: Entity '" << _ref.prettyprint() << "' in '" 
                << _ref.get_locus() 
                << "' is not a function type" 
                << std::endl;
            return result;
        }
        ObjectList<Type> parameter_types = type.parameters(has_ellipsis);

        // This avoids problems with the case 'f(void)' where we have
        // some sort of declarations but the computed type does not.
        if (parameter_types.empty())
        {
            return result;
        }

        TraverseParameters traverse_parameter_declarations;
        ObjectList<AST_t> parameter_declarations = _ref.depth_subtrees(traverse_parameter_declarations);

        ObjectList<Type>::iterator it_type = parameter_types.begin();
        ObjectList<AST_t>::iterator it_tree = parameter_declarations.begin();
        while (it_tree != parameter_declarations.end())
        {
            ParameterDeclaration parameter_decl(*it_tree, _scope_link, *it_type);
            result.append(parameter_decl);

            it_tree++;
            it_type++;
        }

        return result;
    }

    bool ParameterDeclaration::is_named() const
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_NAMED_PARAMETER_DECLARATION);
        return b;
    }

    IdExpression ParameterDeclaration::get_name() const
    {
        AST_t declaration = _ref.get_link_to_child(LANG_PARAMETER_DECLARATION_NAME);
        return IdExpression(declaration, _scope_link);
    }

    Expression IdExpression::get_expression() const
    {
        return Expression(get_ast(), get_scope_link());
    }

    ObjectList<GCCAttribute> GCCAttributeSpecifier::get_gcc_attribute_list() const
    {
        ObjectList<GCCAttribute> result;

        AST_t list = _ref.get_link_to_child(LANG_GCC_ATTRIBUTE_LIST);

        if (list.is_valid())
        {
            ASTIterator iterator = list.get_list_iterator();

            iterator.rewind();
            while (!iterator.end())
            {
                GCCAttribute current_attribute(iterator.item(), _scope_link);
                result.append(current_attribute);

                iterator.next();
            }
        }

        return result;
    }

    std::string GCCAttribute::get_name() const
    {
        AST_t tree = _ref.get_link_to_child(LANG_GCC_ATTRIBUTE_VALUE_NAME);
        return tree.get_text();
    }

    bool GCCAttribute::has_argument_list() const
    {
        AST_t tree = _ref.get_link_to_child(LANG_GCC_ATTRIBUTE_VALUE_ARGS);

        return tree.is_valid();
    }

    ObjectList<Expression> GCCAttribute::get_argument_list() const
    {
        ObjectList<Expression> result;
        AST_t tree = _ref.get_link_to_child(LANG_GCC_ATTRIBUTE_VALUE_ARGS);

        ASTIterator it = tree.get_list_iterator();
        it.rewind();

        while (!it.end())
        {
            Expression expr(it.item(), _scope_link);
            result.append(expr);

            it.next();
        }

        return result;
    }

    void ReplaceSrcIdExpression::set_replace_declarators(bool b)
    {
        _do_not_replace_declarators = !b;
    }

    void ReplaceSrcIdExpression::set_ignore_pragma(bool b)
    {
        _ignore_pragmas = b;
    }

    void ReplaceSrcIdExpression::add_replacement(Symbol sym, const std::string& str)
    {
        _repl_map[sym] = str;
    }

    bool ReplaceSrcIdExpression::has_replacement(Symbol sym) const
    {
        return _repl_map.find(sym) != _repl_map.end();
    }

    std::string ReplaceSrcIdExpression::get_replacement(Symbol sym) const
    {
        if (!this->has_replacement(sym))
        {
            // Return something useful
            return sym.get_qualified_name();
        }
        else
        {
            return _repl_map.find(sym)->second;
        }
    }

    ScopeLink ReplaceSrcIdExpression::get_scope_link() const
    {
        return _sl;
    }

    Source ReplaceSrcIdExpression::replace(AST_t a) const
    {
        Source result;

        const char *c = prettyprint_in_buffer_callback(a.get_internal_ast(), 
                &ReplaceSrcIdExpression::prettyprint_callback, (void*)this);

        // Not sure whether this could happen or not
        if (c != NULL)
        {
            result << std::string(c);
        }

        // The returned pointer came from C code, so 'free' it
        DELETE((void*)c);

        return result;
    }

    Source ReplaceSrcIdExpression::replace(LangConstruct a) const
    {
        return this->replace(a.get_ast());
    }

    const char* ReplaceSrcIdExpression::prettyprint_callback(AST a, void* data)
    {
        ReplaceSrcIdExpression *_this = reinterpret_cast<ReplaceSrcIdExpression*>(data);

        AST_t wrapped_tree(a);

        if (_this->_ignore_pragmas)
        {
            bool b = TL::Bool(wrapped_tree.get_attribute(LANG_IS_PRAGMA_CUSTOM_DIRECTIVE));

            if (b)
            {
                std::string str = wrapped_tree.prettyprint() + "\n";
                return uniquestr(str.c_str());
            }
        }

        if (IdExpression::predicate(wrapped_tree))
        {
            IdExpression id_expr(wrapped_tree, _this->_sl);
            Symbol sym = id_expr.get_symbol();

            if (_this->_repl_map.find(sym) != _this->_repl_map.end())
            {
                const char* result = _this->_repl_map[sym].c_str();
                return result;
            }
        }
        else if ((_this->_repl_this != "")
                && PredicateAttr(LANG_IS_THIS_VARIABLE)(wrapped_tree))
        {
            return _this->_repl_this.c_str();
        }
        else if (!_this->_do_not_replace_declarators)
        {
            Symbol sym = wrapped_tree.get_attribute(LANG_DECLARED_SYMBOL);
            if (sym.is_valid())
            {
                if (_this->_repl_map.find(sym) != _this->_repl_map.end())
                {
                    const char* result = _this->_repl_map[sym].c_str();
                    return result;
                }
            }
        }

        return NULL;
    }

    void ReplaceSrcIdExpression::add_this_replacement(const std::string& str)
    {
        _repl_this = str;
    }

    ObjectList<TemplateParameterConstruct> TemplateHeader::get_parameters() const
    {
        ObjectList<TemplateParameterConstruct> result;
        ASTIterator it = _ref.get_list_iterator();
        it.rewind();

        while (!it.end())
        {
            result.append(TemplateParameterConstruct(it.item(), _scope_link));
            it.next();
        }

        return result;
    }

    std::string TemplateHeader::prettyprint() const
    {
        ObjectList<TemplateParameterConstruct> list = this->get_parameters();

        std::string res = "template<";
        res += concat_strings(list, ",");
        res += ">";

        return res;
    }

    bool TemplateParameterConstruct::is_named() const
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_NAMED_TEMPLATE_PARAMETER);
        return b;
    }

    std::string TemplateParameterConstruct::get_name() const
    {
        TL::AST_t a = _ref.get_link_to_child(LANG_TEMPLATE_PARAMETER_NAME);
        return a.prettyprint();
    }

    bool TemplateParameterConstruct::is_type() const
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_TYPE_TEMPLATE_PARAMETER);
        return b;
    }

    bool TemplateParameterConstruct::is_nontype() const
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_NONTYPE_TEMPLATE_PARAMETER);
        return b;
    }

    bool TemplateParameterConstruct::is_template() const
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_TEMPLATE_TEMPLATE_PARAMETER);
        return b;
    }

    Symbol TemplateParameterConstruct::get_symbol() const
    {
        TL::Symbol sym = _ref.get_attribute(LANG_TEMPLATE_PARAMETER_SYMBOL);
        return sym;
    }
}
