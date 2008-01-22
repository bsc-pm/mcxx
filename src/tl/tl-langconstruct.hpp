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
#ifndef TL_LANGCONSTRUCT_HPP
#define TL_LANGCONSTRUCT_HPP

#include "tl-ast.hpp"
#include "tl-symbol.hpp"
#include "tl-scopelink.hpp"
#include "tl-builtin.hpp"
#include "tl-source.hpp"
#include "cxx-attrnames.h"
#include "cxx-macros.h"
#include <string>
#include <utility>

namespace TL
{
    class FunctionDefinition;
    class IdExpression;
    class LangConstruct
    {
        protected:
            AST_t _ref;
            ScopeLink _scope_link;
        public:
            LangConstruct(AST_t ref, ScopeLink scope_link)
                : _ref(ref), _scope_link(scope_link)
            {
            }

            enum SymbolsWanted
            {
                ALL_SYMBOLS = 0,
                ONLY_OBJECTS,
                ONLY_VARIABLES = ONLY_OBJECTS, // A useful alias
                ONLY_FUNCTIONS
            };

            std::string prettyprint();

            AST_t get_ast()
            {
                return _ref;
            }

            ScopeLink get_scope_link()
            {
                return _scope_link;
            }

            Scope get_scope()
            {
                return _scope_link.get_scope(_ref);
            }

            FunctionDefinition get_enclosing_function();

            ObjectList<IdExpression> all_symbol_occurrences(SymbolsWanted symbols = ALL_SYMBOLS);
            ObjectList<IdExpression> non_local_symbol_occurrences(SymbolsWanted symbols = ALL_SYMBOLS);
            ObjectList<IdExpression> local_symbol_occurrences();

            const static AlwaysFalse<AST_t> predicate;

            virtual ~LangConstruct()
            {
            }
    };

    class Declaration;

    enum IdExpressionCriteria
    {
        VALID_SYMBOLS = 0,
        INVALID_SYMBOLS,
        ALL_FOUND_SYMBOLS
    };

    class IdExpression : public LangConstruct
    {
        private:
        public:
            IdExpression(AST_t ref, ScopeLink scope_link)
                : LangConstruct(ref, scope_link)
            {
            }

            std::string mangle_id_expression() const;

            std::string get_qualified_part() const;
            std::string get_unqualified_part(bool with_template_id = false) const;

            bool is_qualified() const;
            bool is_unqualified() const;

            bool is_template_id() const;
            std::string get_template_name() const;
            std::string get_template_arguments() const;

            Symbol get_symbol() const;
            AST_t get_ast() const;

            Declaration get_declaration();

            static const PredicateAST<LANG_IS_ID_EXPRESSION> predicate;
    };

    class Statement : public LangConstruct
    {
        private:
        public:
            Statement(AST_t ref, ScopeLink scope_link)
                : LangConstruct(ref, scope_link)
            {
            }
             ObjectList<Symbol> non_local_symbols();

            bool is_compound_statement();
            ObjectList<Statement> get_inner_statements();

            const static PredicateAST<LANG_IS_STATEMENT> predicate;
    };

    class Expression;
    class ForStatement : public Statement
    {
        private:
            AST_t _induction_variable;
            AST_t _lower_bound;
            AST_t _upper_bound;
            AST_t _step;

            void gather_for_information();
            bool check_statement();
        public:
            ForStatement(AST_t ref, ScopeLink scope_link)
                : Statement(ref, scope_link)
            {
                if (check_statement())
                {
                    gather_for_information();
                }
            }

            ForStatement(const Statement& st)
                 : Statement(st)
            {
                if (check_statement())
                {
                    gather_for_information();
                }
            }

            IdExpression get_induction_variable();
            Expression get_lower_bound();
            Expression get_upper_bound();
            Expression get_step();

            Statement get_loop_body();

            bool regular_loop();

            AST_t get_iterating_init();
            Expression get_iterating_condition();
            Expression get_iterating_expression();

            const static PredicateAST<LANG_IS_FOR_STATEMENT> predicate;
    };

    class DeclaredEntity;
    class FunctionDefinition : public LangConstruct
    {
        private:
        public:
            void prepend_sibling(AST_t);

            FunctionDefinition(AST_t ref, ScopeLink scope_link)
                : LangConstruct(ref, scope_link)
            {
            }

            IdExpression get_function_name();
            Statement get_function_body();

            bool is_templated() const;
            ObjectList<AST_t> get_template_header();

            DeclaredEntity get_declared_entity();

            AST_t get_point_of_declaration();

            static const PredicateAST<LANG_IS_FUNCTION_DEFINITION> predicate;
    };

    class Expression : public LangConstruct
    {
        private:
            static AST_t advance_over_nests(AST_t);
        public :
            enum OperationKind
            {
                UNKNOWN = 0,
                DERREFERENCE,
                REFERENCE,
                PLUS,
                MINUS,
                ADDITION,
                SUBSTRACTION,
                MULTIPLICATION,
                DIVISION,
                MODULUS,
                SHIFT_LEFT,
                SHIFT_RIGHT,
                LOGICAL_OR,
                LOGICAL_AND,
                LOGICAL_NOT,
                BITWISE_OR,
                BITWISE_AND,
                BITWISE_XOR,
                BITWISE_NOT,
                LOWER_THAN,
                GREATER_THAN,
                LOWER_EQUAL_THAN,
                GREATER_EQUAL_THAN,
                COMPARISON,
                DIFFERENT,
                PREINCREMENT,
                POSTINCREMENT,
                PREDECREMENT,
                POSTDECREMENT,
				CONDITIONAL
            };

            Type get_type();
            Type get_type(bool &is_lvalue);

            Expression(AST_t ref, ScopeLink scope_link)
                : LangConstruct(ref, scope_link)
            {
                this->_ref = advance_over_nests(this->_ref);
            }

            bool is_id_expression();
            IdExpression get_id_expression();

            bool is_binary_operation();
            Expression get_first_operand();
            Expression get_second_operand();

            bool is_unary_operation();
            Expression get_unary_operand();

            // Casting
            bool is_casting();
            AST_t get_cast_type();
            Expression get_casted_expression();

            // Literal
            bool is_literal();

            // exprC(expr-list)
            bool is_function_call();
            // exprC
            Expression get_called_expression();
            // expr-list
            ObjectList<Expression> get_argument_list();

            // expr = expr
            bool is_assignment();

            // expr op= expr
            bool is_operation_assignment();

            // exprA[exprB]
            bool is_array_subscript();
            // exprB
            Expression get_subscript_expression();
            // exprA
            Expression get_subscripted_expression();

            // exprA.exprM
            bool is_member_access();
            // exprA->exprM
            bool is_pointer_member_access();
            // exprA
            Expression get_accessed_entity();
            // exprM
            IdExpression get_accessed_member();

			// exprC ? exprA : exprB
			bool is_conditional();
			// exprC
			Expression get_condition_expression();
			// exprA
			Expression get_true_expression();
			// exprB
			Expression get_false_expression();

            OperationKind get_operation_kind();

			std::string get_operator_str();

            static const PredicateAST<LANG_IS_EXPRESSION_NEST> predicate;
    };

    class ParameterDeclaration : public LangConstruct
    {
        private:
            Type _type;
        public:
            ParameterDeclaration(AST_t tree, ScopeLink sl, Type parameter_type)
                : LangConstruct(tree, sl), _type(parameter_type)
            {
            }

            bool is_named();
            IdExpression get_name();

            Type get_type()
            {
                return _type;
            }

            static const PredicateAST<LANG_IS_PARAMETER_DECLARATION> predicate;
    };

    class DeclaredEntity : public LangConstruct
    {
        public :
            DeclaredEntity(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }

            IdExpression get_declared_entity() DEPRECATED;
            Symbol get_declared_symbol();
            AST_t get_declared_tree();

            bool has_initializer();
            Expression get_initializer();

            bool is_functional_declaration();
            ObjectList<ParameterDeclaration> get_parameter_declarations();
            ObjectList<ParameterDeclaration> get_parameter_declarations(bool &has_ellipsis);

            static const PredicateAST<LANG_IS_DECLARED_NAME> predicate;
    };

    class TypeSpec : public LangConstruct
    {
        public:
            TypeSpec(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }

            bool is_class_specifier();
            Symbol get_class_symbol();

            bool is_enum_specifier();
            Symbol get_enum_symbol();
    };

    class DeclarationSpec : public LangConstruct
    {
        public:
            DeclarationSpec(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }

            TypeSpec get_type_spec();

            // No predicate for this one at the moment
    };

    class Declaration : public LangConstruct
    {
        public:
            Declaration(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }

            ObjectList<DeclaredEntity> get_declared_entities();
            DeclarationSpec get_declaration_specifiers();

            bool is_templated();
            ObjectList<AST_t> get_template_header();

            static const PredicateAST<LANG_IS_DECLARATION> predicate;
    };

    class ReplaceIdExpression : public ObjectList<std::pair<Symbol, AST_t> >
    {
        private:
            std::map<Symbol, AST_t> _repl_map;
        public:
            ReplaceIdExpression()
            {
            }

            void add_replacement(Symbol sym, AST_t ast);
            void add_replacement(Symbol sym, std::string str) DEPRECATED;
            void add_replacement(Symbol sym, Source src) DEPRECATED;

            void add_replacement(Symbol sym, std::string str, AST_t ref_tree, ScopeLink scope_link);
            void add_replacement(Symbol sym, Source src, AST_t ref_tree, ScopeLink scope_link);

            bool has_replacement(Symbol sym);

            template <class T>
            T replace(T orig_stmt)
            {
                std::pair<AST_t, ScopeLink> modified_statement = 
                    orig_stmt.get_ast().duplicate_with_scope(orig_stmt.get_scope_link());

                T result(modified_statement.first, modified_statement.second);

                ObjectList<IdExpression> id_expressions = result.non_local_symbol_occurrences();

                for (ObjectList<IdExpression>::iterator it = id_expressions.begin();
                        it != id_expressions.end();
                        it++)
                {
                    Symbol sym = it->get_symbol();

                    if (_repl_map.find(sym) != _repl_map.end())
                    {
                        AST_t repl_ast = _repl_map[sym];
                        AST_t orig_ast = it->get_ast();

                        orig_ast.replace_with(repl_ast);
                    }
                }

                return result;
            }
    };

    // This is something common, it is a good candidate to be taken off here
    class GetSymbolFromAST : public Functor<Symbol, AST_t>
    {
        private:
            ScopeLink scope_link;
        public:
            virtual Symbol operator()(AST_t& ast) const 
            {
                Scope sc = scope_link.get_scope(ast);

                Symbol result = sc.get_symbol_from_id_expr(ast);

                return result;
            }

            GetSymbolFromAST(ScopeLink _scope_link)
                : scope_link(_scope_link)
            {
            }

            ~GetSymbolFromAST()
            {
            }
    };
}

#endif // TL_LANGCONSTRUCT_HPP
