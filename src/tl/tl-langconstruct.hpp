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

    //! Base class representing a distinguished language construct
    /*!
     * A LangConstruct is always composed of a tree, TL::AST_t, and a
     * scope link, TL::ScopeLink.
     *
     * This is the base class for all classes wrapping distinguished
     * language constructs.
     */
    class LangConstruct
    {
        protected:
            //! Wrapped tree
            AST_t _ref;
            //! Wrapped scope link
            ScopeLink _scope_link;
        public:
            //! Constructor
            LangConstruct(AST_t ref, ScopeLink scope_link)
                : _ref(ref), _scope_link(scope_link)
            {
            }

            //! Used when requesting referenced symbols in a construct
            enum SymbolsWanted
            {
                //! All symbols wanted
                ALL_SYMBOLS = 0,
                //! Only variables (or objects)
                ONLY_OBJECTS,
                //! Synonym for ONLY_OBJECTS
                ONLY_VARIABLES = ONLY_OBJECTS, // A useful alias
                //! Only function references
                ONLY_FUNCTIONS
            };

            //! Convenience function to prettyprint the tree
            /*!
             * This function just calls AST_t::prettyprint(bool) on the underlying
             * tree
             */
            std::string prettyprint();

            //! Returns the wrapped tree of this LangConstruct
            AST_t get_ast()
            {
                return _ref;
            }

            //! Returns the scope link of this LangConstruct
            ScopeLink get_scope_link()
            {
                return _scope_link;
            }

            //! Returns the scope of this LangConstruct
            Scope get_scope()
            {
                return _scope_link.get_scope(_ref);
            }

            //! Returns the enclosing function definition
            FunctionDefinition get_enclosing_function();

            //! Returns a list of all symbolic occurrences
            ObjectList<IdExpression> all_symbol_occurrences(SymbolsWanted symbols = ALL_SYMBOLS);
            //! Returns a list of all symbolic occurrences that are not defined
            //within this construction
            ObjectList<IdExpression> non_local_symbol_occurrences(SymbolsWanted symbols = ALL_SYMBOLS);
            /*!
             * \bug Not implemented
             */
            ObjectList<IdExpression> local_symbol_occurrences();

            //! Common predicate to all LangConstruct
            /*!
             * All LangConstruct classes should have a predicate that
             * states whether the tree is valid to be wrapped in such
             * a LangConstruct. 
             *
             * Currently this is not used too much since some trees
             * have complex predicates stating their validity
             * (e.g. TL::Expression)
             */
            const static AlwaysFalse<AST_t> predicate;

            virtual ~LangConstruct()
            {
            }
    };

    class Declaration;

    //! Used in OpenMP and custom pragma constructs
    //to get symbols named in clauses 
    enum IdExpressionCriteria
    {
        // Only consider the valid symbols
        VALID_SYMBOLS = 0,
        // Consider only invalid names
        INVALID_SYMBOLS,
        // Consider any symbolic name, either valid or invalid
        ALL_FOUND_SYMBOLS
    };

    //! Wraps an id-expression in C++ or a symbol name in C
    /*!
     * This is an "all-pervasive" class throughout all the TL namespace.  It
     * actually wraps a reference (or occurence) to an identifier.  Here
     * identifier can be either a type or variable
     */
    class IdExpression : public LangConstruct
    {
        private:
        public:
            IdExpression(AST_t ref, ScopeLink scope_link)
                : LangConstruct(ref, scope_link)
            {
            }

            //! Returns a string that mangles a complex id-expression
            /*!
             * This function can be used to get unique names of complex
             * id-expression references. For instance,
             *
             *  '%A::%B::%C\<int\>::%D' will be converted into 'A__B__C_int___D'
             *
             * Note that this only concerns to occurrences. The fact that in
             * C++ a same entity can be named in different ways is not the
             * a responsability of this class.
             */
            std::string mangle_id_expression() const;

            //! Gets the qualified part of an id-expression
            /*! Use this function to get all but the unqualified last part
             * of any id-expression. So,
             *
             *  '%A::%B::%C::%D' will return '%A::%B::%C::'
             */
            std::string get_qualified_part() const;
            //! Gets the unqualified part of an id-expression
            /*! Use this function to get the last unqualified part
             * of any id-expression.
             * \param with_template_id If set to true the last template-id will be
             * included too.
             *
             * '%A::%B::%C\<int\>' will return '%C' unless \a with_template_id is set to true,
             * in this latter case it would return 'C\<int\>'
             */
            std::string get_unqualified_part(bool with_template_id = false) const;

            //! States whether this id-expression is qualified
            /*!
             *   '%A::%B' is qualified
             *   '%C' is not qualified
             */
            bool is_qualified() const;
            //! States whether this id-expression is unqualified
            /*!
             *   '%A::%B' is not unqualified
             *   '%C' is unqualified
             */
            bool is_unqualified() const;

            //! States whether this id-expression is a template-id
            /*!
             *   '%A\<int\>' is a template-id
             */
            bool is_template_id() const;
            //! Returns the template-name of the template-id
            /*!  
             *   '%A\<int\>' would return 'A'
             */
            std::string get_template_name() const;
            //! Returns the template-name of the template-id
            /*!  
             *   '%A\<int\>' would return '\<int\>'
             */
            std::string get_template_arguments() const;

            //! Returns the symbol using a lookup
            /*!
             * \bug Note that this function is not quite exact
             * because some symbols are hidden within the same
             * context
             *
             *   {
             *     int %a;
             *     {
             *       %a = 3;
             *
             *       float a;
             *       %a = 4.3;
             *     }
             *   }
             *
             * Both id-expression of 'a' would solve to the innermost 'float a'
             * Maybe in the future a more exact approach will be used.
             */
            Symbol get_symbol() const;

            //! Returns the AST of this id-expression
            AST_t get_ast() const;

            //! Returns a declaration of the symbol
            /*!
             * \bug This function uses get_symbol
             */
            Declaration get_declaration();

            //! Predicate for an IdExpression
            static const PredicateAST<LANG_IS_ID_EXPRESSION> predicate;
    };

    //! LangConstruct that wraps a statement in the code
    class Statement : public LangConstruct
    {
        private:
        public:
            Statement(AST_t ref, ScopeLink scope_link)
                : LangConstruct(ref, scope_link)
            {
            }

            //! Returns all non local referenced symbols
            //in the statement
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

            AST_t get_point_of_declaration();

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
