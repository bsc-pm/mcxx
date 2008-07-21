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

    //! \addtogroup LangConstruct Language construction wrappers
    //! @{

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
             * Maybe in the future a more exact approach will be used. You can use
             * get_computed_symbol to use the symbol deduced by the compiler.
             */
            Symbol get_symbol() const;

            //! Gets the computed symbol by the frontend.
            /*! 
             * This function is more exact than get_symbol but only works on
             * real expressions (not on id-expressions coming from declarators
             * or other non-expression contexts). It does not perform
             * a search in the scope like get_symbol.
             *
             * \bug Please, do not use unless told to do so. This function
             * is fully untested.
             */
            Symbol get_computed_symbol() const;

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


    class Expression;
    class Statement;

    class DeclaredEntity;
    //! This function wraps a whole function definition
    class FunctionDefinition : public LangConstruct
    {
        private:
        public:
            FunctionDefinition(AST_t ref, ScopeLink scope_link)
                : LangConstruct(ref, scope_link)
            {
            }
            
            //! Prepends a tree as a sibling of this function definition
            void prepend_sibling(AST_t);

            //! Returns an id-expression with the function name as it appears
            //in the function definition.
            /*!
             * Member functions defined outside the class will return a qualified id-expression
             */
            IdExpression get_function_name();

            //! Returns the function body
            Statement get_function_body();

            //! States whether this function definition is templated
            bool is_templated() const;
            //! Returns a list of templated headers for this template declaration
            ObjectList<AST_t> get_template_header();

            //! Returns the declared entity of this function definition.
            /*!
             * This is the function name itself
             */
            DeclaredEntity get_declared_entity();

            //! Returns the point of declaration
            AST_t get_point_of_declaration();

            static const PredicateAST<LANG_IS_FUNCTION_DEFINITION> predicate;
    };

    //! This LangConstruct wraps an expression in the language
    /*!
     * This wrapper ignores all parentheses and nested constructions.
     */
    class Expression : public LangConstruct
    {
        private:
            AST_t _orig;
            //! Advances over expression nests
            static AST_t advance_over_nests(AST_t);
        public :
            //! Enum used to get the kind of an operation
            enum OperationKind
            {
                UNKNOWN = 0,
                //! Derreference operator
                DERREFERENCE,
                //! Reference operator
                REFERENCE,
                //! Unary plus
                PLUS,
                //! Unary minus
                MINUS,
                //! Arithmetic addition
                ADDITION,
                //! Arithmetic substraction
                SUBSTRACTION,
                //! Arithmetic multiplication
                MULTIPLICATION,
                //! Arithmetic division
                DIVISION,
                //! Integer modulus
                MODULUS,
                //! Integer shift left
                SHIFT_LEFT,
                //! Integer shift right
                SHIFT_RIGHT,
                //! Logical or ||
                LOGICAL_OR,
                //! Logical and &&
                LOGICAL_AND,
                //! Logical not !
                LOGICAL_NOT,
                //! Bitwise or |
                BITWISE_OR,
                //! Bitwise and &
                BITWISE_AND,
                //! Bitwise xor ^
                BITWISE_XOR,
                //! Bitwise not ~
                BITWISE_NOT,
                //! Lower than operator \<
                LOWER_THAN,
                //! Greater than operator \>
                GREATER_THAN,
                //! Lower equal than operator \<=
                LOWER_EQUAL_THAN,
                //! Greater equal than operator \>=
                GREATER_EQUAL_THAN,
                //! Comparison operator ==
                COMPARISON,
                //! Different operator !=
                DIFFERENT,
                //! Preincrement ++a
                PREINCREMENT,
                //! Postincrement a++
                POSTINCREMENT,
                //! Predecrement --a
                PREDECREMENT,
                //! Postdecrement a--
                POSTDECREMENT,
                //! Conditional expression a ? b : c
                CONDITIONAL
            };

            //! Computes the type of the expression
            Type get_type();
            //! Computes the type of the expression
            /*!
             * \param is_lvalue Will hold whether this is a lvalue
             */
            Type get_type(bool &is_lvalue);

            Expression(AST_t ref, ScopeLink scope_link)
                : LangConstruct(ref, scope_link)
            {
                this->_orig = this->_ref;
                this->_ref = advance_over_nests(this->_ref);
            }

            //! States whether this is an id-expression
            bool is_id_expression();
            //! Returns an id-expression
            IdExpression get_id_expression();

            //! States whether this is a binary operation
            bool is_binary_operation();
            //! Returns the first operand of a binary operation
            Expression get_first_operand();
            //! Returns the second operand of a binary operation
            Expression get_second_operand();

            //! States whether this is an unary operation
            bool is_unary_operation();
            //! Returns the unary operand of this unary operation
            Expression get_unary_operand();

            //! States whether this is a casting
            bool is_casting();
            //! Returns the cast type
            AST_t get_cast_type();
            //! Returns the casted expression
            Expression get_casted_expression();

            //! States whether the expression is a literal
            bool is_literal();

            //! States whether this is a function call
            bool is_function_call();
            //! Returns the called expression
            /*!
             * This returns part before the parentheses of the arguments
             */
            Expression get_called_expression();
            //! Returns the argument list
            ObjectList<Expression> get_argument_list();

            //! States whether this is an assignment expression
            bool is_assignment();

            //! States whether this is an assignment operation expression
            bool is_operation_assignment();

            //! States whether this is an array subscript expression
            bool is_array_subscript();
            //! Returns the subscript expression
            /*!
             * Of an expression 'e1[e2]', this function returns 'e2'
             */
            Expression get_subscript_expression();
            //! Returns the subscripted expression
            /*!
             * Of an expression 'e1[e2]', this function returns 'e1'
             */
            Expression get_subscripted_expression();

            //! States whether this is a member access expression
            /*!
             * \return True if the expression is of the form 'a.b'
             */
            bool is_member_access();
            //! States whether this is a pointer member access expression
            /*!
             * \return True if the expression is of the form 'a->b'
             */
            bool is_pointer_member_access();
            // Return the accessed entity
            /*
             * In both 'a.b' and 'a->b' this function returns 'a'.
             */
            Expression get_accessed_entity();
            // Return the accessed field/member
            /*
             * In both 'a.b' and 'a->b' this function returns 'b'
             */
            IdExpression get_accessed_member();

            //! States whether this is a conditional expression
            bool is_conditional();
            //! Returns the condition expression of a conditional expression
            /*!
             * Given an expression 'a ? b : c', this function returns 'a'
             */
            Expression get_condition_expression();
            //! Returns the expression evaluated when the condition expression is true
            /*!
             * Given an expression 'a ? b : c', this function returns 'b'
             */
            Expression get_true_expression();
            //! Returns the expression evaluated when the condition expression is false
            /*!
             * Given an expression 'a ? b : c', this function returns 'c'
             */
            Expression get_false_expression();

            //! Returns the operation kind
            OperationKind get_operation_kind();

            //! Returns a string representing the involved operator
            /*!
             * Operator assignment expressions like '+=' will return '+'
             * as the involved operator.
             */
            std::string get_operator_str();

            //!States whether the expression is an array section
            bool is_array_section();
            //! Returns the sectioned expression in the array section
            Expression array_section_item();
            //! Returns the lower bound of the array section
            Expression array_section_lower();
            //! Returns the upper bound of the array section
            Expression array_section_upper();

            static const PredicateAST<LANG_IS_EXPRESSION_NEST> predicate;

            /*! Returns the enclosing expression that is meaningful */
            Expression get_enclosing_expression();

            /*! Returns the top enclosing expression */
            Expression get_top_enclosing_expression();

            /*! Returns the original tree which was used to wrap this expression */
            AST_t original_tree()
            {
                return _orig;
            }

            /*! States whether this expression is a top level one */
            bool is_top_level_expression();
    };

    //! This LangConstruct wraps a parameter declaration in a function declarator
    class ParameterDeclaration : public LangConstruct
    {
        private:
            Type _type;
        public:
            ParameterDeclaration(AST_t tree, ScopeLink sl, Type parameter_type)
                : LangConstruct(tree, sl), _type(parameter_type)
            {
            }

            //! States whether the parameter is named
            bool is_named();
            //! Returns an id-expression for the name
            IdExpression get_name();

            // Returns the type of the parameter declaration
            Type get_type()
            {
                return _type;
            }

            static const PredicateAST<LANG_IS_PARAMETER_DECLARATION> predicate;
    };

    //! This LangConstruct wraps a declaration of an entity
    /*!
     * This is roughly equivalent to the declarator part of any declaration in C/C++
     */
    class DeclaredEntity : public LangConstruct
    {
        public :
            DeclaredEntity(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }

            //! Returns an id-expression of the declared entity
            /*!
             * Do not use this function as it does not work for declarations involving typenames
             * like typedefs.
             */
            IdExpression get_declared_entity() DEPRECATED;

            //! Returns the declared symbol in this declaration
            Symbol get_declared_symbol();

            //! Returns the declaration tree
            AST_t get_declared_tree();

            //! States whether this declaration has initializer
            bool has_initializer();
            //! Returns an expression with the initializer itself
            Expression get_initializer();

            //! States whether this declaration is a functional one
            bool is_functional_declaration();
            //! Returns all the parameter declarations
            ObjectList<ParameterDeclaration> get_parameter_declarations();
            //! Returns all the parameter declarations
            /*!
             * \param has_ellipsis Will be set to true if the function receives an ellipsis
             */
            ObjectList<ParameterDeclaration> get_parameter_declarations(bool &has_ellipsis);

            //! States whether the functional declaration does not have
            // a prototype
            bool functional_declaration_lacks_prototype();

            static const PredicateAST<LANG_IS_DECLARED_NAME> predicate;
    };

    //! This class wraps a type specifier in a declaration
    class TypeSpec : public LangConstruct
    {
        public:
            TypeSpec(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }

            //! States whether this type-specifier defines a class
            bool is_class_specifier();
            //! Returns the class symbol defined in the class-specifier
            /*!
             * This function can only be used when is_class_specifier returned true
             */
            Symbol get_class_symbol();

            //! States whether this type-specifier defines an enumerator
            bool is_enum_specifier();
            //! Returns the enym symbol defined in the enum-specifier
            Symbol get_enum_symbol();
    };

    //! This class wraps a declaration-specifier sequence
    //in a declaration
    class DeclarationSpec : public LangConstruct
    {
        public:
            DeclarationSpec(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }

            //! Returns the type-specifier in the declaration-specifier sequence
            TypeSpec get_type_spec();
    };

    //! This class wraps a whole declaration
    /*!
     * Any declaration has two parts. A first DeclarationSpec part
     * where the type-specifier will be defined and a list of DeclaredEntity.
     *
     * For instance,
     *
     * const int a, *b;
     *
     * Will contain 'const int' in the DeclarationSpec and 'a', and 'b' in the 
     * DeclaredEntity list.
     *
     * \remark LangConstruct classes wrap trees, they are not the best way to 
     * gather symbolic information of symbols. In the example above, checking
     * for 'const' is better achieved by asking this to symbols 'a' and 'b' instead
     * of checking the tree (even if for most properties this would be equivalent).
     *
     */
    class Declaration : public LangConstruct
    {
        public:
            Declaration(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }

            //! Returns the list of declared entities in this declaration
            ObjectList<DeclaredEntity> get_declared_entities();
            //! Returns the declaration-specifier sequence of the declaration
            DeclarationSpec get_declaration_specifiers();

            //! States whether this declaration is templated
            bool is_templated();
            //! Returns a list of template-haders
            ObjectList<AST_t> get_template_header();

            //! Returns the point where all the declaration
            //started
            /*!
             * This function only is useful when is_templated returned true
             */
            AST_t get_point_of_declaration();

            static const PredicateAST<LANG_IS_DECLARATION> predicate;
    };

    //! This class wraps a particular attribute in a GCCAttributeSpecifier
    /*!
     * This class wraps one of the attributes in a gcc attribute specifier,
     * the last wrapped in GCCAttributeSpecifier.
     *
     * @code
     * void f() __attribute__((a, b));
     * @endcode
     *
     * Will have two GCCAttribute, one for 'a' and one for 'b'
     */
    class GCCAttribute : public LangConstruct
    {
        private:
        public:
            GCCAttribute(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }

            //! Returns the name of the attribute
            std::string get_name();

            //! States whether it has a list of expressions related to it
            bool has_argument_list() const;

            //! Returns the attribute argument list
            ObjectList<Expression> get_argument_list();
    };

    //! This class wraps a gcc attribute specifier
    /*!
     * This class wraps a gcc attribute specifier like the one below.
     *
     * @code
     * void f() __attribute__((a, b));
     * @endcode
     *
     * Will have two GCCAttribute, one for 'a' and one for 'b'
     */
    class GCCAttributeSpecifier : public LangConstruct
    {
        private:
        public:
            GCCAttributeSpecifier(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }

            //! Returns the list of attributes specified in this
            //attribute-specifier
            ObjectList<GCCAttribute> get_gcc_attribute_list();

            static const PredicateAST<LANG_IS_GCC_ATTRIBUTE> predicate;
    };
    
    //! @}

    //! This class eases replacing references to entities within a LangConstruct
    /*!
     * Since replacing entities in a tree is easier than using other approaches,
     * this class allows replacing a whole LangConstruct symbolic references
     * with other given entities.
     *
     * For instance, expression 'a + b' can be converted into '(*p_a) + p_b[0]' 
     * by just registering that Symbol 'a' has to be replaced with expression '(*p_a)' and Symbol
     * 'b' with expression 'p_b[0]'
     */
    class ReplaceIdExpression
    {
        protected:
            std::map<Symbol, AST_t> _repl_map;
        public:
            ReplaceIdExpression()
            {
            }

            //! Sets a replacement for symbol with a tree
            /*!
             * \param sym The symbol to be replaced
             * \param ast The expression tree used for the replacement
             */
            void add_replacement(Symbol sym, AST_t ast);

            //! Sets a replacement for the symbol with a tree
            /*!
             * \deprecated This function is deprecated. Instead use add_replacement(Symbol, AST_t)
             *
             * \param sym The symbol to be replaced
             * \param str A string containing the expression used for the replacement
             */
            void add_replacement(Symbol sym, std::string str) DEPRECATED;
            
            //! Sets a replacement for the symbol with a tree
            /*!
             * \deprecated This function is deprecated. Instead use add_replacement(Symbol, AST_t)
             *
             * \param sym The symbol to be replaced
             * \param src A Source containing an expression
             */
            void add_replacement(Symbol sym, Source src) DEPRECATED;

            //! Sets a replacement for the symbol with a tree
            /*!
             * \param sym The symbol to be replaced
             * \param str A string containing the expression used for the replacement
             * \param ref_tree Reference tree to perform the parsing of \a str
             * \param scope_link The ScopeLink used to parse the expression \a str
             */
            void add_replacement(Symbol sym, std::string str, AST_t ref_tree, ScopeLink scope_link);
            //! Sets a replacement for the symbol with a tree
            /*!
             * \param sym The symbol to be replaced
             * \param src A Source containing an expression
             * \param ref_tree Reference tree to perform the parsing of \a src
             * \param scope_link The ScopeLink used to parse the Source \a src
             */
            void add_replacement(Symbol sym, Source src, AST_t ref_tree, ScopeLink scope_link);

            //! States whether a replacement for a given symbol has been set
            /*
             * \param sym The symbol for which we request whether there is a replacement
             */
            bool has_replacement(Symbol sym);

            //! Replaces all non local symbols with the given replacements
            /*
             * \item orig_stmt The original LangConstruct
             *
             * \remark Any LangConstruct could be used to do the replacement though usual cases
             * are Statement and Expression.
             */
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

    //! \addtogroup Functors
    //! @{
    
    //! Convenience Functor that applied to an AST_t returns
    // its related symbol.
    class GetSymbolFromAST : public Functor<Symbol, AST_t>
    {
        private:
            ScopeLink scope_link;
        public:
            //! Returns a symbol after a tree
            /*!
             * \param ast This must be an id-expression
             */
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
    //! @}
}

#endif // TL_LANGCONSTRUCT_HPP

// This makes compatible old code with the new header
#include "tl-statement.hpp"
