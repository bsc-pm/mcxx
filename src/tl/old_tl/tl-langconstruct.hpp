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




#ifndef TL_LANGCONSTRUCT_HPP
#define TL_LANGCONSTRUCT_HPP

#include "tl-common.hpp"
#include "tl-object.hpp"
#include "tl-ast.hpp"
#include "tl-symbol.hpp"
#include "tl-scopelink.hpp"
#include "tl-builtin.hpp"
#include "tl-source.hpp"
#include "tl-type.hpp"
#include "cxx-attrnames.h"
#include "cxx-macros.h"
#include <iostream>
#include <set>
#include <string>
#include <utility>

namespace TL
{
    class FunctionDefinition;
    class Statement;
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
    class LIBTL_CLASS LangConstruct : public TL::Object
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
            virtual std::string prettyprint() const;

            //! Convenience function that just calls prettyprint()
            operator std::string() const;

            //! Returns the wrapped tree of this LangConstruct
            AST_t get_ast() const
            {
                return _ref;
            }

            //! Returns the scope link of this LangConstruct
            ScopeLink get_scope_link() const
            {
                return _scope_link;
            }

            //! Returns the scope of this LangConstruct
            Scope get_scope() const
            {
                return _scope_link.get_scope(_ref);
            }

            //! Returns the enclosing function definition
            FunctionDefinition get_enclosing_function() const;

            //! Returns the enclosing statement of this lang construct, if any
            Statement get_enclosing_statement() const;

            //! Returns a list of all symbolic occurrences
            ObjectList<IdExpression> all_symbol_occurrences(SymbolsWanted symbols = ALL_SYMBOLS) const;
            //! Returns a list of all symbolic occurrences that are not defined
            //within this construction
            ObjectList<IdExpression> non_local_symbol_occurrences(SymbolsWanted symbols = ALL_SYMBOLS) const;
            ObjectList<Symbol> non_local_symbols(SymbolsWanted symbols = ALL_SYMBOLS) const;
            /*!
             * \bug Not implemented
             */
            ObjectList<IdExpression> local_symbol_occurrences() const;

            //! Common predicate to all LangConstruct
            /*!
             * All LangConstruct classes should have a predicate that
             * states whether the tree is valid to be wrapped in such
             * a LangConstruct. 
             */
            const static AlwaysFalse<AST_t> predicate;

            virtual ~LangConstruct()
            {
            }
    };

    
    LIBTL_EXTERN std::ostream& operator<< (std::ostream& o, const LangConstruct& lang_construct);

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

    class Expression;
    class Statement;

    //! Wraps an id-expression in C++ or a symbol name in C
    /*!
     * This is an "all-pervasive" class throughout all the TL namespace.  It
     * actually wraps a reference (or occurence) to an identifier.  Here
     * identifier can be either a type or variable
     */
    class LIBTL_CLASS IdExpression : public LangConstruct
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
            //! Returns the template-argument of the template-id
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

            //! Returns a declaration of the symbol
            /*!
             * \bug This function uses get_symbol
             */
            Declaration get_declaration() const;

            //! Predicate for an IdExpression
            static const PredicateAttr predicate;

            //! Returns a full fledged Expression after this IdExpression
            Expression get_expression() const;
    };

    class TemplateHeader;
    class LinkageSpecifier;
    class TemplateParameterConstruct;
    class DeclaredEntity;
    //! This function wraps a whole function definition
    class LIBTL_CLASS FunctionDefinition : public LangConstruct
    {
        private:
        public:
            FunctionDefinition(AST_t ref, ScopeLink scope_link)
                : LangConstruct(ref, scope_link)
            {
                // Allow wrapping a list whose first item is a function-def as
                // well. 
                if (ref.is_list())
                {
                    ASTIterator iter = ref.get_list_iterator();

                    AST_t item = iter.item();

                    if (FunctionDefinition::predicate(item))
                    {
                        _ref = item;
                    }
                }
            }
            
            //! Prepends a tree as a sibling of this function definition
            void prepend_sibling(AST_t);

            //! Returns an id-expression with the function name as it appears
            //in the function definition.
            /*!
             * Member functions defined outside the class will return a qualified id-expression
             */
            IdExpression get_function_name() const;

            //! Returns the function body
            Statement get_function_body() const;

            //! States whether this function definition is templated
            bool is_templated() const;
            //! Returns a list of templated headers for this template declaration
            ObjectList<TemplateHeader> get_template_header() const;

            //! States whether this function definition has a linkage specified
            bool has_linkage_specifier() const;

            //! Returns a list of LinkageSpecifiers enclosing thins declaration
            ObjectList<LinkageSpecifier> get_linkage_specifier() const;

            //! Returns the declared entity of this function definition.
            /*!
             * This is the function name itself
             */
            DeclaredEntity get_declared_entity() const;

            //! Returns the declared function symbol
            /*! This is a synonym of get_declared_entity().get_declared_symbol() */
            Symbol get_function_symbol() const;

            //! Returns the point of declaration
            AST_t get_point_of_declaration() const;

            static const PredicateAttr predicate;
    };

    //! This LangConstruct wraps an expression in the language
    /*!
     * This wrapper ignores all parentheses and nested constructions.
     */
    class LIBTL_CLASS Expression : public LangConstruct
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
                CONDITIONAL,
                //! Assignment operator =
                ASSIGNMENT
            };

            //! Computes the type of the expression
            Type get_type() const;
            //! Computes the type of the expression
            /*!
             * \param is_lvalue Will hold whether this is a lvalue
             */
            Type get_type(bool &is_lvalue) const;

            Expression(AST_t ref, ScopeLink scope_link)
                : LangConstruct(ref, scope_link)
            {
                this->_orig = this->_ref;
                this->_ref = advance_over_nests(this->_ref);
            }

            //! States whether this is an id-expression
            bool is_id_expression();
            //! States whether this is an id-expression
            bool is_accessed_member();
            //! Returns an id-expression. 
            // Only valid if is_id_expression, is_member_acces return true
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

            //! States whether this call is to a known entity
            bool is_named_function_call();
            //! Returns the named entity called
            /*! This function can only be called if is_named_function_call returned true */
            Symbol get_called_entity();

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

            //! States whether this is a 'this' reference
            bool is_this_variable();

            //! Returns the symbol expressed by a 'this' expression
            Symbol get_this_symbol();

            //! States whether this is a 'this' access expression
            /*!
             * \return True if the expression is of the form 'this->x'
             */
            bool is_this_access();
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

            //! Alias for is_array_section_range
            /*
               \deprecated Do not use. Use is_array_section_range instead
            */
            DEPRECATED bool is_array_section();

            //! States if this is a ranged array section
            /*
               a[e1 : e2] 

               is a ranged array section denoting elements from a[e1]
               to a[e2] (this is, the ends are both included). e2 should
               be larger value than e1
             */
            bool is_array_section_range();

            //! States if this is a sized array section
            /*
               a[e1 ; s] 

               is a ranged array section denoting elements from a[e1]
               to a[e1 + s - 1] (where s > 0)
             */
            bool is_array_section_size();

            //! Returns the sectioned expression in the array section
            Expression array_section_item();
            //! Returns the lower bound of the array section
            Expression array_section_lower();
            //! Returns the upper limit of an array section
            /*!
              For a ranged array section this is the upper bound.
              For sized array sections, this is the size of elements
              in the array section starting from the lower bound
              */
            Expression array_section_upper();

            //! States whether the expression is a shaping expression
            bool is_shaping_expression();
            //! Returns the expression being shaped
            Expression shaped_expression();
            //! Returns the shape list
            ObjectList<Expression> shape_list();

            //! States whether the expression is a 'throw' expression
            bool is_throw_expression();
            //! Returns the expression called by the throw expression
            Expression get_throw_expression();
            
            static const PredicateAttr predicate;

            /*! Returns the enclosing expression that is meaningful */
            Expression get_enclosing_expression();

            /*! Returns the top enclosing expression */
            Expression get_top_enclosing_expression();

            /*! States whether the expression is constant or not */
            bool is_constant();

			// Evaluates a constant expression as an int 
			/*!
			  This function is a helper function to evaluate an expression
			  as a const int expression.
			  \param valid States whether the constant evaluation succeeded
			  
			  If valid is true, then the result of the function is the
			  int value of this constant expression. Do not rely on the result
			  of the function if valid is false (thus, check always \a valid)
			  */
			int evaluate_constant_int_expression(bool &valid);

            /*! Returns the original tree which was used to wrap this expression */
            AST_t original_tree()
            {
                return _orig;
            }

            //! States if the frontend tagged this expression with a related symbol
            bool has_symbol();

            //! Returns the symbol with which the frontend tagged this expression
            Symbol get_symbol();

            /*! States whether this expression is a top level one */
            bool is_top_level_expression();
            
            //! States if this is a expression as 'sizeof(expr)'
            bool is_sizeof();
            
            //! States if this is a expression as 'sizeof(type)'
            bool is_sizeof_typeid();
    };

    //! This LangConstruct wraps a parameter declaration in a function declarator
    class LIBTL_CLASS ParameterDeclaration : public LangConstruct
    {
        private:
            Type _type;
        public:
            ParameterDeclaration(AST_t tree, ScopeLink sl, Type parameter_type)
                : LangConstruct(tree, sl), _type(parameter_type)
            {
            }

            //! States whether the parameter is named
            bool is_named() const;
            //! Returns an id-expression for the name
            IdExpression get_name() const;

            // Returns the type of the parameter declaration
            Type get_type() const
            {
                return _type;
            }

            static const PredicateAttr predicate;
    };

    //! This LangConstruct wraps a declaration of an entity
    /*!
     * This is roughly equivalent to the declarator part of any declaration in C/C++
     */
    class LIBTL_CLASS DeclaredEntity : public LangConstruct
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
            IdExpression get_declared_entity() const DEPRECATED;

            //! Returns the declared symbol in this declaration
            Symbol get_declared_symbol() const;

            //! Returns the declaration tree
            AST_t get_declared_tree() const;

            //! States whether this declaration has initializer
            bool has_initializer() const;
            //! Returns an expression with the initializer itself
            Expression get_initializer() const;

            //! Returns the tree of the declarator
            AST_t get_declarator_tree() const;

            //! States whether this declaration is a functional one
            bool is_functional_declaration() const;
            //! Returns all the parameter declarations
            ObjectList<ParameterDeclaration> get_parameter_declarations() const;
            //! Returns all the parameter declarations
            /*!
             * \param has_ellipsis Will be set to true if the function receives an ellipsis
             */
            ObjectList<ParameterDeclaration> get_parameter_declarations(bool &has_ellipsis) const;

            //! States whether the functional declaration does not have
            // a prototype
            bool functional_declaration_lacks_prototype() const;

            static const PredicateAttr predicate;
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
            bool is_class_specifier() const;
            //! Returns the class symbol defined in the class-specifier
            /*!
             * This function can only be used when is_class_specifier returned true
             */
            Symbol get_class_symbol() const;

            //! States whether this type-specifier defines an enumerator
            bool is_enum_specifier() const;
            //! Returns the enym symbol defined in the enum-specifier
            Symbol get_enum_symbol() const;

            //! Returns the Type of this type-specifier
            Type get_type() const;

            // Fix the predicate
            static const PredicateAttr predicate;
    };

    //! This class wraps a declaration-specifier sequence
    //in a declaration
    class LIBTL_CLASS DeclarationSpec : public LangConstruct
    {
        public:
            DeclarationSpec(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }

            //! Returns the type-specifier in the declaration-specifier sequence
            TypeSpec get_type_spec() const;
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
    class LIBTL_CLASS Declaration : public LangConstruct
    {
        public:
            Declaration(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
                PredicateAttr decl_stmt(LANG_IS_DECLARATION_STATEMENT);

                // Adjust declaration statements to behave like real
                // declarations
                if (decl_stmt(ast))
                {
                    ast = ast.get_link_to_child(LANG_DECLARATION_STATEMENT_DECLARATION);
                }
            }

            //! Returns the list of declared entities in this declaration
            ObjectList<DeclaredEntity> get_declared_entities() const;
            //! Returns the declaration-specifier sequence of the declaration
            DeclarationSpec get_declaration_specifiers() const;

            //! States whether this declaration has explicit linkage specifier "X"
            bool has_linkage_specifier() const;

            //! Returns a list of LinkageSpecifiers enclosing thins declaration
            ObjectList<LinkageSpecifier> get_linkage_specifier() const;

            //! States whether this declaration is templated
            bool is_templated() const;
            //! Returns a list of template-haders
            ObjectList<TemplateHeader> get_template_header() const;

            //! Returns the point where all the declaration started
            /*!
             * This function only is useful when is_templated returned true
             */
            AST_t get_point_of_declaration() const;

            //! States whether this declaration is empty or not
            bool is_empty_declaration() const;
            
            // Fix the predicate
            static const PredicateAttr predicate;
    };

    class LIBTL_CLASS TemplateParameterConstruct : public LangConstruct
    {
        public:
            TemplateParameterConstruct(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }

            bool is_named() const;
            std::string get_name() const;

            bool is_type() const ;
            bool is_nontype() const;
            bool is_template() const;

            Symbol get_symbol() const;
    };

    class LIBTL_CLASS TemplateHeader : public LangConstruct
    {
        public:
            TemplateHeader(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }

            ObjectList<TemplateParameterConstruct> get_parameters() const;
            virtual std::string prettyprint() const;
    };

    class LIBTL_CLASS LinkageSpecifier : public LangConstruct
    {
        public:
            LinkageSpecifier(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }

            virtual std::string prettyprint() const;
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
    class LIBTL_CLASS GCCAttribute : public LangConstruct
    {
        private:
        public:
            GCCAttribute(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }

            //! Returns the name of the attribute
            std::string get_name() const;

            //! States whether it has a list of expressions related to it
            bool has_argument_list() const;

            //! Returns the attribute argument list
            ObjectList<Expression> get_argument_list() const;
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
    class LIBTL_CLASS GCCAttributeSpecifier : public LangConstruct
    {
        private:
        public:
            GCCAttributeSpecifier(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }

            //! Returns the list of attributes specified in this
            //attribute-specifier
            ObjectList<GCCAttribute> get_gcc_attribute_list() const;

            static const PredicateAttr predicate;
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

     * \note This class poses some restrictions that are unbearable when
     * performing some transformations. For a class that does not pose such
     * restrictions use ReplaceSrcIdExpression instead
     */
    class LIBTL_CLASS ReplaceIdExpression
    {
        protected:
            std::map<Symbol, std::string> _repl_map;
            std::string _repl_this;
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

            //! Sets a replacement for the symbol with a string
            /*!
             * \param sym The symbol to be replaced
             * \param str A string containing the expression used for the replacement
             */
            void add_replacement(Symbol sym, const std::string& str);
            
            //! Sets a replacement for the symbol with a tree
            /*!
             * \param sym The symbol to be replaced
             * \param src A Source containing an expression
             */
            void add_replacement(Symbol sym, Source src);

            //! Sets a replacement for the symbol with a tree
            /*!
             * \param sym The symbol to be replaced
             * \param str A string containing the expression used for the replacement
             * \param ref_tree Reference tree to perform the parsing of \a str
             * \param scope_link The ScopeLink used to parse the expression \a str
             * \deprecated
             */
            void add_replacement(Symbol sym, const std::string& str, AST_t ref_tree, ScopeLink scope_link) DEPRECATED;
            //! Sets a replacement for the symbol with a tree
            /*!
             * \param sym The symbol to be replaced
             * \param src A Source containing an expression
             * \param ref_tree Reference tree to perform the parsing of \a src
             * \param scope_link The ScopeLink used to parse the Source \a src
             * \deprecated
             */
            void add_replacement(Symbol sym, Source src, AST_t ref_tree, ScopeLink scope_link) DEPRECATED;

            //! Sets a replacement for 'this'
            /*! 
               \param str A string containing the expression used for the replacement
             */
            void add_this_replacement(const std::string& str);

            //! Sets a replacement for 'this'
            /*! 
             * \param src A Source containing an expression
             */
            void add_this_replacement(Source src);

            //! Sets a replacement for 'this'
            /*! 
             * \param ast The expression tree used for the replacement
             */
            void add_this_replacement(AST_t ast);

            //! States whether a replacement for a given symbol has been set
            /*
             * \param sym The symbol for which we request whether there is a replacement
             */
            bool has_replacement(Symbol sym) const;

            //! Replaces all non local symbols with the given replacements
            /*
             * \item orig_stmt The original LangConstruct
             *
             * \remark Any LangConstruct could be used to do the replacement though usual cases
             * are Statement and Expression.
             */
            template <class T>
            T replace(T orig_stmt) const
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
                        AST_t orig_ast = it->get_ast();

                        // This way of duplicating a tree is always safer
                        Source src;
                        src << _repl_map.find(sym)->second
                            ;

                        AST_t repl_ast = src.parse_expression(orig_ast, 
                                orig_stmt.get_scope_link(),
                                Source::DO_NOT_CHECK_EXPRESSION);

                        orig_ast.replace_with(repl_ast);
                    }
                }

                if (_repl_this != "")
                {
                    ObjectList<AST_t> this_references = result.get_ast().depth_subtrees(PredicateAttr(LANG_IS_THIS_VARIABLE));

                    for (ObjectList<AST_t>::iterator it = this_references.begin();
                            it != this_references.end();
                            it++)
                    {
                        AST_t &orig_ast(*it);

                        Source src;
                        src << _repl_this
                            ;

                        AST_t repl_ast = src.parse_expression(orig_ast, 
                                orig_stmt.get_scope_link(),
                                Source::DO_NOT_CHECK_EXPRESSION);

                        orig_ast.replace(repl_ast);
                    }
                }

                return result;
            }
    };

    //! This class eases replacing references to entities within a tree
    /*!
     * Since replacing entities in a tree is easier than using other approaches,
     * this class allows replacing a whole LangConstruct symbolic references
     * with other given entities.
     *
     * In contrast to what ReplaceIdExpression does, this class does not
     * transform one tree into another but returns a Source with the
     * transformation already performed. As a benefit, replacement expressions
     * do not really require a proper context like it happens in
     * ReplaceIdExpression. Sometimes it happens that the replacement
     * expression involves new symbols that might not be declared and using
     * placeholders is not possible or leads to a convoluted transformation.
     *
     * \note This class breaks with the awkward fact that all code generation
     * is performed in Source except for replacing symbol occurrences. You
     * should use this class instead of the more problematic
     * ReplaceIdExpression since that works on trees
     *
     */
    class LIBTL_CLASS ReplaceSrcIdExpression
    {
        protected:
            static const char* prettyprint_callback(AST a, void* data);

            std::map<Symbol, std::string> _repl_map;
            std::string _repl_this;

            ScopeLink _sl;
            bool _do_not_replace_declarators;
            bool _ignore_pragmas;
        public:
            ReplaceSrcIdExpression(ScopeLink sl)
                : _sl(sl), 
                _do_not_replace_declarators(false),
                _ignore_pragmas(false) { }

            //! Sets a replacement for the symbol with a string
            /*!
             * \param sym The symbol to be replaced
             * \param str A string containing the expression used for the replacement
             */
            void add_replacement(Symbol sym, const std::string& str);

            //! States whether sym has a related replacement
            bool has_replacement(Symbol sym) const;

            //! Returns the replacement of the symbol
            std::string get_replacement(Symbol sym) const;

            //! Sets a replacement for this
            /*!
             * \param str A string containing the expression used for the replacement
             */
            void add_this_replacement(const std::string& str);

            //! Perform the replacement returning a prettyprinted coe
            Source replace(AST_t a) const;

            //! Perform the replacement returning a prettyprinted code
            Source replace(LangConstruct a) const;

            void set_replace_declarators(bool b);

            void set_ignore_pragma(bool b);

            ScopeLink get_scope_link() const;

            virtual ~ReplaceSrcIdExpression() { }
    };

    //! \addtogroup Functors
    //! @{
    
    //! Convenience Functor that applied to an AST_t returns
    // its related symbol.
    class LIBTL_CLASS GetSymbolFromAST : public Functor<Symbol, AST_t>
    {
        private:
            ScopeLink scope_link;
        public:
            //! Returns a symbol after a tree
            /*!
             * \param ast This must be an id-expression
             */
            virtual Symbol do_(AST_t& ast) const 
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
