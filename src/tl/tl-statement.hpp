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
#ifndef TL_STATEMENT_HPP
#define TL_STATEMENT_HPP

#include "tl-common.hpp"
#include "tl-langconstruct.hpp"

namespace TL
{
    //! \addtogroup LangConstruct Language construction wrappers
    //! @{
    //
    //! This wraps a condition.
    /*!
     * A condition, in C++ and C99 (but not in C90), is the conditional part of
     * if, while and for statements. They can be either declarations or plain
     * expressions. This class wraps this fact.
     */
    class LIBTL_CLASS Condition : public LangConstruct
    {
        public:
            Condition(AST_t ref, ScopeLink sl)
                : LangConstruct(ref, sl)
            {
            }

            //! States whether the condition is an expression
            bool is_expression();
            
            //! Returns the related expression of this condition (if any)
            Expression get_expression();
            
            //! States whether the condition is a declaration
            bool is_declaration();

            //! Returns the related declaration of this condition (if any)
            Declaration get_declaration();
    };

    //! LangConstruct that wraps a statement in the code
    class LIBTL_CLASS Statement : public LangConstruct
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

            //! States whether this statement is labeled
            bool is_labeled();

            //! Returns the label of the statement, if any
            std::string get_label();

            //! States whether this Statement is actually a compound statement
            bool is_compound_statement();
            //! Returns a list of inner statements
            /*! This function is only valid if is_compound_statement returned true
             */
            ObjectList<Statement> get_inner_statements();

            //! States whether this Statement is enclosed in a compound statement
            bool is_in_compound_statement();

            /*!
             * If is_last returns false, this function returns the statement
             * following the current one.
             */
            Statement next();

            /*!
             * If is_first returns false, this function returns the statement
             * following the current one.
             */
            Statement previous();

            /*! 
             * States whether this is the first statement of a compound statement.
             * If no compound statement is enclosing this statement it returns true.
             */
            bool is_first();

            /*! 
             * States whether this is the last statement of a compound statement.
             * If no compound statement is enclosing this statement it returns true.
             */
            bool is_last();

            const static PredicateAttr predicate;

			//! Prepends a Statement
			void prepend(Statement st);
			//! Appends a Statement
			void append(Statement st);
    };
    
    //! This LangConstruct wraps a for-statement in the code
    class LIBTL_CLASS ForStatement : public Statement
    {
        private:
            //! Gathers the induction variable
            AST_t _induction_variable;
            //! The lower bound of a regular loop
            AST_t _lower_bound;
            //! The upper bound of a regular loop
            AST_t _upper_bound;
            //! The step of a regular loop
            AST_t _step;

            Source _operator_bound;

            //! Gathers information of regular loops
            /*! This function is only called when check_statement returns true
             */
            void gather_for_information();

            //! Checks for a regular loop
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

            //! Returns an id-expression with the induction variable
            IdExpression get_induction_variable();
            
            //! Returns a computed lower bound of a regular loop
            Expression get_lower_bound();
            
            //! Returns a computed upper bound of a regular loop
            Expression get_upper_bound();
            
            //! Returns a computed step of a regular loop
            Expression get_step();

            //! Returns the related bounding operator of a regular loop
            /*! Result can be either <= or >= depending on the original loop operator */
            Source get_bound_operator();

            //! Returns the loop body
            Statement get_loop_body();

            //! States whether this loop is a regular one
            /*!
             * \deprecated Do not use it, instead use is_regular_loop
             */
            bool regular_loop();

            //! States whether this loop is a regular one
            /*!
             * A regular loop is that where computing the lower and upper
             * bounds and its step is easy after the syntax.
             */
            bool is_regular_loop();

            //! Returns the iterating initialization
            /*!
             * Given loops 
             *
             *   '%for(%i = 0; %i < 10; %i++)' 
             *
             * and
             *
             *   '%for(int %i = 0; %i < 10; %i++)' 
             *
             * get_iterating_init will return 
             *    '%i = 0' 
             *
             *    and 
             *
             *    'int i = 0' 
             *
             * respectively
             */
            AST_t get_iterating_init();

            //! Returns the iterating condition
            /*!
             * Given loop
             *
             *  '%for(%i = 0; %i < 10; %i++)' 
             *
             * this function will return 'i < 10'
             */
            Expression get_iterating_condition();
            //! Returns the iterating expression
            /*!
             * Given loop
             *
             *  '%for(%i = 0; %i < 10; %i++)' 
             *
             * this function will return 'i++'
             */
            Expression get_iterating_expression();

            const static PredicateAttr predicate;
    };


    //! This class wraps a while statement
    class LIBTL_CLASS WhileStatement : public Statement
    {
        public:
            WhileStatement(AST_t ref, ScopeLink sl)
                : Statement(ref, sl)
            {
            }

            //! Returns the while iterating condition
            Condition get_condition();

            //! Returns the body of the while statement
            Statement get_body();

            const static PredicateAttr predicate;
    };

    //! This class wraps an is statement
    class LIBTL_CLASS IfStatement : public Statement
    {
        public:
            IfStatement(AST_t ref, ScopeLink sl)
                : Statement(ref, sl)
            {
            }

            //! Returns the if condition
            Condition get_condition();

            //! Returns the body of "then" 
            Statement get_then_body();

            //! States whether this if has an "else" branch
            bool has_else();

            //! Returns the body of "else"
            Statement get_else_body();

            const static PredicateAttr predicate;
    };

    //! This class wraps a do-statement
    class LIBTL_CLASS DoWhileStatement : public Statement
    {
        public:
            DoWhileStatement(AST_t ref, ScopeLink sl)
                : Statement(ref, sl)
            {
            }

            //! Returns the body of the do-while
            Statement get_body();

            //! Returns the iterating expression
            Expression get_expression();

            const static PredicateAttr predicate;
    };

    //! This class wraps a case-statement
    class LIBTL_CLASS CaseStatement : public Statement
    {
        public:
            CaseStatement(AST_t ast, ScopeLink sl)
                : Statement(ast, sl)
            {
            }

            //! Returns the expression of this case statement
            Expression get_case_expression();
            
            //! Returns the statement of this case 
            Statement get_statement();
    };

    //! This class wraps a switch-statement
    class LIBTL_CLASS SwitchStatement : public Statement
    {
        public:
            SwitchStatement(AST_t ref, ScopeLink sl)
                : Statement(ref, sl)
            {
            }

            //! Returns the switch statement condition
            Condition get_condition();

            //! Returns a list of case statements
            ObjectList<CaseStatement> get_cases();

            const static PredicateAttr predicate;
    };

    //! This class wraps a return-statement
    class LIBTL_CLASS ReturnStatement : public Statement
    {
        public:
            ReturnStatement(AST_t ref, ScopeLink sl)
                : Statement(ref, sl)
            {
            }

            //! States whether this return statement has a related expression
            bool has_return_expression();

            //! Returns the returned expression
            Expression get_return_expression();

            const static PredicateAttr predicate;
    };

    //! This class wraps a goto-statement
    class LIBTL_CLASS GotoStatement : public Statement
    {
        public:
            GotoStatement(AST_t ref, ScopeLink sl)
                : Statement(ref, sl)
            {
            }

            //! Returns the label of this goto-statement
            std::string get_label();

            const static PredicateAttr predicate;
    };

    //! @}
}
#endif // TL_STATEMENT_HPP
