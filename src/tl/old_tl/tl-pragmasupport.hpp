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




#ifndef TL_PRAGMASUPPORT_HPP
#define TL_PRAGMASUPPORT_HPP

#include "tl-common.hpp"
#include <string>
#include <stack>
#include <algorithm>
#include "tl-clauses-info.hpp"
#include "tl-compilerphase.hpp"
#include "tl-handler.hpp"
#include "tl-source.hpp"
#include "cxx-attrnames.h"

namespace TL
{
    class LIBTL_CLASS ClauseTokenizer
    {
        public:
            virtual ObjectList<std::string> tokenize(const std::string& str) const = 0;
            virtual ~ClauseTokenizer() { }
    };

    class LIBTL_CLASS NullClauseTokenizer : public ClauseTokenizer
    {
        public:
            virtual ObjectList<std::string> tokenize(const std::string& str) const
            {
                ObjectList<std::string> result;
                result.append(str);
                return result;
            }
    };

    class LIBTL_CLASS ExpressionTokenizer : public ClauseTokenizer
    {
        public:
            virtual ObjectList<std::string> tokenize(const std::string& str) const
            {
                int bracket_nesting = 0;
                ObjectList<std::string> result;

                std::string temporary("");
                for (std::string::const_iterator it = str.begin();
                        it != str.end();
                        it++)
                {
                    const char & c(*it);

                    if (c == ',' 
                            && bracket_nesting == 0
                            && temporary != "")
                    {
                        result.append(temporary);
                        temporary = "";
                    }
                    else
                    {
                        if (c == '('
                                || c == '{'
                                || c == '[')
                        {
                            bracket_nesting++;
                        }
                        else if (c == ')'
                                || c == '}'
                                || c == ']')
                        {
                            bracket_nesting--;
                        }
                        temporary += c;
                    }
                }

                if (temporary != "")
                {
                    result.append(temporary);
                }

                return result;
            }
    };

    class LIBTL_CLASS ExpressionTokenizerTrim : public ExpressionTokenizer
    {
        public:
			virtual ObjectList<std::string> tokenize(const std::string& str) const
			{
				ObjectList<std::string> result;
				result = ExpressionTokenizer::tokenize(str);

				std::transform(result.begin(), result.end(), result.begin(), trimExp);

				return result;
			}

        private:
            static std::string trimExp (const std::string &str) {

            	ssize_t first = str.find_first_not_of(" \t");
            	ssize_t last = str.find_last_not_of(" \t");

            	return str.substr(first, last - first + 1);
            }
    };

    //! This class wraps a clause in a PragmaCustomConstruct
    /*!
      This class allows a clause to be named several times, thus

         #pragma prefix name clause(a) clause(b)

      will be equivalent as if the user had written

         #pragma prefix name clause(a, b)

      There is no way to tell apart these two cases, except for using
      PragmaCustomClause::get_arguments_unflattened, see below.

      Pragma clauses are pretty flexible on what they allow as arguments. Free,
      well parenthesized, text is allowed in clauses. Thus forwarding the
      responsability of giving a syntactic validity and semantic meaning to
      PragmaCustomCompilerPhase classes.

      Since the raw string is most of the time of little use, the class can cook
      some usual cases:

        When the clause should contain a list of expressions, use
        PragmaCustomClause::get_expression_list

        When the clause should contain a list of variable-names (but not
        general expressions), use PragmaCustomClause::get_id_expressions
      
      You can always get raw versions of the clause content (in case you have
      very special syntax in it requiring special parsing) using
      PragmaCustomClause::get_arguments and
      PragmaCustomClause::get_arguments_unflattened. The second version returns
      a list of lists of strings, one list per occurrence of the clause while
      the first flats them in a single list.
    */
    class LIBTL_CLASS PragmaCustomClause : public LangConstruct
    {
        private:

            ObjectList<std::string> _clause_names;
            ObjectList<AST_t> filter_pragma_clause();

        public:
            PragmaCustomClause(const std::string& src, AST_t ref, ScopeLink scope_link)
                : LangConstruct(ref, scope_link) 
            {
                _clause_names.push_back(src);
            }
            
            PragmaCustomClause(const ObjectList<std::string> & src, AST_t ref, ScopeLink scope_link)
               : LangConstruct(ref, scope_link), _clause_names(src) 
            {
            }

            //! Returns the name of the current clause
            std::string get_clause_name() { return _clause_names[0]; }

            //! States whether the clause was actually in the pragma
            /*!
              Since PragmaCustomConstruct always returns a PragmaCustomClause
              use this function to know whether the clause was actually in the
              pragma line. No other function of PragmaCustomClause should be
              used if this function returns false
              */
            bool is_defined();

            //! Convenience function, it returns all the arguments of the clause parsed as expressions
            ObjectList<Expression> get_expression_list();

            //! Convenience function, it returns all arguments of the clause parsed as id-expressions
            ObjectList<IdExpression> get_id_expressions(IdExpressionCriteria criteria = VALID_SYMBOLS);

            //! Do not use this one, its name is deprecated, use get_id_expressions instead
            ObjectList<IdExpression> id_expressions(IdExpressionCriteria criteria = VALID_SYMBOLS);

            //! Returns the string contents of the clause arguments
            /*!
              This function actually returns a list of a single element
              */
            ObjectList<std::string> get_arguments();

            //! Returns the string contents of the clause arguments but using a tokenizer
            /*!
              The tokenizer can further split the text in additional substrings
              */
            ObjectList<std::string> get_arguments(const ClauseTokenizer&);

            //! Returns the string contents of the clause arguments but using a tokenizer
            /*!
              This function is similar to get_arguments but does not combine them in a single list.
              There is a list per each clause occurrence in the pragma
            */
            ObjectList<ObjectList<std::string> > get_arguments_unflattened();

            //! This is like get_arguments but at tree level. This function is of little use
            /*!
              It is highly unlikely that you need this function. Check the others
             */
            ObjectList<AST_t> get_arguments_tree();
    };

    class LIBTL_CLASS PragmaCustomConstruct : public LangConstruct, public LinkData
    {
        private:
            DTO* _dto;

        public:
            PragmaCustomConstruct(AST_t ref, ScopeLink scope_link)
                : LangConstruct(ref, scope_link),
                  _dto(NULL)
            {
            }

            //! Returns the name of the pragma prefix
            std::string get_pragma() const;

            //! Returns the name of the pragma directive
            std::string get_directive() const;

            //! States if this is a directive
            /*!
              When this function is true it means that the pragma itself is a sole entity
             */
            bool is_directive() const;

            //! States if this is a construct
            /*!
              When this function is true it means that the pragma acts as a header of another
              language construct. Functions get_statement and get_declaration can then, be used to
              retrieve that language construct.

              For pragma constructs at block-scope only get_statement should be
              used, otherwise use get_declaration as the nested construct can
              be a declaration or a function definition (or even something else)
             */
            bool is_construct() const;

            //! Returns the statement associated to this pragma construct
            /*!
              Using this function is only valid when the pragma is in block-scope
              and function is_construct returned true
              */
            Statement get_statement() const;

            //! Returns the tree associated to this pragma construct
            /*!
              Using this function is only valid when the pragma is in a scope
              other than block and function is_construct returned true

              This tree can be a Declaration, FunctionDefinition or some other
              tree not wrapped yet in a LangConstruct (e.g. a Namespace
              definition)
              */
            AST_t get_declaration() const;

            //! This function returns the tree related to the pragma itself
            /*!
              This function is rarely needed, only when a change of the pragma itself is required
              */
            AST_t get_pragma_line() const;
            
            //! This is used internally to initialize clauses information
            /*!
              Use it only if you want automatic clause checks but you never call get_clause on it
              It is safe to call it more than once.
              */
            void init_clause_info() const;

            //! States if the pragma encloses a function definition
            /*!
              This is useful when using get_declaration, to quickly know if we can use a FunctionDefinition
              or we should use a Declaration instead
              */
            bool is_function_definition() const;

            //! States if the pragma is followed by a first clause-alike parenthesis pair
            /*!
                #pragma prefix directive(a,b)

                'a,b' is sort of an unnamed clause which is called the parameter of the pragma

                This function states if this pragma has this syntax
            */
            bool is_parameterized() const;

            //! Returns a list of IdExpression's found in the parameter of the pragma
            ObjectList<IdExpression> get_parameter_id_expressions(IdExpressionCriteria criteria = VALID_SYMBOLS) const;

            //! Returns a list of Expressions in the parameter of the pragma
            /*!
              Parameters allow well-parenthesized free text. This function interprets the content of the parameter
              as a list of comma-separated expressions, parses them at this moment (if not parsed already)
              and returns it as a list
              */
            ObjectList<Expression> get_parameter_expressions() const;

            //! Returns the string of the parameter of the pragma
            /*!
              Parameters allow well-parenthesized free text. This function returns the whole text with no tokenization.
              This function will always return one element, but for parallelism with the equivalent function of PragmaCustomClause
              it returns a list (that will contain a single element)
              */
            ObjectList<std::string> get_parameter_arguments() const;

            //! Returns the string of the parameter of the pragma using a tokenizer
            /*!
              This function is identical to get_parameter_arguments() but uses \a tokenizer to
              split the contents of the string.
              */
            ObjectList<std::string> get_parameter_arguments(const ClauseTokenizer& tokenizer) const;

            //! This function returns all the clauses of this pragma
            ObjectList<std::string> get_clause_names() const;

            //! This function returns a PragmaCustomClause object for a named clause
            /*!
              Note that this function always returns a PragmaCustomClause
              object even if no clause with the given /a name exists. Use
              PragmaCustomClause::is_defined to check its existence.
              Adds to the DTO of PragmaCustomCompilerPhase the clause only if /a name exists.
              */
            PragmaCustomClause get_clause(const std::string& name) const;

            PragmaCustomClause get_clause(const ObjectList<std::string>& names) const;

            //! This function set to the object _dto the dto get from de compiler phase
            void set_dto(DTO* dto);

            //! This function returns a boolean that show if some warnings must be printed out
			bool get_show_warnings();
    };

    LIBTL_EXTERN bool is_pragma_custom(const std::string& pragma_preffix, 
            AST_t ast,
            ScopeLink scope_link);

    LIBTL_EXTERN bool is_pragma_custom_directive(const std::string& pragma_preffix, 
            const std::string& pragma_directive, 
            AST_t ast,
            ScopeLink scope_link);

    LIBTL_EXTERN bool is_pragma_custom_construct(const std::string& pragma_preffix, 
            const std::string& pragma_directive, 
            AST_t ast,
            ScopeLink scope_link);

    typedef std::map<std::string, Signal1<PragmaCustomConstruct> > CustomFunctorMap;

    class LIBTL_CLASS PragmaCustomDispatcher : public TraverseFunctor
    {
        private:
            std::string _pragma_handled;
            CustomFunctorMap& _pre_map;
            CustomFunctorMap& _post_map;
			DTO* _dto;
            bool _warning_clauses;

            std::stack<PragmaCustomConstruct*> _construct_stack;

            void dispatch_pragma_construct(CustomFunctorMap& search_map, PragmaCustomConstruct& pragma_custom_construct);
        public:
            PragmaCustomDispatcher(const std::string& pragma_handled, 
                    CustomFunctorMap& pre_map,
                    CustomFunctorMap& post_map,
                    bool warning_clauses);

            virtual void preorder(Context ctx, AST_t node);
            virtual void postorder(Context ctx, AST_t node);
            void set_dto(DTO* dto);
            void set_warning_clauses(bool warning);
    };

    //! Base class for all compiler phases working on user defined pragma lines
    /*!
     * Configuration of mcxx will require a 'pragma_prefix' line in order
     * to properly parse these pragma lines. In addition, the phases
     * will have to call register_directive and register_construct
     * accordingly to register specific constructs and directives.
     */
    class LIBTL_CLASS PragmaCustomCompilerPhase : public CompilerPhase
    {
        private:
            std::string _pragma_handled;
            PragmaCustomDispatcher _pragma_dispatcher;

        public:
            //! Constructor
            /*!
             * \param pragma_handled The pragma prefix actually handled in this phase.
             */
            PragmaCustomCompilerPhase(const std::string& pragma_handled);

            virtual void pre_run(DTO& data_flow);

            //! Entry point of the phase
            /*!
             * This function registers traverse functors to perform
             * a traversal on all the constructs and directives.
             */
            virtual void run(DTO& data_flow);

            //! Custom functor map for directives found in preorder
            CustomFunctorMap on_directive_pre;
            //! Custom functor map for directives found in postorder
            CustomFunctorMap on_directive_post;

            //! Function to register a directive
            /*!
             * This is required for successful parsing of directives
             */
            void register_directive(const std::string& name);
            //! Function to register a construct
            /*!
             * This is required for successful parsing of construct
             *
             * \param bound_to_statement This parameter is only meaningful in
             * Fortran and will have no effect in C/C++.  If true, the
             * construct is bounded to the next single statement. By default in
             * Fortran a construct 'name' is bound to a block of statements,
             * thus requiring a 'end name' directive to know where such block
             * ends. By binding the construct to the next statement, such 'end
             * name' it is not strictly needed anymore thus becoming optional.
             * This parameter does not have any effect in C/C++ since in those
             * languages pragma constructs are always bound to the next
             * statement since blocks are expressed by compound-statements
             * which are statements (recursively) containing other statements
             */
            void register_construct(const std::string& name, bool bound_to_statement = false);

            //! Function to activate a flag in order to warning about all the unused clauses of a pragma
            /*!
             * Each fase must activate this flag if wants to show the warnings
             */
            void warning_pragma_unused_clauses(bool warning);
    };
}

#endif // TL_PRAGMASUPPORT_HPP
