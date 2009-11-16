/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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
#include "tl-compilerphase.hpp"
#include "tl-langconstruct.hpp"
#include "tl-handler.hpp"
#include "tl-traverse.hpp"
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

    class LIBTL_CLASS PragmaCustomClause : public LangConstruct
    {
        private:
            std::string _clause_name;

            ObjectList<AST_t> filter_pragma_clause();

        public:
            PragmaCustomClause(const std::string& src, AST_t ref, ScopeLink scope_link)
                : LangConstruct(ref, scope_link), _clause_name(src) 
            {
            }

            // Convenience function, it returns all the arguments parsed as expressions
            ObjectList<Expression> get_expression_list();

            ObjectList<IdExpression> id_expressions(IdExpressionCriteria criteria = VALID_SYMBOLS);
            
            // Convenience function, it returns all the id-expressions of the arguments when 
            // parsed as expressions
            ObjectList<IdExpression> get_id_expressions(IdExpressionCriteria criteria = VALID_SYMBOLS);

            // Raw clause arguments for custom parsing
            ObjectList<std::string> get_arguments();

            // Raw clause arguments for custom parsing with a given tokenizer
            ObjectList<std::string> get_arguments(const ClauseTokenizer&);

            // Raw clause arguments for even more custom parsing
            ObjectList<ObjectList<std::string> > get_arguments_unflattened();

            // Raw clause arguments tree for custom parsing
            ObjectList<AST_t> get_arguments_tree();

            // States whether the clause was in the pragma
            bool is_defined();
            
            // return the name of the current clause
            std::string get_clause_name() { return _clause_name; }
    };

    class LIBTL_CLASS PragmaCustomConstruct : public LangConstruct, public LinkData
    {
        public:
            PragmaCustomConstruct(AST_t ref, ScopeLink scope_link)
                : LangConstruct(ref, scope_link)
            {
            }

            std::string get_pragma();
            std::string get_directive();

            bool is_directive();
            bool is_construct();

            Statement get_statement();
            AST_t get_declaration();

            bool is_function_definition();

            bool is_parameterized();
            ObjectList<IdExpression> get_parameter_id_expressions(IdExpressionCriteria criteria = VALID_SYMBOLS);
            ObjectList<Expression> get_parameter_expressions();
            ObjectList<std::string> get_parameter_arguments();
            ObjectList<std::string> get_parameter_arguments(const ClauseTokenizer& tokenizer);

            ObjectList<std::string> get_clause_names();
            PragmaCustomClause get_clause(const std::string& name);
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

            std::stack<PragmaCustomConstruct*> _construct_stack;

            void dispatch_pragma_construct(CustomFunctorMap& search_map, PragmaCustomConstruct& pragma_custom_construct);
        public:
            PragmaCustomDispatcher(const std::string& pragma_handled, 
                    CustomFunctorMap& pre_map,
                    CustomFunctorMap& post_map);

            virtual void preorder(Context ctx, AST_t node);
            virtual void postorder(Context ctx, AST_t node);
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
            //! Custom functor map for directives found in preorder
            CustomFunctorMap on_directive_post;

            //! Function to register a directive
            /*!
             * This is required for successful parsing of directives
             */
            void register_directive(const std::string& name);
            //! Function to register a construct
            /*!
             * This is required for successful parsing of construct
             */
            void register_construct(const std::string& name);
    };
}

#endif // TL_PRAGMASUPPORT_HPP
