/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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
#include "tl-nodecl.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-handler.hpp"
#include "tl-source.hpp"

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

    class LIBTL_CLASS PragmaVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
        public:
            virtual void visit_pre(const Nodecl::PragmaCustomDirective & n)
            {
            }
            virtual void visit_pre(const Nodecl::PragmaCustomDeclaration & n)
            {
            }

            virtual void visit_post(const Nodecl::PragmaCustomDirective & n)
            {
            }
            virtual void visit_post(const Nodecl::PragmaCustomDeclaration & n)
            {
            }
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
