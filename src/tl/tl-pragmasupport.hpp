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
            virtual ObjectList<std::string> tokenize(const std::string& str) const;
    };

    class LIBTL_CLASS ExpressionTokenizer : public ClauseTokenizer
    {
        public:
            virtual ObjectList<std::string> tokenize(const std::string& str) const;
    };

    class LIBTL_CLASS ExpressionTokenizerTrim : public ExpressionTokenizer
    {
        public:
			virtual ObjectList<std::string> tokenize(const std::string& str) const;

        private:
            static std::string trimExp (const std::string &str);
    };

    class LIBTL_CLASS PragmaClauseArgList : public Nodecl::List
    {
        public:
        PragmaClauseArgList(Nodecl::List node)
            : Nodecl::List(node) { }
        
        //! Returns a tokenized list of strings of the arguments of the clause
        /*! 
         * Use this function when you want the arguments of the clause in string form but
         * tokenized by some ClauseTokenizer. By default ExpressionTokenizerTrim is used
         * as it is the commonest scenario
         */
        ObjectList<std::string> get_tokenized_arguments(const ClauseTokenizer& = ExpressionTokenizerTrim()) const;

        //! Returns the literal text of the arguments of the clause
        std::string get_raw_arguments() const;

        //! Returns the arguments of the clause parsed as expressions
        /*!
         * Invokes get_tokenized_arguments and for each string, parses it as an expression
         *
         * \note Be careful not to call this function more times than needed,
         * since it will generate new trees each time
         */
        ObjectList<Nodecl::NodeclBase> get_arguments_as_expressions(const ClauseTokenizer& = ExpressionTokenizerTrim()) const;

        //! Returns the arguments of the clause parsed as expressions
        /*!
         * This function behaves like
         * get_arguments_as_expressions(ClauseTokenizer) but using the
         * given ReferenceScope to perform the parse of the expressions
         */
        ObjectList<Nodecl::NodeclBase> get_arguments_as_expressions(Source::ReferenceScope, const ClauseTokenizer& = ExpressionTokenizerTrim()) const;
    };

    //! This is the dual to PragmaCustomClause
    /*! 
     * This class maps a single pragma custom clause, when the flattened
     * view provided by PragmaCustomClause is not desirable
     */
    class LIBTL_CLASS PragmaCustomSingleClause : public Nodecl::PragmaCustomClause
    {
        public:
            PragmaCustomSingleClause(Nodecl::PragmaCustomClause node)
                : Nodecl::PragmaCustomClause(node) { }
            
            //! Returns a tokenized list of strings of the arguments of the clause
            /*! 
             * Use this function when you want the arguments of the clause in string form but
             * tokenized by some ClauseTokenizer. By default ExpressionTokenizerTrim is used
             * as it is the commonest scenario
             */
            ObjectList<std::string> get_tokenized_arguments(const ClauseTokenizer& = ExpressionTokenizerTrim()) const;

            //! Returns the literal text of the arguments of the clause
            std::string get_raw_arguments() const;

            //! Returns the arguments of the clause parsed as expressions
            /*!
             * Invokes get_tokenized_arguments and for each string, parses it as an expression
             *
             * \note Be careful not to call this function more times than needed,
             * since it will generate new trees each time
             */
            ObjectList<Nodecl::NodeclBase> get_arguments_as_expressions(const ClauseTokenizer& = ExpressionTokenizerTrim()) const;

            //! Returns the arguments of the clause parsed as expressions
            /*!
             * This function behaves like
             * get_arguments_as_expressions(ClauseTokenizer) but using the
             * given ReferenceScope to perform the parse of the expressions
             */
            ObjectList<Nodecl::NodeclBase> get_arguments_as_expressions(Source::ReferenceScope, const ClauseTokenizer& = ExpressionTokenizerTrim()) const;
    };

    //! This is a helper class not related to a specific Nodecl
    /*!
     * This class gives an homogenous view of clauses that might appear
     * repeated.
     *
     * #pragma foo bar myclause(x) myclause(y)
     *
     * This class allows you to work as if myclause appeared just once as myclause(x, y)
     */
    class LIBTL_CLASS PragmaCustomClause
    {
        private:
            Nodecl::PragmaCustomLine _pragma_line;

            typedef ObjectList<Nodecl::PragmaCustomClause> PragmaCustomClauseList;
            PragmaCustomClauseList _pragma_clauses;

        public:
            // To be used only by TL::PragmaCustomLine
            PragmaCustomClause(Nodecl::PragmaCustomLine pragma_line, 
                    ObjectList<Nodecl::PragmaCustomClause> pragma_clauses);

            //! States whether this clause appears in the pragma line
            bool is_defined() const;

            //! States whether this clause appears only once in the pragma line
            bool is_singleton() const;

            //! Returns a tokenized list of strings of the arguments of the clause
            /*! 
             * Use this function when you want the arguments of the clause in string form but
             * tokenized by some ClauseTokenizer. By default ExpressionTokenizerTrim is used
             * as it is the commonest scenario
             */
            ObjectList<std::string> get_tokenized_arguments(const ClauseTokenizer& = ExpressionTokenizerTrim()) const;

            //! Returns the literal text of the arguments of the clause
            ObjectList<std::string> get_raw_arguments() const;

            //! Returns the arguments of the clause parsed as expressions
            /*!
             * Invokes get_tokenized_arguments and for each string, parses it as an expression
             *
             * \note Be careful not to call this function more times than needed,
             * since it will generate new trees each time
             */
            ObjectList<Nodecl::NodeclBase> get_arguments_as_expressions(const ClauseTokenizer& = ExpressionTokenizerTrim()) const;

            //! Returns the arguments of the clause parsed as expressions
            /*!
             * This function behaves like
             * get_arguments_as_expressions(ClauseTokenizer) but using the
             * given ReferenceScope to perform the parse of the expressions
             */
            ObjectList<Nodecl::NodeclBase> get_arguments_as_expressions(Source::ReferenceScope, const ClauseTokenizer & = ExpressionTokenizerTrim()) const;
    };

    class LIBTL_CLASS PragmaCustomParameter : public TL::PragmaClauseArgList
    {
        public:
            PragmaCustomParameter(Nodecl::List node)
                : PragmaClauseArgList(node) { }
    };

    class LIBTL_CLASS PragmaCustomLine : public Nodecl::PragmaCustomLine
    {
        public:
            PragmaCustomLine(Nodecl::PragmaCustomLine node)
                : Nodecl::PragmaCustomLine(node)
            {
            }

            //! Returns a clause by name
            TL::PragmaCustomClause get_clause(const std::string &name) const;

            //! Returns a clause by a set of alias names
            TL::PragmaCustomClause get_clause(const ObjectList<std::string>& aliased_names) const;

            //! This function returns all clauses in the order they appear in the pragma
            ObjectList<TL::PragmaCustomSingleClause> get_all_clauses() const;

            //! This function returns all clause names in the order they appear in the pragma
            ObjectList<std::string> get_all_clause_names() const;
            
            //! This function returns the parameter
            /*!
             * The parameter of the clause is just a special clause with no name that can appear
             * right after the pragma. You can perform the same operations as a PragmaCustomSingleClause
             */
            PragmaCustomParameter get_parameter() const;
    };
    
    class LIBTL_CLASS PragmaCustomCommon 
    {
        private:
            TL::PragmaCustomLine _pragma_line;
        public:
            PragmaCustomCommon(Nodecl::PragmaCustomDirective);
            PragmaCustomCommon(Nodecl::PragmaCustomStatement);
            PragmaCustomCommon(Nodecl::PragmaCustomDeclaration);

            //! Returns a clause by name
            TL::PragmaCustomClause get_clause(const std::string &name) const;

            //! Returns a clause by a set of alias names
            TL::PragmaCustomClause get_clause(const ObjectList<std::string>& aliased_names) const;

            //! This function returns all clauses in the order they appear in the pragma
            ObjectList<TL::PragmaCustomSingleClause> get_all_clauses() const;

            //! This function returns all clause names in the order they appear in the pragma
            ObjectList<std::string> get_all_clause_names() const;

            //! This function returns the parameter
            /*!
             * The parameter of the clause is just a special clause with no name that can appear
             * right after the pragma. You can perform the same operations as a PragmaCustomSingleClause
             */
            PragmaCustomParameter get_parameter() const;
    };

    // Note that this is TL::PragmaCustomDirective 
    class LIBTL_CLASS PragmaCustomDirective : public Nodecl::PragmaCustomDirective, public PragmaCustomCommon
    {
        public:
        PragmaCustomDirective(Nodecl::PragmaCustomDirective node)
            : Nodecl::PragmaCustomDirective(node), PragmaCustomCommon(node)
        {
        }
    };

    class LIBTL_CLASS PragmaCustomStatement : public Nodecl::PragmaCustomStatement, public PragmaCustomCommon
    {
        public:
        PragmaCustomStatement(Nodecl::PragmaCustomStatement node)
            : Nodecl::PragmaCustomStatement(node), PragmaCustomCommon(node)
        {
        }
    };

    class LIBTL_CLASS PragmaCustomDeclaration : public Nodecl::PragmaCustomDeclaration, public PragmaCustomCommon
    {
        public:
        PragmaCustomDeclaration(Nodecl::PragmaCustomDeclaration node)
            : Nodecl::PragmaCustomDeclaration(node), PragmaCustomCommon(node)
        {
        }
    };

    struct PragmaMapDispatcher
    {
        typedef Signal1<TL::PragmaCustomDirective> SignalDirective;
        typedef std::map<std::string, SignalDirective> DirectiveMap;

        struct Directive
        {
            DirectiveMap pre;
            DirectiveMap post;
        };
        Directive directive;

        typedef Signal1<TL::PragmaCustomStatement> SignalStatement;
        typedef std::map<std::string, SignalStatement> StatementMap;

        struct Statement
        {
            StatementMap pre;
            StatementMap post;
        };
        Statement statement;

        typedef Signal1<TL::PragmaCustomDeclaration> SignalDeclaration;
        typedef std::map<std::string, SignalDeclaration> DeclarationMap;

        struct Declaration
        {
            DeclarationMap pre;
            DeclarationMap post;
        };
        Declaration declaration;
    };

    class LIBTL_CLASS PragmaVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            std::string _pragma_handled;
            PragmaMapDispatcher& _map_dispatcher;

            template <typename T>
            std::string get_pragma_name(T t)
            {
                if (t.template is<Nodecl::PragmaCustomLine>())
                {
                    return t.template as<Nodecl::PragmaCustomLine>().get_text();
                }
                internal_error("Code unreachable", 0);
            }
        public:
            PragmaVisitor(const std::string& pragma_handled, 
                    PragmaMapDispatcher & map_dispatcher)
                : _pragma_handled(pragma_handled), _map_dispatcher(map_dispatcher) 
            { }


            virtual void visit_pre(const Nodecl::PragmaCustomDirective & n)
            {
                if (n.get_text() == _pragma_handled)
                {
                    std::string pragma_name = get_pragma_name(n.get_pragma_line());
                    PragmaMapDispatcher::DirectiveMap::iterator it = _map_dispatcher.directive.pre.find(pragma_name);

                    if (it != _map_dispatcher.directive.pre.end())
                    {
                        it->second.signal(n);
                    }
                }
            }

            virtual void visit_post(const Nodecl::PragmaCustomDirective & n)
            {
                if (n.get_text() == _pragma_handled)
                {
                    std::string pragma_name = get_pragma_name(n.get_pragma_line());
                    PragmaMapDispatcher::DirectiveMap::iterator it = _map_dispatcher.directive.post.find(pragma_name);

                    if (it != _map_dispatcher.directive.post.end())
                    {
                        it->second.signal(n);
                    }
                }
            }

            virtual void visit_pre(const Nodecl::PragmaCustomStatement & n)
            {
                if (n.get_text() == _pragma_handled)
                {
                    std::string pragma_name = get_pragma_name(n.get_pragma_line());
                    PragmaMapDispatcher::StatementMap::iterator it = _map_dispatcher.statement.pre.find(pragma_name);

                    if (it != _map_dispatcher.statement.pre.end())
                    {
                        it->second.signal(n);
                    }
                }
            }

            virtual void visit_post(const Nodecl::PragmaCustomStatement & n)
            {
                if (n.get_text() == _pragma_handled)
                {
                    std::string pragma_name = get_pragma_name(n.get_pragma_line());
                    PragmaMapDispatcher::StatementMap::iterator it = _map_dispatcher.statement.post.find(pragma_name);

                    if (it != _map_dispatcher.statement.post.end())
                    {
                        it->second.signal(n);
                    }
                }
            }

            virtual void visit_pre(const Nodecl::PragmaCustomDeclaration & n)
            {
                if (n.get_text() == _pragma_handled)
                {
                    std::string pragma_name = get_pragma_name(n.get_pragma_line());
                    PragmaMapDispatcher::DeclarationMap::iterator it = _map_dispatcher.declaration.pre.find(pragma_name);

                    if (it != _map_dispatcher.declaration.pre.end())
                    {
                        it->second.signal(n);
                    }
                }
            }

            virtual void visit_post(const Nodecl::PragmaCustomDeclaration & n)
            {
                if (n.get_text() == _pragma_handled)
                {
                    std::string pragma_name = get_pragma_name(n.get_pragma_line());
                    PragmaMapDispatcher::DeclarationMap::iterator it = _map_dispatcher.declaration.post.find(pragma_name);

                    if (it != _map_dispatcher.declaration.post.end())
                    {
                        it->second.signal(n);
                    }
                }
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
            PragmaMapDispatcher _pragma_map_dispatcher;
        protected:
            PragmaMapDispatcher& dispatcher();
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
