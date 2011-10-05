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


#include "tl-omp-core.hpp"
#include "tl-omp-udr_2.hpp"

namespace TL
{
    namespace OpenMP
    {
        static bool udr_is_builtin_operator(const std::string& op_name)
        {
            return (op_name == "+"
                    || op_name == "-"
                    || op_name == "*"
                    || op_name == "/"
                    || op_name == "&"
                    || op_name == "|"
                    || op_name == "^"
                    || op_name == "&&"
                    || op_name == "||");
        }


        void Core::get_reduction_symbols(
                TL::PragmaCustomLine construct,
                TL::PragmaCustomClause clause, 
                ObjectList<ReductionSymbol>& sym_list)
        {
            DEBUG_CODE()
            {
                std::cerr << "=== Reduction clause [" << construct.get_locus() << "]===" << std::endl;
            }

            if (!clause.is_defined())
                return;

            ObjectList<std::string> clause_arguments = clause.get_raw_arguments();

            for (ObjectList<std::string>::iterator list_it = clause_arguments.begin();
                    list_it != clause_arguments.end();
                    list_it++)
            {
                // The first argument is special, we have to look for a ':' that is not followed by any other ':'
                // #pragma omp parallel for reduction(A::F : A::d)
                std::string current_argument = *list_it;

                // Trim blanks
                current_argument.erase(std::remove(current_argument.begin(), current_argument.end(), ' '), current_argument.end());

                std::string::iterator split_colon = current_argument.end();
                for (std::string::iterator it = current_argument.begin();
                        it != current_argument.end();
                        it++)
                {
                    if ((*it) == ':'
                            && (it + 1) != current_argument.end())
                    {
                        if (*(it + 1) != ':')
                        {
                            split_colon = it;
                            break;
                        }
                        else
                        {
                            // Next one is also a ':' but it is not a valid splitting
                            // ':', so ignore it
                            it++;
                        }
                    }
                }

                if (split_colon == current_argument.end())
                {
                    std::cerr << clause.get_locus() << ": warning: 'reduction' clause does not have a valid operator" << std::endl;
                    std::cerr << clause.get_locus() << ": warning: skipping the whole clause" << std::endl;
                    return;
                }

                std::string original_reductor_name;
                std::copy(current_argument.begin(), split_colon, std::back_inserter(original_reductor_name));

                std::string remainder_arg;
                std::copy(split_colon + 1, current_argument.end(), std::back_inserter(remainder_arg));

                // Tokenize variable list
                ObjectList<std::string> variables = ExpressionTokenizerTrim().tokenize(remainder_arg);

                for (ObjectList<std::string>::iterator it = variables.begin();
                        it != variables.end();
                        it++)
                {
                    std::string &variable(*it);
                    Source src;
                    src
                        << "#line " << construct.get_line() << " \"" << construct.get_filename() << "\"\n"
                        << variable
                        ;

                    Nodecl::NodeclBase var_tree = src.parse_expression(clause.get_pragma_line());
                    Symbol var_sym = var_tree.get_symbol();

                    if (!var_sym.is_valid())
                    {
                        running_error("%s: error: variable '%s' in reduction clause is not valid\n",
                                construct.get_locus().c_str(),
                                var_tree.prettyprint().c_str());
                    }

                    Type var_type = var_sym.get_type();

                    std::string reductor_name = original_reductor_name;
                    // Ammend as needed the reductor name for this variable
                    CXX_LANGUAGE()
                    {
                        if (reductor_name[0] == '.')
                        {
                            if (!var_type.is_named_class()
                                    && !var_type.is_dependent())
                            {
                                std::cerr << construct.get_locus() << ": warning: reductor '" << reductor_name 
                                    << "' is no valid for non class-type variable '" << var_sym.get_qualified_name() << "'"
                                    << ", skipping"
                                    << std::endl;
                                continue;
                            }
                            else
                            {
                                reductor_name = var_type.get_declaration(construct.retrieve_context(), "") + "::" + reductor_name.substr(1);
                            }
                        }
                    }

                    if (var_sym.is_dependent_entity())
                    {
                        std::cerr << construct.get_locus() << ": warning: symbol "
                            << "'" << var_tree.prettyprint() << "' is dependent, skipping it" << std::endl;
                    }
                    else
                    {
                        Nodecl::NodeclBase reductor_name_tree;

                        bool found = false;
                        UDRInfoItem2 udr2;
                        udr2.set_type(var_type);

                        if (var_type.is_class())
                        {
                            reductor_name_tree
                                = udr2.parse_omp_udr_operator_name(construct, reductor_name);
                        }

                        if (reductor_name_tree.is<Nodecl::ErrExpr>())
                        {
                            running_error("%s: error: no suitable reductor operator '%s' was found for reduced variable '%s' of type '%s'",
                                    construct.get_locus().c_str(),
                                    reductor_name.c_str(),
                                    var_tree.prettyprint().c_str(),
                                    var_sym.get_type().get_declaration(var_sym.get_scope(), "").c_str());
                        }
                        else
                        {
                            ReductionSymbol red_sym(var_sym, udr2);
                            sym_list.append(red_sym);
                            if (!udr2.is_builtin_operator() /* && construct.get_show_warnings( ) */)
                            {
                                std::cerr << construct.get_locus() 
                                    << ": note: reduction of variable '" << var_sym.get_name() << "' solved to '" 
                                    << reductor_name << "'"
                                    << std::endl;
                            }
                        }
                    }
                }
            }
        }
    }
}
