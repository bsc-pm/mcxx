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
#include "tl-omptransform.hpp"

namespace TL
{
    namespace Nanos4
    {
        ReplaceIdExpression OpenMPTransform::set_replacements(FunctionDefinition function_definition,
                Statement construct_body,
                ObjectList<Symbol>& shared_references,
                ObjectList<Symbol>& private_references,
                ObjectList<Symbol>& firstprivate_references,
                ObjectList<Symbol>& lastprivate_references,
                ObjectList<OpenMP::ReductionSymbol>& reduction_references,
                ObjectList<OpenMP::ReductionSymbol>& inner_reduction_references,
                ObjectList<Symbol>& copyin_references,
                ObjectList<Symbol>& copyprivate_references,
                ObjectList<ParameterInfo>& parameter_info,
                bool share_always /* = false */)
        {
            Symbol function_symbol = function_definition.get_function_name().get_symbol();
            Scope function_scope = function_definition.get_scope();
            ReplaceIdExpression result;

            ObjectList<Symbol> actually_shared = shared_references;
            // lastprivate references require sharing the target variable
            actually_shared.insert(lastprivate_references);

            // SHARED references
            for (ObjectList<Symbol>::iterator it = actually_shared.begin();
                    it != actually_shared.end();
                    it++)
            {
                // We ignore unqualified/qualified references that are function accessible
                // or unqualified references that are data members of the same class
                // of this function because they can be accessed magically
                if (!share_always 
                        && is_function_accessible(*it)
                        && !is_unqualified_member_symbol(*it, function_definition))
                    continue;

                Symbol &symbol(*it);
                if (!is_unqualified_member_symbol(symbol, function_definition))
                {
                    Type type = symbol.get_type();
                    // Type pointer_type = type.get_pointer_to();

                    // C/C++ oddity
                    Type pointer_type(NULL);

                    if (type.is_array())
                    {
                        // Make an array to pointer conversion because it can be
                        // restricted (C99 allows restricting arrays though but
                        // with a strange syntax)
                        pointer_type = type.array_element().get_pointer_to();
                        if (!disable_restrict_pointers)
                        {
                            pointer_type = pointer_type.get_restrict_type();
                        }

                        ParameterInfo parameter(symbol.get_name(), 
                                symbol.get_qualified_name(construct_body.get_scope()), 
                                symbol, pointer_type, ParameterInfo::BY_POINTER);
                        parameter_info.append(parameter);
                    }
                    else
                    {
                        pointer_type = type.get_pointer_to();

                        if (!disable_restrict_pointers)
                        {
                            pointer_type = pointer_type.get_restrict_type();
                        }

                        ParameterInfo parameter(symbol.get_name(), 
                                "&" + symbol.get_qualified_name(construct_body.get_scope()), 
                                symbol, pointer_type, ParameterInfo::BY_POINTER);
                        parameter_info.append(parameter);
                        result.add_replacement(symbol, "(*" + symbol.get_name() + ")", 
                                construct_body.get_ast(), construct_body.get_scope_link());
                    }
                }
                else
                {
                    // Only if this function is a nonstatic one we need _this access
                    if (is_nonstatic_member_function(function_definition))
                    {
                        result.add_replacement(symbol, "_this->" + symbol.get_name(), 
                                construct_body.get_ast(), construct_body.get_scope_link());
                    }
                }
            }

            // PRIVATE references
            for (ObjectList<Symbol>::iterator it = private_references.begin();
                    it != private_references.end();
                    it++)
            {
                Symbol &symbol(*it);
                Type type = symbol.get_type();

                result.add_replacement(symbol, "p_" + symbol.get_name(),
                        construct_body.get_ast(), construct_body.get_scope_link());
            }

            // FIRSTPRIVATE references
            for (ObjectList<Symbol>::iterator it = firstprivate_references.begin();
                    it != firstprivate_references.end();
                    it++)
            {
                Symbol &symbol(*it);
                Type type = symbol.get_type();

                // Old versions pass firstprivate in a different way
                ParameterInfo::parameter_kind_t param_kind = ParameterInfo::BY_VALUE;

                if (type.is_array())
                {
                    Type pointer_type = type.array_element().get_pointer_to();
                    if (!disable_restrict_pointers)
                    {
                        pointer_type = pointer_type.get_restrict_type();
                    }

                    ParameterInfo parameter("flp_" + symbol.get_name(), 
                            symbol.get_qualified_name(construct_body.get_scope()),
                            symbol, pointer_type, param_kind);
                    parameter_info.append(parameter);

                    result.add_replacement(symbol, "flp_" + symbol.get_name(),
                            construct_body.get_ast(), construct_body.get_scope_link());
                }
                else
                {
                    Type pointer_type = type.get_pointer_to();
                    if (!disable_restrict_pointers)
                    {
                        pointer_type = pointer_type.get_restrict_type();
                    }

                    ParameterInfo parameter("flp_" + symbol.get_name(), 
                            "&" + symbol.get_qualified_name(construct_body.get_scope()),
                            symbol, pointer_type, param_kind);
                    parameter_info.append(parameter);

                    result.add_replacement(symbol, "(*flp_" + symbol.get_name() + ")",
                            construct_body.get_ast(), construct_body.get_scope_link());
                }

            }

            // REDUCTION references
            for (ObjectList<OpenMP::ReductionSymbol>::iterator it = reduction_references.begin();
                    it != reduction_references.end();
                    it++)
            {
                OpenMP::ReductionSymbol &reduction_symbol(*it);
                Symbol symbol = reduction_symbol.get_symbol();
                Type type = symbol.get_type();

                Type pointer_type = type.get_pointer_to();
                if (!disable_restrict_pointers)
                {
                    pointer_type = pointer_type.get_restrict_type();
                }

                ParameterInfo parameter("rdv_" + symbol.get_name(), 
                        "rdv_" + symbol.get_name(),
                        symbol, pointer_type, ParameterInfo::BY_POINTER);
                parameter_info.append(parameter);

                result.add_replacement(symbol, "rdp_" + symbol.get_name(),
                        construct_body.get_ast(), construct_body.get_scope_link());
            }

            // Inner REDUCTION references (those coming from lexical enclosed DO's inner to this PARALLEL)
            for (ObjectList<OpenMP::ReductionSymbol>::iterator it = inner_reduction_references.begin();
                    it != inner_reduction_references.end();
                    it++)
            {
                Symbol symbol(it->get_symbol());
                Type type = symbol.get_type();

                Type pointer_type = type.get_pointer_to();
                if (!disable_restrict_pointers)
                {
                    pointer_type = pointer_type.get_restrict_type();
                }

                ParameterInfo reduction_vector_parameter("rdv_" + symbol.get_name(), 
                        "rdv_" + symbol.get_name(),
                        symbol, pointer_type, ParameterInfo::BY_POINTER);

                parameter_info.append(reduction_vector_parameter);

                ParameterInfo parameter(symbol.get_name(), 
                        "&" + symbol.get_name(),
                        symbol, pointer_type, ParameterInfo::BY_POINTER);
            }

            // COPYIN references
            for (ObjectList<Symbol>::iterator it = copyin_references.begin();
                    it != copyin_references.end();
                    it++)
            {
                Symbol &symbol(*it);
                Type type = symbol.get_type();

                Type pointer_type = type.get_pointer_to();
                if (!disable_restrict_pointers)
                {
                    pointer_type = pointer_type.get_restrict_type();
                }

                ParameterInfo parameter("cin_" + symbol.get_name(), 
                        "&" + symbol.get_qualified_name(construct_body.get_scope()),
                        *it, type, ParameterInfo::BY_POINTER);
                parameter_info.append(parameter);
            }

            // COPYPRIVATE references
            for (ObjectList<Symbol>::iterator it = copyprivate_references.begin();
                    it != copyprivate_references.end();
                    it++)
            {
                Symbol &symbol(*it);
                Type type = symbol.get_type();

                Type pointer_type = type.get_pointer_to();
                if (!disable_restrict_pointers)
                {
                    pointer_type = pointer_type.get_restrict_type();
                }

                ParameterInfo parameter("cout_" + symbol.get_name(),
                        "&" + symbol.get_qualified_name(construct_body.get_scope()),
                        *it, pointer_type, ParameterInfo::BY_POINTER);
                parameter_info.append(parameter);
            }

            if (is_nonstatic_member_function(function_definition))
            {
                // Calls to nonstatic member functions within the body of the construct
                // of a nonstatic member function
                ObjectList<Symbol> function_references = 
                    construct_body.non_local_symbol_occurrences(Statement::ONLY_FUNCTIONS)
                    .map(functor(&IdExpression::get_symbol));
                for (ObjectList<Symbol>::iterator it = function_references.begin();
                        it != function_references.end();
                        it++)
                {
                    if (is_unqualified_member_symbol(*it, function_definition))
                    {
                        Symbol &symbol(*it);
                        result.add_replacement(symbol, "_this->" + symbol.get_name(),
                                construct_body.get_ast(), construct_body.get_scope_link());
                    }
                }
            }

            return result;
        }

        ReplaceIdExpression OpenMPTransform::set_replacements_inline(FunctionDefinition function_definition,
                Statement construct_body,
                ObjectList<Symbol>& shared_references,
                ObjectList<Symbol>& private_references,
                ObjectList<Symbol>& firstprivate_references,
                ObjectList<Symbol>& lastprivate_references,
                ObjectList<OpenMP::ReductionSymbol>& reduction_references,
                ObjectList<OpenMP::ReductionSymbol>& inner_reduction_references,
                ObjectList<Symbol>& copyin_references,
                ObjectList<Symbol>& copyprivate_references)
        {
            Symbol function_symbol = function_definition.get_function_name().get_symbol();
            Scope function_scope = function_definition.get_scope();
            ReplaceIdExpression result;

            // Nothing to do for SHARED references

            // PRIVATE references
            for (ObjectList<Symbol>::iterator it = private_references.begin();
                    it != private_references.end();
                    it++)
            {
                Symbol &symbol(*it);

                result.add_replacement(symbol, "p_" + symbol.get_name(),
                        construct_body.get_ast(), construct_body.get_scope_link());
            }

            // FIRSTPRIVATE references
            // Likewise PRIVATE
            for (ObjectList<Symbol>::iterator it = firstprivate_references.begin();
                    it != firstprivate_references.end();
                    it++)
            {
                Symbol &symbol(*it);

                result.add_replacement(symbol, "p_" + symbol.get_name(),
                        construct_body.get_ast(), construct_body.get_scope_link());
            }

            // LASTPRIVATE
            {
                // Prune all lastprivate that are also firstprivate
                ObjectList<Symbol> pruned_lastprivate_references;
                pruned_lastprivate_references
                    .append(lastprivate_references.filter(
                                not_in_set(firstprivate_references)));

                for (ObjectList<Symbol>::iterator it = pruned_lastprivate_references.begin();
                        it != pruned_lastprivate_references.end();
                        it++)
                {
                    Symbol &symbol(*it);

                    result.add_replacement(symbol, "p_" + symbol.get_name(),
                            construct_body.get_ast(), construct_body.get_scope_link());
                }
            }

            // REDUCTION references
            for (ObjectList<OpenMP::ReductionSymbol>::iterator it = reduction_references.begin();
                    it != reduction_references.end();
                    it++)
            {
                OpenMP::ReductionSymbol &reduction_symbol(*it);
                Symbol symbol = reduction_symbol.get_symbol();

                result.add_replacement(symbol, "rdp_" + symbol.get_name(),
                        construct_body.get_ast(), construct_body.get_scope_link());
            }

            return result;
        }
    }
}
