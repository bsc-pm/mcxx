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
#include "tl-omptransform.hpp"

namespace TL
{
    namespace Nanos4
    {
        void OpenMPTransform::add_data_attribute_to_list(
                OpenMP::Construct &construct,
                ObjectList<Symbol> list_id_expressions,
                OpenMP::DataAttribute data_attrib)
        {
            for (ObjectList<Symbol>::iterator it = list_id_expressions.begin();
                    it != list_id_expressions.end();
                    it++)
            {
                Symbol &symbol (*it);
                construct.add_data_attribute(symbol, data_attrib);
            }
        }

        void OpenMPTransform::get_data_explicit_attributes(
                OpenMP::Construct & construct,
                OpenMP::Directive directive,
                ObjectList<Symbol>& shared_references,
                ObjectList<Symbol>& private_references,
                ObjectList<Symbol>& firstprivate_references,
                ObjectList<Symbol>& lastprivate_references,
                ObjectList<OpenMP::ReductionSymbol>& reduction_references,
                ObjectList<Symbol>& copyin_references,
                ObjectList<Symbol>& copyprivate_references)
        {
            // Get references in shared clause
            OpenMP::Clause shared_clause = directive.shared_clause();
            shared_references.insert(shared_clause.id_expressions().map(functor(&IdExpression::get_symbol)));

            add_data_attribute_to_list(construct, shared_references, OpenMP::DA_SHARED);

            // Get references in private_clause
            OpenMP::Clause private_clause = directive.private_clause();
            private_references.insert(private_clause.id_expressions().map(functor(&IdExpression::get_symbol)));

            add_data_attribute_to_list(construct, private_references, OpenMP::DA_PRIVATE);

            // Get references in firstprivate clause
            OpenMP::Clause firstprivate_clause = directive.firstprivate_clause();
            firstprivate_references.insert(firstprivate_clause.id_expressions().map(functor(&IdExpression::get_symbol)));

            add_data_attribute_to_list(construct, firstprivate_references, OpenMP::DA_FIRSTPRIVATE);

            // Get references in lastprivate clause
            OpenMP::Clause lastprivate_clause = directive.lastprivate_clause();
            lastprivate_references.insert(lastprivate_clause.id_expressions().map(functor(&IdExpression::get_symbol)));

            add_data_attribute_to_list(construct, lastprivate_references, OpenMP::DA_LASTPRIVATE);

            // Get references in reduction clause
            OpenMP::ReductionClause reduction_clause = directive.reduction_clause();
            reduction_references = reduction_clause.id_expressions();

            add_data_attribute_to_list(construct, 
                    reduction_references.map(functor(&OpenMP::ReductionSymbol::get_symbol)), 
                    OpenMP::DA_REDUCTION);

            // Get references in copyin
            OpenMP::Clause copyin_clause = directive.copyin_clause();
            copyin_references.insert(copyin_clause.id_expressions().map(functor(&IdExpression::get_symbol)));

            add_data_attribute_to_list(construct, copyin_references, OpenMP::DA_COPYIN);

            // Get references in copyprivate
            OpenMP::Clause copyprivate_clause = directive.copyprivate_clause();
            copyprivate_references.insert(copyprivate_clause.id_expressions().map(functor(&IdExpression::get_symbol)));

            add_data_attribute_to_list(construct, copyprivate_references, OpenMP::DA_COPYPRIVATE);
        }


        void OpenMPTransform::get_data_attributes(
                OpenMP::Construct &construct,
                OpenMP::Directive directive,
                Statement construct_body,
                ObjectList<Symbol>& shared_references,
                ObjectList<Symbol>& private_references,
                ObjectList<Symbol>& firstprivate_references,
                ObjectList<Symbol>& lastprivate_references,
                ObjectList<OpenMP::ReductionSymbol>& reduction_references,
                ObjectList<Symbol>& copyin_references,
                ObjectList<Symbol>& copyprivate_references)
        {
            get_data_explicit_attributes(
                    construct,
                    directive,
                    shared_references,
                    private_references,
                    firstprivate_references,
                    lastprivate_references,
                    reduction_references,
                    copyin_references,
                    copyprivate_references);

            enum
            {
                PK_DATA_INVALID = 0,
                PK_DATA_SHARED, 
                PK_DATA_PRIVATE,
                PK_DATA_NONE,
            } default_data_sharing = PK_DATA_INVALID;

            OpenMP::DefaultClause default_clause = directive.default_clause();

            if (!default_clause.is_defined())
            {
                // By default it is shared
                default_data_sharing = PK_DATA_SHARED;
            }
            else if (default_clause.is_none())
            {
                default_data_sharing = PK_DATA_NONE;
            }
            else if (default_clause.is_shared())
            {
                default_data_sharing = PK_DATA_SHARED;
            }
            // An extension that we consider sensible
            else if (default_clause.is_custom("private"))
            {
                default_data_sharing = PK_DATA_PRIVATE;
            }
            else
            {
                std::cerr << "Warning: Unknown default clause '" 
                    << default_clause.prettyprint() << "' at " << default_clause.get_ast().get_locus() << ". "
                    << "Assuming 'default(shared)'."
                    << std::endl;
                default_data_sharing = PK_DATA_SHARED;
            }

            // Get every non local reference: this is, not defined in the
            // construct itself, but visible at the point where the
            // construct is defined
            ObjectList<Symbol> non_local_references = construct_body.non_local_symbol_occurrences(Statement::ONLY_VARIABLES)
                .map(functor(&IdExpression::get_symbol));
            // ObjectList<Symbol> non_local_symbols = non_local_references.map(functor(&IdExpression::get_symbol));

            // Filter shareds, privates, firstprivate, lastprivate or
            // reduction that are useless
            ObjectList<Symbol> unreferenced;
            // Add to unreferenced symbols that appear in shared_references but not in non_local_references
            unreferenced.append(shared_references.filter(not_in_set(non_local_references)));
            // shared_references now only contains references that appear in non_local_references
            shared_references = shared_references.filter(in_set(non_local_references));

            // Add to unreferenced symbols that appear in private_references but not in non_local_references
            unreferenced.append(private_references.filter(not_in_set(non_local_references)));
            // private_references now only contains references that appear in non_local_references
            private_references = private_references.filter(in_set(non_local_references));

            // Add to unreferenced symbols that appear in lastprivate_references but not in non_local_references
            unreferenced.append(firstprivate_references.filter(not_in_set(non_local_references)));
            // firstprivate_references now only contains references that appear in non_local_references
            firstprivate_references = firstprivate_references.filter(in_set(non_local_references));

            // Add to unreferenced symbols that appear in lastprivate_references but not in non_local_references
            unreferenced.append(lastprivate_references.filter(not_in_set(non_local_references)));
            // lastprivate_references now only contains references that appear in non_local_references
            lastprivate_references = lastprivate_references.filter(in_set(non_local_references));

            // Add to unreferenced symbols that appear in copyin_references but not in non_local_references
            unreferenced.append(copyin_references.filter(not_in_set(non_local_references)));
            // copyin_references now only contains references that appear in non_local_references
            copyin_references = copyin_references.filter(in_set(non_local_references));

            // Add to unreferenced symbols that appear in copyprivate_references but not in non_local_references
            unreferenced.append(copyprivate_references.filter(not_in_set(non_local_references)));
            // copyprivate_references now only contains references that appear in non_local_references
            copyprivate_references = copyprivate_references.filter(in_set(non_local_references));

            // Add to unreferenced symbols that appear in reduction_references but not in non_local_references
            unreferenced.append(
                    reduction_references.filter(not_in_set(non_local_references, 
                            functor(&OpenMP::ReductionSymbol::get_symbol))).map(functor(&OpenMP::ReductionSymbol::get_symbol))
                    );
            // reduction_references now only contains references that appear in non_local_references
            reduction_references = reduction_references.filter(in_set(non_local_references, 
                        functor(&OpenMP::ReductionSymbol::get_symbol)));

            // Will give a warning for every unreferenced element
            unreferenced.map(functor(&OpenMPTransform::warn_unreferenced_data, *this));

            // If a symbol appears into shared_references, private_references, firstprivate_references, lastprivate_references
            // or copyin_references, copyprivate_references, remove it from non_local_references
            non_local_references = non_local_references.filter(not_in_set(shared_references));
            non_local_references = non_local_references.filter(not_in_set(private_references));
            non_local_references = non_local_references.filter(not_in_set(firstprivate_references));
            non_local_references = non_local_references.filter(not_in_set(lastprivate_references));
            non_local_references = non_local_references.filter(not_in_set(copyin_references));
            non_local_references = non_local_references.filter(not_in_set(copyprivate_references));

            // Get every id-expression related to the ReductionSymbol list
            ObjectList<Symbol> reduction_id_symbols = 
                reduction_references.map(functor(&OpenMP::ReductionSymbol::get_symbol));
            // and remove it from non_local_references
            non_local_references = non_local_references.filter(not_in_set(reduction_id_symbols));

            switch ((int)default_data_sharing)
            {
                case PK_DATA_NONE :
                    {
                        non_local_references.map(functor(&OpenMPTransform::warn_no_data_sharing, *this));
                        /* Fall through shared */
                    }
                case PK_DATA_SHARED :
                    {
                        shared_references.insert(non_local_references);
                        add_data_attribute_to_list(construct, non_local_references, OpenMP::DA_SHARED);
                        break;
                    }
                case PK_DATA_PRIVATE :
                    {
                        private_references.insert(non_local_references);
                        add_data_attribute_to_list(construct, non_local_references, OpenMP::DA_PRIVATE);
                        break;
                    }
                case PK_DATA_INVALID :
                default:
                    {
                        break;
                    }
            }
        }

        ReplaceIdExpression OpenMPTransform::set_replacements(FunctionDefinition function_definition,
                OpenMP::Directive,
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

            // SHARED references
            for (ObjectList<Symbol>::iterator it = shared_references.begin();
                    it != shared_references.end();
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
                if (Nanos4::Version::version < 4200)
                {
                    param_kind = ParameterInfo::BY_POINTER;
                }

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
                }

                result.add_replacement(symbol, "(*flp_" + symbol.get_name() + ")",
                        construct_body.get_ast(), construct_body.get_scope_link());
            }

            {
                ObjectList<Symbol> pruned_lastprivate_references;
                if (Nanos4::Version::version < 4200)
                {
                    // Old versions required special handling for LASTPRIVATE
                    // variables appearing in FIRSTPRIVATE
                    //
                    // LASTPRIVATE references that do not already appear in
                    // FIRSTPRIVATE
                    pruned_lastprivate_references
                        .append(lastprivate_references.filter(
                                    not_in_set(firstprivate_references)));
                }
                else
                {
                    pruned_lastprivate_references = lastprivate_references;
                }

                for (ObjectList<Symbol>::iterator it = pruned_lastprivate_references.begin();
                        it != pruned_lastprivate_references.end();
                        it++)
                {
                    Symbol &symbol(*it);
                    Type type = symbol.get_type();

                    std::string symbol_name = symbol.get_name();
                    if (Nanos4::Version::version < 4200)
                    {
                        symbol_name = "flp_" + symbol.get_name();
                    }
                    else
                    {
                        symbol_name = "lp_" + symbol.get_name();
                    }

                    if (type.is_array())
                    {
                        Type pointer_type = type.array_element().get_pointer_to();
                        if (!disable_restrict_pointers)
                        {
                            pointer_type = pointer_type.get_restrict_type();
                        }

                        ParameterInfo parameter(symbol_name, 
                                symbol.get_qualified_name(construct_body.get_scope()), 
                                symbol, pointer_type, ParameterInfo::BY_POINTER);
                        parameter_info.append(parameter);
                    }
                    else
                    {
                        Type pointer_type = type.get_pointer_to();
                        if (!disable_restrict_pointers)
                        {
                            pointer_type = pointer_type.get_restrict_type();
                        }

                        ParameterInfo parameter(symbol_name, 
                                "&" + symbol.get_qualified_name(construct_body.get_scope()), 
                                *it, pointer_type, ParameterInfo::BY_POINTER);
                        parameter_info.append(parameter);
                    }

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
    }
}
