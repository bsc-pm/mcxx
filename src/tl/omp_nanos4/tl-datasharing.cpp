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

#include "tl-omptransform.hpp"

#include "tl-counters.hpp"

namespace TL
{
    namespace Nanos4
    {
        static void dimensional_replacements_of_variable_type_aux(Type t, Symbol sym, 
                ObjectList<Source> &dim_names, 
                ObjectList<Source> &dim_decls);
        static Type compute_replacemement_type_for_vla(Type t, ObjectList<Source>::iterator dim_names);
        static Type compute_replacemement_type_for_vla_in_outline(Type t, ObjectList<Source>::iterator dim_names);

        static void vla_handling(ObjectList<ParameterInfo>& parameter_info, 
                ReplaceIdExpression& result,
                ScopeLink sl, bool disable_restrict_pointers);

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

            C_LANGUAGE()
            {
                vla_handling(parameter_info, result, construct_body.get_scope_link(), disable_restrict_pointers);
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

        static void vla_handling(ObjectList<ParameterInfo>& parameter_info, 
                ReplaceIdExpression& result,
                ScopeLink sl, bool disable_restrict_pointers)
        {
            // Now review each parameter to see whether it is a
            // variable-sized type (so, a VLA or containing one)
            ObjectList<ParameterInfo> new_parameters;
            for (ObjectList<ParameterInfo>::iterator it = parameter_info.begin();
                    it != parameter_info.end();
                    it++)
            {
                ParameterInfo& param_info(*it);
                Symbol &symbol(param_info.symbol);

                if (symbol.get_type().is_variably_modified())
                {
                    ObjectList<Source> dim_decls;
                    ObjectList<Source> dim_names;

                    dimensional_replacements_of_variable_type_aux(symbol.get_type(),
                            symbol, dim_names, dim_decls);

                    Source new_decls;
                    for (ObjectList<Source>::iterator it = dim_decls.begin();
                            it != dim_decls.end();
                            it++)
                    {
                        new_decls << *it << ";"
                            ;
                    }

                    AST_t point_of_decl = symbol.get_point_of_declaration();
                    AST_t enclosing_stmt_tree;
                    if (symbol.is_parameter())
                    {
                        FunctionDefinition 
                            funct_def(point_of_decl.get_enclosing_function_definition(),
                                    sl);

                        enclosing_stmt_tree = funct_def.get_function_body().get_inner_statements()[0].get_ast();
                    }
                    else
                    {
                        enclosing_stmt_tree = point_of_decl.get_enclosing_statement();
                    }

                    AST_t statement_seq 
                        = new_decls.parse_statement(enclosing_stmt_tree, sl);

                    enclosing_stmt_tree.prepend(statement_seq);

                    for (ObjectList<Source>::iterator it = dim_names.begin();
                            it != dim_names.end();
                            it++)
                    {
                        Scope sc = sl.get_scope(enclosing_stmt_tree);

                        Symbol new_sym = sc.get_symbol_from_name(it->get_source());

                        if (!new_sym.is_valid())
                        {
                            internal_error("Symbol just declared, not found", 0);
                        }

                        Type pointer_type = Type::get_int_type();
                        if (!disable_restrict_pointers)
                        {
                            pointer_type = pointer_type.get_restrict_type();
                        }

                        ParameterInfo new_param(new_sym.get_name(), 
                                "&" + new_sym.get_name(),
                                new_sym, 
                                Type::get_int_type().get_pointer_to(), 
                                ParameterInfo::BY_VALUE);

                        new_parameters.append(new_param);
                    }

                    if (!symbol.is_parameter())
                    {
                        // If this is not a parameter, we'll want to rewrite the declaration itself
                        Type new_type_spawn = compute_replacemement_type_for_vla(symbol.get_type(), dim_names.begin());

                        // Now redeclare
                        Source redeclaration;
                        redeclaration
                            << new_type_spawn.get_declaration(symbol.get_scope(), symbol.get_name())
                            << ";"
                            ;

                        AST_t redeclaration_tree = redeclaration.parse_statement(enclosing_stmt_tree,
                                sl, Source::ALLOW_REDECLARATION);

                        enclosing_stmt_tree.prepend(redeclaration_tree);

                        // Now remove the declarator of the declaration
                        Declaration decl(point_of_decl, sl);

                        if (decl.get_declared_entities().size() == 1)
                        {
                            // We have to remove all the whole declaration
                            enclosing_stmt_tree.remove_in_list();
                        }
                        else
                        {
                            // Remove only this entity
                            ObjectList<DeclaredEntity> entities = decl.get_declared_entities();
                            for (ObjectList<DeclaredEntity>::iterator it = entities.begin();
                                    it != entities.end();
                                    it++)
                            {
                                if (it->get_declared_symbol() == param_info.symbol)
                                {
                                    it->get_ast().remove_in_list();
                                }
                            }
                        }
                    }

                    Type new_type_outline = compute_replacemement_type_for_vla_in_outline(symbol.get_type(), dim_names.begin());
                    new_type_outline = new_type_outline.get_pointer_to();
                    if (!disable_restrict_pointers)
                    {
                        new_type_outline = new_type_outline.get_restrict_type();
                    }

                    // Fix the info for this parameter
                    param_info.type = Type::get_void_type().get_pointer_to();
                    param_info.is_variably_modified = true;
                    param_info.vla_cast_name = param_info.parameter_name;
                    param_info.parameter_name = "_vla_" + param_info.parameter_name;
                    param_info.type_in_outline = new_type_outline;

                    // Override the replacement
                    result.add_replacement(param_info.symbol, "(*" + param_info.symbol.get_name() + ")", 
                            point_of_decl, sl);
                }
            }
            parameter_info.append(new_parameters);
        }

        static void dimensional_replacements_of_variable_type_aux(Type t, Symbol sym, 
                ObjectList<Source> &dim_names, 
                ObjectList<Source> &dim_decls)
        {
            Counter& vla_counter = CounterManager::get_counter("VLA_DIMENSIONS_COUNTER");
            if (t.is_array())
            {
                Source dim_name;
                dim_name
                    << "_" << sym.get_name() << "_" << vla_counter
                    ;
                vla_counter++;

                dimensional_replacements_of_variable_type_aux(t.array_element(), sym, dim_names, dim_decls);

                dim_names.append(dim_name);
                dim_decls.append(Source("") << "int " << dim_name << " = " << t.array_dimension().prettyprint());
            }
            else if (t.is_pointer())
            {
                dimensional_replacements_of_variable_type_aux(t.points_to(), sym, dim_names, dim_decls);
            }
        }

        static Type compute_replacemement_type_for_vla(Type t, ObjectList<Source>::iterator dim_names)
        {
            Type new_type(NULL);
            if (t.is_array())
            {
                new_type = compute_replacemement_type_for_vla(t.array_element(), dim_names + 1);

                new_type = new_type.get_array_to(*dim_names);
            }
            else if (t.is_pointer())
            {
                new_type = compute_replacemement_type_for_vla(t.points_to(), dim_names);
                new_type = new_type.get_pointer_to();
            }
            else
            {
                new_type = t;
            }

            return new_type;
        }

        static Type compute_replacemement_type_for_vla_in_outline(Type t, ObjectList<Source>::iterator dim_names)
        {
            Type new_type(NULL);
            if (t.is_array())
            {
                new_type = compute_replacemement_type_for_vla_in_outline(t.array_element(), dim_names + 1);

                new_type = new_type.get_array_to(std::string("*") + dim_names->get_source());
            }
            else if (t.is_pointer())
            {
                new_type = compute_replacemement_type_for_vla_in_outline(t.points_to(), dim_names);
                new_type = new_type.get_pointer_to();
            }
            else
            {
                new_type = t;
            }

            return new_type;
        }
    }
}
