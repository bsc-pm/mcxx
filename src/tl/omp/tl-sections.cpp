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
    void OpenMPTransform::sections_preorder(OpenMP::SectionsConstruct /* sections_construct */)
    {
        // We push a new level of sections with zero "section" counted
        // so far
        num_sections_stack.push(0);
    }

    void OpenMPTransform::sections_postorder(OpenMP::SectionsConstruct sections_construct)
    {
        // They will hold the entities as they appear in the clauses
        ObjectList<Symbol> shared_references;
        ObjectList<Symbol> private_references;
        ObjectList<Symbol> firstprivate_references;
        ObjectList<Symbol> lastprivate_references;
        ObjectList<OpenMP::ReductionSymbol> reduction_references;
        ObjectList<Symbol> copyin_references;
        ObjectList<Symbol> copyprivate_references;

        // Get the construct_body of the statement
        OpenMP::Directive directive = sections_construct.directive();
        Statement construct_body = sections_construct.body();

        // Get the enclosing function definition
        FunctionDefinition function_definition = sections_construct.get_enclosing_function();
        // its scope
        Scope function_scope = function_definition.get_scope();

        // Get the data attributes for every entity
        get_data_attributes(sections_construct,
                directive,
                construct_body,
                shared_references,
                private_references,
                firstprivate_references,
                lastprivate_references,
                reduction_references,
                copyin_references,
                copyprivate_references);

        ObjectList<ParameterInfo> parameter_info_list;

        ObjectList<OpenMP::ReductionSymbol> reduction_empty;

        ReplaceIdExpression replace_references = 
            set_replacements(function_definition,
                    directive,
                    construct_body,
                    shared_references,
                    private_references,
                    firstprivate_references,
                    lastprivate_references,
                    reduction_references,
                    reduction_empty,
                    copyin_references,
                    copyprivate_references,
                    parameter_info_list);

        int num_sections = num_sections_stack.top();

        Source loop_distribution_code;

        loop_distribution_code = get_loop_distribution_in_sections(num_sections,
                construct_body,
                replace_references);

        // In fact we are not passing anything by parameters since
        // there is no outline here
        parameter_info_list.clear();

        Source private_declarations = get_privatized_declarations(
                sections_construct,
                private_references,
                firstprivate_references,
                lastprivate_references,
                reduction_references,
                copyin_references,
                parameter_info_list
                ); 

        Source lastprivate_code;

        if (!lastprivate_references.empty())
        {
            Source lastprivate_assignments = get_lastprivate_assignments(
                    lastprivate_references,
                    copyprivate_references,
                    parameter_info_list);

            lastprivate_code 
                << "if (intone_last != 0)"
                << "{"
                <<    lastprivate_assignments
                << "}"
                ;
        }

        OpenMP::Clause nowait_clause = directive.nowait_clause();
        Source loop_finalization = get_loop_finalization(/*do_barrier=*/!(nowait_clause.is_defined()));

        Source reduction_code;

        bool orphaned = (parallel_nesting == 0);
        if (orphaned)
        {
            reduction_code = get_critical_reduction_code(reduction_references);
        }
        else
        {
            reduction_code = get_noncritical_inlined_reduction_code(reduction_references, construct_body);
        }

        Source sections_source;
        sections_source 
            << "{"
            <<    private_declarations
            <<    loop_distribution_code
            <<    lastprivate_code
            <<    reduction_code
            <<    loop_finalization
            << "}"
            ;

        num_sections_stack.pop();

        AST_t sections_tree = sections_source.parse_statement(sections_construct.get_ast(),
                sections_construct.get_scope_link());

        sections_construct.get_ast().replace(sections_tree);
    }
}
