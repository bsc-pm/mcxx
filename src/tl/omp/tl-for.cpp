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
    void OpenMPTransform::for_preorder(OpenMP::ForConstruct for_construct)
    {
        OpenMP::Directive directive = for_construct.directive();

        Statement construct_body = for_construct.body();
        // The construct is in fact a ForStatement in a #pragma omp parallel do
        ForStatement for_statement(construct_body);

        IdExpression induction_var = for_statement.get_induction_variable();

        // Save this induction var in the stack
        induction_var_stack.push(induction_var.get_symbol());
        
        // They will hold the entities as they appear in the clauses
        ObjectList<Symbol>& shared_references = 
            for_construct.get_data<ObjectList<Symbol> >("shared_references");
        ObjectList<Symbol>& private_references = 
            for_construct.get_data<ObjectList<Symbol> >("private_references");
        ObjectList<Symbol>& firstprivate_references = 
            for_construct.get_data<ObjectList<Symbol> >("firstprivate_references");
        ObjectList<Symbol>& lastprivate_references = 
            for_construct.get_data<ObjectList<Symbol> >("lastprivate_references");
        ObjectList<OpenMP::ReductionSymbol>& reduction_references =
            for_construct.get_data<ObjectList<OpenMP::ReductionSymbol> >("reduction_references");
        ObjectList<Symbol>& copyin_references = 
            for_construct.get_data<ObjectList<Symbol> >("copyin_references");
        ObjectList<Symbol>& copyprivate_references = 
            for_construct.get_data<ObjectList<Symbol> >("copyprivate_references");

        // Get the data attributes for every entity
        get_data_explicit_attributes(
                for_construct,
                directive,
                shared_references,
                private_references,
                firstprivate_references,
                lastprivate_references,
                reduction_references,
                copyin_references,
                copyprivate_references);

        // Set it private if it was not
        if ((for_construct.get_data_attribute(induction_var.get_symbol()) & OpenMP::DA_PRIVATE) != OpenMP::DA_PRIVATE)
        {
            ObjectList<Symbol>& private_references = 
                for_construct.get_data<ObjectList<Symbol> >("private_references");

            for_construct.add_data_attribute(induction_var.get_symbol(), OpenMP::DA_PRIVATE);

            // And insert into private references as well
            private_references.insert(induction_var.get_symbol());
        }
    }

    void OpenMPTransform::for_postorder(OpenMP::ForConstruct for_construct)
    {
        OpenMP::Directive directive = for_construct.directive();
        Statement construct_body = for_construct.body();
        ForStatement for_statement = construct_body;
        Statement loop_body = for_statement.get_loop_body();
        
        // Get the enclosing function definition
        FunctionDefinition function_definition = for_construct.get_enclosing_function();
        // its scope
        Scope function_scope = function_definition.get_scope();

        // Remove the induction var from the stack
        induction_var_stack.pop();

        ObjectList<OpenMP::ReductionSymbol> reduction_empty;

        // This has been computed in the preorder
        ObjectList<Symbol>& shared_references = 
            for_construct.get_data<ObjectList<Symbol> >("shared_references");
        ObjectList<Symbol>& private_references = 
            for_construct.get_data<ObjectList<Symbol> >("private_references");
        ObjectList<Symbol>& firstprivate_references = 
            for_construct.get_data<ObjectList<Symbol> >("firstprivate_references");
        ObjectList<Symbol>& lastprivate_references = 
            for_construct.get_data<ObjectList<Symbol> >("lastprivate_references");
        ObjectList<OpenMP::ReductionSymbol>& reduction_references =
            for_construct.get_data<ObjectList<OpenMP::ReductionSymbol> >("reduction_references");
        ObjectList<Symbol>& copyin_references = 
            for_construct.get_data<ObjectList<Symbol> >("copyin_references");
        ObjectList<Symbol>& copyprivate_references = 
            for_construct.get_data<ObjectList<Symbol> >("copyprivate_references");

        // The lists of entities passed by pointer and entities
        // privatized in the outline
        ObjectList<ParameterInfo> parameter_info_list;
        // Create the replacement map and the pass_by_pointer set
        ReplaceIdExpression replace_references  = 
            set_replacements(function_definition,
                    directive,
                    loop_body,
                    shared_references,
                    private_references,
                    firstprivate_references,
                    lastprivate_references,
                    reduction_references,
                    reduction_empty,
                    copyin_references,
                    copyprivate_references,
                    parameter_info_list);

        Source parallel_for_body;

        parameter_info_list.clear();

        Source private_declarations = get_privatized_declarations(
                for_construct,
                private_references,
                firstprivate_references,
                lastprivate_references,
                reduction_references,
                copyin_references,
                parameter_info_list
                ); 

        Source loop_distribution_code = get_loop_distribution_code(for_statement,
                replace_references, function_definition, directive);

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

        parallel_for_body
            << "{"
            <<    private_declarations
            <<    loop_distribution_code
            <<    lastprivate_code
            <<    reduction_code
            <<    loop_finalization
            << "}"
            ;

        bool orphaned = (parallel_nesting == 0);

        TL::Source::ParseFlags parse_flags = TL::Source::DEFAULT;

        if (orphaned)
        {
            reduction_code = get_critical_reduction_code(reduction_references);
        }
        else
        {
            reduction_code = get_noncritical_inlined_reduction_code(reduction_references,
                    loop_body);
            // This will reference variables already not declared
            // so do not fail when parsing them
            parse_flags = TL::Source::DO_NOT_CHECK_EXPRESSION;
        }

        AST_t result;
        result = parallel_for_body.parse_statement(loop_body.get_ast(), 
                loop_body.get_scope_link(), parse_flags);

        for_construct.get_ast().replace(result);
    }
}
