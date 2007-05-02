#include "tl-omptransform.hpp"

namespace TL
{
    void OpenMPTransform::for_preorder(OpenMP::ForConstruct for_construct)
    {
        Statement construct_body = for_construct.body();
        // The construct is in fact a ForStatement in a #pragma omp parallel do
        ForStatement for_statement(construct_body);

        IdExpression induction_var = for_statement.get_induction_variable();

        // Save this induction var in the stack
        induction_var_stack.push(induction_var);
    }

    void OpenMPTransform::for_postorder(OpenMP::ForConstruct for_construct)
    {
        OpenMP::Directive directive = for_construct.directive();
        Statement construct_body = for_construct.body();
        ForStatement for_statement = construct_body;
        Statement loop_body = for_statement.get_loop_body();

        // Remove the induction var from the stack
        induction_var_stack.pop();

        // They will hold the entities as they appear in the clauses
        ObjectList<IdExpression> shared_references;
        ObjectList<IdExpression> private_references;
        ObjectList<IdExpression> firstprivate_references;
        ObjectList<IdExpression> lastprivate_references;
        ObjectList<OpenMP::ReductionIdExpression> reduction_references;
        ObjectList<IdExpression> copyin_references;
        ObjectList<IdExpression> copyprivate_references;

        // Get the enclosing function definition
        FunctionDefinition function_definition = for_construct.get_enclosing_function();
        // its scope
        Scope function_scope = function_definition.get_scope();

        // Get the data attributes for every entity
        get_data_explicit_attributes(function_scope,
                directive,
                construct_body,
                shared_references,
                private_references,
                firstprivate_references,
                lastprivate_references,
                reduction_references,
                copyin_references,
                copyprivate_references);

        // The induction variable deserves special treatment
        IdExpression induction_var = for_statement.get_induction_variable();
        // If private_references does not contain an IdExpression whose
        // related symbol is the induction variable symbol, then
        // privatize here (FIXME: I've seen codes where the induction
        // variable appears in lastprivate)
        if (!private_references.contains(functor(&IdExpression::get_symbol), induction_var.get_symbol()))
        {
            // Add the induction variale onto the private references
            private_references.append(induction_var);
            // And now remove, if it was already there, any reference
            // to the induction variable symbol from the set of shared
            // references
            shared_references = shared_references.not_find(functor(&IdExpression::get_symbol), 
                    induction_var.get_symbol());
        }

        ObjectList<OpenMP::ReductionIdExpression> reduction_empty;

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
        if (orphaned)
        {
            reduction_code = get_critical_reduction_code(reduction_references);
        }
        else
        {
            reduction_code = get_noncritical_inlined_reduction_code(reduction_references);
        }

        AST_t result;
        result = parallel_for_body.parse_statement(loop_body.get_ast(), 
                loop_body.get_scope_link());

        for_construct.get_ast().replace(result);
    }
}
