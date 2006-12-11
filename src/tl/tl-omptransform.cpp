#include "tl-omp.hpp"
#include "tl-omptransform.hpp"
#include "tl-predicateutils.hpp"
#include "tl-source.hpp"
#include <iostream>
#include <utility>


namespace TL
{
    class OpenMPTransform : public OpenMP::OpenMPPhase
    {
        private:
            // "persistent" variables in templates
            int num_parallels;
            int parallel_nesting;

        public:
            OpenMPTransform()
            {
            }

            virtual ~OpenMPTransform()
            {
				// This is needed since "init" is a virtual method
            }

            virtual void init()
            {
				// This function is called in OpenMPPhase::run. The user
				// can register here the handlers that will be called for
				// every construction (in preorder and postorder)
				//
                // Register the handlers (callbacks) for every construction

				// #pragma omp parallel
                on_parallel_pre.connect(&OpenMPTransform::parallel_preorder, *this);
                on_parallel_post.connect(&OpenMPTransform::parallel_postorder, *this);

				// #pragma omp parallel for
                on_parallel_for_pre.connect(&OpenMPTransform::parallel_for_preorder, *this);
                on_parallel_for_post.connect(&OpenMPTransform::parallel_for_postorder, *this);

				// #pragma omp for
				on_for_pre.connect(&OpenMPTransform::for_preorder, *this);
				on_for_post.connect(&OpenMPTransform::for_postorder, *this);
            }

			// Parallel in preorder
            void parallel_preorder(OpenMP::ParallelConstruct parallel_construct)
            {
				// One more parallel seen
                num_parallels++;

				// Increase the parallel nesting value
                parallel_nesting++;
            }

			// Parallel in postorder
            void parallel_postorder(OpenMP::ParallelConstruct parallel_construct)
            {
				// Decrease the parallel nesting
                parallel_nesting--;
                
                // Get the directive
                OpenMP::Directive directive = parallel_construct.directive();
                
                // Get the enclosing function definition
                FunctionDefinition function_definition = parallel_construct.get_enclosing_function();
				// its scope
                Scope function_scope = function_definition.get_scope();
				// and the id-expression of the function name
                IdExpression function_name = function_definition.get_function_name();

                // They will hold the entities as they appear in the clauses
                ObjectList<IdExpression> shared_references;
                ObjectList<IdExpression> private_references;
                ObjectList<IdExpression> firstprivate_references;
                ObjectList<IdExpression> lastprivate_references;
                ObjectList<OpenMP::ReductionIdExpression> reduction_references;
                
                // Get the construct_body of the statement
                Statement construct_body = parallel_construct.body();

                // Get the data attributes for every entity
                get_data_attributes(function_scope,
                        directive,
                        construct_body,
                        shared_references,
                        private_references,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references);

				// This list will hold everything that must be passed by pointer
                ObjectList<IdExpression> pass_by_pointer;
				// This list will hold everything that has been privatized
                ObjectList<IdExpression> privatized_entities;
                // Create the replacement map and fill the privatized
				// entities and pass by pointer lists.
                ReplaceIdExpression replace_references = 
                    set_replacements(function_scope,
                            directive,
                            construct_body,
                            shared_references,
                            private_references,
                            firstprivate_references,
                            lastprivate_references,
                            reduction_references,
                            pass_by_pointer,
                            privatized_entities);

                // Get the outline function name
                Source outlined_function_name = get_outlined_function_name(function_name);

                // Create the outline for parallel for using 
				// the privatized entities and pass by pointer
				// lists.
				// Additionally {first|last}private and reduction
				// entities are needed for proper initializations
				// and assignments.
                AST_t outline_code = create_outline_parallel(
                        function_definition,
                        outlined_function_name, 
                        construct_body,
                        replace_references,
                        pass_by_pointer,
                        privatized_entities,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references);

				// In the AST of the function definition, prepend outline_code
				// as a sibling (at the same level)
                function_definition.get_ast().prepend_sibling_function(outline_code);

				// Now create the spawning code. Pass by pointer list and
				// reductions are needed for proper pass of data and reduction
				// vectors declaration
                AST_t spawn_code = create_spawn_code(parallel_construct.get_scope(),
                        parallel_construct.get_scope_link(),
                        outlined_function_name,
                        pass_by_pointer,
                        reduction_references
                        );

				// Now replace the whole construct with spawn_code
                parallel_construct.get_ast().replace_with(spawn_code);
            }

            void parallel_for_preorder(OpenMP::ParallelForConstruct parallel_for_construct)
            {
				// One more parallel seen
                num_parallels++;

				// Increase the parallel nesting value
                parallel_nesting++;
            }

            void parallel_for_postorder(OpenMP::ParallelForConstruct parallel_for_construct)
            {
				// Decrease the parallel nesting level 
                parallel_nesting--;

                // Get the directive
                OpenMP::Directive directive = parallel_for_construct.directive();
                
                // Get the enclosing function definition
                FunctionDefinition function_definition = parallel_for_construct.get_enclosing_function();
                Scope function_scope = function_definition.get_scope();
                IdExpression function_name = function_definition.get_function_name();

                // They will hold the entities as they appear in the clauses
                ObjectList<IdExpression> shared_references;
                ObjectList<IdExpression> private_references;
                ObjectList<IdExpression> firstprivate_references;
                ObjectList<IdExpression> lastprivate_references;
                ObjectList<OpenMP::ReductionIdExpression> reduction_references;
                
                // Get the construct_body of the statement
                Statement construct_body = parallel_for_construct.body();
				// The construct is in fact a ForStatement in a #pragma omp parallel do
                ForStatement for_statement(construct_body);
                Statement loop_body = for_statement.get_loop_body();

                // Get the data attributes for every entity
                get_data_attributes(function_scope,
                        directive,
                        loop_body,
                        shared_references,
                        private_references,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references);

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

				// The lists of entities passes by pointer and entities
				// privatized in the outline
                ObjectList<IdExpression> pass_by_pointer;
                ObjectList<IdExpression> privatized_entities;
                // Create the replacement map and the pass_by_pointer set
                ReplaceIdExpression replace_references = 
                    set_replacements(function_scope,
                            directive,
                            loop_body,
                            shared_references,
                            private_references,
                            firstprivate_references,
                            lastprivate_references,
                            reduction_references,
                            pass_by_pointer,
                            privatized_entities);

                // Get the outline function name
                Source outlined_function_name = get_outlined_function_name(function_name);

                // Create the outline for parallel for
                AST_t outline_code = create_outline_parallel_for(
                        function_definition,
                        outlined_function_name, 
                        for_statement,
                        loop_body,
                        replace_references,
                        pass_by_pointer,
                        privatized_entities,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references);

				// Now prepend the outline
                function_definition.get_ast().prepend_sibling_function(outline_code);

				AST_t spawn_code = create_spawn_code(parallel_for_construct.get_scope(),
						parallel_for_construct.get_scope_link(),
						outlined_function_name,
						pass_by_pointer,
						reduction_references
						);

				// Replace all the whole construct with spawn_code
                parallel_for_construct.get_ast().replace_with(spawn_code);
            }

            AST_t create_spawn_code(
                    Scope scope,
                    ScopeLink scope_link,
                    Source outlined_function_name,
                    ObjectList<IdExpression> pass_by_pointer,
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references)
            {
                Source spawn_code;
                Source reduction_vectors;
                Source groups_definition;
                Source source_num_parameters;
                Source referenced_parameters;

                Source reduction_code;

				// The skeleton of the spawn code will be this one
                spawn_code
                    << "{"
                    << "  int nth_nprocs;"
                    << "  struct nth_desc *nth_selfv;"
                    << "  int nth_nprocs_2;"
                    << "  int nth_arg;"
                    << "  unsigned long long nth_mask;"
                    << "  int nth_num_params;"
                    << "  int nth_p;"
                    << "  extern struct nth_desc *nthf_self_();"
                    << "  extern void nthf_depadd_(struct nth_desc **, int *);"
                    << "  extern void nthf_create_1s_vp_(void (*)(), int *, int *, struct nth_desc **, unsigned long long *, int *, ...);"
                    << "  extern void nthf_block_();"
                    <<    reduction_vectors
                    <<    groups_definition
                    << "  nth_selfv = nthf_self_();"
                    << "  nth_nprocs_2 = nth_nprocs + 1;"
                    << "  nthf_depadd_(&nth_selfv, &nth_nprocs_2);"
                    << "  nth_arg = 0;"
                    << "  nth_mask = (unsigned long long)(~0ULL);"
                    << "  nth_num_params = " << source_num_parameters << ";"
                    << "  for (nth_p = 0; nth_p < nth_nprocs_2 - 1; nth_p++)"
                    << "  {"
                    << "     nthf_create_1s_vp_((void(*)())(" << outlined_function_name << "), &nth_arg, &nth_p, &nth_selfv, "
                    << "        &nth_mask, &nth_num_params " << referenced_parameters << ");"
                    << "  }"
                    << "  nthf_block_();"
                    <<    reduction_code
                    << "}"
                    ;

                int num_parameters = 0;

                // Reduction vectors
				//
				// For every entity in the reduction_references list
                for (ObjectList<OpenMP::ReductionIdExpression>::iterator it = reduction_references.begin();
                        it != reduction_references.end();
                        it++)
                {
					// create a reduction vector after the name of the mangled entity
                    std::string reduction_vector_name = "rdv_" + it->get_id_expression().mangle_id_expression();

					// get its type
                    Symbol reduction_symbol = it->get_symbol();
                    Type reduction_type = reduction_symbol.get_type();

					// create a tree of expression 64
                    // FIXME: hardcoded to 64 processors
                    Source array_length;
                    array_length << "64";
                    AST_t array_length_tree = array_length.parse_expression(it->get_id_expression().get_scope());

					// and get an array of 64 elements
                    Type reduction_vector_type = reduction_type.get_array_to(array_length_tree, 
                            it->get_id_expression().get_scope());

					// now get the code that declares this reduction vector
                    reduction_vectors
                        << reduction_vector_type.get_declaration(reduction_vector_name) << ";";

					// And add to the list of referenced parameters
                    referenced_parameters << ", " << reduction_vector_name;
                    num_parameters++;
                }
                
                // Referenced parameters
				//
				// For every entity in list "pass_by_pointer"
                for (ObjectList<IdExpression>::iterator it = pass_by_pointer.begin();
                        it != pass_by_pointer.end();
                        it++)
                {
					// Simply pass its reference (its address)
                    referenced_parameters << ", &" << it->get_ast().prettyprint();
                }
                num_parameters += pass_by_pointer.size();

				// Fill in the spawn code skeleton, the number of parameters
                source_num_parameters << num_parameters;
                
                // Groups definition
                // TODO
                
                // Reduction code
				//
				// If there is any reduction reference
                if (!reduction_references.empty())
                {
					// Create the source code that gathers the values computed by every thread
                    Source reduction_gathering;

                    reduction_code
                        << "for (int rdv_i = 0; rdv_i < nth_nprocs; rdv_i++)"
                        << "{"
                        <<    reduction_gathering
                        << "}"
                        ;

					// For every entity being reduced
                    for (ObjectList<OpenMP::ReductionIdExpression>::iterator it = reduction_references.begin();
                            it != reduction_references.end();
                            it++)
                    {
						// Construct the name of its related reduction vector
                        std::string reduced_var_name = it->get_id_expression().get_ast().prettyprint();
                        std::string reduction_vector_name = "rdv_" + it->get_id_expression().mangle_id_expression();

						// get the operator involved
                        std::string op = it->get_operation().prettyprint();

						// And reduce for this element of the reduction vector
                        reduction_gathering
                            << reduced_var_name << " = " << reduced_var_name << op << reduction_vector_name << "[rdv_i]" << ";";
                    }
                }
                
                // std::cerr << "CODI SPAWN" << std::endl;
                // std::cerr << spawn_code.get_source(true) << std::endl;
                // std::cerr << "End CODI SPAWN" << std::endl;
                
				// Parse the spawn code and return it
                AST_t result = spawn_code.parse_statement(scope, scope_link);
                return result;
            }

            Source create_outline_common(
                    Source& specific_body,
                    Source outlined_function_name,
                    ObjectList<IdExpression> pass_by_pointer,
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references
                    )
            {
                Source formal_parameters;
                Source reduction_code;

				Source result;
                result
                    << "void " << outlined_function_name << "(" << formal_parameters << ")"
                    << "{"
                    <<    specific_body
                    << "}"
                    ;

                // Reduction vectors are passed first by "value" (there is no
                // need to pass by pointer something that was already passed as
                // a pointer)
                for (ObjectList<OpenMP::ReductionIdExpression>::iterator it = reduction_references.begin();
                        it != reduction_references.end();
                        it++)
                {
                    std::string reduction_vector_name = "rdv_" + it->get_id_expression().mangle_id_expression();

                    Symbol sym = it->get_symbol();
                    Type type = sym.get_type();
                    Type pointer_type = type.get_pointer_to();
                    
                    formal_parameters.append_with_separator(pointer_type.get_declaration(reduction_vector_name), ",");
                }

                // Formal parameters, basically pass_by_pointer things
                // (and sometimes something else)
                for (ObjectList<IdExpression>::iterator it = pass_by_pointer.begin();
                        it != pass_by_pointer.end();
                        it++)
                {
                    Symbol sym = it->get_symbol();
                    Type type = sym.get_type();
                    Type pointer_type = type.get_pointer_to();

                    // Get a declaration of the mangled name of the id-expression
                    formal_parameters.append_with_separator(pointer_type.get_declaration(it->mangle_id_expression()), ",");
                }

				return result;
            }

            AST_t create_outline_parallel(
                    FunctionDefinition function_definition,
                    Source outlined_function_name,
                    Statement construct_body,
                    ReplaceIdExpression replace_references,
                    ObjectList<IdExpression> pass_by_pointer,
                    ObjectList<IdExpression> privatized_entities,
                    ObjectList<IdExpression> firstprivate_references,
                    ObjectList<IdExpression> lastprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references
                    )
            {
                Source outline_parallel;
                Source parallel_body;
                Source empty;

                outline_parallel = create_outline_common(
                        parallel_body, // The body of the outline
                        outlined_function_name,
                        pass_by_pointer,
						reduction_references);
				
                // Replace references using set "replace_references" over construct body
                Statement modified_parallel_body_stmt = replace_references.replace(construct_body);

				Source private_declarations = get_privatized_declarations(privatized_entities, pass_by_pointer, 
						firstprivate_references, reduction_references);
				
                parallel_body 
				    << private_declarations
					<< modified_parallel_body_stmt.get_ast().prettyprint()
					;

                std::cerr << "CODI OUTLINE" << std::endl;
                std::cerr << outline_parallel.get_source(true) << std::endl;
                std::cerr << "End CODI OUTLINE" << std::endl;

                AST_t result;

                result = outline_parallel.parse_global(function_definition.get_scope(), 
                         function_definition.get_scope_link());

                return result;
            }

			// #pragma omp for
			void for_preorder(OpenMP::ForConstruct for_construct)
			{
				// Do nothing at the moment
			}

			void for_postorder(OpenMP::ForConstruct for_construct)
			{
				OpenMP::Directive directive = for_construct.directive();
				ForStatement for_statement = for_construct.body();
				Statement loop_body = for_statement.get_loop_body();

                // They will hold the entities as they appear in the clauses
                ObjectList<IdExpression> shared_references;
                ObjectList<IdExpression> private_references;
                ObjectList<IdExpression> firstprivate_references;
                ObjectList<IdExpression> lastprivate_references;
                ObjectList<OpenMP::ReductionIdExpression> reduction_references;
				
                // Get the enclosing function definition
                FunctionDefinition function_definition = for_construct.get_enclosing_function();
				// its scope
                Scope function_scope = function_definition.get_scope();
                
                // Get the data attributes for every entity
                get_data_explicit_attributes(function_scope,
                        directive,
                        loop_body,
                        shared_references,
                        private_references,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references);
				
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

				// The lists of entities passes by pointer and entities
				// privatized in the outline
                ObjectList<IdExpression> pass_by_pointer;
                ObjectList<IdExpression> privatized_entities;
                // Create the replacement map and the pass_by_pointer set
                ReplaceIdExpression replace_references = 
                    set_replacements(function_scope,
                            directive,
                            loop_body,
                            shared_references,
                            private_references,
                            firstprivate_references,
                            lastprivate_references,
                            reduction_references,
                            pass_by_pointer,
                            privatized_entities);

				Source parallel_for_body;
				
				Source private_declarations = get_privatized_declarations(privatized_entities, pass_by_pointer, 
						firstprivate_references, reduction_references);

				Source loop_distribution_code = create_loop_distribution_code(for_statement,
						replace_references);

				Source lastprivate_assignments = get_lastprivate_assignments(lastprivate_references, 
						pass_by_pointer);

				Source loop_finalization = get_loop_finalization(/*do_barrier=*/true);

				parallel_for_body
					<< "{"
					<<    private_declarations
					<<    loop_distribution_code
					<<    lastprivate_assignments
					<<    loop_finalization
					<< "}"
					;

				std::cerr << "CODI FOR" << std::endl;
				std::cerr << parallel_for_body.get_source(true) << std::endl;
				std::cerr << "End CODI FOR" << std::endl;

                AST_t result;
                result = parallel_for_body.parse_statement(loop_body.get_scope(), 
                         loop_body.get_scope_link());

				for_construct.get_ast().replace_with(result);
			}

			// Create outline for parallel for
            AST_t create_outline_parallel_for(
                    FunctionDefinition function_definition,
                    Source outlined_function_name,
                    ForStatement for_statement,
                    Statement loop_body,
                    ReplaceIdExpression replace_references,
                    ObjectList<IdExpression> pass_by_pointer,
                    ObjectList<IdExpression> privatized_entities,
                    ObjectList<IdExpression> firstprivate_references,
                    ObjectList<IdExpression> lastprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references
                    )
			{
				Source empty;
				Source outline_parallel_for;
				Source parallel_for_body;

				// Get the source of the common parallel X outline
				outline_parallel_for = create_outline_common(
						parallel_for_body,
						outlined_function_name,
						pass_by_pointer,
						reduction_references);

				Source private_declarations = get_privatized_declarations(privatized_entities, pass_by_pointer, 
						firstprivate_references, reduction_references);

				Source loop_distribution = create_loop_distribution_code(for_statement, replace_references);

				Source lastprivate_assignments = get_lastprivate_assignments(lastprivate_references, 
						pass_by_pointer);

				// Barrier is already done at parallel level
				Source loop_finalization = get_loop_finalization(/* do_barrier = */ false);

				parallel_for_body 
					<< private_declarations
					<< loop_distribution
					<< lastprivate_assignments
					<< loop_finalization
					;

				std::cerr << "CODI OUTLINE" << std::endl;
				std::cerr << outline_parallel_for.get_source(true) << std::endl;
				std::cerr << "End CODI OUTLINE" << std::endl;

				AST_t result;

				result = outline_parallel_for.parse_global(function_definition.get_scope(), 
						function_definition.get_scope_link());

				return result;
			}

			void get_data_explicit_attributes(
                    Scope function_scope,
                    OpenMP::Directive directive,
                    Statement construct_body,
                    ObjectList<IdExpression>& shared_references,
                    ObjectList<IdExpression>& private_references,
                    ObjectList<IdExpression>& firstprivate_references,
                    ObjectList<IdExpression>& lastprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression>& reduction_references)
            {
                // Get references in shared clause
                OpenMP::Clause shared_clause = directive.shared_clause();
                shared_references = shared_clause.id_expressions();

                // Get references in private_clause
                OpenMP::Clause private_clause = directive.private_clause();
                private_references = private_clause.id_expressions();

                // Get references in firstprivate clause
                OpenMP::Clause firstprivate_clause = directive.firstprivate_clause();
                firstprivate_references = firstprivate_clause.id_expressions();

                // Get references in lastprivate clause
                OpenMP::Clause lastprivate_clause = directive.lastprivate_clause();
                lastprivate_references = lastprivate_clause.id_expressions();

                // Get references in reduction clause
                OpenMP::ReductionClause reduction_clause = directive.reduction_clause();
                reduction_references = reduction_clause.id_expressions();
			}


            void get_data_attributes(
                    Scope function_scope,
                    OpenMP::Directive directive,
                    Statement construct_body,
                    ObjectList<IdExpression>& shared_references,
                    ObjectList<IdExpression>& private_references,
                    ObjectList<IdExpression>& firstprivate_references,
                    ObjectList<IdExpression>& lastprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression>& reduction_references)
            {
				get_data_explicit_attributes(
						function_scope,
						directive,
						construct_body,
						shared_references,
						private_references,
						firstprivate_references,
						lastprivate_references,
						reduction_references);

                OpenMP::DefaultClause default_clause = directive.default_clause();

                // Everything should have been tagged by the user
                if (default_clause.is_none())
                    return;

				// Get every non local reference: this is, not defined in the
				// construct itself, but visible at the point where the
				// construct is defined
                ObjectList<IdExpression> non_local_symbols = construct_body.non_local_symbol_occurrences(Statement::ONLY_VARIABLES);

				// Filter in any of the private sets. We don't want any
				// id-expression whose related symbol appears in any
				// id-expression of shared, private, firstprivate lastprivate
				// or reduction
                non_local_symbols = non_local_symbols.filter(not_in_set(shared_references, functor(&IdExpression::get_symbol)));
                non_local_symbols = non_local_symbols.filter(not_in_set(private_references, functor(&IdExpression::get_symbol)));
                non_local_symbols = non_local_symbols.filter(not_in_set(firstprivate_references, functor(&IdExpression::get_symbol)));
                non_local_symbols = non_local_symbols.filter(not_in_set(lastprivate_references, functor(&IdExpression::get_symbol)));

				// Get every id-expression related to the ReductionIdExpression list
                ObjectList<IdExpression> reduction_id_expressions = 
                    reduction_references.map(functor(&OpenMP::ReductionIdExpression::get_id_expression));
                non_local_symbols = non_local_symbols.filter(not_in_set(reduction_id_expressions, functor(&IdExpression::get_symbol)));

                shared_references.append(non_local_symbols);
            }

            ReplaceIdExpression set_replacements(Scope function_scope,
                    OpenMP::Directive directive,
                    Statement construct_body,
                    ObjectList<IdExpression>& shared_references,
                    ObjectList<IdExpression>& private_references,
                    ObjectList<IdExpression>& firstprivate_references,
                    ObjectList<IdExpression>& lastprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression>& reduction_references,
                    ObjectList<IdExpression>& pass_by_pointer,
                    ObjectList<IdExpression>& privatized_entities)
            {
                ReplaceIdExpression result;

                // First mix every symbol that might be shareable
                // 
                // "shareable" means that it can be passed by pointer
                // because of it being an entity not accessible from the
                // sibling function

                ObjectList<IdExpression> shareable_references;

                // There will be no repeated here
                shareable_references.append(shared_references);
                shareable_references.append(firstprivate_references);
                shareable_references.append(lastprivate_references);

                for (ObjectList<IdExpression>::iterator it = shareable_references.begin();
                        it != shareable_references.end();
                        it++)
                {
                    Symbol current_sym = it->get_symbol();

                    // This symbol already appears in "pass_by_pointer" id-expressions,
					// ignore it
                    if (pass_by_pointer.contains(functor(&IdExpression::get_symbol), current_sym))
                        continue;

					// From the scope of the enclosing function definition, try to get
					// a symbol
                    Symbol global_sym = function_scope.get_symbol_from_id_expr(it->get_ast());

                    if (!global_sym.is_valid() ||
                            (global_sym != current_sym))
                    {
                        // The symbol is either not accessible or it is not the same
                        // Since it is shared it must be referenced via a pointer

                        // This must be passed by pointer
                        pass_by_pointer.append(*it);

						// The mapping is needed only if the entity appears in
						// shared set because every reference will have to be
						// replaced
                        if (shared_references.contains(functor(&IdExpression::get_symbol), current_sym))
                        {
                            // First get a nice name by mangling it
                            //
                            // FIXME: There are cases we could see an unqualified identifier
                            // that due to scoping issues clashes with another unqualified identifier
                            // (e.g. different using namespaces in different block scopes)
                            std::string mangled_name = it->mangle_id_expression();

                            Source derref_name;
                            derref_name << "(*" << mangled_name << ")";

                            result.add_replacement(current_sym, derref_name);
                        }
                    }

                }
                
                // First mix all things that will be made private
                ObjectList<IdExpression> privatized_references;
                privatized_references.append(private_references);
                privatized_references.append(firstprivate_references);
                privatized_references.append(lastprivate_references);
                privatized_references.append(reduction_references.map(functor(&OpenMP::ReductionIdExpression::get_id_expression)));

                for (ObjectList<IdExpression>::iterator it = privatized_references.begin();
                        it != privatized_references.end();
                        it++)
                {
                    Symbol current_sym = it->get_symbol();

					// If already has replacement, ignore
                    if (result.has_replacement(current_sym))
                        continue;

                    std::string private_mangled_name = std::string("p_") + it->mangle_id_expression();

                    // This entity is privatized
                    privatized_entities.append(*it);

                    // Create a replacement for it
                    result.add_replacement(current_sym, private_mangled_name);
                }

                return result;
            }

			Source create_loop_distribution_code(ForStatement for_statement,
					ReplaceIdExpression replace_references)
			{
				Source parallel_for_body;

                Source loop_initialization;

                Source schedule_decisions;
                Source distributed_loop_body;
                Source loop_reductions;
                Source reduction_initialization;

                parallel_for_body
                    << reduction_initialization
                    << loop_initialization
                    << schedule_decisions
                    << distributed_loop_body
                    << loop_reductions
                    ;

                Statement loop_body = for_statement.get_loop_body();

				IdExpression induction_var = for_statement.get_induction_variable();
                Source induction_var_name;
				// Induction var name is handled specially
                induction_var_name << "p_" << induction_var.mangle_id_expression();

				// Define here the bounds of the loop
                loop_initialization 
                    << "int nth_low;"
                    << "int nth_upper;"
                    << "int nth_step;"
                    << "int intone_start;"
                    << "int intone_end;"
                    << "int intone_last;"

                    << "nth_low = " << for_statement.get_lower_bound().get_ast().prettyprint() << ";"
                    << "nth_upper = " << for_statement.get_upper_bound().get_ast().prettyprint() << ";"
                    << "nth_step = " << for_statement.get_step().get_ast().prettyprint() << ";"
                    ;

                // Schedule decisions
				// TODO - dynamic, guided and runtime support is lacking
                schedule_decisions
                    << "int nth_schedule;"
                    << "int nth_chunk;"
                    << "nth_schedule = 0;"
                    << "nth_chunk = 0;"
                    ;

                // Loop distribution
                Source modified_loop_body;
                distributed_loop_body
                    // FIXME - Eventually an include will solve this
                    << "extern void in__tone_begin_for_(int*, int*, int*, int*, int*);"
                    << "extern int in__tone_next_iters_(int*, int*, int*);"
                    << "extern void in__tone_end_for_(int*);"

                    << "in__tone_begin_for_(&nth_low, &nth_upper, &nth_step, &nth_chunk, &nth_schedule);"

					// Get a slice of the iteration space
                    << "while (in__tone_next_iters_(&intone_start, &intone_end, &intone_last) != 0)"
                    << "{"
					       // And do something with it
                    << "   for (" << induction_var_name << " = intone_start; "
                    << "        nth_step >= 1 ? " << induction_var_name << " <= intone_end : " << induction_var_name << ">= intone_end;"
                    << "        " << induction_var_name << " += nth_step)"
                    << "   {"
                    << "   " << modified_loop_body
                    << "   }"
                    << "}"
                    ;

                // Replace references using set "replace_references" over loop body
                Statement modified_loop_body_stmt = replace_references.replace(loop_body);
				// and get the source of the modified tree
                modified_loop_body << modified_loop_body_stmt.get_ast().prettyprint();

                // Loop reductions
                // TODO
				// The master gathers all the other people, needs to know the team size

                // Loop finalization

				return parallel_for_body;
			}

			Source get_loop_finalization(bool do_barrier)
			{
				Source loop_finalization;

                loop_finalization
                    << "int nth_barrier = " << (int)(do_barrier) << ";"
                    << "in__tone_end_for_(&nth_barrier);"
                    ;

				return loop_finalization;
			}

			Source get_privatized_declarations(ObjectList<IdExpression> privatized_entities,
					ObjectList<IdExpression> pass_by_pointer,
					ObjectList<IdExpression> firstprivate_references,
					ObjectList<OpenMP::ReductionIdExpression> reduction_references)
			{
				Source private_declarations;

                for (ObjectList<IdExpression>::iterator it = privatized_entities.begin();
                        it != privatized_entities.end();
                        it++)
                {
                    Symbol sym = it->get_symbol();
                    Type type = sym.get_type();

                    // Get a declaration of the mangled name of the id-expression
                    if (firstprivate_references.contains(functor(&IdExpression::get_symbol), sym))
                    {
                        Source initializer;

                        if (pass_by_pointer.contains(functor(&IdExpression::get_symbol), sym))
                        {
                            initializer << "(*" << it->mangle_id_expression() << ")";
                        }
                        else
                        {
                            initializer << it->prettyprint();
                        }
                        
                        private_declarations << type.get_declaration_with_initializer("p_" + it->mangle_id_expression(),
                                initializer.get_source()) << ";";
                    }
                    else if (reduction_references.contains(functor(&OpenMP::ReductionIdExpression::get_symbol), sym))
                    {
                        ObjectList<OpenMP::ReductionIdExpression> red_id_expr_list = 
                            reduction_references.find(
                                    functor(&OpenMP::ReductionIdExpression::get_symbol), sym
                                    );

                        OpenMP::ReductionIdExpression red_id_expr = *(red_id_expr_list.begin());
                        IdExpression id_expr = red_id_expr.get_id_expression();

                        std::string neuter = red_id_expr.get_neuter().prettyprint();
                        
                        // Initialize to the neuter
                        private_declarations 
                            << type.get_declaration_with_initializer("p_" + id_expr.mangle_id_expression(), neuter) << ";";
                    }
                    else
                    {
                        private_declarations << type.get_declaration("p_" + it->mangle_id_expression()) << ";";
                    }
                }

				return private_declarations;
			}

			Source get_lastprivate_assignments(ObjectList<IdExpression> lastprivate_references,
					ObjectList<IdExpression> pass_by_pointer)
			{
				Source lastprivate_assignments;
				// Lastprivates
				for (ObjectList<IdExpression>::iterator it = lastprivate_references.begin();
						it != lastprivate_references.end();
						it++)
				{
					if (pass_by_pointer.contains(functor(&IdExpression::get_symbol), it->get_symbol()))
					{
						lastprivate_assignments
							<< "(*" << it->get_ast().prettyprint() << ")" << " = p_" << it->mangle_id_expression() << ";"
							;
					}
					else
					{
						lastprivate_assignments
							<< it->get_ast().prettyprint() << " = p_" << it->mangle_id_expression() << ";"
							;
					}
				}

				return lastprivate_assignments;
			}

            Source get_outlined_function_name(IdExpression function_name)
            {
                Source result;
                if (function_name.is_qualified())
                {
                    result
                        << function_name.get_qualified_part()
                        ;
                }
                result
                    << "nth__" << function_name.get_unqualified_part() << "_" << num_parallels;

                return result;
            }

    };
}

EXPORT_PHASE(TL::OpenMPTransform);
