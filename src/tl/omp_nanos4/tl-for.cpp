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

namespace TL
{
    namespace Nanos4
    {
        void OpenMPTransform::for_preorder(PragmaCustomConstruct for_construct)
        {
            Statement construct_body = for_construct.get_statement();
            // The construct is in fact a ForStatement in a #pragma omp parallel do
            ForStatement for_statement(construct_body);

            IdExpression induction_var = for_statement.get_induction_variable();

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

            OpenMP::DataSharing& data_sharing = openmp_info->get_data_sharing(for_construct.get_ast());

            data_sharing.get_all_symbols(OpenMP::DA_SHARED, shared_references);
            data_sharing.get_all_symbols(OpenMP::DA_PRIVATE, private_references);
            data_sharing.get_all_symbols(OpenMP::DA_FIRSTPRIVATE, firstprivate_references);
            data_sharing.get_all_symbols(OpenMP::DA_LASTPRIVATE, lastprivate_references);

            // As usual reductions are special
            data_sharing.get_all_reduction_symbols(reduction_references);

            data_sharing.get_all_symbols(OpenMP::DA_COPYIN, copyin_references);
            data_sharing.get_all_symbols(OpenMP::DA_COPYPRIVATE, copyprivate_references);
        }

        void OpenMPTransform::for_postorder(PragmaCustomConstruct for_construct)
        {
            Statement construct_body = for_construct.get_statement();
            ForStatement for_statement = construct_body;
            Statement loop_body = for_statement.get_loop_body();

            // Get the enclosing function definition
            FunctionDefinition function_definition = for_construct.get_enclosing_function();
            // its scope
            Scope function_scope = function_definition.get_scope();

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
            
            ReplaceIdExpression replace_references  = 
                set_replacements_inline(function_definition,
                        loop_body,
                        shared_references,
                        private_references,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references,
                        reduction_empty,
                        copyin_references,
                        copyprivate_references);

            Source parallel_for_body;

            Source private_declarations = get_privatized_declarations_inline(
                    for_construct,
                    private_references,
                    firstprivate_references,
                    lastprivate_references,
                    reduction_references,
                    copyin_references
                    ); 

            Source loop_distribution_code = get_loop_distribution_code(
                    for_statement,
                    for_construct,
                    replace_references, function_definition);

            Source lastprivate_code;

            if (!lastprivate_references.empty())
            {
                Source lastprivate_assignments = get_lastprivate_assignments_inline(
                        lastprivate_references, 
                        copyprivate_references);

                lastprivate_code
                    << "if (intone_last != 0)"
                    << "{"
                    <<    lastprivate_assignments
                    << "}"
                    ;
            }

            PragmaCustomClause nowait_clause = for_construct.get_clause("nowait");

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
}
