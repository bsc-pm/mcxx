#include "tl-omp.hpp"
#include "tl-builtin.hpp"
#include "tl-ast.hpp"
#include "tl-source.hpp"
#include "tl-scopelink.hpp"
#include "tl-traverse.hpp"
#include "tl-predicateutils.hpp"
#include "cxx-attrnames.h"
#include <iostream>
#include <set>

extern "C" 
{
	// Symbol necessary to get the compiler phase
    TL::CompilerPhase* give_compiler_phase_object(void)
    {
        return new TL::OpenMPTransform();
    }
}

namespace TL
{
	// The whole phase starts at the function OpenMPTransform::run at the end
	// of this file
	//
	// A predicate for "#pragma omp parallel { ... }"
    PredicateBool<OMP_IS_PARALLEL_CONSTRUCT> parallel_construct;
	// A predicate for the nuclear expression that is neither a parenthesized
	// expression nor a literal (thus the name of an object)
    PredicateBool<LANG_IS_ID_EXPRESSION> id_expression;

	// This class implements a TraverseFunctor
	// a TraverseFunctor has two methods preorder and postorder that
	// user has to override. 
	//
	// A depth-traversal is performed. When a node (call it Q) satisfies the
	// predicate related to this functor (defined below in the run function at
	// the end of the file) member functions 'preorder' and 'postorder' are
	// invoked just before and after the depth-traversal of Q subtrees.
    class ParallelFunctor : public TraverseFunctor
    {
        private:
			// The OpenMP transform context
            OpenMPTransform& omp_context;

			// We will save Symbols here for shared and private entities.
			//
			// NOTE: Symbols are not AST_t, but a wrapper of the symbol table
			// entries of the compiler. Symbol class offers to the user the
			// symbolic work that the semantic phase has performed earlier.
			std::set<Symbol> shared_symbols;
			std::set<Symbol> private_symbols;
        public:
            ParallelFunctor(OpenMPTransform& omp_ctx)
                : omp_context(omp_ctx)
            {
            }

			// This is invoked in preorder for #pragma omp parallel
            virtual void preorder(Context ctx, AST_t node)
            {
				// One more level of nesting
				omp_context.parallel_nesting++;
            }

			// This is executed in postorder for "#pragma omp parallel { ... }"
			//   "node" will be the whole "#pragma omp parallel { ... }" construct
            virtual void postorder(Context ctx, AST_t node)
            {
				// One more parallel region found
                omp_context.num_parallels++;
				// One less level of nesting
				omp_context.parallel_nesting--;

				// We set up data attributes (shared, private, etc). See below
				setup_data_attributes(ctx, node);
				
				// Get the scope of the whole Parallel construct
				//
				// omp_context is defined in tl-omp.hpp, it is some context that
				// this phase is keeping to ease the process.
				Scope construct_scope = omp_context.scope_link.get_scope(node);

				// Get the inner body of this parallel region
				AST_t construct_body = node.get_attribute(OMP_CONSTRUCT_BODY);

				// This will be the variables that will hold several parts of the
				// outlining
				Source outlined_function;
				Source outlined_body;
				Source privatized_variables;
				Source shared_parameters;

				// We create the skeleton of the outline
				//
				// Note that Source::operator<<(Source&) saves a reference to the Source,
				// so here we have defined the "schema" and later we will fill them
				// with the missing source. 
				//
				// Similarly Source::operator<<(const std::string&) just  saves
				// a chunk of text.
				//
				// FIXME - Get the name of the current function
				outlined_function << "void outlined_function(" << shared_parameters << ")"
					<< "{"
					<< privatized_variables
					<< outlined_body
					<< "}";

				// Declare shared parameters. We fill here "shared_parameters"
				for (std::set<Symbol>::iterator it = shared_symbols.begin();
						it != shared_symbols.end();
						it++)
				{
					Type pointer_to = it->get_type().get_pointer_to();

					shared_parameters.append_with_separator(
							pointer_to.get_parameter_declaration_str(it->get_name()), 
							",");
				}

				// Declare private variables. We fill privatized_variables here
				for (std::set<Symbol>::iterator it = private_symbols.begin();
						it != private_symbols.end();
						it++)
				{
					Type symbol_type = it->get_type();

					privatized_variables 
						<< symbol_type.get_simple_declaration_str(std::string("p_") + it->get_name());
				}
				
				// Now we replace shared references with true derreferences
				// First we duplicate the construct_body
				AST_t outlined_body_tree = construct_body.duplicate();
				// Now we get all the references
				AST_list_t shared_references = outlined_body_tree.depth_subtrees().filter(id_expression);
				for(AST_list_t::iterator it = shared_references.begin(); 
						it != shared_references.end(); it++)
				{
					Symbol sym = construct_scope.get_symbol_from_id_expr(*it);

					if (sym != Symbol::invalid())
					{
						if (shared_symbols.find(sym) != shared_symbols.end())
						{
							Source derref_expression;
							derref_expression 
								<< "(*" << it->prettyprint() << ")";

							AST_t derref_expression_tree = derref_expression.parse_expression(construct_scope);
							it->replace_with(derref_expression_tree);
						}
					}
				}

				// Now we replace privatized references to "p_ID"
				AST_list_t privatized_references = outlined_body_tree.depth_subtrees().filter(id_expression);
				for(AST_list_t::iterator it = privatized_references.begin(); 
						it != privatized_references.end(); it++)
				{
					Symbol sym = construct_scope.get_symbol_from_id_expr(*it);

					if (sym != Symbol::invalid())
					{
						if (private_symbols.find(sym) != private_symbols.end())
						{
							it->replace_text(std::string("p_") + sym.get_name());
						}
					}
				}

				outlined_body << outlined_body_tree.prettyprint();

				AST_t enclosing_function_def = node.get_enclosing_function_definition();
				Scope enclosing_function_def_scope = omp_context.scope_link.get_scope(enclosing_function_def);

				AST_t outlined_function_tree = 
					outlined_function.parse_global(enclosing_function_def_scope, omp_context.scope_link);

				node.prepend_sibling_function(outlined_function_tree);

            }

			void setup_data_attributes(Context ctx, AST_t node)
			{
				// Get every shared reference
				//
				// First we declare "shared_clause" as the predicate for the shared clauses
				PredicateBool<OMP_IS_SHARED_CLAUSE> shared_clause;

				// We get a list of subtrees that satisfy "shared_clause", thus
				// they are trees that hold a "SHARED(a, b, c, ...)"
				AST_list_t shared_clauses = node.depth_subtrees().filter(shared_clause);

				// We traverse every element of this node (AST_t) list, recall
				// that every "*it" will be a "SHARED(a, b, c, ...)"
				for (AST_list_t::iterator it = shared_clauses.begin(); 
						it != shared_clauses.end(); it++)
				{
					// Now, we get a list of subtrees that satisfy
					// "id_expression" predicate, thus, they are of the form
					// 		a
					// 		A::b
					// 		A<B1, B2, ...>::b
					AST_set_t references = it->depth_subtrees().filter(id_expression);

					// We traverse every element of this list (recall that
					// every "ref" is an id-expressions)
					for (AST_set_t::iterator ref = references.begin();
							ref != references.end(); ref++)
					{
						// We have to get the scope where this node belongs to
						Scope scope = omp_context.scope_link.get_scope(*ref);
						// Since we now it is an id expression we use the scope
						// to get the Symbol.
						//
						// Symbol is a class that wraps the exact symbolic information
						// that is stored in the symbol table of every scope.
						Symbol sym = scope.get_symbol_from_id_expr(*ref);

						// Add it into the set of shared_symbols (this is a
						// std::set<Symbol>)
						shared_symbols.insert(sym);
					}
				}
				
				// Get every private reference (this is symmetric to the shared case)
				//
				// First we declare "private_clause" as the predicate for the private clauses
				PredicateBool<OMP_IS_PRIVATE_CLAUSE> private_clause;

				// We get a list of subtrees that satisfy "private_clause", thus they are of the form
				// PRIVATE(a, b, c, ...)
				AST_list_t private_clauses = node.depth_subtrees().filter(private_clause);

				// We traverse every element. 
				// Recall that "it" will be an AST that holds a "PRIVATE(...)"
				for (AST_list_t::iterator it = private_clauses.begin(); 
						it != private_clauses.end(); it++)
				{
					// Get a list of references, in fact, we get a list of id_expressions
					AST_set_t references = it->depth_subtrees().filter(id_expression);

					// And we traverse every reference
					for (AST_set_t::iterator ref = references.begin();
							ref != references.end(); ref++)
					{
						// We get the scope of every reference
						Scope scope = omp_context.scope_link.get_scope(*ref);
						
						// And we get its Symbol information
						Symbol sym = scope.get_symbol_from_id_expr(*ref);

						// And we insert it into the private_symbols set
						private_symbols.insert(sym);
					}
				}
				
				// If no DEFAULT(NONE) is not specified (or DEFAULT(SHARED)
				// is), then we have to gather additional shared variables
				// declared just before the construct

				// Declare a predicate for DEFAULT(NONE)
				PredicateBool<OMP_IS_DEFAULT_NONE_CLAUSE> none_clause;

				// And get all the DEFAULT(NONE) in this node 
				// (mental note: maybe we should restrict clause traversals to
				// the directive AST)
				AST_list_t none_clauses = node.depth_subtrees().filter(none_clause);

				// If no "DEFAULT(NONE)" was found, then we have to get all symbols
				// and see if they were declarated prior the parallel construct
				if (none_clauses.empty())
				{
					// Here we get the scope *at the construct* point (not
					// the construct body one). This is useful to catch thinks like the following.
					//
					// int b;
					// #pragma parallel
					// {
					//    int c;
					//    b = 2; <-- This must be "shared"
					//    c = 3; <-- This is already private
					// }
					//
					Scope construct_scope = omp_context.scope_link.get_scope(node);
					AST_t construct_body = node.get_attribute(OMP_CONSTRUCT_BODY);

					AST_list_t references = construct_body.depth_subtrees().filter(id_expression);
					for (AST_list_t::iterator it = references.begin();
							it != references.end();
							it++)
					{
						Symbol sym = construct_scope.get_symbol_from_id_expr(*it);

						// If the symbol was found this means was defined at the point
						// where the construct was defined
						if (sym != Symbol::invalid())
						{
							// An additional check has still to be done
							// since we could shadow a variable if we replaced "int c;"
							// with "int b;" [and removed "c = 3;" of course]
							//
							// int b;
							// #pragma parallel
							// {
							//   int b; <-- Shadows outer b
							//   b = 2; <-- This must not be shared!
							// }
							//
							// Get the inner scope
							Scope inner_scope = omp_context.scope_link.get_scope(*it);
							Symbol inner_symbol = construct_scope.get_symbol_from_id_expr(*it);

							// They have to be the same, if not, the inner_symbol shadows the
							// outer one
							if (sym == inner_symbol)
							{
								// We have to ensure the symbol has not been already set private
								if (private_symbols.find(sym) == private_symbols.end())
								{
									shared_symbols.insert(sym);
								}
							}
						}
					}
				}
			}

            virtual ~ParallelFunctor() { }
    };

	// All this phase starts here
    void OpenMPTransform::run(DTO& dto)
    {
		// Get the translation_unit tree
        translation_unit = dto["translation_unit"];
		// Get the scope_link
        scope_link = dto["scope_link"];
		// Get the global_scope
        global_scope = scope_link.get_scope(translation_unit);

		// Instantiate a DepthTraverse
        DepthTraverse depth_traverse;

		// Functor for #pragma omp parallel
        ParallelFunctor parallel_functor(*this);

		// Register the #pragma omp parallel 
		// filter with its functor
        depth_traverse.add_predicate(parallel_construct, parallel_functor);

		// Traverse in a depth-first fashion the AST
        depth_traverse.traverse(translation_unit);
    }
}
