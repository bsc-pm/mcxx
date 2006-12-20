#include "tl-omp.hpp"
#include "tl-builtin.hpp"
#include "tl-ast.hpp"
#include "tl-source.hpp"
#include "tl-scopelink.hpp"
#include "tl-traverse.hpp"
#include "tl-predicateutils.hpp"
#include "cxx-attrnames.h"

namespace TL
{
	// This is something common, it is a good candidate to be taken off here
	class GetSymbolFromAST : public Functor<Symbol, AST_t>
	{
		private:
			ScopeLink scope_link;
		public:
			virtual Symbol operator()(AST_t& ast) const 
			{
				Scope sc = scope_link.get_scope(ast);

				Symbol result = sc.get_symbol_from_id_expr(ast);

				return result;
			}

			GetSymbolFromAST(ScopeLink _scope_link)
				: scope_link(_scope_link)
			{
			}

			~GetSymbolFromAST()
			{
			}
	};

	namespace OpenMP
	{
		Directive Construct::directive()
		{
			AST_t ast = _ref.get_attribute(OMP_CONSTRUCT_DIRECTIVE);

			Directive result(ast, _scope_link);
			return result;
		}

		Statement Construct::body()
		{
			AST_t ast = _ref.get_attribute(OMP_CONSTRUCT_BODY);
			Statement result(ast, _scope_link);

			return result;
		}

		void CustomConstructFunctor::dispatch_custom_construct(CustomFunctorMap& search_map, Context ctx, AST_t node)
		{
			TL::Bool is_directive = node.get_attribute(OMP_IS_CUSTOM_DIRECTIVE);
			TL::Bool is_construct = node.get_attribute(OMP_IS_CUSTOM_CONSTRUCT);

			AST_t directive;
			if (is_construct)
			{
				directive = node.get_attribute(OMP_CONSTRUCT_DIRECTIVE);
			}
			else
			{
				directive = node;
			}

			TL::String directive_name = directive.get_attribute(OMP_CUSTOM_DIRECTIVE_NAME);

			// Find this directive in custom preorder map
			if (search_map.find(directive_name) != search_map.end())
			{
				// Invoke its functor if found
				CustomConstruct custom_construct(node, ctx.scope_link);
				Signal1<CustomConstruct>& functor = search_map[directive_name];

				functor.signal(custom_construct);
			}
		}

		void CustomConstructFunctor::preorder(Context ctx, AST_t node)
		{
			dispatch_custom_construct(_custom_functor_pre, ctx, node);
		}

		void CustomConstructFunctor::postorder(Context ctx, AST_t node)
		{
			dispatch_custom_construct(_custom_functor_post, ctx, node);
		}

		void OpenMPPhase::run(DTO& data_flow)
		{
			// get the translation_unit tree
			translation_unit = data_flow["translation_unit"];
			// get the scope_link
			scope_link = data_flow["scope_link"];
			// Get the global_scope
			global_scope = scope_link.get_scope(translation_unit);

			// Instantiate a DepthTraverse
			DepthTraverse depth_traverse;

			// Functor for #pragma omp parallel
			PredicateBool<OMP_IS_PARALLEL_CONSTRUCT> parallel_construct;
			ParallelFunctor parallel_functor(on_parallel_pre, on_parallel_post);
			// Register the #pragma omp parallel 
			// filter with its functor
			depth_traverse.add_predicate(parallel_construct, parallel_functor);

			// Functor for #pragma omp parallel for
			PredicateBool<OMP_IS_PARALLEL_FOR_CONSTRUCT> parallel_for_construct;
			ParallelForFunctor parallel_for_functor(on_parallel_for_pre, on_parallel_for_post);
			// Register the #pragma omp parallel for
			// filter with its functor 
			depth_traverse.add_predicate(parallel_for_construct, parallel_for_functor);

			// Functor for #pragma omp for
			PredicateBool<OMP_IS_FOR_CONSTRUCT> for_construct;
			ForFunctor for_functor(on_for_pre, on_for_post);
			// Register the #pragma omp parallel for
			// filter with its functor 
			depth_traverse.add_predicate(for_construct, for_functor);

			// #pragma omp parallel sections
			PredicateBool<OMP_IS_PARALLEL_SECTIONS_CONSTRUCT> parallel_sections_construct;
			ParallelSectionsFunctor parallel_sections_functor(on_parallel_sections_pre, 
					on_parallel_sections_post);
			depth_traverse.add_predicate(parallel_sections_construct, parallel_sections_functor);

			// #pragma omp section
			PredicateBool<OMP_IS_SECTION_CONSTRUCT> section_construct;
			SectionFunctor section_functor(on_section_pre, on_section_post);
			depth_traverse.add_predicate(section_construct, section_functor);
			
			// #pragma omp barrier
			PredicateBool<OMP_IS_BARRIER_DIRECTIVE> barrier_directive;
			BarrierFunctor barrier_functor(on_barrier_pre, on_barrier_post);
			depth_traverse.add_predicate(barrier_directive, barrier_functor);

			// #pragma omp atomic
			PredicateBool<OMP_IS_ATOMIC_CONSTRUCT> atomic_construct;
			AtomicFunctor atomic_functor(on_atomic_pre, on_atomic_post);
			depth_traverse.add_predicate(atomic_construct, atomic_functor);
			
			// #pragma omp critical
			PredicateBool<OMP_IS_CRITICAL_CONSTRUCT> critical_construct;
			CriticalFunctor critical_functor(on_critical_pre, on_critical_post);
			depth_traverse.add_predicate(critical_construct, critical_functor);

			// #pragma omp single
			PredicateBool<OMP_IS_SINGLE_CONSTRUCT> single_construct;
			SingleFunctor single_functor(on_single_pre, on_single_post);
			depth_traverse.add_predicate(single_construct, single_functor);

			// #pragma omp flush
			PredicateBool<OMP_IS_FLUSH_DIRECTIVE> flush_directive;
			FlushFunctor flush_functor(on_flush_pre, on_flush_post);
			depth_traverse.add_predicate(flush_directive, flush_functor);

			// #pragma omp constructs|directives
			// (custom constructions)
			PredicateBool<OMP_IS_CUSTOM_CONSTRUCT> custom_construct;
			CustomConstructFunctor custom_construct_functor(on_custom_construct_pre, on_custom_construct_post);
			depth_traverse.add_predicate(custom_construct, custom_construct_functor);
			PredicateBool<OMP_IS_CUSTOM_DIRECTIVE> custom_directive;
			depth_traverse.add_predicate(custom_directive, custom_construct_functor);
			
			// Let the user register its slots
			this->init();

			// Traverse in a depth-first fashion the AST
			depth_traverse.traverse(translation_unit, scope_link);
		}

		void OpenMPPhase::init()
		{
		}

		Clause Directive::nowait_clause()
		{
			Clause result(_ref, _scope_link, OMP_IS_NOWAIT_CLAUSE);
			return result;
		}

		bool Clause::is_defined()
		{
			PredicateAttr predicate_clause(_clause_filter_name);
			ObjectList<AST_t> clauses = _ref.depth_subtrees().filter(predicate_clause);

			return (!clauses.empty());
		}

		DefaultClause Directive::default_clause()
		{
			class DefaultClausePredicate : public Predicate<AST_t>
			{
				public:
					virtual bool operator()(AST_t& ast) const
					{
						TL::Bool attr1 = ast.get_attribute(OMP_IS_DEFAULT_NONE_CLAUSE);
						TL::Bool attr2 = ast.get_attribute(OMP_IS_DEFAULT_SHARED_CLAUSE);

						return attr1 || attr2;
					}
					virtual ~DefaultClausePredicate() { }
			};

			DefaultClausePredicate default_clause_predicate;

			ObjectList<AST_t> clauses = _ref.depth_subtrees().filter(default_clause_predicate);

			if (clauses.empty())
			{
				AST_t empty;
				DefaultClause result(empty, _scope_link);

				return result;
			}
			else
			{
				DefaultClause result(*(clauses.begin()), _scope_link);

				return result;
			}
		}

		Clause Directive::parameter_clause()
		{
			Clause result(_ref, _scope_link, OMP_IS_PARAMETER_CLAUSE);

			return result;
		}

		Clause Directive::shared_clause()
		{
			Clause result(_ref, _scope_link, OMP_IS_SHARED_CLAUSE);
			return result;
		}

		Clause Directive::private_clause()
		{
			Clause result(_ref, _scope_link, OMP_IS_PRIVATE_CLAUSE);
			return result;
		}

		Clause Directive::firstprivate_clause()
		{
			Clause result(_ref, _scope_link, OMP_IS_FIRSTPRIVATE_CLAUSE);
			return result;
		}

		Clause Directive::lastprivate_clause()
		{
			Clause result(_ref, _scope_link, OMP_IS_LASTPRIVATE_CLAUSE);
			return result;
		}

        Clause Directive::num_threads_clause()
        {
			Clause result(_ref, _scope_link, OMP_IS_NUM_THREADS_CLAUSE);
			return result;
        }

		CustomClause Directive::custom_clause(const std::string& src)
		{
			CustomClause result(src, _ref, _scope_link);
			return result;
		}

		ObjectList<AST_t> CustomClause::filter_custom_clause()
		{
			class PredicateCustomClause : public Predicate<AST_t>
			{
				private:
					PredicateAttr _custom_clause;
					const std::string& _clause_name;

				public:
					PredicateCustomClause(const std::string& clause_name)
						 : _custom_clause(OMP_IS_CUSTOM_CLAUSE), _clause_name(clause_name)
					{
					}

					virtual bool operator()(AST_t& t) const
					{
						if (_custom_clause(t))
						{
							TL::String clause_name_attr = t.get_attribute(OMP_CUSTOM_CLAUSE_NAME);

							return (clause_name_attr.compare_case_insensitive_to(_clause_name));
						}
						else return false;
					}
			};

			PredicateCustomClause predicate_custom_clause(_clause_name);

			ObjectList<AST_t> result = _ref.depth_subtrees(predicate_custom_clause);

			return result;
		}

		bool CustomClause::is_defined()
		{
			ObjectList<AST_t> clauses = filter_custom_clause();

			return (!clauses.empty());
		}

        // Time to start to think to fuse Clause and CustomClause in an inheritance tree
		ObjectList<Expression> Clause::get_expression_list()
		{
			PredicateBool<LANG_IS_EXPRESSION_NEST> expression_nest;
			ObjectList<Expression> result;

			PredicateAttr predicate_clause(_clause_filter_name);
			ObjectList<AST_t> clauses = _ref.depth_subtrees().filter(predicate_clause);

			for (ObjectList<AST_t>::iterator it = clauses.begin();
					it != clauses.end();
					it++)
			{
				ObjectList<AST_t> expression_nest_list = it->depth_subtrees(expression_nest, AST_t::NON_RECURSIVE);

				for (ObjectList<AST_t>::iterator it2 = expression_nest_list.begin();
						it2 != expression_nest_list.end();
						it2++)
				{
					Expression expr(*it2, _scope_link);
					result.append(expr);
				}
			}

			return result;
		}

		ObjectList<Expression> CustomClause::get_expression_list()
		{
			PredicateBool<LANG_IS_EXPRESSION_NEST> expression_nest;
			ObjectList<Expression> result;

			ObjectList<AST_t> custom_clauses = filter_custom_clause();
			for (ObjectList<AST_t>::iterator it = custom_clauses.begin();
					it != custom_clauses.end();
					it++)
			{
				ObjectList<AST_t> expression_nest_list = it->depth_subtrees(expression_nest, AST_t::NON_RECURSIVE);

				for (ObjectList<AST_t>::iterator it2 = expression_nest_list.begin();
						it2 != expression_nest_list.end();
						it2++)
				{
					Expression expr(*it2, _scope_link);
					result.append(expr);
				}
			}

			return result;
		}

		ObjectList<IdExpression> CustomClause::id_expressions(IdExpressionCriteria criteria)
		{
			PredicateBool<LANG_IS_ID_EXPRESSION> id_expr_pred;

			ObjectList<AST_t> clauses = filter_custom_clause();

			ObjectList<IdExpression> result;
			GetSymbolFromAST get_symbol_from_ast(this->_scope_link);

			for(ObjectList<AST_t>::iterator it = clauses.begin();
					it != clauses.end();
					it++)
			{
				ObjectList<AST_t> id_expressions = it->depth_subtrees().filter(id_expr_pred);

				for (ObjectList<AST_t>::iterator jt = id_expressions.begin();
						jt != id_expressions.end();
						jt++)
				{
					Symbol sym = get_symbol_from_ast(*jt);

					bool eligible = false;

					switch (criteria)
					{
						case ALL_SYMBOLS :
							eligible = true;
							break;
						case VALID_SYMBOLS :
							eligible = sym.is_valid();
							break;
						case INVALID_SYMBOLS :
							eligible = !sym.is_valid();
							break;
					}

					if (eligible)
					{
						IdExpression id_expr(*jt, this->_scope_link);
						result.append(id_expr);
					}
				}
			}

			return result;
		}

        ReductionClause Directive::reduction_clause()
        {
            ReductionClause result(_ref, _scope_link);
            return result;
        }

		bool DefaultClause::is_none() const
		{
			return _ref.is_valid() 
				&& TL::Bool(_ref.get_attribute(OMP_IS_DEFAULT_NONE_CLAUSE));
		}

		bool DefaultClause::is_shared() const
		{
			return _ref.is_valid() 
				&& TL::Bool(_ref.get_attribute(OMP_IS_DEFAULT_SHARED_CLAUSE));
		}

		ObjectList<IdExpression> Clause::id_expressions(IdExpressionCriteria criteria)
		{
			PredicateBool<LANG_IS_ID_EXPRESSION> id_expr_pred;

			PredicateAttr predicate_clause(_clause_filter_name);
			ObjectList<AST_t> clauses = _ref.depth_subtrees().filter(predicate_clause);

			ObjectList<IdExpression> result;
			GetSymbolFromAST get_symbol_from_ast(this->_scope_link);

			for(ObjectList<AST_t>::iterator it = clauses.begin();
					it != clauses.end();
					it++)
			{
				ObjectList<AST_t> id_expressions = it->depth_subtrees();

				id_expressions = id_expressions.filter(id_expr_pred);

				for (ObjectList<AST_t>::iterator jt = id_expressions.begin();
						jt != id_expressions.end();
						jt++)
				{
					Symbol sym = get_symbol_from_ast(*jt);

					bool eligible = false;

					switch (criteria)
					{
						case ALL_SYMBOLS :
							eligible = true;
							break;
						case VALID_SYMBOLS :
							eligible = sym.is_valid();
							break;
						case INVALID_SYMBOLS :
							eligible = !sym.is_valid();
							break;
					}

					if (eligible)
					{
						IdExpression id_expr(*jt, this->_scope_link);
						result.append(id_expr);
					}
				}
			}

			return result;
		}

		ObjectList<ReductionIdExpression> ReductionClause::id_expressions(IdExpressionCriteria criteria)
		{
			PredicateBool<LANG_IS_ID_EXPRESSION> id_expr_pred;

			ObjectList<ReductionIdExpression> result;
			GetSymbolFromAST get_symbol_from_ast(this->_scope_link);

			PredicateAttr reduction_clause_predicate(OMP_IS_REDUCTION_CLAUSE);

			ObjectList<AST_t> reduction_clauses = _ref.depth_subtrees().filter(reduction_clause_predicate);

			for (ObjectList<AST_t>::iterator it = reduction_clauses.begin();
					it != reduction_clauses.end();
					it++)
			{
				AST_t reduction_clause = *it;

				AST_t reduct_operator = it->get_attribute(OMP_REDUCTION_OPERATOR);
				AST_t reduct_neuter = it->get_attribute(OMP_REDUCTION_NEUTER);

				AST_t reduct_vars = it->get_attribute(OMP_REDUCTION_VARIABLES);

				ObjectList<AST_t> reduct_references = reduct_vars.depth_subtrees().filter(id_expr_pred);

				for (ObjectList<AST_t>::iterator jt = reduct_references.begin();
						jt != reduct_references.end();
						jt++)
				{
					Symbol sym = get_symbol_from_ast(*jt);

					bool eligible = false;

					switch (criteria)
					{
						case ALL_SYMBOLS :
							eligible = true;
							break;
						case VALID_SYMBOLS :
							eligible = sym.is_valid();
							break;
						case INVALID_SYMBOLS :
							eligible = !sym.is_valid();
							break;
					}

					if (eligible)
					{
						IdExpression id_expr(*jt, this->_scope_link);
						ReductionIdExpression reduct_id_expr(id_expr, reduct_operator, reduct_neuter);

						result.append(reduct_id_expr);
					}
				}
			}

			return result;
		}
	}
}

// namespace TL
// {
// 	// The whole phase starts at the function OpenMPTransform::run at the end
// 	// of this file
// 	//
// 	// A predicate for "#pragma omp parallel { ... }"
//     PredicateBool<OMP_IS_PARALLEL_CONSTRUCT> parallel_construct;
// 	// A predicate for the nuclear expression that is neither a parenthesized
// 	// expression nor a literal (thus the name of an object)
//     PredicateBool<LANG_IS_ID_EXPRESSION> id_expression;
// 
// 	// This class implements a TraverseFunctor
// 	// a TraverseFunctor has two methods preorder and postorder that
// 	// user has to override. 
// 	//
// 	// A depth-traversal is performed. When a node (call it Q) satisfies the
// 	// predicate related to this functor (defined below in the run function at
// 	// the end of the file) member functions 'preorder' and 'postorder' are
// 	// invoked just before and after the depth-traversal of Q subtrees.
//     class ParallelFunctor : public TraverseFunctor
//     {
//         private:
// 			// The OpenMP transform context
//             OpenMPTransform& omp_context;
// 
// 			// We will save Symbols here for shared and private entities.
// 			//
// 			// NOTE: Symbols are not AST_t, but a wrapper of the symbol table
// 			// entries of the compiler. Symbol class offers to the user the
// 			// symbolic work that the semantic phase has performed earlier.
// 			std::set<Symbol> shared_symbols;
// 			std::set<Symbol> private_symbols;
// 
// 			std::string function_name;
//         public:
//             ParallelFunctor(OpenMPTransform& omp_ctx)
//                 : omp_context(omp_ctx)
//             {
//             }
// 
// 			// This is invoked in preorder for #pragma omp parallel
//             virtual void preorder(Context ctx, AST_t node)
//             {
// 				// One more level of nesting
// 				omp_context.parallel_nesting++;
//             }
// 
// 			// This is executed in postorder for "#pragma omp parallel { ... }"
// 			//   "node" will be the whole "#pragma omp parallel { ... }" construct
//             virtual void postorder(Context ctx, AST_t node)
//             {
// 				// One more parallel region found
//                 omp_context.num_parallels++;
// 				// One less level of nesting
// 				omp_context.parallel_nesting--;
// 
// 				// We set up data attributes (shared, private, etc). See below
// 				setup_data_attributes(ctx, node);
// 
// 				// Here we create the outline
// 				create_outline(ctx, node);
// 
// 				// And replace the whole directive with the spawn code
// 				replace_with_spawn_code(ctx, node);
//             }
// 
// 			void replace_with_spawn_code(Context ctx, AST_t node)
// 			{
// 				// This will hold the spawn code
// 				Source spawn_code;
// 				Source shared_parameters;
// 
// 				// The skeleton
// 				spawn_code 
// 					<< "{"
// 					<< "   extern void nthf_create(...);"
// 				    << "   extern int nthf_num_cpus();"
// 				    << "   for (int _i = 0; _i < nthf_num_cpus(); _i++)"
// 					<< "   {"
// 					<< "      nthf_create(outlined_" << this->function_name << ", " << shared_parameters << ");"
// 					<< "   }"
// 					<< "}";
// 
// 				// Here we fill shared_parameters
// 				shared_parameters << shared_symbols.size();
// 				for (std::set<Symbol>::iterator it = shared_symbols.begin();
// 						it != shared_symbols.end(); it++)
// 				{
// 					shared_parameters.append_with_separator(std::string("&") + it->get_name(), ",");
// 				}
// 
// 				// Now parse the spawning code
// 				// First get [again, this must be improved] the construct scope
// 				Scope construct_scope = omp_context.scope_link.get_scope(node);
// 
// 				// We parse this statement
// 				AST_t spawn_code_tree = spawn_code.parse_statement(construct_scope, omp_context.scope_link);
// 
// 				// And replace the whole construct with the spawning code
// 				node.replace_with(spawn_code_tree);
// 			}
// 
// 			void create_outline(Context ctx, AST_t node)
// 			{
// 				// Get the scope of the whole Parallel construct
// 				//
// 				// omp_context is defined in tl-omp.hpp, it is some context that
// 				// this phase is keeping to ease the process.
// 				Scope construct_scope = omp_context.scope_link.get_scope(node);
// 
// 				// Get the inner body of this parallel region
// 				AST_t construct_body = node.get_attribute(OMP_CONSTRUCT_BODY);
// 
// 				// This will be the variables that will hold several parts of the
// 				// outlining
// 				Source outlined_function;
// 				Source outlined_body;
// 				Source privatized_variables;
// 				Source shared_parameters;
// 				Source outline_function_name;
// 
// 				// We create the skeleton of the outline
// 				//
// 				// Note that Source::operator<<(Source&) saves a reference to the Source,
// 				// so here we have defined the "schema" and later we will fill them
// 				// with the missing source. 
// 				//
// 				// Similarly Source::operator<<(const std::string&) just  saves
// 				// a chunk of text.
// 				outlined_function << "void outlined_" << outline_function_name << "(" << shared_parameters << ")"
// 					<< "{"
// 					<< privatized_variables
// 					<< outlined_body
// 					<< "}";
// 
// 				// Declare shared parameters. We fill here "shared_parameters"
// 				// Iterate every symbol in "shared_symbols"
// 				for (std::set<Symbol>::iterator it = shared_symbols.begin();
// 						it != shared_symbols.end();
// 						it++)
// 				{
// 					// We get its Type (this is not an AST_t but a wrapper to
// 					// the type information currently in the symbol table)
// 					Type pointer_to = it->get_type().get_pointer_to();
// 
// 					// This is a convenience function for appending things in
// 					// string lists
// 					shared_parameters.append_with_separator(
// 							// This creates a string that declares this type
// 							// with the given name (note that "parameter" here
// 							// means that won't have a trailing ';' but this
// 							// can be abstracted away)
// 							pointer_to.get_parameter_declaration_str(it->get_name()), 
// 							",");
// 				}
// 
// 				// Declare private variables. We fill privatized_variables here
// 				// We iterate every private symbol
// 				for (std::set<Symbol>::iterator it = private_symbols.begin();
// 						it != private_symbols.end();
// 						it++)
// 				{
// 					Type symbol_type = it->get_type();
// 
// 					privatized_variables 
// 						// And we get the declaration of the symbol name but prefixed with 'p_'
// 						// (note that "simple" here means that will have a trailing ';' and
// 						// like the earlier case, this can be abstracted away)
// 						<< symbol_type.get_simple_declaration_str(std::string("p_") + it->get_name());
// 				}
// 				
// 				// Now we replace shared references with true derreferences
// 				// First we duplicate the construct_body
// 				AST_t outlined_body_tree = construct_body.duplicate();
// 				
// 				// Now we get all the references of the outlined_body_tree
// 				AST_list_t shared_references = outlined_body_tree.depth_subtrees().filter(id_expression);
// 				// And iterate
// 				for(AST_list_t::iterator it = shared_references.begin(); 
// 						it != shared_references.end(); it++)
// 				{
// 					// We get the symbol in the ORIGINAL scope (not the copy
// 					// one since the duplicated tree does not have scope)
// 					Symbol sym = construct_scope.get_symbol_from_id_expr(*it);
// 
// 					if (sym != Symbol::invalid())
// 					{
// 						// If the symbol is found in the scope and belongs to shared
// 						if (shared_symbols.find(sym) != shared_symbols.end())
// 						{
// 							// We derreference it 
// 							Source derref_expression;
// 							derref_expression 
// 								<< "(*" << it->prettyprint() << ")";
// 
// 							// This parses this string (*P) as an expression
// 							AST_t derref_expression_tree = derref_expression.parse_expression(construct_scope);
// 
// 							// And we replace the whole id-expression with this derreference expression
// 							it->replace_with(derref_expression_tree);
// 						}
// 					}
// 				}
// 
// 				// Now we replace privatized references from "id" to "p_id"
// 				// We get all the references of the outlined_body_tree
// 				AST_list_t privatized_references = outlined_body_tree.depth_subtrees().filter(id_expression);
// 				
// 				for(AST_list_t::iterator it = privatized_references.begin(); 
// 						it != privatized_references.end(); it++)
// 				{
// 					// We get its symbolic information
// 					Symbol sym = construct_scope.get_symbol_from_id_expr(*it);
// 
// 					if (sym != Symbol::invalid())
// 					{
// 						// And if it exists and is privatized
// 						if (private_symbols.find(sym) != private_symbols.end())
// 						{
// 							// We simply replace the text.
// 							//
// 							// Before we replaced q -> (*q) via an expression because
// 							// we don't have a variable named '(*q)' but 'q'.
// 							//
// 							// In this case we don't have any variable named 'q', but 'p_q'
// 							it->replace_text(std::string("p_") + sym.get_name());
// 						}
// 					}
// 				}
// 
// 				// Now we prettyprint the outlined_body_tree once it has been
// 				// modified to the corresponding source (recall this was
// 				// referenced in the skeleton of the outline)
// 				outlined_body << outlined_body_tree.prettyprint();
// 
// 				// Get the enclosing function of this node
// 				AST_t enclosing_function_def = node.get_enclosing_function_definition();
// 				// and its scope.
// 				Scope enclosing_function_def_scope = omp_context.scope_link.get_scope(enclosing_function_def);
// 				// Then get its name. Maybe this is the typical "context" that
// 				// can be given to the preorder and postorder functions while
// 				// doing the traversal, to avoid doing this everytime when this
// 				// kind of "locational information" is needed
// 				AST_t function_name_tree = enclosing_function_def.get_attribute(LANG_FUNCTION_NAME);
// 
// 				// Now set the outline_function_name. 
// 				// We use a stringstream to append an integer
// 				std::stringstream ss;
// 				ss << function_name_tree.prettyprint() << omp_context.num_parallels;
// 				this->function_name = ss.str();
// 
// 				outline_function_name << this->function_name;
// 
// 				// Now we parse the source that represents the outlined function
// 				AST_t outlined_function_tree = 
// 					outlined_function.parse_global(enclosing_function_def_scope, omp_context.scope_link);
// 
// 				// And we prepend as a sibling of the enclosing function
// 				node.prepend_sibling_function(outlined_function_tree);
// 			}
// 
// 			void setup_data_attributes(Context ctx, AST_t node)
// 			{
// 				// Get every shared reference
// 				//
// 				// First we declare "shared_clause" as the predicate for the shared clauses
// 				PredicateBool<OMP_IS_SHARED_CLAUSE> shared_clause;
// 
// 				// We get a list of subtrees that satisfy "shared_clause", thus
// 				// they are trees that hold a "SHARED(a, b, c, ...)"
// 				AST_list_t shared_clauses = node.depth_subtrees().filter(shared_clause);
// 
// 				// We traverse every element of this node (AST_t) list, recall
// 				// that every "*it" will be a "SHARED(a, b, c, ...)"
// 				for (AST_list_t::iterator it = shared_clauses.begin(); 
// 						it != shared_clauses.end(); it++)
// 				{
// 					// Now, we get a list of subtrees that satisfy
// 					// "id_expression" predicate, thus, they are of the form
// 					// 		a
// 					// 		A::b
// 					// 		A<B1, B2, ...>::b
// 					AST_set_t references = it->depth_subtrees().filter(id_expression);
// 
// 					// We traverse every element of this list (recall that
// 					// every "ref" is an id-expressions)
// 					for (AST_set_t::iterator ref = references.begin();
// 							ref != references.end(); ref++)
// 					{
// 						// We have to get the scope where this node belongs to
// 						Scope scope = omp_context.scope_link.get_scope(*ref);
// 						// Since we now it is an id expression we use the scope
// 						// to get the Symbol.
// 						//
// 						// Symbol is a class that wraps the exact symbolic information
// 						// that is stored in the symbol table of every scope.
// 						Symbol sym = scope.get_symbol_from_id_expr(*ref);
// 
// 						// Add it into the set of shared_symbols (this is a
// 						// std::set<Symbol>)
// 						shared_symbols.insert(sym);
// 					}
// 				}
// 				
// 				// Get every private reference (this is symmetric to the shared case)
// 				//
// 				// First we declare "private_clause" as the predicate for the private clauses
// 				PredicateBool<OMP_IS_PRIVATE_CLAUSE> private_clause;
// 
// 				// We get a list of subtrees that satisfy "private_clause", thus they are of the form
// 				// PRIVATE(a, b, c, ...)
// 				AST_list_t private_clauses = node.depth_subtrees().filter(private_clause);
// 
// 				// We traverse every element. 
// 				// Recall that "it" will be an AST that holds a "PRIVATE(...)"
// 				for (AST_list_t::iterator it = private_clauses.begin(); 
// 						it != private_clauses.end(); it++)
// 				{
// 					// Get a list of references, in fact, we get a list of id_expressions
// 					AST_set_t references = it->depth_subtrees().filter(id_expression);
// 
// 					// And we traverse every reference
// 					for (AST_set_t::iterator ref = references.begin();
// 							ref != references.end(); ref++)
// 					{
// 						// We get the scope of every reference
// 						Scope scope = omp_context.scope_link.get_scope(*ref);
// 						
// 						// And we get its Symbol information
// 						Symbol sym = scope.get_symbol_from_id_expr(*ref);
// 
// 						// And we insert it into the private_symbols set
// 						private_symbols.insert(sym);
// 					}
// 				}
// 				
// 				// If no DEFAULT(NONE) is not specified (or DEFAULT(SHARED)
// 				// is), then we have to gather additional shared variables
// 				// declared just before the construct
// 
// 				// Declare a predicate for DEFAULT(NONE)
// 				PredicateBool<OMP_IS_DEFAULT_NONE_CLAUSE> none_clause;
// 
// 				// And get all the DEFAULT(NONE) in this node 
// 				// (mental note: maybe we should restrict clause traversals to
// 				// the directive AST)
// 				AST_list_t none_clauses = node.depth_subtrees().filter(none_clause);
// 
// 				// If no "DEFAULT(NONE)" was found, then we have to get all symbols
// 				// and see if they were declarated prior the parallel construct
// 				if (none_clauses.empty())
// 				{
// 					// Here we get the scope *at the construct* point (not
// 					// the construct body one). This is useful to catch thinks like the following.
// 					//
// 					// int b;
// 					// #pragma parallel
// 					// {
// 					//    int c;
// 					//    b = 2; <-- This must be "shared"
// 					//    c = 3; <-- This is already private
// 					// }
// 					//
// 					Scope construct_scope = omp_context.scope_link.get_scope(node);
// 					AST_t construct_body = node.get_attribute(OMP_CONSTRUCT_BODY);
// 
// 					AST_list_t references = construct_body.depth_subtrees().filter(id_expression);
// 					for (AST_list_t::iterator it = references.begin();
// 							it != references.end();
// 							it++)
// 					{
// 						Symbol sym = construct_scope.get_symbol_from_id_expr(*it);
// 
// 						// If the symbol was found this means was defined at the point
// 						// where the construct was defined
// 						if (sym == Symbol::invalid())
// 							continue;
// 						// An additional check has still to be done
// 						// since we could shadow a variable if we replaced "int c;"
// 						// with "int b;" [and removed "c = 3;" of course]
// 						//
// 						// int b;
// 						// #pragma parallel
// 						// {
// 						//   int b; <-- Shadows outer b
// 						//   b = 2; <-- This must not be shared!
// 						// }
// 						//
// 						// Get the inner scope
// 						Scope inner_scope = omp_context.scope_link.get_scope(*it);
// 						Symbol inner_symbol = construct_scope.get_symbol_from_id_expr(*it);
// 
// 						// They have to be the same, if not, the inner_symbol shadows the
// 						// outer one
// 						if (sym == inner_symbol)
// 						{
// 							// We have to ensure the symbol has not been already set private
// 							if (private_symbols.find(sym) == private_symbols.end())
// 							{
// 								shared_symbols.insert(sym);
// 							}
// 						}
// 					} // for
// 				} // if
// 			}
// 
//             virtual ~ParallelFunctor() { }
//     };
// 
// 	// All this phase starts here
//     void OpenMPTransform::run(DTO& dto)
//     {
// 		// get the translation_unit tree
//         translation_unit = dto["translation_unit"];
// 		// get the scope_link
//         scope_link = dto["scope_link"];
// 		// Get the global_scope
//         global_scope = scope_link.get_scope(translation_unit);
// 
// 		// Instantiate a DepthTraverse
//         DepthTraverse depth_traverse;
// 
// 		// Functor for #pragma omp parallel
//         ParallelFunctor parallel_functor(*this);
// 
// 		// Register the #pragma omp parallel 
// 		// filter with its functor
//         depth_traverse.add_predicate(parallel_construct, parallel_functor);
// 
// 		// Now let the user initialize
// 		this->init();
// 
// 		// Traverse in a depth-first fashion the AST
//         depth_traverse.traverse(translation_unit);
//     }
// }
