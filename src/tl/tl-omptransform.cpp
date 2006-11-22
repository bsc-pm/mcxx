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
            int num_parallels;
            int parallel_nesting;
        public:
            OpenMPTransform()
            {
            }

            virtual void init()
            {
                // Register the handlers (callbacks) for every construction
                // for now only '#pragma omp parallel'
                on_parallel_pre.connect(&OpenMPTransform::parallel_pre, *this);
                on_parallel_post.connect(&OpenMPTransform::parallel_post, *this);
            }

            // Parallel preorder
            void parallel_pre(OpenMP::ParallelConstruct parallel_construct)
            {
                parallel_nesting++;
            }

            // Parallel postorder
            void parallel_post(OpenMP::ParallelConstruct parallel_construct)
            {
                parallel_nesting--;
                num_parallels++;

                OpenMP::Directive directive = parallel_construct.directive();

                ObjectList<Symbol> shared_symbols;
                ObjectList<Symbol> private_symbols;

                // Get the enclosing function definition
                FunctionDefinition f = parallel_construct.get_enclosing_function();

                Source outline_function_name; 
                IdExpression function_name = f.get_function_name();

                Symbol function_symbol = function_name.get_symbol();

                outline_function_name
                    << function_name.get_qualified_part()
                    << "outline_"
                    << function_name.get_unqualified_part();

                // Get the body of the statement
                Statement body = parallel_construct.body();

                // Construct the set of shared and privatized symbols
                get_data_attributes(directive, body, shared_symbols, private_symbols);

                // Create the outline function
                Source outline_code;
                Source shared_parameters;
                outline_code 
                    << create_outline(function_symbol, shared_symbols, private_symbols, body, shared_parameters,
                            outline_function_name.get_source());

                if (function_symbol.is_member())
                {
                    // We have to declare it in the class where this symbol was declared
                    Source member_outline_declaration;
                    member_outline_declaration
                        << "static void outline_"
                        << function_name.get_unqualified_part()
                        << "(" << shared_parameters << ");";

                    std::cerr << "--- Outlined code for member ---" << std::endl;
                    std::cerr << member_outline_declaration.get_source() << std::endl;
                    std::cerr << "--- End outline code ---" << std::endl;

                    AST_t member_outline_decl_tree = 
                        member_outline_declaration.parse_global(f.get_scope(), 
                                f.get_scope_link());

                    std::cerr << "--- Member declaration parsed ---" << std::endl;
                }

                // std::cerr << "About to compile outline code '" << outline_code.get_source() << "'" << std::endl;

                AST_t outline_tree = outline_code.parse_global(f.get_scope(), f.get_scope_link());

                // std::cerr << "Outline code parsed" << std::endl;

                f.prepend_sibling(outline_tree);

                // Create the spawning code
                AST_t spawn_tree = create_spawn_code(function_symbol,
						parallel_construct.get_scope(), 
                        parallel_construct.get_scope_link(),
                        outline_function_name, shared_symbols);
                parallel_construct.get_ast().replace_with(spawn_tree);
            }

            AST_t create_spawn_code(Symbol function_symbol,
					Scope spawn_scope,
                    ScopeLink spawn_scope_link,
                    Source& outline_function_name,
                    ObjectList<Symbol>& shared_symbols)
            {
                Source spawning_code;
                Source shared_references;

				int num_parameters = shared_symbols.size();

				if (function_symbol.is_member())
				{
					num_parameters++;
				}

                // Spawn skeleton
                spawning_code 
                    << "{"
                    << "  extern int nth_num_threads();"
                    << "  extern int nth_create(...);"
                    << "  for (int i = 0; i < nth_num_threads(); i++)"
                    << "  {"
                    << "    nth_create(&" << outline_function_name << ", " 
					<<            num_parameters << shared_references << ");"
                    << "  }"
                    << "}"
                    ;

                // Transform every symbol 'name' to '&name'
                ObjectList<std::string> shared_references_names = shared_symbols.map(
                        functor(&OpenMPTransform::reference_to_name, *this)
                        );
				
				if (function_symbol.is_member())
				{
					shared_references << ", this";
				}

                if (!shared_references_names.empty())
                {
                    shared_references << ", " << concat_strings(shared_references_names, ",");
                }

                // std::cerr << "--SPAWN CODE--" << std::endl;
                // std::cerr << spawning_code.get_source() << std::endl;
                // std::cerr << "--END SPAWN CODE--" << std::endl;

                AST_t spawn_tree = spawning_code.parse_statement(spawn_scope, spawn_scope_link);
                return spawn_tree;
            }

            // The data enviroment
            void get_data_attributes(OpenMP::Directive& directive,
                    Statement& body,
                    ObjectList<Symbol>& shared_symbols,
                    ObjectList<Symbol>& private_symbols)
            {
                // Get symbols in shared clause
                OpenMP::Clause shared_clause = directive.shared_clause();
                shared_symbols = shared_clause.symbols();

                print_list("shared_symbols", shared_symbols.map(functor(&Symbol::get_name)));

                // Get symbols in private_clause
                OpenMP::Clause private_clause = directive.private_clause();
                private_symbols = private_clause.symbols();

                print_list("private_symbols", private_symbols.map(functor(&Symbol::get_name)));

                // default(none|shared) clause
                OpenMP::DefaultClause default_clause = directive.default_clause();

                // Recall there is no is_private() in C/C++
                if (!default_clause.is_none())
                {
                    ObjectList<IdExpression> symbol_ocurrences = body.non_local_symbol_occurrences();

                    // We don't want qualified names
                    symbol_ocurrences = symbol_ocurrences.filter(
                            predicate(&IdExpression::is_unqualified)
                            );

                    // For every symbol occurrence get it associated symbol
                    ObjectList<Symbol> symbols = symbol_ocurrences.map(
                            functor(&IdExpression::get_symbol)
                            );

                    // We only want variables
                    symbols = symbols.filter(
                            predicate(&Symbol::is_variable)
                            );

                    // that are not members of a class
                    symbols = symbols.filter(
                         negate(
                             predicate(&Symbol::is_member)
                             )
                         );

                    // and that are not already set private
                    symbols = symbols.filter(not_in_set(private_symbols));

                    // and not already set shared
                    symbols = symbols.filter(not_in_set(shared_symbols));

                    // add to the shared symbols
                    shared_symbols.insert(symbols);
                }
            }

            // Given a symbol declares a suitable parameter pointer to it
            std::string declare_parameter(Symbol& s)
            {
                // Get the type of the symbol
                Type type = s.get_type();
                // Construct a type that is a pointer to the original type
                Type pointer_type = type.get_pointer_to();

                // And return its declaration
                return pointer_type.get_declaration(s.get_name());
            }

            // Given a symbol declares a full private declaration to it
            std::string declare_privates(Symbol& s)
            {
                // Get the type
                Type type = s.get_type();

                // and return its declaration but the symbol 
                // declaration will have "p_" prepended
                return type.get_declaration(std::string("p_") + s.get_name()) 
                    + std::string(";");
            }

            void print_list(std::string name, ObjectList<std::string> list)
            {
                std::cerr << "###       " << name << " ###" << std::endl;
                for (ObjectList<std::string>::iterator it = list.begin();
                        it != list.end();
                        it++)
                {
                    std::cerr << "'" << (*it) << "'" << std::endl;
                }
                std::cerr << "### [end] " << name << " ###" << std::endl;
            }

            // Create the outline
            std::string create_outline(
					Symbol function_symbol,
					ObjectList<Symbol>& shared_symbols, 
                    ObjectList<Symbol>& private_symbols,
                    Statement& body,
                    Source& shared_parameters,
                    const std::string& outline_function_name)
            {
                Source privatized_variables;
                Source outlined_body;

                // Define the skeleton
                Source outline_code;

                outline_code
                    << "void " << outline_function_name << "(" << shared_parameters << ")"
                    << "{"
                    <<     privatized_variables
                    <<     outlined_body
                    << "}";

                // For every shared symbol, return a declaration to it
                ObjectList<std::string> parameter_declarations = shared_symbols.map(
                        functor(&OpenMPTransform::declare_parameter, *this)
                        );

				if (function_symbol.is_member())
				{
					Type class_type = function_symbol.member_of();

					Type pointer_to_class = class_type.get_pointer_to();

					std::string this_declaration = pointer_to_class.get_declaration("_this");

					shared_parameters << this_declaration << ", ";
				}

                // And concat all declarations with ','
                shared_parameters << concat_strings(parameter_declarations, ", ");

                ObjectList<std::string> private_declarations = private_symbols.map(
                        functor(&OpenMPTransform::declare_privates, *this)
                        );

                privatized_variables << concat_strings(private_declarations);
                
                // Copy the body since we will modify it
                std::pair<AST_t, ScopeLink> new_body = 
                    body.get_ast().duplicate_with_scope(body.get_scope_link());
                Statement modified_body(new_body.first, new_body.second);
                ScopeLink modified_body_scope_link = modified_body.get_scope_link();
                
                // Derreference all shared references
                ObjectList<IdExpression> non_local_symbols = 
                    modified_body.non_local_symbol_occurrences();

                for (ObjectList<IdExpression>::iterator 
                        it = non_local_symbols.begin();
                        it != non_local_symbols.end();
                        it++)
                {
                    if (find(shared_symbols.begin(), 
                                shared_symbols.end(), it->get_symbol()) != shared_symbols.end())
                    {
                        AST_t ref = it->get_ast();

                        Source derref_source;
                        derref_source << "(*" << ref.prettyprint() << ")";

                        // Get the scope of this expression
                        Scope expr_scope = modified_body_scope_link.get_scope(ref);

                        // Parse this new expression
                        AST_t derref_expr = derref_source.parse_expression(expr_scope);

                        // And replace it in the tree
                        ref.replace_with(derref_expr);
                    }
                }
                
				// Get again non local symbols
                non_local_symbols = modified_body.non_local_symbol_occurrences();
                // Rename all private references
                for (ObjectList<IdExpression>::iterator it = non_local_symbols.begin();
                        it != non_local_symbols.end();
                        it++)
                {
                    if (find(private_symbols.begin(), private_symbols.end(), 
                                it->get_symbol()) != private_symbols.end())
                    {
                        AST_t ref = it->get_ast();
						Symbol sym = it->get_symbol();

                        // Here we would have to mangle privatized qualified
                        // names (currently not done)
                        //
                        //   q       -> p_q
                        //   A::q    -> p_A__q
                        //   A<0>::q -> p_A_0___q 
                        //
						
						Source privatized_ref;
						privatized_ref << "p_" << sym.get_name();

                        Scope expr_scope = modified_body_scope_link.get_scope(ref);
						AST_t privatized_ref_tree = privatized_ref.parse_expression(expr_scope);

						ref.replace_with(privatized_ref_tree);
                        // name = "p_" + name;
                        // ref.replace_text(name);
                    }
                }
				
				// Get again non local symbols
				// Now replace the "this" entities
				if (function_symbol.is_member())
				{
					non_local_symbols = modified_body.non_local_symbol_occurrences();
					for (ObjectList<IdExpression>::iterator it = non_local_symbols.begin();
							it != non_local_symbols.end();
							it++)
					{
						if (it->is_unqualified())
						{
							Symbol s = it->get_symbol();

							if (s.is_member() 
									&& (function_symbol.member_of() == s.member_of()))
							{
								AST_t ref = it->get_ast();
								std::string name = ref.prettyprint();

								name = "_this->" + name;

								ref.replace_text(name);
							}
						}
					}
				}
                
                outlined_body << modified_body.prettyprint();

                std::cerr << "--OUTLINE CODE--" << std::endl;
                std::cerr << outline_code.get_source() << std::endl;
                std::cerr << "--END OUTLINE CODE--" << std::endl;

                return outline_code.get_source();
            }

            std::string reference_to_name(Symbol& sym)
            {
                return std::string("&") + sym.get_name();
            }

    };
}

EXPORT_PHASE(TL::OpenMPTransform);
