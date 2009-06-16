#include "tl-pretransform.hpp"
#include "tl-langconstruct.hpp"

namespace TL
{
    namespace Nanos4
    {
        OpenMP_PreTransform::OpenMP_PreTransform()
            : _function_num(0)
        {
            on_threadprivate_pre.connect(functor(&OpenMP_PreTransform::handle_threadprivate, *this));
        }

        void OpenMP_PreTransform::init(DTO& dto)
        {
            _scope_link = ScopeLink(dto["scope_link"]);

            // Silence the compiler about unused clauses
            disable_clause_warnings(true);
        }

        void OpenMP_PreTransform::remove_symbol_declaration(Symbol sym)
        {
            AST_t point_of_decl = sym.get_point_of_declaration();
            Declaration decl(point_of_decl, _scope_link);

            ObjectList<DeclaredEntity> declared_entities = decl.get_declared_entities();

            // If this is the only thing declared here, just remove 
            if (declared_entities.size() == 1)
            {
                point_of_decl.remove_in_list();
                return;
            }

            // Otherwise remove it from the list of declared entities
            for (ObjectList<DeclaredEntity>::iterator it = declared_entities.begin();
                    it != declared_entities.end();
                    it++)
            {
                DeclaredEntity& decl_entity(*it);
                Symbol current_symbol = decl_entity.get_declared_symbol();

                if (current_symbol == sym)
                {
                    // Do nothing else
                    decl_entity.get_ast().remove_in_list();
                    return;
                }
            }

            // If not found nothing happens, maybe we should fail
        }

        void OpenMP_PreTransform::handle_threadprivate(OpenMP::ThreadPrivateDirective threadprivate_directive)
        {
            // Get the threadprivate directive
            OpenMP::Directive directive = threadprivate_directive.directive();

            // And get its parameter clause (you can see the (...) as a
            // clause without name, we'll call it "parameter_clause")
            OpenMP::Clause clause = directive.parameter_clause();

            // Now get the list of symbols of this clause
            ObjectList<IdExpression> threadprivate_references = clause.id_expressions();

            // For every symbol in the clause
            int num_elems = threadprivate_references.size();
            for (ObjectList<IdExpression>::iterator it = threadprivate_references.begin();
                    it != threadprivate_references.end();
                    it++)
            {
                Symbol symbol = it->get_symbol();

                // We are in a FunctionDefinition
                if (symbol.has_block_scope())
                {
                    FunctionDefinition enclosing_function = directive.get_enclosing_function();

                    Symbol function_sym = enclosing_function.get_function_symbol();

                    add_local_symbol(_function_sym_list, function_sym, symbol);
                    it->get_ast().remove_in_list();
                    num_elems--;
                }
            }

            if (num_elems == 0)
            {
                threadprivate_directive.get_ast().remove_in_list();
            }
        }

        void OpenMP_PreTransform::add_local_symbol(function_sym_list_t& sym_list, Symbol function_sym, Symbol local)
        {
            bool found = false;
            for (function_sym_list_t::iterator it = sym_list.begin();
                    it != sym_list.end() && !found;
                    it++)
            {
                if (it->first == function_sym)
                {
                    it->second.insert(local);
                    found = true;
                }
            }

            if (!found)
            {
                ObjectList<Symbol> singleton;
                singleton.append(local);
                sym_list.append(function_symbols_pair_t(function_sym, singleton));
            }
        }

        ObjectList<Symbol> OpenMP_PreTransform::get_all_functions(const function_sym_list_t& sym_list)
        {
            ObjectList<Symbol> result;
            for (function_sym_list_t::const_iterator it = sym_list.begin();
                    it != sym_list.end();
                    it++)
            {
                result.insert(it->first);
            }

            return result;
        }

        ObjectList<Symbol> OpenMP_PreTransform::get_symbols_of_function(const function_sym_list_t& sym_list,
                Symbol function_sym)
        {
            ObjectList<Symbol> result;

            for (function_sym_list_t::const_iterator it = sym_list.begin();
                    it != sym_list.end();
                    it++)
            {
                if (it->first == function_sym)
                {
                    result.insert(it->second);
                }
            }

            return result;
        }

        void OpenMP_PreTransform::purge_local_threadprivates()
        {
            ObjectList<Symbol> involved_functions = get_all_functions(_function_sym_list);

            for (ObjectList<Symbol>::iterator it = involved_functions.begin();
                    it != involved_functions.end();
                    it++)
            {
                Symbol &funct_sym(*it);
                AST_t funct_point_of_decl = funct_sym.get_point_of_declaration();

                ObjectList<Symbol> local_syms = get_symbols_of_function(_function_sym_list, funct_sym);

                Source pragma_line, threadprivate_args;
                pragma_line
                    << "#pragma omp threadprivate("
                    << threadprivate_args
                    ;

                TL::ReplaceIdExpression replacement;

                for (ObjectList<Symbol>::iterator localsym_it = local_syms.begin();
                        localsym_it != local_syms.end();
                        localsym_it++)
                {
                    Symbol &local_sym(*localsym_it);

                    Type type = local_sym.get_type();

                    Source global_var_name;
                    global_var_name
                        <<  "__" << funct_sym.get_name() << "_" << _function_num << "_" << local_sym.get_name();

                    replacement.add_replacement(local_sym, global_var_name);

                    threadprivate_args.append_with_separator(global_var_name, ",");

                    // FIXME - In C++ we have to be careful where we put this new declaration
                    // Templates will mess things -> Scope must be judiciously chosen
                    Source global_decl_src;

                    global_decl_src
                        << "static "
                        << type.get_declaration(local_sym.get_scope(), global_var_name)
                        << ";"
                        ;

                    // FIXME - C++!
                    AST_t global_decl_tree = global_decl_src.parse_global(
                            funct_point_of_decl, _scope_link);

                    // 1. Declare globally just before the function
                    funct_point_of_decl.prepend(global_decl_tree);
                }

                pragma_line << ")\n";

                // 2. Add the pragma line with all local symbols
                AST_t pragma_line_tree = pragma_line.parse_global(
                        funct_point_of_decl, _scope_link);

                funct_point_of_decl.prepend(pragma_line_tree);

                for (ObjectList<Symbol>::iterator localsym_it = local_syms.begin();
                        localsym_it != local_syms.end();
                        localsym_it++)
                {
                    Symbol &local_sym(*localsym_it);

                    remove_symbol_declaration(local_sym);
                }

                FunctionDefinition funct_def(funct_point_of_decl,
                        _scope_link);
                Statement function_body = funct_def.get_function_body();
                // 4. Change old references to the new ones
                Statement replaced_function_body = replacement.replace(function_body);
                function_body.get_ast().replace(replaced_function_body.get_ast());

                // Next function number
                _function_num++;
            }
        }
    }
}
