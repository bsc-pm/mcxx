#include "tl-omp.hpp"
#include "tl-builtin.hpp"
#include "tl-ast.hpp"
#include "tl-source.hpp"
#include "tl-scopelink.hpp"
#include "cxx-attrnames.h"
#include <iostream>
#include <set>

extern "C" 
{
    TL::CompilerPhase* give_compiler_phase_object(void)
    {
        return new TL::OpenMPTransform();
    }
}

namespace TL
{
    class IdExpressionPred : public Predicate
    {
        public:
            virtual bool operator()(const AST_t& ast) const
            {
                TL::Object* attr = ast.get_attribute(LANG_IS_ID_EXPRESSION);

                return (attr != NULL
                        && attr->is_bool()
                        && ((bool)(*attr)));
            }
    };

    void OpenMPTransform::run(DTO& dto)
    {
        TL::AST_t* translation_unit = dynamic_cast<TL::AST_t*>(dto["translation_unit"]);
        TL::ScopeLink* scope_link = dynamic_cast<TL::ScopeLink*>(dto["scope_link"]);
        TL::Scope* global_scope = scope_link->get_scope(translation_unit);

        IdExpressionPred id_expression_pred;
        TL::AST_list_t id_expressions_list = translation_unit->get_all_subtrees_predicate(id_expression_pred);

        // Get the symbol
        TL::AST_list_t::iterator it;
        for (it = id_expressions_list.begin();
                it != id_expressions_list.end();
                it++)
        {
            TL::Scope* st = scope_link->get_scope(*it);
            if (st == NULL)
                continue;

            TL::Symbol* sym = st->get_symbol_from_id_expr(*it);
            if (sym == NULL)
                continue;

            TL::Type* type = sym->get_type();

			std::string decl = type->get_simple_declaration_str("prova_" + sym->get_name());

			TL::Source src;
			src << decl;

			TL::AST_t* decl_ast = src.parse_global(global_scope, scope_link);

			translation_unit->append_to_translation_unit(decl_ast);
        }
    }
}
