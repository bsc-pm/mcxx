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
    class ParallelConstructPred : public Predicate
    {
        public:
            virtual bool operator()(const AST_t& ast) const
            {
                TL::Object* attr = ast.get_attribute(OMP_IS_PARALLEL_CONSTRUCT);

                return (attr != NULL
                        && attr->is_bool()
                        && ((bool)(*attr)));
            }
    };

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

        Source s; 
        s   << "int prova_fun() { int k; k = 3; }";

        TL::AST_t* new_fun_ast = s.parse_global(global_scope, scope_link);
        translation_unit->append_to_translation_unit(new_fun_ast);
    }
}
