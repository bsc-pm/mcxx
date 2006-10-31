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
//    class IdExpressionPred : public Predicate
//    {
//        public:
//            virtual bool operator()(const AST_t& ast) const
//            {
//                TL::Object* attr = ast.get_attribute(LANG_IS_ID_EXPRESSION);
//
//                return (attr != NULL
//                        && attr->is_bool()
//                        && ((bool)(*attr)));
//            }
//    };
//
//    class PragmaParallelConstruct : public Predicate
//    {
//        public:
//            virtual bool operator()(const AST_t& ast) const
//            {
//                TL::Object* attr = ast.get_attribute(OMP_IS_PARALLEL_CONSTRUCT);
//
//                return (attr != NULL
//                        && attr->is_bool()
//                        && ((bool)(*attr)));
//            }
//    };

    void OpenMPTransform::run(DTO& dto)
    {
//        TL::AST_t* translation_unit = dynamic_cast<TL::AST_t*>(dto["translation_unit"]);
//        TL::ScopeLink* scope_link = dynamic_cast<TL::ScopeLink*>(dto["scope_link"]);
//        TL::Scope* global_scope = scope_link->get_scope(translation_unit);
//
//        PragmaParallelConstruct parallel_predicate;
//        TL::AST_list_t parallel_construct_list = translation_unit->get_all_subtrees_predicate(parallel_predicate);
//
//		IdExpressionPred id_expression_predicate;
//
//		TL::AST_list_t::iterator it;
//		for (it = parallel_construct_list.begin();
//				it != parallel_construct_list.end();
//				it++)
//		{
//			TL::AST_t* enclosing_function_def = (*it)->get_enclosing_function_definition();
//			TL::AST_t* function_name_tree = dynamic_cast<TL::AST_t*>(enclosing_function_def->get_attribute(LANG_FUNCTION_NAME));
//			std::string function_name_str = function_name_tree->prettyprint();
//
//			TL::AST_t* parallel_body = dynamic_cast<TL::AST_t*>((*it)->get_attribute(OMP_CONSTRUCT_BODY));
//
//			TL::Source outlined_function;
//			outlined_function << "void outline_" << function_name_str << "(";
//
//			TL::AST_set_t references = parallel_body->get_all_subtrees_predicate(id_expression_predicate);
//			TL::AST_set_t::iterator it2;
//			for (it2 = references.begin();
//					it2 != references.end();
//					it2++)
//			{
//				if (it2 != references.begin())
//				{
//					outlined_function << ", ";
//				}
//
//				TL::Scope* st = scope_link->get_scope(*it2);
//
//				TL::Symbol* sym = st->get_symbol_from_id_expr(*it2);
//
//				TL::Type* pointer_to = sym->get_type()->get_pointer_to();
//
//				outlined_function << pointer_to->get_parameter_declaration_str(sym->get_name());
//			}
//
//			outlined_function << ")"
//				<< "{"
//				<< parallel_body->prettyprint()
//			    << "}";
//
//			TL::AST_t* outlined_tree = outlined_function.parse_global(global_scope, scope_link);
//			translation_unit->prepend_to_translation_unit(outlined_tree);
//		}
    }
}
