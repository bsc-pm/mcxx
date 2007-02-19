#ifndef TL_PRAGMASUPPORT_HPP
#define TL_PRAGMASUPPORT_HPP

#include <string>
#include "tl-compilerphase.hpp"
#include "tl-langconstruct.hpp"
#include "tl-handler.hpp"
#include "tl-traverse.hpp"
#include "cxx-attrnames.h"

namespace TL
{
    class PragmaCustomClause : public LangConstruct
    {
        private:
            std::string _clause_name;

            ObjectList<AST_t> filter_pragma_clause();
        public:
            PragmaCustomClause(const std::string& src, AST_t ref, ScopeLink scope_link)
                : LangConstruct(ref, scope_link), _clause_name(src)
            {
            }

            ObjectList<Expression> get_expression_list();
            ObjectList<IdExpression> id_expressions(IdExpressionCriteria criteria = VALID_SYMBOLS);

            bool is_defined();
    };

    class PragmaCustomConstruct : public LangConstruct
    {
        public:
            PragmaCustomConstruct(AST_t ref, ScopeLink scope_link)
                : LangConstruct(ref, scope_link)
            {
            }

            std::string get_pragma();
            std::string get_directive();

            bool is_directive();
            bool is_construct();

            Statement get_statement();

            PragmaCustomClause get_clause(const std::string& name);
    };

    typedef std::map<std::string, Signal1<PragmaCustomConstruct> > CustomFunctorMap;

    class PragmaCustomDispatcher : public TraverseFunctor
    {
        private:
            std::string _pragma_handled;
            CustomFunctorMap& _pre_map;
            CustomFunctorMap& _post_map;

            void dispatch_pragma_construct(CustomFunctorMap& search_map, Context ctx, AST_t node);
        public:
            PragmaCustomDispatcher(const std::string& pragma_handled, CustomFunctorMap& pre_map,
                    CustomFunctorMap& post_map);

            virtual void preorder(Context ctx, AST_t node);
            virtual void postorder(Context ctx, AST_t node);
    };

    class PragmaCustomCompilerPhase : public CompilerPhase
    {
        private:
            std::string _pragma_handled;
            PragmaCustomDispatcher _pragma_dispatcher;
        public:
            PragmaCustomCompilerPhase(const std::string& pragma_handled);
            virtual void run(DTO& data_flow);

            CustomFunctorMap on_directive_pre;
            CustomFunctorMap on_directive_post;
    };
}

#endif // TL_PRAGMASUPPORT_HPP
