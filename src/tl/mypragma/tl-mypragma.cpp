#include "tl-mypragma.hpp"
#include "tl-pragmasupport.hpp"

namespace TL
{
    class MyPragmaPhase : public PragmaCustomCompilerPhase
    {
        public:
            MyPragmaPhase()
                : PragmaCustomCompilerPhase("mypragma")
            {
                on_directive_post["test"].connect(functor(&MyPragmaPhase::test_postorder, *this));
            }

            void test_postorder(PragmaCustomConstruct pragma_custom_construct)
            {
                std::cerr << "In " << pragma_custom_construct.get_ast().get_locus() 
                    << " there is the following \"test\" construct" << std::endl;
                std::cerr << pragma_custom_construct.prettyprint() << std::endl;
                std::cerr << std::endl;

                PragmaCustomClause clause = pragma_custom_construct.get_clause("clause");

                if (clause.is_defined())
                {
                    ObjectList<Expression> expressions = clause.get_expression_list();
                    std::cerr << "List of expressions:" << std::endl;

                    for(ObjectList<Expression>::iterator it = expressions.begin();
                            it != expressions.end();
                            it++)
                    {
                        std::cerr << "'" << it->prettyprint() << "'" << std::endl;
                    }
                }
            }
    };
}

EXPORT_PHASE(TL::MyPragmaPhase);
