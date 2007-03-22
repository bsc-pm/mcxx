#include "tl-mypragma.hpp"
#include "tl-pragmasupport.hpp"

#include <vector>
#include <stack>

namespace TL
{
    struct TestAInfo
    {
        PragmaCustomConstruct construct;
        bool immediate;

        TestAInfo(PragmaCustomConstruct pragma_construct)
            : construct(pragma_construct), immediate(false)
        {
        }
    };

    class TestBInfo
    {
        public:
            PragmaCustomConstruct construct;
            ObjectList<TestAInfo> testa_list;

            TestBInfo(PragmaCustomConstruct pragma_construct)
                 : construct(pragma_construct)
            {
            }
    };

    class MyPragmaPhase : public PragmaCustomCompilerPhase
    {
        private:
            std::stack<TestBInfo> testb_info;
        public:
            MyPragmaPhase()
                : PragmaCustomCompilerPhase("mypragma")
            {
                on_directive_pre["testA"].connect(functor(&MyPragmaPhase::testA_preorder, *this));
                on_directive_post["testA"].connect(functor(&MyPragmaPhase::testA_postorder, *this));

                on_directive_pre["testB"].connect(functor(&MyPragmaPhase::testB_preorder, *this));
                on_directive_post["testB"].connect(functor(&MyPragmaPhase::testB_postorder, *this));
            }

            void testB_preorder(PragmaCustomConstruct pragma_custom_construct)
            {
                TestBInfo testb(pragma_custom_construct);

                Statement st = pragma_custom_construct.get_statement();

                testb_info.push(testb);
            }

            void testA_preorder(PragmaCustomConstruct pragma_custom_construct)
            {
                std::cerr << "found testA --> " << pragma_custom_construct.get_ast().get_locus() << std::endl;
                TestBInfo& testb = testb_info.top();

                TestAInfo testa(pragma_custom_construct);

                check_level(testb.construct.get_statement(), testa);

                testb.testa_list.append(testa);
            }
            
            void check_level(Statement st, TestAInfo& testa)
            {
                if (!st.is_compound_statement())
                {
                    if (st.get_ast() == testa.construct.get_ast())
                    {
                        testa.immediate = true;
                        std::cerr << "immediate --> " << testa.construct.get_ast().get_locus() << std::endl;
                    }
                }
                else 
                {
                    ObjectList<Statement> inner_statements = st.get_inner_statements();
                    for (ObjectList<Statement>::iterator it = inner_statements.begin();
                            it != inner_statements.end();
                            it++)
                    {
                        check_level(*it, testa);
                    }
                }
            }

            void testA_postorder(PragmaCustomConstruct pragma_custom_construct)
            {
                Source src;

                src
                    << "printf(\"Hello world at " 
                    << pragma_custom_construct.get_ast().get_locus() 
                    << "\");"
                    ;

                AST_t replace_tree = src.parse_statement(pragma_custom_construct.get_ast(),
                        pragma_custom_construct.get_scope_link());

                pragma_custom_construct.get_ast().replace(replace_tree);
            }

            void testB_postorder(PragmaCustomConstruct pragma_custom_construct)
            {
                testb_info.pop();

                // pragma_custom_construct.get_ast().replace(pragma_custom_construct.get_statement().get_ast());
            }
    };
}

EXPORT_PHASE(TL::MyPragmaPhase);
