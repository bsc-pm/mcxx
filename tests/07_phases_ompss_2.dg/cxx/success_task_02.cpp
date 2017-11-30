/*
<testinfo>
test_generator="config/mercurium-ompss-2"
</testinfo>
*/


namespace test1
{
    struct A
    {
        void foo()
        {
            #pragma oss task
            {}
        }
    };
}

namespace test2
{
    template < typename T>
    struct B
    {
        void foo()
        {
            #pragma oss task
            {}
        }
    };
}

namespace test3
{
    struct C
    {
        template < typename T>
            struct D
            {
                void foo()
                {
                    #pragma oss task
                    {}
                }
            };
    };
}

namespace test4
{
    template < typename T>
    struct E
    {
        template < typename T2>
            void foo(T*q, T2*p)
            {
                #pragma oss task
                {}
            }
    };
}


namespace test5
{
    template < typename T>
    struct F
    {
        struct G
        {
            template < typename T2>
            void foo(T*q, T2*p)
            {
                #pragma oss task
                {}
            }
        };
    };
}

int main()
{
   // test1
   {
       test1::A a;
       a.foo();
   }

    // test2
    {
        test2::B<int> b;
        b.foo();
    }

    // test3
    {
        test3::C::D<int> d;
        d.foo();
    }

    //test4
    {
        test4::E<int> e;
        e.foo((int*)0, (float*)0);
    }

    //test5
    {
        test5::F<int>::G g;
        g.foo((int*)0, (float*)0);
    }
}
