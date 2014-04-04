/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

namespace foo
{
    struct object
    {
        void bar();
    };

    struct another_object
    {
        void quux();
    };

    namespace detail1
    {
        struct A
        {
            operator object();
        };

        struct B : A
        {
            operator another_object();
        };
    }

    namespace detail2
    {
        struct A
        {
        };

        struct B : A
        {
            operator object();
            operator another_object();
        };
    }

    namespace detail3
    {
        struct A
        {
            operator object();
            operator another_object();
        };

        struct B : A
        {
        };
    }

    void foo1(detail1::B& b)
    {
        b.operator object().bar();
        b.operator another_object().quux();
    }

    void foo2(detail2::B& b)
    {
        b.operator object().bar();
        b.operator another_object().quux();
    }

    void foo3(detail3::B& b)
    {
        b.operator object().bar();
        b.operator another_object().quux();
    }
}
