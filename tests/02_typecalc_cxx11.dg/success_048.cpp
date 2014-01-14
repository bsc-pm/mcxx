/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

namespace std
{
    struct Foo
    {
        Foo();
    };

    struct FooCollection
    {
        FooCollection();
    };

    struct FooIt
    {
        FooIt();

        bool operator!=(const FooIt&) const;
        FooIt& operator++();
        Foo& operator*();
    };
    FooIt begin(FooCollection&);
    FooIt end(FooCollection&);

    struct ConstFooIt
    {
        ConstFooIt();

        bool operator!=(const ConstFooIt&) const;
        ConstFooIt& operator++();
        const Foo& operator*();
    };

    ConstFooIt begin(const FooCollection&);
    ConstFooIt end(const FooCollection&);
}

namespace bar
{
    typedef std::FooCollection FooCollection;
    typedef std::Foo Foo;
}

void f()
{
    bar::FooCollection a;

    for (bar::Foo &p : a)
    {
    }

    const bar::FooCollection &k_a = a;

    for (const bar::Foo &p : k_a)
    {
    }
}
