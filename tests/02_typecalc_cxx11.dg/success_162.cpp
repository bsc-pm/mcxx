/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

namespace B
{
    template <typename T>
    bool foo(T) { return sizeof(T) > 4; }
};

namespace C
{
    template <typename T>
    struct D
    {
        static constexpr bool bar()
        {
            using B::foo;
            return foo(T());
        }
    };
};

namespace E
{
    template <typename T>
        struct F
        {
            void f() noexcept(T::bar());
        };

    F< C::D<int> > f;
}
