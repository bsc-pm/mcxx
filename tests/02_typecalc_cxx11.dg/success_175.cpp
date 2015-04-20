/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template<typename T>
struct A;

template<typename Ret, typename ...T>
struct A<Ret(T...)>
{
    template <typename S>
    A(S&&) { }

    Ret operator()(T...) { return Ret(); }
};

int main(int argc, char** argv)
{
    A<int()> foo(
        [&]()
        {
            int x = foo();
            return x;
        }
        );
}
