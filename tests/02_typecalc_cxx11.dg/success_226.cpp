/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
namespace N
{
    struct A
    {
        typedef int MyInt;
        static MyInt _x;
    };
}

template<typename T>
void foo(T a)
{
    sizeof(typename N::decltype(a)::MyInt);
    N::decltype(a)::_x++;
    decltype(a)::_x++;
}

void foo2(N::A a)
{
    sizeof(typename N::decltype(a)::MyInt);
    N::decltype(a)::_x++;
    decltype(a)::_x++;
}

int main()
{
    N::A a;
    foo(a);
    foo2(a);
}
