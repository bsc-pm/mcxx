/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
struct A
{
    typedef int size_type;
    size_type s;
};

template<typename T = A>
auto foo(T a) -> typename decltype(a)::size_type
{
    return 2* a.s + 1;
}

int main(int argc, char **argv)
{
    A a;
    foo(a);
    return 0;
}
