/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
struct A
{
    A() { }
    A(const A&) { }
    A(A&&) { }
    A(int) { }
};

template <typename T>
void f(T t)
{
    t();
}

int main(int argc, char* argv[])
{
    A b(3);

    f([=]() { return b; });

    return 0;
}
