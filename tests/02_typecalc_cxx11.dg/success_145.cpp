/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename T>
void f(T t)
{
}

struct A
{
    void g()
    {
        A*&& a = this;
    }
};
