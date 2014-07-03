/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

struct once_flag { int x; };
template<typename _Callable, typename... _Args>
void call_once(once_flag& __once, _Callable&& __f, _Args&&... __args);

struct A
{
    once_flag _M_once;
    void f()
    {
        call_once(_M_once, &A::g, this, 3, 4.f);
    }

    void g(int, float);
};

void g()
{
    A a;
    a.f();
}
