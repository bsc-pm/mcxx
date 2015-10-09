/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename T, typename S>
struct A
{
    static void g(int);
};

struct C
{
    void (*fun1)(int);
    bool b;
};

typedef int D;
typedef int E;

void bar()
{
    static const C c = {
        A<D, E>::g,
        false
    };
}
