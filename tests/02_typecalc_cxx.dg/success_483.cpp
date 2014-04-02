/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T1, typename T2>
struct Pair { };

template <typename A, typename D>
Pair<A, D> bar(A const&);

template <typename A, typename B, typename C, typename D>
void quux( A a , C , D , B b , char const* s)
{
    // A wrong ambiguity routine was removing 1, 2, 3, 4, 5
    // from the expression list
    a.foo(1, 2, 3, 4, 5, bar<C, D>(b) , s);
}

struct MyClass
{
    void foo(int, int, int, int, int, const Pair<double, float>&, const char*);
};

void moo()
{
    MyClass m;

    quux(m, double(0), float(0), double(0), "a");
}
