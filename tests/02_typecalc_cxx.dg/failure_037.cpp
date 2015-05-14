/*
<testinfo>
test_generator=config/mercurium-fe-only
test_compile_fail=yes
</testinfo>
*/

struct A
{
        static void foo(float);
        void foo(double);
        void foo(int);
        static void foo(long double);
};

void g(A* a)
{
    double n;
    A::foo(n);
}
