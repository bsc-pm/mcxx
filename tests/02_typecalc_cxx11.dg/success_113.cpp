/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename T>
struct Check { };

template <typename T>
Check<T&&> f1(T&&);

template <typename T>
Check<T> f2(T&);

template <typename T>
Check<T> f3(T);

void m(int, float);

struct A
{
    void g()
    {

        {
            typedef decltype(f1(&A::h)) T1;
            typedef Check< void (A::*&&)(int, float) > T1;
            // f2(&A::h); // Invalid since &A::h is always a rvalue
            typedef decltype(f3(&A::h)) T3;
            typedef Check< void (A::*)(int, float) > T3;
        }
        {
            typedef decltype(f1(m)) T1;
            typedef Check<void (&)(int, float)> T1;

            typedef decltype(f2(m)) T2;
            typedef Check<void (int, float)> T2;

            typedef decltype(f3(m)) T3;
            typedef Check<void (*)(int, float)> T3;
        }
    }
    void h(int, float);
};

void m()
{
    A a;
    a.g();
}
