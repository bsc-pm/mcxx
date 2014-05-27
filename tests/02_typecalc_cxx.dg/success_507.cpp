/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename T>
struct A
{
    static void g(float *);
    static void g(double *);

    void f()
    {
        void (*pg)(void*);
        pg = (void (*)(void*))g;
        pg = g;
    }
};

template <typename T>
struct B
{
    static void g(B *);
    static void g(double *);

    void f()
    {
        void (*pg)(void*);
        pg = (void (*)(void*))g;
        pg = g;
    }
};
