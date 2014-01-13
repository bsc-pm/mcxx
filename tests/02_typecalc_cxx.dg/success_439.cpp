/*
<testinfo>
test_generator="config/mercurium"
</testinfo>
*/
template < typename T1, typename T2>
struct A
{
    struct X
    {
        void (*outline)(void *);
    };

    void bar(X& var) {}
    void foo()
    {
        static X var = {
            .outline = (void(*)(void*)) (void(*)(::A<T1, T2>::X&)) &::A<T1, T2>::bar
        };
    }
};
