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
            // This syntax is only allowed in g++ 4.7
#if  defined(__GNUC__) && (__GNUC__ > 4 || (__GNUC__ == 4) && (__GNUC_MINOR__ >= 7))
            .outline =
#endif
                (void(*)(void*)) (void(*)(::A<T1, T2>::X&)) &::A<T1, T2>::bar
        };
    }
};
