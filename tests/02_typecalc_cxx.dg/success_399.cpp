/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <class T>
struct sparsegroup
{
    template <class A>
    struct alloc_impl { T y; }; 

   struct Settings : public alloc_impl<T> { T x; };

   Settings settings;
};

void foo() {
    sparsegroup<int*> c;

    int *p = 0;
    c.settings.x = p;
    c.settings.y = p;
}

