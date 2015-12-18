/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct B
{
    template <typename T, typename S = T*>
        struct A1;
    template <typename T, typename S = T*>
        struct A2 { int x; };
};

template <typename T, typename S>
struct B::A1
{
    int x;
};


B::A1<int> a1;
B::A2<int> a2;

