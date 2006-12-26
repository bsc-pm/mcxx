struct A
{
    typedef int S;
};

template <class T>
struct B : public A
{
};

struct D : 
    public B<float>
{
    S s;
};

// template <class T>
// struct C : 
//     public B<T>
// {
// };
// 
// template <>
// struct C<int> : public B<int>
// {
//     S s;
// };
