template <typename _T>
struct A
{
    template <typename _Q>
        A(_Q);
};

template <typename _T>
struct B
{
};

template <typename _T>
struct C
{
    void run(const A<_T>&);
};

template <typename _T>
B<_T> adaptor(_T);


void f()
{
    B<int> temp = adaptor(3);

    C<int> c;
    c.run(temp);
}
