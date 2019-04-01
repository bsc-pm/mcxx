/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
template < typename T >
struct C
{
    static constexpr int eof() { return -1; }
};

template < typename T>
struct D
{
    void bar(T x)
    {
        switch (x)
        {
            case C<char>::eof():
                {}
            default:
                {}
        }
    }
};

int kaka()
{
    D<int> d;
    d.bar(3);
}
