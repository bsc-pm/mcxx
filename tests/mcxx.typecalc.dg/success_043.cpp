template <typename _T>
struct A
{
    typedef _T &reference;
    typedef _T *pointer;

    _T _data;

    reference operator*() 
    {
        return _data;
    }

    pointer operator->() 
    {
        return &(operator*());
    }
};

void f()
{
    A<int> c;

    *c = 3;
}

