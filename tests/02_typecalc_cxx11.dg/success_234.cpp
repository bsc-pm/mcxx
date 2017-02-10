/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template < typename T>
bool foo(int m, const T& func)
{
    return func(m);
}

int main()
{
    int x = 10, m = 100;

    if (foo(m, [=](int m) { return ((x + m) < 100); } ))
        return 1;

    switch (foo(m, [=](int m) { return ((x + m) < 100); } ))
    {
        case false:
            break;
        default:
            return 1;
    }

    while (foo(m, [=](int m) { return ((x + m) < 100); } ))
        return 1;

    for (; foo(m, [=](int m) { return ((x + m) < 100); }); )
        return 1;

    return 0;
}
