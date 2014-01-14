/*
<testinfo>
test_generator="config/mercurium-cxx11 run"
</testinfo>
*/

bool f(int (&c)[3])
{
    for ( auto &p : c )
    {
        p = p + 1;
    }

    if (c[0] != 1
            || c[1] != 2
            || c[2] != 3)
        return false;

    for ( auto p : c )
    {
        p = 3;
    }

    if (c[0] != 1
            || c[1] != 2
            || c[2] != 3)
        return false;

    for ( auto &&p : c )
    {
        p = p + 1;
    }

    if (c[0] != 2
            || c[1] != 3
            || c[2] != 4)
        return false;

    return true;
}

int main(int, char**)
{
    int c[3];
    c[0] = 0;
    c[1] = 1;
    c[2] = 2;

    return f(c) ? 0 : 1;
}
