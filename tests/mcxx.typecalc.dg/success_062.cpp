namespace C
{
    enum A
    {
        V1, V2, V3
    };

    A operator|(A, A);
}

typedef C::A K;

void f()
{
    K a, b, c;

    a = b | c;
}
