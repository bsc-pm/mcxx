enum A
{
    V1, V2, V3
};

A operator|(A, A);

void f()
{
    A a, b, c;

    a = b | c;
}
