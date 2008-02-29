struct A
{
    int k();
};

A&& f();

void g()
{
    int m;

    m = f().k();
}
