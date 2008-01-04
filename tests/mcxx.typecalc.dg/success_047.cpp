struct A
{
};

void f()
{
    try
    {
        A a1;
        throw a1;
    }
    catch (A a2)
    {
        throw;
    }
}
