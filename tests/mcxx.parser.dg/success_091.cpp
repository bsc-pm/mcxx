struct A
{
    ~A()
    {
        f();
    }

    void f()
    {
    }
};
