template <class T>
struct A
{
    struct B : public T
    {
        B() : T() { }
    };
};
