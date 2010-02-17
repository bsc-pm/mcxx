namespace A
{
    struct B
    {
    };

    namespace C
    {
        struct D
        {
            operator B();
        };
    }
}

using namespace A::C;

D::operator B()
{
    return B();
}
