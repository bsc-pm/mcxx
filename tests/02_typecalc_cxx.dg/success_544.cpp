/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
void foo(int const & comm) {}

struct communicator
{
    operator int() const;

    void bar()
    {
       foo(this->operator int());
       foo(*this);
    }
};
