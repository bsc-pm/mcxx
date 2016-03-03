/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <int Mode_>
struct A
{
    enum M { Mode = Mode_ };

    void foo(M = Mode);
};

template <int Mode>
// Note that after :: Mode is hidden by A::Mode
void A<Mode>::foo(M m1)
{
}
