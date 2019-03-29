/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

enum class E
{
    VAL1,
    VAL2
};
class C
{
    using M = E;
    bool f()
    {
        return M::VAL1 == M::VAL2;
    }
};

