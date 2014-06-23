/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
struct atomic_flag
{
    atomic_flag(bool __i);
};

atomic_flag a({1});
