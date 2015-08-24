/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

struct atomic_flag
{
    atomic_flag(bool b);
    atomic_flag(const atomic_flag&) = delete;
};

struct B
{
    atomic_flag m1 { 0 };
    atomic_flag m2 = { 0 };
};
