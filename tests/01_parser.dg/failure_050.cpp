/*
<testinfo>
test_generator=config/mercurium-fe-only
test_CFLAGS="-y"
test_CXXFLAGS="-y"
test_compile_fail=yes
</testinfo>
*/

struct A
{
    static wrong();
};

int main()
{
    A::wrong();
}

