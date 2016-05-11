/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

enum ENUM1
{
    INVALID = -1
};

enum ENUM2
{
    A = 0
};

int foo()
{
    ENUM2 a = static_cast<ENUM2>(INVALID);
    ENUM2 b = (ENUM2) INVALID;
}
