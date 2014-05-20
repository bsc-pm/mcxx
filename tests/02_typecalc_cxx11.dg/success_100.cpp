/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

#ifdef __GNUC__

__complex__ float a {1.0f, 2.0f};
__complex__ double b {1.0f, 2.0f};

struct B
{
    __complex__ float a {1.0f, 2.0f};
    __complex__ double b {1.0f, 2.0f};
    __complex__ float c;
    __complex__ double d;
    B()
        : c{1.0f, 2.0f}, d{1.0, 2.0}
    {
    }
};

#endif
