/*
<testinfo>
test_generator="config/mercurium"
</testinfo>
*/
template <bool> struct SparsehashCompileAssert { };

template <typename IntType>
void foo()
{
    typedef SparsehashCompileAssert<(bool(static_cast<IntType>(-1) >
                static_cast<IntType>(0)))>

        serializing_int_requires_an_unsigned_type[bool(static_cast<IntType>(-1) >
                static_cast<IntType>(0)) ? 1 : -1];
}
