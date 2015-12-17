/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template<typename _ITp>
struct __atomic_base
{
    private:
        typedef _ITp __int_type;

        static constexpr int _S_alignment =
            sizeof(_ITp) > alignof(_ITp) ? sizeof(_ITp) : alignof(_ITp);

        alignas(_S_alignment) __int_type _M_i;
};
