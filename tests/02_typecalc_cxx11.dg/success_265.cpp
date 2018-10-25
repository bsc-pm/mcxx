/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/


namespace std __attribute__((__visibility__("default"))) {

    template < typename _ITp >
        struct  __atomic_base
        {
            private:
                typedef _ITp 	__int_type;
                static constexpr int _S_alignment =
                    sizeof(_ITp) > alignof(_ITp) ? sizeof(_ITp) : alignof(_ITp);
                alignas(_S_alignment) __int_type _M_i;
            public:


                __atomic_base() noexcept = default;
                ~__atomic_base() noexcept = default;
		// We used to emmit the following code:
                //	~__atomic_base() noexcept(true) = default;
		// But it seems that Intel's 2018 FE has some problems with it
                __atomic_base(const __atomic_base&) = delete;
                __atomic_base& operator=(const __atomic_base&) = delete;
                __atomic_base& operator=(const __atomic_base&) volatile = delete;
                inline constexpr __atomic_base(typename ::std::__atomic_base<_ITp>::__int_type __i) noexcept(true)
                    : _M_i(__i)
                {
                }
        };

    template < typename _Tp >
        struct atomic;

    template<>
        struct atomic<int> : __atomic_base<int>
        {
            typedef int 			__integral_type;
            typedef __atomic_base<int> 		__base_type;

            atomic() noexcept = default;
            ~atomic() noexcept = default;
            atomic(const atomic&) = delete;
            atomic& operator=(const atomic&) = delete;
            atomic& operator=(const atomic&) volatile = delete;

            constexpr atomic(__integral_type __i) noexcept : __base_type(__i) { }

            using __base_type::operator=;
        };
}


class Lock
{
    std::atomic <int> state;
    enum { Free = 0, Busy = 1};
    public:
    Lock() : state(Free) { }
};

