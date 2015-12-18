/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

namespace std {
    typedef decltype(sizeof(0)) size_t;
}

template<std::size_t Len, std::size_t Align>
struct aligned_storage_impl;
template<std::size_t Len>struct alignas(0x1) aligned_storage_impl<Len, 0x1>{ char dummy[Len]; typedef aligned_storage_impl<Len, 0x1> type;};
template<std::size_t Len>struct alignas(0x2) aligned_storage_impl<Len, 0x2>{ char dummy[Len]; typedef aligned_storage_impl<Len, 0x2> type;};
template<std::size_t Len>struct alignas(0x4) aligned_storage_impl<Len, 0x4>{ char dummy[Len]; typedef aligned_storage_impl<Len, 0x4> type;};
template<std::size_t Len>struct alignas(0x8) aligned_storage_impl<Len, 0x8>{ char dummy[Len]; typedef aligned_storage_impl<Len, 0x8> type;};
template<std::size_t Len>struct alignas(0x10) aligned_storage_impl<Len, 0x10>{ char dummy[Len]; typedef aligned_storage_impl<Len, 0x10> type;};
template<std::size_t Len>struct alignas(0x20) aligned_storage_impl<Len, 0x20>{ char dummy[Len]; typedef aligned_storage_impl<Len, 0x20> type;};
template<std::size_t Len>struct alignas(0x40) aligned_storage_impl<Len, 0x40>{ char dummy[Len]; typedef aligned_storage_impl<Len, 0x40> type;};
template<std::size_t Len>struct alignas(0x80) aligned_storage_impl<Len, 0x80>{ char dummy[Len]; typedef aligned_storage_impl<Len, 0x80> type;};
template<std::size_t Len>struct alignas(0x100) aligned_storage_impl<Len, 0x100>{ char dummy[Len]; typedef aligned_storage_impl<Len, 0x100> type;};
template<std::size_t Len>struct alignas(0x200) aligned_storage_impl<Len, 0x200>{ char dummy[Len]; typedef aligned_storage_impl<Len, 0x200> type;};
template<std::size_t Len>struct alignas(0x400) aligned_storage_impl<Len, 0x400>{ char dummy[Len]; typedef aligned_storage_impl<Len, 0x400> type;};
template<std::size_t Len>struct alignas(0x800) aligned_storage_impl<Len, 0x800>{ char dummy[Len]; typedef aligned_storage_impl<Len, 0x800> type;};
template<std::size_t Len>struct alignas(0x1000) aligned_storage_impl<Len, 0x1000>{ char dummy[Len]; typedef aligned_storage_impl<Len, 0x1000> type;};

