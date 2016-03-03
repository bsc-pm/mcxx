/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

typedef __attribute__((vector_size(16))) float Packet4f;

template <typename Packet>
inline Packet pnegate1(const Packet &a)
{
    return -a;
}

template <>
inline Packet4f pnegate1(const Packet4f &a)
{
    return a;
}

template <typename Packet>
inline Packet pnegate2(Packet &a)
{
    return -a;
}
template <>
inline const Packet4f pnegate2(const Packet4f &a)
{
    return a;
}

template <typename Packet>
inline Packet pnegate3(const Packet &a)
{
    return -a;
}
template <>
inline Packet4f pnegate3(const Packet4f &a)
{
    return a;
}
