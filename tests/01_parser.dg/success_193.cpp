/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

typedef __typeof__(sizeof(0)) my_size_t;

template<typename _Tp, my_size_t fixed_size=4096/sizeof(_Tp)+8> class A
{
public:
    enum { E = (int)((16 + sizeof(_Tp) - 1)/sizeof(_Tp)) };
};

