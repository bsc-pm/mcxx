/*
<testinfo>
test_generator="config/mercurium"
</testinfo>
*/

namespace std
{
    typedef unsigned long size_t;
}

template<typename _Tp, _Tp __v>
struct integral_constant { };

template<typename _Tp>
struct alignment_of : public integral_constant<std::size_t, __alignof__(_Tp)>
{
};
