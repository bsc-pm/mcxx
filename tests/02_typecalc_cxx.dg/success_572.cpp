/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template<class T>
struct Accumulator { };

template<typename _Tp, typename _AccTp>
_AccTp normL2Sqr(const _Tp* a, int n);

template<typename _Tp, typename _AccTp>
_AccTp normL2Sqr(const _Tp* a, const _Tp* b, int n);

extern double sqrt(double x);

template<class T>
struct L2
{
    enum { normType = 3 };
    typedef T ValueType;
    typedef typename Accumulator<T>::Type ResultType;

    ResultType operator()( const T* a, const T* b, int size ) const
    {
        return (ResultType)sqrt((double)normL2Sqr<ValueType, ResultType>(a, b, size));
    }
};

