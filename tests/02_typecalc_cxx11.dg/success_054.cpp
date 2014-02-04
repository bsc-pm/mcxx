/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

typedef unsigned long size_t;

extern long int strtol (const char *__restrict __nptr,
   char **__restrict __endptr, int __base)
     throw () __attribute__ ((__nonnull__ (1)));

template<typename _TRet, typename _Ret = _TRet, typename _CharT,
    typename... _Base>
    _Ret
    __stoa(_TRet (*__convf) (const _CharT*, _CharT**, _Base...),
            const char* __name, const _CharT* __str, size_t* __idx,
            _Base... __base);

struct ministring
{
    const char* c_str() const;
};

inline int stoi(const ministring& __str, size_t* __idx = 0, int __base = 10)
{ 
    return __stoa<long, int>(strtol, "stoi", __str.c_str(),
            __idx, __base);
}

