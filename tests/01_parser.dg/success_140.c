/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

typedef unsigned long size_t;

const char* __string2_1bptr_p(const char*);
int __strcmp_cg(const char*, const char*, int);
int __strcmp_gc(const char*, const char*, int);


#define strcmp(s1,s2) __extension__ ({ size_t __s1_len, __s2_len; (__builtin_constant_p (s1) && __builtin_constant_p (s2) && (__s1_len = __builtin_strlen (s1), __s2_len = __builtin_strlen (s2), (!__string2_1bptr_p (s1) || __s1_len >= 4) && (!__string2_1bptr_p (s2) || __s2_len >= 4)) ? __builtin_strcmp (s1, s2) : (__builtin_constant_p (s1) && __string2_1bptr_p (s1) && (__s1_len = __builtin_strlen (s1), __s1_len < 4) ? (__builtin_constant_p (s2) && __string2_1bptr_p (s2) ? __builtin_strcmp (s1, s2) : __strcmp_cg (s1, s2, __s1_len)) : (__builtin_constant_p (s2) && __string2_1bptr_p (s2) && (__s2_len = __builtin_strlen (s2), __s2_len < 4) ? (__builtin_constant_p (s1) && __string2_1bptr_p (s1) ? __builtin_strcmp (s1, s2) : __strcmp_gc (s1, s2, __s2_len)) : __builtin_strcmp (s1, s2)))); })

int f(const char* a, const char *b)
{
    if ( strcmp(a, b) )
    {
        a = (char*)0;
        b = (char*)0;
    }
}
