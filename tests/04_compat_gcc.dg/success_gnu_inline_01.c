/*
<testinfo>
test_generator="config/mercurium run"
test_CFLAGS="-std=gnu99"
</testinfo>
*/

extern void fun1(void);
__inline __attribute__((gnu_inline)) void fun1(void)
{
}


__inline __attribute__((gnu_inline)) void fun2(void)
{
}
extern void fun2(void);

extern __inline__ __attribute__((gnu_inline)) void fun3(void);

int main(int argc, char *argv[])
{
    fun1();
    fun2();
    fun3();
    return 0;
}

extern void fun3(void)
{
}
