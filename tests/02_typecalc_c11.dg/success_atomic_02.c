/*
<testinfo>
test_generator="config/mercurium-c11"
</testinfo>
*/

#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 9)
typedef _Atomic struct
{
  _Bool __val;
} my_atomic_flag;
#endif
