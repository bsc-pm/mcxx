/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

typedef int delay1;
typedef int delay2;
typedef int delay3;
struct SynIdDelay
{
    unsigned delay1 :2;
    unsigned delay2 :3;
    unsigned delay3 :3, :5;
};

