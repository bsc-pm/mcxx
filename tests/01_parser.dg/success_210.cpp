/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct Dictionary;

template <typename T>
struct Data
{
   T t;
   int x;
   Data(T t_) : t(t_) {};
};

void foo ()
{
   Data<Dictionary> d(Dictionary());
}
