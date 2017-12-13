/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/
class A0
{
    private:
        int _data;
        int *_p_x;

    public:

        constexpr A0() : _data(0), _p_x(&_data) {}
};

class A1
{
    private:
        int _data = 0;
        int *_p_x = &_data;
};

class B
{
    private:
        int _data;
        int *_p_x;
        constexpr B(int* p_x) : _data(0), _p_x(p_x) {}

    public:
        constexpr B() : B(&_data) {}
};

int main()
{
    A0 a0;
    A1 a1;
    B b;
};
