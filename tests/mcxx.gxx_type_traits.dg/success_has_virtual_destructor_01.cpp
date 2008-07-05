
struct true_value { };
struct false_value { };

template <bool _B>
struct check
{
    static true_value f();
};

template <>
struct check<false>
{
    static false_value f();
};

struct A1
{
    int a;
};

struct A2 : A1
{
};

struct A3
{
    ~A3();
};

struct A4 : A3
{
};

struct A5
{
    ~A5();
};

struct A6
{
    A5 a5;
};

struct A7
{
    virtual ~A7();
};

struct A8 : A7
{
};

struct A9 : A8
{
    ~A9();
};

struct A10
{
    A7 a7;
};

int main(int argc, char* argv[])
{
    {
        false_value tr;
        tr = check<__has_virtual_destructor(A1)>::f();
    }

    {
        false_value tr;
        tr = check<__has_virtual_destructor(A2)>::f();
    }

    {
        false_value tr;
        tr = check<__has_virtual_destructor(A3)>::f();
    }

    {
        false_value tr;
        tr = check<__has_virtual_destructor(A4)>::f();
    }

    {
        false_value tr;
        tr = check<__has_virtual_destructor(A5)>::f();
    }

    {
        false_value tr;
        tr = check<__has_virtual_destructor(A6)>::f();
    }

    {
        true_value tr;
        tr = check<__has_virtual_destructor(A7)>::f();
    }

    {
        true_value tr;
        tr = check<__has_virtual_destructor(A8)>::f();
    }

    {
        true_value tr;
        tr = check<__has_virtual_destructor(A9)>::f();
    }

    {
        false_value tr;
        tr = check<__has_virtual_destructor(A10)>::f();
    }
}
