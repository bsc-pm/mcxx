
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
    float b;
};

struct A2
{
    A1 a;
};

struct A3
{
    A3() { }
};

struct A4
{
    ~A4() { }
};

struct A5
{
    A5(const A5&) { }
};

struct A6
{
    A6& operator=(const A6&) { }
};

struct A7
{
    void f() { }
};

struct A8
{
    A6 a6;
};

int main(int argc, char* argv[])
{
    {
        true_value tr;
        tr = check<__is_pod(A1)>::f();
    }

    {
        true_value tr;
        tr = check<__is_pod(A2)>::f();
    }

    {
        false_value tr;
        tr = check<__is_pod(A3)>::f();
    }

    {
        false_value tr;
        tr = check<__is_pod(A4)>::f();
    }

    {
        false_value tr;
        tr = check<__is_pod(A5)>::f();
    }

    {
        false_value tr;
        tr = check<__is_pod(A6)>::f();
    }

    {
        true_value tr;
        tr = check<__is_pod(A7)>::f();
    }

    {
        false_value tr;
        tr = check<__is_pod(A8)>::f();
    }
}
