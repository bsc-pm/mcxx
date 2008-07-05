
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
    A3(const A3&);
};

struct A4 : A3
{
};

struct A5
{
    A5();
    A5(const A5&);
};

struct A6
{
    A5 a5;
};

int main(int argc, char* argv[])
{
    {
        true_value tr;
        tr = check<__has_trivial_copy(A1)>::f();
    }

    {
        true_value tr;
        tr = check<__has_trivial_copy(A2)>::f();
    }

    {
        false_value tr;
        tr = check<__has_trivial_copy(A3)>::f();
    }

    {
        false_value tr;
        tr = check<__has_trivial_copy(A4)>::f();
    }

    {
        false_value tr;
        tr = check<__has_trivial_copy(A5)>::f();
    }

    {
        false_value tr;
        tr = check<__has_trivial_copy(A6)>::f();
    }
}
