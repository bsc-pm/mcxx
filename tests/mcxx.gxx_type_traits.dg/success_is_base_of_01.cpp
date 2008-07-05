
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
};

struct A2
{
};

struct A3 : A1
{
};

struct A4 : A3
{
};

struct A5 : A2, A3
{
};

int main(int argc, char* argv[])
{
    {
        false_value tr;
        tr = check<__is_base_of(A1, A2)>::f();
    }

    {
        true_value tr;
        tr = check<__is_base_of(A1, A3)>::f();
    }

    {
        true_value tr;
        tr = check<__is_base_of(A1, A4)>::f();
    }

    {
        true_value tr;
        tr = check<__is_base_of(A1, A5)>::f();
    }
}
