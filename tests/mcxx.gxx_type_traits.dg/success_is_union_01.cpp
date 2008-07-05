
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

union A2
{
};

typedef int A3;

int main(int argc, char* argv[])
{
    {
        false_value tr;
        tr = check<__is_union(A1)>::f();
    }

    {
        true_value tr;
        tr = check<__is_union(A2)>::f();
    }

    {
        false_value tr;
        tr = check<__is_union(A3)>::f();
    }
}
