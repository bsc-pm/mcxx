struct A1
{
};

struct A2
{
    int b;
};

struct A3 : A2
{
};

struct A4 : A1
{
};

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

int main(int argc, char* argv[])
{
    {
        true_value tr;
        tr = check<__is_empty(A1)>::f();
    }

    {
        false_value fa;
        fa = check<__is_empty(A2)>::f();
    }

    {
        false_value fa;
        fa = check<__is_empty(A3)>::f();
    }

    {
        true_value tr;
        tr = check<__is_empty(A4)>::f();
    }
}
