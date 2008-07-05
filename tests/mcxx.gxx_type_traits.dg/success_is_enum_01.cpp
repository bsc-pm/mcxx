
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

enum E1
{
};

struct E2
{
};

typedef int E3;

int main(int argc, char* argv[])
{
    {
        true_value tr;
        tr = check<__is_enum(E1)>::f();
    }
    
    {
        false_value tr;
        tr = check<__is_enum(E2)>::f();
    }

    {
        false_value tr;
        tr = check<__is_enum(E3)>::f();
    }
}
