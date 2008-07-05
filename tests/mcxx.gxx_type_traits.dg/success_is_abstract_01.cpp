
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
    virtual void f() = 0;
};

struct A4 : A3
{
};

struct A5 : A3
{
    virtual void f() { }
};

struct A6 : A3
{
    void f() { }
};

int main(int argc, char* argv[])
{
    {
        false_value tr;
        tr = check<__is_abstract(A1)>::f();
    }

    {
        false_value tr;
        tr = check<__is_abstract(A2)>::f();
    }

    {
        true_value tr;
        tr = check<__is_abstract(A3)>::f();
    }

    {
        true_value tr;
        tr = check<__is_abstract(A4)>::f();
    }

    {
        false_value tr;
        tr = check<__is_abstract(A5)>::f();
    }

    {
        false_value tr;
        tr = check<__is_abstract(A6)>::f();
    }
}
