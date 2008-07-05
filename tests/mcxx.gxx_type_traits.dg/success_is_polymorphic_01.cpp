
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
    void f() { }
};

struct A2 : A1
{
};

struct A3
{
    virtual void f() = 0;
};

struct A4
{
    virtual void f() { }
};

struct A5 : A3
{
};

struct A6 : A4
{
};

struct A7
{
    A4 a4;
};

int main(int argc, char* argv[])
{
    {
        false_value tr;
        tr = check<__is_polymorphic(A1)>::f();
    }

    {
        false_value tr;
        tr = check<__is_polymorphic(A2)>::f();
    }

    {
        true_value tr;
        tr = check<__is_polymorphic(A3)>::f();
    }

    {
        true_value tr;
        tr = check<__is_polymorphic(A4)>::f();
    }

    {
        true_value tr;
        tr = check<__is_polymorphic(A5)>::f();
    }

    {
        true_value tr;
        tr = check<__is_polymorphic(A6)>::f();
    }

    {
        false_value tr;
        tr = check<__is_polymorphic(A7)>::f();
    }
}
