/*
<testinfo>
test_generator=config/mercurium-fe-only
test_CXXFLAGS="-std=c++11 --pp=off"
</testinfo>
*/

struct A
{
    int x;
};


void f1()
{
    static_assert(__is_trivially_constructible(A, A&), "");
    static_assert(__is_trivially_copyable(A), "");
    static_assert(__is_trivially_assignable(A, A&), "");
}

struct B
{
    int n_;
    B(int n);
};

void f2()
{
    static_assert(!__is_trivially_constructible(B, int), "");
    static_assert(__is_trivially_copyable(B), "");
    static_assert(__is_trivially_assignable(B, B&), "");
}

struct C
{
    int n_;
    C() = default;
    C(const C&);
};

void f3()
{
    static_assert(__is_trivially_constructible(C), "");
    static_assert(!__is_trivially_copyable(C), "");
    static_assert(__is_trivially_assignable(C, C), "");
}

struct D
{
    int n_;
    D& operator=(const D&);
};

void f4()
{
    static_assert(!__is_trivially_assignable(D, D), "");
}

struct E
{
    int n_;
    E() = default;
    E(const E&) = default;
    E& operator=(const E&) = default;
};

void f5()
{
    static_assert(__is_trivially_constructible(A), "");
    static_assert(__is_trivially_constructible(A, A&), "");
    static_assert(__is_trivially_copyable(A), "");
    static_assert(__is_trivially_assignable(A, A&), "");
}
