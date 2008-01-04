namespace A
{
    struct C
    {
    };

    void endl(void);

    C& operator<<(C&, const char*);
    C& operator<<(C&, void (*f)(void));

    C cerr;
};

void f()
{
    A::cerr << "hello" << A::endl;
}
