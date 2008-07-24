template <typename _T>
struct Nullable;

struct nullptr_t { 
    template <typename _T>
    bool operator==(const Nullable<_T>& n);
} static nullptr;

template <typename _T>
struct Nullable
{
    private:
        _T* _t;

    public:
        Nullable(const _T& t)
            : _t(new _T(t)) { }

        Nullable(Nullable& n)
            : _t(new _T(*n._t)) { }

        Nullable& operator=(const Nullable& n)
        {
            if (&n != *this)
            {
                delete _t;
                _t = new _T(n._t);
            }
            return *this;
        }

        Nullable& operator=(const _T& t)
        {
            delete _t;
            _t = new _T(t);
        }

        operator _T()
        {
            return *_t;
        }

        friend struct nullptr_t;

        Nullable(nullptr_t)
            : _t(0) { }


        bool operator==(const Nullable& n)
        {
            return (_t == n._t)
                || ((*_t) == (*(n._t)));
        }

        Nullable& operator=(const nullptr_t&)
        {
            _t = 0;
            return *this;
        }

        bool operator==(const nullptr_t&)
        {
            return (_t == 0);
        }

        ~Nullable()
        {
            delete _t;
        }
};

template <typename _T>
bool nullptr_t::operator==(const Nullable<_T>& n)
{
    return (n._t == 0);
}

int main(int argc, char* argv[])
{
    Nullable<float> a(3.3);

    float k;

    k = a + 2.5;

    if (a == nullptr)
    {
    }

    if (nullptr == a)
    {
    }

    a = nullptr;

    a = 3.2;

    return 0;
}
