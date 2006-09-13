enum E
{
  A = 1,
  B = A + 1
};

template <class T>
struct C
{
};

C<E> c;
