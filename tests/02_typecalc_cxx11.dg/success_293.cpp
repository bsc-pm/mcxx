/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T> struct B {};

template <typename T> struct A1 : B<T *> {
  typedef B<T *> _Base;
  friend _Base;
};
