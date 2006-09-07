template <class T, class S, class Q>
struct A;

template <class T1, class S1, class Q1 = S1*>
struct A;

template <class T2, class S2 = T2*, class Q2>
struct A;

template <class T3 = int, class S3, class Q3>
struct A;


template <class T4, class S4, class Q4>
struct A
{
	typedef Q4 T;
};
 
A<>::T s = 0;
