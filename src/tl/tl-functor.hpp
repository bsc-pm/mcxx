#ifndef TL_FUNCTOR_HPP
#define TL_FUNCTOR_HPP

#include <iostream>

namespace TL
{
    template <class Ret, class T>
    class Functor
    {
        public:
            virtual Ret operator()(T& t) const = 0; 

            // Adapters never define this one but we want it to work anyway
            // This method should be final and your functors should be effect
            // free when given a "const T&"
            virtual Ret operator()(const T& t) const
            {
                return this->operator()(const_cast<T&>(t));
            }

            virtual ~Functor() { }
    };

    template <class Ret, class T>
    class FunctionAdapter : Functor<Ret, T>
    {
        private:
            Ret (*_pf)(T&);
        public:
            FunctionAdapter(Ret (*pf)(T&))
                : _pf(pf)
            {
            }

            virtual Ret operator()(T& t) const 
            {
                return (_pf)(t);
            }

            virtual ~FunctionAdapter()
            {
            }
    };

    template <class Ret, class T>
    class FunctionByValueAdapter : Functor<Ret, T>
    {
        private:
            Ret (*_pf)(T);
        public:
            FunctionByValueAdapter(Ret (*pf)(T))
                : _pf(pf)
            {
            }

            virtual Ret operator()(T& t) const 
            {
                return (_pf)(t);
            }

            virtual ~FunctionByValueAdapter()
            {
            }
    };

    template <class Ret, class T, class Q>
    class MemberFunctionAdapter : public Functor<Ret, T>
    {
        private:
            Q& _q;
            Ret (Q::*_pmf)(T& t);
        public:
            MemberFunctionAdapter(Ret (Q::*pmf)(T& t), Q& q)
                : _q(q), _pmf(pmf)
            {
            }

            virtual Ret operator()(T& t) const
            {
                return (_q.*_pmf)(t);
            }

            virtual ~MemberFunctionAdapter()
            {
            }
    };

    template <class Ret, class T, class Q>
    class MemberFunctionByValueAdapter : public Functor<Ret, T>
    {
        private:
            Q& _q;
            Ret (Q::*_pmf)(T t);
        public:
            MemberFunctionByValueAdapter(Ret (Q::*pmf)(T t), Q& q)
                : _q(q), _pmf(pmf)
            {
            }

            virtual Ret operator()(T& t) const
            {
                return (_q.*_pmf)(t);
            }

            virtual ~MemberFunctionByValueAdapter()
            {
            }
    };

    template <class Ret, class T, class Q>
    class MemberFunctionConstAdapter : public Functor<Ret, T>
    {
        private:
            Q& _q;
            Ret (Q::*_pmf)(T& t) const;
        public:
            MemberFunctionConstAdapter(Ret (Q::*pmf)(T& t) const, Q& q)
                : _q(q), _pmf(pmf)
            {
            }

            virtual Ret operator()(T& t) const
            {
                return (_q.*_pmf)(t);
            }

            virtual ~MemberFunctionConstAdapter()
            {
            }
    };

    template <class Ret, class T, class Q>
    class MemberFunctionByValueConstAdapter : public Functor<Ret, T>
    {
        private:
            Q& _q;
            Ret (Q::*_pmf)(T t) const;
        public:
            MemberFunctionByValueConstAdapter(Ret (Q::*pmf)(T t) const, Q& q)
                : _q(q), _pmf(pmf)
            {
            }

            virtual Ret operator()(T& t) const
            {
                return (_q.*_pmf)(t);
            }

            virtual ~MemberFunctionByValueConstAdapter()
            {
            }
    };

    template <class Ret, class T>
    class DataMemberAdapter : public Functor<Ret, T>
    {
        private:
            Ret T::*_pdm;
        public:
            DataMemberAdapter(Ret T::*pdm)
                : _pdm(pdm)
            {
            }

            ~DataMemberAdapter()
            {
            }

            virtual Ret operator()(T& t) const
            {
                return t.*_pdm;
            }
    };

    template <class Ret, class T>
    class ThisMemberFunctionAdapter : public Functor<Ret, T>
    {
        private:
            Ret (T::*_pmf)();
        public:
            ThisMemberFunctionAdapter(Ret (T::*pmf)())
                : _pmf(pmf)
            {
            }

            virtual Ret operator()(T& t) const
            {
                return (t.*_pmf)();
            }

            virtual ~ThisMemberFunctionAdapter()
            {
            }
    };

    template <class Ret, class T>
    class ThisMemberFunctionConstAdapter : public Functor<Ret, T>
    {
        private:
            Ret (T::*_pmf)() const;
        public:
            ThisMemberFunctionConstAdapter(Ret (T::*pmf)() const)
                : _pmf(pmf)
            {
            }

            virtual Ret operator()(T& t) const
            {
                return (t.*_pmf)();
            }

            virtual ~ThisMemberFunctionConstAdapter()
            {
            }
    };

    template <class Ret, class T>
    FunctionAdapter<Ret, T> functor(Ret (*pf)(T&))
    {
        FunctionAdapter<Ret, T> result(pf);
        return result;
    }

    template <class Ret, class T>
    FunctionByValueAdapter<Ret, T> functor(Ret (*pf)(T))
    {
        FunctionByValueAdapter<Ret, T> result(pf);
        return result;
    }

    template <class Ret, class T, class Q>
    MemberFunctionAdapter<Ret, T, Q> functor(Ret (Q::*pmf)(T& t), Q& q)
    {
        MemberFunctionAdapter<Ret, T, Q> result(pmf, q);
        return result;
    }

    template <class Ret, class T, class Q>
    MemberFunctionByValueAdapter<Ret, T, Q> functor(Ret (Q::*pmf)(T t), Q& q)
    {
        MemberFunctionByValueAdapter<Ret, T, Q> result(pmf, q);
        return result;
    }

    template <class Ret, class T, class Q>
    MemberFunctionConstAdapter<Ret, T, Q> functor(Ret (Q::*pmf)(T& t) const, Q& q)
    {
        MemberFunctionConstAdapter<Ret, T, Q> result(pmf, q);
        return result;
    }

    template <class Ret, class T, class Q>
    MemberFunctionByValueConstAdapter<Ret, T, Q> functor(Ret (Q::*pmf)(T t) const, Q& q)
    {
        MemberFunctionByValueConstAdapter<Ret, T, Q> result(pmf, q);
        return result;
    }

    template <class Ret, class T>
    ThisMemberFunctionAdapter<Ret, T> functor(Ret (T::*pmf)())
    {
        ThisMemberFunctionAdapter<Ret, T> result(pmf);
        return result;
    }

    template <class Ret, class T>
    ThisMemberFunctionConstAdapter<Ret, T> functor(Ret (T::*pmf)() const)
    {
        ThisMemberFunctionConstAdapter<Ret, T> result(pmf);
        return result;
    }

    template <class Ret, class T>
    DataMemberAdapter<Ret, T> functor (Ret T::*pdm)
    {
        DataMemberAdapter<Ret, T> result(pdm);
        return result;
    }
}

#endif // TL_FUNCTOR_HPP
