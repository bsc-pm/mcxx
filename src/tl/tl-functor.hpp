/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
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

    // Ret (Q::*pmf)(T& t)
    template <class Ret, class T, class Q>
    class MemberFunctionAdapterRef : public Functor<Ret, T>
    {
        private:
            Q& _q;
            Ret (Q::*_pmf)(T& t);
        public:
            MemberFunctionAdapterRef(Ret (Q::*pmf)(T& t), Q& q)
                : _q(q), _pmf(pmf)
            {
            }

            virtual Ret operator()(T& t) const
            {
                return (_q.*_pmf)(t);
            }

            virtual ~MemberFunctionAdapterRef()
            {
            }
    };

    // Ret (Q::*pmf)(const T& t)
    template <class Ret, class T, class Q>
    class MemberFunctionAdapterConstRef : public Functor<Ret, T>
    {
        private:
            Q& _q;
            Ret (Q::*_pmf)(const T& t);
        public:
            MemberFunctionAdapterConstRef(Ret (Q::*pmf)(const T& t), Q& q)
                : _q(q), _pmf(pmf)
            {
            }

            virtual Ret operator()(T& t) const
            {
                return (_q.*_pmf)(t);
            }

            virtual ~MemberFunctionAdapterConstRef()
            {
            }
    };

    // Ret (Q::*pmf)(T t)
    template <class Ret, class T, class Q>
    class MemberFunctionAdapterVal : public Functor<Ret, T>
    {
        private:
            Q& _q;
            Ret (Q::*_pmf)(T t);
        public:
            MemberFunctionAdapterVal(Ret (Q::*pmf)(T t), Q& q)
                : _q(q), _pmf(pmf)
            {
            }

            virtual Ret operator()(T& t) const
            {
                return (_q.*_pmf)(t);
            }

            virtual ~MemberFunctionAdapterVal()
            {
            }
    };

    // Ret (Q::*pmf)(T& t) const
    template <class Ret, class T, class Q>
    class MemberConstFunctionAdapterRef : public Functor<Ret, T>
    {
        private:
            Q& _q;
            Ret (Q::*_pmf)(T& t) const;
        public:
            MemberConstFunctionAdapterRef(Ret (Q::*pmf)(T& t) const, Q& q)
                : _q(q), _pmf(pmf)
            {
            }

            virtual Ret operator()(T& t) const
            {
                return (_q.*_pmf)(t);
            }

            virtual ~MemberConstFunctionAdapterRef()
            {
            }
    };
    
    // Ret (Q::*pmf)(const T& t) const
    template <class Ret, class T, class Q>
    class MemberConstFunctionAdapterConstRef : public Functor<Ret, T>
    {
        private:
            Q& _q;
            Ret (Q::*_pmf)(const T& t) const;
        public:
            MemberConstFunctionAdapterConstRef(Ret (Q::*pmf)(const T& t) const, Q& q)
                : _q(q), _pmf(pmf)
            {
            }

            virtual Ret operator()(T& t) const
            {
                return (_q.*_pmf)(t);
            }

            virtual ~MemberConstFunctionAdapterConstRef()
            {
            }
    };

    // Ret (Q::*pmf)(T t) const
    template <class Ret, class T, class Q>
    class MemberConstFunctionAdapterVal : public Functor<Ret, T>
    {
        private:
            Q& _q;
            Ret (Q::*_pmf)(T t) const;
        public:
            MemberConstFunctionAdapterVal(Ret (Q::*pmf)(T t) const, Q& q)
                : _q(q), _pmf(pmf)
            {
            }

            virtual Ret operator()(T& t) const
            {
                return (_q.*_pmf)(t);
            }

            virtual ~MemberConstFunctionAdapterVal()
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
    // This one is FunctionAdapter but it removes the const
    template <class Ret, class T>
    FunctionAdapter<Ret, T> functor(Ret (*pf)(const T&))
    {
        FunctionAdapter<Ret, T> result(pf);
        return result;
    }

    // This one does not need the const to be removed
    template <class Ret, class T>
    FunctionByValueAdapter<Ret, T> functor(Ret (*pf)(T))
    {
        FunctionByValueAdapter<Ret, T> result(pf);
        return result;
    }

    // Ret (Q::*pmf)(T& t)
    template <class Ret, class T, class Q>
    MemberFunctionAdapterRef<Ret, T, Q> functor(Ret (Q::*pmf)(T& t), Q& q)
    {
        MemberFunctionAdapterRef<Ret, T, Q> result(pmf, q);
        return result;
    }

    // Ret (Q::*pmf)(const T& t)
    template <class Ret, class T, class Q>
    MemberFunctionAdapterConstRef<Ret, T, Q> functor(Ret (Q::*pmf)(const T& t), Q& q)
    {
        MemberFunctionAdapterConstRef<Ret, T, Q> result(pmf, q);
        return result;
    }

    // Ret (Q::*pmf)(T t)
    template <class Ret, class T, class Q>
    MemberFunctionAdapterVal<Ret, T, Q> functor(Ret (Q::*pmf)(T t), Q& q)
    {
        MemberFunctionAdapterVal<Ret, T, Q> result(pmf, q);
        return result;
    }

    // Ret (Q::*pmf)(T& t) const
    template <class Ret, class T, class Q>
    MemberConstFunctionAdapterRef<Ret, T, Q> functor(Ret (Q::*pmf)(T& t) const, Q& q)
    {
        MemberConstFunctionAdapterRef<Ret, T, Q> result(pmf, q);
        return result;
    }
    
    // Ret (Q::*pmf)(const T& t) const
    template <class Ret, class T, class Q>
    MemberConstFunctionAdapterConstRef<Ret, T, Q> functor(Ret (Q::*pmf)(const T& t) const, Q& q)
    {
        MemberConstFunctionAdapterConstRef<Ret, T, Q> result(pmf, q);
        return result;
    }

    // Ret (Q::*pmf)(T t) const
    template <class Ret, class T, class Q>
    MemberConstFunctionAdapterVal<Ret, T, Q> functor(Ret (Q::*pmf)(T t) const, Q& q)
    {
        MemberConstFunctionAdapterVal<Ret, T, Q> result(pmf, q);
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
