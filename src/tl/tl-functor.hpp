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

#include "tl-common.hpp"
#include <iostream>

namespace TL
{
    //! \addtogroup Functors Functors and Predicates
    //! @{
    
    //! Function representing a callable entity with only one argument
    /*!
     * \param Ret The returning type of this callable entity
     * \param T The first parameter of this entity
     */
    template <class Ret, class T>
    class Functor
    {
        public:
            //! Call function operator implemented by non-abstract derived types
            virtual Ret operator()(T& t) const = 0; 

            //! Call function operator for const reference arguments
            /*! Adapters never define this one but we want it to work anyway
            * This method should be final and your functors should be effect
            * free when given a "const T&"
            */
            virtual Ret operator()(const T& t) const
            {
                return this->operator()(const_cast<T&>(t));
            }

            virtual ~Functor() { }
    };

    //! Adapter class for non-member functions
    template <class Ret, class T>
    class FunctionAdapter : public Functor<Ret, T>
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

    //! Adapter class for non-member functions expecting a value instead of a
    // reference type
    template <class Ret, class T>
    class FunctionByValueAdapter : public Functor<Ret, T>
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
    //! Adapter class for member functions of a given object
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
    //! Adapter class for member functions of a given object expecting a const reference parameter
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
    //! Adapter class for member functions of a given object expecting value parameter
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
    //! Adapter class for member functions of a given constant object
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
    //! Adapter class for member functions of a given constant object expecting a const reference parameter
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
    //! Adapter class for member functions of a given constant object expecting a value parameter
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

    //! Adapter class for data member functions
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

    //! Adapter class for this same object
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

    //! Adapter class for this same const object
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

    //! Adaptor to create functors of non-member functions
    template <class Ret, class T>
    FunctionAdapter<Ret, T> functor(Ret (*pf)(T&))
    {
        FunctionAdapter<Ret, T> result(pf);
        return result;
    }
    //! Adaptor to create functors of non-member functions with const reference parameter
    template <class Ret, class T>
    FunctionAdapter<Ret, T> functor(Ret (*pf)(const T&))
    {
        FunctionAdapter<Ret, T> result(pf);
        return result;
    }

    //! Adaptor to create functors of non-member functions with value parameter
    template <class Ret, class T>
    FunctionByValueAdapter<Ret, T> functor(Ret (*pf)(T))
    {
        FunctionByValueAdapter<Ret, T> result(pf);
        return result;
    }

    // Ret (Q::*pmf)(T& t)
    //! Adaptor to create functors of member functions of a given object
    template <class Ret, class T, class Q>
    MemberFunctionAdapterRef<Ret, T, Q> functor(Ret (Q::*pmf)(T& t), Q& q)
    {
        MemberFunctionAdapterRef<Ret, T, Q> result(pmf, q);
        return result;
    }

    // Ret (Q::*pmf)(const T& t)
    //! Adaptor to create functors of member functions of a given object receiving a const reference
    template <class Ret, class T, class Q>
    MemberFunctionAdapterConstRef<Ret, T, Q> functor(Ret (Q::*pmf)(const T& t), Q& q)
    {
        MemberFunctionAdapterConstRef<Ret, T, Q> result(pmf, q);
        return result;
    }

    // Ret (Q::*pmf)(T t)
    //! Adaptor to create functors of member functions of a given object receiving a value
    template <class Ret, class T, class Q>
    MemberFunctionAdapterVal<Ret, T, Q> functor(Ret (Q::*pmf)(T t), Q& q)
    {
        MemberFunctionAdapterVal<Ret, T, Q> result(pmf, q);
        return result;
    }

    // Ret (Q::*pmf)(T& t) const
    //! Adaptor to create functors of member functions of a given const object
    template <class Ret, class T, class Q>
    MemberConstFunctionAdapterRef<Ret, T, Q> functor(Ret (Q::*pmf)(T& t) const, Q& q)
    {
        MemberConstFunctionAdapterRef<Ret, T, Q> result(pmf, q);
        return result;
    }
    
    // Ret (Q::*pmf)(const T& t) const
    //! Adaptor to create functors of member functions of a given const object receiving a const reference
    template <class Ret, class T, class Q>
    MemberConstFunctionAdapterConstRef<Ret, T, Q> functor(Ret (Q::*pmf)(const T& t) const, Q& q)
    {
        MemberConstFunctionAdapterConstRef<Ret, T, Q> result(pmf, q);
        return result;
    }

    // Ret (Q::*pmf)(T t) const
    //! Adaptor to create functors of member functions of a given const object receiving a value
    template <class Ret, class T, class Q>
    MemberConstFunctionAdapterVal<Ret, T, Q> functor(Ret (Q::*pmf)(T t) const, Q& q)
    {
        MemberConstFunctionAdapterVal<Ret, T, Q> result(pmf, q);
        return result;
    }

    //! Adaptor to create functors of this same object
    template <class Ret, class T>
    ThisMemberFunctionAdapter<Ret, T> functor(Ret (T::*pmf)())
    {
        ThisMemberFunctionAdapter<Ret, T> result(pmf);
        return result;
    }

    //! Adaptor to create functors of this same const object
    template <class Ret, class T>
    ThisMemberFunctionConstAdapter<Ret, T> functor(Ret (T::*pmf)() const)
    {
        ThisMemberFunctionConstAdapter<Ret, T> result(pmf);
        return result;
    }

    //! Adaptor to create data member functors of a given object
    template <class Ret, class T>
    DataMemberAdapter<Ret, T> functor (Ret T::*pdm)
    {
        DataMemberAdapter<Ret, T> result(pdm);
        return result;
    }
    
    //! @}
}

#endif // TL_FUNCTOR_HPP
