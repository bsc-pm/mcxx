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
#ifndef TL_OBJECTLIST_HPP
#define TL_OBJECTLIST_HPP

#include <vector>
#include <utility>
#include "tl-functor.hpp"
#include "tl-predicate.hpp"
#include <signal.h>

namespace TL
{

template <class T>
class ObjectList : public std::vector<T>
{
    private:
        template <class Q>
        void reduction_helper(Q &result, typename ObjectList<T>::iterator it,
                const Functor<Q, std::pair<T, Q> >& red_func, const Q& neuter)
        {
            if (it == this->end())
            {
                result = neuter;
            }
            else
            {
                T& t = *it;
                reduction_helper(result, it + 1, red_func, neuter);

                std::pair<T, Q> arg(t, result);
                result = red_func(arg);
            }
        }

        int _refcount;
    public:
        void obj_reference()
        {
            this->_refcount++;
        }

        void obj_unreference()
        {
            this->_refcount--;

            if (this->_refcount == 0)
            {
                delete this;
            }
        }

        ObjectList()
            : _refcount(1)
        {
        }

        virtual ~ObjectList()
        {
        }

        ObjectList<T> filter(const Predicate<T>& p)
        {
            ObjectList<T> result;
            for (typename ObjectList<T>::iterator it = this->begin();
                    it != this->end();
                    it++)
            {
                if (p(*it))
                {
                    result.push_back(*it);
                }
            }

            return result;
        }


        template <class S>
        ObjectList<S> map(const Functor<S, T>& f)
        {
            ObjectList<S> result;
            for (typename ObjectList<T>::iterator it = this->begin();
                    it != this->end();
                    it++)
            {
                S s(f(*it));
                result.push_back(s);
            }

            return result;
        }

        template <class S>
        ObjectList<S> map_filter(const Predicate<T>& p, const Functor<S, T>& f)
        {
            return (this->filter(p)).map(f);
        }

        template <class Q>
        Q reduction(const Functor<Q, std::pair<T, Q> >& red_func, const Q& neuter)
        {
            Q result;
            reduction_helper(result, this->begin(), red_func, neuter);

            return result;
        }

        void append(const T& t)
        {
            this->push_back(t);
        }

        void append(const ObjectList<T>& t)
        {
            for (typename ObjectList<T>::const_iterator it = t.begin();
                    it != t.end();
                    it++)
            {
                this->append(*it);
            }
        }

        void insert(const T& t)
        {
            if (!contains(t))
            {
                this->push_back(t);
            }
        }

        void insert(const ObjectList<T>& t)
        {
            for (typename ObjectList<T>::const_iterator it = t.begin();
                    it != t.end();
                    it++)
            {
                this->insert(*it);
            }
        }

        template <class S>
        void insert(const T& t, const Functor<S, T>& f)
        {
            if (!contains(t, f))
            {
                this->push_back(t);
            }
        }

        template <class S>
        void insert(const ObjectList<T>& t, const Functor<S, T>& f)
        {
            for (typename ObjectList<T>::const_iterator it = t.begin();
                    it != t.end();
                    it++)
            {
                this->insert(*it, f);
            }
        }
        
        bool contains(const T& t)
        {
            return (std::find(this->begin(), this->end(), t) != this->end());
        }

        template <class S>
        bool contains(const T& t, const Functor<S, T>& f)
        {
            for (typename ObjectList<T>::const_iterator it = this->begin();
                    it != this->end();
                    it++)
            {
                if (f(*it) == f(t))
                {
                    return true;
                }
            }
            return false;
        }

        template <class S>
        bool contains(const Functor<S, T>& f, const S& s)
        {
            for (typename ObjectList<T>::iterator it = this->begin();
                    it != this->end();
                    it++)
            {
                if (f(*it) == s)
                {
                    return true;
                }
            }
            return false;
        }

        ObjectList<T> find(const T& t)
        {
            ObjectList<T> result;

            for (typename ObjectList<T>::iterator it = find(this->begin(), this->end(), t);
                    it != this->end();
                    it++)
            {
                result.append(*it);
            }
        }

        ObjectList<T> not_find(const T& t)
        {
            ObjectList<T> result;

            for (typename ObjectList<T>::iterator it = this->begin();
                    it != this->end();
                    it++)
            {
                if (!((*it) == t))
                {
                    result.append(*it);
                }
            }

            return result;
        }

        template <class S>
        ObjectList<T> not_find(const Functor<S, T>& f, const S& s)
        {
            ObjectList<T> result;

            for (typename ObjectList<T>::iterator it = this->begin();
                    it != this->end();
                    it++)
            {
                if (!(f(*it) == s))
                {
                    result.append(*it);
                }
            }

            return result;
        }

        template <class S>
        ObjectList<T> find(const Functor<S, T>& f, const S& s)
        {
            ObjectList<T> result;

            for (typename ObjectList<T>::iterator it = this->begin();
                    it != this->end();
                    it++)
            {
                if (f(*it) == s)
                {
                    result.append(*it);
                }
            }

            return result;
        }
};

std::string concat_strings(const ObjectList<std::string>& string_list, const std::string& separator = std::string(""));

}

#endif // TL_OBJECTLIST_HPP
