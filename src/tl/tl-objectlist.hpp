/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/

#ifndef TL_OBJECTLIST_HPP
#define TL_OBJECTLIST_HPP

#include "tl-common.hpp"
#include <vector>
#include <sstream>
#include <utility>
#include <algorithm>
#include "tl-object.hpp"
#include "tl-functor.hpp"
#include "tl-predicate.hpp"
#include <signal.h>

namespace TL
{
//! \addtogroup ObjectList Lists of objects
//! @{

//! This class is a specialized form of vector more suitable for "list-wide" operations
/*!
 * This class can be used like a set with insert functions or like a list with append function.
 * When used as a set it is not optimal and elements will require 'operator=='
 */
template <class T>
class ObjectList : public std::vector<T>, public TL::Object
{
    private:
        template <class Q>
        void reduction_helper(Q &result, typename ObjectList<T>::const_iterator it,
                const Functor<Q, std::pair<T, Q> >& red_func, const Q& neuter) const
        {
            if (it == this->end())
            {
                result = neuter;
            }
            else
            {
                const T &t = *it;
                reduction_helper(result, it + 1, red_func, neuter);

                std::pair<T, Q> arg(t, result);
                result = red_func(arg);
            }
        }

    public:
        typedef typename std::vector<T>::iterator iterator;
        typedef typename std::vector<T>::const_iterator const_iterator;
        typedef typename std::vector<T>::size_type size_type;

        ObjectList()
        {
        }

        ObjectList(const ObjectList& o)
            : std::vector<T>(o)
        {
        }

        ObjectList(size_type num, const T& val = T())
            : std::vector<T>(num, val)
        {
        }

        template <typename input_iterator>
        ObjectList(input_iterator start, input_iterator end)
            : std::vector<T>(start, end)
        {
        }

        template <int N>
        ObjectList(T (&v)[N])
         : std::vector<T>(v, v + N)
        {
        }

        virtual ~ObjectList()
        {
        }

        //! Filters the list using the given predicate
        /*!
         * \param p A Predicate over elements of type T
         * \return A new list of elements of type T that satisfy predicate \a p
         */
        ObjectList<T> filter(const Predicate<T>& p) const
        {
            ObjectList<T> result;
            for (typename ObjectList<T>::const_iterator it = this->begin();
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


        //! Applies a given functor to a list
        /*!
         * \param f A Functor of elements of type T returning elements of type S
         * \return A new list of elements of type S
         */
        template <class S>
        ObjectList<S> map(const Functor<S, T>& f) const
        {
            ObjectList<S> result;
            for (typename ObjectList<T>::const_iterator it = this->begin();
                    it != this->end();
                    it++)
            {
                S s(f(*it));
                result.push_back(s);
            }

            return result;
        }

        //! Combines filter and map
        /*!
         * \param p A Predicate over elements of type T
         * \param f A Functor of elements of type T returning elements of type S
         * \return A new list of elements of type S
         */
        template <class S>
        ObjectList<S> map_filter(const Predicate<T>& p, const Functor<S, T>& f) const
        {
            return (this->filter(p)).map(f);
        }

        //! Performs a reduction on a list
        /*!
         * \param red_func Reductor functor receiving a pair of elements of type T and Q, respectively, returning elements of type Q
         * \param neuter Neuter element of type Q
         * \return An element of type Q with all the reduced values of the list
         */
        template <class Q>
        Q reduction(const Functor<Q, std::pair<T, Q> >& red_func, const Q& neuter) const
        {
            Q result;
            reduction_helper(result, this->begin(), red_func, neuter);

            return result;
        }

        //! Appends an element onto the list
        /*!
         * \param t Adds a copy of t at the end of the list
         */
        ObjectList<T>& append(const T& t)
        {
            this->push_back(t);
            return *this;
        }

        //! Appends a whole list onto the list
        /*!
         * \param t Appends all elements of t at the end of the list
         */
        ObjectList<T>& append(const ObjectList<T>& t)
        {
            for (typename ObjectList<T>::const_iterator it = t.begin();
                    it != t.end();
                    it++)
            {
                this->append(*it);
            }
            return *this;
        }

        //! Inserts element if it was not already in
        /*!
         * \param t Element to be inserted once
         */
        ObjectList<T>& insert(const T& t)
        {
            if (!contains(t))
            {
                this->push_back(t);
            }
            return *this;
        }

        //! Inserts elements of another list
        /*!
         * \param t Elements of a list to be inserted
         *
         * If any of the elements of list \a t was already in the current list, it will
         * not be inserted again
         */
        ObjectList<T>& insert(const ObjectList<T>& t)
        {
            for (typename ObjectList<T>::const_iterator it = t.begin();
                    it != t.end();
                    it++)
            {
                this->insert(*it);
            }
            return *this;
        }

        //! Inserts element with a specified comparator
        /*!
         * \param t Element to be inserted
         * \param f Functor comparator of type T returning type S
         *
         * This function is used to create a set of elements that
         * cannot be compared directly but by means of another type S.
         * Functor \a f gets an S value after a T value.
         */
        template <class S>
        ObjectList<T>& insert(const T& t, const Functor<S, T>& f)
        {
            if (!contains(t, f))
            {
                this->push_back(t);
            }
            return *this;
        }

        //! Inserts elements of another list with a specified comparator
        /*!
         * \param t List of elements to be inserted
         * \param f Functor comparator of type T returning type S
         *
         * This function is used to create a set of elements that
         * cannot be compared directly but by means of another type S.
         * Functor \a f gets an S value after a T value.
         *
         * Elements of \a t are not readded if they yield a value S
         * already present in the current list.
         */
        template <class S>
        ObjectList<T>& insert(const ObjectList<T>& t, const Functor<S, T>& f)
        {
            for (typename ObjectList<T>::const_iterator it = t.begin();
                    it != t.end();
                    it++)
            {
                this->insert(*it, f);
            }
            return *this;
        }
        
        //! States whether an element is already in the list
        /*!
         * \param t Element checked
         * \return Returns true if the element is found in the list
         *
         * This function requires that elements of type T be comparable
         * with 'operator=='
         */
        bool contains(const T& t) const
        {
            return (std::find(this->begin(), this->end(), t) != this->end());
        }

        //! States whether an element is already in the list with a given comparator
        /*!
         * \param t Element checked of type T
         * \param f Functor of elements of type T returning values of type S
         * \return Returns true if the element is found in the list
         *
         * This function requires that elements of type S (not T) be comparable
         * with 'operator=='. Functor \a f is used to get a value of S given
         * a value of T
         */
        template <class S>
        bool contains(const T& t, const Functor<S, T>& f) const
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

        //! States whether an element is already in the list with a given comparator
        /*!
         * \param f Functor of elements of type T returning values of type S
         * \param s Element of type S used for comparison
         * \return Returns true if any element in the list when applied \a f returns a value of \a s
         *
         * This function requires that elements of type S (not T) be comparable
         * with 'operator=='. Functor \a f is used to get a value of S given
         * a value of T
         */
        template <class S>
        bool contains(const Functor<S, T>& f, const S& s) const
        {
            for (typename ObjectList<T>::const_iterator it = this->begin();
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

        //! Returns a list of elements that match a given one
        /*!
         * \param t The element to be matched
         * \return A new list with all the elements of the original one
         * that are equals (according to 'operator==' of T) to \a t
         *
         * This function requires 'operator==' be defined for the type T
         */
        ObjectList<T> find(const T& t) const
        {
            ObjectList<T> result;

            for (typename ObjectList<T>::const_iterator it = std::find(this->begin(), this->end(), t);
                    it != this->end();
                    it++)
            {
                result.append(*it);
            }

            return result;
        }

        //! Returns a list of elements that do not match a given one
        /*!
         * \param t The element to be matched
         * \return A new list with all the elements of the original one
         * that are different (according to 'operator==' of T) to \a t
         *
         * This function requires 'operator==' be defined for the type T
         */
        ObjectList<T> not_find(const T& t) const
        {
            ObjectList<T> result;

            for (typename ObjectList<T>::const_iterator it = this->begin();
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

        //! Returns a list of elements that do not match a comparable value
        /*!
         * \param f A functor of elements of type T and returning elements of type S
         * \param s A value of type S
         * \return All elements of the list that, when applied \a f yield a value of type S
         * that is different (according to 'operator==' of S) the value of \a s
         *
         * This function requires type S to be comparable with 'operator=='
         */
        template <class S>
        ObjectList<T> not_find(const Functor<S, T>& f, const S& s) const
        {
            ObjectList<T> result;

            for (typename ObjectList<T>::const_iterator it = this->begin();
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

        //! Returns a list of elements that match a comparable value
        /*!
         * \param f A functor of elements of type T and returning elements of type S
         * \param s A value of type S
         * \return All elements of the list that, when applied \a f yield a value of type S
         * that is equals (according to 'operator==' of S) the value of \a s
         *
         * This function requires type S to be comparable with 'operator=='
         */
        template <class S>
        ObjectList<T> find(const Functor<S, T>& f, const S& s) const
        {
            ObjectList<T> result;

            for (typename ObjectList<T>::const_iterator it = this->begin();
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
        
        //! Returns a list of elements that do not match a comparable value
        /*!
         * \param t A value of type T
         * \param f A functor of elements of type T and returning elements of type S
         * \return All elements of the list that, when applied \a f yield a value of type S
         * that is different (according to 'operator==' of S) the value that results of
         * applying \a f to \a t
         *
         * This function requires type S to be comparable with 'operator=='
         */
        template <class S>
        ObjectList<T> not_find(const T& t, const Functor<S, T>& f) const
        {
            ObjectList<T> result;

            for (typename ObjectList<T>::const_iterator it = this->begin();
                    it != this->end();
                    it++)
            {
                if (!(f(*it) == f(t)))
                {
                    result.append(*it);
                }
            }

            return result;
        }

        //! Returns a list of elements that match a comparable value
        /*!
         * \param t A value of type T
         * \param f A functor of elements of type T and returning elements of type S
         * \return All elements of the list that, when applied \a f yield a value of type S
         * that is equals (according to 'operator==' of S) the value that results 
         * of applying \a f to \a t
         *
         * This function requires type S to be comparable with 'operator=='
         */
        template <class S>
        ObjectList<T> find(const T& t, const Functor<S, T>& f) const
        {
            ObjectList<T> result;

            for (typename ObjectList<T>::const_iterator it = this->begin();
                    it != this->end();
                    it++)
            {
                if (f(*it) == f(t))
                {
                    result.append(*it);
                }
            }

            return result;
        }
};

//! @}

//! Auxiliar function that concats two ObjectList<_T> with an optionally given separator
/*!
 * \param string_list A list of strings
 * \param separator A separator used between the strings. By default empty.
 * \return A single std::string value with all the strings in \a string_list concatenated and interspersed with
 * \a separator
 *
 * \note Type _T must implement operator<<(std::ostream&, const T&)
 */
template <typename _T>
LIBTL_EXTERN std::string concat_strings(const ObjectList<_T>& string_list, const std::string& separator = "")
{
    std::string result;
    for (typename ObjectList<_T>::const_iterator it = string_list.begin();
            it != string_list.end();
            it++)
    {
        if (it != string_list.begin())
        {
            result += separator;
        }
        std::stringstream ss;
        ss << *it;
        result += ss.str();
    }

    return result;
}

}
#endif // TL_OBJECTLIST_HPP
