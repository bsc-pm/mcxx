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
#include <algorithm>
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
        //! Mandatory function so it can be used with RefPtr
        void obj_reference()
        {
            this->_refcount++;
        }

        //! Mandatory function so it can be used with RefPtr
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

        //! Filters the list using the given predicate
        /*!
         * \param p A Predicate over elements of type T
         * \return A new list of elements of type T that satisfy predicate \a p
         */
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


        //! Applies a given functor to a list
        /*!
         * \param f A Functor of elements of type T returning elements of type S
         * \return A new list of elements of type S
         */
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

        //! Combines filter and map
        /*!
         * \param p A Predicate over elements of type T
         * \param f A Functor of elements of type T returning elements of type S
         * \return A new list of elements of type S
         */
        template <class S>
        ObjectList<S> map_filter(const Predicate<T>& p, const Functor<S, T>& f)
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
        Q reduction(const Functor<Q, std::pair<T, Q> >& red_func, const Q& neuter)
        {
            Q result;
            reduction_helper(result, this->begin(), red_func, neuter);

            return result;
        }

        //! Appends an element onto the list
        /*!
         * \param t Adds a copy of t at the end of the list
         */
        void append(const T& t)
        {
            this->push_back(t);
        }

        //! Appends a whole list onto the list
        /*!
         * \param t Appends all elements of t at the end of the list
         */
        void append(const ObjectList<T>& t)
        {
            for (typename ObjectList<T>::const_iterator it = t.begin();
                    it != t.end();
                    it++)
            {
                this->append(*it);
            }
        }

        //! Inserts element if it was not already in
        /*!
         * \param t Element to be inserted once
         */
        void insert(const T& t)
        {
            if (!contains(t))
            {
                this->push_back(t);
            }
        }

        //! Inserts elements of another list
        /*!
         * \param t Elements of a list to be inserted
         *
         * If any of the elements of list \a t was already in the current list, it will
         * not be inserted again
         */
        void insert(const ObjectList<T>& t)
        {
            for (typename ObjectList<T>::const_iterator it = t.begin();
                    it != t.end();
                    it++)
            {
                this->insert(*it);
            }
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
        void insert(const T& t, const Functor<S, T>& f)
        {
            if (!contains(t, f))
            {
                this->push_back(t);
            }
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
        void insert(const ObjectList<T>& t, const Functor<S, T>& f)
        {
            for (typename ObjectList<T>::const_iterator it = t.begin();
                    it != t.end();
                    it++)
            {
                this->insert(*it, f);
            }
        }
        
        //! States whether an element is already in the list
        /*!
         * \param t Element checked
         * \return Returns true if the element is found in the list
         *
         * This function requires that elements of type T be comparable
         * with 'operator=='
         */
        bool contains(const T& t)
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

        //! Returns a list of elements that match a given one
        /*!
         * \param t The element to be matched
         * \return A new list with all the elements of the original one
         * that are equals (according to 'operator==' of T) to \a t
         *
         * This function requires 'operator==' be defined for the type T
         */
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

        //! Returns a list of elements that do not match a given one
        /*!
         * \param t The element to be matched
         * \return A new list with all the elements of the original one
         * that are different (according to 'operator==' of T) to \a t
         *
         * This function requires 'operator==' be defined for the type T
         */
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

        //! Returns a list of elements that do not match a comparable value
        /*!
         * \param f A functor of elements of type T and returning elements of type S
         * \param s A value of type S
         * \return All elements of the list that, when applied \a yield a value of type S
         * that is different (according to 'operator==' of S) the value of \a s
         *
         * This function requires type S to be comparable with 'operator=='
         */
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

        //! Returns a list of elements that match a comparable value
        /*!
         * \param f A functor of elements of type T and returning elements of type S
         * \param s A value of type S
         * \return All elements of the list that, when applied \a yield a value of type S
         * that is equals (according to 'operator==' of S) the value of \a s
         *
         * This function requires type S to be comparable with 'operator=='
         */
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

//! @}

//! Auxiliar function that concats two ObjectList<std::string> with an optionally given separator
/*!
 * \param string_list A list of strings
 * \param separator A separator used between the strings. By default empty.
 * \return A single std::string value with all the strings in \a string_list concatenated and interspersed with
 * \a separator
 */
std::string concat_strings(const ObjectList<std::string>& string_list, const std::string& separator = std::string(""));

}

#endif // TL_OBJECTLIST_HPP
