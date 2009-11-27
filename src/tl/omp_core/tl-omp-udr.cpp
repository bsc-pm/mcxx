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

#include "tl-omp-core.hpp"
#include "tl-omp-udr.hpp"
#include "tl-overload.hpp"

#include "tl-source.hpp"

#include "cxx-utils.h"

#include "cxx-parser.h"
#include "c99-parser.h"

#include "cxx-exprtype.h"

namespace TL
{
    struct udr_valid_prototypes_t
    {
        Type return_type;
        Type first_arg;
        Type second_arg;
        // OpenMP::UDRInfoItem::Associativity default_assoc;
        bool allows_left;
        bool allows_right;
    };

    struct udr_valid_member_prototypes_t
    {
        Type return_type;
        Type first_arg;
    };

    struct udr_valid_array_prototypes_t
    {
        Type return_type;
        ObjectList<Type> parameters;
        // OpenMP::UDRInfoItem::Associativity default_assoc;
        bool allows_left;
        bool allows_right;
    };

    static bool equivalent_array_types(Type param_type, Type reduct_type, int num_dimensions)
    {
        if (param_type.is_pointer()
                && param_type.points_to().is_same_type(reduct_type))
            return true;

        Type element_type = param_type;
        for (int i = 0; i < num_dimensions; i++)
        {
            if (element_type.is_array())
            {
                element_type = element_type.array_element();
            }
            else if (i == (num_dimensions - 1) && element_type.is_pointer())
            {
                element_type = element_type.points_to();
            }
            else
                return false;
        }

        return element_type.is_same_type(reduct_type);
    }

    static bool function_is_valid_udr_reductor_c_array(
            Type reduct_type,
            ObjectList<udr_valid_array_prototypes_t>& valid_prototypes,
            Symbol sym, 
            OpenMP::UDRInfoItem::Associativity &assoc,
            int num_dimensions)
    {
        using OpenMP::UDRInfoItem;

        if (!sym.is_function())
            return false;

        Type function_type = sym.get_type();

        if (!function_type.is_function())
        {
            internal_error("Function name should have function type!", 0);
        }

        ObjectList<Type> parameter_types = function_type.parameters();

        if (parameter_types.size() != (num_dimensions + 2))
            return false;

        Type return_type = function_type.returns();

        for (ObjectList<udr_valid_array_prototypes_t>::iterator it = valid_prototypes.begin(); 
                it != valid_prototypes.end(); 
                it++)
        {
            if (!return_type.is_same_type(it->return_type))
                continue;

            bool valid = true;
            for (int i = 0; i < num_dimensions; i++)
            {
                if (!parameter_types[i].is_same_type(get_signed_int_type()))
                {
                    valid = false;
                    break;
                }
            }
            if (!valid)
                continue;

            if (equivalent_array_types(parameter_types[parameter_types.size() - 2], reduct_type, num_dimensions)
                    && equivalent_array_types(parameter_types[parameter_types.size() - 1], reduct_type, num_dimensions)
                    && ((assoc == UDRInfoItem::UNDEFINED)
                        || (assoc == UDRInfoItem::RIGHT && it->allows_right)
                        || (assoc == UDRInfoItem::LEFT && it->allows_left)))
            {
                if (assoc == UDRInfoItem::UNDEFINED)
                {
                    if (it->allows_left)
                        assoc = UDRInfoItem::LEFT;
                    else
                        assoc = UDRInfoItem::RIGHT;
                }
                return true;
            }
        }

        return false;
    }

    static bool function_is_valid_udr_reductor_c(
            ObjectList<udr_valid_prototypes_t>& valid_prototypes,
            Symbol sym, 
            OpenMP::UDRInfoItem::Associativity &assoc)
    {
        using OpenMP::UDRInfoItem;

        if (!sym.is_function())
            return false;

        Type function_type = sym.get_type();

        if (!function_type.is_function())
        {
            internal_error("Function name should have function type!", 0);
        }

        ObjectList<Type> parameter_types = function_type.parameters();

        if (parameter_types.size() != 2)
            return false;

        Type return_type = function_type.returns();

        // Remove qualifications since they do not play any role in this comparison
        parameter_types[0] = parameter_types[0].get_unqualified_type();
        parameter_types[1] = parameter_types[1].get_unqualified_type();

        for (ObjectList<udr_valid_prototypes_t>::iterator it = valid_prototypes.begin(); 
                it != valid_prototypes.end(); 
                it++)
        {
            if (return_type.is_same_type(it->return_type)
                    && parameter_types[0].is_same_type(it->first_arg)
                    && parameter_types[1].is_same_type(it->second_arg)
                    && ((assoc == UDRInfoItem::UNDEFINED)
                        || (assoc == UDRInfoItem::RIGHT && it->allows_right)
                        || (assoc == UDRInfoItem::LEFT && it->allows_left)))
            {
                if (assoc == UDRInfoItem::UNDEFINED)
                {
                    if (it->allows_left)
                        assoc = UDRInfoItem::LEFT;
                    else
                        assoc = UDRInfoItem::RIGHT;
                }
                return true;
            }
        }

        return false;
    }

    static bool function_is_valid_udr_reductor_cxx(
            ObjectList<udr_valid_prototypes_t>& valid_prototypes,
            ObjectList<udr_valid_member_prototypes_t>& valid_member_prototypes,
            Type reduct_type,
            Symbol sym, 
            OpenMP::UDRInfoItem::Associativity &assoc)
    {
        if (!sym.is_function())
            return false;

        Type function_type = sym.get_type();

        if (!function_type.is_function())
        {
            internal_error("Function name should have function type!", 0);
        }

        ObjectList<Type> parameter_types = function_type.parameters();

        Type return_type = function_type.returns();

        if (!sym.is_member() || sym.is_static())
        {
            if (parameter_types.size() != 2)
                return false;

            using OpenMP::UDRInfoItem;

            // Remove qualifications since they do not play any role in this comparison
            parameter_types[0] = parameter_types[0].get_unqualified_type();
            parameter_types[1] = parameter_types[1].get_unqualified_type();

            for (ObjectList<udr_valid_prototypes_t>::iterator it = valid_prototypes.begin(); 
                    it != valid_prototypes.end(); 
                    it++)
            {
                if (return_type.is_same_type(it->return_type)
                        && parameter_types[0].is_same_type(it->first_arg)
                        && parameter_types[1].is_same_type(it->second_arg)
                        && ((assoc == UDRInfoItem::UNDEFINED)
                            || (assoc == UDRInfoItem::RIGHT && it->allows_right)
                            || (assoc == UDRInfoItem::LEFT && it->allows_left)))
                {
                    if (assoc == UDRInfoItem::UNDEFINED)
                    {
                        if (it->allows_left)
                            assoc = UDRInfoItem::LEFT;
                        else
                            assoc = UDRInfoItem::RIGHT;
                    }
                    return true;
                }
            }

        }
        else
        {
            if (parameter_types.size() != 1)
                return false;

            // Nonstatic member
            Type class_type = sym.get_class_type();

            if (!class_type.is_same_type(reduct_type))
                return false;

            // Remove qualifications since they do not play any role in this comparison
            parameter_types[0] = parameter_types[0].get_unqualified_type();

            for (ObjectList<udr_valid_member_prototypes_t>::iterator it = valid_member_prototypes.begin();
                    it != valid_member_prototypes.end();
                    it++)
            {
                if (return_type.is_same_type(it->return_type)
                        && parameter_types[0].is_same_type(it->first_arg))
                    return true;
            }
        }

        return false;
    }

    bool udr_is_builtin_operator(const std::string& op_name)
    {
        return (op_name == "+"
                || op_name == "-"
                || op_name == "*"
                || op_name == "/"
                || op_name == "&"
                || op_name == "|"
                || op_name == "^"
                || op_name == "&&"
                || op_name == "||");
    }

    static ObjectList<udr_valid_prototypes_t> get_valid_prototypes_c(Type reduct_type)
    {
        reduct_type = reduct_type.get_unqualified_type();

        Type void_type = Type::get_void_type();
        Type ptr_reduct_type = reduct_type.get_pointer_to();
        Type c_ptr_reduct_type = reduct_type.get_const_type().get_pointer_to();

        udr_valid_prototypes_t valid_prototypes[] = 
        {
            { /* T f(T, T) */      reduct_type, reduct_type,       reduct_type,     /* left */ true,  /* right */ true  },
            { /* T f(T*, T) */     reduct_type, ptr_reduct_type,   reduct_type,     /* left */ true,  /* right */ true  },
            { /* T f(T, T*) */     reduct_type, reduct_type,       ptr_reduct_type, /* left */ true,  /* right */ true  },
            { /* T f(T*, T*) */    reduct_type, ptr_reduct_type,   ptr_reduct_type, /* left */ true,  /* right */ true  },
            { /* void f(T*, T) */  void_type,   ptr_reduct_type,   reduct_type,     /* left */ true,  /* right */ false },
            { /* void f(T, T*) */  void_type,   reduct_type,       ptr_reduct_type, /* left */ false, /* right */ true  },
            { /* void f(T*, T*) */ void_type,   ptr_reduct_type,   ptr_reduct_type, /* left */ true,  /* right */ true  },

            { /* void f(T*, const T*) */ void_type, ptr_reduct_type,   c_ptr_reduct_type, /* left */ true,   /* right */ false },
            { /* void f(const T*, T*) */ void_type, c_ptr_reduct_type, ptr_reduct_type,   /* left */ false,  /* right */ true  },

            { /* T f(const T*, T) */           reduct_type, c_ptr_reduct_type, reduct_type,       /* left */ true,  /* right */ true  },
            { /* T f(T, const T*) */           reduct_type, reduct_type,       c_ptr_reduct_type, /* left */ true,  /* right */ true  },
            { /* T f(T*, const T*) */          reduct_type, ptr_reduct_type,   c_ptr_reduct_type, /* left */ true,  /* right */ true  },
            { /* T f(const T*, T*) */          reduct_type, ptr_reduct_type,   c_ptr_reduct_type, /* left */ true,  /* right */ true  },
            { /* T f(const T*, const T*) */    reduct_type, c_ptr_reduct_type, c_ptr_reduct_type, /* left */ true,  /* right */ true  },
            { /* void f(const T*, T) */        void_type,   c_ptr_reduct_type, reduct_type,       /* left */ true,  /* right */ false },
            { /* void f(T, const T*) */        void_type,   reduct_type,       c_ptr_reduct_type, /* left */ false, /* right */ true  },
            { /* void f(const T*, const T*) */ void_type,   c_ptr_reduct_type, c_ptr_reduct_type, /* left */ true,  /* right */ true  } 
        };

        return ObjectList<udr_valid_prototypes_t>(valid_prototypes);
    }


    static ObjectList<udr_valid_array_prototypes_t> get_valid_prototypes_arrays_c(Type reduct_type, int num_dimensions)
    {
        reduct_type = reduct_type.get_unqualified_type();

        ObjectList<udr_valid_array_prototypes_t> result;
        Type void_type = Type::get_void_type();

        Type ptr_reduct_type = reduct_type.get_pointer_to();
        Type array_reduc_type = reduct_type;
        Type ptr_array_reduc_type = reduct_type;
        for (int i = 0; i < num_dimensions; i++)
        {
            if (i == num_dimensions - 1)
            {
                ptr_array_reduc_type = ptr_array_reduc_type.get_pointer_to();
            }
            else
            {
                ptr_array_reduc_type = ptr_array_reduc_type.get_array_to();
            }
            array_reduc_type = array_reduc_type.get_array_to();
        }

        // FIXME - const qualified versions are still missing
        udr_valid_prototypes_t valid_prototypes[] = 
        {
            { /* void f(dim-list, T*, T*) */ void_type, ptr_reduct_type, ptr_reduct_type, 
                /* left */ true,  /* right */ true  },
            { /* void f(dim-list, T[]..[], T[]..[]) */ void_type, array_reduc_type, array_reduc_type, 
                /* left */ true,  /* right */ true  },
            { /* void f(dim-list, T(*)[]..[], T(*)[]..[]) */ void_type, ptr_array_reduc_type, ptr_array_reduc_type, 
                /* left */ true,  /* right */ true  },
        };

        ObjectList<udr_valid_prototypes_t> plain_prototypes(valid_prototypes);

        for (ObjectList<udr_valid_prototypes_t>::iterator it = plain_prototypes.begin();
                it != plain_prototypes.end();
                it++)
        {
            udr_valid_array_prototypes_t proto = { it->return_type };

            for (int i = 0; i < num_dimensions; i++)
            {
                proto.parameters.append(Type::get_int_type());
            }

            proto.parameters.append(it->first_arg);
            proto.parameters.append(it->second_arg);

            proto.allows_right = it->allows_right;
            proto.allows_left = it->allows_left;

            result.append(proto);
        }

        return result;
    }

    static ObjectList<udr_valid_prototypes_t> get_valid_prototypes_cxx(Type reduct_type)
    {
        reduct_type = reduct_type.get_unqualified_type();

        Type void_type = Type::get_void_type();
        Type ptr_reduct_type = reduct_type.get_pointer_to();
        Type c_ptr_reduct_type = reduct_type.get_const_type().get_pointer_to();
        Type ref_reduct_type = reduct_type.get_reference_to();
        Type c_ref_reduct_type = reduct_type.get_const_type().get_reference_to();

        udr_valid_prototypes_t valid_prototypes[] =
        {
            // ***************
            // * Returning T *
            // ***************
            { /* T f(T, T) */      reduct_type, reduct_type,     reduct_type,     /* right */ true, /* left */ true  },
            { /* T f(T*, T) */     reduct_type, ptr_reduct_type, reduct_type,     /* right */ true, /* left */ true  },
            { /* T f(T, T*) */     reduct_type, reduct_type,     ptr_reduct_type, /* right */ true, /* left */ true  },
            { /* T f(T*, T*) */    reduct_type, ptr_reduct_type, ptr_reduct_type, /* right */ true, /* left */ true  },

            // const-qualified versions
            { /* T f(const T*, T) */ reduct_type, c_ptr_reduct_type, reduct_type, /* left */ true,  /* right */ true  },
            { /* T f(T, const T*) */ reduct_type, reduct_type, c_ptr_reduct_type, /* left */ true,  /* right */ true  },
            { /* T f(const T*, T*) */reduct_type, c_ptr_reduct_type, ptr_reduct_type,   /* left */ true,  /* right */ true  },
            { /* T f(const T*, const T*) */ reduct_type, c_ptr_reduct_type, c_ptr_reduct_type,   /* left */ true,  /* right */ true  },

            { /* T f(T&, T) */     reduct_type, ref_reduct_type, reduct_type,       /* left */ true,  /* right */ true  },
            { /* T f(T, T&) */     reduct_type, reduct_type,     ref_reduct_type,   /* left */ true,  /* right */ true  },
            { /* T f(T&, T&) */    reduct_type, ref_reduct_type, ref_reduct_type,   /* left */ true,  /* right */ true  },

            // const-qualified versions
            { /* T f(const T&, T) */ reduct_type, c_ref_reduct_type, reduct_type,       /* left */ true,  /* right */ true  },
            { /* T f(T, const T&) */ reduct_type, reduct_type, c_ref_reduct_type,   /* left */ true,  /* right */ true  },
            { /* T f(const T&, const T&) */ reduct_type, c_ref_reduct_type, c_ref_reduct_type,   /* left */ true,  /* right */ true  },

            { /* T f(T&, T*) */    reduct_type, ref_reduct_type, ptr_reduct_type,   /* left */ true,  /* right */ true  },
            { /* T f(T*, T&) */    reduct_type, ptr_reduct_type, ref_reduct_type,   /* left */ true,  /* right */ true  },

            // const-qualified versions
            { /* T f(const T&, T*) */ reduct_type, c_ref_reduct_type, ptr_reduct_type,   /* left */ true,  /* right */ true  },
            { /* T f(T&, const T*) */ reduct_type, ref_reduct_type, c_ptr_reduct_type,   /* left */ true,  /* right */ true  },
            { /* T f(const T&, const T*) */ reduct_type, c_ref_reduct_type, c_ptr_reduct_type,   /* left */ true,  /* right */ true  },
            { /* T f(const T*, T&) */ reduct_type, c_ptr_reduct_type, ref_reduct_type,   /* left */ true,  /* right */ true  },
            { /* T f(T*, const T&) */ reduct_type, ptr_reduct_type, c_ref_reduct_type,   /* left */ true,  /* right */ true  },
            { /* T f(const T*, const T&) */ reduct_type, c_ptr_reduct_type, c_ref_reduct_type,   /* left */ true,  /* right */ true  },

            // **********************
            // * Returning const T& *
            // **********************
            { /* const T& f(T, T) */ c_ref_reduct_type, reduct_type,     reduct_type,     /* right */ true, /* left */ true  },
            { /* const T& f(T*, T) */ c_ref_reduct_type, ptr_reduct_type, reduct_type,     /* right */ true, /* left */ true  },
            { /* const T& f(T, T*) */ c_ref_reduct_type, reduct_type,     ptr_reduct_type, /* right */ true, /* left */ true  },
            { /* const T& f(T*, T*) */ c_ref_reduct_type, ptr_reduct_type, ptr_reduct_type, /* right */ true, /* left */ true  },

            // const-qualified versions
            { /* const T& f(const T*, T) */ c_ref_reduct_type, c_ptr_reduct_type, reduct_type, /* left */ true,  /* right */ true  },
            { /* const T& f(T, const T*) */ c_ref_reduct_type, reduct_type, c_ptr_reduct_type, /* left */ true,  /* right */ true  },
            { /* const T& f(const T*, T*) */reduct_type, c_ptr_reduct_type, ptr_reduct_type,   /* left */ true,  /* right */ true  },
            { /* const T& f(const T*, const T*) */ c_ref_reduct_type, c_ptr_reduct_type, c_ptr_reduct_type,   /* left */ true,  /* right */ true  },

            { /* const T& f(T&, T) */ c_ref_reduct_type, ref_reduct_type, reduct_type,       /* left */ true,  /* right */ true  },
            { /* const T& f(T, T&) */ c_ref_reduct_type, reduct_type,     ref_reduct_type,   /* left */ true,  /* right */ true  },
            { /* const T& f(T&, T&) */ c_ref_reduct_type, ref_reduct_type, ref_reduct_type,   /* left */ true,  /* right */ true  },

            // const-qualified versions
            { /* const T& f(const T&, T) */ c_ref_reduct_type, c_ref_reduct_type, reduct_type,       /* left */ true,  /* right */ true  },
            { /* const T& f(T, const T&) */ c_ref_reduct_type, reduct_type, c_ref_reduct_type,   /* left */ true,  /* right */ true  },
            { /* const T& f(const T&, const T&) */ c_ref_reduct_type, c_ref_reduct_type, c_ref_reduct_type,   /* left */ true,  /* right */ true  },

            { /* const T& f(T&, T*) */ c_ref_reduct_type, ref_reduct_type, ptr_reduct_type,   /* left */ true,  /* right */ true  },
            { /* const T& f(T*, T&) */ c_ref_reduct_type, ptr_reduct_type, ref_reduct_type,   /* left */ true,  /* right */ true  },

            // const-qualified versions
            { /* const T& f(const T&, T*) */ c_ref_reduct_type, c_ref_reduct_type, ptr_reduct_type,   /* left */ true,  /* right */ true  },
            { /* const T& f(T&, const T*) */ c_ref_reduct_type, ref_reduct_type, c_ptr_reduct_type,   /* left */ true,  /* right */ true  },
            { /* const T& f(const T&, const T*) */ c_ref_reduct_type, c_ref_reduct_type, c_ptr_reduct_type,   /* left */ true,  /* right */ true  },
            { /* const T& f(const T*, T&) */ c_ref_reduct_type, c_ptr_reduct_type, ref_reduct_type,   /* left */ true,  /* right */ true  },
            { /* const T& f(T*, const T&) */ c_ref_reduct_type, ptr_reduct_type, c_ref_reduct_type,   /* left */ true,  /* right */ true  },
            { /* const T& f(const T*, const T&) */ c_ref_reduct_type, c_ptr_reduct_type, c_ref_reduct_type,   /* left */ true,  /* right */ true  },

            // **********************
            // * Returning T& *
            // **********************
            { /* T& f(T, T) */ ref_reduct_type, reduct_type,     reduct_type,     /* right */ true, /* left */ true  },
            { /* T& f(T*, T) */ ref_reduct_type, ptr_reduct_type, reduct_type,     /* right */ true, /* left */ true  },
            { /* T& f(T, T*) */ ref_reduct_type, reduct_type,     ptr_reduct_type, /* right */ true, /* left */ true  },
            { /* T& f(T*, T*) */ ref_reduct_type, ptr_reduct_type, ptr_reduct_type, /* right */ true, /* left */ true  },

            // const-qualified versions
            { /* T& f(const T*, T) */ ref_reduct_type, c_ptr_reduct_type, reduct_type, /* left */ true,  /* right */ true  },
            { /* T& f(T, const T*) */ ref_reduct_type, reduct_type, c_ptr_reduct_type, /* left */ true,  /* right */ true  },
            { /* T& f(const T*, T*) */reduct_type, c_ptr_reduct_type, ptr_reduct_type,   /* left */ true,  /* right */ true  },
            { /* T& f(const T*, const T*) */ ref_reduct_type, c_ptr_reduct_type, c_ptr_reduct_type,   /* left */ true,  /* right */ true  },

            { /* T& f(T&, T) */ ref_reduct_type, ref_reduct_type, reduct_type,       /* left */ true,  /* right */ true  },
            { /* T& f(T, T&) */ ref_reduct_type, reduct_type,     ref_reduct_type,   /* left */ true,  /* right */ true  },
            { /* T& f(T&, T&) */ ref_reduct_type, ref_reduct_type, ref_reduct_type,   /* left */ true,  /* right */ true  },

            // const-qualified versions
            { /* T& f(const T&, T) */ ref_reduct_type, c_ref_reduct_type, reduct_type,       /* left */ true,  /* right */ true  },
            { /* T& f(T, const T&) */ ref_reduct_type, reduct_type, c_ref_reduct_type,   /* left */ true,  /* right */ true  },
            { /* T& f(const T&, const T&) */ ref_reduct_type, c_ref_reduct_type, c_ref_reduct_type,   /* left */ true,  /* right */ true  },

            { /* T& f(T&, T*) */ ref_reduct_type, ref_reduct_type, ptr_reduct_type,   /* left */ true,  /* right */ true  },
            { /* T& f(T*, T&) */ ref_reduct_type, ptr_reduct_type, ref_reduct_type,   /* left */ true,  /* right */ true  },

            // const-qualified versions
            { /* T& f(const T&, T*) */ ref_reduct_type, c_ref_reduct_type, ptr_reduct_type,   /* left */ true,  /* right */ true  },
            { /* T& f(T&, const T*) */ ref_reduct_type, ref_reduct_type, c_ptr_reduct_type,   /* left */ true,  /* right */ true  },
            { /* T& f(const T&, const T*) */ ref_reduct_type, c_ref_reduct_type, c_ptr_reduct_type,   /* left */ true,  /* right */ true  },
            { /* T& f(const T*, T&) */ ref_reduct_type, c_ptr_reduct_type, ref_reduct_type,   /* left */ true,  /* right */ true  },
            { /* T& f(T*, const T&) */ ref_reduct_type, ptr_reduct_type, c_ref_reduct_type,   /* left */ true,  /* right */ true  },
            { /* T& f(const T*, const T&) */ ref_reduct_type, c_ptr_reduct_type, c_ref_reduct_type,   /* left */ true,  /* right */ true  },

            // **********************
            // * Returning void     *
            // **********************

            { /* void f(T*, T) */  void_type, ptr_reduct_type,   reduct_type,       /* left */ true,  /* right */ false },
            { /* void f(T, T*) */  void_type, reduct_type,       ptr_reduct_type,   /* left */ false, /* right */ true  },
            { /* void f(T*, T*) */ void_type, ptr_reduct_type,   ptr_reduct_type,   /* left */ true,  /* right */ true  },

            // const-qualified versions
            { /* void f(const T*, T*) */ void_type, ptr_reduct_type,   ptr_reduct_type,   /* left */ false,  /* right */ true  },
            { /* void f(T*, const T*) */ void_type, ptr_reduct_type,   ptr_reduct_type,   /* left */ true,  /* right */ false  },

            { /* void f(T&, T) */  void_type, ref_reduct_type,   reduct_type,       /* left */ true,  /* right */ false },
            { /* void f(T, T&) */  void_type, reduct_type,       ref_reduct_type,   /* left */ false, /* right */ true  },
            { /* void f(T&, T&) */ void_type, ref_reduct_type,   ref_reduct_type,   /* left */ true,  /* right */ true  },

            // const-qualified versions
            { /* void f(const T&, T&) */ void_type, ref_reduct_type,   ref_reduct_type,   /* left */ false,  /* right */ true  },
            { /* void f(T&, const T&) */ void_type, ref_reduct_type,   ref_reduct_type,   /* left */ true,  /* right */ false  },

            { /* void f(T*, T&) */ void_type, ptr_reduct_type, ref_reduct_type,  /* left */ true, /* right */ true },
            { /* void f(T&, T*) */ void_type, ref_reduct_type, ptr_reduct_type,  /* left */ true, /* right */ true },

            // const-qualified versions
            { /* void f(const T*, T&) */ void_type, c_ptr_reduct_type, ref_reduct_type,  /* left */ false, /* right */ true },
            { /* void f(T*, const T&) */ void_type, ptr_reduct_type, c_ref_reduct_type,  /* left */ true, /* right */ false },
            { /* void f(const T&, T*) */ void_type, c_ref_reduct_type, ptr_reduct_type,  /* left */ false, /* right */ true },
            { /* void f(T&, const T*) */ void_type, ref_reduct_type, c_ptr_reduct_type,  /* left */ true, /* right */ false },
        };

        return ObjectList<udr_valid_prototypes_t>(valid_prototypes);
    }

    static ObjectList<udr_valid_member_prototypes_t> get_valid_member_prototypes_cxx(Type reduct_type)
    {
        using OpenMP::UDRInfoItem;

        reduct_type = reduct_type.get_unqualified_type();

        Type void_type = Type::get_void_type();
        Type ptr_reduct_type = reduct_type.get_pointer_to();
        Type ref_reduct_type = reduct_type.get_reference_to();
        Type c_ptr_reduct_type = reduct_type.get_const_type().get_pointer_to();
        Type c_ref_reduct_type = reduct_type.get_const_type().get_reference_to();

        udr_valid_member_prototypes_t valid_prototypes[] =
        {
            { /* void f(T) */  void_type, reduct_type      },
            { /* void f(T*) */  void_type, ptr_reduct_type },
            { /* void f(T&) */ void_type, ref_reduct_type  },
            { /* void f(const T*) */  void_type, c_ptr_reduct_type },
            { /* void f(const T&) */ void_type, c_ref_reduct_type  }
        };

        return ObjectList<udr_valid_member_prototypes_t>(valid_prototypes);
    }


    void initialize_builtin_udr_reductions(Scope global_scope)
    {
        using OpenMP::UDRInfoItem;
        using OpenMP::UDRInfoScope;

        static bool already_initialized = false;

        if (already_initialized)
            return;

        already_initialized = true;

        // FIXME - There should be a way to get these without using internal type info
        // See http://nanos.ac.upc.edu/projects/mcxx/ticket/89
        type_t* all_arithmetic_types[] =
        {
            get_char_type(),
            get_signed_int_type(),
            get_signed_short_int_type(),
            get_signed_long_int_type(),
            get_signed_long_long_int_type(),
            get_signed_char_type(),
            get_unsigned_int_type(),
            get_unsigned_short_int_type(),
            get_unsigned_long_int_type(),
            get_unsigned_long_long_int_type(),
            get_unsigned_char_type(),
            get_float_type(),
            get_double_type(),
            get_long_double_type(),
            NULL,
        };

        typedef struct 
        {
            const char* operator_name;
            const char* neuter_tree;
        } reduction_info_t; 

        const char* zero = "0";
        const char* one = "1";
        const char* neg_zero = "~0";

        reduction_info_t builtin_arithmetic_operators[] =
        {
            {"+", zero}, 
            {"-", zero}, 
            {"*", one}, 
            {NULL, NULL}
        };

        reduction_info_t builtin_logic_bit_operators[] =
        {
            {"&", neg_zero}, 
            {"|", zero}, 
            {"^", zero}, 
            {"&&", one}, 
            {"||", zero}, 
            {NULL, NULL}
        };

        int i;
        type_t* type;
        for (i = 0; (type = all_arithmetic_types[i]) != NULL; i++)
        {
            int j;
            for (j = 0; builtin_arithmetic_operators[j].operator_name != NULL; j++)
            {
                UDRInfoScope udr_info_scope(global_scope);
                udr_info_scope.add_udr(
                        UDRInfoItem::get_builtin_udr(Type(type),
                            builtin_arithmetic_operators[j].operator_name,
                            builtin_arithmetic_operators[j].neuter_tree,
                            UDRInfoItem::LEFT,
                            /* is_commutative */ true), "<global-scope>", 0);
            }
            if (is_integral_type(type))
            {
                for (j = 0; builtin_logic_bit_operators[j].operator_name != NULL; j++)
                {
                    UDRInfoScope udr_info_scope(global_scope);
                    udr_info_scope.add_udr(
                            UDRInfoItem::get_builtin_udr(Type(type),
                                builtin_logic_bit_operators[j].operator_name,
                                builtin_logic_bit_operators[j].neuter_tree,
                                UDRInfoItem::LEFT,
                                /* is_commutative */ true), "<global-scope>", 0);
                }
            }
        }
    }

    static std::string get_valid_zero_initializer(Type t)
    {
        if (t.is_array())
        {
            return "{" + get_valid_zero_initializer(t.array_element()) + "}";
        }
        else if (t.is_class())
        {
            ObjectList<Symbol> nonstatic_data = t.get_nonstatic_data_members();
            if (nonstatic_data.empty())
            {
                return "";
            }
            else
            {
                return "{" + get_valid_zero_initializer(t.get_nonstatic_data_members()[0].get_type()) + "}";
            }
        }
        else
        {
            return "0";
        }
    }

    static std::string get_valid_value_initializer(Type t)
    {
        if (t.is_dependent())
            return "";

        if (t.is_class())
        {
            if (!t.is_pod())
            {
                // If it is not pod, default initialization should do the right thing
                return "";
            }
        }
        // For most cases, get_valid_zero_initializer is enough
        return get_valid_zero_initializer(t);
    }

    static Symbol overload_on_udr(Type reduction_type, 
            ObjectList<Symbol> overload_set, 
            OpenMP::UDRInfoItem::Associativity assoc,
            const std::string& filename,
            int line,
            const std::string& op_name,
            Scope scope_of_clause,
            bool nofail = false)
    {
        ObjectList<Symbol> result;
        
        ObjectList<udr_valid_prototypes_t> valid_prototypes 
            = get_valid_prototypes_cxx(reduction_type);
        ObjectList<udr_valid_member_prototypes_t> valid_member_prototypes 
            = get_valid_member_prototypes_cxx(reduction_type);

        bool found_valid = false;

        struct OnlyMembers : Predicate<Symbol>
        {
            virtual bool do_(Symbol& sym) const
            {
                // Well, it turns that the frontend is not properly labelling template names
                // as being members
                Symbol current = sym;
                if (current.get_type().is_template_type())
                {
                    current = current.get_type().get_primary_template().get_symbol();
                }
                return current.is_member()
                    && !current.is_static();
            }
        };

        ObjectList<Symbol> members_set = overload_set.filter(OnlyMembers());

        struct OnlyNonMembers : Predicate<Symbol>
        {
            virtual bool do_(Symbol& sym) const
            {
                return !OnlyMembers()(sym);
            }
        };

        ObjectList<Symbol> non_members_set = overload_set.filter(OnlyNonMembers());

        ObjectList<Symbol> all_viables;
        // First the set of nonmembers
        for (ObjectList<udr_valid_prototypes_t>::iterator it = valid_prototypes.begin();
                it != valid_prototypes.end();
                it++)
        {
            ObjectList<Type> arguments;
            arguments.append(it->first_arg);
            arguments.append(it->second_arg);

            bool valid = false;
            ObjectList<Symbol> argument_conversor;
            ObjectList<Symbol> viable_functs;
            Symbol solved_sym = Overload::solve(
                    non_members_set,
                    Type(NULL), // No implicit
                    arguments,
                    filename,
                    line,
                    valid,
                    viable_functs,
                    argument_conversor);

            all_viables.insert(viable_functs);

            if (valid 
                    && function_is_valid_udr_reductor_cxx(valid_prototypes, valid_member_prototypes, 
                        reduction_type, solved_sym, assoc))
            {
                result.insert(solved_sym);
                found_valid = true;
            }
        }

        if (!found_valid
                && reduction_type.is_named_class())
        {
            // Do likewise for members this time
            for (ObjectList<udr_valid_member_prototypes_t>::iterator it = valid_member_prototypes.begin();
                    it != valid_member_prototypes.end();
                    it++)
            {
                ObjectList<Type> arguments;
                arguments.append(it->first_arg);

                bool valid = false;
                ObjectList<Symbol> argument_conversor;
                ObjectList<Symbol> viable_functs;
                Symbol solved_sym = Overload::solve(
                        members_set,
                        reduction_type.get_reference_to(), // implicit argument (it must be a reference)
                        arguments,
                        filename,
                        line,
                        valid,
                        viable_functs,
                        argument_conversor);

                all_viables.insert(viable_functs);

                if (valid 
                        && function_is_valid_udr_reductor_cxx(valid_prototypes, valid_member_prototypes, 
                            reduction_type, solved_sym, assoc))
                {
                    result.insert(solved_sym);
                }
            }
        }

        if (result.size() > 1)
        {
            if (!nofail)
            {
                for (ObjectList<Symbol>::iterator it = result.begin();
                        it != result.end();
                        it++)
                {
                    std::cerr << filename << ":" << line 
                        << ": note: '" << it->get_type().get_declaration(it->get_scope(), 
                                it->get_qualified_name(it->get_scope())) << "' is an eligible operator" 
                        << " for type '" << reduction_type.get_declaration(scope_of_clause, "") << "'"
                        << std::endl;
                }
                running_error("%s:%d: error: more than one reduction operator '%s' meets " 
                        "OpenMP reduction operator requirements", 
                        filename.c_str(), line, op_name.c_str());
            }
        }
        else if (result.empty())
        {
            if (!nofail)
            {
                if (!overload_set.empty())
                {
                    for (ObjectList<Symbol>::iterator it = all_viables.begin();
                            it != all_viables.end();
                            it++)
                    {
                        std::cerr << filename << ":" << line
                            << ": note: '" << it->get_type().get_declaration(it->get_scope(), 
                                    it->get_qualified_name(it->get_scope())) << "' is not a valid operator" 
                            << " for type '" << reduction_type.get_declaration(scope_of_clause, "") << "'"
                            << std::endl;
                    }
                    running_error("%s:%d: error: no candidate operator for '%s' meets " 
                            "OpenMP reduction operator requirements", 
                            filename.c_str(), line, op_name.c_str());
                }
                else
                {
                    running_error("%s:%d: error: reduction operator '%s' is unknown" ,
                            filename.c_str(), line, op_name.c_str());
                }
            }
        }
        else
        {
            return result[0];
        }

        return Symbol(NULL);
    }

    // This function computes the declared udr function in C++
    Symbol solve_udr_name_cxx(LangConstruct construct,
            Scope scope_of_clause,
            AST_t op_name,
            Type reduction_type,
            OpenMP::UDRInfoItem::Associativity &assoc)
    {
        // Ignore dependent types
        if (reduction_type.is_dependent())
            return Symbol(NULL);

        ObjectList<Symbol> sym_list;

        // FIXME - Use a better strategy for this case
        if (op_name.internal_ast_type_() == AST_OMP_UDR_MEMBER_OP)
        {
            // Fix the name if possible
            if (reduction_type.is_class() || reduction_type.is_dependent())
            {
                // Qualify the name
                Source new_id_expression;
                new_id_expression <<
                    reduction_type.get_declaration(scope_of_clause, "") << "::" << op_name.prettyprint().substr(1);

                // Parse this id_expression again
                op_name = new_id_expression.parse_id_expression(op_name, construct.get_scope_link());
            }
            else
            {
                running_error("%s: error: reduction operator specification '%s' is not valid for non-class type '%s'\n",
                        construct.get_ast().get_locus().c_str(),
                        op_name.prettyprint().c_str(),
                        reduction_type.get_declaration(scope_of_clause, "").c_str());
            }
        }

        if (udr_is_builtin_operator(op_name.prettyprint()))
        {
            // First attempt a member search
            Source src;
            src <<  reduction_type.get_declaration(scope_of_clause, "") << "::operator " << op_name.prettyprint();
            AST_t tree = src.parse_id_expression(op_name, construct.get_scope_link());

            sym_list = scope_of_clause.get_symbols_from_id_expr(tree);

            if (sym_list.empty())
            {
                src = Source("operator ") << op_name.prettyprint();
                tree = src.parse_id_expression(op_name, construct.get_scope_link());

                sym_list = scope_of_clause.get_symbols_from_id_expr(tree);
            }
        }
        else
        {
            sym_list = scope_of_clause.get_symbols_from_id_expr(op_name);
        }

        // Filter functions or dependent types
        struct OnlyFunctionsOrDependent : Predicate<Symbol>
        {
            virtual bool do_(Symbol& sym) const
            {
                return sym.is_function()
                    || sym.is_template_function_name()
                    || sym.is_dependent_entity();
            }
        };

        sym_list = sym_list.filter(OnlyFunctionsOrDependent());

        Symbol result(NULL);
        if (sym_list.empty())
        {
            running_error("%s: error: operator '%s' is not eligible for a user defined reduction", 
                    construct.get_ast().get_locus().c_str(),
                    op_name.prettyprint().c_str());
        }
        else if (sym_list.size() == 1 && sym_list[0].is_dependent_entity())
        {
            // Do nothing for dependent names
        }
        else 
        {
            result = overload_on_udr(reduction_type, 
                    sym_list, 
                    assoc,
                    construct.get_ast().get_file(),
                    construct.get_ast().get_line(),
                    op_name.prettyprint(),
                    scope_of_clause);
        }

        return result;
    }

    // omp_udr_declare_arg : omp_udr_id_expr_list ':' omp_udr_type_specifier
    // {
    //     $$ = ASTMake3(AST_OMP_UDR_DECLARE_ARG, NULL, $1, $3, ASTFileName($1), ASTLine($1), NULL);
    // }
    // /*!if CPLUSPLUS*/
    // | TEMPLATE '<' template_parameter_list '>' omp_udr_id_expr_list ':' omp_udr_type_specifier
    // {
    //     $$ = ASTMake3(AST_OMP_UDR_DECLARE_ARG, $3, $5, $7, $1.token_file, $1.token_line, NULL);
    // }
    static void parse_omp_udr_declare_arguments(const std::string &omp_udr_str, 
            AST_t ref_tree, ScopeLink sl,
            ObjectList<AST_t>& operator_list,
            ObjectList<Type>& type_list,
            AST_t &ref_tree_of_clause,
            Scope& scope_of_clause,
            bool &is_template)
    {
        std::string mangled_str = "@OMP_UDR_DECLARE@ " + omp_udr_str;
        char *str = strdup(mangled_str.c_str());
        C_LANGUAGE()
        {
            mc99_prepare_string_for_scanning(str);
        }
        CXX_LANGUAGE()
        {
            mcxx_prepare_string_for_scanning(str);
        }

        int parse_result = 0;
        AST a;

        CXX_LANGUAGE()
        {
            parse_result = mcxxparse(&a);
        }
        C_LANGUAGE()
        {
            parse_result = mc99parse(&a);
        }

        if (parse_result != 0)
        {
            running_error("Could not parse omp udr argument\n\n%s\n", 
                    TL::Source::format_source(mangled_str).c_str());
        }

        free(str);

        Scope sc = sl.get_scope(ref_tree);

        decl_context_t decl_context = sc.get_decl_context();

        scope_link_t* _scope_link = sl.get_internal_scope_link();

        AST template_header = ASTSon0(a);
        AST id_expr_list = ASTSon1(a);
        AST type_spec_list = ASTSon2(a);

        CXX_LANGUAGE()
        {
            if (template_header != NULL)
            {
                decl_context_t templated_context;
                build_scope_template_header(template_header, decl_context, &templated_context);
                // Replace the current context with the templated one, so
                // parsing does not fail later
                decl_context = templated_context;

                is_template = true;
            }
        }

        // Set the proper scope link
        scope_link_set(_scope_link, a, decl_context);
        ref_tree_of_clause = AST_t(a);
        scope_of_clause = Scope(decl_context);

        AST iter;
        for_each_element(id_expr_list, iter)
        {
            AST operator_id = ASTSon1(iter);

            if (ASTType(operator_id) != AST_OMP_UDR_MEMBER_OP
                    && ASTType(operator_id) != AST_OMP_UDR_BUILTIN_OP)
            {
                check_for_expression(operator_id, decl_context);
            }

            operator_list.append(AST_t(operator_id));
        }

        // Build types
        for_each_element(type_spec_list, iter)
        {
            AST type_id = ASTSon1(iter);

            type_t* type_info = NULL;
            gather_decl_spec_t gather_info;
            memset(&gather_info, 0, sizeof(gather_info));

            AST type_specifier_seq = ASTSon0(type_id);
            AST abstract_decl = ASTSon1(type_id);

            build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info,
                    decl_context);

            type_t* declarator_type = type_info;
            compute_declarator_type(abstract_decl, &gather_info, type_info, &declarator_type,
                    decl_context);

            type_list.append(Type(declarator_type));
        }
    }

    namespace OpenMP
    {

        void Core::declare_reduction_handler_pre(PragmaCustomConstruct construct)
        {
            DEBUG_CODE()
            {
                std::cerr << "=== Declare reduction [" << construct.get_ast().get_locus() << "]===" << std::endl;
            }
            // UDRInfoScope udr_info_scope(construct.get_scope())

            // #pragma omp declare reduction(op-name-list : type-list) order(left|right) commutative
            ScopeLink scope_link = construct.get_scope_link();

            if (!construct.is_parameterized())
            {
                std::cerr << construct.get_ast().get_locus() << ": warning: skipping 'declare reduction' pragma with wrong syntax" << std::endl;
                return;
            }

            std::string parameter_str = construct.get_parameter_arguments()[0];

            ObjectList<AST_t> op_args;
            ObjectList<Type> type_list;
            Scope scope_of_clause;

            AST_t ref_tree_of_clause(NULL);

            bool is_template = false;

            parse_omp_udr_declare_arguments(parameter_str,
                    construct.get_ast(),
                    construct.get_scope_link(),
                    op_args,
                    type_list,
                    ref_tree_of_clause,
                    scope_of_clause,
                    is_template);

            PragmaCustomClause order_clause = construct.get_clause("order");
            UDRInfoItem::Associativity assoc = UDRInfoItem::UNDEFINED;
            if (order_clause.is_defined())
            {
                std::string str = order_clause.get_arguments(ExpressionTokenizer())[0];

                if (str == "right")
                {
                    assoc = UDRInfoItem::RIGHT;
                }
                else if (str == "left")
                {
                }
                else
                {
                    std::cerr << construct.get_ast().get_locus() 
                        << ": warning: ignoring invalid 'order' clause argument." 
                        << std::endl;
                }
            }

            PragmaCustomClause identity_clause = construct.get_clause("identity");
            std::string identity("");
            if (identity_clause.is_defined())
            {
                identity = identity_clause.get_arguments(ExpressionTokenizer())[0];
                // Remove blanks
                identity.erase(std::remove(identity.begin(), identity.end(), ' '), identity.end());
            }

            PragmaCustomClause commutative_clause = construct.get_clause("commutative");
            bool is_commutative = commutative_clause.is_defined();

            bool is_array = false;
            int num_dimensions = 0;
            PragmaCustomClause dimensions_clause = construct.get_clause("dimensions");

            if (dimensions_clause.is_defined())
            {
                is_array = true;

                ObjectList<Expression> expr_list = dimensions_clause.get_expression_list();
                if (expr_list.size() != 1)
                {
                    running_error("%s: error: 'dimensions' clause must have only one argument\n",
                            dimensions_clause.get_ast().get_locus().c_str());
                }
                bool valid = false;
                num_dimensions = expr_list[0].evaluate_constant_int_expression(valid);
                if (!valid)
                {
                    running_error("%s: error: 'dimensions' clause requires a constant-expression\n",
                            dimensions_clause.get_ast().get_locus().c_str());
                }
            }

            for (ObjectList<Type>::iterator type_it = type_list.begin();
                    type_it != type_list.end();
                    type_it++)
            {
                Type &reduction_type(*type_it);
                Symbol op_symbol(NULL);

                // Remove any cv-qualifications
                reduction_type = reduction_type.advance_over_typedefs().get_unqualified_type();
                if (reduction_type.is_reference())
                {
                    reduction_type = reduction_type.references_to();
                }

                if (reduction_type.is_function()
                        || reduction_type.is_array())
                {
                    running_error("%s: error: '%s' is not a valid type for a declare reduction directive",
                            construct.get_ast().get_locus().c_str(),
                            reduction_type.get_declaration(construct.get_scope(), "").c_str());
                }

                UDRInfoScope udr_info_scope(scope_of_clause);

                for (ObjectList<AST_t>::iterator op_it = op_args.begin();
                        op_it != op_args.end();
                        op_it++)
                {
                    AST_t& op_name(*op_it);

                    // Perform lookup and further checking
                    C_LANGUAGE()
                    {
                        Symbol reductor_sym = construct.get_scope().get_symbol_from_id_expr(op_name);

                        if (!reductor_sym.is_valid())
                        {
                            running_error("%s: error: reduction operator '%s' not found in the current scope",
                                    construct.get_ast().get_locus().c_str(),
                                    op_name.prettyprint().c_str());
                        }

                        if (!is_array)
                        {
                            ObjectList<udr_valid_prototypes_t> valid_prototypes = get_valid_prototypes_c(reduction_type);

                            if (!function_is_valid_udr_reductor_c(valid_prototypes, reductor_sym, assoc))
                            {
                                running_error("%s: error: reduction operator '%s' does not meet " 
                                        "OpenMP reduction operator requirements", 
                                        construct.get_ast().get_locus().c_str(),
                                        op_name.prettyprint().c_str());
                            }
                        }
                        else
                        {
                            ObjectList<udr_valid_array_prototypes_t> valid_prototypes 
                                = get_valid_prototypes_arrays_c(reduction_type, num_dimensions);

                            if (!function_is_valid_udr_reductor_c_array(reduction_type, 
                                        valid_prototypes, 
                                        reductor_sym, 
                                        assoc, 
                                        num_dimensions))
                            {
                                running_error("%s: error: reduction operator '%s' does not meet " 
                                        "OpenMP reduction operator requirements for %d-dimensional array types", 
                                        construct.get_ast().get_locus().c_str(),
                                        op_name.prettyprint().c_str(),
                                        num_dimensions);
                            }
                        }
                        op_symbol = reductor_sym;
                    }

                    CXX_LANGUAGE()
                    {
                        op_symbol = solve_udr_name_cxx(construct,
                                scope_of_clause, 
                                op_name,
                                reduction_type, assoc);

                        // The interface of solve_udr_name_cxx should be
                        // improved, if it returns an invalid symbol it means
                        // it was dependent
                        if (!op_symbol.is_valid()
                                && !is_template)
                        {
                            std::cerr << construct.get_ast().get_locus() << ": note: skipping user-defined-reduction in dependent context" << std::endl;
                            continue;
                        }
                    }

                    if (!identity_clause.is_defined())
                    {
                        C_LANGUAGE()
                        {
                            identity = get_valid_zero_initializer(reduction_type);
                        }
                        CXX_LANGUAGE()
                        {
                            identity = get_valid_value_initializer(reduction_type);
                        }
                    }

                    IdExpression op_name_id_expr(op_name, construct.get_scope_link());
                    std::string op_name_unqualif = op_name_id_expr.get_unqualified_part(/* with_template */ false);

                    UDRInfoItem previously_declared =
                        udr_info_scope.get_udr(
                                op_name_unqualif,
                                op_name.prettyprint(),
                                reduction_type, construct.get_scope_link(),
                                construct.get_scope(),
                                construct.get_ast().get_file(),
                                construct.get_ast().get_line());

                    if (!previously_declared.is_valid())
                    {
                        if (!is_template)
                        {
                            if (!is_array)
                            {
                                std::cerr << construct.get_ast().get_locus() << ": note: declaring user-defined reduction for type '"
                                    << reduction_type.get_declaration(scope_of_clause, "") << "'"
                                    << " and operator '" 
                                    << op_symbol.get_type().get_declaration(scope_of_clause, op_symbol.get_qualified_name(scope_of_clause)) << "'"
                                    << std::endl;

                                udr_info_scope.add_udr(UDRInfoItem::get_udr(reduction_type,
                                            op_symbol, identity, assoc, is_commutative),
                                        construct.get_ast().get_file(),
                                        construct.get_ast().get_line());
                            }
                            else
                            {
                                std::cerr << construct.get_ast().get_locus() << ": note: declaring user-defined reduction for "
                                    << num_dimensions << "-dimensional arrays of type '"
                                    << reduction_type.get_declaration(scope_of_clause, "") << "'"
                                    << " and operator '" 
                                    << op_symbol.get_type().get_declaration(scope_of_clause, op_symbol.get_qualified_name(scope_of_clause)) << "'"
                                    << std::endl;

                                udr_info_scope.add_udr(UDRInfoItem::get_array_udr(reduction_type,
                                            num_dimensions,
                                            op_symbol, identity, assoc, is_commutative),
                                        construct.get_ast().get_file(),
                                        construct.get_ast().get_line());
                            }
                        }
                        else
                        {
                            std::cerr << construct.get_ast().get_locus() << ": note: declaring template user-defined reduction for type '"
                                << reduction_type.get_declaration(scope_of_clause, "") << "'"
                                << " and operator '" 
                                << op_name << "'"
                                << std::endl;

                            udr_info_scope.add_udr(UDRInfoItem::get_template_udr(reduction_type,
                                        op_name_unqualif, op_name.prettyprint(), 
                                        identity, assoc, is_commutative, scope_of_clause),
                                    construct.get_ast().get_file(),
                                    construct.get_ast().get_line());
                        }
                    }
                    else
                    {
                        running_error("%s: error: user defined reduction for reduction_type '%s' and operator '%s' already defined",
                                construct.get_ast().get_locus().c_str(),
                                reduction_type.get_declaration(construct.get_scope_link().get_scope(construct.get_ast()), "").c_str(),
                                op_name.prettyprint().c_str());
                    }
                }
            }
        }

        void Core::declare_reduction_handler_post(PragmaCustomConstruct construct) { }
    }

    static std::string instantiate_identity(const std::string _identity,
            ObjectList<TemplateParameter> template_params,
            ObjectList<TemplateArgument> template_args,
            Scope current_scope)
    {
        std::string identity(_identity);

        if (template_params.empty()
                || identity == ""
                || identity == "constructor"
                || identity == "constructor()")
            return identity;

        bool is_construct = false;

        const std::string constructor_pref = "constructor";
        if (identity.substr(0, constructor_pref.size()) == constructor_pref)
        {
            is_construct = true;
            identity = identity.substr(constructor_pref.size());
        }

        std::string parameters;

        for (ObjectList<TemplateParameter>::iterator it_p = template_params.begin();
                it_p != template_params.end();
                it_p++)
        {
            TemplateParameter &tpl_param(*it_p);
            for (ObjectList<TemplateArgument>::iterator it_a = template_args.begin();
                    it_a != template_args.end();
                    it_a++)
            {
                TemplateArgument &tpl_arg(*it_a);
                if (tpl_param.get_position() == tpl_arg.get_position()
                        && tpl_param.get_nesting() == tpl_arg.get_nesting())
                {
                    if (tpl_param.get_kind() == TemplateParameter::TYPE)
                    {
                        parameters += "typedef " + tpl_arg.get_type().get_declaration(current_scope, tpl_param.get_name()) + ";\n";
                    }
                    else if (tpl_param.get_kind() == TemplateParameter::NONTYPE)
                    {
                        parameters += tpl_param.get_symbol().get_type().get_const_type().get_declaration(current_scope, tpl_param.get_name()) 
                            + " = "
                            + tpl_arg.get_expression().prettyprint()
                            + ";\n";
                    }
                    else if (tpl_param.get_kind() == TemplateParameter::TEMPLATE)
                    {
                        internal_error("Template-template parameters not yet supported in UDRs", 0);
                    }
                    break;
                }
            }
        }

        std::string result;
        result = "({" + parameters + identity + ";})";

        if (is_construct)
        {
            result = "constructor(" + result + ")";
        }
        return result;
    }

    OpenMP::UDRInfoItem udr_lookup_cxx(const std::string& _udr_name,
            ObjectList<Symbol> udr_sym_list, 
            Type type, 
            ScopeLink scope_link,
            Scope current_scope,
            const std::string& filename, int line)
    {
        std::string udr_name(_udr_name);
        OpenMP::UDRInfoItem result;

        ObjectList<Type> argument_types;
        argument_types.push_back(type);

        ObjectList<Symbol> viable_functions;
        ObjectList<Symbol> argument_conversor;

        bool valid = false;

        // Solve the overload!
        Symbol overload_sym = TL::Overload::solve( /* candidate_functions */ udr_sym_list,
                Type(NULL), // No implicit 
                argument_types,
                filename.c_str(), // No file
                line, // No line
                valid,
                viable_functions,
                argument_conversor);

        if (valid)
        {
            // Now figure the symbol that actually succeeded the overload
            // We need to use the template type for unification
            if (overload_sym.get_type().is_template_specialized_type())
            {
                Type related_template_type = overload_sym.get_type().get_related_template_type();

                RefPtr<TL::OpenMP::UDRInfoItem> item;

                bool found = false;
                for(ObjectList<Symbol>::iterator it = udr_sym_list.begin();
                        it != udr_sym_list.end() && !found;
                        it++)
                {
                    Symbol &sym(*it);

                    RefPtr<TL::OpenMP::UDRInfoItem> obj = RefPtr<TL::OpenMP::UDRInfoItem>::cast_dynamic(sym.get_attribute("udr_info"));

                    if (sym.get_type().is_same_type(related_template_type))
                    {
                        found = true;
                        item = obj;
                    }
                }

                if (found)
                {
                    // Now we have to "instantiate" the UDR
                    Scope instantiation_scope = Scope::instantiation_scope(overload_sym, 
                            item->get_template_scope().get_template_parameters());

                    // Now parse the id-expression in this context
                    // Source src = item->get_op_name();
                    std::string op_name = item->get_op_name();
                    if (op_name[0] == '.')
                    {
                        op_name =  item->get_type().get_declaration(item->get_template_scope(), "") + "::" + op_name.substr(1);
                    }

                    Source src = op_name;

                    AST_t id_expression_tree = src.parse_id_expression(instantiation_scope, scope_link);

                    ObjectList<Symbol> sym_list = instantiation_scope.get_symbols_from_id_expr(id_expression_tree);

                    Type udr_type = overload_sym.get_type().parameters()[0];

                    Symbol sym = overload_on_udr(udr_type, sym_list, 
                            item->get_assoc(),
                            filename,
                            line,
                            item->get_internal_name(),
                            current_scope);

                    if (sym.is_valid())
                    {
                        std::cerr << filename << ":" << line << ": note: reduction solved to symbol '" 
                            << sym.get_qualified_name(sym.get_scope()) << "'"
                            << std::endl;

                        std::string instantiated_identity = instantiate_identity(item->get_identity(), 
                                item->get_template_scope().get_template_parameters(),
                                overload_sym.get_type().get_template_arguments(),
                                current_scope);

                        result = OpenMP::UDRInfoItem::get_udr(udr_type, sym,
                                instantiated_identity,
                                item->get_assoc(), 
                                item->is_commutative());
                    }
                }
            }
            else
            {
                bool found = false;
                for(ObjectList<Symbol>::iterator it = udr_sym_list.begin();
                        it != udr_sym_list.end() && !found;
                        it++)
                {
                    Symbol &sym(*it);

                    RefPtr<TL::OpenMP::UDRInfoItem> obj = RefPtr<TL::OpenMP::UDRInfoItem>::cast_dynamic(sym.get_attribute("udr_info"));

                    if (sym.get_type().is_same_type(overload_sym.get_type()))
                    {
                        result = *obj;
                        found = true;
                    }
                }
            }
        }

        if (result.is_valid())
        {
            // Now check the udr_name to match with what we discovered so far
            Symbol sym(NULL);

            if (udr_is_builtin_operator(udr_name))
            {
                if (type.is_named_class())
                {
                    // Try a first lookup in the class scope
                    AST_t udr_name_id_expr_tree = Source(
                            type.get_declaration(current_scope, "") + "::operator " + udr_name
                            ).parse_id_expression(current_scope, scope_link);
                    ObjectList<Symbol> udr_name_sym_list = current_scope.get_symbols_from_id_expr(udr_name_id_expr_tree);

                    sym = overload_on_udr(type, udr_name_sym_list, 
                            result.get_assoc(),
                            filename,
                            line,
                            udr_name,
                            current_scope,
                            /* nofail */ true);
                }

                udr_name = "operator " + udr_name;
            }
            
            if (!sym.is_valid())
            {
                AST_t udr_name_id_expr_tree = Source(udr_name).parse_id_expression(current_scope, scope_link);
                ObjectList<Symbol> udr_name_sym_list = current_scope.get_symbols_from_id_expr(udr_name_id_expr_tree);

                sym = overload_on_udr(type, udr_name_sym_list, 
                        result.get_assoc(),
                        filename,
                        line,
                        udr_name,
                        current_scope);
            }

            if (!sym.is_valid()
                    || (sym != result.get_op_symbol()))
            {
                result = OpenMP::UDRInfoItem();
            }
        }

        return result;
    }
    
}
