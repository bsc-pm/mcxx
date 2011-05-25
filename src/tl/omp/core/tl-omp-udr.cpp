/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information 
  regarding developers and contributors.
  
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
#include "cxx-instantiation.h"

namespace TL
{
    namespace OpenMP
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
                // The outermost dimension can be a pointer
                else if (i == 0 && element_type.is_pointer())
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

            // If we have not returned yet, try the other way round, with integer dimensions at the end
            for (ObjectList<udr_valid_array_prototypes_t>::iterator it = valid_prototypes.begin(); 
                    it != valid_prototypes.end(); 
                    it++)
            {
                if (!return_type.is_same_type(it->return_type))
                    continue;

                bool valid = true;
                for (int i = 2; i < parameter_types.size(); i++)
                {
                    if (!parameter_types[i].is_same_type(get_signed_int_type()))
                    {
                        valid = false;
                        break;
                    }
                }
                if (!valid)
                    continue;

                if (equivalent_array_types(parameter_types[0], reduct_type, num_dimensions)
                        && equivalent_array_types(parameter_types[1], reduct_type, num_dimensions)
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
                    return "{ }";
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
            // Fall back
            if (t.is_dependent())
                return "constructor()";

            if (t.is_class())
            {
                if (!t.is_pod())
                {
                    // If it is not pod, default initialization should do the right thing
                    return "constructor()";
                }
            }
            // For most cases, get_valid_zero_initializer is enough
            return get_valid_zero_initializer(t);
        }


        static bool function_is_valid_udr_reductor_c(
                ObjectList<udr_valid_prototypes_t>& valid_prototypes,
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
                UDRInfoItem::Associativity &assoc)
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

                // Remove qualifications if they do not play any role in this comparison
                if (!return_type.is_reference())
                {
                    return_type = return_type.get_unqualified_type();
                }
                parameter_types[0] = parameter_types[0].get_unqualified_type();
                parameter_types[1] = parameter_types[1].get_unqualified_type();

                for (ObjectList<udr_valid_prototypes_t>::iterator it = valid_prototypes.begin(); 
                        it != valid_prototypes.end(); 
                        it++)
                {
                    if (return_type.is_same_type(it->return_type)
                            && parameter_types[0].is_same_type(it->first_arg)
                            && parameter_types[1].is_same_type(it->second_arg))
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

            // List of prototypes where the dim-list is at the end
            for (ObjectList<udr_valid_prototypes_t>::iterator it = plain_prototypes.begin();
                    it != plain_prototypes.end();
                    it++)
            {
                udr_valid_array_prototypes_t proto = { it->return_type };

                proto.parameters.append(it->first_arg);
                proto.parameters.append(it->second_arg);

                for (int i = 0; i < num_dimensions; i++)
                {
                    proto.parameters.append(Type::get_int_type());
                }

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

        // FIXME - This is awful
        // This function creates a fake symbol which is not actually signed in any scope
        // It is used just for builtin UDR which do not have a backing symbol actually
        static scope_entry_t* new_udr_builtin_symbol(type_t* type, 
                const std::string& str, 
                decl_context_t decl_context)
        {
            scope_entry_t* result = (scope_entry_t*)calloc(1, sizeof(*result));
            result->symbol_name = uniquestr(("operator " + str).c_str());
            result->kind = SK_FUNCTION;

            parameter_info_t parameter_info[2] = 
            {
                { 0, type, type },
                { 0, type, type },
            };

            type_t* function_type_info = get_new_function_type(type,
                    parameter_info, 2);

            result->type_information = function_type_info;

            result->decl_context = decl_context;
            result->file = uniquestr("(global-scope)");
            result->line = 0;

            return result;
        }

        void initialize_builtin_udr_reductions(Scope global_scope)
        {
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
                get_complex_type(get_float_type()),
                get_complex_type(get_double_type()),
                get_complex_type(get_long_double_type()),
                NULL,
            };

            typedef struct 
            {
                const char* operator_name;
                AST_t identity;
            } reduction_info_t; 

            AST_t zero(internal_expression_parse("0", global_scope.get_decl_context()));
            AST_t one(internal_expression_parse("1", global_scope.get_decl_context()));
            AST_t neg_zero(internal_expression_parse("~0", global_scope.get_decl_context()));

            reduction_info_t builtin_arithmetic_operators[] =
            {
                {"+", zero}, 
                {"-", zero}, 
                {"*", one}, 
                {NULL, AST_t(NULL)}
            };

            reduction_info_t builtin_logic_bit_operators[] =
            {
                {"&", neg_zero}, 
                {"|", zero}, 
                {"^", zero}, 
                {"&&", one}, 
                {"||", zero}, 
                {NULL, AST_t(NULL)}
            };

            int i;
            type_t* type;
            for (i = 0; (type = all_arithmetic_types[i]) != NULL; i++)
            {
                int j;
                for (j = 0; builtin_arithmetic_operators[j].operator_name != NULL; j++)
                {
                    UDRInfoItem new_udr;

                    new_udr.set_builtin_operator(
                            builtin_arithmetic_operators[j].operator_name);
                    new_udr.set_identity(
                            builtin_arithmetic_operators[j].identity);
                    new_udr.set_reduction_type(Type(type));
                    new_udr.set_associativity(UDRInfoItem::LEFT);
                    new_udr.set_is_commutative(true);

                    ObjectList<Symbol> op_symbols;
                    op_symbols.append(Symbol(new_udr_builtin_symbol( 
                                    type,
                                    builtin_arithmetic_operators[j].operator_name,
                                    global_scope.get_decl_context())));
                    new_udr.set_operator_symbols(op_symbols);

                    new_udr.sign_in_scope(global_scope);
                }
                if (is_integral_type(type))
                {
                    for (j = 0; builtin_logic_bit_operators[j].operator_name != NULL; j++)
                    {
                        UDRInfoItem new_udr;

                        new_udr.set_builtin_operator(
                                builtin_logic_bit_operators[j].operator_name);
                        new_udr.set_identity(
                                builtin_logic_bit_operators[j].identity);
                        new_udr.set_reduction_type(Type(type));
                        new_udr.set_associativity(UDRInfoItem::LEFT);
                        new_udr.set_is_commutative(true);

                        ObjectList<Symbol> op_symbols;
                        op_symbols.append(Symbol(new_udr_builtin_symbol( 
                                        type,
                                        builtin_logic_bit_operators[j].operator_name,
                                        global_scope.get_decl_context())));
                        new_udr.set_operator_symbols(op_symbols);

                        new_udr.sign_in_scope(global_scope);
                    }
                }
            }
        }

        struct OnlyMembers : Predicate<Symbol>
        {
            virtual bool do_(OnlyMembers::ArgType sym) const
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

        struct OnlyNonMembers : Predicate<Symbol>
        {
            virtual bool do_(OnlyNonMembers::ArgType sym) const
            {
                return !OnlyMembers()(sym);
            }
        };

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
            std::stringstream ss;
            ss << "#line " << ref_tree.get_line() << " \"" << ref_tree.get_file() << "\"\n";

            std::string mangled_str = ss.str() + "@OMP_UDR_DECLARE@ " + omp_udr_str;
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
                running_error("Could not parse OpenMP user-defined reduction argument\n\n%s\n", 
                        TL::Source::format_source(mangled_str).c_str());
            }

            free(str);

            Scope sc = sl.get_scope(ref_tree);

            decl_context_t decl_context = sc.get_decl_context();

            scope_link_t* _scope_link = sl.get_internal_scope_link();

            AST template_header = ASTSon0(a);
            AST id_expr_list = ASTSon1(a);
            AST type_spec_list = ASTSon2(a);
            std::cerr << std::endl << "AFTER PARSING" << std::endl;
            std::cerr << " - id_expr '" << AST_t(id_expr_list).prettyprint() << "'" << std::endl;
            std::cerr << " - type_expr '" << AST_t(type_spec_list).prettyprint() << "'" << std::endl;

            CXX_LANGUAGE()
            {
                if (template_header != NULL)
                {
                    decl_context_t templated_context;
                    nodecl_t dummy_nodecl_output = { NULL };
                    build_scope_template_header(template_header, decl_context, &templated_context, &dummy_nodecl_output);
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
                    check_expression(operator_id, decl_context);
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

                std::cerr << std::endl << "BEFORE 'build_scope_decl_specifier_seq'" << std::endl;
                std::cerr << " - type_id '" << AST_t(type_id).prettyprint() << "'" << std::endl;
                std::cerr << " - type_specifier_seq '" << AST_t(type_specifier_seq).prettyprint() << "'" << std::endl;
                std::cerr << " - abstract_decl '" << AST_t(abstract_decl).prettyprint() << "'" << std::endl;
                nodecl_t dummy_nodecl_output = { NULL };
                build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info,
                        decl_context, &dummy_nodecl_output);

                type_t* declarator_type = type_info;
                compute_declarator_type(abstract_decl, &gather_info, type_info, &declarator_type,
                        decl_context, &dummy_nodecl_output);

                type_list.append(Type(declarator_type));
            }
        }

        static void parse_udr_identity(const std::string& omp_udr_identity,
                AST_t reference_tree,
                ScopeLink sl,
                Type udr_type,
                AST_t &parsed_tree)
        {
            std::stringstream ss;
            ss << "#line " << reference_tree.get_line() << " \"" << reference_tree.get_file() << "\"\n";

            std::string parsed_string = ss.str() + "@OMP_UDR_IDENTITY@ ";

            std::string constructor_str = "constructor";

            // Replace 'constructor' with a special token (otherwise
            // 'constructor'syntax  would be seen as a plain function call)
            if (omp_udr_identity.substr(0, constructor_str.size()) == constructor_str)
            {
                parsed_string += "@OMP_UDR_CONSTRUCTOR@" + omp_udr_identity.substr(constructor_str.size());
            }
            else
            {
                parsed_string += omp_udr_identity;
            }

            char *str = strdup(parsed_string.c_str());

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
                running_error("Could not parse OpenMP user-defined reduction identity\n\n%s\n", 
                        TL::Source::format_source(parsed_string).c_str());
            }

            Scope sc = sl.get_scope(reference_tree);
            decl_context_t decl_context = sc.get_decl_context();

            if (ASTType(a) != AST_OMP_UDR_CONSTRUCTOR)
            {
                check_initializer_clause(a, decl_context, udr_type.get_internal_type());
            }
            else
            {
                AST omp_udr_args = ASTSon0(a);
                AST expr_list = ASTSon0(omp_udr_args);

                if (expr_list != NULL)
                {
                    check_expression_list(expr_list, decl_context);
                }
            }

            parsed_tree = AST_t(a);

            free(str);
        }

        static bool solve_overload_for_udr(ObjectList<Symbol> operator_list, Type reduction_type,
                UDRInfoItem::Associativity &assoc,
                ObjectList<Symbol> &all_viables, 
                ObjectList<Symbol> &tentative_result,
                const std::string& filename, int line)
        {
            bool found_valid = false;
            ObjectList<udr_valid_prototypes_t> valid_prototypes = get_valid_prototypes_cxx(reduction_type);
            ObjectList<udr_valid_member_prototypes_t> valid_member_prototypes 
                = get_valid_member_prototypes_cxx(reduction_type);

            ObjectList<Symbol> members_set = operator_list.filter(OnlyMembers());
            ObjectList<Symbol> non_members_set = operator_list.filter(OnlyNonMembers());

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
                        && function_is_valid_udr_reductor_cxx(valid_prototypes, 
                            valid_member_prototypes, 
                            reduction_type, 
                            solved_sym,
                            assoc))
                {
                    tentative_result.insert(solved_sym);
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
                            reduction_type, // implicit 
                            arguments,
                            filename,
                            line,
                            valid,
                            viable_functs,
                            argument_conversor);

                    all_viables.insert(viable_functs);

                    if (valid 
                            && function_is_valid_udr_reductor_cxx(valid_prototypes, valid_member_prototypes, 
                                reduction_type, 
                                solved_sym, assoc))
                    {
                        tentative_result.insert(solved_sym);
                    }
                }
            }

            return (tentative_result.size() == 1);
        }

        static bool is_operator_tree(AST_t a)
        {
            return (a.internal_ast_type_() == AST_OMP_UDR_BUILTIN_OP);
        }

        static bool is_member_tree(AST_t a)
        {
            return (a.internal_ast_type_() == AST_OMP_UDR_MEMBER_OP);
        }

        void Core::declare_reduction_handler_pre(PragmaCustomConstruct construct)
        {
            if (_new_udr)
            {
                declare_reduction_handler_pre_2(construct);
                return;
            }

            DEBUG_CODE()
            {
                std::cerr << "=== Declare reduction [" << construct.get_ast().get_locus() << "]===" << std::endl;
            }

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

            // Common properties
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
                    assoc = UDRInfoItem::LEFT;
                }
                else
                {
                    std::cerr << construct.get_ast().get_locus() 
                        << ": warning: ignoring invalid 'order' clause argument '" 
                        << str << "'"
                        << std::endl;
                }
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
                ObjectList<Symbol> op_symbols;

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


                for (ObjectList<AST_t>::iterator op_it = op_args.begin();
                        op_it != op_args.end();
                        op_it++)
                {
                    // New udr being declared
                    UDRInfoItem new_udr;

                    new_udr.set_is_commutative(is_commutative);

                    new_udr.set_is_array_reduction(is_array);
                    if (is_array)
                    {
                        new_udr.set_num_dimensions(num_dimensions);
                    }
                    new_udr.set_reduction_type(reduction_type);

                    AST_t& op_name(*op_it);

                    // Perform lookup and further checking
                    C_LANGUAGE()
                    {
                        // In C, only names can be declared, builtins are reserved
                        op_symbols = construct.get_scope().get_symbols_from_id_expr(op_name);

                        if (op_symbols.empty())
                        {
                            running_error("%s: error: reduction operator '%s' not found in the current scope",
                                    construct.get_ast().get_locus().c_str(),
                                    op_name.prettyprint().c_str());
                        }

                        if (!is_array)
                        {
                            ObjectList<udr_valid_prototypes_t> valid_prototypes = get_valid_prototypes_c(reduction_type);

                            if (!function_is_valid_udr_reductor_c(valid_prototypes, op_symbols[0], assoc))
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
                                        op_symbols[0], 
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
                    }

                    CXX_LANGUAGE()
                    {
                        // Handle '.member' syntax
                        if (op_name.prettyprint()[0] == '.')
                        {
                            Source src;

                            src << reduction_type.get_declaration(construct.get_scope(), "") << "::" << 
                                op_name.prettyprint().substr(1)
                                ;

                            op_name = src.parse_id_expression(ref_tree_of_clause, construct.get_scope_link());
                        }

                        if (is_template)
                        {
                            new_udr.set_is_template_reduction(is_template);
                        }

                        if (is_operator_tree(op_name))
                        {
                            // We must do two lookups actually
                            if (reduction_type.is_class()
                                    && !reduction_type.is_dependent())
                            {
                                Source src;
                                src << reduction_type.get_declaration(construct.get_scope(), "") << "::operator " << op_name.prettyprint();

                                AST_t tree = src.parse_id_expression(ref_tree_of_clause, construct.get_scope_link());

                                // Lookup first in the class
                                op_symbols = construct.get_scope().get_symbols_from_id_expr(tree, /* examine_uninstantiated */ false);

                                if (!op_symbols.empty()
                                        && op_symbols[0].is_dependent_entity())
                                {
                                    op_symbols.clear();
                                }
                            }
                            //
                            // FIXME - We should do Koenig lookup here
                            Source src;
                            src << "operator " << op_name.prettyprint();

                            AST_t tree = src.parse_id_expression(ref_tree_of_clause, construct.get_scope_link());

                            op_symbols.insert(construct.get_scope().get_symbols_from_id_expr(tree, /* examine_uninstantiated */ false));
                        }
                        else
                        {
                            op_symbols = construct.get_scope().get_symbols_from_id_expr(op_name,
                                    /* examine_uninstantiated */ false);
                        }

                        if (reduction_type.is_dependent()
                                && !is_template)
                        {
                            running_error("%s: error dependent types are not (yet) supported "
                                    "unless specified in a template user-defined-reduction\n",
                                    construct.get_ast().get_locus().c_str());
                        }

                        if (!op_symbols.empty()
                                && !op_symbols[0].is_dependent_entity())
                        {
                            ObjectList<Symbol> viable_operators;
                            ObjectList<Symbol> tentative_result;
                            if (!solve_overload_for_udr(op_symbols, reduction_type, assoc, 
                                        viable_operators, 
                                        tentative_result,
                                        construct.get_ast().get_file(),
                                        construct.get_ast().get_line()))
                            {
                                if (!viable_operators.empty())
                                {
                                    if (!tentative_result.empty())
                                    {
                                        std::cerr << construct.get_ast().get_locus() 
                                            << ": note: more than one function is valid for the user-defined reduction" 
                                            << std::endl;
                                    }
                                    else
                                    {
                                        std::cerr << construct.get_ast().get_locus() 
                                            << ": note: no function is valid for the user-defined reduction" 
                                            << std::endl;
                                    }
                                    std::cerr << construct.get_ast().get_locus() 
                                        << ": note: candidates are"
                                        << std::endl;
                                    for (ObjectList<Symbol>::iterator it = viable_operators.begin();
                                            it != viable_operators.end();
                                            it++)
                                    {
                                        Symbol &sym(*it);
                                        std::cerr << construct.get_ast().get_locus() << ": note:    " 
                                            << sym.get_type().get_declaration(sym.get_scope(), sym.get_qualified_name())
                                            << std::endl;
                                    }
                                }
                                running_error("%s: error: cannot determine operator for user-defined reduction\n",
                                        construct.get_ast().get_locus().c_str());
                            }

                            op_symbols = tentative_result;
                        }
                    }

                    DEBUG_CODE()
                    {
                        std::cerr << "UDR: Associativity (L=1, R=2, U=3): " << assoc << std::endl;
                    }

                    // Associativity is fully defined now after the lookups,
                    // this is so because it might be left undefined so it is
                    // left up to the compiler to deduce it
                    new_udr.set_associativity(assoc);

                    AST_t identity_expr(NULL);
                    PragmaCustomClause identity_clause = construct.get_clause("identity");
                    if (identity_clause.is_defined())
                    {
                        std::string identity_str = identity_clause.get_arguments(ExpressionTokenizerTrim())[0];

                        parse_udr_identity(identity_str, ref_tree_of_clause, 
                                construct.get_scope_link(), reduction_type, identity_expr);
                    }

                    if (!identity_clause.is_defined())
                    {
                        std::string initializer;
                        C_LANGUAGE()
                        {
                            initializer = get_valid_zero_initializer(reduction_type);
                        }
                        CXX_LANGUAGE()
                        {
                            initializer = get_valid_value_initializer(reduction_type);
                        }

                        AST_t default_identity_expr;
                        parse_udr_identity(initializer, ref_tree_of_clause, 
                                construct.get_scope_link(), reduction_type, default_identity_expr);
                        new_udr.set_identity(default_identity_expr);
                    }
                    else
                    {
                        new_udr.set_identity(identity_expr);
                    }

                    if (!op_symbols.empty()
                            && !op_symbols[0].is_dependent_entity())
                    {
                        new_udr.set_operator_symbols(op_symbols);
                    }
                    new_udr.set_operator(IdExpression(op_name, construct.get_scope_link()));

                    bool found = false;
                    ObjectList<Symbol> all_viables;
                    new_udr.lookup_udr(construct.get_scope(), 
                            construct.get_scope_link(),
                            found, 
                            all_viables, 
                            construct.get_ast().get_file(),
                            construct.get_ast().get_line());

                    if (!found)
                    {
                        new_udr.sign_in_scope(construct.get_scope());

                        if (!is_template)
                        {
                            if (!is_array)
                            {
                                std::cerr << construct.get_ast().get_locus() << ": note: declaring user-defined reduction for type '"
                                    << reduction_type.get_declaration(scope_of_clause, "") << "'"
                                    << " and operator '" 
                                    << op_symbols[0].get_type().get_declaration(scope_of_clause, op_symbols[0].get_qualified_name(scope_of_clause)) << "'"
                                    << std::endl;
                            }
                            else
                            {
                                std::cerr << construct.get_ast().get_locus() << ": note: declaring user-defined reduction for "
                                    << num_dimensions << "-dimensional arrays of type '"
                                    << reduction_type.get_declaration(scope_of_clause, "") << "'"
                                    << " and operator '" 
                                    << op_symbols[0].get_type().get_declaration(scope_of_clause, op_symbols[0].get_qualified_name(scope_of_clause)) << "'"
                                    << std::endl;
                            }
                        }
                        else
                        {
                            std::cerr << construct.get_ast().get_locus() << ": note: declaring template user-defined reduction for type '"
                                << reduction_type.get_declaration(scope_of_clause, "") << "'"
                                << " and operator '" 
                                << op_name << "'"
                                << std::endl;
                        }
                    }
                    else
                    {
                        running_error("%s: error: user defined reduction for type '%s' and operator '%s' already defined",
                                construct.get_ast().get_locus().c_str(),
                                reduction_type.get_declaration(construct.get_scope_link().get_scope(construct.get_ast()), "").c_str(),
                                op_name.prettyprint().c_str());
                    }
                }
            }
        }

        void Core::declare_reduction_handler_post(PragmaCustomConstruct construct) 
		{
			if (_new_udr)
				declare_reduction_handler_post_2(construct); 
		}

        UDRInfoItem::UDRInfoItem()
            : _assoc(NONE), 
            _is_builtin(false), 
            _builtin_op(""), 
            _op_expr(NULL, ScopeLink()), 
            _op_symbols(),
            _reduction_type(NULL), 
            _is_template(false), 
            _is_array(false), 
            _num_dimensions(0),
            _is_commutative(false),
            _has_identity(false),
            _identity(NULL),
            _deduction_function(NULL)
        {
        }

        void UDRInfoItem::set_associativity(Associativity assoc)
        {
            _assoc = assoc;
        }

        UDRInfoItem::Associativity UDRInfoItem::get_associativity() const
        {
            return _assoc;
        }

        void UDRInfoItem::set_builtin_operator(const std::string& str)
        {
            _builtin_op = str;
            _is_builtin = true;
        }

        bool UDRInfoItem::is_builtin_operator() const
        {
            return _is_builtin;
        }
        std::string UDRInfoItem::get_builtin_operator() const
        {
            return _builtin_op;
        }

        void UDRInfoItem::set_operator(IdExpression id_expr)
        {
            _op_expr = id_expr;
            _is_builtin = false;
        }

        ObjectList<Symbol> UDRInfoItem::get_operator_symbols() const
        {
            return _op_symbols;
        }

        void UDRInfoItem::set_operator_symbols(const ObjectList<Symbol>& sym_list) 
        {
            _op_symbols = sym_list;
        }

        IdExpression UDRInfoItem::get_operator() const
        {
            return _op_expr;
        }

        void UDRInfoItem::set_reduction_type(Type t)
        {
            _reduction_type = t;
        }

        Type UDRInfoItem::get_reduction_type() const
        {
            return _reduction_type;
        }

        void UDRInfoItem::set_is_array_reduction(bool b)
        {
            _is_array = b;
        }

        bool UDRInfoItem::get_is_array_reduction() const
        {
            return _is_array;
        }

        void UDRInfoItem::set_num_dimensions(int n)
        {
            _num_dimensions = n;
        }

        int  UDRInfoItem::get_num_dimensions() const
        {
            return _num_dimensions;
        }

        void UDRInfoItem::set_is_template_reduction(bool b)
        {
            _is_template = b;
        }

        bool UDRInfoItem::get_is_template_reduction() const
        {
            return _is_template;
        }

        std::string UDRInfoItem::get_symbol_name() const
        {
            if (_is_builtin)
            {
                return ".udr_" + _builtin_op;
            }
            else if (is_operator_tree(_op_expr.get_ast()))
            {
                return ".udr_" + _op_expr.prettyprint();
            }
            else if (is_member_tree(_op_expr.get_ast()))
            {
                return ".udr_" + _op_expr.prettyprint();
            }
            else
            {
                return ".udr_" + _op_expr.get_unqualified_part();
            }
        }

        bool UDRInfoItem::get_is_commutative() const
        {
            return _is_commutative;
        }

        void UDRInfoItem::set_is_commutative(bool b)
        {
            _is_commutative = b;
        }


        UDRInfoItem UDRInfoItem::lookup_udr(Scope sc, 
                ScopeLink sl,
                bool &found, 
                ObjectList<Symbol> &all_viables, 
                const std::string& filename, int line) const
        {
            DEBUG_CODE()
            {
                std::cerr << "UDR: Lookup start" << std::endl;
            }

            const UDRInfoItem& current_udr = *this;

            found = false;
            UDRInfoItem empty_udr;

            ObjectList<UDRInfoItem> udr_lookup;
            {
                ObjectList<Symbol> lookup = sc.cascade_lookup(current_udr.get_symbol_name(), filename, line);
                if (lookup.empty())
                {
                    return empty_udr;
                }

                for (ObjectList<Symbol>::iterator it = lookup.begin();
                        it != lookup.end();
                        it++)
                {
                    Symbol &sym(*it);
                    RefPtr<UDRInfoItem> obj = 
                        RefPtr<UDRInfoItem>::cast_dynamic(sym.get_attribute("udr_info"));

                    udr_lookup.append(*obj);
                }
            }

            // Now filter the udr info item
            C_LANGUAGE()
            {
                UDRInfoItem result;
                for (ObjectList<UDRInfoItem>::iterator it = udr_lookup.begin();
                        it != udr_lookup.end() && !found;
                        it++)
                {
                    UDRInfoItem& obj(*it);

                    // If they are array reductions their dimensions match
                    if (obj.get_reduction_type().is_same_type(current_udr.get_reduction_type())
                            && ((obj.get_is_array_reduction() 
                                    == current_udr.get_is_array_reduction())
                                && (!obj.get_is_array_reduction()
                                    || (obj.get_num_dimensions() == current_udr.get_num_dimensions()))))
                    {
                        result = obj;
                        // There is only one in C
                        Symbol op_sym = obj.get_operator_symbols()[0];
                        all_viables.insert(op_sym);
                    }
                }

                if (all_viables.size() != 1)
                {
                    return empty_udr;
                }
                else 
                {
                    found = true;
                    return result;
                }
            }

            // Only C++ here
            ObjectList<UDRInfoItem> templated_udrs;
            ObjectList<UDRInfoItem> viable_udr;

            for (ObjectList<UDRInfoItem>::iterator it = udr_lookup.begin();
                    it != udr_lookup.end(); it++)
            {
                if (it->get_is_template_reduction())
                {
                    templated_udrs.append(*it);
                }
                // If they are array reductions their dimensions match
                else if ((current_udr.get_is_array_reduction()
                            == it->get_is_array_reduction())
                        && (!it->get_is_array_reduction()
                            || (it->get_num_dimensions() 
                                == current_udr.get_num_dimensions())))
                {
                    viable_udr.append(*it);
                }
            }

            if (!templated_udrs.empty())
            {
                // Expand the templates
                for (ObjectList<UDRInfoItem>::iterator it = templated_udrs.begin();
                        it != templated_udrs.end(); 
                        it++)
                {
                    UDRInfoItem& obj(*it);

                    Symbol deduction_function = obj.get_deduction_function();
                    ObjectList<Symbol> candidates;
                    candidates.append(deduction_function);

                    ObjectList<Type> arguments;
                    arguments.append(current_udr.get_reduction_type());

                    bool valid = false;
                    ObjectList<Symbol> argument_conversor;
                    ObjectList<Symbol> viable_functs;
                    Symbol solved_sym = Overload::solve(
                            candidates,
                            Type(NULL), // No implicit
                            arguments,
                            filename,
                            line,
                            valid,
                            viable_functs,
                            argument_conversor);

                    if (valid)
                    {
                        template_argument_list_t* template_arguments =
                            template_specialized_type_get_template_arguments(solved_sym.get_type().get_internal_type());

                        decl_context_t updated_context = update_context_with_template_arguments(
                                sc.get_decl_context(),
                                template_arguments);

                        Scope updated_scope(updated_context);

                        ObjectList<Symbol> sym_list;
                        AST_t op_name = obj.get_operator().get_ast();
                        if (is_operator_tree(op_name))
                        {
                            Type reduction_type = current_udr.get_reduction_type();
                            if (reduction_type.is_class()
                                    && !reduction_type.is_dependent())
                            {
                                Source src;
                                src << reduction_type.get_declaration(sc, "") << "::operator " << op_name.prettyprint();

                                AST_t tree = src.parse_id_expression(op_name, sl);

                                // Lookup first in the class
                                sym_list = sc.get_symbols_from_id_expr(tree, /* examine_uninstantiated */ false);

                                if (!sym_list.empty()
                                        && sym_list[0].is_dependent_entity())
                                {
                                    sym_list.clear();
                                }
                            }
                            if (sym_list.empty())
                            {
                                Source src;
                                src << "operator " << op_name.prettyprint();

                                AST_t tree = src.parse_id_expression(op_name, sl);

                                // Lookup first in the class
                                sym_list = sc.get_symbols_from_id_expr(tree, /* examine_uninstantiated */ false);
                            }

                        }
                        else
                        {
                            sym_list = updated_scope.get_symbols_from_id_expr(obj.get_operator().get_ast());
                        }

                        if (!sym_list.empty())
                        {
                            // Instantiate the UDR with this new info and add it to the viable_udr
                            UDRInfoItem new_udr(obj);
                            // Fix the operator symbols
                            new_udr.set_operator_symbols(sym_list);

                            // EXPERIMENTAL CODE
                            // "Instantiate" the identity
                            if (obj.has_identity())
                            {
                                AST inst_tree = instantiate_tree(obj.get_raw_identity().get_internal_ast(), 
                                        updated_context);
                                new_udr.set_identity(AST_t(inst_tree));
                            }

                            if ((new_udr.get_is_array_reduction()
                                        == current_udr.get_is_array_reduction())
                                    && (!new_udr.get_is_array_reduction()
                                        || (new_udr.get_num_dimensions() 
                                            == current_udr.get_num_dimensions())))
                            {
                                viable_udr.append(new_udr);
                            }
                        }
                    }
                }
            }

            // Construct the symbol list
            bool found_valid = false;
            UDRInfoItem result;

            ObjectList<udr_valid_prototypes_t> valid_prototypes = get_valid_prototypes_cxx(current_udr.get_reduction_type());
            ObjectList<udr_valid_member_prototypes_t> valid_member_prototypes 
                = get_valid_member_prototypes_cxx(current_udr.get_reduction_type());

            ObjectList<Symbol> tentative_result;
            for (ObjectList<UDRInfoItem>::iterator it = viable_udr.begin();
                    it != viable_udr.end(); it++)
            {
                UDRInfoItem& obj(*it);

                ObjectList<Symbol> operator_list;
                operator_list.insert(obj.get_operator_symbols());

                ObjectList<Symbol> members_set = operator_list.filter(OnlyMembers());
                ObjectList<Symbol> non_members_set = operator_list.filter(OnlyNonMembers());

                ObjectList<Symbol> all_viables;
                // First the set of nonmembers
                for (ObjectList<udr_valid_prototypes_t>::iterator it = valid_prototypes.begin();
                        it != valid_prototypes.end();
                        it++)
                {
                    DEBUG_CODE()
                    {
                        std::cerr << "UDR: Testing prototype (" 
                            << it->first_arg.get_declaration(sc, "") << ", "
                            << it->second_arg.get_declaration(sc, "") << ")" << std::endl;
                    }
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

                    Associativity assoc = UNDEFINED;

                    if (valid 
                            && function_is_valid_udr_reductor_cxx(valid_prototypes, 
                                valid_member_prototypes, 
                                current_udr.get_reduction_type(), 
                                solved_sym,
                                assoc))
                    {
                        tentative_result.insert(solved_sym);
                        found_valid = true;

                        result = obj;
                        ObjectList<Symbol> solved_syms;
                        solved_syms.append(solved_sym);
                        result.set_operator_symbols(solved_syms);
                    }
                }

                if (!found_valid
                        && current_udr.get_reduction_type().is_named_class())
                {
                    // Do likewise for members this time
                    for (ObjectList<udr_valid_member_prototypes_t>::iterator it = valid_member_prototypes.begin();
                            it != valid_member_prototypes.end();
                            it++)
                    {
                        ObjectList<Type> arguments;
                        arguments.append(it->first_arg);

                        DEBUG_CODE()
                        {
                            std::cerr << "UDR: Testing prototype (" 
                                << it->first_arg.get_declaration(sc, "") << ")"
                                << std::endl;
                        }

                        bool valid = false;
                        ObjectList<Symbol> argument_conversor;
                        ObjectList<Symbol> viable_functs;
                        Symbol solved_sym = Overload::solve(
                                members_set,
                                current_udr.get_reduction_type().get_reference_to(), // implicit argument (it must be a reference)
                                arguments,
                                filename,
                                line,
                                valid,
                                viable_functs,
                                argument_conversor);

                        all_viables.insert(viable_functs);

                        Associativity assoc = UNDEFINED;

                        if (valid 
                                && function_is_valid_udr_reductor_cxx(valid_prototypes, valid_member_prototypes, 
                                    current_udr.get_reduction_type(), 
                                    solved_sym, assoc))
                        {
                            tentative_result.insert(solved_sym);

                            result = obj;
                            ObjectList<Symbol> solved_syms;
                            solved_syms.append(solved_sym);
                            result.set_operator_symbols(solved_syms);
                        }
                    }
                }
            }

            DEBUG_CODE()
            {
                std::cerr << "UDR: Lookup end" << std::endl;
                std::cerr << "UDR: Candidate num elements: " << tentative_result.size() << std::endl;

                if (!tentative_result.empty())
                {
                    for (ObjectList<Symbol>::iterator it = tentative_result.begin();
                            it != tentative_result.end();
                            it++)
                    {
                        std::cerr << "UDR: Candidate: " 
                            << it->get_type().get_declaration(it->get_scope(), it->get_qualified_name()) << std::endl;
                    }
                }
                else
                {
                    std::cerr << "UDR: No candidates" << std::endl;
                }
            }

            if (tentative_result.size() != 1)
            {
                return empty_udr;
            }
            else 
            {
                found = 1;
                return result;
            }
        }

        void UDRInfoItem::sign_in_scope(Scope sc) const
        {
            Symbol sym = sc.new_artificial_symbol(this->get_symbol_name());

            RefPtr<UDRInfoItem> cp(new UDRInfoItem(*this));

            sym.set_attribute("udr_info", cp);

            DEBUG_CODE()
            {
                std::cerr << "UDR: Signing in " << this->get_symbol_name() << std::endl;
            }
        }

        bool UDRInfoItem::has_identity() const
        {
            return _has_identity;
        }

        void UDRInfoItem::set_identity(AST_t identity)
        {
            _identity = identity;
            _has_identity = _identity.is_valid();
        }

        AST_t UDRInfoItem::get_identity() const
        {
            if (identity_is_constructor())
            {
                return _identity.children()[0];
            }
            else
                return _identity;
        }

        AST_t UDRInfoItem::get_raw_identity() const
        {
            return _identity;
        }

        bool UDRInfoItem::identity_is_constructor() const
        {
            if (_identity.is_valid())
            {
                return _identity.internal_ast_type_() == AST_OMP_UDR_CONSTRUCTOR;
            }
            else 
                return false;
        }

        Symbol UDRInfoItem::get_deduction_function() 
        {
            if (!_is_template)
                return Symbol(NULL);

            if (!_deduction_function.is_valid())
            {
                type_t* reduction_type = _reduction_type.get_internal_type();
                scope_entry_t* entry = (scope_entry_t*)calloc(1, sizeof(*entry));
                entry->kind = SK_TEMPLATE;

                parameter_info_t parameter_info[1] = 
                {
                    { /* .is_ellipsis */ 0, reduction_type, reduction_type }
                };

                type_t* primary_type = get_new_function_type(get_void_type(),
                        parameter_info, 1);

                entry->symbol_name = uniquestr(".udr_function");
                entry->type_information =
                    get_new_template_type(
                            // template_specialized_type_get_template_parameters(get_actual_class_type(reduction_type)),
                            named_type_get_symbol(reduction_type)->decl_context.template_parameters,
                            primary_type,
                            uniquestr(".udr_function"),
                            // This will fail for non named types!
                            named_type_get_symbol(reduction_type)->decl_context,
                            0, uniquestr("(null)"));

                entry->decl_context = named_type_get_symbol(reduction_type)->decl_context;

                _deduction_function = Symbol(entry);
            }

            return _deduction_function;
        }
    }
}
