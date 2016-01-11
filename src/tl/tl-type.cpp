/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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




#include "tl-symbol.hpp"
#include "tl-type.hpp"
#include "tl-scope.hpp"
#include "tl-nodecl.hpp"
#include "tl-member-decl.hpp"
#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include "cxx-scope.h"
#include "cxx-cexpr.h"
#include "cxx-exprtype.h"
#include "cxx-entrylist.h"

#include "fortran03-typeutils.h"

#include "codegen-phase.hpp"
#include "codegen-fortran.hpp"

namespace TL
{
    Type Type::fix_references()
    {
        TL::Type fixed_type = this->fix_references_();

        if (this->is_same_type(fixed_type))
            // We try hard to preserve types unchanged
            return *this;
        else
            return fixed_type;
    }

    Type Type::fix_references_()
    {
        if ((IS_C_LANGUAGE && this->is_any_reference())
                || (IS_CXX_LANGUAGE && this->is_rebindable_reference()))
        {
            TL::Type ref = this->references_to();
            if (ref.is_array())
            {
                // T (&a)[10] -> T * const
                // T (&a)[10][20] -> T (* const)[20]
                ref = ref.array_element();
            }

            // T &a -> T * const a
            TL::Type ptr = ref.get_pointer_to();
            if (!this->is_rebindable_reference())
            {
                ptr = ptr.get_const_type();
            }
            return ptr;
        }
        else if (IS_FORTRAN_LANGUAGE && this->is_any_reference())
        {
            return this->references_to();
        }
        else if (this->is_array())
        {
            if (this->array_is_region())
            {
                Nodecl::NodeclBase lb, reg_lb, ub, reg_ub;
                this->array_get_bounds(lb, ub);
                this->array_get_region_bounds(reg_lb, reg_ub);
                TL::Scope sc = array_type_get_region_size_expr_context(this->get_internal_type());

                return this->array_element().fix_references_().get_array_to_with_region(lb, ub, reg_lb, reg_ub, sc);
            }
            else
            {
                Nodecl::NodeclBase size = this->array_get_size();
                TL::Scope sc = array_type_get_array_size_expr_context(this->get_internal_type());

                return this->array_element().fix_references_().get_array_to(size, sc);
            }
        }
        else if (this->is_pointer())
        {
            TL::Type fixed = this->points_to().fix_references_().get_pointer_to();

            cv_qualifier_t cv_qualif = CV_NONE;
            ::advance_over_typedefs_with_cv_qualif(this->get_internal_type(), &cv_qualif);

            fixed = ::get_cv_qualified_type(fixed.get_internal_type(), cv_qualif);

            return fixed;
        }
        else if (this->is_function())
        {
            // Do not fix unprototyped functions
            if (this->lacks_prototype())
                return (*this);

            cv_qualifier_t cv_qualif = CV_NONE;
            ::advance_over_typedefs_with_cv_qualif(this->get_internal_type(), &cv_qualif);

            ref_qualifier_t ref_qualifier = function_type_get_ref_qualifier(this->get_internal_type());
            TL::Type fixed_result = this->returns().fix_references_();
            bool has_ellipsis = 0;

            TL::ObjectList<TL::Type> fixed_parameters = this->parameters(has_ellipsis);
            for (TL::ObjectList<TL::Type>::iterator it = fixed_parameters.begin();
                    it != fixed_parameters.end();
                    it++)
            {
                *it = it->fix_references_();
            }

            TL::ObjectList<TL::Type> nonadjusted_fixed_parameters = this->nonadjusted_parameters();
            for (TL::ObjectList<TL::Type>::iterator it = nonadjusted_fixed_parameters.begin();
                    it != nonadjusted_fixed_parameters.end();
                    it++)
            {
                *it = it->fix_references_();
            }

            TL::Type fixed_function = fixed_result.get_function_returning(
                    fixed_parameters,
                    nonadjusted_fixed_parameters,
                    has_ellipsis,
                    ref_qualifier);

            fixed_function = TL::Type(get_cv_qualified_type(fixed_function.get_internal_type(), cv_qualif));

            return fixed_function;
        }
        // Note: we are not fixing classes
        else
        {
            // Anything else must be left untouched
            return (*this);
        }
    }

    std::string Type::get_declaration_with_initializer(Scope sc, const std::string& symbol_name,
            const std::string& initializer, TypeDeclFlags flags) const
    {
        return get_declaration_string(_type_info, sc._decl_context, symbol_name.c_str(),
                initializer.c_str(), 0, 0, NULL, NULL, flags == PARAMETER_DECLARATION);
    }

    std::string Type::get_declaration_with_parameters(Scope sc, const std::string& symbol_name,
            ObjectList<std::string>& parameters, ObjectList<std::string>& parameter_attributes, TypeDeclFlags flags) const
    {
        int num_parameters = this->parameters().size();

        const char** parameter_names  = new const char*[num_parameters + 1];
        const char** param_attributes = new const char*[num_parameters + 1];

        for (int i = 0; i < num_parameters; i++)
        {
            parameter_names[i] = NULL;
            param_attributes[i] = NULL;
        }

        int orig_size = parameters.size();
        for (int i = 0; i < orig_size; i++)
        {
            parameter_names[i] = uniquestr(parameters[i].c_str());
            param_attributes[i] = uniquestr(parameter_attributes[i].c_str());
        }

        const char* result = get_declaration_string(_type_info, sc._decl_context, symbol_name.c_str(),
                "", 0, num_parameters, parameter_names, param_attributes, flags == PARAMETER_DECLARATION);

        for (int i = 0; i < num_parameters; i++)
        {
            if (i < orig_size)
            {
                parameters[i] = parameter_names[i];
            }
            else
            {
                if (parameter_names[i] != NULL)
                    parameters.append(parameter_names[i]);
                else
                    parameters.append("");
            }
        }

        delete[] parameter_names;
        delete[] param_attributes;

        return result;
    }

    std::string Type::get_simple_declaration(Scope sc, const std::string&
            symbol_name, TypeDeclFlags flags) const
    {
        return get_declaration_string(_type_info, sc._decl_context,
                symbol_name.c_str(), "", 0, 0, NULL, NULL, flags == PARAMETER_DECLARATION);
    }

    std::string Type::get_declaration(Scope sc, const std::string& symbol_name,
            TypeDeclFlags flags) const
    {
        return get_declaration_string(_type_info, sc._decl_context,
                symbol_name.c_str(), "", 0, 0, NULL, NULL, flags == PARAMETER_DECLARATION);
    }

    std::string Type::get_fortran_declaration(Scope sc, const std::string& symbol_name,
            TypeDeclFlags flags)
    {
        if (!IS_FORTRAN_LANGUAGE)
        {
            fatal_error("This function cannot be called if we are not in Fortran");
        }

        Codegen::FortranBase &codegen_phase = static_cast<Codegen::FortranBase&>(Codegen::get_current());

        std::string type_specifier, array_specifier;
        codegen_phase.codegen_type(*this, type_specifier, array_specifier);

        return type_specifier + " :: " + symbol_name + array_specifier;
    }

    Type Type::get_pointer_to() const
    {
        type_t* work_type = this->_type_info;

        if (is_any_reference())
        {
            // We cannot get a pointer to a reference, get the referenced
            // type and make it pointer
            work_type = reference_type_get_referenced_type(work_type);
        }

        type_t* result_type = get_pointer_type(work_type);

        return result_type;
    }

    Type Type::get_vector_of_bytes(unsigned int vector_size) const
    {
        type_t* work_type = this->_type_info;

        type_t* result_type = get_vector_type_by_bytes(work_type, vector_size);

        return result_type;
    }

    Type Type::get_vector_of_elements(unsigned int num_elements) const
    {
        type_t* work_type = this->_type_info;

        type_t* result_type = get_vector_type_by_elements(work_type, num_elements);

        return result_type;
    }

    Type Type::get_generic_vector_to()
    {
        type_t* work_type = this->_type_info;

        type_t* result_type = get_generic_vector_type(work_type);

        return result_type;
    }

    Type Type::get_array_to(Nodecl::NodeclBase array_expr, Scope sc)
    {
        type_t* result_type = this->_type_info;

        const decl_context_t* decl_context = sc.get_decl_context();

        type_t* array_to = get_array_type(result_type, array_expr.get_internal_nodecl(), decl_context);

        return Type(array_to);
    }

    Type Type::get_array_to(Nodecl::NodeclBase lower_bound, Nodecl::NodeclBase upper_bound, Scope sc)
    {
        type_t* result_type = this->_type_info;

        const decl_context_t* decl_context = sc.get_decl_context();

        type_t* array_to = get_array_type_bounds(result_type,
                lower_bound.get_internal_nodecl(),
                upper_bound.get_internal_nodecl(),
                decl_context);

        return Type(array_to);
    }

    Type Type::get_array_to_with_descriptor(Nodecl::NodeclBase lower_bound, Nodecl::NodeclBase upper_bound, Scope sc)
    {
        type_t* result_type = this->_type_info;

        const decl_context_t* decl_context = sc.get_decl_context();

        type_t* array_to = get_array_type_bounds_with_descriptor(result_type,
                lower_bound.get_internal_nodecl(),
                upper_bound.get_internal_nodecl(),
                decl_context);

        return Type(array_to);
    }

    Type Type::get_array_to_with_region(Nodecl::NodeclBase lower_bound,
            Nodecl::NodeclBase upper_bound,
            Nodecl::NodeclBase region_lower_bound,
            Nodecl::NodeclBase region_upper_bound,
            Scope sc)
    {
        type_t* result_type = this->_type_info;

        const decl_context_t* decl_context = sc.get_decl_context();

        // Make the range of the region
        Nodecl::NodeclBase range = Nodecl::Range::make(
                region_lower_bound,
                region_upper_bound,
                const_value_to_nodecl(const_value_get_one(4, 1)),
                region_lower_bound.get_type(),
                region_lower_bound.get_locus());

        type_t* array_to = get_array_type_bounds_with_regions(
                result_type,
                lower_bound.get_internal_nodecl(),
                upper_bound.get_internal_nodecl(),
                decl_context,
                range.get_internal_nodecl(),
                decl_context);

        return array_to;
    }

    Type Type::get_array_to()
    {
        type_t* result_type = this->_type_info;

        const decl_context_t* null_decl_context;
        memset(&null_decl_context, 0, sizeof(null_decl_context));
        type_t* array_to = get_array_type(result_type, nodecl_null(), null_decl_context);

        return Type(array_to);
    }

    Type Type::get_function_returning(const ObjectList<Type>& type_list,
            const ObjectList<Type>& nonadjusted_type_list,
            bool has_ellipsis,
            ref_qualifier_t reference_qualifier)
    {
        int i;
        parameter_info_t *parameters_list;
        int num_parameters = type_list.size();
   
        parameters_list = NEW_VEC(parameter_info_t,
                num_parameters + has_ellipsis);

        for (i=0; i<num_parameters; i++)
        {
            parameters_list[i].is_ellipsis = 0;
            parameters_list[i].type_info = type_list[i]._type_info;
            parameters_list[i].nonadjusted_type_info = nonadjusted_type_list[i]._type_info;
        }

        if (has_ellipsis)
        {
            num_parameters++;
            parameters_list[i].is_ellipsis = 1;
            parameters_list[i].type_info = get_ellipsis_type();
            parameters_list[i].nonadjusted_type_info = NULL;
        }

        return (Type(get_new_function_type(_type_info, parameters_list, num_parameters, reference_qualifier)));
    }

    Type Type::get_function_returning(const ObjectList<Type>& type_list, bool has_ellipsis,
            ref_qualifier_t reference_qualifier)
    {
        ObjectList<Type> nonadjusted_type_list(type_list.size());

        return get_function_returning(type_list, nonadjusted_type_list, has_ellipsis, reference_qualifier);
    }

    ref_qualifier_t Type::get_reference_qualifier() const
    {
        return ::function_type_get_ref_qualifier(this->_type_info);
    }

    bool Type::is_error_type() const
    {
        return ::is_error_type(_type_info);
    }

    bool Type::operator<(Type t) const
    {
        return this->_type_info < t._type_info;
    }

    Type& Type::operator=(Type t)
    {
        this->_type_info = t._type_info;
        return (*this);
    }

    bool Type::is_pointer() const
    {
        return (is_pointer_type(_type_info));
    }

    bool Type::is_array() const
    {
        return (is_array_type(_type_info));
    }

    bool Type::is_fortran_array() const
    {
        return (fortran_is_array_type(_type_info));
    }

    int Type::fortran_rank() const
    {
        if (!is_fortran_array())
            return 0;
        else
            return (fortran_get_rank_of_type(_type_info));
    }

    bool Type::is_vector() const
    {
        return (::is_vector_type(_type_info));
    }

    bool Type::is_mask() const
    {
        return (::is_mask_type(_type_info));
    }

    int Type::get_mask_num_elements() const
    {
        return mask_type_get_num_bits(_type_info);
    }

    bool Type::is_generic_vector() const
    {
        return (is_generic_vector_type(_type_info));
    }

    Type Type::vector_element() const
    {
        return vector_type_get_element_type(_type_info);
    }
    
    bool Type::is_auto() const
    {
        return ::is_auto_type(_type_info);
    }

    bool Type::is_decltype_auto() const
    {
        return ::is_decltype_auto_type(_type_info);
    }

    int Type::vector_num_elements() const
    {
        return vector_type_get_num_elements(_type_info);
    }

    bool Type::is_any_reference() const
    {
        return (is_lvalue_reference_type(_type_info)
                || is_rvalue_reference_type(_type_info)
                || is_rebindable_reference());
    }

    bool Type::is_lvalue_reference() const
    {
        return (is_lvalue_reference_type(_type_info));
    }

    bool Type::is_rvalue_reference() const
    {
        return (is_rvalue_reference_type(_type_info));
    }

    bool Type::is_rebindable_reference() const
    {
        return (is_rebindable_reference_type(_type_info));
    }

    bool Type::is_function() const
    {
        return is_function_type(_type_info);
    }

    bool Type::is_dependent_typename() const
    {
        return ::is_dependent_typename_type(_type_info);
    }

    bool Type::is_dependent() const
    {
        return is_dependent_type(_type_info);
    }

    void Type::dependent_typename_get_components(Symbol& entry_symbol, Nodecl::NodeclBase& parts)
    {
        scope_entry_t* entry = NULL;
        nodecl_t n = nodecl_null();
        ::dependent_typename_get_components(_type_info, &entry, &n);

        entry_symbol = Symbol(entry);
        parts = Nodecl::NodeclBase(n);
    }

    bool Type::is_expression_dependent() const
    {
        return false;
    }

    bool Type::is_pack() const
    {
        return ::is_pack_type(_type_info);
    }

    TL::Type Type::pack_type_get_packed() const
    {
        return ::pack_type_get_packed_type(_type_info);
    }

    Type Type::returns() const
    {
        return function_type_get_return_type(_type_info);
    }

    ObjectList<Type> Type::parameters() const
    {
        bool b;
        return parameters(b);
    }

    ObjectList<Type> Type::parameters(bool& has_ellipsis) const
    {
        has_ellipsis = function_type_get_has_ellipsis(_type_info);

        ObjectList<Type> result;
        for (int i = 0; i < function_type_get_num_parameters(_type_info); i++)
        {
            // The last one is the ellipsis and lacks type
            if (has_ellipsis
                    && ((i + 1) == function_type_get_num_parameters(_type_info)))
            {
                break;
            }
            Type t(function_type_get_parameter_type_num(_type_info, i));
            result.push_back(t);
        }

        return result;
    }

    ObjectList<Type> Type::nonadjusted_parameters() const
    {
        bool b;
        return nonadjusted_parameters(b);
    }

    ObjectList<Type> Type::nonadjusted_parameters(bool& has_ellipsis) const
    {
        has_ellipsis = function_type_get_has_ellipsis(_type_info);

        ObjectList<Type> result;
        for (int i = 0; i < function_type_get_num_parameters(_type_info); i++)
        {
            // The last one is the ellipsis and lacks type
            if (has_ellipsis
                    && ((i + 1) == function_type_get_num_parameters(_type_info)))
            {
                break;
            }
            Type t(function_type_get_nonadjusted_parameter_type_num(_type_info, i));
            result.push_back(t);
        }

        return result;
    }

    Type Type::points_to() const
    {
        return pointer_type_get_pointee_type(_type_info);
    }

    Type Type::array_element() const
    {
        return array_type_get_element_type(_type_info);
    }

    bool Type::array_is_vla() const
    {
        return array_type_is_vla(_type_info);
    }

    Type Type::references_to() const
    {
        return reference_type_get_referenced_type(_type_info);
    }

    bool Type::is_non_derived_type() const
    {
        return this->is_scalar_type();
    }

    bool Type::is_scalar_type() const
    {
        return ::is_scalar_type(_type_info);
    }

    bool Type::is_direct_type() const
    {
        return this->is_non_derived_type();
    }

    bool Type::is_void() const
    {
        return is_void_type(_type_info);
    }

    bool Type::is_enum() const
    {
        return (::is_enum_type(_type_info));
    }

    bool Type::is_unnamed_enum() const
    {
        return (::is_enum_type(_type_info)
                && !is_named_type(_type_info));
    }

    bool Type::is_named_enum() const
    {
        return (::is_enum_type(_type_info)
                && is_named_type(_type_info));
    }

    Type Type::enum_get_underlying_type() const
    {
        return ::enum_type_get_underlying_type(_type_info);
    }

    bool Type::is_named() const
    {
        return is_named_type(_type_info);
    }

    bool Type::is_indirect() const
    {
        return is_indirect_type(_type_info);
    }

    Symbol Type::get_symbol() const
    {
        return named_type_get_symbol(_type_info);
    }

    bool Type::is_class() const
    {
        return (is_class_type(_type_info));
    }

    bool Type::is_named_class() const
    {
        return (is_named_class_type(_type_info));
    }

    bool Type::is_unnamed_class() const
    {
        return (is_unnamed_class_type(_type_info));
    }

    int Type::get_num_dimensions() const
    {
        Type t = *this;
        int n_dim = 0;
        while (t.is_array())
        {
            n_dim++;
            t = t.array_element();
        }
        return n_dim;
    }

    bool Type::array_has_size() const
    {
        if (is_array())
        {
            return (!array_type_is_unknown_size(_type_info));
        }

        return false;
    }

    bool Type::array_requires_descriptor() const
    {
        if (is_array())
        {
            return (array_type_with_descriptor(_type_info));
        }

        return false;
    }

    Nodecl::NodeclBase Type::array_get_size() const
    {
        return array_type_get_array_size_expr(_type_info);
    }

    void Type::array_get_bounds(Nodecl::NodeclBase& lower, Nodecl::NodeclBase& upper) const
    {
        lower = array_type_get_array_lower_bound(_type_info);
        upper = array_type_get_array_upper_bound(_type_info);
    }

    bool Type::array_is_region() const
    {
        return array_type_has_region(_type_info);
    }

    void Type::array_get_region_bounds(Nodecl::NodeclBase& region_lower, Nodecl::NodeclBase& region_upper) const
    {
        region_lower = array_type_get_region_lower_bound(_type_info);
        region_upper = array_type_get_region_upper_bound(_type_info);
    }

    Nodecl::NodeclBase Type::array_get_region_size() const
    {
        return array_type_get_region_size_expr(_type_info);
    }

    Type Type::get_void_type(void)
    {
        return Type(::get_void_type());
    }

    Type Type::get_bool_type(void)
    {
        return Type(::get_bool_type());
    }
    Type Type::get_char_type(void)
    {
        return Type(::get_char_type());
    }

    Type Type::get_unsigned_char_type(void)
    {
        return Type(::get_unsigned_char_type());
    }

    Type Type::get_short_int_type(void)
    {
        return Type(::get_signed_short_int_type());
    }

    Type Type::get_unsigned_short_int_type(void)
    {
        return Type(::get_unsigned_short_int_type());
    }

    Type Type::get_int_type(void)
    {
        return Type(::get_signed_int_type());
    }

    Type Type::get_unsigned_int_type(void)
    {
        return Type(::get_unsigned_int_type());
    }

    Type Type::get_long_int_type(void)
    {
        return Type(::get_signed_long_int_type());
    }

    Type Type::get_unsigned_long_int_type(void)
    {
        return Type(::get_unsigned_long_int_type());
    }

    Type Type::get_long_long_int_type(void)
    {
        return Type(::get_signed_long_long_int_type());
    }

    Type Type::get_unsigned_long_long_int_type(void)
    {
        return Type(::get_unsigned_long_long_int_type());
    }

    Type Type::get_float_type(void)
    {
        return Type(::get_float_type());
    }

    Type Type::get_double_type(void)
    {
        return Type(::get_double_type());
    }

    Type Type::get_mask_type(unsigned int mask_size)
    {
        return Type(::get_mask_type(mask_size));
    }

    Type Type::get_auto_type()
    {
        return Type(::get_auto_type());
    }

    bool Type::is_integral_type() const
    {
        return ::is_integral_type(_type_info);
    }

    bool Type::is_signed_integral() const
    {
        return is_signed_integral_type(_type_info);
    }

    bool Type::is_unsigned_integral() const
    {
        return is_unsigned_integral_type(_type_info);
    }

    bool Type::is_signed_int() const
    {
        return is_signed_int_type(_type_info);
    }

    bool Type::is_unsigned_int() const
    {
        return is_unsigned_int_type(_type_info);
    }

    bool Type::is_signed_short_int() const
    {
        return is_signed_short_int_type(_type_info);
    }

    bool Type::is_unsigned_short_int() const
    {
        return is_unsigned_short_int_type(_type_info);
    }

    bool Type::is_signed_long_int() const
    {
        return is_signed_long_int_type(_type_info);
    }

    bool Type::is_unsigned_long_int() const
    {
        return is_unsigned_long_int_type(_type_info);
    }

    bool Type::is_signed_long_long_int() const
    {
        return is_signed_long_long_int_type(_type_info);
    }

    bool Type::is_unsigned_long_long_int() const
    {
        return is_unsigned_long_long_int_type(_type_info);
    }


    bool Type::is_char() const
    {
        return is_char_type(_type_info);
    }

    bool Type::is_signed_char() const
    {
        return is_signed_char_type(_type_info);
    }

    bool Type::is_unsigned_char() const
    {
        return is_unsigned_char_type(_type_info);
    }

    bool Type::is_wchar_t() const
    {
        return is_wchar_t_type(_type_info);
    }


    bool Type::is_floating_type() const
    {
        return ::is_floating_type(_type_info);
    }

    bool Type::is_long_double() const
    {
        return is_long_double_type(_type_info);
    }

    bool Type::is_double() const
    {
        return is_double_type(_type_info);
    }

    bool Type::is_float() const
    {
        return is_float_type(_type_info);
    }


    bool Type::is_bool() const
    {
        return is_bool_type(_type_info);
    }

    bool Type::is_pointer_to_member() const
    {
        return is_pointer_to_member_type(_type_info);
    }

    Type Type::pointed_class() const
    {
        return pointer_to_member_type_get_class_type(_type_info);
    }

    bool Type::is_complex() const
    {
        return is_complex_type(_type_info);
    }

    Type Type::complex_get_base_type() const
    {
        return ::complex_type_get_base_type(_type_info);
    }

    Type Type::get_lvalue_reference_to()
    {
        return get_lvalue_reference_type(this->_type_info);
    }

    Type Type::get_rvalue_reference_to()
    {
        return get_rvalue_reference_type(this->_type_info);
    }

    Type Type::get_rebindable_reference_to()
    {
        return get_rebindable_reference_type(this->_type_info);
    }

    Type Type::get_unqualified_type()
    {
        // Might return itself if not qualified
        return get_cv_qualified_type(this->_type_info, CV_NONE);
    }

    Type Type::get_unqualified_type_but_keep_restrict()
    {
        // Might return itself if not qualified
        if (is_restrict_qualified_type(this->_type_info))
        {
            return get_cv_qualified_type(this->_type_info, CV_RESTRICT);
        }
        else
        {
            return get_cv_qualified_type(this->_type_info, CV_NONE);
        }
    }

    Type Type::get_const_type()
    {
        // Might return itself if already const qualified
        return get_cv_qualified_type(this->_type_info,
                (cv_qualifier_t)(get_cv_qualifier(this->_type_info) | CV_CONST));
    }

    Type Type::get_volatile_type()
    {
        // Might return itself if already volatile qualified
        return get_cv_qualified_type(this->_type_info,
                (cv_qualifier_t)(get_cv_qualifier(this->_type_info) | CV_VOLATILE));
    }

    Type Type::get_restrict_type()
    {
        // Might return itself if already restrict qualified
        return get_cv_qualified_type(this->_type_info,
                (cv_qualifier_t)(get_cv_qualifier(this->_type_info) | CV_RESTRICT));
    }

    Type Type::get_as_qualified_as(TL::Type t)
    {
        cv_qualifier_t cv = get_cv_qualifier(t._type_info);
        return get_cv_qualified_type(this->_type_info, cv);
    }

    Type Type::get_added_qualification_of(TL::Type t)
    {
        cv_qualifier_t cv = get_cv_qualifier(t._type_info);
        return get_cv_qualified_type(this->_type_info,
                (cv_qualifier_t)(cv | get_cv_qualifier(this->_type_info)));
    }

    Type Type::no_ref() const
    {
        if (::is_lvalue_reference_type(this->_type_info)
                || ::is_rvalue_reference_type(this->_type_info))
        {
            return ::reference_type_get_referenced_type(this->_type_info);
        }
        return this->_type_info;
    }

    int Type::get_alignment_of()
    {
        return type_get_alignment(this->_type_info);
    }

    bool Type::is_const() const
    {
        return is_const_qualified_type(_type_info);
    }

    bool Type::is_volatile() const
    {
        return is_volatile_qualified_type(_type_info);
    }

    bool Type::is_restrict() const
    {
        return is_restrict_qualified_type(_type_info);
    }

    ObjectList<Symbol> Type::get_fields() const
    {
        return get_nonstatic_data_members();
    }

    ObjectList<Symbol> Type::get_nonstatic_data_members() const
    {
        ObjectList<Symbol> result;
        scope_entry_list_t* list = class_type_get_nonstatic_data_members(
                ::get_actual_class_type(_type_info));
        Scope::convert_to_vector(list, result);
        entry_list_free(list);

        return result;
    }

    ObjectList<Symbol> Type::get_static_data_members() const
    {
        ObjectList<Symbol> result;
        scope_entry_list_t* list = class_type_get_static_data_members(
                ::get_actual_class_type(_type_info));
        Scope::convert_to_vector(list, result);
        entry_list_free(list);

        return result;
    }

    ObjectList<Symbol> Type::get_all_data_members() const
    {
        ObjectList<Symbol> result = get_nonstatic_data_members();
        result.append(get_static_data_members());
        return result;
    }

    ObjectList<Symbol> Type::get_all_members() const
    {
        ObjectList<Symbol> result;
        scope_entry_list_t* list = class_type_get_members(
                    ::get_actual_class_type(_type_info));
        Scope::convert_to_vector(list, result);
        entry_list_free(list);

        return result;
    }

    ObjectList<MemberDeclarationInfo> Type::get_member_declarations() const
    {
        TL::ObjectList<MemberDeclarationInfo> result;

        int num_decls = 0;

        member_declaration_info_t* mdi = ::class_type_get_member_declarations(_type_info, &num_decls);

        int i;
        for (i = 0; i < num_decls; i++)
        {
            result.push_back(
                    MemberDeclarationInfo(mdi[i].entry, mdi[i].decl_context, mdi[i].is_definition)
                    );
        }

        DELETE(mdi);

        return result;
    }

    bool Type::some_member_is_mutable() const
    {
        ObjectList<Symbol> nonstatic_data_members = get_nonstatic_data_members();

        for (ObjectList<Symbol>::iterator it = nonstatic_data_members.begin();
                it != nonstatic_data_members.end();
                it++)
        {
            Symbol &sym(*it);

            if (sym.is_mutable())
            {
                return true;
            }
        }

        return false;
    }

    bool Type::is_template_type() const
    {
        return (::is_template_type(_type_info));
    }

    ObjectList<Type> Type::get_specializations() const
    {
        ObjectList<Type> result;

        int n = template_type_get_num_specializations(_type_info);

        for (int i = 0; i < n; i++)
        {
            result.append(template_type_get_specialization_num(_type_info, i));
        }

        return result;
    }

    TemplateParameters Type::template_type_get_template_parameters() const
    {
        return ::template_type_get_template_parameters(_type_info);
    }

    Type Type::get_primary_template() const
    {
        return ::template_type_get_primary_type(_type_info);
    }

    bool Type::is_template_specialized_type() const
    {
        return (::is_template_specialized_type(_type_info));
    }

    TemplateParameters Type::template_specialized_type_get_template_parameters() const
    {
        return ::template_specialized_type_get_template_parameters(_type_info);
    }

    TemplateParameters Type::template_specialized_type_get_template_arguments() const
    {
        return ::template_specialized_type_get_template_arguments(_type_info);
    }

    Type Type::get_related_template_type() const
    {
        return Type(::template_specialized_type_get_related_template_type(_type_info));
    }

    Symbol Type::get_related_template_symbol() const
    {
        return template_type_get_related_symbol(_type_info);
    }

    bool Type::is_same_type(Type t) const
    {
        return equivalent_types(this->_type_info, t._type_info);
    }

    bool Type::depends_on_nonconstant_values() const
    {
        return type_depends_on_nonconstant_values(this->_type_info);
    }

    bool Type::lacks_prototype() const
    {
        return function_type_get_lacking_prototype(this->_type_info);
    }

    bool Type::is_trailing_return() const
    {
        return function_type_get_has_trailing_return(this->_type_info);
    }

    Type Type::basic_type() const
    {
        if (this->is_array())
        {
            return this->array_element().basic_type();
        }
        else if (this->is_pointer()
                || this->is_pointer_to_member())
        {
            return this->points_to().basic_type();
        }
        else if (this->is_function())
        {
            return this->returns().basic_type();
        }
        else if (this->is_any_reference())
        {
            return this->references_to().basic_type();
        }
        else if (this->is_vector())
        {
            return this->vector_element().basic_type();
        }
        else
        {
            return *this;
        }
    }

    Type Type::get_canonical_type()
    {
         return ::canonical_type(this->_type_info);
    }

    Type Type::get_enum_underlying_type()
    {
        return ::enum_type_get_underlying_type(this->_type_info);
    }

    ObjectList<Symbol> Type::enum_get_enumerators()
    {
        ObjectList<Symbol> enumerators;

        int i;
        for (i = 0; i < enum_type_get_num_enumerators(this->_type_info); i++)
        {
            scope_entry_t* enumerator = enum_type_get_enumerator_num(this->_type_info, i);
            enumerators.append(enumerator);
        };

        return enumerators;
    }

    bool Type::is_incomplete() const
    {
        return is_incomplete_type(_type_info);
    }

    bool Type::class_type_is_complete_independent() const
    {
        return ::class_type_is_complete_independent(_type_info);
    }

    bool Type::class_type_is_complete_dependent() const
    {
        return ::class_type_is_complete_dependent(_type_info);
    }

    bool Type::class_type_is_incomplete_independent() const
    {
        return ::class_type_is_incomplete_independent(_type_info);
    }

    bool Type::class_type_is_incomplete_dependent() const
    {
        return ::class_type_is_incomplete_dependent(_type_info);
    }

    type_tag_t Type::class_type_get_class_kind() const
    {
        return ::class_type_get_class_kind(_type_info);
    }

    bool Type::class_type_is_packed() const
    {
        return ::class_type_is_packed(_type_info);
    }

    unsigned int Type::get_size() const
    {
        unsigned int result;

        if (is_generic_vector_type(_type_info))
        {
            result = this->basic_type().get_size();
        }
        else
        {
            result = (unsigned int) type_get_size(_type_info);
        }
        return result;
    }

    Type Type::advance_over_typedefs()
    {
        return Type(::advance_over_typedefs(_type_info));
    }

    ObjectList<Symbol> Type::get_bases_class_symbol_list()
    {
        ObjectList<Symbol> base_symbol_list;

        scope_entry_list_t* all_bases = class_type_get_all_bases(_type_info, /* include_dependent */ 0);
        Scope::convert_to_vector(all_bases, base_symbol_list);
        entry_list_free(all_bases);

        return base_symbol_list;
    }

    ObjectList<Type::BaseInfo> Type::get_bases()
    {
        ObjectList<Type::BaseInfo> result;

        int n = class_type_get_num_bases(_type_info);
        for (int i = 0; i < n; i++)
        {
            scope_entry_t* symbol = NULL;
            char is_virtual = 0, is_dependent = 0, is_expansion = 0;
            access_specifier_t as = AS_UNKNOWN;

            symbol = class_type_get_base_num(_type_info, i,
                    &is_virtual,
                    &is_dependent,
                    &is_expansion,
                    &as);

            result.append(BaseInfo(symbol, is_virtual, is_dependent, is_expansion, as));
        }

        return result;
    }

    ObjectList<Symbol> Type::class_get_friends()
    {
        ObjectList<Symbol> friend_symbol_list;

        scope_entry_list_t* all_friends = class_type_get_friends(_type_info);
        Scope::convert_to_vector(all_friends, friend_symbol_list);
        entry_list_free(all_friends);

        return friend_symbol_list;
    }

    ObjectList<Symbol> Type::class_get_inherited_constructors()
    {
        ObjectList<Symbol> inherited_constructors_list;

        scope_entry_list_t* inherited_constructors =
            class_type_get_inherited_constructors(_type_info);
        Scope::convert_to_vector(inherited_constructors, inherited_constructors_list);
        entry_list_free(inherited_constructors);

        return inherited_constructors_list;
    }

    bool Type::is_pod()
    {
        return ::is_pod_type(_type_info);
    }

    bool Type::is_aggregate()
    {
        return ::is_aggregate_type(_type_info);
    }

    bool Type::is_builtin()
    {
        return ::is_builtin_type(_type_info);
    }

    bool Type::is_unresolved_overload()
    {
        return ::is_unresolved_overloaded_type(_type_info);
    }

    bool Type::is_pointer_to_class() const
    {
        return ::is_pointer_to_class_type(_type_info);
    }

    bool Type::is_any_reference_to_class() const
    {
        return ::is_any_reference_to_class_type(_type_info);
    }

    bool TL::Type::is_interoperable() const
    {
        return ::variant_type_is_interoperable(_type_info);
    }

    bool Type::is_base_class(Type t) const
    {
        return ::class_type_is_base(_type_info, t._type_info);
    }

    bool Type::is_derived_class(Type t) const
    {
        return ::class_type_is_derived(_type_info, t._type_info);
    }

    ObjectList<Symbol> Type::get_unresolved_overload_set()
    {
        ObjectList<Symbol> result;
        scope_entry_list_t* entry_list = ::unresolved_overloaded_type_get_overload_set(_type_info);
        Scope::convert_to_vector(entry_list, result);
        entry_list_free(entry_list);

        return result;
    }
    TemplateParameters Type::unresolved_overloaded_type_get_explicit_template_arguments()
    {
        return ::unresolved_overloaded_type_get_explicit_template_arguments(_type_info);
    }

    //! Returns all the builtins of the type system
    ObjectList<Type> Type::get_integer_types()
    {
        Type all_integer_types[] =
        {
            Type(get_char_type()),
            Type(get_signed_int_type()),
            Type(get_signed_short_int_type()),
            Type(get_signed_long_int_type()),
            Type(get_signed_long_long_int_type()),
            Type(get_signed_char_type()),
            Type(get_unsigned_int_type()),
            Type(get_unsigned_short_int_type()),
            Type(get_unsigned_long_int_type()),
            Type(get_unsigned_long_long_int_type()),
            Type(get_unsigned_char_type()),
        };

        return ObjectList<Type>(all_integer_types);
    }

    ObjectList<Type> Type::get_floating_types()
    {
        Type all_floating_types[] =
        {
            Type(get_float_type()),
            Type(get_double_type()),
            Type(get_long_double_type()),
        };

        return ObjectList<Type>(all_floating_types);
    }

    ObjectList<Type> Type::get_arithmetic_types()
    {
        ObjectList<Type> res(Type::get_integer_types());
        res.append(Type::get_floating_types());
        return res;
        //return Type::get_integer_types().append(Type::get_floating_types());
    }

    bool Type::is_variably_modified() const
    {
        return ::is_variably_modified_type(_type_info);
    }
    
    Type Type::get_size_t_type()
    {
        return ::get_size_t_type();
    }

    Type Type::get_ptrdiff_t_type()
    {
        return ::get_ptrdiff_t_type();
    }

    std::string Type::print_declarator() const
    {
        return ::print_declarator(_type_info);
    }

    TemplateArgument::TemplateArgumentKind TemplateArgument::get_kind() const
    {
        return _tpl_param_value->kind;
    }

    Nodecl::NodeclBase TemplateArgument::get_value() const
    {
        ERROR_CONDITION(template_parameter_kind_is_pack(_tpl_param_value->kind),
                "Do not call this function on template packs", 0);
        return _tpl_param_value->value;
    }

    Type TemplateArgument::get_type() const
    {
        ERROR_CONDITION(template_parameter_kind_is_pack(_tpl_param_value->kind),
                "Do not call this function on template packs", 0);
        return _tpl_param_value->type;
    }

    bool TemplateArgument::is_default() const
    {
        return _tpl_param_value->is_default;
    }

    bool TemplateParameters::operator==(TemplateParameters t) const
    {
        return this->_tpl_params == t._tpl_params;
    }

    bool TemplateParameters::operator!=(TemplateParameters t) const
    {
        return !(this->operator==(t));
    }

    bool TemplateParameters::is_valid() const
    {
        return _tpl_params != NULL;
    }

    int TemplateParameters::get_num_parameters() const
    {
        return _tpl_params->num_parameters;
    }

    std::pair<TL::Symbol, TemplateParameters::TemplateParameterKind> TemplateParameters::get_parameter_num(int n) const
    {
        return std::make_pair(_tpl_params->parameters[n]->entry, _tpl_params->parameters[n]->kind);
    }

    bool TemplateParameters::has_argument(int n) const
    {
        return (n < _tpl_params->num_parameters
                && _tpl_params->arguments != NULL
                && _tpl_params->arguments[n] != NULL);
    }

    TemplateArgument TemplateParameters::get_argument_num(int n) const
    {
        return _tpl_params->arguments[n];
    }

    bool TemplateParameters::has_enclosing_parameters() const
    {
        return _tpl_params->enclosing != NULL;
    }

    TemplateParameters TemplateParameters::get_enclosing_parameters() const
    {
        return TemplateParameters(_tpl_params->enclosing);
    }

    bool TemplateParameters::get_is_explicit_specialization() const
    {
        return _tpl_params->is_explicit_specialization;
    }

    bool TemplateParameters::get_is_explicit_instantiation() const
    {
        return _tpl_params->is_explicit_instantiation;
    }

    Type::BaseInfo::BaseInfo(TL::Symbol _base,
            bool _is_virtual,
            bool _is_dependent,
            bool _is_expansion,
            access_specifier_t _access_specifier)
        : base(_base),
        is_virtual(_is_virtual),
        is_dependent(_is_dependent),
        is_expansion(_is_expansion),
        access_specifier(_access_specifier)
    {
    }
}
