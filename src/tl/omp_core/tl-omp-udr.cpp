#include "tl-omp-udr.hpp"
#include "cxx-utils.h"

namespace TL
{
    bool function_is_valid_udr_reductor_c(Type reduct_type, Symbol sym, bool right_associative)
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

        Type ptr_reduct_type = reduct_type.get_pointer_to();

        // FIXME - We need a way to get basic types
        Type void_type = Type::get_void_type();

        struct valid_prototypes_tag
        {
            Type return_type;
            Type first_arg;
            Type second_arg;
            bool allows_left;
            bool allows_right;
        } valid_prototypes[] =
        {
            { /* T f(T, T) */      reduct_type, reduct_type,     reduct_type,     /* left */ true,  /* right */ true  },
            { /* T f(T*, T) */     reduct_type, ptr_reduct_type, reduct_type,     /* left */ true,  /* right */ true  },
            { /* T f(T, T*) */     reduct_type, reduct_type,     ptr_reduct_type, /* left */ true,  /* right */ true  },
            { /* T f(T*, T*) */    reduct_type, ptr_reduct_type, ptr_reduct_type, /* left */ true,  /* right */ true  },
            { /* void f(T*, T) */  void_type, ptr_reduct_type,   reduct_type,     /* left */ true,  /* right */ false },
            { /* void f(T, T*) */  void_type, reduct_type,       ptr_reduct_type, /* left */ false, /* right */ true  },
            { /* void f(T*, T*) */ void_type, ptr_reduct_type,   ptr_reduct_type, /* left */ true,  /* right */ true  },
            { /* sentinel */ Type(NULL), Type(NULL), Type(NULL) }
        };

        // Remove qualifications since they do not play any role in this comparison
        parameter_types[0] = parameter_types[0].get_unqualified_type();
        parameter_types[1] = parameter_types[1].get_unqualified_type();

        for (int i = 0; valid_prototypes[i].return_type.is_valid(); i++)
        {
            if (return_type.is_same_type(valid_prototypes[i].return_type)
                    && parameter_types[0].is_same_type(valid_prototypes[i].first_arg)
                    && parameter_types[1].is_same_type(valid_prototypes[i].second_arg)
                    && ((right_associative && valid_prototypes[i].allows_right)
                        || (!right_associative && valid_prototypes[i].allows_left)))
                return true;
        }

        return false;
    }

    bool function_is_valid_udr_reductor_cxx(Type reduct_type, Symbol sym, bool right_associative)
    {
        if (!sym.is_function())
            return false;

        Type function_type = sym.get_type();

        if (!function_type.is_function())
        {
            internal_error("Function name should have function type!", 0);
        }

        ObjectList<Type> parameter_types = function_type.parameters();

        // FIXME - We need a way to get basic types
        Type void_type(NULL);
        Type ptr_reduct_type = reduct_type.get_pointer_to();
        Type ref_reduct_type = reduct_type.get_reference_to();

        Type return_type = function_type.returns();

        if (!sym.is_member() || !sym.is_static())
        {
            if (parameter_types.size() != 2)
                return false;

            struct valid_prototypes_tag
            {
                Type return_type;
                Type first_arg;
                Type second_arg;
                bool allows_left;
                bool allows_right;
            } valid_prototypes[] =
            {
                { /* T f(T, T) */      reduct_type, reduct_type,     reduct_type,      /* left */ true,  /* right */ true  },
                { /* T f(T*, T) */     reduct_type, ptr_reduct_type, reduct_type,      /* left */ true,  /* right */ true  },
                { /* T f(T, T*) */     reduct_type, reduct_type,     ptr_reduct_type,  /* left */ true,  /* right */ true  },
                { /* T f(T*, T*) */    reduct_type, ptr_reduct_type, ptr_reduct_type,  /* left */ true,  /* right */ true  },

                { /* void f(T*, T) */  void_type, ptr_reduct_type,   reduct_type,      /* left */ true,  /* right */ false },
                { /* void f(T, T*) */  void_type, reduct_type,       ptr_reduct_type,  /* left */ false, /* right */ true  },
                { /* void f(T*, T*) */ void_type, ptr_reduct_type,   ptr_reduct_type,  /* left */ true,  /* right */ true  },

                { /* T f(T&, T) */     reduct_type, ref_reduct_type, reduct_type,      /* left */ true,  /* right */ true  },
                { /* T f(T, T&) */     reduct_type, reduct_type,     ref_reduct_type,  /* left */ true,  /* right */ true  },
                { /* T f(T&, T&) */    reduct_type, ref_reduct_type, ref_reduct_type,  /* left */ true,  /* right */ true  },

                { /* T f(T&, T*) */    reduct_type, ref_reduct_type, ptr_reduct_type,  /* left */ true,  /* right */ true  },
                { /* T f(T*, T&) */    reduct_type, ptr_reduct_type, ref_reduct_type,  /* left */ true,  /* right */ true  },

                { /* void f(T&, T) */  void_type, ref_reduct_type,   reduct_type,      /* left */ true,  /* right */ false },
                { /* void f(T, T&) */  void_type, reduct_type,       ref_reduct_type,  /* left */ false, /* right */ true  },
                { /* void f(T&, T&) */ void_type, ref_reduct_type,   ref_reduct_type,  /* left */ true,  /* right */ true  },
                { /* sentinel */ Type(NULL), Type(NULL), Type(NULL) }
            };

            // Remove qualifications since they do not play any role in this comparison
            parameter_types[0] = parameter_types[0].get_unqualified_type();
            parameter_types[1] = parameter_types[1].get_unqualified_type();

            for (int i = 0; valid_prototypes[i].return_type.is_valid(); i++)
            {
                if (return_type.is_same_type(valid_prototypes[i].return_type)
                        && parameter_types[0].is_same_type(valid_prototypes[i].first_arg)
                        && parameter_types[1].is_same_type(valid_prototypes[i].second_arg)
                        && ((right_associative && valid_prototypes[i].allows_right)
                            || (!right_associative && valid_prototypes[i].allows_left)))
                    return true;
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

            struct valid_prototypes_tag
            {
                Type return_type;
                Type first_arg;
            } valid_prototypes[] =
            {
                { /* void f(T) */  void_type, reduct_type      },
                { /* void f(T*) */  void_type, ptr_reduct_type },
                { /* void f(T&) */ void_type, ref_reduct_type  },

                { /* sentinel */ Type(NULL), Type(NULL)        }
            };

            // Remove qualifications since they do not play any role in this comparison
            parameter_types[0] = parameter_types[0].get_unqualified_type();

            for (int i = 0; valid_prototypes[i].return_type.is_valid(); i++)
            {
                if (return_type.is_same_type(valid_prototypes[i].return_type)
                        && parameter_types[0].is_same_type(valid_prototypes[i].first_arg))
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
}
