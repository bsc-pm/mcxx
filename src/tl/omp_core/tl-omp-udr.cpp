#include "tl-omp-core.hpp"
#include "tl-omp-udr.hpp"
#include "tl-overload.hpp"
#include "cxx-utils.h"

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
        using OpenMP::UDRInfoItem;

        reduct_type = reduct_type.get_unqualified_type();

        Type void_type = Type::get_void_type();
        Type ptr_reduct_type = reduct_type.get_pointer_to();

        udr_valid_prototypes_t valid_prototypes[] = 
        {
            { /* T f(T, T) */      reduct_type, reduct_type,     reduct_type,     /* left */ true,  /* right */ true  },
            { /* T f(T*, T) */     reduct_type, ptr_reduct_type, reduct_type,     /* left */ true,  /* right */ true  },
            { /* T f(T, T*) */     reduct_type, reduct_type,     ptr_reduct_type, /* left */ true,  /* right */ true  },
            { /* T f(T*, T*) */    reduct_type, ptr_reduct_type, ptr_reduct_type, /* left */ true,  /* right */ true  },
            { /* void f(T*, T) */  void_type, ptr_reduct_type,   reduct_type,     /* left */ true,  /* right */ false },
            { /* void f(T, T*) */  void_type, reduct_type,       ptr_reduct_type, /* left */ false, /* right */ true  },
            { /* void f(T*, T*) */ void_type, ptr_reduct_type,   ptr_reduct_type, /* left */ true,  /* right */ true  } 
        };

        return ObjectList<udr_valid_prototypes_t>(valid_prototypes);
    }

    static ObjectList<udr_valid_prototypes_t> get_valid_prototypes_cxx(Type reduct_type)
    {
        using OpenMP::UDRInfoItem;

        reduct_type = reduct_type.get_unqualified_type();

        Type void_type = Type::get_void_type();
        Type ptr_reduct_type = reduct_type.get_pointer_to();
        Type c_ptr_reduct_type = reduct_type.get_const_type().get_pointer_to();
        Type ref_reduct_type = reduct_type.get_reference_to();
        Type c_ref_reduct_type = reduct_type.get_const_type().get_reference_to();

        udr_valid_prototypes_t valid_prototypes[] =
        {
            { /* T f(T, T) */      reduct_type, reduct_type,     reduct_type,     /* right */ true, /* left */ true  },
            { /* T f(T*, T) */     reduct_type, ptr_reduct_type, reduct_type,     /* right */ true, /* left */ true  },
            { /* T f(T, T*) */     reduct_type, reduct_type,     ptr_reduct_type, /* right */ true, /* left */ true  },
            { /* T f(T*, T*) */    reduct_type, ptr_reduct_type, ptr_reduct_type, /* right */ true, /* left */ true  },

            // const-qualified versions
            { /* T f(const T*, T) */ reduct_type, c_ptr_reduct_type, reduct_type, /* left */ true,  /* right */ true  },
            { /* T f(T, const T*) */ reduct_type, reduct_type, c_ptr_reduct_type, /* left */ true,  /* right */ true  },
            { /* T f(const T*, T*) */reduct_type, c_ptr_reduct_type, ptr_reduct_type,   /* left */ true,  /* right */ true  },
            { /* T f(const T*, const T*) */ reduct_type, c_ptr_reduct_type, c_ptr_reduct_type,   /* left */ true,  /* right */ true  },

            { /* void f(T*, T) */  void_type, ptr_reduct_type,   reduct_type,       /* left */ true,  /* right */ false },
            { /* void f(T, T*) */  void_type, reduct_type,       ptr_reduct_type,   /* left */ false, /* right */ true  },
            { /* void f(T*, T*) */ void_type, ptr_reduct_type,   ptr_reduct_type,   /* left */ true,  /* right */ true  },

            // const-qualified versions
            { /* void f(const T*, T*) */ void_type, ptr_reduct_type,   ptr_reduct_type,   /* left */ false,  /* right */ true  },
            { /* void f(T*, const T*) */ void_type, ptr_reduct_type,   ptr_reduct_type,   /* left */ true,  /* right */ false  },

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
        using OpenMP::UDRInfoSet;

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
                UDRInfoSet udr_info_set(global_scope, Type(type));
                udr_info_set.add_udr(
                        UDRInfoItem(Type(type),
                            builtin_arithmetic_operators[j].operator_name,
                            builtin_arithmetic_operators[j].neuter_tree,
                            UDRInfoItem::LEFT,
                            /* is_commutative */ true));
            }
            if (is_integral_type(type))
            {
                for (j = 0; builtin_logic_bit_operators[j].operator_name != NULL; j++)
                {
                    UDRInfoSet udr_info_set(global_scope, Type(type));
                    udr_info_set.add_udr(
                            UDRInfoItem(Type(type),
                                builtin_logic_bit_operators[j].operator_name,
                                builtin_logic_bit_operators[j].neuter_tree,
                                UDRInfoItem::LEFT,
                                /* is_commutative */ true));
                }
            }
        }
    }

    static void trim_spaces(std::string& str)
    {
        // Trim Both leading and trailing spaces
        size_t startpos = str.find_first_not_of(" \t"); // Find the first character position after excluding leading blank spaces
        size_t endpos = str.find_last_not_of(" \t"); // Find the first character position from reverse af

        // if all spaces or empty return an empty string
        if((std::string::npos == startpos) || (std::string::npos == endpos))
        {
            str = "";
        }
        else
            str = str.substr( startpos, endpos-startpos+1 );
    }

    static std::string get_valid_zero_initializer(Type t)
    {
        if (t.is_array())
        {
            return "{" + get_valid_zero_initializer(t.array_element()) + "}";
        }
        else if (t.is_class())
        {
            // Use the first one
            return "{" + get_valid_zero_initializer(t.get_nonstatic_data_members()[0].get_type()) + "}";
        }
        else
        {
            return "0";
        }
    }

    static std::string get_valid_value_initializer(Type t)
    {
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

     Symbol solve_udr_name_cxx(LangConstruct construct,
             std::string &op_name,
             Type reduction_type,
             OpenMP::UDRInfoItem::Associativity &assoc)
     {
         Symbol op_symbol(NULL);
         // Fix the names as needed
         if (op_name[0] == '.')
         {
             // Fix the name if possible
             if (reduction_type.is_class() || reduction_type.is_dependent())
             {
                 // Qualify the name
                 op_name = reduction_type.get_declaration(construct.get_scope(), "") + "::" + op_name.substr(1);
             }
             else
             {
                 running_error("%s: error: reduction operator specification '%s' is not valid for non-class type '%s'\n",
                         construct.get_ast().get_locus().c_str(),
                         op_name.c_str(),
                         reduction_type.get_declaration(construct.get_scope(), "").c_str());
             }
         }

         Type op_symbol_type(NULL);

         if (udr_is_builtin_operator(op_name))
         {
             // Builtin operators require a bit more of work
             op_name = "operator " + op_name;

             // First attempt a member search
             Source src;
             src <<  reduction_type.get_declaration(construct.get_scope(), "") << "::" << op_name;

             AST_t tree = src.parse_expression(construct.get_ast(), construct.get_scope_link(), 
                     // Do not nag the user if this attempt fails
                     Source::DO_NOT_CHECK_EXPRESSION); 
             Expression expr(tree, construct.get_scope_link());

             if (!expr.get_type().is_valid())
             {
                 // Attempt a second lookup, this time in the current scope
                 src = op_name;
                 tree = src.parse_expression(construct.get_ast(), construct.get_scope_link());

                 expr = Expression(tree, construct.get_scope_link());

                 if (!expr.get_type().is_valid())
                 {
                     running_error("%s: error: invalid reduction operator '%s'",
                             construct.get_ast().get_locus().c_str(),
                             op_name.c_str());
                 }
             }
             else
             {
                 // Use the full name
                 op_name = src;
             }

             if (expr.is_id_expression())
             {
                 op_symbol_type = expr.get_type();
             }
             else
             {
                 running_error("%s: error: invalid reduction operator '%s' since it is not an id-expression",
                         construct.get_ast().get_locus().c_str(),
                         op_name.c_str());
             }
         }
         else
         {
             // Parse it to cause a scope search when typechecking
             Source src = op_name;

             AST_t expr_tree = src.parse_expression(construct.get_ast(), construct.get_scope_link());
             Expression expr(expr_tree, construct.get_scope_link());

             if (!expr.get_type().is_valid())
             {
                 running_error("%s: error: invalid reduction operator '%s'",
                         construct.get_ast().get_locus().c_str(),
                         op_name.c_str());
             }

             if (expr.is_id_expression())
             {
                 op_symbol_type = expr.get_type();
             }
             else
             {
                 running_error("%s: error: invalid reduction operator '%s' since it is not an id-expression",
                         construct.get_ast().get_locus().c_str(),
                         op_name.c_str());
             }
         }

         if (op_symbol_type.is_unresolved_overload())
         {
             ObjectList<udr_valid_prototypes_t> valid_prototypes 
                 = get_valid_prototypes_cxx(reduction_type);
             ObjectList<udr_valid_member_prototypes_t> valid_member_prototypes 
                 = get_valid_member_prototypes_cxx(reduction_type);

             ObjectList<Symbol> overload_set = op_symbol_type.get_unresolved_overload_set();

             bool found_valid = false;

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
                         overload_set,
                         Type(NULL), // No implicit
                         arguments,
                         construct.get_ast().get_file(),
                         construct.get_ast().get_line(),
                         valid,
                         viable_functs,
                         argument_conversor);

                 if (valid 
                         && function_is_valid_udr_reductor_cxx(valid_prototypes, valid_member_prototypes, 
                             reduction_type, solved_sym, assoc))
                 {
                     if (!found_valid)
                     {
                         found_valid = true;
                         op_symbol = solved_sym;
                     }
                     else if (solved_sym != op_symbol)
                     {
                         running_error("%s: error: reduction operator '%s' and '%s' both meet the requirements of OpenMP",
                                 construct.get_ast().get_locus().c_str(),
                                 op_symbol.get_type().get_declaration(op_symbol.get_scope(), 
                                     op_symbol.get_qualified_name(op_symbol.get_scope())).c_str(),
                                 solved_sym.get_type().get_declaration(solved_sym.get_scope(), 
                                     solved_sym.get_qualified_name(op_symbol.get_scope())).c_str());
                     }
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
                             overload_set,
                             reduction_type.get_reference_to(), // implicit argument (it must be a reference)
                             arguments,
                             construct.get_ast().get_file(),
                             construct.get_ast().get_line(),
                             valid,
                             viable_functs,
                             argument_conversor);

                     if (valid 
                             && function_is_valid_udr_reductor_cxx(valid_prototypes, valid_member_prototypes, 
                                 reduction_type, solved_sym, assoc))
                     {
                         if (!found_valid)
                         {
                             found_valid = true;
                             op_symbol = solved_sym;
                         }
                         else if (solved_sym != op_symbol)
                         {
                             running_error("%s: error: reduction operator '%s' and '%s' both meet the requirements of OpenMP",
                                     construct.get_ast().get_locus().c_str(),
                                     op_symbol.get_type().get_declaration(op_symbol.get_scope(), 
                                         op_symbol.get_qualified_name(op_symbol.get_scope())).c_str(),
                                     solved_sym.get_type().get_declaration(solved_sym.get_scope(), 
                                         solved_sym.get_qualified_name(solved_sym.get_scope())).c_str());
                         }
                     }
                 }
             }

             if (!found_valid)
             {

                 for (ObjectList<Symbol>::iterator it = overload_set.begin();
                         it != overload_set.end();
                         it++)
                 {
                     std::cerr << construct.get_ast().get_locus()
                         << ": note: '" << it->get_type().get_declaration(it->get_scope(), 
                                 it->get_qualified_name(it->get_scope())) << "' is not an eligible operator" 
                         << " for type '" << reduction_type.get_declaration(construct.get_scope(), "") << "'"
                         << std::endl;
                 }

                 running_error("%s: error: reduction operator '%s' does not meet " 
                         "OpenMP reduction operator requirements", 
                         construct.get_ast().get_locus().c_str(),
                         op_name.c_str());
             }
         }
         else if (op_symbol_type.is_dependent())
         {
             // Do nothing
         }
         else
         {
             running_error("%s: error: operator '%s' does not have function type", 
                     construct.get_ast().get_locus().c_str(),
                     op_name.c_str());
         }

         return op_symbol;
     }

    namespace OpenMP
    {

        void Core::declare_reduction_handler_pre(PragmaCustomConstruct construct)
        {
            // UDRInfoSet udr_info_set(construct.get_scope())

            // #pragma omp declare reduction type(type-name-list) operator(op-name-list) order(left|right) commutative
            ScopeLink scope_link = construct.get_scope_link();

            PragmaCustomClause type_clause = construct.get_clause("type");
            // Do NOT tokenize this one
            ObjectList<std::string> type_args = type_clause.get_arguments();

            PragmaCustomClause operator_clause = construct.get_clause("operator");
            ObjectList<std::string> op_args = operator_clause.get_arguments(ExpressionTokenizer());

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

            for (ObjectList<std::string>::iterator type_it = type_args.begin();
                    type_it != type_args.end();
                    type_it++)
            {
                Source src(*type_it);

                ObjectList<Type> type_list = src.parse_type_list(construct.get_ast(), construct.get_scope_link());

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

                    UDRInfoSet udr_info_set(construct.get_scope(), reduction_type);

                    for (ObjectList<std::string>::iterator op_it = op_args.begin();
                            op_it != op_args.end();
                            op_it++)
                    {
                        std::string& op_name(*op_it);

                        trim_spaces(op_name);

                        // Perform lookup and further checking
                        C_LANGUAGE()
                        {
                            Symbol reductor_sym = construct.get_scope().get_symbol_from_name(op_name);

                            if (!reductor_sym.is_valid())
                            {
                                running_error("%s: error: reduction operator '%s' not found in the current scope",
                                        construct.get_ast().get_locus().c_str(),
                                        op_name.c_str());
                            }

                            ObjectList<udr_valid_prototypes_t> valid_prototypes = get_valid_prototypes_c(reduction_type);

                            if (!function_is_valid_udr_reductor_c(valid_prototypes, reductor_sym, assoc))
                            {
                                running_error("%s: error: reduction operator '%s' does not meet " 
                                        "OpenMP reduction operator requirements", 
                                        construct.get_ast().get_locus().c_str(),
                                        op_name.c_str());
                            }

                            op_symbol = reductor_sym;
                        }

                        CXX_LANGUAGE()
                        {
                            op_symbol = solve_udr_name_cxx(construct, op_name, reduction_type, assoc);
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

                        if (!udr_info_set.lookup_udr(op_symbol.get_qualified_name()))
                        {
                            std::cerr << construct.get_ast().get_locus() << ": note: introducing user-defined reduction for type '"
                                << reduction_type.get_declaration(construct.get_scope_link().get_scope(construct.get_ast()), "") << "'"
                                << " and operator '" 
                                << op_symbol.get_type().get_declaration(op_symbol.get_scope(), op_symbol.get_qualified_name(op_symbol.get_scope())) << "'"
                                << std::endl;
                            udr_info_set.add_udr(UDRInfoItem(reduction_type, op_symbol, identity, assoc, is_commutative));
                        }
                        else
                        {
                            running_error("%s: error: user defined reduction for reduction_type '%s' and operator '%s' already defined",
                                    construct.get_ast().get_locus().c_str(),
                                    reduction_type.get_declaration(construct.get_scope_link().get_scope(construct.get_ast()), "").c_str(),
                                    op_name.c_str());
                        }
                    }
                }
            }
        }

        void Core::declare_reduction_handler_post(PragmaCustomConstruct construct) { }

    }
}
