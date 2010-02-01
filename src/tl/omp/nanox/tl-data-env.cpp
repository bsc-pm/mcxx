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

#include "tl-omp-nanox.hpp"
#include "tl-data-env.hpp"
#include "tl-langconstruct.hpp"
#include "tl-counters.hpp"
#include <sstream>

namespace TL
{
    namespace Nanox
    {
        const std::string DATA_ENV_ARG_TYPE_COUNTER = "data_env_arg_type_counter";

        static void dimensional_replacements_of_variable_type_aux(Type type, 
                Symbol sym, 
                ObjectList<Source> &dim_names, 
                ObjectList<Source> &dim_decls)
        {
            Counter& vla_counter = CounterManager::get_counter("VLA_DIMENSIONS_COUNTER");
            if (type.is_array())
            {
                Source dim_name;
                dim_name
                    << "_" << sym.get_name() << "_" << vla_counter
                    ;
                vla_counter++;

                dimensional_replacements_of_variable_type_aux(type.array_element(), sym, dim_names, dim_decls);

                dim_names.append(dim_name);
                dim_decls.append(Source("") << "int " << dim_name << " = " << type.array_dimension().prettyprint());
            }
            else if (type.is_pointer())
            {
                dimensional_replacements_of_variable_type_aux(type.points_to(), sym, dim_names, dim_decls);
            }
        }

        static Type compute_replacemement_type_for_vla(Type type, ObjectList<Source>::iterator dim_names)
        {
            Type new_type(NULL);
            if (type.is_array())
            {
                new_type = compute_replacemement_type_for_vla(type.array_element(), dim_names + 1);

                new_type = new_type.get_array_to(*dim_names);
            }
            else if (type.is_pointer())
            {
                new_type = compute_replacemement_type_for_vla(type.points_to(), dim_names);
                new_type = new_type.get_pointer_to();
            }
            else
            {
                new_type = type;
            }

            return new_type;
        }

        static void valued_type(Symbol sym, 
                ScopeLink sl, 
                DataEnvironInfo& data_env_info)
        {
            bool is_raw_buffer = false;

            Type type = sym.get_type();

            if (IS_C_LANGUAGE
                    && type.is_variably_modified())
            {
                // Only VLA arrays or pointers to VLA are actually allowed.
                // Other kinds of variably modified types involve local types
                // (this is a kind of local type but we allow it for
                // convenience)
            }
            else if (IS_CXX_LANGUAGE)
            {
                if (type.is_array())
                {
                    Type element_type = type.array_element();
                    // This is the "easiest" way to build a type
                    Source src;
                    src
                        << "char [(" 
                        << type.array_dimension().prettyprint() 
                        << ") * sizeof(" 
                        << element_type.get_declaration(sym.get_scope(), "") 
                        << ")]"
                        ;

                    type = src.parse_type(sym.get_point_of_declaration(), sl);
                    is_raw_buffer = true;
                }
                // Some classes do not need this treatment, in particular if they are pod
                else if (type.is_named_class())
                {
                    Source src;
                    src
                        << "char [sizeof(" << type.get_declaration(sym.get_scope(), "") << ")]"
                        ;

                    type = src.parse_type(sym.get_point_of_declaration(), sl);
                    is_raw_buffer = true;
                }
            }

            std::string field_name = data_env_info.get_field_name_for_symbol(sym);

            DataEnvironItem data_env_item(sym, type, field_name);
            CXX_LANGUAGE()
            {
                data_env_item.set_is_raw_buffer(is_raw_buffer);
            }
        }

        static void pointer_type(Symbol sym, 
                ScopeLink sl, 
                DataEnvironInfo& data_env_info)
        {
            Type type = sym.get_type();
            if (IS_C_LANGUAGE
                    && type.is_variably_modified())
            {
            }
            else if (type.is_array())
            {
                type = type.array_element().get_pointer_to().get_restrict_type();
            }
            else
            {
                type = type.get_pointer_to().get_restrict_type();
            }

            std::string field_name = data_env_info.get_field_name_for_symbol(sym);

            DataEnvironItem data_env_item(sym, type, field_name);
            data_env_item.set_is_pointer(true);

            data_env_info.add_item(data_env_item);
        }

        void compute_data_environment(ObjectList<Symbol> value,
                ObjectList<Symbol> shared,
                ScopeLink scope_link,
                DataEnvironInfo &data_env_info)
        {
            struct auxiliar_struct_t
            {
                ObjectList<Symbol>* list;
                void (*transform_type)(Symbol, ScopeLink, DataEnvironInfo&);
            } aux_struct[] =
            {
                { &shared, pointer_type },
                { &value, valued_type },
                { NULL, NULL },
            };

            for (unsigned int i = 0;
                    aux_struct[i].list != NULL;
                    i++)
            {
                ObjectList<Symbol>& current_list(*aux_struct[i].list);
                for (ObjectList<Symbol>::iterator it = current_list.begin();
                        it != current_list.end();
                        it++)
                {
                    Symbol &sym(*it);

                    std::string field_name = sym.get_name();
                    (aux_struct[i].transform_type)(sym, scope_link, data_env_info);
                }
            }
        }

        void fill_data_environment_structure(
                Scope sc,
                const DataEnvironInfo &data_env_info,
                Source &struct_decl,
                Source &struct_fields,
                std::string& struct_name,
                ObjectList<OpenMP::DependencyItem> dependencies)
        {

            std::stringstream ss;

            int data_env_struct = TL::CounterManager::get_counter(DATA_ENV_ARG_TYPE_COUNTER);
            TL::CounterManager::get_counter(DATA_ENV_ARG_TYPE_COUNTER)++;

            ss << "_nx_data_env_" << data_env_struct << "_t";
            struct_name = ss.str();

            C_LANGUAGE()
            {
                struct_decl
                    << "typedef "
                    << "struct " << struct_name << "_tag {"
                    << struct_fields
                    << "}" << struct_name << ";"
                    ;
            }

            CXX_LANGUAGE()
            {
                struct_decl
                    << "struct " << struct_name << " {"
                    << struct_fields
                    << "};"
                    ;
            }

            ObjectList<DataEnvironItem> data_env_item_list;

            data_env_info.get_items(data_env_item_list);

            for (ObjectList<DataEnvironItem>::iterator it = data_env_item_list.begin();
                    it != data_env_item_list.end();
                    it++)
            {
                DataEnvironItem &data_env_item(*it);

                struct_fields
                    << data_env_item.get_type().get_declaration(sc, data_env_item.get_field_name()) << ";"
                    ;
            }

            int dep_counter = 0;
            for (ObjectList<OpenMP::DependencyItem>::iterator it = dependencies.begin();
                    it != dependencies.end();
                    it++)
            {
                if (!it->is_symbol_dependence())
                {
                    std::stringstream ss;
                    ss << "dep_" << dep_counter;

                    struct_fields << it->get_dependency_expression()
                        .get_type().get_pointer_to()
                        .get_declaration(it->get_dependency_expression().get_scope(), ss.str())
                        << ";"
                        ;
                }

                dep_counter++;
            }
        }

        void fill_data_args(const std::string& arg_var_accessor, 
                const DataEnvironInfo& data_env, 
                ObjectList<OpenMP::DependencyItem> dependencies,
                Source& result)
        {
            ObjectList<DataEnvironItem> data_env_items;
            data_env.get_items(data_env_items);

            for (ObjectList<DataEnvironItem>::iterator it = data_env_items.begin();
                    it != data_env_items.end();
                    it++)
            {
                DataEnvironItem& data_env_item(*it);
                Symbol sym = data_env_item.get_symbol();
                Type type = sym.get_type();
                const std::string field_name = data_env_item.get_field_name();

                if (data_env_item.is_pointer())
                {
                    if (type.is_array())
                    {
                        result << arg_var_accessor << field_name
                            << "= (" << sym.get_name() << ");";
                    }
                    else
                    {
                        result << arg_var_accessor << field_name
                            << "= &(" << sym.get_name() << ");";
                    }
                }
                else
                {
                    if (type.is_array())
                    {
                        C_LANGUAGE()
                        {
                            result << "__builtin_memcpy(&" << arg_var_accessor << field_name << ", "
                                << sym.get_name() << ","
                                << "sizeof(" << sym.get_name() << "));"
                                ;
                        }
                        CXX_LANGUAGE()
                        {
                            Source ptr_type_decl;
                            ptr_type_decl << type.get_pointer_to().get_declaration(sym.get_scope(), "") 
                                ;
                            Source type_decl;
                            type_decl << type.get_declaration(sym.get_scope(), "") 
                                ;
                            result << "for (int _i = 0; _i < (" << type.array_dimension().prettyprint() << "); _i++)"
                                << "{"
                                << "new ( &((" << ptr_type_decl << ")" << field_name << ")[_i])"
                                << type_decl << "(" << sym.get_name() << "[_i]);"
                                << "}"
                                ;
                        }
                    }
                    else
                    {
                        C_LANGUAGE()
                        {
                            result << arg_var_accessor << field_name
                                << "= " << sym.get_name() << ";";
                        }
                        CXX_LANGUAGE()
                        {
                            result << "new (&" << arg_var_accessor << field_name << ")" 
                                << type.get_declaration(sym.get_scope(), "") 
                                << "(" << sym.get_name() << ");";
                        }
                    }
                }
            }

            int num_dep = 0;
            for (ObjectList<OpenMP::DependencyItem>::iterator it = dependencies.begin();
                    it != dependencies.end();
                    it++)
            {
                if (!it->is_symbol_dependence())
                {
                    RefPtr<Source> base(new Source);
                    result << arg_var_accessor << "dep_" << num_dep << "= &(" << (*base) << ");"
                        ;

                    Expression dep_expr = it->get_dependency_expression();

                    bool is_shaped = false;

                    // Remove internal expressions that are not valid C
                    while (dep_expr.is_array_section()
                            || dep_expr.is_shaping_expression())
                    {
                        RefPtr<Source> new_base(new Source);
                        if (dep_expr.is_array_section())
                        {
                            Source dims_src;

                            dims_src << "[" << dep_expr.array_section_lower() << "]";
                            (*base) << (*new_base) << dims_src;

                            dep_expr = dep_expr.array_section_item();
                        }
                        else /* if (dep_expr.is_shaping_expression()) */
                        {
                            is_shaped = true;
                            // Create a meaningful casting
                            Type cast_type = dep_expr.get_type();
                            cast_type = cast_type.array_element().get_pointer_to();

                            (*base) << "((" << cast_type.get_declaration(dep_expr.get_scope(), "") << ")" << (*new_base)  << ")" ;

                            dep_expr = dep_expr.shaped_expression();
                        }

                        base = new_base;
                    }
                    (*base) << "(" << dep_expr << ")";
                }
                num_dep++;
            }
        }

        void do_outline_replacements(Statement body,
                const DataEnvironInfo& data_env_info,
                Source &replaced_outline,
                Source &initial_code)
        {
            ReplaceSrcIdExpression replace_src(body.get_scope_link());
            ObjectList<DataEnvironItem> data_env_items;
            data_env_info.get_items(data_env_items);

            // First set up all replacements and needed castings
            for (ObjectList<DataEnvironItem>::iterator it = data_env_items.begin();
                    it != data_env_items.end();
                    it++)
            {
                DataEnvironItem& data_env_item(*it);
                Symbol sym = data_env_item.get_symbol();
                Type type = sym.get_type();
                const std::string field_name = data_env_item.get_field_name();

                if (data_env_item.is_pointer())
                {
                    if (type.is_array())
                    {
                        // Just replace a[i] by (_args->a), no need to derreferentiate
                        replace_src.add_replacement(sym, "(_args->" + field_name + ")");
                    }
                    else
                    {
                        replace_src.add_replacement(sym, "(*_args->" + field_name + ")");
                    }
                }
                else
                {
                    // FIXME - Check if data is held in a raw buffer data type, to create proper adjustments
                    if (data_env_item.is_raw_buffer())
                    {
                        C_LANGUAGE()
                        {
                            // Set up a casting pointer
                            initial_code
                                << type.get_pointer_to().get_declaration(sym.get_scope(), field_name) 
                                << "="
                                << "("
                                << type.get_pointer_to().get_declaration(sym.get_scope(), "")
                                << ") _args->" << field_name << ";"
                                ;

                            replace_src.add_replacement(sym, "(*" + field_name + ")");
                        }
                        CXX_LANGUAGE()
                        {
                            // Set up a reference to the raw buffer properly casted to the data type
                            initial_code
                                << type.get_reference_to().get_declaration(sym.get_scope(), field_name)
                                << "(" 
                                << "(" << type.get_pointer_to().get_declaration(sym.get_scope(), "") << ")"
                                << "_args->" << field_name
                                << ");"
                                ;

                            // This is the neatest aspect of references
                            replace_src.add_replacement(sym, field_name);
                        }
                    }
                    else
                    {
                        replace_src.add_replacement(sym, "(_args->" + sym.get_name() + ")");
                    }
                }
            }

            replaced_outline << replace_src.replace(body.get_ast());
        }

    }
}
