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

        const std::string OMP_NANOX_VLA_DIMS = "omp.nanox.vla_dims";


#if 0
        static void print_list(ObjectList<Source> src)
        {
            std::cerr << "== SOURCE LIST ==" << std::endl;
            for (ObjectList<Source>::iterator it = src.begin();
                    it != src.end();
                    it++)
            {
                std::cerr << it->get_source() << std::endl;
            }
            std::cerr << "== END of SOURCE LIST ==" << std::endl;
        }
#endif

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
                    << "_dim_" << sym.get_name() << "_" << vla_counter
                    ;
                vla_counter++;

                dim_names.append(dim_name);
                dim_decls.append(Source("") << "int " << dim_name << " = " << type.array_dimension().prettyprint());

                dimensional_replacements_of_variable_type_aux(type.array_element(), sym, dim_names, dim_decls);
            }
            else if (type.is_pointer())
            {
                dimensional_replacements_of_variable_type_aux(type.points_to(), sym, dim_names, dim_decls);
            }
        }

        static Type compute_replacement_type_for_vla(Type type, 
                ObjectList<Source>::iterator dim_names_begin,
                ObjectList<Source>::iterator dim_names_end)
        {
            Type new_type(NULL);
            if (type.is_array())
            {
                new_type = compute_replacement_type_for_vla(type.array_element(), dim_names_begin + 1, dim_names_end);

                if (dim_names_begin == dim_names_end)
                {
                    internal_error("Invalid dimension list", 0);
                }

                new_type = new_type.get_array_to(*dim_names_begin);
            }
            else if (type.is_pointer())
            {
                new_type = compute_replacement_type_for_vla(type.points_to(), dim_names_begin, dim_names_end);
                new_type = new_type.get_pointer_to();
            }
            else
            {
                new_type = type;
            }

            return new_type;
        }

        static void convert_vla(Symbol sym, ObjectList<Symbol>& converted_vlas, ScopeLink sl)
        {
            if (converted_vlas.contains(sym))
                return;

            ObjectList<Source> dim_decls;
            ObjectList<Source> dim_names;

            dimensional_replacements_of_variable_type_aux(sym.get_type(),
                    sym, dim_names, dim_decls);

            Source new_decls;
            for (ObjectList<Source>::iterator it = dim_decls.begin();
                    it != dim_decls.end();
                    it++)
            {
                new_decls << *it << ";"
                    ;
            }

            AST_t point_of_decl = sym.get_point_of_declaration();
            AST_t enclosing_stmt_tree;
            if (sym.is_parameter())
            {
                FunctionDefinition 
                    funct_def(point_of_decl.get_enclosing_function_definition(), sl);

                enclosing_stmt_tree = funct_def.get_function_body().get_inner_statements()[0].get_ast();
            }
            else
            {
                enclosing_stmt_tree = point_of_decl.get_enclosing_statement();
            }

            AST_t statement_seq 
                = new_decls.parse_statement(enclosing_stmt_tree, sl);
            enclosing_stmt_tree.prepend(statement_seq);

            if (!sym.is_parameter())
            {
                // If this is not a parameter, we'll want to rewrite the declaration itself
                Type new_type_spawn = compute_replacement_type_for_vla(sym.get_type(), dim_names.begin(), dim_names.end());

                // Now redeclare
                Source redeclaration;
                redeclaration
                    << new_type_spawn.get_declaration(sym.get_scope(), sym.get_name())
                    << ";"
                    ;

                AST_t redeclaration_tree = redeclaration.parse_statement(enclosing_stmt_tree,
                        sl, Source::ALLOW_REDECLARATION);

                enclosing_stmt_tree.prepend(redeclaration_tree);

                // Now remove the declarator of the declaration
                Declaration decl(point_of_decl, sl);

                if (decl.get_declared_entities().size() == 1)
                {
                    // We have to remove all the whole declaration
                    enclosing_stmt_tree.remove_in_list();
                }
                else
                {
                    // Remove only this entity
                    ObjectList<DeclaredEntity> entities = decl.get_declared_entities();
                    for (ObjectList<DeclaredEntity>::iterator it = entities.begin();
                            it != entities.end();
                            it++)
                    {
                        if (it->get_declared_symbol() == sym)
                        {
                            it->get_ast().remove_in_list();
                        }
                    }
                }
            }

            ObjectList<Source>* new_dim_ptr = new ObjectList<Source>(dim_names);

            RefPtr<ObjectList<Source> > dim_names_ref(new_dim_ptr);
            sym.set_attribute(OMP_NANOX_VLA_DIMS, dim_names_ref);

            converted_vlas.insert(sym);
        }

        static void valued_type(Symbol sym, 
                ScopeLink sl, 
                DataEnvironInfo& data_env_info,
                ObjectList<Symbol>& converted_vlas)
        {
            bool is_raw_buffer = false;
            bool is_vla_type = false;

            ObjectList<Source> dim_list;

            Type type = sym.get_type();

            if (IS_C_LANGUAGE
                    && type.is_variably_modified())
            {
                // Only VLA arrays or pointers to VLA are actually allowed.
                // Other kinds of variably modified types involve local types
                // (this is a kind of local type but we allow it for
                // convenience)

                // Normalize VLA, if needed
                convert_vla(sym, converted_vlas, sl);
                is_vla_type = true;

                RefPtr<Object> ref = sym.get_attribute(OMP_NANOX_VLA_DIMS);
                RefPtr<ObjectList<Source> > dim_list_ref 
                    = RefPtr<ObjectList<Source> >::cast_dynamic(ref);
                dim_list = ObjectList<Source>(dim_list_ref->begin(), dim_list_ref->end());
                type = Type::get_void_type().get_pointer_to();
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
            C_LANGUAGE()
            {
                data_env_item.set_is_vla_type(is_vla_type);
                data_env_item.set_vla_dimensions(dim_list);
            }
            CXX_LANGUAGE()
            {
                data_env_item.set_is_raw_buffer(is_raw_buffer);
            }

            data_env_item.set_is_copy(true);

            data_env_info.add_item(data_env_item);
        }

        static void pointer_type(Symbol sym, 
                ScopeLink sl, 
                DataEnvironInfo& data_env_info,
                ObjectList<Symbol>& converted_vlas)
        {
            bool is_vla_type = false;

            ObjectList<Source> dim_list;

            Type type = sym.get_type();
            if (IS_C_LANGUAGE
                    && type.is_variably_modified())
            {
                // Only VLA arrays or pointers to VLA are actually allowed.
                // Other kinds of variably modified types involve local types
                // (this is a kind of local type but we allow it for
                // convenience)

                // Normalize VLA, if needed
                convert_vla(sym, converted_vlas, sl);
                is_vla_type = true;

                RefPtr<Object> ref = sym.get_attribute(OMP_NANOX_VLA_DIMS);
                RefPtr<ObjectList<Source> > dim_list_ref 
                    = RefPtr<ObjectList<Source> >::cast_dynamic(ref);
                dim_list = *dim_list_ref;
                type = Type::get_void_type().get_pointer_to();
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
            C_LANGUAGE()
            {
                data_env_item.set_is_vla_type(is_vla_type);
                data_env_item.set_vla_dimensions(dim_list);
            }

            data_env_info.add_item(data_env_item);
        }

        static void private_type(Symbol sym, 
                ScopeLink sl, 
                DataEnvironInfo& data_env_info,
                ObjectList<Symbol>& converted_vlas)
        {
            Type type = sym.get_type();
            DataEnvironItem data_env_item(sym, type, "");

            data_env_item.set_is_private(true);

            data_env_info.add_item(data_env_item);
        }

    }

    void Nanox::compute_data_environment(
            OpenMP::DataSharingEnvironment &data_sharing,
            ScopeLink scope_link,
            DataEnvironInfo &data_env_info,
            ObjectList<Symbol>& converted_vlas)
    {
        ObjectList<Symbol> shared;
        data_sharing.get_all_symbols(OpenMP::DS_SHARED, shared);

        ObjectList<Symbol> value;
        data_sharing.get_all_symbols(OpenMP::DS_FIRSTPRIVATE, value);

        ObjectList<Symbol> private_symbols;
        data_sharing.get_all_symbols(OpenMP::DS_PRIVATE, private_symbols);

        struct auxiliar_struct_t
        {
            ObjectList<Symbol>* list;
            void (*transform_type)(Symbol, ScopeLink, DataEnvironInfo&, ObjectList<Symbol>&);
        } aux_struct[] =
        {
            { &shared, pointer_type },
            { &value, valued_type },
            { &private_symbols, private_type },
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
                (aux_struct[i].transform_type)(sym, scope_link, data_env_info, converted_vlas);
            }
        }

        ObjectList<OpenMP::CopyItem> copies;
        data_sharing.get_all_copies(copies);

        for (ObjectList<OpenMP::CopyItem>::iterator it = copies.begin();
                it != copies.end();
                it++)
        {
            Expression expr = it->get_copy_expression();
            Symbol sym = it->get_symbol();

            OpenMP::DataSharingAttribute ds_attr = data_sharing.get(sym);
            bool is_private = true;
            if (ds_attr == OpenMP::DS_UNDEFINED)
            {
                std::cerr 
                    << expr.get_ast().get_locus() 
                    << ": warning: data-reference '" 
                    << expr.prettyprint() << "' does not have a data-sharing attribute, skipping" 
                    << std::endl;
                std::cerr
                    << expr.get_ast().get_locus() 
                    << ": info: this may be caused because the referenced data is not used in the construct"
                    << std::endl;
                continue;
            }
            else if ((ds_attr & OpenMP::DS_SHARED) == OpenMP::DS_SHARED)
            {
                is_private = false;
            }

            if (it->get_kind() == OpenMP::COPY_DIR_IN)
            {
                data_env_info.add_copy_in_item(CopyData(expr, sym, is_private));
            }
            else if (it->get_kind() == OpenMP::COPY_DIR_OUT)
            {
                data_env_info.add_copy_out_item(CopyData(expr, sym, is_private));
            }
            else if (it->get_kind() == OpenMP::COPY_DIR_INOUT)
            {
                data_env_info.add_copy_inout_item(CopyData(expr, sym, is_private));
            }
            else
            {
                internal_error("Invalid copy kind", 0);
            }

            if (is_private && it->get_kind() != OpenMP::COPY_DIR_IN)
            {
                std::cerr 
                    << expr.get_ast().get_locus()
                    << ": warning: copy out of data-reference '" << expr.prettyprint() 
                    << "' will have no effect since the related variable is private"
                    << std::endl;
            }
        }
    }

    void Nanox::fill_data_environment_structure(
            Scope sc,
            DataEnvironInfo &data_env_info,
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

        ObjectList<DataEnvironItem> data_env_item_list = data_env_info.get_items();

        for (ObjectList<DataEnvironItem>::iterator it = data_env_item_list.begin();
                it != data_env_item_list.end();
                it++)
        {
            DataEnvironItem &data_env_item(*it);

            if (data_env_item.is_private())
                continue;

            if (IS_C_LANGUAGE 
                    && data_env_item.is_vla_type())
            {
                ObjectList<Source> vla_dims = data_env_item.get_vla_dimensions(); 

                for (ObjectList<Source>::iterator it = vla_dims.begin();
                        it != vla_dims.end();
                        it++)
                {
                    struct_fields
                        << "int " << *it << ";"
                        ;
                }
            }

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

    void Nanox::fill_data_args(
            const std::string& arg_var_name,
            const DataEnvironInfo& data_env, 
            ObjectList<OpenMP::DependencyItem> dependencies,
            bool is_pointer_struct,
            Source& result)
    {
        std::string arg_var_accessor;
        ObjectList<DataEnvironItem> data_env_items = data_env.get_items();

        Source base_offset;

        std::string ptr;
        if (is_pointer_struct)
        {
            arg_var_accessor = arg_var_name + "->";
            base_offset = "((char*)(" + arg_var_name + ") + sizeof(*" + arg_var_name + "))";
        }
        else
        {
            arg_var_accessor = arg_var_name + ".";

            base_offset = "((char*)(&" + arg_var_name + ") + sizeof(" + arg_var_name + "))";
        }


        for (ObjectList<DataEnvironItem>::iterator it = data_env_items.begin();
                it != data_env_items.end();
                it++)
        {
            DataEnvironItem& data_env_item(*it);

            if (data_env_item.is_private())
                continue;

            Symbol sym = data_env_item.get_symbol();
            Type type = sym.get_type();
            const std::string field_name = data_env_item.get_field_name();

            if (data_env_item.is_vla_type())
            {
                // VLA require additional effort
                ObjectList<Source> dim_list = data_env_item.get_vla_dimensions();

                for (ObjectList<Source>::iterator it = dim_list.begin();
                        it != dim_list.end();
                        it++)
                {
                    result << arg_var_accessor << *it << "="
                        << *it
                        << ";"
                        ;
                }
            }

            if (!data_env_item.is_copy())
            {
                if (type.is_array())
                {
                    result << arg_var_accessor << field_name
                        << "= " << sym.get_name() << ";";
                }
                else
                {
                    result << arg_var_accessor << field_name
                        << "= &(" << sym.get_name() << ");";
                }
            }
            else
            {
                if (data_env_item.is_vla_type()
                        && type.is_array())
                {
                    // We have to adjust the VLA pointers for arrays
                    result << arg_var_accessor << field_name << " = "
                        << Source(base_offset) << ";"
                        ;

                    base_offset = Source() << arg_var_accessor << field_name 
                        << "+ (sizeof(" << sym.get_type().basic_type().get_declaration(sym.get_scope(), "") << ") ";

                    ObjectList<Source> dim_list = data_env_item.get_vla_dimensions();
                    for (ObjectList<Source>::iterator it = dim_list.begin();
                            it != dim_list.end();
                            it++)
                    {
                        base_offset << "* (" << *it << ")";
                    }

                    base_offset << ")";
                }

                if (type.is_array())
                {
                    C_LANGUAGE()
                    {
                        result << "__builtin_memcpy(" << arg_var_accessor << field_name << ", "
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
                    if (IS_CXX_LANGUAGE
                            && type.is_named_class())
                    {
                        result << "new (&" << arg_var_accessor << field_name << ")" 
                            << type.get_declaration(sym.get_scope(), "") 
                            << "(" << sym.get_name() << ");";
                    }
                    else
                    {
                        result << arg_var_accessor << field_name
                            << "= " << sym.get_name() << ";";
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
}
