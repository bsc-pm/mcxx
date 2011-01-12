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
                Source redeclaration, initializer;
                redeclaration
                    << new_type_spawn.get_declaration(sym.get_scope(), sym.get_name())
                    << initializer
                    << ";"
                    ;

                if (sym.has_initialization()) 
                {
                    initializer << sym.get_initialization().prettyprint()
                        ;
                }

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

            std::string field_name = data_env_info.get_field_name_for_symbol(sym);

            Type alignment_type = type;

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
                if (type.is_reference())
                    type = type.references_to();

                if (type.is_array())
                {
                    Type element_type = type.array_element();
                    // This is the "easiest" way to build a type
                    alignment_type = element_type;

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

            DataEnvironItem data_env_item(sym, type, field_name);
            data_env_item.set_alignment(alignment_type);

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
            else if (type.is_reference())
            {
                type = type.references_to().get_pointer_to().get_restrict_type();
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
            data_env_item.set_alignment(type);

            data_env_info.add_item(data_env_item);
        }

        static void private_type(Symbol sym, 
                ScopeLink sl, 
                DataEnvironInfo& data_env_info,
                ObjectList<Symbol>& converted_vlas)
        {
            Type type = sym.get_type();

            if (type.is_reference())
                type = type.references_to();

            DataEnvironItem data_env_item(sym, type, "");

            data_env_item.set_is_private(true);

            data_env_item.set_alignment(type);

            data_env_info.add_item(data_env_item);
        }

    }

    static void get_data_sharing_symbols(OpenMP::DataSharingEnvironment& data_sharing,
            ObjectList<Symbol>& shared,
            ObjectList<Symbol>& value,
            ObjectList<Symbol>& private_symbols)
    {
        ObjectList<OpenMP::DependencyItem> dependences;
        data_sharing.get_all_dependences(dependences);

        data_sharing.get_all_symbols(OpenMP::DS_SHARED, shared);
        data_sharing.get_all_symbols(OpenMP::DS_FIRSTPRIVATE, value);
        data_sharing.get_all_symbols(OpenMP::DS_PRIVATE, private_symbols);
    }

    void Nanox::compute_data_environment(
            OpenMP::DataSharingEnvironment &data_sharing,
            ScopeLink scope_link,
            DataEnvironInfo &data_env_info,
            ObjectList<Symbol>& converted_vlas)
    {
        ObjectList<Symbol> shared;
        ObjectList<Symbol> value;
        ObjectList<Symbol> private_symbols;
        get_data_sharing_symbols(data_sharing, shared, value, private_symbols);

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
            DataReference expr = it->get_copy_expression();
            Symbol sym = expr.get_base_symbol();

            OpenMP::DataSharingAttribute ds_attr = data_sharing.get_data_sharing(sym);
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

            data_env_info.add_copy_item(*it);

            // if (is_private && it->get_kind() != OpenMP::COPY_DIR_IN)
            // {
            //     std::cerr 
            //         << expr.get_ast().get_locus()
            //         << ": warning: copy out of data-reference '" << expr.prettyprint() 
            //         << "' will have no effect since the related variable is private"
            //         << std::endl;
            // }
        }
    }

    namespace Nanox
    {
        void fill_data_environment_structure(
                Scope sc,
                DataEnvironInfo &data_env_info,
                Source &struct_decl,
                Source &struct_def,
                Source &struct_name_qualifier,
                Source &struct_fields,
                std::string& struct_name,
                ObjectList<OpenMP::DependencyItem> dependencies,
                bool compiler_alignment);
    }

    void Nanox::fill_data_environment_structure(
            Scope sc,
            DataEnvironInfo &data_env_info,
            Source &struct_decl,
            Source &struct_def,
            Source &struct_name_qualifier,
            Source &struct_fields,
            std::string& struct_name,
            ObjectList<OpenMP::DependencyItem> dependencies,
            bool compiler_alignment)
    {
        std::stringstream ss;

        int data_env_struct = TL::CounterManager::get_counter(DATA_ENV_ARG_TYPE_COUNTER);
        TL::CounterManager::get_counter(DATA_ENV_ARG_TYPE_COUNTER)++;

        ss << "_nx_data_env_" << data_env_struct << "_t";
        struct_name = ss.str();

        C_LANGUAGE()
        {
            struct_def
                << "typedef "
                << "struct " << struct_name << "_tag {"
                << struct_fields
                << "}" << struct_name << ";"
                ;
        }

        CXX_LANGUAGE()
        {
            struct_decl
                << "struct " << struct_name << ";"
                ;
            struct_def
                << "struct " << struct_name_qualifier << struct_name << " {"
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

            Type t = data_env_item.get_type();

            if (t.is_reference())
                t = t.references_to();

            Source alignment;
//AQUI
            struct_fields
                << t.get_unqualified_type().get_declaration(sc, data_env_item.get_field_name()) 
                << alignment
                << ";"
            ;

            if (data_env_item.is_raw_buffer())
            {
		        if (compiler_alignment)
		        {
		            alignment << " __attribute__((aligned(" << t.get_alignment_of() << ")))";
		        }
		        else
		        {
		            alignment << " __attribute__((aligned(" << data_env_item.get_alignment().get_alignment_of() << ")))";
		        }
            }
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

            if (type.is_reference())
            {
                type = type.references_to();
            }

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
                        << "= " << sym.get_qualified_name() << ";";
                }
                else
                {
                    result << arg_var_accessor << field_name
                        << "= &(" << sym.get_qualified_name() << ");";
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
                            << sym.get_qualified_name() << ","
                            << "sizeof(" << sym.get_qualified_name() << "));"
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
                            << type_decl << "(" << sym.get_qualified_name() << "[_i]);"
                            << "}"
                            ;
                    }
                }
                else
                {
                    if (IS_CXX_LANGUAGE
                            && type.is_named_class())
                    {
                        result << "new (" << arg_var_accessor << field_name << ")" 
                            << type.get_declaration(sym.get_scope(), "") 
                            << "(" << sym.get_qualified_name() << ");";
                    }
                    else
                    {
                        result << arg_var_accessor << field_name
                            << "= " << sym.get_qualified_name() << ";";
                    }
                }
            }
        }
    }

    void Nanox::define_arguments_structure(
            const LangConstruct& ctr,
            std::string& struct_arg_type_name,
            DataEnvironInfo& data_environ_info,
            const ObjectList<OpenMP::DependencyItem>& dependences,
            Source additional_fields
            )
    {
        FunctionDefinition funct_def = ctr.get_enclosing_function();
        Symbol function_symbol = funct_def.get_function_symbol();

        Scope scope_of_struct = ctr.get_scope();
        if (function_symbol.is_member())
        {
            // Fix the scope because it will end inside the class
            // FIXME !!! -> this is the right thing // scope_of_struct = function_symbol.get_class_type().get_scope();
            scope_of_struct = function_symbol.get_scope();
        }

        Source struct_arg_type_decl, struct_arg_type_def, struct_arg_type_qualif, struct_fields;

        struct_fields
            << additional_fields;

        fill_data_environment_structure(
                scope_of_struct,
                data_environ_info,
                struct_arg_type_decl,
                struct_arg_type_def,
                struct_arg_type_qualif,
                struct_fields,
                struct_arg_type_name, 
                dependences,
                // FIXME
                /* _compiler_alignment */ true);

        Source template_header;
        if (funct_def.is_templated()
                && function_symbol.get_type().is_template_specialized_type())
        {
            Source template_params;
            template_header
                << "template <" << template_params << ">"
                ;
            Source template_args;
            ObjectList<TemplateHeader> template_header_list;

            if (!function_symbol.is_member())
            {
                template_header_list = funct_def.get_template_header();
            }
            else
            {
                template_header_list = 
                    Declaration(function_symbol.get_point_of_declaration(), ctr.get_scope_link()).get_template_header();
            }

            ObjectList<TemplateParameterConstruct> tpl_params = template_header_list.back().get_parameters();
            for (ObjectList<TemplateParameterConstruct>::iterator it2 = tpl_params.begin();
                    it2 != tpl_params.end();
                    it2++)
            {
                template_params.append_with_separator(it2->prettyprint(), ",");
            }

            template_header_list = funct_def.get_template_header();
            tpl_params = template_header_list.back().get_parameters();
            for (ObjectList<TemplateParameterConstruct>::iterator it2 = tpl_params.begin();
                    it2 != tpl_params.end();
                    it2++)
            {   
                template_args.append_with_separator(it2->get_name(), ",");
            }

            struct_arg_type_name += "< " + std::string(template_args) + ">";
        }

        Source newly_generated_code;
        newly_generated_code
            << template_header
            << struct_arg_type_def
            ;

        if (function_symbol.is_member())
        {
            if (!function_symbol.is_static())
            {
                Type this_pointer = function_symbol.get_class_type();

                if (function_symbol.get_type().is_const())
                {
                    this_pointer = this_pointer.get_const_type();
                }

                this_pointer = this_pointer.get_pointer_to();

                struct_fields
                    << this_pointer.get_declaration(scope_of_struct, "_this") << ";"
                    ;
            }

            AST_t decl_point = function_symbol.get_point_of_declaration();

            AST_t ref_tree;
            if (FunctionDefinition::predicate(decl_point))
            {
                FunctionDefinition funct_def(decl_point, ctr.get_scope_link());
                ref_tree = funct_def.get_point_of_declaration();
            }
            else 
            {
                Declaration decl(decl_point, ctr.get_scope_link());
                ref_tree = decl.get_point_of_declaration();
            }

            // This is a bit crude but allows knowing if the function is defined
            // inside the class or not
            bool is_inline_member_function = ctr.get_ast().get_enclosing_class_specifier().is_valid();

            if (!is_inline_member_function)
            {
                Source in_class_declaration;
                Source template_header_class;
                in_class_declaration 
                    << template_header
                    << struct_arg_type_decl;

                Declaration class_decl(function_symbol
                        .get_point_of_declaration()
                        .get_enclosing_class_specifier(),
                        ctr.get_scope_link());
                if (class_decl.is_templated())
                {
                    ObjectList<TemplateHeader> template_header_list = class_decl.get_template_header();

                    for (ObjectList<TemplateHeader>::iterator it = template_header_list.begin();
                            it != template_header_list.end();
                            it++)
                    {
                        Source template_params;
                        template_header_class << "template<" << template_params << ">";
                        ObjectList<TemplateParameterConstruct> tpl_params = it->get_parameters();
                        for (ObjectList<TemplateParameterConstruct>::iterator it2 = tpl_params.begin();
                                it2 != tpl_params.end();
                                it2++)
                        {
                            template_params.append_with_separator(it2->prettyprint(), ",");
                        }
                    }
                }

                AST_t in_class_declaration_tree
                    = in_class_declaration.parse_member(
                            decl_point,
                            ctr.get_scope_link(),
                            function_symbol.get_class_type().get_symbol());
                decl_point.prepend(in_class_declaration_tree);

                std::string globally_qualified 
                    = function_symbol.get_class_type().get_declaration(ctr.get_scope_link().get_scope(decl_point), "") + "::";
                // It is not valid to write 'struct ::A::B { }' but it is OK to do 'struct A::B { }'

                std::string::iterator it = globally_qualified.begin();
                // Skip blanks if any (there should not be any, lest there were)
                while (it != globally_qualified.end() && *it == ' ') it++; 
                // Skip first colon
                if (it != globally_qualified.end() && *it == ':') it++;
                // Skip second colon
                if (it != globally_qualified.end() && *it == ':') it++;

                struct_arg_type_qualif << std::string(it, globally_qualified.end());

                Source fixed_newly_generated_code;
                fixed_newly_generated_code
                    << template_header_class
                    << newly_generated_code
                    ;

                AST_t outline_code_tree
                    = fixed_newly_generated_code.parse_declaration(
                            ctr.get_ast().get_enclosing_function_definition_declaration(),
                            ctr.get_scope_link());
                ctr.get_ast().prepend_sibling_function(outline_code_tree);
            }
            else
            {
                AST_t outline_code_tree
                    = newly_generated_code.parse_member(
                            ctr.get_ast().get_enclosing_function_definition_declaration(),
                            ctr.get_scope_link(),
                            function_symbol.get_class_type().get_symbol());
                ctr.get_ast().prepend_sibling_function(outline_code_tree);
            }
        }
        else
        {
            AST_t outline_code_tree
                = newly_generated_code.parse_declaration(
                        ctr.get_ast().get_enclosing_function_definition_declaration(),
                        ctr.get_scope_link());
            ctr.get_ast().prepend_sibling_function(outline_code_tree);
        }
    }
}
