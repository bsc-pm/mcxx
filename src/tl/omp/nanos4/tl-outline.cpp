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

#include "tl-omptransform.hpp"

namespace TL
{
    namespace Nanos4
    {
        static void generate_static_array_initializer(Source& init, 
                ObjectList<int>::iterator current,
                ObjectList<int>::iterator end,
                Source element_level_initializer)
        {
            if (current == end)
            {
                init << element_level_initializer;
            }
            else
            {
                init << "{";
                Source sub_init;
                generate_static_array_initializer(sub_init,
                        current + 1,
                        end,
                        element_level_initializer);
                for (int i = 0; i < *current; i++)
                {
                    init << sub_init;
                    if ((i + 1) != *current)
                    {
                        init << ",";
                    }
                }
                init << "}";
            }
        }

        static void generate_dynamic_array_initializer(Source& init,
                Type type,
                Source array_name,
                Source element_level_initializer,
                int n = 0)
        {
            if (!type.is_array())
            {
                Source array_item;

                init << array_item << "=" << element_level_initializer << ";";

                array_item << array_name;
                for (int i = 0; i < n; i++)
                {
                    array_item << "[_i" << i << "]";
                }
            }
            else
            {
                Source inner_init;

                init
                    << "{"
                    << "int _i" << n << ";"
                    << "for (_i" << n << " = 0; _i" << n << " < (" << type.array_dimension().prettyprint() << "); _i" << n << "++)"
                    << "{"
                    <<     inner_init
                    << "}"
                    << "}"
                    ;

                generate_dynamic_array_initializer(inner_init,
                        type.array_element(),
                        array_name,
                        element_level_initializer,
                        n + 1);
            }
        }


        Source OpenMPTransform::get_outline_common(
                FunctionDefinition function_definition,
                Source& specific_body,
                Source outlined_function_name,
                ObjectList<ParameterInfo> parameter_info_list,
                PragmaCustomConstruct &construct,
                bool team_parameter
                )
        {
            Source formal_parameters, 
                static_qualifier, 
                forward_declaration, 
                inline_attribute, 
                template_header;

            IdExpression function_name = function_definition.get_function_name();

            Source instrumentation_code_before, instrumentation_code_after;
            instrumentation_outline(instrumentation_code_before, 
                    instrumentation_code_after, 
                    function_definition, 
                    outlined_function_name,
                    construct);

            Source vla_castings;

            Source result;
            result
                << forward_declaration
                << template_header
                << static_qualifier
                << inline_attribute
                << "void " << outlined_function_name << "(" << formal_parameters << ")"
                << "{"
                <<    vla_castings
                <<    instrumentation_code_before
                <<    specific_body
                <<    instrumentation_code_after
                << "}"
                ;

            if (allow_inlining_of_outlines)
            {
                inline_attribute
                    << " __inline__ "
                    ;
            }

            Symbol function_symbol = function_definition.get_ast().get_attribute(LANG_FUNCTION_SYMBOL);

            if (function_definition.is_templated())
            {
                ObjectList<TemplateHeader> template_headers = function_definition.get_template_header();
                // std::cerr << "(1) Num templates " << template_headers.size() << std::endl;
                for (ObjectList<TemplateHeader>::iterator it = template_headers.begin();
                        it != template_headers.end();
                        it++)
                {
                    ObjectList<std::string> template_parameters = 
                        it->get_parameters().map(functor<std::string, TemplateParameterConstruct>(&LangConstruct::prettyprint));
                    template_header << "template <" << concat_strings(template_parameters, ",") << ">";
                }
            }

            // If the function is a member and is not qualified we need an additional
            // static here
            if (function_symbol.is_member() 
                    && !function_name.is_qualified())
            {
                static_qualifier << "static ";
            }

            formal_parameters = get_formal_parameters(
                    function_definition, 
                    parameter_info_list,
                    function_definition.get_function_body().get_scope(),
                    team_parameter);

            if (!function_symbol.is_member())
            {
                // We want to forward the declaration
                Declaration point_of_decl = function_name.get_declaration();
                DeclarationSpec decl_specs = point_of_decl.get_declaration_specifiers();
                ObjectList<DeclaredEntity> declared_entities = point_of_decl.get_declared_entities();
                DeclaredEntity declared_entity = *(declared_entities.begin());

                forward_declaration 
                    << template_header
                    << decl_specs.prettyprint()
                    << " "
                    << declared_entity.prettyprint()
                    << ";";
            }

            C_LANGUAGE()
            {
                // VLA castings
                for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                        it != parameter_info_list.end();
                        it++)
                {
                    ParameterInfo &param_info(*it);
                    if (!param_info.is_variably_modified)
                        continue;

                    // Now add a casting
                    vla_castings
                        << param_info.type_in_outline.get_declaration(
                                function_definition.get_function_body().get_scope(),
                                param_info.vla_cast_name)
                        << " = " 
                        << "("
                        << param_info.type_in_outline.get_declaration(
                                function_definition.get_function_body().get_scope(),
                                "")
                        << ")"
                        << param_info.parameter_name
                        << ";"
                        ;
                }
            }

            return result;
        }

        Source OpenMPTransform::get_formal_parameters(
                FunctionDefinition function_definition,
                ObjectList<ParameterInfo> parameter_info_list,
                Scope decl_scope,
                bool team_parameter)
        {
            int num_params = 0;
            Source formal_parameters;

            if (team_parameter)
            {
                // A team parameter is required
                formal_parameters
                    .append_with_separator("nth_team_t* nth_current_team", ",");
                num_params++;
            }

            // Add _this if needed
            if (is_nonstatic_member_function(function_definition))
            {
                IdExpression function_name = function_definition.get_function_name();

                Statement function_body = function_definition.get_function_body();
                Scope function_body_scope = function_body.get_scope();
                Symbol this_symbol = function_body_scope.get_symbol_from_name("this");

                // decl_scope.printscope();

                Type class_type = this_symbol.get_type();
                formal_parameters.append_with_separator(
                        // Fix this scope
                        class_type.get_declaration(decl_scope, "_this", Type::PARAMETER_DECLARATION), 
                        ",");
                num_params++;
            }

            // First the pointer ones
            for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                    it != parameter_info_list.end();
                    it++)
            {
                if (it->kind != ParameterInfo::BY_POINTER)
                    continue;

                Type type = it->type;
                std::string name = it->parameter_name;

                formal_parameters.append_with_separator(
                        type.get_declaration(decl_scope, name, Type::PARAMETER_DECLARATION), 
                        ",");
                num_params++;
            }

            // Now the value ones
            for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                    it != parameter_info_list.end();
                    it++)
            {
                if (it->kind != ParameterInfo::BY_VALUE)
                    continue;

                Type type = it->type;
                std::string name = it->parameter_name;

                formal_parameters.append_with_separator(
                        type.get_declaration(decl_scope, name, Type::PARAMETER_DECLARATION)
                        , ",");
                num_params++;
            }

            if (num_params == 0)
            {
                formal_parameters << "void";
            }

            return formal_parameters;
        }

        Source OpenMPTransform::get_privatized_declarations(
                PragmaCustomConstruct &construct,
                ObjectList<Symbol> private_references,
                ObjectList<Symbol> firstprivate_references,
                ObjectList<Symbol> lastprivate_references,
                ObjectList<OpenMP::ReductionSymbol> reduction_references,
                ObjectList<Symbol> copyin_references,
                ObjectList<ParameterInfo> parameter_info_list
                )
        {
            Source private_declarations;

            ObjectList<Symbol> pruned_lastprivate_references;
            pruned_lastprivate_references
                .append(lastprivate_references.filter(
                            not_in_set(firstprivate_references)));

            // PRIVATE
            for (ObjectList<Symbol>::iterator it = private_references.begin();
                    it != private_references.end();
                    it++)
            {
                Symbol &sym(*it);
                Type type = sym.get_type();

                private_declarations << 
                    comment("Private entity : '" + sym.get_qualified_name() + "'");
                private_declarations
                    << type.get_declaration(
                            construct.get_scope(),
                            "p_" + sym.get_name())
                    << ";"
                    ;
            }

            // LASTPRIVATE
            for (ObjectList<Symbol>::iterator it = pruned_lastprivate_references.begin();
                    it != pruned_lastprivate_references.end();
                    it++)
            {
                Symbol &sym(*it);
                Type type = sym.get_type();

                private_declarations
                    << comment("Lastprivate entity : 'p_" + sym.get_name() + "'")
                    << type.get_declaration(
                            construct.get_scope(),
                            "p_" + sym.get_name())
                    << ";"
                    ;
            }

            // REDUCTION
            for (ObjectList<OpenMP::ReductionSymbol>::iterator it = reduction_references.begin();
                    it != reduction_references.end();
                    it++)
            {
                Symbol sym = it->get_symbol();
                OpenMP::UDRInfoItem udr = it->get_udr();
                Type type = sym.get_type();

                Source type_declaration, static_initializer, dynamic_initializer, secondary_decl;

                private_declarations
                    << comment("Reduction private entity : 'rdp_" + sym.get_name() + "'")
                    << type_declaration
                    << static_initializer << ";"
                    << dynamic_initializer
                    << secondary_decl
                    ;

                bool is_variably_modified = type.is_variably_modified();

                std::string name = "rdp_" + sym.get_name();
                bool is_pointer = type.is_pointer();
                if (is_pointer)
                {
                    name = "p_rdp_" + sym.get_name();
                    type = type.points_to();
                }
                if (!udr.get_is_array_reduction()
                        || !is_variably_modified)
                {
                    type_declaration
                        << type.get_declaration(
                                construct.get_scope(),
                                name)
                        ;
                }
                else
                {
                    // This is a bit more involved
                    bool found = false;
                    for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                            it != parameter_info_list.end();
                            it++)
                    {
                        if (it->symbol == sym)
                        {
                            // Use the parameter type instead, but ignore an
                            // additional pointer we added for proper casting
                            type = it->full_type_in_outline.points_to();
                            found = true;
                            break;
                        }
                    }

                    if (!found)
                    {
                        internal_error("VLA info not found", 0);
                    }

                    type_declaration
                        << type.get_declaration(
                                construct.get_scope(),
                                name);
                        ;
                }

                if (is_pointer)
                {
                    secondary_decl
                        << type.get_pointer_to().get_declaration(
                                construct.get_scope(),
                                "rdp_" + sym.get_name()) << "= &" << name << ";";
                }

                // Fill the initializers
                if (!udr.has_identity())
                {
                    if (!udr.get_is_array_reduction())
                    {
                        if (udr.identity_is_constructor())
                        {
                            static_initializer << udr.get_identity()
                                ;
                        }
                        else
                        {
                            static_initializer << " = " << udr.get_identity()
                                ;
                        }
                    }
                    else // is array
                    {
                        Source element_level_initializer;

                        if (udr.identity_is_constructor())
                        {
                            // Prepend with the constructor name
                            element_level_initializer
                                << type.get_declaration(construct.get_scope(), "")
                                << udr.get_identity();
                        }
                        else
                        {
                            element_level_initializer 
                                << udr.get_identity();
                        }

                        if (!is_variably_modified)
                        {
                            ObjectList<int> dimension_sizes(udr.get_num_dimensions(), 0);
                            Type array_type = type;

                            for (int i = 0; i < udr.get_num_dimensions(); i++)
                            {
                                Expression expr(array_type.array_dimension(),
                                        construct.get_scope_link());
                                bool valid;
                                dimension_sizes[i] = expr.evaluate_constant_int_expression(valid);
                                if (!valid)
                                {
                                    internal_error("Error when evaluating constant expression '%s' of non vla array!\n", 
                                            expr.prettyprint().c_str());
                                }
                            }

                            static_initializer << " = ";

                            generate_static_array_initializer(static_initializer, 
                                    dimension_sizes.begin(), 
                                    dimension_sizes.end(), 
                                    element_level_initializer);
                        }
                        else
                        {
                            generate_dynamic_array_initializer(dynamic_initializer,
                                    type,
                                    Source("rdp_" + sym.get_name()),
                                    element_level_initializer);
                        }
                    }
                }
            }

            // COPYIN
            for (ObjectList<Symbol>::iterator it = copyin_references.begin();
                    it != copyin_references.end();
                    it++)
            {
                Symbol &sym (*it);
                Source copyin_code;
                private_declarations
                    << comment("Initializing copyin entity '" + sym.get_name() + "'")
                    << copyin_code
                    ;

                if (!sym.get_type().is_array())
                {
                    copyin_code << 
                        sym.get_qualified_name(construct.get_scope()) << " = " << "(*cin_" + sym.get_name() << ");"
                        ;
                }
                else
                {
                    copyin_code <<
                        array_copy(sym.get_type(), 
                                sym.get_qualified_name(construct.get_scope()),
                                "(*cin_" + sym.get_name() + ")", 0);
                }
            }

            return private_declarations;
        }

        Source OpenMPTransform::get_privatized_declarations_inline(
                PragmaCustomConstruct &construct,
                ObjectList<Symbol> private_references,
                ObjectList<Symbol> firstprivate_references,
                ObjectList<Symbol> lastprivate_references,
                ObjectList<OpenMP::ReductionSymbol> reduction_references,
                ObjectList<Symbol> copyin_references
                )
        {
            Source private_declarations;

            ObjectList<Symbol> pruned_lastprivate_references;
            pruned_lastprivate_references
                .append(lastprivate_references.filter(
                            not_in_set(firstprivate_references)));

            // PRIVATE
            for (ObjectList<Symbol>::iterator it = private_references.begin();
                    it != private_references.end();
                    it++)
            {
                Symbol &sym(*it);
                Type type = sym.get_type();

                private_declarations << 
                    comment("Private entity : '" + sym.get_qualified_name() + "'");
                private_declarations
                    << type.get_declaration(
                            construct.get_scope(),
                            "p_" + sym.get_name())
                    << ";"
                    ;
            }

            // FIRSTPRIVATE
            for (ObjectList<Symbol>::iterator it = firstprivate_references.begin();
                    it != firstprivate_references.end();
                    it++)
            {
                Symbol &sym(*it);
                Type type = sym.get_type();

                Source initializer_value;
                initializer_value << sym.get_qualified_name(construct.get_scope());

                private_declarations << 
                    comment("Firstprivate entity : 'p_" + sym.get_name() + "'");

                if (type.is_array())
                {
                    // Both in C and C++ the firstprivatized array must be properly copied
                    private_declarations 
                        << type.get_declaration(
                                construct.get_scope(),
                                "p_" + sym.get_name())
                        << ";"
                        ;

                    private_declarations 
                        << comment("This firstprivate entity is an array and must be initialized element-wise");

                    Source array_assignment = array_copy(type, "p_" + sym.get_name(),
                            initializer_value.get_source(), 0);

                    private_declarations << array_assignment;
                }
                else
                {
                    C_LANGUAGE()
                    {
                        // If it is not an array just assign
                        private_declarations 
                            << type.get_declaration(
                                    construct.get_scope(),
                                    "p_" + sym.get_name())
                            << ";"
                            << comment("Using plain assignment to initialize firstprivate entity")
                            << "p_" + sym.get_name() << "=" << initializer_value.get_source() << ";"
                            ;
                    }
                    CXX_LANGUAGE()
                    {
                        // In C++ if this is a class we invoke the copy-constructor
                        if (type.is_class())
                        {
                            private_declarations 
                                << comment("Using copy constructor to initialize firstprivate entity")
                                << type.get_declaration(
                                        construct.get_scope(),
                                        "p_" + sym.get_name())
                                << "(" << initializer_value.get_source() << ")"
                                << ";"
                                ;
                        }
                        else
                        {
                            // Otherwise simply assign
                            private_declarations 
                                << type.get_declaration(
                                        construct.get_scope(),
                                        "p_" + sym.get_name())
                                << ";"
                                << comment("Using assignment operator to initialize firstprivate entity")
                                << "p_" + sym.get_name() << "=" << initializer_value.get_source() << ";"
                                ;
                        }
                    }
                }
            }

            // LASTPRIVATE
            for (ObjectList<Symbol>::iterator it = pruned_lastprivate_references.begin();
                    it != pruned_lastprivate_references.end();
                    it++)
            {
                Symbol &sym(*it);
                Type type = sym.get_type();

                private_declarations
                    << comment("Lastprivate entity : 'p_" + sym.get_name() + "'")
                    << type.get_declaration(
                            construct.get_scope(),
                            "p_" + sym.get_name())
                    << ";"
                    ;
            }

            // REDUCTION
            for (ObjectList<OpenMP::ReductionSymbol>::iterator it = reduction_references.begin();
                    it != reduction_references.end();
                    it++)
            {
                Symbol sym = it->get_symbol();
                Type type = sym.get_type();
                OpenMP::UDRInfoItem udr = it->get_udr();

                Source init;

                private_declarations
                    << comment("Reduction private entity : 'rdp_" + sym.get_name() + "'")
                    << type.get_declaration(
                            construct.get_scope(),
                            "rdp_" + sym.get_name())
                    << init
                    << ";"
                    ;

                if (udr.identity_is_constructor())
                {
                    init << udr.get_identity()
                        ;
                }
                else if (udr.has_identity())
                {
                    // Do nothing for empty initializers
                }
                else
                {
                    init << " = " << udr.get_identity()
                        ;
                }
            }

            // COPYIN
            for (ObjectList<Symbol>::iterator it = copyin_references.begin();
                    it != copyin_references.end();
                    it++)
            {
                Symbol &sym (*it);
                Source copyin_code;
                private_declarations
                    << comment("Initializing copyin entity '" + sym.get_name() + "'")
                    << copyin_code
                    ;

                if (!sym.get_type().is_array())
                {
                    copyin_code << 
                        sym.get_qualified_name(construct.get_scope()) << " = " << "(*cin_" + sym.get_name() << ");"
                        ;
                }
                else
                {
                    copyin_code <<
                        array_copy(sym.get_type(), 
                                sym.get_qualified_name(construct.get_scope()),
                                "(*cin_" + sym.get_name() + ")", 0);
                }
            }

            return private_declarations;
        }

        Source OpenMPTransform::get_lastprivate_assignments(
                ObjectList<Symbol> firstprivate_references,
                ObjectList<Symbol> lastprivate_references,
                ObjectList<Symbol> copyprivate_references,
                ObjectList<ParameterInfo> parameter_info_list)
        {
            Source lastprivate_assignments;
            // LASTPRIVATE
            for (ObjectList<Symbol>::iterator it = lastprivate_references.begin();
                    it != lastprivate_references.end();
                    it++)
            {
                Symbol &symbol(*it);
                Type type = symbol.get_type();

                std::string output_object;

                if (parameter_info_list.contains(functor(&ParameterInfo::symbol), symbol))
                {
                    Type t = symbol.get_type();

                    if (t.is_array())
                    {
                        output_object = symbol.get_name();
                    }
                    else
                    {
                        output_object = "(*" + symbol.get_name() + ")";
                    }
                }
                else
                {
                    output_object = symbol.get_name();
                }

                lastprivate_assignments
                    << comment("Assignment of lastprivate entity: '" + output_object + "'");

                std::string input_object = "p_" + symbol.get_name();

                if (firstprivate_references.contains(symbol))
                {
                    input_object = "(*flp_" + symbol.get_name() + ")";
                }

                if (type.is_array())
                {
                    Source array_assignment = array_copy(type, output_object,
                            input_object, 0);

                    lastprivate_assignments 
                        << comment("Entity is an array and must be assigned element-wise")
                        << array_assignment;
                }
                else
                {
                    lastprivate_assignments
                        << output_object << " = " << input_object << ";"
                        ;
                }
            }

            // COPYPRIVATE
            for (ObjectList<Symbol>::iterator it = copyprivate_references.begin();
                    it != copyprivate_references.end();
                    it++)
            {
                Source copyprivate_code;
                Symbol &symbol(*it);
                lastprivate_assignments
                    << comment("Assignment of copyprivate entity 'cout_" + symbol.get_name() + "'")
                    << copyprivate_code
                    ;

                if (!symbol.get_type().is_array())
                {
                    copyprivate_code
                        << "(*cout_" << symbol.get_name() << ")" << " = p_" << symbol.get_name() << ";"
                        ;
                }
                else
                {
                    copyprivate_code <<
                        array_copy(symbol.get_type(), 
                                "(*cout_" + symbol.get_name() + ")",
                                "p_" + symbol.get_name(),
                                0);
                }
            }

            return lastprivate_assignments;
        }

        Source OpenMPTransform::get_lastprivate_assignments_inline(
                ObjectList<Symbol> lastprivate_references,
                ObjectList<Symbol> copyprivate_references)
        {
            Source lastprivate_assignments;
            // LASTPRIVATE
            for (ObjectList<Symbol>::iterator it = lastprivate_references.begin();
                    it != lastprivate_references.end();
                    it++)
            {
                Symbol &symbol(*it);
                Type type = symbol.get_type();

                std::string output_object;

                // FIXME - This must be the fully qualified name
                output_object = symbol.get_name();

                lastprivate_assignments
                    << comment("Assignment of lastprivate entity: '" + output_object + "'");

                if (type.is_array())
                {
                    Source array_assignment = array_copy(type, output_object,
                            "p_" + symbol.get_name(), 0);

                    lastprivate_assignments 
                        << comment("Entity is an array and must be assigned element-wise")
                        << array_assignment;
                }
                else
                {
                    lastprivate_assignments
                        << output_object << " = p_" << symbol.get_name() << ";"
                        ;
                }
            }

            // COPYPRIVATE
            for (ObjectList<Symbol>::iterator it = copyprivate_references.begin();
                    it != copyprivate_references.end();
                    it++)
            {
                Source copyprivate_code;
                Symbol &symbol(*it);
                lastprivate_assignments
                    << comment("Assignment of copyprivate entity 'cout_" + symbol.get_name() + "'")
                    << copyprivate_code
                    ;

                if (!symbol.get_type().is_array())
                {
                    copyprivate_code
                        << "(*cout_" << symbol.get_name() << ")" << " = p_" << symbol.get_name() << ";"
                        ;
                }
                else
                {
                    copyprivate_code <<
                        array_copy(symbol.get_type(), 
                                "(*cout_" + symbol.get_name() + ")",
                                "p_" + symbol.get_name(),
                                0);
                }
            }

            return lastprivate_assignments;
        }

        Source OpenMPTransform::get_outlined_function_name(IdExpression function_name, 
                bool want_fully_qualified /* = true */, 
                bool want_templated_name /* = false */)
        {
            Source result;
            if (function_name.is_qualified() && want_fully_qualified)
            {
                result
                    << function_name.get_qualified_part()
                    ;
            }
            if (function_name.is_qualified() && want_templated_name)
            {
                result << " template ";
            }

            result
                << "nth__" << function_name.get_unqualified_part() << "_" << num_parallels;

            return result;
        }

        Source OpenMPTransform::array_copy(Type t, const std::string& dest, const std::string& orig, int level)
        {
            Source result;

            if (use_memcpy_always)
            {
                result
                    << "__builtin_memcpy(" << dest << ", " << orig << ", sizeof(" << dest << "));"
                    ;
            }
            else
            {
                std::stringstream subscript;

                for (int i = 0; i < level; i++)
                {
                    subscript << "[_i_" << i << "]";
                }

                if (!t.is_array())
                {
                    result 
                        << dest << subscript.str() << "=" << orig << subscript.str() << ";"
                        ;
                }
                else
                {
                    std::stringstream index_var;
                    index_var << "_i_" << level;

                    Source next_dim_array_copy = array_copy(t.array_element(), dest, orig, level+1);

                    result 
                        << "{"
                        << "  int " << index_var.str() << ";"
                        << "  for (" << index_var.str() << " = 0;" 
                        <<              index_var.str() 
                        <<                 " < (sizeof(" << dest 
                        <<                 subscript.str() << ")/sizeof(" << dest << subscript.str() << "[0]));"
                        <<              index_var.str() << "++" << ")"
                        << "  {"
                        <<       next_dim_array_copy
                        << "  }"
                        << "}"
                        ;
                }
            }

            return result;
        }


        void OpenMPTransform::instrumentation_outline(Source& instrumentation_code_before,
                Source& instrumentation_code_after,
                FunctionDefinition function_definition,
                Source outlined_function_name,
                PragmaCustomConstruct &construct)
        {
            if (instrumentation_requested())
            {
                std::string file_name = "\"" + function_definition.get_ast().get_file() + "\"";

                int file_line = construct.get_ast().get_line();

                instrumentation_code_before
                    << "nth_instrumentation_ctx ctx;"
                    << "nth_instrument_push_ctx(&ctx, " << file_name << ", " << file_line << ", \"" << outlined_function_name << "\");"
                    ;

                instrumentation_code_after
                    << "nth_instrument_pop_ctx();"
                    ;
            }
        }

        Source OpenMPTransform::get_member_function_declaration(
                FunctionDefinition function_definition,
                Declaration function_declaration,
                Source outlined_function_name,
                ObjectList<ParameterInfo> parameter_info_list,
                bool team_parameter
                )
        {
            Source result;
            Source formal_parameters;
            Source template_header;
            Scope decl_scope = function_declaration.get_scope();

            result
                << template_header
                << "static void " << outlined_function_name << "(" << formal_parameters << ");"
                ;

            if (function_declaration.is_templated())
            {
                ObjectList<TemplateHeader> template_headers = function_declaration.get_template_header();
                // std::cerr << "(3) Num templates " << template_headers.size() << std::endl;
                for (ObjectList<TemplateHeader>::iterator it = template_headers.begin();
                        it != template_headers.end();
                        it++)
                {
                    ObjectList<std::string> template_parameters = 
                        it->get_parameters().map(functor<std::string, TemplateParameterConstruct>(&LangConstruct::prettyprint));
                    template_header << "template <" << concat_strings(template_parameters, ",") << ">";
                }
            }

            formal_parameters = get_formal_parameters(function_definition, 
                    parameter_info_list,
                    decl_scope, team_parameter);

            return result;
        }

        // This function computes a proper reference to the function
        std::string OpenMPTransform::get_outline_function_reference(FunctionDefinition function_definition,
                ObjectList<ParameterInfo>& parameter_info_list,
                bool team_parameter)
        {
            IdExpression function_name = function_definition.get_function_name();
            Symbol function_symbol = function_definition.get_ast().get_attribute(LANG_FUNCTION_SYMBOL);

            Source outlined_function_name_decl;

            // We have to ensure that this qualification refers to the proper function
            // in C++ this is achieved via a casting. A cast of an overload function name
            // does not obey unconditionally the programmer but selects the proper overloaded
            // function (if any, otherwise the program is ill-formed)
            if (function_symbol.is_function()
                    && function_symbol.get_type().is_template_specialized_type())
            {
                Source overload_selector_cast_parameters;

                // A team parameter is required
                if (team_parameter)
                {
                    overload_selector_cast_parameters.append_with_separator(
                            "nth_team_t* nth_current_team", ",");
                }

                if (is_nonstatic_member_function(function_definition))
                {
                    // Do not forget the "this" type
                    Statement function_body = function_definition.get_function_body();
                    Scope function_body_scope = function_body.get_scope();

                    Symbol this_symbol = function_body_scope.get_symbol_from_name("this");

                    // decl_scope.printscope();
                    Type class_type = this_symbol.get_type();

                    overload_selector_cast_parameters.append_with_separator(
                            class_type.get_declaration(function_body_scope, ""),
                            ",");
                }

                for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                        it != parameter_info_list.end();
                        it++)
                {
                    overload_selector_cast_parameters.append_with_separator(
                            it->type.get_declaration(function_definition.get_scope(), ""),
                            ",");
                }

                outlined_function_name_decl << "(void(*)(" << overload_selector_cast_parameters << "))";

                ObjectList<TemplateHeader> template_headers = function_definition.get_template_header();
                // std::cerr << "(2) Num templates " << template_headers.size() << std::endl;

                Source outlined_function_name = get_outlined_function_name(function_name, /*qualif=*/true, 
                        /*template=*/ !template_headers.empty());
                outlined_function_name_decl << outlined_function_name;

                if (!template_headers.empty())
                {
                    outlined_function_name_decl << "<";
                    // Get the last template parameter
                    TemplateHeader &last_template_header = *(template_headers.rbegin());

                    ObjectList<std::string> template_parameters = 
                        last_template_header.get_parameters().map(functor(&TemplateParameterConstruct::get_name));

                    outlined_function_name_decl << concat_strings(template_parameters, ",");

                    outlined_function_name_decl << ">";
                }
            }
            else
            {
                Source outlined_function_name = get_outlined_function_name(function_name);
                outlined_function_name_decl << outlined_function_name;
            }

            return outlined_function_name_decl.get_source();
        }

        void OpenMPTransform::declare_member_if_needed(Symbol function_symbol,
                FunctionDefinition function_definition,
                IdExpression function_name,
                ObjectList<ParameterInfo> parameter_info_list,
                bool team_parameter)
        {
            // If the function is a member and is qualified (therefore the
            // function definition is outside the class) we have to create
            // an additional declaration for the new member
            if (function_symbol.is_member() 
                    && function_name.is_qualified())
            {
                Source outline_function_decl = get_outlined_function_name(function_name, /*qualified=*/false);

                Declaration decl = function_name.get_declaration();
                Scope class_scope = decl.get_scope();
                Type class_type = function_symbol.get_class_type();

                Source member_declaration = get_member_function_declaration(
                        function_definition,
                        decl,
                        outline_function_decl,
                        parameter_info_list,
                        team_parameter);

                AST_t member_decl_tree = member_declaration.parse_member(decl.get_point_of_declaration(), 
                        decl.get_scope_link(), class_type);

                decl.get_ast().append(member_decl_tree);
            }
        }

        AST_t OpenMPTransform::finish_outline(FunctionDefinition function_definition, 
                Source outline_parallel,
                ObjectList<ParameterInfo> parameter_info_list,
                bool team_parameter)
        {
            IdExpression function_name = function_definition.get_function_name();
            Symbol function_symbol = function_definition.get_ast().get_attribute(LANG_FUNCTION_SYMBOL);
            declare_member_if_needed(function_symbol, 
                    function_definition, 
                    function_name, 
                    parameter_info_list,
                    team_parameter);

            AST_t result;

            result = outline_parallel.parse_declaration(function_definition.get_point_of_declaration(), 
                    function_definition.get_scope_link());

            return result;
        }

        void OpenMPTransform::invoke_destructors(ObjectList<ParameterInfo> parameter_info_list, Source &destructor_calls)
        {
            CXX_LANGUAGE()
            {
                for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                        it != parameter_info_list.end();
                        it++)
                {
                    if (it->kind == ParameterInfo::BY_VALUE)
                    {
                        Type type = it->symbol.get_type();
                        if (type.is_reference())
                        {
                            type = type.references_to();
                        }

                        if (type.is_named_class())
                        {
                            Symbol class_name = type.get_symbol();

                            // FIXME - We need the scope to properly write the qualified name
                            destructor_calls
                                << "(*" << it->parameter_name << ")." << class_name.get_qualified_name() << "::~" << class_name.get_name() << "();";
                        }
                    }
                }
            }
        }
    }
}
