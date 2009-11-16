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

#include "tl-ompserialize.hpp"
#include "tl-omp.hpp"
#include "tl-compilerphase.hpp"
#include "tl-langconstruct.hpp"
#include "tl-predicate.hpp"
#include "tl-builtin.hpp"
#include "tl-traverse.hpp"
#include "tl-taskserialize.hpp"
#include "cxx-attrnames.h"

namespace TL
{
    namespace Nanos4
    {

        class OpenMPSerialize : public TL::CompilerPhase
        {
            public:
                OpenMPSerialize()
                {
                    set_phase_name("OpenMP Serialization");
                    set_phase_description("This phase duplicates functions removing their OpenMP constructs");
                }

                virtual void pre_run(DTO& dto)
                {
                }

                virtual void run(DTO& dto)
                {
                    AST_t translation_unit = dto["translation_unit"];
                    ScopeLink scope_link = dto["scope_link"];

                    ObjectList<Symbol> functions_to_serialize;

                    // First compute the set of functions that must be serialized
                    compute_set_of_serializable(translation_unit, scope_link, 
                            functions_to_serialize);

                    // Now 'functions_to_serialize' contains all the functions
                    // that must be serialized, now create the serial duplicate
                    create_serialized_versions(functions_to_serialize, translation_unit, scope_link);

                    // Once everything has been created add it to the IR information passed,
                    // next phase will receive this information
                    RefPtr<SerializedFunctionsInfo> info(new SerializedFunctionsInfo());
                    info->serialized_functions = functions_to_serialize;
                    dto.set_object(SERIALIZED_FUNCTIONS_INFO, info);
                }

            private:
                void compute_set_of_serializable(AST_t tree, ScopeLink scope_link,
                        ObjectList<Symbol> &functions_to_serialize)
                {
                    // First compute all functions defined
                    ObjectList<AST_t> function_definition_list 
                        = tree.depth_subtrees(FunctionDefinition::predicate);
                    ObjectList<Symbol> functions_defined;
                    for (ObjectList<AST_t>::iterator it = function_definition_list.begin();
                            it != function_definition_list.end();
                            it++)
                    {
                        FunctionDefinition function_def(*it, scope_link);
                        // FIXME - Fix this some day
                        Symbol function_symbol 
                            = function_def.get_ast().get_attribute(LANG_FUNCTION_SYMBOL);
                        functions_defined.append(function_symbol);
                    }

                    // Now for every task construct get the called functions for which we have
                    // their definition
                    ObjectList<AST_t> task_construct_list = tree.depth_subtrees(TaskConstructPred(scope_link));
                    for (ObjectList<AST_t>::iterator it = task_construct_list.begin();
                            it != task_construct_list.end();
                            it++)
                    {
                        // FIXME - This is pathetic :)
                        PragmaCustomConstruct task_construct(*it, scope_link);
                        Statement stmt = task_construct.get_statement();

                        // Now find all function calls 
                        // FIXME - We should factorize this
                        ObjectList<AST_t> function_calls = stmt.get_ast().depth_subtrees(PredicateAttr (LANG_IS_FUNCTION_CALL) );
                        for (ObjectList<AST_t>::iterator funct_call_tree = function_calls.begin();
                                funct_call_tree != function_calls.end();
                                funct_call_tree++)
                        {
                            Expression funct_call(*funct_call_tree, scope_link);
                            Expression called_entity = funct_call.get_called_expression();
                            if (called_entity.is_id_expression())
                            {
                                // Ignore functions called by indirection
                                IdExpression id_expression = called_entity.get_id_expression();
                                Symbol sym = id_expression.get_symbol();

                                if (functions_defined.contains(sym))
                                {
                                    std::cerr << "INFO: Adding '" << sym.get_name() << "' to the list of OpenMP serializable" << std::endl;

                                    // And insert it to the list of functions to serialize (this makes most of the magic
                                    // of this closure)
                                    functions_to_serialize.insert(sym);
                                }
                            }
                        }
                    }

                    // Once we have the initial set, we have to include all the functions called from
                    // the functions as serializable
                    //
                    // Note that we use 'i' as the index because we insert elements into functions_to_serialize
                    for (unsigned int i = 0; 
                            i < functions_to_serialize.size();
                            i++)
                    {
                        Symbol &it = functions_to_serialize[i];

                        ObjectList<AST_t> current_function_def 
                            = function_definition_list.filter(SpecificFunctionDef(it, scope_link));

                        if (current_function_def.empty())
                        {
                            // Ignore not defined
                            std::cerr << "WARNING: Function definition for '" << it.get_name() << "' not found" << std::endl;
                            continue;
                        }

                        FunctionDefinition function_def(current_function_def[0], scope_link);

                        Statement stmt = function_def.get_function_body();

                        // Now find all function calls 
                        // FIXME - We should factorize this
                        ObjectList<AST_t> function_calls = stmt.get_ast().depth_subtrees(PredicateAttr (LANG_IS_FUNCTION_CALL) );
                        for (ObjectList<AST_t>::iterator funct_call_tree = function_calls.begin();
                                funct_call_tree != function_calls.end();
                                funct_call_tree++)
                        {
                            Expression funct_call(*funct_call_tree, scope_link);
                            Expression called_entity = funct_call.get_called_expression();
                            if (called_entity.is_id_expression())
                            {
                                // Ignore functions called by indirection
                                IdExpression id_expression = called_entity.get_id_expression();
                                Symbol sym = id_expression.get_symbol();

                                if (functions_defined.contains(sym))
                                {
                                    std::cerr << "INFO: Adding '" << sym.get_name() << "' to the list of OpenMP serializable" << std::endl;

                                    // And insert it to the list of functions to serialize (this makes most of the magic
                                    // of this closure)
                                    functions_to_serialize.insert(sym);
                                }
                            }
                        }
                    }
                }

                void create_serialized_versions(ObjectList<Symbol> &functions_to_serialize, 
                        AST_t tree, ScopeLink scope_link)
                {
                    // Get the function_definition_list to be a bit faster here
                    ObjectList<AST_t> function_definition_list 
                        = tree.depth_subtrees(FunctionDefinition::predicate);

                    // First create the declaration of every serialized function
                    // (we do this to be able to parse references to these new functions)
                    for (ObjectList<Symbol>::iterator it = functions_to_serialize.begin();
                            it != functions_to_serialize.end();
                            it++)
                    {
                        Source function_declaration;
                        Symbol sym(*it);
                        Type type = sym.get_type();

                        function_declaration
                            << type.get_declaration(sym.get_scope(), 
                                    "__serial_" + sym.get_name() + "_") << ";";
                        // Parse just to update symbolic information
                        AST_t function_declaration_tree = 
                            function_declaration.parse_declaration(
                                    sym.get_point_of_declaration(),
                                    scope_link);

                        AST_t declaration_point = sym.get_point_of_declaration();

                        declaration_point.prepend(function_declaration_tree);
                    }

                    // Now create the definition
                    for (ObjectList<Symbol>::iterator it = functions_to_serialize.begin();
                            it != functions_to_serialize.end();
                            it++)
                    {
                        ObjectList<AST_t> current_function_def 
                            = function_definition_list.filter(SpecificFunctionDef(*it, scope_link));

                        if (current_function_def.empty())
                        {
                            // Ignore not defined
                            std::cerr << "WARNING: Function definition for '" << it->get_name() << "' not found and we wanted to serialize it" << std::endl;
                            continue;
                        }

                        FunctionDefinition function_def(current_function_def[0], scope_link);

                        serialize_one_function(function_def, functions_to_serialize);
                    }
                }

                void serialize_one_function(FunctionDefinition function_def,
                        ObjectList<Symbol> &function_syms_to_serialize)
                {
                    // FIXME - Fix this some day
                    Symbol function_symbol = function_def.get_ast().get_attribute(LANG_FUNCTION_SYMBOL);
                    Type function_type = function_symbol.get_type();
                    Type return_type = function_type.returns();
                    IdExpression function_name = function_def.get_function_name();

                    Source serialized_function;
                    Source serial_parameters;
                    AST_t function_body_placeholder;

                    serialized_function
                        << return_type.get_declaration(function_def.get_scope(), "") 
                        << " "
                        << "__serial_" << function_name.prettyprint() << "_ (" << serial_parameters << ")"
                        << "{\n"
                        <<    statement_placeholder(function_body_placeholder)
                        << "}"
                        ;

                    bool has_ellipsis;
                    ObjectList<Type> parameter_types = function_type.parameters(has_ellipsis);

                    DeclaredEntity declared_entity = function_def.get_declared_entity();
                    ObjectList<ParameterDeclaration> parameters = declared_entity.get_parameter_declarations();

                    for (ObjectList<ParameterDeclaration>::iterator it = parameters.begin();
                            it != parameters.end();
                            it++)
                    {
                        ParameterDeclaration &param(*it);
                        serial_parameters.append_with_separator(
                                param.get_type().get_declaration(
                                    function_def.get_scope(), 
                                    param.get_name().prettyprint(),
                                    Type::PARAMETER_DECLARATION
                                    ),
                                ","
                                );
                    }

                    C_LANGUAGE()
                    {
                        if (parameters.empty()
                                && !function_type.lacks_prototype())
                        {
                            serial_parameters.append_with_separator("void", ",");
                        }
                    }

                    if (has_ellipsis)
                    {
                        serial_parameters.append_with_separator("...", ",");
                    }

                    // Parse the definition
                    AST_t newly_serialized_function_def =
                        serialized_function.parse_declaration(function_def.get_ast(),
                                function_def.get_scope_link());

                    {
                        // This duplicates the function body in the context of
                        // the newly created function definition
                        Source function_body_src;
                        function_body_src << function_def.get_function_body().prettyprint();

                        // Parse
                        AST_t function_body_tree
                            = function_body_src.parse_statement(function_body_placeholder, function_def.get_scope_link());
                        // Now replace it
                        function_body_placeholder.replace(function_body_tree);

                        // Traversal
                        DepthTraverse depth_traverse;

                        // Now remove any OpenMP vestige
                        AnyOpenMPConstruct any_openmp_construct_pred(function_def.get_scope_link());
                        RemoveOpenMP remove_openmp_traverse_functor;
                        depth_traverse.add_predicate(any_openmp_construct_pred, remove_openmp_traverse_functor);

                        // Now fix function calls
                        PredicateAttr function_call_pred(LANG_IS_FUNCTION_CALL) ;
                        FixFunctionCalls fix_function_calls(function_syms_to_serialize);
                        depth_traverse.add_predicate(function_call_pred, fix_function_calls);

                        depth_traverse.traverse(function_body_tree, function_def.get_scope_link());
                    }

                    function_def.get_ast().prepend_sibling_function(newly_serialized_function_def);
                }
        };
    }
}

EXPORT_PHASE(TL::Nanos4::OpenMPSerialize);
