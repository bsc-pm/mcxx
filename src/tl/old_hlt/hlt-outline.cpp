/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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




#include "hlt-outline.hpp"
#include "hlt-exception.hpp"
#include "cxx-utils.h"
#include <algorithm>
#include <functional>

using namespace TL::HLT;

int Outline::_num_outlines = 0;

Outline::Outline(ScopeLink sl, Statement stmt)
    : _sl(sl),
    _function_def(NULL),
    _packed_arguments(false), 
    _do_not_embed(false), 
    _use_nonlocal_scope(true),
    _outline_num(_num_outlines++),
    _outline_performed(false),
    _overriden_outline_name(false),
    _default_parameter_passing(POINTER)
{
    _outline_statements.append(stmt);
}

Outline::Outline(ScopeLink sl, ObjectList<Statement> stmt_list)
    : _sl(sl),
    _function_def(NULL),
    _packed_arguments(false), 
    _do_not_embed(false), 
    _use_nonlocal_scope(true),
    _outline_statements(stmt_list),
    _outline_num(_num_outlines++),
    _outline_performed(false),
    _overriden_outline_name(false),
    _default_parameter_passing(POINTER)
{
}

TL::Source Outline::get_source()
{
    do_outline();

    if (!_do_not_embed)
    {
        // Now embed the outline
        embed_outline();
    }

    return _outlined_source;
}

Outline& Outline::use_packed_arguments()
{
    _packed_arguments = true;
    return *this;
}

Outline& Outline::do_not_embed()
{
    _do_not_embed = true;
    return *this;
}

void Outline::do_outline()
{
    if (_outline_performed)
        return;
    
    _outline_performed = true;

    // We can start building the outline code
    Source template_headers,
           template_headers_fwd,
           required_qualification,
           outline_parameters,
           outline_body,
           static_qualifier,
           forward_declarations;

    _outlined_source
        << forward_declarations
        << template_headers
        << static_qualifier
        << "void " << required_qualification << _outline_name << "(" << outline_parameters << ")"
        << outline_body
        ;

    // This gets some information about the enclosing function
    compute_outline_name(template_headers_fwd, template_headers, 
            required_qualification, static_qualifier);

    // Now find out all the required symbols
    compute_referenced_entities();

    outline_parameters = get_parameter_declarations(_sl.get_scope(_outline_statements[0].get_ast()));

    if (!_enclosing_function.is_member())
    {
        compute_additional_declarations(template_headers_fwd, _sl.get_scope(_outline_statements[0].get_ast()));
    }
    else
    {
        // Additional declarations will go inside the class
        compute_additional_declarations(template_headers_fwd, _enclosing_function.get_class_type().get_symbol().get_scope());
    }

    compute_outlined_body(outline_body);

    if (_is_member)
    {
        if (!_is_inlined_member)
        {
            // For non inlined member functions we need some more things
            declare_members(template_headers_fwd);
        }
        else
        {
            fill_member_forward_declarations(template_headers_fwd, forward_declarations);
        }
    }
    else if (!_is_member)
    {
        fill_nonmember_forward_declarations(template_headers_fwd, forward_declarations);
    }

}

static std::string template_header_regeneration(TL::TemplateHeader template_header)
{
    using namespace TL;
    ObjectList<std::string> template_parameters 
        = template_header.get_parameters().map(
                functor<std::string, TemplateParameterConstruct>(&LangConstruct::prettyprint)
                );

    return "template <" + concat_strings(template_parameters, ",") + " >";
}

void Outline::compute_outline_name(Source &template_headers_fwd, 
        Source &template_headers,
        Source &required_qualification, 
        Source &static_qualifier)
{
    // Note: We are assuming all statements come from the same function
    // definition
    // This disqualifies empty lists of statements
    _function_def = new FunctionDefinition(_outline_statements[0].get_enclosing_function());
    _enclosing_function = _function_def->get_function_symbol();

    _is_member = _enclosing_function.is_member();

    IdExpression id_expr = _function_def->get_function_name();

    // FIXME - This is a bit lame
    _is_inlined_member = (!id_expr.is_qualified() && _is_member);

    if (id_expr.is_qualified())
    {
        required_qualification
            << id_expr.get_qualified_part() 
            ;
    }

    _is_templated = _function_def->is_templated();
    _has_linkage_specifier = _function_def->has_linkage_specifier();
    if (_is_templated)
    {
        _template_header = _function_def->get_template_header();

        template_headers <<
            concat_strings(_template_header.map(functor(template_header_regeneration)))
            ;

        if (!_is_member)
        {
            template_headers_fwd << template_headers;
        }
        else
        {
            if (_enclosing_function.get_class_type()
                    .get_symbol().get_type().is_template_specialized_type())
            {

                ObjectList<TemplateHeader> one_less_template_header(_template_header.begin() + 1, _template_header.end());
                template_headers_fwd <<
                    concat_strings(one_less_template_header.map(functor(template_header_regeneration)))
                    ;
            }
            else
            {
                template_headers_fwd << template_headers;
            }
        }
    }
    else if (_has_linkage_specifier)
    {
        ObjectList<LinkageSpecifier> linkage_specifiers = _function_def->get_linkage_specifier();
        template_headers_fwd << concat_strings(linkage_specifiers, " ");
    }

    if (!_overriden_outline_name)
    {
        _outline_name
            << "_ol_" << _outline_num << "_" << _enclosing_function.get_name()
            ;
    }

    if (_is_member && _is_inlined_member)
    {
        static_qualifier = Source("static ");
    }
}

static void get_referenced_entities(TL::Statement stmt, TL::ObjectList<TL::Symbol>* entities)
{
    TL::ObjectList<TL::IdExpression> local_list = stmt.non_local_symbol_occurrences(TL::Statement::ONLY_VARIABLES);
    entities->insert(local_list.map(functor(&TL::IdExpression::get_symbol)));
}

struct GetDeclarationInScope 
{
    private:
        Outline& _outline;
        TL::Scope _sc;
    public:
        GetDeclarationInScope(Outline& outline, TL::Scope sc)
            : _outline(outline), _sc(sc)
        {
        }

        std::string operator()(TL::Symbol sym)
        {
            Outline::ParameterPassing passing = _outline.get_parameter_passing(sym);

            TL::Type type = sym.get_type();
            switch (passing)
            {
                case Outline::POINTER:
                    {
                        if (type.is_array())
                        {
                            type = type.array_element();
                        }

                        type = type.get_pointer_to();
                        break;
                    }
                case Outline::VALUE:
                    {
                        break;
                    }
                default:
                    {
                        throw HLTException("invalid passing mode");
                    }
            }

            return type.get_declaration(_sc, sym.get_name());
        }
};

struct GetFieldDeclarations : public GetDeclarationInScope
{
    private:
        TL::Source _src;
    public:
        GetFieldDeclarations(Outline& outline, TL::Scope sc, TL::Source& src)
            : GetDeclarationInScope(outline, sc),
            _src(src)
        {
        }

        void operator()(TL::Symbol sym)
        {
             _src << GetDeclarationInScope::operator()(sym) << ";";
        }
};

static bool is_local_or_nonstatic_member(const TL::Symbol& sym)
{
    return sym.has_local_scope()
        || (sym.is_member() && !sym.is_static());
}

void Outline::compute_additional_declarations(Source template_headers, 
        Scope scope_of_decls)
{
    if (_packed_arguments)
    {
        Source arg_typename;

        CXX_LANGUAGE()
        {
            if (_enclosing_function.get_type().is_template_specialized_type())
            {
                _additional_decls_source
                    << template_headers
                    ;
            }
        }

        arg_typename
            << "struct _arg_pack_" << _outline_num << "_t"
                ;

        C_LANGUAGE()
        {
            _packed_argument_typename << arg_typename;
        }
        CXX_LANGUAGE()
        {
            _packed_argument_typename << "_arg_pack_" << _outline_num << "_t";

            if (_enclosing_function.get_type().is_template_specialized_type())
            {
                TemplateHeader& last_template_header = *(_template_header.rbegin());

                _packed_argument_typename
                    << "<"
                    << concat_strings(last_template_header.get_parameters().map(functor(&TemplateParameterConstruct::get_name)), ",")
                    << ">"
                    ;
            }
        }

        Source fields;
        _additional_decls_source
            << arg_typename
            << "{"
            << fields
            << "};"
            ;

        if (_enclosing_function.is_member()
                && !_enclosing_function.is_static())
        {
            Type ptr_class_type = _enclosing_function.get_class_type();

            Type enclosing_function_type = _enclosing_function.get_type();

            if (enclosing_function_type.is_const())
            {
                ptr_class_type = ptr_class_type.get_const_type();
            }

            ptr_class_type = ptr_class_type.get_pointer_to();

            fields
                << ptr_class_type.get_declaration(scope_of_decls, "_this") << ";"
                ;
        }

        std::for_each(_parameter_passed_symbols.begin(), _parameter_passed_symbols.end(),
                GetFieldDeclarations(*this, scope_of_decls, fields));

    }
}

TL::Source Outline::get_parameter_declarations(Scope scope_of_decls)
{
    Source parameters;

    if (_parameter_passed_symbols.empty())
    {
        C_LANGUAGE()
        {
            parameters << "void";
        }
    }
    else
    {
        if (_packed_arguments)
        {
            Source packed_argument_typename;
            packed_argument_typename 
                << _packed_argument_typename
                ;
            parameters
                << packed_argument_typename << " *_args"
                ;
        }
        else
        {
            if (_enclosing_function.is_member() && !_enclosing_function.is_static())
            {
                Type ptr_class_type = _enclosing_function.get_class_type();

                Type enclosing_function_type = _enclosing_function.get_type();

                if (enclosing_function_type.is_const())
                {
                    ptr_class_type = ptr_class_type.get_const_type();
                }

                ptr_class_type = ptr_class_type.get_pointer_to();

                parameters
                    << ptr_class_type.get_declaration(scope_of_decls, "_this")
                    ;
            }

            GetDeclarationInScope get_declarations(*this, scope_of_decls);
            ObjectList<std::string> declarations =
                _parameter_passed_symbols.map(functor(&GetDeclarationInScope::operator(), get_declarations));

            parameters.append_with_separator(
                    concat_strings(declarations, ","),
                    ",");
        }
    }

    return parameters;
}

struct DoNotPass : public TL::Predicate<TL::Symbol>
{
    private:
        Outline& _outline;
    public:
        DoNotPass(Outline& outline)
            : _outline(outline)
        {
        }

        virtual bool do_(DoNotPass::ArgType sym) const
        {
            return (_outline.get_parameter_passing(sym) != Outline::DO_NOT_PASS);
        }
};

void Outline::compute_referenced_entities()
{
    ObjectList<Symbol> all_referenced_symbols;

    std::for_each(_outline_statements.begin(), _outline_statements.end(), 
            std::bind2nd(std::ptr_fun(get_referenced_entities), &all_referenced_symbols));

    if (_use_nonlocal_scope)
    {
        // Remove those that can use the "file" scope
        _replaced_symbols = all_referenced_symbols.filter(predicate(is_local_or_nonstatic_member));
        _parameter_passed_symbols = all_referenced_symbols.filter(predicate(&Symbol::has_local_scope));
    }
    else
    {
        _replaced_symbols = all_referenced_symbols;
        _parameter_passed_symbols = all_referenced_symbols;
    }

    // Remove those that we've been told they should not be passed
    _parameter_passed_symbols = _parameter_passed_symbols.filter(DoNotPass(*this));
}

struct AuxiliarOutlineReplace
{
    private:
        Outline &_outline;
        TL::ReplaceSrcIdExpression *_replacements;
        TL::Symbol _enclosing_function;
        bool _packed_args;

    public:
        AuxiliarOutlineReplace(
                Outline &outline,
                TL::ReplaceSrcIdExpression& replacements,
                TL::Symbol enclosing_function,
                bool packed_args)
            : _outline(outline),
            _replacements(&replacements),
            _enclosing_function(enclosing_function),
            _packed_args(packed_args) { }

        void operator()(TL::Symbol sym)
        {
            Outline::ParameterPassing passing = _outline.get_parameter_passing(sym);
            if (!IS_CXX_LANGUAGE
                    || !sym.is_member() 
                    || !(_enclosing_function.is_member() && !_enclosing_function.is_static())
                    || !sym.get_class_type().is_same_type(_enclosing_function.get_class_type()))
            {
                if (_packed_args)
                {
                    if (passing == Outline::POINTER)
                    {
                        _replacements->add_replacement(sym, "(*_args->" + sym.get_name() + ")");
                    }
                    else if (passing == Outline::VALUE)
                    {
                        _replacements->add_replacement(sym, "(_args->" + sym.get_name() + ")");
                    }
                    else
                    {
                        throw HLTException("invalid passing mode");
                    }
                }
                else
                {
                    if (passing == Outline::POINTER)
                    {
                        _replacements->add_replacement(sym, "(*" + sym.get_name() + ")");
                    }
                    else if (passing == Outline::VALUE)
                    {
                        // Do nothing, actually
                        // _replacements->add_replacement(sym, "(" + sym.get_name() + ")");
                    }
                    else
                    {
                        throw HLTException("invalid passing mode");
                    }
                }
            }
            else
            {
                if (_packed_args)
                {
                    _replacements->add_replacement(sym, "(_args->_this->" + sym.get_name() + ")");
                }
                else
                {
                    _replacements->add_replacement(sym, "(_this->" + sym.get_name() + ")");
                }
            }
        }
};

struct auxiliar_replace_t
{
    TL::Source *src;
    TL::ReplaceSrcIdExpression *repls;
};

static void print_replaced_stmts(TL::Statement stmt, auxiliar_replace_t aux)
{
    (*aux.src) << aux.repls->replace(stmt);
}

void Outline::compute_outlined_body(Source &outlined_body)
{
    ReplaceSrcIdExpression replacements(_sl);

    std::for_each(_replaced_symbols.begin(),
            _replaced_symbols.end(),
            AuxiliarOutlineReplace(*this, replacements, _enclosing_function, _packed_arguments));

    auxiliar_replace_t aux = { &outlined_body, &replacements };

    std::for_each(_outline_statements.begin(),
            _outline_statements.end(),
            std::bind2nd(std::ptr_fun(print_replaced_stmts), aux));
}

void Outline::declare_members(Source template_headers)
{
    Source member_decl;

    member_decl
        << _additional_decls_source
        ;

    if (_enclosing_function.get_type().is_template_specialized_type())
    {
        member_decl
            << template_headers
            ;
    }

    Source parameters;
    member_decl
        << "static void " << _outline_name << "(" << parameters << ");"
        ;

    AST_t point_of_decl = _enclosing_function.get_point_of_declaration();
    Type class_type = _enclosing_function.get_class_type();

    parameters = get_parameter_declarations(class_type.get_symbol().get_scope());

    AST_t member_tree = member_decl.parse_member(
            point_of_decl, _sl,
            // class_type is a named type, get the type of its symbol
            class_type.get_symbol());

    point_of_decl.append(member_tree);
}

void Outline::fill_nonmember_forward_declarations(Source template_headers, Source &forward_declarations)
{
    forward_declarations
        // FIXME
        << _additional_decls_source
        << template_headers
        << _enclosing_function.get_type().get_declaration(_enclosing_function.get_scope(), _enclosing_function.get_name()) << ";";
}

void Outline::fill_member_forward_declarations(Source /*template_headers*/, Source &forward_declarations)
{
    forward_declarations
        << _additional_decls_source
        ;
}

void Outline::embed_outline()
{
    AST_t outline_tree;
    if (!_is_member || !_is_inlined_member)
    {
        outline_tree = _outlined_source.parse_declaration(_function_def->get_point_of_declaration(),
                _sl);
    }
    else
    {
        // This requires a different function
        outline_tree = _outlined_source.parse_member(_function_def->get_point_of_declaration(),
                _sl, _enclosing_function.get_class_type().get_symbol());

    }
    _function_def->get_ast().prepend_sibling_function(outline_tree);
}

Outline& Outline::set_outline_name(const std::string& str)
{
    _outline_name = str;
}

std::string Outline::get_outline_name()
{
    do_outline();
    return _outline_name;
}

Outline::~Outline()
{
    delete _function_def;
}

void Outline::set_default_parameter_passing(ParameterPassing passing)
{
    _default_parameter_passing = passing;
}

void Outline::set_parameter_passing(Symbol sym, ParameterPassing passing)
{
    if (!_parameter_info.contains(functor(&ParameterInfo::related_symbol), sym))
    {
        ParameterInfo param_info;

        param_info.related_symbol = sym;
        param_info.passing = passing;
        param_info.outline_ref = sym.get_name();

        _parameter_info.append(param_info);
    }
    else
    {
        ParameterInfo &param_info = *(_parameter_info
                .find(functor(&ParameterInfo::related_symbol), sym).begin());

        param_info.passing = passing;
    }
}

Outline::ParameterPassing Outline::get_parameter_passing(Symbol sym)
{
    set_parameter_passing_if_not_set(sym);

    ParameterInfo &param_info = *(_parameter_info
            .find(functor(&ParameterInfo::related_symbol), sym).begin());

    return param_info.passing;
}

void Outline::set_parameter_passing_if_not_set(Symbol sym)
{
    if (!_parameter_info.contains(functor(&ParameterInfo::related_symbol), sym))
    {
        ParameterInfo param_info;

        param_info.related_symbol = sym;
        param_info.passing = _default_parameter_passing;
        param_info.outline_ref = sym.get_name();

        _parameter_info.append(param_info);
    }
}

TL::ObjectList<TL::Symbol> Outline::get_parameter_list()
{
    do_outline();
    return _parameter_passed_symbols;
}

std::string Outline::get_packing_struct_typename()
{
    do_outline();
    return _packed_argument_typename;
}
