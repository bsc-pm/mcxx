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




#include "tl-omp-core.hpp"
#include "tl-omp-deps.hpp"
#include "cxx-diagnostic.h"
#include "cxx-exprtype.h"
#include "cxx-entrylist.h"
#include "fortran03-exprtype.h"

// Needed for parsing OpenMP standard clauses
#include <sys/types.h>
#include <regex.h>


namespace TL { namespace OpenMP {

    void add_extra_symbols(Nodecl::NodeclBase data_ref,
            DataSharingEnvironment& ds,
            ObjectList<Symbol>& extra_symbols)
    {
        struct DataRefVisitorDep : public Nodecl::ExhaustiveVisitor<void>
        {
            struct ExtraDataSharing : public Nodecl::ExhaustiveVisitor<void>
            {
                DataSharingEnvironment& _data_sharing;
                ObjectList<Symbol>& _symbols;

                const ObjectList<Symbol>& _iterators;

                ExtraDataSharing(DataSharingEnvironment& ds_, ObjectList<Symbol>& symbols,
                        const ObjectList<Symbol>& iterators)
                    :_data_sharing(ds_), _symbols(symbols), _iterators(iterators) { }

                void visit(const Nodecl::Symbol& node)
                {
                    TL::Symbol sym = node.get_symbol();

                    if (!sym.is_valid()
                            || !sym.is_variable()
                            || sym.is_fortran_parameter()
                            // For multidependences, we do not care about iterator symbols
                            || _iterators.contains(sym))
                        return;

                    if ((_data_sharing.get_data_sharing(sym, /* check_enclosing */ false) & ~DS_IMPLICIT)
                            == DS_UNDEFINED)
                    {
                       _symbols.append(sym);
                    }
                }

                void visit(const Nodecl::ClassMemberAccess& node)
                {
                    walk(node.get_lhs());
                    // Do not walk the rhs
                }

            };

            ExtraDataSharing _extra_data_sharing;

            ObjectList<TL::Symbol> _iterators;

            DataRefVisitorDep(DataSharingEnvironment& ds_, ObjectList<Symbol>& symbols)
                : _extra_data_sharing(ds_, symbols, _iterators) { }

            void visit_pre(const Nodecl::Symbol &node)
            {
                if (node.get_type().no_ref().is_array())
                {
                    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                    {
                        _extra_data_sharing.walk(node.get_type().no_ref().array_get_size());
                    }
                    else if (IS_FORTRAN_LANGUAGE)
                    {
                        Nodecl::NodeclBase lb, ub;
                        node.get_type().no_ref().array_get_bounds(lb, ub);

                        _extra_data_sharing.walk(lb);
                        _extra_data_sharing.walk(ub);
                    }
                }
            }

            void visit_pre(const Nodecl::Shaping &node)
            {
                _extra_data_sharing.walk(node.get_shape());
            }

            void visit_pre(const Nodecl::ArraySubscript &node)
            {
                _extra_data_sharing.walk(node.get_subscripts());
            }

            void visit_pre(const Nodecl::ClassMemberAccess &node)
            {
                _extra_data_sharing.walk(node.get_lhs());
            }

            // Note that we alter the traversal here because we do not want to
            // traverse the iterators, only the dependence
            void visit(const Nodecl::MultiReference& node)
            {
                _iterators.push_back(node.get_symbol());
                walk(node.get_dependence());
                _iterators.pop_back();
            }
        };

        DataRefVisitorDep data_ref_visitor_dep(ds, extra_symbols);
        data_ref_visitor_dep.walk(data_ref);
    }

    static void add_data_sharings(ObjectList<Nodecl::NodeclBase> &expression_list,
            DataSharingEnvironment& data_sharing,
            DependencyDirection dep_attr,
            DataSharingAttribute default_data_attr,
            bool in_ompss_mode,
            const std::string &clause_name,
            ObjectList<Symbol>& extra_symbols)
    {
        for (ObjectList<Nodecl::NodeclBase>::iterator it = expression_list.begin();
                it != expression_list.end();
                it++)
        {
            DataReference expr(*it);
            if (!expr.is_valid())
            {
                warn_printf("%s",
                        expr.get_error_log().c_str());

                warn_printf("%s: warning: invalid dependency expression '%s', skipping\n",
                        expr.get_locus_str().c_str(),
                        expr.prettyprint().c_str());

                continue;
            }

            DependencyItem dep_item(*it, dep_attr);

            Symbol sym = expr.get_base_symbol();

            if (!in_ompss_mode)
            {
                /* In OpenMP mode */

                // Dependences over non-static data members are invalid in OpenMP (OpenMP 4.0 [117:17-18])
                if (// usual case: a.x, (*this).y, ...
                    expr.is<Nodecl::ClassMemberAccess>()
                        // this case only happens when we are inside a template class and we are accessing to a
                        // non-static data member without specifying explicitly the implicit object ('this'):
                        //
                        //  template < typename T>
                        //  struct C
                        //  {
                        //      void foo()
                        //      {
                        //          #pragma omp task inout(p)
                        //              p = 0;
                        //      }
                        //      int *p;
                        //  };
                        //
                        // Note: If the class is not dependent Mercurium adds the implicit object, transforming
                        // the current expression (i.e. a symbol) into a class member access
                        || (sym.is_variable() && sym.is_member() && !sym.is_static()))
                {
                    warn_printf("%s: warning: invalid dependency expression '%s', skipping\n",
                            expr.get_locus_str().c_str(),
                            expr.prettyprint().c_str());

                    info_printf("%s: info: dependences over non-static data members are not allowed in OpenMP\n",
                            expr.get_locus_str().c_str());

                    continue;
                }
                // We cannot define a dependence over 'this' in OpenMP
                else if (sym.get_name() == "this")
                {
                    warn_printf("%s: warning: invalid dependency expression '%s', skipping\n",
                            expr.get_locus_str().c_str(),
                            expr.prettyprint().c_str());

                    continue;
                }
            }


            if((default_data_attr & DS_AUTO) == DS_AUTO)
            {
                data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_AUTO),
                        "'default(auto)'");
            }
            else if (in_ompss_mode)
            {
                // In OmpSs, the storage of a dependence is always SHARED. Note that with this
                // definition we aren't defining the data-sharings of the variables involved
                // in that expression.
                //
                // About the data-sharings of the variables involved in the dependence expression:
                // - Fortran: the base symbol of the dependence expression is always SHARED
                // - C/C++:
                //  * The base symbol of a trivial dependence (the expression is a symbol) must always be SHARED:
                //          int x, a[10];
                //          inout(x) -> shared(x)
                //          inout(a) -> shared(a)
                //  * The base symbol of an array expression or a reference to an array must be SHARED too:
                //          int a[10];
                //          inout(a[4])   -> shared(a)
                //          inout(a[1:2]) -> shared(a)
                //  * The base symbol of a class member access must be shared too:
                //          struct C { int z; } c;
                //          in(c.z)       -> shared(c)
                //  * Otherwise, the data-sharing of the base symbol is FIRSTPRIVATE:
                //          int* p;
                //          inout(*p)     -> firstprivate(p)
                //          inout(p[10])  -> firstprivate(p)
                //          inout(p[1:2]) -> firstprivate(p)
                //          inout([10][20] p) -> firstprivate(p)
                if (IS_FORTRAN_LANGUAGE)
                {
                    data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_SHARED | DS_IMPLICIT),
                            "the variable is mentioned in a dependence and it did not have an explicit data-sharing");
                }
                else if (expr.is<Nodecl::Symbol>())
                {
                    data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_SHARED | DS_IMPLICIT),
                            "the variable is mentioned in a dependence and it did not have an explicit data-sharing");
                }
                else if (sym.get_type().is_array()
                        || (sym.get_type().is_any_reference()
                            && sym.get_type().references_to().is_array()))
                {
                    data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_SHARED | DS_IMPLICIT),
                            "the variable is an array mentioned in a non-trivial dependence "
                            "and it did not have an explicit data-sharing");
                }
                else if (sym.get_type().is_class())
                {
                    data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_SHARED | DS_IMPLICIT),
                            "the variable is an object mentioned in a non-trivial dependence "
                            "and it did not have an explicit data-sharing");
                }
                else
                {
                    data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_FIRSTPRIVATE | DS_IMPLICIT),
                            "the variable is a non-array mentioned in a non-trivial dependence "
                            "and it did not have an explicit data-sharing");
                }
            }

            data_sharing.add_dependence(dep_item);
            add_extra_symbols(expr, data_sharing, extra_symbols);
        }
    }

    void Core::get_dependences_info(TL::PragmaCustomLine construct,
            DataSharingEnvironment& data_sharing,
            DataSharingAttribute default_data_attr,
            ObjectList<Symbol>& extra_symbols)
    {
        // Ompss clauses
        PragmaCustomClause input_clause = construct.get_clause("in",/* deprecated */ "input");
        get_dependences_ompss_info_clause(input_clause, construct, data_sharing, DEP_DIR_IN,
                default_data_attr, "in", extra_symbols);

        PragmaCustomClause input_private_clause = construct.get_clause("inprivate");
        get_dependences_ompss_info_clause(input_private_clause, construct, data_sharing, DEP_DIR_IN_PRIVATE,
                default_data_attr, "inprivate", extra_symbols);

        PragmaCustomClause output_clause = construct.get_clause("out", /* deprecated */ "output");
        get_dependences_ompss_info_clause(output_clause, construct, data_sharing, DEP_DIR_OUT,
                default_data_attr, "out", extra_symbols);

        PragmaCustomClause inout_clause = construct.get_clause("inout");
        get_dependences_ompss_info_clause(inout_clause, construct, data_sharing, DEP_DIR_INOUT,
                default_data_attr, "inout", extra_symbols);

        PragmaCustomClause concurrent_clause = construct.get_clause("concurrent");
        get_dependences_ompss_info_clause(concurrent_clause, construct, data_sharing, DEP_CONCURRENT,
                default_data_attr, "concurrent", extra_symbols);

        PragmaCustomClause commutative_clause = construct.get_clause("commutative");
        get_dependences_ompss_info_clause(commutative_clause, construct, data_sharing, DEP_COMMUTATIVE,
                default_data_attr, "commutative", extra_symbols);

        // OpenMP standard clauses
        PragmaCustomClause depends = construct.get_clause("depend");
        get_dependences_openmp(construct, depends, data_sharing,
                default_data_attr, extra_symbols);
    }

    namespace {
        decl_context_t decl_context_map_id(decl_context_t d)
        {
            return d;
        }
    }

    void Core::parse_dependences_openmp_clause(
            TL::ReferenceScope parsing_scope,
            TL::PragmaCustomClause clause,
            TL::ObjectList<Nodecl::NodeclBase> &in,
            TL::ObjectList<Nodecl::NodeclBase> &out,
            TL::ObjectList<Nodecl::NodeclBase> &inout,
            const locus_t* locus
            )
    {
        if (!clause.is_defined())
            return;

        ObjectList<std::string> arguments = clause.get_tokenized_arguments();

        // Since we coalesce all the arguments of a clauses with the same name
        // in a case like depend(in : a, b) depend(out : c, d) will be a list
        // containing "in:a", "b", "out:c", "d"

        int cflags = REG_EXTENDED;
        if (IS_FORTRAN_LANGUAGE)
        {
            cflags |= REG_ICASE;
        }

        regex_t preg;
        if (regcomp(&preg, "^[[:blank:]]*((in)|(out)|(inout))[[:blank:]]*:(.*)$", cflags) != 0)
        {
            internal_error("Invalid regular expression", 0);
        }
        const int num_matches = 6;
        regmatch_t pmatch[num_matches] = { };

        TL::ObjectList<Nodecl::NodeclBase> *dep_set = NULL;
        for (ObjectList<std::string>::iterator it = arguments.begin();
                it != arguments.end();
                it++)
        {
            std::string clause_name;
            int match = regexec(&preg, it->c_str(), num_matches, pmatch, 0);

            std::string current_dep_expr = *it;

            if (match == 0)
            {
                // Zero-th match is the whole regular expression
                ERROR_CONDITION(pmatch[1].rm_so == -1, "Invalid match", 0);
                std::string dependency_type;
                for (int i = pmatch[1].rm_so; i < pmatch[1].rm_eo; i++)
                {
                    dependency_type += tolower((*it)[i]);
                }

                if (dependency_type == "in")
                {
                    dep_set = &in;
                }
                else if (dependency_type == "out")
                {
                    dep_set = &out;
                }
                else if (dependency_type == "inout")
                {
                    dep_set = &inout;
                }
                else
                {
                    internal_error("Code unreachable", 0);
                }

                // Now compute the proper dependence expression
                current_dep_expr.clear();
                ERROR_CONDITION(pmatch[5].rm_so == -1, "Invalid match", 0);
                for (int i = pmatch[5].rm_so; i < pmatch[5].rm_eo; i++)
                {
                    current_dep_expr += (*it)[i];
                }
            }
            else if (match == REG_NOMATCH)
            {
                if (dep_set == NULL)
                {
                    error_printf("%s: error: skipping item '%s' in 'depend' clause because it lacks dependence-type\n",
                            locus_to_str(locus),
                            current_dep_expr.c_str());
                    continue;
                }
            }
            else
            {
                internal_error("Unexpected result %d from regexec\n", match);
            }

            Source src;
            src << "#line " << clause.get_pragma_line().get_line() << " \"" << clause.get_pragma_line().get_filename() << "\"\n";
            src << current_dep_expr;

            // Now, parse a single OpenMP list item and hand it to the usual dependency routines
            Nodecl::NodeclBase expr;
            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                expr = src.parse_generic(parsing_scope,
                        /* ParseFlags */ Source::DEFAULT,
                        "@OMP-DEPEND-ITEM@",
                        Source::c_cxx_check_expression_adapter,
                        decl_context_map_id);
            }
            else if (IS_FORTRAN_LANGUAGE)
            {
                expr = src.parse_generic(parsing_scope,
                        /* ParseFlags */ Source::DEFAULT,
                        "@OMP-DEPEND-ITEM@",
                        Source::fortran_check_expression_adapter,
                        decl_context_map_id);
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

            // Singleton
            dep_set->append(expr);
        }

        regfree(&preg);
    }

    void Core::get_dependences_openmp(
            TL::PragmaCustomLine construct,
            TL::PragmaCustomClause clause,
            DataSharingEnvironment& data_sharing,
            DataSharingAttribute default_data_attr,
            ObjectList<Symbol>& extra_symbols)
    {
            TL::ObjectList<Nodecl::NodeclBase> in, out, inout;
            parse_dependences_openmp_clause(
                    construct,
                    clause,
                    in,
                    out,
                    inout,
                    construct.get_locus());

            add_data_sharings(in, data_sharing,
                    DEP_DIR_IN, default_data_attr, this->in_ompss_mode(), "depend(in:)", extra_symbols);
            add_data_sharings(out, data_sharing,
                    DEP_DIR_OUT, default_data_attr, this->in_ompss_mode(), "depend(out:)", extra_symbols);
            add_data_sharings(inout, data_sharing,
                    DEP_DIR_INOUT, default_data_attr, this->in_ompss_mode(), "depend(inout:)", extra_symbols);
    }

    namespace {

        void ompss_multidep_check_range(AST range,
                decl_context_t decl_context,
                void (*check_expression)(AST a, decl_context_t decl_context, nodecl_t* nodecl_output),
                nodecl_t* nodecl_output)
        {
            switch (ASTKind(range))
            {
                case AST_OMPSS_ITERATOR_RANGE_SECTION: // lower : upper
                    {
                        AST lower = ASTSon0(range);
                        nodecl_t nodecl_lower = nodecl_null();

                        check_expression(lower, decl_context, &nodecl_lower);
                        if (nodecl_is_err_expr(nodecl_lower))
                        {
                            *nodecl_output = nodecl_lower;
                            return;
                        }

                        AST upper = ASTSon1(range);
                        nodecl_t nodecl_upper = nodecl_null();

                        check_expression(upper, decl_context, &nodecl_upper);
                        if (nodecl_is_err_expr(nodecl_upper))
                        {
                            *nodecl_output = nodecl_upper;
                            return;
                        }

                        nodecl_t nodecl_stride = nodecl_null();
                        AST stride = ASTSon2(range);
                        if (stride != NULL)
                        {
                            check_expression(stride, decl_context, &nodecl_stride);
                            if (nodecl_is_err_expr(nodecl_stride))
                            {
                                *nodecl_output = nodecl_stride;
                                return;
                            }
                        }
                        else
                        {
                            nodecl_stride = const_value_to_nodecl(const_value_get_signed_int(1));
                        }

                        *nodecl_output = nodecl_make_range(
                                nodecl_lower,
                                nodecl_upper,
                                nodecl_stride,
                                get_signed_int_type(),
                                ast_get_locus(range));

                        if (nodecl_is_constant(nodecl_lower)
                                && nodecl_is_constant(nodecl_upper)
                                && nodecl_is_constant(nodecl_stride))
                        {
                            nodecl_set_constant(
                                    *nodecl_output,
                                    const_value_make_range(
                                        nodecl_get_constant(nodecl_lower),
                                        nodecl_get_constant(nodecl_upper),
                                        nodecl_get_constant(nodecl_stride)));
                        }

                        break;
                    }
                case AST_OMPSS_ITERATOR_RANGE_SIZE: // lower ; num_elements [C/C++ only]
                    {
                        AST lower = ASTSon0(range);
                        nodecl_t nodecl_lower = nodecl_null();

                        check_expression(lower, decl_context, &nodecl_lower);
                        if (nodecl_is_err_expr(nodecl_lower))
                        {
                            *nodecl_output = nodecl_lower;
                            return;
                        }

                        AST size = ASTSon1(range);
                        nodecl_t nodecl_length = nodecl_null();

                        check_expression(size, decl_context, &nodecl_length);
                        if (nodecl_is_err_expr(nodecl_length))
                        {
                            *nodecl_output = nodecl_length;
                            return;
                        }

                        nodecl_t nodecl_stride = nodecl_null();
                        AST stride = ASTSon2(range);
                        if (stride != NULL)
                        {
                            check_expression(stride, decl_context, &nodecl_stride);
                            if (nodecl_is_err_expr(nodecl_stride))
                            {
                                *nodecl_output = nodecl_stride;
                                return;
                            }
                        }
                        else
                        {
                            nodecl_stride = const_value_to_nodecl(const_value_get_signed_int(1));
                        }

                        nodecl_t nodecl_upper = nodecl_make_minus(
                                nodecl_make_add(
                                    nodecl_lower,
                                    nodecl_length,
                                    get_signed_int_type(),
                                    ast_get_locus(range)),
                                const_value_to_nodecl(const_value_get_signed_int(1)),
                                get_signed_int_type(),
                                ast_get_locus(range));

                        if (nodecl_is_constant(nodecl_lower)
                                && nodecl_is_constant(nodecl_length))
                        {
                            nodecl_set_constant(
                                    nodecl_upper,
                                    const_value_sub(
                                        const_value_add(
                                            nodecl_get_constant(nodecl_lower),
                                            nodecl_get_constant(nodecl_length)),
                                        const_value_get_signed_int(1)));
                        }

                        *nodecl_output = nodecl_make_range(
                                nodecl_lower,
                                nodecl_upper,
                                nodecl_stride,
                                get_signed_int_type(),
                                ast_get_locus(range));

                        if (nodecl_is_constant(nodecl_lower)
                                && nodecl_is_constant(nodecl_upper)
                                && nodecl_is_constant(nodecl_stride))
                        {
                            nodecl_set_constant(
                                    *nodecl_output,
                                    const_value_make_range(
                                        nodecl_get_constant(nodecl_lower),
                                        nodecl_get_constant(nodecl_upper),
                                        nodecl_get_constant(nodecl_stride)));
                        }

                        break;
                    }
                default:
                    internal_error("Unexpected node kind '%s'\n", ast_print_node_type(ASTKind(range)));
            }
        }

        void ompss_multidep_expression(
                AST a,
                decl_context_t decl_context,
                decl_context_t iterator_context,
                void (*check_expression)(AST a, decl_context_t decl_context, nodecl_t* nodecl_output),
                nodecl_t* nodecl_output)
        {
            if (ASTKind(a) == AST_OMPSS_MULTI_DEPENDENCY)
            {
                AST ompss_iterator = ASTSon1(a);
                AST identifier = ASTSon0(ompss_iterator);
                AST range = ASTSon1(ompss_iterator);

                const char* iterator_name = NULL;
                if (IS_FORTRAN_LANGUAGE)
                {
                    iterator_name = strtolower(ASTText(identifier));
                }
                else
                {
                    iterator_name = ASTText(identifier);
                }

                {
                    // Shadow check
                    scope_entry_list_t* entry_list = query_name_str(decl_context, iterator_name, NULL);
                    if (entry_list != NULL)
                    {
                        scope_entry_t* entry = entry_list_head(entry_list);
                        entry_list_free(entry_list);
                        if (entry->kind == SK_VARIABLE
                                && entry->decl_context.current_scope != NULL
                                && entry->decl_context.current_scope->kind == BLOCK_SCOPE
                                && entry->decl_context.current_scope->related_entry == decl_context.current_scope->related_entry)
                        {
                            warn_printf("%s: warning: iterator name '%s' in multidependence shadows a previous variable\n",
                                    ast_location(identifier),
                                    iterator_name);
                            info_printf("%s: info: declaration of the shadowed variable\n",
                                    locus_to_str(entry->locus));
                        }
                    }
                }

                scope_entry_t* new_iterator = new_symbol(iterator_context,
                        iterator_context.current_scope,
                        iterator_name);
                new_iterator->kind = SK_VARIABLE;
                new_iterator->type_information = get_signed_int_type();
                new_iterator->locus = ast_get_locus(ompss_iterator);

                nodecl_t nodecl_range = nodecl_null();
                ompss_multidep_check_range(range, iterator_context, check_expression, &nodecl_range);

                if (nodecl_is_err_expr(nodecl_range))
                {
                    *nodecl_output = nodecl_range;
                    return;
                }

                nodecl_t nodecl_subexpr = nodecl_null();
                ompss_multidep_expression(ASTSon0(a),
                        decl_context,
                        iterator_context,
                        check_expression,
                        &nodecl_subexpr);

                if (nodecl_is_err_expr(nodecl_subexpr))
                {
                    *nodecl_output = nodecl_subexpr;
                    return;
                }

                *nodecl_output = nodecl_make_multi_reference(nodecl_range,
                        nodecl_subexpr,
                        new_iterator,
                        nodecl_get_type(nodecl_subexpr),
                        ast_get_locus(a));
            }
            else
            {
                check_expression(a, iterator_context, nodecl_output);
            }
        }

        void c_cxx_ompss_dep_expression(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
        {
            if (ASTKind(a) == AST_OMPSS_MULTI_DEPENDENCY)
            {
                decl_context_t iterator_context = new_block_context(decl_context);
                ompss_multidep_expression(a,
                        decl_context,
                        iterator_context,
                        Source::c_cxx_check_expression_adapter,
                        nodecl_output);
            }
            else
            {
                Source::c_cxx_check_expression_adapter(a, decl_context, nodecl_output);
            }
        }

        void fortran_ompss_dep_expression(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
        {
            if (ASTKind(a) == AST_OMPSS_MULTI_DEPENDENCY)
            {
                decl_context_t iterator_context = new_block_context(decl_context);
                ompss_multidep_expression(a,
                        decl_context,
                        iterator_context,
                        Source::fortran_check_expression_adapter,
                        nodecl_output);
            }
            else
            {
                Source::fortran_check_expression_adapter(a, decl_context, nodecl_output);
            }
        }
    }

    ObjectList<Nodecl::NodeclBase> Core::parse_dependences_ompss_clause(
            PragmaCustomClause& clause,
            TL::ReferenceScope parsing_scope)
    {
        ObjectList<Nodecl::NodeclBase> result;

        ObjectList<std::string> arguments = clause.get_tokenized_arguments();

        for (ObjectList<std::string>::iterator it = arguments.begin();
                it != arguments.end();
                it++)
        {
            Source src;
            src << "#line " << clause.get_pragma_line().get_line() << " \"" << clause.get_pragma_line().get_filename() << "\"\n";
            src << *it;

            Nodecl::NodeclBase expr;
            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                expr = src.parse_generic(parsing_scope,
                        /* ParseFlags */ Source::DEFAULT,
                        "@OMPSS-DEPENDENCY-EXPR@",
                        c_cxx_ompss_dep_expression,
                        decl_context_map_id);
            }
            else if (IS_FORTRAN_LANGUAGE)
            {
                expr = src.parse_generic(parsing_scope,
                        /* ParseFlags */ Source::DEFAULT,
                        "@OMPSS-DEPENDENCY-EXPR@",
                        fortran_ompss_dep_expression,
                        decl_context_map_id);
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

            result.append(expr);
        }

        return result;
    }

    void Core::get_dependences_ompss_info_clause(PragmaCustomClause clause,
            Nodecl::NodeclBase construct,
            DataSharingEnvironment& data_sharing,
            DependencyDirection dep_attr,
            DataSharingAttribute default_data_attr,
            const std::string& clause_name,
            ObjectList<Symbol>& extra_symbols)
    {
        if (clause.is_defined())
        {
            ObjectList<Nodecl::NodeclBase> expr_list = parse_dependences_ompss_clause(clause, construct);
            add_data_sharings(expr_list, data_sharing,
                    dep_attr, default_data_attr, this->in_ompss_mode(), clause_name, extra_symbols);
        }
    }

    std::string get_dependency_direction_name(DependencyDirection d)
    {
        switch (d)
        {
            case DEP_DIR_UNDEFINED:
                return "<<undefined-dependence>>";
            case DEP_DIR_IN:
            case DEP_DIR_IN_VALUE:
                return "in";
            case DEP_DIR_OUT:
                return "out";
            case DEP_DIR_INOUT:
                return "inout";
            case DEP_CONCURRENT:
                return "concurrent";
            case DEP_COMMUTATIVE:
                return "commutative";
            default:
                return "<<unknown-dependence-kind?>>";
        }
    }
} }
