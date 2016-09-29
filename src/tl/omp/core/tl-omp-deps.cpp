/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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
            DataEnvironment& ds,
            ObjectList<Symbol>& extra_symbols)
    {
        struct DataRefVisitorDep : public Nodecl::ExhaustiveVisitor<void>
        {
            struct ExtraDataSharing : public Nodecl::ExhaustiveVisitor<void>
            {
                DataEnvironment& _data_sharing;
                ObjectList<Symbol>& _symbols;

                const ObjectList<Symbol>& _iterators;

                ExtraDataSharing(DataEnvironment& ds_, ObjectList<Symbol>& symbols,
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

                    DataSharingValue current_datasharing =
                        _data_sharing.get_data_sharing(sym,
                                /* check_enclosing */ false);
                    if (current_datasharing.attr == DS_UNDEFINED)
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

            DataRefVisitorDep(DataEnvironment& ds_, ObjectList<Symbol>& symbols)
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
            void visit(const Nodecl::MultiExpression& node)
            {
                _iterators.push_back(node.get_symbol());
                walk(node.get_base());
                _iterators.pop_back();
            }
        };

        DataRefVisitorDep data_ref_visitor_dep(ds, extra_symbols);
        data_ref_visitor_dep.walk(data_ref);
    }

    // This function handles all the dependences, adding them to the DataEnvironment.
    // In OmpSs it also computes the data-sharing of each base symbol of each dependence.
    template < DependencyDirection dep_dir>
    static void get_info_from_dependences(
            const ObjectList<Nodecl::NodeclBase>& expression_list,
            DataSharingAttribute default_data_attr,
            bool in_ompss_mode,
            const std::string &clause_name,
            // Out
            DataEnvironment& data_sharing_environment,
            ObjectList<Symbol>& extra_symbols)
    {
        for (ObjectList<Nodecl::NodeclBase>::const_iterator it = expression_list.begin();
                it != expression_list.end();
                it++)
        {
            DataReference expr(*it);
            if (!expr.is_valid())
            {
                expr.commit_diagnostic();
                warn_printf_at(expr.get_locus(),
                        "invalid '%s' expression  in '%s' clause, skipping\n",
                        expr.prettyprint().c_str(), clause_name.c_str());
                continue;
            }

            DependencyItem dep_item(*it, dep_dir);

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
                    warn_printf_at(expr.get_locus(),
                            "invalid '%s' expression  in '%s' clause, skipping\n",
                            expr.prettyprint().c_str(), clause_name.c_str());

                    info_printf_at(expr.get_locus(),
                            "dependences over non-static data members are not allowed in OpenMP\n");

                    continue;
                }
                // We cannot define a dependence over 'this' in OpenMP
                else if (sym.get_name() == "this")
                {
                    warn_printf_at(expr.get_locus(),
                            "invalid '%s' expression  in '%s' clause, skipping\n",
                            expr.prettyprint().c_str(), clause_name.c_str());

                    continue;
                }
            }


            if ((default_data_attr & DS_AUTO) == DS_AUTO)
            {
                data_sharing_environment.set_data_sharing(sym, DS_AUTO, DSK_EXPLICIT, "'default(auto)'");
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
                    if (sym.get_type().is_pointer())
                    {
                        data_sharing_environment.set_data_sharing(sym, DS_FIRSTPRIVATE, DSK_IMPLICIT,
                                "the variable is a pointer mentioned in a dependence "
                                "and it did not have an explicit data-sharing");
                    }
                    else
                    {
                        data_sharing_environment.set_data_sharing(sym, DS_SHARED, DSK_IMPLICIT,
                                "the variable is mentioned in a dependence and it did not have an explicit data-sharing");
                    }
                }
                else if (expr.is<Nodecl::Symbol>())
                {
                    data_sharing_environment.set_data_sharing(sym, DS_SHARED, DSK_IMPLICIT,
                            "the variable is mentioned in a dependence and it did not have an explicit data-sharing");
                }
                else if (sym.get_type().no_ref().is_array())
                {
                    data_sharing_environment.set_data_sharing(sym, DS_SHARED, DSK_IMPLICIT,
                            "the variable is an array mentioned in a non-trivial dependence "
                            "and it did not have an explicit data-sharing");
                }
                else if (sym.get_type().no_ref().is_class())
                {
                    data_sharing_environment.set_data_sharing(sym, DS_SHARED, DSK_IMPLICIT,
                            "the variable is an object mentioned in a non-trivial dependence "
                            "and it did not have an explicit data-sharing");
                }
                else
                {
                    data_sharing_environment.set_data_sharing(sym, DS_FIRSTPRIVATE, DSK_IMPLICIT,
                            "the variable is a non-array mentioned in a non-trivial dependence "
                            "and it did not have an explicit data-sharing");
                }
            }

            // Verify that we are not mixing strong and weak dependences
            TL::ObjectList<DependencyItem> all_deps;
            data_sharing_environment.get_all_dependences(all_deps);
            for (TL::ObjectList<DependencyItem>::iterator existing_dep = all_deps.begin();
                    existing_dep != all_deps.end();
                    existing_dep++)
            {
                Nodecl::NodeclBase existing_dep_expr = existing_dep->get_dependency_expression();
                // We only check symbols here because some testcases exists where the
                // user does inout(a[i][j]) and concurrent(a[0][0]) at the same time
                if (!expr.is<Nodecl::Symbol>()
                        || !existing_dep_expr.is<Nodecl::Symbol>())
                    continue;

                if (existing_dep_expr.get_symbol() == expr.get_symbol())
                {
                    DependencyDirection existing_dep_dir = existing_dep->get_kind();
                    if (existing_dep_dir == DEP_OMPSS_CONCURRENT
                            || existing_dep_dir == DEP_OMPSS_COMMUTATIVE)
                    {
                        if (dep_dir != existing_dep_dir)
                        {
                            error_printf_at(it->get_locus(),
                                    "cannot override '%s' directionality of symbol '%s'\n",
                                    get_dependency_direction_name(existing_dep_dir).c_str(),
                                    expr.get_base_symbol().get_name().c_str());
                        }
                        else
                        {
                            warn_printf_at(it->get_locus(),
                                    "redundant '%s' directionality clause for symbol '%s'\n",
                                    get_dependency_direction_name(existing_dep_dir).c_str(),
                                    expr.get_base_symbol().get_name().c_str()
                                    );
                        }
                    }
                    else if ((is_strict_dependency(existing_dep_dir)
                                && !is_strict_dependency(dep_dir))
                            || (is_weak_dependency(existing_dep_dir)
                                && !is_weak_dependency(dep_dir)))
                    {
                        error_printf_at(it->get_locus(),
                                "cannot override '%s' directionality of symbol '%s' with directionality '%s'\n",
                                get_dependency_direction_name(existing_dep_dir).c_str(),
                                expr.get_base_symbol().get_name().c_str(),
                                get_dependency_direction_name(dep_dir).c_str());
                    }
                }
            }

            data_sharing_environment.add_dependence(dep_item);
            add_extra_symbols(expr, data_sharing_environment, extra_symbols);
        }
    }

    void Core::handle_task_dependences(
            TL::PragmaCustomLine pragma_line,
            Nodecl::NodeclBase parsing_context,
            DataSharingAttribute default_data_attr,
            DataEnvironment& data_sharing_environment,
            ObjectList<Symbol>& extra_symbols)
    {
        // Ompss clauses
        get_basic_dependences_info(
                pragma_line,
                parsing_context,
                data_sharing_environment,
                default_data_attr, extra_symbols);

        ObjectList<Nodecl::NodeclBase> expr_list;

        expr_list = parse_dependences_ompss_clause(
                pragma_line.get_clause("weakin"),
                parsing_context);
        get_info_from_dependences<DEP_OMPSS_WEAK_IN>(
                expr_list, default_data_attr, this->in_ompss_mode(),
                "weakin", data_sharing_environment, extra_symbols);

        expr_list = parse_dependences_ompss_clause(
                pragma_line.get_clause("weakout"),
                parsing_context);
        get_info_from_dependences<DEP_OMPSS_WEAK_OUT>(
                expr_list, default_data_attr, this->in_ompss_mode(),
                "weakout", data_sharing_environment, extra_symbols);

        expr_list = parse_dependences_ompss_clause(
                pragma_line.get_clause("weakinout"),
                parsing_context);
        get_info_from_dependences<DEP_OMPSS_WEAK_INOUT>(
                expr_list, default_data_attr, this->in_ompss_mode(),
                "weakinout", data_sharing_environment, extra_symbols);

        expr_list = parse_dependences_ompss_clause(
                pragma_line.get_clause("inprivate"),
                parsing_context);
        get_info_from_dependences<DEP_OMPSS_DIR_IN_PRIVATE>(
                expr_list, default_data_attr, this->in_ompss_mode(),
                "inprivate", data_sharing_environment, extra_symbols);

        expr_list = parse_dependences_ompss_clause(
                pragma_line.get_clause("concurrent"),
                parsing_context);
        get_info_from_dependences<DEP_OMPSS_CONCURRENT>(
                expr_list, default_data_attr, this->in_ompss_mode(),
                "concurrent", data_sharing_environment, extra_symbols);

        expr_list = parse_dependences_ompss_clause(
                pragma_line.get_clause("commutative"),
                parsing_context);
        get_info_from_dependences<DEP_OMPSS_COMMUTATIVE>(
                expr_list, default_data_attr, this->in_ompss_mode(),
                "commutative", data_sharing_environment, extra_symbols);

        // OpenMP standard clauses
        PragmaCustomClause depends = pragma_line.get_clause("depend");
        get_dependences_openmp(depends, parsing_context, data_sharing_environment,
                default_data_attr, extra_symbols);
    }

    void Core::get_basic_dependences_info(
            TL::PragmaCustomLine pragma_line,
            Nodecl::NodeclBase parsing_context,
            DataEnvironment& data_sharing_environment,
            DataSharingAttribute default_data_attr,
            ObjectList<Symbol>& extra_symbols)
    {
        ObjectList<Nodecl::NodeclBase> expr_list;

        expr_list = parse_dependences_ompss_clause(
                pragma_line.get_clause("in",/* deprecated */ "input"),
                parsing_context);
        get_info_from_dependences<DEP_DIR_IN>(
                expr_list, default_data_attr, this->in_ompss_mode(),
                "in", data_sharing_environment, extra_symbols);

        expr_list = parse_dependences_ompss_clause(
                pragma_line.get_clause("out",/* deprecated */ "output"),
                parsing_context);
        get_info_from_dependences<DEP_DIR_OUT>(
                expr_list, default_data_attr, this->in_ompss_mode(),
                "out", data_sharing_environment, extra_symbols);

        expr_list = parse_dependences_ompss_clause(
                pragma_line.get_clause("inout"),
                parsing_context);
        get_info_from_dependences<DEP_DIR_INOUT>(
                expr_list, default_data_attr, this->in_ompss_mode(),
                "inout", data_sharing_environment, extra_symbols);
    }

    void Core::handle_taskwait_dependences(
            PragmaCustomLine pragma_line,
            Nodecl::NodeclBase parsing_context,
            DataSharingAttribute default_data_attr,
            DataEnvironment& data_environment,
            ObjectList<Symbol>& extra_symbols)
    {
        // Handling the 'on' clause of the taskwait construct
        ObjectList<Nodecl::NodeclBase> expr_list =
            parse_dependences_ompss_clause(
                    pragma_line.get_clause("on"),
                    parsing_context);
        get_info_from_dependences<DEP_DIR_INOUT>(
                expr_list, default_data_attr, this->in_ompss_mode(),
                "on", data_environment, extra_symbols);

        // Handling the 'in', 'out' and 'inout' clauses of the taskwait construct
        get_basic_dependences_info(
                pragma_line,
                parsing_context,
                data_environment,
                default_data_attr,
                extra_symbols);

        // Handling the OpenMP dependency clauses of the taskwait construct
        get_dependences_openmp(
                pragma_line.get_clause("depend"),
                parsing_context,
                data_environment,
                default_data_attr,
                extra_symbols);
    }

    void Core::handle_implicit_dependences_of_task_reductions(
            TL::PragmaCustomLine pragma_line,
            DataSharingAttribute default_data_attr,
            DataEnvironment& data_sharing_environment,
            ObjectList<Symbol>& extra_symbols)
    {
        Nodecl::NodeclBase parsing_context = pragma_line;
        TL::ObjectList<ReductionSymbol> reductions;
        data_sharing_environment.get_all_reduction_symbols(reductions);

        TL::ObjectList<Nodecl::NodeclBase> reduction_expressions =
            reductions.map<Nodecl::NodeclBase>(&ReductionSymbol::get_reduction_expression);

        get_info_from_dependences<DEP_OMPSS_CONCURRENT>(
                reduction_expressions, default_data_attr, this->in_ompss_mode(),
                "reduction", data_sharing_environment, extra_symbols);
    }

    namespace {
        const decl_context_t* decl_context_map_id(const decl_context_t* d)
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
                    error_printf_at(locus,
                            "skipping item '%s' in 'depend' clause because it lacks dependence-type\n",
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
            src << pad_to_column(clause.get_pragma_line().get_column()) << current_dep_expr;

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

            dep_set->append(expr);
        }

        regfree(&preg);
    }

    void Core::get_dependences_openmp(
            TL::PragmaCustomClause clause,
            Nodecl::NodeclBase parsing_context,
            DataEnvironment& data_sharing_environment,
            DataSharingAttribute default_data_attr,
            ObjectList<Symbol>& extra_symbols)
    {
            TL::ObjectList<Nodecl::NodeclBase> in, out, inout;
            parse_dependences_openmp_clause(
                    parsing_context,
                    clause,
                    in,
                    out,
                    inout,
                    clause.get_locus());

            get_info_from_dependences<DEP_DIR_IN>(in, default_data_attr,
                    this->in_ompss_mode(), "depend(in: )", data_sharing_environment, extra_symbols);

            get_info_from_dependences<DEP_DIR_OUT>(out, default_data_attr,
                    this->in_ompss_mode(), "depend(out: )", data_sharing_environment, extra_symbols);

            get_info_from_dependences<DEP_DIR_INOUT>(inout, default_data_attr,
                    this->in_ompss_mode(), "depend(inout: )", data_sharing_environment, extra_symbols);
    }

    namespace {

        void c_cxx_ompss_dep_expression(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output)
        {
            Source::c_cxx_check_expression_adapter(a, decl_context, nodecl_output);
        }

        void fortran_ompss_dep_expression(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output)
        {
            Source::fortran_check_expression_adapter(a, decl_context, nodecl_output);
            if (!nodecl_is_null(*nodecl_output)
                    && !nodecl_is_err_expr(*nodecl_output))
            {
                // Make sure this is a variable
                *nodecl_output = ::fortran_expression_as_variable(*nodecl_output);
            }
        }
    }

    ObjectList<Nodecl::NodeclBase> Core::parse_dependences_ompss_clause(
            PragmaCustomClause clause,
            TL::ReferenceScope parsing_scope)
    {
        ObjectList<Nodecl::NodeclBase> result;

        // We return an empty list of expressions if the current clause is not defined
        if (!clause.is_defined())
            return result;

        ObjectList<std::string> arguments = clause.get_tokenized_arguments();
        for (ObjectList<std::string>::iterator it = arguments.begin();
                it != arguments.end();
                it++)
        {
            Source src;
            src << "#line " << clause.get_pragma_line().get_line() << " \"" << clause.get_pragma_line().get_filename() << "\"\n";
            src << pad_to_column(clause.get_pragma_line().get_column()) << *it;

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

    bool is_strict_dependency(DependencyDirection dir)
    {
        switch (dir)
        {
            case DEP_DIR_IN:
            case DEP_DIR_OUT:
            case DEP_DIR_INOUT:
            case DEP_OMPSS_DIR_IN_PRIVATE:
                return true;
            default:
                return false;
        }
    }

    bool is_weak_dependency(DependencyDirection dir)
    {
        switch (dir)
        {
            case DEP_OMPSS_WEAK_IN:
            case DEP_OMPSS_WEAK_OUT:
            case DEP_OMPSS_WEAK_INOUT:
                return true;
            default:
                return false;
        }
    }

    std::string get_dependency_direction_name(DependencyDirection d)
    {
        switch (d)
        {
            case DEP_DIR_UNDEFINED:
                return "<<undefined-dependence>>";
            case DEP_DIR_IN:
                return "in";
            case DEP_DIR_OUT:
                return "out";
            case DEP_DIR_INOUT:
                return "inout";
            case DEP_OMPSS_CONCURRENT:
                return "concurrent";
            case DEP_OMPSS_COMMUTATIVE:
                return "commutative";
            case DEP_OMPSS_WEAK_IN:
                return "weakin";
            case DEP_OMPSS_WEAK_OUT:
                return "weakout";
            case DEP_OMPSS_WEAK_INOUT:
                return "weakinout";
            default:
                return "<<unknown-dependence-kind?>>";
        }
    }
} }
