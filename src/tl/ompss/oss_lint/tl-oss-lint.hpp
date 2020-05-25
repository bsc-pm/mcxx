/*--------------------------------------------------------------------
 ( C) Copyright 2006-2014 Barcelona* Supercomputing Center
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

#ifndef TL_OSS_LINT_HPP
#define TL_OSS_LINT_HPP

#include "tl-dom-tree.hpp"
#include "tl-analysis-base.hpp"
#include "tl-compilerphase.hpp"
#include "tl-nodecl-visitor.hpp"

namespace TL {
namespace Nanos6 {

    // **************************************************************************************************** //
    // *************************** Class implementing OmpSs Lint static analysis ************************** //


    class LintAttributes {
    private:
        Analysis::NodeclSet _in;
        Analysis::NodeclSet _out;
        Analysis::NodeclSet _inout;
        Analysis::NodeclSet _alloc;
        Analysis::NodeclSet _free;

    public:
        LintAttributes()
            : _in(), _out(), _inout(), _alloc(), _free()
        {}

        LintAttributes(const LintAttributes& attrs)
            : _in(attrs._in), _out(attrs._out), _inout(attrs._inout),
              _alloc(attrs._alloc), _free(attrs._free)
        {}

        void add_attrs(LintAttributes* new_attrs)
        {
//             std::cerr << "Copying attributes from " << new_attrs << " to " << this << std::endl;
//             std::cerr << "Attrs in : " << new_attrs->_in.size() << std::endl;
//             std::cerr << "Attrs out : " << new_attrs->_out.size() << std::endl;
//             std::cerr << "Attrs inout : " << new_attrs->_inout.size() << std::endl;
//             std::cerr << "Attrs alloca : " << new_attrs->_alloc.size() << std::endl;
//             std::cerr << "Attrs free : " << new_attrs->_free.size() << std::endl;
            for (Analysis::NodeclSet::iterator it = new_attrs->_in.begin(); it != new_attrs->_in.end(); ++it)
                add_in(*it);
            for (Analysis::NodeclSet::iterator it = new_attrs->_out.begin(); it != new_attrs->_out.end(); ++it)
                add_out(*it);
            for (Analysis::NodeclSet::iterator it = new_attrs->_inout.begin(); it != new_attrs->_inout.end(); ++it)
                add_inout(*it);
            _alloc.insert(new_attrs->_alloc.begin(), new_attrs->_alloc.end());
            _free.insert(new_attrs->_free.begin(), new_attrs->_free.end());
        }

        void add_in(const Nodecl::NodeclBase& n)
        {
            if (_out.find(n) != _out.end())
            {
                _inout.insert(n);
                _out.erase(n);
            }
            if (_inout.find(n) == _inout.end())
            {
                _in.insert(n);
            }
        }

        Analysis::NodeclSet& get_in()
        {
            return _in;
        }

        void add_out(const Nodecl::NodeclBase& n)
        {
            _out.insert(n);
            if (_in.find(n) != _in.end())
            {
                _in.erase(n);
            }
            if (_inout.find(n) != _inout.end())
            {
                _inout.erase(n);
            }
        }

        Analysis::NodeclSet& get_out()
        {
            return _out;
        }

        void add_inout(const Nodecl::NodeclBase& n)
        {
//             std::cerr << "                      **  (" << this << ") Add inout: " << n.prettyprint() << std::endl;
            _inout.insert(n);
            if (_in.find(n) != _in.end())
            {
                _in.erase(n);
            }
            if (_out.find(n) != _out.end())
            {
                _out.erase(n);
            }
            _inout.insert(n);
        }

        Analysis::NodeclSet& get_inout()
        {
            return _inout;
        }

        void add_alloc(const Nodecl::NodeclBase& n)
        {
            _alloc.insert(n);
        }

        void add_alloc(const Analysis::NodeclSet& n)
        {
            _alloc.insert(n.begin(), n.end());
        }

        Analysis::NodeclSet& get_alloc()
        {
            return _alloc;
        }

        void add_free(const Nodecl::NodeclBase& n)
        {
            _free.insert(n);
        }

        void add_free(const Analysis::NodeclSet& n)
        {
            _free.insert(n.begin(), n.end());
        }

        Analysis::NodeclSet& get_free()
        {
            return _free;
        }

        bool is_empty() const
        {
            return _in.empty() && _out.empty() && _inout.empty() && _alloc.empty() && _free.empty();
        }

        void clear()
        {
            _in.clear();
            _out.clear();
            _inout.clear();
            _alloc.clear();
            _free.clear();
        }
    };

    /*! Enrich the source code with clauses and directives for the dynamic OmpSs Lint checker:
     *    * #pragma lint in() out()  -> ignore what is inside
     *    * #pragma oss task in(...) out(...) verified(expr) -> ignore what is inside
     *    * From each BB, traverse inner-outer to decide #pragma oss lint in() out()
     *    * #pragma oss task in(...) out(...) -> check that dependences are correct and add verified 
     */
    class OssLint : public TL::CompilerPhase
    {
    private:

        std::string _only_tasks_str;
        bool _only_tasks;
        ObjectList<TL::Analysis::ExtensibleGraph*> _pcfgs;

        void set_only_tasks_mode(const std::string& oss_lint_only_tasks_str);

        void launch_correctness(
            const TL::Analysis::AnalysisBase& analysis,
            std::string current_filename);

        void oss_lint_analyze_all(TL::Analysis::ExtensibleGraph* pcfg);
        void oss_lint_analyze_tasks(TL::Analysis::ExtensibleGraph* pcfg);

        bool analyze_path(Analysis::Node* n,
                ObjectList<Nodecl::NodeclBase>& stmts,
                LintAttributes* attrs,
                Analysis::Utils::InductionVarList& ind_vars,
                bool& continue_outer_levels);

        bool analyze_stmt(Analysis::Node* n,
                ObjectList<Nodecl::NodeclBase>& stmts,
                LintAttributes* attrs,
                Analysis::Utils::InductionVarList& ind_vars,
                bool& continue_outer_levels);

        bool analyze_simple_node(
                Analysis::Node* n,
                ObjectList<Nodecl::NodeclBase>& path,
                LintAttributes* attrs,
                const Analysis::Utils::InductionVarList& ind_vars,
                bool& continue_outer_levels);

        bool analyze_task_node(
                Analysis::Node* n,
                ObjectList<Nodecl::NodeclBase>& path,
                LintAttributes* attrs,
                Analysis::Utils::InductionVarList& ind_vars,
                bool& continue_outer_levels);

        bool analyze_simple_graph_node(Analysis::Node* n,
                ObjectList<Nodecl::NodeclBase>& path,
                LintAttributes* attrs,
                Analysis::Utils::InductionVarList& ind_vars,
                bool& continue_outer_levels);

        bool analyze_graph_node(Analysis::Node* n,
                ObjectList<Nodecl::NodeclBase>& path,
                LintAttributes* attrs,
                Analysis::Utils::InductionVarList& ind_vars,
                bool& continue_outer_levels);

        bool analyze_nested_call(
                const Nodecl::FunctionCall& function_call,
                Nodecl::List& nested_env);

        void wrap_statements(
                ObjectList<Nodecl::NodeclBase>& stmts,
                LintAttributes* attrs);

        Analysis::ExtensibleGraph* get_pcfg_by_symbol(
                const TL::Symbol& s);

    public:
        OssLint();

        virtual void run(TL::DTO& dto);

        virtual ~OssLint() { }
    };

    class LIBTL_CLASS OssLintVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
    private:
        
    public:
        //! Visiting methods
        
        Ret visit( const Nodecl::ForStatement& n );
    };

    // ************************* End Class implementing OmpSs Lint static analysis ************************ //
    // **************************************************************************************************** //

}
}

#endif      // TL_OSS_LINT_HPP
