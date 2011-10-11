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


#ifndef TL_LOOP_ANALYSIS_HPP
#define TL_LOOP_ANALYSIS_HPP


#include "tl-nodecl.hpp"
#include "tl-node.hpp"
#include "tl-symbol.hpp"


namespace TL
{
    struct InductionVarInfo {
        Symbol _s;
        Nodecl::NodeclBase _lb;
        Nodecl::NodeclBase _ub;     // value included in the range
        Nodecl::NodeclBase _step;
        bool _step_is_one;
        
        InductionVarInfo(Symbol s, Nodecl::NodeclBase lb)
            : _s(s), _lb(lb), _ub(Nodecl::NodeclBase::null()), _step(Nodecl::NodeclBase::null()), _step_is_one(false)
        {}
        
        Symbol get_symbol()
        {
            return _s;
        }

        Nodecl::NodeclBase get_lb() const
        {
            return _lb;
        }

        void set_lb(Nodecl::NodeclBase lb)
        {
            _lb = lb;
        }

        Nodecl::NodeclBase get_ub() const
        {
            return _ub;
        }

        void set_ub(Nodecl::NodeclBase ub)
        {
            _ub = ub;
        }

        Nodecl::NodeclBase get_step() const
        {
            return _step;
        }

        void set_step(Nodecl::NodeclBase step)
        {
            _step = step;
        }
        
        bool step_is_one() const
        {
            return _step_is_one;
        }
        
        void set_step_is_one(bool step_is_one)
        {
            _step_is_one = step_is_one;
        }
    };
        
    class LIBTL_CLASS LoopAnalysis {    
        
    private:
        ObjectList<InductionVarInfo*> _induction_vars;
        
        // *** Private methods *** //
        void compute_induction_vars_from_loop_control(Nodecl::LoopControl loop_control, Node* loop_node);
        void traverse_loop_init(Nodecl::NodeclBase init);
        void traverse_loop_cond(Nodecl::NodeclBase cond);
        void traverse_loop_step(Nodecl::NodeclBase step);
        
        void compute_arrays_info_in_loop(Node* node);
        
        /*!
         * 
         */
        Nodecl::ArraySection set_array_access_range(Node* node, Nodecl::ArraySubscript subscript, 
                                                    ExtensibleSymbol ei, char use_type);
        void set_array_access_range_in_list(Node* node, ext_sym_set ext_syms_l, char use_type);
        
        /*!
         * \param node Node in the graph we are analysing
         * \return Whether the node defines any of the induction variables or if those variables have an undefined behaviour
         *         - 0 the symbol is not defined
         *         - 1 the symbol is defined
         *         - 2 we cannot ensure what's happening with the symbol
         */
        char induction_vars_are_defined_in_node(Node* node);
        
        void prettyprint_induction_var_info(InductionVarInfo* var_info);
        
    public:
        
        // *** Constructors *** //
        LoopAnalysis();
       
        
        // *** Modifiers *** //
        void compute_induction_varaibles_info(Node* loop_node);
        
        void compute_arrays_info(Node* node);
        
        void analyse_loops(Node* node);
        
        
        // *** Getters and setters *** //
        InductionVarInfo* induction_vars_l_contains_symbol(Symbol s);
    };
}

#endif      // TL_LOOP_ANALYSIS_HPP