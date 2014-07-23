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

#ifndef TL_INDUCTION_VARIABLES_DATA_HPP
#define TL_INDUCTION_VARIABLES_DATA_HPP

#include "tl-analysis-utils.hpp"
#include "tl-nodecl.hpp"

namespace TL {
namespace Analysis {
namespace Utils {

    // ********************************************************************************************* //
    // ************************* Class representing and induction variable ************************* //

    enum InductionVarType {
        BASIC_IV,
        DERIVED_IV
    };

    class LIBTL_CLASS InductionVar {
    private:
        NBase _var;

        NBase _lb;         /*!< Lower bound within a loop */
        NBase _ub;         /*!< Upper bound within a loop (included) */
        NBase _incr;       /*!< Stride within a loop */

        NodeclList _incrs;  /*!< List of modifications to an Induction Variable */
                                                // Example: loop { iv = iv + 100; iv = iv + 200 }
                                                // _incrs = { 100, 200 } 
        
        InductionVarType _type;         /*!< Type of iv: '1' = basic, '2' = derived */
        NBase _family;     /*!< Family of the IV. For basic IVs, the family is the IV itself */

    public:

        // *** Constructors *** //
        //! Constructor to store variables that have upper and lower limits and stride but aren't Induction Variables
        InductionVar(NBase var);
        //! Induction Variable common constructor
        InductionVar(NBase var, InductionVarType type, NBase family);


        // *** Getters and Setters *** //
        NBase get_variable() const;
        void set_variable(NBase s);

        NBase get_lb() const;
        void set_lb(NBase lb);

        NBase get_ub() const;
        void set_ub(NBase ub);

        NBase get_increment() const;
        void set_increment(NBase incr);
        bool is_increment_one() const;

        NodeclList get_increment_list() const;
        void set_increment_list(NodeclList incr_list);
        
        std::string get_type_as_string() const;

        NBase get_family() const;

        bool is_basic();

        bool operator==(const InductionVar& rhs) const;

        std::string print_iv_as_range() const;  
    };

    typedef ObjectList<InductionVar*> InductionVarList;
    
    struct LinearVars {
        ObjectList<Symbol> _syms;
        NBase _step;
        
        LinearVars(ObjectList<Symbol> syms, NBase step)
            : _syms(syms), _step(step)
        {}
        
        ObjectList<Symbol> get_symbols() const { return _syms; }
        
        NBase get_step() const { return _step; }
    };
    
    // *********************** END class representing and induction variable *********************** //
    // ********************************************************************************************* //



    // ********************************************************************************************* //
    // ********************************* Induction Variables utils ********************************* //

    typedef std::multimap<int, InductionVar*> InductionVarsPerNode;
    
    //! Prints into a string all data associated to each induction variable of \p iv_list
    std::string prettyprint_induction_vars(InductionVarList iv_list, bool to_dot);

    //! Prints all induction variables information to the standard error
    void print_induction_vars(InductionVarsPerNode iv_list);
    
    //! Look for nodecl \p var in the \p iv_list list of induction variables
    bool induction_variable_list_contains_variable(InductionVarList iv_list, NBase var);

    //! Looks for the induction variable \p var in the list \p ivs of induction variables
    InductionVar* get_induction_variable_from_list(InductionVarList ivs, NBase var);
    
    //! Looks for the induction variable \p var in a map \p ivs of "node, induction variables"
    InductionVar* get_induction_variable_from_list(Utils::InductionVarsPerNode ivs, NBase var);

    // ******************************* END Induction Variables utils ******************************* //
    // ********************************************************************************************* //
}
}
}

#endif      // TL_INDUCTION_VARIABLES_DATA_HPP