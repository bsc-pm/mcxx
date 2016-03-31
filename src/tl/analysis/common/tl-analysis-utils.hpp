/*--------------------------------------------------------------------
 (C) Copyright 2006-2014 Barcelona Supercomputing Center             **
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

#ifndef TL_ANALYSIS_UTILS_HPP
#define TL_ANALYSIS_UTILS_HPP

#include "tl-nodecl-utils.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-tribool.hpp"

#include <set>
#include <map>

#define VERBOSE (debug_options.analysis_verbose || \
                 debug_options.enable_debug_code)

#define ANALYSIS_PERFORMANCE_MEASURE debug_options.analysis_perf

#define ANALYSIS_INFO debug_options.analysis_info

namespace TL {
namespace Analysis {

    double time_nsec();

    typedef Nodecl::NodeclBase NBase;
    typedef ObjectList<NBase> NodeclList;
    typedef std::set<NBase, Nodecl::Utils::Nodecl_structural_less> NodeclSet;
    typedef std::pair<NBase, NBase> NodeclPair;
    typedef std::multimap<NBase, NodeclPair, Nodecl::Utils::Nodecl_structural_less> NodeclMap; 
    typedef std::map<Nodecl::NodeclBase, tribool, Nodecl::Utils::Nodecl_structural_less> NodeclTriboolMap;

namespace Utils {

    // ******************************************************************************************* //
    // ************************** Common methods with analysis purposes ************************** //

    //! Returns a hashed string depending on \ast
    std::string generate_hashed_name(NBase ast);

    //! Returns the Nodecl containing the main in \ast, if it exists.
    //! Returns a null Nodecl otherwise
    NBase find_main_function(NBase ast);

    // ************************ END common methods with analysis purposes ************************ //
    // ******************************************************************************************* //


    
    // **************************************************************************************** //
    // ********************* Methods to manage nodecls and their containers ******************* //
    
    //! Returns all nodecl bases of a given nodecl
    NodeclList get_nodecls_base(const NBase& n);
    
    //!Returns the nodecl base of a nodecl when it only has one (a nodecl base has always a related symbol)
    NBase get_nodecl_base(const NBase& n);
    
    bool nodecl_set_contains_nodecl(const NBase& nodecl, const NodeclSet& set);
    bool nodecl_set_contains_nodecl_pointer(const NBase& nodecl, const NodeclSet& set);
    NBase nodecl_set_contains_enclosing_nodecl(const NBase& n, const NodeclSet& set);
    Nodecl::List nodecl_set_contains_enclosed_nodecl(const NBase& n, const NodeclSet& set);
    Nodecl::List nodecl_set_contains_pointed_nodecl(const NBase& n, const NodeclSet& set);
    
    NodeclMap nodecl_map_union(const NodeclMap& m1, const NodeclMap& m2);
    NodeclSet nodecl_set_union(const NodeclSet& s1, const NodeclSet& s2);
    
    NodeclSet nodecl_set_difference(const NodeclSet& s1, const NodeclSet& s2);
    NodeclMap nodecl_map_minus_nodecl_set(const NodeclMap& m, const NodeclSet& s);
    
    bool nodecl_set_equivalence(const NodeclSet& s1, const NodeclSet& s2);
    bool nodecl_map_equivalence(const NodeclMap& s1, const NodeclMap& s2);
    
    // ********************* Methods to manage nodecls and their containers ******************* //
    // **************************************************************************************** //
    
    
    
    // **************************************************************************************** //
    // **************************** Class for Auto-Scoping purposes *************************** //
    
    class LIBTL_CLASS AutoScopedVariables
    {
    private:
        NodeclSet _private_vars;
        NodeclSet _firstprivate_vars;
        NodeclSet _race_vars;
        NodeclSet _shared_vars;
        NodeclSet _undef_vars;
        
    public:
        // ************* Constructor ************* //
        
        AutoScopedVariables();
        AutoScopedVariables(NodeclSet private_vars, NodeclSet firstprivate_vars,
                            NodeclSet race_vars, NodeclSet shared_vars, NodeclSet undef_vars);
        
        // ********* Getters and setters ********* //
        
        NodeclSet get_private_vars();
        NodeclSet get_firstprivate_vars();
        NodeclSet get_race_vars();
        NodeclSet get_shared_vars();
        NodeclSet get_undef_vars();
    };
    
    // ************************** END class for Auto-Scoping purposes ************************* //
    // **************************************************************************************** //
    

    // ******************************************************************************************* //
    // ****************************** Visitor for Top Level nodes ******************************** //

    //! Visitor to visit Top Level nodes
    //! It also recognizes the main function in C/C++ codes if it exists
    class LIBTL_CLASS TopLevelVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
    private:
        // ******* Class attributes ******* //
        NBase _main;
        ObjectList<NBase> _functions;
        std::map<Symbol, NBase> _analysis_asserted_funcs;
        std::string _filename;

    public:
        // ********* Constructors ********* //
        //! Constructor
        TopLevelVisitor();

        // ****** Getters and setters ****** //
        NBase get_main() const;
        ObjectList<NBase> get_functions() const;
        std::map<Symbol, NBase> get_asserted_funcs() const;

        // ******** Visiting methods ******* //

        void walk_functions(const NBase& n);

        //! Visiting methods
        /*!We re-implement all TopLevel nodecl to avoid continue visiting in case
         * the main function has already been found.
         * TopLevel
         *      FunctionCode
         *      ObjectInit
         *      CxxDecl             : CxxDecl, CxxDef, CxxExplicitInstantiation
         *                            CxxExternExplicitInstantiation, CxxUsingNamespace, CxxUsingDecl
         *      PragmaDirective     : PragmaCustomDirective, PragmaCustomDeclaration
         *      Compatibility       : UnknownPragma, SourceComment, PreprocessorLine, Verbatim
         *                            AsmDefinition, GccAsmDefinition, GccAsmSpec, UpcSyncStatement
         *                            GccBuiltinVaArg, GxxTrait, Text
         */
        Ret unhandled_node(const NBase& n);
        Ret visit(const Nodecl::AsmDefinition& n);
        Ret visit(const Nodecl::Analysis::AssertDecl& n);
        Ret visit(const Nodecl::GccAsmDefinition& n);
        Ret visit(const Nodecl::GccAsmSpec& n);
        Ret visit(const Nodecl::GccBuiltinVaArg& n);
        Ret visit(const Nodecl::CxxDecl& n);
        Ret visit(const Nodecl::CxxDef& n);
        Ret visit(const Nodecl::CxxExplicitInstantiationDef& n);
        Ret visit(const Nodecl::CxxExplicitInstantiationDecl& n);
        Ret visit(const Nodecl::CxxUsingNamespace& n);
        Ret visit(const Nodecl::CxxUsingDecl& n);
        Ret visit(const Nodecl::FunctionCode& n);
        Ret visit(const Nodecl::GxxTrait& n);
        Ret visit(const Nodecl::ObjectInit& n);
        Ret visit(const Nodecl::OpenMP::SimdFunction& n);
        Ret visit(const Nodecl::OmpSs::TaskCall& n);
        Ret visit(const Nodecl::PragmaCustomDeclaration& n);
        Ret visit(const Nodecl::PragmaCustomDirective& n);
        Ret visit(const Nodecl::PreprocessorLine& n);
        Ret visit(const Nodecl::SourceComment& n);
        Ret visit(const Nodecl::Text& n);
        Ret visit(const Nodecl::UnknownPragma& n);
        Ret visit(const Nodecl::UpcSyncStatement& n);
        Ret visit(const Nodecl::Verbatim& n);
    };

    // **************************** END visitor for Top Level nodes ****************************** //
    // ******************************************************************************************* //
    
    
    
    // ******************************************************************************************* //
    // **************************** Class defining the types of usage **************************** //
    
    struct UsageKind {
        enum Usage_tag {
            NONE        = 1u << 1,
            USED        = 1u << 2,
            DEFINED     = 1u << 3,
            UNDEFINED   = 1u << 4
        } _usage_type;
        
        UsageKind()
            : _usage_type(NONE)
        {}
        
        UsageKind(Usage_tag u)
            : _usage_type(u)
        {}
        
        UsageKind(int u)
            : _usage_type(Usage_tag(u))
        {}
        
        UsageKind operator|(UsageKind u)
        {
            return UsageKind(int(this->_usage_type) | int(u._usage_type));
        }
    };
    
    // ************************** END class defining the types of usage ************************** //
    // ******************************************************************************************* //
    
    
    
    // ******************************************************************************************* //
    // ************************************ Printing methods ************************************* //
    
    void makeup_dot_block(std::string& str);
    std::string prettyprint_nodecl_set(const NodeclSet& s, bool print_in_dot);
    std::string prettyprint_nodecl_map(const NodeclMap& m, bool print_in_dot);
    
    // ********************************** END printing methods *********************************** //
    // ******************************************************************************************* //
    
}
}
}

#endif          // TL_ANALYSIS_UTILS_HPP
