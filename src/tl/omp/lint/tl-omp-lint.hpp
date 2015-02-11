/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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

#ifndef TL_OMP_LINT_HPP
#define TL_OMP_LINT_HPP

#include "tl-analysis-base.hpp"
#include "tl-compilerphase.hpp"
#include "tl-nodecl-visitor.hpp"

namespace TL {
namespace OpenMP {
    
    // We need this method to be visible so it can be used on demand
    // (Necessary for correctness analysis checker phase)
    void launch_correctness(
            const TL::Analysis::AnalysisBase& analysis,
            std::string log_file_path);
    
    class WritesVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
    private:
        ObjectList<Nodecl::NodeclBase> _defined_vars;
        bool _define;
        
        void visit_assignment( const Nodecl::NodeclBase& lhs, const Nodecl::NodeclBase& rhs );
        void visit_xx_crement( const Nodecl::NodeclBase& rhs );
        
    public:
        // *** Constructor *** //
        WritesVisitor( );
        
        // *** Getters *** //
        ObjectList<Nodecl::NodeclBase> get_defined_symbols( );
        
        // *** Modifiers *** //
        void clear( );
        
        // *** Visiting methods *** //
        Ret visit( const Nodecl::AddAssignment& n );
        Ret visit( const Nodecl::ArithmeticShrAssignment& n );
        Ret visit( const Nodecl::ArraySubscript& n );
        Ret visit( const Nodecl::Assignment& n );
        Ret visit( const Nodecl::BitwiseAndAssignment& n );
        Ret visit( const Nodecl::BitwiseOrAssignment& n );
        Ret visit( const Nodecl::BitwiseShlAssignment& n );
        Ret visit( const Nodecl::BitwiseShrAssignment& n );
        Ret visit( const Nodecl::BitwiseXorAssignment& n );
        Ret visit( const Nodecl::ClassMemberAccess& n );
        Ret visit( const Nodecl::Dereference& n );
        Ret visit( const Nodecl::DivAssignment& n );
        Ret visit( const Nodecl::MinusAssignment& n );
        Ret visit( const Nodecl::ModAssignment& n );
        Ret visit( const Nodecl::MulAssignment& n );
        Ret visit( const Nodecl::ObjectInit& n );
        Ret visit( const Nodecl::Postdecrement& n );
        Ret visit( const Nodecl::Postincrement& n );
        Ret visit( const Nodecl::Predecrement& n );
        Ret visit( const Nodecl::Preincrement& n );
        Ret visit( const Nodecl::Reference& n );
        Ret visit( const Nodecl::Symbol& n );
    };
    
    //! This class transforms OpenMP pragmas to the Nodecl representation of parallelism
    class Lint : public TL::CompilerPhase
    {
    private:
        std::string _disable_phase;
        std::string _correctness_log_path;
        std::string _lint_deprecated_flag;
        std::string _ompss_mode_str;

        bool _ompss_mode_enabled;

        void set_ompss_mode( const std::string& ompss_mode_str);
        void set_lint_deprecated_flag(const std::string& lint_deprecated_flag_str);
        
    public:
        Lint();

        virtual void run(TL::DTO& dto);
        virtual void pre_run(TL::DTO& dto);

        virtual ~Lint() { }
    };
    
}
}

#endif // TL_OMP_LINT_HPP
