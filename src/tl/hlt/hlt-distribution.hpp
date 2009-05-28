/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#ifndef HLT_DISTRIBUTION_HPP
#define HLT_DISTRIBUTION_HPP

#include "hlt-transform.hpp"
#include "tl-langconstruct.hpp"

namespace TL
{
    namespace HLT
    {
        //! \addtogroup HLT High Level Transformations
        //! @{

        //! This class implements loop distribution
        /*!
          Loop distribution transforms a given loop into several loops, each
          one performing one of the statements of the original loop. In
          addition, the class supports expanding scalars, this is, adding them
          a dimension so non loop carried dependences can be preserved
         */
        class LIBHLT_CLASS LoopDistribution : public BaseTransform
        {
            protected:
                virtual TL::Source get_source();
            private:
                TL::ForStatement _for_stmt;
                TL::Source do_distribution();
                TL::ObjectList<TL::Symbol> _expand;
            public:
                //! Creates a LoopDistribution object
                /*!
                  \param for_stmt Regular loop that will be distributed
                 */
                LoopDistribution(TL::ForStatement for_stmt);
                //! Creates a LoopDistribution object
                /*!
                  \param for_stmt Regular loop that will be distributed
                  \param expanded List of symbols we want to expand in order to
                  preserve dependences
                 */
                LoopDistribution(TL::ForStatement for_stmt, 
                        TL::ObjectList<TL::Symbol> expanded);
        };

        //! Creates a LoopDistribution object
        /*!
          \param for_stmt Regular loop that will be distributed
          \param expanded_scalars List of symbols we want to expand in order to
          preserve dependences
         */
        LIBHLT_EXTERN LoopDistribution distribute_loop(TL::ForStatement for_stmt, ObjectList<TL::Symbol> expanded_scalars);
        //! @}
    }
}

#endif // HLT_DISTRIBUTION_HPP
