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




#ifndef TL_SETDTO_COMPILER_PHASE_HPP
#define TL_SETDTO_COMPILER_PHASE_HPP

#include "tl-compilerphase.hpp"
#include "tl-symbol.hpp"
#include "tl-type.hpp"

namespace TL
{
    //! Base class for any compiler phase
    class LIBTL_CLASS SetDTOCompilerPhase : public CompilerPhase
    {
		private:
			ObjectList<std::string> _variable;
			ObjectList<std::string> _type;
			ObjectList<std::string> _value;

		public:
            //! Constructor of the phase
			SetDTOCompilerPhase();

            //! Entry point of the phase
            /*!
             * This function set some dto features read from the Phase
             */
			virtual void run(TL :: DTO& dto);

            //! Modification to do in the DTO
            /*!
             * This function stores the modifications that will be set in the DTO
			 * when the phase runs
             */
			int set_dto(const char* data);
	};
}

#endif // TL_SETDTO_COMPILER_PHASE_HPP
