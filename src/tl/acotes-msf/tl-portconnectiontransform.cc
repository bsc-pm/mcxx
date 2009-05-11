/*
    Acotes Translation Phase
    Copyright (C) 2007 - David Rodenas Pico <david.rodenas@bsc.es>
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
    
    $Id: tl-acotestransform.cpp 1611 2007-07-10 09:28:44Z drodenas $
*/
#include "tl-portconnectiontransform.h"


#include <assert.h>
#include <sstream>
#include "ac-port.h"
#include "ac-portconnection.h"
#include "ac-task.h"

namespace TL { namespace Acotes {
    
        
    /* ******************************************************
     * * No constructor
     * ******************************************************/
    
    PortConnectionTransform::PortConnectionTransform(const std::string &d)
            : driver(d)
    {
    }
    
    
    /* ******************************************************
     * * Auxiliary generators
     * ******************************************************/
    
    Source PortConnectionTransform::generatePortConnection(PortConnection* portConnection)
    {
        assert(portConnection);
        
        Source ss;

      if ((portConnection->getInput()->getTask()->isImplicitTask()) ||
          (portConnection->getOutput()->getTask()->isImplicitTask()))
        ss << "";
      else {
	//ss << declare_buffer_handle_associated xavim
	ss << "msf_buffer_handle_p " 
           << portConnection->getOutput()->getTask()->getName() << "_"
           << portConnection->getOutput()->getNumber() << "_"
           << portConnection->getInput()->getTask()->getName() << "_"
           << portConnection->getInput()->getNumber() << "_buff = 0L;";

        ss << "msf_tasks_buffer_connect"
                << "( " << portConnection->getOutput()->getTask()->getName()
                << ", " << portConnection->getOutput()->getNumber()
                << ", " << portConnection->getInput()->getTask()->getName()
                << ", " << portConnection->getInput()->getNumber()
                << ", 0, " // was 16
           << portConnection->getOutput()->getTask()->getName() << "_"
           << portConnection->getOutput()->getNumber() << "_"
           << portConnection->getInput()->getTask()->getName() << "_"
           << portConnection->getInput()->getNumber() << "_buff"
		<< ");";
      }
        
        return ss;
    }
    
    Source PortConnectionTransform::generateConnection(PortConnection* portConnection)
    {
        assert(portConnection);
        
        Source ss;

      if ((portConnection->getInput()->getTask()->isImplicitTask()) ||
          (portConnection->getOutput()->getTask()->isImplicitTask()))
        ss << "";
      else {
	//ss << "msf_buffer_handle_p " 
        //   << portConnection->getOutput()->getTask()->getName() << "_"
        //   << portConnection->getOutput()->getNumber() << "_"
       //    << portConnection->getInput()->getTask()->getName() << "_"
       //    << portConnection->getInput()->getNumber() << "_buff = 0L;";

        ss << "fprintf (__f_tg, \"" 
               << portConnection->getOutput()->getTask()->getName() 
               << " -> "
               << portConnection->getInput()->getTask()->getName()
               << " [label=\\\""
               << portConnection->getOutput()->getTask()->getName()
               << "," << portConnection->getOutput()->getNumber()
               << ":%d "
               << portConnection->getInput()->getTask()->getName()
               << "," << portConnection->getInput()->getNumber()
               << ":%d\\\"];\\n\", acotes__bs[acotes__tg]["
                  << portConnection->getOutput()->getTask()->getNum()
                  << "]["
                  << portConnection->getOutput()->getNumber()
                  << "][0], acotes__bs[acotes__tg]["
                  << portConnection->getInput()->getTask()->getNum()
                  << "]["
                  << portConnection->getInput()->getNumber()
                  << "][0]);";
      }
        
        return ss;
    }
    
    
    
} /* end namespace Acotes */ } /* end namespace TL */
