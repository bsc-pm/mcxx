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




#ifndef TL_FOR_NEST_HPP
#define TL_FOR_NEST_HPP

#include <tl-common.hpp>
#include <tl-statement.hpp>

namespace TL
{
    /*
       This class is useful when finding nested loops. This class handles
       perfect and nonperfect nest loops.
       */
    class LIBTL_CLASS ForNestInfo : public Object
    {
        private:
            bool contains_a_for_statement(Statement stmt);
            bool contains_a_for_statement(Statement stmt, AST_t &result);

            bool has_nested_for(ForStatement &for_stmt, bool &perfect);
            void gather_nest_info();

            bool _is_perfect;
            ObjectList<ForStatement> _for_nest;
            ForStatement _for_stmt;
        public:
            //! Creates a ForNestInfo using a ForStatement as a reference
            /*! for_stmt is the reference ForStatement used 
              when computing all properties of this class
              */
            ForNestInfo(ForStatement for_stmt);

            /*! Returns the list of nested loops */
            ObjectList<ForStatement> get_nest_list();

            //! States whether the loop nest is perfect
            /*! A perfect loop nest is made of for statements whose loop body
             * is just another for loop except for the innermost loop. 
             * 
             * A non-perfect loop nest is made of for statements whose loop
             * body can contain other statements that are not for statements,
             * and, except for the innermost, contain one for statemeent.
             */
            bool is_perfect();

            //! States whether all loops in the nest are regular loops
            bool is_all_regular();
    };
}

#endif // TL_FOR_NEST_HPP
