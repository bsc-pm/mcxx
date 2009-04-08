#ifndef TL_FOR_NEST_HPP
#define TL_FOR_NEST_HPP

#include <tl-statement.hpp>

namespace TL
{
    /*
       This class is useful when finding nested loops. This class handles
       perfect and nonperfect nest loops.
       */
    class ForNestInfo : public Object
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
