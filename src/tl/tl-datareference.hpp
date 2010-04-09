#ifndef TL_DATA_REFERENCE_HPP
#define TL_DATA_REFERENCE_HPP

#include "tl-langconstruct.hpp"
#include "tl-source.hpp"

namespace TL
{
    /*!
      This class is used to handle a subset of expressions which have the following property:

        They express objects or subobjects of a known program entity.

      Only the following expressions can satisfy this property:

           d -> id
                d[e1]
                d[e1:e2]
                d.id
                [e1]...[eN] pd
                *d

      Where 'd' is a data reference and 'pd' a data reference whose type is pointer

      Note that silly expressions like &*d and *&d are assumed to be d
    */
    class DataReference : public Expression
    {
        private:
            bool _valid;
            Symbol _base_symbol;
            Type _type;
            Source _size;
            Source _addr;

            static bool gather_info_data_expr_rec(Expression expr, 
                    Symbol &base_sym, 
                    Source &size, 
                    Source &addr, 
                    bool enclosing_is_array);

            static bool gather_info_data_expr(Expression &expr, 
                    Symbol &base_sym, 
                    Source &size, 
                    Source &addr);
        public:
            DataReference(AST_t ast, ScopeLink scope_link);
            //! Constructors of a DataReference
            /*! 
              Use is_valid to know if the expression wrapped as a DataReference
              is eligible as a data reference.
             */
            DataReference(Expression expr);

            //! States whether this expression is a data reference
            /*!
              Not all expressions are data references, as defined by this class,
              use this function to check it
              */
            bool is_valid() const;

            //! Gets the base symbol
            /*!
              The base symbol is the entity to which we know we are expressing
              its object or a subobject

              Note for instance that a.b and a.c have the same base symbol, while
              the subobject being named is different.
              */
            Symbol get_base_symbol() const;

            //! Returns a way to obtain an address of the data reference
            /*!
              Since data references express named entities, there is a way to
              get its address. This function returns a Source with an
              expression which evaluates to the address of the data reference.
              */
            Source get_address() const;

            //! Returns the size of the data reference
            /*!
              This function returns an expression which evaluates to the known
              size of a data reference
              */
            Source get_sizeof() const;
    };
}

#endif // TL_DATA_REFERENCE_HPP
