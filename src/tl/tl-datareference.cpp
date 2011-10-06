#include "tl-datareference.hpp"

namespace TL
{
    bool DataReference::gather_info_data_expr_rec(Nodecl::NodeclBase expr, 
            Symbol &base_sym, 
            Source &size, 
            Source &addr, 
            Type& type,
            bool enclosing_is_array,
            bool & pointer_access_member,
            std::stringstream& warnlog)
    {
        internal_error("Not implemented yet", 0);
    }

    bool DataReference::gather_info_data_expr(Nodecl::NodeclBase &expr, 
            Symbol &base_sym, 
            Source &size, 
            Source &addr,
            Type &type,
            std::stringstream& warnlog)
    {
        internal_error("Not implemented yet", 0);
    }

    Source DataReference::safe_expression_size(Type type, Scope sc)
    {
        internal_error("Not implemented yet", 0);
    }

    //! Constructors of a DataReference
    /*! 
      Use is_valid to know if the expression wrapped as a DataReference
      is eligible as a data reference.
      */
    DataReference::DataReference(Nodecl::NodeclBase expr)
        : _type(NULL)
    {
        internal_error("Not implemented yet", 0);
    }

    //! Copy constructor
    DataReference::DataReference(const DataReference& data_ref)
        : _type(data_ref._type)
    {
        internal_error("Not implemented yet", 0);
    }

    //! Copy assignment operator
    DataReference& DataReference::operator=(const DataReference& data_ref)
    {
        internal_error("Not implemented yet", 0);
    }

    //! States whether this expression is a data reference
    /*!
      Not all expressions are data references, as defined by this class,
      use this function to check it
      */
    bool DataReference::is_valid() const
    {
        internal_error("Not implemented yet", 0);
    }

    //! States whether this expression is a data reference
    /*!
      Not all expressions are data references, as defined by this class,
      use this function to check it
      */
    bool DataReference::is_valid(std::string& reason) const
    {
        internal_error("Not implemented yet", 0);
    }

    //! Returns the warning log
    /*!
      This is the same message as is_valid(std::string&) stores in its first parameter
      */
    std::string DataReference::get_warning_log() const
    {
        internal_error("Not implemented yet", 0);
    }

    //! Gets the base symbol
    /*!
      The base symbol is the entity to which we know we are expressing
      its object or a subobject

      Note for instance that a.b and a.c have the same base symbol, while
      the subobject being named is different.
      */
    Symbol DataReference::get_base_symbol() const
    {
        internal_error("Not implemented yet", 0);
    }

    //! Returns a way to obtain an address of the data reference
    /*!
      Since data references express named entities, there is a way to
      get its address. This function returns a Source with an
      expression which evaluates to the address of the data reference.
      */
    Source DataReference::get_address() const
    {
        internal_error("Not implemented yet", 0);
    }

    //! Returns the size of the data reference
    /*!
      This function returns an expression which evaluates to the known
      size of a data reference
      */
    Source DataReference::get_sizeof() const
    {
        internal_error("Not implemented yet", 0);
    }

    //! Returns a type representing the data covered by the data reference
    /*!
      This function returns a type which represents the data covered
      by the data reference.

      \note The type returned may not be fully valid if it contains arrays
      as this function uses Type::get_array_to(const std::string&)
      */
    Type DataReference::get_data_type() const
    {
        internal_error("Not implemented yet", 0);
    }

}
