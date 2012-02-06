#include "tl-datareference.hpp"
#include "tl-nodecl-visitor.hpp"

namespace TL
{
    struct DataReferenceVisitor : public Nodecl::NodeclVisitor<void>
    {
        public:
            DataReferenceVisitor(DataReference& data_ref)
                : _data_ref(data_ref)
            {
            }

        private:
            DataReference& _data_ref;

            virtual void unhandled_node(const Nodecl::NodeclBase & tree) 
            { 
                _data_ref._is_valid = false;
                _data_ref._error_log = 
                    tree.get_locus() + ": error: expression not allowed in data-reference\n";
            }

            // Symbol
            virtual void visit(const Nodecl::Symbol& sym)
            {
                _data_ref._base_symbol = sym.get_symbol();

                TL::Type t = sym.get_symbol().get_type();
                if (t.is_any_reference())
                    t = t.references_to();

                _data_ref._data_type = t;
            }

#if 0
            virtual void visit(const Nodecl::Derreference& derref)
            {
                if (derref.get_rhs().is<Nodecl::Reference>())
                {
                    // *&a is like a
                    walk(derref.get_rhs().as<Nodecl::Reference>().get_rhs());
                    return;
                }

                walk(derref.get_rhs());

                if (!_data_ref.is_valid)
                    return;

                TL::Type t = derref.get_type();
                if (t.is_any_reference())
                    t = t.references_to();

                t = t.points_to();
                _data_ref._data_type = t;
            }
             
            virtual void visit(const Nodecl::Reference& ref)
            {
                if (ref.get_rhs().is<Nodecl::Derreference>())
                {
                    // &*a is like a
                    walk(ref.get_rhs().as<Nodecl::Derreference>().get_rhs());
                    return;
                }

                walk(derref.get_rhs());
                if (!_data_ref.is_valid)
                    return;

                TL::Type t = ref.get_type();
                _data_ref._data_type = t;
            }
#endif

            virtual void visit(const Nodecl::ArraySubscript& array)
            {
                walk(array.get_subscripted());

                if (!_data_ref._is_valid)
                    return;

                TL::Type t = array.get_type();
                if (t.is_any_reference())
                    t = t.references_to();

                _data_ref._data_type = t;
            }

            virtual void visit(const Nodecl::ClassMemberAccess& member)
            {
                walk(member.get_lhs());

                if (!_data_ref._is_valid)
                    return;

                TL::Type t = member.get_type();
                if (t.is_any_reference())
                    t = t.references_to();

                _data_ref._data_type = t;
            }
    };

    DataReference::DataReference(Nodecl::NodeclBase expr)
        : _is_valid(true), 
        _base_symbol(NULL), 
        _data_type(NULL), 
        _error_log("")
    {
        if (expr.get_type().is_error_type())
        {
            _is_valid = false;
            return;
        }

        DataReferenceVisitor data_ref_visitor(*this);
        data_ref_visitor.walk(expr);
    }

    //! States whether this expression is a data reference
    /*!
      Not all expressions are data references, as defined by this class,
      use this function to check it
      */
    bool DataReference::is_valid() const
    {
        return _is_valid;
    }

    //! Returns the warning log
    /*!
      This is the same message as is_valid(std::string&) stores in its first parameter
      */
    std::string DataReference::get_error_log() const
    {
        return _error_log;
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
        return _base_symbol;
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
        return _data_type;
    }
}
