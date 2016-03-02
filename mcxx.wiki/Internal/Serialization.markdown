
 Only pointer types must be serialized, all values should be stored inline.

## Serializable types requirements

Interface. Each type TYPE to be serialized will have to implement the following functions


        void serialize_TYPE(serializer_t* s, TYPE* t)
        {
          // Only pointer fields must be serialized
          (s->serialize)(s, &t->field1, serialize_TYPE1);
          (s->serialize)(s, &t->field2, serialize_TYPE2);
          ...
          (s->serialize)(s, &t->fieldN, serialize_TYPEN);
        }

## Serialization process

We will serialize Nodecl::TopLevel using a Berkeley DB database.