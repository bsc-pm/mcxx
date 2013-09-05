#!/usr/bin/python

import string

# Normal case
#
# void __kmpc_atomic_<datatype>_<operation>( ident_t *id_ref, int gtid, TYPE * lhs, TYPE rhs );
#

# Capture case
#
# TYPE __kmpc_atomic_<datatype>_<operation>_cpt( ident_t *id_ref, int gtid, TYPE * lhs, TYPE rhs, int flag );
#

type_map = {
"fixed1" : "int8_t",
"fixed1u" : "uint8_t",
"fixed2" : "int16_t",
"fixed2u" : "uint16_t",
"fixed4" : "int32_t",
"fixed4u" : "uint32_t",
"fixed8" : "int32_t",
"fixed8u" : "uint32_t",
"float4" : "float",
"float8" : "double",
"float10" : "long double",
"float16" : "__float128",
"cmplx4" : "_Complex float",
"cmplx8" : "_Complex double",
"cmplx10" : "_Complex long double",
"cmplx16" : "_Complex __float128"
}

f = open("atomic-list.def", "r")
for l in f:
    fields = string.split(l.rstrip(), "_");
    typename = fields[0]
    op = fields[1]
    rev = ""
    cpt = False
    i = 2
    while (i < len(fields)):
        if fields[i] == "rev":
            rev = "_rev"
        elif fields[i] == "cpt":
            cpt = True
        i = i + 1
    # _Complex __float128 is not possible
    if typename == "cmplx16":
        continue

    c_typename = type_map.get(typename, "<<error-%s>>" % (typename))
    if not cpt:
        print "void __kmpc_atomic_%s_%s%s( ident_t* id_ref, int gtid, %s *lhs, %s rhs);" % (typename, op, rev, c_typename, c_typename)
    else :
        if typename[0:5] != "cmplx":
            print "%s __kmpc_atomic_%s_%s%s_cpt( ident_t* id_ref, int gtid, %s *lhs, %s rhs, int flag);" % (c_typename, typename, op, rev, c_typename, c_typename)
        else:
            print "void __kmpc_atomic_%s_%s%s_cpt( ident_t* id_ref, int gtid, %s *lhs, %s rhs, %s out, int flag);" % (typename, op, rev, c_typename, c_typename, c_typename)

