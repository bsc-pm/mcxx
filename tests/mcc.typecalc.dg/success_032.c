void __my_finite (double __x)
{ 
    int *i;
    i = ((union { double __d; int __i[2]; }) {__d: __x}).__i;
}
