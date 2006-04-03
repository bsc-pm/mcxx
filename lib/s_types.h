#ifndef S_TYPES
#define S_TYPES

typedef enum
{ false, true }
bool_type;

/*
 *
 *    Prototipo de funcion destructora de un objeto
 *    Object es el objeto a borrar.La función no debe hacer el free si no k 
 *    lo deja listo para que se pueda hacer el free del objeto.
 *     
 */

typedef void delete_func (void *object);


#endif
