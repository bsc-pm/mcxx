#include <exception>
#include <iostream>

class myexception: public exception
{
    virtual const char* what() const throw()
    {
        return "My exception happened";
    }
} myex;

int main(int argc, char** argv)
{
    int c = rand() % 4;
    try {
        switch (c)
        {
            case 0: throw "";
                    break;
            case 1: throw 1;
                    break;
            case 2: throw myex;
                    break;
            case 3: throw;
                    break;
            default: std::cout << "This isn't possible" << std::endl;
                     return 1;
        }
    }
    catch( char *str ) 
    {
        std::cout << "Char pointer exception" << std::endl;
    }
    catch( int n ) 
    {
        std::cout << "Integer exception" << std::endl;
    }
    catch (exception& e)
    {
        cout << e.what() << endl;
    }
    catch ( ... ) 
    {
        std::cout << "Default exception" << std::endl;
    }
    
    return 0;
}