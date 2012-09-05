
int main (int argc, char** argv)
{
    if (argc > 1)
        switch (argc)
        {
            case 1: std::cout << "argc = 1" << std::endl;
                    break;
            case 2: std::cout << "argc = 2" << std::endl;
            case 3: std::cout << "argc = 2 || argc = 3" << std::endl;
                    break;
            default: std::cout << "default case" << std::endl;
            case 4: std::cout << "argc = 4" << std::endl;
                    break;
        };
    else if (argc == 0)
    {
        ;
    }
    else
    {
        (argc > 5) ? std::cout << "argc > 5" << std::endl;
                   : std::cout << "argc < 5" << std::endl;
    }
    
    return 0;
}