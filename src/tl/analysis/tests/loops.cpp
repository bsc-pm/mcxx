
int main (int argc, char** argv)
{
    float counter = 0.0;
    for (int i = 0; i < 10; i++)
        for (int j = 0 j < 10; ++j)
            for (;;)
                counter = counter + 1.0;
    
    int i = 20, j = 0;
    do {
        i--;
    } while (i > 0)
        
    i = 20;
    while (i>0 && j<20)
    {
        --i;
        j++;
    }
    
    for (i = 0; i < 30; i = i+1)
    {
        if (i < 10)
            break;
        else if (i < 20)
            continue;
        else
            std:cout << "20 < i < 30" << std::endl;
    }
    
l1: std::cout << "label 1" << std::endl;
    goto l1;
    
    return 0;
}